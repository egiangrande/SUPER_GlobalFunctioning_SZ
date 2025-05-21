#Fit linear mixed effects models to longitudinal global functioning & PGS data
library(data.table)
library(dplyr)
library(purrr)
library(ggplot2)
library(tidyr)
library(nlme)
library(jtools)
library(broom.mixed)
library(MuMIn)

set.seed(123)

#Read in cleaned data set that includes the following for participants who met inclusion criteria: 
#Psychiatric hospitalizations (tall, i.e., one per row)
#Admission GF, Discharge GF, admission date, discharge date, length of stay (days),
#year of birth, sex, 
# SZ PGS, EA PGS, first 10 genetic PCs

df <- #

#Calculate log(length of stay)
df$logLos <- log(as.numeric(df$lengthOfStay)+1)

#----------------------------------------------------------------------------
#Fit linear mixed effects models with SZ PGS and EA PGS as fixed effects, covariates as fixed effects,
# a random intercept (participant), and a first-order residual autocorrelation structure in continuous 
# time (corCAR1) using lme() from the nlme package:  

#Admission GF: 
admitLME <- lme(admitGF ~ SZ_PGS + EA_PGS + sex + yearOfBirth + 
                         PC1 + PC2+ PC3+ PC4+ PC5+ PC6+ PC7+ PC8+ PC9+ PC10,
                       random = ~ 1 | Study_ID,
                       correlation = corCAR1(form = ~ admitDateNumeric | Study_ID),
                       data = df,
                       na.action=na.exclude)
summary(admitSzEaContAR)
intervals(admitSzEaContAR)

#Calculate pseudo-R^2: 
r.squaredGLMM(admitSzEaContAR)

#Discharge GF
dischargeLME <- lme(dischargeGF ~ SZ_PGS + EA_PGS + sex + yearOfBirth + 
                             logLos+ 
                             PC1 + PC2+ PC3+ PC4+ PC5+ PC6+ PC7+ PC8+ PC9+ PC10,
                           random = ~ 1 | Study_ID,
                           correlation = corCAR1(form = ~ dischargeDateNumeric | Study_ID),
                           data = df,
                           na.action=na.exclude)
summary(dischargeLME)
intervals(dischargeLME)
r.squaredGLMM(dischargeLME)

#Change GF (Discharge GF adjusted for Admit GF)
changeLME <- lme(dischargeGF ~ SZ_PGS + EA_PGS +
                                admitGF +
                                sex + yearOfBirth + 
                                logLos+ 
                                PC1 + PC2+ PC3+ PC4+ PC5+ PC6+ PC7+ PC8+ PC9+ PC10,
                              random = ~ 1 | Study_ID,
                              correlation = corCAR1(form = ~ dischargeDateNumeric | Study_ID),
                              data = df,
                              na.action=na.exclude)
summary(changeLME)
intervals(changeLME)
r.squaredGLMM(changeLME)

#----------------------------------------------------------------------------
#Plot results

#Extract PGS estimates:
admitLMETidy_model <- broom.mixed::tidy(admitLME)
admitLMETidy_model <- admitLMETidy_model[!admitLMETidy_model$term %in% c("(Intercept)", "PC1", "PC2", "PC3", "PC4", "PC5", "PC6", "PC7", "PC8", "PC9", "PC10",
                                                                                              "sexMale", "yearOfBirth", "logLos", "sd_(Intercept)", "sd_Observation"), ]
admitLMETidy_model$term <- factor(admitLMETidy_model$term, levels = c("EA_PGS", "SZ_PGS"))

dischargeLMETidy_model <- broom.mixed::tidy(dischargeLME)
dischargeLMETidy_model <- dischargeLMETidy_model[!dischargeLMETidy_model$term %in% c("(Intercept)", "PC1", "PC2", "PC3", "PC4", "PC5", "PC6", "PC7", "PC8", "PC9", "PC10",
                                                                                                          "sexMale", "yearOfBirth", "logLos", "sd_(Intercept)", "sd_Observation"), ]
dischargeLMETidy_model$term <- factor(dischargeLMETidy_model$term, levels = c("EA_PGS", "SZ_PGS"))

changeLMETidy_model <- broom.mixed::tidy(dischargeAdmitAdjusted)
changeLMETidy_model <- changeLMETidy_model[!changeLMETidy_model$term %in% c("(Intercept)", "PC1", "PC2", "PC3", "PC4", "PC5", "PC6", "PC7", "PC8", "PC9", "PC10",
                                                                                                                   "admitGF",
                                                                                                                   "sexMale", "yearOfBirth", "logLos", "sd_(Intercept)", "sd_Observation"), ]
changeLMETidy_model$term <- factor(changeLMETidy_model$term, levels = c("EA_PGS", "SZ_PGS"))

#Add model names 
admitLMETidy_model$model <- "Admission GF"
dischargeSzEaContARTidy_model$model <- "Discharge GF"
changeLMETidy_model$model <- "Change GF"

#Adjust spacing
model_positions <- c("Admission GF" = 1, "Discharge GF" = 1.5, "Change GF" = 2) 
admitLMETidy_model$x_adjusted <- ifelse(admitLMETidy_model$term == "SZ_PGS", -0.02, 0.02)
dischargeSzEaContARTidy_model$x_adjusted <- ifelse(dischargeSzEaContARTidy_model$term == "SZ_PGS", -0.02, 0.02)
changeLMETidy_model$x_adjusted <- ifelse(changeLMETidy_model$term == "SZ_PGS", -0.02, 0.02)

combined_df <- bind_rows(admitLMETidy_model, dischargeSzEaContARTidy_model, adjDischargeSzEaContARTidy_model)
combined_df$x_position <- model_positions[as.character(combined_df$model)] + combined_df$x_adjusted
combined_df$term <- factor(combined_df$term, levels = c("SZ_PGS", "EA_PGS"))

point_shapes <- c("SZ_PGS" = 15, "EA_PGS" = 16)  # Square for SZ PGS, Circle for EA PGS
point_colors <- c("SZ_PGS" = "springgreen3", "EA_PGS" = "navyblue")
line_types <- c("SZ_PGS" = "solid", "EA_PGS" = "solid")  # Solid lines for both terms

x_labels <- c("Admission\nGF",  "Discharge\nGF",  "Change\nGF") 

ggplot(combined_df, aes(x = x_position, y = estimate, shape = term, color = term, linetype = term)) +  
  geom_linerange(aes(ymin = estimate - (1.96 * std.error), ymax = estimate + (1.96 * std.error)), 
                 size = 1) +
  geom_point(size = 7) +
  geom_hline(yintercept = 0, linetype = "solid", color = "black", size = 1) +
  theme_minimal() +
  ylab(expression(beta~"("*95*"% CI)")) +
  theme(
    axis.text.x = element_text(size = 26, lineheight = 1.1),
    axis.text.y = element_text(size = 26),
    axis.title.x = element_blank(),
    axis.title.y = element_text(size = 26),
    panel.grid.major.x = element_blank(),
    panel.grid.minor = element_blank(),
    axis.line = element_line(linewidth = 0.5, color = "black"),
    legend.position = c(.35, .92),
    legend.direction = "vertical",
    legend.box = "horizontal",
    legend.background = element_rect(fill = "white", color = "black"),
    legend.box.background = element_rect(fill = "white", color = "black", linewidth = 1),
    legend.text = element_text(size = 26),
    legend.title = element_blank()
  ) +
  scale_color_manual(
    values = point_colors,
    breaks = c("SZ_PGS", "EA_PGS"),
    labels = c("Schizophrenia PGS", "Educational Attainment PGS")
  ) +
  scale_shape_manual(
    values = point_shapes,
    breaks = c("SZ_PGS", "EA_PGS"),
    labels = c("Schizophrenia PGS", "Educational Attainment PGS")
  ) +
  scale_linetype_manual(
    values = line_types,
    breaks = c("SZ_PGS", "EA_PGS"),
    labels = c("Schizophrenia PGS", "Educational Attainment PGS")
  ) +
  scale_y_continuous(limits = c(-0.6, 0.6), breaks = seq(-0.5, 0.5, by = 0.1)) +
  scale_x_continuous(breaks = c(1, 1.5, 2), labels = x_labels)
