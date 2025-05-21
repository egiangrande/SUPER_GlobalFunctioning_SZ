#Sensitivity analysis stratifying linear mixed effects models by Admission GF tertile. 

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
#Create tertiles 
quantile(df$admitGAS, probs = c(0, 1/3, 2/3, 1), na.rm = TRUE)

tertDf <- df %>%
  mutate(admitGF_tertile = ntile(admitGF, 3))  # ntile() divides into 3 equal groups

# Step 2: Create separate dataframes for each tertile
tertile1_df <- tertDf %>% filter(admitGF_tertile == 1) #Lower
tertile2_df <- tertDf %>% filter(admitGF_tertile == 2) #Middle
tertile3_df <- tertDf %>% filter(admitGF_tertile == 3) #Upper

#-----------------------------------------------------------------------
#Lower tertile
tert1_admitLME <- lme(admitGF ~ SZ_PGS + EA_PGS + sex + yearOfBirth + 
                        PC1 + PC2+ PC3+ PC4+ PC5+ PC6+ PC7+ PC8+ PC9+ PC10,
                      random = ~ 1 | Study_ID,
                      correlation = corCAR1(form = ~ admitDateNumeric | Study_ID),
                      data = tertile1_df,
                      na.action=na.exclude)
summary(tert1_admitLME)
intervals(tert1_admitLME)
r.squaredGLMM(tert1_admitLME)

tert1_dischargeLME <- lme(dischargeGF ~ SZ_PGS + EA_PGS + sex + yearOfBirth + 
                            logLos+ 
                            PC1 + PC2+ PC3+ PC4+ PC5+ PC6+ PC7+ PC8+ PC9+ PC10,
                          random = ~ 1 | Study_ID,
                          correlation = corCAR1(form = ~ dischargeDateNumeric | Study_ID),
                          data = tertile1_df,
                          na.action=na.exclude)
summary(tert1_dischargeLME)
intervals(tert1_dischargeLME)
r.squaredGLMM(tert1_dischargeLME)

tert1_changeLME <- lme(dischargeGF ~ SZ_PGS + EA_PGS +
                         admitGF +
                         sex + yearOfBirth + 
                         logLos+ 
                         PC1 + PC2+ PC3+ PC4+ PC5+ PC6+ PC7+ PC8+ PC9+ PC10,
                       random = ~ 1 | Study_ID,
                       correlation = corCAR1(form = ~ dischargeDateNumeric | Study_ID),
                       data = tertile1_df,
                       na.action=na.exclude)
summary(tert1_changeLME)
intervals(tert1_changeLME)
r.squaredGLMM(tert1_changeLME)

#-------------------------------------------------------------------
#Middle Tertile
tert2_admitLME <- lme(admitGF ~ SZ_PGS + EA_PGS + sex + yearOfBirth + 
                        PC1 + PC2+ PC3+ PC4+ PC5+ PC6+ PC7+ PC8+ PC9+ PC10,
                      random = ~ 1 | Study_ID,
                      correlation = corCAR1(form = ~ admitDateNumeric | Study_ID),
                      data = tertile2_df,
                      na.action=na.exclude)
summary(tert2_admitLME)
intervals(tert2_admitLME)
r.squaredGLMM(tert2_admitLME)

tert2_dischargeLME <- lme(dischargeGF ~ SZ_PGS + EA_PGS + sex + yearOfBirth + 
                            logLos+ 
                            PC1 + PC2+ PC3+ PC4+ PC5+ PC6+ PC7+ PC8+ PC9+ PC10,
                          random = ~ 1 | Study_ID,
                          correlation = corCAR1(form = ~ dischargeDateNumeric | Study_ID),
                          data = tertile2_df,
                          na.action=na.exclude)
summary(tert2_dischargeLME)
intervals(tert2_dischargeLME)
r.squaredGLMM(tert2_dischargeLME)

tert2_changeLME <- lme(dischargeGF ~ SZ_PGS + EA_PGS +
                         admitGF +
                         sex + yearOfBirth + 
                         logLos+ 
                         PC1 + PC2+ PC3+ PC4+ PC5+ PC6+ PC7+ PC8+ PC9+ PC10,
                       random = ~ 1 | Study_ID,
                       correlation = corCAR1(form = ~ dischargeDateNumeric | Study_ID),
                       data = tertile2_df,
                       na.action=na.exclude)
summary(tert2_changeLME)
intervals(tert2_changeLME)
r.squaredGLMM(tert2_changeLME)

#-------------------------------------------------------------------
#Upper Tertile 
tert3_admitLME <- lme(admitGF ~ SZ_PGS + EA_PGS + sex + yearOfBirth + 
                        PC1 + PC2+ PC3+ PC4+ PC5+ PC6+ PC7+ PC8+ PC9+ PC10,
                      random = ~ 1 | Study_ID,
                      correlation = corCAR1(form = ~ admitDateNumeric | Study_ID),
                      data = tertile3_df,
                      na.action=na.exclude)
summary(tert3_admitLME)
intervals(tert3_admitLME)
r.squaredGLMM(tert3_admitLME)

tert3_dischargeLME <- lme(dischargeGF ~ SZ_PGS + EA_PGS + sex + yearOfBirth + 
                            logLos+ 
                            PC1 + PC2+ PC3+ PC4+ PC5+ PC6+ PC7+ PC8+ PC9+ PC10,
                          random = ~ 1 | Study_ID,
                          correlation = corCAR1(form = ~ dischargeDateNumeric | Study_ID),
                          data = tertile3_df,
                          na.action=na.exclude)
summary(tert3_dischargeLME)
intervals(tert3_dischargeLME)
r.squaredGLMM(tert3_dischargeLME)

tert3_changeLME <- lme(dischargeGF ~ SZ_PGS + EA_PGS +
                         admitGF +
                         sex + yearOfBirth + 
                         logLos+ 
                         PC1 + PC2+ PC3+ PC4+ PC5+ PC6+ PC7+ PC8+ PC9+ PC10,
                       random = ~ 1 | Study_ID,
                       correlation = corCAR1(form = ~ dischargeDateNumeric | Study_ID),
                       data = tertile3_df,
                       na.action=na.exclude)
summary(tert3_changeLME)
intervals(tert3_changeLME)
r.squaredGLMM(tert3_changeLME)

#---------------------------------------------------------------------------------
#Plot results

#Extract PGS estimates from each model and organize in prep for combined plot:

#Lower Tertile
tert1_admitLMETidy_model <- broom.mixed::tidy(tert1_admitLME)
tert1_admitLMETidy_model <- tert1_admitLMETidy_model[!tert1_admitLMETidy_model$term %in% c("(Intercept)", "PC1", "PC2", "PC3", "PC4", "PC5", "PC6", "PC7", "PC8", "PC9", "PC10",
                                                                                           "sexMale", "yearOfBirth", "logLos", "sd_(Intercept)", "sd_Observation"), ]
tert1_admitLMETidy_model$term <- factor(tert1_admitLMETidy_model$term, levels = c("EA_PGS", "SZ_PGS"))

tert1_dischargeLMETidy_model <- broom.mixed::tidy(tert1_dischargeLME)
tert1_dischargeLMETidy_model <- tert1_dischargeLMETidy_model[!tert1_dischargeLMETidy_model$term %in% c("(Intercept)", "PC1", "PC2", "PC3", "PC4", "PC5", "PC6", "PC7", "PC8", "PC9", "PC10",
                                                                                                       "sexMale", "yearOfBirth", "logLos", "sd_(Intercept)", "sd_Observation"), ]
tert1_dischargeLMETidy_model$term <- factor(tert1_dischargeLMETidy_model$term, levels = c("EA_PGS", "SZ_PGS"))

tert1_changeLMETidy_model <- broom.mixed::tidy(tert1_changeLME)
tert1_changeLMETidy_model <- tert1_changeLMETidy_model[!tert1_changeLMETidy_model$term %in% c("(Intercept)", "PC1", "PC2", "PC3", "PC4", "PC5", "PC6", "PC7", "PC8", "PC9", "PC10",
                                                                                              "sexMale", "yearOfBirth", "logLos", "sd_(Intercept)", "sd_Observation"), ]
tert1_changeLMETidy_model$term <- factor(tert1_changeLMETidy_model$term, levels = c("EA_PGS", "SZ_PGS"))

tert1_admitLMETidy_model$model <- "Admission GF"
tert1_dischargeLMETidy_model$model <- "Discharge GF"
tert1_changeLMETidy_model$model <- "Change GF"

tert1_admitLMETidy_model$x_adjusted <- ifelse(tert1_admitLMETidy_model$term == "SZ_PGS", -0.02, 0.02)
tert1_dischargeLMETidy_model$x_adjusted <- ifelse(tert1_dischargeLMETidy_model$term == "SZ_PGS", -0.02, 0.02)
tert1_changeLMETidy_model$x_adjusted <- ifelse(tert1_changeLMETidy_model$term == "SZ_PGS", -0.02, 0.02)

tert1_combined_df <- bind_rows(tert1_admitLMETidy_model, tert1_dischargeLMETidy_model, tert1_changeLMETidy_model)

tert1_combined_df$x_position <- model_positions[as.character(tert1_combined_df$model)] + tert1_combined_df$x_adjusted

tert1_combined_df$term <- factor(tert1_combined_df$term, levels = c("SZ_PGS", "EA_PGS"))

#Middle Tertile
tert2_admitLMETidy_model <- broom.mixed::tidy(tert2_admitLME)
tert2_admitLMETidy_model <- tert2_admitLMETidy_model[!tert2_admitLMETidy_model$term %in% c("(Intercept)", "PC1", "PC2", "PC3", "PC4", "PC5", "PC6", "PC7", "PC8", "PC9", "PC10",
                                                                                           "sexMale", "yearOfBirth", "logLos", "sd_(Intercept)", "sd_Observation"), ]
tert2_admitLMETidy_model$term <- factor(tert2_admitLMETidy_model$term, levels = c("EA_PGS", "SZ_PGS"))

tert2_dischargeLMETidy_model <- broom.mixed::tidy(tert2_dischargeLME)
tert2_dischargeLMETidy_model <- tert2_dischargeLMETidy_model[!tert2_dischargeLMETidy_model$term %in% c("(Intercept)", "PC1", "PC2", "PC3", "PC4", "PC5", "PC6", "PC7", "PC8", "PC9", "PC10",
                                                                                                       "sexMale", "yearOfBirth", "logLos", "sd_(Intercept)", "sd_Observation"), ]
tert2_dischargeLMETidy_model$term <- factor(tert2_dischargeLMETidy_model$term, levels = c("EA_PGS", "SZ_PGS"))

tert2_changeLMETidy_model <- broom.mixed::tidy(tert2_changeLME)
tert2_changeLMETidy_model <- tert2_changeLMETidy_model[!tert2_changeLMETidy_model$term %in% c("(Intercept)", "PC1", "PC2", "PC3", "PC4", "PC5", "PC6", "PC7", "PC8", "PC9", "PC10",
                                                                                              "sexMale", "yearOfBirth", "logLos", "sd_(Intercept)", "sd_Observation"), ]
tert2_changeLMETidy_model$term <- factor(tert2_changeLMETidy_model$term, levels = c("EA_PGS", "SZ_PGS"))

tert2_admitLMETidy_model$model <- "Admission GF"
tert2_dischargeLMETidy_model$model <- "Discharge GF"
tert2_changeLMETidy_model$model <- "Change GF"

tert2_admitLMETidy_model$x_adjusted <- ifelse(tert2_admitLMETidy_model$term == "SZ_PGS", -0.02, 0.02)
tert2_dischargeLMETidy_model$x_adjusted <- ifelse(tert2_dischargeLMETidy_model$term == "SZ_PGS", -0.02, 0.02)
tert2_changeLMETidy_model$x_adjusted <- ifelse(tert2_changeLMETidy_model$term == "SZ_PGS", -0.02, 0.02)

tert2_combined_df <- bind_rows(tert2_admitLMETidy_model, tert2_dischargeLMETidy_model, tert2_changeLMETidy_model)
tert2_combined_df$x_position <- model_positions[as.character(tert2_combined_df$model)] + tert2_combined_df$x_adjusted
tert2_combined_df$term <- factor(tert2_combined_df$term, levels = c("SZ_PGS", "EA_PGS"))

#Upper Tertile
tert3_admitLMETidy_model <- broom.mixed::tidy(tert3_admitLME)
tert3_admitLMETidy_model <- tert3_admitLMETidy_model[!tert3_admitLMETidy_model$term %in% c("(Intercept)", "PC1", "PC2", "PC3", "PC4", "PC5", "PC6", "PC7", "PC8", "PC9", "PC10",
                                                                                           "sexMale", "yearOfBirth", "logLos", "sd_(Intercept)", "sd_Observation"), ]
tert3_admitLMETidy_model$term <- factor(tert3_admitLMETidy_model$term, levels = c("EA_PGS", "SZ_PGS"))

tert3_dischargeLMETidy_model <- broom.mixed::tidy(tert3_dischargeLME)
tert3_dischargeLMETidy_model <- tert3_dischargeLMETidy_model[!tert3_dischargeLMETidy_model$term %in% c("(Intercept)", "PC1", "PC2", "PC3", "PC4", "PC5", "PC6", "PC7", "PC8", "PC9", "PC10",
                                                                                                       "sexMale", "yearOfBirth", "logLos", "sd_(Intercept)", "sd_Observation"), ]
tert3_dischargeLMETidy_model$term <- factor(tert3_dischargeLMETidy_model$term, levels = c("EA_PGS", "SZ_PGS"))

tert3_changeLMETidy_model <- broom.mixed::tidy(tert3_changeLME)
tert3_changeLMETidy_model <- tert3_changeLMETidy_model[!tert3_changeLMETidy_model$term %in% c("(Intercept)", "PC1", "PC2", "PC3", "PC4", "PC5", "PC6", "PC7", "PC8", "PC9", "PC10",
                                                                                              "sexMale", "yearOfBirth", "logLos", "sd_(Intercept)", "sd_Observation"), ]
tert3_changeLMETidy_model$term <- factor(tert3_changeLMETidy_model$term, levels = c("EA_PGS", "SZ_PGS"))

tert3_admitLMETidy_model$model <- "Admission GF"
tert3_dischargeLMETidy_model$model <- "Discharge GF"
tert3_changeLMETidy_model$model <- "Change GF"

tert3_admitLMETidy_model$x_adjusted <- ifelse(tert3_admitLMETidy_model$term == "SZ_PGS", -0.02, 0.02)
tert3_dischargeLMETidy_model$x_adjusted <- ifelse(tert3_dischargeLMETidy_model$term == "SZ_PGS", -0.02, 0.02)
tert3_changeLMETidy_model$x_adjusted <- ifelse(tert3_changeLMETidy_model$term == "SZ_PGS", -0.02, 0.02)

tert3_combined_df <- bind_rows(tert3_admitLMETidy_model, tert3_dischargeLMETidy_model, tert3_changeLMETidy_model)
tert3_combined_df$x_position <- model_positions[as.character(tert3_combined_df$model)] + tert3_combined_df$x_adjusted
tert3_combined_df$term <- factor(tert3_combined_df$term, levels = c("SZ_PGS", "EA_PGS"))

#Combine and plot:
add_term_tertile_labels <- function(df, tertile_label) {
  df$tertile <- factor(tertile_label, levels = c("Lower", "Middle", "Upper"))
  df$term_full <- paste0(df$term, "_", df$tertile)
  return(df)
}

tert1_combined_df <- add_term_tertile_labels(tert1_combined_df, "Lower")
tert2_combined_df <- add_term_tertile_labels(tert2_combined_df, "Middle")
tert3_combined_df <- add_term_tertile_labels(tert3_combined_df, "Upper")

combined_df <- bind_rows(tert1_combined_df, tert2_combined_df, tert3_combined_df)

combined_df$term_full <- factor(combined_df$term_full, levels = c(
  "SZ_PGS_Lower", "EA_PGS_Lower",
  "SZ_PGS_Middle", "EA_PGS_Middle",
  "SZ_PGS_Upper", "EA_PGS_Upper"
))

model_positions <- c("Admission GF" = 1, "Discharge GF" = 1.5, "Change GF" = 2)
pgs_nudges <- c("SZ_PGS" = -.009, "EA_PGS" = .009)
tertile_nudges <- c("Lower" = -0.08, "Middle" = 0, "Upper" = 0.08)

combined_df$x_position <- model_positions[combined_df$model] +
  pgs_nudges[as.character(combined_df$term)] +
  tertile_nudges[as.character(combined_df$tertile)]

linetypes_by_tertile <- c("Lower" = "dotted", "Middle" = "solid", "Upper" = "dashed")

x_labels <- c("Admission\nGF", "Discharge\nGF", "Change\nGF")
point_shapes <- c("SZ_PGS" = 15, "EA_PGS" = 16)
point_colors <- c("SZ_PGS" = "springgreen3", "EA_PGS" = "navyblue")

ggplot(combined_df, aes(x = x_position, y = estimate,
                        shape = term, color = term, linetype = tertile)) +  
  geom_linerange(aes(ymin = estimate - (1.96 * std.error),
                     ymax = estimate + (1.96 * std.error)), size = 1) +
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
    legend.position = c(.30, .92),
    legend.direction = "vertical",
    legend.box = "horizontal",
    legend.background = element_rect(fill = "white", color = "black"),
    legend.text = element_text(size = 26, lineheight = 1.5),  
    legend.title = element_blank()
  ) +
  scale_color_manual(
    values = point_colors,
    breaks = c("SZ_PGS", "EA_PGS"),
    labels = c("Schizophrenia PGS", "Educational Attainment PGS"),
    guide = guide_legend(order = 1)
  ) +
  scale_shape_manual(
    values = point_shapes,
    breaks = c("SZ_PGS", "EA_PGS"),
    labels = c("Schizophrenia PGS", "Educational Attainment PGS"),
    guide = guide_legend(order = 1)
  ) +
  scale_linetype_manual(
    values = linetypes_by_tertile,
    breaks = c("Upper", "Middle", "Lower"),
    labels = c("Upper Tertile", "Middle Tertile", "Lower Tertile"),
    guide = guide_legend(
      order = 2,
      override.aes = list(
        size = 1.5,
        color = "black",
        shape = NA,
        linetype = c("dashed", "solid", "dotted")
      )
    )
  ) +
  scale_y_continuous(limits = c(-0.65, 0.65), breaks = seq(-0.5, 0.5, by = 0.1)) +
  scale_x_continuous(breaks = c(1, 1.5, 2), labels = x_labels)
