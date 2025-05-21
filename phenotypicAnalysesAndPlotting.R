#Phenotypic analyses and plotting

library(data.table)
library(dplyr)
library(purrr)
library(ggplot2)
library(tidyr)
library(lme4)

set.seed(123)

##Read in cleaned data set that includes the following for participants who met inclusion criteria: 
#Psychiatric hospitalizations (tall, i.e., one per row)
#Admission GF, Discharge GF, admission date, discharge date, length of stay (days),
#year of birth, sex, 
#additional deep phenotypes from SUPER questionnaire, interview, and CANTAB.

pheno <- #

#------------------------------------------------------------------------------
#Plot distributions of raw GF (admission, discharge, and delta (discharge - admission))
pheno$deltaGF <- pheno$dischargeGF - pheno$admitGF
triGfDf <- subset(pheno, select = c(Study_ID, admitGF, dischargeGF,deltaGF))

triGfDfLong <- triGfDf %>%
  select(admitGF, dischargeGF,deltaGF) %>%
  pivot_longer(cols = c(admitGF, dischargeGF,deltaGF),
               names_to = "GF_type",
               values_to = "GF_value")

trimedians <- triGfDfLong %>%
  group_by(GF_type) %>%
  summarize(median_value = median(GF_value, na.rm = TRUE))

triGfDfLong$GF_type <- factor(triGfDfLong$GF_type, 
                                levels = c("admitGF", "dischargeGF", "deltaGF"), 
                                labels = c("Admission GF", "Discharge GF", "Delta GF"))

trimedians$GF_type <- factor(trimedians$GF_type, 
                              levels = c("admitGF", "dischargeGF", "deltaGF"), 
                              labels = c("Admission GF", "Discharge GF", "Delta GF"))
ggplot() +
  geom_histogram(data = subset(triGfDfLong, GF_type != "Delta GF"), 
                 aes(x = GF_value, color = GF_type, fill = GF_type), 
                 binwidth = 5, alpha = 0.1, position = "identity") +
  geom_histogram(data = subset(triGfDfLong, GF_type == "Delta GF"), 
                 aes(x = GF_value, color = GF_type, fill = GF_type), 
                 binwidth = 5, boundary = 0, alpha = 0.1, position = "identity") +
  geom_vline(xintercept = 0, color = "black", linetype = "solid", size = 1) +
  geom_vline(data = trimedians, aes(xintercept = median_value, color = GF_type),
             linetype = "dashed", size = 1) +  
  scale_color_manual(values = c("Admission GF" = "dodgerblue2", 
                                "Discharge GF" = "tomato3", 
                                "Delta GF" = "orange")) +  
  scale_fill_manual(values = c("Admission GF" = "dodgerblue2", 
                               "Discharge GF" = "tomato3", 
                               "Delta GF" = "orange")) +
  scale_x_continuous(limits = c(-30, 100), breaks = seq(-30, 100, by = 10)) +  
  scale_y_continuous(limits = c(0, 13000), breaks = seq(0, 12500, by = 2500)) +  
  labs(x = "Global Functioning", y = "Hospitalization Count", fill = NULL, color = NULL) +  
  theme_minimal() +
  theme(
    legend.position = c(0.85, 0.9),  
    legend.direction = "vertical",
    legend.box = "horizontal",
    legend.box.background = element_rect(color = "black", linewidth = 1),  
    legend.text = element_text(size = 26, color = "black"),  
    legend.title = element_blank(),  
    axis.text.x = element_text(size = 22),  
    axis.text.y = element_text(size = 22),  
    axis.title.x = element_text(size = 26),  
    axis.title.y = element_text(size = 26),  
    plot.title = element_text(size = 20),   
    panel.border = element_blank(),         
    axis.line = element_line(size = 0.5, color = "black"),  
    axis.line.x = element_line(size = 0.5, color = "black")  
  )

#----------------------------------------------------------------------------------
#Forest plot of associations among deep phenotypes and GF:

#List of phenotypic predictors. Numeric predictors are standardized. 
predictors <- c("hospitalizations_5_years", "days_in_hospital_5_years", "medLengthOfStay", 
                "suicideAttemptHx",
                "edu",
                "schoolDifficultiesHx",
                "rtiMedianRT", "rtiAllErrors", 
                "palPatternsReached", "palTotalAttempts","palFirstAttemptMem", "palTotalErrorsAdj")  

#Estimate associations:

#Admission GF 
admitResults <- data.frame(variable = character(), coefficient = numeric(), lower_CI = numeric(), upper_CI = numeric(), stringsAsFactors = FALSE)

for (var in predictors) {
  model <- lmer(as.formula(paste("admitGF ~", var, "+ sex + yearOfBirth + (1 | Study_ID)")), data = pheno, na.action = na.omit)
  coef <- summary(model)$coefficients[2, 1]
  conf_int <- confint(model, method = "profile")
  row_name <- grep(var, rownames(conf_int), value = TRUE)
  if (length(row_name) > 0) {
    conf_int_var <- conf_int[row_name[1], ]
    admitResults <- rbind(admitResults, data.frame(variable = var, coefficient = coef, lower_CI = conf_int_var[1], upper_CI = conf_int_var[2]))
  } else {
    message(paste("No matching row found for variable:", var))
  }
}

#Discharge GF
dischargeResults <- data.frame(variable = character(), coefficient = numeric(), lower_CI = numeric(), upper_CI = numeric(), stringsAsFactors = FALSE)

for (var in predictors) {
  model <- lmer(as.formula(paste("dischargeGF ~", var, "+sex + yearOfBirth + (1 | Study_ID)")), data = pheno, na.action = na.omit)
  coef <- summary(model)$coefficients[2, 1]
  conf_int <- confint(model, method = "profile")
  row_name <- grep(var, rownames(conf_int), value = TRUE)
  if (length(row_name) > 0) {
    conf_int_var <- conf_int[row_name[1], ]
    dischargeResults <- rbind(dischargeResults, data.frame(variable = var, coefficient = coef, lower_CI = conf_int_var[1], upper_CI = conf_int_var[2]))
  } else {
    message(paste("No matching row found for variable:", var))
  }
}

#Change GF
ChangeResults <- data.frame(variable = character(), coefficient = numeric(), lower_CI = numeric(), upper_CI = numeric(), stringsAsFactors = FALSE)

for (var in predictors) {
  model <- lmer(as.formula(paste("dischargeGF ~", var, "+ sex + yearOfBirth + admitGF+ (1 | Study_ID)")), data = pheno, na.action = na.omit)
    coef <- summary(model)$coefficients[2, 1]
    conf_int <- confint(model, method = "profile")
  row_name <- grep(var, rownames(conf_int), value = TRUE)
  
  if (length(row_name) > 0) {
    conf_int_var <- conf_int[row_name[1], ]
    ChangeResults <- rbind(ChangeResults, data.frame(variable = var, coefficient = coef, lower_CI = conf_int_var[1], upper_CI = conf_int_var[2]))
  } else {
    message(paste("No matching row found for variable:", var))
  }
}

#----------------------------------------------------------------------------------
#Plot
admitResults$OutcomeType <- "Admission GF"
dischargeResults$OutcomeType <- "Discharge GF"
ChangeResults$OutcomeType <- "Change GF"

allResults <- rbind(admitResults, dischargeResults, ChangeResults)

allResults$OutcomeType <- factor(allResults$OutcomeType, levels = c("Admission GF", "Discharge GF", "Change GF"))

outcome_colors <- c("Admission GF" = "dodgerblue2",  "Discharge GF" = "tomato3", "Change GF" = "orange")

outcome_shapes <- c("Admission GF" = 16,"Discharge GF" = 15, "Change GF" = 17)   

label_mapping <- c(
  "hospitalizations_5_years" = "Hospitalizations (5 Years)",
  "days_in_hospital_5_years" = "Days in Hospital (5 Years)",
  "medLengthOfStay" = "Median Length of Stay",
  "suicideAttemptHx" = "Suicide Attempt History",
  "edu" = "Education",
  "schoolDifficultiesHx" = "School Difficulties History",
  "rtiMedianRT" = "RTI Median Reaction Time",
  "rtiAllErrors" = "RTI All Errors",
  "palPatternsReached" = "PAL Patterns Reached",
  "palTotalAttempts" = "PAL Total Attempts",
  "palFirstAttemptMem" = "PAL First Attempt Memory",
  "palTotalErrorsAdj" = "PAL Total Errors Adjusted"
)

allResults$OutcomeType <- factor(allResults$OutcomeType, levels = rev(levels(allResults$OutcomeType)))

ggplot(allResults, aes(x = variable, y = coefficient, color = OutcomeType, shape = OutcomeType)) +
  geom_pointrange(aes(ymin = lower_CI, ymax = upper_CI), 
                  position = position_dodge(width = 0.8), size = 1.2) + 
  coord_flip(ylim = c(-3.5, 3.5)) +   
  geom_hline(yintercept = 0, color = "black", linetype = "solid", size = 1) +
  scale_color_manual(values = outcome_colors2, guide = guide_legend(reverse = TRUE)) +
  scale_shape_manual(values = outcome_shapes2, guide = guide_legend(reverse = TRUE)) +  
  scale_x_discrete(limits = rev(predictors), labels = label_mapping) + 
  scale_y_continuous(breaks = seq(-4, 4, by = 1), expand = c(0, 0)) +
  labs(x = NULL) +
  ylab(expression(beta~"("*95*"% CI)")) +
  theme_minimal() +
  theme(
    panel.grid.major.y = element_blank(),
    panel.grid.major.x = element_line(color = "grey80", size = 0.4),  
    panel.grid.minor = element_blank(),  
    legend.position = c(0.85, 0.85), 
    legend.direction = "vertical",
    legend.box = "horizontal",
    legend.background = element_rect(fill = "white", color = "black"),  
    legend.box.background = element_rect(fill = "white", color = "black", linewidth = 1),  
    legend.text = element_text(size = 26, color = "black"),  
    legend.title = element_blank(),  
    axis.text.x = element_text(size = 22),  
    axis.text.y = element_text(size = 22, margin = margin(r = 10)),  
    axis.title.x = element_text(size = 22),  
    axis.title.y = element_text(size = 22),  
    plot.title = element_blank(),   
    panel.border = element_blank(),         
    axis.line = element_line(size = 0.5, color = "black"),  
    axis.line.x = element_line(size = 0.5, color = "black"),
    axis.line.y = element_line(size = 0.5, color = "black")  
  )
