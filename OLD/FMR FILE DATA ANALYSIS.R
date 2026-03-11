### DATA ANALYSIS
### WE DON'T DO THAT: NATIONAL IDENTITY AND NUCLEAR NON-USE
### SUBMISSION FOR DEPARTMENTAL HONORS
### COLUMBIA UNIVERSITY DEPARTMENT OF POLITICAL SCIENCE
### AUTHOR: WYATT KING

### STEP 0:  SET UP
rm(list = ls())
library(tidyverse)
library(haven)
library(stargazer)
library(glmnet)

setwd("/Users/wyatttheking/Desktop/Columbia 2025/Senior Thesis/Data Analysis")
survey<- read_sav("Test Responses.sav")

### STEP 1: DATA CLEANING
survey_cleaned<- survey %>%
  mutate(Date= date(EndDate)) %>%
  filter(Date > mdy("02/15/2026")) %>%
  select(-c(1:19)) %>%
  
  mutate(
    
  NFU= case_when(
    is.na(NFU_Confirm) & is.na(NFU_NI_Confirm) ~ 0,
    NFU_Confirm | NFU_NI_Confirm ~ 1
  ),
  
  National_Identity= case_when(
    is.na(Control_NI_Confirm) & is.na(NFU_NI_Confirm) ~ 0,
    Control_NI_Confirm | NFU_NI_Confirm ~ 1
  ),
  
  Approve_Bin= case_when(
    is.na(Approve_Attack) ~ NA,
    Approve_Attack >= 5 ~ 1,
    Approve_Attack < 5 ~ 0
  ),
  
  Approve_Tri= case_when(
    is.na(Approve_Attack) ~ NA,
    Approve_Attack >= 5 ~ 2,
    Approve_Attack == 4 ~ 1,
    Approve_Attack < 4 ~ 0
  ),
  
  Prefer_Bin= case_when(
    is.na(Prefer_Attack) ~ NA,
    Prefer_Attack > 6 ~ 1,
    Prefer_Attack <= 6 ~ 0
    
  ),
  
  Vote_Bin= case_when(
    is.na(Vote_Attack) ~ NA,
    Vote_Attack > 4 ~ 1,
    Vote_Attack <= 4 ~ 0
    
  ),
  
  Vote_Tri= case_when(
    is.na(Vote_Attack) ~ NA,
    Vote_Attack > 4 ~ 2,
    Vote_Attack == 4 ~ 1,
    Vote_Attack < 4 ~ 0
  ),
  
  Protest_Bin= case_when(
    is.na(Protest_Attack) ~ NA,
    Protest_Attack > 4 ~ 1,
    Protest_Attack <= 4 ~ 0
  ),
  
  Protest_Tri= case_when(
    is.na(Protest_Attack) ~ NA,
    Protest_Attack > 4 ~ 2,
    Protest_Attack == 4 ~ 1,
    Protest_Attack < 4 ~ 0
  ),
  
  Ethical_Bin= case_when(
    is.na(Ethical_Attack) ~ NA,
    Ethical_Attack > 4 ~ 1,
    Ethical_Attack <= 4 ~ 0
  ),
  
  Ethical_Tri= case_when(
    is.na(Ethical_Attack) ~ NA,
    Ethical_Attack > 4 ~ 2,
    Ethical_Attack == 4 ~ 1,
    Ethical_Attack < 4 ~ 0
  ),
  
  Ahimsa_Bin= case_when(
    is.na(Ahimsa_Attack) ~ NA,
    Ahimsa_Attack > 4 ~ 1,
    Ahimsa_Attack <= 4 ~ 0
  ),
  
  Ahimsa_Tri= case_when(
    is.na(Ahimsa_Attack) ~ NA,
    Ahimsa_Attack > 4 ~ 2,
    Ahimsa_Attack == 4 ~ 1,
    Ahimsa_Attack < 4 ~ 0
  ),
  
  NationalID_Bin= case_when(
    is.na(NationalID_Attack) ~ NA,
    NationalID_Attack > 4 ~ 1,
    NationalID_Attack <= 4 ~ 0
  ),
  
  NationalID_Tri= case_when(
    is.na(NationalID_Attack) ~ NA,
    NationalID_Attack > 4 ~ 2,
    NationalID_Attack == 4 ~ 1,
    NationalID_Attack < 4 ~ 0
  )
  ) %>%
  mutate(across(.cols = c("Gender", "Party", "Caste", 
                          "Religion", "Income", "NFU", 
                          "National_Identity"), as.factor))

### DATA VISUALIZATIONS

survey_cleaned %>%
  select(ends_with("Tri")) %>%
  pivot_longer(cols = everything(),
               names_to = "variable",
               values_to = "value") %>%
  count(variable, value, sort = TRUE) %>%
  ggplot(aes(x = variable, y = n, fill = factor(value))) +
  geom_bar(stat = "identity", position = "stack") +
  labs(
    x = "Variable",
    y = "Count",
    fill = "Level"
  ) +
  scale_x_discrete(labels = c(
    "Ahimsa_Tri" = "Ahimsa",
    "Approve_Tri" = "Approve",
    "Ethical_Tri" = "Ethical",
    "NationalID_Tri" = "National ID",
    "Protest_Tri" = "Protest",
    "Vote_Tri" = "Vote"
  )) +
  scale_fill_brewer(palette = "Blues")+
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

### STEP 1.1: INVERSE COVARIANCE WEIGHTING

y_vars<- survey_cleaned[, c("Approve_Attack", "Prefer_Attack", "Vote_Attack", 
                            "Protest_Attack", "Ahimsa_Attack", "Ethical_Attack")]

inverse_cov_index <- function(data) {
  Z <- scale(data)
  Sigma <- cov(Z, use = "pairwise.complete.obs")
  Sigma_inv <- solve(Sigma)
  ones <- rep(1, ncol(Z))
  w <- Sigma_inv %*% ones
  w <- w / sum(w)
  as.numeric(Z %*% w)
}

survey_cleaned$index<- inverse_cov_index(y_vars)

### OLS MODEL

m1<- lm(index~ NFU*National_Identity+Gender+Party+Caste+Religion+Income, data=survey_cleaned)
summary(m1)

## STEP 2: DATA ANALYSIS
library(VGAM)

### SPECIFY VARIABLE COMBINATIONS
vars1<- c("Approve_Attack", "Approve_Bin", "NFU", 
          "National_Identity", "Income", "Religion", 
          "Gender", "Age_1", "Party", "Caste")

vars2<- c("Prefer_Attack", "Prefer_Bin", "NFU", 
          "National_Identity", "Income", "Religion", 
          "Gender", "Age_1", "Party", "Caste")

vars3<- c("Vote_Attack", "Vote_Bin", "NFU", 
          "National_Identity", "Income", "Religion", 
          "Gender", "Age_1", "Party", "Caste")

vars4<- c("Protest_Attack", "Protest_Bin", "NFU", 
          "National_Identity", "Income", "Religion", 
          "Gender", "Age_1", "Party", "Caste")

vars5<- c("Ethical_Attack", "Ethical_Bin", "NFU", 
          "National_Identity", "Income", "Religion", 
          "Gender", "Age_1", "Party", "Caste")

vars6<- c("Ahimsa_Attack", "Ahimsa_Bin", "NFU", 
          "National_Identity", "Income", "Religion", 
          "Gender", "Age_1", "Party", "Caste")

vars7<- c("NationalID_Attack", "NationalID_Bin", "NFU", 
          "National_Identity", "Income", "Religion", 
          "Gender", "Age_1", "Party", "Caste")

vars_total<- list(vars1, vars2, vars3, vars4, vars5, vars6, vars7)

### CREATE FUNCTION TO FIT LOGISTIC AND ORDERED LOGISTIC REGRESSION MODELS
func_models<-function(x){
  find_bin<- grepl("_Bin", x)
  find_multi<- grepl("_Attack", x)
  
  bin_form<- paste0(x[find_bin], " ~ ", paste0(x[c(find_bin==find_multi)], collapse=" + "), " + NFU:National_Identity")
  bin_form<- as.formula(bin_form)
  
  multi_form<- paste0(x[find_multi], " ~ ", paste0(x[c(find_bin==fxind_multi)], collapse=" + "), " + NFU:National_Identity")
  multi_form<- as.formula(multi_form)
  
  m_log<- glm(bin_form, family = "binomial", data=survey_cleaned)
  m_multi_log<- vglm(multi_form, family = multinomial, data=survey_cleaned)
  
  model_results<- list(m_log, m_multi_log)
    
  return(model_results)
}

model_outputs<-lapply(vars_total, func_models)
models_log <- lapply(model_outputs, `[[`, 1)
models_multi <- lapply(model_outputs, `[[`, 2)

### GENERATE LATEX OUTPUTS
stargazer(models_log,
          title = "Logistic Regression Models",
          dep.var.labels = c("Approve", "Prefer", "Vote", "Protest", "Ethical", "Ahimsa", "National Identity"),
          covariate.labels = c("NFU",
                               "Identity Arguments",
                               "NFU x Identity"),
          omit = c("Caste", "Income", "Party", "Religion", "Gender", "Age"),
          notes = c("Fixed effects for caste, income, party, religion, gender, and age included but not shown",
                    "* p<0.1; ** p<0.05; *** p<0.01"),
          notes.append = FALSE)

stargazer(models_multi,
          title = "Multinomial Logistic Regression Models",
          dep.var.labels = c("Approve", "Prefer", "Vote", "Protest", "Ethical", "Ahimsa", "National Identity"),
          covariate.labels = c("NFU",
                               "Identity Arguments",
                               "NFU x Identity"),
          omit = c("Caste", "Income", "Party", "Religion", "Gender", "Age"),
          notes = c("Fixed effects for caste, income, party, religion, gender, and age included but not shown",
                    "* p<0.1; ** p<0.05; *** p<0.01"),
          notes.append = FALSE)

### 2A: DIFFERENCE OF PROPORTIONS



### 2C: VISUALIZATIONS
stargazer(m1_log, m2_log, m3_log, m4_log, m5_log, 
          keep = c("National_Identity", "NFU", "National_Identity:NFU", "Party"),
          notes = c("Fixed effects included but not shown for gender, age, caste, religion, and income."))

### ORDINAL REGRESSION




