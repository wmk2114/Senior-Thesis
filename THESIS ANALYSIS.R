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

### CLEAN
dep_vars<- colnames(survey)[grepl("_Attack", colnames(survey))]
survey_cleaned<- survey %>%
  mutate(Date= date(EndDate)) %>%
  filter(Date > mdy("02/15/2026")) %>%
  select(-c(1:19)) %>%
  
  ### CLEAN COLUMNS FOR TREATMENTS
  
  mutate(
    
    NFU= case_when(
      is.na(NFU_Confirm) & is.na(NFU_NI_Confirm) ~ 0,
      NFU_Confirm | NFU_NI_Confirm ~ 1
    ),
    
    National_Identity= case_when(
      is.na(Control_NI_Confirm) & is.na(NFU_NI_Confirm) ~ 0,
      Control_NI_Confirm | NFU_NI_Confirm ~ 1
    )) %>%
  
  ### IMPUTATIONS
  ### IMPUTE NA VALUES FOR RESPONDENTS WHO SAY DON'T KNOW
  
  mutate(across(
    .cols = all_of(dep_vars),
    ~ if_else(.x == 98, 4, .x),
    .names = "{.col}_LikertImputed"
  )) %>%
  
  ### IMPUTE VALUES OF 4 FOR RESPONDENTS WHO SAY DON'T KNOW
  
  mutate(across(
    .cols = all_of(dep_vars),
    ~ if_else(.x == 98, NA_real_, .x),
    .names = "{.col}_NAImputed"
  )) %>%
  
  mutate(across(.cols = c("Gender", "Party", "Caste", 
                          "Religion", "Income", "NFU", 
                          "National_Identity"), as.factor))
  
### ADD DICHOTOMIZED VARIABLES
### DICHOTOMIZE FOR COLUMNS WITH IMPUTED NAS

survey_cleaned<- survey_cleaned %>%
  mutate(across(
    .cols = colnames(survey_cleaned)[grepl("_NAImputed",  colnames(survey_cleaned))],
    ~ if_else(is.na(.x), NA, ifelse(.x >= 4, 1, 0)),
    .names = "{.col}_Dichot"
  )) %>%
  
  ### DICHOTOMIZE FOR COLUMNS WITH IMPUTED LIKERT
  mutate(across(
    .cols = colnames(survey_cleaned)[grepl("_LikertImputed",  colnames(survey_cleaned))],
    ~ if_else(is.na(.x), NA, ifelse(.x >= 4, 1, 0)),
    .names = "{.col}_Dichot"
  ))


### STEP 1: BUILD FUNCTIONS
ind_vars<- c("Gender", "Age_1", "Party", "Education", "Religion", "Income", "Caste",
             "NFU", "National_Identity", "NFU:National_Identity")

### OLS MODELS

#### ICW
make_icw_index <- function(data, cols, treat_vars, new_var_name = "icw_index") {
  
  # outcome matrix
  Y <- data %>%
    dplyr::select(dplyr::all_of(cols)) %>%
    as.matrix()
  
  # standardize outcomes
  Y_std <- scale(Y)
  
  # identify pure control group
  control_rows <- apply(data[, treat_vars] == 0, 1, all)
  
  # covariance matrix estimated on controls
  Sigma <- cov(
    Y_std[control_rows, ],
    use = "pairwise.complete.obs"
  )
  
  # inverse covariance
  Sigma_inv <- solve(Sigma)
  
  # weights
  ones <- rep(1, ncol(Y_std))
  weights <- Sigma_inv %*% ones /
    as.numeric(t(ones) %*% Sigma_inv %*% ones)
  
  # construct index
  data[[new_var_name]] <- as.numeric(Y_std %*% weights)
  
  return(data)
}

survey_cleaned <- make_icw_index(
  data = survey_cleaned,
  cols = ols_vars_likert_imputed,
  treat_vars = c("NFU", "National_Identity"),
  new_var_name = "attack_index_imputed_icw"
)

survey_cleaned <- make_icw_index(
  data = survey_cleaned,
  cols = dep_vars,
  treat_vars = c("NFU", "National_Identity"),
  new_var_name = "attack_index_icw"
)

#### PCA
make_pca_index <- function(data, cols, treat_vars, new_var_name = "pca_index") {
  
  # outcome matrix
  Y <- data %>%
    dplyr::select(dplyr::all_of(cols)) %>%
    as.matrix()
  
  # standardize outcomes
  Y_std <- scale(Y)
  
  # identify pure control group
  control_rows <- apply(data[, treat_vars] == 0, 1, all)
  
  # estimate PCA on controls only
  pca_model <- prcomp(Y_std[control_rows, ], center = FALSE, scale. = FALSE)
  
  # extract PC1 loadings
  weights <- pca_model$rotation[,1]
  
  # construct PCA index for full sample
  data[[new_var_name]] <- as.numeric(Y_std %*% weights)
  
  return(data)
}

survey_cleaned <- make_pca_index(
  data = survey_cleaned,
  cols = ols_vars_likert_imputed,
  treat_vars = c("NFU", "National_Identity"),
  new_var_name = "attack_index_imputed_pca"
)

survey_cleaned <- make_pca_index(
  data = survey_cleaned,
  cols = dep_vars,
  treat_vars = c("NFU", "National_Identity"),
  new_var_name = "attack_index_pca"
)

icw_form <- as.formula(paste("attack_index_icw ~", paste(ind_vars, collapse = "+")))
pca_form <- as.formula(paste("attack_index_pca ~", paste(ind_vars, collapse = "+")))
icw_imputed_form <- as.formula(paste("attack_index_imputed_icw ~", paste(ind_vars, collapse = "+")))
pca_imputed_form <- as.formula(paste("attack_index_imputed_pca ~", paste(ind_vars, collapse = "+")))

icw_model <- lm(icw_form, data = survey_cleaned)
pca_model <- lm(pca_form, data = survey_cleaned)
icw_imputed_form <- lm(icw_form, data = survey_cleaned)
pca_imputed_form <- lm(pca_form, data = survey_cleaned)

ols_models<- list(icw_model, icw_imputed_form, pca_model, pca_imputed_form)

### LOGISTIC REGRESSIONS
run_log_reg<-function(dep_var, ind_vars, data){
  rhs<-paste0(ind_vars, collapse = "+")
  lhs_rhs<- paste0(c(dep_var, rhs), collapse = "~")
  form<- as.formula(lhs_rhs)
  log_model<- glm(form, data = data, family = "binomial")
  return(log_model)
}

### RUN LOGISTIC REGRESSION MODELS USING IMPUTED NAs
dep_dichot_na_impute<- colnames(survey_cleaned)[grepl("_NAImputed_Dichot", colnames(survey_cleaned))]

log_regs_na_impute <- lapply(
  dep_dichot_na_impute,
  run_log_reg,
  ind_vars = ind_vars,
  data = survey_cleaned
)

### RUN LOGISTIC REGRESSION MODELS USING LIKERT IMPUTED
dep_dichot_likert_impute<- colnames(survey_cleaned)[grepl("_LikertImputed_Dichot", colnames(survey_cleaned))]
log_regs_likert_impute <- lapply(
  dep_dichot_likert_impute,
  run_log_reg,
  ind_vars = ind_vars,
  data = survey_cleaned
)

### TABULAR OUTPUTS

stargazer(ols_models,
          title = "OLS Models",
          dep.var.labels = c("ICW", "PCA"),
          omit = c("Caste", "Income", "Party", "Religion", "Gender", "Age", "Education"),
          covariate.labels = c("NFU",
                               "Identity Arguments",
                               "NFU x Identity"),
          notes = c("Fixed effects for caste, education, income, party, religion, gender, and age included but not shown",
                    "* p<0.1; ** p<0.05; *** p<0.01"),
          notes.append = FALSE)

stargazer(log_regs_likert_impute,
          title = "Logistic Regression Models, Imputations on Likert Scale",
          dep.var.labels = c("Approve", "Prefer", "Vote", "Protest", "National Identity", "Ahimsa", "Ethical"),
          omit = c("Caste", "Income", "Party", "Religion", "Gender", "Age", "Education"),
          covariate.labels = c("NFU",
                               "Identity Arguments",
                               "NFU x Identity"),
          notes = c("Fixed effects for caste, education, income, party, religion, gender, and age included but not shown",
                    "* p<0.1; ** p<0.05; *** p<0.01"),
          notes.append = FALSE)

stargazer(log_regs_na_impute,
          title = "Logistic Regression Models",
          dep.var.labels = c("Approve", "Prefer", "Vote", "Protest", "National Identity", "Ahimsa", "Ethical"),
          omit = c("Caste", "Income", "Party", "Religion", "Gender", "Age", "Education"),
          covariate.labels = c("NFU",
                               "Identity Arguments",
                               "NFU x Identity"),
          notes = c("Fixed effects for caste, education, income, party, religion, gender, and age included but not shown",
                    "* p<0.1; ** p<0.05; *** p<0.01"),
          notes.append = FALSE)




