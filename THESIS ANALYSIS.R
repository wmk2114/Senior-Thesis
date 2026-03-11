################################################################################
### DATA ANALYSIS FOR WE DON'T DO THAT: NATIONAL IDENTITY AND NUCLEAR NON-USE
### SUBMISSION FOR DEPARTMENTAL HONORS
### COLUMBIA UNIVERSITY DEPARTMENT OF POLITICAL SCIENCE
### AUTHOR: WYATT KING
################################################################################

################################################################################
# DISCLAIMER
# ChatGPT was used to generate pseudocode for this analysis.
# The pseudocode and documentation were subsequently edited
# and reviewed by the author, Wyatt King.
# All code was initially written by Wyatt, with occasional syntax issues being
# fixed by ChatGPT.
################################################################################

################################################################################
# STEP 0: ENVIRONMENT SETUP
#
# Pseudocode:
#
# 1. Clear the R workspace to remove previously stored objects.
# 2. Load required R packages for data manipulation, statistical modeling,
#    and regression table generation.
# 3. Set the working directory to the project folder containing the dataset.
# 4. Import the survey dataset from a .sav file.
################################################################################

rm(list = ls())
library(tidyverse)
library(haven)
library(stargazer)
library(glmnet)

setwd("~/Desktop/Columbia 2025/Senior Thesis/Data Analysis")
survey<- read_sav("SIMULATED DATA.sav")


################################################################################
# STEP 1: DATA CLEANING
#
# Pseudocode:
#
# 1. Identify all dependent variables related to nuclear attack approval by
#    selecting variables containing the string "_Attack".
#
# 2. Clean the dataset by performing the following operations:
#       a. Convert the survey completion date to a standard date format.
#       b. Remove responses submitted before March 11, 2026 (indicating simulated data).
#       c. Remove metadata columns that are not relevant to the analysis.
#
# 3. Construct treatment indicators:
#       - NFU treatment
#       - National Identity treatment
#
# 4. Handle "Don't Know" responses (coded as 98) in two ways:
#       a. Replace with the midpoint of the Likert scale (4).
#       b. Replace with NA (missing).
#
# 5. Convert relevant demographic and treatment variables to factors.
################################################################################

dep_vars<- colnames(survey)[grepl("_Attack", colnames(survey))]
survey_cleaned<- survey %>%
  
  # REMOVE EXTRANEOUS COLUMNS
  
  mutate(Date= date(EndDate)) %>%
  filter(Date > mdy("02/15/2026")) %>%
  select(-c(1:19)) %>%
  
  # FORMAT TREATMENT COLUMNS
  
  mutate(
    
    NFU= case_when(
      is.na(NFU_Confirm) & is.na(NFU_NI_Confirm) ~ 0,
      NFU_Confirm | NFU_NI_Confirm ~ 1
    ),
    
    National_Identity= case_when(
      is.na(Control_NI_Confirm) & is.na(NFU_NI_Confirm) ~ 0,
      Control_NI_Confirm | NFU_NI_Confirm ~ 1
    )) %>%
  
  # CREATE NEW COLUMNS, IMPUTING VALUES OF 4 FOR RESPONDENTS WHO SAY DON'T KNOW
  
  mutate(across(
    .cols = all_of(dep_vars),
    ~ if_else(.x == 98, 4, .x),
    .names = "{.col}_LikertImputed"
  )) %>%
  
  # CREATE NEW COLUMNS, IMPUTING NA VALUES FOR RESPONDENTS WHO SAY DON'T KNOW
  
  mutate(across(
    .cols = all_of(dep_vars),
    ~ if_else(.x == 98, NA_real_, .x),
    .names = "{.col}_NAImputed"
  )) %>%
  
  # CONVERT RELEVANT COLUMNS TO FACTORS
  
  mutate(across(.cols = c("Gender", "Party", "Caste", 
                          "Religion", "Income", "NFU", 
                          "National_Identity"), as.factor))


################################################################################
# STEP 2: CREATE DICHOTOMIZED VARIABLES
#
# Pseudocode:
#
# 1. Generate binary outcome variables from Likert-scale responses.
#
# 2. For NA-imputed variables:
#       - Keep NA values unchanged.
#       - Assign 1 if response >= 4 (opposition or indifference to nuclear use).
#       - Assign 0 if response < 4 (support for nuclear use).
#
# 3. Repeat the same dichotomization procedure for midpoint-imputed variables.
################################################################################

survey_cleaned<- survey_cleaned %>%
  
  ### DICHOTOMIZE FOR COLUMNS WITH IMPUTED NAS
  mutate(across(
    .cols = colnames(survey_cleaned)[grepl("_NAImputed",  colnames(survey_cleaned))],
    ~ if_else(is.na(.x), NA, if_else(.x >= 4, 1, 0)),
    .names = "{.col}_Dichot"
  )) %>%
  
  ### DICHOTOMIZE FOR COLUMNS WITH IMPUTED LIKERT
  mutate(across(
    .cols = colnames(survey_cleaned)[grepl("_LikertImputed",  colnames(survey_cleaned))],
    ~ if_else(is.na(.x), NA, if_else(.x >= 4, 1, 0)),
    .names = "{.col}_Dichot"
  ))


################################################################################
# STEP 3: CONSTRUCT OUTCOME INDICES
#
# Two index construction methods are used:
#
# 1. Inverse Covariance Weighting (ICW)
# 2. Principal Components Analysis (PCA)
#
# Both indices are estimated using only respondents in the pure control group,
# defined as observations where:
#       NFU = 0
#       National_Identity = 0
################################################################################


################################################################################
# ICW INDEX CONSTRUCTION
################################################################################

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

ols_vars_likert_imputed<- colnames(survey_cleaned)[grepl("_LikertImputed", 
                                                         colnames(survey_cleaned)) 
                                                   & !grepl("_Dichot", colnames(survey_cleaned))]

ols_vars_na_imputed<- colnames(survey_cleaned)[grepl("_NAImputed", 
                                                         colnames(survey_cleaned)) 
                                                   & !grepl("_Dichot", colnames(survey_cleaned))]

survey_cleaned <- make_icw_index(
  data = survey_cleaned,
  cols = ols_vars_likert_imputed,
  treat_vars = c("NFU", "National_Identity"),
  new_var_name = "attack_index_likert_icw"
)

survey_cleaned <- make_icw_index(
  data = survey_cleaned,
  cols = ols_vars_na_imputed,
  treat_vars = c("NFU", "National_Identity"),
  new_var_name = "attack_index_na_icw"
)


################################################################################
# PCA INDEX CONSTRUCTION
################################################################################

make_pca_index <- function(data, cols, treat_vars, new_var_name = "pca_index") {
  
  # outcome matrix
  Y <- data %>%
    dplyr::select(dplyr::all_of(cols)) %>%
    as.matrix()
  
  # standardize outcomes
  Y_std <- scale(Y)
  
  # identify pure control group
  control_rows <- apply(data[, treat_vars] == 0, 1, all)
  
  control_complete <- control_rows & complete.cases(Y_std)
  
  # estimate PCA on controls only
  pca_model <- prcomp(Y_std[control_complete, ], center = FALSE, scale. = FALSE)
  
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
  new_var_name = "attack_index_likert_pca"
)

survey_cleaned <- make_pca_index(
  data = survey_cleaned,
  cols = ols_vars_na_imputed,
  treat_vars = c("NFU", "National_Identity"),
  new_var_name = "attack_index_na_pca"
)


################################################################################
# STEP 4: OLS REGRESSION MODELS
#
# Pseudocode:
#
# 1. Define a set of demographic and treatment covariates.
# 2. Construct regression formulas for ICW and PCA outcome indices.
# 3. Estimate four OLS models:
#       - ICW index
#       - PCA index
#       - ICW index with Likert imputation
#       - PCA index with Likert imputation
################################################################################

ind_vars<- c("Gender", "Age_1", "Party", "Education", "Religion", "Income", "Caste",
             "NFU", "National_Identity", "NFU:National_Identity")

icw_likert_form <- as.formula(paste("attack_index_likert_icw ~", paste(ind_vars, collapse = "+")))
pca_likert_form <- as.formula(paste("attack_index_likert_pca ~", paste(ind_vars, collapse = "+")))
icw_na_form <- as.formula(paste("attack_index_na_icw ~", paste(ind_vars, collapse = "+")))
pca_na_form <- as.formula(paste("attack_index_na_pca ~", paste(ind_vars, collapse = "+")))

icw_likert_model <- lm(icw_likert_form, data = survey_cleaned)
pca_likert_model <- lm(pca_likert_form, data = survey_cleaned)
icw_na_model <- lm(icw_na_form, data = survey_cleaned)
pca_na_model <- lm(pca_na_form, data = survey_cleaned)

ols_models<- list(icw_likert_model, icw_na_model, pca_likert_model, pca_na_model)


################################################################################
# STEP 5: LOGISTIC REGRESSION MODELS
#
# Pseudocode:
#
# 1. Define a helper function that constructs and estimates logistic
#    regression models.
#
# 2. Run logistic models for dichotomized outcomes with NA imputation.
#
# 3. Run logistic models for dichotomized outcomes with Likert midpoint
#    imputation.
################################################################################

run_log_reg<-function(dep_var, ind_vars, data){
  rhs<-paste0(ind_vars, collapse = "+")
  lhs_rhs<- paste0(c(dep_var, rhs), collapse = "~")
  form<- as.formula(lhs_rhs)
  log_model<- glm(form, data = data, family = "binomial")
  return(log_model)
}

dep_dichot_na_impute<- colnames(survey_cleaned)[
  grepl("_NAImputed_Dichot", colnames(survey_cleaned))
  ]

log_regs_na_impute <- lapply(
  dep_dichot_na_impute,
  run_log_reg,
  ind_vars = ind_vars,
  data = survey_cleaned
)

dep_dichot_likert_impute<- colnames(survey_cleaned)[
  grepl("_LikertImputed_Dichot", colnames(survey_cleaned))
  ]

log_regs_likert_impute <- lapply(
  dep_dichot_likert_impute,
  run_log_reg,
  ind_vars = ind_vars,
  data = survey_cleaned
)


################################################################################
# STEP 6: GENERATE REGRESSION TABLES
################################################################################

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


################################################################################
# APPENDIX: SUPPLEMENTARY ANALYSIS
################################################################################

### STEP 7: SUPPLEMENTARY ANALYSIS — LIKERT THRESHOLD DICHOTOMIZATIONS

thresholds <- 1:6
for (t in thresholds) {
  survey_cleaned <- survey_cleaned %>%
    
    mutate(across(
      .cols = all_of(ols_vars_likert_imputed),
      ~ if_else(is.na(.x), NA, ifelse(.x <= t, 0, 1)),
      .names = paste0("{.col}_Dichot_", t)
    )) %>%
    
    mutate(across(
      .cols = all_of(ols_vars_na_imputed),
      ~ if_else(is.na(.x), NA, ifelse(.x <= t, 0, 1)),
      .names = paste0("{.col}_Dichot_", t)
    ))
}

dep_likert_dichot_thresholds <- colnames(survey_cleaned)[
  grepl("LikertImputed_Dichot_[1-6]$", colnames(survey_cleaned))
  ]

log_regs_likert_thresholds <- lapply(
  dep_likert_dichot_thresholds,
  run_log_reg,
  ind_vars = ind_vars,
  data = survey_cleaned
)

model_likert_order<-unlist(lapply(log_regs_likert_thresholds, function(m) colnames(m$model)[1]))
outcome_likert_names <- unique(sub("_Dichot_[1-6]$", "", model_likert_order))

threshold_models_by_likert_outcome <- lapply(outcome_likert_names, function(outcome){
  matches <- grepl(outcome, model_likert_order)
  log_regs_likert_thresholds[matches]
})

names(threshold_models_by_likert_outcome) <- outcome_likert_names

### REPEAT FOR REGRESSIONS WHERE WE INCLUDE NAs

dep_na_dichot_thresholds <- colnames(survey_cleaned)[
  grepl("NAImputed_Dichot_[1-6]$", colnames(survey_cleaned))
]

log_regs_na_thresholds <- lapply(
  dep_na_dichot_thresholds,
  run_log_reg,
  ind_vars = ind_vars,
  data = survey_cleaned
)

model_na_order<-unlist(lapply(log_regs_na_thresholds, function(m) colnames(m$model)[1]))
outcome_na_names <- unique(sub("_Dichot_[1-6]$", "", model_na_order))

threshold_models_by_na_outcome <- lapply(outcome_na_names, function(outcome){
  matches <- grepl(outcome, model_na_order)
  log_regs_likert_thresholds[matches]
})

names(threshold_models_by_na_outcome) <- outcome_na_names

### STARGAZER TABLES
base_outcomes <- sub("_Dichot_[1-6]$", "", model_order)

outcome_labels <- c(
  "Approve_Attack" = "Approve Nuclear Use",
  "Prefer_Attack" = "Prefer Nuclear Use",
  "Vote_Attack" = "Vote for Nuclear Use",
  "Protest_Attack" = "Protest Against Nuclear Use",
  "National_Identity_Attack" = "Nuclear Use Consistent with National Identity",
  "Ahimsa_Attack" = "Nuclear Use Violates Ahimsa",
  "Ethical_Attack" = "Nuclear Use is Ethical"
)

dep_labels <- outcome_labels[base_outcomes]

test<-lapply(names(threshold_models_by_likert_outcome), function(outcome){
  
  stargazer(
    threshold_models_by_likert_outcome[[outcome]],
    title = paste("Logistic Models Across Likert Thresholds:", outcome_labels[outcome]),
    dep.var.labels = outcome_labels[outcome],
    omit = c("Caste", "Income", "Party", "Religion", "Gender", "Age", "Education"),
    covariate.labels = c("NFU",
                         "Identity Arguments",
                         "NFU x Identity"),
    column.labels = c(">1", ">2", ">3", ">4", ">5", ">6")
  )
  
})

test[[1]]

lapply(names(threshold_models_by_na_outcome), function(outcome){
  
  stargazer(
    threshold_models_by_na_outcome[[outcome]],
    title = paste("Logistic Models Across Likert Thresholds:", outcome),
    omit = c("Caste", "Income", "Party", "Religion", "Gender", "Age", "Education"),
    covariate.labels = c("NFU",
                         "Identity Arguments",
                         "NFU x Identity")
  )
  
})




