# Source data - all NHANES; source code - Aline
# Analysis - Gas cooking and adult asthma outcomes (still have asthma, exacerbations, ER visits)

setwd("C:/Users/hm2943/OneDrive - Hennepin Healthcare/Documents/Research/NHANES")

library(readr)
library(dplyr)
library(tidyr)
library(missForest)
library(tibble)
library(survey)

merged_nhanes <- read_csv("merged_nhanes.csv")

### -------------------------------------------
# Step 1: Define Variables for Imputation
### -------------------------------------------
vars_to_impute_adults <- c(
  "RIDAGEYR",  # Age
  "RIAGENDR",  # Sex
  "RIDRETH3",  # Race/ethnicity
  "INDFMPIR",  # Income
  "HOQ065"     # Housing
)

id_vars <- c("SEQN", "SDMVPSU", "SDMVSTRA", "WTMEC2YR", "WTSVOC2Y", "YEAR")
vars_to_keep_but_not_impute <- c("MCQ035", "MCQ040", "MCQ050", "VTQ241A")

### -------------------------------------------
# Step 2: Filter to Adults (18+) WITH Current Asthma 
### -------------------------------------------
subset_adults_for_impute <- merged_nhanes %>%
  filter(RIDAGEYR >= 18) %>%
  select(all_of(c(id_vars, vars_to_impute_adults, vars_to_keep_but_not_impute)))

### -------------------------------------------
# Step 3: Run the missForest imputation
### -------------------------------------------
# missForest imputation
rownames(subset_adults_for_impute) <- subset_adults_for_impute$SEQN
data_to_impute <- subset_adults_for_impute[, vars_to_impute_adults]
data_to_impute <- as.data.frame(data_to_impute)
data_to_impute$HOQ065 <- as.factor(data_to_impute$HOQ065)
# Set HOQ065 as factor - avoids imputing as numerical; INDFMPIR is continuous initially

imputation_model_rf <- missForest(data_to_impute,verbose = T) # verbose will print out the errors and show you where the imputation stops

imputed_df <- imputation_model_rf$ximp
imputed_df <- rownames_to_column(imputed_df, var = "SEQN")

# Check for any remaining missing values
colSums(is.na(imputed_df))

# bring back the rest of the variables
other_variables <- merged_nhanes %>% select(!vars_to_impute_adults)
imputed_df$SEQN <- subset_adults_for_impute$SEQN
nhanes_imputed <- merge(other_variables, imputed_df, by = "SEQN")


### ------------------------------------------
# Re-code Variables
### ------------------------------------------
nhanes_imputed <- nhanes_imputed %>%
  mutate(

    # Outcome: Still have asthma? NHANES 1 = Yes, 2 = No -> 0 = No, 1 = Yes
    MCQ035_bin = case_when(
      MCQ035 == 1 ~ 1,
      MCQ035 == 2 ~ 0,
      TRUE ~ NA_real_
    ),
    
    # Asthma exacerbation? NHANES 1 = Yes, 2 = No -> 0 = No, 1 = Yes
    MCQ040_bin = case_when(
      MCQ040 == 1 ~ 1,
      MCQ040 == 2 ~ 0,
      TRUE ~ NA_real_
    ),
    
    # ER visit due to asthma? NHANES 1 = Yes, 2 = No -> 0 = No, 1 = Yes
    MCQ050_bin = case_when(
      MCQ050 == 1 ~ 1,
      MCQ050 == 2 ~ 0,
      TRUE ~ NA_real_
    ),
    
    # Gas stove use (exposure) NHANES 1 = Yes, 2 = No -> 0 = No, 1 = Yes
    VTQ241A_bin = case_when(
      VTQ241A == 1 ~ 1,
      VTQ241A == 2 ~ 0,
      TRUE ~ NA_real_
    ),
    
    # PIR-based income category: Low, Medium, High
    income_cat = case_when(
      INDFMPIR < 1.8 ~ "Low",
      INDFMPIR >= 1.8 & INDFMPIR < 3.9 ~ "Medium",
      INDFMPIR >= 3.9 & INDFMPIR <= 5 ~ "High",
      TRUE ~ NA_character_
    ),
    
    #Housing (owned vs rented)
    housing_status = case_when(
      HOQ065 == 1 ~ "Owned",
      HOQ065 == 2 ~ "Rented",
      TRUE ~ NA_character_
    )
  )

# Recode variables as factors - two levels
nhanes_imputed <- nhanes_imputed %>% mutate_at(c("MCQ035_bin", "MCQ040_bin", "MCQ050_bin","VTQ241A_bin","housing_status", "RIAGENDR"), as.factor)
# Recode variables as ordered factors
nhanes_imputed$income_cat <- factor(nhanes_imputed$income_cat, levels = c("Low", "Medium", "High"))
# Check order of levels
levels(nhanes_imputed$income_cat)

### ------------------------------------------------
# Weighting and model
### ------------------------------------------------

# Step 1: Create 6-year VOC weight
nhanes_imputed <- nhanes_imputed %>%
  mutate(WTSVOC6YR = WTSVOC2Y / 3)

# Step 2: Subset to analysis of interest - NAs will be excluded by model
adults_asthma <- nhanes_imputed %>%
  filter(!is.na(WTSVOC6YR), WTSVOC6YR != 0, housing_status == "Owned")

# Step 3: Define survey design
nhanes_svy_design <- svydesign(
  ids = ~SDMVPSU,
  strata = ~SDMVSTRA,
  weights = ~WTSVOC6YR,
  nest = TRUE,
  data = adults_asthma
)

# Step 4: Run model (outcome ~ VTQ241A + gender + inc + rental)
adults_asthma_model <- svyglm(
  MCQ050_bin ~ VTQ241A_bin + RIAGENDR + income_cat,
  design = nhanes_svy_design,
  family = quasibinomial(link = "logit")
)

summary(adults_asthma_model)
