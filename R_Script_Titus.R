# Load required libraries
library(dplyr)
library(tidyr)
library(lubridate)
library(stringr)
library(knitr)

# Read the CSV files and check column names
school_sample <- read.csv("school_sample.csv", stringsAsFactors = FALSE)
student_scores <- read.csv("student_scores.csv", stringsAsFactors = FALSE)

# Check for duplicate 
duplicate_schools <- school_sample %>%
  group_by(school_id) %>%
  filter(n() > 1) %>%
  arrange(school_id)

# Clean school_sample 
school_sample_clean <- school_sample %>%
  mutate(
    school_id = as.numeric(school_id),
    school_name = str_trim(school_name),
    school_name = str_replace_all(school_name, "_", " "),
    school_name = str_replace_all(school_name, "\\s+", " "),
    school_name = str_to_title(school_name),
    # Create composite key
    school_key = paste(school_id, school_name)) %>%
  distinct(school_key, .keep_all = TRUE)  

# Clean student_scores - Fixed by removing the $ at the end
student_scores_clean <- student_scores %>%
  mutate(
    # Fix school_id (remove trailing 8 from Uhuru primary's ID)
    school_id = as.numeric(str_remove(as.character(school_id), "8$")),
    # Standardize school names to match school_sample
    school_name = str_trim(school_name),
    school_name = str_replace_all(school_name, "_", " "),
    school_name = str_replace_all(school_name, "\\s+", " "),
    school_name = str_to_title(school_name),
    # Create matching composite key
    school_key = paste(school_id, school_name),
    # cleaning gender and date
    gender = str_to_upper(gender),
    subsidy_start_date = as.Date(subsidy_start_date, format = "%d-%b-%Y"),
    subsidy_start_date = as.character(subsidy_start_date) %>%
      str_replace("2424", "2024") %>%
      str_replace("2323", "2023") %>%
      as.Date(format = "%Y-%m-%d"))


# Join datasets 

combined_data <- student_scores_clean %>%
  left_join(school_sample_clean, by = "school_key")

# Check for any unmatched records

unmatched <- student_scores_clean %>%
  anti_join(school_sample_clean, by = "school_key")


## Data Quality Issues Report

# Missing Treatment/Control assignments

missing_treatment <- combined_data %>%
  filter(is.na(study_arm)) %>%
  select(school_key, district.x)

# Score range validation

score_summary <- combined_data %>%
  summarise(
    min_math = min(eg_math_score),
    max_math = max(eg_math_score),
    min_reading = min(eg_reading_score),
    max_reading = max(eg_reading_score))

# Age distribution by grade

age_grade_summary <- combined_data %>%
  group_by(student_grade) %>%
  summarise(
    min_age = min(age),
    max_age = max(age),
    mean_age = mean(age),
    n = n())

# Check for inconsistencies in district information

district_mismatch <- combined_data %>%
  filter(district.x != district.y) %>%
  select(school_key, district.x, district.y)

# Check survey duration outliers

duration_summary <- combined_data %>%
  summarise(
    min_duration = min(survey_duration),
    max_duration = max(survey_duration),
    mean_duration = mean(survey_duration),
    sd_duration = sd(survey_duration))

# Calculate summary statistics by treatment group

balance_table <- combined_data %>%
  group_by(study_arm) %>%
  summarise(
    sample_size = n(),
    mean_age = round(mean(age, na.rm = TRUE), 2),
    sd_age = round(sd(age, na.rm = TRUE), 2),
    mean_math = round(mean(eg_math_score, na.rm = TRUE), 2),
    sd_math = round(sd(eg_math_score, na.rm = TRUE), 2),
    mean_reading = round(mean(eg_reading_score, na.rm = TRUE), 2),
    sd_reading = round(sd(eg_reading_score, na.rm = TRUE), 2),
    pct_female = round(mean(gender == "FEMALE", na.rm = TRUE) * 100, 1)) %>%
  mutate(
    age_stats = paste0(mean_age, " (", sd_age, ")"),
    math_stats = paste0(mean_math, " (", sd_math, ")"),
    reading_stats = paste0(mean_reading, " (", sd_reading, ")")) %>%
  select(study_arm, sample_size, age_stats, math_stats, reading_stats, pct_female)

# Calculate overall statistics

overall_stats <- combined_data %>%
  summarise(
    study_arm = "Overall",
    sample_size = n(),
    mean_age = round(mean(age, na.rm = TRUE), 2),
    sd_age = round(sd(age, na.rm = TRUE), 2),
    mean_math = round(mean(eg_math_score, na.rm = TRUE), 2),
    sd_math = round(sd(eg_math_score, na.rm = TRUE), 2),
    mean_reading = round(mean(eg_reading_score, na.rm = TRUE), 2),
    sd_reading = round(sd(eg_reading_score, na.rm = TRUE), 2),
    pct_female = round(mean(gender == "FEMALE", na.rm = TRUE) * 100, 1)) %>%
  mutate(
    age_stats = paste0(mean_age, " (", sd_age, ")"),
    math_stats = paste0(mean_math, " (", sd_math, ")"),
    reading_stats = paste0(mean_reading, " (", sd_reading, ")")) %>%
  select(study_arm, sample_size, age_stats, math_stats, reading_stats, pct_female)

# Combine and format final table
final_balance_table <- bind_rows(balance_table, overall_stats)

# Statistical Tests for Balance 

balance_tests <- list(
  age_test = t.test(age ~ study_arm, data = combined_data),
  math_test = t.test(eg_math_score ~ study_arm, data = combined_data),
  reading_test = t.test(eg_reading_score ~ study_arm, data = combined_data))

# Chi-square test for gender

gender_test <- chisq.test(table(combined_data$study_arm, combined_data$gender))

# Create statistical test summary
test_summary <- data.frame(
  Variable = c("Age", "Math Score", "Reading Score", "Gender"),
  t_stat = c(
    balance_tests$age_test$statistic,
    balance_tests$math_test$statistic,
    balance_tests$reading_test$statistic,
    NA
  ),
  p_value = c(
    balance_tests$age_test$p.value,
    balance_tests$math_test$p.value,
    balance_tests$reading_test$p.value,
    gender_test$p.value)) %>%
  mutate(
    p_value = round(p_value, 3),
    t_stat = round(t_stat, 3))

## Attrition Analysis 

# Calculate attrition rates by treatment status
attrition_analysis <- combined_data %>%
  group_by(study_arm) %>%
  summarise(
    total_students = n(),
    missing_math = sum(is.na(eg_math_score)),
    missing_reading = sum(is.na(eg_reading_score)),
    attrition_rate_math = round(missing_math / total_students * 100, 1),
    attrition_rate_reading = round(missing_reading / total_students * 100, 1))

# Print formatted tables using kable
cat("\nTreatment-Control Balance Analysis\n")
print(kable(final_balance_table,
            col.names = c("Group", "Sample Size", "Age Mean (SD)", 
                          "Math Score Mean (SD)", "Reading Score Mean (SD)", 
                          "Female %"),
            caption = "Treatment-Control Balance Analysis"))

cat("\nStatistical Tests for Balance\n")
print(kable(test_summary,
            col.names = c("Variable", "T-statistic", "P-value"),
            caption = "Statistical Tests for Treatment-Control Balance"))

cat("\nAttrition Analysis\n")
print(kable(attrition_analysis,
            caption = "Attrition Analysis by Treatment Status"))


# Treatment and Control Comparison Table 
comparison_table <- combined_data %>%
  group_by(study_arm) %>%
  summarise(
    `Sample Size` = n(),
    `Mean Age` = round(mean(age, na.rm = TRUE), 2),
    `Age SD` = round(sd(age, na.rm = TRUE), 2),
    `Mean Math Score` = round(mean(eg_math_score, na.rm = TRUE), 2),
    `Math SD` = round(sd(eg_math_score, na.rm = TRUE), 2),
    `Mean Reading Score` = round(mean(eg_reading_score, na.rm = TRUE), 2),
    `Reading SD` = round(sd(eg_reading_score, na.rm = TRUE), 2),
    `Male Ratio` = round(mean(gender == "Male", na.rm = TRUE) * 100, 1),
    `Female Ratio` = round(mean(gender == "FEMALE", na.rm = TRUE) * 100, 1)
  ) %>%
  mutate(
    `Female Ratio` = paste0(`Female Ratio`, "%"))

# Statistical tests
t_test_age <- t.test(age ~ study_arm, data = combined_data)
t_test_math <- t.test(eg_math_score ~ study_arm, data = combined_data)
t_test_reading <- t.test(eg_reading_score ~ study_arm, data = combined_data)