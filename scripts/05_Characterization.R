# ==============================================================================
# SCRIPT 05: TABLES 1, 2 & 3 (FINAL POLISHED VERSION)
# ==============================================================================

library(tidyverse)
library(gtsummary)
library(flextable)

# 1. SETUP & LOAD
output_dir <- "Paper 1/df_analysis/05_tables/"
if(!dir.exists(output_dir)) dir.create(output_dir, recursive = TRUE)

df_analysis <- read_csv("Paper 1/df_analysis/03_clean_data/clean_analysis_data.csv", 
                        show_col_types = FALSE)

# ==============================================================================
# 2. DATA WRANGLING
# ==============================================================================

df_table <- df_analysis %>%
  mutate(
    # A. standardize Group Names
    Group = case_when(
      str_detect(Group, "(?i)trainer") ~ "Coach", 
      str_detect(Group, "(?i)athlete") ~ "WR Athlete",
      TRUE ~ Group
    ),
    Group = factor(Group, levels = c("WR Athlete", "Coach")),
    
    # B. Effectiveness (Unified Variable)
    Raw_Effectiveness = coalesce(D3_Prog_Effectiveness, D4_Effectiveness),
    Raw_Effectiveness = str_squish(Raw_Effectiveness),
    Unified_Effectiveness = case_when(
      str_detect(Raw_Effectiveness, "(?i)not") ~ "Not effective",
      str_detect(Raw_Effectiveness, "(?i)very") ~ "Very effective",
      str_detect(Raw_Effectiveness, "(?i)moderately") ~ "Moderately effective",
      str_detect(Raw_Effectiveness, "(?i)slightly") ~ "Slightly effective",
      str_detect(Raw_Effectiveness, "(?i)effective") ~ "Effective",
      TRUE ~ NA_character_
    ),
    Unified_Effectiveness = factor(Unified_Effectiveness, levels = c(
      "Not effective", "Slightly effective", "Moderately effective", 
      "Effective", "Very effective"
    )),
    
    # C. general data type conversions and adjustments
    B2_Age = as.numeric(B2_Age),
    B3_Experience_Years = as.numeric(B3_Experience_Years),
    Unified_Education = coalesce(C1_Education, C3_Tech_Qualification),
    Comp_Level = factor(Comp_Level, levels = c("Local", "State", "Regional", "National", "International")),
    
    # D. adjustments for Coach-specific variables (C3, C4, C5)
    
    # 1. change C3_Tech_Qualification to English (if needed)
    C3_Tech_Qualification = str_replace_all(C3_Tech_Qualification, "(?i)nível", "Level"),
    
    # 2. scale C4_SC_Info_Freq to factor with labels
    C4_SC_Info_Freq = as.numeric(C4_SC_Info_Freq),
    C4_SC_Info_Freq = factor(C4_SC_Info_Freq,
                             levels = c(1, 2, 3, 4, 5),
                             labels = c("Never", "Rarely", "Sometimes", "Commonly", "Always")),
    
    # 3. who prescribes S&C (C5_SC_Prescriber) - unify to "S&C Coach" or "Coach"
    C5_SC_Prescriber = case_when(
      # first: if it contains "S&C" or "strength" (case-insensitive), label as "S&C Coach"
      str_detect(C5_SC_Prescriber, "(?i)S&C") ~ "S&C Coach",
      str_detect(C5_SC_Prescriber, "(?i)strength") ~ "S&C Coach",
      
      # second: if it contains "self", "próprio", "coach", or "treinador" (case-insensitive), label as "Coach"
      str_detect(C5_SC_Prescriber, "(?i)self") ~ "Coach",
      str_detect(C5_SC_Prescriber, "(?i)próprio") ~ "Coach",
      str_detect(C5_SC_Prescriber, "(?i)coach") ~ "Coach", 
      str_detect(C5_SC_Prescriber, "(?i)treinador") ~ "Coach",
      
      TRUE ~ C5_SC_Prescriber
    )
  )

# ==============================================================================
# 3. TABLE 1: DEMOGRAPHICS
# ==============================================================================
table1 <- df_table %>%
  select(Group, B1_Gender, B2_Age, B3_Experience_Years, Unified_Education, 
         Comp_Level, Unified_Effectiveness) %>% 
  tbl_summary(
    by = Group, 
    statistic = list(all_continuous() ~ "{mean} ± {sd}", all_categorical() ~ "{n} ({p}%)"),
    digits = all_continuous() ~ 1, missing = "ifany", 
    label = list(B1_Gender ~ "Gender", B2_Age ~ "Age (years)", 
                 B3_Experience_Years ~ "Experience (years)", Unified_Education ~ "Education",
                 Comp_Level ~ "Competition Level", Unified_Effectiveness ~ "Perceived Effectiveness")
  ) %>%
  add_overall() %>% bold_labels() %>% italicize_levels() %>%
  modify_caption("**Table 1. Demographics**")

print(table1)
table1 %>% as_flex_table() %>% save_as_docx(path = paste0(output_dir, "Table_1.docx"))

# ==============================================================================
# 4. TABLE 2: ATHLETE PROFILE & EQUIPMENT (SORTED BY FREQUENCY)
# ==============================================================================

# 1. Main Table (General Profile)
t2_main <- df_table %>%
  filter(Group == "WR Athlete") %>%
  select(C2_Classification, C4_Impairment_ICD, C3_Distance, C6_Prescriber, 
         C8_Chair_Origin, C7_Maintenance, C9_Glove_Type, C10_Wheel_Type, C11_Roller_Available) %>%
  tbl_summary(
    sort = list(all_categorical() ~ "frequency"),
    statistic = list(all_categorical() ~ "{n} ({p}%)"),
    missing = "ifany",
    label = list(
      C2_Classification ~ "Sport Class (T-Level)", 
      C4_Impairment_ICD ~ "Impairment Type",
      C3_Distance ~ "Event Distances (Profiles)", 
      C6_Prescriber ~ "Who Prescribes Training",
      C8_Chair_Origin ~ "Racing Chair Origin", 
      C7_Maintenance ~ "Who performs WR Maintenance",
      C9_Glove_Type ~ "Glove Type", 
      C10_Wheel_Type ~ "Wheel Type", 
      C11_Roller_Available ~ "Access to Rollers"
    )
  )

# 2. Technology Table (Conditional Logic)
if("C12_Tech_Available" %in% names(df_table)) {
  
  # --- STEP A: Data Preparation (Pivot Wider) ---
  df_tech_base <- df_table %>%
    filter(Group == "WR Athlete") %>%
    select(C12_Tech_Available) %>%
    mutate(id = row_number()) 
  
  # Separate and Pivot
  df_tech_wide <- df_tech_base %>%
    separate_rows(C12_Tech_Available, sep = ",") %>%
    mutate(C12_Tech_Available = str_trim(C12_Tech_Available)) %>%
    filter(C12_Tech_Available != "" & !is.na(C12_Tech_Available)) %>% 
    mutate(val = 1) %>%
    pivot_wider(
      names_from = C12_Tech_Available,
      values_from = val,
      values_fill = 0,
      id_cols = id 
    )
  
  # Join back to ensure correct N
  df_tech_final_data <- df_tech_base %>%
    select(id) %>%
    left_join(df_tech_wide, by = "id") %>%
    select(-id) %>% 
    mutate(across(everything(), ~replace_na(., 0))) 
  
  # --- STEP B.2: SORT COLUMNS BY FREQUENCY (NEW LOGIC) ---
  # We calculate the sum of each column and reorder the dataframe
  # so the table appears sorted from highest % to lowest %
  tech_order <- df_tech_final_data %>%
    summarise(across(everything(), sum)) %>%
    pivot_longer(everything(), names_to = "tech", values_to = "n") %>%
    arrange(desc(n)) %>%
    pull(tech)
  
  # Reorder the dataframe columns based on the count
  df_tech_final_data <- df_tech_final_data %>%
    select(all_of(tech_order))
  
  # --- STEP C: Generate Technology Summary ---
  t2_tech <- df_tech_final_data %>%
    tbl_summary(
      statistic = list(all_continuous() ~ "{n} ({p}%)"), 
      type = list(everything() ~ "dichotomous"),          
      value = list(everything() ~ 1),                     
      # Note: We don't need sort=frequency here because we already sorted the input columns
    )
  
  # --- STEP D: Stack and Save ---
  table2_final <- tbl_stack(
    list(t2_main, t2_tech),
    group_header = c("", "**Access to Technology (Checklist)**") 
  ) %>%
    bold_labels() %>% 
    italicize_levels() %>% 
    modify_header(label = "**Athlete Specifics**") %>%
    modify_caption("**Table 2. Athlete Sport Profile, Equipment and Technology**")
  
  print(table2_final)
  
  table2_final %>% as_flex_table() %>% save_as_docx(path = paste0(output_dir, "Table_2_Athlete_Final.docx"))
  
} else {
  print("Column C12_Tech_Available not found. Generating main table only.")
  t2_main %>% as_flex_table() %>% save_as_docx(path = paste0(output_dir, "Table_2_Athlete_Final.docx"))
  
}

# ==============================================================================
# 5. TABLE 3: COACH SPECIFICS & MONITORING (FINAL VERSION)
# ==============================================================================

# 1. Main Table (Categorical Variables)
t3_main <- df_table %>%
  filter(Group == "Coach") %>%
  select(
    B4.1_Role, 
    B5_Athlete_Age_Group, 
    C3_Tech_Qualification, 
    C4_SC_Info_Freq, 
    C5_SC_Prescriber
  ) %>% 
  tbl_summary(
    statistic = list(all_categorical() ~ "{n} ({p}%)"),
    missing = "ifany",
    label = list(
      B4.1_Role ~ "Role in the Team",
      B5_Athlete_Age_Group ~ "Age Group of Athletes Trained",
      C3_Tech_Qualification ~ "Technical Qualification",
      C4_SC_Info_Freq ~ "Frequency of S&C Information Search",
      C5_SC_Prescriber ~ "Who Prescribes S&C"
    )
  )

# 2. Load Monitoring Methods (Checklist) - Fixed N% Logic
if("D3.2_Load_Methods" %in% names(df_table)) {
  
  # --- STEP A: Data Preparation (Pivot Wider) ---
  # Create base with ID to track individual coaches
  df_methods_base <- df_table %>%
    filter(Group == "Coach") %>%
    select(D3.2_Load_Methods) %>%
    mutate(id = row_number()) 
  
  # Separate rows and Pivot to Wide format (One-Hot Encoding)
  df_methods_wide <- df_methods_base %>%
    separate_rows(D3.2_Load_Methods, sep = ",") %>%
    mutate(D3.2_Load_Methods = str_trim(D3.2_Load_Methods)) %>%
    filter(D3.2_Load_Methods != "" & !is.na(D3.2_Load_Methods)) %>% 
    mutate(val = 1) %>%
    pivot_wider(
      names_from = D3.2_Load_Methods,
      values_from = val,
      values_fill = 0,
      id_cols = id 
    )
  
  # Join back to original base to ensure coaches with NO methods are still counted in N
  df_methods_final_data <- df_methods_base %>%
    select(id) %>%
    left_join(df_methods_wide, by = "id") %>%
    select(-id) %>% 
    mutate(across(everything(), ~replace_na(., 0))) 
  
  # --- STEP B: Sort Columns by Frequency ---
  # Reorder columns so the table shows the most used methods first
  methods_order <- df_methods_final_data %>%
    summarise(across(everything(), sum)) %>%
    pivot_longer(everything(), names_to = "method", values_to = "n") %>%
    arrange(desc(n)) %>%
    pull(method)
  
  df_methods_final_data <- df_methods_final_data %>%
    select(all_of(methods_order))
  
  # --- STEP C: Generate Summary Table ---
  t3_methods <- df_methods_final_data %>%
    tbl_summary(
      statistic = list(all_continuous() ~ "{n} ({p}%)"), 
      type = list(everything() ~ "dichotomous"),          
      value = list(everything() ~ 1)
      # No need for label list; it uses column names automatically
    )
  
  # --- STEP D: Stack and Save ---
  table3_final <- tbl_stack(
    list(t3_main, t3_methods),
    group_header = c("", "**Load Monitoring Methods Used (Checklist)**")
  ) %>%
    bold_labels() %>% 
    italicize_levels() %>% 
    modify_header(label = "**Coach Specifics**") %>%
    modify_caption("**Table 3. Coach Professional Context and Monitoring Practices**")
  
  print(table3_final)
  
  table3_final %>% as_flex_table() %>% save_as_docx(path = paste0(output_dir, "Table_3_Coach_Specifics.docx"))
  
} else {
  # Fallback if column is missing
  print("Column D3.2_Load_Methods not found. Generating main table only.")
  t3_main %>% as_flex_table() %>% save_as_docx(path = paste0(output_dir, "Table_3_Coach_Specifics.docx"))
}
