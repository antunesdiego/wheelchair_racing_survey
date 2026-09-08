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
    # A. Grouping (Athlete vs Coach)
    Group = case_when(
      str_detect(Group, "(?i)trainer") ~ "Coach", 
      str_detect(Group, "(?i)athlete") ~ "WR Athlete",
      TRUE ~ Group
    ),
    Group = factor(Group, levels = c("WR Athlete", "Coach")),
    
    # B. Efectiveness (Unified Variable)
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
    
    # C. convert numeric variables and unify education
    B2_Age = as.numeric(B2_Age),
    B3_Experience_Years = as.numeric(B3_Experience_Years),
    Unified_Education = coalesce(C1_Education, C3_Tech_Qualification),
    Comp_Level = factor(Comp_Level, levels = c("Local", "State", "Regional", "National", "International")),
    
    # D. Adjustments for Coach-Specific Variables
    
    # 1. change Portuguese to English for Technical Qualification
    C3_Tech_Qualification = str_replace_all(C3_Tech_Qualification, "(?i)nível", "Level"),
    
    # 2. scale S&C Info Frequency to English and factor
    C4_SC_Info_Freq = as.numeric(C4_SC_Info_Freq),
    C4_SC_Info_Freq = factor(C4_SC_Info_Freq,
                             levels = c(1, 2, 3, 4, 5),
                             labels = c("Never", "Rarely", "Sometimes", "Commonly", "Always")),
    
    # 3. who prescribes S&C: unify to "S&C Coach" or "Coach" (Self, Technical Coach, etc)
    C5_SC_Prescriber = case_when(
      # First: Identify S&C Coach (ou Strength Coach)
      str_detect(C5_SC_Prescriber, "(?i)S&C") ~ "S&C Coach",
      str_detect(C5_SC_Prescriber, "(?i)strength") ~ "S&C Coach",
      
      # Second: the rest (Self, Technical Coach, etc) changes to "Coach"
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

# Clean up C12_Tech_Available to remove "Other" and treat it as NA
df_table <- df_table %>%
  mutate(
    C12_Tech_Available = na_if(C12_Tech_Available, "Other")
  )

# Create Distance Group Variable
df_table <- df_table %>%
  mutate(
    Distance_Group = case_when(
      # Long-distance — se compete em qualquer prova longa
      str_detect(C3_Distance, "5000m|10000m|Maratona|Meia Maratona") ~ "Long-distance (≥5000m)",
      # Middle-distance — se compete em 800m ou 1500m sem provas longas
      str_detect(C3_Distance, "800m|1500m") ~ "Middle-distance (800–1500m)",
      # Sprint — só provas curtas
      TRUE ~ "Sprint (100–400m)"
    )
  )

# Create Eligible Impairment Variable (World Para Athletics)
df_table <- df_table %>%
  mutate(
    Eligible_Impairment = case_when(
      C2_Classification %in% c("T33", "T34")             ~ "Hypertonia / Ataxia / Athetosis",
      C2_Classification %in% c("T52", "T53", "T54") & 
        C4_Impairment_ICD %in% c("Amputation", 
                                  "Congenital malformations") ~ "Limb Deficiency",
      C2_Classification %in% c("T52", "T53", "T54")      ~ "Impaired Muscle Power",
      TRUE ~ NA_character_
    )
  )

# Reorder C2_Classification to ensure proper order in the table
df_table <- df_table %>%
  mutate(
    C2_Classification = factor(C2_Classification, 
                               levels = c("T33", "T34", 
                                          "T52", "T53", "T54"))
  )

# 1. Main Table (General Profile)
t2_main <- df_table %>%
  filter(Group == "WR Athlete") %>%
  select(C2_Classification, C4_Impairment_ICD, Eligible_Impairment, Distance_Group, C6_Prescriber, 
         C8_Chair_Origin, C7_Maintenance, C9_Glove_Type, C10_Wheel_Type, C11_Roller_Available) %>%
  tbl_summary(
    sort = list(c(C4_Impairment_ICD, Distance_Group, C6_Prescriber,
                  C8_Chair_Origin, C7_Maintenance, C9_Glove_Type,
                  C10_Wheel_Type, C11_Roller_Available) ~ "frequency"),
    statistic = list(all_categorical() ~ "{n} ({p}%)"),
    missing = "ifany",
    label = list(
      C2_Classification ~ "Sport Class (T-Level)", 
      C4_Impairment_ICD ~ "Underlying Health Condition (UHC)",
      Eligible_Impairment ~ "Eligible Impairment (World Para Athletics)",
      Distance_Group ~ "Event Distance Profile", 
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
    filter(C12_Tech_Available != "" & 
       !is.na(C12_Tech_Available) &
       C12_Tech_Available != "Other") %>% 
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
  
    # --- STEP B: Remove "Others" and Sort by Frequency ---
  df_methods_final_data <- df_methods_final_data %>%
    select(-any_of("Others"))
  
  methods_order <- df_methods_final_data %>%
    summarise(across(everything(), sum)) %>%
    pivot_longer(everything(), names_to = "method", values_to = "n") %>%
    arrange(desc(n)) %>%
    pull(method)
  
  df_methods_final_data <- df_methods_final_data %>%
    select(all_of(methods_order))

  # --- STEP B2: Split into Execution vs Outcome groups ---
  execution_methods <- c(
    "Coordination and motor test",
    "Isometric Strength",
    "Pliometria com os braços",
    "Velocidade (GPS)"
  )
  
  outcome_methods <- c(
    "Rate Perception of Effort Scale",
    "Training session time",
    "Speed",
    "Distance",
    "Frequência Cardíaca",
    "Percentage of competitive performance",
    "Ballistic exercises"
  )
  
  exec_cols    <- intersect(execution_methods, names(df_methods_final_data))
  outcome_cols <- intersect(outcome_methods,   names(df_methods_final_data))
  
  df_exec    <- df_methods_final_data %>% select(all_of(exec_cols))
  df_outcome <- df_methods_final_data %>% select(all_of(outcome_cols))

  # --- STEP B3: Translate to English ---
  df_exec <- df_exec %>%
    rename_with(~ case_when(
      . == "Pliometria com os braços" ~ "Plyometric with Arms",
      . == "Velocidade (GPS)"         ~ "GPS Device (Speed)",
      TRUE ~ .
    ))

  df_outcome <- df_outcome %>%
    rename_with(~ case_when(
      . == "Rate Perception of Effort Scale"       ~ "Rate of Perceived Exertion (RPE)",
      . == "Frequência Cardíaca"                   ~ "Heart Rate",
      . == "Percentage of competitive performance" ~ "% of Competitive Performance",
      . == "Ballistic exercises"                   ~ "Performance Test with Ballistic Exercises",
      TRUE ~ .
    ))

  # --- STEP C: Generate Summary Tables ---
  t3_exec <- df_exec %>%
    tbl_summary(
      statistic = list(all_continuous() ~ "{n} ({p}%)"),
      type = list(everything() ~ "dichotomous"),
      value = list(everything() ~ 1)
    )
  
  t3_outcome <- df_outcome %>%
    tbl_summary(
      statistic = list(all_continuous() ~ "{n} ({p}%)"),
      type = list(everything() ~ "dichotomous"),
      value = list(everything() ~ 1)
    )

  # --- STEP D: Stack and Save ---
  table3_final <- tbl_stack(
    list(t3_main, t3_exec, t3_outcome),
    group_header = c(
      "",
      "**Load Monitoring — Performance Execution**",
      "**Load Monitoring — Performance Outcome**"
    )
  ) %>%
    bold_labels() %>%
    italicize_levels() %>%
    modify_header(label = "**Coach Specifics**") %>%
    modify_caption("**Table 3. Coach Professional Context and Monitoring Practices**")
  
  print(table3_final)
  
    table3_final %>% as_flex_table() %>% 
    save_as_docx(path = paste0(output_dir, "Table_3_Coach_Specifics.docx"))

} else {
  # Fallback if column is missing
  print("Column D3.2_Load_Methods not found. Generating main table only.")
  t3_main %>% as_flex_table() %>% save_as_docx(path = paste0(output_dir, "Table_3_Coach_Specifics.docx"))
}

