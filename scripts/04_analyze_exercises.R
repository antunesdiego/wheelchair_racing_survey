# ==============================================================================
# SCRIPT 04: EXERCISE STANDARDIZATION & TEXT MINING (FINAL VERSION V5)
# Description: Cleans text data from exercise columns using hierarchical regex.
#              Features: 
#              - Handles newlines (\n) for vertical lists.
#              - Prioritizes Specific Methods over General Terms.
#              - Covers E1 to E6.
# ==============================================================================

# 1. SETUP --------------------------------------------------------------------
library(tidyverse)
library(stringi)
library(patchwork)
library(tidytext)
library(packcircles)
library(viridis)

# Load Clean Data
df_clean <- read_csv("Paper 1/df_analysis/03_clean_data/clean_analysis_data.csv", 
                     show_col_types = FALSE)

# 2. HELPER: TEXT NORMALIZATION & SEPARATION ----------------------------------
clean_and_separate <- function(data, col_name) {
  data %>%
    select(Group, ID_Col, Raw_Text = all_of(col_name)) %>%
    filter(!is.na(Raw_Text)) %>%
    # SEPARADORES: Vírgula, Ponto, Barra, " e ", hífen E NOVA LINHA (\n)
    separate_rows(Raw_Text, sep = ",|;|/| e |\\.|-|\\n") %>%
    mutate(Term_Clean = str_to_lower(Raw_Text)) %>%
    mutate(Term_Clean = stringi::stri_trans_general(Term_Clean, "Latin-ASCII")) %>%
    mutate(Term_Clean = str_trim(Term_Clean)) %>%
    filter(Term_Clean != "")
}

# 3. FUNCTION E1: STRENGTH ----------------------------------------------------
process_E1 <- function(data) {
  clean_and_separate(data, "E1_Ex_Strength") %>%
    mutate(
      Exercise_Category = case_when(
        # --- PRIMARY STRENGTH (HIERARCHY TOP) ---
        str_detect(Term_Clean, "supino|bench") ~ "Bench Press",
        str_detect(Term_Clean, "remada|row|serrote") ~ "Seated Row",
        str_detect(Term_Clean, "puxada|barra|costa|pulldown|pollouver|dorsal") ~ "Lat Pull-Down",
        str_detect(Term_Clean, "desenvolvimento|ombro|militar|eleva|press") ~ "Shoulder Press",
        str_detect(Term_Clean, "paralela|mergulho|dip") ~ "Dips",
        str_detect(Term_Clean, "flexao|push up|apoio") ~ "Push up",
        str_detect(Term_Clean, "biceps|rosca|curl") ~ "Biceps Curls",
        str_detect(Term_Clean, "triceps|tricpes|testa|corda") ~ "Triceps Extensions",
        str_detect(Term_Clean, "antebraco|punho|wrist") ~ "Wrist Curls/Extensions",
        str_detect(Term_Clean, "core|prancha|rotacao|abdomen|abdominal") ~ "Core Strength",
        
        # --- POWER & SPECIFIC ---
        str_detect(Term_Clean, "plio") ~ "Plyometrics",
        str_detect(Term_Clean, "arremesso|explosao|ball|bola|medicina") ~ "Ballistic exercise",
        str_detect(Term_Clean, "treno|sled") ~ "Resisted Sled Towing",
        str_detect(Term_Clean, "rampa|subida") ~ "Uphill Training",
        str_detect(Term_Clean, "rolo") ~ "High-Resistance Roller Training",
        str_detect(Term_Clean, "elastico|band") ~ "Resistance band",
        str_detect(Term_Clean, "educativo|toque|tecnica|encaixe|largada") ~ "Propulsion Technique Drills",
        
        # --- OTHERS / MISPLACED ---
        str_detect(Term_Clean, "rodagem") ~ "Continuous Endurance Training",
        TRUE ~ "Other"
      )
    ) %>% filter(Exercise_Category != "Other")
}

# 4. FUNCTION E2: SPEED -------------------------------------------------------
process_E2 <- function(data) {
  clean_and_separate(data, "E2_Ex_Speed") %>%
    mutate(
      Exercise_Category = case_when(
        # --- SPEED SPECIFIC (HIERARCHY TOP) ---
        str_detect(Term_Clean, "declive") ~ "Overspeed Training",
        str_detect(Term_Clean, "sprint|tiro|velocidade") ~ "Sprint",
        str_detect(Term_Clean, "explosiv|salto|ball|bola|medicina|arremesso|lancamento|lancar") ~ "Ballistic exercise",
        str_detect(Term_Clean, "plio") ~ "Plyometrics",
        str_detect(Term_Clean, "treno|pneus|paraquedas") ~ "Resisted Sled Towing",
        str_detect(Term_Clean, "rampa|subida") ~ "Uphill Training",
        str_detect(Term_Clean, "motorzinho|agilidade|encaixe|toque|progressivo|educativo|manivela|propulsao") ~ "Propulsion Technique Drills",
        str_detect(Term_Clean, "rolo") ~ "High-Resistance Roller Training",
        
        # --- STRENGTH/GENERAL (LOWER HIERARCHY) ---
        str_detect(Term_Clean, "elastico") ~ "Resistance band",
        str_detect(Term_Clean, "supino|push press") ~ "Bench Press",
        str_detect(Term_Clean, "paralela") ~ "Dips",
        str_detect(Term_Clean, "triceps") ~ "Triceps Extensions",
        str_detect(Term_Clean, "biceps") ~ "Biceps Curls",
        str_detect(Term_Clean, "remada") ~ "Seated Row",
        str_detect(Term_Clean, "barra|pullover|puxada") ~ "Lat Pull-Down",
        str_detect(Term_Clean, "desenvolvimento|eleva") ~ "Shoulder Press",
        str_detect(Term_Clean, "abdominal|giro|rotacao|tronco") ~ "Core Strength",
        str_detect(Term_Clean, "flexao") ~ "Push up",
        str_detect(Term_Clean, "rodagem") ~ "Continuous Endurance Training",
        TRUE ~ "Other"
      )
    ) %>% filter(Exercise_Category != "Other")
}

# 5. FUNCTION E3: AGILITY -----------------------------------------------------
process_E3 <- function(data) {
  clean_and_separate(data, "E3_Ex_Agility") %>%
    mutate(
      Exercise_Category = case_when(
        # --- AGILITY SPECIFIC ---
        str_detect(Term_Clean, "zigue|zague|suicidio|vareta|slalom|cone|shuttle|ladder|empinar|aeroporto") ~ "Chair Handling Skills",
        str_detect(Term_Clean, "reacao|foco") ~ "Reaction Time Drills",
        str_detect(Term_Clean, "motorzinho|manivela|toque|educativo|tecnica|propul|proporcao|saida") ~ "Propulsion Technique Drills",
        
        # --- POWER/SPEED MIX ---
        str_detect(Term_Clean, "descendo") ~ "Overspeed Training",
        str_detect(Term_Clean, "tiro|sprint") ~ "Sprint",
        str_detect(Term_Clean, "potencia|explosiv|palmas|ball|bola|arremesso") ~ "Ballistic exercise",
        str_detect(Term_Clean, "plio") ~ "Plyometrics",
        str_detect(Term_Clean, "subindo|rampa") ~ "Uphill Training",
        str_detect(Term_Clean, "ritmo") ~ "Fartlek",
        
        # --- GENERAL ---
        str_detect(Term_Clean, "rolo|ergometro") ~ "High-Resistance Roller Training",
        str_detect(Term_Clean, "mobilidade") ~ "Joint Mobility Exercises",
        str_detect(Term_Clean, "alongamento") ~ "Stretching Exercises",
        str_detect(Term_Clean, "elastico") ~ "Resistance band",
        
        # --- STRENGTH FILLERS ---
        str_detect(Term_Clean, "supino") ~ "Bench Press",
        str_detect(Term_Clean, "paralela") ~ "Dips",
        str_detect(Term_Clean, "remada") ~ "Seated Row",
        str_detect(Term_Clean, "barra|puxada") ~ "Lat Pull-Down",
        str_detect(Term_Clean, "desenvolvimento|ombro") ~ "Shoulder Press",
        str_detect(Term_Clean, "triceps") ~ "Triceps Extensions",
        str_detect(Term_Clean, "biceps") ~ "Biceps Curls",
        str_detect(Term_Clean, "flexao") ~ "Push up",
        str_detect(Term_Clean, "tronco|core|abdomina") ~ "Core Strength",
        TRUE ~ "Other"
      )
    ) %>% filter(Exercise_Category != "Other")
}

# 6. FUNCTION E4: FLEXIBILITY -------------------------------------------------
process_E4 <- function(data) {
  clean_and_separate(data, "E4_Ex_Flexibility") %>%
    mutate(
      Exercise_Category = case_when(
        # --- PRIMARY FLEXIBILITY ---
        str_detect(Term_Clean, "liberacao|miofascial|soltura") ~ "Myofascial Release",
        str_detect(Term_Clean, "mobilidade|mobilizacao|anjinho|circundacao|bastao|manguito|postura") ~ "Joint Mobility Exercises",
        str_detect(Term_Clean, "fisio|passiva|passivos") ~ "Physiotherapy / Assisted Stretching",
        str_detect(Term_Clean, "flexibilidade|alongamento") ~ "Stretching Exercises",
        str_detect(Term_Clean, "aquecimento") ~ "Warm up",
        
        # --- MISPLACED STRENGTH ---
        str_detect(Term_Clean, "elastico|borracha") ~ "Resistance band",
        str_detect(Term_Clean, "remada") ~ "Seated Row",
        str_detect(Term_Clean, "flexao") ~ "Push up",
        str_detect(Term_Clean, "abdominal|prancha|tronco") ~ "Core Strength",
        str_detect(Term_Clean, "triceps|cotovelo") ~ "Triceps Extensions",
        str_detect(Term_Clean, "biceps") ~ "Biceps Curls",
        str_detect(Term_Clean, "ombro") ~ "Shoulder Press",
        str_detect(Term_Clean, "punho") ~ "Wrist Curls/Extensions",
        str_detect(Term_Clean, "educativo") ~ "Propulsion Technique Drills",
        TRUE ~ "Other"
      )
    ) %>% filter(Exercise_Category != "Other")
}

# 7. FUNCTION E5: INJURY PREVENTION -------------------------------------------
process_E5 <- function(data) {
  clean_and_separate(data, "E5_Ex_InjuryPrev") %>%
    mutate(
      Exercise_Category = case_when(
        # --- SPECIFIC PREVENTION (PRIORITY) ---
        str_detect(Term_Clean, "manguito|rotador|roteador|rotacao|ombro|escapul") ~ "Rotator Cuff Strengthening",
        str_detect(Term_Clean, "gelo|gelada") ~ "Cryotherapy",
        str_detect(Term_Clean, "dormir|sono|descanso|regenerativo") ~ "Rest & Recovery",
        str_detect(Term_Clean, "preensao") ~ "Hand Grip",
        str_detect(Term_Clean, "mobilidade") ~ "Joint Mobility Exercises",
        str_detect(Term_Clean, "alongamento|flexibilidade") ~ "Stretching Exercises",
        str_detect(Term_Clean, "aquecimento") ~ "Warm up",
        
        # --- GENERAL STRENGTH ---
        str_detect(Term_Clean, "elastico|borracha|band") ~ "Resistance band",
        str_detect(Term_Clean, "tronco|core|prancha|dorsal|ponte") ~ "Core Strength",
        str_detect(Term_Clean, "supino") ~ "Bench Press",
        str_detect(Term_Clean, "remada") ~ "Seated Row",
        str_detect(Term_Clean, "punho|antebraco|mao") ~ "Wrist Curls/Extensions",
        str_detect(Term_Clean, "triceps") ~ "Triceps Extensions",
        str_detect(Term_Clean, "biceps") ~ "Biceps Curls",
        str_detect(Term_Clean, "educativo") ~ "Propulsion Technique Drills",
        TRUE ~ "Other"
      )
    ) %>% filter(Exercise_Category != "Other")
}

# 8. FUNCTION E6: SPECIFIC CONDITIONING (UPDATED) -----------------------------
process_E6 <- function(data) {
  clean_and_separate(data, "E6_Ex_SpecificCond") %>%
    mutate(
      Exercise_Category = case_when(
        # --- TIER 1: HIGH INTENSITY / SPECIFIC (PRIORITY) ---
        str_detect(Term_Clean, "fartlek|fart leck") ~ "Fartlek",
        
        str_detect(Term_Clean, "tiro|pista|400m|intervalado|progressivo|50-50|150-50") ~ "Track Training",
        str_detect(Term_Clean, "velocidade|sprint|acelera") ~ "Sprint",
        str_detect(Term_Clean, "subida|rampa") ~ "Uphill Training",
        str_detect(Term_Clean, "treno") ~ "Resisted Sled Towing",
        str_detect(Term_Clean, "pliometria") ~ "Plyometrics",
        
        # --- TIER 2: VOLUME / ENDURANCE ---
        str_detect(Term_Clean, "rua|longa|rodagem|volume") ~ "Continuous Endurance Training",
        str_detect(Term_Clean, "rolo|ergometro") ~ "High-Resistance Roller Training",
        
        # --- TIER 3: SKILL & TECHNIQUE ---
        str_detect(Term_Clean, "reacao|tempo de reacao") ~ "Reaction Time Drills",
        
        str_detect(Term_Clean, "motorzinho|finalizacao|saida|educativo|coordenativo|toque|encaixe|manivela") ~ "Propulsion Technique Drills",
        
        # --- TIER 4: GENERAL STRENGTH / OTHERS ---
        str_detect(Term_Clean, "forca|halter|academia|circuito|corda|musculacao") ~ "Strength Training",
        str_detect(Term_Clean, "preensao") ~ "Hand Grip",
        str_detect(Term_Clean, "supino") ~ "Bench Press",
        str_detect(Term_Clean, "remada|remo") ~ "Seated Row",
        str_detect(Term_Clean, "triceps") ~ "Triceps Extensions",
        str_detect(Term_Clean, "puxador") ~ "Lat Pull-Down",
        str_detect(Term_Clean, "elastico") ~ "Resistance band",
        str_detect(Term_Clean, "tronco|prancha") ~ "Core Strength",
        str_detect(Term_Clean, "ombro") ~ "Shoulder Press",
        str_detect(Term_Clean, "mobilidade") ~ "Joint Mobility Exercises",
        str_detect(Term_Clean, "aquecimento") ~ "Warm up",
        str_detect(Term_Clean, "flexao") ~ "Push up",
        
        TRUE ~ "Other"
      )
    ) %>% filter(Exercise_Category != "Other")
}

# 9. FUNCTION E7: PERFORMANCE FACTORS (NEW!) ----------------------------------
process_E7 <- function(data) {
  clean_and_separate(data, "E7_Ex_Performance") %>%
    mutate(
      Exercise_Category = case_when(
        # --- TIER 1: SPECIALIZED METHODS (Highest Priority) ---
        str_detect(Term_Clean, "supra|hiper") ~ "Overspeed Training",
        str_detect(Term_Clean, "treno") ~ "Resisted Sled Towing",
        str_detect(Term_Clean, "rampa") ~ "Uphill Training",
        str_detect(Term_Clean, "fartlek") ~ "Fartlek",
        str_detect(Term_Clean, "explosao|rapidos|arremeco|arremesso") ~ "Ballistic exercise",
        str_detect(Term_Clean, "reacao") ~ "Reaction Time Drills",
        
        # --- TIER 2: RACING SPECIFICS ---
        # User defined: Sprint/Largada separately from Tiro/Pista
        str_detect(Term_Clean, "largada|sprint|aceleracao") ~ "Sprint",
        str_detect(Term_Clean, "tiro|pelotao|velocidade|pista|dinamicos") ~ "Track Training",
        str_detect(Term_Clean, "rodagem") ~ "Continuous Endurance Training",
        str_detect(Term_Clean, "rolo") ~ "High-Resistance Roller Training",
        
        # --- TIER 3: TECHNIQUE & SKILL ---
        str_detect(Term_Clean, "educativo|motorzinho|cincotoque|propulsao|luva") ~ "Propulsion Technique Drills",
        str_detect(Term_Clean, "mobilidade") ~ "Joint Mobility Exercises",
        
        # --- TIER 4: PSYCHOLOGICAL & EQUIPMENT ---
        str_detect(Term_Clean, "mentalidade|vontade") ~ "Mental Toughness",
        str_detect(Term_Clean, "equipamento") ~ "Wheelchair Racing Equipment",
        
        # --- TIER 5: STRENGTH & CONDITIONING ---
        str_detect(Term_Clean, "supino|peitoral") ~ "Bench Press",
        str_detect(Term_Clean, "remada") ~ "Seated Row",
        str_detect(Term_Clean, "triceps") ~ "Triceps Extensions",
        str_detect(Term_Clean, "biceps") ~ "Biceps Curls",
        str_detect(Term_Clean, "costas") ~ "Lat Pull-Down", # Mapped 'costas' to Lat Pull-Down based on context
        str_detect(Term_Clean, "desenvolvimento|ombro") ~ "Shoulder Press",
        str_detect(Term_Clean, "rotadores") ~ "Rotator Cuff Strengthening",
        str_detect(Term_Clean, "abdominal|lombar|tronco|postura") ~ "Core Strength",
        str_detect(Term_Clean, "flexao") ~ "Push up",
        str_detect(Term_Clean, "isometria") ~ "Isometric Strength",
        str_detect(Term_Clean, "halter|musculacao") ~ "Strength Training",
        
        TRUE ~ "Other"
      )
    ) %>% filter(Exercise_Category != "Other")
}

# 10. FUNCTION E8: MOBILITY & MISC (FINAL COLUMN) -----------------------------
process_E8 <- function(data) {
  clean_and_separate(data, "E8_Ex_Mobility") %>%
    mutate(
      Exercise_Category = case_when(
        # --- TIER 1: SPECIFIC PRE-HAB/MOBILITY (Priority) ---
        # User defined: Motorzinho/Manejo included in Mobility context here
        str_detect(Term_Clean, "anjinho|mobilidade|movimento|manejo|motorzinho") ~ "Joint Mobility Exercises",
        str_detect(Term_Clean, "manguito") ~ "Rotator Cuff Strengthening",
        str_detect(Term_Clean, "alongamento") ~ "Stretching Exercises",
        str_detect(Term_Clean, "aquecimento") ~ "Warm up",
        
        # --- TIER 2: TECHNIQUE & POWER ---
        str_detect(Term_Clean, "medicine|arremesso") ~ "Ballistic exercise",
        str_detect(Term_Clean, "educativo") ~ "Propulsion Technique Drills",
        
        # --- TIER 3: MISPLACED STRENGTH/ENDURANCE ---
        str_detect(Term_Clean, "elastico|borracha") ~ "Resistance band",
        str_detect(Term_Clean, "tronco") ~ "Core Strength",
        str_detect(Term_Clean, "desenvolvimento|ombro") ~ "Shoulder Press",
        str_detect(Term_Clean, "remada") ~ "Seated Row",
        str_detect(Term_Clean, "triceps") ~ "Triceps Extensions",
        str_detect(Term_Clean, "punho") ~ "Wrist Curls/Extensions",
        str_detect(Term_Clean, "rodagem") ~ "Continuous Endurance Training",
        
        TRUE ~ "Other"
      )
    ) %>% filter(Exercise_Category != "Other")
}

# 11. RUN PIPELINE & SAVE ------------------------------------------------------
dir.create("Paper 1/df_analysis/04_exercises", showWarnings = FALSE)

message("Processing E1 (Strength)...")
write_csv(process_E1(df_clean), "Paper 1/df_analysis/04_exercises/E1_Strength_Cleaned.csv")

message("Processing E2 (Speed)...")
write_csv(process_E2(df_clean), "Paper 1/df_analysis/04_exercises/E2_Speed_Cleaned.csv")

message("Processing E3 (Agility)...")
write_csv(process_E3(df_clean), "Paper 1/df_analysis/04_exercises/E3_Agility_Cleaned.csv")

message("Processing E4 (Flexibility)...")
write_csv(process_E4(df_clean), "Paper 1/df_analysis/04_exercises/E4_Flexibility_Cleaned.csv")

message("Processing E5 (Injury Prev)...")
write_csv(process_E5(df_clean), "Paper 1/df_analysis/04_exercises/E5_InjuryPrev_Cleaned.csv")

message("Processing E6 (Specific Cond)...")
write_csv(process_E6(df_clean), "Paper 1/df_analysis/04_exercises/E6_SpecificCond_Cleaned.csv")

message("Processing E7 (Performance)...")
write_csv(process_E7(df_clean), "Paper 1/df_analysis/04_exercises/E7_Performance_Cleaned.csv")

message("Processing E8 (Mobility)...")
write_csv(process_E8(df_clean), "Paper 1/df_analysis/04_exercises/E8_Mobility_Cleaned.csv")

message("\n✅ MISSION ACCOMPLISHED! ALL 8 FILES SAVED!")


###############################################
#Graphics for inspection - E1 Strength Exercises
###############################################
# 1. Load the cleaned data (generated in script04)
df_plot <- read_csv("Paper 1/df_analysis/04_exercises/E1_Strength_Cleaned.csv", 
                    show_col_types = FALSE)

# 2. Generate the Graph
ggplot(df_plot, aes(y = fct_rev(fct_infreq(Exercise_Category)), fill = Group)) +
  
  # Bar Geometry (position = "dodge" places side by side)
  geom_bar(position = position_dodge(preserve = "single"), width = 0.8) +
  
  # Colors and Style
  scale_fill_viridis_d(option = "D", begin = 0.3, end = 0.8) + # Cores profissionais
  theme_classic(base_size = 14) + # Tema limpo para publicação
  
  # Texts and Captions
  labs(
    title = "Top Strength Exercises Prescribed/Performed",
    subtitle = "Analysis of open-ended responses (E1)",
    x = "Frequency (n)",
    y = NULL, # Remove o rótulo Y pois os nomes já explicam
    fill = "Group"
  ) +
  
  # Fine-tuning the view
  theme(
    legend.position = "top",          # Legenda em cima
    panel.grid.major.x = element_line(color = "grey90"), # Linhas verticais suaves
    axis.text.y = element_text(color = "black") # Texto preto forte no eixo Y
  )

###############################################
#Graphics for inspection - E2 Speed Exercises
###############################################

# 1. Load speed data
df_plot_speed <- read_csv("Paper 1/df_analysis/04_exercises/E2_Speed_Cleaned.csv", 
                          show_col_types = FALSE)

# 2. Generate Graph
ggplot(df_plot_speed, aes(y = fct_rev(fct_infreq(Exercise_Category)), fill = Group)) +
  
  geom_bar(position = position_dodge(preserve = "single"), width = 0.8) +
  
  scale_fill_viridis_d(option = "D", begin = 0.3, end = 0.8) + 
  theme_classic(base_size = 12) + 
  
  labs(
    title = "Speed Exercises Prescribed/Performed",
    subtitle = "Analysis of E2 column (All mentions)",
    x = "Frequency (n)",
    y = NULL,
    fill = "Group"
  ) +
  
  theme(
    legend.position = "top",
    panel.grid.major.x = element_line(color = "grey90"),
    axis.text.y = element_text(size = 10, color = "black") 
  )


###############################################
#Graphics for inspection - E3 Agility Exercises
###############################################

# 1. Load Agility Data
df_plot_agility <- read_csv("Paper 1/df_analysis/04_exercises/E3_Agility_Cleaned.csv", 
                            show_col_types = FALSE)

# 2. Generate Graph
ggplot(df_plot_agility, aes(y = fct_rev(fct_infreq(Exercise_Category)), fill = Group)) +
  
  # Bars side-by-side
  geom_bar(position = position_dodge(preserve = "single"), width = 0.8) +
  
  # professional style
  scale_fill_viridis_d(option = "D", begin = 0.3, end = 0.8) + 
  theme_classic(base_size = 14) +
  
  # labels and titles
  labs(
    title = "Agility Exercises Prescribed/Performed",
    subtitle = "Analysis of E3 column",
    x = "Frequency (n)",
    y = NULL,
    fill = "Group"
  ) +
  
  # Adjust de layout
  theme(
    legend.position = "top",
    panel.grid.major.x = element_line(color = "grey90"),
    axis.text.y = element_text(color = "black")
  )

###############################################
#Graphics for inspection - E4 Flexibility Exercises
###############################################

# 1. load Flexibility Data
df_plot_flex <- read_csv("Paper 1/df_analysis/04_exercises/E4_Flexibility_Cleaned.csv", 
                         show_col_types = FALSE)

# 2. Generate Graph
ggplot(df_plot_flex, aes(y = fct_rev(fct_infreq(Exercise_Category)), fill = Group)) +
  
  # Bars side-by-side
  geom_bar(position = position_dodge(preserve = "single"), width = 0.8) +
  
  # Professional Style
  scale_fill_viridis_d(option = "D", begin = 0.3, end = 0.8) + 
  theme_classic(base_size = 14) +
  
  # Labels and Titles
  labs(
    title = "Flexibility Methods Prescribed/Performed",
    subtitle = "Analysis of E4 column (Stretching vs. Mobility)",
    x = "Frequency (n)",
    y = NULL,
    fill = "Group"
  ) +
  
  # Layout Adjustments
  theme(
    legend.position = "top",
    panel.grid.major.x = element_line(color = "grey90"),
    axis.text.y = element_text(color = "black")
  )

###############################################
#Graphics for inspection - E5 Injury prevention
###############################################

# 1. Load Clean Data (E5)
df_plot_injury <- read_csv("Paper 1/df_analysis/04_exercises/E5_InjuryPrev_Cleaned.csv", 
                           show_col_types = FALSE)

# 2. Generate Plot
ggplot(df_plot_injury, aes(y = fct_rev(fct_infreq(Exercise_Category)), fill = Group)) +
  
  # Bars side-by-side
  geom_bar(position = position_dodge(preserve = "single"), width = 0.8) +
  
  # Professional Style
  scale_fill_viridis_d(option = "D", begin = 0.3, end = 0.8) + 
  theme_classic(base_size = 14) +
  
  # Labels
  labs(
    title = "Injury Prevention Strategies Prescribed/Performed",
    subtitle = "Analysis of E5 column (Prevention & Recovery)",
    x = "Frequency (n)",
    y = NULL,
    fill = "Group"
  ) +
  
  # Theme Adjustments
  theme(
    legend.position = "top",
    panel.grid.major.x = element_line(color = "grey90"),
    axis.text.y = element_text(color = "black")
  )

###############################################
#Graphics for inspection - E6 Specific Training
###############################################
# 1. Load Cleaned Data (E6)
df_plot_specific <- read_csv("Paper 1/df_analysis/04_exercises/E6_SpecificCond_Cleaned.csv", 
                             show_col_types = FALSE)

# 2. Genereate Graph
ggplot(df_plot_specific, aes(y = fct_rev(fct_infreq(Exercise_Category)), fill = Group)) +
  geom_bar(position = position_dodge(preserve = "single"), width = 0.8) +
  scale_fill_viridis_d(option = "D", begin = 0.3, end = 0.8) + 
  theme_classic(base_size = 14) +
  labs(
    title = "Specific Conditioning Strategies",
    subtitle = "Analysis of E6 (Track vs. Road vs. Skills)",
    x = "Frequency (n)",
    y = NULL,
    fill = "Group"
  ) +
  theme(
    legend.position = "top",
    panel.grid.major.x = element_line(color = "grey90"),
    axis.text.y = element_text(color = "black")
  )

###############################################
#Graphics for inspection - E7 Performance Factors
###############################################
# 1. load Cleaned Data (E7)
df_plot_perf <- read_csv("Paper 1/df_analysis/04_exercises/E7_Performance_Cleaned.csv", 
                         show_col_types = FALSE)

# 2. Generate Graph
ggplot(df_plot_perf, aes(y = fct_rev(fct_infreq(Exercise_Category)), fill = Group)) +
  
  # Bars side-by-side
  geom_bar(position = position_dodge(preserve = "single"), width = 0.8) +
  
  # Professional Style
  scale_fill_viridis_d(option = "D", begin = 0.3, end = 0.8) + 
  theme_classic(base_size = 14) +
  
  # Texts
  labs(
    title = "Performance Improvement Strategies",
    subtitle = "Analysis of E7 (Physical, Technical & Psychological Factors)",
    x = "Frequency (n)",
    y = NULL,
    fill = "Group"
  ) +
  
  # adjustments for better readability
  theme(
    legend.position = "top",
    panel.grid.major.x = element_line(color = "grey90"),
    axis.text.y = element_text(color = "black")
  )

###############################################
#Graphics for inspection - E8 Mobility Exercises
###############################################

# 1. Load Cleaned Data (E8)
df_plot_mob <- read_csv("Paper 1/df_analysis/04_exercises/E8_Mobility_Cleaned.csv", 
                        show_col_types = FALSE)

# 2. Generate Graph
ggplot(df_plot_mob, aes(y = fct_rev(fct_infreq(Exercise_Category)), fill = Group)) +
  geom_bar(position = position_dodge(preserve = "single"), width = 0.8) +
  scale_fill_viridis_d(option = "D", begin = 0.3, end = 0.8) + 
  theme_classic(base_size = 14) +
  labs(
    title = "Mobility & Additional Strategies",
    subtitle = "Analysis of E8 (Joint Mobility vs. Other definitions)",
    x = "Frequency (n)",
    y = NULL,
    fill = "Group"
  ) +
  theme(
    legend.position = "top",
    panel.grid.major.x = element_line(color = "grey90"),
    axis.text.y = element_text(color = "black")
  )



#All graphics are now generated and ready for inspection. The next step is to create a final publication-ready figure that combines all 7 domain panels along with a legend/info box, ensuring that only exercises with a total frequency of 2 or more are included. This will provide a clear and concise visual summary of the exercise categories across different groups.

# ==============================================================================
# FINAL VISUALIZATION: FIGURE 2 (PUBLICATION READY)
# Filter: Total Frequency >= 2 (Removes singletons)
# Layout: 7 Domain Panels + 1 Legend/Info Box
# ==============================================================================


# 1. GENERAL SETUP
base_path <- "Paper 1/df_analysis/04_exercises/"


# Custom Theme for Publication (Clean & Minimalist)
theme_publication <- function(base_size = 11) {
  theme_classic(base_size = base_size) +
    theme(
      plot.title = element_text(face = "bold", size = base_size + 2, hjust = 0.5),
      # Reduced Y-axis text size to avoid overlapping labels
      axis.text.y = element_text(color = "black", size = 10), 
      axis.text.x = element_text(color = "black", size = 10),
      # Only vertical dashed lines to guide the eye
      panel.grid.major.x = element_line(color = "grey92", linetype = "dashed"),
      panel.grid.major.y = element_blank(),
      legend.position = "none", # Hides individual legends
      plot.margin = margin(5, 15, 5, 5)
    )
}

# 2. PANEL GENERATION FUNCTION
create_pub_panel <- function(file_name, title_text) {
  
  full_path <- paste0(base_path, file_name)
  if(!file.exists(full_path)) return(NULL)
  
  # A. Data Preparation
  df <- read_csv(full_path, show_col_types = FALSE) %>%
    # Standardize Group Names
    mutate(Group = case_when(
      Group == "Athlete" ~ "WR Athletes",
      Group == "Trainer" ~ "Coaches",
      TRUE ~ Group
    )) %>%
    # Set Factor levels to ensure consistent colors
    mutate(Group = factor(Group, levels = c("WR Athletes", "Coaches"))) %>%
    count(Exercise_Category, Group) %>%
    rename(value = n)
  
  # B. FILTERING LOGIC (UPDATED: n >= 2)
  top_exercises <- df %>%
    group_by(Exercise_Category) %>%
    summarise(Total = sum(value)) %>%
    
    # CRITERION 1: Keep exercises with at least 2 total citations
    filter(Total >= 2) %>%   
    
    # CRITERION 2: Cap at Top 12 to fit the page layout
    slice_max(Total, n = 12) %>% 
    
    pull(Exercise_Category)
  
  # Safety check: return empty plot if no data passes filter
  if(length(top_exercises) == 0) return(ggplot() + theme_void())
  
  df_filtered <- df %>% filter(Exercise_Category %in% top_exercises)
  
  # C. Plotting
  ggplot(df_filtered, 
         aes(y = reorder(Exercise_Category, value), 
             x = value, 
             fill = Group)) +
    
    # Adjusted width for elegant spacing
    geom_col(position = position_dodge(width = 0.7), width = 0.6) +
    
    labs(title = title_text, x = NULL, y = NULL) +
    
    # Manual Colors (Navy for Athletes, Green for Coaches)
    scale_fill_manual(values = c("WR Athletes" = "#3b528b", "Coaches" = "#5ec962")) +
    
    # X-Axis: Step 2 (0, 2, 4...)
    scale_x_continuous(
      breaks = function(x) seq(0, ceiling(max(x)), by = 2),
      expand = expansion(mult = c(0, 0.05))
    ) +
    
    theme_publication()
}

# 3. LEGEND & INFO PANEL (SLOT 8)
create_legend_panel <- function() {
  # Dummy data to generate the legend keys
  dummy_data <- data.frame(
    Group = factor(c("WR Athletes", "Coaches"), levels = c("WR Athletes", "Coaches")),
    x = c(0,0), y = c(0,0)
  )
  
  ggplot(dummy_data, aes(x=x, y=y, fill=Group)) +
    geom_col(alpha = 0) + 
    
    
    scale_fill_manual(
      values = c("WR Athletes" = "#3b528b", "Coaches" = "#5ec962"),
      name = NULL 
    ) +
    theme_void() +
    theme(
      legend.position = "top", 
      legend.justification = "center",
      legend.direction = "horizontal",
      legend.text = element_text(size = 9, face = "bold"),
      legend.key.size = unit(0.4, "cm"),
      legend.key = element_rect(fill = NA, color = NA) 
    ) +
    guides(fill = guide_legend(override.aes = list(alpha = 1))) 
}

# 4. GENERATE PANELS
p1 <- create_pub_panel("E1_Strength_Cleaned.csv", "A. Strength Development")
p2 <- create_pub_panel("E2_Speed_Cleaned.csv", "B. Speed/Power Development")
p3 <- create_pub_panel("E3_Agility_Cleaned.csv", "C. Agility Development")
p4 <- create_pub_panel("E4_Flexibility_Cleaned.csv", "D. Flexibility Development")
p5 <- create_pub_panel("E5_InjuryPrev_Cleaned.csv", "E. Reduce WR Related Injuries")
p6 <- create_pub_panel("E6_SpecificCond_Cleaned.csv", "F. WR Specific Conditioning")
p7 <- create_pub_panel("E7_Performance_Cleaned.csv", "G. WR Performance Enhancement")
# p8_info <- create_legend_panel()

# 5. ASSEMBLE LAYOUT (3 Columns: 3 | 2 | 2)
# Install cowplot if needed
# install.packages("cowplot")
library(cowplot)

# Temporarily activate legend on one panel to extract it
p1_leg <- p1 + theme(
  legend.position = "bottom",
  legend.text = element_text(size = 11, face = "bold"),
  legend.key.size = unit(0.5, "cm")
)

# Extract legend
legend <- cowplot::get_legend(p1_leg)

# Main layout without legends
final_layout <- (p1 / p2 / p3) | (p4 / p5) | (p6 / p7)

# Add title and subtitle annotations
final_plot_main <- final_layout +
  plot_annotation(
    title = 'Landscape of exercise preferences reported by WR Para-athletes and coaches',
    subtitle = 'Most frequently cited exercises (Total citations ≥ 2) per category',
    theme = theme(
      plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
      plot.subtitle = element_text(size = 12, hjust = 0.5, color = "grey30")
    )
  ) &
  theme(plot.margin = margin(8, 15, 8, 15))

# Combine plot + legend
final_plot <- cowplot::plot_grid(
  final_plot_main, legend,
  ncol = 1,
  rel_heights = c(1, 0.05)
)

# 6. SAVE OUTPUT
# Saving as PNG (High Res) and PDF (Vector)
ggsave("Paper 1/df_analysis/04_exercises/FIG2_FINAL_N2.png", 
       final_plot, width = 16, height = 10, dpi = 300)

ggsave("Paper 1/df_analysis/04_exercises/FIG2_FINAL_N2.pdf", 
       final_plot, width = 16, height = 10)

# Display in RStudio
print(final_plot)


# 7. QUICK CHECK: COUNT OF EXERCISES PER CATEGORY
read_csv("Paper 1/df_analysis/04_exercises/E8_Mobility_Cleaned.csv") %>%
  count(Exercise_Category, sort = TRUE) %>%
  print()


# ==============================================================================
# 6. SAVE OUTPUT FOR JSCR (TIFF & EPS)
# ==============================================================================

# Dimensions: A4 paisagem é aprox 11.7 x 8.3 in.

# A. TIFF FORMAT (High Resolution Raster)
ggsave("Paper 1/df_analysis/04_exercises/Figure_2.tiff",
       final_plot, 
       width = 16, 
       height = 10, 
       units = "in",
       dpi = 600,            
       compression = "lzw")  

# B. EPS FORMAT (Vector - Best for Editing/Printing)
ggsave("Paper 1/df_analysis/04_exercises/Figure_2.eps",
       final_plot, 
       width = 16, 
       height = 10, 
       units = "in",
       device = cairo_ps)    

message("Figures saved successfully in TIFF and EPS formats.")
