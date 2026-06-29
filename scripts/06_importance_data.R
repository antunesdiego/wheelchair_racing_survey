# =============================================================================
# FINAL SCRIPT: DATA IMPORT, CLEANING, AND N-COUNT VISUALIZATION
# =============================================================================

# 1. SETUP & LIBRARIES

library(dplyr)
library(tidyr)
library(ggplot2)
library(patchwork)
library(RColorBrewer)

# 2. LOAD DATA
# -----------------------------------------------------------------------------
file_path <- "Paper 1/df_analysis/03_clean_data/clean_analysis_data.csv"

if(!file.exists(file_path)) {
  stop("ERROR: File not found at: ", file_path)
}

df <- read.csv(file_path, stringsAsFactors = FALSE)
print("Data loaded successfully.")

# =============================================================================
# 3. DEFINE SCALES & CONVERT COLUMNS
# =============================================================================

# --- SCALE A: IMPORTANCE (1-5) ---
imp_levels <- 1:5
imp_labels <- c("Not important", "Somewhat important", "Moderately important", "Important", "Very important")

cols_racing <- c("D1_Imp_PushPhase", "D1_Imp_RecoveryPhase", "D1_Imp_StrokeFreq", "D1_Imp_Tactics", 
                 "D1_Imp_Start", "D1_Imp_StrokeAmp", "D1_Imp_SpecificCond", "D1_Imp_GeneralPerf")
cols_physical <- c("D2_Imp_Strength", "D2_Imp_SpeedPower", "D2_Imp_Agility", 
                   "D2_Imp_Flexibility", "D2_Imp_InjuryPrev", "D2_Imp_Rehab", "D2_Imp_Mobility")

all_imp_cols <- c(cols_racing, cols_physical)
valid_imp_cols <- intersect(names(df), all_imp_cols)

df[valid_imp_cols] <- lapply(df[valid_imp_cols], function(x) {
  factor(x, levels = imp_levels, labels = imp_labels, ordered = TRUE)
})

# --- SCALE B: FREQUENCY (1-5) ---
freq_levels <- 1:5
freq_labels <- c("Never", "Rarely", "Sometimes", "Commonly", "Always")

cols_source <- c("C5_Source_Coach", "C5_Source_Athletes", "C5_Source_Trainer", 
                 "C5_Source_Therapist", "C5_Source_Physio", "C5_Source_Courses", 
                 "C5_Source_Articles", "C5_Source_SocialMedia", "C5_Source_Books")

valid_source_cols <- intersect(names(df), cols_source)

df[valid_source_cols] <- lapply(df[valid_source_cols], function(x) {
  factor(x, levels = freq_levels, labels = freq_labels, ordered = TRUE)
})

# =============================================================================
# 4. PREPARE SUBSETS 
# =============================================================================

# A. Sources of Knowledge
df_sources <- df %>%
  select(any_of(cols_source)) %>%
  rename(
    "Coach" = C5_Source_Coach,
    "Other Athletes" = C5_Source_Athletes,
    "S&C Coach" = C5_Source_Trainer,
    "Therapist" = C5_Source_Therapist,
    "Physiologist" = C5_Source_Physio,
    "Courses" = C5_Source_Courses,
    "Scientific Papers" = C5_Source_Articles,
    "Social Media" = C5_Source_SocialMedia,
    "Books" = C5_Source_Books
  )

# B. Racing Importance
df_racing <- df %>%
  select(any_of(cols_racing)) %>%
  rename(
    "Push Phase" = D1_Imp_PushPhase, "Recovery Phase" = D1_Imp_RecoveryPhase, "Stroke Frequency" = D1_Imp_StrokeFreq,
    "Tactics" = D1_Imp_Tactics, "Start" = D1_Imp_Start,
    "Stroke Amplitude" = D1_Imp_StrokeAmp, "WR Conditioning Specific" = D1_Imp_SpecificCond,
    "WR Performance" = D1_Imp_GeneralPerf
  )

# C. Physical Importance
df_physical <- df %>%
  select(any_of(cols_physical)) %>%
  rename(
    "Strength" = D2_Imp_Strength, "Speed/Power" = D2_Imp_SpeedPower,
    "Agility" = D2_Imp_Agility, "Flexibility" = D2_Imp_Flexibility,
    "Injury Prev." = D2_Imp_InjuryPrev, "Rehab" = D2_Imp_Rehab,
    "Mobility" = D2_Imp_Mobility
  )

# =============================================================================
# 5. PLOTTING FUNCTION 
# =============================================================================

plot_n_bars <- function(dataset, title_text, legend_title) {
  
  plot_data <- dataset %>%
    pivot_longer(everything(), names_to = "Question", values_to = "Response") %>%
    filter(!is.na(Response)) %>%
    group_by(Question, Response) %>%
    summarise(n = n(), .groups = 'drop') %>%
    mutate(Question = factor(Question, levels = rev(names(dataset))))
  
  ggplot(plot_data, aes(x = n, y = Question, fill = Response)) +
    
    # Bars and Text
    geom_col(position = position_stack(reverse = TRUE), width = 0.75, color = "white", size = 0.2) +
    
    geom_text(aes(label = n), 
              position = position_stack(vjust = 0.5, reverse = TRUE), 
              size = 3, fontface = "bold", color = "black") +
    
    # Colors
    scale_fill_brewer(palette = "RdYlBu", direction = 1, name = legend_title, drop = FALSE) +
    
    # --- LEGEND ADJUSTMENT ---
    guides(fill = guide_legend(
      reverse = FALSE,
      nrow = 2,            # Forces 2 rows (Compact and safe)
      byrow = TRUE,        # Fills row by row
      title.position = "top"
    )) + 
    
    # Labels & Theme
    labs(title = title_text, x = "Count (n)", y = "") +
    theme_minimal() +
    theme(
      plot.title = element_text(size = 11, face = "bold", hjust = 0),
      axis.text.y = element_text(size = 10, color = "black"),
      
      # Legend Position
      legend.position = "bottom",
      legend.justification = "left", # Aligns to the left
      legend.box.just = "left",
      
      # MARGIN RESET (Crucial for Figure A)
      # We set left margin to 0 so it aligns with the axis but doesn't go off-page.
      legend.margin = margin(t = 0, b = 0, l = 0, r = 0), 
      
      legend.text = element_text(size = 8),
      legend.title = element_text(size = 9, face = "bold"),
      legend.key.size = unit(0.4, "cm"),
      
      panel.grid.major.y = element_blank(),
      panel.grid.minor = element_blank()
    )
}

# =============================================================================
# 6. GENERATE PLOTS 
# =============================================================================

# A. Sources 
p_sources  <- plot_n_bars(df_sources, "A. Sources of Knowledge", "Frequency")

# B. Racing (No title in legend to save space)
p_racing   <- plot_n_bars(df_racing, "B. Importance for WR Components", NULL)

# C. Physical (No title in legend)
p_physical <- plot_n_bars(df_physical, "C. Importance for Physical Components", NULL)

# Minor adjustment: remove X-axis title from the middle plot to clean up the look
p_racing <- p_racing + theme(axis.title.x = element_blank())

# =============================================================================
# 7. COMBINE AND SAVE 
# =============================================================================

# Layout: Sources (Left) | Racing (Top Right) / Physical (Bottom Right)
final_fig <- (p_sources | (p_racing / p_physical)) +
  plot_annotation(
    tag_levels = 'A', 
    theme = theme(
      plot.margin = margin(10, 10, 10, 10),
      plot.tag = element_text(size = 14, face = "bold")
    )
  )

# Define Output Path
output_path <- "Paper 1/df_analysis/04_outputs"
dir.create(output_path, showWarnings = FALSE)

# Save TIFF (600 dpi, LZW compression - Standard for Printing)
ggsave(file.path(output_path, "Figure_2.tiff"),
       final_fig, 
       width = 11.7, 
       height = 8.3, 
       units = "in",
       dpi = 600, 
       compression = "lzw", 
       bg = "white")

# Save EPS (Vector - Standard for Editing)
ggsave(file.path(output_path, "Figure_2.eps"),
       final_fig, 
       width = 11.7, 
       height = 8.3, 
       units = "in",
       device = cairo_ps, 
       bg = "white")

message("Figure 2 successfully updated and saved.")
print(final_fig)
