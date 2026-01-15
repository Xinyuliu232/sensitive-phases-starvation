
############################################################
# Sensitive phases project
# Reproducible paths + auto-save PNG figures
############################################################

### Load necessities ###
library(ggplot2)
library(readxl)
library(ggthemes)
library(showtext)
library(colorspace)
library(multcompView)
library(dplyr)
library(stringr)
library(dunn.test)
library(ggpubr)
library(survminer)
library(survival)
library(ggdist)

showtext_auto()



DATA_DIR <- "data"
FIG_DIR  <- "figures"
dir.create(FIG_DIR, showWarnings = FALSE, recursive = TRUE)

data_file <- file.path(DATA_DIR, "Worksheet_sensitive_phases_starvation_processed.csv")

# save ggplot as PNG
save_png <- function(p, filename, width = 10, height = 7, dpi = 300) {
  out <- file.path(FIG_DIR, filename)
  ggsave(filename = out, plot = p, width = width, height = height, dpi = dpi, bg = "white")
  message("Saved: ", out)
}

############################################################
# 1) Load data
############################################################
adultdata <- read.csv(
  "Worksheet_sensitive_phases_starvation_processed.csv",
  sep = ";",
  header = TRUE,
  na.strings = c("NA", "", " ")
)

treatmentscol <- c("chartreuse4", "gold", "darkorange3", "red")
sexcol       <- c("bisque4", "gray10")
larvalcol    <- c("chartreuse4", "darkorange3")

adultdata$treatment   <- factor(adultdata$treatment, levels = c("no", "adult", "larval", "double"))
adultdata$larvaltreat <- factor(adultdata$larvaltreat, levels = c("no", "larval"))

adultmale_subset   <- subset(adultdata, s == "male"   & !is.na(s))
adultfemale_subset <- subset(adultdata, s == "female" & !is.na(s))

############################################################
# 2) Summary tables 
############################################################
summary_table_n      <- table(adultdata$treatment, adultdata$s)
summary_table_behav  <- table(adultdata$treatment, adultdata$s, complete.cases(adultdata$velocity))
summary_table_metabo <- table(adultdata$treatment, adultdata$s, complete.cases(adultdata$lipidmasstotal))

print(summary_table_n)
print(summary_table_behav)
print(summary_table_metabo)

############################################################
# 3) Plots (ALL saved as PNG)
############################################################

# Distance moved
p_dist <- ggplot(adultdata %>% filter(!is.na(s)),
                 aes(x = treatment, y = distancemoved, color = treatment)) +
  scale_color_manual(values = treatmentscol) +
  labs(title = "Distance moved", x = "Starvation treatment", y = "Distance moved (cm)") +
  theme_bw(base_size = 22) +
  theme(legend.position = "none") +
  theme(text = element_text(family = "Calibri")) +
  geom_boxplot(aes(fill = after_scale(desaturate(lighten(color, .6), .6))),
               size = 1, outlier.shape = NA) +
  geom_jitter() +
  facet_wrap(~s, scales = "fixed", ncol = 2)

save_png(p_dist, "adult_distance_moved_by_treatment_sex.png", width = 12, height = 7)

# Immobility duration (box + jitter)
p_immobile_box <- ggplot(adultdata %>% filter(!is.na(s)),
                         aes(x = treatment, y = immobileduration, color = treatment)) +
  scale_color_manual(values = treatmentscol) +
  labs(title = "Immobility duration", x = "Starvation treatment", y = "Time spent immobile (s)") +
  scale_y_continuous(limits = c(950, 3700)) +
  theme_bw(base_size = 22) +
  theme(legend.position = "none") +
  geom_boxplot(aes(fill = after_scale(desaturate(lighten(color, .6), .6))),
               size = 1, outlier.shape = NA) +
  geom_jitter() +
  facet_wrap(~s, scales = "fixed", ncol = 2)

save_png(p_immobile_box, "adult_immobility_duration_box_by_treatment_sex.png", width = 12, height = 7)

# Immobility duration (dots + median)
p_immobile_dots <- ggplot(adultdata %>% filter(!is.na(s)),
                          aes(x = treatment, y = immobileduration, color = treatment)) +
  scale_color_manual(values = treatmentscol) +
  labs(title = "Immobility duration (dots)", x = "Starvation treatment", y = "Time spent immobile (s)") +
  theme_bw(base_size = 22) +
  theme(legend.position = "none") +
  geom_point(size = 3, alpha = 0.7) +
  stat_summary(fun = median, geom = "point", size = 2, shape = 21, fill = "black", color = "white") +
  facet_wrap(~s, scales = "fixed", ncol = 2)

save_png(p_immobile_dots, "adult_immobility_duration_dots_by_treatment_sex.png", width = 12, height = 7)

# Spearman correlation 
calculate_spearman <- function(data) {
  data %>%
    filter(!is.na(distancemoved) & !is.na(immobileduration)) %>%
    group_by(treatment) %>%
    summarize(
      correlation = cor(distancemoved, immobileduration, method = "spearman"),
      p_value = cor.test(distancemoved, immobileduration, method = "spearman")$p.value,
      .groups = "drop"
    )
}
spearman_correlations <- calculate_spearman(adultdata)
print(spearman_correlations)

# Treatment body mass
p_treat_mass <- ggplot(adultdata %>% filter(!is.na(s)),
                       aes(x = treatment, y = treatmentweight, color = treatment)) +
  scale_color_manual(values = treatmentscol) +
  labs(title = "Treatment body mass", x = "Starvation treatment", y = "Body mass (mg)") +
  theme_bw(base_size = 22) +
  theme(legend.position = "none") +
  theme(text = element_text(family = "Calibri")) +
  geom_boxplot(aes(fill = after_scale(desaturate(lighten(color, .6), .6))),
               size = 1, outlier.shape = NA) +
  geom_jitter() +
  facet_wrap(~s, scales = "fixed", ncol = 2)

save_png(p_treat_mass, "adult_treatment_body_mass_by_treatment_sex.png", width = 12, height = 7)

# Mass change
p_masschange <- ggplot(adultdata %>% filter(!is.na(s)),
                       aes(x = treatment, y = masschange, color = treatment)) +
  scale_color_manual(values = treatmentscol) +
  scale_y_continuous(limits = c(-4.5, 2.5)) +
  labs(title = "Mass change after treatment", x = "Starvation treatment", y = "Body mass change (mg)") +
  theme_bw(base_size = 22) +
  theme(legend.position = "none") +
  theme(text = element_text(family = "Calibri")) +
  geom_boxplot(aes(fill = after_scale(desaturate(lighten(color, .6), .6))),
               size = 1, outlier.shape = NA) +
  expand_limits(y = -2.2) +
  geom_jitter() +
  facet_wrap(~s, scales = "fixed", ncol = 2)

save_png(p_masschange, "adult_mass_change_by_treatment_sex.png", width = 12, height = 7)

# Adult lifespan
p_adultlife <- ggplot(adultdata %>% filter(!is.na(s)),
                      aes(x = treatment, y = adultlifespan, color = treatment)) +
  scale_color_manual(values = treatmentscol) +
  labs(title = "Adult lifespan", x = "Starvation treatment", y = "Lifespan (d)") +
  theme_bw(base_size = 22) +
  theme(legend.position = "none") +
  theme(text = element_text(family = "Calibri")) +
  geom_boxplot(aes(fill = after_scale(desaturate(lighten(color, .6), .6))),
               size = 1, outlier.shape = NA) +
  geom_jitter() +
  facet_wrap(~s, scales = "fixed", ncol = 2)

save_png(p_adultlife, "adult_lifespan_by_treatment_sex.png", width = 12, height = 7)

# Total lifespan
p_totallife <- ggplot(adultdata %>% filter(!is.na(s)),
                      aes(x = treatment, y = totallifespan, color = treatment)) +
  scale_color_manual(values = treatmentscol) +
  labs(title = "Total lifespan", x = "Starvation treatment", y = "Lifespan (d)") +
  theme_bw(base_size = 22) +
  theme(legend.position = "none") +
  theme(text = element_text(family = "Calibri")) +
  geom_boxplot(aes(fill = after_scale(desaturate(lighten(color, .6), .6))),
               size = 1, outlier.shape = NA) +
  geom_jitter() +
  facet_wrap(~s, scales = "fixed", ncol = 2)

save_png(p_totallife, "total_lifespan_by_treatment_sex.png", width = 12, height = 7)

# Larval development (raincloud)
p_larval_dev <- ggplot(adultdata %>% filter(!is.na(s)),
                       aes(x = larvaltreat, y = larvalstageduration,
                           fill = larvaltreat, color = larvaltreat)) +
  scale_fill_manual(values = larvalcol) +
  scale_color_manual(values = larvalcol) +
  labs(title = "Larval development", x = "Starvation treatment", y = "Time until eonymph stage (d)") +
  scale_y_continuous(limits = c(11, 17.5)) +
  theme_bw(base_size = 22) +
  theme(legend.position = "none") +
  theme(text = element_text(family = "Calibri")) +
  geom_point(size = 1.5, alpha = 0.5,
             position = position_jitter(width = 0.1, height = 0.1)) +
  ggdist::stat_halfeye(justification = -0.3, width = 0.8, adjust = 1, .width = 0) +
  stat_summary(fun = median, geom = "point", shape = 21, size = 3) +
  facet_wrap(~s, scales = "fixed", ncol = 2)

save_png(p_larval_dev, "larval_development_raincloud_by_sex.png", width = 12, height = 7)

# Pupal stage duration
p_pupal <- ggplot(adultdata %>% filter(!is.na(s)),
                  aes(x = larvaltreat, y = pupaestageduration, color = larvaltreat)) +
  scale_color_manual(values = larvalcol) +
  labs(title = "Pupal stage duration", x = "Starvation treatment", y = "Days in pupal stage (d)") +
  theme_bw(base_size = 22) +
  theme(legend.position = "none") +
  theme(text = element_text(family = "Calibri")) +
  geom_violin(aes(fill = after_scale(desaturate(lighten(color, .6), .6))),
              linewidth = 1) +
  geom_point() +
  facet_wrap(~s, scales = "fixed", ncol = 2)

save_png(p_pupal, "pupal_stage_duration_violin_by_sex.png", width = 12, height = 7)

# Initial adult body mass (raincloud)
p_init_mass <- ggplot(adultdata %>% filter(!is.na(s)),
                      aes(x = larvaltreat, y = hatchingweightmg,
                          fill = larvaltreat, color = larvaltreat)) +
  scale_fill_manual(values = larvalcol) +
  scale_color_manual(values = larvalcol) +
  labs(title = "Initial adult body mass", x = "Starvation treatment", y = "Initial adult body mass (mg)") +
  scale_y_continuous(limits = c(6.8, 24.5)) +
  theme_bw(base_size = 22) +
  theme(legend.position = "none") +
  theme(text = element_text(family = "Calibri")) +
  geom_point(size = 1.5, alpha = 0.5,
             position = position_jitter(seed = 1, width = 0.1)) +
  ggdist::stat_halfeye(justification = -0.3, width = 0.6, adjust = 0.5, .width = 0) +
  stat_summary(fun = median, geom = "point", shape = 21, size = 3) +
  facet_wrap(~s, scales = "fixed", ncol = 2)

save_png(p_init_mass, "initial_adult_body_mass_raincloud_by_sex.png", width = 12, height = 7)

############################################################
# 4) Kaplan–Meier survival (save PNG from ggsurvplot)
############################################################

# Females
fitfemales <- survfit(Surv(adultlifespan) ~ treatment, data = adultfemale_subset)

survplotfemales <- ggsurvplot(
  fitfemales,
  pval = TRUE,
  conf.int = TRUE,
  conf.int.style = "step",
  xlab = "Adult lifespan (d)",
  size = 1.5,
  xlim = c(0.9, 22.5),
  ggtheme = theme_bw(),
  risk.table = FALSE,
  risk.table.y.text.col = TRUE,
  risk.table.y.text = FALSE,
  ncensor.plot = FALSE,
  legend.labs = c("No starvation", "Adult starvation", "Larval starvation", "Double starvation"),
  palette = c("chartreuse4", "gold", "darkorange3", "red")
)

save_png(survplotfemales$plot, "KM_females_adult_lifespan.png", width = 10, height = 7)

surv_difffemales <- survdiff(Surv(adultlifespan) ~ treatment, data = adultfemale_subset)
print(surv_difffemales)

resfemales <- pairwise_survdiff(Surv(adultlifespan) ~ treatment,
                                data = adultfemale_subset,
                                p.adjust.method = "BH", rho = 0)
print(resfemales)

# Males
fitmales <- survfit(Surv(adultlifespan) ~ treatment, data = adultmale_subset)

survplotmales <- ggsurvplot(
  fitmales,
  pval = TRUE,
  conf.int = TRUE,
  conf.int.style = "step",
  xlab = "Adult lifespan (d)",
  size = 1.5,
  xlim = c(0.9, 22.5),
  ggtheme = theme_bw(),
  risk.table = FALSE,
  risk.table.y.text.col = TRUE,
  risk.table.y.text = FALSE,
  ncensor.plot = FALSE,
  legend.labs = c("No starvation", "Adult starvation", "Larval starvation", "Double starvation"),
  palette = c("chartreuse4", "gold", "darkorange3", "red")
)

save_png(survplotmales$plot, "KM_males_adult_lifespan.png", width = 10, height = 7)

surv_diffmales <- survdiff(Surv(adultlifespan) ~ treatment, data = adultmale_subset)
print(surv_diffmales)

resmales <- pairwise_survdiff(Surv(adultlifespan) ~ treatment,
                              data = adultmale_subset,
                              p.adjust.method = "BH", rho = 0)
print(resmales)

############################################################
# 5) Metabolism plots (save PNG)
############################################################

p_lipids_total <- ggplot(adultdata %>% filter(!is.na(s)),
                         aes(x = treatment, y = lipidmasstotal, color = treatment)) +
  scale_color_manual(values = treatmentscol) +
  labs(title = "Lipid mass per individual", x = "Starvation treatment", y = "Lipids (mg)") +
  theme_bw(base_size = 22) +
  theme(legend.position = "none") +
  theme(text = element_text(family = "Calibri")) +
  geom_boxplot(aes(fill = after_scale(desaturate(lighten(color, .6), .6))),
               size = 1, outlier.shape = NA) +
  geom_jitter() +
  facet_wrap(~s, scales = "fixed", ncol = 2)

save_png(p_lipids_total, "lipids_mass_total_by_treatment_sex.png", width = 12, height = 7)

p_lipids_permass <- ggplot(adultdata %>% filter(!is.na(s)),
                           aes(x = treatment, y = lipidmgmgbodymass, color = treatment)) +
  scale_color_manual(values = treatmentscol) +
  labs(title = "Lipid mass per body mass", x = "Starvation treatment", y = "Lipid mass per body mass (mg/mg)") +
  theme_bw(base_size = 22) +
  theme(legend.position = "none") +
  theme(text = element_text(family = "Calibri")) +
  geom_boxplot(aes(fill = after_scale(desaturate(lighten(color, .6), .6))),
               size = 1, outlier.shape = NA) +
  geom_jitter() +
  facet_wrap(~s, scales = "fixed", ncol = 2)

save_png(p_lipids_permass, "lipids_per_body_mass_by_treatment_sex.png", width = 12, height = 7)

p_carbs_total <- ggplot(adultdata %>% filter(!is.na(s)),
                        aes(x = treatment, y = carbsmasstotal, color = treatment)) +
  scale_color_manual(values = treatmentscol) +
  labs(title = "Carbs mass per individual", x = "Starvation treatment", y = "Total carbs (mg)") +
  theme_bw(base_size = 22) +
  theme(legend.position = "none") +
  theme(text = element_text(family = "Calibri")) +
  geom_boxplot(aes(fill = after_scale(desaturate(lighten(color, .6), .6))),
               size = 1, outlier.shape = NA) +
  geom_jitter() +
  facet_wrap(~s, scales = "fixed", ncol = 2)

save_png(p_carbs_total, "carbs_mass_total_by_treatment_sex.png", width = 12, height = 7)

p_carbs_permass <- ggplot(adultdata %>% filter(!is.na(s)),
  aes(x = treatment, y = carbsmgmgbodymass, color = treatment)) +
  scale_color_manual(values = treatmentscol) +
  labs(title = "Carbs mass per body mass", x = "Starvation treatment", y = "Carb. mass per body mass (mg/mg)") +
  theme_bw(base_size = 22) +
  theme(legend.position = "none") +
  theme(text = element_text(family = "Calibri")) +
  geom_boxplot(aes(fill = after_scale(desaturate(lighten(color, .6), .6))),
               size = 1, outlier.shape = NA) +
  geom_jitter() +
  facet_wrap(~s, scales = "fixed", ncol = 2)

save_png(p_carbs_permass, "carbs_per_body_mass_by_treatment_sex.png", width = 12, height = 7)

# Metabolism extra (male box + female dots)
p_lipids_extra <- ggplot(mapping = aes(x = treatment, y = lipidmgmgbodymass, color = treatment)) +
  geom_boxplot(data = adultmale_subset,
               aes(fill = after_scale(desaturate(lighten(color, .6), .6))),
               size = 1, outlier.shape = NA) +
  geom_jitter(data = adultmale_subset) +
  geom_jitter(data = adultfemale_subset, width = 0.2, size = 2) +
  stat_summary(data = adultfemale_subset, fun = median, geom = "point",
               color = "black", size = 1.5, alpha = 0.7) +
  scale_color_manual(values = treatmentscol) +
  scale_fill_manual(values = treatmentscol) +
  scale_y_continuous(limits = c(0.0, 1.17)) +
  labs(title = "Lipid mass per body mass", x = "Starvation treatment", y = "Lipid mass per body mass (mg/mg)") +
  theme_bw(base_size = 22) +
  theme(text = element_text(family = "Calibri")) +
  theme(legend.position = "none") +
  facet_wrap(~s, scales = "fixed", ncol = 2)

save_png(p_lipids_extra, "lipids_per_body_mass_male_box_female_dots.png", width = 12, height = 7)

p_carbs_extra <- ggplot(mapping = aes(x = treatment, y = carbsmgmgbodymass, color = treatment)) +
  geom_boxplot(data = adultmale_subset,
               aes(fill = after_scale(desaturate(lighten(color, .6), .6))),
               size = 1, outlier.shape = NA) +
  geom_jitter(data = adultmale_subset) +
  geom_jitter(data = adultfemale_subset, width = 0.2, size = 2) +
  stat_summary(data = adultfemale_subset, fun = median, geom = "point",
               color = "black", size = 1.5, alpha = 0.7) +
  scale_color_manual(values = treatmentscol) +
  scale_fill_manual(values = treatmentscol) +
  scale_y_continuous(limits = c(0.0, 0.105)) +
  labs(title = "Carbs mass per body mass", x = "Starvation treatment", y = "Carb. mass per body mass (mg/mg)") +
  theme_bw(base_size = 22) +
  theme(text = element_text(family = "Calibri")) +
  theme(legend.position = "none") +
  facet_wrap(~s, scales = "fixed", ncol = 2)

save_png(p_carbs_extra, "carbs_per_body_mass_male_box_female_dots.png", width = 12, height = 7)
########################################################
################ AUTOMATED STATISTICAL TESTING #########
########################################################

library(dplyr)
library(dunn.test)
library(multcompView)
library(stringr)

# Create output directory
dir.create("results", showWarnings = FALSE)

########################################################
# 1) Helper functions to tidy test outputs
########################################################

# Wilcoxon rank-sum test (2 groups)
tidy_wilcox <- function(test, variable, sex) {
  data.frame(
    variable = variable,
    sex = sex,
    test = "Wilcoxon rank-sum",
    statistic = as.numeric(test$statistic),
    p_value = test$p.value,
    stringsAsFactors = FALSE
  )
}

# Kruskal–Wallis test (4 groups)
tidy_kruskal <- function(test, variable, sex) {
  data.frame(
    variable = variable,
    sex = sex,
    test = "Kruskal-Wallis",
    statistic = as.numeric(test$statistic),
    df = as.numeric(test$parameter),
    p_value = test$p.value,
    stringsAsFactors = FALSE
  )
}

# Dunn post hoc test
tidy_dunn <- function(dunn_obj, variable, sex) {
  data.frame(
    variable = variable,
    sex = sex,
    comparison = dunn_obj$comparisons,
    Z = dunn_obj$Z,
    p_unadjusted = dunn_obj$P,
    p_adjusted = dunn_obj$P.adjusted,
    stringsAsFactors = FALSE
  )
}

# Convert Dunn test results to compact letter display
calculate_letters <- function(dunn_obj, variable, sex) {
  
  df <- dunn_obj %>%
    as.data.frame() %>%
    mutate(
      significant = if_else(P.adjusted >= 0.05, 1, 0),
      comparisons = str_remove_all(comparisons, " ")
    )
  
  letter_vector <- setNames(df$significant, df$comparisons)
  letters <- multcompLetters(letter_vector)
  
  data.frame(
    variable = variable,
    sex = sex,
    treatment = names(letters$Letters),
    letter = letters$Letters,
    stringsAsFactors = FALSE
  )
}

########################################################
# 2) Data containers and test configuration
########################################################

# Split data by sex
df_clean <- adultdata %>% filter(!is.na(s))

sex_data <- split(df_clean, df_clean$s)



# Variables tested with Wilcoxon (2 larval treatments)
wilcox_vars <- list(
  larvalstageduration = "larvaltreat",
  hatchingweightmg    = "larvaltreat"
)

# Variables tested with Kruskal–Wallis + Dunn (4 treatments)
kruskal_vars <- c(
  "treatmentweight",
  "masschange",
  "totallifespan",
  "adultlifespan",
  "distancemoved",
  "immobileduration",
  "lipidmgmgbodymass",
  "carbsmgmgbodymass"
)

########################################################
# 3) Run all tests automatically
########################################################

overall_results <- list()
posthoc_results <- list()
letters_results <- list()

### ---- Wilcoxon tests ---- ###
for (sex in names(sex_data)) {
  
  df <- sex_data[[sex]]
  
  for (var in names(wilcox_vars)) {
    
    grouping_var <- wilcox_vars[[var]]
    
    test <- wilcox.test(
      df[[var]] ~ df[[grouping_var]],
      exact = FALSE
    )
    
    overall_results[[paste(var, sex, sep = "_")]] <-
      tidy_wilcox(test, var, sex)
  }
}

### ---- Kruskal–Wallis + Dunn tests ---- ###
for (sex in names(sex_data)) {
  
  df <- sex_data[[sex]]
  
  for (var in kruskal_vars) {
    
    # Kruskal–Wallis test
    kw_test <- kruskal.test(df[[var]] ~ df$treatment)
    
    overall_results[[paste(var, sex, sep = "_")]] <-
      tidy_kruskal(kw_test, var, sex)
    
    # Dunn post hoc test
    dunn_test <- dunn.test(
      x = df[[var]],
      g = df$treatment,
      method = "bonferroni"
    )
    
    posthoc_results[[paste(var, sex, sep = "_")]] <-
      tidy_dunn(dunn_test, var, sex)
    
    letters_results[[paste(var, sex, sep = "_")]] <-
      calculate_letters(dunn_test, var, sex)
  }
}

########################################################
# 4) Combine results and save to files
########################################################

overall_df <- bind_rows(overall_results)
posthoc_df <- bind_rows(posthoc_results)
letters_df <- bind_rows(letters_results)

write.csv(
  overall_df,
  "results/overall_tests_summary.csv",
  row.names = FALSE
)

write.csv(
  posthoc_df,
  "results/posthoc_dunn_tests.csv",
  row.names = FALSE
)

write.csv(
  letters_df,
  "results/significance_letters.csv",
  row.names = FALSE
)

