


### sensitive phases project data
### Load necesseties ###

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

setwd("~/Desktop/sensitivity_phase")



#### Define data colors and fonts used ###
########### Adult data
adultdata <- read.csv(
  "Worksheet_sensitive_phases_starvation_processed.csv",
  sep=";",
  header= TRUE,
  na.strings = c("NA",""," ")
)

treatmentscol <- c("chartreuse4", "gold","darkorange3","red" )
sexcol <- c("bisque4", "gray10" )
larvalcol <- c("chartreuse4", "darkorange3")

### giving the general order
adultdata$treatment <- factor(adultdata$treatment, levels = c("no", "adult", "larval", "double"))
adultdata$larvaltreat <- factor(adultdata$larvaltreat, levels = c("no","larval"))
# Subset data for both sexes
adultmale_subset <- subset(adultdata, s == 'male' & !is.na(s))
adultfemale_subset <- subset(adultdata, s == 'female' & !is.na(s))

##general overview about n 
summary_table_n <- table(adultdata$treatment, adultdata$s)
summary_table_n

summary_table_behav <- table(adultdata$treatment, adultdata$s, complete.cases(adultdata$velocity))
summary_table_behav

summary_table_metabo <- table(adultdata$treatment, adultdata$s, complete.cases(adultdata$lipidmasstotal))
summary_table_metabo

####################################################################### facet wrap - s summary Distance moved
#########changed from boxplot to dotplot
ggplot(adultdata %>% filter(!is.na(s)), aes(x = treatment, y = distancemoved, color = treatment)) +
  scale_color_manual(values = treatmentscol) +
  labs(title = "Distance moved", x = "Starvation treatment", y = "Distance moved (cm)") +
  theme_bw(base_size = 22) +
  theme(legend.position = "none") +
  theme(text = element_text(family = "Calibri")) +
  geom_boxplot(aes(color = treatment, fill = after_scale(desaturate(lighten(color, .6), .6))), size = 1, outlier.shape = NA)+
  geom_jitter()+
  facet_wrap(~s, scales = "fixed", ncol = 2)
####################################################################### facet wrap

####################################################################### facet wrap - s summary Immobility duration
ggplot(adultdata %>% filter(!is.na(s)), aes(x = treatment, y = immobileduration, color = treatment, )) +
  scale_color_manual(values = treatmentscol) +
  labs(title = "Immobility duration", x = "Starvation treatment", y = "Time spent immobile (s)") +
  scale_y_continuous(limits = c(950,3700))+
  theme_bw(base_size = 22) +
  theme(legend.position = "none") +
  geom_boxplot(aes(color = treatment, fill = after_scale(desaturate(lighten(color, .6), .6))), size = 1, outlier.shape = NA) +
  geom_jitter() +
  facet_wrap(~s, scales = "fixed", ncol = 2)
####################################################################### second version only dots
#ggplot(adultdata %>% filter(!is.na(s)), aes(x = treatment, y = immobileduration, color = treatment)) +
#  scale_color_manual(values = treatmentscol) +
  labs(title = "Immobility duration", x = "Starvation treatment", y = "Time spent immobile (s)") +
  theme_bw(base_size = 22) +
  theme(legend.position = "none") +
  geom_point(aes(color = treatment), size = 3, alpha = 7/10) +
  stat_summary(geom = "point",fun = median,col = "white" ,size = 2,shape = 21, fill = "black")+
  facet_wrap(~s, scales = "fixed", ncol = 2)
#######################
#################################################################################
  #Spearman rank correlation between distance moved and immobile duration
  #Spearman rank correlation between distance moved and immobile duration
  calculate_spearman <- function(data) {
    data %>%
      filter(!is.na(distancemoved) & !is.na(immobileduration)) %>%
      group_by(treatment) %>%
      summarize(
        correlation = cor(distancemoved, immobileduration, method = "spearman"),
        p_value = cor.test(distancemoved, immobileduration, method = "spearman")$p.value
      )
  }
  
  spearman_correlations <- calculate_spearman(adultdata)
  print(spearman_correlations)
  spearman_correlations <- calculate_spearman(adultdata)
  print("Spearman's rank correlation for the entire dataset:")
  print(spearman_correlations)

####################################################################### treatment body mass facet wrap
ggplot(adultdata %>% filter(!is.na(s)), aes(x = treatment, y = treatmentweight, color = treatment)) +
  scale_color_manual(values = treatmentscol) +
  labs(title = "Treatment body mass", x = "Starvation treatment", y = "Body mass (mg)") +
  theme_bw(base_size = 22) +
  theme(legend.position = "none") +
  theme(text = element_text(family = "Calibri")) +
  geom_boxplot(aes(color = treatment, fill = after_scale(desaturate(lighten(color, .6), .6))), size = 1, outlier.shape = NA) +
  geom_jitter() +
  facet_wrap(~s, scales = "fixed", ncol = 2)
####################################################################### facet wrap
####################################################################### mass change
ggplot(adultdata %>% filter(!is.na(s)), aes(x = treatment, y = masschange, color = treatment)) +
  scale_color_manual(values = treatmentscol) +
  scale_y_continuous(limits = c(-4.5,2.5))+
  labs(title = "Mass change after treatment", x = "Starvation treatment", y = "Body mass change (mg)") +
  theme_bw(base_size = 22) +
  theme(legend.position = "none") +
  theme(text = element_text(family = "Calibri")) +
  geom_boxplot(aes(color = treatment, fill = after_scale(desaturate(lighten(color, .6), .6))), size = 1, outlier.shape = NA) +
  expand_limits(y = -2.2)+
  geom_jitter() +
  facet_wrap(~s, scales = "fixed", ncol = 2)
####################################################################### facet wrap

########################################################### facet wraps lifespans and durations
####################################################################### facet wrap - s summary
##############adult
ggplot(adultdata %>% filter(!is.na(s)), aes(x = treatment, y = adultlifespan, color = treatment)) +
  scale_color_manual(values = treatmentscol) +
  labs(title = "Adult lifespan", x = "Starvation treatment", y = "Lifespan (d)") +
  theme_bw(base_size = 22) +
  theme(legend.position = "none") +
  theme(text = element_text(family = "Calibri")) +
  geom_boxplot(aes(color = treatment, fill = after_scale(desaturate(lighten(color, .6), .6))), size = 1, outlier.shape = NA) +
  geom_jitter() +
  facet_wrap(~s, scales = "fixed", ncol = 2)
##############total
ggplot(adultdata %>% filter(!is.na(s)), aes(x = treatment, y = totallifespan, color = treatment)) +
  scale_color_manual(values = treatmentscol) +
  labs(title = "Total lifespan", x = "Starvation treatment", y = "Lifespan (d)") +
  theme_bw(base_size = 22) +
  theme(legend.position = "none") +
  theme(text = element_text(family = "Calibri")) +
  geom_boxplot(aes(color = treatment, fill = after_scale(desaturate(lighten(color, .6), .6))), size = 1, outlier.shape = NA) +
  geom_jitter() +
  facet_wrap(~s, scales = "fixed", ncol = 2)

####################time till eonymph raincloud
ggplot(adultdata %>% filter(!is.na(s)), aes(x = larvaltreat, y = larvalstageduration, fill = larvaltreat, color = larvaltreat)) +
  scale_fill_manual(values = larvalcol) +
  scale_color_manual(values = larvalcol) +
  labs(title = "Larval development", x = "Starvation treatment", y = "Time until eonymph stage (d)") +
  scale_y_continuous(limits = c(11,17.5))+
  theme_bw(base_size = 22) +
  theme(legend.position = "none") +
  theme(text = element_text(family = "Calibri")) +
  geom_point(size = 1.5, alpha = .5, position = position_jitter(width = 0.1, height = 0.1)) + 
  ggdist::stat_halfeye(justification = -.3, width = .8, adjust = 1, .width = 0) +
  stat_summary(fun = median, geom = "point", shape = 21, size = 3) +
  facet_wrap(~s, scales = "fixed", ncol = 2)
##############pupalstageduration
ggplot(adultdata %>% filter(!is.na(s)), aes(x = larvaltreat, y = pupaestageduration, color = larvaltreat)) +
  scale_color_manual(values=larvalcol) +
  labs(title="Pupal stage duration",x ="Starvation treatment", y = "Days in pupal stage (d)")+
  theme_bw(base_size = 22) +
  theme(legend.position = "none") +
  theme(text = element_text(family = "Calibri")) +
  geom_violin(aes(color = larvaltreat, fill = after_scale(desaturate(lighten(color, .6), .6))), linewidth = 1,)+
  geom_point()+
  facet_wrap(~s, scales = "fixed", ncol = 2)
###############Hatching-> eclosion body mass -> raincloud
ggplot(adultdata %>% filter(!is.na(s)), aes(x = larvaltreat, y = hatchingweightmg, fill = larvaltreat, color = larvaltreat)) +
  scale_fill_manual(values = larvalcol) +
  scale_color_manual(values = larvalcol) +
  labs(title = "Initial adult body mass", x = "Starvation treatment", y = "Initial adult body mass (mg)") +
  scale_y_continuous(limits = c(6.8,24.5))+
  theme_bw(base_size = 22) +
  theme(legend.position = "none") +
  theme(text = element_text(family = "Calibri")) +
  geom_point(size = 1.5, alpha = .5, position = position_jitter(seed = 1, width = .1)) + 
  ggdist::stat_halfeye(justification = -.3, width = .6, adjust = .5, .width = 0) +
  stat_summary(fun = median, geom = "point", shape = 21, size = 3) +
  facet_wrap(~s, scales = "fixed", ncol = 2)

############################################ Kaplan meier survival

####################################################################### Kaplan meier females
fitfemales <- survfit(Surv(adultlifespan) ~ treatment, data = adultfemale_subset)
print(fitfemales)
# Summary of survival curves
summary(fitfemales)
# Access to the sort summary table
summary(fitfemales)$table

survplotfemales <- ggsurvplot(
  fitfemales, # survfit object with calculated statistics.
  pval = TRUE, # show p-value of log-rank test.
  conf.int = TRUE, # show confidence intervals for point estimaes of survival curves.
  conf.int.style = "step", # customize style of confidence intervals
  xlab = "Adult lifespan (d)", # customize X axis label.
  size = 1.5,
  xlim = c(0.9,22.5),
  #break.time.by = 200, # break X axis in time intervals by 200.
  ggtheme = theme_bw(), # customize plot and risk table with a theme.
  risk.table = FALSE, # absolute number and percentage at risk.
  risk.table.y.text.col = T,# colour risk table text annotations.
  risk.table.y.text = FALSE,# show bars instead of names in text annotations in legend of risk table.
  ncensor.plot = FALSE, # plot the number of censored subjects at time t
  #surv.median.line = "hv", # add the median survival pointer.
  legend.labs = c("No starvation", "Adult starvation","Larval starvation","Double starvation"), # change legend labels.
  palette = c("chartreuse4", "gold","darkorange3","red" ))
survplotfemales

surv_difffemales <- survdiff(Surv(adultlifespan) ~ treatment, 
                      data = adultfemale_subset)
surv_difffemales

#Multiple Comparisons of Survival Curves with BH correction
resfemales<-pairwise_survdiff(Surv(adultlifespan) ~ treatment,
                       data = adultfemale_subset,
                       p.adjust.method = "BH", rho = 0)
resfemales


####################################################################### Kaplan meier males
fitmales <- survfit(Surv(adultlifespan) ~ treatment, data = adultmale_subset)
print(fitmales)
# Summary of survival curves
summary(fitmales)
# Access to the sort summary table
summary(fitmales)$table

survplotmales <- ggsurvplot(
  fitmales, # survfit object with calculated statistics.
  pval = TRUE, # show p-value of log-rank test.
  conf.int = TRUE, # show confidence intervals for point estimaes of survival curves.
  conf.int.style = "step", # customize style of confidence intervals
  xlab = "Adult lifespan (d)", # customize X axis label.
  size = 1.5,
  xlim = c(0.9,22.5),
  #break.time.by = 200, # break X axis in time intervals by 200.
  ggtheme = theme_bw(), # customize plot and risk table with a theme.
  risk.table = FALSE, # absolute number and percentage at risk.
  risk.table.y.text.col = T,# colour risk table text annotations.
  risk.table.y.text = FALSE,# show bars instead of names in text annotations in legend of risk table.
  ncensor.plot = FALSE, # plot the number of censored subjects at time t
  #surv.median.line = "hv", # add the median survival pointer.
  legend.labs = c("No starvation", "Adult starvation","Larval starvation","Double starvation"), # change legend labels.
  palette = c("chartreuse4", "gold","darkorange3","red" ))
survplotmales

surv_diffmales <- survdiff(Surv(adultlifespan) ~ treatment, 
                             data = adultmale_subset)
surv_diffmales

#Multiple Comparisons of Survival Curves with BH correction
resmales<-pairwise_survdiff(Surv(adultlifespan) ~ treatment,
                              data = adultmale_subset,
                              p.adjust.method = "BH", rho = 0)
resmales


#######################################################################
####################                Metabolism
####################################################################### facet wrap lipids and carbs
##############lipids mass total
ggplot(adultdata %>% filter(!is.na(s)), aes(x = treatment, y = lipidmasstotal, color = treatment)) +
  scale_color_manual(values = treatmentscol) +
  labs(title = "lipid mass per individual", x = "Starvation treatment", y = "Lipids (mg)") +
  theme_bw(base_size = 22) +
  theme(legend.position = "none") +
  theme(text = element_text(family = "Calibri")) +
  geom_boxplot(aes(color = treatment, fill = after_scale(desaturate(lighten(color, .6), .6))), size = 1, outlier.shape = NA) +
  geom_jitter() +
  facet_wrap(~s, scales = "fixed", ncol = 2)
##############lipids mass per mg bodyweight
ggplot(adultdata %>% filter(!is.na(s)), aes(x = treatment, y = lipidmgmgbodymass, color = treatment)) +
  scale_color_manual(values = treatmentscol) +
  labs(title = "lipid mass per body mass", x = "Starvation treatment", y = "Lipid mass per body mass (mg/mg)") +
  theme_bw(base_size = 22) +
  theme(legend.position = "none") +
  theme(text = element_text(family = "Calibri")) +
  geom_boxplot(aes(color = treatment, fill = after_scale(desaturate(lighten(color, .6), .6))), size = 1, outlier.shape = NA) +
  geom_jitter() +
  facet_wrap(~s, scales = "fixed", ncol = 2)
######################################################carbs
ggplot(adultdata %>% filter(!is.na(s)), aes(x = treatment, y = carbsmasstotal, color = treatment)) +
  scale_color_manual(values = treatmentscol) +
  labs(title = "carbs mass per individual", x = "Starvation treatment", y = "Total carbs (mg)") +
  theme_bw(base_size = 22) +
  theme(legend.position = "none") +
  theme(text = element_text(family = "Calibri")) +
  geom_boxplot(aes(color = treatment, fill = after_scale(desaturate(lighten(color, .6), .6))), size = 1, outlier.shape = NA) +
  geom_jitter() +
  facet_wrap(~s, scales = "fixed", ncol = 2)
##############carbs mass per mg bodyweight
ggplot(adultdata %>% filter(!is.na(s)), aes(x = treatment, y = carbsmgmgbodymass, color = treatment)) +
  scale_color_manual(values = treatmentscol) +
  labs(title = "carbs mass per body mass", x = "Starvation treatment", y = "Carb. mass per body mass (mg/mg)") +
  theme_bw(base_size = 22) +
  theme(legend.position = "none") +
  theme(text = element_text(family = "Calibri")) +
  geom_boxplot(aes(color = treatment, fill = after_scale(desaturate(lighten(color, .6), .6))), size = 1, outlier.shape = NA) +
  geom_jitter() +
  facet_wrap(~s, scales = "fixed", ncol = 2)

############################Metabolism extra plots where females are only dots because of low sample size
###################lipids
ggplot(mapping = aes(x = treatment, y = lipidmgmgbodymass, color = treatment)) +
  # Male Boxplots
  geom_boxplot(data = adultmale_subset, aes(fill = after_scale(desaturate(lighten(color, .6), .6))), size = 1, outlier.shape = NA) +
  geom_jitter(data = adultmale_subset)+
  # Female Jitter with median
  geom_jitter(data = adultfemale_subset, width = 0.2, aes(y = lipidmgmgbodymass), size = 2) +
  stat_summary(data = adultfemale_subset, fun = median, geom = "point", color= "black", size = 1.5, alpha = 7/10) +
  # Global aesthetics and settings
  scale_color_manual(values = treatmentscol) +
  scale_fill_manual(values = treatmentscol) +
  scale_y_continuous(limits = c(0.0,1.17))+
  labs(title = "Lipid mass per body mass", x = "Starvation treatment", y = "Lipid mass per body mass (mg/mg)") +
  theme_bw(base_size = 22) +
  theme(text = element_text(family = "Calibri")) +
  theme(legend.position = "none") +
  facet_wrap(~s, scales = "fixed", ncol = 2)

####################carbs
ggplot(mapping = aes(x = treatment, y = carbsmgmgbodymass, color = treatment)) +
  # Male Boxplots
  geom_boxplot(data = adultmale_subset, aes(fill = after_scale(desaturate(lighten(color, .6), .6))), size = 1, outlier.shape = NA) +
  geom_jitter(data = adultmale_subset)+
  # Female Jitter with median
  geom_jitter(data = adultfemale_subset, width = 0.2, aes(y = carbsmgmgbodymass), size = 2) +
  stat_summary(data = adultfemale_subset, fun = median, geom = "point", color= "black", size = 1.5, alpha = 7/10) +
  # Global aesthetics and settings
  scale_color_manual(values = treatmentscol) +
  scale_fill_manual(values = treatmentscol) +
  scale_y_continuous(limits = c(0.0,0.105))+
  labs(title = "Carbs mass per body mass", x = "Starvation treatment", y = "Carb. mass per body mass (mg/mg)") +
  theme_bw(base_size = 22) +
  theme(text = element_text(family = "Calibri")) +
  theme(legend.position = "none") +
  facet_wrap(~s, scales = "fixed", ncol = 2)

###############################################################

########################################################    TESTING    #######################################################
##############################Larvalstage##########
###  testing for differences in larval developmental time 
#wilcoxon - mann-whitney u for two treatments -> larval

# Perform Wilcoxon rank sum test for males
test_larvaldevelop_male <- wilcox.test(adultmale_subset$larvalstageduration ~ adultmale_subset$larvaltreat, mu = 0, alternative = "two.sided",exact = FALSE)
# Perform Wilcoxon rank sum test for females
test_larvaldevelop_female <- wilcox.test(adultfemale_subset$larvalstageduration ~ adultfemale_subset$larvaltreat, mu = 0, alternative = "two.sided",exact = FALSE)

print(test_larvaldevelop_male)
print(test_larvaldevelop_female)

###  testing for differences in hatching body mass
# Perform Wilcoxon rank sum test for males
test_hatchingweightmg_male <- wilcox.test(adultmale_subset$hatchingweightmg ~ adultmale_subset$larvaltreat, mu = 0, alternative = "two.sided",exact = FALSE)
# Perform Wilcoxon rank sum test for females
test_hatchingweightmg_female <- wilcox.test(adultfemale_subset$hatchingweightmg ~ adultfemale_subset$larvaltreat, mu = 0, alternative = "two.sided",exact = FALSE)

print(test_hatchingweightmg_male)
print(test_hatchingweightmg_female)

######################################################adult stage testing####################
###Kruskal wallis for four treatments + posthoc testing
#######treatment body mass
"treatmentbodymass (treatmentweight)"
# Perform kruskal test for males
test_treatmentweight_male <- kruskal.test(adultmale_subset$treatmentweight ~ adultmale_subset$treatment)
# posthoc
posthoc_treatmentweight_male <- dunn.test(adultmale_subset$treatmentweight, g = adultmale_subset$treatment, method = "bonferroni")

# Perform kruskal test for females
test_treatmentweight_female <- kruskal.test(adultfemale_subset$treatmentweight ~ adultfemale_subset$treatment)
# posthoc
posthoc_treatmentweight_female <- dunn.test(adultfemale_subset$treatmentweight, g = adultfemale_subset$treatment, method = "bonferroni")

print(test_treatmentweight_male)
print(test_treatmentweight_female)
print(posthoc_treatmentweight_male)
print(posthoc_treatmentweight_female)


####### mass change
"masschange"
# Perform kruskal test for males
test_masschange_male <- kruskal.test(adultmale_subset$masschange ~ adultmale_subset$treatment)
# posthoc
posthoc_masschange_male <- dunn.test(adultmale_subset$masschange, g = adultmale_subset$treatment, method = "bonferroni")

# Perform kruskal test for females
test_masschange_female <- kruskal.test(adultfemale_subset$masschange ~ adultfemale_subset$treatment)
# posthoc
posthoc_masschange_female <- dunn.test(adultfemale_subset$masschange, g = adultfemale_subset$treatment, method = "bonferroni")

print(test_masschange_male)
print(test_masschange_female)
print(posthoc_masschange_male)
print(posthoc_masschange_female)


######### total lifespan
"total lifespan"
# Perform kruskal test for males
test_totallifespan_male <- kruskal.test(adultmale_subset$totallifespan ~ adultmale_subset$treatment)
posthoc_totallifespan_male <- dunn.test(adultmale_subset$totallifespan, g = adultmale_subset$treatment, method = "bonferroni")
# Perform kruskal test for females
test_totallifespan_female <- kruskal.test(adultfemale_subset$totallifespan ~ adultfemale_subset$treatment)
posthoc_totallifespan_female <- dunn.test(adultfemale_subset$totallifespan, g = adultfemale_subset$treatment, method = "bonferroni")

print(test_totallifespan_male)
print(test_totallifespan_female)
print(posthoc_totallifespan_male)
print(posthoc_totallifespan_female)


######### adult lifespan
test_adultlifespan_male <- kruskal.test(adultmale_subset$adultlifespan ~ adultmale_subset$treatment)
# Perform kruskal test for females
test_adultlifespan_female <- kruskal.test(adultfemale_subset$adultlifespan ~ adultfemale_subset$treatment)

print(test_adultlifespan_male)
print(test_adultlifespan_female)


######### distance moved
"distance moved"
test_distancemoved_male <- kruskal.test(adultmale_subset$distancemoved ~ adultmale_subset$treatment)
posthoc_distancemoved_male <- dunn.test(adultmale_subset$distancemoved, g = adultmale_subset$treatment, method = "bonferroni")
# Perform kruskal test for females
test_distancemoved_female <- kruskal.test(adultfemale_subset$distancemoved ~ adultfemale_subset$treatment)
posthoc_distancemoved_female <- dunn.test(adultfemale_subset$distancemoved, g = adultfemale_subset$treatment, method = "bonferroni")

print(test_distancemoved_male)
print(test_distancemoved_female)
print(posthoc_distancemoved_male)
print(posthoc_distancemoved_female)



######### immobileduration
test_immobileduration_male <- kruskal.test(adultmale_subset$immobileduration ~ adultmale_subset$treatment)
# Perform kruskal test for females
test_immobileduration_female <- kruskal.test(adultfemale_subset$immobileduration ~ adultfemale_subset$treatment)

print(test_immobileduration_male)
print(test_immobileduration_female)


######### lipids per mg body mass 
"lipids per mg body mass"
test_lipidsper_male <- kruskal.test(adultmale_subset$lipidmgmgbodymass ~ adultmale_subset$treatment)
posthoc_lipidsper_male <- dunn.test(adultmale_subset$lipidmgmgbodymass, g = adultmale_subset$treatment, method = "bonferroni")
# Perform kruskal test for females
test_lipidsper_female <- kruskal.test(adultfemale_subset$lipidmgmgbodymass ~ adultfemale_subset$treatment)
posthoc_lipidsper_female <- dunn.test(adultfemale_subset$lipidmgmgbodymass, g = adultfemale_subset$treatment, method = "bonferroni")

print(test_lipidsper_male)
print(test_lipidsper_female)
print(posthoc_lipidsper_male)
print(posthoc_lipidsper_female)


######### carbs per mg body mass
"carbs per mg body mass"
test_carbsper_male <- kruskal.test(adultmale_subset$carbsmgmgbodymass ~ adultmale_subset$treatment)
posthoc_carbsper_male <- dunn.test(adultmale_subset$carbsmgmgbodymass, g = adultmale_subset$treatment, method = "bonferroni")
# Perform kruskal test for females
test_carbsper_female <- kruskal.test(adultfemale_subset$carbsmgmgbodymass ~ adultfemale_subset$treatment)
posthoc_carbsper_female <- dunn.test(adultfemale_subset$carbsmgmgbodymass, g = adultfemale_subset$treatment, method = "bonferroni")

print(test_carbsper_male)
print(test_carbsper_female)
print(posthoc_carbsper_male)
print(posthoc_carbsper_female)


##### Calculating letters for significance in boxplots ### thanks to dr dozi

calculate_multcompletters <- function(posthoctestresults){
  reformatted.posthoc <- posthoctestresults %>%
    as.data.frame() %>%
    mutate(significant = if_else(P.adjusted >= 0.05, 1, 0),
           comparisons = str_remove_all(comparisons, " ")) 

  
  prepare.vector.multcomp <- as.vector(reformatted.posthoc$significant) %>%
    setNames(reformatted.posthoc$comparisons)
  
  get.multcomp.letters <- multcompLetters(prepare.vector.multcomp) %>%
    return(.)
}

###treatmentweight
letters_treatmentweightfem <- calculate_multcompletters(posthoc_treatmentweight_female) %>%
  print(.)
letters_treatmentweightma <- calculate_multcompletters(posthoc_treatmentweight_male) %>%
  print(.)


###masschange
letters_masschangefem <- calculate_multcompletters(posthoc_masschange_female) %>%
  print(.)
letters_masschangema <- calculate_multcompletters(posthoc_masschange_male) %>%
  print(.)


###masschange
letters_totallifespanfem <- calculate_multcompletters(posthoc_totallifespan_female) %>%
  print(.)
letters_totallifespanma <- calculate_multcompletters(posthoc_totallifespan_male) %>%
  print(.)


###lipidsper
letters_lipidsperfem <- calculate_multcompletters(posthoc_lipidsper_female) %>%
  print(.)
letters_lipidsperma <- calculate_multcompletters(posthoc_lipidsper_male) %>%
  print(.)


###carbsper
letters_carbsperfem <- calculate_multcompletters(posthoc_carbsper_female) %>%
  print(.)
letters_carbsperma <- calculate_multcompletters(posthoc_carbsper_male) %>%
  print(.)
#########################################################################################################################

