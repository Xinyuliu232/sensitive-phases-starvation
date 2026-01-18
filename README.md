# Sensitive phases of starvation

This repository contains the analysis code and results for a project examining how starvation at different developmental stages affects behavioral and metabolic traits.

The primary goal of this project is to evaluate whether the **timing of starvation exposure**, rather than starvation itself, leads to long-term phenotypic differences, and whether these effects differ by sex.

This repository focuses on **comparative statistical analysis and visualization**, not predictive modeling.

---

## 1) Project background

Early-life nutritional stress is known to influence adult physiology and behavior, but accumulating evidence suggests that organisms may respond differently depending on **when** the stress occurs.

In this project, starvation was applied at different developmental phases to test the presence of potential **sensitive windows**, defined as periods during which starvation exposure produces disproportionately large or persistent effects.

The analysis addresses two main questions:

1. Does starvation at different developmental stages lead to distinct behavioral or metabolic outcomes?
2. Are these effects sex-specific?

---

## 2) Experimental design

### 2.1 Starvation treatments

Four treatment groups were included:

* no starvation (control)
* larval starvation
* adult starvation
* double starvation (larval + adult)

These treatments allow comparison between:

* early vs late exposure
* single-stage vs double exposure
* absence vs presence of starvation

---

### 2.2 Sex stratification

Both males and females were included in the experiment.

Because preliminary exploration suggested sex-dependent variability, most analyses were conducted **separately by sex**, rather than including sex only as a covariate.

---

### 2.3 Phenotypic outcomes

Measured traits include:

Behavioral traits:

* distance moved
* immobility duration

Metabolic traits:

* lipid content
* carbohydrate content

All traits were analyzed as continuous outcomes.

---

## 3) Input data

### Processed dataset

All analyses use the processed dataset:

```
Worksheet_sensitive_phases_starvation_processed.csv
```

The dataset contains:

* individual-level measurements
* treatment group labels
* sex information
* behavioral and metabolic trait values

Raw experimental data are not included in this repository.

---

## 4) Analysis workflow

All analyses were performed in R.

The workflow follows a consistent structure across traits and sexes to ensure comparability.

---

### 4.1 Data cleaning and formatting

Steps include:

* removal of missing values when required
* conversion of treatment variables to factors
* consistent ordering of treatment levels
* separation of datasets by sex when applicable

No transformation was applied unless required by the statistical test.

---

### 4.2 Group-based comparisons

For each trait, comparisons were performed across starvation treatments.

Depending on data characteristics, analyses include:

* non-parametric tests for group differences
* pairwise post-hoc comparisons
* multiple-comparison adjustment

The goal is to identify **which developmental-stage starvation differs from control and from each other**, rather than testing a single global effect.

---

### 4.3 Sex-specific analyses

Analyses were typically conducted as:

* female-only subset
* male-only subset

This avoids assuming identical response patterns across sexes and allows direct visual comparison of treatment effects.

---

### 4.4 Visualization strategy

Plots were generated using `ggplot2`.

Visualization choices were made to emphasize both:

* group-level trends
* individual-level variation

Figures include:

* boxplots summarizing group distributions
* overlaid individual data points
* median indicators
* faceting by sex when appropriate

All figures are saved automatically to the `figures/` directory.

---

## 5) Statistical output

Statistical test results and post-hoc comparisons are saved to the `results/` directory.

These outputs are intended to accompany figures rather than replace them.

The analysis emphasizes **effect patterns across treatments** rather than binary significance interpretation.

---

## 6) Reproducing the analysis

To reproduce the analysis:

1. Place all files in the same directory
2. Open R
3. Run:

```r
source("SensitivePhasesProject_script.R")
```

The script will:

* load the processed dataset
* perform all analyses
* generate figures
* write statistical results to disk

---

## 7) Notes and limitations

* This repository contains processed data only.
* No causal inference is claimed.
* Sample size differs across treatments, which may influence power.
* Some intermediate objects are stored in `.RData` for convenience.

---



