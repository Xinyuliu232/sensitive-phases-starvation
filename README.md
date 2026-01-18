Sensitive phases of starvation

This repository contains the analysis code and results for a project examining how starvation at different developmental stages affects later behavioral and metabolic traits.

The main goal of this project is to test whether starvation exposure during specific life stages leads to long-term effects, and whether these effects differ by sex.

Project background

Early-life nutritional stress is known to influence adult physiology and behavior, but the timing of exposure may be critical.
In this project, starvation was applied at different developmental phases to evaluate potential sensitive windows.

The analysis focuses on comparing phenotypic outcomes across treatments rather than building predictive models.

Experimental design

Starvation treatments include:

no starvation (control)

larval starvation

adult starvation

double starvation (larval + adult)

Both males and females were analyzed, and most analyses were conducted separately by sex.

Measured outcomes include behavioral traits (e.g. distance moved, immobility duration) and energy-related traits (lipids, carbohydrates).



All analyses were performed in R.

The workflow includes:

data cleaning and formatting

grouping by starvation treatment

sex-specific subset analyses

group comparisons across treatments

post-hoc multiple comparisons when applicable

visualization using ggplot2

Both boxplots and dot plots were used to show group distributions and individual variation.

Reproducing the analysis

To reproduce the analysis, place all files in the same directory and run:

source("SensitivePhasesProject_script.R")

The script will generate figures and statistical outputs saved in the corresponding folders.

Notes

This repository contains processed data only.

The purpose of this project is exploratory and descriptive.

Some intermediate objects are stored in .RData for convenience.
