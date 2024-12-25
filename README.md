# Exploratory Factor and Graph Analysis of Perfectionism Questionnaire

This repository contains the code and report for analyzing a dataset of 18 questionnaire items assessing perfectionism. The analysis uses **Exploratory Factor Analysis (EFA)** and **Exploratory Graph Analysis (EGA)** to explore the structure of the questionnaire and identify its dimensionality.

---

## Overview

### Goals:
1. Assess the dimensional structure of the 18-item perfectionism questionnaire.
2. Evaluate the role of item **Q5** ("I compare myself to others to evaluate my achievements").
3. Use EGA to visually and statistically identify clusters of items.

### Tools and Methods:
- **Exploratory Factor Analysis (EFA)**:
  - Conducted with oblimin and promax rotations.
  - Used to identify factor loadings and explain variance.
- **Exploratory Graph Analysis (EGA)**:
  - Implemented using the `EGA` package to visualize the questionnaire's structure.
  - Emphasized stability analysis via bootstrapping.

---

## Repository Contents

### Files:
1. **`analysis_script.R`**: Complete R script for running the analysis.
2. **`report.pdf`**: Detailed report summarizing the analysis steps, results, and interpretations.
3. **Graphs folder**:
   - `scree_plot.png`: Scree plot of eigenvalues for determining factor count.
   - `efa_network.png`: Network visualization from EFA results.
   - `ega_network.png`: EGA network visualization showing item clusters.

### Data:
- The dataset used in this analysis was sourced from a publicly available GitHub repository:
  - `cleaned_perfectionism_data.csv`

---

## Analysis Summary

### Key Findings:
- **Dimensionality**: Three dimensions were identified, corresponding to **PSS**, **PSI**, and **PSP**.
- **Q5 Stability**: Item Q5 showed cross-loadings and lower stability, requiring further evaluation.
- **Network Structure**: EGA revealed clear clusters matching the theoretical dimensions of perfectionism.

### Metrics:
- **Confirmatory Factor Analysis (CFA)**:
  - RMSEA: 0.099
  - CFI: 0.848
  - SRMR: 0.086
- **EGA Stability**:
  - Q5 Stability: 54.2%
  - Other items: Stability > 90%.

---

## Instructions for Reproducing the Analysis

1. Clone this repository:
   ```bash
   git clone https://github.com/<your-repo>/perfectionism-analysis.git
