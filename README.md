# Biostatistical-Analysis-System
Bachelor´s Degree Final Project of Biomedical Engineering


## Overview
The **Biostatistical Analysis System** is an interactive, didactic R Shiny application designed for students in the Biomedical Engineering degree program. It serves as a comprehensive tool for modeling and analyzing biological variability, covering foundational topics from both **Biostatistics I** and **Biostatistics II** curricula.

The application allows users to perform statistical calculations, visualize probability distributions, and conduct hypothesis testing in a user-friendly, web-based environment.

## Features

The system is organized into modular sections based on the official course syllabus:

### Biostatistics I
* **Descriptive Statistics:** Tools for visualizing data distributions (histograms, boxplots, time series, scatter plots) and calculating summary statistics.
* **Probability:** Calculators for combinatorics and Bayes' Theorem, with an interactive Venn Diagram visualizer.
* **Discrete Random Variables:** Visualization of mass and cumulative distribution functions (Binomial, Geometric, Negative Binomial, Poisson).
* **Continuous Random Variables:** Modeling of Normal and Exponential distributions.
* **Joint Probability:** Exploration of multidimensional probability scenarios (Discrete and Continuous).
* **Confidence Estimation:** Interactive visualization of confidence intervals based on sample data.

### Biostatistics II
* **Statistical Inference:** Parametric and non-parametric hypothesis testing for one and two independent/paired samples.
* **Simple Linear Regression:** Modeling and visualization of linear relationships between variables.
* **Multiple Linear Regression:** Multivariable modeling with diagnostics (Observed vs. Predicted).
* **ANOVA:** Analysis of Variance for both independent groups and repeated measures designs.

## Requirements
To run this application, you need **R** and the **Shiny** environment installed. The following R packages are required:

- `shiny`
- `shinydashboard`
- `ggplot2`
- `dplyr`
- `DT`
- `broom`
- `ggVennDiagram`
- `tidyr`

## How to Run
1. Ensure all required packages are installed using:
   ```R
   install.packages(c("shiny", "shinydashboard", "ggplot2", "dplyr", "DT", "broom", "ggVennDiagram", "tidyr"))
2. Open the app.R file in RStudio.
3. Click the "Run App" button at the top of the editor.
