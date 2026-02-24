# BDML Final Project --- Universidad de los Andes (2022-2)

This repository contains the final project for **Big Data & Machine
Learning** (Universidad de los Andes, 2022-2).\
It includes data processing, visualization, spatial analysis, and
modeling performed in **R**.

------------------------------------------------------------------------

## 📁 Repository Structure

    ├── .gitignore  
    ├── MODELS.R                     # Model training & evaluation scripts  
    ├── Mapas modelos.R              # Spatial mapping and geospatial model visualization  
    ├── Pre-processing.R             # Data preprocessing and feature engineering  
    ├── script.R                     # Main exploratory analysis & mapping script  
    ├── README.md  
    └── LICENSE

### Description of Main Scripts

-   **Pre-processing.R** --- Data cleaning, transformation, and
    preparation for analysis and modeling.
-   **script.R** --- Exploratory data analysis including histograms,
    descriptive statistics, and spatial visualization (choropleth maps
    and regional distributions).
-   **MODELS.R** --- Machine learning model training, validation, and
    evaluation.
-   **Mapas modelos.R** --- Spatial visualization of model results and
    geographic patterns.

------------------------------------------------------------------------

## 🎯 Project Objective

The objective of this project is to analyze and model socioeconomic and
educational outcomes across Colombian regions using spatial data and
machine learning techniques.

The project integrates:

-   Data preprocessing and feature engineering\
-   Exploratory spatial data analysis\
-   Statistical and machine learning modeling\
-   Geographic visualization of results

The analysis focuses on understanding regional patterns and building
predictive models that capture spatial and socioeconomic variation.

------------------------------------------------------------------------

## 🧠 Analysis Workflow

### 1. Data Loading & Cleaning

-   Import datasets
-   Clean and standardize variables
-   Prepare model-ready datasets

### 2. Exploratory Analysis

-   Distributional analysis
-   Regional comparisons
-   Spatial mapping using shapefiles

### 3. Modeling

-   Train predictive models
-   Evaluate performance
-   Compare alternative specifications

### 4. Spatial Visualization of Results

-   Map predicted values
-   Visualize regional disparities
-   Interpret geographic patterns

------------------------------------------------------------------------

## 📦 Main R Packages Used

-   tidyverse
-   sf
-   skimr
-   viridis
-   rio
-   ggplot2

(Additional packages may be loaded within specific scripts.)

------------------------------------------------------------------------

## 🔁 Reproducibility

To replicate the analysis:

1.  Clone the repository.
2.  Open in RStudio (optional but recommended).
3.  Install required R packages.
4.  Run scripts in the following order:
    -   `Pre-processing.R`
    -   `script.R`
    -   `MODELS.R`
    -   `Mapas modelos.R`

Outputs include descriptive plots, spatial maps, and model results.

------------------------------------------------------------------------

## 📜 License

This project is licensed under the **MIT License**.

See the `LICENSE` file for full details.

------------------------------------------------------------------------

## 👤 Author

Juan José Rincón\
Universidad de los Andes --- BDML 2022-2
