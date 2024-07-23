# Eclipse Analysis 2023-2024

## Overview

This project analyzes and visualizes solar eclipses occurring in 2023 and 2024 across various locations. It utilizes data from NASA’s eclipse databases and incorporates several analytical techniques, including statistical summaries, data visualizations, and machine learning models. The objective is to understand the distribution, timing, and duration of different types of eclipses and to explore potential anomalies.

## Features

- **Data Collection**: Fetches eclipse data from NASA for 2023 and 2024 and processes the data for total, partial, and annular eclipses (source code from TidyTuesday).
- **Data Cleaning**: Cleans and organizes the data, including separating total and partial eclipses and filtering relevant information (source code from TidyTuesday).
- **Data Storage**: Saves cleaned data into CSV files for further analysis.
- **Visualizations**:
  - **Latitude and Longitude Distributions**: Histograms showing the distribution of latitudes and longitudes for the eclipses.
  - **Partial Eclipse Timing**: Histograms depicting the start, peak, and end times of partial eclipses.
  - **Annular/Total Eclipse Durations**: Histograms and boxplots of the durations of annular and total eclipses.
  - **Geographical Maps**: Maps showing the geographical distribution of eclipse durations.
- **Machine Learning**:
  - **Predictive Modeling**: Linear regression model to predict annular eclipse 2023 duration based on location.
  - **K-Means Clustering**: K-means clustering to identify patterns in the geographical distribution of partial eclipse 2023.
  - **Anomaly Detection**: Isolation Forest algorithm to detect anomalies in partial eclipse 2024 data.
  - **Spatial Analysis**: Moran's I test for spatial autocorrelation in total eclipse 2024 duration data.

## Data Sources

- [Eclipse Cities 2024](https://svs.gsfc.nasa.gov/vis/a000000/a005000/a005073/cities-eclipse-2024.json)
- [Eclipse Cities 2023](https://svs.gsfc.nasa.gov/vis/a000000/a005000/a005073/cities-eclipse-2023.json)

## Dependencies

R packages: `tidyverse`, `jsonlite`, `janitor`, `fs`, `ggplot2`, `maps`, `cluster`, `isotree`, `spdep`

## Directory Structure

- `csv_files/`: Contains CSV files of eclipse data.
- `time_viz/`: Contains visualizations related to eclipse timing.
- `location_viz/`: Contains visualizations related to eclipse locations.
- `machine_learning_results/`: Contains outputs from machine learning analyses and predictive models.
