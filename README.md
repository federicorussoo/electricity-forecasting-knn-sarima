# Forecasting Italian Electricity Prices with k-Nearest Neighbors

This repository contains the **R code developed for my Bachelor's thesis** in Statistics at the **University of Padua**:

**[Read the thesis](https://thesis.unipd.it/retrieve/71f88cf2-ded8-4152-82dd-bc58d5dc2260/Russo_Federico.pdf)** — The thesis is written in Italian.

The project investigates the use of **k-Nearest Neighbors (kNN)** for forecasting hourly electricity prices in the Italian day-ahead electricity market (MGP), with a **SARIMA model** used as a benchmark.

---

## Repository structure

```text
R/
├── knn_library.R
├── main_analysis.R
└── sarima_benchmark.R
```

- **`knn_library.R`** — Core kNN implementation, including distance measures, neighbour selection, parameter optimization, accuracy evaluation and iterative forecasting.
- **`sarima_benchmark.R`** — SARIMA benchmark implementation using a SARIMA(2,0,0)(0,1,2)[24] model with calendar effects.
- **`main_analysis.R`** — Complete application of the methods to **January 2025**, including forecasting, evaluation and comparison between kNN and SARIMA.

---

## Data

The analysis focuses on the **Northern Italian market zone (MGP Nord)**.

The original electricity price data are not included in the repository. Users wishing to reproduce the analysis should obtain the corresponding data from the official GME sources at https://www.mercatoelettrico.org/it-it/Home/Esiti/Elettricita/MGP/Esiti/PUN and adapt the input path in the R scripts if necessary. The analysis requires the corresponding Excel files to be placed in the `data/` directory.

```text
data/
├── north_2023.xlsx
├── north_2024.xlsx
└── north_2025_h1.xlsx
```

---

## Running the analysis

The project is implemented in **R**. After installing the required packages and adding the input data, run from the repository root:

```r
source("R/main_analysis.R")
```

The script automatically loads the kNN library and SARIMA benchmark.

## Overview

The repository contains the **core code developed for the thesis**, while `main_analysis.R` provides an example of the forecasting workflow using **January 2025**. The script plots two charts to evaluate the daily performance and average hourly profiles, and automatically identifies the best performing model configuration. The full empirical study and results are available in the thesis.
