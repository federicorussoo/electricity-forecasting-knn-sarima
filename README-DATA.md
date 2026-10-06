## Data

The analysis focuses on the **Northern Italian market zone (MGP Nord)**.

The original electricity price data are not included in the repository. Users wishing to reproduce the analysis should obtain the corresponding data from the official GME sources at https://www.mercatoelettrico.org/it-it/Home/Esiti/Elettricita/MGP/Esiti/PUN and adapt the input path in the R scripts if necessary. The analysis requires the corresponding Excel files to be placed in the `data/` directory.

```text
data/
├── north_2023.xlsx
├── north_2024.xlsx
└── north_2025_h1.xlsx