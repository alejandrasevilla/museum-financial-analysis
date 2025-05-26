# Museum Financial Analysis

![R](https://img.shields.io/badge/R-4.1+-blue?logo=r) ![Status](https://img.shields.io/badge/status-completed-success) ![License](https://img.shields.io/badge/license-IMLS%20Public-lightgrey)

This project analyzes the financial performance of U.S. museums using the **2018 Museum Data Files** from the Institute of Museum and Library Services (IMLS). It explores what drives **revenue** and **profitability** across institutions, building predictive models to support strategic funding decisions and long-term sustainability.

------------------------------------------------------------------------

## Repository Structure

```         
museum-financial-analysis/
├── data/             # Not included – see README.txt for download info
├── analysis/         # Contains full analysis script: museum_revenue_profitability_analysis.R
├── report/           # Final project report (PDF)
├── presentation/     # PowerPoint slides (PDF)
└── README.md         # This file
```

------------------------------------------------------------------------

## Getting Started

To reproduce the analysis:

1.  Clone the repository:

    ``` bash
    git clone https://github.com/alejandrasevilla/museum-financial-analysis.git
    ```

2.  Open the project in RStudio:

    ``` bash
    open museum-financial-analysis.Rproj
    ```

3.  Download the dataset from IMLS and place it in the `data/` folder.\
    *(See `data/README.txt` for step-by-step instructions.)*

4. Run the full analysis:

    ```r
    source("analysis/museum_revenue_profitability_analysis.R")
    ```

------------------------------------------------------------------------

## Methodology

-   **Data Cleaning:** Handled missing values, consolidated categories, removed outliers.
-   **Feature Engineering:** Created revenue class, profitability flag, and additional indicators.
-   **Exploratory Analysis:** Visualized trends by revenue, region, museum type, and more.
-   **Modeling:**
    -   *Revenue Classification:* Ridge, GBM, Random Forest, Logistic Regression, Naive Bayes
    -   *Profitability Prediction:* Logistic Regression, Ridge, Naive Bayes, Random Forest
-   **Evaluation:** Accuracy, Precision, Recall, F1 Score, AUC (50x cross-validation)

------------------------------------------------------------------------

## Results Summary

| Task | Best Model | F1 Score | AUC | Key Insight |
|-------------|-------------|-------------|-------------|--------------------|
| Revenue Classification | Ridge Regression | 0.84 | 0.84 | Location and discipline strongly influence revenue |
| Profitability Prediction | Logistic Regression | 0.81 | 0.68 | Private/university affiliation ↑ profitability |

------------------------------------------------------------------------

## Data

The original dataset is publicly available at:

[IMLS Museum Universe Data Files (2018)](https://www.imls.gov/research-evaluation/data-collection/museum-universe-data-file)

Refer to `data/README.txt` for detailed download and placement instructions.

------------------------------------------------------------------------

##Tools Used

-   **Languages/Libraries:** `R`, `tidyverse`, `ggplot2`, `caret`, `glmnet`, `gbm`, `e1071`, `randomForest`, `pROC`
-   **Visualization:** `ggplot2`, `cowplot`, `maps`, `wordcloud`
-   **Modeling Techniques:** Logistic regression, regularization, boosting, ensemble models

------------------------------------------------------------------------

## Author

**Alejandra Sevilla**\
M.S. in Analytics – Georgia Tech (2025)\
Houston, TX\
[GitHub](https://github.com/alejandrasevilla) \| [LinkedIn](https://www.linkedin.com/in/alejandra-sevilla-m/)

------------------------------------------------------------------------

## License

This project is licensed under the [MIT License](LICENSE).

The dataset used is publicly available from the Institute of Museum and Library Services (IMLS) and is subject to their terms of use.\
Please cite IMLS appropriately if you reuse or adapt this work.

------------------------------------------------------------------------
