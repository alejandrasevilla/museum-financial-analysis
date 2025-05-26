# Data Directory – Museum Financial Analysis

This folder is **excluded from version control** via `.gitignore` to comply with file size and licensing guidelines.

The dataset used in this project comes from the **Institute of Museum and Library Services (IMLS)**:

[IMLS Museum Universe Data File (2018)](https://www.imls.gov/research-evaluation/data-collection/museum-universe-data-file)

---

## How to Download and Prepare the Data

1. Go to the [official IMLS data page](https://www.imls.gov/research-evaluation/data-collection/museum-universe-data-file).
2. Scroll down to **"2018 Museum Universe Data File"**.
3. Download the ZIP file containing the data and documentation.
4. Extract the contents and locate the following three CSV files:
    ```
    MuseumFile2018_File1_Nulls.csv
    MuseumFile2018_File2_Nulls.csv
    MuseumFile2018_File3_Nulls.csv
    ```
5. Place all three files into this `data/` folder.

---

## Notes

- These files are **not included** in the GitHub repository due to licensing and file size policies.
- Running the analysis script will automatically combine and clean these datasets.
- Optionally, the script may generate a processed file such as `museum_data_cleaned.csv` inside this same folder. This file is also excluded from version control.

---

For variable definitions and additional context, refer to the technical documentation provided within the ZIP download on the IMLS website.
