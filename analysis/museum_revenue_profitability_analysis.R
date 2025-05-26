################################################################################
# Museum Revenue and Profitability Analysis Pipeline
# Author: Alejandra Sevilla
# Last Updated: 2025-05-26
#
# Description:
# This script performs a full end-to-end analysis of U.S. museums using the 
# 2018 Museum Data File from IMLS. It includes:
# - Data import and cleaning
# - Feature engineering
# - Exploratory Data Analysis (EDA)
# - Binary classification models for revenue and profitability
# - Cross-validation and model performance comparison
# - Visualizations for interpretability
#
# Data Source:
# IMLS Museum Data Files (2018)
#
# Required Packages:
# dplyr, ggplot2, ggcorrplot, cowplot, MASS, caret, car, randomForest, rvest,
# e1071, maps, pROC, glmnet, wordcloud, RColorBrewer, gbm, stringr, reshape2
#
# Note:
# Raw data files should be placed in the `data/` directory.
# Output figures and results are saved within the `output/` directory.
################################################################################

# ================================
# 1. DATA IMPORT AND SETUP
# ================================

# ================================

# Read the datasets
file1 <- read.csv("data/MuseumFile2018_File1_Nulls.csv")
file2 <- read.csv("data/MuseumFile2018_File2_Nulls.csv")
file3 <- read.csv("data/MuseumFile2018_File3_Nulls.csv")

# Rename columns in file2 and file3 to match file1
colnames(file2)[colnames(file2) == "DISCIPLINE"] <- "DISCIPL"
colnames(file3)[colnames(file3) == "DISCIPLINE"] <- "DISCIPL"

# Ensure both IMLSAD_F and IRS990_F are present in all datasets
# If IRS990_F is missing in file1, create it and set it to NA
file1$IRS990_F <- NA

# If IMLSAD_F is missing in file2 and file3, create it and set it to NA
file2$IMLSAD_F <- NA
file3$IMLSAD_F <- NA

# Combine the datasets
museum_data <- rbind(file1, file2, file3)

# Confirm import
head(museum_data)

# Display the number of rows and columns
cat("Number of rows:", nrow(museum_data), "\n")
cat("Number of columns:", ncol(museum_data), "\n")

# Function to install and load packages only if they are not installed
install_if_missing <- function(package) {
  if (!require(package, character.only = TRUE)) {
    install.packages(package, dependencies = TRUE)
    library(package, character.only = TRUE)
  }
}

install_if_missing("dplyr")
install_if_missing("ggplot2")
install_if_missing("ggcorrplot")
install_if_missing("cowplot")
install_if_missing("MASS")
install_if_missing("caret")
install_if_missing("car")
install_if_missing("randomForest")
install_if_missing("rvest")
install_if_missing("e1071")
install_if_missing("maps")
install_if_missing("pROC")
install_if_missing("glmnet")
install_if_missing("wordcloud")
install_if_missing("RColorBrewer")
install_if_missing("gbm")
install_if_missing("stringr")


# 2. DATA CLEANING
# ================================
# - Merge and column renaming
# - Drop irrelevant variables
# - Handle missing values (REVENUE15, INCOME15, LOCALE4, BEAREG)
# - Final structure check

# ================================

# Check the structure of the combined dataset
print("Initial dataset structure:")
str(museum_data)

# Initial count of missing values
print("Initial count of missing values per variable:")
print(colSums(is.na(museum_data)))
    
# Check for duplicate rows
num_duplicates <- sum(duplicated(museum_data))
print(paste("Number of duplicate rows:", num_duplicates))

# Remove unnecessary columns
museum_data_cleaned <- museum_data[, !colnames(museum_data) %in% c("MID", "EIN", "DUNS", "AKADBA","NAICS", "LEGALNAME", 
                                                                   "ALTNAME", "ADSTREET", "ADZIP", "PHSTREET", "PHCITY", 
                                                                   "PHSTATE", "PHZIP", "ADZIP5", "PHZIP5", "PHONE", "WEBURL", "IRS_NAME15", 
                                                                   "TAXPER15", "IRS_STREET15", "IRS_CITY15", "IRS_STATE15", "IRS_ZIP15", 
                                                                   "INSTCITY", "INSTST", "FIPSST", "FIPSCO", "GSTREET", "GCITY", "GSTATE", 
                                                                   "GZIP", "GZIP5", "USER_F", "OTHER_F", "SOURCE", "IRS990_F")]

# Structure after removing non-essential variables
print("Dataset structure after removing non-essential variables:")
str(museum_data_cleaned)

# Missing values after removing non-essential columns
print("Missing values per variable after removing non-essential columns:")
print(colSums(is.na(museum_data_cleaned)))

# Remove rows with missing values in either REVENUE15 or INCOME15
print("Missing REVENUE15 and INCOME15 before removal:")
print(sum(is.na(museum_data_cleaned$REVENUE15)))
print(sum(is.na(museum_data_cleaned$INCOME15)))

museum_data_cleaned <- museum_data_cleaned[!is.na(museum_data_cleaned$REVENUE15) & !is.na(museum_data_cleaned$INCOME15), ]

print("Missing REVENUE15 and INCOME15 after removal:")
print(sum(is.na(museum_data_cleaned$REVENUE15)))
print(sum(is.na(museum_data_cleaned$INCOME15)))

# Handle LOCALE4 missing values using ADCITY
print("Missing LOCALE4 before imputation:")
print(sum(is.na(museum_data_cleaned$LOCALE4)))

city_locale_lookup <- museum_data_cleaned %>%
  filter(!is.na(LOCALE4)) %>%
  group_by(ADCITY) %>%
  summarize(LOCALE4_most_common = as.numeric(names(which.max(table(LOCALE4)))), .groups = "drop")

museum_data_cleaned <- left_join(museum_data_cleaned, city_locale_lookup, by = "ADCITY")
museum_data_cleaned <- museum_data_cleaned %>%
  mutate(LOCALE4 = ifelse(is.na(LOCALE4), LOCALE4_most_common, LOCALE4))

museum_data_cleaned <- museum_data_cleaned[, !(colnames(museum_data_cleaned) %in% "LOCALE4_most_common")]

remaining_missing_values <- sum(is.na(museum_data_cleaned$LOCALE4))
print(paste("Remaining missing LOCALE4 after imputation:", remaining_missing_values))

# Remove rows where LOCALE4 is still missing (if any remain)
museum_data_cleaned <- museum_data_cleaned[!is.na(museum_data_cleaned$LOCALE4), ]

# Handle missing BEAREG
print("Missing BEAREG before imputation:")
print(sum(is.na(museum_data_cleaned$BEAREG)))

# Function to assign BEAREG based on ADSTATE, with additional print statements
assign_beareg <- function(state) {
  if (state %in% c("CT", "ME", "MA", "NH", "RI", "VT")) {
    return(1)  # New England
  } else if (state %in% c("DE", "DC", "MD", "NJ", "NY", "PA")) {
    return(2)  # Mid-East
  } else if (state %in% c("IL", "IN", "MI", "OH", "WI")) {
    return(3)  # Great Lakes
  } else if (state %in% c("IA", "KS", "MN", "MO", "NE", "ND", "SD")) {
    return(4)  # Plains
  } else if (state %in% c("AL", "AR", "FL", "GA", "KY", "LA", "MS", "NC", "SC", "TN", "VA", "WV")) {
    return(5)  # Southeast
  } else if (state %in% c("AZ", "NM", "OK", "TX")) {
    return(6)  # Southwest
  } else if (state %in% c("CO", "ID", "MT", "UT", "WY")) {
    return(7)  # Rocky Mountains
  } else if (state %in% c("AK", "CA", "HI", "NV", "OR", "WA")) {
    return(8)  # Far West
  } else {
    print(paste("Unknown state:", state))  # Print any state that doesn't fit
    return(NA)  # Unknown state
  }
}

# Apply the function to fill in missing BEAREG values based on ADSTATE
museum_data_cleaned <- museum_data_cleaned %>%
  mutate(BEAREG = ifelse(is.na(BEAREG), sapply(ADSTATE, assign_beareg), BEAREG))

# Check the states that couldn't be matched to a BEAREG region
unknown_states <- unique(museum_data_cleaned$ADSTATE[is.na(museum_data_cleaned$BEAREG)])

# Check for any remaining missing BEAREG values
remaining_missing_beareg <- sum(is.na(museum_data_cleaned$BEAREG))
print(paste("Remaining missing BEAREG after imputation:", remaining_missing_beareg))

# If needed, remove rows with still-missing BEAREG
museum_data_cleaned <- museum_data_cleaned[!is.na(museum_data_cleaned$BEAREG), ]

# Verify no missing values in the final dataset
final_missing_values <- colSums(is.na(museum_data_cleaned))

## More handling of missing values

# Drop rows where RULEDATE15 is missing
museum_data_cleaned <- museum_data_cleaned[!is.na(museum_data_cleaned$RULEDATE15), ]

# Drop IPEDS column
museum_data_cleaned <- subset(museum_data_cleaned, select = -IPEDS)

# Drop AAMREG
museum_data_cleaned <- subset(museum_data_cleaned, select = -AAMREG)

# Drop IMLSAD_F
museum_data_cleaned <- subset(museum_data_cleaned, select = -IMLSAD_F)

# Final missing values and structure
print("Final dataset structure:")
str(museum_data_cleaned)

print("Final count of missing values per variable:")
print(colSums(is.na(museum_data_cleaned)))

# Summary of final cleaned data
print("Summary of the cleaned dataset:")
summary(museum_data_cleaned)


# ================================
# 3. FEATURE ENGINEERING
# ================================
# - Decode categorical fields (DISCIPL, LOCALE4, BEAREG, etc.)
# - Clean CoLocationResource
# - Clean and map NTEECode
# - Create IncomeCategory labels
# - Format TaxExemptionDate

# Create a lookup table to map DISCIPL codes to their corresponding names
discipline_lookup <- data.frame(
  DISCIPL = c("ART", "BOT", "CMU", "HST", "NAT", "SCI", "ZAW", "GMU", "HSC"),
  Discipline = c("Art Museums",
                 "Arboretums, Botanical Gardens, & Nature Centers",
                 "Children's Museums",
                 "History Museums",
                 "Natural History & Natural Science Museums",
                 "Science & Technology Museums & Planetariums",
                 "Zoos, Aquariums, & Wildlife Conservation",
                 "Uncategorized or General Museums",
                 "Historical Societies, Historic Preservation")
)

# Merge the lookup table to replace DISCIPL codes with full names
museum_data_cleaned <- left_join(museum_data_cleaned, discipline_lookup, by = "DISCIPL")

# Remove the original DISCIPL column (optional) after renaming
museum_data_cleaned <- museum_data_cleaned[, !(colnames(museum_data_cleaned) %in% "DISCIPL")]

# Rename the new column to DISCIPLINE
colnames(museum_data_cleaned)[colnames(museum_data_cleaned) == "Discipline"] <- "DISCIPLINE"

# Define mapping for BEAREG codes to their descriptions
beareg_mapping <- c(
  "1" = "New England",
  "2" = "Mid-East",
  "3" = "Great Lakes",
  "4" = "Plains",
  "5" = "Southeast",
  "6" = "Southwest",
  "7" = "Rocky Mountains",
  "8" = "Far West"
)

# Define mapping for LOCALE4 codes to their descriptions
locale4_mapping <- c(
  "1" = "City",
  "2" = "Suburb",
  "3" = "Town",
  "4" = "Rural"
)

# Apply the mappings to replace the numeric codes with their descriptions
museum_data_cleaned <- museum_data_cleaned %>%
  mutate(
    BEAREG = as.character(beareg_mapping[as.character(BEAREG)]),  # Convert BEAREG code to description
    LOCALE4 = as.character(locale4_mapping[as.character(LOCALE4)])  # Convert LOCALE4 code to description
  )

# Complete renaming of columns
museum_data_cleaned <- museum_data_cleaned %>%
  rename(
    CoLocationResource = CO_LOC_RES,          # Rename CO_LOC_RES
    NTEECode = NTEEC,                         # Rename NTEEC to MuseumTypeCode
    MuseumName = COMMONNAME,                  # Rename COMMONNAME to MuseumName
    City = ADCITY,                            # Rename ADCITY to City
    State = ADSTATE,                          # Rename ADSTATE to State
    IncomeCategoryCode = INCOMECD15,          # Rename INCOMECD15 to IncomeCategoryCode
    Income2015 = INCOME15,                    # Rename INCOME15 to Income2015
    Revenue2015 = REVENUE15,                  # Rename REVENUE15 to Revenue2015
    TaxExemptionDate = RULEDATE15,            # Rename RULEDATE15 to TaxExemptionDate
    InstitutionName = INSTNAME,               # Rename INSTNAME to InstitutionName
    Region = BEAREG,                          # Rename BEAREG to Region
    LocaleType = LOCALE4,                     # Rename LOCALE4 to LocaleType
    NonprofitFlag = BMF15_F,                  # Rename BMF15_F to NonprofitFlag
    PrivateFoundationFlag = PFND_F,           # Rename PFND_F to PrivateFoundationFlag
    FinancialTypeFlag = FCT3P_F,              # Rename FCT3P_F to FinancialTypeFlag
    UniversityAffiliationFlag = UNI_F,        # Rename UNI_F to UniversityAffiliationFlag
    MuseumDiscipline = DISCIPLINE,            # Rename DISCIPLINE to MuseumDiscipline
    Latitude = LATITUDE,
    Longitude = LONGITUDE
  )

##  Handling of ColocationResource

# Replace blanks in CoLocationResource with "No Co-located Resource"
museum_data_cleaned$CoLocationResource[museum_data_cleaned$CoLocationResource == " "] <- "NO CO-LOCATED RESOURCE"

# Define a combined classification for CoLocationResource based on the unique values
museum_data_cleaned <- museum_data_cleaned %>%
  mutate(
    CoLocationResource = case_when(
      CoLocationResource %in% c("MUSEUM DISTRICT", "MUSEUM DISTRICT, NATURAL HISTORY MUSEUM", "DALLAS ARTS DISTRICT", "DESIGN DISTRICT", 
                                "MUSEUM DISTRICT (HARVARD)") ~ "MUSEUM DISTRICT",
      CoLocationResource %in% c("GARDEN", "BOTANIC GARDEN", "ARBORETUM, GREENHOUSE", "GALLERIES; GARDENS; BUILDINGS; HISTORY MUSEUM", 
                                "ESTATE; GARDEN") ~ "GARDEN/NATURE CENTER",
      CoLocationResource %in% c("PLANETARIUM", "PLANETARIUM, AQUARIUM", "PLANETARIUM, OBSERVATORY") ~ "PLANETARIUM/OBSERVATORY",
      CoLocationResource %in% c("MULTIPLE BUILDINGS", "MULTIPLE GALLERIES", "MULTIPLE GALLERIES, FINE ARTS LIBRARY", 
                                "MULTIPLE BUILDING & COLLECTIONS") ~ "MULTIPLE BUILDINGS/GALLERIES",
      CoLocationResource %in% c("LIBRARY", "LIBRARY (BUILT 1884)", "ARCHIVES", "ARCHIVES, GALLERY") ~ "LIBRARY/ARCHIVES",
      CoLocationResource %in% c("SCHOOL", "RESEARCH CENTER", "STUDENT PROJECT SPACE", "PERFORMANCE SPACE") ~ "EDUCATIONAL/PERFORMANCE SPACE",
      CoLocationResource %in% c("ART MUSEUM") ~ "ART MUSEUM",
      CoLocationResource %in% c("HISTORY MUSEUM") ~ "HISTORY MUSEUM",
      CoLocationResource %in% c("NATURAL HISTORY MUSEUM") ~ "NATURAL HISTORY MUSEUM",
      CoLocationResource %in% c("SCIENCE MUSEUM") ~ "SCIENCE MUSEUM",
      CoLocationResource %in% c("CHILDRENS MUSEUM") ~ "CHILDRENS MUSEUM",
      TRUE ~ CoLocationResource  # Retain original value if not part of a specified group
    )
  )

## Handling of NTEECode
# Get unique values in the NTEECode column
unique_ntee_codes <- unique(museum_data_cleaned$NTEECode)

# Count the number of blank entries in the NTEECode column
num_blank_ntee <- sum(museum_data_cleaned$NTEECode == " ")

# Filter for rows with blank NTEECode and summarize by NonprofitFlag
ntee_blank_summary <- museum_data_cleaned %>%
  filter(NTEECode == " ") %>%
  group_by(NonprofitFlag) %>%
  summarise(Count = n())

# Replace blank NTEECode entries with "UNCLASSIFIED"
museum_data_cleaned$NTEECode[museum_data_cleaned$NTEECode == " "] <- "UNCLASSIFIED"

# Define the URL
url <- "https://urbaninstitute.github.io/nccs-legacy/ntee/ntee.html"

# Scrape the data from the URL
webpage <- read_html(url)

# Extract NTEE codes and their descriptions
ntee_codes <- webpage %>%
  html_nodes("table") %>%
  html_table(fill = TRUE) %>%
  .[[1]] # Access the first table on the page

# Rename columns to make them clear
colnames(ntee_codes) <- c("NTEECode", "FullDescription")

# Select only the first two columns of ntee_codes using base R
ntee_codes <- ntee_codes[, 1:2]

# Ensure the column names are as expected
colnames(ntee_codes) <- c("NTEECode", "FullDescription")

# Proceed with merging
museum_data_cleaned <- left_join(museum_data_cleaned, ntee_codes, by = "NTEECode")

# Replace NTEECode column with FullDescription where applicable using mutate
museum_data_cleaned <- museum_data_cleaned %>%
  mutate(NTEECode = ifelse(!is.na(FullDescription), FullDescription, NTEECode))

# Remove the FullDescription column using base R
museum_data_cleaned <- museum_data_cleaned[, !colnames(museum_data_cleaned) %in% "FullDescription"]

# Convert NTEECode to uppercase to standardize the format
museum_data_cleaned$NTEECode <- toupper(museum_data_cleaned$NTEECode)

# Replace NTEE codes that match either single letter + two numbers or letter + number + letter with 'Unknown NTEE'
museum_data_cleaned$NTEECode <- ifelse(grepl("^[A-Z]\\d{2}$|^[A-Z]\\d[A-Z]$", museum_data_cleaned$NTEECode), 
                                       "UNKNOWN NTEE", 
                                       museum_data_cleaned$NTEECode)

## Handling of Income Category Code
# Define the mapping for IncomeCategoryCode with ordered levels
income_mapping <- c(
  "0" = "$0",
  "1" = "$1 - $9K",
  "2" = "$10K - $24K",
  "3" = "$25K - $99K",
  "4" = "$100K - $499K",
  "5" = "$500K - $999K",
  "6" = "$1M - $4.9M",
  "7" = "$5M - $9.9M",
  "8" = "$10M - $49.9M",
  "9" = "$50M+"
)

# Apply the mapping and convert to an ordered factor
museum_data_cleaned <- museum_data_cleaned %>%
  mutate(IncomeCategoryCode = factor(income_mapping[as.character(IncomeCategoryCode)],
                                     levels = income_mapping, ordered = TRUE))

## Handling of TaxExemptionDate
# Convert TaxExemptionDate to a proper date format (YYYY-MM)
museum_data_cleaned <- museum_data_cleaned %>%
  mutate(
    TaxExemptionDate = paste0(substr(TaxExemptionDate, 1, 4), "-", substr(TaxExemptionDate, 5, 6))
  )

# Modify InstitutionName based on UniversityAffiliationFlag
museum_data_cleaned$InstitutionName <- ifelse(
  museum_data_cleaned$UniversityAffiliationFlag == 1 & museum_data_cleaned$InstitutionName == " ",
  "Affiliated University, Unknown",
  ifelse(
    museum_data_cleaned$UniversityAffiliationFlag == 0,
    "No University Affiliation",
    museum_data_cleaned$InstitutionName
  )
)

# Summary of final cleaned data
print("Summary of the cleaned dataset:")
summary(museum_data_cleaned)

# ================================
# 4. EXPLORATORY DATA ANALYSIS (EDA)
# ================================
# - Outlier analysis (log scale)
# - RevenueCategory creation (High vs Low)
# - Histograms, boxplots, scatterplots
# - Group comparisons (Discipline, Region, Locale, etc.)
# - Geomap and wordcloud

## CHECK FOR OUTLIERS ------------------------

# Summary statistics for numerical columns
summary(museum_data_cleaned$Income2015)

# Investigate outliers in Income2015
outlier_threshold <- quantile(museum_data_cleaned$Income2015, 0.99) 
outlier_data <- museum_data_cleaned[museum_data_cleaned$Income2015 > outlier_threshold, ]

# Identify outliers where Income2015 is greater than 20,000,000,000
outliers <- museum_data_cleaned[museum_data_cleaned$Income2015 > 20000000000, ]

# Count the number of outliers
num_outliers <- nrow(outliers)
print(paste("Number of outliers in Income2015:", num_outliers))

# Boxplot for Income in log scale (without modifying original data)
boxplot(log(museum_data_cleaned$Income2015 + 1), 
        main = "Boxplot of Income (Log Scale)",
        horizontal = TRUE,
        col = "lightblue",
        border = "darkblue",
        notch = TRUE,
        outline = TRUE,
        xaxt = 'n',
        xlab = "Log Income")

grid(nx = NULL, ny = NULL, col = "lightgray", lty = "dotted")
axis(1, at = axTicks(1), labels = format(exp(axTicks(1)) - 1, big.mark = ",", scientific = FALSE))  # Convert back for display

# Boxplot for Revenue in log scale (without modifying original data)
boxplot(log(museum_data_cleaned$Revenue2015 + 1),  
        main = "Boxplot of Revenue (Log Scale)",
        horizontal = TRUE,
        col = "lightgreen",
        border = "darkgreen",
        notch = TRUE,
        outline = TRUE,
        xaxt = 'n',
        xlab = "Log Revenue")

grid(nx = NULL, ny = NULL, col = "lightgray", lty = "dotted")
axis(1, at = axTicks(1), labels = format(exp(axTicks(1)) - 1, big.mark = ",", scientific = FALSE))  # Convert back for display

# Scatterplot of Income vs Revenue in log scale (without modifying original data)
plot(log(museum_data_cleaned$Income2015 + 1), log(museum_data_cleaned$Revenue2015 + 1),
     main="Scatterplot of Income vs Revenue (Log Scale)",
     xlab="Log Income", ylab="Log Revenue", pch=19, col="darkred",
     xaxt='n', yaxt='n')  # Suppress default axes

axis(1, at = axTicks(1), labels = format(exp(axTicks(1)) - 1, big.mark = ",", scientific = FALSE))  # Convert back for display
axis(2, at = axTicks(2), labels = format(exp(axTicks(2)) - 1, big.mark = ",", scientific = FALSE))  # Convert back for display

# Q-Q Plot for Income2015 (without modifying original data)
qqnorm(museum_data_cleaned$Income2015, 
       main = "Q-Q Plot of Income2015",
       col = "darkblue", 
       pch = 19)
qqline(museum_data_cleaned$Income2015, col = "red", lwd = 2)  
grid(nx = NULL, ny = NULL, col = "lightgray", lty = "dotted")

# Q-Q Plot for Revenue2015 (without modifying original data)
qqnorm(museum_data_cleaned$Revenue2015, 
       main = "Q-Q Plot of Revenue2015",
       col = "darkgreen", 
       pch = 19)
qqline(museum_data_cleaned$Revenue2015, col = "red", lwd = 2) 
grid(nx = NULL, ny = NULL, col = "lightgray", lty = "dotted")

# Create a histogram for Log Revenue2015
ggplot(museum_data_cleaned, aes(x = log(Revenue2015 + 1)) ) +  
  geom_histogram(binwidth = 0.5,  
                 fill = "lightgreen", 
                 color = "black", 
                 alpha = 0.7) +
  scale_x_continuous(labels = scales::comma_format()) +  
  labs(title = "Histogram of Log Revenue (2015)",
       x = "Log Revenue (2015)",
       y = "Count") +
  theme_minimal()

# Boxplot to visualize Revenue
boxplot(museum_data_cleaned$Revenue2015,
        main = "Boxplot of Revenue (2015)",
        horizontal = TRUE,
        col = "lightgreen",
        border = "darkgreen",
        notch = TRUE)

# Boxplot to visualize Income
boxplot(museum_data_cleaned$Income2015,
        main = "Boxplot of Income (2015)",
        horizontal = TRUE,
        col = "lightgreen",
        border = "darkgreen",
        notch = TRUE)

# Use the boxplot stats to set thresholds
box_stats <- boxplot.stats(museum_data_cleaned$Revenue2015)$stats
print(box_stats)

# Calculate the 75th percentile of Revenue2015
threshold_value <- quantile(museum_data_cleaned$Revenue2015, 0.75, na.rm = TRUE)

# Create a new variable for RevenueCategory: "High" or "Low"
museum_data_cleaned$RevenueCategory <- ifelse(museum_data_cleaned$Revenue2015 > threshold_value, "High", "Low")

# Check the distribution of the new RevenueCategory variable
table(museum_data_cleaned$RevenueCategory)

# CHARTS ------------------------

# Create a histogram for Log Revenue2015 
ggplot(museum_data_cleaned, aes(x = log(Revenue2015 + 1))) +  
  geom_histogram(binwidth = 0.5,  
                 fill = "#6a51a3",  
                 color = "#3f007d",  
                 alpha = 0.8) +  
  scale_x_continuous(labels = scales::comma_format()) +  
  labs(title = "Distribution of Log Revenue (2015)",
       x = "Log Revenue (2015)",
       y = "Frequency") +
  theme_minimal(base_size = 15) +  
  theme(
    plot.title = element_text(face = "bold", size = 16, hjust = 0.5),
    axis.title.x = element_text(size = 14),
    axis.title.y = element_text(size = 14),
    axis.text = element_text(size = 12)
  )

# Create a histogram for Log Income2015 
ggplot(museum_data_cleaned, aes(x = log(Income2015 + 1))) +  
  geom_histogram(binwidth = 0.5,  
                 fill = "#43a2ca",  
                 color = "#0868ac",  
                 alpha = 0.8) +  
  scale_x_continuous(labels = scales::comma_format()) +  
  labs(title = "Distribution of Log Income (2015)",
       x = "Log Income (2015)",
       y = "Frequency") +
  theme_minimal(base_size = 15) +  
  theme(
    plot.title = element_text(face = "bold", size = 16, hjust = 0.5),
    axis.title.x = element_text(size = 14),
    axis.title.y = element_text(size = 14),
    axis.text = element_text(size = 12)
  )


# Bar Plots

# Create a bar plot for RevenueCategory by Museum Discipline
ggplot(museum_data_cleaned, aes(x = MuseumDiscipline, fill = RevenueCategory)) +
  geom_bar(position = "dodge") +
  labs(title = "Revenue Distribution by Museum Discipline", 
       x = "Museum Discipline", 
       y = "Count", 
       fill = "Revenue Category") +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, size = 10),  
    plot.margin = unit(c(1, 2, 1, 2), "cm")  
  ) +
  scale_x_discrete(labels = function(x) str_wrap(x, width = 20))  



# Create a bar plot for RevenueCategory by Region
ggplot(museum_data_cleaned, aes(x = Region, fill = RevenueCategory)) +
  geom_bar(position = "dodge") +
  labs(title = "Revenue Distribution by Region", 
       x = "Region", 
       y = "Count", 
       fill = "Revenue Category") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Create a bar plot for RevenueCategory by LocaleType
ggplot(museum_data_cleaned, aes(x = LocaleType, fill = RevenueCategory)) +
  geom_bar(position = "dodge") +
  labs(title = "Revenue Distribution by Locale Type", 
       x = "Locale Type", 
       y = "Count", 
       fill = "Revenue Category") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Create a bar plot for RevenueCategory by CoLocationResource
ggplot(museum_data_cleaned, aes(x = CoLocationResource, fill = RevenueCategory)) +
  geom_bar(position = "dodge") +
  labs(title = "Revenue Distribution by Co-location Resource", 
       x = "Co-location Resource", 
       y = "Count", 
       fill = "Revenue Category") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Create a bar plot for RevenueCategory by State
ggplot(museum_data_cleaned, aes(x = State, fill = RevenueCategory)) +
  geom_bar(position = "dodge") +
  labs(title = "Revenue Distribution by State", 
       x = "State", 
       y = "Count", 
       fill = "Revenue Category") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Create a bar plot for RevenueCategory by IncomeCategoryCode
ggplot(museum_data_cleaned, aes(x = IncomeCategoryCode, fill = RevenueCategory)) +
  geom_bar(position = "dodge") +
  labs(title = "Revenue Distribution by Income Category Code", 
       x = "Income Category Code", 
       y = "Count", 
       fill = "Revenue Category") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Create a bar plot for RevenueCategory by NonprofitFlag
ggplot(museum_data_cleaned, aes(x = factor(NonprofitFlag), fill = RevenueCategory)) +
  geom_bar(position = "dodge") +
  labs(title = "Revenue Distribution by Nonprofit Flag", 
       x = "Nonprofit Flag", 
       y = "Count", 
       fill = "Revenue Category") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Create a bar plot for RevenueCategory by PrivateFoundationFlag
ggplot(museum_data_cleaned, aes(x = factor(PrivateFoundationFlag), fill = RevenueCategory)) +
  geom_bar(position = "dodge") +
  labs(title = "Revenue Distribution by Private Foundation Flag", 
       x = "Private Foundation Flag", 
       y = "Count", 
       fill = "Revenue Category") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Create a bar plot for RevenueCategory by FinancialTypeFlag
ggplot(museum_data_cleaned, aes(x = factor(FinancialTypeFlag), fill = RevenueCategory)) +
  geom_bar(position = "dodge") +
  labs(title = "Revenue Distribution by Financial Type Flag", 
       x = "Financial Type Flag", 
       y = "Count", 
       fill = "Revenue Category") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Create a bar plot for RevenueCategory by UniversityAffiliationFlag
ggplot(museum_data_cleaned, aes(x = factor(UniversityAffiliationFlag), fill = RevenueCategory)) +
  geom_bar(position = "dodge") +
  labs(title = "Revenue Distribution by University Affiliation Flag", 
       x = "University Affiliation Flag", 
       y = "Count", 
       fill = "Revenue Category") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# BOXPLOT FOR CONTINUOUS VARIABLES (WITHOUT LOG TRANSFORMATION)
# Boxplot for Income grouped by RevenueCategory
plot_income <- ggplot(museum_data_cleaned, aes(x = RevenueCategory, y = Income2015)) +
  geom_boxplot(fill = "lightblue", color = "darkblue", notch = TRUE) +
  scale_y_continuous(labels = scales::comma) +
  labs(title = "Boxplot of Income Grouped by Revenue Category",
       x = "Revenue Category (High / Low)",
       y = "Income (2015)") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 0, hjust = 1))

# Boxplot for Revenue grouped by RevenueCategory
plot_revenue <- ggplot(museum_data_cleaned, aes(x = RevenueCategory, y = Revenue2015)) +
  geom_boxplot(fill = "lightcoral", color = "darkred", notch = TRUE) +
  scale_y_continuous(labels = scales::comma) +
  labs(title = "Boxplot of Revenue Grouped by Revenue Category",
       x = "Revenue Category (High / Low)",
       y = "Revenue (2015)") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 0, hjust = 1))

# Combine the two boxplots using cowplot
cowplot::plot_grid(plot_income, plot_revenue, labels = "AUTO", ncol = 2)

# Top 10 income and revenue makers
# Define the selected columns
selected_columns <- c("CoLocationResource", "MuseumName", "City", "State", "IncomeCategoryCode", "Income2015", "Revenue2015", "InstitutionName", 
                      "LocaleType", "MuseumDiscipline")

# Top 10 Museums by Income
top10_income <- museum_data_cleaned[order(-museum_data_cleaned$Income2015), ][1:10, selected_columns]

print("Top 10 Museums by Income:")
print(top10_income)

# Top 10 Museums by Revenue
top10_revenue <- museum_data_cleaned[order(-museum_data_cleaned$Revenue2015), ][1:10, selected_columns]

print("Top 10 Museums by Revenue:")
print(top10_revenue)

## STACKED BAR CHARTS
# Stacked bar chart for MuseumDiscipline
ggplot(museum_data_cleaned, aes(x = MuseumDiscipline, fill = RevenueCategory)) +
  geom_bar(position = "fill") +  # Fill to show proportions
  labs(title = "Proportion of High and Low Revenue by Museum Discipline", 
       x = "Museum Discipline", y = "Proportion", fill = "Revenue Category") +
  scale_fill_manual(values = c("#6baed6", "#fd8d3c"),  
                    labels = c("High Revenue", "Low Revenue")) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Stacked bar chart for Region
ggplot(museum_data_cleaned, aes(x = Region, fill = RevenueCategory)) +
  geom_bar(position = "fill") +  # Fill to show proportions
  labs(title = "Proportion of High and Low Revenue by Region", 
       x = "Region", y = "Proportion", fill = "Revenue Category") +
  scale_fill_manual(values = c("#6baed6", "#fd8d3c"),  
                    labels = c("High Revenue", "Low Revenue")) +
  theme_minimal()

# Stacked bar chart for LocaleType
ggplot(museum_data_cleaned, aes(x = LocaleType, fill = RevenueCategory)) +
  geom_bar(position = "fill") +  # Fill to show proportions
  labs(title = "Proportion of High and Low Revenue by Locale Type", 
       x = "Locale Type", y = "Proportion", fill = "Revenue Category") +
  scale_fill_manual(values = c("#6baed6", "#fd8d3c"),  
                    labels = c("High Revenue", "Low Revenue")) +
  theme_minimal()

# Stacked bar chart for State
ggplot(museum_data_cleaned, aes(x = State, fill = RevenueCategory)) +
  geom_bar(position = "fill") +  # Fill to show proportions
  labs(title = "Proportion of High and Low Revenue by State", 
       x = "State", y = "Proportion", fill = "Revenue Category") +
  scale_fill_manual(values = c("#6baed6", "#fd8d3c"),  
                    labels = c("High Revenue", "Low Revenue")) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5))

# Stacked bar chart for IncomeCategoryCode
ggplot(museum_data_cleaned, aes(x = IncomeCategoryCode, fill = RevenueCategory)) +
  geom_bar(position = "fill") +  # Fill to show proportions
  labs(title = "Proportion of High and Low Revenue by Income Category", 
       x = "Income Category", y = "Proportion", fill = "Revenue Category") +
  scale_fill_manual(values = c("#6baed6", "#fd8d3c"),  
                    labels = c("High Revenue", "Low Revenue")) +
  theme_minimal()

# Stacked bar chart for NonprofitFlag
ggplot(museum_data_cleaned, aes(x = factor(NonprofitFlag), fill = RevenueCategory)) +
  geom_bar(position = "fill") +
  labs(title = "Proportion of High and Low Revenue by Nonprofit Status", 
       x = "Nonprofit Status", y = "Proportion", fill = "Revenue Category") +
  scale_fill_manual(values = c("#6baed6", "#fd8d3c"),
                    labels = c("High Revenue", "Low Revenue")) +
  theme_minimal()

# Stacked bar chart for PrivateFoundationFlag
ggplot(museum_data_cleaned, aes(x = factor(PrivateFoundationFlag), fill = RevenueCategory)) +
  geom_bar(position = "fill") +
  labs(title = "Proportion of High and Low Revenue by Private Foundation Status", 
       x = "Private Foundation Status", y = "Proportion", fill = "Revenue Category") +
  scale_fill_manual(values = c("#6baed6", "#fd8d3c"),
                    labels = c("High Revenue", "Low Revenue")) +
  theme_minimal()

# Stacked bar chart for FinancialTypeFlag
ggplot(museum_data_cleaned, aes(x = factor(FinancialTypeFlag), fill = RevenueCategory)) +
  geom_bar(position = "fill") +
  labs(title = "Proportion of High and Low Revenue by Financial Type", 
       x = "Financial Type", y = "Proportion", fill = "Revenue Category") +
  scale_fill_manual(values = c("#6baed6", "#fd8d3c"),
                    labels = c("High Revenue", "Low Revenue")) +
  theme_minimal()

# Top unique museums by revenue (considering unique revenue values)
top_unique_revenue <- museum_data_cleaned %>%
  distinct(Revenue2015, .keep_all = TRUE) %>% 
  arrange(desc(Revenue2015)) %>%
  head(10)

# Plot the top 20 institutions by unique revenue in descending order
ggplot(top_unique_revenue, aes(x = reorder(InstitutionName, Revenue2015), y = Revenue2015, fill = MuseumDiscipline)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  scale_y_continuous(labels = scales::comma) +
  labs(title = "Top 10 Institutions by Unique Revenue (2015)",
       x = "Institution Name",
       y = "Revenue (2015)",
       fill = "Museum Discipline") +
  theme_minimal() +
  theme(axis.text.y = element_text(size = 10, hjust = 1))

# Create a map of the USA
us_map <- map_data("state")

# Filter to keep only museums within typical longitude and latitude bounds for the USA, including Alaska and Hawaii
museum_data_filtered <- museum_data_cleaned %>%
  filter(
    (Longitude >= -125 & Longitude <= -65 & Latitude >= 25 & Latitude <= 50) | 
      (Longitude >= -179 & Longitude <= -129 & Latitude >= 50 & Latitude <= 72) | 
      (Longitude >= -160 & Longitude <= -154 & Latitude >= 18 & Latitude <= 23)   
  )

# Plot the map
ggplot() +
  geom_polygon(data = us_map, aes(x = long, y = lat, group = group), fill = "gray90", color = "white") +
  geom_point(data = museum_data_filtered, aes(x = Longitude, y = Latitude, color = RevenueCategory), alpha = 0.6, size = 3) +
  scale_color_manual(values = c("High" = "#E74C3C", "Low" = "#3498DB")) +  
  labs(title = "Distribution of Museums in the USA by Revenue Category",
       x = "Longitude",
       y = "Latitude") +
  theme_minimal() +
  theme(legend.position = "bottom")

# Generate a word cloud for museum names
# Clean the MuseumName column by converting it to valid UTF-8 and removing NA values
museum_names_cleaned <- museum_data_cleaned$MuseumName
museum_names_cleaned <- iconv(museum_names_cleaned, from = "UTF-8", to = "ASCII", sub = "")
museum_names_cleaned <- na.omit(museum_names_cleaned)

# Generate the word cloud
wordcloud(words = museum_names_cleaned, 
          scale = c(3, 0.5), 
          max.words = 100, 
          random.order = FALSE, 
          colors = brewer.pal(8, "Dark2"))


# ================================
# 5. MODELING: REVENUE CLASSIFICATION
# ================================
# - Train/test split
# - Logistic Regression with threshold tuning
# - Random Forest with CV tuning
# - Ridge Regression
# - Gradient Boosting (GBM)
# - Naive Bayes

### MODEL 1: Predict whether a museum falls into a category of high revenue or low revenue based on the total revenue --------------------------
# Split the dataset into training and testing sets
# Convert categorical variables to factors
museum_data_cleaned$RevenueCategory <- as.factor(museum_data_cleaned$RevenueCategory)
museum_data_cleaned$MuseumDiscipline <- as.factor(museum_data_cleaned$MuseumDiscipline)
museum_data_cleaned$Region <- as.factor(museum_data_cleaned$Region)
museum_data_cleaned$LocaleType <- as.factor(museum_data_cleaned$LocaleType)
museum_data_cleaned$CoLocationResource <- as.factor(museum_data_cleaned$CoLocationResource)
museum_data_cleaned$IncomeCategoryCode <- as.factor(museum_data_cleaned$IncomeCategoryCode)
museum_data_cleaned$NonprofitFlag <- as.factor(museum_data_cleaned$NonprofitFlag)
museum_data_cleaned$PrivateFoundationFlag <- as.factor(museum_data_cleaned$PrivateFoundationFlag)
museum_data_cleaned$FinancialTypeFlag <- as.factor(museum_data_cleaned$FinancialTypeFlag)
museum_data_cleaned$UniversityAffiliationFlag <- as.factor(museum_data_cleaned$UniversityAffiliationFlag)

# Consolidate low-frequency levels into 'Other'
consolidate_low_freq <- function(factor_var, threshold = 5) {
  levels_count <- table(factor_var)
  levels_to_replace <- names(levels_count[levels_count < threshold])
  levels(factor_var)[levels(factor_var) %in% levels_to_replace] <- "Other"
  return(factor_var)
}

museum_data_cleaned$MuseumDiscipline <- consolidate_low_freq(museum_data_cleaned$MuseumDiscipline)
museum_data_cleaned$Region <- consolidate_low_freq(museum_data_cleaned$Region)
museum_data_cleaned$LocaleType <- consolidate_low_freq(museum_data_cleaned$LocaleType)
museum_data_cleaned$CoLocationResource <- consolidate_low_freq(museum_data_cleaned$CoLocationResource)
museum_data_cleaned$IncomeCategoryCode <- consolidate_low_freq(museum_data_cleaned$IncomeCategoryCode)

# Create RevenueCategory_numeric 
museum_data_cleaned <- museum_data_cleaned %>%
  mutate(RevenueCategory_numeric = ifelse(RevenueCategory == "High", 1, 0))

# Set a seed for reproducibility
set.seed(123)

# Split the data into training (70%) and testing (30%) sets
trainIndex <- createDataPartition(museum_data_cleaned$RevenueCategory, p = 0.7, 
                                  list = FALSE, 
                                  times = 1)
museum_train <- museum_data_cleaned[trainIndex, ]
museum_test <- museum_data_cleaned[-trainIndex, ]

# Ensure levels in the test set match the levels in the training set for all factor variables
factor_vars <- sapply(museum_train, is.factor)
for (var in names(factor_vars[factor_vars])) {
  museum_test[[var]] <- factor(museum_test[[var]], levels = levels(museum_train[[var]]))
}

# Confirm the distribution of the target variable in both sets
table(museum_train$RevenueCategory)
table(museum_test$RevenueCategory)

# Check the structure of the training and testing datasets
str(museum_train)
str(museum_test)

### PART A: MODEL BUILDING ------------------------------------------------------------------------------------------------------------------------------------
## LOGISTIC REGRESSION MODEL IMPLEMENTATION WITH THRESHOLD TUNING --------------------------------------------------------------------------------
# Build a logistic regression model to predict RevenueCategory using selected important variables
logistic_model <- glm(RevenueCategory ~ MuseumDiscipline + LocaleType + Region + CoLocationResource + State +
                        NonprofitFlag + PrivateFoundationFlag + FinancialTypeFlag + UniversityAffiliationFlag, 
                      data = museum_train, family = binomial)

# Summary of the logistic regression model
summary(logistic_model)

# Make predictions on the test set (probabilities)
logistic_test_probabilities <- predict(logistic_model, museum_test, type = "response")

# Hyperparameter Tuning: Find the Best Threshold --------------------------------------------------------------------------
thresh_seq <- seq(0.1, 0.9, by = 0.01)  # Sequence of thresholds to test
best_f1 <- 0
best_thresh <- 0.5  # Default threshold

f1_scores <- c()  # Store F1 scores for plotting
for (thresh in thresh_seq) {
  logistic_test_predicted_classes <- ifelse(logistic_test_probabilities > thresh, "High", "Low")
  logistic_test_predicted_factor <- factor(logistic_test_predicted_classes, levels = levels(museum_test$RevenueCategory))
  cm <- confusionMatrix(logistic_test_predicted_factor, museum_test$RevenueCategory)
  f1 <- cm$byClass["F1"]  # Extract F1-score from confusion matrix
  f1_scores <- c(f1_scores, f1)
  
  if (!is.na(f1) && f1 > best_f1) {
    best_f1 <- f1
    best_thresh <- thresh
  }
}

cat("Best Threshold based on F1-Score: ", round(best_thresh, 2), " with F1-Score: ", round(best_f1, 4), "\n")

# Plot F1-Score vs. Threshold
plot(thresh_seq, f1_scores, type = "l", col = "blue", lwd = 2, 
     xlab = "Threshold", ylab = "F1-Score", main = "F1-Score vs. Threshold for Logistic Regression")
abline(v = best_thresh, col = "red", lty = 2)
legend("topright", legend = c("F1-Score", "Best Threshold"), col = c("blue", "red"), lty = c(1, 2))

# Make final predictions on the test set using the best threshold
logistic_test_predicted_classes <- ifelse(logistic_test_probabilities > best_thresh, "High", "Low")

# Confusion matrix for Logistic Regression on test set
logistic_test_predicted_factor <- factor(logistic_test_predicted_classes, levels = levels(museum_test$RevenueCategory))
logistic_confusion_matrix <- confusionMatrix(logistic_test_predicted_factor, museum_test$RevenueCategory)

# Calculate metrics for Logistic Regression
logistic_accuracy <- logistic_confusion_matrix$overall["Accuracy"]
logistic_precision <- logistic_confusion_matrix$byClass["Precision"]
logistic_sensitivity <- logistic_confusion_matrix$byClass["Sensitivity"]
logistic_specificity <- logistic_confusion_matrix$byClass["Specificity"]
logistic_f1 <- logistic_confusion_matrix$byClass["F1"]

# ROC and AUC for Logistic Regression
logistic_roc <- roc(museum_test$RevenueCategory, as.numeric(logistic_test_probabilities))
logistic_auc <- auc(logistic_roc)

## Random Forest Model ------------------------------------------------------------------------------------------------------------------
# Expanded hyperparameter tuning with a wider range for mtry and ntree
tune_grid <- expand.grid(mtry = c(3,6,9))
control <- trainControl(method = "cv", number = 5)

# Train Random Forest model using caret's train function
rf_model <- train(RevenueCategory ~ MuseumDiscipline + LocaleType + Region + CoLocationResource + State + NonprofitFlag + PrivateFoundationFlag + FinancialTypeFlag + UniversityAffiliationFlag,
                  data = museum_train,
                  method = "rf",
                  trControl = control,
                  tuneGrid = tune_grid,
                  ntree = 300) 

# Print the best tuning parameters
print(rf_model$bestTune)

# Plot Cross-Validation Results for Random Forest
plot(rf_model, main = "Cross-Validation Results for Random Forest")

# Make predictions on the test set using the best model
# Make predictions on the test set using the best model
rf_probabilities <- predict(rf_model, museum_test, type = "prob")

# ROC and AUC for Random Forest
rf_roc <- roc(museum_test$RevenueCategory, rf_probabilities[, "High"])
rf_auc <- auc(rf_roc)

# Identify the optimal threshold from the ROC curve
youden_index_rf <- which.max(rf_roc$sensitivities + rf_roc$specificities - 1)
optimal_threshold_rf <- rf_roc$thresholds[youden_index_rf]
print(paste("Optimal Threshold for Random Forest: ", round(optimal_threshold_rf, 4)))

# Make final predictions using the optimal threshold
rf_predictions <- ifelse(rf_probabilities[, "High"] > optimal_threshold_rf, "High", "Low")
rf_predictions_factor <- factor(rf_predictions, levels = levels(museum_test$RevenueCategory))

# Confusion matrix for Random Forest on test set
rf_confusion_matrix <- confusionMatrix(rf_predictions_factor, museum_test$RevenueCategory)

# Calculate metrics for Random Forest
rf_accuracy <- rf_confusion_matrix$overall["Accuracy"]
rf_precision <- rf_confusion_matrix$byClass["Precision"]
rf_sensitivity <- rf_confusion_matrix$byClass["Sensitivity"]
rf_specificity <- rf_confusion_matrix$byClass["Specificity"]
rf_f1 <- rf_confusion_matrix$byClass["F1"]

## Ridge Regression ------------------------------------------------------------------------------------------------------------------------
# Prepare data for Ridge Regression
x_train <- model.matrix(RevenueCategory ~ MuseumDiscipline + LocaleType + Region + CoLocationResource + 
                          State + NonprofitFlag + PrivateFoundationFlag + FinancialTypeFlag + UniversityAffiliationFlag, data = museum_train)[, -1]
y_train <- ifelse(museum_train$RevenueCategory == "High", 1, 0)

x_test <- model.matrix(RevenueCategory ~ MuseumDiscipline + LocaleType + Region + CoLocationResource + State + 
                         NonprofitFlag + PrivateFoundationFlag + FinancialTypeFlag + UniversityAffiliationFlag, data = museum_test)[, -1]
y_test <- ifelse(museum_test$RevenueCategory == "High", 1, 0)

# Fit Ridge Regression model
set.seed(123)
ridge_model <- cv.glmnet(x_train, y_train, alpha = 0, family = "binomial")

# Get the best lambda value
best_lambda_ridge <- ridge_model$lambda.min
print(paste("Best Lambda for Ridge Regression: ", round(best_lambda_ridge, 4)))

# Make predictions on the test set
ridge_probabilities <- predict(ridge_model, s = best_lambda_ridge, newx = x_test, type = "response")

# Plot cross-validation results with title
par(mfrow = c(1, 1)) # Reset any multiple plot settings
plot(ridge_model)
title(main = "Cross-Validation Error vs. Lambda for Ridge Regression")

# Make predictions on the test set
ridge_probabilities <- predict(ridge_model, s = best_lambda_ridge, newx = x_test, type = "response")
ridge_predicted_classes <- ifelse(ridge_probabilities > 0.5, 1, 0)

# Confusion matrix for Ridge Regression on test set
ridge_predicted_factor <- factor(ridge_predicted_classes, levels = c(0, 1), labels = c("Low", "High"))
y_test_factor <- factor(y_test, levels = c(0, 1), labels = c("Low", "High"))
ridge_confusion_matrix <- confusionMatrix(ridge_predicted_factor, y_test_factor)

# Calculate metrics for Ridge Regression
ridge_accuracy <- ridge_confusion_matrix$overall["Accuracy"]
ridge_precision <- ridge_confusion_matrix$byClass["Precision"]
ridge_sensitivity <- ridge_confusion_matrix$byClass["Sensitivity"]
ridge_specificity <- ridge_confusion_matrix$byClass["Specificity"]
ridge_f1 <- ridge_confusion_matrix$byClass["F1"]

# ROC and AUC for Ridge Regression
ridge_roc <- roc(y_test, as.numeric(ridge_probabilities))
ridge_auc <- auc(ridge_roc)

# Extract coefficients for the ridge regression
ridge_coefficients <- coef(ridge_model, s = best_lambda_ridge)

# Convert the coefficients to a data frame for easier analysis
coefficients_df <- as.data.frame(as.matrix(ridge_coefficients))
names(coefficients_df) <- c("Coefficient")

# Print the sorted coefficients by absolute value to understand importance
coefficients_df$Feature <- rownames(coefficients_df)
coefficients_df <- coefficients_df[order(-abs(coefficients_df$Coefficient)), ]
print(coefficients_df)

# Filter the top 10 coefficients by absolute value
top_10_coefficients <- head(coefficients_df, 10)

# Plotting the top 10 coefficients
ggplot(top_10_coefficients, aes(x = reorder(Feature, abs(Coefficient)), y = Coefficient, fill = Coefficient)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  theme_minimal() +
  labs(title = "Top 10 Most Influential Variables in Ridge Regression",
       x = "Feature",
       y = "Coefficient Value") +
  scale_fill_gradient2(low = "blue", mid = "white", high = "red", midpoint = 0) +
  theme(axis.text.y = element_text(size = 10, face = "bold"),
        axis.title = element_text(size = 12),
        plot.title = element_text(size = 14, face = "bold", hjust = 0.5))

## GRADIENT BOOSTING MACHINE (GBM) IMPLEMENTATION ----------------------------------------------------------------------------------------
# Ensure all predictor variables are factors or numeric
museum_train$State <- as.factor(museum_train$State)
museum_test$State <- as.factor(museum_test$State)

# Convert the response variable to binary format (0 and 1)
museum_train$RevenueCategory <- ifelse(museum_train$RevenueCategory == "High", 1, 0)
museum_test$RevenueCategory <- ifelse(museum_test$RevenueCategory == "High", 1, 0)

# Fit GBM model
gbm_model <- gbm(RevenueCategory ~ MuseumDiscipline + LocaleType + Region + CoLocationResource + State + 
                   NonprofitFlag + PrivateFoundationFlag + FinancialTypeFlag + UniversityAffiliationFlag,
                 data = museum_train, 
                 distribution = "bernoulli",
                 n.trees = 500,
                 interaction.depth = 3,
                 shrinkage = 0.01,
                 cv.folds = 5,
                 n.cores = NULL, # Use all cores by default
                 verbose = FALSE)

# Determine the best number of trees using cross-validation
best_iter <- gbm.perf(gbm_model, method = "cv")

# Plot Cross-Validation Results for GBM
plot(gbm_model$cv.error, type = "l", col = "blue", lwd = 2, 
     xlab = "Number of Trees", ylab = "Cross-Validation Error", 
     main = "Cross-Validation Error vs. Number of Trees for GBM")
abline(v = best_iter, col = "red", lty = 2)
legend("topright", legend = c("CV Error", "Best Iteration"), col = c("blue", "red"), lty = c(1, 2))

# Make predictions on the test set
gbm_probabilities <- predict(gbm_model, museum_test, n.trees = best_iter, type = "response")
gbm_predicted_classes <- ifelse(gbm_probabilities > 0.5, 1, 0)

# ROC and AUC for GBM
gbm_roc <- roc(y_test, as.numeric(gbm_probabilities))
gbm_auc <- auc(gbm_roc)

# Confusion matrix for GBM on test set
gbm_predicted_factor <- factor(gbm_predicted_classes, levels = c(0, 1), labels = c("Low", "High"))
y_test_factor <- factor(y_test, levels = c(0, 1), labels = c("Low", "High"))
gbm_confusion_matrix <- confusionMatrix(gbm_predicted_factor, y_test_factor)

# Calculate metrics for GBM
gbm_accuracy <- gbm_confusion_matrix$overall["Accuracy"]
gbm_precision <- gbm_confusion_matrix$byClass["Precision"]
gbm_sensitivity <- gbm_confusion_matrix$byClass["Sensitivity"]
gbm_specificity <- gbm_confusion_matrix$byClass["Specificity"]
gbm_f1 <- gbm_confusion_matrix$byClass["F1"]

# Feature Importance for GBM
# Get variable importance from the GBM model
gbm_importance <- summary(gbm_model, n.trees = best_iter)

# Print variable importance
print(gbm_importance)

# Plot variable importance
barplot(gbm_importance$rel.inf, names.arg = gbm_importance$var, las = 2, cex.names = 0.7,
        main = "Variable Importance for GBM Model", col = "skyblue", horiz = TRUE)

# Adjust the width of the plot (for RStudio or if saving to a file)
par(mai = c(1, 2, 1, 1)) # Increasing left margin to make space for labels
barplot(gbm_importance$rel.inf, 
        names.arg = gbm_importance$var, 
        las = 2,                       
        cex.names = 0.7,               
        main = "Top Features by Importance in GBM Model", 
        col = "skyblue",               
        horiz = TRUE,                  
        xlab = "Relative Influence")


## NAIVE BAYES --------------------------------------------------------------------------------------------------------------------------------
# Convert RevenueCategory back to factor for classification
museum_train$RevenueCategory <- as.factor(museum_train$RevenueCategory)
museum_test$RevenueCategory <- as.factor(museum_test$RevenueCategory)

# Train a Naive Bayes Model
naive_bayes_model <- naiveBayes(RevenueCategory ~ MuseumDiscipline + LocaleType + Region + CoLocationResource + 
                                  State + NonprofitFlag + PrivateFoundationFlag + FinancialTypeFlag + 
                                  UniversityAffiliationFlag, 
                                data = museum_train)

# Summary of the Naive Bayes Model
print(naive_bayes_model)

# Make predictions on the test set using the Naive Bayes model
naive_bayes_probabilities <- predict(naive_bayes_model, museum_test, type = "raw")

# Check the column names of the predicted probabilities to determine the correct label
print(colnames(naive_bayes_probabilities))  # This will show you if you should use "0" or "1"

# Assuming "1" is the label for high revenue, use it to calculate the ROC and AUC
naive_bayes_roc <- roc(museum_test$RevenueCategory, naive_bayes_probabilities[, "1"])  # Replace "High" with "1"

# Calculate and print AUC for the Naive Bayes model
naive_bayes_auc <- auc(naive_bayes_roc)

# Make class predictions on the test set
naive_bayes_predictions <- predict(naive_bayes_model, museum_test)

# Convert predictions to factor to match the actual labels
naive_bayes_predictions_factor <- as.factor(naive_bayes_predictions)
museum_test_factor <- as.factor(museum_test$RevenueCategory)

# Confusion Matrix for Naive Bayes model
naive_bayes_confusion_matrix <- confusionMatrix(naive_bayes_predictions_factor, museum_test_factor)

# Calculate metrics for Naive Bayes Model
naive_bayes_accuracy <- naive_bayes_confusion_matrix$overall["Accuracy"]
naive_bayes_precision <- naive_bayes_confusion_matrix$byClass["Precision"]
naive_bayes_sensitivity <- naive_bayes_confusion_matrix$byClass["Sensitivity"]
naive_bayes_specificity <- naive_bayes_confusion_matrix$byClass["Specificity"]
naive_bayes_f1 <- naive_bayes_confusion_matrix$byClass["F1"]

# METRICS PRINTING ------------------------------------------------------------------------------------------------------------------------

# Print all metrics for Logistic Regression
cat("\nLogistic Regression Metrics:\n")
print(paste("Logistic Regression Accuracy: ", round(logistic_accuracy, 4)))
print(paste("Logistic Regression Precision: ", round(logistic_precision, 4)))
print(paste("Logistic Regression Sensitivity: ", round(logistic_sensitivity, 4)))
print(paste("Logistic Regression Specificity: ", round(logistic_specificity, 4)))
print(paste("Logistic Regression F1 Score: ", round(logistic_f1, 4)))
print(paste("Logistic Regression AUC: ", round(logistic_auc, 4)))

# Print all metrics for Random Forest
cat("\nRandom Forest Metrics:\n")
print(paste("Random Forest Accuracy: ", round(rf_accuracy, 4)))
print(paste("Random Forest Precision: ", round(rf_precision, 4)))
print(paste("Random Forest Sensitivity: ", round(rf_sensitivity, 4)))
print(paste("Random Forest Specificity: ", round(rf_specificity, 4)))
print(paste("Random Forest F1 Score: ", round(rf_f1, 4)))
print(paste("Random Forest AUC: ", round(rf_auc, 4)))

# Print all metrics for Ridge Regression
print(paste("Ridge Regression Accuracy: ", round(ridge_accuracy, 4)))
print(paste("Ridge Regression Precision: ", round(ridge_precision, 4)))
print(paste("Ridge Regression Sensitivity: ", round(ridge_sensitivity, 4)))
print(paste("Ridge Regression Specificity: ", round(ridge_specificity, 4)))
print(paste("Ridge Regression F1 Score: ", round(ridge_f1, 4)))
print(paste("Ridge Regression AUC: ", round(ridge_auc, 4)))

# Print metrics for GBM
print(paste("GBM Accuracy: ", round(gbm_accuracy, 4)))
print(paste("GBM Precision: ", round(gbm_precision, 4)))
print(paste("GBM Sensitivity: ", round(gbm_sensitivity, 4)))
print(paste("GBM Specificity: ", round(gbm_specificity, 4)))
print(paste("GBM F1 Score: ", round(gbm_f1, 4)))
print(paste("GBM AUC: ", round(gbm_auc, 4)))

# Print metrics for Naive Bayes 
print(paste("Naive Bayes Model Accuracy:", round(naive_bayes_accuracy, 4)))
print(paste("Naive Bayes Model Precision:", round(naive_bayes_precision, 3)))
print(paste("Naive Bayes Model Sensitivity (Recall for High Revenue):", round(naive_bayes_sensitivity, 4)))
print(paste("Naive Bayes Model Specificity (Recall for Low Revenue):", round(naive_bayes_specificity, 4)))
print(paste("Naive Bayes Model F1 Score:", round(naive_bayes_f1, 4)))
print(paste("Naive Bayes Model AUC:", round(naive_bayes_auc, 4)))

### PART B: CROSS VALIDATION ###

# Set the number of iterations for cross-validation
B <- 50

# Create empty lists to store metrics for all models
metrics_logit <- list(accuracy = c(), precision = c(), sensitivity = c(), specificity = c(), f1_score = c(), auc = c())
metrics_rf <- list(accuracy = c(), precision = c(), sensitivity = c(), specificity = c(), f1_score = c(), auc = c())
metrics_ridge <- list(accuracy = c(), precision = c(), sensitivity = c(), specificity = c(), f1_score = c(), auc = c())
metrics_gbm <- list(accuracy = c(), precision = c(), sensitivity = c(), specificity = c(), f1_score = c(), auc = c())
metrics_nb <- list(accuracy = c(), precision = c(), sensitivity = c(), specificity = c(), f1_score = c(), auc = c())

# Set seed for reproducibility
set.seed(7406)

# Use best hyperparameters from Part A
best_ntree_rf <- 300  # Example: Optimal number of trees for Random Forest from Part A
best_mtry_rf <- 4     # Example: Optimal mtry for Random Forest from Part A
best_lambda_ridge <- 0.01  # Example: Optimal lambda for Ridge Regression from Part A
best_iter_gbm <- 300  # Example: Optimal number of boosting iterations for GBM from Part A
best_thresh <- 0.5  # Optimal threshold for logistic regression (can be adjusted based on Part A)

for (b in 1:B) {
  # Create a random 70%/30% train-test split
  trainIndex <- createDataPartition(museum_data_cleaned$RevenueCategory, p = 0.70, list = FALSE, times = 1)
  train_set_cv <- museum_data_cleaned[trainIndex, ]
  test_set_cv <- museum_data_cleaned[-trainIndex, ]
  
  # Ensure factor levels match between train and test sets
  factor_vars <- sapply(train_set_cv, is.factor)
  for (var in names(factor_vars[factor_vars])) {
    test_set_cv[[var]] <- factor(test_set_cv[[var]], levels = levels(train_set_cv[[var]]))
  }
  
  # Convert State to factor if it is not already
  if (!is.factor(train_set_cv$State)) {
    train_set_cv$State <- as.factor(train_set_cv$State)
    test_set_cv$State <- as.factor(test_set_cv$State)
  }
  
  tryCatch({
    # Suppress warnings regarding ROC settings
    suppressWarnings({
      # 1. Logistic Regression Model
      logistic_model_cv <- glm(RevenueCategory ~ MuseumDiscipline + LocaleType + Region + CoLocationResource + State +
                                 NonprofitFlag + PrivateFoundationFlag + FinancialTypeFlag + UniversityAffiliationFlag, 
                               data = train_set_cv, family = binomial)
      
      logistic_test_probabilities <- predict(logistic_model_cv, test_set_cv, type = "response")
      logistic_test_predicted <- ifelse(logistic_test_probabilities > best_thresh, "High", "Low")
      
      # Confusion matrix for logistic regression
      cm_logit <- confusionMatrix(factor(logistic_test_predicted, levels = levels(test_set_cv$RevenueCategory)), test_set_cv$RevenueCategory)
      metrics_logit$accuracy <- c(metrics_logit$accuracy, cm_logit$overall['Accuracy'])
      metrics_logit$precision <- c(metrics_logit$precision, cm_logit$byClass['Precision'])
      metrics_logit$sensitivity <- c(metrics_logit$sensitivity, cm_logit$byClass['Sensitivity'])
      metrics_logit$specificity <- c(metrics_logit$specificity, cm_logit$byClass['Specificity'])
      metrics_logit$f1_score <- c(metrics_logit$f1_score, cm_logit$byClass['F1'])
      
      # AUC for logistic regression
      logistic_roc_cv <- roc(test_set_cv$RevenueCategory, as.numeric(logistic_test_probabilities), quiet = TRUE)
      metrics_logit$auc <- c(metrics_logit$auc, auc(logistic_roc_cv))
      
      # 2. Random Forest Model
      rf_model_cv <- randomForest(RevenueCategory ~ MuseumDiscipline + LocaleType + Region + CoLocationResource + State +
                                    NonprofitFlag + PrivateFoundationFlag + FinancialTypeFlag + UniversityAffiliationFlag,
                                  data = train_set_cv, ntree = best_ntree_rf, mtry = best_mtry_rf)
      
      rf_test_pred <- predict(rf_model_cv, test_set_cv, type = "prob")
      rf_test_pred_class <- ifelse(rf_test_pred[, "High"] > 0.5, "High", "Low")
      
      # Confusion matrix for random forest
      cm_rf <- confusionMatrix(factor(rf_test_pred_class, levels = levels(test_set_cv$RevenueCategory)), test_set_cv$RevenueCategory)
      metrics_rf$accuracy <- c(metrics_rf$accuracy, cm_rf$overall['Accuracy'])
      metrics_rf$precision <- c(metrics_rf$precision, cm_rf$byClass['Precision'])
      metrics_rf$sensitivity <- c(metrics_rf$sensitivity, cm_rf$byClass['Sensitivity'])
      metrics_rf$specificity <- c(metrics_rf$specificity, cm_rf$byClass['Specificity'])
      metrics_rf$f1_score <- c(metrics_rf$f1_score, cm_rf$byClass['F1'])
      
      # AUC for random forest
      rf_roc_cv <- roc(test_set_cv$RevenueCategory, rf_test_pred[, "High"], quiet = TRUE)
      metrics_rf$auc <- c(metrics_rf$auc, auc(rf_roc_cv))
      
      # 3. Ridge Regression Model
      x_train_cv <- model.matrix(RevenueCategory ~ MuseumDiscipline + LocaleType + Region + CoLocationResource +
                                   State + NonprofitFlag + PrivateFoundationFlag + FinancialTypeFlag + UniversityAffiliationFlag, 
                                 data = train_set_cv)[, -1]
      y_train_cv <- ifelse(train_set_cv$RevenueCategory == "High", 1, 0)
      
      x_test_cv <- model.matrix(RevenueCategory ~ MuseumDiscipline + LocaleType + Region + CoLocationResource + State +
                                  NonprofitFlag + PrivateFoundationFlag + FinancialTypeFlag + UniversityAffiliationFlag, 
                                data = test_set_cv)[, -1]
      y_test_cv <- ifelse(test_set_cv$RevenueCategory == "High", 1, 0)
      
      ridge_model_cv <- glmnet(x_train_cv, y_train_cv, alpha = 0, family = "binomial")
      ridge_test_pred <- predict(ridge_model_cv, s = best_lambda_ridge, newx = x_test_cv, type = "response")
      ridge_test_pred_class <- ifelse(ridge_test_pred > 0.5, "High", "Low")
      
      # Confusion matrix for ridge regression
      cm_ridge <- confusionMatrix(factor(ridge_test_pred_class, levels = levels(test_set_cv$RevenueCategory)), test_set_cv$RevenueCategory)
      metrics_ridge$accuracy <- c(metrics_ridge$accuracy, cm_ridge$overall['Accuracy'])
      metrics_ridge$precision <- c(metrics_ridge$precision, cm_ridge$byClass['Precision'])
      metrics_ridge$sensitivity <- c(metrics_ridge$sensitivity, cm_ridge$byClass['Sensitivity'])
      metrics_ridge$specificity <- c(metrics_ridge$specificity, cm_ridge$byClass['Specificity'])
      metrics_ridge$f1_score <- c(metrics_ridge$f1_score, cm_ridge$byClass['F1'])
      
      # AUC for ridge regression
      ridge_roc_cv <- roc(test_set_cv$RevenueCategory, as.numeric(ridge_test_pred), quiet = TRUE)
      metrics_ridge$auc <- c(metrics_ridge$auc, auc(ridge_roc_cv))
      
      # 4. GBM Model
      gbm_model_cv <- gbm(RevenueCategory_numeric ~ MuseumDiscipline + LocaleType + Region + CoLocationResource + State + 
                            NonprofitFlag + PrivateFoundationFlag + FinancialTypeFlag + UniversityAffiliationFlag,
                          data = train_set_cv, 
                          distribution = "bernoulli",
                          n.trees = best_iter_gbm,
                          interaction.depth = 3,
                          shrinkage = 0.01,
                          verbose = FALSE)
      
      gbm_test_pred <- predict(gbm_model_cv, test_set_cv, n.trees = best_iter_gbm, type = "response")
      gbm_test_pred_class <- ifelse(gbm_test_pred > 0.5, "High", "Low")
      
      # Confusion matrix for GBM
      cm_gbm <- confusionMatrix(factor(gbm_test_pred_class, levels = levels(test_set_cv$RevenueCategory)), test_set_cv$RevenueCategory)
      metrics_gbm$accuracy <- c(metrics_gbm$accuracy, cm_gbm$overall['Accuracy'])
      metrics_gbm$precision <- c(metrics_gbm$precision, cm_gbm$byClass['Precision'])
      metrics_gbm$sensitivity <- c(metrics_gbm$sensitivity, cm_gbm$byClass['Sensitivity'])
      metrics_gbm$specificity <- c(metrics_gbm$specificity, cm_gbm$byClass['Specificity'])
      metrics_gbm$f1_score <- c(metrics_gbm$f1_score, cm_gbm$byClass['F1'])
      
      # AUC for GBM
      gbm_roc_cv <- roc(test_set_cv$RevenueCategory, as.numeric(gbm_test_pred), quiet = TRUE)
      metrics_gbm$auc <- c(metrics_gbm$auc, auc(gbm_roc_cv))
      
      # 5. Naive Bayes Model
      naive_bayes_model_cv <- naiveBayes(RevenueCategory ~ MuseumDiscipline + LocaleType + Region + CoLocationResource + 
                                           State + NonprofitFlag + PrivateFoundationFlag + FinancialTypeFlag + UniversityAffiliationFlag, 
                                         data = train_set_cv)
      
      nb_test_pred <- predict(naive_bayes_model_cv, test_set_cv, type = "raw")
      nb_test_pred_class <- ifelse(nb_test_pred[, "High"] > 0.5, "High", "Low")
      
      # Confusion matrix for Naive Bayes
      cm_nb <- confusionMatrix(factor(nb_test_pred_class, levels = levels(test_set_cv$RevenueCategory)), test_set_cv$RevenueCategory)
      metrics_nb$accuracy <- c(metrics_nb$accuracy, cm_nb$overall['Accuracy'])
      metrics_nb$precision <- c(metrics_nb$precision, cm_nb$byClass['Precision'])
      metrics_nb$sensitivity <- c(metrics_nb$sensitivity, cm_nb$byClass['Sensitivity'])
      metrics_nb$specificity <- c(metrics_nb$specificity, cm_nb$byClass['Specificity'])
      metrics_nb$f1_score <- c(metrics_nb$f1_score, cm_nb$byClass['F1'])
      
      # AUC for Naive Bayes
      nb_roc_cv <- roc(test_set_cv$RevenueCategory, nb_test_pred[, "High"], quiet = TRUE)
      metrics_nb$auc <- c(metrics_nb$auc, auc(nb_roc_cv))
    })
  }, error = function(e) {
    message("Iteration ", b, " skipped (non-critical): ", e$message)
  })
}

# Calculate average metrics for each model
average_metrics <- function(metrics_list) {
  lapply(metrics_list, function(x) mean(x, na.rm = TRUE))
}

average_metrics_logit <- average_metrics(metrics_logit)
average_metrics_rf <- average_metrics(metrics_rf)
average_metrics_ridge <- average_metrics(metrics_ridge)
average_metrics_gbm <- average_metrics(metrics_gbm)
average_metrics_nb <- average_metrics(metrics_nb)

# Print average metrics
print("Average Metrics for Logistic Regression:")
print(average_metrics_logit)
print("Average Metrics for Random Forest:")
print(average_metrics_rf)
print("Average Metrics for Ridge Regression:")
print(average_metrics_ridge)
print("Average Metrics for GBM:")
print(average_metrics_gbm)
print("Average Metrics for Naive Bayes:")
print(average_metrics_nb)

# Create data frame of average metrics for plotting
metrics_df <- data.frame(
  Model = c("Logistic Regression", "Random Forest", "Ridge Regression", "GBM", "Naive Bayes"),
  Accuracy = c(mean(metrics_logit$accuracy), mean(metrics_rf$accuracy), mean(metrics_ridge$accuracy), mean(metrics_gbm$accuracy), mean(metrics_nb$accuracy)),
  Precision = c(mean(metrics_logit$precision), mean(metrics_rf$precision), mean(metrics_ridge$precision), mean(metrics_gbm$precision), mean(metrics_nb$precision)),
  Sensitivity = c(mean(metrics_logit$sensitivity), mean(metrics_rf$sensitivity), mean(metrics_ridge$sensitivity), mean(metrics_gbm$sensitivity), mean(metrics_nb$sensitivity)),
  Specificity = c(mean(metrics_logit$specificity), mean(metrics_rf$specificity), mean(metrics_ridge$specificity), mean(metrics_gbm$specificity), mean(metrics_nb$specificity)),
  F1_Score = c(mean(metrics_logit$f1_score), mean(metrics_rf$f1_score), mean(metrics_ridge$f1_score), mean(metrics_gbm$f1_score), mean(metrics_nb$f1_score)),
  AUC = c(mean(metrics_logit$auc), mean(metrics_rf$auc), mean(metrics_ridge$auc), mean(metrics_gbm$auc), mean(metrics_nb$auc))
)

# Plot average metrics for all models
library(ggplot2)
metrics_melted <- reshape2::melt(metrics_df, id.vars = "Model")

ggplot(metrics_melted, aes(x = Model, y = value, fill = variable)) +
  geom_bar(stat = "identity", position = position_dodge()) +
  theme_minimal() +
  labs(title = "Average Metrics for All Models", x = "Model", y = "Metric Value", fill = "Metric") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Plot ROC curves for all models
plot(logistic_roc_cv, col = "red", lwd = 2, main = "ROC Curves for All Models")
plot(rf_roc_cv, col = "blue", lwd = 2, add = TRUE)
plot(ridge_roc_cv, col = "green", lwd = 2, add = TRUE)
plot(gbm_roc_cv, col = "purple", lwd = 2, add = TRUE)
plot(nb_roc_cv, col = "orange", lwd = 2, add = TRUE)
legend("bottomright", legend = c("Logistic Regression", "Random Forest", "Ridge Regression", "GBM", "Naive Bayes"),
       col = c("red", "blue", "green", "purple", "orange"), lwd = 2)


# ================================
# 6. MODELING: PROFITABILITY CLASSIFICATION
# ================================
# - Create Profitability label
# - Logistic Regression
# - Random Forest
# - Ridge Regression
# - Naive Bayes

# Adding Profitability column
museum_data_cleaned <- museum_data_cleaned %>%
  mutate(Profitability = ifelse((Income2015 - Revenue2015) > 0, "Profitable", "NonProfitable"))  # Remove the dash for compatibility

# Convert Profitability to a factor
museum_data_cleaned$Profitability <- as.factor(museum_data_cleaned$Profitability)

# Set seed and split data
set.seed(123)
trainIndex <- createDataPartition(museum_data_cleaned$Profitability, p = 0.75, list = FALSE, times = 1)
museum_train <- museum_data_cleaned[trainIndex, ]
museum_test <- museum_data_cleaned[-trainIndex, ]

# Set up cross-validation control
cv_control <- trainControl(method = "cv", number = 5, savePredictions = TRUE, classProbs = TRUE, summaryFunction = twoClassSummary) # 5-fold cross-validation

### SINGLE TRAIN-TEST SPLIT EVALUATION FOR ALL MODELS ###

### Logistic Regression ###
logistic_model_profit <- glm(Profitability ~ MuseumDiscipline + LocaleType + Region + CoLocationResource + State +
                               NonprofitFlag + PrivateFoundationFlag + FinancialTypeFlag + UniversityAffiliationFlag, 
                             data = museum_train, family = binomial)

# Feature Summary
logistic_summary <- summary(logistic_model_profit)
print(logistic_summary)

# Predict and evaluate
logistic_test_prob <- predict(logistic_model_profit, museum_test, type = "response")
logistic_test_pred <- ifelse(logistic_test_prob > 0.5, "Profitable", "NonProfitable")
logistic_conf_matrix <- confusionMatrix(factor(logistic_test_pred, levels = levels(museum_test$Profitability)), museum_test$Profitability)
print(logistic_conf_matrix)
print(logistic_conf_matrix$byClass) # Precision, Recall, F1

# AUC-ROC for Logistic Regression
logistic_roc <- roc(as.numeric(museum_test$Profitability) - 1, as.numeric(logistic_test_prob))
logistic_auc <- auc(logistic_roc)
print(paste("AUC for Logistic Regression: ", logistic_auc))

# Visualizations
# Feature Importance Visualization for Logistic Regression
importance_df <- data.frame(Feature = names(coef(logistic_model_profit)), Coefficient = coef(logistic_model_profit))
importance_df <- importance_df[order(-abs(importance_df$Coefficient)), ]

# Select Top 25 Features
importance_df_top25 <- head(importance_df, 25)

# Plot Top 25 Feature Importance
library(ggplot2)
ggplot(importance_df_top25, aes(x = reorder(Feature, -abs(Coefficient)), y = abs(Coefficient))) +
  geom_bar(stat = "identity", fill = "darkorange") +
  coord_flip() +
  labs(title = "Top 25 Feature Importance for Logistic Regression", x = "Feature", y = "Absolute Coefficient Value") +
  theme_minimal()


### Random Forest Model ###
rf_model_profit <- randomForest(Profitability ~ MuseumDiscipline + LocaleType + Region + CoLocationResource + State +
                                  NonprofitFlag + PrivateFoundationFlag + FinancialTypeFlag + UniversityAffiliationFlag, 
                                data = museum_train, ntree = 300, mtry = 4)

# Predict and evaluate
rf_test_pred <- predict(rf_model_profit, museum_test)
rf_conf_matrix <- confusionMatrix(rf_test_pred, museum_test$Profitability)
print(rf_conf_matrix)
print(rf_conf_matrix$byClass) # Precision, Recall, F1

# AUC-ROC for Random Forest
rf_test_prob <- predict(rf_model_profit, museum_test, type = "prob")[, "Profitable"]
rf_roc <- roc(as.numeric(museum_test$Profitability) - 1, as.numeric(rf_test_prob))
rf_auc <- auc(rf_roc)
print(paste("AUC for Random Forest: ", rf_auc))

### Ridge Regression Model ###
x_train <- model.matrix(Profitability ~ MuseumDiscipline + LocaleType + Region + CoLocationResource + 
                          State + NonprofitFlag + PrivateFoundationFlag + FinancialTypeFlag + UniversityAffiliationFlag, data = museum_train)[, -1]
y_train <- ifelse(museum_train$Profitability == "Profitable", 1, 0)

x_test <- model.matrix(Profitability ~ MuseumDiscipline + LocaleType + Region + CoLocationResource + State + 
                         NonprofitFlag + PrivateFoundationFlag + FinancialTypeFlag + UniversityAffiliationFlag, data = museum_test)[, -1]
y_test <- ifelse(museum_test$Profitability == "Profitable", 1, 0)

ridge_model_profit <- cv.glmnet(x_train, y_train, alpha = 0, family = "binomial")

# Predict and evaluate
ridge_probabilities <- predict(ridge_model_profit, s = "lambda.min", newx = x_test, type = "response")
ridge_pred_classes <- ifelse(ridge_probabilities > 0.5, "Profitable", "NonProfitable")
ridge_conf_matrix <- confusionMatrix(factor(ridge_pred_classes, levels = levels(museum_test$Profitability)), museum_test$Profitability)
print(ridge_conf_matrix)
print(ridge_conf_matrix$byClass) # Precision, Recall, F1

# AUC-ROC for Ridge Regression
ridge_roc <- roc(y_test, ridge_probabilities)
ridge_auc <- auc(ridge_roc)
print(paste("AUC for Ridge Regression: ", ridge_auc))

### Naive Bayes Model ###
naive_bayes_model <- naiveBayes(
  Profitability ~ MuseumDiscipline + LocaleType + Region + CoLocationResource + State +
    NonprofitFlag + PrivateFoundationFlag + FinancialTypeFlag + UniversityAffiliationFlag,
  data = museum_train
)

# Predict on the test set and evaluate
nb_test_predictions <- predict(naive_bayes_model, museum_test)
nb_conf_matrix <- confusionMatrix(factor(nb_test_predictions, levels = levels(museum_test$Profitability)), museum_test$Profitability)
print(nb_conf_matrix)
print(nb_conf_matrix$byClass) # Precision, Recall, F1

# Naive Bayes AUC-ROC
nb_test_prob <- predict(naive_bayes_model, museum_test, type = "raw")[, "Profitable"]
nb_roc <- roc(as.numeric(museum_test$Profitability) - 1, nb_test_prob)
nb_auc <- auc(nb_roc)
print(paste("AUC for Naive Bayes: ", nb_auc))

### CROSS-VALIDATION EVALUATION FOR ALL MODELS ###

### Logistic Regression with Cross-Validation ###
logistic_cv_model <- train(Profitability ~ MuseumDiscipline + LocaleType + Region + CoLocationResource + State +
                             NonprofitFlag + PrivateFoundationFlag + FinancialTypeFlag + UniversityAffiliationFlag,
                           data = museum_train,
                           method = "glm",
                           family = "binomial",
                           trControl = cv_control,
                           metric = "ROC")

# Cross-Validation Confusion Matrix and Metrics
logistic_cv_conf_matrix <- confusionMatrix(logistic_cv_model$pred$pred, logistic_cv_model$pred$obs)
print("Logistic Regression Cross-Validation Confusion Matrix:")
print(logistic_cv_conf_matrix)
print(logistic_cv_conf_matrix$byClass) # Precision, Recall, F1

### Random Forest with Cross-Validation ###
tune_grid <- expand.grid(mtry = c(2, 4, 6))
rf_cv_model <- train(Profitability ~ MuseumDiscipline + LocaleType + Region + CoLocationResource + State +
                       NonprofitFlag + PrivateFoundationFlag + FinancialTypeFlag + UniversityAffiliationFlag,
                     data = museum_train,
                     method = "rf",
                     trControl = cv_control,
                     tuneGrid = tune_grid,
                     ntree = 300,
                     metric = "ROC")

# Cross-Validation Confusion Matrix and Metrics
rf_cv_conf_matrix <- confusionMatrix(rf_cv_model$pred$pred, rf_cv_model$pred$obs)
print("Random Forest Cross-Validation Confusion Matrix:")
print(rf_cv_conf_matrix)
print(rf_cv_conf_matrix$byClass) # Precision, Recall, F1

### Ridge Regression with Cross-Validation ###
tune_grid_ridge <- expand.grid(alpha = 0, lambda = 10^seq(-3, 3, length = 100))

ridge_cv_model <- train(Profitability ~ MuseumDiscipline + LocaleType + Region + CoLocationResource + State +
                          NonprofitFlag + PrivateFoundationFlag + FinancialTypeFlag + UniversityAffiliationFlag,
                        data = museum_train,
                        method = "glmnet",
                        trControl = cv_control,
                        tuneGrid = tune_grid_ridge,
                        family = "binomial",
                        metric = "ROC")

# Cross-Validation Confusion Matrix and Metrics
ridge_cv_conf_matrix <- confusionMatrix(ridge_cv_model$pred$pred, ridge_cv_model$pred$obs)
print("Ridge Regression Cross-Validation Confusion Matrix:")
print(ridge_cv_conf_matrix)
print(ridge_cv_conf_matrix$byClass) # Precision, Recall, F1

### Naive Bayes with Cross-Validation ###
nb_grid <- expand.grid(laplace = c(0, 1, 2), usekernel = c(TRUE, FALSE), adjust = c(1))

nb_cv_model <- train(Profitability ~ MuseumDiscipline + LocaleType + Region + CoLocationResource + State +
                       NonprofitFlag + PrivateFoundationFlag + FinancialTypeFlag + UniversityAffiliationFlag,
                     data = museum_train,
                     method = "naive_bayes",
                     trControl = cv_control,
                     tuneGrid = nb_grid,
                     metric = "ROC")

# Cross-Validation Confusion Matrix and Metrics
nb_cv_conf_matrix <- confusionMatrix(nb_cv_model$pred$pred, nb_cv_model$pred$obs)
print("Naive Bayes Cross-Validation Confusion Matrix:")
print(nb_cv_conf_matrix)
print(nb_cv_conf_matrix$byClass) # Precision, Recall, F1

# Bar Plot for Profitability by Museum Discipline
ggplot(museum_data_cleaned, aes(x = str_wrap(MuseumDiscipline, width = 15), fill = Profitability)) +
  geom_bar(position = "dodge") +
  labs(title = "Profitability Distribution by Museum Discipline", 
       x = "Museum Discipline", 
       y = "Count", 
       fill = "Profitability") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Pie Chart for Overall Profitability
profit_count <- table(museum_data_cleaned$Profitability)
pie(profit_count, labels = names(profit_count), main = "Overall Profitability Distribution", col = c("lightblue", "lightcoral"))

# Boxplot for Revenue and Income Grouped by Profitability
ggplot(museum_data_cleaned, aes(x = Profitability, y = Revenue2015)) +
  geom_boxplot(fill = "lightgreen", color = "darkgreen", notch = TRUE) +
  scale_y_continuous(labels = scales::comma) +
  labs(title = "Revenue by Profitability", x = "Profitability", y = "Revenue 2015") +
  theme_minimal()

ggplot(museum_data_cleaned, aes(x = Profitability, y = Income2015)) +
  geom_boxplot(fill = "lightblue", color = "darkblue", notch = TRUE) +
  scale_y_continuous(labels = scales::comma) +
  labs(title = "Income by Profitability", x = "Profitability", y = "Income 2015") +
  theme_minimal()

ggplot(museum_data_cleaned, aes(x = LocaleType, fill = Profitability)) +
  geom_bar(position = "dodge") +
  labs(title = "Profitability Distribution by Locale Type", x = "Locale Type", y = "Count", fill = "Profitability") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggplot(museum_data_cleaned, aes(x = Region, fill = Profitability)) +
  geom_bar(position = "dodge") +
  labs(title = "Profitability Distribution by Region", x = "Region", y = "Count", fill = "Profitability") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggplot(museum_data_cleaned, aes(x = Revenue2015, y = Income2015, color = Profitability)) +
  geom_point(alpha = 0.6) +
  scale_x_continuous(labels = scales::comma) +
  scale_y_continuous(labels = scales::comma) +
  labs(title = "Revenue vs Income Colored by Profitability", x = "Revenue 2015", y = "Income 2015") +
  theme_minimal()

ggplot(museum_data_cleaned, aes(x = LocaleType, y = Revenue2015, fill = Profitability)) +
  geom_boxplot() +
  scale_y_continuous(labels = scales::comma) +
  labs(title = "Revenue Distribution by Locale Type and Profitability", x = "Locale Type", y = "Revenue 2015", fill = "Profitability") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggplot(museum_data_cleaned, aes(x = LocaleType, y = Income2015, fill = Profitability)) +
  geom_boxplot() +
  scale_y_continuous(labels = scales::comma) +
  labs(title = "Income Distribution by Locale Type and Profitability", x = "Locale Type", y = "Income 2015", fill = "Profitability") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggplot(museum_data_cleaned, aes(x = MuseumDiscipline, fill = Profitability)) +
  geom_bar(position = "dodge") +
  facet_wrap(~ LocaleType) +
  labs(title = "Profitability by Museum Discipline and Locale Type", x = "Museum Discipline", y = "Count", fill = "Profitability") +
  coord_flip() +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


prop_table <- prop.table(table(museum_data_cleaned$Profitability, museum_data_cleaned$NonprofitFlag), margin = 2)
barplot(prop_table, beside = TRUE, legend = TRUE, col = c("lightblue", "lightcoral"),
        main = "Profitability by Nonprofit Flag", ylab = "Proportion", xlab = "Nonprofit Flag")

ggplot(museum_data_cleaned, aes(x = PrivateFoundationFlag, fill = Profitability)) +
  geom_bar(position = "fill") +
  labs(title = "Proportion of Profitability by Private Foundation Flag", x = "Private Foundation Flag", y = "Proportion", fill = "Profitability") +
  theme_minimal()

