library(readr)
library(dplyr)
library(tidyr)
library(readxl)
library(lubridate)
library(zoo)
library(stringr)
library(tibble)
library(purrr)

# ===========================AREA CHART===========================
# Load the original OECD CPI dataset
raw_data <- read_csv("dataSources/Consumer price indices (CPIs, HICPs), COICOP 1999.csv")

# Filter: Food (CP01), Australia and OECD, CPI measure with GY transformation
yoy_data <- raw_data %>%
  filter(
    REF_AREA %in% c("AUS", "OECD"),
    EXPENDITURE == "CP01",
    MEASURE == "CPI",
    TRANSFORMATION == "GY"
  ) %>%
  select(REF_AREA, TIME_PERIOD, OBS_VALUE) %>%
  mutate(OBS_VALUE = as.numeric(OBS_VALUE))

# Pivot wider so we get columns: Quarter, Australia YoY %, OECD YoY %
yoy_wide <- yoy_data %>%
  pivot_wider(names_from = REF_AREA, values_from = OBS_VALUE) %>%
  rename(
    Quarter = TIME_PERIOD,
    `Australia YoY %` = AUS,
    `OECD YoY %` = OECD
  ) %>%
  arrange(Quarter)  # ensure correct time ordering

# Export full range: 2019–2025
write_csv(yoy_wide, "data/YoY_Food_Inflation_AUS_OECD_2019_2024.csv")

# ===========================IMPORT EXPORT CHART===========================

# Manually map food SITC columns
food_col_indices <- c(3, 4, 5, 6, 7, 8, 9, 10)  # columns C to J

# --- For EXPORT data ---
df_export <- read_excel("dataSources/5368012b.xlsx", sheet = "Data1", skip = 10)
colnames(df_export)[1] <- "Month"
df_export <- df_export %>%
  mutate(Month = as.Date(Month)) %>%
  arrange(Month)

export_food <- df_export %>%
  select(Month, all_of(food_col_indices)) %>%
  mutate(Total_Export = rowSums(across(-Month), na.rm = TRUE))

export_q <- export_food %>%
  mutate(
    Year = year(Month),
    Q = quarter(Month),
    Quarter = paste0(Year, "-Q", Q)
  ) %>%
  group_by(Quarter) %>%
  summarise(Total_Export = sum(Total_Export, na.rm = TRUE)) %>%
  ungroup()

# --- For IMPORT data ---
df_import <- read_excel("dataSources/5368013b.xlsx", sheet = "Data1", skip = 10)
colnames(df_import)[1] <- "Month"
df_import <- df_import %>%
  mutate(Month = as.Date(Month)) %>%
  arrange(Month)

import_food <- df_import %>%
  select(Month, all_of(food_col_indices)) %>%
  mutate(Total_Import = rowSums(across(-Month), na.rm = TRUE))

import_q <- import_food %>%
  mutate(
    Year = year(Month),
    Q = quarter(Month),
    Quarter = paste0(Year, "-Q", Q)
  ) %>%
  group_by(Quarter) %>%
  summarise(Total_Import = sum(Total_Import, na.rm = TRUE)) %>%
  ungroup()

# --- Load CPI data ---
df_cpi <- read_excel("dataSources/ABS_CPI_Table_13(Quarterly).xlsx", sheet = "Data1", skip = 10)
names(df_cpi)[1:2] <- c("Date", "Food_CPI_Index")

df_cpi <- df_cpi %>%
  filter(!is.na(Food_CPI_Index)) %>%
  mutate(
    Date = as.Date(Date),
    Year = year(Date),
    Q = quarter(Date),
    Quarter = paste0(Year, "-Q", Q),
    Food_CPI_YoY = (Food_CPI_Index / lag(Food_CPI_Index, 4) - 1) * 100
  ) %>%
  select(Quarter, Food_CPI_Index, Food_CPI_YoY)

# --- Merge all ---
final_df <- full_join(export_q, import_q, by = "Quarter") %>%
  left_join(df_cpi, by = "Quarter") %>%
  arrange(Quarter) %>%
  filter(Quarter >= "2019-Q1" & Quarter <= "2025-Q1") %>%
  mutate(
    Total_Export = round(Total_Export, 1),
    Total_Import = round(Total_Import, 1),
    Food_CPI_Index = round(Food_CPI_Index, 2),
    Food_CPI_YoY = round(Food_CPI_YoY, 2)
  )

# Export CSV
write.csv(final_df, "data/Food_Production_vs_Inflation_by_Quarter_new.csv", row.names = FALSE)

# ===========================SLOPE CHART===========================

# Load data
cpi_raw <- read_excel("dataSources/ABS_Monthly_CPI Indicator.xlsx", sheet = "Data1", skip = 10)

# Rename relevant columns
colnames(cpi_raw)[1:7] <- c(
  "Date",
  "All_Groups_CPI",
  "Food_Beverages",
  "Bread_Cereal",
  "Meat_Seafood",
  "Dairy",
  "Fruit_Veg"
)

# Convert and filter for Dec 2019 and Dec 2024
cpi_filtered <- cpi_raw %>%
  mutate(Date = as.Date(Date)) %>%
  filter(month(Date) == 12 & year(Date) %in% c(2019, 2024)) %>%
  select(Date, Bread_Cereal, Meat_Seafood, Dairy, Fruit_Veg)

# Pivot to long format
cpi_long <- cpi_filtered %>%
  pivot_longer(-Date, names_to = "Category", values_to = "Value") %>%
  mutate(Year = year(Date)) %>%
  select(-Date) %>%
  pivot_wider(names_from = Year, values_from = Value, names_prefix = "")

# Calculate % change
cpi_long <- cpi_long %>%
  rename(`2019 Value` = `2019`, `2024 Value` = `2024`) %>%
  mutate(`% Change` = round((`2024 Value` / `2019 Value` - 1) * 100, 2)) %>%
  mutate(Category = recode(Category,
                           "Bread_Cereal" = "Bread & Cereal",
                           "Meat_Seafood" = "Meat & Seafood",
                           "Fruit_Veg" = "Fruit & Vegetables"
  ))

# Reorder columns
cpi_summary <- cpi_long %>%
  select(Category, `2019 Value`, `2024 Value`, `% Change`)

# View or export
print(cpi_summary)
write.csv(cpi_summary, "data/Food_Category_CPI_Percent_Change_2019_2024.csv", row.names = FALSE)

# ===========================FOOD CATEGORIES CHART===========================

# Load CPI file
cpi_raw <- read_excel("dataSources/ABS_Monthly_CPI Indicator.xlsx", sheet = "Data1", skip = 10)

# Rename relevant columns
colnames(cpi_raw)[1:7] <- c(
  "Date",
  "All_Groups_CPI",
  "Food_Beverages",
  "Bread_Cereal",
  "Meat_Seafood",
  "Dairy",
  "Fruit_Veg"
)

# Compute YoY % change
cpi_yoy <- cpi_raw %>%
  mutate(Date = as.Date(Date)) %>%
  arrange(Date) %>%
  mutate(
    across(
      c(Food_Beverages, Bread_Cereal, Meat_Seafood, Dairy, Fruit_Veg),
      ~ (.x / lag(.x, 12) - 1) * 100,
      .names = "{.col}_YoY"
    )
  ) %>%
  select(Date, ends_with("_YoY")) %>%
  filter(year(Date) >= 2019)

# Reshape to long format and rename categories
cpi_yoy_long <- cpi_yoy %>%
  pivot_longer(-Date, names_to = "Category", values_to = "YoY_Change") %>%
  mutate(
    Category = recode(Category,
                      "Bread_Cereal_YoY" = "Bread & Cereal",
                      "Meat_Seafood_YoY" = "Meat & Seafood",
                      "Dairy_YoY" = "Dairy",
                      "Fruit_Veg_YoY" = "Fruit & Vegetables",
                      "Food_Beverages_YoY" = "Food & Non-Alcoholic Beverages"
    ),
    Quarter = paste0(year(Date), "-Q", quarter(Date))
  ) %>%
  select(Quarter, Category, YoY_Change) %>%
  arrange(Category, Quarter)

# Export to CSV
write.csv(cpi_yoy_long, "data/Food_Category_YoY_Change_Quarterly_Format_new.csv", row.names = FALSE)

# ===========================VEG - PPI - FREIGHT - RAIN===========================

# ---- 1. VEGETABLES CPI (A128480215V) ----
veg_df <- read_excel("dataSources/ABS_Monthly_CPI Indicator.xlsx", sheet = "Data1", skip = 9)
colnames(veg_df)[1:2] <- c("Date", "Vegetables")
veg_df <- veg_df %>%
  mutate(Date = as.Date(Date)) %>%
  filter(!is.na(Vegetables)) %>%
  arrange(Date)

veg_yoy <- veg_df %>%
  mutate(`Vegetable YoY (%)` = (Vegetables / lag(Vegetables, 12) - 1) * 100) %>%
  select(Date, `Vegetable YoY (%)`) %>%
  mutate(Quarter = as.yearqtr(Date)) %>%
  group_by(Quarter) %>%
  summarise(`Vegetable YoY (%)` = mean(`Vegetable YoY (%)`, na.rm = TRUE)) %>%
  filter(Quarter >= as.yearqtr("2020 Q1") & Quarter <= as.yearqtr("2024 Q4"))

# ---- 2. FOOD PPI (A3343827C) ----
ppi_df <- read_excel("dataSources/PPI.xlsx", sheet = "Data1", skip = 9)
colnames(ppi_df)[1:2] <- c("Date", "Food_PPI")
ppi_df <- ppi_df %>%
  mutate(Date = as.Date(Date)) %>%
  filter(!is.na(Food_PPI)) %>%
  arrange(Date)

ppi_yoy <- ppi_df %>%
  mutate(`PPI YoY (%)` = (Food_PPI / lag(Food_PPI, 4) - 1) * 100) %>%
  select(Date, `PPI YoY (%)`) %>%
  mutate(Quarter = as.yearqtr(Date)) %>%
  group_by(Quarter) %>%
  summarise(`PPI YoY (%)` = mean(`PPI YoY (%)`, na.rm = TRUE)) %>%
  filter(Quarter >= as.yearqtr("2020 Q1") & Quarter <= as.yearqtr("2024 Q4"))

# ---- 3. FREIGHT INDEX (A2314058K) ----
freight_df <- read_excel("dataSources/ABS Producer Price Index – Freight Transport.xlsx", sheet = "Data1", skip = 9)
colnames(freight_df)[1:2] <- c("Date", "Freight_Index")
freight_df <- freight_df %>%
  mutate(Date = as.Date(Date)) %>%
  filter(!is.na(Freight_Index)) %>%
  arrange(Date)

freight_yoy <- freight_df %>%
  mutate(`Freight YoY (%)` = (Freight_Index / lag(Freight_Index, 4) - 1) * 100) %>%
  select(Date, `Freight YoY (%)`) %>%
  mutate(Quarter = as.yearqtr(Date)) %>%
  group_by(Quarter) %>%
  summarise(`Freight YoY (%)` = mean(`Freight YoY (%)`, na.rm = TRUE)) %>%
  filter(Quarter >= as.yearqtr("2020 Q1") & Quarter <= as.yearqtr("2024 Q4"))

# ---- 4. RAINFALL FUNCTION (handles type mismatch) ----
process_rain <- function(path) {
  rain_raw <- read.csv(path)
  
  # Clean and enforce numeric type for month columns
  month_cols <- c("Jan", "Feb", "Mar", "Apr", "May", "Jun",
                  "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")
  
  rain_raw[month_cols] <- lapply(rain_raw[month_cols], function(x) suppressWarnings(as.numeric(as.character(x))))
  
  rain_long <- rain_raw %>%
    select(Year, all_of(month_cols)) %>%
    pivot_longer(-Year, names_to = "Month", values_to = "Rainfall") %>%
    mutate(
      Date = as.Date(paste(Year, Month, "1"), format = "%Y-%b-%d")
    ) %>%
    drop_na(Date, Rainfall) %>%
    arrange(Date)
  
  rain_q <- rain_long %>%
    mutate(Quarter = as.yearqtr(Date)) %>%
    group_by(Quarter) %>%
    summarise(Rainfall = sum(Rainfall, na.rm = TRUE))
  
  return(rain_q)
}

# ---- 5. PROCESS RAINFALL FILES ----
rain1 <- process_rain("dataSources/IDCJAC0001_23034_Data12.csv")
rain2 <- process_rain("dataSources/IDCJAC0001_40842_Data12.csv")

rain_avg <- full_join(rain1, rain2, by = "Quarter") %>%
  mutate(Average = rowMeans(select(., Rainfall.x, Rainfall.y), na.rm = TRUE)) %>%
  arrange(Quarter) %>%
  mutate(`Rainfall YoY (%)` = (Average / lag(Average, 4) - 1) * 100) %>%
  select(Quarter, `Rainfall YoY (%)`) %>%
  filter(Quarter >= as.yearqtr("2020 Q1") & Quarter <= as.yearqtr("2024 Q4"))

# ---- 6. MERGE ALL ----
final <- reduce(list(veg_yoy, ppi_yoy, freight_yoy, rain_avg), full_join, by = "Quarter") %>%
  arrange(Quarter) %>%
  mutate(Quarter = format(Quarter, "%Y-Q%q"))

# ---- 7. EXPORT ----
write.csv(final, "data/Vegetable_PPI_Freight_Rainfall_YoY_2020_2024_new.csv", row.names = FALSE)

# ===========================POLICY CHART===========================

# Step 1: Load the original OECD CPI dataset
file_path <- "dataSources/Consumer price indices (CPIs, HICPs), COICOP 1999.csv"
df <- read_csv(file_path)

# Step 2: Filter for Australia, Food category, and YoY Growth
filtered_df <- df %>%
  filter(
    REF_AREA == "AUS",
    Expenditure == "Food and non-alcoholic beverages",
    Transformation == "Growth rate, over 1 year"
  )

# Step 3: Select and rename columns
df_plot <- filtered_df %>%
  select(TIME_PERIOD, YoY_Food_CPI = OBS_VALUE)

# Step 4: Convert TIME_PERIOD to Date and format as Quarter (e.g., 2020-Q1)
df_plot <- df_plot %>%
  mutate(
    TIME_PERIOD = as.yearqtr(TIME_PERIOD, format = "%Y-Q%q"),
    Quarter = format(TIME_PERIOD, "%Y-Q%q")
  ) %>%
  select(Quarter, YoY_Food_CPI) %>%
  arrange(Quarter)

# Step 5: Export to CSV
output_path <- "data/Reprocessed_YoY_Food_CPI_Australia_QDash.csv"
write_csv(df_plot, output_path)

# ===========================FINAL BAR CHART===========================

# Step 1: Input manually defined values
df_manual <- tibble::tibble(
  Factor = c("Transport/Fuel Costs", "PPI - Food Manufacturing", "Climate Volatility"),
  `2019 Value` = c(113.7, 119.9, 456.1),
  `2024 Value (Q4)` = c(132.9, 145.5, 844.2),
  Sustainable = c(120, 120, 120)
)

# Step 2: Calculate 'Current' as an indexed value relative to 2019 baseline
df_manual <- df_manual %>%
  mutate(
    Current = (`2024 Value (Q4)` / `2019 Value`) * 100,
    `% Above Sustainable` = Current - Sustainable,
    PctAbove = round(`% Above Sustainable`, 1),
    y_pos = row_number()
  )

# Step 3: Export to CSV
write_csv(df_manual, "data/Final_Bar_Chart_Data___Indexed_Drivers_UserVersion.csv")
