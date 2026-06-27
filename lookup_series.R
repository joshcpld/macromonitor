library(readabs)
library(dplyr)
library(stringr)

###################################################################
# Run this in RStudio. Downloads each ABS table and prints the
# series_id for every series we need in australia.csv.
# Copy the IDs back to the agent when done.
###################################################################

fetch <- function(cat_no, tables) {
  message("Downloading ", cat_no, " table ", tables, "...")
  read_abs(cat_no = cat_no, tables = tables) %>%
    filter(series_type == "Seasonally Adjusted") %>%
    distinct(series_id, series)
}

# ---------------------------------------------------------------
# 5206.0 Table 2 - Expenditure on GDP, Chain volume, SA
# ---------------------------------------------------------------
t5206_2 <- fetch("5206.0", 2)

cat("\n--- 5206.0 Table 2 matches ---\n")
targets_5206_2 <- c(
  "Business Investment"    = "New business investment",
  "Dwelling Investment"    = "Dwellings",
  "Government (consumption)" = "General government.*Final consumption",
  "Public GFCF"            = "Public.*Gross fixed capital",
  "Net Exports (Exports)"  = "Exports of goods and services",
  "Consumption (check)"    = "Households.*Final consumption"
)
for (label in names(targets_5206_2)) {
  match <- t5206_2 %>% filter(str_detect(series, regex(targets_5206_2[[label]], ignore_case = TRUE)))
  cat(label, ":\n")
  print(match)
}

# ---------------------------------------------------------------
# 5206.0 Table 4 - Chain price indexes (Terms of Trade)
# ---------------------------------------------------------------
t5206_4 <- fetch("5206.0", 4)
cat("\n--- 5206.0 Table 4 matches ---\n")
print(t5206_4 %>% filter(str_detect(series, regex("terms of trade", ignore_case = TRUE))))

# ---------------------------------------------------------------
# 6401.0 Table 2 - CPI groups, SA
# ---------------------------------------------------------------
t6401_2 <- fetch("6401.0", 2)
cat("\n--- 6401.0 Table 2 matches ---\n")
print(t6401_2 %>% filter(str_detect(series, regex("all groups|trimmed mean", ignore_case = TRUE))))

# ---------------------------------------------------------------
# 6484.0 Table 1 - Monthly CPI Indicator
# ---------------------------------------------------------------
t6484_1 <- read_abs("6484.0", tables = 1) %>% distinct(series_id, series, series_type)
cat("\n--- 6484.0 Table 1 (all types) ---\n")
print(t6484_1 %>% filter(str_detect(series, regex("all groups", ignore_case = TRUE))))

# ---------------------------------------------------------------
# 6202.0 Table 1 - Labour Force Survey, SA
# ---------------------------------------------------------------
t6202_1 <- fetch("6202.0", 1)
cat("\n--- 6202.0 Table 1 matches ---\n")
print(t6202_1 %>% filter(str_detect(series, regex("participation rate.*persons|monthly hours.*persons", ignore_case = TRUE))))

# ---------------------------------------------------------------
# 6354.0 Table 1 - Job Vacancies, SA
# ---------------------------------------------------------------
t6354_1 <- fetch("6354.0", 1)
cat("\n--- 6354.0 Table 1 matches ---\n")
print(t6354_1 %>% filter(str_detect(series, regex("total", ignore_case = TRUE))))

# ---------------------------------------------------------------
# 8501.0 Table 11 - Retail Turnover, SA chain volume
# ---------------------------------------------------------------
t8501_11 <- fetch("8501.0", 11)
cat("\n--- 8501.0 Table 11 matches ---\n")
print(t8501_11 %>% filter(str_detect(series, regex("total", ignore_case = TRUE))))

# ---------------------------------------------------------------
# 8731.0 Table 1 - Building Approvals, SA
# ---------------------------------------------------------------
t8731_1 <- fetch("8731.0", 1)
cat("\n--- 8731.0 Table 1 matches ---\n")
print(t8731_1 %>% filter(str_detect(series, regex("total dwellings", ignore_case = TRUE))))

# ---------------------------------------------------------------
# 5625.0 Table 2 - Private CapEx, SA
# ---------------------------------------------------------------
t5625_2 <- fetch("5625.0", 2)
cat("\n--- 5625.0 Table 2 matches ---\n")
print(t5625_2 %>% filter(str_detect(series, regex("total", ignore_case = TRUE))))

# ---------------------------------------------------------------
# 5601.0 Table 1 - Lending Indicators, SA
# ---------------------------------------------------------------
t5601_1 <- fetch("5601.0", 1)
cat("\n--- 5601.0 Table 1 matches ---\n")
print(t5601_1 %>% filter(str_detect(series, regex("housing.*total|total.*housing", ignore_case = TRUE))))

# ---------------------------------------------------------------
# 5368.0 Table 1 - International Trade, SA
# ---------------------------------------------------------------
t5368_1 <- fetch("5368.0", 1)
cat("\n--- 5368.0 Table 1 matches ---\n")
print(t5368_1 %>% filter(str_detect(series, regex("balance on goods", ignore_case = TRUE))))

message("\nAll done. Paste the output back to the agent.")
