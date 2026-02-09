## Inplementing the second visualization in R
## to fulfill the requirements of the assignment

# Packages
library(readr)
library(dplyr)
library(lubridate)
library(tidyr)
library(ggplot2)
library(scales)

# 1) Load data
setwd(dirname(normalizePath(sys.frame(1)$ofile))) 
# Set working directory to the location of this script
df <- read_csv("covidtesting.csv", show_col_types = FALSE) 
# Load the dataset, suppressing column type messages

# 2) Clean / prep
df <- df %>%
  mutate(
    `Reported Date` = mdy(`Reported Date`),
    `Confirmed Positive` = suppressWarnings(as.numeric(`Confirmed Positive`))
  ) %>%
  arrange(`Reported Date`) %>%
  filter(!is.na(`Reported Date`), !is.na(`Confirmed Positive`))
# Keep only rows with valid dates and confirmed positive cases

# Lineage columns
lineage_cols <- c(
  "Total_Lineage_B.1.1.7_Alpha",
  "Total_Lineage_B.1.351_Beta",
  "Total_Lineage_P.1_Gamma",
  "Total_Lineage_B.1.617.2_Delta"
)

# Coerce lineage columns to numeric, keep rows with any lineage data
df_late_2021 <- df %>%
  mutate(across(all_of(lineage_cols), ~ suppressWarnings(as.numeric(.)))) %>%
  filter(if_any(all_of(lineage_cols), ~ !is.na(.))) %>%
  filter(
    `Reported Date` >= ymd("2021-04-01"), `Reported Date` <= ymd("2021-12-31")
  )

# 3) Reshape to long format for plotting
plot_df <- df_late_2021 %>%
  select(`Reported Date`, all_of(lineage_cols)) %>%
  pivot_longer(
    cols = all_of(lineage_cols),
    names_to = "Lineage",
    values_to = "CumulativeCases"
  ) %>%
  filter(!is.na(CumulativeCases)) %>%
  mutate(
    Lineage = recode(
      Lineage,
      "Total_Lineage_B.1.1.7_Alpha"   = "Alpha (B.1.1.7)",
      "Total_Lineage_B.1.351_Beta"    = "Beta (B.1.351)",
      "Total_Lineage_P.1_Gamma"       = "Gamma (P.1)",
      "Total_Lineage_B.1.617.2_Delta" = "Delta (B.1.617.2)"
    )
  )

# 4) Plot
p <- ggplot(
  plot_df, aes(x = `Reported Date`, y = CumulativeCases, color = Lineage)
) +
  geom_line(linewidth = 1) +
  scale_x_date(date_breaks = "1 month", date_labels = "%b %Y") +
  labs(
    title = "COVID-19 Variant Dynamics Leading Into the 2022 Case Surge",
    x = "Date (2021)",
    y = "Cumulative Reported Cases by Lineage",
    color = NULL
  ) +
  theme_minimal(base_size = 12) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    panel.grid.minor = element_blank(),
    legend.position = "right"
  )

print(p)

# Save to same folder
ggsave(
  filename = "covid_variant_dynamics_2021.png",
  plot = p,
  width = 8,
  height = 5,
  dpi = 300
)