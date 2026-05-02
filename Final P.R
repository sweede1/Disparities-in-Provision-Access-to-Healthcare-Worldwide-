library(tidyverse)
library(ggplot2)
world_data = read.csv("WHS2025_DATADOWNLOAD.csv")

# This lists every unique indicator name present in the dataset
unique(world_data$IndicatorName)

# Define the variables you want to summarize
target_indicators <- c("Healthy life expectancy at birth (years)", 
                       "Population with household expenditures on health > 10% of total household expenditure or income (%)",                                                                                   
                       "Population with household expenditures on health > 25% of total household expenditure or income (%)",
                       "Life expectancy at birth (years)",
                       "Density of medical doctors (per 10 000 population)",
                       "Total net official development assistance to medical research and basic health sectors per capita (US$), by recipient country",
                       "Prevalence of wasting in children under 5 (%)",
                       "Proportion of population using safely-managed drinking-water services (%)")


# Create the Summary Table
summary_table <- world_data %>%
  filter(IndicatorName %in% target_indicators) %>%
  group_by(IndicatorName, Location) %>% 
  summarize(
    Mean = mean(NumericValue, na.rm = TRUE),
    Median = median(NumericValue, na.rm = TRUE),
    Std_Dev = sd(NumericValue, na.rm = TRUE),
    Min = min(NumericValue, na.rm = TRUE),
    Max = max(NumericValue, na.rm = TRUE),
    .groups = 'drop'
  )

# Export to CSV
write.csv(summary_table, "Descriptive_Stats_Final_v2.csv", row.names = FALSE)


# Properly close the filter function and check the indicator spelling
water_data <- summary_table %>%
  filter(IndicatorName == "Proportion of population using safely-managed drinking-water services (%)") %>%
  group_by(Location) %>%
  slice_max(Mean, n = 1) %>%
  arrange(Mean) %>% # Sorts from lowest to highest Mean
  head(10)

# Update ggplot to use 'Mean' on the axes
ggplot(water_data, aes(x = reorder(Location, Mean), y = Mean)) +
  geom_col(fill = "steelblue") +
  coord_flip() +
  labs(title = "Top 10 Countries: Lowest Access to Safely Managed Water",
       subtitle = "Based on Mean percentage of population",
       x = "Country", 
       y = "Mean Percentage (%)") +
  theme_minimal()

#### Highest Access to Safe Water
# Filter for Highest Access using a flexible name search
highest_water_data <- summary_table %>%
  filter(str_detect(IndicatorName, "drinking-water")) %>%
  filter(Mean <= 100) %>% # Ensures we don't have data errors
  arrange(desc(Mean)) %>% # Sorts from highest to lowest
  head(10)

# Create the Graph
ggplot(highest_water_data, aes(x = reorder(Location, Mean), y = Mean)) +
  geom_col(fill = "darkslategray4") +
  coord_flip() +
  geom_text(aes(label = paste0(round(Mean, 1), "%")), hjust = 1.2, color = "white") +
  labs(title = "Top 10 Countries: Highest Access to Safely Managed Water",
       subtitle = "Percentage of population with access",
       x = "Country", 
       y = "Mean Percentage (%)") +
  theme_minimal()


########### Wasting
# Load the summary data you created
df <- read.csv("Descriptive_Stats_Final.csv")

# Filter, Sort, and Select Top 15
# Sort by 'Mean' in descending order to see the highest prevalence first
wasting_sorted <- df %>%
  filter(IndicatorName == "Prevalence of wasting in children under 5 (%)") %>%
  arrange(desc(Mean)) %>%
  slice(1:15)

# Create the Visualization
ggplot(wasting_sorted, aes(x = reorder(Location, Mean), y = Mean)) +
  geom_bar(stat = "identity", fill = "firebrick") +
  coord_flip() + # Makes the country names readable
  labs(
    title = "Top 15 Countries: Highest Prevalence of Child Wasting",
    subtitle = "Data sorted by mean prevalence (Under 5 years of age)",
    x = "Country",
    y = "Prevalence (%)",
    caption = "Source: Descriptive_Stats_Final.csv"
  ) +
  theme_minimal()

# Filter for wasting and get the 10 countries with the lowest mean
lowest_wasting_data <- df %>%
  filter(IndicatorName == "Prevalence of wasting in children under 5 (%)") %>%
  arrange(Mean) %>%
  head(10) %>%
  select(Location, Mean)

# print column names
colnames(lowest_wasting_data) <- c("Country", "Mean Wasting (%)")
print(lowest_wasting_data, row.names = FALSE)

# Create the bar graph
ggplot(lowest_wasting_data, aes(x = reorder(Country, `Mean Wasting (%)`), y = `Mean Wasting (%)`)) +
  geom_col(fill = "steelblue") + 
  coord_flip() + # Makes country names easier to read
  geom_text(aes(label = `Mean Wasting (%)`), hjust = -0.2, size = 3.5) + 
  labs(
    title = "Countries with Lowest Child Wasting Prevalence",
    subtitle = "Top 10 nations based on Descriptive_Stats_Final.csv",
    x = "Country",
    y = "Prevalence (%)"
  ) +
  theme_minimal()

world_data %>%
  filter(IndicatorName == "Prevalence of wasting in children under 5 (%)") %>%
  filter(NumericValue > 0) %>% # Excludes potential data errors or zeroes
  slice_min(NumericValue, n = 5) %>%
  select(Location, Year, NumericValue)

###### healthcare budget to overall life expectancy
#Filter for your specific indicators
budget_indicator <- "Total net official development assistance to medical research and basic health sectors per capita (US$), by recipient country"
life_exp_indicator <- "Life expectancy at birth (years)"

# Clean and prepare the two datasets
oda_clean <- world_data %>%
  filter(IndicatorName == budget_indicator) %>%
  drop_na(NumericValue)

life_exp_clean <- world_data %>%
  filter(IndicatorName == life_exp_indicator) %>%
  drop_na(NumericValue)

# Join the data by Location and Year
correlation_data <- inner_join(
  oda_clean %>% group_by(Location) %>% slice_max(Year, n = 1),
  life_exp_clean %>% group_by(Location) %>% slice_max(Year, n = 1),
  by = "Location",
  suffix = c("_oda", "_life")
)

# Calculate Correlation Coefficient
cor_result <- cor(correlation_data$NumericValue_oda, correlation_data$NumericValue_life, use = "complete.obs")
print(paste("The Correlation Coefficient is:", round(cor_result, 3)))

# Create Visualization with readable labels
ggplot(correlation_data, aes(x = NumericValue_oda, y = NumericValue_life)) +
  geom_point(color = "darkcyan", alpha = 0.6) +
  geom_smooth(method = "lm", color = "firebrick", se = TRUE) +
  # Label the top and bottom 5 to show the "Standard of Living" contrast
  geom_text(aes(label = ifelse(NumericValue_life < 55 | NumericValue_oda > 40, Location, "")), 
            vjust = -1, size = 3, check_overlap = TRUE) +
  labs(
    title = "Correlation: Medical ODA per Capita vs. Life Expectancy",
    subtitle = paste("Correlation Coefficient:", round(cor_result, 3)),
    x = "Medical ODA per capita (US$)",
    y = "Life Expectancy at Birth (Years)"
  ) +
  theme_minimal()

##### life Expectancy
library(tidyverse)

#  Filter for the 10 countries with the absolute lowest Life Expectancy
# We use slice_min to ensure we get the bottom of the mortality scale
lowest_life_data <- summary_table %>%
  filter(IndicatorName == "Life expectancy at birth (years)") %>% 
  arrange(Mean) %>% 
  head(10)

# Create the Graph
ggplot(lowest_life_data, aes(x = reorder(Location, -Mean), y = Mean)) +
  geom_col(fill = "firebrick", alpha = 0.8) + 
  coord_flip() + 
  geom_text(aes(label = round(Mean, 1)), hjust = 1.2, color = "white", size = 3.5) +
  labs(
    title = "Top 10 Countries with Lowest Life Expectancy",
    subtitle = "Based on Mean Life Expectancy at Birth (Years)",
    x = "Country",
    y = "Life Expectancy (Years)"
  ) +
  theme_minimal()

######## 
# Identify high-burden countries (>25% expenditure)
high_cost <- summary_table %>%
  filter(IndicatorName == "Population with household expenditures on health > 25% of total household expenditure or income (%)") %>%
  arrange(desc(Mean)) %>%
  head(15)
# We take the top 10 countries with the highest health expenditure burden
pie_data <- high_cost %>%
  head(10) %>%
  mutate(label = paste0(round(Mean, 1), "%"))

# 2. Create the Pie Chart with labels
ggplot(pie_data, aes(x = "", y = Mean, fill = Location)) +
  geom_bar(stat = "identity", width = 1, color = "white") + # White border helps distinguish slices
  coord_polar("y", start = 0) +
  # This adds the amount to each slice
  geom_text(aes(label = label), 
            position = position_stack(vjust = 0.5), 
            size = 4, 
            color = "white", 
            fontface = "bold") +
  labs(
    title = "Household Health Expenditure",
    subtitle = "Top 10 countries with >25% household expenditure on health",
    x = NULL,
    y = NULL
  ) +
  theme_void() + 
  theme(legend.title = element_text(face = "bold"))


####### Medical Doctor Density
# Identify low-doctor countries
low_doctors <- summary_table %>%
  filter(IndicatorName == "Density of medical doctors (per 10 000 population)") %>%
  arrange(Mean) %>%
  head(15)

# Create Bar Graph for Lowest Doctor Density
ggplot(low_doctors, aes(x = reorder(Location, Mean), y = Mean)) +
  geom_col(fill = "darkred", alpha = 0.8) +
  coord_flip() +
  labs(
    title = "Top 15 Countries: Lowest Density of Medical Doctors",
    subtitle = "Medical doctors per 10,000 population",
    x = "Country",
    y = "Mean Density"
  ) +
  theme_minimal()

#Find countries appearing in both lists 
overlap_countries <- intersect(high_cost$Location, low_doctors$Location)
print(overlap_countries)
### No overlapping countries