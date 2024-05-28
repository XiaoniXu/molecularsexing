# Load necessary libraries
library(tidyverse)  
library(vegan)
library(ape)
library(devtools)
library(dplyr)
library(ggplot2)
library(tidyr)
library(purrr)

setwd("C:/Users/berns/Desktop/Sexual Dimorphism")


# Read CSV data
data <- read.csv("Measurements_2024XX.csv")

# Summarize and visualize data
summary_data <- data %>%
  filter(!is.na(bill.length) & !is.na(bill.width) & !is.na(bill.depth) & 
           !is.na(tarsus.length) & !is.na(tarsus.depth) & !is.na(tail.length) &
           !is.na(wing.chord)) %>%
  group_by(species) %>%
  summarise(
    mean_bill_length = mean(bill.length, na.rm = TRUE),
    mean_bill_width = mean(bill.width, na.rm = TRUE),
    mean_bill_depth = mean(bill.depth, na.rm = TRUE),
    mean_tarsus_length = mean(tarsus.length, na.rm = TRUE),
    mean_tarsus_depth = mean(tarsus.depth, na.rm = TRUE),
    mean_tail_length = mean(tail.length, na.rm = TRUE),
    mean_wing_chord = mean(wing.chord, na.rm = TRUE)
  )

# Melt the summary_data to long format for easier plotting
melted_summary <- summary_data %>%
  pivot_longer(cols = starts_with("mean_"), names_to = "measurement", values_to = "mean_value")

# Function to calculate Cohen's d effect size
calculate_effect_size <- function(group1, group2) {
  pooled_sd <- sqrt(((length(group1) - 1) * sd(group1, na.rm = TRUE)^2 + (length(group2) - 1) * sd(group2, na.rm = TRUE)^2) / 
                      (length(group1) + length(group2) - 2))
  effect_size <- abs(mean(group1, na.rm = TRUE) - mean(group2, na.rm = TRUE)) / pooled_sd
  return(effect_size)
}

# Perform t-tests and calculate effect sizes for each measurement for each species
t_test_results <- lapply(measurements, function(measurement) {
  result <- filtered_data %>%
    group_by(species) %>%
    summarise(
      p_value = t.test(as.formula(paste(measurement, "~ sex")))$p.value,
      effect_size = calculate_effect_size(
        group1 = filtered_data %>% filter(species == first(species) & sex == "male") %>% pull(!!sym(measurement)),
        group2 = filtered_data %>% filter(species == first(species) & sex == "female") %>% pull(!!sym(measurement))
      )
    )
  result <- result %>% rename_with(~paste0(measurement, ".", .), -species)
  return(result)
})

# Combine the t-test results into a single dataframe
t_test_results_df <- reduce(t_test_results, full_join, by = "species")

# View the table
print(t_test_results_df)


# Specify the indices of columns to delete
columns_to_delete <- c(3, 5, 7, 9, 11, 13)

# Delete the specified columns
t_test_results_df <- t_test_results_df[, -columns_to_delete]

# View the updated dataframe
print(t_test_results_df)

# Specify the new column names
new_column_names <- c("species", "bill.length", "bill.width", "bill.depth", "tarsus.length", "tarsus.depth", "tail.length", "wing.chord")

# Assign the new column names to the dataframe
colnames(t_test_results_df) <- new_column_names

# View the updated dataframe
print(t_test_results_df)

# Convert the dataframe to long format for plotting
df_long <- tidyr::pivot_longer(t_test_results_df, cols = -species, names_to = "measurement", values_to = "p_value")

# Create the heatmap plot
heatmap_plot <- ggplot(df_long, aes(x = species, y = measurement, fill = p_value)) +
  geom_tile(color = "white") +
  scale_fill_gradient(low = "blue", high = "red") +  # Adjust the color gradient as needed
  labs(title = "Heatmap of T-test Results",
       x = "Species",
       y = "Measurement",
       fill = "P-value") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Print the heatmap plot
print(heatmap_plot)

#MANOVA
#Test multiple variables at once

# Wait for the plates 11-14
# Summarize the results: spreadsheet that has effect size and significance level for every species for every variable



