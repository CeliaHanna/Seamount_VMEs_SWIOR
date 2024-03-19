###### CHAPTER 2 HILL NUMBERS 

library(vegan)
library(dplyr)
library(ggplot2)
library(hillR)


# First work out hill numbers for raw data 

hill_taxa(species_abundance_numeric, q = 0, MARGIN = 1, base = exp(1))
hill_taxa(species_abundance_numeric, q = 1, MARGIN = 1, base = exp(1))
hill_taxa(species_abundance_numeric, q = 2, MARGIN = 1, base = exp(1))


# Plot Hill numbers from raw data 

hill_numbers <- data.frame(
  Seamount = c('Atlantis', 'Coral', 'Melville Bank', 'Sapmer'),
  q0 = c(23, 48, 34, 12),
  q1 = c(10.733580, 21.515901, 7.934988, 3.766076),
  q2 = c(7.074144, 10.238277, 3.286074, 2.136602)
)

# Gather the data to a long format for ggplot
long_hill_numbers <- pivot_longer(hill_numbers, cols = c(q0, q1, q2), names_to = "q", values_to = "HillNumber")

# Convert the q factor to a numeric for plotting
long_hill_numbers$q <- str_replace(long_hill_numbers$q, "q", "")
long_hill_numbers$q <- as.numeric(long_hill_numbers$q)

# Plot with specified colors
ggplot(long_hill_numbers, aes(x = q, y = HillNumber, group = Seamount, color = Seamount)) +
  geom_line() +
  geom_point() +
  theme_minimal() +
  labs(x = "Order of q", y = "Hill Number", title = "") +
  scale_x_continuous(breaks = 0:2) +
  scale_color_manual(values = c("Atlantis" = "orange", "Coral" = "royalblue2", "Melville Bank" = "hotpink1", "Sapmer" = "purple4"))


## ANOVAs to look for signifcant differences in hill numbers of raw data 







# Make sure 'data' has row names set to site names if not already
rownames(data) <- data$Site

# Adjusted function to return a data frame directly for each row
calculate_diversity_df <- function(abundance_row, size = 60, iterations = 1000) {
  richness_estimates <- numeric(iterations)
  shannon_estimates <- numeric(iterations)
  simpson_estimates <- numeric(iterations)
  evenness_estimates <- numeric(iterations)
  
  for (i in 1:iterations) {
    rarefied <- rrarefy(abundance_row, size)
    rarefied <- rarefied[rarefied > 0]
    
    richness_estimates[i] <- length(rarefied)
    shannon_estimates[i] <- diversity(rarefied, index = "shannon")
    simpson_estimates[i] <- diversity(rarefied, index = "simpson")
    evenness_estimates[i] <- diversity(rarefied, index = "invsimpson")
  }
  
  data.frame(
    average_richness = mean(richness_estimates),
    sd_richness = sd(richness_estimates),
    average_shannon = mean(shannon_estimates),
    sd_shannon = sd(shannon_estimates),
    average_simpson = mean(simpson_estimates),
    sd_simpson = sd(simpson_estimates),
    average_evenness = mean(evenness_estimates),
    sd_evenness = sd(evenness_estimates)
    
  )
}

# Apply the function and combine results
results_list <- lapply(split(species_abundance_numeric, rownames(species_abundance_numeric)), calculate_diversity_df)
results_df <- do.call(rbind, results_list)
rownames(results_df) <- names(results_list)

# Convert row names to a column (if needed)
results_df <- tibble::rownames_to_column(results_df, var = "Site")

# View the first few rows of the results
head(results_df)

# Assuming 'data' is your species abundance matrix with sites as rows and species as columns
# Apply the function to each row (site) in your dataset
results <- apply(species_abundance_numeric, 1, calculate_diversity) 

# Convert the list of results to a dataframe for easier viewing and analysis
results_df <- do.call(rbind, results) %>% 
  as.data.frame() %>%
  mutate(Site = rownames(data))

# Reorder the dataframe to have the Site column first
results_df <- results_df %>% select(Site, everything())

# View the first few rows of the results
head(results_df)


results_df


## Plotting hill numbers from rarefied data

# Your data frame with the average and standard deviation values
df <- data.frame(
  Site = 1:4,
  average_richness = c(16.007, 24.452, 16.061, 11.231),
  sd_richness = c(1.7527188, 2.4210654, 2.1198521, 0.8014245),
  average_shannon = c(2.249128, 2.739921, 1.849898, 1.311913),
  sd_shannon = c(0.11003089, 0.14934785, 0.19901642, 0.06994965),
  average_simpson = c(0.8504411, 0.8899750, 0.6860894, 0.5318594),
  sd_simpson = c(0.01837073, 0.02514996, 0.06023310, 0.02495440)
)

# Vector of seamount names corresponding to each site number
seamount_names <- c("Atlantis", "Coral", "Melville Bank", "Sapmer")

# Add the seamount names to the data frame
df$Seamount <- factor(seamount_names[df$Site])

# Reshape the data to long format for ease of plotting with ggplot
df_long <- df %>% 
  pivot_longer(
    cols = starts_with("average"),
    names_to = "measure",
    names_prefix = "average_",
    values_to = "average"
  ) %>%
  pivot_longer(
    cols = starts_with("sd"),
    names_to = "measure_sd",
    names_prefix = "sd_",
    values_to = "sd"
  ) %>%
  filter(str_replace(measure, "_richness|_shannon|_simpson", "") == 
           str_replace(measure_sd, "_richness|_shannon|_simpson", "")) %>%
  select(-measure_sd)

# Plotting with seamount labels
ggplot(df_long, aes(x = Seamount, y = average, fill = measure)) + 
  geom_bar(stat = "identity", position = position_dodge(width = 0.7), width = 0.6) + 
  geom_errorbar(aes(ymin = average - sd, ymax = average + sd), 
                position = position_dodge(width = 0.7), width = 0.25) +
  geom_text(aes(label = sprintf("%.2f", average), y = average + sd), 
            position = position_dodge(width = 0.7), vjust = 0, size = 3) +
  scale_fill_brewer(palette = "Pastel1") +
  theme_minimal() +
  labs(x = "Seamount", y = "Value", fill = "Index") +
  facet_wrap(~measure, scales = "free_y")  # Separate plots for each measure







### JUST DOING FOR RICHNESS ## AND PERFORMING ANOVA 


calculate_diversity_df <- function(abundance_row, size = 60, iterations = 1000) {
  richness_estimates <- numeric(iterations)
  
  for (i in 1:iterations) {
    rarefied <- rrarefy(abundance_row, size)
    rarefied <- rarefied[rarefied > 0]
    
    richness_estimates[i] <- length(rarefied)
  }
  
  return(data.frame(richness_estimates))
}

# Assuming 'species_abundance_numeric' and 'data$Site' are defined as before
results_list <- lapply(split(species_abundance_numeric, rownames(species_abundance_numeric)), calculate_diversity_df)

# Create an empty data frame to store results
richness_results <- data.frame(Site = character(), Iteration = integer(), Richness = numeric())

# Populate the data frame
for (site in names(results_list)) {
  site_data <- results_list[[site]]
  iterations <- seq_len(nrow(site_data))
  richness_results <- rbind(richness_results, data.frame(Site = site, Iteration = iterations, Richness = site_data$richness_estimates))
}

# Check the structure of the new data frame
head(richness_results)


anova_result <- aov(Richness ~ Site, data = richness_results)
summary(anova_result)

tukeyTukeyHSD(anova_result)

## Plot the results of the TUKEY test for richness differences 

# Convert comparison into a factor for ordered plotting

# Example data based on your provided Tukey HSD results
tukey_results <- data.frame(
  comparison = c("2-1", "3-1", "4-1", "3-2", "4-2", "4-3"),
  diff = c(8.544, 0.013, -4.764, -8.531, -13.308, -4.777),
  lwr = c(8.3299145417, -0.2010854583, -4.9780854583, -8.7450854583, -13.5220854583, -4.9910854583),
  upr = c(8.7580854583, 0.2270854583, -4.5499145417, -8.3169145417, -13.0939145417, -4.5629145417),
  p_adj = c(0.0000000249, 0.9986468493, 0.0000000249, 0.0000000249, 0.0000000249, 0.0000000249)
)

# Note: Your actual process might directly extract this from the TukeyHSD object

tukey_results$comparison <- factor(tukey_results$comparison, levels = tukey_results$comparison)

ggplot(tukey_results, aes(x = comparison, y = diff)) +
  geom_point(size = 4) +
  geom_errorbar(aes(ymin = lwr, ymax = upr), width = 0.2) +
  theme_minimal() +
  labs(x = "Comparison", y = "Difference in Mean Richness",
       title = "Tukey HSD Test Results for Site Comparisons",
       subtitle = "Point estimates and 95% confidence intervals") +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


