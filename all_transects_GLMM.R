library(MASS)
library(glmmTMB)

# Load dataframe 

all <- read.csv('/Users/user/Desktop/all_transects.csv')

colnames(all)

# check distribution of response variable

hist(all$Richness)


# Adding dive number

all$dive <- as.numeric(gsub(".*[Dd][Ii][Vv][Ee]([0-9]+).*", "\\1",all$Transect))

 
##### standardise env variables #####


all$StandardisedDepth <- scale(all$AverageDepth, center = TRUE, scale = TRUE)
all$StandardisedSalinity <- scale(all$AverageSalinity, center = TRUE, scale = TRUE)
all$StandardisedTemperature<- scale(all$AverageTemperature, center = TRUE, scale = TRUE)
all$StandardisedProductivity<- scale(all$AverageProductivity, center = TRUE, scale = TRUE)
all$StandardisedGradient <- scale(all$AverageGradient, center = TRUE, scale = TRUE)
all$StandardisedDensity <- scale(all$density, center = TRUE, scale = TRUE)



### number of transects per seamount ####

# Assuming 'df' is your dataframe
transects_per_seamount <- all %>%
  group_by(Seamount) %>%
  summarise(Transects = n())

# View the result
print(transects_per_seamount)

# depth range of each seamount 

# Assuming 'df' is your dataframe and 'depth' is the column with depth information
depths_per_seamount <- all %>%
  group_by(Seamount) %>%
  summarise(MaxDepth = max(AverageDepth, na.rm = TRUE),
            MinDepth = min(AverageDepth, na.rm = TRUE))

# View the result
print(depths_per_seamount)



#### ADDING DENSITY AND PLOTTING OUT ENV VARIABLES ####

library(gsw)

# Calculate Absolute Salinity (SA) and Conservative Temperature (CT) which are required for density calculation
SA <- gsw_SA_from_SP(all$AverageSalinity, all$AveragePressure, longitude = 0, latitude = 0) # Assuming a generic longitude and latitude
CT <- gsw_CT_from_t(SA, all$AverageTemperature, all$AveragePressure)

# Calculate density
density <- gsw_rho(SA, CT, all$AveragePressure)

# You can add the density back to your dataframe if you want
all$density <- density

# View the modified dataframe
head(all)


#### BOXPLOTS OF TEMPERATURE, SALINITY AND DENSITY PER SEAMOUNT #####
dev.off()
par(mfrow=c(3,1))
par(mar = c(4, 4, 2, 2))


boxplot(AverageTemperature ~ factor(Seamount, levels = c("Atlantis", "Sapmer", "Melville Bank", "Coral")), 
        data = all,
        xlab = "", ylab = "Temperature",
        main = "",
        col = "skyblue", # Change color as needed
        border = "black") # Change border color as needed


boxplot(AverageSalinity ~ factor(Seamount, levels = c("Atlantis", "Sapmer", "Melville Bank", "Coral")), 
        data = all,
        xlab = "", ylab = "Salinity",
        main = "",
        col = "orange", # Change color as needed
        border = "black") # Change border color as needed


boxplot(density ~ factor(Seamount, levels = c("Atlantis", "Sapmer", "Melville Bank", "Coral")), 
        data = all,
        xlab = "Seamount", ylab = "Density",
        main = "",
        col = "pink", # Change color as needed
        border = "black") # Change border color as needed


boxplot(AverageProductivity ~ factor(Seamount, levels = c("Atlantis", "Sapmer", "Melville Bank", "Coral")), 
        data = all_transects_600_800,
        xlab = "Seamount", ylab = "Density",
        main = "",
        col = "pink", # Change color as needed
        border = "black") # Change border color as needed



##### look at richness differences between seamounts using ANOVA #####

colnames(all)

anova <- aov(Richness ~ Substrate, data = all)
summary(anova)

# Perform Tukey HSD test
tukey_result <- TukeyHSD(anova)

# View the results
print(tukey_result)




###### correlation of env variables #####

# Converting seamount names to numeric 

all<- all %>%
  mutate(seamount_number = case_when(
    Seamount == "Atlantis" ~ 1,
    Seamount == "Sapmer" ~ 2,
    Seamount == "Melville Bank" ~ 3,
    Seamount == "Coral" ~ 4,
    TRUE ~ NA_real_ # This line handles any names that do not match the ones specified
  ))

# Convert substrate type to numeric values 

all <- all %>%
  mutate(substrate_numbers = case_when(
    Substrate == "Volcanic" ~ 1,
    Substrate == "volcanic_sediment" ~ 2,
    Substrate == "Sediment" ~ 3,
    Substrate == "biogenic_matrix" ~ 4,
    Substrate == "coral_rubble" ~ 5,
    TRUE ~ NA_integer_  # Assign NA to any Substrate not listed above
  ))


# making a dataframe of environmental variables only 

env_all <- dplyr::select(all, seamount_number, StandardisedTemperature,
                         StandardisedSalinity,StandardisedGradient, 
                         StandardisedDepth, StandardisedProductivity) 

pairs(env_all)

env_all_clean <- na.omit(env_all)  # Removes rows with NA values
cor_matrix <- cor(env_all_clean, method = "pearson")


corrplot(cor_matrix, method = "circle", type = "upper", order = "hclust",
         tl.col = "black", tl.srt = 45, # Text label color and rotation
         addCoef.col = "black", # Add correlation coefficients to the plot
         tl.cex = 0.6) # Adjust text label size


# convert substrate tyoe and SAI_evidence to a factors

all$substrate_numbers <- factor(all$substrate_numbers)
all$SAI_Evidence<- factor(all$SAI_Evidence)

## Visualise relationship between salinity and temperature


# Create a scatter plot of salinity vs. temperature
ggplot(all, aes(x = AverageTemperature, y = AverageSalinity)) +
  geom_point() +  # This adds the scatter plot points
  labs(title = "Salinity vs. Temperature", x = "Temperature", y = "Salinity") +  # Add labels
  theme_minimal()  # Use a minimal theme for a nicer look


#### Richness GLMMs #####


## without zero inflation: AIC =  1151.7 

p_model_all <- glmmTMB(Richness ~ (StandardisedTemperature + StandardisedSalinity + StandardisedGradient 
                                   + SAI_Evidence + substrate_numbers) * StandardisedDepth + 
                         (1|dive), data = all, family = poisson)

summary(p_model_all)

## with zero inflation: AIC = 1149.5

p_model_zero_inflated <- glmmTMB(Richness ~ (StandardisedTemperature + StandardisedSalinity +
                                               StandardisedGradient + SAI_Evidence + substrate_numbers) * StandardisedDepth 
                                 + (1|dive), ziformula = ~1, data = all, family = poisson)

summary(p_model_zero_inflated)

### USING NEGATIVE BINOMIAL 

# not accounting for 0 inflation: AIC =    1107.3

N_model <- glmmTMB(Richness ~ (StandardisedTemperature + StandardisedSalinity + StandardisedGradient +
                                 + SAI_Evidence + substrate_numbers) *  StandardisedDepth
                   + (1|dive), data = all, family = nbinom2)

summary(N_model)

# Accounting for 0 inflation: AIC =      1105.4


N_model_zero_inflated <- glmmTMB(Richness ~ (StandardisedTemperature + StandardisedSalinity+ StandardisedGradient
                                             + SAI_Evidence + substrate_numbers)  *  StandardisedDepth
                                 + (1|dive), data = all,  ziformula = ~1, family = nbinom2)



summary(N_model_zero_inflated)
