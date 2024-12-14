#########################################################################
#ANOVA to confirm differences between Q values
##########################################################################
# Load necessary libraries
library(dplyr)

# Define the file paths for your CSV files using your directory
file_paths <- c("C:/Users/BerrymanInstitue/Documents/Agent Based Modeling/0.1Butterflies.experiment Corridor Width Over Time.csv",
                "C:/Users/BerrymanInstitue/Documents/Agent Based Modeling/0.3Butterflies.experiment Corridor Width Over Time.csv",
                "C:/Users/BerrymanInstitue/Documents/Agent Based Modeling/0.4Butterflies.experiment Corridor Width Over Time.csv",
                "C:/Users/BerrymanInstitue/Documents/Agent Based Modeling/0.5Butterflies.experiment Corridor Width Over Time.csv",
                "C:/Users/BerrymanInstitue/Documents/Agent Based Modeling/0.7Butterflies.experiment Corridor Width Over Time.csv",
                "C:/Users/BerrymanInstitue/Documents/Agent Based Modeling/0.9Butterflies.experiment Corridor Width Over Time.csv")

# Corresponding q values for each file
q_values <- c(0.1, 0.3, 0.4, 0.5, 0.7, 0.9)

# Create an empty dataframe to store all the data
combined_data <- data.frame()

# Loop through each file and combine the data
for (i in 1:length(file_paths)) {
  # Read the CSV file, skipping the first 24 lines to reach the data section
  df <- read.csv(file_paths[i], skip = 24, header = TRUE)
  
  # Rename columns to match 'tick' and 'corridor_width'
  colnames(df) <- c("tick", "corridor_width", "color", "pen_down")
  
  # Remove unnecessary columns ("color" and "pen_down") and rows with missing data
  df <- df %>% select(tick, corridor_width) %>% filter(!is.na(tick), !is.na(corridor_width))
  
  # Add the q value as a new column
  df$q <- q_values[i]
  
  # Combine with the main dataframe
  combined_data <- rbind(combined_data, df)
}

# View the first few rows of the combined data to ensure it's correct
head(combined_data)

# Perform ANOVA to test the effect of q on corridor_width
anova_result <- aov(corridor_width ~ as.factor(q), data = combined_data)

# Display the ANOVA results
summary(anova_result)

# Perform post-hoc Tukey's test to see which q values differ
TukeyHSD(anova_result)
#Q is signficantly impacting coordior width
#and there is significant changes between Q value groups (most between 0.1 and 0.9)
#between 0.4 and 0.5 also significant.

##############################################################
#line plot code
#############################################################
# Load necessary libraries
library(ggplot2)

# Plot the corridor width over time for each q value
ggplot(combined_data, aes(x = tick, y = corridor_width, color = as.factor(q), group = as.factor(q))) +
  geom_line(size = 1) +
  labs(title = "Corridor Width Over Time for Different q Values",
       x = "Tick",
       y = "Corridor Width",
       color = "q Values") +
  theme_minimal() +
  theme(legend.position = "right") +
  scale_color_brewer(palette = "Set1")  # Using a color palette for better visualization

