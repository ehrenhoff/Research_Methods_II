# Install packages if necessary.

# install.packages("tidyverse")
# install.packages("readxl")
# install.packages("viridis")

rm(list = ls())
library(tidyverse)
library(readxl)
library(viridis)


# Just for testing machine
#setwd("~/Classes/Spring 2024/Research Methods 2/R/Tutorial Project")

# Read in the data
mockData <- read_excel("mockData1.xlsx")

# Pivot all columns but first one
mockData <- pivot_longer(mockData, -1)

# Get rid of ellipses 
mockData <- mutate(mockData, name = gsub("\\.\\.\\.\\d+", "", name))

# Separate "name" column into two colummns: AMP and Concentration (uM):
mockData <- separate(mockData, name, c("AMP", "Concentration (uM)"), "-")

## This is one way to create a Type column and populate it with the AMP type
mockData$Type <- NA
# Step 1: Needs to be done before step 2.
mockData$Type <- if_else(mockData$AMP %in% c("G AMP1", "G AMP2"), "Guided", "Unguided")
# Step 2: Do this last to avoid the empty string being overwritten.
mockData$Type <- if_else(mockData$AMP %in% c("Blank", "Positive Control"), "", mockData$Type)


# This is a cleaner alternative to the above, but less descriptive.
# mockDataAlt <- mutate(mockData, Type = if_else(AMP %in% c("Blank", "Positive Control"), "",
#                                           if_else(AMP %in% c("G AMP1", "G AMP2"), "Guided",
#                                                   "Unguided")))
# True
# identical(mockData, mockDataAlt)


# Create BASE_AMP column to show the AMP name, whether guided or unguided
# if there is a "G" in the AMP value, split it at the space and take out the second part (the base AMP)
mockData$BASE_AMP=if_else(grepl("G", mockData$AMP), as.character(map(strsplit(mockData$AMP," "),2)), mockData$AMP)

# After this we will have a grouped table that contains all our values in longer format.
mockData <- mockData %>% group_by(AMP, BASE_AMP, `Concentration (uM)`, Type, Time)

# Now we can calculate summary statistics by group, including the mean and the upper_confindence and lower_confindence confidence intervals.
mockData <- summarize(mockData, Mean=mean(value),
          number=n(),
          upper_confindence=mean(value)+sd(value)/sqrt(n()),
          lower_confindence=mean(value)-sd(value)/sqrt(n()))

# Ensure the 'Concentration (uM)' is numeric for correct ordering in plots
mockData <- mockData %>%
  mutate(`Concentration (uM)` = as.numeric(`Concentration (uM)`))


# The final thing is to discard all some data, since bacterial growth rates are variable, 
# and we might end up with more intermediate measurements than we need. In this case I decided  
# to use a modulus operation to discard all but every third row.
# create a vector of rows with mod 3 calculations:
test <- (1:nrow(mockData)) %% 3

# Filter where mod 3 == 0 is true
mockData <- mockData[-(1:nrow(mockData)) %% 3 == 0, ]


## Now that data is formatted properly, we can plot it.

# Custom palette for plot with transparent blue ribbon (the blue geom_smooth ribbon affects color, so I needed to adjust the other color values to get the overall color I wanted.)
custom_palette <- c(viridis_pal()(0.2), "#cf5829", viridis_pal(option = "C")(0.5))

# Custom theme with bold axis text and title
my_theme <- theme_minimal() +
  theme(
    axis.title = element_text(face = "bold"),  # Axis titles (Hours and OD)
    plot.title = element_text(face = "bold")  # Plot title
)


## Plot the data:
# Use the custom theme with color adjusted for ribbon
mockPlot <- mockData %>%
  filter(BASE_AMP != "Blank" & BASE_AMP != "Positive Control") %>%
  ggplot(aes(x = Time, y = Mean)) +
  geom_line(aes(group = Type, color = Type)) +  
  geom_errorbar(aes(ymin = lower_confindence,ymax = upper_confindence,group = Type, color = Type), size=0.5, width=0.05) +  
  geom_ribbon(aes(ymin = lower_confindence, ymax = upper_confindence, group = Type), 
              fill = "lightblue", alpha = 0.5) +
  labs(color = "Type", x = "Hours", y = "Optical Density (OD600 nm)",
       caption = "With exposure to antimicrobial peptides at concentrations of 0.5 µm - 32 µm") +
  scale_color_manual(values = custom_palette) +
  facet_grid(BASE_AMP ~ as.factor(`Concentration (uM)`), scales = "free_y") +
  my_theme +
  ggtitle("Hypothetical Bacterial Growth")

print(mockPlot)

## This plot needs to be scaled quite large in order to show distinct error bars. 
## It can be adjusted further on different screen resolutions.
## My plot is included in the folder.
ggsave("mockPlot.jpg", mockPlot, units = c ("px"), width = 2880, height = 1575)









             
             






