####################################################################################################################

# Capstone Project
#
# Exploring Crime Patterns and Predictions in Philadelphia Through Machine Learning
#
# For the Requirement of *HarvardX: PH125.9x Data Science: Capstone Course
# 
# Author: Guler Arsal
#

####################################################################################################################
# # The following packages are required.
####################################################################################################################
# Install and load required libraries (explicitly listed)
if (!require(knitr)) install.packages("knitr", repos = "http://cran.us.r-project.org")
if (!require(kableExtra)) install.packages("kableExtra", repos = "http://cran.us.r-project.org")
if (!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")
if (!require(dplyr)) install.packages("dplyr", repos = "http://cran.us.r-project.org")
if (!require(caret)) install.packages("caret", repos = "http://cran.us.r-project.org")
if (!require(ggplot2)) install.packages("ggplot2", repos = "http://cran.us.r-project.org")
if (!require(ggpubr)) install.packages("ggpubr", repos = "http://cran.us.r-project.org")
if (!require(lubridate)) install.packages("lubridate", repos = "http://cran.us.r-project.org")
if (!require(stringr)) install.packages("stringr", repos = "http://cran.us.r-project.org")
if (!require(scales)) install.packages("scales", repos = "http://cran.us.r-project.org")
if (!require(forcats)) install.packages("forcats", repos = "http://cran.us.r-project.org")
if (!require(readr)) install.packages("readr", repos = "http://cran.us.r-project.org")
if (!require(reticulate)) install.packages("reticulate", repos = "http://cran.us.r-project.org")
if (!require(bookdown)) install.packages("bookdown", repos = "http://cran.us.r-project.org")
if (!require(VIM)) install.packages("VIM", repos = "http://cran.us.r-project.org")
if (!require(viridis)) install.packages("viridis", repos = "http://cran.us.r-project.org")
if (!require(RColorBrewer)) install.packages("RColorBrewer", repos = "http://cran.us.r-project.org")
if (!require(sf)) install.packages("sf", repos = "http://cran.us.r-project.org")
if (!require(rpart)) install.packages("rpart", repos = "http://cran.us.r-project.org")
if (!require(rpart.plot)) install.packages("rpart.plot", repos = "http://cran.us.r-project.org")
if (!require(zipcodeR)) install.packages("zipcodeR", repos = "http://cran.us.r-project.org")
if (!require(randomForest)) install.packages("randomForest", repos = "http://cran.us.r-project.org")
if (!require(smotefamily)) install.packages("smotefamily", repos = "http://cran.us.r-project.org")
if (!require(themis)) install.packages("themis", repos = "http://cran.us.r-project.org")
if (!require(recipes)) install.packages("recipes", repos = "http://cran.us.r-project.org")
if (!require(pROC)) install.packages("pROC", repos = "http://cran.us.r-project.org")

# Load all libraries after installation
library(knitr)
library(kableExtra)
library(tidyverse)
library(dplyr)
library(caret)
library(ggplot2)
library(ggpubr)
library(lubridate)
library(stringr)
library(scales)
library(forcats)
library(readr)
library(reticulate)
library(bookdown)
library(VIM)
library(viridis)
library(RColorBrewer)
library(sf)
library(rpart)
library(rpart.plot)
library(zipcodeR)
library(randomForest)
library(smotefamily)
library(themis)
library(recipes)
library(pROC)

####################################################################################################################
# Philadelphia Crime Dataset
####################################################################################################################
# Download Philadelphia Crime Dataset from KAGGLE
# Kaggle is a Python package (not R package), so I need to install and use it via Python 

# Install the reticulate package in R 
# install.packages("reticulate")

# Install Python from R
# reticulate::install_python()

# Check Python configuration
# py_config()

# Before installing Kaggle, make sure to set up your Kaggle API key
# Step 1: Get your API Key (Go to Kaggle's Account Page, Under the API section, click Create New API Token. This will download a kaggle.json file)
# Step 2: Place the API Key (Move the kaggle.json file to the folder C:/Users/<Your Username>/.kaggle/ )

# Install the kaggle Python package
# py_install("kaggle")

# Import the Kaggle API extended module
kaggle_api <- import("kaggle.api.kaggle_api_extended", convert = TRUE)

# Run Kaggle CLI command to download dataset
system("kaggle datasets download -d mchirico/philadelphiacrimedata -p data --unzip")

# Load the CSV file into R
df <- read_csv("data/crime.csv")

####################################################################################################################
## Philadelphia Crime Dataset Overview
####################################################################################################################
# Create the data frame with the added Category column
crime_data <- data.frame(
  Variable = c(
    "Dc_Dist", "Psa", "Dispatch_Date_Time", "Dispatch_Date", 
    "Dispatch_Time", "Hour", "Dc_Key", "Location_Block", 
    "UCR_General", "Text_General_Code", "Police_Districts", 
    "Month", "Lon", "Lat"),
  Description = c(
    "District Code of the crime occurred",
    "Police Service Area code, which further divides a district for local policing",
    "The date and time when the crime dispatch occurred",
    "The date of the crime dispatch",
    "The time of the crime dispatch (hours and minutes)",
    "Hour of the day",
    "A unique identifier for each crime incident",
    "The block location where the crime occurred",
    "Uniform Crime Reporting General category code, used to classify crimes",
    "A textual general code for the type of crime",
    "The police district where the crime occurred",
    "The month of the dispatch",
    "Longitude of the crime location",
    "Latitude of the crime location"),
  Category = c(
    "Spatial", "Spatial", "Temporal", "Temporal", 
    "Temporal", "Temporal", "Crime Type", "Spatial", 
    "Crime Type", "Crime Type", "Spatial", 
    "Temporal", "Spatial", "Spatial")
)

# Generate a table 
kbl(crime_data, 
    # format = 'latex', 
    booktabs = TRUE, 
    caption = "Summary of Variables in Philadelphia Crime Dataset",
    linesep = "\\addlinespace[12pt]",
    col.names = c("Variable Name", "Description", "Category")) %>%
  kable_styling(latex_options = c("HOLD_position"),
                position = "left", 
                stripe_color = "gray!15", 
                font_size = 11, 
                full_width = TRUE) %>%
  column_spec(1, width = "3.5cm") %>%  
  column_spec(2, width = "9cm") %>%
  column_spec(3, width = "2.5cm") %>%
  row_spec(0, bold = TRUE)



# Calculate and filter the number of missing values
# Print the filtered table using kable
kable(
  data.frame(
    Column = names(df),
    Missing_Values = colSums(is.na(df)),
    Percentage = round((colSums(is.na(df)) / nrow(df)) * 100, 2)
  ) %>%
    filter(Missing_Values > 0),
  # format = 'latex',
  booktabs = TRUE,
  caption = "Number and Percentage of Missing Values in Columns",
  linesep = "\\addlinespace[12pt]",
  col.names = c("Variable Name", "Missing Values", "Percentage (%)"),
  row.names = FALSE
) %>%
  kable_styling(
    latex_options = c("HOLD_position"),
    position = "left",
    stripe_color = "gray!15",
    font_size = 11,
    full_width = FALSE
  ) %>%
  row_spec(0, bold = TRUE)

####################################################################################################################
# Exploratory Data Analysis
####################################################################################################################

# FIGURE 1: Number of Crimes by Crime Type
# Distribution of Number of Crimes by Crime Type
ggplot(df %>% 
         filter(!is.na(Text_General_Code)),  # Filter out NA values in Text_General_Code
       aes(x = fct_rev(fct_infreq(Text_General_Code)), fill = after_stat(count))) +  # Ordered
  geom_bar() +
  coord_flip() +  # Flip the axes for horizontal bars
  labs(x = "Crime Type", y = "Number of Crimes") +
  scale_y_continuous(labels = comma) +
  scale_fill_viridis(option = "viridis", direction = -1, guide = "none") +  # Apply viridis palette
  theme(axis.text.y = element_text(size = 7),  # Set y-axis text size smaller
        legend.position = "none")


# THEMATIC ORGANIZATIONOF CRIME TYPES
# Define other offenses crime types
other_offenses <- c(
  "All Other Offenses", 
  "Other Assaults", 
  "Other Sex Offenses (Not Commercialized)")

# Define property crime types
property_crimes <- c(
  "Thefts", 
  "Vandalism/Criminal Mischief", 
  "Theft from Vehicle",   
  "Motor Vehicle Theft",
  "Fraud", 
  "Recovered Stolen Motor Vehicle", 
  "Burglary Residential", 
  "Burglary Non-Residential", 
  "Arson", 
  "Forgery and Counterfeiting", 
  "Embezzlement", 
  "Receiving Stolen Property")

# Define the drug and alcohol-related crime types
drug_alcohol_crimes <- c(
  "Narcotic / Drug Law Violations", 
  "DRIVING UNDER THE INFLUENCE", 
  "Liquor Law Violations", 
  "Public Drunkenness")

# Define the crimes against public order and safety
public_order_crimes <- c(
  "Disorderly Conduct", 
  "Weapon Violations", 
  "Gambling Violations",
  "Prostitution and Commercialized Vice", 
  "Vagrancy/Loitering")

# Define the violent crime types
violent_crimes <- c(
  "Aggravated Assault No Firearm", 
  "Aggravated Assault Firearm", 
  "Robbery No Firearm", 
  "Robbery Firearm",  
  "Offenses Against Family and Children",   
  "Rape", 
  "Homicide - Gross Negligence", 
  "Homicide - Justifiable",
  "Homicide - Criminal" )

# Create a new variable called "type" 
df <- df %>%
  mutate(crime_type = case_when(
    Text_General_Code %in% other_offenses ~ "Other Offenses",
    Text_General_Code %in% property_crimes ~ "Property Crimes",
    Text_General_Code %in% drug_alcohol_crimes ~ "Drug and Alcohol-Related Crimes",
    Text_General_Code %in% public_order_crimes ~ "Crimes Against Public Order",
    Text_General_Code %in% violent_crimes ~ "Violent Crimes"))         


####################################################################################################################
## Temporal Crime Analysis
####################################################################################################################

# Extract month (as label), year, and minute from Dispatch_Date_Time
df <- df %>% mutate(
  Month_label = factor(month(Dispatch_Date_Time, label = TRUE, abbr = TRUE)),
  year = year(Dispatch_Date_Time), 
  month = month(Dispatch_Date_Time),
  day = mday(Dispatch_Date_Time),       # Day of the month from 1 to 31
  weekday = wday(Dispatch_Date_Time, week_start = 1) # Numeric day of the week (1 = Monday)
)



# FIGURE 2: Number of Crimes by Crime Date
# Grouping by Dispatch_Date to get counts  & Plot using ggplot 
df %>% 
  group_by(Dispatch_Date) %>% 
  summarise(n = n(), .groups = 'drop') %>%
  ggplot(aes(Dispatch_Date, n)) +
  geom_line(color = "#1f78b4") +
  geom_smooth(aes(y = n), method = "loess", se = FALSE, color = "black", linetype = "dashed") + 
  labs(x = "Crime Date", y = "Number of Crimes") +
  geom_vline(xintercept = as.Date(c("2005-12-25", "2006-12-25", "2007-12-25", "2008-12-25", "2009-12-25",
                                    "2010-12-25", "2011-12-25", "2012-12-25", "2013-12-25", "2014-12-25",
                                    "2015-12-25", "2016-12-25", "2017-12-25")), # Adding vertical lines to highlight Christmas
             color = "red", linetype = "dotted") +
  annotate("text", x = as.Date("2016-12-25"), y = 900, label = "Christmas Day", 
           color = "red", angle = 90, vjust = -0.7, hjust = 0.9, size = 3)



# FIGURE 3: Number of Crimes by Crime Month
# Grouping by Month to get counts & Plot using ggplot 
df %>% mutate(Month = ym(Month)) %>%
  group_by(Month) %>% 
  summarise(n = n(), .groups = 'drop') %>%
  ggplot(aes(Month, n)) +
  geom_line(color = "#1f78b4") +
  geom_smooth(aes(y = n), method = "loess", se = FALSE, color = "black", linetype = "dashed", size = 0.8) + 
  labs(x = "Crime Month", y = "Number of Crimes") +
  scale_x_date(date_breaks = "12 months", date_labels = "%Y-%m") +  # Show every 12 months
  scale_y_continuous(labels = comma)



# FIGURE 4: Number of Crimes by Crime Month for Other Crimes
df %>%
  mutate(Month = ym(Month)) %>%
  filter(Text_General_Code %in% other_offenses) %>%
  group_by(Month, Text_General_Code) %>%
  summarise(n = n(), .groups = 'drop') %>%
  ggplot(aes(x = Month, y = n)) +
  geom_line(color = "#1f78b4") +
  geom_smooth(aes(y = n), method = "loess", se = FALSE, color = "black", linetype = "dashed", size = 0.6) + 
  labs(x = "Crime Month", y = "Number of Crimes") +
  scale_y_continuous(labels = comma) +
  facet_wrap(~ Text_General_Code, scales = "free_y", nrow = 3, ncol = 1) +  
  theme(strip.text = element_text(size = 8, face = "bold"),
        strip.background = element_blank(),
        panel.spacing = unit(0.5, "lines"),
        axis.text.y = element_text(size = 8),
        axis.text.x = element_text(size = 8))



# FIGURE 5: Number of Crimes by Crime Month for Property Crimes
# Filter the data for the property crime types & Plot using ggplot
df %>%
  mutate(Month = ym(Month)) %>%
  filter(Text_General_Code %in% property_crimes) %>%  # Use %in% to filter for multiple crime types
  mutate(Text_General_Code = factor(Text_General_Code, levels = property_crimes)) %>%
  group_by(Month, Text_General_Code) %>%  # Group by Dispatch_Date and Text_General_Code
  summarise(n = n(), .groups = 'drop')  %>% # Drop grouping after summarising
  ggplot(aes(x = Month, y = n)) +
  geom_line(color = "#1f78b4") +
  geom_smooth(aes(y = n), method = "loess", se = FALSE, color = "black", linetype = "dashed", size = 0.5) + 
  labs(x = "Crime Month", y = "Number of Crimes") +
  scale_y_continuous(labels = comma) +
  facet_wrap(~ Text_General_Code, scales = "free_y", nrow = 6, ncol = 2) +  
  theme(strip.text = element_text(size = 8, face = "bold"),
        strip.background = element_blank(),  # Remove background to make it look narrower
        panel.spacing = unit(0.5, "lines"),  # Adjust spacing between panels if needed
        axis.text.y = element_text(size = 8),
        axis.text.x = element_text(size = 8)) # Set y-axis text size smalleR



# FIGURE 6: Number of Crimes by Crime Month for Drug and Alcohol-Related Crimes
# Filter the data for drug and alcohol-related crimes & Plot using ggplot
df %>%
  mutate(Month = ym(Month)) %>%
  filter(Text_General_Code %in% drug_alcohol_crimes) %>%
  mutate(Text_General_Code = factor(Text_General_Code, levels = drug_alcohol_crimes)) %>%
  group_by(Month, Text_General_Code) %>%
  summarise(n = n(), .groups = 'drop') %>%
  ggplot(aes(x = Month, y = n)) +
  geom_line(color = "#1f78b4") +
  geom_smooth(aes(y = n), method = "loess", se = FALSE, color = "black", linetype = "dashed", size = 0.5) + 
  labs(x = "Crime Month", y = "Number of Crimes") +
  scale_y_continuous(labels = comma) +
  facet_wrap(~ Text_General_Code, scales = "free_y", nrow = 4, ncol = 1) +  
  theme(strip.text = element_text(size = 8, face = "bold"),
        strip.background = element_blank(),
        panel.spacing = unit(0.5, "lines"),
        axis.text.y = element_text(size = 8),
        axis.text.x = element_text(size = 8))



# FIGURE 7: Number of Crimes by Crime Month for Crimes Against Public Order and Safety
# Filter the data for public order crimes & Plot using ggplot
df %>%
  mutate(Month = ym(Month)) %>%
  filter(Text_General_Code %in% public_order_crimes) %>%
  mutate(Text_General_Code = factor(Text_General_Code, levels = public_order_crimes)) %>%
  group_by(Month, Text_General_Code) %>%
  summarise(n = n(), .groups = 'drop') %>%
  ggplot(aes(x = Month, y = n)) +
  geom_line(color = "#1f78b4") +
  geom_smooth(aes(y = n), method = "loess", se = FALSE, color = "black", linetype = "dashed", size = 0.5) + 
  labs(x = "Crime Month", y = "Number of Crimes") +
  scale_y_continuous(labels = comma) +
  facet_wrap(~ Text_General_Code, scales = "free_y", nrow = 5, ncol = 1) +  
  theme(strip.text = element_text(size = 8, face = "bold"),
        strip.background = element_blank(),
        panel.spacing = unit(0.5, "lines"),
        axis.text.y = element_text(size = 8),
        axis.text.x = element_text(size = 8)) 



# FIGURE 8: Number of Crimes by Crime Month for Violent Crimes
# Filter the data for violent crimes & Plot using ggplot
df %>%
  mutate(Month = ym(Month)) %>%
  filter(Text_General_Code %in% violent_crimes) %>%
  mutate(Text_General_Code = factor(Text_General_Code, levels = violent_crimes)) %>%
  group_by(Month, Text_General_Code) %>%
  summarise(n = n(), .groups = 'drop') %>%
  ggplot(aes(x = Month, y = n)) +
  geom_line(color = "#1f78b4") +
  geom_smooth(aes(y = n), method = "loess", se = FALSE, color = "black", linetype = "dashed", size = 0.5) + 
  labs(x = "Crime Month", y = "Number of Crimes") +
  scale_y_continuous(labels = comma) +
  facet_wrap(~ Text_General_Code, scales = "free_y", nrow = 5, ncol = 2) +  
  theme(strip.text = element_text(size = 8, face = "bold"),
        strip.background = element_blank(),
        panel.spacing = unit(0.5, "lines"),
        axis.text.y = element_text(size = 8),
        axis.text.x = element_text(size = 8)) 



# FIGURE 9: Number of Crimes by Monthly Aggregation
# Bar plot that aggregates crime data for each month
df %>%
  count(Month_label) %>% 
  ggplot(aes(x = Month_label, y = n, fill = n)) +  # Fill based on the count (n)
  geom_bar(stat = "identity", color = "black") +
  labs(x = "Crime Month", y = "Number of Crimes") +
  scale_y_continuous(labels = comma) +
  scale_fill_viridis(option = "viridis", direction = -1, labels = scales::comma) +  # Use viridis palette
  guides(fill = guide_colorbar(title = "Number of Crimes")) +  # Show the legend title
  theme(legend.position = "right", 
        legend.title = element_text(size = 10),
        legend.text = element_text(size = 10)) 



# FIGURE 10: Number of Crimes by Crime Hour
# Distribution of Number of Crimes by Crime Hour
ggplot(df, aes(x = Hour)) +
  geom_bar(aes(fill = after_stat(count)), color = "black") +  # Use geom_bar instead of geom_histogram
  labs(x = "Crime Hour", y = "Number of Crimes") +
  scale_y_continuous(labels = comma) +
  scale_x_continuous(breaks = seq(0, 23, by = 1)) +  # Show all hour numbers
  scale_fill_viridis_c(option = "viridis", direction = -1, guide = "none")  # Apply viridis color palette



# FIGURE 11: Number of Crimes by Crime Hour for Each Year
# Distribution of Number of Crimes by Crime Hour (grouped by year)
df %>% 
  mutate(year = as.factor(year)) %>% 
  group_by(year, Hour) %>%
  summarise(n = n(), .groups = 'drop') %>%
  ggplot(aes(Hour, n, color = year)) + 
  geom_line(size = 1) + 
  labs(x = "Hour of the Day", y = "Number of Crimes", color = NULL) +
  scale_y_continuous(labels = comma) +
  scale_x_continuous(breaks = seq(0, 23, by = 1)) +  # Show all hour numbers
  scale_color_viridis(discrete = TRUE, option = "viridis") + # The default palette is viridis, but you can try different palettes such as "magma", "plasma", "inferno", and "cividis"
  theme(legend.position = "right",legend.text = element_text(size = 8)) 



# FIGURE 12: Number of Crimes by Crime Hour for Each Crime Type
# Distribution of Different Crime Types Over The Hours of The Day
ggplot(df %>% 
         filter(!is.na(Text_General_Code)) %>% 
         mutate(Text_General_Code = factor(Text_General_Code)),  # Temporarily reorder crime types
       aes(x = Hour, fill = after_stat(count))) +
  geom_histogram(binwidth = 1, color = "black") +
  labs(x = "Crime Hour", y = "Number of Crimes") +
  scale_y_continuous(labels = comma) +
  scale_x_continuous(breaks = seq(0, 23, by = 2)) +  # Show all hour numbers
  scale_fill_viridis(option = "viridis", direction = -1, guide = "none") +
  facet_wrap(~ Text_General_Code, scales = "free_y", nrow = 11, ncol = 3) +  
  theme(strip.text = element_text(size = 7, face = "bold"),
        strip.background = element_blank(),  # Remove background to make it look narrower
        panel.spacing = unit(0.5, "lines"),  # Adjust spacing between panels if needed
        axis.text.y = element_text(size = 6))  # Set y-axis text size smaller



# FIGURE 13: Number of Crime by Weekday
# Bar plot that aggregates crime data for each month
df %>%  mutate(weekday = factor(wday(Dispatch_Date_Time, label = TRUE, abbr = TRUE))) %>% 
  count(weekday) %>% 
  ggplot(aes(x = weekday, y = n, fill = n)) +  # Fill based on the count (n)
  geom_bar(stat = "identity", color = "black") +
  labs(x = "Crime Weekday", y = "Number of Crimes") +
  scale_y_continuous(labels = comma) +
  scale_fill_viridis(option = "viridis", direction = -1, labels = scales::comma) +  # Use viridis palette
  guides(fill = guide_colorbar(title = "Number of Crimes")) +  # Show the legend title
  theme(legend.position = "right", 
        legend.title = element_text(size = 10),
        legend.text = element_text(size = 10)) 



####################################################################################################################
## Spatial Crime Analysis
####################################################################################################################

# Downloading Police District Boundaries from opendataphilly.org
# Set the URL for the GeoJSON file
url <- "https://opendata.arcgis.com/datasets/62ec63afb8824a15953399b1fa819df2_0.geojson"
# Specify the destination file path
destfile <- "Boundaries_District.geojson"

# Download the file
download.file(url, destfile, mode = "wb")  # 'wb' is for binary mode

# Read the GeoJSON file into an sf object
boundary <- st_read("Boundaries_District.geojson")



# Update the Dc_Dist variable to reflect district mergers
df <- df %>%
  mutate(Dc_Dist = case_when(
    Dc_Dist == "04"  ~ "03",   # Merge old 4th District into 3rd
    Dc_Dist == "06"  ~ "09",   # Merge old 6th District into 9th
    Dc_Dist == "23" ~ "22",  # Merge old 23rd District into 22nd
    Dc_Dist == "92" ~ "16",  # Merge old 92nd District into 16th
    TRUE ~ Dc_Dist            # Keep all other values unchanged
  ))



# FIGURE 14: Number of Crimes by District Code
# Number of Crimes by District Code
ggplot(df, aes(x = fct_infreq(Dc_Dist), fill = after_stat(count))) + # ordered
  geom_bar(color = "black") +
  labs(x = "District Code", y = "Number of Crimes") +
  scale_y_continuous(labels = comma) +
  scale_fill_viridis(option = "viridis", direction = -1, guide = "none") 



# FIGURE 15: Spatial Distribution of Crime Density Across Districts
# Randomly sample 10% of the data 
set.seed(123)  # Set seed for reproducibility
df_sample <- df[sample(nrow(df), size = 0.1 * nrow(df)), ]

# Calculate centroids for each polygon (district)
boundary_centroids <- boundary %>%  st_centroid() 

# Plot the crime density and district boundaries with centroids for labeling
ggplot(df_sample, aes(x = Lon, y = Lat)) +
  stat_density_2d(geom = "polygon", contour = TRUE,
                  aes(fill = after_stat(level)),
                  bins = 12) +
  # Add the district boundaries to the plot
  geom_sf(data = boundary, color = "black", fill = "ivory2", alpha = 0.2, size = 1, inherit.aes = FALSE) +
  # Add the district labels at centroids
  geom_text(data = boundary_centroids, 
            aes(x = st_coordinates(geometry)[, 1],  # Extract the longitude
                y = st_coordinates(geometry)[, 2],  # Extract the latitude
                label = DISTRICT_), 
            size = 4, color = "red4") +  # Add the labels for each district
  scale_fill_distiller(palette = "OrRd", direction = 1) +
  labs(fill = "Density Level",
       x = "Longitude",
       y = "Latitude") +  
  theme(legend.position = "right")   # Position legend on the right



# FIGURE 16: Number of Crimes by Crime Type and Districs
# Summarize the data and remove NA values
df_summary <- df %>% 
  group_by(crime_type, Dc_Dist) %>%
  summarise(n = n(), .groups = 'drop') %>%
  filter(!is.na(crime_type))  

ggplot(df_summary, aes(x = Dc_Dist, y = n, fill = n)) +
  geom_bar(stat = "identity") +
  labs(x = "Districts", y = "Number of Crimes") +
  scale_y_continuous(labels = comma) +
  scale_fill_viridis(option = "viridis", direction = -1, guide = "none") +
  facet_wrap(~ crime_type, scales = "free_y", nrow = 6, ncol = 1) +  # Create a separate plot for each district
  scale_x_discrete(limits = unique(df_summary$Dc_Dist)) +  # Ensure all districts are shown on the x-axis 
  theme(
    strip.text = element_text(size = 9, face = "bold"),  # More readable facet labels
    strip.background = element_blank(),  # Remove background to make it look narrower
    panel.spacing = unit(0.7, "lines"),  # Slightly increased spacing between panels
    axis.text.x = element_text(size = 8),  
    axis.text.y = element_text(size = 8)
  ) 



# FIGURE 17: Heatmap of Number of Crimes by Districts and Police Service Area
# Summarize the data to get crime counts by Districts and Psa
df_summary <- df %>%
  group_by(Dc_Dist, Psa) %>%
  summarise(Frequency = n(), .groups = 'drop') %>%
  arrange(Psa)

# Create a heatmap
ggplot(df_summary, aes(x = Dc_Dist, y = Psa, fill = Frequency)) +
  geom_tile(color = "white") +
  scale_fill_viridis_c(option = "viridis", direction = -1, labels = comma) +  # Apply viridis palette
  labs(x = "District Code",
       y = "Police Service Area",
       fill = "Number of Crimes") +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        legend.position = "right",
        legend.text = element_text(size = 8),
        legend.title = element_text(size = 10),
        axis.text.x = element_text(hjust = 1)) 



####################################################################################################################
# Machine Learning Models
####################################################################################################################

# Calculate the frequency of each crime type
crime_freq <- df %>%
  count(Text_General_Code) %>%
  arrange(desc(n))  # Sort by frequency, highest first

# Get the top 10 most frequent crimes
top_10_crimes <- crime_freq %>%
  top_n(10, n) %>%
  pull(Text_General_Code)

# Filter the dataset to include only the top 10 most frequent crimes and remove rows with NA values
df_filtered <- df %>%
  filter(Text_General_Code %in% top_10_crimes, !is.na(Lat) & !is.na(Lon) & !is.na(Text_General_Code)) %>%
  mutate(
    Text_General_Code = factor(Text_General_Code), 
    weekday = as.integer(weekday),
    month = as.integer(month),
    Dc_Dist = as.integer(Dc_Dist)
  ) %>%
  select(Lat, Lon, Hour, day, weekday, month, year, Dc_Dist, Text_General_Code)



## Data Splitting Strategy
# Split the data into training (80%) and testing (20%) sets
set.seed(123)  # For reproducibility
trainIndex <- createDataPartition(df_filtered$Text_General_Code, p = 0.8, list = FALSE)
trainData <- df_filtered[trainIndex, ]
testData <- df_filtered[-trainIndex, ]

# Further split the training data into training and validation sets (80/20 split of the training set)
set.seed(123)  # For reproducibility
trainIndex2 <- createDataPartition(trainData$Text_General_Code, p = 0.8, list = FALSE)
trainData_raw <- trainData[trainIndex2, ]   # 64% of original data
validationData <- trainData[-trainIndex2, ]   # 16% of original data



## Handling Class Imbalance: SMOTE Application
# Create a recipe with SMOTE and apply it to the training data only (trainData_raw)
trainData_final <- recipe(Text_General_Code ~ ., data = trainData_raw) %>%
  step_smote(Text_General_Code) %>%
  prep() %>%
  bake(new_data = NULL)



####################################################################################################################
## Model Training and Validation
####################################################################################################################

### Model 1a: Decision Tree Model (Original Data)
# Train the Decision Tree model on the original training data
m1a <- rpart(Text_General_Code ~ ., data = trainData_raw) 

# Predict on the validation data 
pred_m1a <- predict(m1a, validationData, type = "class")

# Evaluate the model on the validation set
cm_m1a <- confusionMatrix(pred_m1a, validationData$Text_General_Code)

# Print overall statistics on validation set
print(cm_m1a$overall)

##########################################################

### Model 1b: Decision Tree Model with SMOTE-Enhanced Data
# Train the Decision Tree model on the final training data (trainData_final)
m1b <- rpart(Text_General_Code ~ ., data = trainData_final) 

# Predict on the validation data for tuning/evaluation purposes
pred_m1b <- predict(m1b, validationData, type = "class")

# Evaluate the model on the validation set
cm_m1b <- confusionMatrix(pred_m1b, validationData$Text_General_Code)

# Print overall statistics on validation set
print(cm_m1b$overall)

##########################################################

### Model 1c: Tuned Decision Tree Model (Original Data)
# Define train control for cross-validation
train_control <- trainControl(method = "cv", number = 5, verboseIter = FALSE)

# Train a Decision Tree model with tuning
m1c <- train(
  Text_General_Code ~ ., 
  data = trainData_raw, 
  method = "rpart", 
  trControl = train_control,
  tuneGrid = expand.grid(cp = seq(0.0005, 0.005, by = 0.0005))  # Tune the complexity parameter (cp)
)

# Predict on validation data for evaluation
pred_m1c <- predict(m1c, validationData)

# Evaluate the model on the validation set
cm_m1c <- confusionMatrix(pred_m1c, validationData$Text_General_Code)

# View the best cp value chosen
print(m1c$bestTune)

# Print overall statistics on validation set
print(cm_m1c$overall)

##########################################################

### Model 2a: Random Forest Model (Original Data)
# Set seed for reproducibility
set.seed(123)

# Train the Random Forest model
m2a <- randomForest(Text_General_Code ~ ., data = trainData_raw, ntree = 50)

# Predict on the validation data
pred_m2a <- predict(m2a, validationData)

# Evaluate the model on the validation set
cm_m2a <- confusionMatrix(pred_m2a, validationData$Text_General_Code)

# Print overall statistics on validation set
print(cm_m2a$overall)

##########################################################

### Model 2b: Random Forest Model with SMOTE-Enhanced Data
# Set seed for reproducibility
set.seed(123)

# Train the Random Forest model
m2b <- randomForest(Text_General_Code ~ ., data = trainData_final, ntree = 50)

# Predict on the validation data
pred_m2b <- predict(m2b, validationData)

# Evaluate the model on the validation set
cm_m2b <- confusionMatrix(pred_m2b, validationData$Text_General_Code)

# Print overall statistics on validation set
print(cm_m2b$overall)



####################################################################################################################
## Model Comparison and Final Selection
####################################################################################################################

# Create a data frame with model performance metrics
model_metrics <- data.frame(
  Model = c("Model 1a", "Model 1b", "Model 1c", "Model 2a", "Model 2b"),
  Description = c("Decision Tree", 
                  "Decision Tree, SMOTE", 
                  "Decision Tree, Tuned", 
                  "Random Forest", 
                  "Random Forest, SMOTE"),
  Accuracy = c(cm_m1a$overall["Accuracy"], 
               cm_m1b$overall["Accuracy"], 
               cm_m1c$overall["Accuracy"], 
               cm_m2a$overall["Accuracy"], 
               cm_m2b$overall["Accuracy"]),
  Kappa = c(cm_m1a$overall["Kappa"], 
            cm_m1b$overall["Kappa"], 
            cm_m1c$overall["Kappa"], 
            cm_m2a$overall["Kappa"], 
            cm_m2b$overall["Kappa"]),
  Accuracy_Lower = c(cm_m1a$overall["AccuracyLower"], 
                     cm_m1b$overall["AccuracyLower"], 
                     cm_m1c$overall["AccuracyLower"], 
                     cm_m2a$overall["AccuracyLower"], 
                     cm_m2b$overall["AccuracyLower"]),
  Accuracy_Upper = c(cm_m1a$overall["AccuracyUpper"], 
                     cm_m1b$overall["AccuracyUpper"], 
                     cm_m1c$overall["AccuracyUpper"], 
                     cm_m2a$overall["AccuracyUpper"], 
                     cm_m2b$overall["AccuracyUpper"]),
  Accuracy_Null = c(cm_m1a$overall["AccuracyNull"], 
                    cm_m1b$overall["AccuracyNull"], 
                    cm_m1c$overall["AccuracyNull"], 
                    cm_m2a$overall["AccuracyNull"], 
                    cm_m2b$overall["AccuracyNull"]),
  Accuracy_P_Value = c(cm_m1a$overall["AccuracyPValue"], 
                       cm_m1b$overall["AccuracyPValue"], 
                       cm_m1c$overall["AccuracyPValue"], 
                       cm_m2a$overall["AccuracyPValue"], 
                       cm_m2b$overall["AccuracyPValue"])
)


# Convert metrics to percentages and format with two decimal places
model_metrics[, c("Accuracy", "Kappa", "Accuracy_Lower", "Accuracy_Upper", "Accuracy_Null")] <- 
  lapply(model_metrics[, c("Accuracy", "Kappa", "Accuracy_Lower", "Accuracy_Upper", "Accuracy_Null")], function(x) round(x * 100, 2))

# Print the comparison table using kable and kableExtra for styling
kable(model_metrics, 
      # format = "latex",  
      booktabs = TRUE, 
      caption = "Comparison of Model Performance Metrics", 
      linesep = "\\addlinespace[10pt]",
      col.names = c("Model", "Description", "Accuracy (%)", "Kappa (%)", "Accuracy Lower (%)", "Accuracy Upper (%)", "Accuracy Null (%)", "Accuracy P-Value"), 
      row.names = FALSE
) %>%
  kable_styling(
    latex_options = c("HOLD_position"), 
    stripe_color = "gray!15", 
    font_size = 10, 
    full_width = FALSE
  ) %>%
  row_spec(0, bold = TRUE) %>%  
  column_spec(1, width = "1.5cm") %>%  
  column_spec(2, width = "2.5cm") %>%    
  column_spec(3, width = "1.5cm") %>%    
  column_spec(4, width = "1.3cm") %>%    
  column_spec(5, width = "1.5cm") %>%  
  column_spec(6, width = "1.5cm") %>%    
  column_spec(7, width = "1.5cm") %>%   
  column_spec(8, width = "1.5cm")  


# FIGURE 18: Feature Importance for Model 2a
# Extract variable importance data and rename row names
importance_df <- as.data.frame(importance(m2a))
rownames(importance_df) <- c(
  "Latitude", "Longitude", "Hour", "Day",
  "Weekday", "Month", "Year", "District"
)

importance_df$Variable <- rownames(importance_df)
rownames(importance_df) <- NULL

# Plot with ggplot2 and align variable names to the left
ggplot(importance_df, aes(x = MeanDecreaseGini, y = reorder(Variable, MeanDecreaseGini))) + 
  geom_point(color = "deepskyblue2", size = 4) +  
  labs(x = "Mean Decrease in Gini", y = "") +
  scale_x_continuous(labels = scales::comma) +
  theme_minimal(base_size = 14) +
  theme(
    axis.title.x = element_text(face = "plain", size = 12),
    axis.text.y = element_text(hjust = 0),
    panel.border = element_rect(color = "gray30", fill = NA) 
  )


####################################################################################################################
## Final Model Performance and Generalization 
####################################################################################################################

# Predict on the test set using the best model (based on validation results)
pred_test <- predict(m2a, testData)

# Evaluate on the test set
cm_test <- confusionMatrix(pred_test, testData$Text_General_Code)

# Print overall statistics on test set
print(cm_test$overall)



## The class-wise metrics for Model 2a 
# Select important metrics and convert to data frame with Class label
class_metrics <- as.data.frame(cm_test$byClass[, c("Sensitivity", "Specificity", "Precision", "F1", "Balanced Accuracy", "Prevalence")])

class_metrics$Class <- rownames(class_metrics)  # Add 'Class' column

# Reorder by 'Prevalence' and select columns in desired order
class_metrics <- class_metrics[order(-class_metrics$Prevalence), c("Class", "Sensitivity", "Specificity", "Precision", "F1", "Balanced Accuracy", "Prevalence")]

# Remove the "Class:" label from the row names and update
class_metrics$Class <- gsub("Class: ", "", class_metrics$Class)

# Convert metrics to percentages and format with two decimal places
class_metrics[ , -1] <- lapply(class_metrics[ , -1], function(x) round(x * 100, 2))

# Print the class-wise metrics table using kable and kableExtra for styling
kable(class_metrics, 
      # format = 'latex', 
      booktabs = TRUE, 
      caption = "Class-Wise Metrics for Model 2a",
      linesep = "\\addlinespace[10pt]",
      row.names = FALSE,
      col.names = c("Class", "Sensitivity (%)", "Specificity (%)", "Precision (%)", "F1 (%)", "Balanced Accuracy (%)", "Prevalence (%)")
) %>%
  kable_styling(
    latex_options = c("HOLD_position"),
    position = "left",
    stripe_color = "gray!15",
    font_size = 10,
    full_width = FALSE
  ) %>%
  row_spec(0, bold = TRUE) %>%  
  column_spec(1, width = "4.5cm") %>%  
  column_spec(2, width = "1.6cm") %>%    
  column_spec(3, width = "1.6cm") %>%    
  column_spec(4, width = "1.5cm") %>%    
  column_spec(5, width = "1.2cm") %>%  
  column_spec(6, width = "1.7cm") %>%    
  column_spec(7, width = "1.5cm") 



# FIGURE 19: Confusion Matrix for Model 2a
# Convert the confusion matrix to a tidy data frame
cm_data <- as.data.frame(cm_test$table)
colnames(cm_data) <- c("Predicted", "Actual", "Freq")

# Plot the confusion matrix using ggplot2
ggplot(cm_data, aes(x = Predicted, y = Actual, fill = Freq)) +
  geom_tile(color = "gray80") +
  geom_text(aes(label = Freq), color = "gray20", size = 3.5) +
  scale_fill_gradient(low = "white", high = "red") +
  theme_minimal() +
  labs(x = "Predicted Class",
       y = "Actual Class") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))



# FIGURE 20: One-vs-Rest ROC Curves for Each Class
# Predict probabilities for the test set (needed for ROC curve)
pred_prob <- predict(m2a, testData, type = "prob")

# Calculate ROC curve for each class (in multiclass, ROC is computed for each class vs all)
roc_curve <- multiclass.roc(testData$Text_General_Code, pred_prob)

# Define your class labels
classes <- levels(testData$Text_General_Code)

# Plotting setup: create colors for each class curve for easy distinction
colors <- rainbow(length(classes))

# Create the empty plot with limits and labels
plot(NULL, xlim = c(0, 1), ylim = c(0, 1), xlab = "1 - Specificity", ylab = "Sensitivity")
abline(a = 0, b = 1, lty = 2, col = "gray")  # Add the diagonal line (random classifier)

# Loop through each class to calculate and plot its one-vs-rest ROC curve
for (i in seq_along(classes)) {
  # Create a binary response for the current class
  binary_response <- ifelse(testData$Text_General_Code == classes[i], 1, 0)
  
  # Calculate the ROC curve for this class vs all others
  roc_single <- roc(binary_response, pred_prob[, i])
  
  # Plot the first ROC curve
  if (i == 1) {
    plot(roc_single, col = colors[i], add = TRUE, legacy.axes = TRUE)
  } else {
    # For subsequent curves, use lines() to add them
    lines(roc_single, col = colors[i], lwd = 2)
  }
}

# Add a legend to the plot
legend("bottomright", legend = classes, col = colors, lwd = 2)


