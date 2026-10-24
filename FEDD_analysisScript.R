#####libraries####
# Check for and install 'readxl' if not already installed
if (!require(readxl, quietly = TRUE)) {
  install.packages("readxl")
  library(readxl)
}
# Check for and install 'ggplot2' if not already installed
if (!require(ggplot2, quietly = TRUE)) {
  install.packages("ggplot2")
  library(ggplot2)
}
# Check for and install 'dplyr' if not already installed
if (!require(dplyr, quietly = TRUE)) {
  install.packages("dplyr")
  library(dplyr)
}
# Check for and install 'tidyr' if not already installed
if (!require(tidyr, quietly = TRUE)) {
  install.packages("tidyr")
  library(tidyr)
}
library(readxl)
library(ggplot2)
library(dplyr)
library(tidyr)
# Set the path to your Excel file
file_path <- "C:/Users/ayush/OneDrive/Documents/TTC_4.0_Speed_55.xlsx"

####Plotting overview####
# Read the Excel file into a dataframe named 'data'
data <- read_excel(file_path)
# Convert 'Time' from seconds since the epoch to POSIXct date-time objects
#data$Time <- as.POSIXct(data$Time, origin = "1970-01-01", tz = "UTC")
# Filter the data for the three vehicle IDs
filtered_data <- data %>% 
  filter(customID %in% c(1021, 1022, 1024))
# Plotting
ggplot_object <- ggplot(filtered_data, aes(x = Time, y = `position X`, color = as.factor(customID))) +
  geom_line() +
  labs(title = "Position Over Time by Vehicle",
       x = "Time",
       y = "position X",
       color = "Vehicle ID") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))  # Rotate x-axis labels for better visibility
# Print the plot
print(ggplot_object)

# Plotting position X vs position Z
ggplot_object <- ggplot(filtered_data, aes(x = `position X`, y = `position Z`, color = as.factor(customID))) +
  geom_line() +
  labs(title = "Vehicle Position: X vs Z Coordinates",
       x = "Position X",
       y = "Position Z",
       color = "Vehicle ID") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))  # Adjust for better visibility if needed
# Print the plot
print(ggplot_object)


####Refine Dataframe####
# Create the new dataframe 'data2'
data2 <- data %>%
  filter(customID %in% c(1021, 1022), `position X` > 6500)
# Split the data for each vehicle
data_1001 <- data2 %>% 
  filter(customID == 1021) %>% 
  select(Time, position_X = `position X`, speedInMetresPerSecond) %>%
  rename(x1 = position_X, v1 = speedInMetresPerSecond)
data_1002 <- data2 %>% 
  filter(customID == 1022) %>% 
  select(Time, position_X = `position X`, speedInMetresPerSecond) %>%
  rename(x2 = position_X, v2 = speedInMetresPerSecond)
# Perform an inner join on the 'Time' column to ensure matching timestamps
data3 <- inner_join(data_1001, data_1002, by = "Time")
#CALCULATIONS
# Add new columns to data3 for the differences in position and speed, and compute the TTC
data3 <- data3 %>%
  mutate(del.x = x1 - x2,   # Difference in position: x2 - x1
         del.v = v1 - v2,   # Difference in speed: v1 - v2
         TTC = ifelse(del.v != 0, del.x / del.v, NA))  # Time to Collision, avoiding division by zero



#####TTC plotting####
# Plotting Time vs. TTC
ggplot(data3, aes(x = Time, y = TTC)) +
  geom_line() +  # Use a line plot to show changes over time
  labs(title = "Time to Collision (TTC) Over Time",
       x = "Time",
       y = "Time to Collision (TTC)") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +  # Rotate x-axis labels for better visibility
  ylim(0, 100)  # Set the y-axis limits to only show from 0 to 100


# Find the minimum positive TTC value and corresponding Time
min_ttc_point <- data3 %>% 
  filter(!is.na(TTC) & TTC>0 & Time<60) %>%  # Exclude NA and negative values
  filter(TTC == min(TTC, na.rm = TRUE)) %>%  # Get the lowest positive TTC
  select(Time, TTC)
# Print the lowest positive TTC point
print(min_ttc_point)



####Headway#####
# Filter the data2 for vehicle IDs 1001 and 1002 (this step might be redundant if data2 only contains these two IDs already)
data2_filtered <- data2 %>% 
  filter(customID %in% c(1021, 1022))
# Plotting Time vs position X
ggplot(data2_filtered, aes(x = Time, y = `position X`, color = as.factor(customID))) +
  geom_line() +  # Use line plot to show the trends over time for each vehicle
  labs(title = "Time vs Position X for Vehicles 1021 and 1022",
       x = "Time",
       y = "Position X",
       color = "Vehicle ID") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))  # Rotate x-axis labels for better visibility

####Printing MIN HEADWAY####
# Assuming data2 is already filtered to include only vehicles 1001 and 1002
data_1021 <- data2 %>% filter(customID == 1021) %>% select(Time, `position X`)
data_1024 <- data2 %>% filter(customID == 1024) %>% select(Time, `position X`)
# Assuming Time is numerical and represents seconds directly
# Create a fine-grained sequence of position X values covering the range of both vehicles
x_range <- seq(min(c(data_1021$`position X`, data_1024$`position X`)), max(c(data_1021$`position X`, data_1024$`position X`)), by = 1)
# Interpolate Time values for both vehicles over this range
time_1021 <- approx(data_1021$`position X`, data_1021$Time, x_range)$y
time_1024 <- approx(data_1024$`position X`, data_1024$Time, x_range)$y
# Calculate time differences at these interpolated positions
time_differences <- abs(time_1021 - time_1024)
# Find the minimum time difference
min_headway <- min(time_differences, na.rm = TRUE)
# Output the minimum headway
print(min_headway)

