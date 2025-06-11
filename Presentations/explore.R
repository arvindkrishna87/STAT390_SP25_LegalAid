#load packages-------------
library(readxl)
library(dplyr)
library(stringr)
library(purrr)
library(lubridate)

#upload data----------------
file_path <- "~/Desktop/stat 390/Call journey data/"
file_list <- list.files(path = file_path, pattern = "\\.xlsx$", full.names = TRUE)

#create the month/year column--------------
extract_month_year <- function(filename) {
  date_part <- str_extract(filename, "\\(\\d{2}-\\d{2}-\\d{2}")
  month_num <- str_sub(date_part, 2, 3)
  year_suffix <- str_sub(date_part, -2)
  full_year <- paste0("20", year_suffix)
  month_label <- month.name[as.integer(month_num)]
  return(paste(month_label, full_year))
}

read_and_tag <- function(file) {
  df <- read_excel(file, skip = 2) 
  df$month_year <- extract_month_year(file)
  return(df)
}
combined_data <- map_dfr(file_list, read_and_tag)

combined_data <- combined_data %>%
  mutate(
    activity_datetime = ymd_hms(`Activity Start Timestamp`),
    weekday_number = wday(activity_datetime),
    Weekend_Weekday = ifelse(weekday_number %in% c(1, 7), "Weekend", "Weekday")
  )

#save the csv-----------------
write.csv(combined_data, file = "~/Desktop/stat 390/Call journey data/combined_call_data.csv", row.names = FALSE)

#count distinct for num contact session IDs for each activity name
combined_data %>%
  group_by(`Activity Name`) %>%
  summarise(contact_session_count = n_distinct(`Contact Session ID`)) %>%
  arrange(desc(contact_session_count)) 


weekend_activities <- combined_data %>%
  filter(Weekend_Weekday == "Weekend") %>%
  select(`Activity Name`) %>%   # Or use the appropriate activity column
  distinct()

#count distinct
table <- combined_data %>%
  filter(`Activity Name` == "FamilyMenu") %>%
  group_by(`Queue Name`) %>%
  summarise(
    Distinct_Contact_Sessions = n_distinct(`Contact Session ID`)
  ) %>%
  arrange(desc(Distinct_Contact_Sessions))

print(table)


combined_data %>% 
  filter(`Activity Name` == "FamilyMenu") %>%
  summarise(Total_Contact_Sessions = n_distinct(`Contact Session ID`))




