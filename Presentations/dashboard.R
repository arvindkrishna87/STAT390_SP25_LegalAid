# CODE FOR DASHBOARD ----

# filtering all calls to match CAR date range ----

library(tidyverse) 
all_calls <- read_csv("all_calls.csv") # this is a cleaned csv (see previous uploaded qmds)
car <- read_csv("CAR_combined.csv") %>%
  janitor::clean_names()

# find car data range
car$activity_start_timestamp_mdy <- as.POSIXct(car$activity_start_timestamp)
all_calls$start_time_mdy <- as.POSIXct(all_calls$start_time)

car_date_min <- min(car$activity_start_timestamp_mdy, na.rm = TRUE)
car_date_max <- max(car$activity_start_timestamp_mdy, na.rm = TRUE)
cat("car date range:", format(car_date_min, "%m/%d"), "-", format(car_date_max, "%m/%d"), "\n")

all_calls_date_min <- min(all_calls$start_time_mdy, na.rm = TRUE)
all_calls_date_max <- max(all_calls$start_time_mdy, na.rm = TRUE)
cat("all_calls date range:", format(all_calls_date_min, "%m/%d"), "-", format(all_calls_date_max, "%m/%d"), "\n")

all_calls_filtered <- all_calls %>% # use this all calls df going forward
  filter(start_time >= car_date_min & start_time <= car_date_max)

# prepare data for tableau ----

## assign leg numbers ----
legs_df <- all_calls_filtered %>%
  distinct(correlation_id, start_time, called_number, .keep_all = TRUE) %>%
  arrange(correlation_id, start_time) %>%
  group_by(correlation_id) %>%
  mutate(leg_number = row_number()) %>%
  ungroup()

## get external inbound ids: TERMINATING, CallTower ----
inbound_ids <- legs_df %>%
  group_by(correlation_id) %>%
  slice(1) %>%
  filter(direction == "TERMINATING", pstn_vendor_name == "CallTower") %>%
  pull(correlation_id)

## create a new df and assign "call role" to external inbound calls ----
role_df <- legs_df %>%
  filter(correlation_id %in% inbound_ids) %>%
  mutate(call_role = case_when( # direct vs. transfer
    leg_number == 1 ~ "Direct",
    leg_number > 1 & is.na(pstn_vendor_name) ~ "Transfer"
  )) %>%
  filter(!is.na(call_role))

## add labels for main number and special intake lines ----
role_df <- role_df %>%
  mutate(intake_line_name = case_when(
    called_number %in% c(
      "13123411070","13125068646", "13125068647", "13122296080", "13123478342"
    ) ~ "Main Number",
    called_number == "13124235938" ~ "legalclinics",
    called_number == "18884018200" ~ "Nursing Home Ombudsman",
    called_number == "13122296014" ~ "Markham Eviction Help Desk",
    called_number %in% c("18004459025", "13124312299") ~ "Migrant Legal Assistance Program / Farmworker",
    called_number %in% c("13123478347","18882652188") ~ "A2J Immigration",
    called_number == "13122296344" ~ "Bankruptcy Helpdesk VM",
    called_number == "13124235904" ~ "Austin Intake VM",
    called_number == "13124235900" ~ "CLASP VM",
    called_number == "13122296071" ~ "Criminal Records",
    called_number == "13123478392" ~ "Education Law Referrals VM",
    called_number == "13124235909" ~ "Fair Housing Intake VM",
    called_number == "13123478309" ~ "HIV Intake VM",
    called_number == "13122296072" ~ "JEHD (Juvenile Expungement Help Desk)",
    called_number == "13124312101" ~ "OP Appeals Project",
    called_number == "13123478340" ~ "Veterans Rights Project VM",
    called_number == "13122296073" ~ "Trafficking Survivors Assistance Project",
    TRUE ~ "Other"
    ))

role_df <- role_df %>%
  mutate(
    hour = hour(start_time),
    hour_of_day = case_when(
      hour == 0  ~ "12:00 AM - 12:59 AM",
      hour == 1  ~ "1:00 AM - 1:59 AM",
      hour == 2  ~ "2:00 AM - 2:59 AM",
      hour == 3  ~ "3:00 AM - 3:59 AM",
      hour == 4  ~ "4:00 AM - 4:59 AM",
      hour == 5  ~ "5:00 AM - 5:59 AM",
      hour == 6  ~ "6:00 AM - 6:59 AM",
      hour == 7  ~ "7:00 AM - 7:59 AM",
      hour == 8  ~ "8:00 AM - 8:59 AM",
      hour == 9  ~ "9:00 AM - 9:59 AM",
      hour == 10 ~ "10:00 AM - 10:59 AM",
      hour == 11 ~ "11:00 AM - 11:59 AM",
      hour == 12 ~ "12:00 PM - 12:59 PM",
      hour == 13 ~ "1:00 PM - 1:59 PM",
      hour == 14 ~ "2:00 PM - 2:59 PM",
      hour == 15 ~ "3:00 PM - 3:59 PM",
      hour == 16 ~ "4:00 PM - 4:59 PM",
      hour == 17 ~ "5:00 PM - 5:59 PM",
      hour == 18 ~ "6:00 PM - 6:59 PM",
      hour == 19 ~ "7:00 PM - 7:59 PM",
      hour == 20 ~ "8:00 PM - 8:59 PM",
      hour == 21 ~ "9:00 PM - 9:59 PM",
      hour == 22 ~ "10:00 PM - 10:59 PM",
      hour == 23 ~ "11:00 PM - 11:59 PM"
    ),
    hour_of_day = factor(hour_of_day, levels = c(
      "12:00 AM - 12:59 AM", "1:00 AM - 1:59 AM", "2:00 AM - 2:59 AM", "3:00 AM - 3:59 AM",
      "4:00 AM - 4:59 AM", "5:00 AM - 5:59 AM", "6:00 AM - 6:59 AM", "7:00 AM - 7:59 AM",
      "8:00 AM - 8:59 AM", "9:00 AM - 9:59 AM", "10:00 AM - 10:59 AM", "11:00 AM - 11:59 AM",
      "12:00 PM - 12:59 PM", "1:00 PM - 1:59 PM", "2:00 PM - 2:59 PM", "3:00 PM - 3:59 PM",
      "4:00 PM - 4:59 PM", "5:00 PM - 5:59 PM", "6:00 PM - 6:59 PM", "7:00 PM - 7:59 PM",
      "8:00 PM - 8:59 PM", "9:00 PM - 9:59 PM", "10:00 PM - 10:59 PM", "11:00 PM - 11:59 PM"
    )),
    business_hours = ifelse(hour %in% c(8,9,10,11,12,13,14,15,16), "During Business Hours", "Outside Business Hours"),
    month = factor(month, levels = c("April 2024", "May 2024", "June 2024", "July 2024", "August 2024", "September 2024", "October 2024", "November 2024", "December 2024", "January 2025", "February 2025", "March 2025")),
    day_of_week = weekdays(start_time),
    weekend = ifelse(day_of_week %in% c("Sunday","Saturday"), "Weekend", "Weekday")
  )

# output df for tableau ----
write.csv(role_df, "call_roles_by_number.csv", row.names = FALSE)
