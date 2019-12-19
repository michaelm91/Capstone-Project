library(tidyverse)
library(lubridate)
library(fastDummies)
library(RSocrata)

# Read data from local copy
# TODO remove this for final product
# setwd("C:/datasets/capstone/")
# incidents <- read_csv("C:/datasets/DataMontgomery/Crash_Reporting_Incidents_Data.csv", guess_max = 50000)
# drivers <- read_csv("C:/datasets/DataMontgomery/Crash_Reporting_Drivers_Data.csv", guess_max = 50000)
# non_motorists <- read_csv("C:/datasets/DataMontgomery/Crash_Reporting_Non_Motorists_Data.csv", guess_max = 50000)

# Pull from web. This will be used for final product
key <- read_file("key.txt")
incidents <- read.socrata("https://data.montgomerycountymd.gov/resource/bhju-22kf.csv", key)
drivers <- read.socrata("https://data.montgomerycountymd.gov/resource/mmzv-x632.csv", key)
non_motorists <- read.socrata("https://data.montgomerycountymd.gov/resource/n7fk-dce5.csv", key)

# furthest north/south/east/west latitudes and longitudes in Montgomery County
# will be used for detecting points in the data that are not within the county
moco_north <- 39.35
moco_south <- 38.93
moco_east <- -76.89
moco_west <- -77.53

problem_location_data <- function(df) {
  df %>%
    mutate(not_in_county = (longitude < moco_west) | (longitude > moco_east) | (latitude > moco_north) | (latitude < moco_south))
}


# Section 1: helper functions

# converts a string to snake case
to_snake_case <- function(x) {
  tolower(x) %>%
    str_replace_all("([:punct:]|[:blank:])+", "_")
}

# converts na characters in a dataframe to a string that reads N/A
force_na_characters <- function(df) {
  df %>%
    mutate_if(is.character, function(x) if_else(is.na(x), "N/A", x)) %>%
    mutate_if(is.character, function(x) if_else(x == "", "N/A", x))
}

# function for extracting various date and time info
date_process <- function(df) {
  df %>%
    mutate(
      incident_date = as.Date(crash_date_time),
      incident_hour = hour(crash_date_time),
      incident_minute = minute(crash_date_time),
      incident_month = month(crash_date_time),
      incident_day = day(crash_date_time),
      incident_weekday = wday(crash_date_time),
      incident_year = year(crash_date_time),
      on_weekend = incident_weekday %in% c(1, 7)
    )
}

# Section 2: basic cleanup on all tables

# convert the column names of the crash data frames to snake case
names(incidents) <- names(incidents) %>% to_snake_case()
names(drivers) <- names(drivers) %>% to_snake_case()
names(non_motorists) <- names(non_motorists) %>% to_snake_case()

# make copies of tables when they are read in
# useful to check what is in original data
incidents_original <- incidents
drivers_original <- drivers
non_motorists_original <- non_motorists

# Section 3: incidents cleanup

incidents_dummy_cols <- c(
  "at_fault", "weather", "road_division", "hit_run",
  "nontraffic", "driver_substance_abuse", "acrs_report_type",
  "collision_type", "traffic_control", "lane_type", "road_grade", "light"
)

incidents <- incidents %>%
  date_process() %>%
  problem_location_data() %>%
  force_na_characters() %>%
  mutate(weather = str_remove_all(weather, ",")) %>%
  dummy_cols(select_columns = incidents_dummy_cols, split = ",") %>%
  mutate(
    surface_problem = surface_condition %in% c("WET", "WATER(STANDING/MOVING)", "SNOW", "SLUSH", "ICE", "OIL"),
    condidtion_problem = !(road_condition %in% c("N/A", "NO DEFECTS", "UNKNOWN")),
    parking_off_road = str_detect(tolower(off_road_description), "parking"),
    # create various variables for substance abuse
    no_substance_detected = driver_substance_abuse == "NONE DETECTED",
    any_substance_contributed = str_detect(driver_substance_abuse, "CONTRIBUTED"),
    any_substance_detected = str_detect(driver_substance_abuse, "DETECTED"),
    # distance variable with consistent units
    distance_in_feet = case_when(
      distance_unit == "MILE" ~ distance * 5280,
      distance_unit == "UNKNOWN" & distance < 1 ~ distance * 5280,
      TRUE ~ distance
    ),
    daylight = light == "DAYLIGHT",
    dark = str_detect(light, "DARK"),
    no_lights = str_detect(light, "NO LIGHT")
  ) %>%
  select(-c("distance", "distance_unit")) %>%
  mutate_if(is.logical, as.integer)

# remove NA dummy columns
incidents_na_dummys <- incidents_dummy_cols %>%
  paste0("_N/A") %>%
  .[. %in% names(incidents)]

incidents <- incidents %>% select(-incidents_na_dummys)

# convert dummy columns to snake case
names(incidents) <- names(incidents) %>% to_snake_case()

incidents_analysis <- incidents %>% select(-incidents_dummy_cols)

# Section 4: drivers cleanup

drivers_dummy_cols <- c(
  "weather", "driver_substance_abuse", "acrs_report_type",
  "collision_type", "traffic_control", "surface_condition",
  "light", "driver_at_fault", "injury_severity",
  "driver_distracted_by", "vehicle_damage_extent", "vehicle_body_type",
  "vehicle_movement", "vehicle_going_dir", "vehicle_continuing_dir", "equipment_problems", "parked_vehicle"
)

drivers <- drivers %>%
  date_process() %>%
  problem_location_data() %>%
  force_na_characters() %>%
  mutate(weather = str_remove_all(weather, ",")) %>%
  dummy_cols(select_columns = drivers_dummy_cols, split = ",") %>%
  mutate(
    surface_problem = surface_condition %in% c("WET", "WATER(STANDING/MOVING)", "SNOW", "SLUSH", "ICE", "OIL"),
    parking_off_road = str_detect(tolower(off_road_description), "parking")
  ) %>%
  mutate_if(is.logical, as.integer)

# remove NA dummy columns
drivers_na_dummys <- drivers_dummy_cols %>%
  paste0("_N/A") %>%
  .[. %in% names(drivers)]
drivers <- drivers %>% select(-drivers_na_dummys)

# convert dummy columns to snake case
names(drivers) <- names(drivers) %>% to_snake_case()

drivers_analysis <- drivers %>% select(-drivers_dummy_cols)

# Section 5: Non Motorists cleanup

non_motorists_dummy_cols <- c(
  "acrs_report_type", "related_non_motorist", "collision_type", "weather", "surface_condition", "light",
  "traffic_control", "driver_substance_abuse", "non_motorist_substance_abuse", "pedestrian_type", "pedestrian_movement",
  "pedestrian_actions", "pedestrian_location", "pedestrian_obeyed_traffic_signal", "pedestrian_visibility",
  "at_fault", "injury_severity"
)

non_motorists <- non_motorists %>%
  date_process() %>%
  problem_location_data() %>%
  force_na_characters() %>%
  mutate(weather = str_remove_all(weather, ",")) %>%
  dummy_cols(select_columns = non_motorists_dummy_cols, split = ",") %>%
  mutate(
    surface_problem = surface_condition %in% c("WET", "WATER(STANDING/MOVING)", "SNOW", "SLUSH", "ICE", "OIL"),
  ) %>%
  mutate_if(is.logical, as.integer)

# remove NA dummy columns

non_motorists_na_dummys <- non_motorists_dummy_cols %>%
  paste0("_N/A") %>%
  .[. %in% names(non_motorists)]

non_motorists <- non_motorists %>% select(-non_motorists_na_dummys)

# convert dummy columns to snake case
names(non_motorists) <- names(non_motorists) %>% to_snake_case()

non_motorists_analysis <- non_motorists %>% select(-non_motorists_dummy_cols)

# created cleaned csv files

write_csv(incidents, "incidents_clean.csv")
write_csv(drivers, "drivers_clean.csv")
write_csv(non_motorists, "non_motorists_clean.csv")

# write to feather files for use in python

feather::write_feather(incidents, "incidents.feather")
feather::write_feather(drivers, "drivers.feather")
feather::write_feather(non_motorists, "non_motorists.feather")

nm_join <- non_motorists %>%
  group_by(report_number) %>%
  summarise_at(
    vars(
      c(
        related_non_motorist_pedestrian:related_non_motorist_in_animal_drawn_veh,
        non_motorist_substance_abuse_none_detected:injury_severity_fatal_injury
      )
    ),
    sum
  )
names(nm_join)[2:length(nm_join)] <- paste0("nm_", names(nm_join)[2:length(nm_join)])

d_join <- drivers %>%
  group_by(report_number) %>%
  summarise_at(
    vars(
      c(
        driver_substance_abuse_none_detected:driver_substance_abuse_combination_contributed,
        driver_at_fault_yes:parked_vehicle_yes
      )
    ),
    sum
  )

names(d_join)[2:length(d_join)] <- paste0("d_", names(d_join)[2:length(d_join)])

combo_table <- incidents %>%
  full_join(d_join, by = "report_number") %>%
  full_join(nm_join, by = "report_number") %>%
  mutate_at(vars(incident_hour:nm_injury_severity_fatal_injury), function(x) if_else(is.na(x), 0, as.double(x)))

feather::write_feather(combo_table, "combo.feather")
