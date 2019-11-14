library(tidyverse)
incidents <- feather::read_feather("incidents.feather")
drivers <- feather::read_feather("drivers.feather")
non_motorists <- feather::read_feather("non_motorists.feather")

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
names(nm_join)[2:length(nm_join)] <- paste0("nm_",names(nm_join)[2:length(nm_join)])

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

names(d_join)[2:length(d_join)] <- paste0("d_",names(d_join)[2:length(d_join)])

combo_table <- incidents %>%
  full_join(d_join, by = "report_number") %>%
  full_join(nm_join, by = "report_number") %>%
  mutate_at(vars(incident_hour:nm_injury_severity_fatal_injury), function(x) if_else(is.na(x), 0, as.double(x)))

feather::write_feather(combo_table, "combo.feather")