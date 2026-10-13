install.packages("janitor")
library(janitor)
library(tidyverse)
library(scales)

Rodents <- read_csv("311_rodent_calls.csv")
all_districts <- read_csv("nyc_council_context.csv")

unique(Rodents$`Resolution Description`)

#create groups based on resolution description

Confirmed <- c("The Department of Health and Mental Hygiene found violations on the property you reported. Follow-up inspections will be scheduled.", 
               "An inspection of the property was conducted and it passed with minor violations found.", 
               "An inspection of this property was conducted and violations were identified. The owner is correcting the condition. If the condition persists, please submit another complaint to 311.",
               "An investigation was performed on this property and the property owner was provided with information and guidance on eliminating rodents or rodent conditions on their property. If the condition persists, please submit another complaint to 311.")

Not_Confirmed <- c("The Department of Health and Mental Hygiene inspected the property you reported and did not find any violations at the time of the inspection. The property passed inspection.")

Indeterminate <- c("The Department of Health and Mental Hygiene tried to inspect the property you reported but could not access the site. If you have information that will help the inspector access the site, please go online to nyc.gov/311 or call 311 and refile your complaint with contact information.",
                   "This service request was closed because the Department of Health and Mental Hygiene received an earlier complaint about the same location.  You can find inspection results for this address by going to the online Rat Portal at www.nyc.gov/rats.", 
                   "The Department of Health and Mental Hygiene could not locate the address provided. Please go online to nyc.gov/311 or call 311 and file another request and provide the complete street address of the builiding. If your complaint was about a vacant lot, abandoned building ,street, or in a sewer, provide the nearest street address or intersection.", 
                   "The Department of Health and Mental Hygiene closed this complaint and referred it to another agency because it was not within its jurisdiction.",
                   "The Department of Health and Mental Hygiene (DOHMH) actioned and closed your service request. No further updates will be available.", 
                   "The Department of Health and Mental Hygiene could not conduct an inspection at the property you reported because dangerous conditions at that site make it unsafe for inspectors to attempt access.", NA)

#create a new column based on these groups


Rodents <- Rodents %>% 
  mutate(
    Confirmation_Status = case_when(
      `Resolution Description` %in% Confirmed ~ "Confirmed",
      `Resolution Description` %in% Not_Confirmed ~ "Not Confirmed",
      `Resolution Description` %in% Indeterminate ~ "Indeterminate",
    ))

count(Rodents, Confirmation_Status) #total number of each type of case

simpleRodents <- Rodents %>% 
  group_by(`Council District`, Confirmation_Status) %>% 
  summarize(Totals = n(), .groups = "drop") %>% 
  pivot_wider(
    names_from = Confirmation_Status,
    values_from = Totals,
    values_fill = 0)%>%
  mutate(Total = rowSums(across(where(is.numeric)))) %>% 
  filter(!is.na(`Council District`)) %>% 
  clean_names()


all_districts$council_district <- as.character(sprintf("%02d" ,as.numeric(all_districts$council_district_id)))

DistrictRodents <- inner_join(all_districts, simpleRodents)

DistrictRodents<- DistrictRodents %>% 
  mutate(
    Reporting_Rate = (total/population)*1000,
    Confirmation_Issue_Rate= (confirmed/population)*1000) 



pluto <- read_csv("pluto_25v4.csv")

library(dplyr)

pluto %>%
  count(landuse) %>%
  mutate(percent = n / sum(n))


landuse_labels <- c(
  "01" = "1-2 Family Residential",
  "02" = "Multi-Family Walk-Up",
  "03" = "Multi-Family Elevator",
  "04" = "Mixed Residential & Commercial",
  "05" = "Commercial & Office",
  "06" = "Industrial & Manufacturing",
  "07" = "Transportation & Utility",
  "08" = "Public Facilities & Institutions",
  "09" = "Open Space & Recreation",
  "10" = "Parking Facilities",
  "11" = "Vacant Land"
)

pluto <- pluto %>% 
  count(landuse) %>%
  mutate(
    percent = n / sum(n),
    landuse_name = landuse_labels[landuse]
  ) %>%
  arrange(desc(n))

Rodents <- Rodents %>%
  mutate(pluto_landuse = case_when(
    
    `Location Type` == "1-2 Family Dwelling" ~ 01,
    
    `Location Type` %in% c(
      "3+ Family Apt. Building",
      "Single Room Occupancy (SRO)"
    ) ~ 02,
    
    `Location Type` %in% c(
      "3+ Family Mixed Use Building",
      "1-2 Family Mixed Use Building"
    ) ~ 04,
    
    `Location Type` %in% c(
      "Office Building",
      "Commercial Building"
    ) ~ 05,
    
    `Location Type` %in% c(
      "Parking Lot or Garage",
      "Parking Lot/Garage"
    ) ~ 10,
    
    `Location Type` %in% c(
      "Government Building",
      "Hospital",
      "School",
      "School/Pre-School",
      "Day Care or Nursery",
      "Day Care/Nursery",
      "Summer Camp"
    ) ~ 08,
    
    `Location Type` == "Public Garden" ~ 09,
    
    `Location Type` %in% c(
      "Vacant Lot",
      "Vacant Building"
    ) ~ 11,
    
    `Location Type` %in% c(
      "Street",
      "Sidewalk",
      "Public Stairs",
      "Catch Basin or Sewer",
      "Catch Basin/Sewer",
      "Construction Site"
    ) ~ 07,
    
    TRUE ~ NA_real_
  ))


pluto %>% 
  group_by(landuse) %>% 
  summarize(
    count = n()
  )


calls_summary <- Rodents%>%
  group_by(pluto_landuse) %>%
  summarize(num_calls = n()) %>%
  mutate(
    landuse = sprintf("%02d", as.integer(pluto_landuse))
  )

conf_summary <- Rodents%>%
  group_by(pluto_landuse) %>%
  summarize(
    num_conf = sum(Confirmation_Status=="Confirmed")
    ) %>%   
  mutate(
    landuse = sprintf("%02d", as.integer(pluto_landuse))
  )

all_houses <- inner_join(pluto, calls_summary) %>% 
  mutate(
    call_rate_pct= (num_calls/n)*100,
    landuse_name= factor(landuse_name)
  )


all_houses <- inner_join(all_houses, conf_summary) %>% 
  mutate(
    conf_pct= (num_conf/n)
  )

all_houses %>% 
  filter(landuse != "07") %>% 
  ggplot(aes(x=fct_rev(fct_reorder(landuse_name, conf_pct)), y=conf_pct)) +     #conf percentage by house type
  geom_col(fill= "darkred")+
  labs(
    title = "Confirmation Percentage by Building",
    x= "Location Type",
    y= "Confirmation Percentage",
    caption= "https://www.nyc.gov/content/planning/pages/resources/datasets/mappluto-pluto-change",
    subtitle = "Confirmed Calls per Type/Total # of Lots per Type"
  )+
  scale_y_continuous(labels = percent_format())+
  theme_classic() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))



all_houses %>% 
  filter(landuse != "07") %>% 
  ggplot(aes(x=fct_rev(fct_reorder(landuse_name, call_rate_pct)), y=call_rate_pct/100)) +     #call percentage by house type
  geom_col(fill= "darkred")+
  labs(
    title = "Call Percentage by Building",
    x= "Location Type",
    y= "Call Percentage",
    caption= "https://www.nyc.gov/content/planning/pages/resources/datasets/mappluto-pluto-change",
    subtitle = "Calls per Type/Total # of Lots per Type"
  )+
  scale_y_continuous(labels = percent_format())+
  theme_classic() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
