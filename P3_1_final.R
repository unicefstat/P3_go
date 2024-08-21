# File:    P3_Task1_final
#
# 
# Title:   Population-weighted coverage of health services
# 
# Author:  Anonymous for P3 Assessment
# Date:    2024-08-20


# 1 Load library

library(datasets)
library(readr)
library(readxl)
library(writexl)
library(dplyr)
library(ggplot2)
library(tidyr)
library(reshape2)

# Set Working Directory #####

# For Mac
setwd("~/Documents/GitHub/P3_go/")

# For Windows
# setwd("C:/Users/abc/Documents/GitHub/P3_go/")

# 2 Load data

# Load health coverage data
health_data <- read_excel("GLOBAL_DATAFLOW_2018-2022.xlsx")

# Load population data in Projections sheet
population_data <- read_excel("WPP2022_GEN_F01_DEMOGRAPHIC_INDICATORS_COMPACT_REV1.xlsx", sheet = "Projections")

# Load on-track/off-track classification data
classification_data <- read_excel("On-track and off-track countries.xlsx")

# 3 Clean Data

# 3.1 Clean health data

###################################################################
############# CLEAN & PREPARE Health ##############################
################################################################### data_anc4 [132 x2] data_sab [159 x2]

health_data_clean <- health_data %>%
  select(-`...3`) %>%                                            # Remove the specific column 
  filter(!is.na(`2022`)) %>%                                     # Remove NA in Column `2022`
  rename(                                                        # Rename `New` = `Old`
    `Country` = `Time period`,
    `Health` = `...2`
  )

# Rename row values
health_data_clean$`Health` <- ifelse(health_data_clean$`Health` == "Antenatal care 4+ visits - percentage of women (aged 15-49 years) attended at least four times during pregnancy by any provider", "ANC4",
                                     ifelse(health_data_clean$`Health` == "Skilled birth attendant - percentage of deliveries attended by skilled health personnel", "SAB", health_data_clean$`Health`))

health_data_clean$`2022` <- as.numeric(health_data_clean$`2022`)   # Change column from character to numeric
health_data_clean$`2021` <- as.numeric(health_data_clean$`2021`)
health_data_clean$`2020` <- as.numeric(health_data_clean$`2020`)
health_data_clean$`2019` <- as.numeric(health_data_clean$`2019`)
health_data_clean$`2018` <- as.numeric(health_data_clean$`2018`)

health_data_clean <- health_data_clean %>%
  mutate(`LatestYearValue` = coalesce(`2022`, `2021`, `2020`, `2019`, `2018`))  # Keep only value from latest year

health_data_clean <- health_data_clean %>%
  select(-`2022`, -`2021`, -`2020`, -`2019`, -`2018`)             # Remove years not needed


# Make subset ANC4 and SAB datasets
data_anc4 <- health_data_clean %>%
  filter(`Health` == "ANC4")                                     # Keep only ANC4

data_anc4 <- data_anc4 %>%
  select(-`Health`) %>%                                            # Remove the specific column 
  rename(`ANC4` = `LatestYearValue`)                               # Rename `New` = `Old`

data_sab <- health_data_clean %>%
  filter(`Health` == "SAB")                                     # Keep only SAB

data_sab <- data_sab %>%
  select(-`Health`) %>%                                            # Remove the specific column 
  rename(`SAB` = `LatestYearValue`)                               # Rename `New` = `Old`

###################################################################
###################################################################



# 3.2 Clean population data


###################################################################
############# CLEAN & PREPARE Population ##########################
################################################################### [236 × 4]

population_data_clean <- population_data %>%
  select(`...3`, `...6`, `...11`, `...24`) %>%                   # keep columns
  filter(!is.na(`...6`)) %>%                                     # Remove NA in Column `...6`
  rename(                                                        # Rename `New` = `Old`
    `Country` = `...3`,
    `ISO3` = `...6`,
    `Year` = `...11`,
    `Births (thousands)` = `...24`
  )

population_data_clean <- population_data_clean[-1, ]             # Remove the first row 

population_data_clean$`Births (thousands)` <- as.numeric(population_data_clean$`Births (thousands)`)        # Change column from from character to numeric

population_data_clean <- population_data_clean %>%
  filter(!is.na(`Births (thousands)`))                                     # Remove 79 NA in Column

population_data_clean <- population_data_clean %>%
  filter(`Year` == "2022")                                     # Keep only 2022 [236 x4]
population_data_clean <- population_data_clean %>%
  mutate(`Births (thousands)` = `Births (thousands)` * 1000)     # multiply by 1000 Births
population_data_clean <- population_data_clean %>%
  rename(`Births` = `Births (thousands)`)                 # Rename `NewName1` = `OldName1`


###################################################################
###################################################################



# 3.3 Clean on-track/off-track classification data


###################################################################
############# CLEAN & PREPARE Classification ######################
################################################################### [200 × 3]

classification_data_clean <- classification_data %>%
  rename(                                                        # Rename columns `NewName1` = `OldName1`
    `ISO3` = `ISO3Code`,
    `Country` = `OfficialName`
  )

# Rename row values
classification_data_clean$Status.U5MR <- ifelse(classification_data_clean$Status.U5MR == "Acceleration Needed", "Off Track",
                                                ifelse(classification_data_clean$Status.U5MR == "Achieved", "On Track", classification_data_clean$Status.U5MR))

###################################################################
###################################################################



# 4 Prepare Data


################################################################### class [200 × 3]
############# MERGE ############################################### pop [236 × 4]
################################################################### health [291 × 3] data_anc4 [132 x2] data_sab [159 x2]
################################################################### ANC4 [80 x 4] SAB  [134 x 4]



# Full Outer Join
merged_class_pop <- merge(x = classification_data_clean, y = population_data_clean, by = "ISO3", all = TRUE)

merged_class_pop_2 <- merge(x = merged_class_pop, y = data_anc4, by.x = "Country.y", by.y = "Country", all = TRUE)

merged <- merge(x = merged_class_pop_2, y = data_sab, by.x = "Country.y", by.y = "Country", all = TRUE)

merged_clean <- merged %>%
  filter(!is.na(`ISO3`)) %>%                                  # Remove NA in Column `ISO3`
  select(-`Country.x`)                                        # Remove the column

merged_clean <- merged_clean %>%
  rename(`Country` = `Country.y`)                             # Rename `New` = `Old`


# ANC4 [236 x 4] -> [80 x 4]
calculate_anc4 <- merged_clean %>%
  select(`Country`, `Status.U5MR`, `Births`, `ANC4`) %>%      # keep columns
  filter(!is.na(`Country`)) %>%                               # Remove NA in Column `Country`
  filter(!is.na(`ANC4`))                                      # Remove NA in Column `ANC4`

# SAB [236 x 4] -> [133 x 4]
calculate_sab <- merged_clean %>%
  select(`Country`, `Status.U5MR`, `Births`, `SAB`) %>%       # keep columns
  filter(!is.na(`Country`))  %>%                              # Remove NA in Column `Country`
  filter(!is.na(`SAB`)) %>%                                   # Remove NA in Column `SAB`
  filter(!is.na(`Status.U5MR`))                               # Remove NA in Column `Satus.U5MR`

###################################################################
###################################################################



# 5 Calculate Population-Weighted Coverage


################################################################### ANC4_on  [42 x 5]
############# CALCULATE ########################################### ANC4_off [38 x 5]
################################################################### SAB_on [98 x 5]
################################################################### SAB_off [35 x 5]

anc4_on <- calculate_anc4 %>%
  filter(`Status.U5MR` == "On Track")                         # Keep only On Track [42 x 5]

anc4_off <- calculate_anc4 %>%
  filter(`Status.U5MR` == "Off Track")                        # Keep only Off Track [38 x 5]

sab_on <- calculate_sab %>%
  filter(`Status.U5MR` == "On Track")                         # Keep only On Track [98 x 5]

sab_off <- calculate_sab %>%
  filter(`Status.U5MR` == "Off Track")                        # Keep only Off Track [35 x 5]


wc_anc4_on <- round(sum(anc4_on$ANC4 * anc4_on$Births) / sum(anc4_on$Births))
wc_anc4_off <- round(sum(anc4_off$ANC4 * anc4_off$Births) / sum(anc4_off$Births))
wc_sab_on <- round(sum(sab_on$SAB * sab_on$Births) / sum(sab_on$Births))
wc_sab_off <- round(sum(sab_off$SAB * sab_off$Births) / sum(sab_off$Births))

wc_anc4 <- round(sum(calculate_anc4$ANC4 * calculate_anc4$Births) / sum(calculate_anc4$Births))
wc_sab <- round(sum(calculate_sab$SAB * calculate_sab$Births) / sum(calculate_sab$Births))


###################################################################
###################################################################




# 6 Visualize results


###################################################################
############# VISUALIZE ###########################################
###################################################################
###################################################################


wc_data <- data.frame(
  Indicator = c("ANC4", "ANC4", "ANC4", "SAB", "SAB", "SAB"),
  Status = c("Overall","On-Track", "Off-Track", "Overall", "On-Track", "Off-Track"),
  Coverage = c(wc_anc4, wc_anc4_on, wc_anc4_off, wc_sab, wc_sab_on, wc_sab_off)
)


ggplot(wc_data, aes(x = Indicator, y = Coverage, fill = Status)) +
  geom_bar(stat = "identity", position = "dodge") +
  geom_text(aes(label = Coverage), 
            position = position_dodge(width = 0.9), 
            vjust = 2.5, hjust = 0.5, color = "white") +
  labs(title = "Population-Weighted Coverage of Health Services",
       y = "Weighted Coverage (%)",
       x = "Health Service Indicator",
       fill = "Country U5MR Status") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))

###################################################################
###################################################################

# 7 Interpret

