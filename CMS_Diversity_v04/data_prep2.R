library(tidyverse)
library(shiny)
library(shinythemes)
library(rsconnect)

data <- read_csv("SchoolData_2008_2020.csv")

# # Obtain a list of schools that existed in all 13 years
# schools_13yrs <- data %>% count(School_Consistant) %>% filter(n == 13)
# 
# 
# # Add a flag that indicates if a school existed in each of the 13 years
# df_13yrs <- data %>%
#   mutate(existed_all_13yrs = ifelse(School_Consistant %in% schools_13yrs$School_Consistant,
#                                     1, 0))


# -------------------------------------------------------------------------------------
# Create a dataframe containing the percentages of each group 
# across all students in the district each year
district_makeup <- data %>%
  group_by(Year) %>%
  summarise(district_total_n_students = sum(Total),
            district_prcnt_Indian = 100 * sum(Indian_Pop) / sum(Total),
            district_prcnt_Asian = 100 * sum(Asian_Pop) / sum(Total),
            district_prcnt_Hispanic = 100 * sum(Hispanic_Pop) / sum(Total),
            district_prcnt_Black = 100 * sum(Black_Pop) / sum(Total),
            district_prcnt_White = 100 * sum(White_Pop) / sum(Total),
            district_prcnt_Pacific = 100 * sum(Pacific_Pop) / sum(Total),
            district_prcnt_Mixed = 100 * sum(Mixed_Pop) / sum(Total),
            district_n_Indian = sum(Indian_Pop),
            district_n_Asian = sum(Asian_Pop),
            district_n_Hispanic = sum(Hispanic_Pop),
            district_n_Black = sum(Black_Pop),
            district_n_White = sum(White_Pop),
            district_n_Pacific = sum(Pacific_Pop),
            district_n_Mixed = sum(Mixed_Pop))


tidydistrict_prcnts <- district_makeup %>%
  mutate(Year = as.integer(Year),
         Black = district_prcnt_Black,
         White = district_prcnt_White,
         Hispanic = district_prcnt_Hispanic,
         Asian = district_prcnt_Asian,
         Indian = district_prcnt_Indian,
         Pac_Islander = district_prcnt_Pacific,
         Two_or_More = district_prcnt_Mixed) %>%
  select(Year,
         Black,
         White,
         Hispanic,
         Asian,
         Indian,
         Pac_Islander,
         Two_or_More) %>%
  gather(key="Group", value = "Percent_of_District", -Year) %>%
  mutate(Group = factor(Group,
                        levels = c("Black", "White", "Hispanic",
                                   "Asian", "Indian", "Pac_Islander",
                                   "Two_or_More")))

tidydistrict_counts <- district_makeup %>%
  mutate(Year = as.integer(Year),
         Black = district_n_Black,
         White = district_n_White,
         Hispanic = district_n_Hispanic,
         Asian = district_n_Asian,
         Indian = district_n_Indian,
         Pac_Islander = district_n_Pacific,
         Two_or_More = district_n_Mixed) %>%
  select(Year,
         Black,
         White,
         Hispanic,
         Asian,
         Indian,
         Pac_Islander,
         Two_or_More) %>%
  gather(key="Group", value = "Count", -Year) %>%
  mutate(Group = factor(Group,
                        levels = c("Black", "White", "Hispanic",
                                   "Asian", "Indian", "Pac_Islander",
                                   "Two_or_More")))


# -------------------------------------------------------------------------------------


# Gather the groups into a single column with their percents in a separate column
# to aid in bar plot creation
tidydata <- data %>%
  mutate(Year = as.integer(Year), 
         School = School_Consistant,
         Indian = Indian_Prcnt,
         Asian = Asian_Prcnt,
         Hispanic = Hispanic_Prcnt,
         Black = Black_Prcnt,
         White = White_Prcnt,
         Pac_Islander = Pacific_Prcnt,
         Two_or_More = Mixed_Prcnt) %>%
  select(Year,
         School,
         Type,
         LearningCommunity,
         lat,
         lon,
         Black,
         White,
         Hispanic,
         Asian,
         Indian,
         Pac_Islander,
         Two_or_More) %>%
  gather(key = "Group", 
         value = "Percentage", 
         -c(Year, School, Type, LearningCommunity, lat,lon)) %>%
  # Manually input levels so the schools are ordered consistently on bar chart
  mutate(Group = factor(Group,
                        levels = c("Black", "White", "Hispanic",
                                   "Asian", "Indian", "Pac_Islander",
                                   "Two_or_More")))


# -------------------------------------------------------------------------------------


# Add district makeup numbers to the main dataset for use in creating a table
# of schools that mirror the district makeup for percentage Black, White, and Hispanic
data_district_added <- data %>%
  left_join(district_makeup, by="Year")

