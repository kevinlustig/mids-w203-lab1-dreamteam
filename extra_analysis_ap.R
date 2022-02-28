
################################################################################
## Load in necessary packages ##

library(dplyr)
library(readr)
library(scales)
library(tidyverse)
################################################################################

################################################################################
## Set working directory ##
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
################################################################################

################################################################################
## Build dataset ##
#Load data
raw <- read.csv('datasets/anes_timeseries_2020_csv_20220210.csv')

#Filter out responses that could not be validated
#data_validated <- raw %>% filter(V200004 == "3" & V200006 == "1" & V200007 == "1" & (V200008 == "1" | V200008 == "5"))

# Format dataset
columns = c(
  #General info
  "V200001"="id_V200001",
  #Voter status
  "V202068x"="voter_post_vote_status_V202068x",
  "V202109x"="voter_turnout_V202109x",
  #Voting difficulty
  "V201024"="difficulty_manner_voted_V201024",
  "V202116"="difficulty_when_voted_V202116",
  "V202117"="difficulty_how_voted_V202117",
  "V202118"="difficulty_how_usually_votes_V202118",
  "V202119"="difficulty_how_difficult_V202119",
  "V202120a"="difficulty_encountered_registration_V202120a",
  "V202120b"="difficulty_encountered_id_V202120b",
  "V202120c"="difficulty_encountered_absentee_V202120c",
  "V202120d"="difficulty_encountered_ballot_V202120d",
  "V202120e"="difficulty_encountered_polling_place_V202120e",
  "V202120f"="difficulty_encountered_wait_V202120f",
  "V202120g"="difficulty_encountered_work_V202120g",
  "V202120h"="difficulty_encountered_weather_V202120h",
  "V202120i"="difficulty_encountered_mailing_V202120i",
  "V202120j"="difficulty_encountered_other_V202120j",
  "V202120k"="difficulty_encountered_none_V202120k",
  "V202121"="difficulty_wait_time_V202121",
  "V202122"="difficulty_time_to_polling_place_V202122",
  "V202123"="difficulty_didnt_vote_reason_V202123",
  #Party affiliation
  "V201231x"="party_pre_id_V201231x",
  "V202065x"="party_prepost_registration_V202065x",
  "V202064"="party_post_registration_V202064"
)

data <- raw %>% select(names(columns))
data <- data %>% rename_with(~ columns[.])
################################################################################

################################################################################
## Filter Dataset by Voter Status and Party Affiliation ## 

### Voter Status ###
#Eliminate anyone who does not have any voter status info or who is not registered and did not vote
data <- data %>% filter(data$voter_post_vote_status_V202068x > 0)
# Note that the above drops the 1,227 people who we don't consider voters.

# Check how many people registered but did not vote 
sum(data$voter_post_vote_status_V202068x==1)
# ~650 people registered but did not vote. We will keep this group and consider them "voters" (intended to vote)
#Not necessary to filter on voter turnout summary, as it is already a summary that includes V202068x
#data <- data %>% filter(data$voter_turnout_V202109x >= 0)

### Party Affiliation ###
#Eliminate anyone without a summary party ID value
#NOTE: Only reduces the dataset by < 20 rows - likely most voters that provided vote info also provided party info
data <- data %>% filter(data$party_pre_id_V201231x > 0)

#Not going to filter on registration because if we have party ID summary, this is enough to decide affiliation
#However, if lots of voters have conflicting ID and registration, we might want to revisit this
#NOTE: This significantly reduces the size of the dataset - most respondents don't have values for both of these questions
#data <- data %>% filter(data$party_prepost_registration_V202065x > 0)
#data <- data %>% filter(data$party_post_registration_V202064 > 0)

## Cleanup

### Looking for columns with all NAs, none found
#not_all_na <- function(x) any(!is.na(x))
#dv <- dv %>% select(where(not_all_na))

### Auto type detection/conversion
# No need for this, as all selected columns are already Z
#data <- readr::type_convert(data)

### Determine Party Affiliation 

# Explore: how many are independent with no lean?
sum(data$party_pre_id_V201231x==4) #719 - a meaningful number
# We can't consider these members of either D or R party
# We are choosing NOT to use how they have voted in the past or in this election to tag them as a party
# Therefore we will leave them as "independent" for now (so we have three possible party tags - D, R, and I)
# If partyID is 1,2, or 3 (strong dem, not very strong dem, indep-dem), tag with D
# If partyID is 4 (indep) tag with I 
# If partyID is 5,6, or 7 (strong rep, not very strong rep, indep-rep), tag with R

data <- data %>%
  mutate(party = case_when(party_pre_id_V201231x <=3 ~ "D",
                           party_pre_id_V201231x ==4 ~ "I",
                           party_pre_id_V201231x >=5 ~ "R"))

table(data$party) # This tells us how many of each we have 
################################################################################

################################################################################
## Difficulty Variables ##

# namez<-data.frame(colnames(data))

# Subjective difficulty variable only for people who voted; no data to extrapolate to those who didn't vote 
# unless we try to make assumptions on the obstacles they may have faced for not voting and then try to extrapolate
# but then we're not doing it for people who voted for this variable (because we already have it) and we don't have solid
# information on what exact conditions made it a certain level of difficulty experienced by people who did not vote

## Add in columns combining difficulty scenarios for people who did and did not vote ##
data2 <- data %>% mutate(
  # "PEOPLE WHO DID NOT VOTE - VARIABLE RESPONSE" ~ "PEOPLE WHO DID VOTE - MULTIPLE VARIABLES"
  # " " ~ "Registration problem" - only people who voted
  # For people who didn't vote they had option "I am not registered". This could be by choice, as such it is not seen as a difficulty
  # for people for did not vote; being seen here as choosing not to get registered 
  diff_registration = case_when(
    difficulty_encountered_registration_V202120a == 1 ~ 1,
    TRUE ~ 0
  ),
  # "I did not have the correct form of identification" ~ "Concern about identification card"
  diff_idcard = case_when(
    difficulty_encountered_id_V202120b == 1 ~ 1,
    TRUE ~ 0
  ),
  # "I requested but did not receive an absentee ballot" ~ "Difficulty obtaining an absentee ballot" 
  # Did not include the reason "out of town" for those who didn't vote because we can't assume that was a choice which made it
  # difficult to vote or it was just a choice they made instead of staying in their area of residence to vote
  diff_absentee = case_when(
    difficulty_encountered_absentee_V202120c == 1 ~ 1, 
    TRUE ~ 0
  ),
  # " " ~ Confusion about ballot or machine" - only people who voted; no equivalent for people who didn't vote
  diff_confusionball = case_when(
    difficulty_encountered_ballot_V202120d == 1 ~ 1, 
    TRUE ~ 0
  ),
  # "Transportation" ~ "Difficulty getting to polling place"
  diff_accesspoll = case_when(
    difficulty_encountered_polling_place_V202120e == 1 ~ 1, 
    TRUE ~ 0
  ),
  # "The line at the polls was too long" ~ "Long wait times"
  diff_wait = case_when(
    difficulty_encountered_wait_V202120f == 1 ~ 1, 
    TRUE ~ 0
  ),
  # " " ~ Work schedule" - only people who voted
  # For the variable on people who didn't work, the closest is "Too busy" which may or may not be related to work schedules
  diff_worksched = case_when(
    difficulty_encountered_work_V202120g == 1 ~ 1, 
    TRUE ~ 0
  ),
  # "Bad weather" ~ "Bad weather"
  diff_weather = case_when(
    difficulty_encountered_weather_V202120h == 1 ~ 1, 
    TRUE ~ 0
  ),
  # " " ~ Issue mailing ballot" - only for people who voted; no equivalent for people who didn't vote 
  diff_ballmail = case_when(
    difficulty_encountered_mailing_V202120i == 1 ~ 1, 
    TRUE ~ 0
  ),
  diff_other = case_when(
    difficulty_encountered_other_V202120j == 1~ 1,
    TRUE ~ 0
  ), 
)


## Create the Obstacles Index ##
data2 <- data2 %>% mutate(
  diff_index = select(., diff_registration, 
                      diff_idcard, 
                      diff_absentee,
                      diff_confusionball, 
                      diff_accesspoll, 
                      diff_wait, 
                      diff_worksched, 
                      diff_weather, 
                      diff_ballmail, 
                      diff_other) %>% 
    rowSums(na.rm = TRUE))

## Quick analysis of different difficulty situations, index, and subjective difficulty 

# Select relevant columns 
data_analysis2 <- data2 %>% select(diff_registration, 
                                 diff_idcard, 
                                 diff_absentee,
                                 diff_confusionball, 
                                 diff_accesspoll, 
                                 diff_wait, 
                                 diff_worksched, 
                                 diff_weather, 
                                 diff_ballmail, 
                                 diff_other, 
                                 diff_index)

# Create a function that creates a data frame with the counts and % of each option 
analysis_df2 <- function(x) {
  as.data.frame(table(x, useNA = "ifany")) %>%
    mutate(Perc = scales::percent(Freq/sum(Freq)), accuracy = 1L) %>%
    select(-accuracy)
}

# Apply the function on all the columns in the data frame
analysis_all_df2 <- apply(data_analysis2,2, analysis_df2)
analysis_all_df2


## Based on above list of dfs from analysis_all_df2
# Total in our dataset is 7035
# Total who answered 1 or more difficulties of those who voted is 956
# Total of those who encountered 1 difficulty is 762
# Percentage of those enountered 1 difficulty 
762/956 
# you get 79.7% 

