library(dplyr)
library(readr)

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

#### Build dataset

#Load data
raw <- read.csv('datasets/anes_timeseries_2020_csv_20220210.csv')

#Filter out responses that could not be validated
#data_validated <- raw %>% filter(V200004 == "3" & V200006 == "1" & V200007 == "1" & (V200008 == "1" | V200008 == "5"))

#### Format dataset

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

#### Filtering

### Voter Status
#Eliminate anyone who does not have any voter status info or who is not registered and did not vote
data <- data %>% filter(data$voter_post_vote_status_V202068x > 0)
# Note that the above drops the 1,227 people who we don't consider voters.
# Check how many people registered but did not vote 
sum(data$voter_post_vote_status_V202068x==1)
# ~650 people registered but did not vote. We will keep this group and consider them "voters" (intended to vote)
#Not necessary to filter on voter turnout summary, as it is already a summary that includes V202068x
#data <- data %>% filter(data$voter_turnout_V202109x >= 0)

### Party Affiliation
#Eliminate anyone without a summary party ID value
#NOTE: Only reduces the dataset by < 20 rows - likely most voters that provided vote info also provided party info
data <- data %>% filter(data$party_pre_id_V201231x > 0)

#Not going to filter on registration because if we have party ID summary, this is enough to decide affiliation
#However, if lots of voters have conflicting ID and registration, we might want to revisit this
#NOTE: This significantly reduces the size of the dataset - most respondents don't have values for both of these questions
#data <- data %>% filter(data$party_prepost_registration_V202065x > 0)
#data <- data %>% filter(data$party_post_registration_V202064 > 0)

#### Cleanup

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
