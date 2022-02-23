library(dplyr)
library(readr)

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

#Load data
data <- read.csv('datasets/anes_timeseries_2020_csv_20220210.csv')

#Filter out responses that could not be validated
data_validated <- data %>% filter(V200004 == "3" & V200006 == "1" & V200007 == "1" & (V200008 == "1" | V200008 == "5"))

#Select respondent ID columns and survey response columns as of interest for exploration
dv_id <- subset(data_validated, select=V200001:V160001_orig)
dv_survey <- subset(data_validated, select=V201004:V203008)
dv <- cbind(dv_id,dv_survey)

#Filter to just respondents who were also surveyed in 2016 in case that's of interest
dv_2016 <- dv %>% filter(V160001_orig != "-1")

### Looking for columns with all NAs, none found
#not_all_na <- function(x) any(!is.na(x))
#dv <- dv %>% select(where(not_all_na))

#Auto type detection/conversion
dv <- readr::type_convert(dv)
dv_2016 <- readr::type_convert(dv_2016)
