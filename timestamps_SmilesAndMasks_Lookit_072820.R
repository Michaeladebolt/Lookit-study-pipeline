################################################################################
#   Smiles and Masks Lookit study 
#   Written by: Michaela DeBolt
#   Last edit: 8/16/20
#   Contact: mdebolt@ucdavis.edu
################################################################################

# This code reads in the "all-frames" file containing all of the frame information for 
# all the participants in the study. From this file, we extract the time stamps for each 
# event to calculate the duration of each event, for each individual infant and trial. 

# You must download the individual frame files for each participant under the Frame Data
# data downloading menu in Lookit. 

# You must also download a current demographic file under the Demographic Snapshots
# data downloading menu in Lookit. 

#### Working directories #######################################################

processedData <- "~/Box/Research/Smiles_and_Masks_LookitSudy_2020/data/processed_data/" 
frameData <- "~/Box/Research/Smiles_and_Masks_LookitSudy_2020/data/frame_data/"
demoData <- "~/Box/Research/Smiles_and_Masks_LookitSudy_2020/data/demographic_data/"
setwd(frameData)

#### Libraries #################################################################

library(tidyverse)
library(lubridate)

#### Read in raw data ##########################################################

# Read in the "frame data"
allFrameData <- data.frame(filename = list.files(path = frameData, pattern = ".csv"))

tempData <- list()
for(i in 1:nrow(allFrameData)) {
  tempData[[i]] <- read.csv(file = paste0(frameData,allFrameData[i,1]), na.strings = " ", header = T)
}
frameDat <- bind_rows(tempData) 

# Read in the demographic data
demoDat <- read.csv(paste0(demoData, "Smiles-and-Masks_all-demographic-snapshots.csv"), na.strings = " ")
# Note that the number of rows in the demographic data should match the number of rows in the "allFramesData" object

#### Tidy up the data sets #####################################################

# Remove extra events that we don't need at the moment
remove <- c("0-first-instructions", "3-face-mask-q", "1-video-config", "2-video-consent", "16-my-exit-survey", "15-study-completion", "4-second-instructions" )

# Remove unwanted frame_ids
frameDat %>% filter(!frame_id %in% remove) -> frameDat2

# Create a "trial" number from the frame_id column in the data
frameDat3 <- separate(data = frameDat2, col = frame_id, into = c("trialNum"), sep = "-", remove = F) # Warnings are fine!
str(frameDat3)

# Ensure our new trial number is numeric
frameDat3$trialNum <- as.numeric(frameDat3$trialNum)

# Create a column to identify whether the event was a test image or calibration sequence
frameDat3$event_kind <- ifelse(grepl("test", x = frameDat3$frame_id), "test", "calibration")

# Create dataframes containing both the stream time and the timestamp information to later merge with entire dataset.
# The purpose of this is so that when we merge the data, all of the needed information will be on one row.
streamKey <- frameDat3 %>% select(response_uuid, trialNum, event_number, newKey = key, streamTime = value) %>%
  filter(newKey == "streamTime")

timeStampKey <- frameDat3 %>% select(response_uuid, trialNum, event_number, newKey2 = key, timeStamp = value) %>% 
  filter(newKey2 == "timestamp")

# Merge these two dataframes together to get the steamTime variable on every row
frameDat4 <- merge(streamKey, frameDat3, by = c("response_uuid", "trialNum", "event_number"), all.y = T)

frameDat5 <- merge(timeStampKey, frameDat4, by = c("response_uuid", "trialNum", "event_number"), all.y = T)

# Create an additional dataframe with the stopping capture information to get the end of the trial duration ("streamtime")
stopCaptureKey <- frameDat5 %>% select(response_uuid, trialNum, newValue = value, endstreamTime = streamTime ) %>% 
  filter(newValue == "exp-lookit-preferential-looking:stoppingCapture")

frameDat6 <- merge(stopCaptureKey, frameDat5, by = c("response_uuid", "trialNum"), all.y = T)

# Sort data by newTrial number so the data is in the order in which it actually appeared
frameDat7 <- frameDat6 %>% arrange(response_uuid, trialNum, event_number) 

#### Extract the frame information that we care about ##########################

# The test trials begin with: exp-lookit-preferential-looking:startTestImages

# The calibration trials begin with: exp-lookit-preferential-looking:startCalibration (center, left, and right gives us redundant information for calibration)

wantedInfo <- c("exp-lookit-preferential-looking:startTestImages", "center", "left", "right")

subjectInfo <- frameDat7 %>% filter(value %in% wantedInfo)
subjectInfo$streamTime <- as.numeric(as.character(subjectInfo$streamTime))
subjectInfo$endstreamTime <- as.numeric(as.character(subjectInfo$endstreamTime))
str(subjectInfo)

# Mark the last calibration item:
subjectInfo2 <- subjectInfo %>% group_by(response_uuid, trialNum) %>%
  mutate(maxEventNum = ifelse(event_kind == "calibration", max(event_number), 
                              ifelse(frame_id =="14-test-trials-second", event_number, 0)))

subjectInfo2$test <- ifelse(subjectInfo2$event_number == subjectInfo2$maxEventNum, subjectInfo2$endstreamTime, NA)
subjectInfo2$test2 <- ifelse(subjectInfo2$event_kind == "calibration", lead(subjectInfo2$streamTime,1), subjectInfo2$endstreamTime)
subjectInfo2$endOfTrialTime <- ifelse(!is.na(subjectInfo2$test), subjectInfo2$test, subjectInfo2$test2)

subjectInfo2$frame_duration_s <- subjectInfo2$endOfTrialTime - subjectInfo2$streamTime 

# combine the calibration label with the location "value"
subjectInfo2$event_kind2 <- ifelse(subjectInfo2$event_kind=="calibration", paste0(subjectInfo2$event_kind, "_", subjectInfo2$value), subjectInfo2$event_kind)

#### Create date and time ######################################################

subjectInfo2$timeStamp <- as.character(subjectInfo2$timeStamp)
subjectInfo3 <- separate(data = subjectInfo2, sep = "T", col = timeStamp, into = c("date", "time")) 

#### Clean up file and print ###################################################

finalDat <- subjectInfo3 %>% select(response_uuid, child_hashed_id, test_date = date, timestamp = time, trial_number = trialNum, event_number,
                                    frame_id, event_kind = event_kind2, frame_duration_s, frame_start_s = streamTime, frame_end_s = endOfTrialTime)

write.csv(x = finalDat, file = paste0(processedData, "processed_frame_data", "/processed_frame_data_", Sys.Date(), ".csv"), row.names = F)

#### Save a copy of the frame data to the folder containing videos #############

# This section splits up the final data into separate csv files for each infant
# and then saves this file in the same folder containing the baby's videos. This 
# file can then be used by RAs as a reference, if needed. 

## abandoned this idea ^^ just going to have RAs code the entire video









