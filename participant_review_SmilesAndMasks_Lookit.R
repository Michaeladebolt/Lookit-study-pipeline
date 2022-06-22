################################################################################
#   Smiles and Masks Lookit study 
#   Written by: Michaela DeBolt
#   Last edit: 12/02/21
#   Contact: mdebolt@ucdavis.edu
################################################################################

#' This code reviews how many participants we have in the study that have provided
#' usable data. The code also prints a .csv with relevant information to upload to Databrary. 
#'
#' The code also determines how long each video is using the av() package.
#' 
#' You need to have run the "organizeVideos.R" code firstthat organizes all the videos into 
#' folders labelled by the child's ID.
#' 
#' 
#' You also need the most recent exports of the demographics and the response data.
#' 
#' Note that this script can also be used to determine if the child partipated in the study more than once
#' based on the number of videos in their folder. 
#' 

#### Libraries #################################################################
library(tidyverse)
library(av)

#### Set various directory routes  #############################################
# Change these working directory paths to match the location of where these folders/files are organized on your computer
organizedLookitVideos <- "/Volumes/General/Backup/CodingProjects/Smiles_and_Masks_LookitStudy/lookit_videos/organized_lookit_videos_2021-07-13/"
# 7/13/21 is the most up-to-date folder.
responseData <- "~/Box/Research/Smiles_and_Masks_LookitSudy_2020/data/response_overview_data/"
demoData <- "~/Box/Research/Smiles_and_Masks_LookitSudy_2020/data/demographic_data/"
processedData <- "~/Box/Research/Smiles_and_Masks_LookitSudy_2020/data/processed_data/"
misc <- "~/Box/Research/Smiles_and_Masks_LookitSudy_2020/images_stimuli_misc/"
setwd(organizedLookitVideos)

#### Read in the response & demographic data ###################################

response <- read.csv(paste0(responseData, "Smiles-and-Masks_all-responses-identifiable.csv"))
demos <- read.csv(paste0(demoData, "Smiles-and-Masks_all-demographic-snapshots.csv"))

# demographic data and response data should have the same number of rows. 

# indicate which study the infant participated in from the response data:
response$study_type <- ifelse(response$response__conditions.0.frameList.0.frameList.0.doRecording=="True", "mem", "pref")


#### Create upload file for Databrary ##########################################

##  This doesn't work right now! SKIP

# The demographic spreadsheet uploaded to Databrary needs to contain specific columns. 

db.dat <- data.frame(testDate = response$response__date_created, 
                     ID = response$child__hashed_id,
                     #birthdate = response$child__age_at_birth, # dont really need, but this used to be the DOB?
                     gender = response$child__gender,
                     race = demos$demographic__race_identification,
                     language = response$child__language_list,
                     country = demos$demographic__country,
                     state = demos$demographic__state)


db.dat$testDate <- lubridate::as_date(db.dat$testDate)
db.dat$birthdate <- lubridate::mdy(db.dat$birthdate)

db.dat$gender <- ifelse(db.dat$gender=="m", "Male", "Female")

#write.csv(db.dat, "~/Box/Research/Smiles_and_Masks_LookitSudy_2020/data/processed_data/databrary_files/databraryDemographicSpreadsheet.csv", row.names = F)

#### Determine video duration #################################################

#' If we know the expected length for each kind of trial, then we can flag instances/trials where
#' the recording is shorter/longer than expected - this could be due to pausing events or slow internet uploads.
#' In addition, if we know how many videos Lookit will produce in the typical case, then we can also know
#' if a child participated in the study twice (we can also tell this more easily from their UUID)

tempDat <- list()
child_ids <- list.files(path = organizedLookitVideos) #list all the file names to get a list of the child_ids

for (i in 1:length(child_ids)){
  videos <- list.files(path = paste0(organizedLookitVideos, child_ids[i]), pattern = ".mp4" ) #list all the files for a specific subject
  videoDurations <- list()
  for(k in 1:length(videos)) {
    tryCatch({
      duration <- av_media_info(paste0(organizedLookitVideos, child_ids[i],"/", videos[k]))$duration
      videoDurations[[k]] <- data.frame(child_id = child_ids[i], filename = videos[k], duration = duration)
      print(k)
    }, #catch
    error=function(e){cat("ERROR :",conditionMessage(e), "\n")}) 
  }# end the participant videos loop
  
  tempDat[[i]]  <- bind_rows(videoDurations)
  print(paste0("child_id:", " ", child_ids[i]) ) 
}# end the child_ids loop
# "decoding for stream 0 failed" warning is fine

# Combine data into dataframe
combined_data <- bind_rows(tempDat) 
str(combined_data)
length(unique(combined_data$child_id))
length(child_ids) == length(unique(combined_data$child_id))  # should be the same length as child_ids
# IF this is not the same length, a child was left off from the above video length checking loop - check! 


# Add the trial name to the combined data
combined_data2 <- separate(data = combined_data, col = filename, sep = "_", into = c("X1","X2" ,"trial_name", "response_uuid"),remove = F) # Warnings are fine
combined_data3 <- select(combined_data2, - c(X1, X2)) #remove; just extra/not needed

# Remove the consent and end-of-study video 
combined_data4 <- combined_data3 %>% filter(!grepl("consent", trial_name) & !grepl("completion", trial_name) & trial_name!= "6-intro-video" )

# Add a trial number from the trial_name column
combined_data5 <- separate(data = combined_data4, col = trial_name, sep = "-", into = c("trial_num"),remove = F) # Warnings are fine

# Make sure variables are of the right type
str(combined_data5)
combined_data5$trial_num <- as.numeric(combined_data5$trial_num)
combined_data5$child_id <- as.factor(combined_data5$child_id)
combined_data5$trial_name <- as.factor(combined_data5$trial_name)
combined_data5$response_uuid <- as.factor(combined_data5$response_uuid)

# Add in the testing date from the response data 
response_small <- response %>% select(response__uuid, response__date_created) # make smaller 'response' dataframe
# merge data sets -- 
combined_data6 <- merge(combined_data5, response_small, by.x = "response_uuid", by.y = "response__uuid")

# Clean up date and time column a little bit:
combined_data7 <- separate(data = combined_data6, col = response__date_created, sep = " ", into = c("date", "time"),remove = F) # Warnings are fine

# Indicate which study the participant was in based on trial_name names:
combined_data7$study <- ifelse(grepl(pattern = "mem", x = combined_data7$trial_name), "memory", 
                               ifelse(combined_data7$trial_name=="6-intro-video", "memory", 
                                      ifelse(combined_data7$trial_name == "10-null", "memory", "pref")))

# Label the type of trial within each study -- test trial or calibration trial
combined_data7$trial_type  <- ifelse(grepl("mem", combined_data7$trial_name) | grepl("test", combined_data7$trial_name), "test", "cal")
# Fix this because it is missing some memory calibration trials 


# rename object now that we're done with the first pass of organizing
sum.dat <- combined_data7
write.csv(sum.dat, paste0(processedData,paste0("trial_durations_", Sys.time(), ".csv")), row.names = F)

# Left off here (as of 12/9/21)

#### How many participants in each study (at this point) #######################
#* note that this is a count before any trial duration evaluation has occurred

# The first phase (preference only) infants were run between dates 7/25/20 - 10/19/20
library(lubridate)
sum.dat$date <- as.Date(sum.dat$date) # Change the date column to "date" type
str(sum.dat)

# indicate phases
sum.dat$study_phase <- ifelse(sum.dat$date >= as.Date("2020-07-25") & sum.dat$date <= as.Date("2020-10-06"), "phase1", "phase2")

# Order dates for the first phase (preference study):
# pref-orderA: 7/25/20 - 8/13/20
# pref-orderB: 8/14/20 - 9/22/20
# pref-orderA: 9/23/20 - 11/19/20

# Order dates for the second phase (preference + memory)
# pref-orderA: 12/4/20 - 1/5-21
# pref-orderB: 1/6/21 - current (2/4/21)
# mem-orderA: 12/4/21 - 1/5/21
# mem-orderB: 1/6/21 - current (4/28/21)

# I stopped the entire study on July 1st 2021

# Within the second phase, indicate which order was run:
sum.dat$order <- ifelse(sum.dat$study == "memory" & sum.dat$date >= as.Date("2020-12-04") & sum.dat$date <= as.Date("2021-01-05"), "mem-orderA", 
                        ifelse(sum.dat$study == "memory" & sum.dat$date >= as.Date("2021-01-06") & sum.dat$date <= as.Date("2021-03-16"), "mem-orderB",
                               ifelse(sum.dat$study == "memory" & sum.dat$date >= as.Date("2021-03-26") & sum.dat$date <= as.Date("2021-05-21"), "mem-orderC",
                                      ifelse(sum.dat$study == "pref" & sum.dat$date >= as.Date("2020-07-25") & sum.dat$date <= as.Date("2020-08-13"), "pref-orderA",
                                             ifelse(sum.dat$study == "pref" & sum.dat$date >= as.Date("2020-08-14") & sum.dat$date <= as.Date("2020-09-22"), "pref-orderB",
                                                    ifelse(sum.dat$study == "pref" & sum.dat$date >= as.Date("2020-09-23") & sum.dat$date <= as.Date("2020-11-19"), "pref-orderA", 
                                                           ifelse(sum.dat$study == "pref" & sum.dat$dat >= as.Date("2020-12-04") & sum.dat$date <= as.Date("2021-01-05"), "pref-orderA", 
                                                                  ifelse(sum.dat$study == "pref" & sum.dat$dat >= as.Date("2021-01-06") & sum.dat$date <= as.Date("2021-05-21"), "pref-orderB", NA))))))))
                                      

# Count the number of infants in each phase and order: 
sum.dat %>% group_by(study_phase, order) %>% 
  summarise(count = length(unique(child_id))) -> order_summary
write.csv(order_summary, paste0(processedData, "subjectCounts.csv"), row.names = F)
write.csv(sum.dat, paste0(processedData, "subjectPhaseID.csv"), row.names = F)

    
# Visualize:          
ggplot()+
  scale_x_date(date_labels = "%b %d", date_breaks = "3 weeks") +
  ylab("Order") + xlab("Testing week") +
  geom_rect(data=data.frame(from=c(as.Date("2020-07-25"),as.Date("2020-11-06")),
                            to=c(as.Date("2020-10-06"),as.Date("2021-05-21")),
                            Phase = c("First", "Second")),
            aes(xmin=from,xmax=to,ymin=-Inf,ymax=Inf,fill= Phase), alpha=0.2) +
  geom_jitter(data = sum.dat, aes(x = date, y = order), width = .2, height = .2, size = 4, shape = 1, alpha = .7) +
  theme_bw(base_size = 35) +
  scale_fill_manual(values = c("darkmagenta", "palegreen4")) +
  theme(axis.text.x=element_text(angle=60, hjust=1),
        legend.position = "top") 
ggsave(paste0(misc, "InfantOrderStudyBreakdown.jpg"), height = 15, width = 15)
  
sum(order_summary$count) 

#### Count how many trials each participant completed ##########################

sum.dat$count <- 1
sum.dat %>% group_by(child_id, response_uuid) %>% 
  summarise(total_trials = sum(count)) -> total_trials

hist(total_trials$total_trials)

#### Study participation counts ################################################

# Find participants that have done the study more than once -- look at the child id column 
repeats <- names(which(table(response$child__hashed_id) > 1))

# Data frame containing the child id's that repeated the study
repeat_responses <- response %>% filter(child__hashed_id %in% repeats) 

#total_trials %>% filter(child_id %in% repeats)

# Determine which recording was first, and this is the recording to keep:
repeat_responses$response__date_created <- lubridate::as_datetime(repeat_responses$response__date_created)

repeat_responses_summary <- repeat_responses %>% 
  group_by(child__hashed_id, study_type) %>%
  mutate(first_response = response__date_created == min(response__date_created))  %>% 
  select(child__hashed_id, response__uuid, study_type, first_response) %>% 
  arrange(child__hashed_id)
write.csv(repeat_responses_summary, paste0(processedData, "repeat_responses_summary_", Sys.time(), ".csv"))

# Another way to do this would be to create a data frame from ALL the data:
repeat_responses_summary <- response %>% 
  group_by(child__hashed_id, study_type) %>%
  mutate(first_response = response__date_created == min(response__date_created))  %>% 
  select(child__hashed_id, response__uuid, study_type, first_response) %>% 
  arrange(child__hashed_id)
write.csv(repeat_responses_summary, paste0(processedData, "repeat_responses_summary_", Sys.time(), ".csv"))

# Determine which of these runs were actually coded -  this will require reading in the coded data... 
  



# Filter out the repeats (the baby's second time viewing the stimuli) from video list

sum.dat2 <- sum.dat %>% filter(!response_uuid %in% repeatsToRemove$response__uuid) 
# removed 50 videos from second attempts (from 4 babies)

sum.dat2 <- sum.dat

# re-count trials:
sum.dat2$count <- 1
sum.dat2 %>% group_by(child_id, response_uuid) %>% 
  summarise(total_trials = sum(count)) -> total_trials2



#### Evaluate video duration ###################################################
# round duration to be less conservative
sum.dat2$duration_round <- round(sum.dat2$duration, 1)

sum.dat3 <- sum.dat2 %>% mutate(duration_eval = ifelse(grepl(pattern = "calibration-trial-1", x = sum.dat2$trial_type) & duration < 19.5, "too_short",
                                           ifelse(grepl(pattern = "calibration-trial-2", x = sum.dat2$trial_type) & duration < 9.5, "too_short", 
                                                  ifelse(grepl(pattern = "test", x = sum.dat2$trial_type) & duration < 11.5, "too_short", "fine")))) 
sum.dat4 <- sum.dat3 %>% filter(duration_eval == "fine")

final.summary <- sum.dat4

#### Filter the response data based on the summary data ########################

final_child_ids <- distinct_at(sum.dat4, vars(child_id, response_uuid))
final.data <- response %>% filter(response__uuid %in% final_child_ids$response_uuid)

# Aug 14th was the order change event
final.data$response__date_created <- lubridate::as_datetime(final.data$response__date_created)
final.data$order <- ifelse(final.data$response__date_created < "2020-08-14", "A", "B")
# check to make sure this worked:
final.data %>% select(response__date_created, order)

# remove Michaela test
final.data %>% filter(child__name!= "Michaela") -> final.response

#### Summary statistics ########################################################

final.response$gender <- ifelse(final.response$child__gender=="f", 1, 0)
final.response$count <- 1
final.response %>% group_by(order) %>% 
  summarise(avgAgeDays = mean(child__age_in_days), 
            avgAgeMos = mean(child__age_in_days)/30.4,
            medAgeDays = median(child__age_in_days),
            medAgeMos = median(child__age_in_days)/30.4,
            avgGender = mean(gender),
            n = sum(count)) -> responseSummary


final.summary %>% group_by(child_id, response_uuid) %>% 
  summarise(nTrials = sum(count)) -> final.summary.ntrials

hist(final.summary.ntrials$nTrials)



# Figure
ggplot()+
  geom_jitter(data = final.response, aes(x = order, y = child__age_in_days, color = child__gender),
              width = .1, size = 5, alpha = .9) +
  ylab("Age in days") +
  xlab("Study order") +
  labs(color='Gender') + 
  ggtitle("Smiles and Masks") +
  scale_colour_manual(values = c("darkorange2", "darkseagreen4"))  +
  scale_y_continuous(limits = c(120, 370), breaks = seq(120, 370, 50)) +
  scale_x_discrete(labels=c("A" = "A \n(n = 30)", "B" = "B \n(n = 28)")) +
  theme_bw() +
  theme(text = element_text(size=16)) 
  










