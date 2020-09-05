################################################################################
#   Smiles and Masks Lookit study 
#   Written by: Michaela DeBolt
#   Last edit: 8/16/20
#   Contact: mdebolt@ucdavis.edu
################################################################################

# This code reviews how many participants we have in the study that have provided
# usable data.

# Add more info here.

#### Libraries #################################################################
library(tidyverse)
library(av)

#### Set various directory routes  #############################################
# Change these working directory paths to match the location of where these folders/files are organized on your computer
organizedLookitVideos <- "~/Box/Research/Smiles_and_Masks_LookitSudy_2020/lookit_videos/organized_lookit_videos_2020-09-03/"
responseData <- "~/Box/Research/Smiles_and_Masks_LookitSudy_2020/data/response_overview_data/"

setwd(organizedLookitVideos)

#### Read in the response data #################################################

response <- read.csv(paste0(responseData, "Smiles-and-Masks_all-responses-identifiable.csv"))

#### Determine video duration #################################################

# If we know the expected length for each kind of trial, then we can flag instances/trials where
# the recording is shorter/longer than expected - this could be due to pausing events or slow internet uploads.

tempDat <- list()
child_ids <- list.files(path = organizedLookitVideos) #list all the file names to get a list of the child_ids

for (i in 1:length(child_ids))
{
  videos <- list.files(path = paste0(organizedLookitVideos, child_ids[i]) ) #list all the files for a specific subject
  
  videoDurations <- list()
  for(k in 1:length(videos)) {
    duration <- av_media_info(paste0(organizedLookitVideos, child_ids[i],"/", videos[k]))$duration
    videoDurations[[k]] <- data.frame(child_id = child_ids[i], filename = videos[k], duration = duration)
    print(k)
  }# end the participant videos loop
  tempDat[[i]]  <- bind_rows(videoDurations)
  print(i)
}# end the child_ids loop
# "decoding for stream 0 failed" warning is fine

# Combine data into dataframe
combined_data <- bind_rows(tempDat) 
str(combined_data)
length(unique(combined_data$child_id))
length(child_ids) == length(unique(combined_data$child_id))  # should be the same length as child_ids

# Clean up the combined_data a little bit
combined_data2 <- separate(data = combined_data, col = filename, sep = "_", into = c("X1","X2" ,"trial_type", "response_uuid"),remove = F) # Warnings are fine
combined_data3 <- select(combined_data2, - c(X1, X2)) #remove; just extra/not needed

# Remove the consent and end-of-study video 
combined_data4 <- combined_data3 %>% filter(!grepl("consent", trial_type) & !grepl("completion", trial_type))

# Add a trial number from the trial_type column
combined_data5 <- separate(data = combined_data4, col = trial_type, sep = "-", into = c("trial_num"),remove = F) # Warnings are fine

# Make sure variables are of the right type
str(combined_data5)
combined_data5$trial_num <- as.numeric(combined_data5$trial_num)
combined_data5$child_id <- as.factor(combined_data5$child_id)
combined_data5$trial_type <- as.factor(combined_data5$trial_type)
combined_data5$response_uuid <- as.factor(combined_data5$response_uuid)

# rename object now that we're done with the first pass of organizing
sum.dat <- combined_data5

#### Count how many trials each participant completed ##########################

sum.dat$count <- 1
sum.dat %>% group_by(child_id, response_uuid) %>% 
  summarise(total_trials = sum(count)) -> total_trials

hist(total_trials$total_trials)

#### Study participation counts ################################################
# Find participants that have done the study more than once -- look at the child id column 

repeats <- names(which(table(response$child__hashed_id) > 1))
response %>% filter(child__hashed_id %in% repeats) -> repeat_responses

total_trials %>% filter(child_id %in% repeats)

# Determine which recording was first, and this is the recording to keep:
repeat_responses$response__date_created <- lubridate::as_datetime(repeat_responses$response__date_created)

repeat_responses %>% group_by(child__hashed_id) %>%
  filter(response__date_created == min(response__date_created)) %>% 
  ungroup() %>% 
  select(child__hashed_id, response__uuid) -> keep_repeat_responses

repeat_responses %>% filter(!response__uuid %in% keep_repeat_responses$response__uuid) -> repeatsToRemove

# Filter out the repeats (the baby's second time viewing the stimuli) from video list

sum.dat2 <- sum.dat %>% filter(!response_uuid %in% repeatsToRemove$response__uuid) 
# removed 50 videos from second attempts (from 4 babies)

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
  










