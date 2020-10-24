################################################################################
#   Smiles and Masks Lookit study 
#   Written by: Michaela DeBolt
#   Last edit: 8/16/20
#   Contact: mdebolt@ucdavis.edu
################################################################################

# This file creates a .csv for the Ruby script that contains all of the trials that the RA will code for.
# using the Frame data from the timestamp R code, this file also figures out instances in which the parent
# paused the trial and extended the time beyond what is normal, etc. 

# what we are left with is a csv file that we can give to Ruby to create the datavyu files.

#### Libraries #################################################################
library(stringr)
library(tidyverse)

#### Set various directory routes  #############################################
# Change these working directory paths to match the location of where these folders/files are organized on your computer
# Make sure all of these folders/files contain the most up-to-date information & exports, etc. 
organizedLookitVideos <- "~/Box/Research/Smiles_and_Masks_LookitSudy_2020/lookit_videos/organized_lookit_videos_2020-10-19/"
#rubyData <- "~/Box/Research/Smiles_and_Masks_LookitSudy_2020/data/processed_data/files_for_ruby_code/"
rubyData <- "~/Desktop/data/"
responseData <- "~/Box/Research/Smiles_and_Masks_LookitSudy_2020/data/response_overview_data/"

# Create a dataframe with the unique subject ID information
setwd(organizedLookitVideos)
sub_ids <- data.frame(filename = list.files(path = organizedLookitVideos))
master_data <- list()

for (i in 1:nrow(sub_ids))
{
  filenames <- list.files(path = paste0(organizedLookitVideos, sub_ids[i,1])) #list all the files for a specific subject
  master_data[[i]] <- data.frame(child_id = sub_ids[i,1], filenames = filenames)
}# end for loop

combined_data <- bind_rows(master_data) 
str(combined_data) #good

# extract the names of the trials from the filename variable
temp <- separate(data = combined_data, col = filenames, sep = "_", into = c("X1","X2" ,"trial_type", "response_uuid"),remove = F)
#warnings are fine!

combined_data2 <- cbind(combined_data, trial_name = temp$trial_type) #combine datasets

# remove all trials in which we don't actually want the RAs to code for
combined_data3 <- combined_data2 %>% filter(!grepl("consent", trial_name) & !grepl("completion", trial_name))

# Read in the response data to get the test date for each child:
response <- read.csv(paste0(responseData, "Smiles-and-Masks_all-responses-identifiable.csv"), header = TRUE, na.strings = " ")
response2 <- select(response, "child__hashed_id", "response__date_created")
response2$response__date_created <- as.character(response2$response__date_created)
response2 <- separate(data = response2, col = response__date_created, into = c("date", "time"), sep = " ")
  
# Merge the two datasets
combined_data4 <- merge(combined_data3, response2, by.x = "child_id", by.y = "child__hashed_id")

# Create the final data set that will be used in the Ruby code:
final_data <- combined_data4 %>% select(child_id, test_date = date, filename = filenames, trial_name) %>% 
  mutate(coder_1_initials = NA, coder_1_looks = NA, coder_1_notes = NA)


#### Print separate files for each row in the final_data object ################
for(i in 1:nrow(final_data)){
  write.csv(x = final_data[i,], 
            file = paste0(rubyData, final_data$child_id[i], "_", final_data$trial_name[i],
                          "_", final_data$test_date[i],".csv"), row.names = FALSE)
}

length(unique(final_data$filename)) # This should be how many files are printed in total.
# So, if a baby does the study more than once, they will only get files printed for their
# first time around -- which is fine, because the information in the datavyu spreadsheet will
# be the same. 
