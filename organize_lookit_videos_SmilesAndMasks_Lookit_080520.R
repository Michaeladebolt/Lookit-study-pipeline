################################################################################
#   Smiles and Masks Lookit study 
#   Written by: Michaela DeBolt
#   Last edit: 8/5/20
#   Contact: mdebolt@ucdavis.edu
################################################################################

# This file organizes all the videos from all of the participants in the study. Specifically,
# this code creates a folder for each participant using their "child_hashed_id" assigned to them within Lookit.
# If the same participant participates in the study more than once, all of their study videos will be 
# organized in the same folder. 

# Before running the code below, you will need to create 2 folders entitled, "lookit_videos" and "response_overview".
# The lookit_videos folder will contain all of the video files you downloaded from Lookit in a single folder. It it important that you don't edit
# the file names in any way. The response_overview folder will contain the Your-Study-Name_all-responses-identifiable file.csv. This file can be obtained from the 
# Lookit site under the "All Responses" heading and the "Response overview" subsection. It doesn't matter if you select identifying information or not -- it is important
# that this file contains the child's hashed id ("child_hashed_id") and the response_uuid ("response_uuid")

#### Libraries #################################################################
library(stringr)
library(tidyverse)

#### Set various directory routes  #############################################
# Change these working directory paths to match the location of where these folders/files are organized on your computer
lookitVideos <- "~/Box/Research/Smiles_and_Masks_LookitSudy_2020/lookit_videos/"
responseData <- "~/Box/Research/Smiles_and_Masks_LookitSudy_2020/data/response_overview_data/"

### Read in relevant data ######################################################
responseDat <-  read.csv(paste0(responseData, "Smiles-and-Masks_all-responses-identifiable.csv"), na.strings = " ")

# Create a dataframe with the unique subject ID information
setwd(lookitVideos)
allVideos <- data.frame(filename = list.files(path = lookitVideos, pattern = ".mp4"))
allVideos2 <- separate(data = allVideos, col = filename, sep = "_", into = c("X1","X2" ,"trial_type", "response_uuid"),remove = F)
allVideos3 <- merge(allVideos2, responseDat, by.x = "response_uuid", by.y = "response__uuid")

sub_ids <- unique(allVideos3[,c("response_uuid","child__hashed_id")])  # create a list of the study IDs


### Create a folder to put all the organized videos ############################
# This folder will be created within the same folder in which all of your video files are stored. This can be changed by substituting another location
# for the "lookitVideos" path. 
newDir <- paste0(lookitVideos, "organized_lookit_videos_", Sys.Date()) # Create the new folder name with today's date
dir.create(newDir) # Create the new folder 

# Loop through unique subject IDs and put videos in their new homes
for (i in 1:nrow(sub_ids))
{
  filenames <- list.files(path = lookitVideos, pattern = sub_ids[i,1]) #list all the files for a specific subject
  newSubDir <- dir.create(paste0(newDir, "/", sub_ids[i,2])) # Create new subject-specific folder
  file.copy(from = filenames, to = paste0(newDir, "/", sub_ids[i,2])) # Copies all the files into the new homes
}# end for loop

# done!

# Warnings indicate that you have a participant that did the study more than once. They are fine to ignore. 

