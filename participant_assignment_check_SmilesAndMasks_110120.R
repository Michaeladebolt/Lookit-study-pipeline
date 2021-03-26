################################################################################
#   Smiles and Masks Lookit study 
#   Written by: Michaela DeBolt
#   Last edit: 11/01/20
#   Contact: mdebolt@ucdavis.edu
################################################################################

# Figure out how many participants we have in the log and how many we have 
# in the organized video folder (downloaded from Lookit)

#### Libraries and WDs #########################################################
library(tidyverse)
library(googlesheets4)

organizedLookitVideos <- "~/Box/Research/Smiles_and_Masks_LookitSudy_2020/lookit_videos/organized_lookit_videos_2021-01-19/"
codingFolders <- "~/Box/Research/Smiles_and_Masks_LookitSudy_2020/datavyu_coding/"
setwd(organizedLookitVideos)

#### Read in the current coding log with assignments ###########################

googlesheets4::gs4_auth(email = "michaeladebolt@gmail.com")

log <- read_sheet(ss = "https://docs.google.com/spreadsheets/d/1e2uYY-hlBQaREzYPKf0_bEe0Y9GUxzeO3fdITv19XKk/edit#gid=0",
                  col_names = T, na = "NULL")

log <- log[-c(1:2),] # Remove the column name definition row -- This column is super important though because it sets the data
# type for the entire column, which is used below.

# Count how many babies we have in the organized videos folder
in_folder <- data.frame(files = list.files(organizedLookitVideos), rand = "hi") #98 total in the folder

# Count how many babies are listed on the coding log
in_log <- data.frame(files = log$child_id, rand = "hi") #92

# Merge two dataframes to find which babies on not on the log
together <- merge(in_folder, in_log, by = "files", all.x = TRUE)

# make a list of traning babies: 
trainingBabies <- c("PTB42L",
                    "EF5DUR",
                    "U4SWUU",
                    "3JQYYT",
                    "4KVC6T",
                    "73A7AJ")
together2 <- together %>% filter(!files %in% trainingBabies) 

# Babies that are in the folder, but not on the log:
needToBeAddedToLog <- together2 %>% filter(is.na(rand.y))

# print a list of babies that need to be added to coding log -- this will be a .csv file that you can 
# easily copy/paste into the log. You can also edit the log here directly, but you will lose the color scheme. 

write.csv(needToBeAddedToLog, "needToBeAddedToLog.csv", row.names = F)














