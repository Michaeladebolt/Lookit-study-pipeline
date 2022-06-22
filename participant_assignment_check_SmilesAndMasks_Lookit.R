################################################################################
#   Smiles and Masks Lookit study 
#   Written by: Michaela DeBolt
#   Last edit: 11/01/20
#   Contact: mdebolt@ucdavis.edu
################################################################################

# Figure out how many participants we have in the log and how many we have 
# in the organized video folder (downloaded from Lookit), and then prints a file
# to indicate which participants need to be added to the coding log.

#### Libraries and WDs #########################################################
library(tidyverse)
library(googlesheets4)

organizedLookitVideos <- "/Volumes/General/Backup/CodingProjects/Smiles_and_Masks_LookitStudy/lookit_videos/organized_lookit_videos_2021-07-13/"
codingFolders <- "~/Box/Research/Smiles_and_Masks_LookitSudy_2020/datavyu_coding/"
setwd(organizedLookitVideos)

#### Read in the current coding log with assignments ###########################

googlesheets4::gs4_auth(email = "")

log <- read_sheet(ss = "",
                  col_names = T, na = "NULL")

log <- log[-c(1:1),] # Remove the column name definition row -- This column is super important though because it sets the data
# type for the entire column, which is used below.

# Count how many babies we have in the organized videos folder
in_folder <- data.frame(files = list.files(organizedLookitVideos), rand = "hi") #290 total in the folder

# Count how many babies are listed on the coding log
in_log <- data.frame(files = log$child_id, rand = "hi") #248

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


#### Read in log and assign babies to RAs ######################################

# Having just printed the list of babies that need to be coded and pasted them onto the google sheet,
# now, we will read in the sheet and randomly assign RAs to the babies and re-print this list.


googlesheets4::gs4_auth(email = "")

log <- read_sheet(ss = "",
                  col_names = T, na = "NULL")

log <- log[-c(1:1),] # Remove the column name definition row -- This column is super important though because it sets the data
# type for the entire column, which is used below.

# Get a list of the infants that did not have a primary coder assigned.

needsPrimary <- log %>% filter(is.na(coder_1_exported)) %>% 
  select(child_id, coder_1_assignment)





set.seed(1)
RAs <- c("AW",
"AD",
"AE",
"AN",
"BB",
"CB",
"EK",
"FQ",
"GG",
"JL",
"JR",
"MA",
"PS",
"SE",
"TN",
"TC")

for(i in 1:nrow(needsACoder)){
  
  needsACoder$coder_1_assignment[i] <- sample(RAs, 1)
  
}

setwd(organizedLookitVideos)

processedData <- "~/Box/Research/Smiles_and_Masks_LookitSudy_2020/data/processed_data/"
setwd(processedData)
write.csv(x = needsACoder, "newCoderAssignments.csv", row.names = F)

# Now, move the new assignments into the RAs folders on the NAS!
organizedLookitVideos <- "~/Box/Research/Smiles_and_Masks_LookitSudy_2020/lookit_videos/organized_lookit_videos_2021-04-28/"
codingFolders <- "/Volumes/General/Backup/CodingProjects/Smiles_and_Masks_LookitStudy/VideosToBeCoded/"
setwd(codingFolders)


for(i in 1:length(needsACoder$child_id)) {
  # Create path to RA's coding folder on the NAS
  ra_coder <- paste0(codingFolders, "TBC_SmilesAndMasks_", needsACoder$coder_1_assignment[i], "/")
  # Make a list of the files to transfer
  originalFiles <- paste0(organizedLookitVideos,needsACoder$child_id[i], "/", list.files(paste0(organizedLookitVideos,needsACoder$child_id[i]) ))
  # Create a container folder in the RA's coding folder in the NAS
  dir.create(paste0(ra_coder,needsACoder$child_id[i] )) # Create new subject-specific folder in the RA's folder
  # Move the files into the RA's coding folder
  file.copy(from = paste0(organizedLookitVideos,needsACoder$child_id[i], "/", list.files(paste0(organizedLookitVideos,needsACoder$child_id[i]) )), 
              to = paste0(ra_coder, needsACoder$child_id[i]) ) 
  print(needsACoder$child_id[i])
  }# i for end


file.copy(from = paste0( organizedLookitVideos, log$child_id[i], "/", rel_files), 
          to = paste0(codingFolders, "SmilesAndMasks_", log$coder_2_assignment[i], "/", log$child_id[i]) )












