################################################################################
#   Smiles and Masks Lookit study 
#   Written by: Michaela DeBolt
#   Last edit: 9/4/20
#   Contact: mdebolt@ucdavis.edu
################################################################################

# Description

#### Libraries #################################################################
library(tidyverse)
library(googlesheets4)


#### Set various directory routes  #############################################
# Change these working directory paths to match the location of where these folders/files are organized on your computer
organizedLookitVideos <- "~/Box/Research/Smiles_and_Masks_LookitSudy_2020/lookit_videos/organized_lookit_videos_2020-09-03/"
responseData <- "~/Box/Research/Smiles_and_Masks_LookitSudy_2020/data/response_overview_data/"
codingFolder <- "~/Box/Research/Smiles_and_Masks_LookitSudy_2020/datavyu_coding/"
datavyuFiles <- "~/Box/Research/Smiles_and_Masks_LookitSudy_2020/data/processed_data/datavyu_files/"
setwd(organizedLookitVideos)

#### Randomly assign primary & secondary coders ################################
set.seed(6768)
child_ids <- list.files(path = organizedLookitVideos) #list all the file names to get a list of the child_ids
coder_ids <- gsub(pattern = "SmilesAndMasks_", replacement = "", x = list.files(path = codingFolder, pattern = "_")) # Get initials of coders

# Set up new dataframe to store information
dat <- data.frame(child_ids = child_ids, coder_1_assignment = sample(coder_ids, size = length(child_ids) , replace = T),
                  coder_1_complete = NA, coder_1_notes = NA, coder_2_assignment = NA, coder_2_complete = NA, coder_2_notes = NA)
dat %>% filter(child_ids != "PTB42L" & child_ids != "V32NYE") -> dat # remove training baby and Michaela test

# Assign the second coder
coders <- data.frame(coder_ids)
for(i in 1:length(dat$coder_1_assignment)){
  options <- coders %>% filter(coders != dat$coder_1_assignment[i]) %>% as.list()
  dat$coder_2_assignment[i] <- sample(options[[1]], size = 1)
}

#### Write to Google sheets ####################################################
#googlesheets4::gs4_auth(email = "michaeladebolt@gmail.com")
existing_log <- read_sheet(ss = "https://docs.google.com/spreadsheets/d/18B5HTdkbM9cI5BoopPay4f0H5m2NiGsQKzIHfgghl98/edit#gid=1015067118",
                        na = "NA")

## Do some editing to the existing log based on whether things are being coded etc....

# Save to an updated object name....updated_log
updated_log <- dat[order(dat$coder_1_assignment),]

# Create new google sheet with updated information
gs4_create("smiles-and-masks-coding-log", sheets = updated_log)
## Need to add a second sheet with variable definitions. 

#### Move videos into RA's folders based on assignments #########################

# Loop through unique subject IDs and put videos in their new homes

for (i in 1:length(dat$coder_1_assignment)) {
  # Assign variables
  child_id <- dat$child_ids[i] # The first child's ID in the dataframe
  videoFiles <- list.files(path = paste0(organizedLookitVideos, child_id)) #list all the video files for baby i
  dvFiles <- list.files(path = datavyuFiles, pattern = child_id) # list all the dv files for baby i
  
  # Filter out videos that we don't need to have in the coding folders: study completion and consent
  videoData <- data.frame(videoFiles)
  videoFiles <- videoData %>% filter(!grepl("consent", videoFiles) & !grepl("completion", videoFiles))
  videoFiles <- c(videoFiles$videoFiles)
  
  # Create the child's folder within the first coders folder
  dir.create(paste0(codingFolder, "SmilesAndMasks_", dat$coder_1_assignment[i],"/", child_id)) # Create new subject-specific folder in the RA's folder
  # Copy videos into first coder's folder
  file.copy(from = paste0(organizedLookitVideos,child_id,"/", videoFiles), to = paste0(codingFolder, "SmilesAndMasks_", dat$coder_1_assignment[i],"/", child_id)) # Copy video files
  # Copy datavyu files into first coder's folder
  file.copy(from = paste0(datavyuFiles, dvFiles), to = paste0(codingFolder, "SmilesAndMasks_", dat$coder_1_assignment[i],"/", child_id)) # Copy video files
  
  
  # Create the child's folder within the second coders folder
  dir.create(paste0(codingFolder, "SmilesAndMasks_", dat$coder_2_assignment[i],"/", child_id)) # Create new subject-specific folder in the RA's folder
  # Copy videos into second coder's folder
  file.copy(from = paste0(organizedLookitVideos,child_id,"/", videoFiles), to = paste0(codingFolder, "SmilesAndMasks_", dat$coder_2_assignment[i],"/", child_id)) # Copy video files
  # Copy datavyu files into second coder's folder
  file.copy(from = paste0(datavyuFiles, dvFiles), to = paste0(codingFolder, "SmilesAndMasks_", dat$coder_2_assignment[i],"/", child_id)) # Copy video files
  
}# end for loop


# Edit the folders for babies that did the study twice so these different "sessions" are clear to the RA, and 
# so the RA has a fresh .opf file to use for the second session. 

setwd(codingFolder)
coderFolders <- list.files(codingFolder)

for(m in 1:length(coderFolders)) {
  
  files <- list.files(coderFolders[m])
  
  for(i in 1:length(files)) {
    videos <- list.files(paste0(codingFolder, coderFolders[m], "/", files[i]), pattern = ".mp4")
    dvfiles <- list.files(paste0(codingFolder, coderFolders[m], "/", files[i]), pattern = ".opf")
    data <- data.frame(filenames = videos)
    data <- separate(data = data, col = filenames, sep = "_", into = c("X1","X2" ,"trial_type", "response_uuid"), remove = F) #warnings are fine!
    
    if( length(unique(data$response_uuid)) > 1 ) { # If there is more than one session
      # append the uuid to the opf files from the video names if you can figure it out. 
      subfolderNames <- unique(data$response_uuid)
      
      # Create separate folders for each session based on the session ID
      for (j in 1:length(subfolderNames)){
        folder <- dir.create(paste0(codingFolder, coderFolders[m], "/", files[i], "/", subfolderNames[j]) )
        file.copy(from = paste0(codingFolder, coderFolders[m], "/", files[i], "/", dvfiles), 
                  to = paste0(codingFolder, coderFolders[m], "/", files[i], "/", subfolderNames[j]) ) # Copy dv files
        # Would still like to re-name the dv files to add the uuid to the end. 
        moreVideos <-  list.files(paste0(codingFolder, coderFolders[m], "/", files[i]), pattern = subfolderNames[j])
        file.copy(from = paste0(codingFolder, coderFolders[m], "/", files[i],"/", moreVideos), to = paste0(codingFolder, coderFolders[m], "/", files[i],"/", subfolderNames[j]) ) # Copy video files
      }# subfolder j end
      
      # Remove remaining files because everything has been moved
      path <- paste0(codingFolder, coderFolders[m], "/", files[i],"/")
      file.remove(paste0(path, list.files(path)) ) # Won't delete the folders you just created
    } else {
      next()
    }
  }# for end
  
}


#### Trim the second coding assignment .opf files ##############################
# We only want RAs to code 25% of the data, which corresponds to about 2 trials per infant.
# At the moment, the RA has all of the trials in their 2nd coding folder -- so, we will 
# randomly select 2 trials from this list for the RAs to code (excluding the calibration trials), 
# and then delete the remaining trials (.opf) files. 

setwd(codingFolder)
coderFolders <- list.files(codingFolder)

for(i in 1:length(coderFolders)){
  # extract which babies are the reliability babies:
  
  # Get the RA's initials from their folder
  currentFolder <- strsplit(coderFolders[i], "_")
  currentRA <- currentFolder[[1]][2]
  
  # Find all the the current RA's reliability assignments
  currentChildIDs <- dat %>% filter(coder_2_assignment == currentRA) %>% select(child_ids) 
  
  for(j in 1:length(currentChildIDs$child_ids)) { # For each 2nd reliability baby
    # Get list of .opf files in the current folder
    dv_files <-  data.frame(files = list.files(paste0(codingFolder, coderFolders[i], "/", currentChildIDs$child_ids[j]), pattern = ".opf" ))
    dv_files2 <- dv_files %>% filter(!grepl("calibration", files))# remove calibration trials from the options
    
    if( length(dv_files2$files) >= 3) { # Check to see if this baby has at least 3 trials to begin with.
      # Randomly select 25% of trials to keep in RA's folder
      keepFiles <- sample(dv_files2$files, size = .375*length(dv_files2$files), replace = FALSE) #select 3 files without replacement
      removeFiles <- dv_files %>% filter(!files %in% keepFiles)
      
      # Remove all the files except for the 2 selected files to code:
      file.remove(paste0(codingFolder, coderFolders[i], "/", currentChildIDs$child_ids[j], "/", removeFiles$files))  # Won't delete the folders you just created
    } else {
      next()
    }#if          
  }#j 
}# i


# done!

