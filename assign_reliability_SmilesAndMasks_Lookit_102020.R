################################################################################
#   Smiles and Masks Lookit study 
#   Written by: Michaela DeBolt
#   Last edit: 10/20/20
#   Contact: mdebolt@ucdavis.edu
################################################################################

# The code reads in the coding assignment log and determines which babies have been coded
# by the primary coder, and then moves 2 test trials and 1 calibration trial into the 
# second coder's folder for reliability purposes.


#### Libraries #################################################################
library(tidyverse)
library(googlesheets4)

#### Set various directory routes  #############################################
# Change these working directory paths to match the location of where these folders/files are organized on your computer
organizedLookitVideos <- "~/Box/Research/Smiles_and_Masks_LookitSudy_2020/lookit_videos/organized_lookit_videos_2020-10-19/"
codingFolders <- "~/Box/Research/Smiles_and_Masks_LookitSudy_2020/datavyu_coding/"
setwd(codingFolders)

#### Read in the coding assignment Google sheet ################################
# Already-coded babies:
googlesheets4::gs4_auth(email = "michaeladebolt@gmail.com")

log <- read_sheet(ss = "https://docs.google.com/spreadsheets/d/1e2uYY-hlBQaREzYPKf0_bEe0Y9GUxzeO3fdITv19XKk/edit#gid=0",
                           col_names = T, na = "NULL")

log <- log[-c(1:2),] # Remove the column name definition row -- This column is super important though because it sets the data
# type for the entire column, which is used below.

#### Assign specific trials to the second coder ################################

# Note that the coder 1 must have the files at least in their folder -- they don't have to be coded but they need to be in their
# folder, or else the loop will fail. 

for(i in 1:nrow(log)) {
  if(!is.na(log$coder_2_assignment[i]) & log$coder_2_completed[i]=="NULL") { # If 2nd coder has been assigned, but not yet coded.
    # locate the coded .opf files (and the video files for later):
    coded_files <- data.frame(files = list.files(paste0(codingFolders,"SmilesAndMasks_",log$coder_1_assignment[i], "/", log$child_id[i]), pattern = ".opf") ) 
    video_files <- data.frame(files = list.files(paste0(codingFolders,"SmilesAndMasks_",log$coder_1_assignment[i], "/", log$child_id[i]), pattern = ".mp4") ) 
    
    # Make smaller data frames and force type to be character
    test_files <- coded_files %>% filter(!grepl("calibration", files)) 
    test_files$files <- as.character(test_files$files)
    
    cal_files <- coded_files %>% filter(!grepl("test", files)) 
    cal_files$files <- as.character(cal_files$files)

    #set.seed(8675309)
    # randomly select 1 calibration and 2 test trials:
    rel_files <- c(sample(cal_files$files, 1, replace = FALSE), sample(test_files$files, 2, replace = FALSE))
    
    # accompanying video names based on above selections
    vid_hints <- gsub(x = gsub(pattern = paste0(log$child_id[i], "_"), replacement = "", x = rel_files), pattern = ".opf", replacement = "") 
    #video_files %>% filter(!grepl(vid_hints, files)) 
    
    # Check to see if the folder already exists (from a previous run of the code)
    existing_folders <- data.frame(folders = list.files(paste0(codingFolders, "SmilesAndMasks_", log$coder_2_assignment[i])))
    current_id <- log$child_id[i]

    # Check in the folders to make sure the data aren't already in there.
    if(nrow(existing_folders %>% filter(folders == log$child_id[i])) == 0 ) {
      
      # create folder for the 2nd coder to store new files
      dir.create(paste0(codingFolders, "SmilesAndMasks_", log$coder_2_assignment[i], "/", log$child_id[i])) # Create new subject-specific folder in the RA's folder
      
      # Move fresh .opf files in the 2nd coder's folder
      file.copy(from = paste0( organizedLookitVideos, log$child_id[i], "/", rel_files), 
                to = paste0(codingFolders, "SmilesAndMasks_", log$coder_2_assignment[i], "/", log$child_id[i]) ) 
      
      # Copy the video files over while we're at it:
      slected_vids <- video_files[grepl(paste(vid_hints, collapse = "|"), video_files$files),]
      file.copy(from = paste0( organizedLookitVideos, log$child_id[i], "/", slected_vids), 
                to = paste0(codingFolders, "SmilesAndMasks_", log$coder_2_assignment[i], "/", log$child_id[i]) ) 
      
    }else{
      next()
    }
    
  } else{ 
    next()
  }
}
