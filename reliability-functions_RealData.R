################################################################################
#   Smiles and Masks Lookit study 
#   Written by: Michaela DeBolt
#   Last edit: 10/22/20
#   Contact: mdebolt@ucdavis.edu
################################################################################

# This files was intended to use on the real coded data, because the second coder
# is only going to be coding a portion of what the first coder coded, the original 
# reliability script needed to be adjusted to account for this.

# Functions to compute reliability between two coder's data on the same child.
# These functions work on the frame-exported data from Datavyu. 

# This function requires that the data has been exported into .txt. files and are
# delimited with a bracket " ] ", using the "FrameExport.rb" file. Exporting the 
# frame data with FrameExport.rb will ensure they are exported with a specific frame
# rate. 


library(tidyverse)
library(stringr)

RA1 <- "GG"
RA2 <- "MD"
child_id <- "ZDVCQ2"
i = 2

#### Flexible reliability function #############################################

flexible <- function(RA1 = "RA1", RA2 = "RA2", child_id = "PTB42L") {
  
  # Set working directory to the coder's files
  #setwd("~/Desktop/datavyu_coding/")
  setwd("~/Box/Research/Smiles_and_Masks_LookitSudy_2020/datavyu_coding/")
  # Find the RA's folder
  RA1_files <- list.files(pattern = RA1)
  RA2_files <- list.files(pattern = RA2)
  
  # List all the files that correspond to the child_id parameter 
  RA1_coded_files <- data.frame(files = list.files(paste0(RA1_files,"/", child_id), pattern = ".txt") )
  RA2_coded_files <- data.frame(files = list.files(paste0(RA2_files,"/", child_id), pattern = ".txt") )
  
  # Remove initials from the file names so they are identical between coders:
  RA1_coded_files$files <- gsub(pattern = paste0("_",RA1), x = RA1_coded_files$files, replacement = "")
  RA2_coded_files$files <- gsub(pattern = paste0("_",RA2), x = RA2_coded_files$files, replacement = "")
  
  # Remove the trials from the 1st coder that were not coded by the 2nd coder:
  RA1_coded_files <- RA1_coded_files %>% filter(files %in% RA2_coded_files$files)
  
  # Add the coder's initials back to the file names, or else this throws an error later when reading in data
  RA1_coded_files$files <- paste0(str_sub(RA1_coded_files$files,1, -5), "_", RA1, ".txt") 
  RA2_coded_files$files <- paste0(str_sub(RA2_coded_files$files,1, -5), "_", RA2, ".txt") 
  
  # Change these dataframes back into lists to be used in the loops below:
  RA1_coded_files <-unlist(as.list(RA1_coded_files$files))
  RA2_coded_files <-unlist(as.list(RA2_coded_files$files))

  # List all of the files for the child_id and for both RA's combined
  #all_files <- c(RA1_coded_files, RA2_coded_files)
  data_list_RA1 <- list()
  data_list_RA2 <- list()
  # Read in all of the files for the child_id and both RA's data
  if( length(RA1_coded_files) == length(RA2_coded_files) ) {
    for(i in 1:length(RA1_coded_files)) {
      # RA1
      data_list_RA1[[i]] <- read.csv(paste0(RA1_files,"/", child_id,"/", RA1_coded_files[i]), na.strings = "", sep = "]")
      # RA2
      data_list_RA2[[i]] <- read.csv(paste0(RA2_files,"/", child_id,"/", RA2_coded_files[i]), na.strings = "", sep = "]") 
      
      dim(data_list_RA1[[i]])
      dim(data_list_RA2[[i]])
      # Check to see if the RA's trials have the same number of frames. If not, then extend the length of the 
      # shorter trial to be as long as the longer trial. 
      if( dim(data_list_RA1[[i]])[1] != dim(data_list_RA2[[i]])[1] ){
        # Find out which trial has more frames
        max_nFrames <- max(data_list_RA1[[i]]$framenum, data_list_RA2[[i]]$framenum) # compare the 2 n_frames to get 
        # Make the frames equal between the two trials
        # ORRR should we make the longer one shorter??? 
        data_list_RA1[[i]] <- merge(data.frame(framenum = seq(1:max_nFrames)), data_list_RA1[[i]], by = "framenum", all.x = TRUE) 
        data_list_RA2[[i]] <- merge(data.frame(framenum = seq(1:max_nFrames)), data_list_RA2[[i]], by = "framenum", all.x = TRUE) 
      } else {
        next() #move on
      }
    }# for  
  } else{
    print("RA's don't have the same number of coded trials for this child!")
  }
  
  # Set up a lot of storage lists:
  coder_1_looks.ordinal <- list()
  coder_2_looks.ordinal <- list()
  coder_1_looks.code01 <- list()
  coder_2_looks.code01 <- list()
  trial <- list()
  n_frame <- list()
  all_trials <- list()
  reliability <- list()
  # Combine each trial for each RA:
  for(i in 1:length(data_list_RA1)) {
    coder_1_looks.ordinal[[i]] <- data_list_RA1[[i]]$coder_1_looks_ordinal #Assign coder 1 looks
    coder_2_looks.ordinal[[i]] <- data_list_RA2[[i]]$coder_1_looks_ordinal #Assign coder 2 looks
    
    coder_1_looks.code01[[i]] <- data_list_RA1[[i]]$coder_1_looks_code01 #Assign coder 1 looks
    coder_2_looks.code01[[i]] <- data_list_RA2[[i]]$coder_1_looks_code01 #Assign coder 2 looks
    
    trial[[i]] <- data_list_RA1[[i]]$trial_trial_name
    n_frame[[i]] <- data_list_RA1[[i]]$framenum
    
    temp <- data.frame(bind_cols( coder_1_looks.ordinal = coder_1_looks.ordinal[[i]],  
                                  coder_1_looks.code01 = coder_1_looks.code01[[i]], 
                                  coder_2_looks.ordinal = coder_2_looks.ordinal[[i]],
                                  coder_2_looks.code01 = coder_2_looks.code01[[i]],
                                  trial = trial[[i]]), n_frame = n_frame[[i]] ) 
    
    all_trials[[i]] <- temp
    }
  
  for(i in 1:length(all_trials)) {
  data <- all_trials[[i]] 
  # Mark instances of looking off screen/not looking/cells that were simply not coded
  data$coder_1_looks.ordinal <- tidyr::replace_na(data = data$coder_1_looks.ordinal, replace = "offscreen")
  data$coder_1_looks.code01 <- tidyr::replace_na(data = as.character(data$coder_1_looks.code01), replace = "offscreen")
  
  data$coder_2_looks.ordinal <- tidyr::replace_na(data = data$coder_2_looks.ordinal, replace = "offscreen")
  data$coder_2_looks.code01 <- tidyr::replace_na(data = as.character(data$coder_2_looks.code01), replace = "offscreen")
  
  # Mark agreement between looking and not-looking frames -- this requires the ordinal AND looking value to be the same
  # data$agree <- ifelse(data$coder_1_looks.ordinal==data$coder_2_looks.ordinal &
  #                        data$coder_1_looks.code01 == data$coder_2_looks.code01, 1,0)
  
  # Less conservative agreement -- does not require the ordinal value to be the same
  data$agree <- ifelse(data$coder_1_looks.code01 == data$coder_2_looks.code01, 1,0)
  
  # Calculate reliability for this trial and store in a dataframe:
  r <- data.frame(raw_agreement = sum(data$agree)/max(data$n_frame)*100, coder1 = RA1, coder2 = RA2, 
                  trial = data$trial[1])
  reliability[[i]] <- r
  }
  final_reliability <- bind_rows(reliability)
  return(final_reliability)
}  

#### Try it ####################################################################

results <- flexible(RA1 = "BB", RA2 = "SK", child_id = "QVXNAC") # good
mean(results$raw_agreement)

results <- flexible(RA1 = "GG", RA2 = "MD", child_id = "Y5E66Z") # good
mean(results$raw_agreement)

results <- flexible(RA1 = "SK", RA2 = "MD", child_id = "Y5E66Z") # good
mean(results$raw_agreement)

results <- flexible(RA1 = "GG", RA2 = "SK", child_id = "Y5E66Z") # good
mean(results$raw_agreement)

results <- flexible(RA1 = "GG", RA2 = "MD", child_id = "ZDVCQ2") # good
mean(results$raw_agreement)

results <- flexible(RA1 = "GG", RA2 = "MD", child_id = "ZHHXWU") # good
mean(results$raw_agreement)

results <- flexible(RA1 = "GG", RA2 = "MD", child_id = "ZHMBXX") # good
mean(results$raw_agreement)



