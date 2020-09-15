################################################################################
#   Smiles and Masks Lookit study 
#   Written by: Michaela DeBolt
#   Last edit: 9/7/20
#   Contact: mdebolt@ucdavis.edu
################################################################################

# Functions to compute reliability between two coder's data on the same child.
# These functions work on the frame-exported data from Datavyu. 

# Make sure the trials are exported with the same frame rate for each child! This
# is super important. 

library(tidyverse)

RA1 <- "GG"
RA2 <- "AD"
child_id <- "PTB42L"

#### Flexible reliability function #############################################

flexible <- function(RA1 = "RA1", RA2 = "RA2", child_id = "PTB42L") {
  
  # Set working directory to the coder's files
  setwd("~/Box/Research/Smiles_and_Masks_LookitSudy_2020/datavyu_coding/")
  # Find the RA's folder
  RA1_files <- list.files(pattern = RA1)
  RA2_files <- list.files(pattern = RA2)
  
  # List all the files that correspond to the child_id parameter 
  RA1_coded_files <- list.files(paste0(RA1_files,"/", child_id), pattern = ".csv")
  RA2_coded_files <- list.files(paste0(RA2_files,"/", child_id), pattern = ".csv")
  
  # List all of the files for the child_id and for both RA's combined
  #all_files <- c(RA1_coded_files, RA2_coded_files)
  data_list_RA1 <- list()
  data_list_RA2 <- list()
  # Read in all of the files for the child_id and both RA's data
  if( length(RA1_coded_files) == length(RA2_coded_files) ) {
    for(i in 1:length(RA1_coded_files)) {
      data_list_RA1[[i]] <- read.csv(paste0(RA1_files,"/", child_id,"/", RA1_coded_files[i]), na.strings = "") 
      data_list_RA2[[i]] <- read.csv(paste0(RA2_files,"/", child_id,"/",  RA2_coded_files[i]), na.strings = "") 
      dim(data_list_RA1[[i]])
      # Check to see if the RA's trials have the same number of frames. If not, then extend the length of the 
      # shorter trial to be as long as the longer trial. 
      if( dim(data_list_RA1[[i]])[1] != dim(data_list_RA2[[i]])[1] ){
        # Find out which trial has more frames
        max_nFrames <- max(data_list_RA1[[i]]$nFrame, data_list_RA2[[i]]$nFrame) # compare the 2 n_frames to get 
        # Make the frames equal between the two trials
        # ORRR should we make the longer one shorter??? 
        data_list_RA1[[i]] <- merge(data.frame(nFrame = seq(1:max_nFrames)), data_list_RA1[[i]], by = "nFrame", all.x = TRUE) 
        data_list_RA2[[i]] <- merge(data.frame(nFrame = seq(1:max_nFrames)), data_list_RA2[[i]], by = "nFrame", all.x = TRUE) 
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
    coder_1_looks.ordinal[[i]] <- data_list_RA1[[i]]$coder_1_looks.ordinal #Assign coder 1 looks
    coder_2_looks.ordinal[[i]] <- data_list_RA2[[i]]$coder_1_looks.ordinal #Assign coder 2 looks
    
    coder_1_looks.code01[[i]] <- data_list_RA1[[i]]$coder_1_looks.code01 #Assign coder 1 looks
    coder_2_looks.code01[[i]] <- data_list_RA2[[i]]$coder_1_looks.code01 #Assign coder 2 looks
    
    trial[[i]] <- data_list_RA1[[i]]$trial.trial_name
    n_frame[[i]] <- data_list_RA1[[i]]$nFrame
    
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
  
  # Mark agreement between looking and not-looking frames
  data$agree <- ifelse(data$coder_1_looks.ordinal==data$coder_2_looks.ordinal &
                         data$coder_1_looks.code01 == data$coder_2_looks.code01, 1,0)
  
  # Calculate reliability for this trial and store in a dataframe:
  # This maybe should be nFrame - 1 -- have to think about this.
  r <- data.frame(raw_agreement = sum(data$agree)/max(data$n_frame)*100, coder1 = RA1, coder2 = RA2, 
                  trial = data$trial[1])
  reliability[[i]] <- r
  }
  
  final_reliability <- bind_rows(reliability)
  return(final_reliability)
}  

  
#### Try it ####################################################################

save_r <- flexible(RA1 = "BB", RA2 = "GG", child_id = "PTB42L")

# GG, AD, VP, & BB -- second pass of new coders
GG_AD <- flexible(RA1 = "GG", RA2 = "AD", child_id = "PTB42L")
GG_VP <- flexible(RA1 = "GG", RA2 = "VP", child_id = "PTB42L")
GG_BB <- flexible(RA1 = "GG", RA2 = "BB", child_id = "PTB42L")

AD_VP <- flexible(RA1 = "AD", RA2 = "VP", child_id = "PTB42L")
AD_BB <- flexible(RA1 = "AD", RA2 = "BB", child_id = "PTB42L")

VP_BB <- flexible(RA1 = "VP", RA2 = "BB", child_id = "PTB42L")

# Make data frame with all possible agreements
all_r <- rbind(GG_AD, GG_VP, GG_BB, AD_VP, AD_BB, VP_BB)
all_r$both_coders <- paste0(all_r$coder1, " & ", all_r$coder2)
all_r$raw_agreement <- all_r$raw_agreement/100


# PS, JA, AW, & FQ -- 1st round of coders
PS_JA <- flexible(RA1 = "PS", RA2 = "JA", child_id = "PTB42L")
PS_AW <- flexible(RA1 = "PS", RA2 = "AW", child_id = "PTB42L")
PS_FQ <- flexible(RA1 = "PS", RA2 = "FQ", child_id = "PTB42L")

JA_AW <- flexible(RA1 = "JA", RA2 = "AW", child_id = "PTB42L")
JA_FQ <- flexible(RA1 = "JA", RA2 = "FQ", child_id = "PTB42L")

FQ_AW <- flexible(RA1 = "FQ", RA2 = "AW", child_id = "PTB42L")

# Make data frame with all possible agreements
all_r <- rbind(PS_JA, PS_AW, PS_FQ, JA_AW, JA_FQ, FQ_AW)
all_r$both_coders <- paste0(all_r$coder1, " & ", all_r$coder2)
all_r$raw_agreement <- all_r$raw_agreement/100


codersA <- c("PS", "JA", "FQ", "AW")
codersB <- c("BB", "VP", "AD", "GG")

coder_list <- expand.grid(codersB, codersA)

results <- list()
for(i in 1:nrow(coder_list)) {
  coder1 <- as.character(coder_list[i,]$Var1)
  coder2 <- as.character(coder_list[i,]$Var2)
  results[[i]] <- flexible(RA1 = coder1, RA2 = coder2, child_id = "PTB42L")
}
 all_r <- bind_rows(results)
 all_r$both_coders <- paste0(all_r$coder1, " & ", all_r$coder2)
 all_r$raw_agreement <- all_r$raw_agreement/100

 
##### Figure ###################################################################
library(ggrepel)
ggplot(data = all_r, aes(x = trial, y = raw_agreement, label = both_coders)) +
  geom_point(size = 4, alpha = .5) +
  geom_hline(yintercept = .9) +
  geom_text_repel(direction = "both") +
  ggtitle("Smiles and Masks") +
  theme(axis.text.x = element_text(angle = 90)) +
  theme(text = element_text(size=16)) +
  ylab("Percent agreement") +
  xlab("Trial name") +
  scale_y_continuous(limits = c(.10, 1), labels = scales::percent) 
ggsave("reliability_plot_V3.jpg", width = 12, height = 10)

hist(all_r$raw_agreement)  
  

flexible(RA1 = "AW", RA2 = "GG", child_id = "PTB42L")





