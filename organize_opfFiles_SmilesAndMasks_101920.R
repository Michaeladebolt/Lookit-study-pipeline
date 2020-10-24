################################################################################
#   Smiles and Masks Lookit study 
#   Written by: Michaela DeBolt
#   Last edit: 10/19/20
#   Contact: mdebolt@ucdavis.edu
################################################################################

# This code puts all the .opf files into the baby's folder containing their videos 

#### Libraries #################################################################
library(tidyverse)

#### Set various directory routes  #############################################
# Change these working directory paths to match the location of where these folders/files are organized on your computer
organizedLookitVideos <- "~/Box/Research/Smiles_and_Masks_LookitSudy_2020/lookit_videos/organized_lookit_videos_2020-10-19/"
dv_files <- "~/Box/Research/Smiles_and_Masks_LookitSudy_2020/data/processed_data/datavyu_files/" # Where all the individual .opf files are stored
# dv = datavyu
setwd(organizedLookitVideos)

#### Move .opf files into individual baby folders ##############################

setwd(dv_files)
baby_folders <- list.files(organizedLookitVideos)

for(m in 1:length(baby_folders)) {
  single_baby <- baby_folders[m]
  single_baby_dvfiles <- list.files(paste0(dv_files), pattern = single_baby)
  
  for(i in 1:length(single_baby_dvfiles)) {
    file.copy(from = paste0(dv_files, single_baby_dvfiles[i]), 
              to = paste0(organizedLookitVideos, single_baby, "/") ) # Copy dv files
  }# i for end
}