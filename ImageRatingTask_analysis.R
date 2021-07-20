#%%%%%%%%%%%%%%%%%%%%%%% IMAGE RATING TASK %%%%%%%%%%%%%%%%%%%%%%%
#%%%%%%%%%%%%%%%%%%%%%%   Analysis script   %%%%%%%%%%%%%%%%%%%%%%



#----------------------- General info:

#   Pilot study for TMR sleep study
#   Task: sample emotional ratings for pre-screened image set 
#   Goal: validate final selection of images for each category to be used in TMR / localizer task


#----------------------- Original design versions:

#   Lenovo Ideapad330 x64
#   Processor Intel(R) Core(TM) i5-8250U CPU @ 1.60GHz / 1800 Mhz / 4 Core(s) / 8 Logical Processor(s)
#   Microsoft Windows 10 Home - Version 10.0.19042 Build 19042
#   R version 3.6.3 (2020-02-29)
#   RStudio Version 1.3.1073

#----------------------- Edit log

# 16/07/21: script created [LS]


#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

#----------------------- Install/import libraries

library(dplyr)
library(tidyr)
library(rstudioapi)

#----------------------- Script

### Import data from csv

# SET WORKING DIRECTORY TO SOURCE FILE LOCATION - manually (Menu > Session) or CLI: setwd(".../Leila2Sarah/IRT_DATA")

filelist = dir(pattern="csv")

path = "3_ImageRatingTask_S1_2021-07-08_11h02.59.595.csv"    # Define file path

dfin = read.csv(path, header = TRUE, sep = ",") # Read csv into data frame

glimpse(dfin)  # Easy visualization of vars


### Select data of interest

# Once per participant
part = first(dfin$participant)
age = as.numeric(first(dfin$age))
gender = as.character(first(dfin$gender))
hand = as.character(first(dfin$handedness..L.R.))
session = substr(as.character(first(dfin$expName)),17,18)

# Once per session
premood = as.numeric(first(na.omit(dfin$slider_pre.response)))
postmood = as.numeric(first(na.omit(dfin$slider_post.response)))
diffmood = postmood-premood
comments = first(na.omit(dfin$textbox_openfb.text))

df = dfin %>% select(matches('keys$|filename')) %>% select(!starts_with('test')) # Selection main data

# Parse image name

imgs = array(unique(df$filename)) # extract file names from file
imagename = c() # create empty list

for (i in imgs[2:length(imgs)]){
  i = as.character(i)
  name = strsplit(strsplit(i, "/")[[1]][3],".jpg")
  imagename = c(imagename, name)
}

imagename = unlist(imagename)
participant = rep(part, times=length(imagename))

# Extract data of interest

valence = df$key_val.keys[!is.na(df$key_val.keys)]
arousal = df$key_aro.keys[!is.na(df$key_aro.keys)]
quality = df$key_qual.keys[!is.na(df$key_qual.keys)]
ncategs = df$key_ncat.keys[!is.na(df$key_ncat.keys)]
categs = df$key_cats.keys[!is.na(df$key_cats.keys)]

labcategs = factor(categs, levels = c(1,2,3,4,5,6,7,8), labels = c("Adults (human)","Children","Mammals","Animals (non-mammal)","Food","Buildings","Vehicles","Water"))
labcategs = levels(labcategs)[as.numeric(labcategs)]

tocheck = c()

for(i in 1:8){
  assign(paste0("cat",i), rep(0,times=length(imagename)))
}

while (i <= length(ncategs)){
  







i = 1

while (i <= length(ncategs)){
  if (ncategs[i] == 0){
    tocheck = c(tocheck, imagename[i])
  }
  if (ncategs[i] == 1){
    cat1 = append(cat1, list(categs[i]))
  }
  if (ncategs[i] > 1){
    tocheck = c(tocheck, imagename[i])
    idx = i + ncategs[i] - 1
    cats = append(cats, list(labcategs[i:idx]))
  }
  i = i + 1
}

#while (i <= length(ncategs)){
#  if (ncategs[i] == 0|(ncategs[i] > 1)){
#    tocheck = c(tocheck, imagename[i])
#  }
#  idx = i + ncategs[i] - 1
#  for (c in [i:idx]){
#    paste0("cat",i) = 
#  }
#  i = i + 1
#}

dfout = data.frame(participant,imagename,valence,arousal,quality,ncategs) # create empty output data frame
