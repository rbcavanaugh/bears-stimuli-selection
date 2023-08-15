############## RUN THIS SCRIPT TO CLEAN ALL FILES IN A FOLDER #################

# Hi, welcome. This script process all .vtt files in the /data folder in
# this directory. I've hopefully left fairly clear instructions for how
# the script works and how to use it. 
# 
# First, make sure that you have opened the R project before running the script
# To check that you've done so, you should see "bears-transcription-parsing"
# next to the blue cube in the top right of RStudio. the here() command below
# should also split out a file path that ends in bears-transcription-parsing. 
# If not, navigate to this folder in RStudio (bottom right pane), finder (mac)
# or file explorer (windows) and open the project by clicking (or double clicking)
# on the blue cube with an R in it. 
# 
# After you've made sure you opened the R project, the next step is to copy
# all of the completed and corrected VTT files into the "data" folder in 
# this R project (bears-transcription-parsing/data). All of the files in this
# folder will be cleaned and parsed. 

# Load the packages below. You will need to install them first if they
# have never been installed on your computer before. 
# You can do install.packages("here") in the console (for example)
# or go to the tools menu and install packages. Once they are installed on 
# your computer, you should not have to do this step again
# 
# Note1: to run a line of code, put your cursor at the end of it and hit 
# command + enter (mac)  or ctrl + enter (windows). shift + cmd + enter (mac) 
# or shift + ctr + enter (windows) will run the whole thing. 
# 
# Note2: If you have put all the files in the folder and otherwise are ready
# to go, you can also just select all and hit the run button above. The
# results will be saved in the output folder. Note that this will overwrite
# any previous results from TODAY (previous days results will not change). 
# If you need to save different files today, change the write.csv() functions 
# near the bottom to have unique file names

library(tidyverse)
library(tidytext)
library(textstem)
library(textclean)
library(here)
library(glue)

# When you run this line, it will print a file path to the console below. 
# make sure it ends in bears-transcription-parsing
# If it doesn't, you haven't opened the R project file correctly (see above)
here()

# these two lines of code find all of the files in the data folder ending in vtt
filepaths = list.files(here("VTT_updated_813"), full.names = TRUE, pattern = "vtt")
files = list.files(here("VTT_updated_813"), full.names = FALSE, pattern = "vtt")

# This loads the two key functions that clean up the VTT files
source(here("R", "functions.R"))

# This creates an empty list for adding cleaned data to
tsList = list()

# Loops through the files and cleaned and saves their nouns
for(i in seq_along(files)){
  
  # participant and session info from the filename
  participant = strsplit(files[i], split = "_")[[1]][1]
  session = strsplit(files[i], split = "_")[[1]][2]
  
  # clean up the files (function in functions.R)
  timestamp = getTimestamp(read.delim(filepaths[i], sep = "\t"))
  # get the nouns (function in functions.R)
  
  # add participant,  session, and the filename to the dataframe that is returned
  timestamp$participant = participant
  timestamp$session = session
  timestamp$file = files[i]
  
  # append the result to the list
  tsList[[i]] = timestamp %>% select(participant, file, session, stimuli, occurrence, start, end, duration)
}

# add all the cleaned files together
stim = bind_rows(tsList)  |> 
  mutate(stimuli = str_replace_all(stimuli, "-", "_"),
         stimuli = ifelse(str_detect(stimuli,"ghouls"),
                          "dinosaurs_spacemen_and_ghouls",
                          stimuli))

test = stim %>% filter(is.na(duration))
stim |> count(stimuli) -> test
write.csv(stim, here::here("output", paste0(Sys.Date(), "_timestamp.csv")), row.names = FALSE)
write.csv(stim %>% filter(is.na(duration)), paste0("output/", Sys.Date(), "_timestamp-errors.csv"), row.names = FALSE)

test = stim %>% filter(is.na(duration)) %>% drop_na(stimuli)

# this should print to zero if they are all ok!
nrow(test)

