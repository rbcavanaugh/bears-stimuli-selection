############## RUN THIS SCRIPT TO CLEAN ALL FILES IN A FOLDER #################

# This is an extra version of the initial file used to check all files for errors
# It is not a part of the processing stream. 

library(tidyverse)
library(tidytext)
library(textstem)
library(textclean)
library(udpipe)
library(here)
library(glue)

# When you run this line, it will print a file path to the console below. 
# make sure it ends in bears-transcription-parsing
# If it doesn't, you haven't opened the R project file correctly (see above)
here()

# these two lines of code find all of the files in the data folder ending in vtt
filepaths = list.files(here("VTT_updated_813"), full.names = TRUE, pattern = "vtt")
files = list.files(here("VTT_updated_813"), full.names = FALSE, pattern = "vtt")

# Note 3: To retrieve parts of speech other than nouns, replace "NOUN" in the
# code below in the getNouns() function with one of the following:
# "INTJ"  "PUNCT" "VERB"  "PRON"  "DET"   "NOUN"  "AUX"   "ADP"   "ADJ"
# "CCONJ" "ADV"   "NUM"  "PART"  "PROPN" "SCONJ" "X"  "SYM"  
part_of_speech = "NOUN"

# This loads the two key functions that clean up the VTT files
source(here("R", "functions.R"))

# This creates an empty list for adding cleaned data to
nounList = list()

# Loops through the files and cleaned and saves their nouns
for(i in seq_along(files)){
  
  # participant and session info from the filename
  participant = strsplit(files[i], split = "_")[[1]][1]
  session = strsplit(files[i], split = "_")[[1]][2]
  
  # clean up the files (function in functions.R)
  cleaned = cleanVTT(read.delim(filepaths[i], sep = "\t"))
  # get the nouns (function in functions.R)
  # nouns = getNouns(cleaned, pos = part_of_speech)
  # 
  # # add participant,  session, and the filename to the dataframe that is returned
  cleaned$participant = participant
  cleaned$session = session
  cleaned$file = files[i]
  # 
  # # reorder the columns and ungroup b/c I'm picky
  # nouns <- nouns |> select(file, participant, session, stimuli, lemma) |> ungroup()
  # 
  # cat(glue("got nouns for {participant}, {session}"), "\n")
  # append the result to the list
  nounList[[i]] = cleaned
}

allNouns = bind_rows(nounList)
# add all the cleaned files together
allNouns = bind_rows(nounList) |> 
  mutate(stimuli = str_replace_all(stimuli, "-", "_"),
         stimuli = ifelse(str_detect(stimuli,"ghouls"),
                          "dinosaurs_spacemen_and_ghouls",
                          stimuli))

allNouns |> count(stimuli) |> nrow() == 26

# save the result
# write.csv(allNouns, file = here("output", paste0(Sys.Date(), "_allNouns.csv")), row.names = FALSE)
# allNouns <- read.csv(here("output", "2023-08-13_allNouns.csv"))

# how many participants are there?
numParticipants = length(unique(allNouns$participant))

# Lets get a summary dataframe that will be important for calculating corelex
nounCounts = allNouns |> 
  count(stimuli, lemma) |> 
  mutate(percent = (n/numParticipants)*100)

# save the result
# write.csv(nounCounts, file = here("output", paste0(Sys.Date(), "_nounCounts.csv")), row.names = FALSE)

nounCountsbyThreshold = 
  nounCounts |> 
  mutate(
    "threshold: 33%" = ifelse(percent >= 33.33, 1, 0),
    "threshold: 40%" = ifelse(percent >= 40.00, 1, 0),
    "threshold: 50%" = ifelse(percent >= 50.00, 1, 0),
    "threshold: 60%" = ifelse(percent >= 60.00, 1, 0),
    "threshold: 67%" = ifelse(percent >= 66.66, 1, 0)
  ) |> 
  pivot_longer(cols = "threshold: 33%":"threshold: 67%",
               names_to = "threshold",
               values_to = "passed") |> 
  group_by(stimuli, threshold) |> 
  summarize(passed_threshold = sum(passed), .groups = "drop")

# write.csv(nounCountsbyThreshold,
#           file = here("output", paste0(Sys.Date(), "_nounCountsThreshold.csv")), row.names = FALSE)


# split up into a file per stimuli (the nounCounts object) and save as excel files
# with new columns for 
# is a noun
# is picturable
# something went wrong check this word

nounsOut <- nounCounts |> 
  arrange(desc(n)) |> 
  mutate(
    isPicturable = "",
    notNoun = "",
    checkThisWord = "",
    duplicateOf = "",
    synonymOf = ""
  )

stimuli = unique(nounsOut$stimuli)

for(i in stimuli){
  tmp = nounsOut |> filter(stimuli == i)
  filename = paste0(i, ".csv")
  # uncomment the next line to resave files. 
   write.csv(tmp, file = here("check-nouns", filename), row.names = FALSE)
}

# 
# Additional task
# We need to know how many unique contributions each stimulus is making to the
# overall word bank. so how many 'value added' words that are not shared