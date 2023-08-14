library(tidyverse)
library(here)
library(tidyverse)
library(tidytext)
library(textstem)
library(textclean)
library(udpipe)
library(here)
library(glue)

naming <- read.csv(here("data", "naming-battery-items.csv"))

spaces <- naming |> filter(str_detect(modal_with_spaces, " ")) |> 
  select(modal_with_spaces)

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
  # add participant,  session, and the filename to the dataframe that is returned
  cleaned$participant = participant
  # 
  # # reorder the columns and ungroup b/c I'm picky
  # nouns <- nouns |> select(file, participant, session, stimuli, lemma) |> ungroup()
  # 
  # cat(glue("got nouns for {participant}, {session}"), "\n")
  # append the result to the list
  nounList[[i]] = cleaned
}

# add all the cleaned files together
all = bind_rows(nounList) |> 
  mutate(stimuli = str_replace_all(stimuli, "-", "_"),
         stimuli = ifelse(stimuli == "dinasours_spacemen_and_ghouls",
                          "dinosaurs_spacemen_and_ghouls", 
                          stimuli))

search = expand_grid(all, spaces) |> 
  mutate(found = str_detect(response, modal_with_spaces))

# 32 participants
# 
multiword <- search |> 
  group_by(modal_with_spaces, stimuli) |> 
  summarize(total_found = sum(found),
            percent_found = total_found/32) |> 
  arrange(desc(percent_found))

write.csv(multiword, here::here("data", "multiword.csv"),row.names = FALSE)
