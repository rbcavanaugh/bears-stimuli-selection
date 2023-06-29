library(tidyverse)
library(tidytext)
library(textstem)
library(textclean)
library(udpipe)
library(here)

cleanVTT <- function(dat){
  
  # 1. 
  
  # Select all of the text only - ignore the timestamps and identifiers
  txt = dat %>%
    # create two new columns
    # detect the arrow, or the row before the arrow row
    mutate(arrow = ifelse(str_detect(pattern = "-->", string = WEBVTT), 1, 0),
           before_arrow = lead(arrow)) %>%
    # only keep rows with arrow and before_arrow == 0
    filter(arrow == 0, before_arrow == 0) %>%
    # get rid of quotation marks?
    mutate(text = str_remove_all(string = WEBVTT, pattern = '"'))
  
  text_string = txt %>%
    pull(text)
  
  # collapse it all together. 
  text = paste(text_string, collapse = " ")
  
  # 2. 
  
  # replace all the curly brackets and stuff in between them
  no_curly = str_replace_all(text, "\\s*\\{[^\\}]+\\}", "")
  
  nocurly_sentences = tibble(
    text = no_curly
  ) %>%
    unnest_tokens(input = "text", output = "words",
                  to_lower = TRUE, token = "regex", pattern = " ")
  
  # 3. 
  
  start_end = nocurly_sentences %>%
    mutate(start = ifelse(str_detect(words, "#start"), 1, 0),
           end = ifelse(str_detect(words, "#end"), 1, 0))
  
  stimuli_names = start_end %>%
    # what is the tag when start is 1 and then when end is 1
    mutate(tag = case_when(
      start == 1 ~ str_extract(string = words,
                               pattern = "(?<=^|\\s)#[^\\s]+"),
      end == 1   ~ str_extract(string = words,
                               pattern = "(?<=^|\\s)#[^\\s]+"))
    ) %>%
    # separate the tag into #start/#end and the stimuli name
    separate(tag, into = c("start_end", "stimuli"), sep = "_", extra = "merge") %>%
    # remove the # from before #start or #end
    mutate(start_end = str_remove(start_end, "#"))
  
  # 4. 
  
  fill_stimuli = stimuli_names %>%
    # make two columns of stimuli that are the same
    mutate(prev_stim = stimuli,
           next_stim = stimuli) %>%
    # fill one of the down and the other one up
    fill(prev_stim, .direction = "down") %>%
    fill(next_stim, .direction = "up") %>%
    # when these two columns are the same, those are the rows of our stimuli
    mutate(stimuli = ifelse(prev_stim == next_stim, prev_stim, stimuli)) %>%
    # we only need these columns
    select(words, stimuli) %>%
    # get rid of rows that are not stimuli
    drop_na(stimuli) %>%
    # remove start and end rows
    filter(
      str_detect(words, "#start", negate = TRUE),
      str_detect(words, "#end", negate = TRUE)
    ) %>%
    # remove everything after the end marker in the row
    # then remove words starting with #start
    # these appear to always be at the beginning
    mutate(# replace contractions
      words = textclean::replace_contraction(words),
      words = textclean::replace_number(words),
      words = str_replace_all(words, "'", " "),
      words = str_replace_all(words, '[[:punct:] ]+', ""))
  
  # 5. 
  
  transcript_by_stim = fill_stimuli %>%
    group_by(stimuli) %>%
    summarize(response = paste(words, collapse = " "),.groups = "drop")
  
  return(transcript_by_stim)
}


getNouns <- function(transcript_by_stim, pos = "NOUN"){
  l = list()
  
  for(i in 1:nrow(transcript_by_stim)){
    l[[i]] = udpipe(x = transcript_by_stim$response[[i]], object = "english")
    
    l[[i]]$stimuli = transcript_by_stim$stimuli[[i]]
  }
  
  tagged = bind_rows(l) %>%
    select(stimuli, token, lemma, upos, xpos)
  
  #print(unique(tagged$upos))
  
  unique_nouns = tagged %>%
    filter(upos == pos) %>%
    group_by(stimuli) %>%
    summarize(lemma = unique(lemma), .groups = "drop")
  
  return(unique_nouns)
}


