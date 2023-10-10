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
      words = str_replace_all(words, '[[:punct:]]+', ""))
  
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

# Function for Yukki's Thesis
getNouns_withCount <- function(transcript_by_stim, pos = "NOUN"){
  l = list()
  
  for(i in 1:nrow(transcript_by_stim)){
    l[[i]] = udpipe(x = transcript_by_stim$response[[i]], object = "english")
    
    l[[i]]$stimuli = transcript_by_stim$stimuli[[i]]
  }
  
  tagged = bind_rows(l) %>%
    select(stimuli, token, lemma, upos, xpos)
  
  #print(unique(tagged$upos))
  
 #  https://dplyr.tidyverse.org/reference/count.html
 #  the function you want here is called count()
 #  you might want to use select() to pick only the columns you need
 #  before you use count(). 
  unique_nouns = tagged %>%
    filter(upos == pos) %>%
    #
    #
    #
    # These are the two lines to change
    # 
    # group_by(stimuli) %>%
    # summarize(lemma = unique(lemma), .groups = "drop")
    # 
    # what I could have done
    # unique_nouns = tagged %>%
    #   filter(upos == pos) %>%
    #   distinct(stimuli, lemma)
  
  return(unique_nouns)
}


getTimestamp<- function(dat){
  dat %>%
    # create two new columns
    # detect the arrow, or the row before the arrow row
    filter(str_detect(string = tolower(WEBVTT), pattern = paste(c("#start", "#end", "-->"),collapse = '|'))) %>% 
    mutate(arrow = ifelse(str_detect(pattern = "-->", string = WEBVTT), 1, 0),
           before_arrow = lead(arrow),
           start = ifelse(str_detect(pattern = "#start", string = tolower(WEBVTT)), 1, 0),
           before_start = lead(start),
           end = ifelse(str_detect(pattern = "#end", string = tolower(WEBVTT)), 1, 0),
           before_end = lead(end)) %>% 
    # only keep rows with arrow and before_arrow == 0
    filter(start == 1 | end == 1 | before_start == 1 | before_end == 1) %>%
    # get rid of quotation marks?
    mutate(text = tolower(str_remove_all(string = WEBVTT, pattern = '"')))  %>% 
    select(text, start, end) %>% 
    mutate(timestamp = ifelse(str_detect(text, "-->"), text, NA)) %>% 
    fill(timestamp) %>%
    filter(str_detect(string = tolower(text), pattern = paste(c("#start", "#end"),collapse = '|'))) %>% 
    separate(timestamp, sep = " --> ", into = c("utt_start_time", "utt_end_time")) %>% 
    mutate(tag = case_when(
      start == 1 ~ str_extract(string = text,
                               pattern = "(?<=^|\\s)#[^\\s]+"),
      end == 1   ~ str_extract(string = text,
                               pattern = "(?<=^|\\s)#[^\\s]+"))
    ) %>%
    # separate the tag into #start/#end and the stimuli name
    separate(tag, into = c("start_end", "stimuli"), sep = "_", extra = "merge") %>%
    # remove the # from before #start or #end
    mutate(start_end = str_remove(start_end, "#")) %>% 
    mutate(count = row_number(), .by = stimuli) %>%
    mutate(occurrence = case_when(
      count <= 2 ~ 1,
      count <= 4 ~ 2,
      count <= 6 ~ 3,
      TRUE ~ 4
    )) %>%
    select(stimuli, start_end, utt_start_time, utt_end_time, occurrence) %>% 
    mutate(timestamp = ifelse(start_end == "start", utt_start_time, utt_end_time)) %>%
    select(stimuli, start_end, timestamp, occurrence) %>% 
    mutate(timestamp = strptime(timestamp, "%H:%M:%OS")) %>%
    pivot_wider(names_from = start_end, values_from = timestamp) %>%
    mutate(duration = end-start)
}

