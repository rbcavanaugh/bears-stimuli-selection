
# So you want to update the files used by the shiny app? This is where to start.

# This file has naming item data - the word, modal, filenames for the webapp.
# source of the image, agreement. If you update this file, then you should also
# re-run R/04-join-naming-to-discourse.R.
# save it in bears-stimuli-selection/data AND in bears-stimuli-selection/shiny/data/ 
# (need to save in both locations)
naming_database_file = "words-2024-01-29.csv" #  "final_database_4-11-23.csv"

# this is the output of 04-join-naming-to-discourse.R
# if there are any changes to final_database_04-11-23.csv, 
# or the VTT files / the nounCounts files from discourse
# then this file should be updated by re-running 04-join-naming-to-discourse.R.
discourse_naming_joined_file = "join_checked_automated.csv"

# this is the output of 03-timestamp-norming.R
# If the VTT files change, then it should be updated by re-running 03-timestamp-norming.R
# Most likely this file will not need to change
discourse_timestamp_file = "2023-08-14_timestamp.csv"

# this is the output of 05-merge-difficulty-variables.R. If we do more
# norming on items so that we have more data on age of acquisition or
# NPhon or lexical frequency, update the input files in that script
# and re-run it to update this file.
naming_item_parameter_file = "AoA-phonemes-freq_combined_2023_11_02.csv"



read_in_all_files <- function(shiny,
                              min_naming_agreement = 75,
                              min_discourse_salience = 33,
                              target_prob_correct = 0.33,
                              min_discourse_stimuli = 9,
                              min_discourse_items = 54,
                              total_tx_items = 180){
  
  #print(here::here())
  local = str_detect(here::here(), "github")
  
  if(!isTRUE(shiny)){
    
    naming_file = here("shiny", "data", naming_database_file)
    discourse_naming_joined_file = here("shiny", "data", discourse_naming_joined_file)
    timestamp_file = here("shiny", "data", discourse_timestamp_file)
    naming_parameters_file = here("shiny", "data", naming_item_parameter_file)
    
  } else if(isTRUE(local)) {
    
    naming_file = here("shiny", "data", naming_database_file)
    discourse_naming_joined_file = here("shiny", "data", discourse_naming_joined_file)
    timestamp_file = here("shiny", "data", discourse_timestamp_file)
    naming_parameters_file = here("shiny", "data",
                                  naming_item_parameter_file)
    
  } else {
    naming_file = here("data", naming_database_file)
    discourse_naming_joined_file = here("data", discourse_naming_joined_file)
    timestamp_file = here("data", discourse_timestamp_file)
    naming_parameters_file = here("data",
                                  naming_item_parameter_file)
  }
  
  # any dataframe that holds the word, the source, and the agreement scores...
  naming <- suppressMessages(read_csv(naming_file, col_types = cols())) |> 
    select(lemma = modal, source, agreement, filename = picture) |> 
    group_by(lemma) |> slice_sample(n = 1) |> 
    distinct() |> 
    filter(agreement >= min_naming_agreement) |> 
    group_by(lemma) |> 
    mutate(source = paste(source, collapse = ", ")) |> 
    distinct()
  
  # this is a joined df where I hand chekced the fuzzy join in 01-new-approach
  # between discourse and naming. it only adds a few items...
  
  fuzz_join = suppressMessages(read_csv(discourse_naming_joined_file,
                                        col_types = cols())) |> 
    filter(match == 1 | dist == 0) |> 
    select(source:lemma_dis) |> 
    filter(percent >= min_discourse_salience, agreement >= min_naming_agreement) |> 
    select(source, lemma=lemma_naming, stimuli, percent, lemma_dis) |> 
    distinct()
  
  # total number of unique found words between naming and discourse
  # not saved
  fuzz_join |> 
    distinct(lemma) |> nrow()
  
  # get the average time to produce of each stimuli for more balancing
  # used a few times later on
  times <- read_csv(timestamp_file, col_types = cols()) |> 
    mutate(stimuli = str_replace_all(stimuli, "-", "_"),
           stimuli = ifelse(str_detect(stimuli,
                                       "ghouls"),
                            "dinosaurs_spacemen_and_ghouls",
                            stimuli)) |> 
    group_by(stimuli) |> 
    summarize(m_time = mean(duration),
              me_time = median(duration),
              sd_time = m_time + sd(duration))
  
  
  # dataframe holding AoA, phonemes, lexical freq necessary to calculate item difficulty
  # Has changed a few times
  # item_params = read.csv("data/item_parameters.csv")
  # updated to new merged sheet with a few more items...
  # item_params = read.csv("data/AoA-phonemes-freq_joined_2023-08-02.csv") |> 
  # updated again...
  # 
  item_params = 
    suppressWarnings(
      read_csv(naming_parameters_file) |> 
        select(Word, LgSUBTLCD, Age_Of_Acquisition, NPhon) |> 
        mutate(LgSUBTLCD = readr::parse_number(LgSUBTLCD))
        #        #NPhon = readr::parse_number(NPhon),
        #        Age_Of_Acquisition = readr::parse_number(Age_Of_Acquisition)) 
    )
  
  # join together and calculate item difficulty
  # For T-scale difficulty to match the shiny app:
  # The equation would be:  39.47 – 2.01*Lg10CD + 1.08*NumberOfPhonemes + 1.45*AoA
  # - Hula, 8-19-2023
  diff = naming |> 
    full_join(item_params, by = c("lemma" = "Word")) |> 
    left_join(fuzz_join, by = join_by(lemma, source), relationship = "many-to-many") |> 
    mutate(difficulty = 39.47 + 1.08*NPhon - 2.01*LgSUBTLCD + 1.45*Age_Of_Acquisition)
  # old irt scale not used anymore
  #mutate(difficulty = -1.22 + 0.15*NPhon -0.36*LgSUBTLCD + 0.21*Age_Of_Acquisition)
  
  # best agreement from naming with a difficulty score. should go up in
  # size after getting more difficulty scores...hopefuly
  cl = diff |> 
    select(lemma_naming = lemma, stimuli, agreement, percent,
           difficulty, filename) |> #, target
    mutate(in_discourse = ifelse(is.na(stimuli), 0, 1)) |> 
    group_by(lemma_naming) |> 
    filter(agreement == max(agreement)) |> 
    drop_na(difficulty) |> 
    ungroup()
  
  return(list(cl = cl,
              diff = diff,
              fuzz_join = fuzz_join,
              item_params = item_params,
              naming = naming,
              times = times,
              
              naming_file = naming_file,
              discourse_naming_joined_file = discourse_naming_joined_file,
              timestamp_file = timestamp_file,
              naming_parameters_file = naming_parameters_file
              ))
  
  
}
