# library(tidyverse)
# 
# test <- read.csv(here::here("shiny", "data", "test_stimuli_2023-08-15.csv"))
# test <- read.csv(here::here("data", "test_stimuli_2023-08-15.csv"))
# 
# create_app_input_file(test) -> test5

create_app_input_file <- function(df_selected_stimuli, naming_only){
  
  # condition assignments
  conditions <- tibble(
    condition_num = as.factor(c(1, 2, 3)),
    condition = c("eab", "em", "am")
  )
  
  # we only need these columns
  df <- df_selected_stimuli |> 
    select(word, in_discourse, discourse_stimuli, condition_num = condition, tx, filename, participant_id) |> 
    left_join(conditions, join_by(condition_num))

# test
  # check for NA values
  # check for balanced conditions
  # check for 180 unique words

# why getting NA values?

  if(naming_only != 1){
    # discourse item formatting
    df_discourse <- 
      df |> 
        filter(in_discourse == 1) |> 
        drop_na(discourse_stimuli) |> 
        distinct(participant_id, discourse_stimuli, condition) |> 
        mutate(value = 1, condition2 = condition, type = "discourse",) |> 
        pivot_wider(names_from = condition, values_from = value) |> 
        rename('eab-discourse-probes' = eab,
               'em-discourse-probes' = em,
               'am-discourse-probes' = am,
               condition = condition2) |> 
        mutate(
               'eab-naming-probes' = NA,
               'eab-treatment' = NA,
               'em-naming-probes' = NA,
               'em-treatment' = NA,
               'am-naming-probes' = NA,
               'am-treatment' = NA
               ) |> 
         mutate(across(`eab-discourse-probes`:`am-treatment`, ~replace_na(as.double(.), 0))) |> 
        select(
          participant_id,
          item = discourse_stimuli,
          type,
          'eab-treatment', 'eab-naming-probes', 'eab-discourse-probes',
          'em-treatment', 'em-naming-probes', 'em-discourse-probes',
          'am-treatment', 'am-naming-probes', 'am-discourse-probes'
        )
  }

  # naming item formatting
  df_naming <- 
    df |> 
      distinct(participant_id, word, filename, condition, tx) |> 
      mutate(value = 1,
             condition2 = condition,
             type = "naming") |> 
      pivot_wider(names_from = condition, values_from = value) |> 
      rename('eab-naming-probes' = eab,
             'em-naming-probes' = em,
             'am-naming-probes' = am,
             condition = condition2) |> 
      mutate(
        
        'eab-discourse-probes' = NA,
        'eab-treatment' = NA,
        'em-discourse-probes' = NA,
        'em-treatment' = NA,
        'am-discourse-probes' = NA,
        'am-treatment' = NA,
        
        'eab-treatment' = ifelse(tx == 1, `eab-naming-probes`, NA),
        'em-treatment' = ifelse(tx == 1, `em-naming-probes`, NA),
        'am-treatment' = ifelse(tx == 1, `am-naming-probes`, NA)
        ) |> 
      mutate(across(`eab-naming-probes`:`am-treatment`, ~replace_na(as.double(.), 0))) |> 
      select(
        participant_id,
        item = word,
        filename,
        type,
        condition,
        tx,
        'eab-treatment', 'eab-naming-probes', 'eab-discourse-probes',
        'em-treatment', 'em-naming-probes', 'em-discourse-probes',
        'am-treatment', 'am-naming-probes', 'am-discourse-probes'
      ) |> 
      arrange(condition, tx)

  if(naming_only != 1){
      df_out <- bind_rows(df_naming, df_discourse)
  } else {
    df_out = df_naming
  }

  return(df_out)
}

