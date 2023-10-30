get_time_dat <- function(discourse_items, df_final){
  
  df_final |> 
    count(condition, name = "total probe items") |> 
    mutate(condition = as.numeric(condition)) -> n_cond
  
  discourse_items |> 
    add_count(condition) |> 
    group_by(stimuli) |> 
    mutate(mean_p_correct = mean(p_correct)) |> 
    ungroup() |> 
    distinct(stimuli, nn, m_time, sd_time, mean_p_correct, condition) |> 
    add_count(condition, name = "n_discourse_tasks") |> 
    summarize(mean_time = sum(m_time),
              sd_time = sum(sd_time),
              n_discourse_words = mean(nn),
              n_discourse_tasks = mean(n_discourse_tasks),
              mean_prob_correct = mean(mean_p_correct), .by = condition) |> 
    left_join(n_cond, by = "condition")
}


get_check_stats <- function(df_final){
  df_final |>
    mutate(condition = as.factor(condition),
           tx = fct_relevel(as.factor(ifelse(tx == 1, "tx", "control")), rev),
           in_discourse = as.factor(ifelse(in_discourse == 1, "discourse", "naming_only"))) |> 
    group_by(condition, tx, in_discourse) |> 
    summarize(
      item_difficulty__mean = mean(item_difficulty),
      core_lex_percent__mean = mean(core_lex_percent, na.rm = TRUE),
      agreement__mean = mean(agreement),
      item_difficulty__sd = sd(item_difficulty),
      core_lex_percent__sd = sd(core_lex_percent, na.rm = TRUE),
      agreement__sd = sd(agreement)
    ) 
}

get_check_stats_overall <- function(df_final){
  df_final |>
    mutate(condition = as.factor(condition),
           tx = fct_relevel(as.factor(ifelse(tx == 1, "tx", "control")), rev),
           in_discourse = as.factor(ifelse(in_discourse == 1, "discourse", "naming_only"))) |> 
    group_by(condition, tx) |> 
    summarize(
      item_difficulty__mean = mean(item_difficulty),
      core_lex_percent__mean = mean(core_lex_percent, na.rm = TRUE),
      agreement__mean = mean(agreement),
      item_difficulty__sd = sd(item_difficulty),
      core_lex_percent__sd = sd(core_lex_percent, na.rm = TRUE),
      agreement__sd = sd(agreement)
    ) 
}

get_p1 <- function(check_stats, check_stats_overall, naming_only, pick_theme = theme_bw(base_size = 14)){
  # visualize checks
  if(naming_only != 1){
  check_stats |> 
    bind_rows(check_stats_overall) |> 
    pivot_longer(cols = 4:9) |> 
    separate(name, into = c("parameter", "metric"), sep = "__") |> 
    pivot_wider(names_from = "metric", values_from = "value") |> 
    mutate(lb = mean-sd, ub = mean+sd) |> 
    filter(parameter == "item_difficulty") |> 
    ggplot(aes(x = tx, y = mean, fill = condition, color = condition)) +
    facet_wrap(~in_discourse) +
    #geom_col(position = position_dodge(1)) +
    geom_point(position = position_dodge(1)) + 
    geom_errorbar(aes(ymin = lb, ymax = ub), position = position_dodge(1), width = 0.5) +
    labs(y = "ITEM DIFFICULTY",
         x = "Control (n = 20) vs. Tx Items (n = 40)",
         fill = "Condition", color = "Condition",
         caption = "Error bars represent standard deviation") +
    scale_y_continuous(limits = c(0, 80)) +
      coord_cartesian(clip = "off") +
      pick_theme-> id
  
  check_stats |> 
    bind_rows(check_stats_overall) |> 
    pivot_longer(cols = 4:9) |> 
    separate(name, into = c("parameter", "metric"), sep = "__") |> 
    pivot_wider(names_from = "metric", values_from = "value") |> 
    mutate(lb = mean-sd, ub = mean+sd) |> 
    filter(parameter == "core_lex_percent") |> 
    mutate(ub = ifelse(ub > 100, 100, ub)) |> 
    ggplot(aes(x = tx, y = mean, fill = condition, color = condition)) +
    facet_wrap(~in_discourse) +
    #geom_col(position = "dodge") +
    geom_point(position = position_dodge(1)) + 
    geom_errorbar(aes(ymin = lb, ymax = ub), position = position_dodge(1), width = 0.5) +
    labs(y = "CORE LEXICON PERCENT",
         x = "Control (n = 20) vs. Tx Items (n = 40)",
         fill = "Condition", color = "Condition",
         caption = "Error bars represent standard deviation") +
    scale_y_continuous(limits = c(0, 100)) +
    coord_cartesian(clip = "off") +
    pick_theme -> clp
  
  check_stats |> 
    bind_rows(check_stats_overall) |> 
    pivot_longer(cols = 4:9) |> 
    separate(name, into = c("parameter", "metric"), sep = "__") |> 
    pivot_wider(names_from = "metric", values_from = "value") |> 
    mutate(lb = mean-sd, ub = mean+sd) |> 
    filter(parameter == "agreement") |> 
    mutate(ub = ifelse(ub > 100, 100, ub)) |> 
    ggplot(aes(x = tx, y = mean, fill = condition, color = condition)) +
    facet_wrap(~in_discourse) +
    #geom_col(position = "dodge") +
    geom_point(position = position_dodge(1)) + 
    geom_errorbar(aes(ymin = lb, ymax = ub), position = position_dodge(1), width = 0.5) +
    labs(y = "NAME AGREEMENT",
         x = "Control (n = 20) vs. Tx Items (n = 40)",
         fill = "Condition", color = "Condition",
         caption = "Error bars represent standard deviation") +
    scale_y_continuous(limits = c(0, 100)) +
    coord_cartesian(clip = "off") +
    pick_theme -> a
  
  p = id / a / clp
  } else {
    check_stats |> 
      bind_rows(check_stats_overall) |> 
      pivot_longer(cols = 4:9) |> 
      separate(name, into = c("parameter", "metric"), sep = "__") |> 
      pivot_wider(names_from = "metric", values_from = "value") |> 
      mutate(lb = mean-sd, ub = mean+sd) |> 
      filter(parameter == "item_difficulty") |> 
      ggplot(aes(x = tx, y = mean, fill = condition, color = condition)) +
     # facet_wrap(~in_discourse) +
      geom_col(position = "dodge") +
      geom_point(position = position_dodge(1)) + 
      geom_errorbar(aes(ymin = lb, ymax = ub), position = position_dodge(1), width = 0.5) +
      labs(y = "ITEM DIFFICULTY",
           x = "Control (n = 20) vs. Tx Items (n = 40)",
           fill = "Condition", color = "Condition",
           caption = "Error bars represent standard deviation") +
      scale_y_continuous(limits = c(20, 80))  +
      coord_cartesian(clip = "off") +
      pick_theme -> id
    
    check_stats |> 
      bind_rows(check_stats_overall) |> 
      pivot_longer(cols = 4:9) |> 
      separate(name, into = c("parameter", "metric"), sep = "__") |> 
      pivot_wider(names_from = "metric", values_from = "value") |> 
      mutate(lb = mean-sd, ub = mean+sd) |> 
      filter(parameter == "agreement") |> 
      ggplot(aes(x = tx, y = mean, fill = condition, color = condition)) +
      #facet_wrap(~in_discourse) +
      geom_col(position = "dodge") +
      geom_point(position = position_dodge(1)) + 
      geom_errorbar(aes(ymin = lb, ymax = ub), position = position_dodge(1), width = 0.5) +
      labs(y = "NAME AGREEMENT",
           x = "Control (n = 20) vs. Tx Items (n = 40)",
           fill = "Condition", color = "Condition",
           caption = "Error bars represent standard deviation") +
      scale_y_continuous(limits = c(0, 100)) +
      coord_cartesian(clip = "off") +
      pick_theme -> a
    
    p = id / a
  }
  return(p)
}


get_p2 <- function(df_final, naming_only, pick_theme = theme_bw(base_size = 14)){
  
  if(naming_only != 1){
      df_final |>
        mutate(condition = as.factor(condition),
               tx = fct_relevel(as.factor(ifelse(tx == 1, "tx", "control")), rev),
               in_discourse = as.factor(ifelse(in_discourse == 1, "discourse", "naming_only"))) |> 
        bind_rows(
          df_final |>
            mutate(condition = as.factor(condition),
                   tx = fct_relevel(as.factor(ifelse(tx == 1, "tx", "control")), rev),
                   in_discourse = "overall") 
        ) |> 
        ggplot(aes(x = tx, y = item_difficulty, fill = condition)) +
        facet_wrap(~in_discourse) +
        geom_boxplot() +
        scale_y_continuous(limits = c(20, 80)) +
      pick_theme -> id2
      
      df_final |>
        mutate(condition = as.factor(condition),
               tx = fct_relevel(as.factor(ifelse(tx == 1, "tx", "control")), rev),
               in_discourse = as.factor(ifelse(in_discourse == 1, "discourse", "naming_only"))) |> 
        bind_rows(
          df_final |>
            mutate(condition = as.factor(condition),
                   tx = fct_relevel(as.factor(ifelse(tx == 1, "tx", "control")), rev),
                   in_discourse = "overall") 
        ) |> 
        ggplot(aes(x = tx, y = agreement, fill = condition)) +
        facet_wrap(~in_discourse) +
        geom_boxplot() + 
        scale_y_continuous(limits = c(0, 100)) +
        pick_theme  -> a2
      
      df_final |>
        mutate(condition = as.factor(condition),
               tx = fct_relevel(as.factor(ifelse(tx == 1, "tx", "control")), rev),
               in_discourse = as.factor(ifelse(in_discourse == 1, "discourse", "naming_only"))) |> 
        bind_rows(
          df_final |>
            mutate(condition = as.factor(condition),
                   tx = fct_relevel(as.factor(ifelse(tx == 1, "tx", "control")), rev),
                   in_discourse = "overall") 
        ) |> 
        ggplot(aes(x = tx, y = core_lex_percent, fill = condition)) +
        facet_wrap(~in_discourse) +
        geom_boxplot() +
        scale_y_continuous(limits = c(0, 100)) +
        pick_theme -> clp2
      
      p = id2 / a2 / clp2
  } else {
    
    df_final |>
      mutate(condition = as.factor(condition),
             tx = fct_relevel(as.factor(ifelse(tx == 1, "tx", "control")), rev),
             in_discourse = as.factor(ifelse(in_discourse == 1, "discourse", "naming_only"))) |> 
      bind_rows(
        df_final |>
          mutate(condition = as.factor(condition),
                 tx = fct_relevel(as.factor(ifelse(tx == 1, "tx", "control")), rev),
                 in_discourse = "overall") 
      ) |> 
      ggplot(aes(x = tx, y = item_difficulty, fill = condition)) +
     # facet_wrap(~in_discourse) +
      geom_boxplot() +
      scale_y_continuous(limits = c(20, 80)) +
      pick_theme -> id2
    
    df_final |>
      mutate(condition = as.factor(condition),
             tx = fct_relevel(as.factor(ifelse(tx == 1, "tx", "control")), rev),
             in_discourse = as.factor(ifelse(in_discourse == 1, "discourse", "naming_only"))) |> 
      bind_rows(
        df_final |>
          mutate(condition = as.factor(condition),
                 tx = fct_relevel(as.factor(ifelse(tx == 1, "tx", "control")), rev),
                 in_discourse = "overall") 
      ) |> 
      ggplot(aes(x = tx, y = agreement, fill = condition)) +
      #facet_wrap(~in_discourse) +
      geom_boxplot() + 
      scale_y_continuous(limits = c(0, 100)) +
      pick_theme -> a2
    
    p = id2 / a2
    
  }
  return(p)
}


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
    naming_file = here("shiny", "data", "final_database_4-11-23.csv")
    discourse_naming_joined_file = here("shiny", "data", "2023-08-27_join_checked.csv")
    timestamp_file = here("shiny", "data", "2023-08-14_timestamp.csv")
    naming_parameters_file = here("shiny", "data",
                                  "AoA-phonemes-freq_from-ELP.xlsx")
  } else if(isTRUE(local)) {
    naming_file = here("shiny", "data", "final_database_4-11-23.csv")
    discourse_naming_joined_file = here("shiny", "data", "2023-08-27_join_checked.csv")
    timestamp_file = here("shiny", "data", "2023-08-14_timestamp.csv")
    naming_parameters_file = here("shiny", "data",
                                  "AoA-phonemes-freq_from-ELP.xlsx")
    
  } else {
    naming_file = here("data", "final_database_4-11-23.csv")
    discourse_naming_joined_file = here("data", "2023-08-27_join_checked.csv")
    timestamp_file = here("data", "2023-08-14_timestamp.csv")
    naming_parameters_file = here("data",
                                  "AoA-phonemes-freq_from-ELP.xlsx")
  }
  
  # any dataframe that holds the word, the source, and the agreement scores...
  naming <- suppressMessages(read_csv(naming_file, col_types = cols())) |> 
    select(lemma = modal, source, agreement, filename = `confirmed file name`) |> 
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
    drop_na(match) |> 
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
      read_excel(naming_parameters_file) |> 
        select(Word, LgSUBTLCD, Age_Of_Acquisition, NPhon) |> 
        mutate(LgSUBTLCD = readr::parse_number(LgSUBTLCD),
               #NPhon = readr::parse_number(NPhon),
               Age_Of_Acquisition = readr::parse_number(Age_Of_Acquisition)) 
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


get_blacklist_options <- function(bl = "discourse", ...){
  
  files = read_in_all_files(shiny = TRUE, ...)
  
  cl = files$cl
  
  if(bl == "discourse"){
    return(unique(cl$stimuli[which(!is.na(cl$stimuli))]))
  } else if(bl == "naming") {
    return(unique(cl$lemma_naming[which(!is.na(cl$lemma_naming))]))
  }
  
}

