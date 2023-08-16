

library(here)
library(tidyverse)
library(tidytext)
library(readxl)
library(fuzzyjoin)
library(anticlust) # use dev version!
library(patchwork)


select_stimuli <- function(participant_theta,
                           min_naming_agreement = 70,
                           min_discourse_salience = 30,
                           target_prob_correct = 0.33,
                           min_discourse_stimuli = 9,
                           min_discourse_items = 54,
                           seed = 42,
                           participant_id = ""){
  
 # participant_theta = 0.5
 # min_naming_agreement = 70
 # min_discourse_salience = 30
 # target_prob_correct = 0.333
 # min_discourse_stimuli = 9
 # min_discourse_items = 54
 # seed = 42
 # participant_id = ""
  
  if(min_discourse_stimuli == 0){
    naming_only = 1
  } else {
    naming_only = 0
  }
  set.seed(seed)
  
    #### reading in files ####
    # any dataframe that holds the word, the source, and the agreement scores...
    naming <- read_csv(here("data", "final_database_4-11-23.csv")) |> 
      select(lemma = modal, source, agreement, filename = `confirmed file name`) |> #, target = `confirmed file name`
      distinct() |> 
      filter(agreement >= min_naming_agreement) |> 
      group_by(lemma) |> 
      mutate(source = paste(source, collapse = ", ")) |> 
      distinct()
    
    # this is a joined df where I hand chekced the fuzzy join in 01-new-approach
    # between discourse and naming
    
    # fuzz_join = read.csv(here("data", "found_in_discourse_exact.csv"))  |> select(-X) |> 
    #   mutate(lemma_dis = lemma)
    # fuzz_join_fuzz = read.csv(here("data", "found_in_discourse_fuzzy.csv")) |> filter(match == 1) |> 
    #   select(-X, -match, -dist) |> 
    #   rename(lemma = lemma_naming)
    
    fuzz_join = read_csv(here("data", "join_checked.csv")) |> 
      drop_na(match) |> 
      select(source:lemma_dis) |> 
      filter(percent >= min_discourse_salience, agreement >= min_naming_agreement) |> 
      select(source, lemma=lemma_naming, stimuli, percent, lemma_dis) |> 
      distinct()
    
    
    # total number of unique found words between naming and discourse
    fuzz_join |> 
      distinct(lemma) |> nrow()
    
    # get the average time to produce of each stimuli for more balancing
    times <- read_csv(here("data", "2023-08-14_timestamp.csv")) |> 
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
    #item_params = read.csv("data/item_parameters.csv")
    # updated to new merged sheet with a few more items...
    #item_params = read.csv("data/AoA-phonemes-freq_joined_2023-08-02.csv") |> 
    item_params = read_excel(here("data", "AoA-phonemes-freq for singe noun targets with at least 70 percent name agreement from ELP.xlsx")) |> 
      select(Word, LgSUBTLCD, Age_Of_Acquisition, NPhon) |> 
      mutate(LgSUBTLCD = readr::parse_number(LgSUBTLCD),
             #NPhon = readr::parse_number(NPhon),
             Age_Of_Acquisition = readr::parse_number(Age_Of_Acquisition)) 
    
    # join together and calculate item difficulty
    diff = naming |> 
      full_join(item_params, by = c("lemma" = "Word")) |> 
      left_join(fuzz_join, by = join_by(lemma, source)) |> 
      mutate(difficulty = -1.22 + 0.15*NPhon -0.36*LgSUBTLCD + 0.21*Age_Of_Acquisition)
    
    # best agreement from naming with a difficulty score. should go up in
    # size after getting more difficulty scores
    cl = diff |> 
      select(lemma_naming = lemma, stimuli, agreement, percent, difficulty, filename) |> #, target
      mutate(in_discourse = ifelse(is.na(stimuli), 0, 1)) |> 
      group_by(lemma_naming) |> 
      filter(agreement == max(agreement)) |> 
      #slice_sample(n = 1) |> 
      drop_na(difficulty) |> 
      ungroup()
    
    # split out into naming only items and items that are also in discourse
    naming_db = cl |> filter(in_discourse == 0)
    discourse_db = cl |> filter(in_discourse == 1)
    
    # assigm ability and difficulty
    theta = participant_theta # ability
    #b = -1    # difficulty
    
    # some other important variables
    # how many discourse and naming items to select.  
    # the ideal probability of a correct response
    # n_discourse = 84
    # n_naming = 180-n_discourse
    
    ideal_prob_correct = target_prob_correct
    
    # function for calculating probility of correct response for a 1-PL model
    # given an ability (theta) and item difficulty (b)
    p_cor = function(theta, b){
      exp(theta-b)/(1+exp(theta-b))
    }
    p_cor = Vectorize(p_cor) # vectorized
    
    
  if(naming_only != 1){
    
          big_stim = c("wanderer", "quest", "journey", "flowerman")
          # get the closest discourse items to p(correct) = 0.33
          discourse_items = discourse_db |> ungroup() |> 
            mutate(p_correct = p_cor(theta, difficulty),
                   closest = abs(ideal_prob_correct - p_correct),
                   stimuli = as.factor(stimuli)) |> 
            arrange(closest) |> 
            filter(p_correct < 0.75) |> 
            add_count(stimuli) |> 
            filter(n>2) |> 
            # add_count(lemma_naming, name = "n_lemma") |>
            # mutate(n2 = ifelse(!(stimuli %in% big_stim), n,
            #                    ifelse(n_lemma == 1, n, NA))) |>
            # mutate(n3 = ifelse(all(is.na(n2)), 1, 0), .by = lemma_naming) |>
            # mutate(n2 = ifelse(n3 == 1, n, n2)) |>
            #  group_by(lemma_naming) |>
            # filter(n2 == max(n2, na.rm = TRUE)) |>
            group_by(lemma_naming) |>
            filter(n == max(n)) |>
            ungroup()
          
          discourse_items |> count(stimuli, sort = TRUE) -> test
              
          
          sl = discourse_items |> 
            summarize(#m_a = mean(agreement, na.rm = TRUE),
              m_p = mean(percent, na.rm = TRUE),
              m_d = mean(difficulty, na.rm = TRUE),
              n = n(), .by = "stimuli") |> 
            left_join(times, by = "stimuli") |> 
            arrange(desc(n)) 
          
          #number of discourse items
          nd = nrow(discourse_items)
          #number of discourse stimuli
          ns = nrow(sl)
          
          if(ns < min_discourse_stimuli | nd < min_discourse_items){
            return(
              list(
                dat = NA,
                plot1 = NA,
                plot2 = NA,
                time = NA,
                error = TRUE,
                error_detail = tribble(
                  ~variable, ~value,
                  "number of discourse stimuli", ns,
                  "number of discouse items", nd #,
                  #"number of total items" = length(initial_groupings),
                  #"lowest number of discourse items in a condition" = min_cat)
                )
              )
            )
          }
          
          
          if(any(is.na(sl$m_time))){stop("Error: NA in time value")}
          
          matched = matching(
            sl[,3:4],
            p = 3, 
          )
          
          sl$group = matched
          
          sl2 = sl |> 
            mutate(rand = rnorm(n())) |> 
            arrange(group, rand) |> 
            mutate(condition = rep_len(seq(1, 3, 1), n())) |> 
            mutate(condition = ifelse(is.na(group), NA, condition)) |> 
            arrange(group, desc(n))
          
          sl_out = sl2 |> drop_na(group)
          
          sl_out |> count(condition, wt = n)
          
          
          add_c <- count(sl_out, condition, wt = n) |> arrange(n) |> 
             pull(condition)
          
          non_matched = sl2 |> filter(is.na(group)) |> arrange(desc(n)) 
          
          if(length(add_c) >= 1){ 
            tmp = non_matched |> 
              head(1) |> 
              mutate(condition = add_c[1])
            
            sl_out = bind_rows(sl_out, tmp)
            non_matched = tail(non_matched, -1)
            print("adjusted 1")
          }
          
          sl_out |> count(condition, wt = n)
          
          add_c <- count(sl_out, condition, wt = n) |> arrange(n) |> 
            pull(condition)
          
          if(length(add_c) >= 1){ 
            tmp = non_matched |> 
              head(1) |> 
              mutate(condition = add_c[1])
            
            sl_out = bind_rows(sl_out, tmp)
            non_matched = tail(non_matched, -1)
            print("adjusted 2")
          }
            
          sl_out |> count(condition, wt = n)
          
          test = sl_out |> filter(n > 2) |> arrange(desc(n)) |> group_by(condition) |> mutate(nn = cumsum(n))
          
          
          discourse_items = discourse_items |> 
            inner_join(sl_out |> select(-n, -group), by = "stimuli") |> 
            filter(!is.na(condition))
          
          min_cat = min(discourse_items |> count(condition) |> pull(n))
          total_discourse_items = nrow(discourse_items)
          
          if(min_cat < min_discourse_items/3 | total_discourse_items < min_discourse_items){
            return(
              list(
                dat = NA,
                plot1 = NA,
                plot2 = NA,
                time = NA,
                error = TRUE,
                error_detail = tribble(
                    ~variable, ~value,
                    "number of discourse stimuli", ns,
                    "number of discouse items", nd,
                    #"number of total items" = length(initial_groupings),
                    "lowest number of discourse items in a condition", min_cat)
              )
            )
          }
          
          count(discourse_items, condition)
          
          discourse_items = discourse_items |> 
            group_by(condition) |> 
            slice_min(order_by = p_correct, n = min_cat) |> 
            ungroup()
          
          matching(
            discourse_items[,3:5],
            match_between = discourse_items$condition
          ) -> m2
          
          discourse_items$m2 = m2
          
          tmp = discourse_items
          
          # m2 <= 40 here is the upper cap on discourse items per condition. 
          discourse_items = discourse_items |> drop_na(m2) |> filter(m2 <= 40)
          discourse_items_per_cat = max(discourse_items$m2)
          discourse_items_total = nrow(discourse_items)
          
          discourse_items |> drop_na(m2) |> count(condition)
          
          
          mean_sd_tab(discourse_items[, 3:5], discourse_items$condition, na.rm = TRUE)
          
          
          # now pre-assign these to conditions, and pick the best words from them, 
          # then pick the remaining naming items...?
          
          # get the closest remaining items to p(correct) = 0.33
          remaining_db = discourse_db |> 
            filter(!(lemma_naming %in% discourse_items$lemma_naming)) |> 
            bind_rows(naming_db)
          
          N_naming = 180-nrow(discourse_items)
          
          if(mean(discourse_items$p_correct)<0.5){
            ideal_prob_correct = ideal_prob_correct
          } else {
            ideal_prob_correct = ideal_prob_correct/2
          }
          
          additional_items = remaining_db |> 
            mutate(p_correct = p_cor(theta, difficulty),
                   closest = abs(ideal_prob_correct - p_correct)) |> 
            filter(p_correct < 0.75) |> 
            distinct(lemma_naming, agreement, difficulty, filename, in_discourse, p_correct, closest) |> 
            arrange(closest) |> 
            head(N_naming)
          
          
          # join it all together so we can divide it up
          dat = bind_rows(discourse_items, additional_items) |> 
            select(word = lemma_naming,
                   in_discourse,
                   discourse_stimuli = stimuli,
                   agreement, item_difficulty = difficulty, core_lex_percent = percent,
                   condition, filename, p_correct) #, target
          
  } else {
        
    dat = cl |> 
      mutate(p_correct = p_cor(theta, difficulty),
             closest = abs(ideal_prob_correct - p_correct)) |> 
      filter(p_correct < 0.75) |> 
      distinct(lemma_naming, agreement, difficulty, filename, in_discourse, p_correct, closest) |> 
      arrange(closest) |> 
      head(180) |> 
      mutate(stimuli = NA, in_discourse = NA, percent = NA, condition = NA) |> 
      select(word = lemma_naming,
             in_discourse,
             discourse_stimuli = stimuli,
             agreement, item_difficulty = difficulty, core_lex_percent = percent,
             condition, filename, p_correct)
    
    discourse_items_total = 0
    discourse_items = NULL
    
    additional_items = tibble(a = rep(0, 180))
      }

    sample_this = rep(c(1, 2, 3), length.out = 180-discourse_items_total)

    # initialize groupings for anticlustering; new items get random group affiliation
    initial_groupings <- c(discourse_items$condition, sample(sample_this) )
    
    if(length(initial_groupings) < 180){
      return(
        list(
          dat = NA,
          plot1 = NA,
          plot2 = NA,
          time = NA,
          error = TRUE,
          error_detail = tribble(
            ~variable, ~value,
            "number of discourse stimuli", ns,
            "number of discouse items", nd,
            "number of total items", length(initial_groupings),
            "lowest number of discourse items in a condition", min_cat)
        )
      )
    }
    table(initial_groupings)
    
    N = nrow(additional_items)
    
    new_groups <- anticlustering(
      dat[,4:5],
      K = initial_groupings,
      objective = "kplus",
      method = "local-maximum",
      categories = c(discourse_items$condition, rep(4, N))
    )
    
    # "old" items are still grouped together, new items are assigned evenly to groups 1-3:
    table(new_groups, c(discourse_items$condition, rep(4, N)))
    
    dat$condition_all = new_groups
    
    # check quality of the solution:
    mean_sd_tab(dat[,4:6], dat$condition_all, na.rm = TRUE)
    #mean_sd_tab(dat |> drop_na(condition) |> select(4:6), dat |> drop_na(condition) |> pull(condition), na.rm = TRUE)
    
    dat |> count(condition_all)
    
    dat$core_lex_percent_imp = dat$core_lex_percent
    dat$core_lex_percent_imp[is.na(dat$core_lex_percent)] <- mean(dat$core_lex_percent, na.rm = TRUE)
    
    # nest the data into three dataframes by condition
    dat_nest = dat |> 
      nest_by(condition_all)
    
    # for each one, repeat the process into treated and untreated
    dat_out = list()
    if(naming_only != 1){
        for(i in 1:nrow(dat_nest)){
          tmp = dat_nest$data[[i]]
          gr <- anticlustering(
            tmp[, c(4, 5, 10)], # use the variables directly
            K = c(40, 20),
            method = "local-maximum",
            categories = tmp$in_discourse,
            repetitions = 100,
            objective = "kplus",
            standardize = TRUE
          )
          tmp$tx = ifelse(gr == 1, 1, 0)
          tmp$condition = dat_nest$condition_all[i]
          dat_out[[i]] = tmp
        }
    } else{
      for(i in 1:nrow(dat_nest)){
        tmp = dat_nest$data[[i]]
        gr <- anticlustering(
          tmp[, c(4, 5)], # use the variables directly
          K = c(40, 20),
          method = "local-maximum",
          categories = tmp$in_discourse,
          repetitions = 100,
          objective = "kplus",
          standardize = TRUE
        )
        tmp$tx = ifelse(gr == 1, 1, 0)
        tmp$condition = dat_nest$condition_all[i]
        dat_out[[i]] = tmp
      }
    }
    
    # final dataset
    df_final = bind_rows(dat_out) |> 
      select(word, in_discourse, discourse_stimuli, agreement, item_difficulty, p_correct,
             core_lex_percent, condition, tx, filename) |> 
      mutate(
        in_discourse = as.factor(in_discourse),
        discourse_stimuli = as.factor(discourse_stimuli),
        condition = as.factor(condition),
        tx = as.factor(tx),
        participant_id = participant_id,
        min_naming_agreement = min_naming_agreement,
        min_discourse_salience = min_discourse_salience,
        target_prob_correct = target_prob_correct,
        min_discourse_stimuli = min_discourse_stimuli,
        min_discourse_items = min_discourse_items,
        seed = seed
      )
    

    # dataframes for plot
    # by discourse vs not
    check_stats = get_check_stats(df_final)
    # overall
    check_stats_overall = get_check_stats_overall(df_final)
    check_stats_overall$in_discourse = "Overall"
  
    # get plots for tabs 1 and 2
    p_mean_sd = get_p1(check_stats, check_stats_overall, naming_only)
    p_box     = get_p2(df_final, naming_only)

    # get table for time dat
    if(naming_only != 1){
      time_dat = get_time_dat(discourse_items)
    } else {
      time_dat = tibble(data = NA)
    }
    
    input_file <- create_app_input_file(df_final, naming_only = naming_only)

    return(
      list(
        dat = df_final,
        plot1 = p_mean_sd,
        plot2 = p_box,
        time = time_dat,
        input_file = input_file,
        error = FALSE
      )
    )
    
}
