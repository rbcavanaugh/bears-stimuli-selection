

library(here)
library(tidyverse)
library(tidytext)
library(readxl)
library(fuzzyjoin)
library(anticlust) # use dev version!
library(patchwork)


select_stimuli <- function(participant_theta){
  
  participant_theta = 0
    #### reading in files ####
    # any dataframe that holds the word, the source, and the agreement scores...
    naming <- read_csv(here("data", "naming-battery-items.csv")) |> 
      select(lemma = modal, source, agreement) |> 
      filter(agreement > 70) |> 
      group_by(lemma) |> 
      mutate(source = paste(source, collapse = ", ")) |> 
      distinct()
    
    # this is a joined df where I hand chekced the fuzzy join in 01-new-approach
    # between discourse and naming
    
    fuzz_join = read.csv(here("data", "found_in_discourse_exact.csv"))  |> select(-X) |> 
      mutate(lemma_dis = lemma) |> 
      filter(percent >= 30, agreement >= 70) |> 
      select(source, lemma, stimuli, percent, lemma_dis)
    
    # total number of unique found words between naming and discourse
    fuzz_join |> 
      distinct(lemma)  |> nrow()
    
    # get the average time to produce of each stimuli for more balancing
    times <- read_csv(here("output", "2023-07-20_timestamp.csv")) |> 
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
      select(lemma_naming = lemma, stimuli, agreement, percent, difficulty) |> 
      mutate(in_discourse = ifelse(is.na(stimuli), 0, 1)) |> 
      group_by(lemma_naming) |> 
      filter(agreement == max(agreement)) |> 
     # slice_sample(n = 1) |> 
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
    n_discourse = 84
    n_naming = 180-n_discourse
    ideal_prob_correct = 0.333
    
    # function for calculating probility of correct response for a 1-PL model
    # given an ability (theta) and item difficulty (b)
    p_cor = function(theta, b){
      exp(theta-b)/(1+exp(theta-b))
    }
    p_cor = Vectorize(p_cor) # vectorized
    
    # get the closest discourse items to p(correct) = 0.33
    discourse_items = discourse_db |> 
      mutate(p_correct = p_cor(theta, difficulty),
             closest = abs(ideal_prob_correct - p_correct),
             stimuli = as.factor(stimuli)) |> 
      arrange(closest) |> 
      filter(p_correct < 0.75) |> 
      add_count(stimuli) |> 
      filter(n > 4)
    
    
    sl = discourse_items |> 
      summarize(#m_a = mean(agreement, na.rm = TRUE),
        m_p = mean(percent, na.rm = TRUE),
        m_d = mean(difficulty, na.rm = TRUE),
        n = n(), .by = "stimuli") |> 
      left_join(times, by = "stimuli")
    
    if(any(is.na(sl$m_time))){stop("Error: NA in time value")}
    
    
    matched = matching(
      sl[,2:5],
      p = 3, 
    )
    
    sl$group = matched
    
    sl2 = sl |> 
      mutate(rand = rnorm(n())) |> 
      arrange(group, rand) |> 
      mutate(condition = rep_len(seq(1, 3, 1), n())) |> 
      select(stimuli, condition, m_time)
    
    discourse_items = discourse_items |> 
      left_join(sl2, by = "stimuli") |> 
      filter(!is.na(condition))
    
    min_cat = min(discourse_items |> count(condition) |> pull(n))
    
    if(min_cat < 28){stop("Error: too few items")}
    
    discourse_items = discourse_items |> 
      arrange(p_correct)# |> 
      #slice_min(n = min_cat, order_by = p_correct, by = condition)
    
    matching(
      discourse_items[,3:5],
      match_between = discourse_items$condition
    ) -> m2
    
    discourse_items$m2 = m2
    
    discourse_items = discourse_items |> filter(m2 <= 28)
    
    
    mean_sd_tab(discourse_items[, 3:5], discourse_items$condition, na.rm = TRUE)
    
    
    # now pre-assign these to conditions, and pick the best words from them, 
    # then pick the remaining naming items...?
    
    # get the closest remaining items to p(correct) = 0.33
    remaining_db = discourse_db |> 
      filter(!(lemma_naming %in% discourse_items$lemma_naming)) |> 
      bind_rows(naming_db)
    
    N_naming = 180-nrow(discourse_items)
    
    additional_items = remaining_db |> 
      mutate(p_correct = p_cor(theta, difficulty),
             closest = abs(0.333 - p_correct)) |> 
      filter(p_correct < 0.75) |> 
      arrange(closest) |> 
      head(N_naming)
    
    
    # join it all together so we can divide it up
    dat = bind_rows(discourse_items, additional_items) |> 
      select(word = lemma_naming,
             in_discourse,
             discourse_stimuli = stimuli,
             agreement, item_difficulty = difficulty, core_lex_percent = percent,
             condition)
    
    
    sample_this = rep(c(1, 2, 3), length.out = 180-(28*3))
    sample(sample_this)
    
    # initialize groupings for anticlustering; new items get random group affiliation
    initial_groupings <- c(discourse_items$condition, sample(sample_this) )
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
    mean_sd_tab(dat |> drop_na(condition) |> select(4:6), dat |> drop_na(condition) |> pull(condition), na.rm = TRUE)
    
    dat |> count(condition_all)
    
    dat$core_lex_percent_imp = dat$core_lex_percent
    dat$core_lex_percent_imp[is.na(dat$core_lex_percent)] <- mean(dat$core_lex_percent, na.rm = TRUE)
    
    # nest the data into three dataframes by condition
    dat_nest = dat |> 
      nest_by(condition_all)
    
    # for each one, repeat the process into treated and untreated
    dat_out = list()
    for(i in 1:nrow(dat_nest)){
      tmp = dat_nest$data[[i]]
      gr <- anticlustering(
        tmp[, c(4, 5, 8)], # use the variables directly
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
    
    # final dataset
    df_final = bind_rows(dat_out) |> 
      select(word, in_discourse, discourse_stimuli, agreement, item_difficulty,
             core_lex_percent, condition, tx) |> 
      mutate(
        in_discourse = as.factor(in_discourse),
        discourse_stimuli = as.factor(discourse_stimuli),
        condition = as.factor(condition),
        tx = as.factor(tx),
      )
    
    # dataframe for plot
    check_stats = df_final |>
      mutate(condition = as.factor(condition),
             tx = as.factor(ifelse(tx == 1, "tx", "control")),
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
    
    check_stats_overall = df_final |>
      mutate(condition = as.factor(condition),
             tx = as.factor(ifelse(tx == 1, "tx", "control")),
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
    
    check_stats
    check_stats_overall$in_discourse = "Overall"
    
    # visualize checks
    check_stats |> 
      bind_rows(check_stats_overall) |> 
      pivot_longer(cols = 4:9) |> 
      separate(name, into = c("parameter", "metric"), sep = "__") |> 
      pivot_wider(names_from = "metric", values_from = "value") |> 
      mutate(lb = mean-sd, ub = mean+sd) |> 
      filter(parameter == "item_difficulty") |> 
      ggplot(aes(x = tx, y = mean, fill = condition)) +
      facet_wrap(~in_discourse) +
      geom_col(position = "dodge") +
      geom_point(position = position_dodge(1)) + 
      geom_errorbar(aes(ymin = lb, ymax = ub), position = position_dodge(1), width = 0.5) +
      labs(y = "ITEM DIFFICULTY",
           x = "Control (n = 20) vs. Tx Items (n = 40)",
           fill = "Condition",
           caption = "Error bars represent standard deviation") +
      scale_y_continuous(limits = c(-1, 1)) -> id
    
    check_stats |> 
      bind_rows(check_stats_overall) |> 
      pivot_longer(cols = 4:9) |> 
      separate(name, into = c("parameter", "metric"), sep = "__") |> 
      pivot_wider(names_from = "metric", values_from = "value") |> 
      mutate(lb = mean-sd, ub = mean+sd) |> 
      filter(parameter == "core_lex_percent") |> 
      ggplot(aes(x = tx, y = mean, fill = condition)) +
      facet_wrap(~in_discourse) +
      geom_col(position = "dodge") +
      geom_point(position = position_dodge(1)) + 
      geom_errorbar(aes(ymin = lb, ymax = ub), position = position_dodge(1), width = 0.5) +
      labs(y = "CORE LEXICON PERCENT",
           x = "Control (n = 20) vs. Tx Items (n = 40)",
           fill = "Condition",
           caption = "Error bars represent standard deviation") +
      scale_y_continuous(limits = c(0, 100)) -> clp
    
    check_stats |> 
      bind_rows(check_stats_overall) |> 
      pivot_longer(cols = 4:9) |> 
      separate(name, into = c("parameter", "metric"), sep = "__") |> 
      pivot_wider(names_from = "metric", values_from = "value") |> 
      mutate(lb = mean-sd, ub = mean+sd) |> 
      filter(parameter == "agreement") |> 
      ggplot(aes(x = tx, y = mean, fill = condition)) +
      facet_wrap(~in_discourse) +
      geom_col(position = "dodge") +
      geom_point(position = position_dodge(1)) + 
      geom_errorbar(aes(ymin = lb, ymax = ub), position = position_dodge(1), width = 0.5) +
      labs(y = "NAME AGREEMENT",
           x = "Control (n = 20) vs. Tx Items (n = 40)",
           fill = "Condition",
           caption = "Error bars represent standard deviation") +
      scale_y_continuous(limits = c(0, 100)) -> a
    
   p_mean_sd = id / a / clp
    
    df_final |>
      mutate(condition = as.factor(condition),
             tx = as.factor(ifelse(tx == 1, "tx", "control")),
             in_discourse = as.factor(ifelse(in_discourse == 1, "discourse", "naming_only"))) |> 
      bind_rows(
        df_final |>
          mutate(condition = as.factor(condition),
                 tx = as.factor(ifelse(tx == 1, "tx", "control")),
                 in_discourse = "overall") 
      ) |> 
      ggplot(aes(x = tx, y = item_difficulty, fill = condition)) +
      facet_wrap(~in_discourse) +
      geom_boxplot() +
      scale_y_continuous(limits = c(-2, 2)) -> id2
    
    df_final |>
      mutate(condition = as.factor(condition),
             tx = as.factor(ifelse(tx == 1, "tx", "control")),
             in_discourse = as.factor(ifelse(in_discourse == 1, "discourse", "naming_only"))) |> 
      bind_rows(
        df_final |>
          mutate(condition = as.factor(condition),
                 tx = as.factor(ifelse(tx == 1, "tx", "control")),
                 in_discourse = "overall") 
      ) |> 
      ggplot(aes(x = tx, y = agreement, fill = condition)) +
      facet_wrap(~in_discourse) +
      geom_boxplot() + 
      scale_y_continuous(limits = c(0, 100))  -> a2
    
    df_final |>
      mutate(condition = as.factor(condition),
             tx = as.factor(ifelse(tx == 1, "tx", "control")),
             in_discourse = as.factor(ifelse(in_discourse == 1, "discourse", "naming_only"))) |> 
      bind_rows(
        df_final |>
          mutate(condition = as.factor(condition),
                 tx = as.factor(ifelse(tx == 1, "tx", "control")),
                 in_discourse = "overall") 
      ) |> 
      ggplot(aes(x = tx, y = core_lex_percent, fill = condition)) +
      facet_wrap(~in_discourse) +
      geom_boxplot() +
      scale_y_continuous(limits = c(0, 100)) -> clp2
    
    p_box = id2 / a2 / clp2
    
    
    time_dat = sl |> 
      mutate(rand = rnorm(n())) |> 
      arrange(group, rand) |> 
      mutate(condition = rep_len(seq(1, 3, 1), n())) |> 
      group_by(condition) |> 
      summarize(mean_time = sum(m_time),
                sd_time = sum(sd_time))
    
    
    return(
      list(
        dat = df_final,
        plot1 = p_mean_sd,
        plot2 = p_box,
        time = time_dat
      )
    )
    
}
