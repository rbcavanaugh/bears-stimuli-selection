
# These are the packages needed
library(here)
library(tidyverse)
library(tidytext)
library(readxl)
library(fuzzyjoin)
library(anticlust) # use dev version!
library(patchwork)

# !!!!!!!!!!! Tip !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
# If you're trying to understand this code and something doesn't make sense
# add print(object_name) where object_name is an object you don't quite understand
# and then run the app and select stimui. it'll print the object and it'll help
# provide some context
# !!!!!!!!!!! End Tip !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

#' Select Stimuli
#'
#' @param participant_theta Naming ability estimate. dbl between 30 and 70
#' @param min_naming_agreement Minimum tolerable naming agreement
#' @param min_discourse_salience Minimum tolerable discourse salience
#' @param target_prob_correct Target Correct percentage. eg., 1/3 or 0.33
#' @param min_discourse_stimuli The minimum number of allowable discourse stimuli
#' @param min_discourse_items The minimum number of allowable discourse items
#' @param seed A seed for reproducibility
#' @param participant_id chr; string for participant
#' @param total_tx_items total number of treatment items. defaults to 180 for study 1
#'
#' @return A list of values for the shiny app. output$dat returns the data
select_stimuli <- function(participant_theta,
                           min_naming_agreement = 70,
                           min_discourse_salience = 30,
                           target_prob_correct = 0.33,
                           min_discourse_stimuli = 9,
                           min_discourse_items = 54,
                           total_tx_items = 180,
                           seed = 42,
                           participant_id){
  
  # function parameters
  # Thte only two that are required are
  # participant_theta (ideally a value between 30 and 70)
  # Based on PNT naming assessment output
  # And a character value for the participant_id like "p42"
  
# -----------------------------------------------------------------------------#
# SETUP
# -----------------------------------------------------------------------------#
  
  # sets the seed
  set.seed(seed)
  
  # function for calculating probility of correct response for a 1-PL model
  # given an ability (theta) and item difficulty (b) and discrimination (0.182)
  p_cor = function(theta, b, discrimination = 0.182){
    exp(discrimination*(theta-b))/(1+exp(discrimination*(theta-b)))
  }
  p_cor = Vectorize(p_cor) # vectorized
  
  # assigm ability and ideal prob correct
  theta = participant_theta #
  ideal_prob_correct = target_prob_correct
  
  # these are placeholder values that can be used when testing the function. 
 # participant_theta = 50
 # min_naming_agreement = 70
 # min_discourse_salience = 30
 # target_prob_correct = 0.333
 # min_discourse_stimuli = 9
 # min_discourse_items = 54
 # seed = 42
 # participant_id = ""
  
  # If min_discourse_stimuli is zero, then just pull items to match
  # them based on naming parameters, ignoring discourse stimuli entirely
  if(min_discourse_stimuli == 0){
    naming_only = 1
  } else {
    naming_only = 0
  }
  
# -----------------------------------------------------------------------------#
# READING IN FILES
# -----------------------------------------------------------------------------#

    # any dataframe that holds the word, the source, and the agreement scores...
    naming <- suppressMessages(read_csv(here("data", "final_database_4-11-23.csv"), col_types = cols())) |> 
      select(lemma = modal, source, agreement, filename = `confirmed file name`) |> 
      distinct() |> 
      filter(agreement >= min_naming_agreement) |> 
      group_by(lemma) |> 
      mutate(source = paste(source, collapse = ", ")) |> 
      distinct()
    
    # this is a joined df where I hand chekced the fuzzy join in 01-new-approach
    # between discourse and naming. it only adds a few items...
  
    fuzz_join = suppressMessages(read_csv(here("data", "join_checked.csv"), col_types = cols())) |> 
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
    times <- read_csv(here("data", "2023-08-14_timestamp.csv"), col_types = cols()) |> 
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
        read_excel(here("data", "AoA-phonemes-freq for singe noun targets with at least 70 percent name agreement from ELP.xlsx")) |> 
          select(Word, LgSUBTLCD, Age_Of_Acquisition, NPhon) |> 
          mutate(LgSUBTLCD = readr::parse_number(LgSUBTLCD),
                 #NPhon = readr::parse_number(NPhon),
                 Age_Of_Acquisition = readr::parse_number(Age_Of_Acquisition)) 
    )
    
    cat("- Setup and Loaded Files \n")
# -----------------------------------------------------------------------------#
# Data wrangling before item selection
# -----------------------------------------------------------------------------#
    
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
    
    # split out into naming only items and items that are also in discourse
    naming_db = cl |> filter(in_discourse == 0)
    discourse_db = cl |> filter(in_discourse == 1)

    cat("- Initial data wrangling \n")
# -----------------------------------------------------------------------------#
# As long as min_discourse_items is not 0, going to pull from discourse_db
# -----------------------------------------------------------------------------#
  if(naming_only != 1){
    
    
    # -----------------------------------------------------------------------------#
    # This is where we select what to do with duplicates at the moment
    # its not a great approach, but it basically takes the discourse item with
    # the highest salience; if ties, the one from the stimuli with the most items
    # and then if tied still, a random from the remaining
    # -----------------------------------------------------------------------------#
    
          # get the closest discourse items to p(correct) = 0.33
          # Do this by taking the absolute value of the difference between
          # ideal_prob_correct and predicted p_correct and then the smallest values
          discourse_items = discourse_db |> ungroup() |> 
            mutate(p_correct = p_cor(!!theta, difficulty),
                   closest = abs(ideal_prob_correct - p_correct),
                   stimuli = as.factor(stimuli)) |> 
            arrange(closest) |> 
            filter(p_correct < 0.75) |> 
            add_count(stimuli) |> 
            filter(n>2) |> 
            mutate(rand_sel = rnorm(n=n())) |> 
            # important! picking item if ...
            slice_min(n = 1, by = lemma_naming,
                      order_by = tibble(percent, n, rand_sel)) |>
            select(-rand_sel) |> 
            ungroup()
          
          # holds summary information for the discourse items
          # Also used to assign the discoures items into different groups
          sl = discourse_items |> 
            summarize(
              m_p = mean(percent, na.rm = TRUE),
              m_d = mean(difficulty, na.rm = TRUE),
              n = n(), .by = "stimuli") |> 
            left_join(times, by = "stimuli") |> 
            arrange(desc(n)) 
          
          #number of discourse items
          nd = nrow(discourse_items)
          #number of discourse stimuli
          ns = nrow(sl)
          
        # Error check #1
        # We need to have ns (the number of discoures stimuli)
        # and nd (number of discoures items) at least as big as the
        # minimums set in the arguments of the function
        # if not, we'll kill the functino here and return these numbers
        # in a list so that the app can display the problem in a pop-up
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
          
          # This is a check for an error I was getting with NA values
          # It was a bug in the code - if it happens something has gone wrong
          # So I've set it to error out at this point (don't return anything)
          # if it occurs again
          if(any(is.na(sl$m_time))){stop("Error: NA in time value")}
          
# -----------------------------------------------------------------------------#
# First anticlustering part. We're matching into sets of 3 triplets based
# on two columns at the moment. 3 and 4 refer to m_d (mean difficulty)
# and n, the total number of naming items found in the discourse item
# -----------------------------------------------------------------------------#
          matched = matching(
            sl[,3:4],
            p = 3, 
          )
          
          # add the grop assignment back to sl
          # group is a number for the triplet. so there will be 3 1's, 3 2's
          # 3 3's etc. There will be as many triplets as possible 
          # So if there are 25 stimuli that have enough items, then there will be
          # 8 triplets, and the last one out will be marked as NA. We assign it
          # to the group with the fewest naming items later on...
          sl$group = matched
          
          # shuffle the discourse stimuli within each triplet so we can
          # randomly assign them to a condition (1, 2, or 3)
          sl2 = sl |> 
            mutate(rand = rnorm(n())) |> 
            arrange(group, rand) |> 
            mutate(condition = rep_len(seq(1, 3, 1), n())) |> 
            mutate(condition = ifelse(is.na(group), NA, condition)) |> 
            arrange(group, desc(n))
          
          # this gets rid of the last one out so that we can add it back in 
          # assigned to a condition in a sec
          sl_out = sl2 |> drop_na(group)
          # get a count (not saved)
          sl_out |> count(condition, wt = n)
          # we're going to get a weighted count of condition weighted by
          # the number of naming items in that condition to find the
          # condition with the fewest
          add_c <- count(sl_out, condition, wt = n) |> arrange(n) |> 
             pull(condition)
          
          # arrange by n descending so that we can take the top one to add
          # the condition to
          non_matched = sl2 |> filter(is.na(group)) |> arrange(desc(n)) 
          
          # only do this if there is an NA value...
          if(length(add_c) >= 1){ 
            tmp = non_matched |> 
              head(1) |> 
              mutate(condition = add_c[1])
            # add the odd man out to the condition with the fewest items
            sl_out = bind_rows(sl_out, tmp)
            # remove it rom non_matched
            non_matched = tail(non_matched, -1)
          }
          
          # count again (not saved)
          sl_out |> count(condition, wt = n)
          
          # repeat the process one more time
          # We won't do it a third time, becuase in that case we would have just
          # had another triplet above and no NA values. 
          add_c <- count(sl_out, condition, wt = n) |> arrange(n) |> 
            pull(condition)
          
          if(length(add_c) >= 1){ 
            tmp = non_matched |> 
              head(1) |> 
              mutate(condition = add_c[1])
            
            sl_out = bind_rows(sl_out, tmp)
            non_matched = tail(non_matched, -1)
          }
            
          sl_out |> count(condition, wt = n)
          
          # only keep the discourse_items that have been assigned. 
          discourse_items = discourse_items |> 
            inner_join(sl_out |> select(-n, -group), by = "stimuli") |> 
            filter(!is.na(condition))
          
          # min_cat is a variable saved which has the lowest number of naming items
          # in any of the three conditinos
          # pulled from the discourse variables 
          min_cat = min(discourse_items |> count(condition) |> pull(n))
          # we also want a total count of discourse items. 
          total_discourse_items = nrow(discourse_items)
          
          # if we don't get enough discourse items, then error out 
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
                    "lowest number of discourse items in a condition", min_cat)
              )
            )
          }
          
          # we want even groups of naming items in each discourse task
          # so lets take the hardest items in each category 
          # up to min_cat for each category
          # we could do it another way, but we need hard items...
          # perhaps this isn't a great idea for very severe people
          # maybe we should take the hardest items as long as they are
          # > 5% predicted correct...as will hula says
          discourse_items = discourse_items |> 
            group_by(condition) |> 
            slice_min(order_by = p_correct, n = min_cat) |> 
            ungroup()
          
          cat("- First matching \n")
# -----------------------------------------------------------------------------#
# Second anticlustering part. We're going to make triplets again, but this time
# we're going to make them so that each triplet contains one item from each
# condition. Basically, we're matching as many items as possible from the conditions
# some items will get left out; thats ok. 
# 
# Now that we've set the chunk above to even out the conditions,
#  we could probably use anticlustering
# for this bit, but this still works so. 
# -----------------------------------------------------------------------------#
          
          # make triplets
          # based on agreement, percent salience, and difficulty
          matching(
            discourse_items[,3:5],
            match_between = discourse_items$condition
          ) -> m2
          # assign back to the data
          discourse_items$m2 = m2
          tmp = discourse_items
          
          # m2 <= 40 here is the upper cap on discourse items per condition. 
          # More than 40 is unlikely anyway, but it also ensures that there
          # are enough non-discourse items remaining to drive the probability
          # of a correct response down for milder folks. 
          discourse_items = discourse_items |> drop_na(m2) |> filter(m2 <= 40)
          
          # save these values again for later use
          discourse_items_per_cat = max(discourse_items$m2)
          discourse_items_total = nrow(discourse_items)
          
          # check that the values are reasonably matched
          # print(mean_sd_tab(discourse_items[, 3:5],
          #                   discourse_items$condition,
          #                   na.rm = TRUE))
          #                   
          cat("- Second matching \n")
# -----------------------------------------------------------------------------#
# Adding naming only items to the discourse items
# We want to add them to the already matched categories to maintain
# the balance, incorporating the work we've already done to match
# see: https://github.com/m-Py/anticlust/issues/47
# -----------------------------------------------------------------------------#
          
          # get the closest remaining items to p(correct) = 0.33
          # the remaining items are all the naming items but also the
          # discourse items that have not yet been assigned. 
          remaining_db = discourse_db |> 
            filter(!(lemma_naming %in% discourse_items$lemma_naming)) |> 
            bind_rows(naming_db)
          
          # how many items are left that need to be assigned. 
          # will change this value for study 2 !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!! STUDY 2 CHANGE HERE
          N_naming = total_tx_items-nrow(discourse_items)
          
          # If we're not hitting anything close to the probabilty correct target
          # bump the target down for the naming items
          if(mean(discourse_items$p_correct)<0.5){
            ideal_prob_correct = ideal_prob_correct
          } else {
            ideal_prob_correct = ideal_prob_correct/2
          }
          
          # again, recalculate the p_correct for these items, 
          # take only the hard enough items
          # And then we want the ones that are closest to our
          # desired p_correct, and only enough to fill the remaining slots
          additional_items = remaining_db |> 
            mutate(p_correct = p_cor(theta, difficulty),
                   closest = abs(ideal_prob_correct - p_correct)) |> 
            filter(p_correct < 0.75) |> 
            distinct(lemma_naming, agreement, difficulty, filename,
                     in_discourse, p_correct, closest) |> 
            arrange(closest) |> 
            head(N_naming)
          
          
          # join it all together so we can divide it up
          # condition assignment is below. 
          dat = bind_rows(discourse_items, additional_items) |> 
            select(word = lemma_naming,
                   in_discourse,
                   discourse_stimuli = stimuli,
                   agreement, item_difficulty = difficulty, core_lex_percent = percent,
                   condition, filename, p_correct) #, target
          
  } else {
    
# -----------------------------------------------------------------------------#
# This is what happens first if we only care about discourse
# some much simpler wrnagling, setting some NA values, but otherwise very
# similar
# -----------------------------------------------------------------------------#
        
    dat = cl |> 
      mutate(p_correct = p_cor(theta, difficulty),
             closest = abs(ideal_prob_correct - p_correct)) |> 
      filter(p_correct < 0.75) |> 
      distinct(lemma_naming, agreement, difficulty, filename, in_discourse, p_correct, closest) |> 
      arrange(closest) |> 
      head(total_tx_items) |> 
      mutate(stimuli = NA, in_discourse = NA, percent = NA, condition = NA) |> 
      select(word = lemma_naming,
             in_discourse,
             discourse_stimuli = stimuli,
             agreement, item_difficulty = difficulty, core_lex_percent = percent,
             condition, filename, p_correct)
    
    # For study 2, will need to add error if not enough items are found here.  ########### Study 2 addition....
    
    discourse_items_total = 0
    discourse_items = NULL
    
    additional_items = tibble(a = rep(0, total_tx_items)) 
    
  }
    
# -----------------------------------------------------------------------------#
# Assigning the remaining items to each condition
# see: https://github.com/m-Py/anticlust/issues/47
# -----------------------------------------------------------------------------#

    sample_this = rep(c(1, 2, 3), length.out = total_tx_items-discourse_items_total)

    # initialize groupings for anticlustering; new items get random group affiliation
    initial_groupings <- c(discourse_items$condition, sample(sample_this) )
    
    # if we don't get 180 items, we need to error out. 
    if(length(initial_groupings) < total_tx_items){
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
            "lowest number of discourse items in a condition", min_cat,
            "number of total items in initial_groupings", length(initial_groupings),
          )
        )
      )
    }
    
    # how many additional items are needed
    N = nrow(additional_items)
    

    # Here's where new groups are assigned.
    # They're balanced for agreement and item difficulty
    new_groups <- anticlustering(
      dat[,4:5],
      K = initial_groupings,
      objective = "kplus",
      method = "local-maximum",
      categories = c(discourse_items$condition, rep(4, N))
    )
    
    # "old" items are still grouped together, new items are assigned evenly to groups 1-3:
    table(new_groups, c(discourse_items$condition, rep(4, N)))
    
    # assign the new groups back to the data
    dat$condition_all = new_groups
    
    # check quality of the solution:
    mean_sd_tab(dat[,4:6], dat$condition_all, na.rm = TRUE)
    #mean_sd_tab(dat |> drop_na(condition) |> select(4:6), dat |> drop_na(condition) |> pull(condition), na.rm = TRUE)
    
    cat("- Naming Items added to discourse \n")
# -----------------------------------------------------------------------------#
# Final assignment is for treated vs. control words
# -----------------------------------------------------------------------------#
    
    # note whether the core lex score is imputed or not. 
    # If we dont hvae core lex percent (because not a discourse item)
    # we just imput with the mean so it doens't impact the algorithm
    dat$core_lex_percent_imp = dat$core_lex_percent
    dat$core_lex_percent_imp[is.na(dat$core_lex_percent)] <- mean(dat$core_lex_percent, na.rm = TRUE)
    
    # nest the data into three dataframes by condition so we can 
    # loop over the conditions with anticlustering
    dat_nest = dat |> 
      nest_by(condition_all)
    
    items_per_condition = total_tx_items/3
    ntx = round(items_per_condition*(2/3))
    #print(ntx)
    ncontrol = items_per_condition - ntx
    #print(ncontrol)
    # for each condition, assign into treated and untreated
    # operates slightly differently if we're just pulling 
    # from naming items (the else) or not. 
    dat_out = list()
    if(naming_only != 1){
        for(i in 1:nrow(dat_nest)){
          tmp = dat_nest$data[[i]]
          gr <- anticlustering(
            tmp[, c(4, 5, 10)], # use the variables directly
            K = c(ntx, ncontrol),
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
          K = c(ntx, ncontrol),
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
    
    cat("- Items assigned to tx and control \n")
# -----------------------------------------------------------------------------#
# THIS IS THE FINAL DATASET WOO!!
# -----------------------------------------------------------------------------#
    
    # final dataset
    # bind the condition nested data back together
    # select and rename columns as needed
    # set certain variables to factor
    # save the input criteria for reproducibility
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
    
    cat("- Final dataset generated \n")
# -----------------------------------------------------------------------------#
# Data wrangling for app output and plots
# check app_functions.R for this stuff
# -----------------------------------------------------------------------------#
    
    # dataframes for plot
    # by discourse vs not
    check_stats = suppressMessages(get_check_stats(df_final))
    # overall
    check_stats_overall = suppressMessages(get_check_stats_overall(df_final))
    check_stats_overall$in_discourse = "Overall"
  
    # get plots for tabs 1 and 2
    p_mean_sd = suppressMessages(get_p1(check_stats, check_stats_overall, naming_only))
    p_box     = suppressMessages(get_p2(df_final, naming_only))

    # get table for time dat
    if(naming_only != 1){
      time_dat = suppressMessages(get_time_dat(discourse_items))
    } else {
      time_dat = tibble(data = NA)
    }
    
    cat("- Plotting and summary tables generated \n")
    cat("- Stimuli selection complete ┏(-_-)┛ ┗(-_-)┓ ┗(-_-)┛ ┏(-_-)┓ \n")
# -----------------------------------------------------------------------------#
# If everything goes to plan, this is the list returned by the function
# -----------------------------------------------------------------------------#
    return(
      list(
        dat = df_final,
        plot1 = p_mean_sd,
        plot2 = p_box,
        time = time_dat,
        error = FALSE
      )
    )
    
}



#' Scores an uploaded file for generating an input file. 
#'
#' @param new_dat 
#'
#' @return
#' @export
#'
#' @examples
score_upload <- function(new_dat){
  
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
  
  naming_only = ifelse(unique(new_dat$min_discourse_stimuli)==0, 1, 0)
  
  # dataframes for plot
  # by discourse vs not
  check_stats = get_check_stats(new_dat)
  # overall
  check_stats_overall = get_check_stats_overall(new_dat)
  check_stats_overall$in_discourse = "Overall"
  
  # get plots for tabs 1 and 2
  p_mean_sd = get_p1(check_stats, check_stats_overall, naming_only)
  p_box     = get_p2(new_dat, naming_only)
  
  new_dat |> left_join(times, by = c("discourse_stimuli" = "stimuli")) |> 
    rename(stimuli = discourse_stimuli)  |> 
    drop_na(stimuli) |> 
    add_count(condition) |> 
    group_by(stimuli) |> 
    mutate(mean_p_correct = mean(p_correct)) |> 
    ungroup() |> 
    distinct(stimuli, n, m_time, sd_time, mean_p_correct, condition) |> 
    add_count(condition, name = "n_discourse_tasks") |> 
    summarize(mean_time = sum(m_time),
              sd_time = sum(sd_time),
              n_discourse_words = mean(n),
              n_discourse_tasks = mean(n_discourse_tasks),
              mean_prob_correct = mean(mean_p_correct), .by = condition) 
  
  # get table for time dat
  #time_dat = get_time_dat(discourse_items)
  
  # input_file <- create_app_input_file(new_dat, naming_only = naming_only)
  
  # return various bits that can be assigned to the reactive values in the app
  return(
    list(
      dat = new_dat,
      plot1 = p_mean_sd,
      plot2 = p_box,
      time = tibble(data = NA),
      #input_file = input_file,
      error = FALSE
    )
  )
}
