get_time_dat <- function(discourse_items){
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
              mean_prob_correct = mean(mean_p_correct), .by = condition) 
}


get_check_stats <- function(df_final){
  df_final |>
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
}

get_check_stats_overall <- function(df_final){
  df_final |>
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
}

get_p1 <- function(check_stats, check_stats_overall){
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
    scale_y_continuous(limits = c(-2, 2)) -> id
  
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
  
  p = id / a / clp
  return(p)
}


get_p2 <- function(df_final){
  
  
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
  
  p = id2 / a2 / clp2
  return(p)
}


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
  
  
  # dataframes for plot
  # by discourse vs not
  check_stats = get_check_stats(new_dat)
  # overall
  check_stats_overall = get_check_stats_overall(new_dat)
  check_stats_overall$in_discourse = "Overall"
  
  # get plots for tabs 1 and 2
  p_mean_sd = get_p1(check_stats, check_stats_overall)
  p_box     = get_p2(new_dat)
  
  new_dat |> left_join(times, by = c("discourse_stimuli" = "stimuli")) |> 
    rename(stimuli = discourse_stimuli) -> test
  
  time_dat <- test |> 
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
  
  return(
    list(
      dat = new_dat,
      plot1 = p_mean_sd,
      plot2 = p_box,
      time = time_dat,
      error = FALSE
    )
  )
}