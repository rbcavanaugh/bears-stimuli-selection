

library(here)
library(tidyverse)
library(tidytext)
library(readxl)
library(fuzzyjoin)
library(anticlust) # use dev version!

# any dataframe that holds the word, the source, and the agreement scores...
naming <- read_csv("data/naming-battery-items.csv") |> 
  select(lemma = modal, source, agreement) |> 
  filter(agreement > 70) |> 
  group_by(lemma) |> 
  mutate(source = paste(source, collapse = ", ")) |> 
  distinct()

# this is a joined df where I hand chekced the fuzzy join in 01-new-approach
# between discourse and naming
fuzz_join = read.csv("data/join_checked.csv") |> 
  mutate(found = ifelse(is.na(lemma_dis) | drop == 1, 0, 1)) |> 
  filter(percent >= 30, agreement >= 70) |> 
  select(source, lemma_naming, stimuli, percent, lemma_dis, found)

# total number of unique found words between naming and discourse
fuzz_join |> 
  filter(found == 1) |> 
  distinct(lemma_naming)  |> nrow()

# dataframe holding AoA, phonemes, lexical freq necessary to calculate item difficulty
item_params = read.csv("data/item_parameters.csv")

# join together and calculate item difficulty
diff = naming |> 
  full_join(item_params, by = c("lemma" = "Word")) |> 
  left_join(fuzz_join, by = join_by(lemma == lemma_naming, source)) |> 
  mutate(difficulty = -1.22 + 0.15*NPhon -0.36*LgSUBTLCD + 0.21*Age_Of_Acquisition)

# best agreement from naming with a difficulty score. should go up in
# size after getting more difficulty scores
cl = diff |> 
  select(lemma_naming = lemma, stimuli, agreement, percent, difficulty) |> 
  mutate(in_discourse = ifelse(is.na(stimuli), 0, 1)) |> 
  group_by(lemma_naming) |> 
  filter(agreement == max(agreement)) |> 
  slice_sample(n = 1) |> 
  drop_na(difficulty) |> 
  ungroup()

# split out into naming only items and items that are also in discourse
naming_db = cl |> filter(in_discourse == 0)
discourse_db = cl |> filter(in_discourse == 1)

# assigm ability and difficulty
theta = 0 # ability
b = -1    # difficulty

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
         closest = abs(ideal_prob_correct - p_correct)) |> 
  arrange(closest) |> 
  head(n_discourse)

# get the closest remaining items to p(correct) = 0.33
remaining_db = discourse_db |> 
  filter(!(lemma_naming %in% discourse_items$lemma_naming)) |> 
  bind_rows(naming_db)

additional_items = remaining_db |> 
  mutate(p_correct = p_cor(theta, difficulty),
         closest = abs(0.333 - p_correct)) |> 
  arrange(closest) |> 
  head(n_naming)
  
# join it all together so we can divide it up
dat = bind_rows(discourse_items, additional_items) |> arrange(p_correct) |> 
  select(word = lemma_naming, in_discourse, discourse_stimuli = stimuli, agreement, item_difficulty = difficulty, core_lex_percent = percent) |> 
  arrange(item_difficulty)

# Mean imputation:
# this is necessary because the anticlustering algorithm doesn't handle NA values well
# essentially we're going to assume that any naming items have the mean salience
# score from discourse items. When we check the final balance, we'll check it 
# with these items back as NA so that it doesn't influence the item checking at the end.
dat$core_lex_percent_imp = dat$core_lex_percent
dat$core_lex_percent_imp[is.na(dat$core_lex_percent)] <- mean(dat$core_lex_percent, na.rm = TRUE)

# first anticlustering step. 3 groups of 60 AND balance for the in_discourse variable
groups <- anticlustering(
  dat[, c(4, 5, 7)], # use the variables directly
  K = c(60,60, 60),
  method = "local-maximum",
  categories = dat$in_discourse,
  repetitions = 100,
  objective = "kplus",
  standardize = TRUE
)

# create a new column using the groups generated
dat$group <- groups

# validate on original data (not imputed):
mean_sd_tab(dat[, 4:6], dat$group, na.rm = TRUE)
#     agreement         item_difficulty     core_lex_percent
#     "90.77 (8.69)"    "0.54 (0.37)"       "51.95 (16.56)" 
#     "90.77 (8.68)"    "0.54 (0.37)"       "51.95 (16.58)" 
#     "90.76 (8.68)"    "0.54 (0.37)"       "52.08 (16.58)" 

table(dat$group, dat$in_discourse)
# group(y) by in_discourse(x)
#    0  1
# 1 36 24
# 2 36 24
# 3 36 24

# nest the data into three dataframes by condition
dat_nest = dat |> 
  nest_by(group)

# for each one, repeat the process into treated and untreated
dat_out = list()
for(i in 1:nrow(dat_nest)){
  tmp = dat_nest$data[[i]]
  gr <- anticlustering(
    tmp[, c(4, 5, 7)], # use the variables directly
    K = c(40, 20),
    method = "local-maximum",
    categories = tmp$in_discourse,
    repetitions = 100,
    objective = "kplus",
    standardize = TRUE
  )
  tmp$tx = ifelse(gr == 1, 1, 0)
  tmp$group = i
  dat_out[[i]] = tmp
}

# final dataset
df_final = bind_rows(dat_out)

# checks
mean_sd_tab(dat[, 4:6], dat$group, na.rm = TRUE)

# dataframe for plot
check_stats = df_final |>
  mutate(group = as.factor(group),
         tx = as.factor(ifelse(tx == 1, "tx", "control")),
         in_discourse = as.factor(ifelse(in_discourse == 1, "discourse", "naming_only"))) |> 
  group_by(group, tx, in_discourse) |> 
  summarize(
    item_difficulty__mean = mean(item_difficulty),
    core_lex_percent__mean = mean(core_lex_percent, na.rm = TRUE),
    agreement__mean = mean(agreement),
    item_difficulty__sd = sd(item_difficulty),
    core_lex_percent__sd = sd(core_lex_percent, na.rm = TRUE),
    agreement__sd = sd(agreement)
  ) 

check_stats

# visualize checks
check_stats |> 
  pivot_longer(cols = 4:9) |> 
  separate(name, into = c("parameter", "metric"), sep = "__") |> 
  pivot_wider(names_from = "metric", values_from = "value") |> 
  mutate(lb = mean-sd, ub = mean+sd) |> 
  ggplot(aes(x = tx, y = mean, fill = group)) +
  facet_wrap(in_discourse~parameter, scales = "free") +
  geom_col(position = "dodge") +
  geom_point(position = position_dodge(1)) + 
  geom_errorbar(aes(ymin = lb, ymax = ub), position = position_dodge(1), width = 0.5) +
  labs(y = "Mean value of 3 balancing factors",
       x = "Control (n = 20) vs. Tx Items (n = 40)",
       fill = "Condition",
       caption = "Error bars represent standard deviation")
  
  
