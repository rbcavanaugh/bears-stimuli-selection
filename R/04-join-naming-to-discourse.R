
# This file joins naming and discourse items. 
# It is mostly automated, BUT there is one place noted below that may benefit
# from some hand checking and tweaking. 

library(here)
library(tidyverse)
library(tidytext)
library(readxl)
library(textstem)
library(textclean)
library(udpipe)
library(fuzzyjoin)

# these are the lemmas from discourse
all <- read.csv(here("output", paste0("2023-08-14", "_nounCounts.csv")))

# only keep the ones with sufficient salience
df_30 = all |> 
  mutate(n = as.integer(n), percent = as.double(percent)) |> 
  filter(
    percent >= 25) |> 
  select(stimuli, lemma_hc = lemma, n, percent)

#multiwords from 02-multiword-words.R
multi <- read.csv(here("output", "multiword.csv")) |> 
  filter(percent_found >= .25) |> 
  select(stimuli, lemma_hc = modal_with_spaces, n = total_found, percent = percent_found) |> 
  mutate(percent = percent*100, lemma_hc = str_remove(lemma_hc, " "))

# combine to get all nouns
df_30 <- bind_rows(multi, df_30) |> rename(lemma = lemma_hc)

# this is the naming database file with all the potential naming items
# limit to agreement has to be better than 70. 
naming <- read_csv(here("data", "words-2024-01-29.csv"), col_select = 1:7) |> 
  select(lemma = modal, source, agreement, target = picture) |> 
  distinct() |> 
  filter(agreement > 70) |> 
  group_by(lemma) |> 
  mutate(source = paste(source, collapse = ", ")) |> 
  distinct()

# This joins the naming and discourse items with a tiny bit of flexibility
# for plural/ or other small differences like tshirt vs. shirt.
stringdist_join(
  naming, df_30,
  by = "lemma",
  mode = "left",
  ignore_case = TRUE, 
  method = "jw", 
  max_dist = 0.07, 
  distance_col = "dist"
) |> arrange(desc(dist)) |> 
  select(source, agreement, stimuli, n, percent, lemma_naming = lemma.x, lemma_dis = lemma.y, dist) -> fuzz_join

# this saves the raw output without checking
# If you want to re-recheck the matches, then save this file, 
# open it in excel, and check all of the columns that have a dist > 0
# (inexact matches) and see if there are any that really are a match even if 
# the strings are slightly different. Put a 1 if you consider it a match. leave
# it as NA otherwise. Then saved that file as join_checked.csv and run
# the code below to read it back in and join it to the fuzz_join data. 
# Then re-save the fuzz_join dataframe after the left_join below.
# write.csv(fuzz_join |> mutate(match = NA), here("output", "join_checked.csv"), row.names = FALSE)

## automate the hand-fixed...
## join_checked is a file that I fixed by hand and reuploaded
## so its still hand-fixed, but I'm just re-using the same hand-fixes if
## this script is re-run
fixes <- read.csv(here("output", "join_checked.csv")) |> 
  filter(match == 1, dist > 0) |> 
  select(lemma_naming, lemma_dis, match) |> 
  distinct()

fuzz_join <- fuzz_join |> left_join(fixes, by = c("lemma_naming" = "lemma_naming", "lemma_dis" = "lemma_dis"))

write.csv(fuzz_join, here::here("output", "join_checked_automated.csv"), row.names = FALSE)
write.csv(fuzz_join, here::here("shiny", "data", "join_checked_automated.csv"), row.names = FALSE)




#### Needed these for generating targets for second round of stimuli norming...
#### not used for the processing stream
#### 
df_30$id = seq(1, nrow(df_30), 1)

exact_join = left_join(df_30,  naming, by = "lemma")

with_stim = exact_join |>
  select(stimuli, lemma, source, agreement, target) |>
  group_by(lemma) |>
  summarize(stimuli = paste(stimuli, collapse = ", "))

exact_join2 = left_join(df_30 |> distinct(lemma), naming, by = "lemma") |>
  left_join(with_stim, by = "lemma")

# write.csv(exact_join2, here("data", "discourse_lemmas_matched_to_naming.csv"))
# write.csv(exact_join, here("data", "found_in_discourse_exact.csv"))



