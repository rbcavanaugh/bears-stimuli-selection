# Picturable
# 
library(here)
library(tidyverse)
library(tidytext)
library(readxl)
library(textstem)
library(textclean)
library(udpipe)
library(fuzzyjoin)

# files = list.files(here("check-nouns hand corrected", "Final Noun Lists"),full.names = TRUE, pattern = ".xlsx" )
# fileNames = list.files(here("check-nouns hand corrected", "Final Noun Lists"), pattern = ".xlsx" )
# 
# dl = list()
# for(i in 1:length(files)){
#   
#   dl[[i]] = read_excel(files[[i]], col_types = "text", sheet = 1) |> 
#     mutate(source = fileNames[[i]])
#   
# }
# 
# all = bind_rows(dl) |> select(-`...11`, -`...12`)
# rm(dl)

all <- read.csv(here("output", paste0("2023-07-21", "_nounCounts.csv")))

df_30 = all |> 
  mutate(n = as.integer(n), percent = as.double(percent)) |> 
  filter(
    # isPicturable == 1,
    # notNoun == 0,
    percent >= 30) |> 
  select(stimuli, lemma_hc = lemma, n, percent)

#multiwords
multi <- read.csv("data/multiword.csv") |> 
  filter(percent_found >= 30) |> 
  select(stimuli, lemma_hc = modal_with_spaces, n = total_found, percent = percent_found) |> 
  mutate(percent = percent*100, lemma_hc = str_remove(lemma_hc, " "))

df_30 <- bind_rows(multi, df_30) |> rename(lemma = lemma_hc)

naming <- read_csv(here("data", "final_database_4-11-23.csv")) |> 
  select(lemma = modal, source, agreement, target = `confirmed file name`) |> 
  distinct() |> 
  filter(agreement > 70) |> 
  group_by(lemma) |> 
  mutate(source = paste(source, collapse = ", ")) |> 
  distinct()

stringdist_join(
  naming, df_30,
  by = "lemma",
  mode = "left",
  ignore_case = TRUE, 
  method = "jw", 
  max_dist = 0.1, 
  distance_col = "dist"
) |> arrange(desc(dist)) |> 
  select(source, agreement, stimuli, n, percent, lemma_naming = lemma.x, lemma_dis = lemma.y, dist)-> fuzz_join

#write.csv(fuzz_join, "data/join_check.csv")

# repeat it the other way...

df_30$id = seq(1, nrow(df_30), 1)

exact_join = left_join(df_30,  naming, by = "lemma")
exact_join2 = left_join(df_30 |> distinct(lemma), naming, by = "lemma")

write.csv(exact_join2, here("data", "discourse_lemmas_matched_to_naming.csv"))
write.csv(exact_join, here("data", "found_in_discourse_exact.csv"))

stringdist_join(
  df_30, naming,
  by = "lemma",
  mode = "left",
  ignore_case = TRUE, 
  method = "jw", 
  max_dist = 0.1, 
  distance_col = "dist"
) |> arrange(desc(dist)) |> 
  select(id, stimuli, n, percent, lemma_dis = lemma.y, lemma_naming = lemma.x, source, agreement,  dist, target) %>% 
  filter(dist > 0) -> fuzz_join2

#write.csv(fuzz_join2, "data/found_in_discourse_fuzzy.csv")





