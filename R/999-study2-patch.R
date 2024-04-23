library(tidyverse)
d3 <- read_csv("~/Downloads/d3_stimuli_2024-04-17.csv")
d3 %>% filter(in_discourse == 1) %>% count(condition, tx)
d3 %>% count(condition, tx)


library("remotes") # if not available: install.packages("remotes")
install_github("m-Py/anticlust")


test = test %>% select(-1)
dat_nest = test |> 
  nest_by(condition_all)
i=3
tmp = dat_nest$data[[i]]
ntx_tmp = ifelse(nrow(tmp)==220, 200, 40)
control_tmp = 20
#print(tmp)
gr <- anticlustering(
  tmp[, c(4, 5, 10)], # use the variables directly
  K = c(ntx_tmp, control_tmp),
  method = "local-maximum",
  categories = tmp$in_discourse,
  repetitions = 100,
  objective = "variance",
  standardize = TRUE
)
# print(gr)
# 
tmp$tx = ifelse(gr == 1, 1, 0)
tmp %>% count(in_discourse, tx) %>% arrange()



tmp$condition = dat_nest$condition_all[i]
dat_out[[i]] = tmp


n_tx = 220

in_discourse1 <- tmp[tmp$in_discourse == 1, ]
in_discourse0 <- tmp[tmp$in_discourse == 0, ]

n_discourse <- nrow(in_discourse1)
n_not_discourse <- nrow(in_discourse0)

n_discourse_untx <- ifelse(round(n_discourse*0.33) < 10, 10, round(n_discourse*0.33))
n_discourse_tx <- n_discourse - n_untx

n_naming_tx = n_tx - n_discourse_tx - 20
n_naming_untx = 20 - n_discourse_untx

groups1 <- anticlustering(
  in_discourse0[,c(4, 5, 10)], 
  K = c(140, 20),
  method = "local-maximum",
  repetitions = 100,
  objective = "kplus",
  standardize = TRUE
)

groups2 <- anticlustering(
  in_discourse1[,c(4, 5, 10)], 
  K = c(10, 10), # not sure which number add up
  method = "local-maximum",
  repetitions = 100,
  objective = "kplus",
  standardize = TRUE
)











library(anticlust)
library(devtools)

dat <-  devtools::source_gist("d155e04609ede056428004ae460dbc4b")$value # wow, magic

# Mean imputation:
copy <- dat
copy$core_lex_percent[is.na(copy$core_lex_percent)] <- mean(copy$core_lex_percent, na.rm = TRUE)

groups <- anticlustering(
  copy[, 4:6], # use the variables directly
  K = c(160, 20),
  method = "local-maximum",
  categories = copy$in_discourse,
  repetitions = 100,
  objective = "kplus",
  standardize = TRUE
)

dat$group <- groups

# validat on original data (not imputed):
mean_sd_tab(dat[, 4:6], dat$group, na.rm = TRUE)
agreement      item_difficulty core_lex_percent
#> 1 "90.44 (9.07)" "-0.06 (0.73)"  "51.04 (15.90)" 
#> 2 "90.43 (9.06)" "-0.06 (0.73)"  "51.04 (15.90)" 
#> 3 "90.40 (9.06)" "-0.06 (0.73)"  "50.87 (15.89)" 
#> 4 "90.42 (9.03)" "-0.06 (0.72)"  "51.08 (16.03)" 

table(dat$group, dat$in_discourse)
0  1
1 22 18
2 22 18
3 22 18
4 34 26