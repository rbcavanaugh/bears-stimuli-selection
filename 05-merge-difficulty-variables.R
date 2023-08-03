library(tidyverse)
library(here)

df <- readxl::read_excel(
  here("data", "AoA-phonemes-freq for singe noun targets with at least 70 percent name agreement from ELP.xlsx")
) |> 
  mutate(Age_Of_Acquisition = readr::parse_number(Age_Of_Acquisition),
         Word = tolower(Word))

add_kup <- readxl::read_excel(
  here("data", "Copy of AoA_ratings_Kuperman_et_al_BRM.xlsx")
) |> 
  select(Word, aoa = Rating.Mean) |> 
  mutate(Word = tolower(Word))

add_sbtlx <- readxl::read_excel(
  here("data", "SUBTLEXusExcel2007.xlsx")
) |> 
  select(Word, lf = Lg10CD) |> 
  mutate(Word = tolower(Word))

df_c = df |> 
  left_join(add_kup, by = "Word") |> 
  left_join(add_sbtlx, by = "Word") |> 
  mutate(
    LgSUBTLCD2 = ifelse(is.na(LgSUBTLCD), lf, LgSUBTLCD),
    Age_Of_Acquisition2 = ifelse(is.na(Age_Of_Acquisition), aoa, Age_Of_Acquisition)
  )

write.csv(df_c, row.names = FALSE, file = "data/AoA-phonemes-freq_joined_2023-08-02.csv")

nrow(
  df_c |> filter(!is.na(Age_Of_Acquisition) & !is.na(LgSUBTLCD))
)

# 559

nrow(
  df_c |> filter(!is.na(Age_Of_Acquisition2) & !is.na(LgSUBTLCD2))
)

# 588
