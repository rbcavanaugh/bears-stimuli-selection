library(tidyverse)
library(here)

# read in files
df <- readxl::read_excel(
  here("data", "AoA-phonemes-freq_from-ELP.xlsx")
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

# join them together, 
# takke the values from the first file, then the second if missing from the first
# if another file is read in that has additional scores for the SAME words, then
# add a left_join for that file, and another mutate() with ifelse statements
# referencing the columns LgSUBTLCD2 and Age_Of_Acquisition2 which then 
# only get updated with the third dataframe. 
# If adding data for NEW words, then see note below
df_c = df |> 
  left_join(add_kup, by = "Word") |> 
  left_join(add_sbtlx, by = "Word") |> 
  mutate(
    LgSUBTLCD2 = ifelse(is.na(LgSUBTLCD), lf, LgSUBTLCD),
    Age_Of_Acquisition2 = ifelse(is.na(Age_Of_Acquisition), aoa, Age_Of_Acquisition)
  )

# if adding data for new words, organize that dataframe so that it has the same
# four columns as df_combined and then use bind_rows to combine the two dataframes
# resave the values with a new date and update the reference to the files in
# read-in-files-for-shiny-app.R. 
df_combined <- df_c |> 
  select(Word, Age_Of_Acquisition = Age_Of_Acquisition2, LgSUBTLCD = LgSUBTLCD2, NPhon)

write.csv(df_c, row.names = FALSE, file = here("output", "AoA-phonemes-freq_combined_2023_11_02.csv"))
write.csv(df_combined, row.names = FALSE, file = here("output", "AoA-phonemes-freq_combined_2023_11_02.csv"))
write.csv(df_combined, row.names = FALSE, file = here("shiny", "data", "AoA-phonemes-freq_combined_2023_11_02.csv"))

# how many rows are there with complete data?

# initial files
nrow(df_c |> filter(!is.na(Age_Of_Acquisition) & !is.na(LgSUBTLCD) & !is.na(NPhon)))
# 559

# with added values
nrow(df_c |> filter(!is.na(Age_Of_Acquisition2) & !is.na(LgSUBTLCD2) & !is.na(NPhon)))
# 588
