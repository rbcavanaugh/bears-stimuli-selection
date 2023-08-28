# These are the packages needed
library(here)
library(tidyverse)
library(tidytext)
library(readxl)
library(fuzzyjoin)
library(anticlust) # use dev version!
library(patchwork)

# load files from shiny app
for(i in list.files(here::here("shiny", "R"), full.names = TRUE)){source(i)}

# Hey! this is all available in the shiny app. You can run it by doing
source(here::here("shiny", "app.R"))
here()
shinyApp(ui, server)

# or you can just navigate to shiny/app.R and hitting run app
# but if you don't want to use the shiny app, here's how to do it line by line:

# run the select_stimuli() function
# There are additional arguments - look at the script if you need to change them
ss <- select_stimuli(
  participant_theta = 48,
  participant_id = "p1",
  shiny = FALSE,
  total_tx_items = 500
)

ss$dat |> count(condition, tx) |> arrange(desc(tx), condition)

# selected stimuli
head(ss$dat)

# but there's more returned - its a list of stuff
glimpse(ss) 

# for example, you can look at the a plot
ss$plot1

# or statistics about the chosen stimuli
ss$time

# you can use the ss$dat object to generate the app input file
# you also need to say which group goes with which study condition
# c1 = the first condition, c2, second etc
# you can give values of em, am, or eab. be careful not to use them more than once
app_input = create_app_input_file(ss$dat, c1 = "em", c2 = "am", c3 = "eab")

# preview app input file
head(app_input)
