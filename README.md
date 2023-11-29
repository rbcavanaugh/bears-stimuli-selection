
# bears-stimuli-selection

last updated on 2023-11-29.

This project uses {renv}. To install relevant packages, run:

``` r
install.packages("renv")
renv::restore()
```

- This project requires \>= R 4.1.0

*note that the development version of the {anticlust} package is
required, and there is a current issue with the {digest} package on
apple silicon m1/m2 (a dependency of rmarkdown). If installation fails,
run
`install.packages('digest', repos = c('https://eddelbuettel.r-universe.dev', 'https://cloud.r-project.org'))`*

## Organization

``` r
fs::dir_tree(recurse = 0)
#> .
#> ├── R
#> ├── README.Rmd
#> ├── README.md
#> ├── VTT_updated_813
#> ├── bears-stimuli-selection.Rproj
#> ├── data
#> ├── english-ewt-ud-2.5-191206.udpipe
#> ├── output
#> ├── renv
#> ├── renv.lock
#> ├── rsconnect
#> ├── shiny
#> └── unused-old
```

- R: *scripts*
  - 01-clean-all-files.R: *first step to clean VTT files*
  - 02-multiword-words.R: *includes functions to add multiword words*
  - 03-timestamp-norming.R: *pulls discourse timestamp data and time to
    complete tasks*
  - 04-join-naming-to-discourse.R: *joins naming data to discourse data*
  - 05-merge-difficulty-variables.R: *merges difficulty variables AoA,
    NPhon, LF*
  - 06-stimuli-selection.R: *instructions for running shiny app in R.
    also see shiny/app.R*
- VTT_updaed_813: *source vtt data files*
- VTT-old: *old source vtt files*
- bears-stimuli-selection.Rproj: *R project file. must be opened to use
  scripts*
- data: *external data files relevant to the processing stream. add new
  external files here*
- output: *files generated by a script as part of the processing strema*
- shiny: *location of shiny app. also includes some files from data and
  output*
  - shiny/R: *scripts for the shiny app*
    -shiny/r/read-in-files-for-shiny-app.R: *start here to update files
    for stimuli selection*
  - shiny/data: *data files for the shiny app*
  - shiny/app.R: *shiny app file*
  - shiny/rsconnect: *shiny app deployment files*

## Overview

Here’s a conceptual outline of the current stimuli selection process:

1.  Given a participants naming ability…
2.  generated a predicted probability of a correct response for all
    items
3.  Pick the best 84 items that appear in the naming battery AND
    discourse stimuli that are closest to p(correct) = 33%.
4.  Pick the best remaining 96 items regardless of whether they appear
    in naming and discoures or just naming. (side note, 84/96 currently
    used because they result in equal size conditions/item type groups.
    we could also do 72/108).
5.  Use the anticlustering algorithm to split the items into 3 condition
    groups of 60, balancing item difficulty, agreement, discourse
    salience, and the % in discourse.
6.  Then for each condition (n = 60), split again into sets of 40
    (treated) and 20 (contorl) balancing these same factors.

## Updating Files.

- To update files, go to the `shiny/r/read-in-files-for-shiny-app.R`
  script and read the notes at the top of the page.

## Republishing the app online

1.  Create a free shinyapps.io lab account at <https://www.shinyapps.io>
2.  Open /shiny/app.R
3.  Click ‘Run App’ in the top right of the script pane to make sure the
    app runs. Check the console for errors
4.  Click the light blue icon in the top right of the scripe pane (next
    to Run)
5.  Follow the instructions to link the new account to your rstudio
    instance
6.  Publish the app. This will open the window in your browser and you
    can copy the link

## Sometimes you might get an error about ‘lgfortran’ not working

1.  Open terminal
2.  Install Homebrew:
    `/bin/bash -c "$(curl -fsSL https://raw.githubusercontent.com/Homebrew/install/HEAD/install.sh)"`

- make sure you run the two lines in “next steps” by pasting them one by
  one after the installation is complete.

3.  Install gfortran: `brew install gcc`
4.  Run the following two lines separately in terminal:

- `mkdir -p ~/.R`
- `touch ~/.R/Makevars`

5.  Open the file `~/.R/Makevars` in a text editor

- to do this, open finder
- click ‘Go’ in the top menu bar
- click ‘Home’
- Hold down shift + command key and press the period key to show hidden
  files
- Open the .R folder
- Open the Makevars file in text editor

6.  Paste the following into the file:

<!-- -->

    FC = /opt/homebrew/Cellar/gcc/11.3.0_2/bin/gfortran
    F77 = /opt/homebrew/Cellar/gcc/11.3.0_2/bin/gfortran
    FLIBS = -L/opt/homebrew/Cellar/gcc/11.3.0_2/lib/gcc/11

7.  Restart RStudio and attempt to install the problematic package again
8.  If it works, run `renv::restore()` again.
