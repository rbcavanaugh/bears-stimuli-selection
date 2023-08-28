library(shiny)
library(waiter)
library(shinyjs)

# this is automatically sourced for the shiny app since its in the R folder...
# it contains the logic for stimuli selection
# this file (mostly) just holds the logic for the app
# source(here::here("shiny", "R", "05-stimuli selection-working-version.R"))

# Define UI
ui <- fluidPage(
  # making the error message a little more obvious
  tags$head(
    tags$style(HTML("
      .shiny-output-error-validation {
        color: #000000;
        font-weight: bold;
        font-style: italic;
        margin: auto;
        padding: 30px;
        width: 50%;
      }
    "))
  ),
  # enables js for the loading spinny thingy
  waiter::use_waiter(),
  # allows disabling/enabling buttons
  shinyjs::useShinyjs(),
  # Application title
  titlePanel("BEARS stimuli selection"),
  # layout of the page. sidebar + main page
  sidebarLayout(
    
    # Sidebar content
    sidebarPanel(
      # Participant id widget
      textInput(inputId = "participant", label = "Participant ID"),
      # Naming ability widget
      numericInput(inputId = "theta",
                   label = "Naming Ability T-scale ~N(50, 10)",
                   # these are the potential values for the widget
                   value = 50, min = 30, max = 70, step = 0.1),
      # When you hit submit, it runs the function in the stimuli selection script
      actionButton("submit", "Select Stimuli"),
      # Enables download of the stimuli file (not for the treatment app though)
      downloadButton("download_stim", "Download Stimuli"),
      hr(),
      # Allows uploading a file with manual corrections as long as its in the 
      # same for mat as the download_stim file. 
      fileInput("file1", "Upload existing stimuli file", accept = ".csv"),
      hr(),
      # These are additional inputs for tweaking the stimuli selection algorithm
      # They are set by default and don't need to be changed by the user. 
      radioButtons(inputId = "total_tx_items",
                    label = "Study (Total probe words)",
                   choices = c("Study 1 (180)" = 180,
                               "Study 2 (500)" = 500), inline = TRUE
      ),
      numericInput(inputId = "min_naming_agreement",
                   label = "Min picture naming agreement allowed (%)",
                   value = 70, min = 50, max = 100, step = 1),
      numericInput(inputId = "min_discourse_salience",
                   label = "Min discourse salience allowed (%)",
                   value = 30, min = 20, max = 50, step = 1),
      numericInput(inputId = "target_prob_correct",
                   label = "Target baseline probe naming accuracy",
                   value = 0.33, min = 0, max = 0.66, step = 0.01),
      numericInput(inputId = "min_discourse_stimuli",
                   label = "Min number of discourse items (0 = naming only)",
                   value = 9, min = 0, max = 18, step = 1),
      numericInput(inputId = "min_discourse_items",
                   label = "Min number of probe words salient in discourse items",
                   value = 54, min = 30, max = 90, step = 3),
      numericInput(inputId = "seed",
                   label = "Set seed for reproducibility",
                   value = 42, min = 1, max = 100000, step = 1)
      
    ),
    
    # This shows the results of the calculation in various formats
    # Also has a tab for allowing download of the treatment app input file
    mainPanel(
      # These are the tabs, in order. Look at output$XXX in the server side
      # to see what each tab outputs. for example output$p1 for the first plot.
      tabsetPanel(
        # Main summary stats for different conditions
        tabPanel(title = "Summary (mean + sd)",
                 plotOutput("p1", height = "80vh")),
        # robust stats with tukey box plots
        tabPanel(title = "Summary (box plot)",
                 plotOutput("p2", height = "80vh")),
        # Information about testing time anticipated and number of discoures items
        tabPanel(title = "Testing Time",
                 tableOutput("timetab"),
                 p("Table shows mean duration(minutes) and mean + 1SD duration from norming")),
        # Preview of the stimuli selected
        tabPanel(title = "Preview stimuli",
                 div(style = 'overflow-y: scroll; height: 70vh;', tableOutput("tab"))),
        # Tab to generate the input file once stimuli have been selected
        tabPanel(title = "Generate Input File",
                 shiny::fluidRow(
                   br(),
                   column(width = 10, align = "center",
                    # table of stimuli by group
                    tableOutput("stimuli_table")
                   )
                 ),
                 shiny::fluidRow(
                   # these inputs allow the user to set which condition goes with which group
                   shiny::column(width = 5, offset = 1,
                               selectInput("g1",  label = "Group 1",
                                           choices = c("Effort Maximized" = "em",
                                                       "Accuracy Maximized" = "am",
                                                       "Balanced" = "eab")),
                               selectInput("g2",  label = "Group 2",
                                           choices = c("Accuracy Maximized" = "am",
                                                       "Balanced" = "eab",
                                                       "Effort Maximized" = "em")),
                               selectInput("g3",  label = "Group 3",
                                           choices = c("Balanced" = "eab",
                                                       "Effort Maximized" = "em",
                                                       "Accuracy Maximized" = "am"))),
                   br(),
                   # buttons to generate the input file. note the latter is disabled to start
                   shiny::column(width = 3, offset = 1,
                                 br(),
                                 actionButton("generate_input", "Generate Input File"),
                                 br(),br(),
                                 shinyjs::disabled(
                                  downloadButton("download_input", "Download Input File")
                                 )
                                 
                                 )
                 )
                ),
        tabPanel(title = "Preview input file",
                 # preview the input file that's generated
                 tableOutput("preview_input")),
        # this is the about page and holds the about page text. 
        tabPanel(title = "About",
                 div(
                   h4("Getting Started"),
                   p("To get started using the app, enter a participant ID, and select a naming
                     ability that was generated from the PNT. The naming ability score must be between
                     -30 and 70. After entering these values, you can hit the 'Select Stimuli' button."),
                   p("The additional choices in the bottom of the sidebar menu are there in case we need
                   to make minor adjustments to the parameters that affect stimuli selection (for example, 
                   if we needed to relax the agreement criteria to pull from additional eligible items."),
                   p("In the hopefully rare case that someone is too mild and we can't generate enough
                     discourse items for them, you can set the 'Min number of discourse items (0 = naming only)'
                     field to 0. This will produce three balanced lists for naming items ignoring
                      the discourse task entirely."),
                   p("When you've selected your stimuli, you can preview them in the 'Preview stimuli' tab
                     which shows the essential columns. you can download this file using the 'download stimuli 
                     button. Note that the downloaded file includes additional columns which are necessary
                     for full reproducibility."),
                   p("At this stage, the conditions are only a value of '1', '2', and '3' but are not
                   yet assigned to experimental conditions. The plan for this step is to do it in redcap?
                   The generate input tab includes a brief table of which stimuli are included in each 
                   yet-to-be-assigned condition. Once you're ready, use this tab to assign an experimental
                   condition (effort maximized, accuracy maximized, effort-accuracy balanced) to each condition.
                   You can then generate the input file for the web app and download this file. It should be saved 
                   along with the stimuli selection file as it documents the condition assignments."),
                   p("If you need to, you can upload a downloaded stimuli file later (bottom left of page),
                   and then make the condition assignments - they don't need to be done right away."),
                   p("Please contact Rob with any issues/bugs. This app is still experimental.")
                 )
        )
      )
      
    )
  )
)

# Server logic
server <- function(input, output, session) {

# -----------------------------------------------------------------------------#
# Setup
# -----------------------------------------------------------------------------#

  # initializes the spinny thingy
  w <- Waiter$new(id = c("p1", "p2"),
                  html = spin_3(), 
                  color = transparent(.5))
  
  # holds all the data that can change throughout the app
  v <- reactiveValues(
    output = NA,
    input_file_ready = 0
  )
  
# -----------------------------------------------------------------------------#
# Runs stimuli selection command
# -----------------------------------------------------------------------------#

  observe({print(as.numeric(input$total_tx_items))})
  # what happens when you hit the submit button
  # first, validate that the participant ID has a value
  # Then show the spinny thingy
  # then run the select_stimuli() function from the stimuli selection script with
  # all of the inputs from the sidebar, most of which are set by default
  observeEvent(input$submit,{
    if(input$participant == ""){
      showNotification("Error: Participant ID should not be blank. Enter ID and re-run",
                       type = "error")
    } 
    req(nchar(input$participant)>=1)
    w$show()
    print(input$total_tx_items)
    v$output = select_stimuli(participant_theta      = input$theta,
                              min_naming_agreement   = input$min_naming_agreement,
                              min_discourse_salience = input$min_discourse_salience,
                              target_prob_correct    = input$target_prob_correct,
                              min_discourse_stimuli  = input$min_discourse_stimuli,
                              min_discourse_items    = input$min_discourse_items,
                              total_tx_items         = as.numeric(input$total_tx_items),
                              seed                   = input$seed,
                              participant_id         = input$participant
                              )
    
  })
  
  
# -----------------------------------------------------------------------------#
# What happens when not enough items are selected!
# -----------------------------------------------------------------------------#
  
  # this is the output for the error table when not enough stimuli from a category are selected
  output$error_table <- renderTable({
    req(!is.na(v$output$error_detail))
   v$output$error_detail
  }, digits = 0)
  
  # shows the modal for the error table
  observeEvent(v$output,{
    req(!is.na(v$output))
    if(isTRUE(v$output$error)){
      showModal(modalDialog(
        title = "Error",
        div(
          p("There were too few discourse items or tasks eligible based on participant naming ability:"),
          tableOutput("error_table")
        ),
        easyClose = TRUE
      ))
    }
  })

# -----------------------------------------------------------------------------#
# When you hit the generate_input button, this creates the app input file
# -----------------------------------------------------------------------------#
  observeEvent(input$generate_input,{
    req(!is.na(v$output))
    # if no discourse items, set naming_only to 1. 
    naming_only = ifelse(input$min_discourse_stimuli == 0, 1, 0)
    # validation for condition assignment
    if(length(unique(c(input$g1, input$g2, input$g3)))!=3){
          showNotification(
           "Each group needs to have a different condition",
          type = "error"
          )
    } else {
    # function to generate the app input file
    # its in app_functions.R
      v$input_file <- create_app_input_file(
            v$output$dat,
            naming_only = naming_only,
            c1 = input$g1,
            c2 = input$g2,
            c3 = input$g3
        )
      # show a notification when its done
      showNotification(
        "Input file ready to download",
        type = "message"
      )
      # set the variable file ready to 1, and enable the download button
      v$input_file_ready = 1
      shinyjs::enable("download_input")
    }
  })

# -----------------------------------------------------------------------------#
# Content for the other tabs
# -----------------------------------------------------------------------------#

  # Previewing the input file 
  output$preview_input <- renderTable({
    validate(
      need(v$input_file_ready == 1,
           message = "Generate input file to preview")
    )
    v$input_file 
  }, 
  digits = 0)
  
  # preview of the stimuli selected table. 
  output$tab <- renderTable(({
    req(!is.na(v$output))
    req(!isTRUE(v$output$error))
    v$output$dat |> select(word:filename)
  }))
  
  # Plot 1 output
  output$p1 <- renderPlot({
    validate(
      need(!is.na(v$output), message = "Start by entering a Participant ID and setting a naming ability value, and selecting 'Select Stimuli'")
      )
    req(!is.na(v$output))
    req(!isTRUE(v$output$error))
    v$output$plot1
  })
  
  # Plot 2 output
  output$p2 <- renderPlot({
    req(!is.na(v$output))
    req(!isTRUE(v$output$error))
    v$output$plot2
  })
  
  # the time table output
  output$timetab <- renderTable(({
    req(!is.na(v$output))
    req(!isTRUE(v$output$error))
    v$output$time
  }))
  
  # This is the table of stimuli by condition to help with group assignment
  output$stimuli_table <- renderTable({
    validate(
      need(!is.na(v$output),
             message = "Run algorithm or upload a stimuli file to show stimuli by group")
    )
    req(!is.na(v$output))
    v$output$dat |> filter(in_discourse == 1) |>
      group_by(condition) |>
      distinct(discourse_stimuli) |> 
      arrange(discourse_stimuli) |> 
      mutate(obs = 1, num = cumsum(obs)) |>
      pivot_wider(names_from = condition, values_from = discourse_stimuli) |>
      select(stim_number = num, group_1 = `1`, group_2 = `2`, group_3 = `3`)
  })
  

# -----------------------------------------------------------------------------#
# Downloads and Uploads
# -----------------------------------------------------------------------------#

  output$download_stim <- downloadHandler(
    filename = function() {
      paste(input$participant, "_stimuli_", Sys.Date(), ".csv", sep="")
    },
    content = function(file) {
      write.csv(v$output$dat, file)
    }
  )
  
  output$download_input <- downloadHandler(
    filename = function() {
      paste(input$participant, "_app-input_", Sys.Date(), ".csv", sep="")
    },
    content = function(file) {
      write.csv(v$input_file, file)
    }
  )
  
  observeEvent(input$file1, {
    file <- input$file1
    ext <- tools::file_ext(file$datapath)
   
    req(file)
    v$dat_upload = read.csv(file$datapath)
    updateTextInput("participant",
                    value = paste0(unique(v$dat_upload$participant_id), collapse = ""),
                    session = session)
    
  })
  
  # what happens after the file is uploaded - recalculate stuff
  observeEvent(v$dat_upload,{
    req(v$dat_upload)
    w$show()
    v$output = score_upload(v$dat_upload)
  })
  
}

# Complete app with UI and server components
shinyApp(ui, server)
