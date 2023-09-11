library(shiny)
# library(waiter)
library(shinyjs)
library(DT)
library(bslib)

# this is automatically sourced for the shiny app since its in the R folder...
# it contains the logic for stimuli selection
# this file (mostly) just holds the logic for the app
# source(here::here("shiny", "R", "05-stimuli selection-working-version.R"))

# Define UI
ui <- page_sidebar(
  theme = bs_theme(version = 5, spacer = "0.9rem", 
                   bootswatch = "cosmo", primary = "#213AA6", base_font = font_google("Roboto"), 
                   heading_font = font_google("Roboto"), font_scale = 0.85, secondary = "#E3B317"),
  # making the error message a little more obvious
  tags$head(
    tags$style(HTML("
      .bslib-page-title{background-color: #213AA6}
      .shiny-output-error-validation {
        color: #000000;
        font-weight: bold;
        font-style: italic;
        margin: auto;
        padding: 30px;
        width: 50%;
      }
      #shiny-notification-panel {
              width: 500px;
      }
    "))
  ),
  # enables js for the loading spinny thingy
  # waiter::use_waiter(),
  # allows disabling/enabling buttons
  shinyjs::useShinyjs(),
  # Application title
  title = "BEARS stimuli selection",
  # layout of the page. sidebar + main page
  sidebar = sidebar(width = 400,
      tabsetPanel(
        tabPanel("Main Settings",
                 br(),
                 # Participant id widget
                 textInput(inputId = "participant", label = "Participant ID (required)"),
                 # Naming ability widget
                 numericInput(inputId = "theta",
                              label = "Naming Ability T-scale ~N(50, 10)",
                              # these are the potential values for the widget
                              value = 50, min = 30, max = 70, step = 0.1),
                 # When you hit submit, it runs the function in the stimuli selection script
                 radioButtons(inputId = "total_tx_items",
                              label = "Study (Total probe words)",
                              choices = c("Study 1 (180)" = 180,
                                          "Study 2 (500)" = 500), inline = TRUE
                 ),
                 numericInput(inputId = "min_words_per_discourse_item",
                              label = "Minimum number of qualifying words to consider using a discourse item.",
                              value = 4, min = 2, max = 6, step = 1),
                 p("Decrease to add more discourse words, increase to reduce discourse testing burden"), hr(),
                 actionButton("submit", "Select Stimuli"),
                 # Enables download of the stimuli file (not for the treatment app though)
                 downloadButton("download_stim", "Download Stimuli"),
                 hr(),
                 # Allows uploading a file with manual corrections as long as its in the 
                 fileInput("file1", "Upload existing stimuli file", accept = ".csv")
        ),
        tabPanel("Extra Settings",
                 br(),
                 numericInput(inputId = "min_naming_agreement",
                              label = "Min picture naming agreement allowed (%)",
                              value = 75, min = 50, max = 100, step = 1),
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
                              value = 42, min = 1, max = 100000, step = 1))
      )
    ),
    
    # This shows the results of the calculation in various formats
    # Also has a tab for allowing download of the treatment app input file
    div(
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
                 DTOutput("timetab"),
                 p("Table shows mean duration(minutes) and mean + 1SD duration from norming")),
        # Preview of the stimuli selected
        tabPanel(title = "Preview stimuli",
                 div(style = 'overflow-y: scroll; height: 70vh;', DT::DTOutput("tab"))),
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
                   shiny::column(width = 5, uiOutput("conditions")),
                   br(),
                   # buttons to generate the input file. note the latter is disabled to start
                   shiny::column(width = 3,
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
                 DTOutput("preview_input")),
        # this is the about page and holds the about page text. 
        tabPanel(title = "About",
                 div(
                   br(),
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

# Server logic
server <- function(input, output, session) {

# -----------------------------------------------------------------------------#
# Setup
# -----------------------------------------------------------------------------#
  # bs_themer()
  # initializes the spinny thingy
  # w <- Waiter$new(id = c("p1", "p2"),
  #                 html = spin_3(), 
  #                 color = transparent(.5))
  
  # holds all the data that can change throughout the app
  v <- reactiveValues(
    output = NA,
    input_file_ready = 0
  )
  
# -----------------------------------------------------------------------------#
# Runs stimuli selection command
# -----------------------------------------------------------------------------#

  #observe({print(as.numeric(input$total_tx_items))})
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
    
    # Create a Progress object
    progress <- shiny::Progress$new()
    progress$set(message = "Selecting stimuli...", value = 0)
    # Close the progress when this reactive exits (even if there's an error)
    on.exit(progress$close())
    
    # Create a callback function to update progress.
    # Each time this is called:
    # - If `value` is NULL, it will move the progress bar 1/5 of the remaining
    #   distance. If non-NULL, it will set the progress to that value.
    # - It also accepts optional detail text.
    updateProgress <- function(value = NULL, detail = NULL) {
      if (is.null(value)) {
        value <- progress$getValue()
        value <- value + (1 / 9)
      }
      progress$set(value = value, detail = detail)
    }
    
    tmp_error = 0
    tryCatch({
      v$output = select_stimuli(participant_theta      = input$theta,
                                min_naming_agreement   = input$min_naming_agreement,
                                min_discourse_salience = input$min_discourse_salience,
                                target_prob_correct    = input$target_prob_correct,
                                min_discourse_stimuli  = input$min_discourse_stimuli,
                                min_discourse_items    = input$min_discourse_items,
                                total_tx_items         = as.numeric(input$total_tx_items),
                                min_words_per_discourse_item = input$min_words_per_discourse_item,
                                seed                   = input$seed,
                                participant_id         = input$participant,
                                updateProgress         = updateProgress
      )
    }, error = function(e) {
      showNotification(paste(e, collapse = "\n"), type = "error")
      tmp_error = 1
      return()
    }, silent=TRUE)
    
    if(input$total_tx_items == "500" & tmp_error == 0){
     tmp =  
       (v$output$dat |> 
        count(condition) |> 
        filter(n==60) |> 
        droplevels() |> 
        pull(condition))[[1]]
     
    # print(tmp)
     
    updateSelectInput(session = session, inputId = paste0("g", tmp), selected = "ss")
    shinyjs::disable(id = paste0("g", tmp))
    
    # the remaining two conditions should be assigned randomly and enabled. 
    enable = sample(c(1, 2, 3)[-as.numeric(as.character(tmp))])
    
    updateSelectInput(session = session, inputId = paste0("g", enable[1]), selected = "sl")
    updateSelectInput(session = session, inputId = paste0("g", enable[2]), selected = "a")
    
    shinyjs::enable(id = paste0("g", enable[1]))
    shinyjs::enable(id = paste0("g", enable[2]))
    }
    
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
  output$conditions <- renderUI({
    div(
                  selectInput("g1",  label = "Group 1",
                              choices = study1_conditions),
                  selectInput("g2",  label = "Group 2",
                              choices = study1_conditions),
                  selectInput("g3",  label = "Group 3",
                              choices = study1_conditions)
                  )
  })
  outputOptions(output, "conditions", suspendWhenHidden = FALSE)
  
  
  study1_conditions = c("Effort Maximized" = "em",
                        "Accuracy Maximized" = "am",
                        "Balanced" = "eab")
  
  study2_conditions = c("Static (40)" = "ss",
                        "Static (200)" = "sl",
                        "Adaptive (200)" = "a")
  
  
  observeEvent(input$total_tx_items,{
    if(input$total_tx_items == "180"){
      updateSelectInput(session=session, inputId = "g1", label = "Group 1", choices = study1_conditions, selected = "em")
      updateSelectInput(session=session, inputId = "g2", label = "Group 2", choices = study1_conditions, selected = "am")
      updateSelectInput(session=session, inputId = "g3", label = "Group 3", choices = study1_conditions, selected = "eab")
      
    } else if (input$total_tx_items == "500"){
      updateSelectInput(session=session, inputId = "g1", label = "Group 1", choices = study2_conditions, selected = "ss")
      updateSelectInput(session=session, inputId = "g2", label = "Group 2", choices = study2_conditions, selected = "sl")
      updateSelectInput(session=session, inputId = "g3", label = "Group 3", choices = study2_conditions, selected = "a")
    }
  })
  
  
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
            c3 = input$g3,
            study = input$total_tx_items
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
  output$preview_input <- renderDT({
    validate(
      need(v$input_file_ready == 1,
           message = "Generate input file to preview")
    )
    datatable(v$input_file ,
              rownames = FALSE,
              filter = list(position = 'top', clear = FALSE),
              options = list(
                dom = "t",
                paging = FALSE,
                scrollY = TRUE,
                scrollX = TRUE,
                search = list(regex = TRUE, caseInsensitive = TRUE)
              )
    )# |> formatRound(digits=2, columns = 4:7)
    
  })
  
  # preview of the stimuli selected table. 
  output$tab <- DT::renderDT({
    req(!is.na(v$output))
    req(!isTRUE(v$output$error))
    datatable(v$output$dat |> select(word:filename),
              rownames = FALSE,
              filter = list(position = 'top', clear = FALSE),
              options = list(
                dom = "t",
                paging = FALSE,
                scrollY = TRUE,
                scrollX = TRUE,
                search = list(regex = TRUE, caseInsensitive = TRUE)
              )
              ) |> formatRound(digits=2, columns = 4:7)
  })
  
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
  output$timetab <- renderDT(({
    req(!is.na(v$output))
    req(!isTRUE(v$output$error))
    datatable(v$output$time)
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
