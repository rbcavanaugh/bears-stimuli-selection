library(shiny)
library(waiter)
library(shinyjs)
#source(here::here("shiny", "R", "05-stimuli selection-working-version.R"))
#source(here::here("R", "05-stimuli selection-working-version.R"))

# Define UI
ui <- fluidPage(
  waiter::use_waiter(),
  shinyjs::useShinyjs(),
  # Application title
  titlePanel("BEARS stimuli selection"),
  
  sidebarLayout(
    
    # Sidebar with a slider input
    sidebarPanel(
      textInput("participant", "Participant ID"),
      numericInput("theta",
                   label = "Naming Ability (-4 to 4)",
                   value = 0, min = -4, max = 4, step = 0.01),
      actionButton("submit", "Select Stimuli"),
      downloadButton("download_stim", "Download Stimuli"),
      hr(),
      numericInput("min_naming_agreement",
                   label = "Min naming agreement allowed (%)",
                   value = 70, min = 50, max = 100, step = 1),
      numericInput("min_discourse_salience",
                   label = "Min discourse salience allowed (%)",
                   value = 30, min = 20, max = 50, step = 1),
      numericInput("target_prob_correct",
                   label = "Target naming accuracy",
                   value = 0.33, min = 0, max = 0.66, step = 0.01),
      numericInput("min_discourse_stimuli",
                   label = "Min number of discourse stimuli (0 = naming only)",
                   value = 9, min = 0, max = 18, step = 1),
      numericInput("min_discourse_items",
                   label = "Min number of discourse items",
                   value = 54, min = 30, max = 90, step = 3),
      numericInput(inputId = "seed",
                   label = "Set seed for reproducibility",
                   value = 42, min = 1, max = 100000, step = 1),
      hr(),
      fileInput("file1", "Upload stimuli file to remake plots", accept = ".csv")
      
    ),
    
    # Show a plot of the generated distribution
    mainPanel(
      tabsetPanel(
        tabPanel(title = "About",
                 div(
                   h4("Getting Started"),
                   p("To get started using the app, enter a participant ID, and select a naming
                     ability that was generated from the PNT. The naming ability score must be between
                     -4 and 4. After entering these values, you can hit the 'Select Stimuli' button."),
                   p("The additional choices in the bottom of the sidebar menu are there in case we need
                   to make minor adjustments to the parameters that affect stimuli selection (for example, 
                   if we needed to relax the agreement criteria to pull from additional eligible items."),
                   p("In the hopefully rare case that someone is too mild and we can't generate enough
                     discourse items for them, you can set the 'Min number of discourse stimuli (0 = naming only)'
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
                 ),
        tabPanel(title = "Summary (mean + sd)",
                 plotOutput("p1", height = "80vh")),
        tabPanel(title = "Summary (box plot)",
                 plotOutput("p2", height = "80vh")),
        tabPanel(title = "Testing Time",
                 tableOutput("timetab"),
                 p("Table shows mean duration(minutes) and mean + 1SD duration from norming")),
        tabPanel(title = "Preview stimuli",
                 tableOutput("tab")),
        tabPanel(title = "Generate Input File",
                 shiny::fluidRow(
                   br(),
                   column(width = 10, align = "center",
                    tableOutput("stimuli_table")
                   )
                 ),
                 shiny::fluidRow(
                   shiny::column(width = 5, offset = 1,
                               selectInput("g1",  label = "Group 1", choices = c("Effort Maximized" = "em", "Accuracy Maximized" = "am", "Balanced" = "eab")),
                               selectInput("g2",  label = "Group 2", choices = c("Accuracy Maximized" = "am", "Balanced" = "eab", "Effort Maximized" = "em")),
                               selectInput("g3",  label = "Group 3", choices = c("Balanced" = "eab", "Effort Maximized" = "em", "Accuracy Maximized" = "am"))),
                   br(),
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
                 tableOutput("preview_input")),
      )
      
    )
  )
)

# Server logic
server <- function(input, output, session) {
  
  w <- Waiter$new(id = c("p1", "p2"),
                  html = spin_3(), 
                  color = transparent(.5))
  
  v <- reactiveValues(
    output = NA,
    input_file_ready = 0
  )
  
  observeEvent(input$submit,{
    if(input$participant == ""){
      showNotification("Warning: Participant ID should not be blank. Enter ID and re-run",
                       type = "error")
    }
    w$show()
    v$output = select_stimuli(participant_theta      = input$theta,
                              min_naming_agreement   = input$min_naming_agreement,
                              min_discourse_salience = input$min_discourse_salience,
                              target_prob_correct    =  input$target_prob_correct,
                              min_discourse_stimuli  = input$min_discourse_stimuli,
                              min_discourse_items    =  input$min_discourse_items,
                              seed                   = input$seed,
                              participant_id         = input$participant
                              )
    
  })
  
  output$error_table <- renderTable({
    req(!is.na(v$output$error_detail))
   v$output$error_detail
  })
  
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
  
  observeEvent(input$generate_input,{
    req(!is.na(v$output))
    naming_only = ifelse(input$min_discourse_stimuli == 0, 1, 0)
    if(length(unique(c(input$g1, input$g2, input$g3)))!=3){
          showNotification(
           "Each group needs to have a different condition",
          type = "error"
          )
    } else {
      
      v$input_file <- create_app_input_file(
            v$output$dat,
            naming_only = naming_only,
            c1 = input$g1,
            c2 = input$g2,
            c3 = input$g3
        )
      showNotification(
        "Input file ready to download",
        type = "message"
      )
      v$input_file_ready = 1
      shinyjs::enable("download_input")
    }
  })
  
  output$preview_input <- renderTable({
    validate(
      need(v$input_file_ready == 1,
           message = "Generate input file to preview")
    )
    v$input_file 
  }, 
  digits = 0)
  
  
  output$p1 <- renderPlot({
    req(!is.na(v$output))
    req(!isTRUE(v$output$error))
    v$output$plot1
  })
  
  output$p2 <- renderPlot({
    req(!is.na(v$output))
    req(!isTRUE(v$output$error))
    v$output$plot2
  })
  
  output$tab <- renderTable(({
    req(!is.na(v$output))
    req(!isTRUE(v$output$error))
    v$output$dat |> select(word:filename)
  }))
  
  
  output$timetab <- renderTable(({
    req(!is.na(v$output))
    req(!isTRUE(v$output$error))
    v$output$time
  }))
  
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
  
  observeEvent(v$dat_upload,{
    req(v$dat_upload)
    w$show()
    v$output = score_upload(v$dat_upload)
  })
  
}

# Complete app with UI and server components
shinyApp(ui, server)
