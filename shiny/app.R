library(shiny)
library(waiter)
#source(here::here("shiny", "R", "05-stimuli selection-working-version.R"))
#source(here::here("R", "05-stimuli selection-working-version.R"))

# Define UI
ui <- fluidPage(
  waiter::use_waiter(),
  # Application title
  titlePanel("BEARS stimuli selection"),
  
  sidebarLayout(
    
    # Sidebar with a slider input
    sidebarPanel(
      textInput("participant", "Participant ID"),
      numericInput("theta",
                   label = "Naming Ability (-4 to 4)",
                   value = 0, min = -4, max = 4, step = 0.01),
      actionButton("submit", "Run Algorithm"),
      downloadButton("downloadData", "Download"),
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
                   label = "Min number of discourse stimuli",
                   value = 9, min = 3, max = 18, step = 1),
      numericInput("min_discourse_items",
                   label = "Min number of discourse items",
                   value = 54, min = 30, max = 90, step = 3),
      numericInput(inputId = "seed",
                   label = "Set seed for reproducibility",
                   value = 42, min = 1, max = 100000, step = 1),
      hr(),
      fileInput("file1", "Upload File to remake plots", accept = ".csv")
      
    ),
    
    # Show a plot of the generated distribution
    mainPanel(
      tabsetPanel(
        tabPanel(title = "Summary (mean + sd)",
                 plotOutput("p1", height = "80vh")),
        tabPanel(title = "Summary (box plot)",
                 plotOutput("p2", height = "80vh")),
        tabPanel(title = "Testing Time",
                 tableOutput("timetab"),
                 p("Table shows mean duration(minutes) and mean + 1SD duration from norming")),
        tabPanel(title = "stimuli",
                 tableOutput("tab"))
      )
      
    )
  )
)

# Server logic
server <- function(input, output) {
  
  w <- Waiter$new(id = c("p1", "p2"),
                  html = spin_3(), 
                  color = transparent(.5))
  
  v <- reactiveValues(
    output = NA
  )
  
  observeEvent(input$submit,{
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
  
  output$downloadData <- downloadHandler(
    filename = function() {
      paste(input$participant, "_stimuli_", Sys.Date(), ".csv", sep="")
    },
    content = function(file) {
      write.csv(v$output$dat, file)
    }
  )
  
  observeEvent(input$file1, {
    file <- input$file1
    ext <- tools::file_ext(file$datapath)
   
    req(file)
    v$dat_upload = read.csv(file$datapath)
    
  })
  
  observeEvent(v$dat_upload,{
    req(v$dat_upload)
    v$output = score_upload(v$dat_upload)
  })
  
}

# Complete app with UI and server components
shinyApp(ui, server)
