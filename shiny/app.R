library(shiny)

source("R/05-stimuli selection-take2.R")

# Define UI
ui <- fluidPage(
  
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
      downloadButton("downloadData", "Download")
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
  
  v <- reactiveValues(
    output = NA
  )
  
  observeEvent(input$submit,{
    v$output = select_stimuli(input$theta)
  })
  
  observeEvent(v$output,{
    req(!is.na(v$output))
    if(isTRUE(v$output$error)){
      showModal(modalDialog(
        title = "Error",
        div(
          p("There were too few discourse items or tasks eligible based on participant naming ability:"),
          p(paste("Discourse Tasks Eligible (at least 9):", v$output$error_detail[1])),
          p(paste("Discourse Items Eligible (at least 84):", v$output$error_detail[2])),
          p(paste("Lowest number of discourse eligible items in a condition (at least 28):", v$output$error_detail[4])),
          p(paste("Total Items eligible (at least 180):", v$output$error_detail[3]))
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
    v$output$dat
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
  
}

# Complete app with UI and server components
shinyApp(ui, server)
