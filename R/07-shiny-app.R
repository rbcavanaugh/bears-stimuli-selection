library(shiny)

source(here("R", "05-stimuli selection-take2.R"))

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
                 plotOutput("p1")),
        tabPanel(title = "Summary (box plot)",
                 plotOutput("p2")),
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
  
  
  output$p1 <- renderPlot({
    req(!is.na(v$output))
    v$output$plot1
  })
  
  output$p2 <- renderPlot({
    req(!is.na(v$output))
    v$output$plot2
  })
  
  output$tab <- renderTable(({
    req(!is.na(v$output))
    v$output$dat
  }))
  
  
  output$timetab <- renderTable(({
    req(!is.na(v$output))
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
