library(shiny)

ui <- fluidPage(
  sidebarLayout(
    
    sidebarPanel(
      h1("Shiny - PCR"),
      hr(),
      fileInput(inputId = "file",
                label = h3("Input files"),
                multiple = TRUE),
      selectInput(inputId = "hsk",
                  label = "Housekeeping genes",
                  choices = LETTERS[1:5], #This will be change to a code that generate from the inputed files
                  multiple = T),
      textInput(inputId = "groupSep",
                label = "Group separator",
                placeholder = "A symbol like [ . , : ; - _ ]")
    ),
    mainPanel(
      h1("Main panel")
    )
    
  )
)

server <- function(input, output, session) {
  
}

shinyApp(ui, server)