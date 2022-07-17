# Libraries ---------------------------------------------------------------

library(shiny)
library(dplyr)
library(tidyr)
library(stringr)
library(purrr)
library(readxl)
library(tibble)
library(shinyalert)
library(glue)


#
options(shiny.maxRequestSize=30*1024^2)

#Geometric mean function
geometric.mean <- function(x,na.rm=TRUE){ 
  exp(mean(log(x),na.rm=na.rm))
}



ui <- fluidPage(
  sidebarLayout(
    
    sidebarPanel(
      h1("Shiny - PCR"),
      hr(),
      fileInput(inputId = "file",
                label = h3("Input files"),
                multiple = TRUE,
                accept = ".xls"),
      uiOutput("hsk_select"),
      textInput(inputId = "groupSep",
                label = "Group separator",
                placeholder = "A symbol like [ . , : ; - _ ]")
    ),
    mainPanel(
      h1("Main panel"),
      tableOutput(outputId = "dataTable")
    )
    
  )
)

server <- function(input, output, session) {
  
  data <- reactive({
    
    if(is.null(input$file))
      return()
    else 
    {
      nfiles = nrow(input$file) 
      xls = list()
      for (i in 1 : nfiles)
      {
        
        xls[[i]] = read_xls(input$file[[i, 'datapath']],
                            skip = 35,
                            sheet = "Results")
        
      }
      
      names(xls) <- paste("Plate",1:(nfiles),sep = "_")
      
      l2 <- xls %>% 
        map(.f = ~filter(., "Taks" != "NTC")) %>% 
        map(.f = ~filter(., !is.na("Sample Name"))) %>% 
        map(.f = ~filter(., "CT" != "Undetermined")) %>%
        map(.f = ~select(.,"Sample Name","Target Name","Ct Mean")) %>% 
        map(~rename(., "CT" = "Ct Mean","Sample" = "Sample Name", "Gene" = "Target Name"))
      
      Qnt_Tidy <- tibble(Data = l2, Plate = names(l2)) %>% 
        unnest(Data) %>% 
        filter(!is.na(CT))
      
      return(Qnt_Tidy)
    }
  })
  
  #Select Housekeeping genes
  output$hsk_select <- renderUI({
    
    selectInput(inputId = "hsk",
                label = "Housekeeping genes",
                choices = unique(data()$Gene), #This will be change to a code that generate from the inputed files
                multiple = T)
  })
  
  #Shiny alert of the group names
  observeEvent(input$groupSep, {
    
    if(is.null(input$file)) {
      return(NULL)
    }
    
    else{
      # Show a modal when the files are inputed
      sample_names<- data() %>% 
        select(Sample) %>% 
        separate(col = Sample,into = c("Groups","Rep"),sep = input$groupSep) %>% 
        select(Groups) %>% 
        distinct() %>% 
        pull(Groups)
      
      shinyalert("Your group names are:",
                 paste(sample_names,
                       collapse = ", "), type = "info")
    }
    
  })
  
  #Test if input works
  output$dataTable <- renderTable({
    
    data()
  })
  
}

shinyApp(ui, server)