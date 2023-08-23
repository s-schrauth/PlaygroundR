pacman::p_load(shiny,
               tidyverse,
               RSQLite,
               DT,
               here,
               DBI)

# load hardcoded objects
source(here("Ruhrstrasse", "plant_stuff", "hardcoding.R"))

# load functions
source(here("Ruhrstrasse", "plant_stuff", "functions.R"))


ui <- fluidPage(

    titlePanel("Datenbank-Management"),
    
    
    
    #conditionalPanel(condition = "input.XXX =='YYY'")
    
    
    
    
    sidebarLayout(
      sidebarPanel(
        helpText(h3("Pflanzen-Eingabe")),
        textInput("Name", "Name", ""),
        textInput("Art", "Art", ""),
        numericInput("Wasser_min_Sommer", "Mininum an Tagen zwischen Gießen (Sommer)", -99),
        numericInput("Wasser_max_Sommer", "Maximum an Tagen zwischen Gießen (Sommer)", -99),
        numericInput("Wasser_min_Winter", "Mininum an Tagen zwischen Gießen (Winter)", -99),
        numericInput("Wasser_max_Winter", "Maximum an Tagen zwischen Gießen (Winter)", -99),
        numericInput("Duengen_Sommer", "Anzahl an Dünge-Wochen im Sommer (max. 26)", -99),
        numericInput("Duengen_Winter", "Anzahl an Dünge-Wochen im Winter (max. 26)", -99),
        actionButton("bestaetigen", "Ok"),
        width = 3
      ),
      
      mainPanel(
        checkboxGroupInput("sowi", 
                           label = h4("Jahreszeiten"),
                           choices = list("Sommer" = "so",
                                          "Winter" = "wi"),
                           selected = "so"),
        DT::dataTableOutput("daten"),
        width = 9
      ),
      position = "right"
    )
)

# Define server logic required to draw a histogram
server <- function(input, output, session) {
  
  # Whenever a field is filled, aggregate all form data
  formData <- reactive({
    data <- sapply(labels$codename, function(x) input[[x]])
    data
  })
  
  # When the Submit button is clicked, save the form data
  observeEvent(input$bestaetigen, {
    saveData(formData(), table = "pflanzen")

    
    lapply(labels$codename[c(1,2)], 
           updateTextInput, 
           session = session,
           value = "")
    
    lapply(labels$codename[-c(1,2)], 
           updateNumericInput, 
           session = session,
           value = -99)
  })
  
  # Show the previous responses
  # (update with current response when Submit is clicked)
  output$daten <- DT::renderDataTable({
    input$bestaetigen
    loadData(table = "pflanzen", 
             labels = labels, 
             SoWi = input$sowi)
  })     
}

# Run the application 
shinyApp(ui = ui, server = server)
