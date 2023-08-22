pacman::p_load(shiny,
               tidyverse,
               RSQLite,
               DT,
               here,
               DBI)

fields <- c("Name",
            "Art",
            "Wasser_min_Sommer",
            "Wasser_max_Sommer",
            "Wasser_min_Winter",
            "Wasser_max_Winter",
            "Duengen_Sommer",
            "Duengen_Winter"
            )

sqlitePath <-  here("Ruhrstrasse", "plant_stuff", "plant_database")
table <- "pflanzen"

saveData <- function(data) {
  # Connect to the database
  db <- dbConnect(SQLite(), sqlitePath)
  # Construct the update query by looping over the data fields
  query <- sprintf(
    "INSERT INTO %s (%s) VALUES ('%s')",
    table, 
    paste(names(data), collapse = ", "),
    paste(data, collapse = "', '")
  )
  # Submit the update query and disconnect
  dbGetQuery(db, query)
  dbDisconnect(db)
}

loadData <- function() {
  # Connect to the database
  db <- dbConnect(SQLite(), sqlitePath)
  # Construct the fetching query
  query <- sprintf("SELECT * FROM %s", table)
  # Submit the fetch query and disconnect
  data <- dbGetQuery(db, query)
  dbDisconnect(db)
  data
}

ui <- fluidPage(

    titlePanel("Pflanzen-Eingabe"),
    
    DT::dataTableOutput("daten", width = 300),
    tags$hr(),
    textInput("Name", "Name", ""),
    textInput("Art", "Art", ""),
    numericInput("Wasser_min_Sommer", "Mininum an Tagen zwischen Gießen (Sommer)", -99),
    numericInput("Wasser_max_Sommer", "Maximum an Tagen zwischen Gießen (Sommer)", -99),
    numericInput("Wasser_min_Winter", "Mininum an Tagen zwischen Gießen (Winter)", -99),
    numericInput("Wasser_max_Winter", "Maximum an Tagen zwischen Gießen (Winter)", -99),
    numericInput("Duengen_Sommer", "Anzahl an Dünge-Wochen im Sommer (max. 26)", -99),
    numericInput("Duengen_Winter", "Anzahl an Dünge-Wochen im Winter (max. 26)", -99),
    actionButton("bestaetigen", "Bestätigen")
)

# Define server logic required to draw a histogram
server <- function(input, output, session) {
  
  # Whenever a field is filled, aggregate all form data
  formData <- reactive({
    data <- sapply(fields, function(x) input[[x]])
    data
  })
  
  # When the Submit button is clicked, save the form data
  observeEvent(input$bestaetigen, {
    saveData(formData())
    
    updateTextInput(session, "Name", value = "")
    updateTextInput(session, "Art", value =  "")
    updateNumericInput(session, "Wasser_min_Sommer", value = -99)
    updateNumericInput(session, "Wasser_max_Sommer",value = -99)
    updateNumericInput(session, "Wasser_min_Winter", value = -99)
    updateNumericInput(session, "Wasser_max_Winter", value = -99)
    updateNumericInput(session, "Duengen_Sommer", value = -99)
    updateNumericInput(session, "Duengen_Winter", value = -99)
  })
  
  # Show the previous responses
  # (update with current response when Submit is clicked)
  output$daten <- DT::renderDataTable({
    input$bestaetigen
    loadData()
  })     
}

# Run the application 
shinyApp(ui = ui, server = server)
