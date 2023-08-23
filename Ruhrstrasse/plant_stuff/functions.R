# Functions for Shiny App
library(tidyverse)

## Path to SQL database
sqlitePath <-  here("Ruhrstrasse", "plant_stuff", "plant_database")

## Function for saving new entries
saveData <- function(data, table) {
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

## Function for loading existing entries
loadData <- function(table, labels, SoWi) {
  # Connect to the database
  db <- dbConnect(SQLite(), sqlitePath)
  # Construct the fetching query
  query <- sprintf("SELECT * FROM %s", table)
  # Submit the fetch query and disconnect
  data <- dbGetQuery(db, query)
  dbDisconnect(db)
  
  cols <- labels %>% 
    filter(sowi %in% c("sowi", SoWi)) %>% 
    select(codename) %>%  
    unlist(use.names = FALSE)
  
  col_names <- labels %>% 
    filter(codename %in% cols) %>% 
    select(labels) %>% 
    unlist(use.names = FALSE)
  
  data_show <- data %>% 
    select(-c(ID_Pflanzen)) %>% 
    select(all_of(cols))
    
  DT::datatable(data_show,
                class = "hover",
                rownames = FALSE,
                colnames = col_names,
                options = list(dom = 't'))
}

## Function for editing existing entries

editData <- function(table, id){
  # Connect to the database
  db <- dbConnect(SQLite(), sqlitePath)
  # Construct the fetching query
  query <- sprintf("SELECT * FROM %s WHERE ID_Pflanzen = %i", table, id)
  # Submit the fetch query and disconnect
  data <- dbGetQuery(db, query)
  dbDisconnect(db)
  data
  
}