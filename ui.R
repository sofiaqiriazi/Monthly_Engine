# Rely on the 'WorldPhones' dataset in the datasets
# package (which generally comes preloaded).
library(shiny)
library(datasets)
library(magrittr)
library(XML)
library(reshape)
library(gsheet)

jetson <- "https://docs.google.com/spreadsheets/d/1oPTPmoJ9phtMOkp-nMB7WHnPESomLzqUj9t0gcE9bYA"
conflicts <- gsheet2text(jetson, sheetid = 819472314)
conflicts.long <- read.csv(text=conflicts)

Dates <- sapply(conflicts.long[,1],as.character.Date)
conflicts.long$Date <- as.Date(conflicts.long$Date, format="%m/%d/%Y")

odd_indexes<-seq(2,19,1)
regions <- colnames(conflicts.long[odd_indexes])
list_regs <- rep(NA,18)
for (i in 1:18){
  list_regs[i] <- strsplit(regions[i],  "_(?=[^_]+$)",perl=TRUE)[[1]][1]
}

shinyUI(
  # Use a fluid Bootstrap layout
  fluidPage(  
    
    tags$head(
      tags$link(rel = "stylesheet", type = "text/css", href = "style.css"),
      tags$script(src="main.js")
    ),
    
    # Give the page a title
    titlePanel("Predictive Engine"),
    
    # Generate a row with a sidebar
    sidebarLayout(      
      # Define the sidebar with one input
      sidebarPanel(
        
        selectInput("region", "Region:", 
                    choices=list_regs)
      ),
      
      # Create a spot for the barplot
      mainPanel(
        tableOutput("datatable")
      )
      
    )
  )
)
