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
                    choices=list_regs, selected = "Bay"),
        #max = nrow(conflicts.long)-1,
        hr(),
        
        sliderInput("futureconflict", "Average number of Conflicts", min = 1, max = max(as.numeric(unlist(conflicts.long))),
                    value = 10, step = 100, round = 0),
        sliderInput("futureconflict", "Average number of Conflicts", min = 1, max = max(as.numeric(unlist(conflicts.long))),
                    value = 10, step = 100, round = 0),
        hr(),
        helpText("According to the selected Region the model is affected by the following parameters:"),
        sliderInput("var1", textOutput("SliderText"), min = 1, max = 1999,
                    value = 10, step = 100, round = 0),
        helpText("Data from Innovation Jetson Google Sheet"),
        tableOutput("datatable")
      ),
      
      # Create a spot for the barplot
      mainPanel(

        checkboxGroupInput("Future_Indicators", "",
                           c("Arrivals", 
                             "Future Arrivals", 
                             "Future Departures"),
                           selected=c(
                             "Arrivals", 
                             "Future Arrivals", 
                             "Future Departures"),
                           inline=TRUE),
        
        plotOutput("graph2"),
        sliderInput("futuremonths", "Months to Predict", max = as.Date(max(conflicts.long$Date)), min = (as.Date(max(conflicts.long$Date))-30*24),
                    value = c((as.Date(max(conflicts.long$Date))-30*24),as.Date(max(conflicts.long$Date))), timeFormat="%b %Y",width='100%'),
        checkboxGroupInput("Indicators", "",
                           c("Incidents", 
                             "Arrivals", 
                             "Departures"),
                           selected=c(
                             "Incidents", 
                             "Arrivals", 
                             "Departures"),
                           inline=TRUE),
        
        plotOutput("graph1"),
        sliderInput("months", "Months", min = (as.Date(min(conflicts.long$Date))+30*3),max =as.Date(max(conflicts.long$Date)),
                    value=c((as.Date(min(conflicts.long$Date))+30*3),as.Date(max(conflicts.long$Date))),timeFormat="%b %Y",width='100%')
      
        #plotOutput("IncidentPlot"),
        #plotOutput("ArrivalsPlot"),
        #plotOutput("DeparturesPlot")
      )
      
    )
  )
)
