library(shiny)
library(datasets)
library(magrittr)
library(XML)
library(reshape)
library(gsheet)
library(ggplot2)
library(scales)

# Use the google spreadsheet
jetson <- "https://docs.google.com/spreadsheets/d/1oPTPmoJ9phtMOkp-nMB7WHnPESomLzqUj9t0gcE9bYA"
conflicts <- gsheet2text(jetson, sheetid = 819472314)
conflicts.long <- read.csv(text=conflicts)

arrs <-gsheet2text(jetson, sheetid = 457614883)
arrs.long <- read.csv(text=arrs)
#arrs.long <- head(arrs.long, -30)

deps <-gsheet2text(jetson, sheetid = 677621454)
deps.long <-read.csv(text=deps)
#deps.long <- head(deps.long, -30)


rain <-gsheet2text(jetson, sheetid = 1473662223)
rain.long <- read.csv(text=rain,stringsAsFactors = FALSE)
#rain.long <- head(rain.long, -30)

Dates <- sapply(conflicts.long[,1],as.character.Date)
conflicts.long$Date <- as.Date(conflicts.long$Date, format="%m/%d/%Y")
arrs.long$Date <- as.Date(arrs.long$Date, format="%m/%d/%Y")
deps.long$Date <- as.Date(deps.long$Date, format="%m/%d/%Y")
rain.long$Date <-as.Date(rain.long$Date, format="%m.%d.%Y")

# Force columns to be text
conflicts.long[,2:ncol(conflicts.long)] <- sapply(conflicts.long[,2:ncol(conflicts.long)], as.numeric)
arrs.long[,2:ncol(arrs.long)] <- sapply(arrs.long[,2:ncol(arrs.long)], as.numeric)
deps.long[,2:ncol(deps.long)] <- sapply(deps.long[,2:ncol(deps.long)], as.numeric)
rain.long[,2:ncol(rain.long)] <- sapply(rain.long[,2:ncol(rain.long)], as.numeric)

monthStart <- function(x) {
  x <- as.POSIXlt(x)
  x$mday <- 1
  as.Date(x)
}

date_index <- function(x){
  full.date <- as.POSIXct(x, tz="GMT")
  index <- which(conflicts.long$Date== monthStart(full.date))
  return(index)
}



bay_arrivals <- function(start, end){
  #this section to make the predictions per Region
  for (t in start:end){
    
    #BAY PREDICTION FOR JULY
      #output$r0 <- "Jubbadaa_Hoose"
      #Bay_Arrival = 13.436169844673*Jubbada_Hoose_Conflict + 1.16638118095275*Awdal_Departures + 
      #0.861554563637453*Togdheer_Arrival + 
      #6.77121426152958e-7*Awdal_Arrival*Bari_Arrival*delay(Banadir_Departures, 4) - 
      #Gedo_rain - 1.80033045410828e-7*Bari_Arrival*Jubbada_Dhexe_Arrival*delay(Banadir_Departures, 4)
      
      PA[t] <- 0
      
      # detecting the vars in the dataset.
      JH_C <- conflicts.long[ (t),"Jubbada_Hoose_Conflict"]
      A_D <- deps.long[ (t),"Awdal_Departures"]
      T_A <- arrs.long[ (t),"Togdheer_Arrival"]
      A_A <- arrs.long[ (t),"Awdal_Arrival"]
      B_A <- arrs.long[ (t),"Bari_Arrival"]
      B_D <- deps.long[ (t-4), "Banadir_Departures"]
      G_R <- rain.long[ (t),"Gedo_rain"]
      JD_A <- arrs.long[ (t),"Jubbada_Dhexe_Arrival"]
      #reg
      JH_C_reg <- 13.436169844673
      A_D_reg <- 1.16638118095275
      T_A_reg <- 0.861554563637453
      exp_reg <- 6.77121426152958e-7
      B_A_reg <- 1.80033045410828e-7
      
      
      PA[t] <- sum(JH_C_reg *JH_C,A_D_reg* A_D ,T_A_reg*T_A,
                   exp_reg*A_A*B_A*B_D,-G_R,-B_A_reg*B_A*JD_A*B_D,na.rm = TRUE)
      #Bay_Incidents
      PI[t] <- 0
      #Bay_Departures
      PD[t] <- 0
      
    
  }
  
  
  return(PA)
  
}

bay_bestfit_arrivals <- function(region, futuremonths){
  #this section to make the predictions per Region
  for (t in futuremonths:1){
    
    #BAY PREDICTION FOR JULY
    #output$r0 <- "Jubbadaa_Hoose"
    #Bay_Arrival = 13.436169844673*Jubbada_Hoose_Conflict + 1.16638118095275*Awdal_Departures + 
    #0.861554563637453*Togdheer_Arrival + 
    #6.77121426152958e-7*Awdal_Arrival*Bari_Arrival*delay(Banadir_Departures, 4) - 
    #Gedo_rain - 1.80033045410828e-7*Bari_Arrival*Jubbada_Dhexe_Arrival*delay(Banadir_Departures, 4)
    
    PA[t] <- 0
    
    # detecting the vars in the dataset.
    JH_C <- conflicts.long[ (total_len-t),"Jubbada_Hoose_Conflict"]
    A_D <- deps.long[ (total_len-t),"Awdal_Departures"]
    T_A <- arrs.long[ (total_len-t),"Togdheer_Arrival"]
    A_A <- arrs.long[ (total_len-t),"Awdal_Arrival"]
    B_A <- arrs.long[ (total_len-t),"Bari_Arrival"]
    B_D <- deps.long[ (total_len-t-4), "Banadir_Departures"]
    G_R <- rain.long[ (total_len-t),"Gedo_rain"]
    JD_A <- arrs.long[ (total_len-t),"Jubbada_Dhexe_Arrival"]
    #reg
    JH_C_reg <- 13.436169844673
    A_D_reg <- 1.16638118095275
    T_A_reg <- 0.861554563637453
    exp_reg <- 6.77121426152958e-7
    B_A_reg <- 1.80033045410828e-7
    
    
    PA[t] <- sum(JH_C_reg *JH_C,A_D_reg* A_D ,T_A_reg*T_A,
                 exp_reg*A_A*B_A*B_D,-G_R,-B_A_reg*B_A*JD_A*B_D,na.rm = TRUE)
    #Bay_Incidents
    PI[t] <- 0
    #Bay_Departures
    PD[t] <- 0
    
    
  }
  
  PA <- rev(PA)
  return(PA)
  
}
# Define a server for the Shiny app
# the ids refer to the google sheet refering to the special identifier
shinyServer(function(input, output, session) {
  produce_slider <- function(x){
    if(x =="Bay"){
      sliderMonth <- reactiveValues()
      observe({
        sliderMonth$Month <- "Shabbeelle Hoose"
      })
      
      output$SliderText <- renderText({sliderMonth$Month})  
    }
    else{
      sliderMonth <- reactiveValues()
      observe({
        sliderMonth$Month <- "Awdal"
      })
      
      output$SliderText <- renderText({sliderMonth$Month})
    }
    return(TRUE)
  }
  
  mydata <- reactive({
    
    # prepare columns for the merged graph
    region <-input$region
    time_start <- date_index(input$months[1])
    time_end <- date_index(input$months[2])
    

    #testing values
    #total_len <- nrow(conflicts.long)
    #futuredays <- 30 
    #region <- "Bay_Conflict"
    reg_con <- paste(region,"Conflict",sep="_")
    reg_arr <- paste(region,"Arrival",sep="_")
    reg_dep <- paste(region,"Departures",sep="_")
    reg_rain <- paste(region,"rain",sep="_")
    I <- conflicts.long[ time_start:time_end,reg_con]
    A <- arrs.long[ time_start:time_end, reg_arr ]
    D <- deps.long[ time_start:time_end, reg_dep ]
    extend <- time_end-time_start +1
    
    long <- data.frame(
      Period=rep((1:extend),3),
      Date = conflicts.long$Date[time_start:time_end],
      Population = c(I, A, D), 
      Indicator=rep(c("Incidents", 
                      "Arrivals", 
                      "Departures"), 
                    each=(extend)))
    wide <- cbind(I[time_start:time_end], 
                  A[time_start:time_end], 
                  D[time_start:time_end])
    list(long=long, wide=wide)
    
    
  })
  
  pred_data <- reactive({
    
    #testing

    region <- input$region
    fmonths_start <- date_index(input$futuremonths[1])
    fmonths_end <- date_index(input$futuremonths[2])
    # prepare columns for the merged graph
    
    reg_con <- paste(region,"Conflict",sep="_")
    reg_arr <- paste(region,"Arrival",sep="_")
    reg_dep <- paste(region,"Departures",sep="_")
    reg_rain <- paste(region,"rain",sep="_")
    
    I <- conflicts.long[ fmonths_start:fmonths_end,reg_con]
    A <- arrs.long[ fmonths_start:fmonths_end, reg_arr ]
    D <- deps.long[ fmonths_start:fmonths_end, reg_dep ]
    
    #AA <- A[(total_len-30):total_len]
    R <- rain.long[ fmonths_start:fmonths_end, reg_rain]
    
    len <- fmonths_end - fmonths_start+1
    PI <- PA <- PD <- rep(NA, len)
    
    
    if(region == "Bay"){
        if(produce_slider(region)==TRUE){
          PA <- bay_arrivals(fmonths_start, fmonths_end)
          PI <- PA[fmonths_start:fmonths_end]  
        }  
      
        
    }
    else{
      produce_slider(region)
    }
    A<- A[1:len]
    long <- data.frame(
      Period=rep((1:len),3), 
      Date = conflicts.long$Date[fmonths_start:fmonths_end],
      Population = c(A, PI, PD), 
      Indicator=rep(c("Arrivals", 
                      "Future Arrivals", 
                      "Future Departures"), 
                    each=len))
    wide <- cbind(A, PI, PD)
    list(long=long, wide=wide)
    
    
  })
  
  #Create a datatable with all the values from the inputs
  output$datatable <- renderTable({
    Tdata <- cbind(pred_data()[["wide"]])
    Tdata <- cbind(day=1:nrow(Tdata), Tdata)
    Tdata[seq(1, nrow(Tdata), length.out=9),]
  })
  
  
  output$graph1 <- renderPlot({
    
    long <- mydata()[["long"]]
    p <- ggplot(long[long$Indicator %in% input$Indicators,], 
                aes(x=Date, y=Population, group=Indicator))    
    p <- p + 
      geom_line(aes(colour = Indicator), size=1, alpha=.75) + 
      ggtitle("All results for all months")+
      scale_x_date(name="Month", date_breaks = "6 month", date_minor_breaks = "1 month", date_labels = "%b %Y")+ 
      scale_y_continuous(labels = comma, name="People")
    print(p)
  })
  
  output$graph2 <- renderPlot({
    
    long <- pred_data()[["long"]]
    p <- ggplot(long[long$Indicator %in% input$Future_Indicators,], 
                aes(x=Date, y=Population, group=Indicator))    
    p <- p + 
      geom_line(aes(colour = Indicator), size=1, alpha=.75) + 
      ggtitle("Future Predictions")+
      scale_x_date(name="Month", date_breaks = "2 month", date_minor_breaks = "1 month", date_labels = "%b %Y")+ 
      scale_y_continuous(labels = comma, name="People")
    print(p)
  })
  
  }

)
