library(shiny)
library(datasets)
library(magrittr)
library(XML)
library(reshape)
library(gsheet)
library(ggplot2)
library(scales)
library(zoo)

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

#read the WaterDrumPrices
water <-gsheet2text(jetson, sheetid =27261871)
water.long <- read.csv(text=water,stringsAsFactors = FALSE)

#read the Rivers
rivers <-gsheet2text(jetson, sheetid =407366559)
rivers.long <- read.csv(text=rivers,stringsAsFactors = FALSE)

#read the Goat Prices
goats <-gsheet2text(jetson, sheetid =1601716765)
goats.long <- read.csv(text=goats,stringsAsFactors = FALSE)

#read the Fatalities
fatalities <-gsheet2text(jetson, sheetid =343810263)
fatalities.long <- read.csv(text=fatalities,stringsAsFactors = FALSE)

discharge <- gsheet2text(jetson, sheetid=407366559)
discharge <- read.csv(text=discharge,stringsAsFactors = FALSE)

Dates <- sapply(conflicts.long[,1],as.character.Date)
conflicts.long$Date <- as.Date(conflicts.long$Date, format="%m/%d/%Y")
arrs.long$Date <- as.Date(arrs.long$Date, format="%m/%d/%Y")
deps.long$Date <- as.Date(deps.long$Date, format="%m/%d/%Y")
rain.long$Date <-as.Date(rain.long$Date, format="%m/%d/%Y")
water.long$Date <-as.Date(water.long$Date, format="%m/%d/%Y")
rivers.long$Date <-as.Date(rivers.long$Date, format="%m/%d/%Y")
goats.long$Date <-as.Date(goats.long$Date, format="%m/%d/%Y")
fatalities.long$Date <-as.Date(fatalities.long$Date, format="%m/%d/%Y")

# Force columns to be text
conflicts.long[,2:ncol(conflicts.long)] <- sapply(conflicts.long[,2:ncol(conflicts.long)], as.numeric)
arrs.long[,2:ncol(arrs.long)] <- sapply(arrs.long[,2:ncol(arrs.long)], as.numeric)
deps.long[,2:ncol(deps.long)] <- sapply(deps.long[,2:ncol(deps.long)], as.numeric)
rain.long[,2:ncol(rain.long)] <- sapply(rain.long[,2:ncol(rain.long)], as.numeric)
water.long[,2:ncol(water.long)] <- sapply(water.long[,2:ncol(water.long)], as.numeric,na.rm=TRUE)
rivers.long[,2:ncol(rivers.long)] <- sapply(rivers.long[,2:ncol(rivers.long)], as.numeric)
goats.long[,2:ncol(goats.long)] <- sapply(goats.long[,2:ncol(goats.long)], as.numeric)
fatalities.long[,2:ncol(fatalities.long)] <- sapply(fatalities.long[,2:ncol(fatalities.long)], as.numeric)

# hc<-seq(as.Date(max(conflicts.long[,"Date"])), as.Date("2019-01-6"), by="months")
# ha<-seq(as.Date(max(arrs.long[,"Date"])), as.Date("2019-01-6"), by="months")
# hd<-seq(as.Date(max(deps.long[,"Date"])), as.Date("2019-01-6"), by="months")
# hr<-seq(as.Date(max(rain.long[,"Date"])), as.Date("2019-01-6"), by="months")
# 
# fill_start <- nrow(conflicts.long)
# fill_end <- nrow(conflicts.long) + length(hc)
# nrowconflicts <-nrow(conflicts.long)
# 
# for (i in 1:length(hc)){
#   conflicts.long[nrowconflicts+i,"Date"] <- hc[i]
#   arrs.long[nrowconflicts+i,"Date"] <- ha[i]
#   deps.long[nrowconflicts+i,"Date"] <- hd[i]
#   rain.long[nrowconflicts+i,"Date"] <- hr[i]
# }


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

bay_6A_arrivals <- function(start, end){
  start = 6
  end = 92
  len = 92
  PI <- PA <- PD <- rep(NA, len)
  
  for (t in start:end){
    #Bay_Arrival = Shabeellaha_Dhexe_rain +
    SD_R <- rain.long[t,"Shabeellaha_Dhexe_rain"]

    #delay(Woqooyi_Galbeed_Arrival, 1) + 
    WG_A <- arrs.long[(t-1),"Woqooyi_Galbeed_Arrival"]
    #wma(Togdheer_Arrival, 6) + 
    T_A <- weighted.mean(arrs.long[ (t-6):t, "Togdheer_Arrival"])
    #max(
    #2.84291053988138*Bakool_Arrival + 
    Bak_A <- 2.84291053988138*arrs.long[t,"Bakool_Arrival"]
    #1.90605269499887*Bari_Arrival +
    Bari_A <-  1.90605269499887* arrs.long[t,"Bari_Arrival"]
    #0.582938861571093*Bay_Departures + 
    Bay_D <- 0.582938861571093*deps.long[t,"Bay_Departures"]
    #delay(Woqooyi_Galbeed_Arrival, 1) -
    WG_A2 <- arrs.long[(t-1),"Woqooyi_Galbeed_Arrival"]
    #Shabeellaha_Dhexe_rain*sma(Jubbada_Hoose_Departures, 2) +
    SH_R_JH_D <- -1*rain.long[t,"Shabeellaha_Dhexe_rain"]*mean(deps.long[(t-2):t,"Jubbada_Hoose_Departures"])
    #- 0.0801078676959042*Hiiraan_WaterDrumPrice +
    H_WDP <- -0.0801078676959042*as.numeric(water.long[t, "Hiiraan_WaterDrumPrice"])
    #- 6.62472178142272*Awdal_Departures, 
    A_D <- -6.62472178142272*deps.long[t,"Awdal_Departures"]
    #0.263886473589876*Bay_Departures)
    Bay_D2 <- 0.263886473589876*deps.long[t,"Bay_Departures"]

#Bay_Arrival = Shabeellaha_Dhexe_rain + delay(Woqooyi_Galbeed_Arrival, 1) + wma(Togdheer_Arrival, 6) 
#+ max(2.84291053988138*Bakool_Arrival + 1.90605269499887*Bari_Arrival + 0.582938861571093*Bay_Departures 
#+ delay(Woqooyi_Galbeed_Arrival, 1) - (Shabeellaha_Dhexe_rain*sma(Jubbada_Hoose_Departures, 2) )
#- 0.0801078676959042*Hiiraan_WaterDrumPrice - 6.62472178142272*Awdal_Departures, 0.263886473589876*Bay_Departures)


    #breaking the function to section max
    PA[t] <- 0
    
    section_max <- max(sum(Bak_A,Bari_A,Bay_D,WG_A2,SH_R_JH_D,H_WDP,A_D,na.rm=TRUE),Bay_D2)

    section_add <- sum(SD_R, WG_A, T_A, section_max, na.rm=TRUE)
    
    PA[t] <- section_add

    
    #Bay_Incidents
    PI[t] <- 0
    #Bay_Departures
    PD[t] <- 0
      
  }
  return(PA)
}

bay_6B_arrivals <- function(start, end){
  #Bay_Arrival = Shabeellaha_Dhexe_rain + 
  #delay(Woqooyi_Galbeed_Arrival, 1) + 
  #wma(Togdheer_Arrival, 6) + 
  #max(2.85396032063776*Bakool_Arrival + 
  #2.00585641198899*Bari_Arrival + 
  #0.584222032988209*Bay_Departures - 
  #Shabeellaha_Dhexe_rain*sma(Jubbada_Hoose_Departures, 2) - 
  #0.0794226038273046*Hiiraan_WaterDrumPrice - 
  #6.45129861220252*Awdal_Departures, 0.273181515405487*Bay_Departures)
  start = 6
  end = 92
  len = 92
  PI <- PA <- PD <- rep(NA, len)
  
  for (t in start:end){
    #Bay_Arrival = Shabeellaha_Dhexe_rain +
    SD_R <- rain.long[t,"Shabeellaha_Dhexe_rain"]
    
    #delay(Woqooyi_Galbeed_Arrival, 1) + 
    WG_A <- arrs.long[(t-1),"Woqooyi_Galbeed_Arrival"]
    #wma(Togdheer_Arrival, 6) + 
    T_A <- weighted.mean(arrs.long[ (t-6):t, "Togdheer_Arrival"])
    #max(
    #2.85396032063776*Bakool_Arrival + 
    Bak_A <- 2.85396032063776*arrs.long[t,"Bakool_Arrival"]
    #2.00585641198899*Bari_Arrival +
    Bari_A <-  2.00585641198899* arrs.long[t,"Bari_Arrival"]
    #0.584222032988209*Bay_Departures + 
    Bay_D <- 0.584222032988209*deps.long[t,"Bay_Departures"]
    #Shabeellaha_Dhexe_rain*sma(Jubbada_Hoose_Departures, 2) +
    SH_R_JH_D <- -1*rain.long[t,"Shabeellaha_Dhexe_rain"]*mean(deps.long[(t-2):t,"Jubbada_Hoose_Departures"])
    #-0.0794226038273046*Hiiraan_WaterDrumPrice
    H_WDP <- -0.0794226038273046*as.numeric(water.long[t, "Hiiraan_WaterDrumPrice"])
    #-6.45129861220252*Awdal_Departures, 
    A_D <- -6.62472178142272*deps.long[t,"Awdal_Departures"]
    #0.2731815154054876*Bay_Departures)
    Bay_D2 <- 0.263886473589876*deps.long[t,"Bay_Departures"]
    
    #Bay_Arrival = Shabeellaha_Dhexe_rain + delay(Woqooyi_Galbeed_Arrival, 1) + wma(Togdheer_Arrival, 6) 
    #+ max(2.84291053988138*Bakool_Arrival + 1.90605269499887*Bari_Arrival + 0.582938861571093*Bay_Departures 
    #+ delay(Woqooyi_Galbeed_Arrival, 1) - (Shabeellaha_Dhexe_rain*sma(Jubbada_Hoose_Departures, 2) )
    #- 0.0801078676959042*Hiiraan_WaterDrumPrice - 6.62472178142272*Awdal_Departures, 0.263886473589876*Bay_Departures)
    
    
    #breaking the function to section max
    PA[t] <- 0
    
    section_max <- max(sum(Bak_A,Bari_A,Bay_D,SH_R_JH_D,H_WDP,A_D,na.rm=TRUE),Bay_D2)
    
    section_add <- sum(SD_R, WG_A, T_A, section_max, na.rm=TRUE)
    
    PA[t] <- section_add
    
    
    #Bay_Incidents
    PI[t] <- 0
    #Bay_Departures
    PD[t] <- 0
    
  }
  return(PA)
}


bay_6C_arrivals <- function(start, end){

	#Bay_Arrival = Shabelle_River_discharge + 
	#wma(Togdheer_Arrival, 8) + 

	#max(
	#2.81910559622002*Bakool_Arrival + 
	#2.48532945247097*Bari_Arrival + 
	#0.595514845826536*Bay_Departures +
	#-1*Shabeellaha_Dhexe_rain*sma(Jubbada_Hoose_Departures, 2) +
	#-1*0.0833170517942912*Hiiraan_WaterDrumPrice +
	# -5.84944477753492*Awdal_Departures, 
	# 0.312135550188447*Bay_Departures)
  start = 8
  end = 92
  len = 92
  PI <- PA <- PD <- rep(NA, len)
  
  for (t in start:end){



  	S_RD <- rivers.long[t,"Shabelle_River_discharge"]

    #wma(Togdheer_Arrival, 8) + 
    T_A <- weighted.mean(arrs.long[ (t-8):t, "Togdheer_Arrival"])
    #max(
    #2.85396032063776*Bakool_Arrival + 
    Bak_A <- 2.81910559622002*arrs.long[t,"Bakool_Arrival"]
    #2.00585641198899*Bari_Arrival +
    Bari_A <- 2.48532945247097*arrs.long[t,"Bari_Arrival"]
    #0.584222032988209*Bay_Departures + 
    Bay_D <- 0.595514845826536*deps.long[t,"Bay_Departures"]
    #Shabeellaha_Dhexe_rain*sma(Jubbada_Hoose_Departures, 2) +
    SH_R_JH_D <- -1*rain.long[t,"Shabeellaha_Dhexe_rain"]*mean(deps.long[(t-2):t,"Jubbada_Hoose_Departures"])
    #-0.0794226038273046*Hiiraan_WaterDrumPrice
    H_WDP <- -0.0833170517942912*as.numeric(water.long[t, "Hiiraan_WaterDrumPrice"])
    #-6.45129861220252*Awdal_Departures, 
    A_D <- -5.84944477753492*deps.long[t,"Awdal_Departures"]
    #0.2731815154054876*Bay_Departures)
    Bay_D2 <- 0.312135550188447*deps.long[t,"Bay_Departures"]
    
    #Bay_Arrival = Shabeellaha_Dhexe_rain + delay(Woqooyi_Galbeed_Arrival, 1) + wma(Togdheer_Arrival, 6) 
    #+ max(2.84291053988138*Bakool_Arrival + 1.90605269499887*Bari_Arrival + 0.582938861571093*Bay_Departures 
    #+ delay(Woqooyi_Galbeed_Arrival, 1) - (Shabeellaha_Dhexe_rain*sma(Jubbada_Hoose_Departures, 2) )
    #- 0.0801078676959042*Hiiraan_WaterDrumPrice - 6.62472178142272*Awdal_Departures, 0.263886473589876*Bay_Departures)
    
    
    #breaking the function to section max
    PA[t] <- 0
    
    section_max <- max(sum(Bak_A,Bari_A,Bay_D,SH_R_JH_D,H_WDP,A_D,na.rm=TRUE),Bay_D2)
    
    section_add <- sum(S_RD, T_A, section_max, na.rm=TRUE)
    
    PA[t] <- section_add
    
    
    #Bay_Incidents
    PI[t] <- 0
    #Bay_Departures
    PD[t] <- 0
    
  }
  return(PA)
}

bay_7A_arrivals <- function(start,end){
#Bay_Arrival = 4.06749222225571*delay(Bari_Departures, 4) + 
#3.84843445400171*Bari_Arrival + 
#2.88659255100737*delay(Sanaag_Departures, 1) + 
#0.0032902302975162*Bakool_Arrival*Woqooyi_Galbeed_Arrival + 
#1.92099158604754e-8*Bay_Departures*Bakool_Arrival^2 + 
#exp(2.88659255100737*Awdal_Fatalities) - 
#0.00255197115076363*Woqooyi_Galbeed_Arrival*delay(Bari_Departures, 3)
start = 8
end = 92
len = 92

PI <- PA <- PD <- rep(NA, len)
  
  for (t in start:end){



  	Bay_D <- 4.06749222225571*deps.long[(t-4),"Bari_Departures"]

    Bari_A <- 3.84843445400171*arrs.long[t,"Bari_Arrival"]

  	S_D <- 2.88659255100737*deps.long[(t-1),"Sanaag_Departures"]

  	Bak_A_WG_A <- 0.0032902302975162*arrs.long[t,"Bakool_Arrival"]*arrs.long[t,"Woqooyi_Galbeed_Arrival"]

  	Bay_D_Bak_A <- 1.92099158604754e-8*deps.long[t,"Bay_Departures"]*(arrs.long[t,"Bakool_Arrival"]^2)

  	A_F <- exp(2.88659255100737*fatalities.long[t,"Awdal_Fatalities"])
    
  	WG_A_Bari_D <- -0.00255197115076363*arrs.long[t,"Woqooyi_Galbeed_Arrival"]*deps.long[(t-3),"Bari_Departures"]

  	PA[t] <- 0
       
    section_add <- sum(Bay_D,Bari_A,S_D,Bak_A_WG_A, Bay_D_Bak_A,A_F,WG_A_Bari_D, na.rm=TRUE)
    
    PA[t] <- section_add
    
    
    #Bay_Incidents
    PI[t] <- 0
    #Bay_Departures
    PD[t] <- 0
    
  }
  return(PA)
}


bay_7B_arrivals <- function(start,end){

#Bay_Arrival = 
#4.64702773857703*Bari_Arrival 
#+ 3.9551600313037*delay(Bari_Departures, 4) 
#+ 3.00585559868906*delay(Sanaag_Departures, 1) 
#+ 0.0264941670042537*Bay_Fatalities*Bay_Departures 
#+ 0.0032023952665391*Bakool_Arrival*Woqooyi_Galbeed_Arrival 
#- 526.62694153389 
#- 0.0364748473553382*Bay_Fatalities*delay(Bari_Departures, 3)
start = 8
end = 92
len = 92

PI <- PA <- PD <- rep(NA, len)
  
  for (t in start:end){


	#4.64702773857703*Bari_Arrival 
	Bari_A <- 4.64702773857703*arrs.long[t,"Bari_Arrival"]

	#+ 3.9551600313037*delay(Bari_Departures, 4)
  	Bay_D <- 3.9551600313037*deps.long[(t-4),"Bari_Departures"]

	#+ 3.00585559868906*delay(Sanaag_Departures, 1) 
  	S_D <- 3.00585559868906*deps.long[(t-1),"Sanaag_Departures"]


	#+ 0.0264941670042537*Bay_Fatalities*Bay_Departures 
  	Bak_A_WG_A <- 0.0264941670042537*fatalities.long[t,"Bay_Fatalities"]*deps.long[t,"Bay_Departures"]

	#+ 0.0032023952665391*Bakool_Arrival*Woqooyi_Galbeed_Arrival 
  	Bay_D_Bak_A <- 0.0032023952665391*arrs.long[t,"Bakool_Arrival"]*(arrs.long[t,"Woqooyi_Galbeed_Arrival"])

  	#- 526.62694153389 
	num <- - 526.62694153389 
  	
	#- 0.0364748473553382*Bay_Fatalities*delay(Bari_Departures, 3)

	Bay_F_Bari_D <- -0.0364748473553382*fatalities.long[t,"Bay_Fatalities"]*deps.long[(t-3),"Bari_Departures"]
  	

    section_add <- sum(Bari_A,Bay_D,S_D, Bak_A_WG_A, Bay_D_Bak_A,num, Bay_F_Bari_D, na.rm=TRUE)
    
    PA[t] <- section_add
    
    
    #Bay_Incidents
    PI[t] <- 0
    #Bay_Departures
    PD[t] <- 0
    
  }
  return(PA)
}


bay_7C_arrivals <- function(start,end){

#Bay_Arrival = 
#Nugaal_rain + 
#0.708094855345896*Woqooyi_Galbeed_Conflict*Bakool_Arrival 
#+ 7.67429595004198e-5*Gedo_Arrival*Shabeellaha_Hoose_Arrival 
#- 0.329795608516243*Jubbada_Dhexe_Conflict*Bakool_Arrival
start = 8
end = 92
len = 92

PI <- PA <- PD <- rep(NA, len)
  
  for (t in start:end){

  	N_R <- rain.long[t,"Nugaal_rain"]
	#0.708094855345896*Woqooyi_Galbeed_Conflict*Bakool_Arrival 
	WG_C<-0.708094855345896*conflicts.long[t,"Woqooyi_Galbeed_Conflict"]*arrs.long[t,"Bakool_Arrival"] 

	#+ 7.67429595004198e-5*Gedo_Arrival*Shabeellaha_Hoose_Arrival 
	G_A_SH_A <- 7.67429595004198e-5*arrs.long[t,"Gedo_Arrival"]*arrs.long[t,"Shabeellaha_Hoose_Arrival"]

	#-0.329795608516243*Jubbada_Dhexe_Conflict*Bakool_Arrival
	JD_C_B_A <- -0.329795608516243*conflicts.long[t,"Jubbada_Dhexe_Conflict"]*arrs.long[t,"Bakool_Arrival"]

    section_add <- sum(N_R, WG_C, G_A_SH_A, JD_C_B_A, na.rm=TRUE)
    
    PA[t] <- section_add
    
    
    #Bay_Incidents
    PI[t] <- 0
    #Bay_Departures
    PD[t] <- 0
    
  }
  return(PA)
}

bay_8A_arrivals <- function(start,end){

start = 15

end=92
len=92

PI <- PA <- PD <- rep(NA, len)

for(t in start:end){
  
A <- rivers.long[(t-4),"Juba_River_discharge"]
B <- deps.long[(t-2),"Bari_Departures"]
C <- median(deps.long[(t-9):t,"Bay_Departures"])
D <- arrs.long[t,"Sool_Arrival"] 
E <- arrs.long[(t-15),"Bari_Arrival"]
Fa <- arrs.long[t,"Gedo_Arrival"] 
G <- 4.69615656952437*arrs.long[t,"Bakool_Arrival"]
H <- 2.32455930952855*mean(arrs.long[(t-15):t,"Hiiraan_Arrival"] )
I <- -1*water.long[t,"Hiiraan_WaterDrumPrice"]
J <- -1*mean(deps.long[(t-2):t,"Gedo_Departures"])
K <- -7.50004997165576*deps.long[t,"Nugaal_Departures"]

L <- 1.13795004879906
M <- - 646.71173531923

N <- sum(Fa,G,H,I,J,K,na.rm=TRUE)

O <- max(E, N, na.rm=TRUE)

P <- max(D, O, na.rm=TRUE)

Q <- max(C, L*P,na.rm=TRUE)

R<- sum(Q,M,na.rm=TRUE)

BayR <-  max(A, max(B, R,na.rm=TRUE),na.rm=TRUE)
    
    PA[t] <- BayR
    
    
    #Bay_Incidents
    PI[t] <- 0
    #Bay_Departures
    PD[t] <- 0
    
  }
  return(PA)
}


bay_8B_arrivals <- function(start,end){

start = 15

end=92
len=92

PI <- PA <- PD <- rep(NA, len)

for(t in start:end){
  

A <- fatalities.long[t,"Banaadir_Fatalities"]
B <- median(deps.long[(t-9):t,"Bay_Departures"])
C <- arrs.long[t,"Sool_Arrival"]
D <- arrs.long[t,"Gedo_Arrival"]
E <- 4.76272011396427*arrs.long[t,"Bakool_Arrival"]
Fa <- 2.14648317039228*mean(arrs.long[(t-15):t,"Hiiraan_Arrival"])

G <- -1*water.long[t,"Hiiraan_WaterDrumPrice"]
H <- - 7.61800896236803*deps.long[t,"Nugaal_Departures"]
I <- sum(D, E, Fa, G, H, na.rm=TRUE)
J <- 1.11612628987806*max(C, I,na.rm=TRUE)
K <- max(B,J, na.rm=TRUE)

B_R <- sum(A,K,na.rm=TRUE)

    
    PA[t] <- B_R
    
    
    #Bay_Incidents
    PI[t] <- 0
    #Bay_Departures
    PD[t] <- 0
    
  }
  return(PA)
}

bay_8C_arrivals <- function(start,end){

start = 15

end=92
len=92

PI <- PA <- PD <- rep(NA, len)

for(t in start:end){


A<- arrs.long[t,"Sool_Arrival"]
B <- median(deps.long[(t-9):t,"Bay_Departures"])
C <- 5.12362836857411*arrs.long[t,"Bakool_Arrival"]
D <- 2.36682580358288*mean(arrs.long[(t-15):t,"Hiiraan_Arrival"])
E <- arrs.long[t,"Gedo_Arrival"]
Fa <- -1*water.long[t,"Hiiraan_WaterDrumPrice"]
G <- -8.93031647771045*deps.long[t,"Nugaal_Departures"]

H <- sum(C,D,E,Fa,G,na.rm=TRUE)

B_R <- max(A, max(B, H,na.rm=TRUE),na.rm =TRUE)

    
    PA[t] <- B_R
    
    
    #Bay_Incidents
    PI[t] <- 0
    #Bay_Departures
    PD[t] <- 0
    
  }
  return(PA)
}

bay_8D_arrivals <- function(start,end){

start = 15

end=92
len=92

PI <- PA <- PD <- rep(NA, len)

for(t in start:end){


B <- median(deps.long[(t-9):t,"Bay_Departures"])
C <- 400.2215621049
E <- arrs.long[t,"Gedo_Arrival"]
D <- 4.7589591895442*arrs.long[t,"Bakool_Arrival"]
Fa <- 2.16688492800835*mean(arrs.long[(t-15):t,"Hiiraan_Arrival"])
G <- -1*water.long[t,"Hiiraan_WaterDrumPrice"]
H <- - 7.62060961492084*deps.long[t,"Nugaal_Departures"]
Tots <- sum(E,D,Fa,G,H,na.rm=TRUE)
I <- 1.11705165726443*max(C, Tots, na.rm=TRUE)

B_R <-max(B, I, na.rm=TRUE)


    
    PA[t] <- B_R
    
    
    #Bay_Incidents
    PI[t] <- 0
    #Bay_Departures
    PD[t] <- 0
    
  }
  return(PA)
}

bay_9A_arrivals <- function(start,end){

start = 15

end=92
len=92

PI <- PA <- PD <- rep(NA, len)

for(t in start:end){


A <- rivers.long[(t-4):t,"Juba_River_discharge"]
B <- arrs.long[t,"Sool_Arrival"]
C <- arrs.long[t,"Sanaag_Arrival"]
D <- median(deps.long[(t-9):t,"Bay_Departures"])
E <- - 722.044533668692
Fa <- 6.17433893641372e-5
G <- 6.17433893641372e-5*arrs.long[t,"Gedo_Arrival"]*arrs.long[t,"Shabeellaha_Hoose_Arrival"]
H <- 8.00242011030487e-9*arrs.long[t,"Bakool_Arrival"]*arrs.long[t,"Gedo_Arrival"]*arrs.long[t,"Shabeellaha_Hoose_Arrival"]
I <- 0.12338263288809*arrs.long[t,"Bakool_Arrival"]*conflicts.long[t,"Woqooyi_Galbeed_Conflict"]^2
J <- - 258.46338778245*fatalities.long[t,"Gedo_Fatalities"]

K<- sum(C,D,E, na.rm=TRUE)
L <- sum(Fa, G,H, I, J, na.rm=TRUE)

B_R <- max(A, max(B, max( K,L,na.rm=TRUE),na.rm=TRUE),na.rm=TRUE)


    
    PA[t] <- B_R
    
    
    #Bay_Incidents
    PI[t] <- 0
    #Bay_Departures
    PD[t] <- 0
    
  }
  return(PA)
}

bay_9B_arrivals <- function(start,end){

start = 15

end=92
len=92

PI <- PA <- PD <- rep(NA, len)

for(t in start:end){


A <- 250.733325202228
B <- arrs.long[t,"Sool_Arrival"]
C <- arrs.long[t,"Sanaag_Arrival"]
D <- median(deps.long[(t-9):t,"Bay_Departures"])
E <- - 968.883338991468
Fa <- 6.04816086006533e-5*arrs.long[t,"Gedo_Arrival"]*arrs.long[t,"Shabeellaha_Hoose_Arrival"]
G <- 8.02723957887815e-9*arrs.long[t,"Bakool_Arrival"]*arrs.long[t,"Gedo_Arrival"]*arrs.long[t,"Shabeellaha_Hoose_Arrival"]
H <- 0.122774506198443*arrs.long[t,"Bakool_Arrival"]*conflicts.long[t,"Woqooyi_Galbeed_Conflict"]^2
I <- - 256.263235226128*fatalities.long[t,"Gedo_Fatalities"]

J <- sum(C,D,E, na.rm=TRUE)
K <- sum(Fa,G,H,I, na.rm=TRUE)

B_R = sum(A,max(B, max(J, K, na.rm=TRUE),na.rm=TRUE),na.rm=TRUE)


    
    PA[t] <- B_R
    
    
    #Bay_Incidents
    PI[t] <- 0
    #Bay_Departures
    PD[t] <- 0
    
  }
  return(PA)
}

bay_9C_arrivals <- function(start,end){
  
  start = 15
  
  end=92
  len=92
  
  PI <- PA <- PD <- rep(NA, len)
  
  for(t in start:end){
    
    
A <- fatalities.long[t,"Mudug_Fatalities"]
B <- arrs.long[t,"Sanaag_Arrival"]
C <- median(deps.long[(t-9):t,"Bay_Departures"])
D <- 8.12603260178045e-5*arrs.long[t,"Gedo_Arrival"]*arrs.long[t,"Shabeellaha_Hoose_Arrival"]
E <- 6.19210913635713e-9*arrs.long[t,"Bakool_Arrival"]*arrs.long[t,"Gedo_Arrival"]*arrs.long[t,"Shabeellaha_Hoose_Arrival"]
Fa <- 0.112411952049919*arrs.long[t,"Bakool_Arrival"]*arrs.long[t,"Woqooyi_Galbeed_Conflict"]^2
G <- - 17.9398145837931*fatalities.long[t,"Gedo_Fatalities"]*fatalities.long[t,"Mudug_Fatalities"]


sum1 <- sum(B,C,na.rm=TRUE)
sum2 <- sum(D,E,Fa,G, na.rm=TRUE)
max1 <- max(sum1,sum2,na.rm=TRUE)
B_R <- sum(A, max1, na.rm=TRUE)
    
    
    PA[t] <- B_R
    
    
    #Bay_Incidents
    PI[t] <- 0
    #Bay_Departures
    PD[t] <- 0
    
  }
  return(PA)
}


bay_10A_arrivals <- function(start,end){
  
  start = 15
  
  end=92
  len=92
  
  PI <- PA <- PD <- rep(NA, len)
  
  for(t in start:end){
    
A <- 0.336294386323842*deps.long[t,"Bay_Departures"]
B <- 0.883241683448547*deps.long[t,"Bay_Departures"]*median(fatalities.long[(t-6):t,"Woqooyi_Galbeed_Fatalities"])
C <- 0.000300318923725438*arrs.long[t,"Bakool_Arrival"]*arrs.long[t,"Togdheer_Arrival"]
D <- 5.03074980853227e-5*arrs.long[(t-14),"Mudug_Arrival"]^2
E <- 1.24465096290049*arrs.long[t,"Togdheer_Arrival"]*median(fatalities.long[(t-6):t,"Woqooyi_Galbeed_Fatalities"])^3*median(fatalities.long[(t-13):t,"Jubbada_Dhexe_Fatalities"])^2
Fa <- mean(rivers.long[(t-5):t,"Juba_River_discharge"])
G <- -1*mean(rivers.long[(t-2):t,"Shabelle_River_discharge"])

B_R <- sum(A,B,C,D,E,Fa,G,na.rm=TRUE)   
    
    PA[t] <- B_R
    
    
    #Bay_Incidents
    PI[t] <- 0
    #Bay_Departures
    PD[t] <- 0
    
  }
  return(PA)
}

bay_10B_arrivals <- function(start,end){
  
  start = 15
  
  end=92
  len=92
  
  PI <- PA <- PD <- rep(NA, len)
  
  for(t in start:end){
    
A <- 0.320513983017835*deps.long[t,"Bay_Departures"]
B <- 0.894457297103651*deps.long[t,"Bay_Departures"]*median(fatalities.long[(t-6):t,"Woqooyi_Galbeed_Fatalities"])
C <- 0.000300114103985695*arrs.long[t,"Bakool_Arrival"]*arrs.long[t,"Togdheer_Arrival"]
D <- 5.02315874441267e-5*arrs.long[(t-14),"Mudug_Arrival"]^2
E <- 1.24211263635356*arrs.long[t,"Togdheer_Arrival"]*median(fatalities.long[(t-6):t,"Woqooyi_Galbeed_Fatalities"])^3*median(fatalities.long[(t-13):t,"Jubbada_Dhexe_Fatalities"])^2
Fa <- mean(rivers.long[(t-6):t,"Juba_River_discharge"])
G <- - 97.3156863418184

B_R <- sum( A, B , C , D , E , Fa , G, na.rm=TRUE) 
    
    PA[t] <- B_R
    
    
    #Bay_Incidents
    PI[t] <- 0
    #Bay_Departures
    PD[t] <- 0
    
  }
  return(PA)
}

bay_10C_arrivals <- function(start,end){
  
  start = 15
  
  end=92
  len=92
  
  PI <- PA <- PD <- rep(NA, len)
  
  for(t in start:end){
    
A <- 296.475535608648
B <- 0.305268739338313*deps.long[t,"Bay_Departures"]
C <- 0.918562429161476*deps.long[t,"Bay_Departures"]*median(fatalities.long[(t-6):t,"Woqooyi_Galbeed_Fatalities"])
D <- 0.000299985274249788*arrs.long[t,"Bakool_Arrival"]*arrs.long[t,"Togdheer_Arrival"]
E <- 5.01244719322297e-5*arrs.long[(t-14),"Mudug_Arrival"]^2
Fa <- 1.23683041943114*arrs.long[t,"Togdheer_Arrival"]*median(fatalities.long[(t-6):t,"Woqooyi_Galbeed_Fatalities"])^2*median(fatalities.long[(t-13):t,"Jubbada_Dhexe_Fatalities"])

B_R <- sum(A , B , C , D , E , Fa, na.rm=TRUE)
    
    PA[t] <- B_R
    
    
    #Bay_Incidents
    PI[t] <- 0
    #Bay_Departures
    PD[t] <- 0
    
  }
  return(PA)
}
bay_11A_arrivals <- function(start,end){
  
  start = 15
  
  end=92
  len=92
  
  PI <- PA <- PD <- rep(NA, len)
  
  for(t in start:end){
    
A <- rivers.long[(t-4), "Juba_River_discharge"]
B <- arrs.long[t,"Nugaal_Arrival"]
C <- conflicts.long[t,"Togdheer_Conflict"]*arrs.long[(t-8),"Awdal_Arrival"]
D <- 0.751209786050943*conflicts.long[t,"Woqooyi_Galbeed_Conflict"]*conflicts.long[t,"Bakool_Arrival"]
E <- 7.68973645229911e-5*arrs.long[t,"Gedo_Arrival"]*arrs.long[t,"Shabeellaha_Hoose_Arrival"]
Fa <- conflicts.long[(t-2),"Awdal_Conflict"]*arrs.long[(t-8),"Awdal_Arrival"]*mean(fatalities.long[(t-4):t,"Togdheer_Fatalities"])
G <- -5412.02896954434
H <- 0.326381289307618*deps.long[t,"Bay_Departures"]
I <- arrs.long[t,"Sool_Arrival"]

SUM1 <- sum(B , C , D , E , Fa , G,na.rm=TRUE)
MAX1 <- max( SUM1, H , na.rm=TRUE)
SUM2 <- sum( A , MAX1, na.rm=TRUE)
B_R <- max(SUM2, I, na.rm=TRUE)
    
    PA[t] <- B_R
    
    
    #Bay_Incidents
    PI[t] <- 0
    #Bay_Departures
    PD[t] <- 0
    
  }
  return(PA)
}


bay_11B_arrivals <- function(start,end){
  
  start = 15
  
  end=92
  len=92
  
  PI <- PA <- PD <- rep(NA, len)
  
  for(t in start:end){
    
A <- rain.long[t,"Awdal_rain"]
B <- rivers.long[(t-4),"Juba_River_discharge"]
C <- arrs.long[t,"Nugaal_Arrival"]
D <- 0.765883391615063*conflicts.long[t,"Woqooyi_Galbeed_Conflict"]*arrs.long[t,"Bakool_Arrival"]
E <- 7.54284708582738e-5*arrs.long[t,"Gedo_Arrival"]*arrs.long[t,"Shabeellaha_Hoose_Arrival"]
Fa <- conflicts.long[(t-2):t, "Awdal_Conflict"]*arrs.long[(t-8),"Awdal_Arrival"]*median(fatalities.long[(t-2):t,"Togdheer_Fatalities"])
G <- -5408.79915088888
H <-  0.337840783325857*deps.long[t,"Bay_Departures"]
S <- arrs.long[t,"Sool_Arrival"]

K <- sum(C , D ,E ,Fa ,G, na.rm=TRUE)
L <- max(K, H, na.rm=TRUE )
SUM1 <- sum(A,B,L, na.rm=TRUE)


B_R <- max( SUM1, S, na.rm=TRUE)
    
    PA[t] <- B_R
    
    
    #Bay_Incidents
    PI[t] <- 0
    #Bay_Departures
    PD[t] <- 0
    
  }
  return(PA)
}


bay_arrivals_all <- function(start,end){
  PI <- PA <- PD <- rep(NA, len)
  
    for (t in start:end){
    
    #BAY PREDICTION FOR JULY
      #Bay_Arrival = Awdal_Arrival +
      A_A <- arrs.long[ (t),"Awdal_Arrival"] 
      #0.0203037108512618*Banadir_Arrival + 
      Ban_A_reg <- 0.0203037108512618
      Ban_A <- arrs.long[ (t), "Banadir_Arrival"]

      #Woqooyi_Galbeed_Conflict*Woqooyi_Galbeed_rain +
      WG_C <- conflicts.long[ (t),"Woqooyi_Galbeed_Conflict"]
      WG_R <- rain.long[ (t),"Woqooyi_Galbeed_rain"]
    
      #0.00680481834069344*Bakool_Arrival*Woqooyi_Galbeed_Arrival + 
      Bak_A_reg <- 0.00680481834069344
      Bak_A <- arrs.long[ (t), "Bakool_Arrival"]
      WG_A <- arrs.long[ (t),"Woqooyi_Galbeed_Arrival"]
    

      #3.08795663688879e-7*Banadir_Arrival*Woqooyi_Galbeed_Arrival^2 - 
      WG_A_reg <- 3.08795663688879e-7
      Ban_A <- arrs.long[ (t), "Banadir_Arrival"]
      WG_A <- arrs.long[ (t),"Woqooyi_Galbeed_Arrival"]  
      #2.0551200483303*Awdal_Departures -
      A_D_reg <- -2.0551200483303
      A_D <- deps.long[ (t),"Awdal_Departures"] 
      #1.25835377627807e-7*Banadir_Arrival*Sool_Arrival*Woqooyi_Galbeed_Arrival
      B_A_reg <- -1.25835377627807e-7
      S_A <- arrs.long[ (t), "Sool_Arrival"]


      PA[t] <- 0
      
      
      PA[t] <- sum(A_A, Ban_A_reg*Ban_A, WG_C*WG_R, Bak_A_reg*Bak_A*WG_A, 
        WG_A_reg*Ban_A*WG_A*WG_A, A_D_reg*A_D, B_A_reg*Ban_A*S_A*WG_A, na.rm = TRUE)
      #Bay_Incidents
      PI[t] <- 0
      #Bay_Departures
      PD[t] <- 0
      
    
  }
  
  
  return(PA)
}

bay_arrivals <- function(start, end){
  #this section to make the predictions per Region
  PI <- PA <- PD <- rep(NA, len)
  
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

bay_bestwaterfit_arrivals <- function(region, futuremonths){
  #this section to make the predictions per Region
  for (t in futuremonths:1){
    
    #BAY PREDICTION FOR JULY
    #output$r0 <- "Jubbadaa_Hoose"
    #Bay_Arrival = 1.31268762505684*Sanaag_Departures + 1.31268762505684*delay(Bari_Departures, 11) + 
    # 0.659938976916995*Togdheer_Arrival + 
    #delay(Woqooyi_Galbeed_Conflict, 2)*smm(Woqooyi_Galbeed_Departures, 17) + 
    #mma(Jubbada_Hoose_Conflict, 2)*mma(Bari_Conflict, 4) + 
    #-15436.7884288987/sma(Togdheer_Departures, 9) + 
    #1.74774106348673e-5*Banadir_Arrival*Bay_WaterDrumPrice + 
    #mma(Jubbada_Hoose_Conflict, 2) - 0.420988035304692*Banadir_Arrival
    
    PA[t] <- 0
    
    # detecting the vars in the dataset.
    S_D <- deps.long[ (total_len-t),"Sanaag_Departures"]
    B_D <- deps.long[ (total_len-t-11), "Bari_Departures"]
    T_A <- arrs.long[ (total_len-t),"Togdheer_Arrival"]
    WG_C <- conflicts.long[ (total_len-t-2),"Woqooyi_Galbeed_Conflict"]
    WG_D <- median(deps.long[(total_len-t-17):(total_len-t),"Woqooyi_Galbeed_Departures"])
    # fix this to be moderate
    JH_C <- mean(conflicts.long[(total_len-t-2):(total_len-t),"Jubbada_Hoose_Conflict"])
    B_C <- mean(conflicts.long[(total_len-t-4):(total_len-t),"Bari_Conflict"])
    T_D <- median(deps.long[ (total_len-t-9):(total_len-t), "Togdheer_Departures"])
    B_A <- arrs.long[ (total_len-t),"Banadir_Arrival"]
    #B_W <-water.long[total_len-t),"Bay_WaterDrumPrice"]
    
    #reg
    S_D_reg <- 1.31268762505684
    B_D_reg <- 1.31268762505684
    T_A_reg <- 0.659938976916995
    T_D_reg <- -15436.7884288987
    B_A_B_W_reg <- 1.74774106348673e-5
    B_A_reg <- - 0.42098803530469
    
    #PA[t] <- sum(S_D_reg*S_D, B_D_reg*B_D, T_A_reg*T_A, WG_C*WG_D, 
    #JH_C*B_D,T_D_reg/T_D,B_A_B_W_reg*B_W,JH_C, B_A_reg*B_A, na.rm = TRUE)
    
    
    PA[t] <- sum(S_D_reg*S_D, B_D_reg*B_D, T_A_reg*T_A, WG_C*WG_D, 
                 JH_C*B_D,T_D_reg/T_D,JH_C, B_A_reg*B_A,na.rm = TRUE)

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
      exact_time <- as.character(input$date2) 
      
      value <- as.Date(exact_time, format = "%d/%m/%Y")
      position <- which(arrs.long$Date == value)
      updateSliderInput(session, "dep_var_1", "Arrivals in Jubbada Hoose", value = as.numeric(arrs.long[position,"Jubbada_Hoose_Arrival"]),
                        min = 1, max = (max(as.numeric(unlist(arrs.long[,"Jubbada_Hoose_Arrival"])),na.rm=T)+5000) , step = 2)  
      updateSliderInput(session, "dep_var_2", "Arrivals in Awdal", value =  as.numeric(arrs.long[position,"Awdal_Arrival"]),
                        min = 1, max = (max(as.numeric(unlist(arrs.long[,"Awdal_Arrival"])),na.rm=T)+5000) , step = 2)  
      updateSliderInput(session, "dep_var_3", "Arrivals in Togdheer", value =  as.numeric(arrs.long[position,"Togdheer_Arrival"]),
                        min = 1, max = (max(as.numeric(unlist(arrs.long[,"Togdheer_Arrival"])),na.rm=T)+5000) , step = 2)  
      updateSliderInput(session, "dep_var_4", "Arrivals in Bay", value =  as.numeric(arrs.long[position,"Bay_Arrival"]),
                        min = 1, max = (max(as.numeric(unlist(arrs.long[,"Bay_Arrival"])),na.rm=T)+5000) , step = 2) 
      
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
          PA <- bay_11B_arrivals(fmonths_start, fmonths_end)
          PI <- PA[fmonths_start:fmonths_end]  
        }  
    } 
        
    else{
      produce_slider(region)
    }
    A<- A[1:len]
    Date = conflicts.long$Date[fmonths_start:fmonths_end]
    long <- data.frame(
      Period=rep((1:len),3), 
      Date,
      Population = c(A, PI, PD), 
      Indicator=rep(c("Actual Arrivals", 
                      "Historic Model Arrivals", 
                      "Historic Model Departures"), 
                    each=len))
    Actual_Arrivals <- A
    Model_Arrivals <- PI
    
    wide <- cbind(Date, Actual_Arrivals, Model_Arrivals)
    list(long=long, wide=wide)
    
    
  })
  
  #Create a datatable with all the values from the inputs
  output$datatable <- renderTable({
    Tdata <- cbind(pred_data()[["wide"]])
    Tdata <- cbind(Tdata)
    #Tdata[seq(1, nrow(Tdata), length.out=nrow(Tdata)),]
  })
  
  
  output$graph1 <- renderPlot({
    
    long <- mydata()[["long"]]
    p <- ggplot(long[long$Indicator %in% input$Indicators,], 
                aes(x=Date, y=Population, group=Indicator))    
    p <- p + 
      geom_line(aes(colour = Indicator), size=1, alpha=.75) + 
      ggtitle("Arrivals, Departures, Incidents as Recorder")+
      scale_x_date(name="Month", date_breaks = "6 month", date_minor_breaks = "1 month", date_labels = "%b %Y")+ 
      scale_y_continuous(labels = comma)
    print(p)
  })
  
  output$graph2 <- renderPlot({
    
    long <- pred_data()[["long"]]
    p <- ggplot(long[long$Indicator %in% input$Future_Indicators,], 
                aes(x=Date, y=Population, group=Indicator))    
    p <- p + 
      geom_line(aes(colour = Indicator), size=1, alpha=.75) + 
      ggtitle("Historic Predictions")+
      scale_x_date(name="Month", date_breaks = "1 month", date_minor_breaks = "1 month", date_labels = "%b %Y")+ 
      scale_y_continuous(labels = comma)
    print(p)
  })
  
  }

)

