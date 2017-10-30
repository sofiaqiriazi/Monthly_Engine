library(shiny)
library(datasets)
library(magrittr)
library(XML)
library(reshape)
library(gsheet)
library(ggplot2)
library(scales)
library(zoo)
library(pracma)
library(psych)

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
discharge.long <- read.csv(text=discharge,stringsAsFactors = FALSE)

stations <- gsheet2text(jetson, sheetid=1052168743)
stations.long <- read.csv(text=stations,stringsAsFactors = FALSE)

Dates <- sapply(conflicts.long[,1],as.character.Date)
conflicts.long$Date <- as.Date(conflicts.long$Date, format="%m/%d/%Y")
arrs.long$Date <- as.Date(arrs.long$Date, format="%m/%d/%Y")
deps.long$Date <- as.Date(deps.long$Date, format="%m/%d/%Y")
rain.long$Date <-as.Date(rain.long$Date, format="%m/%d/%Y")
water.long$Date <-as.Date(water.long$Date, format="%m/%d/%Y")
rivers.long$Date <-as.Date(rivers.long$Date, format="%m/%d/%Y")
goats.long$Date <-as.Date(goats.long$Date, format="%m/%d/%Y")
fatalities.long$Date <-as.Date(fatalities.long$Date, format="%m/%d/%Y")
stations.long$Date <-as.Date(stations.long$Date, format="%m/%d/%Y")

# Force columns to be text
conflicts.long[,2:ncol(conflicts.long)] <- sapply(conflicts.long[,2:ncol(conflicts.long)], as.numeric)
arrs.long[,2:ncol(arrs.long)] <- sapply(arrs.long[,2:ncol(arrs.long)], as.numeric)
deps.long[,2:ncol(deps.long)] <- sapply(deps.long[,2:ncol(deps.long)], as.numeric)
rain.long[,2:ncol(rain.long)] <- sapply(rain.long[,2:ncol(rain.long)], as.numeric)
water.long[,2:ncol(water.long)] <- sapply(water.long[,2:ncol(water.long)], as.numeric,na.rm=TRUE)
rivers.long[,2:ncol(rivers.long)] <- sapply(rivers.long[,2:ncol(rivers.long)], as.numeric)
goats.long[,2:ncol(goats.long)] <- sapply(goats.long[,2:ncol(goats.long)], as.numeric)
fatalities.long[,2:ncol(fatalities.long)] <- sapply(fatalities.long[,2:ncol(fatalities.long)], as.numeric)
stations.long[,2:ncol(stations.long)] <- sapply(stations.long[,2:ncol(stations.long)], as.numeric)

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
    A <- rain.long[t,"Shabeellaha_Dhexe_rain"] 
    B <- arrs.long[(t-1),"Woqooyi_Galbeed_Arrival"] 
    C <- mean(arrs.long[(t-6):t,"Togdheer_Arrival"])
    
    E <- 2.84291053988138*arrs.long[t,"Bakool_Arrival"]
    Fa <- 1.90605269499887*arrs.long[t,"Bari_Arrival"]
    G <- 0.582938861571093*deps.long[t,"Bay_Departures"]
    H <- arrs.long[(t-1),"Woqooyi_Galbeed_Arrival"]
    I <- -1*rain.long[t,"Shabeellaha_Dhexe_rain"]*mean(deps.long[(t-2):t,"Jubbada_Hoose_Departures"])
    J <- -0.0801078676959042*water.long[t,"Hiiraan_WaterDrumPrice"]
    K <- -6.62472178142272*deps.long[t,"Awdal_Departures"]
    L <- 0.263886473589876*deps.long[t,"Bay_Departures"]
    
    
    
    D <- max(sum(E,Fa,G ,H,I,J,K, na.rm=TRUE), L)
    
    fin <- sum(A,B,C,D, na.rm=TRUE)    
    PA[t] <- fin

    
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


bay_11C_arrivals <- function(start,end){
  
  start = 15
  
  end=92
  len=92
  
  PI <- PA <- PD <- rep(NA, len)
  
  for(t in start:end){
    
A <- 310
B <- fatalities.long[t,"Shabeellaha_Dhexe_Fatalities"]
C <- arrs.long[t,"Nugaal_Arrival"]
D <- 0.767007545393261*conflicts.long[t,"Woqooyi_Galbeed_Conflict"]*arrs.long[t,"Bakool_Arrival"]
E <- 7.61894156939422e-5*arrs.long[t,"Gedo_Arrival"]*arrs.long[t,"Shabeellaha_Hoose_Arrival"]
Fa <- -5609.18124757954
G <- 0.30889223926414*deps.long[t,"Bay_Departures"]
SUM1 <- sum(D , E , Fa, na.rm=TRUE )
MAX1 <- max(SUM1, G,na.rm=TRUE)
B_R <- sum( A , B , C , MAX1, na.rm= TRUE)
    
    PA[t] <- B_R
    
    
    #Bay_Incidents
    PI[t] <- 0
    #Bay_Departures
    PD[t] <- 0
    
  }
  return(PA)
}

bay_12A_arrivals <- function(start,end){
  
  start = 15
  
  end=93
  len=93
  
  PI <- PA <- PD <- rep(NA, len)
  
  for(t in start:end){
    
A <- deps.long[(t-13),"Galgaduud_Departures"]
B <- 2.26751515362942*deps.long[(t-12),"Bari_Departures"]
D <-  0.00588088839695529*arrs.long[(t-1),"Mudug_Arrival"]*arrs.long[(t-1),"Woqooyi_Galbeed_Arrival"]

CA<- median(rain.long[(t-4):(t-1),"Banaadir_rain"])

CB<- mean(movavg(arrs.long[(t-4):(t-1),"Sool_Arrival"],3,"m"))

if(is.na(CA) || CA > 0 ){

 C <- CB

}
else{ 
  C <- -5714
} 
 

G <- 2.31702827594048*arrs.long[(t-1),"Sanaag_Arrival"]
H <- 5.97879951112646*deps.long[(t-3),"Bari_Departures"]

I <- deps.long[(t-4),"Sanaag_Departures"] * rain.long[(t-5),"Hiiraan_rain"]
E <- max(H, I, na.rm=TRUE)

FIN <- max(386, max(A, max(sum(B ,D, C ,E, na.rm=TRUE), G),na.rm=TRUE),na.rm=TRUE)
    
    PA[t] <- FIN
    
    
    #Bay_Incidents
    PI[t] <- 0
    #Bay_Departures
    PD[t] <- 0
    
  }
  return(PA)
}

bay_12B_arrivals <- function(start,end){
  
  start = 15
  
  end=94
  len=94
  
  PI <- PA <- PD <- rep(NA, len)
  
  for(t in start:end){
    
A <- deps.long[(t-13),"Galgaduud_Departures"]
B <- 2.27391080050669*deps.long[(t-12),"Bari_Departures"]
C <- 0.00588055939993242*arrs.long[(t-1),"Mudug_Arrival"]*arrs.long[(t-1),"Woqooyi_Galbeed_Arrival"]
D <- mean(rain.long[(t-4):(t-1),"Banaadir_rain"])
E <- median(arrs.long[(t-4):(t-1),"Sool_Arrival"])
G <- 5.95887287478984*deps.long[(t-3),"Bari_Departures"]
H <- deps.long[(t-4),"Sanaag_Departures"]*rain.long[(t-5),"Hiiraan_rain"]
K <- 2.29816996652923*arrs.long[(t-1),"Sanaag_Arrival"]


X <- sum(311.894138182276,K,na.rm=TRUE)
if(is.na(D) || D >0 )
{
  Q <- E
}
else{Q <- -5716}

Y <- sum(B, C, Q, max(G, H, na.rm=TRUE),na.rm =TRUE)

W <- max(Y, X,na.rm=TRUE)
FIN <- max(A, W,na.rm=TRUE)

    PA[t] <- FIN
    
    
    #Bay_Incidents
    PI[t] <- 0
    #Bay_Departures
    PD[t] <- 0
    
  }
  return(PA)
}


bay_12C_arrivals <- function(start,end){
  
  start = 15
  
  end=93
  len=93
  
  PI <- PA <- PD <- rep(NA, len)
  
  for(t in start:end){
    
A <- 7.09622785331628*deps.long[(t-3),"Bari_Departures"]
B <- 2.43787508914871*deps.long[(t-12),"Bari_Departures"]
C <- 0.00601323333840485*arrs.long[(t-1),"Mudug_Arrival"]*arrs.long[(t-1),"Woqooyi_Galbeed_Arrival"]
D <- deps.long[(t-4),"Sanaag_Departures"]*rain.long[(t-5),"Hiiraan_rain"]
E <- deps.long[(t-13),"Galgaduud_Departures"]
G <- median(rain.long[(t-4):(t-1),"Banaadir_rain"])
H <- 5.95351409100946*deps.long[(t-4),"Bari_Departures"]
X <- 2.46409172818795*arrs.long[(t-1),"Sanaag_Arrival"]
if (is.na(G) || G>0)
{Y<- H}
else
{ Y <- -3205.61372701243}

FIN <- max(A, 0.93871317992841*max(sum(B , C , D , E , Y , na.rm=TRUE), X, na.rm=TRUE),na.rm=TRUE)

    PA[t] <- FIN
    
    
    #Bay_Incidents
    PI[t] <- 0
    #Bay_Departures
    PD[t] <- 0
    
  }
  return(PA)
}
bay_12D_arrivals <- function(start,end){
  
  start = 15
  
  end=93
  len=93
  
  PI <- PA <- PD <- rep(NA, len)
  
  for(t in start:end){
    
X <- mean(rain.long[(t-7):(t-1),"Banaadir_rain"])
Y <- deps.long[(t-4),"Bari_Departures"]*median(rain.long[(t-6):(t-1),"Hiiraan_rain"])

if(is.na(X) || X > 0){A<- Y}

else{A<- -2915}

B <- 2.98981569846976*deps.long[(t-12),"Bari_Departures"]
C <- 1.90068199745955*deps.long[(t-14),"Bari_Departures"]
D <- 0.005464125090538*arrs.long[(t-1),"Mudug_Arrival"]*arrs.long[(t-1),"Woqooyi_Galbeed_Arrival"]
E <- 7.08510220750896*deps.long[(t-3),"Bari_Departures"]
Z <- deps.long[(t-13),"Galgaduud_Departures"]
T <- 2.31719227653268*arrs.long[(t-1),"Sanaag_Arrival"]
FIN <- max(sum(A,max(sum(B,C,D,na.rm=TRUE), sum(E,Z,na.rm=TRUE),na.rm=TRUE),na.rm=TRUE), T,na.rm=TRUE)

    PA[t] <- FIN
    
    
    #Bay_Incidents
    PI[t] <- 0
    #Bay_Departures
    PD[t] <- 0
    
  }
  return(PA)
}
bay_12E_arrivals <- function(start,end){
  
  start = 15
  
  end=93
  len=93
  
  PI <- PA <- PD <- rep(NA, len)
  
  for(t in start:end){
X <- mean(rain.long[(t-7):(t-1),"Banaadir_rain"])
Y <- deps.long[(t-4),"Bari_Departures"]*median(rain.long[(t-6):(t-1),"Hiiraan_rain"])
if(is.na(X) || X>0){A<- Y}
else{A<- -2908}
B <- 3*deps.long[(t-12),"Bari_Departures"]
C <- 0.005464125090538*arrs.long[(t-1),"Mudug_Arrival"]*arrs.long[(t-1),"Woqooyi_Galbeed_Arrival"]
D <- deps.long[(t-14),"Bari_Departures"]
E <- 7.08538265288562*deps.long[(t-3),"Bari_Departures"]
G <- deps.long[(t-13),"Galgaduud_Departures"]
H <- 2.31173952956057*arrs.long[(t-1),"Sanaag_Arrival"]

FIN <- max(sum(A,max(sum(B, C, D, na.rm=TRUE), sum(E, G,na.rm=TRUE)),na.rm=TRUE), H, na.rm=TRUE)

    PA[t] <- FIN
    
    
    #Bay_Incidents
    PI[t] <- 0
    #Bay_Departures
    PD[t] <- 0
    
  }
  return(PA)
}
bay_12F_arrivals <- function(start,end){
  
  start = 15
  
  end=93
  len=93
  
  PI <- PA <- PD <- rep(NA, len)
  
  for(t in start:end){
X <- mean(rain.long[(t-7):(t-1),"Banaadir_rain"])
Y <- 6.01020527817426*deps.long[(t-4),"Bari_Departures"]
if(is.na(X)|| X >0 ){A <- Y}
else {A<- -2883}
B <- 3*deps.long[(t-12),"Bari_Departures"]
C <- 0.005464125090538*arrs.long[(t-1),"Mudug_Arrival"]*arrs.long[(t-1),"Woqooyi_Galbeed_Arrival"]
D <- 1.79377455952668*deps.long[(t-14),"Bari_Departures"]
E <- 7.08538265288562*deps.long[(t-3),"Bari_Departures"]
G <- deps.long[(t-13),"Galgaduud_Departures"]
H <- 2.31173952956057*arrs.long[(t-1),"Sanaag_Arrival"]

FIN <-  max(sum(A, max(sum(B, D, C, na.rm=TRUE), sum(E, G,na.rm=TRUE), na.rm=TRUE),na.rm=TRUE), H,na.rm=TRUE)


    PA[t] <- FIN
    
    
    #Bay_Incidents
    PI[t] <- 0
    #Bay_Departures
    PD[t] <- 0
    
  }
  return(PA)
}

bay_12NEWA_arrivals <- function(start,end){
  
  start = 15
  
  end=94
  len=94
  
  PI <- PA <- PD <- rep(NA, len)
  
  for(t in start:end){
A <- arrs.long[(t-1),"Bari_Arrival"]
B <- median(rain.long[(t-4):(t-1),"Banaadir_rain"],na.rm = TRUE)
C <- mean(arrs.long[(t-3):(t-1),"Sool_Arrival"],na.rm = TRUE)

if(is.na(B) || B>0){
  D <- C
}
else{
  D <- -5711.49956439839
}

E <- 0.00577144170042261*arrs.long[(t-1),"Mudug_Arrival"]*arrs.long[(t-1),"Woqooyi_Galbeed_Arrival"]
G <- 1.73906550532281*deps.long[(t-13),"Galgaduud_Departures"]
KA <- median(rain.long[(t-4):(t-1),"Banaadir_rain"],na.rm=TRUE)
KB <- 5.91536930063778*deps.long[(t-3),"Bari_Departures"]
KC <- deps.long[(t-4),"Sanaag_Departures"]*rain.long[(t-5),"Hiiraan_rain"]

if (is.na(KA) || KA >0){
  K<-KB
}
else{
  K<-KC
}

LA <- median(rain.long[(t-14):(t-1),"Sool_rain"],na.rm=TRUE)
LB <- 2.31175490528636*arrs.long[(t-1),"Sanaag_Arrival"]
LC <- rivers.long[(t-5),"Juba_River_discharge"]


if(is.na(LA) || LA>0 ){
  L<- LB
}
else{
  L<- LC
}

FIN <- max(sum(A, D, max(E, G, na.rm=TRUE), K, na.rm=TRUE), L, na.rm=TRUE)

    PA[t] <- FIN
    
    
    #Bay_Incidents
    PI[t] <- 0
    #Bay_Departures
    PD[t] <- 0
    
  }
  return(PA)
}

bay_12NEWB_arrivals <- function(start,end){
  
  start = 15
  
  end=93
  len=93
  
  PI <- PA <- PD <- rep(NA, len)
  
  for(t in start:end){
A <- arrs.long[(t-1),"Bari_Arrival"]
BA <- median(rain.long[(t-4):(t-1),"Banaadir_rain"])
BB <- mean(arrs.long[(t-3):(t-1),"Sool_Arrival"])
if (is.na(BA) || BA>0){
  B<- BB
}
else{
  B<- -5710.82225073712
}

C <- 0.00577144170042261*arrs.long[(t-1),"Mudug_Arrival"]*arrs.long[(t-1),"Woqooyi_Galbeed_Arrival"]
D <- deps.long[(t-13),"Galgaduud_Departures"]*mean(fatalities.long[(t-5):(t-1),"Woqooyi.Galbeed_Fatalities"])

GA <- fatalities.long[(t-1),"Jubbada.Dhexe_Fatalities"]
GB <- 5.91366075067026*deps.long[(t-3),"Bari_Departures"]
GC <- deps.long[(t-4),"Sanaag_Departures"]*rain.long[(t-5),"Hiiraan_rain"]

if (is.na(GA) || GA>0){
  G<- GB
}
else{
  G<- -5710.82225073712
}
SA <- median(rain.long[(t-14):(t-1),"Sool_rain"])
SB <- 2.31175490528636*arrs.long[(t-1),"Sanaag_Arrival"]
if (is.na(SA) || SA>0){
  S<- SB
}
else{
  S<- 442.911557453352
}

FIN <- max(sum(A, B, max(C, D,na.rm=TRUE), G, na.rm=TRUE), S, na.rm=TRUE)

    PA[t] <- FIN
    
    
    #Bay_Incidents
    PI[t] <- 0
    #Bay_Departures
    PD[t] <- 0
    
  }
  return(PA)
}

bay_12NEWC_arrivals <- function(start,end){
  
  start = 15
  
  end=93
  len=93
  
  PI <- PA <- PD <- rep(NA, len)
  
  for(t in start:end){
A <- arrs.long[(t-1),"Bari_Arrival"]
BA <- median(rain.long[(t-4):(t-1),"Banaadir_rain"])
BB <- mean(arrs.long[(t-3):(t-1),"Sool_Arrival"])
if (is.na(BA)|| BA>0){
  B <- BB
}
else{
  B <- -5861.79559834364 
}

C <- 0.00578426367028078*arrs.long[(t-1),"Mudug_Arrival"]*arrs.long[(t-1),"Woqooyi_Galbeed_Arrival"]
D <- 1.75944428063076*deps.long[(t-13),"Galgaduud_Departures"]

EA <- median(rain.long[(t-4):(t-1),"Banaadir_rain"])
EB <- 5.91200111736018*deps.long[(t-3),"Bari_Departures"]
EC <- deps.long[(t-4),"Sanaag_Departures"]*rain.long[(t-5),"Hiiraan_rain"]

if (is.na(EA) || EA>0){
  E <- EB
}
else{
  E <- EC
}

G <- 2.31223534024849*arrs.long[(t-1),"Sanaag_Arrival"]

FIN <- max(426.077738789064, max(sum(A, B, max(C, D,na.rm=TRUE), E,na.rm=TRUE), G), na.rm=TRUE)
    
    PA[t] <- FIN
    
    
    #Bay_Incidents
    PI[t] <- 0
    #Bay_Departures
    PD[t] <- 0
    
  }
  return(PA)
}


################################################################################################
#
bay_TEST1A_arrivals <- function(start,end){
  
  start = 15
  
  end=93
  len=93
  
  PI <- PA <- PD <- rep(NA, len)
  
  for(t in start:end){
A <- 0.797107265273338*arrs.long[(t-1),"Sool_Arrival"]
B <- 0.612331581418625*arrs.long[(t-13),"Mudug_Arrival"]
C <- arrs.long[(t-1),"Sool_Arrival"]*tan(arrs.long[(t-1),"Awdal_Arrival"])
D <- 0.0278892569471559*deps.long[(t-1),"Sanaag_Departures"]*arrs.long[(t-1),"Awdal_Arrival"]
E <- 0.00505997610411542*arrs.long[(t-1),"Mudug_Arrival"]*arrs.long[(t-1),"Woqooyi_Galbeed_Arrival"]
G <- deps.long[(t-14),"Sool_Departures"]
H <- -1*arrs.long[(t-1),"Awdal_Arrival"]
J <- -1*arrs.long[(t-4),"Jubbada_Dhexe_Arrival"]
K <- tan(0.993604035057624*mean(fatalities.long[(t-17):(t-16),"Nugaal_Fatalities"]))
L <- sum (A,B,C,D,E,G,H,J,na.rm=TRUE)

FIN <- max(L, K,na.rm=TRUE)
    PA[t] <- FIN
    
    
    #Bay_Incidents
    PI[t] <- 0
    #Bay_Departures
    PD[t] <- 0
    
  }
  return(PA)
}

bay_12DATESA_arrivals <- function(start,end){
  
  start = 16
  
  end=94
  len=94
  
  PI <- PA <- PD <- rep(NA, len)
  
  for(t in start:end){
A <- deps.long[(t-1),"Jubbada_Dhexe_Departures"]
B <- mean(conflicts.long[(t-16):(t-1),"Galgaduud_Conflict"])

#Returns 1 if x <= y, 0 otherwise
CA <- mean(fatalities.long[(t-4):(t-1),"Bay_Fatalities"])
CB <- mean(conflicts.long[(t-4):(t-1),"Togdheer_Conflict"])
if (CA<=CB){C<-1}
else{C<-0}

D <- mean(arrs.long[(t-3):(t-1),"Bari_Arrival"])

E <- max(conflicts.long[(t-1),"Bay_Conflict"]^2, fatalities.long[(t-1),"Sool_Fatalities"]*deps.long[(t-1),"Togdheer_Departures"],na.rm=TRUE)
GAA <- conflicts.long[(t-1),"Bay_Conflict"]
GAB <- 2.01083064621521*arrs.long[(t-1),"Sanaag_Arrival"] 

if(is.na(GAA) || GAA > 0){
  GA <- GAB
}
else{
  GA <- 1595.33850434883
}

GB <- fatalities.long[(t-1),"Sool_Fatalities"]
GC <- deps.long[(t-1),"Togdheer_Departures"]
if (is.na(GB) || GB >0 ){
  G <- GA
}
else{
  G <- GC
}
KA <- logistic(fatalities.long[(t-1),"Mudug_Fatalities"]) 
KB <- median(arrs.long[(t-5):(t-1),"Mudug_Arrival"])
KC <- median(fatalities.long[(t-6):(t-1),"Togdheer_Fatalities"])
K <- (KA*KB*KC)
L <- tanh(median(KC))
ABC <- A*B*C
tot <- sum(ABC,D,E,G,na.rm=TRUE)
FIN <-  max(tot, K^L,na.rm=TRUE)
    PA[t] <- FIN
    
    
    #Bay_Incidents
    PI[t] <- 0
    #Bay_Departures
    PD[t] <- 0
    
  }
  return(PA)
}


bay_12DATESB_arrivals <- function(start,end){
  
  start = 16
  
  end=94
  len=94
  
  PI <- PA <- PD <- rep(NA, len)
  
  for(t in start:end){
    
A <- arrs.long[(t-1),"Nugaal_Arrival"]
B <- 361
C <- conflicts.long[(t-1),"Bay_Conflict"]^2
D <- 0.00594668706750925*arrs.long[(t-1),"Mudug_Arrival"]*arrs.long[(t-1),"Woqooyi_Galbeed_Arrival"]
E <- fatalities.long[(t-1),"Sool_Fatalities"]
G <- 3.20250999835703*mean(conflicts.long[(t-8):(t-1),"Gedo_Conflict"])*mean(deps.long[(t-8):(t-1),"Togdheer_Departures"])
H <- arrs.long[(t-1),"Nugaal_Arrival"]
if (is.na(E) || E>0){
  I<- G
}
else{
  I<- H
}
J <- -1*fatalities.long[(t-9),"Jubbada_Hoose_Fatalities"]*mean(deps.long[(t-9):(t-1),"Jubbada_Dhexe_Departures"])
K <- 2.31215780686505*arrs.long[(t-1),"Sanaag_Arrival"]
L <- water.long[(t-1),"Sool_WaterDrumPrice"]
M <- -146858.497640287
N <- sum(D, I , J, na.rm=TRUE)
P <- sum (L, M, na.rm=TRUE)
FIN <- max(max(A, max(B, max(max(C, N,na.rm=TRUE), K,na.rm=TRUE), na.rm=TRUE),na.rm=TRUE), P,na.rm=TRUE)
    PA[t] <- FIN
    
    
    #Bay_Incidents
    PI[t] <- 0
    #Bay_Departures
    PD[t] <- 0
    
  }
  return(PA)
}
bay_BLUEA_arrivals <- function(start,end){
  
  start = 16
  
  end=94
  len=94
  
  PI <- PA <- PD <- rep(NA, len)
  
  for(t in start:end){

A <- stations.long[(t-2),"Hiiraan_Belet_WeyneStation_Shabelle_River"]
B <- -1*asinh(median(stations.long[(t-5):(t-2),"Hiiraan_Belet_WeyneStation_Shabelle_River"],na.rm=TRUE))
CA <- 1.79605197655313
C <- 1392.18823411085*median(rain.long[(t-5):(t-2),"Bari_rain"],na.rm=TRUE) %% rain.long[(t-6),"Bakool_rain"]
D <- -1*stations.long[(t-2),"Hiiraan_Belet_WeyneStation_Shabelle_River"]
AA <- deps.long[(t-13),"Galgaduud_Departures"]
AB <- 2.26751515362942*deps.long[(t-12),"Bari_Departures"]
AC <- 0.00588088839695529*arrs.long[(t-1),"Mudug_Arrival"]*arrs.long[(t-1),"Woqooyi_Galbeed_Arrival"]
BA <- median(rain.long[(t-4):(t-1),"Banaadir_rain"],na.rm=TRUE)
BB <- mean(arrs.long[(t-4):(t-1),"Sool_Arrival"],na.rm=TRUE)
CA <- 5.97879951112646*deps.long[(t-3),"Bari_Departures"]
CB <- deps.long[(t-4),"Sanaag_Departures"]*rain.long[(t-5),"Hiiraan_rain"]
DA <- 2.31702827594048*arrs.long[(t-1),"Sanaag_Arrival"]
E <- stations.long[(t-2),"Hiiraan_Belet_WeyneStation_Shabelle_River"]
EA <- factorial(E)
EB <- cosh(EA)
FA <- deps.long[(t-13),"Galgaduud_Departures"]
FB <- 2.26751515362942*deps.long[(t-12),"Bari_Departures"]
FC <- 0.00588088839695529*arrs.long[(t-1),"Mudug_Arrival"]*arrs.long[(t-1),"Woqooyi_Galbeed_Arrival"]
FD <- median(rain.long[(t-4):(t-1),"Banaadir_rain"],na.rm=TRUE)
FE <- mean(arrs.long[(t-4):(t-1),"Sool_Arrival"],na.rm=TRUE)
GA <- 5.97879951112646*deps.long[(t-3),"Bari_Departures"]
GB <- deps.long[(t-4),"Sanaag_Departures"]*rain.long[(t-5),"Hiiraan_rain"]
GC <- 2.31702827594048*arrs.long[(t-1),"Sanaag_Arrival"]

if(is.na(FD) || FD >0 ){ FIF <- FE}
else{FIF <- -5714}
GMAX <- max(GA, GB, na.rm=TRUE)
GMAXMAX <- max(sum(FB, FC, FIF, GMAX, na.rm=TRUE), GC, na.rm=TRUE)
FAGMAX <- max(FA, GMAXMAX,na.rm=TRUE)
CMAX <- max(CA, CB, na.rm=TRUE)
if(is.na(BA) || BA>0){BIF <- BB}
else{BIF <- -5714}
DSUM <- sum(AB, AC, BIF, CMAX, na.rm=TRUE)
DMAX <- max(DSUM, DA,na.rm=TRUE)
CSUM <- sum(CA, C, D,na.rm=TRUE)
AMAX <- max(AA, DMAX, na.rm=TRUE)
MAXAMAX <- max(386, AMAX,na.rm=TRUE)
if (is.na(CSUM) || CSUM>0){CSUMIF <- MAXAMAX}
else{CSUMIF <- 613.300238738678}
ABSUM <- sum(A, B, na.rm=TRUE)
EMAXFAG <- EB*max(386, FAGMAX, na.rm=TRUE)
if (is.na(ABSUM) || ABSUM>0){MAINIF <- CSUMIF}
else{MAINIF <- EMAXFAG}
FIN <- sum (MAINIF, -72.320225746779,na.rm=TRUE)

    PA[t] <- FIN
    
    
    #Bay_Incidents
    PI[t] <- 0
    #Bay_Departures
    PD[t] <- 0
    
  }
  return(PA)
}
# Define a server for the Shiny app
# the ids refer to the google sheet refering to the special identifier
shinyServer(function(input, output, session) {

  
  pred_data <- reactive({
    
    #testing

    region <- input$region
    fmonths_start <- which(conflicts.long$Date == monthStart(as.Date("2011-07-01")))
    fmonths_end <- which(conflicts.long$Date == monthStart(as.Date("2017-10-01")))
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
          PA <- bay_BLUEA_arrivals(fmonths_start, fmonths_end)
          PI <- PA[fmonths_start:fmonths_end]
    }
    else{
          PA <- rep(NA, len)
          PI <- rep(NA, len)
    }
        
    A<- A[1:len]
    Date <- conflicts.long$Date[fmonths_start:fmonths_end]
    long <- data.frame(
      Period=rep((1:len),2), 
      Date=Date,
      Population = c(A, PI), 
      Indicator=rep(c("Actual Arrivals", 
                      "Model Arrivals"), 
                    each=len))
    Actual_Arrivals <- A
    Model_Arrivals <- PI
    Date <- Date
    wide <- cbind(Date = format(Date,"%Y %b"),
                  Actual_Arrivals = as.integer(Actual_Arrivals), 
                  Model_Arrivals =as.integer(Model_Arrivals))
    list(long=long, wide=wide)
    
    
  })
  
  #Create a datatable with all the values from the inputs
  output$datatable <- renderTable({
    Tdata <- cbind(pred_data()[["wide"]])
    Tdata <- cbind(Tdata)
    Tdata[seq((nrow(Tdata)-12), nrow(Tdata), length.out=13),]
  })
  
  
  }

)

