
A <- fatalities.long[t,"Banaadir_Fatalities"]
B <- median(deps.long[(t-9):t,"Bay_Departures"])
C <- arrs.long[t,"Sool_Arrival"]
D <- arrs.long[t,"Gedo_Arrival"]
E <- 4.76272011396427*arrs.long[t,"Bakool_Arrival"]
Fa <- 2.14648317039228*mean(arrs.long[(t-15):5,"Hiiraan_Arrival"])

G <- -1*water.long[t,"Hiiraan_WaterDrumPrice"]
H <- - 7.61800896236803*deps.long[t,"Nugaal_Departures"]
I <- sum(D, E, Fa, G, H, na.rm=TRUE)
J <- 1.11612628987806*max(C, I,na.rm=TRUE)
K <- max(B,J, na.rm=TRUE)

B_R <- sum(A,K,na.rm=TRUE)



##############################################

A <- arrs.long[t,"Sool_Arrival"]
B <- median(deps.long[(t-9):t,"Bay_Departures"])
C <- 5.12362836857411*arrs.long[t,"Bakool_Arrival"]
D <- 2.36682580358288*mean(arrs.long[(t-15):t,"Hiiraan_Arrival"])
E <- arrs.long[t,"Gedo_Arrival"]
Fa <- -1*water.long[t,"Hiiraan_WaterDrumPrice"]
G <- -8.93031647771045*deps.long[t,"Nugaal_Departures"]

H <- sum(C,D,E,Fa,G,na.rm=TRUE)

B_R <- max(A, max(B, H,na.rm=TRUE), na.rm =TRUE)


Bay_Arrival = max(Sool_Arrival, max(smm(Bay_Departures, 9), 5.12362836857411*Bakool_Arrival + 2.36682580358288*sma(Hiiraan_Arrival, 15) + Gedo_Arrival - Hiiraan_WaterDrumPrice - 8.93031647771045*Nugaal_Departures))

############################################

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


################################################################

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

#########################################################################

A <- 250.733325202228
B <- arrs.long[t,"Sool_Arrival"]
C <- arrs.long[t,"Sanaag_Arrival"]
D <- median(deps.long[(t-9):t,"Bay_Departures"])
E <- - 968.883338991468
Fa <- 6.04816086006533e-5*arrs.long[t,"Gedo_Arrival"]*arrs.long[t,"Shabeellaha_Hoose_Arrival"]
G <- 8.02723957887815e-9*arrs.long[t,"Bakool_Arrival"*arrs.long[t,"Gedo_Arrival"]*arrs.long[t,"Shabeellaha_Hoose_Arrival"]
H <- 0.122774506198443*arrs.long[t,"Bakool_Arrival"]*confilicts.long[t,"Woqooyi_Galbeed_Conflict"]^2
I <- - 256.263235226128*fatalities.long[t,"Gedo_Fatalities"]

J <- sum(C,D,E, na.rm=TRUE)
K <- sum(Fa,G,H,I, na.rm=TRUE)

B_R = sum(A,max(B, max(J, K, na.rm=TRUE),na.rm=TRUE),na.rm=TRUE)

#############################################################################################
A <- fatalities.long[t,"Mudug_Fatalities"]
B <- arrs.long[t,"Sanaag_Arrival"]
C <- median(deps.long[(t-9):t,"Bay_Departures"])
D <- 8.12603260178045e-5*arrs.long[t,"Gedo_Arrival"]*arrs.long[t,"Shabeellaha_Hoose_Arrival"]
E <- 6.19210913635713e-9*arrs.long[t,"Bakool_Arrival"]*arrs.long[t,"Gedo_Arrival"]*arrs.long[t,"Shabeellaha_Hoose_Arrival"]
Fa <- 0.112411952049919*arrs.long[t,"Bakool_Arrival"]*arrs.long[t,"Woqooyi_Galbeed_Conflict"]^2
G <- - 17.9398145837931*fatalities.long[t,"Gedo_Fatalities"]*fatalities.long[t,"Mudug_Fatalities"]


sum1 <- sum(B,C,na.rm=TRUE)
sum2 <- sum(D,E,Fa,G, na.rm=TRUE)
max1 <- max(sum1,sum2)
B_R <- sum(A, max1, na.rm=TRUE)

###############################################################################################
A <- 0.336294386323842*deps.long[t,"Bay_Departures"]
B <- 0.883241683448547*deps.long[t,"Bay_Departures"]*median(fatalities.long[(t-6):t,"Woqooyi_Galbeed_Fatalities"])
C <- 0.000300318923725438*arrs.long[t,"Bakool_Arrival"]*arrs.long[t,"Togdheer_Arrival"]
D <- 5.03074980853227e-5*arrs.long[(t-14),"Mudug_Arrival"]^2
E <- 1.24465096290049*arrs.long[t,"Togdheer_Arrival"]*median(fatalities.long[(t-6):t,"Woqooyi_Galbeed_Fatalities"])^3*median(fatalities.long[(t-13):t,"Jubbada_Dhexe_Fatalities"])^2
Fa <- mean(rivers.long[(t-5):t,"Juba_River_discharge"])
G <- -1*mean(rivers.long[(t-2):t,"Shabelle_River_discharge"])

B_R <- sum(A,B,C,D,E,Fa,G,na.rm=TRUE)

#######################################################################################################
A <- 0.320513983017835*deps.long[t,"Bay_Departures"]
B <- 0.894457297103651*deps.long[t,"Bay_Departures"]*median(fatalities.long[(t-6):t,"Woqooyi_Galbeed_Fatalities"])
C <- 0.000300114103985695*arrs.long[t,"Bakool_Arrival"]*arrs.long[t,"Togdheer_Arrival"]
D <- 5.02315874441267e-5*arrs.long[(t-14),"Mudug_Arrival"]^2
E <- 1.24211263635356*arrs.long[t,"Togdheer_Arrival"]*median(fatalities.long[(t-6):t,"Woqooyi_Galbeed_Fatalities"])^3*median(fatalities.long[(t-13):t,"Jubbada_Dhexe_Fatalities"])^2
Fa <- mean(rivers.long[(t-6):t,"Juba_River_discharge"])
G <- - 97.3156863418184

B_R <- sum( A, B , C , D , E , Fa , G, na.rm=TRUE)

####################################################################################################
A <- 296.475535608648
B <- 0.305268739338313*deps.long[t,"Bay_Departures"]
C <- 0.918562429161476*deps.long[t,"Bay_Departures"]*median(fatalities.long[(t-6):t,"Woqooyi_Galbeed_Fatalities"])
D <- 0.000299985274249788*arrs.long[t,"Bakool_Arrival"]*arrs.long[t,"Togdheer_Arrival"]
E <- 5.01244719322297e-5*arrs.long[(t-14),"Mudug_Arrival"]^2
Fa <- 1.23683041943114*arrs.long[t,"Togdheer_Arrival"]*median(fatalities.long[(t-6):t,"Woqooyi_Galbeed_Fatalities"])^2*median(fatalities.long[(t-13):t,"Jubbada_Dhexe_Fatalities"])

B_R <- sum(A , B , C , D , E , Fa, na.rm=TRUE)

########################################################################################################
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

###################################################################################################################
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

###################################################################################################################
###################################################################################################################
A <- fatalities.long[t,"Banaadir_Fatalities"]
B <- arrs.long[t,"Nugaal_Arrival"]
C <- rivers.long[(t-4),"Juba_River_discharge"]
D <- 0.766608274625863*conflicts.long[t,"Woqooyi_Galbeed_Conflict"]*arrs.long[t,"Bakool_Arrival"]
E <- 7.61315532207373e-5*arrs.long[t,"Gedo_Arrival"]*arrs.long[t,"Shabeellaha_Hoose_Arrival"]
Fa <- -5491.48807365147
G <- 0.346256266902831*deps.long[t,"Bay_Departures"]
S <- arrs.long[t,"Sool_Arrival"]
H <- -181.194263116577
SUM1 <- sum(D , E , Fa, na.rm=TRUE)
MAX1 <- max(SUM1,G, na.rm=TRUE)
SUM2 <- sum(A , B , C , MAX1, na.rm=TRUE)
MAX2 <- max(SUM2, S, na.rm=TRUE)
B_R <- sum(MAX2 , H, na.rm=TRUE)



###################################################################################################################
###################################################################################################################
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