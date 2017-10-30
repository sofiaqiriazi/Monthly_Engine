A <- deps.long[(t-13),"Galgaduud_Departures"]
B <- 2.26751515362942*deps.long[(t-12),"Bari_Departures"]
D <-  0.00588088839695529*arrs.long[(t-1),"Mudug_Arrival"]*arrs.long[(t-1),"Woqooyi_Galbeed_Arrival"]

CA<- median(rain.long[(t-4):(t-1),"Banaadir_rain"])
CB<- mean(arrs.long[(t-4):(t-1),"Sool_Arrival"])

if(CA > 0 ){

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

#######################################################################################

A <- deps.long[(t-13),"Galgaduud_Departures"]
B <- 2.27391080050669*deps.long[(t-12),"Bari_Departures"]
C <- 0.00588055939993242*arrs.long[(t-1),"Mudug_Arrival"]*arrs.long[(t-1),"Woqooyi_Galbeed_Arrival"]
D <- mean(rain.long[(t-4):(t-1),"Banaadir_rain"])
E <- median(arrs.long[(t-4):(t-1),"Sool_Arrival"])
G <- 5.95887287478984*deps.long[(t-3),"Bari_Departures"]
H <- deps.long[(t-4),"Sanaag_Departures"]*rain.long[(t-5),"Hiiraan_rain"]
K <- 2.29816996652923*arrs.long[(t-1),"Sanaag_Arrival"]


X <- sum(311.894138182276,K,na.rm=TRUE)
if(is.na(D) || D >0 ){Q <- E}
else{Q <- -5716}

Y <- sum(B, C, if(D, E, -5716), max(G, H,na.rm=TRUE),na.rm =TRUE)

W <- max(Y, X,na.rm=TRUE)
FIN <- max(A, W,na.rm=TRUE)

############################################################################################
A <- 7.09622785331628*deps.long[(t-3),"Bari_Departures"]
B <- 2.43787508914871*delay(Bari_Departures, 12)
C <- 0.00601323333840485*delay(Mudug_Arrival, 1)*delay(Woqooyi_Galbeed_Arrival, 1)
D <- delay(Sanaag_Departures, 4)*delay(Hiiraan_rain, 5)
E <- delay(Galgaduud_Departures, 13)
G <- smm(delay(Banaadir_rain, 1), 3)
H <- 5.95351409100946*delay(Bari_Departures, 4)
X <- 2.46409172818795*delay(Sanaag_Arrival, 1)

if (is.na(G) || G>0)
{Y<- H}
else
{ Y <- -3205.61372701243}

FIN <- max(A, 0.93871317992841*max(sum(B , C , D , E , Y , na.rm=TRUE), X, na.rm=TRUE),na.rm=TRUE)

###############################################################################################
X <- mean(rain.long[(t-7):(t-1),"Banaadir_rain"])
Y <- deps.long[(t-4),"Bari_Departures"]*media(rain.long[(t-6):(t-1),"Hiiraan_rain"])

if(is.na(X) || X>0){A<- Y}

else{A<- -2915}

B <- 2.98981569846976*deps.long[(t-12,"Bari_Departures")]
C <- 1.90068199745955*deps.long[(t-14),"Bari_Departures"]
D <- 0.005464125090538*arrs.long[(t-1),"Mudug_Arrival"]*arrs.long[(t-1),"Woqooyi_Galbeed_Arrival"]
E <- 7.08510220750896*deps.long[(t-3),"Bari_Departures"]
Z <- deps.long[(t-13),"Galgaduud_Departures"]
T <- 2.31719227653268*arrs.long[(t-1),"Sanaag_Arrival"]
FIN <- max(sum(A,max(sum(B,C,D,na.rm=TRUE), sum(E,Z,na.rm=TRUE),na.rm=TRUE),na.rm=TRUE), T,na.rm=TRUE)
######################################################################################################### 
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

##########################################################################################################

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
#############################################################################################################
A <- arrs.long[(t-1),"Bari_Arrival"]
B <- median(rain.long[(t-4):(t-1),"Banaadir_rain"])
C <- mean(arrs.long[(t-3):(t-1),"Sool_Arrival"])

if(is.na(B) || B>0){
	D <- C
}
else{
	D <- -5711.49956439839
}

E <- 0.00577144170042261*arrs.long[(t-1),"Mudug_Arrival"]*arrs.long[(t-1),"Woqooyi_Galbeed_Arrival"]
G <- 1.73906550532281*deps.long[(t-13),"Galgaduud_Departures"]
KA <- median(rain.long[(t-4):(t-1),"Banaadir_rain"])
KB <- 5.91536930063778*deps.long[(t-3),"Bari_Departures"]
KC <- deps.long[(t-4),"Sanaag_Departures"]*rain.long[(t-5),"Hiiraan_rain"]

if (is.na(KA) || KA >0){
	K<-KB
}
else{
	K<-KC
}

LA <- median(rain.long[(t-14):(t-1),"Sool_rain"])
LB <- 2.31175490528636*arrs.long[(t-1),"Sanaag_Arrival"]
LC <- rivers.long[(t-5),"Juba_River_discharge"]


if(is.na(LA) || LA>0 ){
	L<- LB
}
else{
	L<- LC
}

FIN <- max(sum(A, D, max(E, G, na.rm=TRUE), K, na.rm=TRUE), L, na.rm=TRUE)

###############################################################################################################
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

###############################################################################################################
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
	EA <- EB
}
else{
	EA <- EC
}

G <- 2.31223534024849*arrs.long[(t-1),"Sanaag_Arrival"]

FIN <- max(426.077738789064, max(sum(A, B, max(C, D,na.rm=TRUE), E,na.rm=TRUE), G), na.rm=TRUE)

##################################################################################################
A <- 0.797107265273338*arrs.long[(t-1),"Sool_Arrival"]
B <- 0.612331581418625*arrs.long[(t-13),"Mudug_Arrival"]
C <- arrs.long[(t-1),"Sool_Arrival"]*tan(arrs.long[(t-1),"Awdal_Arrival"])
D <- 0.0278892569471559*deps.long[(t-1),"Sanaag_Departures"]*arrs.long[(t-1),"Awdal_Arrival"]
E <- 0.00505997610411542*arrs.long[(t-1),"Mudug_Arrival"]*arrs.long[(t-1),"Woqooyi_Galbeed_Arrival"]
G <- deps.long[(t-14),"Sool_Departures"]
H <- -1*arrs.long[(t-1),"Awdal_Arrival"]
J <- -1*arrs.long[(t-4),"Jubbada_Dhexe_Arrival"]
K <- tan(0.993604035057624*mean(fatalities.long[(t-17):(t-16),"Nugaal_Fatalities"])
L <- sum (A,B,C,D,E,G,H,J,na.rm=TRUE)

FIN <- max(L, K,na.rm=TRUE)

###############################################################################################################

A <- deps.long[(t-1),"Jubbada_Dhexe_Departures"]
B <- mean(conflict.long[(t-16):(t-1),"Galgaduud_Conflict"])

#Returns 1 if x <= y, 0 otherwise
CA <- mean(fatalities.long[(t-4):(t-1),"Bay_Fatalities"])
CB <- mean(conflict.long[(t-4):(t-1),"Togdheer_Conflict"])
if (CA<=CB){C<-1}
else{C<-0}

D <- mean(arrs.long[(t-3):(t-1),"Bari_Arrival"])

E <- max(conflict.long[(t-1),"Bay_Conflict"]^2, fatalities.long[(t-1),"Sool_Fatalities"]*deps.long[(t-1),"Togdheer_Departures"],na.rm=TRUE)
GAA <- conflict.long[(t-1),"Bay_Conflict"]
GAB <- 2.01083064621521*arrs.long[(t-1),"Sanaag_Arrival"] 
if(GAA, GAB, 1595.33850434883)
if(is.na(GAA) || GAA >0){
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

#######################################################################################################
A <- arrs.long[(t-1),"Nugaal_Arrival"]
B <- 361
C <- conflicts.long[(t-1),"Bay_Conflict"]^2
D <- 0.00594668706750925*arrs.long[(t-1),"Mudug_Arrival"]*arrs.long[(t-1),"Woqooyi_Galbeed_Arrival"]
E <- fatalities.long[(t-1),"Sool_Fatalities"]
G <- 3.20250999835703*mean(conflicts.long[(t-8):(t-1),"Gedo_Conflict"])*mean(deps.long[(t-8):(t-1),"Togdheer_Departures"])
H <- arrs.long[(t-1),"Nugaal_Arrival"]
I <- if(E, G, H)
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

Bay_Arrival = max(max(A, max(B, max(max(C,N), 2.31215780686505*delay(Sanaag_Arrival, 1)))), delay(Sool_WaterDrumPrice, 1) - 146858.497640287)

#####################
Bay_Arrival = max(386, max(delay(Galgaduud_Departures, 13), max(2.26751515362942*delay(Bari_Departures, 12) + 0.00588088839695529*delay(Mudug_Arrival, 1)*delay(Woqooyi_Galbeed_Arrival, 1) + if(smm(delay(Banaadir_rain, 1), 3), mma(delay(Sool_Arrival, 1), 3), -5714) + max(5.97879951112646*delay(Bari_Departures, 3), delay(Sanaag_Departures, 4)*delay(Hiiraan_rain, 5)), 2.31702827594048*delay(Sanaag_Arrival, 1))))
##########################
##########################
##########################
##########################
##########################
##########################
A <- stations.long[(t-2),"Hiiraan_Belet_WeyneStation_Shabelle_River"]
B <- -1*asinh(median(stations.long[(t-5):(t-2),"Hiiraan_Belet_WeyneStation_Shabelle_River"]))
CA <- 1.79605197655313
C <- 1392.18823411085*median(rain.long[(t-5):(t-2),"Bari_rain"]) %% rain.long[(t-6),"Bakool_rain"]
D <- -1*stations.long[(t-2),"Hiiraan_Belet_WeyneStation_Shabelle_River"]
AA <- deps.long[(t-13),"Galgaduud_Departures"]
AB <- 2.26751515362942*deps.long[(t-12),"Bari_Departures"]
AC <- 0.00588088839695529*arrs.long[(t-1),"Mudug_Arrival"]*arrs.long[(t-1),"Woqooyi_Galbeed_Arrival"]
BA <- median(rain.long[(t-4):(t-1),"Banaadir_rain"])
BB <- mean(arrs.long[(t-4):(t-1),"Sool_Arrival"])
CA <- 5.97879951112646*deps.long[(t-3),"Bari_Departures"]
CB <- deps.long[(t-4),"Sanaag_Departures"]*rain.long[(t-5),"Hiiraan_rain"]
DA <- 2.31702827594048*arrs.long[(t-1),"Sanaag_Arrival"]
E <- stations.long[(t-2),"Hiiraan_Belet_WeyneStation_Shabelle_River"]
EA <- factorial(E)
EB <- cosh(EA)
FA <- deps.long[(t-13),"Galgaduud_Departures"]
FB <- 2.26751515362942*deps.long[(t-12),"Bari_Departures"]
FC <- 0.00588088839695529*arrs.long[(t-1),"Mudug_Arrival"]*arrs.long[(t-1),"Woqooyi_Galbeed_Arrival"]
FD <- median(rain.long[(t-4):(t-1),"Banaadir_rain"])
FE <- mean(arrs.long[(t-4):(t-1),"Sool_Arrival"])
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