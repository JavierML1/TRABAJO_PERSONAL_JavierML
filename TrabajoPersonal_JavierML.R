

install.packages("nycflights13")

fly<- nycflights13::flights


tarde <- fly[which(fly$arr_delay>60),]

dim(tarde)

ejer2<- fly[fly$dest == "SFO" | fly$dest == "OAK", ]
dim(ejer2)
print("13643 vuelos volaron hacia San Francisco")

ejer3<- fly[fly$carrier == "UA" | fly$carrier == "AA", ]
dim(ejer3)
print("91394 fueron operados por United American o American Airlines")


ejer4 <- fly[fly$month == 4 | fly$month == 5 | fly$month == 6, ]
dim(ejer4)
print("85369 vuelos salieron en primavera")
 
ejer5 <- fly[which(fly$arr_delay>60 & fly$dep_delay<60), ]
dim(ejer5)
print("4956 vuelos llegaron mas de una hora tarde habiendo salido con menos de una hora de retraso")


ejer6 <- fly[which(fly$dep_delay>60 & fly$arr_delay<30), ]
dim(ejer6)
print("En 181 ocasiones el avion salio mas de una hora tarde pero consiguio llegar con menos de 30 minutos de retraso")


ejer7 <- fly[which(fly$hour>=0 & fly$hour<=7), ]
dim(ejer7)
print("50726 vuelos se produjeron entre las medianoche y las 7 de la mañana")


