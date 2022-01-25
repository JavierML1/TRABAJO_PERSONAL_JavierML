

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


ejer8 <- fly[is.na(fly$dep_time), ]
dim(ejer8)
print("8255 vuelos tienen un valor desconocido de dep_time")



#ejer9
apply(X = is.na(fly), MARGIN = 2, FUN = sum)


ejer10.1 <- fly[order(fly$dep_delay, na.last=FALSE), ]
tail(ejer10.1)

ejer10.2 <- fly[order(fly$dep_delay), ]
head(ejer10.2)




fly$vel_med <- (fly$distance/fly$air_time)
ejer11 <- fly[order(fly$vel_med, decreasing=TRUE), ]
head(ejer11)


ejer12 <- fly[order(fly$distance, decreasing=TRUE), ]
head(ejer12)


ejer13 <- fly[order(fly$distance), ]
head(ejer13)


fly$dep_time_min <- (fly$dep_time %/% 100 * 60 + fly$dep_time %% 100)

fly$sched_dep_time_min <- (fly$sched_dep_time %/% 100 * 60 + fly$sched_dep_time %% 100)


#ejer15

fly$resta <- (abs(fly$dep_time_min - fly$sched_dep_time_min) - abs(fly$dep_delay))
table(fly$resta)


fly$dep_delay <- (abs(fly$dep_time_min - fly$sched_dep_time_min))
fly$resta <- (abs(fly$dep_time_min - fly$sched_dep_time_min) - abs(fly$dep_delay))
table(fly$resta)
