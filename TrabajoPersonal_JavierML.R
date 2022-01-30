

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

fly$relacion <-  (fly$dep_time_min - fly$sched_dep_time_min - fly$dep_delay)
table(fly$relacion)


#ejer16

cancelado_por_dia <- 
  fly %>%
  mutate(cancelado = (is.na(arr_delay) | is.na(dep_delay))) %>%
  group_by(year, month, day) %>%
  summarise(
    cancelado_num = sum(cancelado),
    flights_num = n(),
  )

ggplot(cancelado_por_dia) +
  geom_point(aes(x = flights_num, y = cancelado_num))


#ejer17

cancelados_y_retrasos <- 
  fly %>%
  mutate(cancelado = (is.na(arr_delay) | is.na(dep_delay))) %>%
  group_by(year, month, day) %>%
  summarise(
    prop_cancelado = mean(cancelado),
    media_dep_delay = mean(dep_delay, na.rm = TRUE),
    media_arr_delay = mean(arr_delay, na.rm = TRUE)
  ) %>%
  ungroup()

ggplot(cancelados_y_retrasos) +
  geom_point(aes(x = media_dep_delay, y = prop_cancelado))

ggplot(cancelados_y_retrasos) +
  geom_point(aes(x = media_arr_delay, y = prop_cancelado))

# E18 MIRAR CON JAVI

cancelados_y_retrasos_A <- 
  fly %>%
  mutate(cancelado = (is.na(arr_delay) | is.na(dep_delay))) %>%
  group_by(origin, dest) %>%
  summarise(
    prop_cancelado = mean(cancelado),
    media_dep_delay = mean(dep_delay, na.rm = TRUE),
    media_arr_delay = mean(arr_delay, na.rm = TRUE)
  ) %>%
  ungroup()

ggplot(cancelados_y_retrasos_A) +
  geom_point(aes(x = media_dep_delay, y = prop_cancelado))

ggplot(cancelados_y_retrasos_A) +
  geom_point(aes(x = media_arr_delay, y = prop_cancelado))

#ejer19

fly %>%
  group_by(carrier) %>%
  summarise(arr_delay = mean(arr_delay, na.rm = TRUE)) %>%
  arrange(desc(arr_delay))

fly %>%
  group_by(carrier) %>%
  summarise(dep_delay = mean(dep_delay, na.rm = TRUE)) %>%
  arrange(desc(dep_delay))


#ejer20

fly %>%
  group_by(hour) %>%
  summarise(dep_delay = mean(dep_delay, na.rm = TRUE)) %>%
  arrange(dep_delay)


# E21
make_datetime_100 <- function(year, month, day, time) 
{ 
  make_datetime(year, month, day, time %/% 100, time %% 100) 
}

flights_dt <- fly %>% 
  filter(!is.na(dep_time), !is.na(arr_time)) %>% 
  mutate( dep_time = make_datetime_100(year, month, day, dep_time), 
          arr_time = make_datetime_100(year, month, day, arr_time), 
          sched_dep_time = make_datetime_100(year, month, day, sched_dep_time), 
          sched_arr_time = make_datetime_100(year, month, day, sched_arr_time) ) %>% 
  select(origin, dest, ends_with("delay"), ends_with("time"))

flights_dt2<- flights_dt %>%
  mutate(dow = wday(sched_dep_time)) %>%
  group_by(dow) %>%
  summarise(
    dep_delay = mean(dep_delay),
    arr_delay = mean(arr_delay, na.rm = TRUE)
  ) %>%
  print(n = Inf)

flights_dt %>%
  mutate(wday = wday(dep_time, label = TRUE)) %>% #porque de dep_time
  group_by(wday) %>% 
  summarize(ave_dep_delay = mean(dep_delay, na.rm = TRUE)) %>% 
  ggplot(aes(x = wday, y = ave_dep_delay)) + 
  geom_bar(stat = "identity") 


# E22
retraso_acum<- fly %>% 
  filter(arr_delay > 0) %>% 
  group_by(dest) %>% 
  summarise(total_delay= sum(arr_delay))%>% 
  arrange(total_delay)

retraso_acum





# E23

suma_retrasos <-sum(retraso_acum$total_delay)
proporcion_retrasos <- retraso_acum$total_delay / suma_retrasos

retrasos_prop <- data.frame(retraso_acum$dest , proporcion_retrasos)

retrasos_prop




#retraso_acum2<- vuelos %>% 
filter(arr_delay > 0) %>% 
  group_by(dest, origin, carrier, flight) %>% 
  summarise(arr_delay = sum(arr_delay)) %>% 
  group_by(dest) %>% 
  mutate( arr_delay_prop = arr_delay / sum(arr_delay) ) %>% 
  arrange(dest, desc(arr_delay_prop)) %>% 
  select(carrier, flight, origin, dest, arr_delay_prop)


# 24
library(lubridate)
flights_dt %>% 
  mutate(sched_dep_hour = hour(sched_dep_time)) %>% #ERROR!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  group_by(sched_dep_hour) %>% 
  summarise(dep_delay = mean(dep_delay)) %>% 
  ggplot(aes(y = dep_delay, x = sched_dep_hour)) + 
  geom_point() + geom_smooth()



#ejer26
sessionInfo()





















