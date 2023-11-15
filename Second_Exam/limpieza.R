install.packages("writexl")
library(writexl)
library(dplyr)
library(stringr)


#Elimino las columnas que no quiero del ORIGEN
data <- origen %>% select(-X, -video, -ordering, -attributes, -isOriginalTitle, -language, -tagline, -revenue, -budget, -runtime, -status)
data <-data.frame(data)

#Elimino las columnas que no quiero de TESTEAR
data2 <- testear %>% select(-X, -video, -ordering, -attributes, -isOriginalTitle, -language, -tagline, -revenue, -budget, -runtime, -status)
data2 <-data.frame(data2)
  
#Cuento la cantidad de escritores y directores y los pongo en una nueva columna, yo borro los writers y directors
data$cantidad_writers <- str_count(data$writers, ',') + 1
data$cantidad_directors <- str_count(data$directors, ',') + 1
data2$cantidad_writers <- str_count(data2$writers, ',') + 1
data2$cantidad_directors <- str_count(data2$directors, ',') + 1

frec_directors <- table(data$directors)
data$frec_directors <- frec_directors[data$directors] / sum(frec_directors)
data2$frec_directors <- frec_directors[data2$directors] / sum(frec_directors)#Agrego estos datos en dataset de testear
m6 <- mean(data$frec_directors)
data2$frec_directors <- ifelse(is.na(data2$frec_directors), m6, data2$frec_directors)

data <- data %>% select(-directors, -writers)
data <- data %>% select(-adult) #borro adult porque hay dos columnas
data2 <- data2 %>% select(-directors, -writers)
data2 <- data2 %>% select(-adult) #borro adult porque hay dos columnas

#Hago frecuency encoder para titletype
frec_titleType <- table(data$titleType)
data$frec_titleType <- frec_titleType[data$titleType] / sum(frec_titleType)
data2$frec_titleType <- frec_titleType[data2$titleType] / sum(frec_titleType) #Agrego estos datos en dataset de testear
data <- data %>% select(-titleType)
data2 <- data2 %>% select(-titleType)

#Hago frecuency encoder para genre_x y cuento la cantidad de generos que tiene 1 peli
frec_genres_x <- table(data$genres_x)
data$frec_genres_x <- frec_genres_x[data$genres_x] / sum(frec_genres_x)
data$cantidad_generos <- str_count(data$genres_x, ',') + 1
data2$cantidad_generos <- str_count(data2$genres_x, ",") + 1

m7 <- mean(data$frec_genres_x, na.rm = TRUE)
data$frec_genres_x <- ifelse(is.na(data$frec_genres_x), m7, data$frec_genres_x)
data2$frec_genres_x <- frec_genres_x[data2$genres_x] / sum(frec_genres_x) #Agrego estos datos en dataset de testear
data2$frec_genres_x <- ifelse(is.na(data2$frec_genres_x), m7, data2$frec_genres_x)

data <- data %>% select(-genres_x)
data2 <- data2 %>% select(-genres_x)

#genres_y lo vuelo porque no aporta nada
data <- data %>% select(-genres_y)
data2 <- data2 %>% select(-genres_y)

#runtimeMinutes cambio los ceros por la media
data$runtimeMinutes[data$runtimeMinutes == 0] <- mean(data$runtimeMinutes, na.rm = TRUE)
data2$runtimeMinutes[data2$runtimeMinutes == 0] <- mean(data2$runtimeMinutes, na.rm = TRUE)

#seasonNumber, episodeNumber y popularity cambiar los NAs a 0
m1 <- mean(data$seasonNumber, na.rm = TRUE)
m2 <- mean(data$episodeNumber, na.rm = TRUE)
m3 <- mean(data$popularity, na.rm = TRUE)
data <- data %>% mutate(seasonNumber = ifelse(is.na(seasonNumber), m1, seasonNumber))
data <- data %>% mutate(episodeNumber = ifelse(is.na(episodeNumber), m2, episodeNumber))
data <- data %>% mutate(popularity = ifelse(is.na(popularity), m3, popularity))
data2 <- data2 %>% mutate(seasonNumber = ifelse(is.na(seasonNumber), m1 , seasonNumber))
data2 <- data2 %>% mutate(episodeNumber = ifelse(is.na(episodeNumber), m2, episodeNumber))
data2 <- data2 %>% mutate(popularity = ifelse(is.na(popularity), m3, popularity))

#Production_companies y production_countries
data$production_companies <- ifelse(data$production_companies == "[]", "", data$production_companies)
data$production_countries <- ifelse(data$production_countries == "[]", "", data$production_countries)
data2$production_companies <- ifelse(data2$production_companies == "[]", "", data2$production_companies)
data2$production_countries <- ifelse(data2$production_countries == "[]", "", data2$production_countries)

#hago frecuency encoder para original_language
frec_ori_language <- table(data$original_language)
data$frec_ori_language <- frec_ori_language[data$original_language] / sum(frec_ori_language)
m4 <- mean(data$frec_ori_language, na.rm = TRUE)

data <- data %>% mutate(frec_ori_language = ifelse(is.na(frec_ori_language), m4, frec_ori_language))
data2$frec_ori_language <- frec_ori_language[data2$original_language] / sum(frec_ori_language) #Agrego estos datos en dataset de testear
data2 <- data2 %>% mutate(frec_ori_language = ifelse(is.na(frec_ori_language), m4, frec_ori_language))

data <- data %>% select(-original_language)
data2 <- data2 %>% select(-original_language)

#Trabajo las companies
frec_companies <- table(data$production_companies)
data$frec_companies <- frec_companies[data$production_companies] / sum(frec_companies)
m5 <- mean(data$frec_companies, na.rm = TRUE)

data <- data %>% mutate(frec_companies = ifelse(is.na(frec_companies), m5, frec_companies))
data2$frec_companies <- frec_companies[data2$production_companies] / sum(frec_companies) #Agrego estos datos en dataset de testear
data2 <- data2 %>% mutate(frec_companies = ifelse(is.na(frec_companies), m5, frec_companies))

data <- data %>% select(-production_companies)
data2 <- data2 %>% select(-production_companies)

#Trabajo las countries
frec_countries <- table(data$production_countries)
data$frec_countries <- frec_countries[data$production_countries] / sum(frec_countries)
m6 <- mean(data$frec_countries, na.rm = TRUE)

data <- data %>% mutate(frec_countries = ifelse(is.na(frec_countries), m6, frec_countries))
data2$frec_countries <- frec_countries[data2$production_countries] / sum(frec_countries) #Agrego estos datos en dataset de testear
data2 <- data2 %>% mutate(frec_countries = ifelse(is.na(frec_countries), m6, frec_countries))

data <- data %>% select(-production_countries)
data2 <- data2 %>% select(-production_countries)

#Agrego una nueva columna
data$duracion <- data$endYear - data$startYear
data$duracion <- ifelse(data$duracion < 0, 0, data$duracion)
data2$duracion <- data2$endYear - data2$startYear
data2$duracion <- ifelse(data2$duracion < 0, 0, data2$duracion)

#Exporto ORIGEN_LIMPIO
write_xlsx(data, path = "C:/Users/paula/OneDrive/Documentos/Facultad/Análisis Predictivo/segundo_parcial/origen_limpio.xlsx")

#Exporto ORIGEN_LIMPIO
write_xlsx(data2, path = "C:/Users/paula/OneDrive/Documentos/Facultad/Análisis Predictivo/segundo_parcial/testear_limpio.xlsx")

anyNA(data)
anyNA(data2)
sapply(data2, function(x) any(is.na(x)))
