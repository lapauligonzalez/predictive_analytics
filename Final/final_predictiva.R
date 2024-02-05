library(vcd)
library(gridExtra)
library(tidyverse)
library(ggplot2)
library(corrplot)
library(scales)
library(glue)
library(GGally)
library(ggridges)
library(plotly)
library(factoextra)
library(amap)
library(sf)
library(igraph)

# ---- Analisis Exploratorio
dataset <- read.csv("C:/Users/paula/OneDrive/Documentos/Facultad/Análisis Predictivo/dataset.csv")
datosFiltrados <- dataset

summary(datosFiltrados)
datosNumericos <- datosFiltrados %>% select(popularity, duration_ms, danceability,
                                            energy, loudness, speechiness,
                                            acousticness, instrumentalness, liveness,
                                            valence, tempo)

# Chequeo de NAs
colnames(datosFiltrados)[apply(datosFiltrados, 2, anyNA)] # Hay 3 columnas que tienen datos faltantes
sum(apply(datosFiltrados, 1, function(x) sum(is.na(x)))) # Solo son 3 registros los que tienen faltantes
datosFiltrados <- na.omit(datosFiltrados) # Borro los 3 registros con NAs

dat_c <- datosNumericos %>%
  select_if(is.numeric) %>%
  scale()

dat_c <- as.data.frame(dat_c)

#Graficos de densidad de las variables numericas
plots <- lapply(names(dat_c), function(colname) { # Crea graficos de densidad 
  ggplot(data = dat_c, aes_string(x = colname)) +
    geom_density(fill = "green3", alpha = 0.5) +
    labs(title = paste("Grafico de Densidad de", colname)) +
    theme(text = element_text(family = "mono"),
          plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
          panel.background = element_rect(fill = '#F0F0F0', color = 'grey'),
          panel.grid.major = element_line(color = 'grey'),
          panel.grid.minor = element_line(color = 'grey'))
})
grid.arrange(grobs = plots, ncol = 2) # Organiza los graficos en una cuadricula utilizando gridExtra

# Análisis de outliers
hacer_boxplot <- function(variable, titulo, colores){
  ggplot(variable, aes(x = reorder(Variable, Valor, na.rm=T),y = Valor)) +
    geom_boxplot(fill = colores,size = 0.7) +
    labs(title = titulo, x = "Variables", y = "Valor") +
    theme(text = element_text(family = "mono"),
          plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
          panel.background = element_rect(fill = '#F0F0F0', color = 'grey'),
          panel.grid.major = element_line(color = 'grey'),
          panel.grid.minor = element_line(color = 'grey'))+
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
}

# Variables numéricas con rango de [0,1]
variables_numericas <- datosFiltrados %>% select(all_of(c("acousticness", "instrumentalness", "liveness", "valence","speechiness", "danceability", "energy" ))) #Sólo uso las numéricas
variables_boxplot <- tidyr::gather(variables_numericas, Variable, Valor) #Las meto en un df con 2 columnas
colores <- colorRampPalette(c("green", "black"))(7) #Creo la paleta de colores 
hacer_boxplot(variables_boxplot, "Boxplots de las variables numéricas con rango [0,1]", colores)

# Variables numéricas rango fuera del [0,1]
variables_numericas_2 <- datosFiltrados %>% select(popularity, loudness, tempo)
variables_boxplot_2 <- tidyr::gather(variables_numericas_2, Variable, Valor)
colores_2 <- colorRampPalette(c("green", "black"))(3) 
hacer_boxplot(variables_boxplot_2, "Boxplots de las variables numéricas con rango fuera de [0,1]", colores_2)

# Duration_ms en un boxplot aparte porque tenia otro rango 
ggplot(datosFiltrados, aes(y = duration_ms)) +
  geom_boxplot(fill = "#0B5015",size = 0.7) +
  labs(title = "Boxplot de duration_ms", x = "Variable", y = "Valor") +
  theme(text = element_text(family = "mono"),
        plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
        panel.background = element_rect(fill = '#F0F0F0', color = 'grey'),
        panel.grid.major = element_line(color = 'grey'),
        panel.grid.minor = element_line(color = 'grey'))+
  scale_y_continuous(labels = comma_format())

# Matriz de correlaciones
datosNumericos <- dat_c %>% select(popularity, duration_ms, danceability,
                                   energy, loudness, speechiness,
                                   acousticness, instrumentalness, liveness, 
                                   valence, tempo)
M1 <- cor(datosNumericos)
ggplot(data = as.data.frame(as.table(M1)),
       aes(x = Var1, y = Var2, fill = Freq)) +
  geom_tile() +
  geom_text(aes(label = round(Freq, 2)), vjust = 1) +
  scale_fill_gradient2(low = "#0B5015", mid = "white", high = "green3",
                       midpoint = 0) +
  theme_minimal() +
  coord_fixed() +
  labs(title = "Matriz de Correlación") +
  theme(text = element_text(family = "mono"),
        plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
        panel.background = element_rect(fill = '#F0F0F0', color = 'grey'),
        panel.grid.major = element_line(color = 'grey'),
        panel.grid.minor = element_line(color = 'grey'),
        axis.text.x = element_text(angle = 45, hjust = 1, face = "bold"), 
        axis.text.y = element_text(angle = 45, hjust= 1, face = "bold"))

#Pie chart para ver la cantidad de explicit-vs-no explicit 

datosFiltrados$explicit_numeric <- as.integer(as.logical(datosFiltrados$explicit)) # Crear una copia de la columna "explicit" como valores numericos (1 para True, 0 para False)
resumen_explicit <- data.frame(
  Categoria = c("Explicit", "No explicit"),
  Frecuencia = c(sum(datosFiltrados$explicit_numeric == 1), sum(datosFiltrados$explicit_numeric == 0))
)
total <- sum(resumen_explicit$Frecuencia)
resumen_explicit$Porcentaje <- (resumen_explicit$Frecuencia / total) * 100

library(ggplot2)
ggplot(resumen_explicit, aes(x = "", y = Frecuencia, fill = Categoria)) +
  geom_bar(stat = "identity", width = 1) +
  geom_text(aes(label = paste(round(Porcentaje, 2), "%")), position = position_stack(vjust = 0.5)) +
  coord_polar(theta = "y") +
  scale_fill_manual(values = c("darkgreen", "lightgreen")) + 
  labs(title = "Proporción de canciones explícitas y no explícitas") +
  theme(text = element_text(family = "mono"),
        plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
        panel.background = element_rect(fill = '#F0F0F0', color = 'grey'),
        panel.grid.major = element_line(color = 'grey'),
        panel.grid.minor = element_line(color = 'grey'))

#Asociacion de explicit con variables numéricas

norm_completa= dat_c %>% mutate(datosFiltrados$explicit,datosFiltrados$time_signature,datosFiltrados$key,datosFiltrados$mode,datosFiltrados$track_genre)

canciones_explicitas <- norm_completa[norm_completa$"datosFiltrados$explicit" == TRUE, ] # Filtrar las canciones explicitas
canciones_no_explicitas <- norm_completa[norm_completa$"datosFiltrados$explicit" == FALSE, ] # Filtrar canciones no explicitas

#energy
t.test(canciones_explicitas$energy, canciones_no_explicitas$energy)
#tempo
t.test(canciones_explicitas$tempo, canciones_no_explicitas$tempo)
#loudness
t.test(canciones_explicitas$loudness, canciones_no_explicitas$loudness)
#popularity
t.test(canciones_explicitas$popularity, canciones_no_explicitas$popularity)
#speechiness
t.test(canciones_explicitas$speechiness, canciones_no_explicitas$speechiness)
#duration
t.test(canciones_explicitas$duration_ms, canciones_no_explicitas$duration_ms)
#liveness
t.test(canciones_explicitas$liveness, canciones_no_explicitas$liveness)
#valence
t.test(canciones_explicitas$valence, canciones_no_explicitas$valence)
#acousticness
t.test(canciones_explicitas$acousticness, canciones_no_explicitas$acousticness)
#danceability
t.test(canciones_explicitas$danceability, canciones_no_explicitas$danceability)
#instrumentalness
t.test(canciones_explicitas$instrumentalness, canciones_no_explicitas$instrumentalness)

# Analizando a Explicit en relacion a Energy y Loudness
ggplot(norm_completa, aes(x = loudness, y = energy, color = datosFiltrados$explicit)) +
  geom_point(alpha = 0.3) +
  labs(x = "Loudness", y = "Energy", title = "Relacion entre Energy, Loudness y Explicit") +
  scale_color_manual(values = c("green3", "black"), labels = c("No Explicit", "Explicit")) +
  geom_smooth(method = "lm", formula = y ~ x, se = FALSE) +
  theme(text = element_text(family = "mono"),
        plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
        panel.background = element_rect(fill = '#F0F0F0', color = 'grey'),
        panel.grid.major = element_line(color = 'grey'),
        panel.grid.minor = element_line(color = 'grey'),
        axis.text.x = element_text(hjust = 1, face = "bold"), 
        axis.text.y = element_text(hjust= 1, face = "bold")) 

# Analizando a Explicit en relacion a Tempo y Loudness
ggplot(norm_completa, aes(x = tempo, y = loudness, color = datosFiltrados$explicit)) +
  geom_point(alpha = 0.3) +
  labs(x = "Tempo", y = "Loudness", title = "Relacion entre Tempo, Loudness y Explicit") +
  scale_color_manual(values = c("green3", "black"), labels = c("No Explicit", "Explicit")) +
  geom_smooth(aes(group = datosFiltrados$explicit), method = "lm", se = FALSE) +
  theme(text = element_text(family = "mono"),
        plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
        panel.background = element_rect(fill = '#F0F0F0', color = 'grey'),
        panel.grid.major = element_line(color = 'grey'),
        panel.grid.minor = element_line(color = 'grey'),
        axis.text.x = element_text(hjust = 1, face = "bold"), 
        axis.text.y = element_text(hjust= 1, face = "bold"))

#Calcular valores de V de Cramer
#explicit-genero
tabla_contingencia <- table(norm_completa$`datosFiltrados$track_genre`, norm_completa$`datosFiltrados$explicit`) # Crear una tabla de contingencia entre las variables track_genre y explicit
resultado_cramer <- assocstats(tabla_contingencia) # Calcular el coeficiente V de Cramer
sqrt(resultado_cramer$chisq / (sum(tabla_contingencia) * (min(nrow(tabla_contingencia), ncol(tabla_contingencia)) - 1)))

#explicit-key
tabla_contingenciaII <- table(norm_completa$`datosFiltrados$key`, norm_completa$`datosFiltrados$explicit`) # Crear una tabla de contingencia entre las variables key y explicit
resultado_cramerII <- assocstats(tabla_contingenciaII)
sqrt(resultado_cramerII$chisq / (sum(tabla_contingenciaII) * (min(nrow(tabla_contingenciaII), ncol(tabla_contingenciaII)) - 1)))

#explicit-mode
tabla_contingenciaIII <- table(norm_completa$`datosFiltrados$mode`, norm_completa$`datosFiltrados$explicit`)# Crear una tabla de contingencia entre las variables key y explicit
resultado_cramerIII <- assocstats(tabla_contingenciaIII)
sqrt(resultado_cramerIII$chisq / (sum(tabla_contingenciaIII) * (min(nrow(tabla_contingenciaIII), ncol(tabla_contingenciaIII)) - 1)))

#explicit-time signature
tabla_contingenciaIV <- table(norm_completa$`datosFiltrados$time_signature`, norm_completa$`datosFiltrados$explicit`) # Crear una tabla de contingencia entre las variables key y explicit
resultado_cramerIV <- assocstats(tabla_contingenciaIV)
sqrt(resultado_cramerIV$chisq / (sum(tabla_contingenciaIV) * (min(nrow(tabla_contingenciaIV), ncol(tabla_contingenciaIV)) - 1)))
