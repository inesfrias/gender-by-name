library(dplyr)
library(data.table)
library(plotly)

setwd ("/Users/inesfrias/Documents/pers_gender")

#------------------------
# PARTE 1 - Punto C
#    Grafico de la cantidad estimada de nombres de varón y nombres de mujer para cada año
#------------------------

pers_hist_full_sexo <- read.csv(file = "./data/pers_hist_full_sexo.csv", header = TRUE, sep = ",") 

years <- distinct(pers_hist_full_sexo, anio)

#------------------------
# Agrupa por genero y año
#------------------------
#test <-group_by(pers_hist_full_sexo, sexo) %>%  summarise(cant = sum(cantidad))  # 32884570
#rm(test)

female  <- filter(pers_hist_full_sexo, sexo == "f") %>% group_by(anio) %>% 
  summarise(cant = sum(cantidad)) %>% select (cant)
male    <- filter(pers_hist_full_sexo, sexo == "m") %>% group_by(anio) %>% 
  summarise(cant = sum(cantidad)) %>% select (cant)

years <- years[['anio']]
female <- female[['cant']]
male <- male[['cant']]

data <- data.frame(years, female, male)
data$years <- factor(years, levels = data[["years"]])

p <- plot_ly(data, x = ~years, y = ~female, type = 'bar', name = 'Femenino', marker = list(color = 'rgb(49,130,189)')) %>%
  add_trace(y = ~male, name = 'Masculino', marker = list(color = 'rgb(204,204,204)')) %>%
  layout(xaxis = list(title = "", tickangle = -45),
         yaxis = list(title = ""),
         margin = list(b = 100),
         barmode = 'group')

# Muestra el plot
p
