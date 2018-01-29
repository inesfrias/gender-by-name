library(dplyr)
library(tidyr)
library(reshape2)

setwd ("/Users/inesfrias/Documents/pers_gender")

pers_hist <- read.csv("./data/historico-nombres.csv")
dim(pers_hist)
colnames(pers_hist)


# PARTE 1 - Punto A
# Extrae la columna nombre a dataframe separado para poder trabajar valores unique y un dataframe de
# tamaño considerablemente menor

pers_name <- data.frame(pers_hist$nombre)
nrow(pers_name)  # 9761609

pers_name <- distinct(pers_name)
nrow(pers_name)  # 3439833
names(pers_name) <- c("nombre_uniq")

# Se encuentran anotaciones entre parentesis que introducen ruido
# Se eliminan al matchear los patrones del regex
pers_name <- pers_name %>% mutate(gsub('\\([- .a-zA-Z0-9]+\\)|[0-9]+', ' ',
                                       pers_name$nombre_uniq, ignore.case = TRUE) %>%
                                    gsub(' [a-z] ', ' ',
                                         ., ignore.case = TRUE) %>%
                                    gsub(' [a-z]\\.', ' ',
                                         ., ignore.case = TRUE) %>%
                                    gsub(' [a-z]$', ' ',
                                         ., ignore.case = TRUE))

names(pers_name) <- c("nombre_uniq","nombre_clean")

# Se encuentran nombres como Maria de las Mercedes, Maria de los Angeles
# Se eliminan las combinaciones con preposiciones del, de la, de las, de los (l/u case)
pers_name <- pers_name %>% mutate(gsub('\\ de\\ l[a|o][s]?\\ |\\ del\\ |\\ de\\ |\\ da\\ ', ' ', 
                                       pers_name$nombre_clean, ignore.case = TRUE))
names(pers_name) <- c("nombre_uniq","nombre_clean","nombre_noPrep")

pers_name <- subset(pers_name, trimws(nombre_noPrep) != "")

single_names_cols <- c("nombre_1","nombre_2","nombre_3", "nombre_4", "nombre_5", 
                       "nombre_6", "nombre_7", "nombre_8", "nombre_9", "nombre_10", "nombre_11")

# Dry run para ver si la separacion en columnas arroja warnings
pers_name %>% separate(nombre_noPrep, single_names_cols)

# El dry run en un principio arrojo dos valores poco comunes
pers_name[695642,] # Josina Livina Dolores Lucia Carmen Balduina Yolanda Maria Inmaculada Gislena Javiera  -> valido
pers_name[1297794,] # Roberto Carlos(antes De Cumplimentar Hablar Con Augusto Borja Cdr-jujuy 0388-4310036)  -> invalido, se corrigio luego en el primer paso


# Resultado
pers_name <- pers_name %>% separate(nombre_noPrep, single_names_cols, remove = FALSE)

pers_name_uniq <- melt(pers_name[ , single_names_cols], 
                       measure.vars = single_names_cols, 
                       na.rm = TRUE, 
                       variable.name = "variable", 
                       value.name = "nombre")

pers_name_uniq <- pers_name_uniq %>% mutate(toupper(nombre))
names(pers_name_uniq) <- c("variable", "nombre", "mayusc")
pers_name_uniq <- distinct(pers_name_uniq, mayusc)

pers_name_uniq <- arrange(pers_name_uniq, mayusc)
nrow(pers_name_uniq)  # 200649

write.csv(pers_name_uniq[,c("mayusc")], file = "pers_name_uniq.csv")


