library(dplyr)
library(reshape2)
library(ggplot2)

setwd ("/Users/inesfrias/Documents/pers_gender")

COLOR_FILL<- "#34b7ea"
COLOR_BORDER <- "#F6A4AC"
RECORDS_MIN <- 500

#------------------------
# PARTE 2 
#    Analisis exploratorio
#------------------------

pers_hist_full <- read.csv("./data/historico-nombres.csv")

#------------------------
# Chequeo de valores perdidos  
#------------------------
pers_hist_full_NA <- pers_hist_full %>%
  melt(measure.vars = 1:3) %>%
  group_by(variable) %>%
  summarize(perc_na = round(sum(is.na(value))/length(value), digits = 3)) %>%
  arrange(desc(perc_na)) %>%
  as.data.frame

pers_hist_full_NA

#------------------------
# Chequeo y limpieza de dataseet 
#------------------------

# Anotaciones busqueda de casos
pers_name_err <- pers_hist_full %>% summarize(annot_qty = sum(grepl('\\([- .,a-zA-Z0-9\\°\\º\\/]+\\)|[\\/0-9\\°\\º,]+', 
                                                                nombre, ignore.case = TRUE)))
pers_name_err$annot_qty  # 960

pers_name_err <- pers_hist_full %>% filter(nombre %in% grep('\\([- .,a-zA-Z0-9\\°\\º\\/]+\\)|[\\/0-9\\°\\º,]+',
                                      nombre, ignore.case = TRUE, value = TRUE)) %>% select(nombre)
View(pers_name_err)


# Anotaciones reemplazo 
pers_name_tmp <- transmute(pers_hist_full, gsub('\\([- .,a-zA-Z0-9\\°\\º\\/]+\\)|[\\/0-9\\°\\º,]+', '',
                                                  nombre, ignore.case = TRUE))
names(pers_name_tmp) <- c("nombre")

pers_name_clean <- data.frame(pers_name_tmp$nombre, 
                             pers_hist_full$cantidad, 
                             pers_hist_full$anio) 
names(pers_name_clean) <- c("nombre", "cantidad", "anio")


# Caracter unico y abreviaturas busqueda de casos
pers_name_err <- pers_name_clean %>% filter(nombre %in% grep(' [a-z] ', nombre, ignore.case = TRUE, value = TRUE)|
                                           nombre %in% grep(' [a-z]\\.', nombre, ignore.case = TRUE, value = TRUE)|
                                           nombre %in% grep(' [a-z]$', nombre, ignore.case = TRUE, value = TRUE)) %>%
                                    select(nombre)
View(pers_name_err)

# Caracter unico y abreviaturas reemplazo  
pers_name_tmp <- pers_name_clean %>% transmute(gsub(' [a-z] ', ' ', pers_name_clean$nombre, ignore.case = TRUE) %>%
                                     gsub(' [a-z]\\.', ' ', ., ignore.case = TRUE) %>%
                                     gsub(' [a-z]$', ' ', ., ignore.case = TRUE))
names(pers_name_tmp) <- c("nombre")

# Transformo a minuscula 
pers_name_clean <- data.frame(as_data_frame(tolower(pers_name_tmp$nombre)), 
                              pers_name_clean$cantidad, 
                              pers_name_clean$anio) 
names(pers_name_clean) <- c("nombre", "cantidad", "anio")

write.csv(pers_name_clean, "./data/historico-nombres-clean.csv", row.names = FALSE, quote = FALSE)

#------------------------
# Estadisticos descriptivos
#------------------------

# Conteo de nombres
pers_name_clean <- pers_name_clean %>% mutate(nchar(gsub('[^ ]+', '', pers_name_clean$nombre))+1)
names(pers_name_clean) <- c("nombre", "cantidad", "anio", "cant_nombres")

pers_name_clean <- pers_name_clean %>% mutate(total_nombres = cantidad * cant_nombres)

# Saca el promedio de cantidad de nombres de personas ( x anio )
names_per_year_total  <-filter(pers_name_clean, !is.na(cant_nombres) & cantidad > RECORDS_MIN) %>% 
  group_by(anio) %>% summarise(registros = sum(cantidad), 
                               total_nombres = sum(total_nombres), 
                               prom_nombres = round(sum(total_nombres)/sum(cantidad), digits = 3))

# Grafico de baston nombres
ggplot(data = names_per_year_total, aes(x=anio, y=prom_nombres)) + 
  geom_point(size = 1) +
  geom_segment(aes(x=anio,
                   xend=anio,
                   y=0,
                   yend=prom_nombres)) +
  labs(title = "Cantidad nombres promedio/año",
       x = "Año",
       y = "Cant nombres") +
  theme(axis.text.x = element_text(angle = 65, vjust = 0.6)) +
  scale_x_continuous(breaks = seq(min(names_per_year_total$anio), 
                                  max(names_per_year_total$anio), by = 5))


# Conteo de caracteres por nombre 
pers_name_clean <- pers_name_clean %>% mutate(nchar(gsub('[ ]+', '',nombre, ignore.case = TRUE)))
names(pers_name_clean) <- c("nombre", "cantidad", "anio", "cant_nombres", "total_nombres", "cant_chars")

pers_name_clean <- pers_name_clean %>% mutate(total_chars = cantidad * cant_chars)

# Saca el promedio de cantidad de caracteres por personas ( x anio )
chars_per_year_total  <-filter(pers_name_clean, cantidad > RECORDS_MIN) %>% 
  group_by(anio) %>% summarise(registros = sum(cantidad), 
                               total_chars = sum(total_chars), 
                               prom_chars = round(sum(total_chars)/sum(cantidad), digits = 3))


# Grafico de baston chars
ggplot(data = chars_per_year_total, aes(x=anio, y=prom_chars)) + 
  geom_point(size = 1) +
  geom_segment(aes(x=anio,
                   xend=anio,
                   y=0,
                   yend=prom_chars)) +
  labs(title = "Cantidad caracteres nombre promedio/año",
       x = "Año",
       y = "Cant caracteres") +
  theme(axis.text.x = element_text(angle = 65, vjust = 0.6)) +
  scale_x_continuous(breaks = seq(min(chars_per_year_total$anio), 
                                  max(chars_per_year_total$anio), by = 5))


# Asociacion con personajes television
names_actor <- filter(pers_name_clean, nombre == 'maria') %>% 
  select(anio, cantidad) %>%
  group_by(anio) %>%
  summarize(total = sum(cantidad))

ggplot(data = names_actor, aes(x=anio, y=total)) +
  geom_bar(fill=COLOR_FILL, stat="identity") +
  labs(title = "Nombre: maria",
       x = "Año",
       y = "Cant registros") +
  theme(axis.text.x = element_text(angle = 65, vjust = 0.6)) +
  scale_x_continuous(breaks = seq(min(chars_per_year_total$anio), 
                                  max(chars_per_year_total$anio), by = 5))





