library(randomForest)
library(dplyr)
library(caret)

setwd ("/Users/inesfrias/Documents/pers_gender")

#------------------------
# Function char_count 
#   Generacion de features completa
#------------------------

char_count <- function(nombre) {
  arr <- vector(mode = "integer", length = 26 + 26*26 + 2)  
  
  nombre_split <- strsplit(nombre, "")[[1]]
  #------------------------
  # Regla 1 - Cantidad de caracteres
  #------------------------
  for (x in nombre_split) {
    if (utf8ToInt(x) <= utf8ToInt('z')) {
      arr[utf8ToInt(x)-utf8ToInt('a')+1] <- arr[utf8ToInt(x)-utf8ToInt('a')+1] + 1
    }
  }
  
  #------------------------
  # Regla 2 - 2-grams
  #------------------------
  for (x in 1:(length(nombre_split)-1)) {
    ind = (utf8ToInt(nombre_split[x])-utf8ToInt('a'))*26 + (utf8ToInt(nombre_split[x+1])-utf8ToInt('a')) + 26
    arr[ind] <- arr[ind] + 1
  }
  
  #------------------------
  # Regla 3 - Caracter final del nombre 
  #------------------------
  arr[length(arr)-1] <- utf8ToInt(nombre_split[length(nombre_split)])-utf8ToInt('a')
  
  #------------------------
  # Regla 4 - Longitud del nombre 
  #------------------------
  arr[length(arr)]   <- length(nombre_split)
  
  return (arr)
}

#------------------------
# Function char_count_tuned
#   Generacion de features seleccionados
#------------------------

char_count_tuned <- function(nombre) {
  arr <- vector(mode = "integer", length = 9)  
  
  nombre_split <- strsplit(nombre, "")[[1]]
 
  #print(nombre_split)
  
  if (!is.na(nombre_split) & length(nombre_split) > 1) { 
    #------------------------
    # Regla 1 - Cantidad de caracteres
    #------------------------
    for (x in nombre_split) {
      if (utf8ToInt(x) <= utf8ToInt('z')) { 
        ## 'a'
        if ((utf8ToInt(x)-utf8ToInt('a')+1) == 1) {
          arr[1] <- arr[1] + 1
        } 
        ## 'i'
        else if ((utf8ToInt(x)-utf8ToInt('a')+1) == 9) {
          arr[2] <- arr[2] + 1
        } 
        ## 'o'
        else if ((utf8ToInt(x)-utf8ToInt('a')+1) == 15) {
          arr[3] <- arr[3] + 1
        }
      }
    }
  
    #------------------------
    # Regla 2 - 2-grams
    #------------------------
    for (x in 1:(length(nombre_split)-1)) {
        
        ind = (utf8ToInt(nombre_split[x])-utf8ToInt('a'))*26 + (utf8ToInt(nombre_split[x+1])-utf8ToInt('a')) + 26
        
        ## 364 = na
        if (ind == 364) {
          arr[4] <- arr[4] + 1
        }
        ## 378 = no
        if (ind == 378) {
          arr[5] <- arr[5] + 1
        }
        
        ## 248 = io
        if (ind == 248) {
          arr[6] <- arr[6] + 1
        }
        
        ## 234 = ia
        if (ind == 234) {
          arr[7] <- arr[7] + 1
        }
    }
    
    #------------------------
    # Regla 3 - Caracter final del nombre  
    #------------------------
    arr[8] <- utf8ToInt(nombre_split[length(nombre_split)])-utf8ToInt('a')
    
    #------------------------
    # Regla 4 - Longitud del nombre 
    #------------------------
    arr[9]   <- length(nombre_split)
  }  
  return (arr)
}

# Dataset historico
pers_hist_full <- read.csv("./data/historico-nombres.csv")
nrow(pers_hist_full)  # 9761609

# Dataset referencia https://data.buenosaires.gob.ar/dataset/nombres-permitidos 
pers_ref_full <- read.csv("./data/nombres-ref.csv", sep = ";")

#------------------------
# PARTE 1 - Punto B
# Elimino duplicados para trabajar solo con los datos necesarios y convierto a lowercase 
#------------------------
pers_hist <- as_data_frame(tolower(pers_hist_full$nombre))
names(pers_hist) <- c("nombre")
pers_hist <- as.data.frame(distinct(pers_hist, nombre))

nrow(pers_hist)  # 3301045
write.csv(pers_hist, file = "./data/pers_hist.csv")

pers_ref <- data.frame(tolower(pers_ref_full$NOMBRE), tolower(pers_ref_full$SEXO))
names(pers_ref) <- c("nombre", "sexo")

#------------------------ 
# BASELINE: con el dataset de referencia sin hacer ninguna regla  <- NADA QUE HACER ACA
#------------------------

## 75% of the sample size
smp_size <- floor(0.75 * nrow(pers_ref))
set.seed(123)
train_ind <- sample(seq_len(nrow(pers_ref)), size = smp_size)

pers_ref_train <- pers_ref[train_ind, ]
pers_ref_test  <- pers_ref[-train_ind, ]

summary(pers_ref_train)
summary(pers_ref_test)


#------------------------
# Con el dataset de referencia creo un clasificador aplicando todas reglas / combinaciones
# Evaluo la medida importance para seleccionar con cuales quedarme
#------------------------

feature_n <- data_frame()

for (name in pers_ref_train$nombre) {
  
  char_count_arr <- char_count(name)
  
  df_char_count <- t(as_tibble(char_count_arr))
  names(df_char_count) <- paste ("f1", c(1:length(char_count_arr)), sep = "_")
  
  feature_n <- bind_rows(feature_n, df_char_count)
  
}

# Join variables nuevas + train - nombre
pers_fn <- bind_cols(pers_ref_train, feature_n)
pers_fn <- pers_fn[,-c(1)]

# RandomForest - Creacion del modelo
rf_model <-randomForest(sexo~., data = pers_fn, ntree=500)  
print(rf_model)

# Mean Decrease Gini - Cuanto disminuye la precision del modelo si eliminamos la variable X
importance(rf_model)
varImpPlot(rf_model)  

#------------------------
# Armado de dataframe con variables de frecuencia, 2-grams, largo y terminacion del nombre del 
# dataset de referencia
#------------------------

feature_1 <- data_frame()

for (name in pers_ref_train$nombre) {

  char_count_arr <- char_count_tuned(name)
  
  df_char_count <- t(as_tibble(char_count_arr))
  names(df_char_count) <- paste ("f1", c(1:length(char_count_arr)), sep = "_")
  
  feature_1 <- bind_rows(feature_1, df_char_count)

}

# Join variables nuevas + train - nombre
pers_f1 <- bind_cols(pers_ref_train, feature_1)
pers_f1 <- pers_f1[,-c(1)]


# RandomForest - Creacion del modelo
rf_model <-randomForest(sexo~., data = pers_f1, ntree=500)  
print(rf_model)

# Guardo el modelo para procesar luego
saveRDS(rf_model, file = "rf_model.rds")

# Mean Decrease Gini -  
importance(rf_model)
varImpPlot(rf_model)  

#------------------------
# Validacion sobre test - Genero mismas variables
#------------------------

feature_test <- data_frame()
for (name in pers_ref_test$nombre) {

  char_count_arr <- char_count_tuned(name)
  
  df_char_count <- t(as_tibble(char_count_arr))
  names(df_char_count) <- paste ("f1", c(1:length(char_count_arr)), sep = "_")
  
  feature_test <- bind_rows(feature_test, df_char_count)
  
}
# Join con test - nombre
pers_test <- bind_cols(pers_ref_test, feature_test)
pers_test <- pers_test[,-c(1)]

rf_pred <- predict(rf_model, newdata = pers_test[,-c(1)])
table(rf_pred, pers_test$sexo)


#------------------------
# Accuracy - Matriz de confusion sobre test
#------------------------
confusionMatrix(rf_pred, pers_ref_test$sexo)


#------------------------
# Genero mismas variables
#------------------------
x <- 0
feature_hist <- data_frame()
starting_point <- 1

for (name in pers_hist[ c(starting_point:nrow(pers_hist)), 1]) {
  if (x == 200000) {
    print(paste("file", y))
    write.csv(feature_hist, file = paste ("feature_hist_", y, ".csv", sep=""))
    feature_hist <- data_frame()
    x <- 0
    y <- y + 1
  }
  x <- x + 1
  
  char_count_arr <- char_count_tuned(name)
  
  df_char_count <- t(as_tibble(char_count_arr))
  names(df_char_count) <- paste ("f1", c(1:length(char_count_arr)), sep = "_")
  
  feature_hist <- bind_rows(feature_hist, df_char_count)
  
}

if (x < 200000) {
  print(paste("file", y))
  write.csv(feature_hist, file = paste ("feature_hist_", y, ".csv", sep=""))
}

