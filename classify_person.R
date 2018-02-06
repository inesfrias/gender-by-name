library(dplyr)
library(randomForest)

setwd ("/Users/inesfrias/Documents/pers_gender")

#------------------------
# Join de subsets del dataset historico de nombres con variables generadas
#------------------------
system("./merge_hist.sh feature_hist_")

#------------------------
# Reads de Datasets 
#------------------------
# Dataset historico
pers_hist_full <- read.csv("./data/historico-nombres.csv")
nrow(pers_hist_full)  # 9761609
pers_hist_full_lower <- as_data_frame(tolower(pers_hist_full$nombre))
names(pers_hist_full_lower) <- c("nombre_min")

pers_hist_full_lower <- bind_cols(pers_hist_full, pers_hist_full_lower)
names(pers_hist_full_lower) <- c("nombre_may", "cantidad", "anio", "nombre")

# Dataset historico reducido
pers_hist <- read.csv("./data/pers_hist.csv")
nrow(pers_hist)  # 3301045

# Dataset con variables generadas 
pers_hist_features <- read.csv("./output/pers_hist_features.csv", header = FALSE)
pers_hist_features <- pers_hist_features[,-c(1)]
names(pers_hist_features) <- paste ("f1", c(1:ncol(pers_hist_features)), sep = "_")
nrow(pers_hist_features)  # 3301045

#------------------------
# Random Forest
#   Levanta el modelo RandomForest serializado
#------------------------
rf_model_ori <- readRDS(file = "rf_model.rds")

# Predict del historico con variables generadas
rf_pred <- predict(rf_model_ori, newdata = pers_hist_features)

#------------------------
# Bind historico con la prediccion
#------------------------
pred_sexo <- as.data.frame(rf_pred)
names(pred_sexo) <- c("sexo")
pers_hist_sexo <- bind_cols(pers_hist, pred_sexo)
pers_hist_sexo <- pers_hist_sexo[,-c(1)]
  
write.csv(pers_hist_sexo, file = "./data/pers_hist_sexo.csv")
summary(pers_hist_sexo$sexo)

#------------------------
# Left join con el historico completo
#------------------------
pers_hist_full_sexo <- left_join(pers_hist_full_lower, pers_hist_sexo, by = "nombre")
summary(pers_hist_full_sexo$sexo)
write.csv(pers_hist_full_sexo[,-c(4)], file = "./data/pers_hist_full_sexo.csv", row.names = FALSE, quote = FALSE)
