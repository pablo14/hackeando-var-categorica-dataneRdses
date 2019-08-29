# Script presentacion: Hackeando variables categóricas @ data neRds ES
# por https://www.linkedin.com/in/pcasas/
# Mas info de lo que ocurre acá: https://librovivodecienciadedatos.ai/preparacion-de-datos.html#alta_cardinalidad_modelo_predictivo 


library(funModeling)
library(caret)
library(tidyverse)


# Inspeccionando 
status(data_country)
freq(data_country)














# Agrupando automaticamente
country_groups=auto_grouping(data = data_country, 
                             input = "country", 
                             target="has_flu", 
                             n_groups=5, 
                             seed = 999)



# Modelo clustering que se usó
country_groups$fit_cluster



# Tabla de equivalencia
country_groups$df_equivalence



# Tabla de estadística de los resultados de la agrupación
country_groups$recateg_results



# Generamos el set de datos con la nueva agrupación
data_country_2=data_country %>% inner_join(country_groups$df_equivalence, by="country")


# Chequeamos....
df_status(data_country_2)


# Listo! Probar usar una variable vs. la otra para medir el impacto
# Construir el primer modelo, sin reducir la cardinalidad.
fitControl <- trainControl(method = "cv",
                           number = 4,
                           classProbs = TRUE,
                           summaryFunction = twoClassSummary)

fit_normal <- train(has_flu ~ country,
                    data = data_country_2,
                    method = "gbm",
                    trControl = fitControl,
                    verbose = FALSE,
                    metric = "ROC")


fit_prep <- train(has_flu ~ country_rec,
                  data = data_country_2,
                  method = "gbm",
                  trControl = fitControl,
                  verbose = FALSE,
                  metric = "ROC")

# Obtener el mejor valor de ROC 
roc1=round(mean(fit_normal$results$ROC),2)
roc2=round(mean(fit_prep$results$ROC),2)

# Porcentaje de mejora
100*(roc2-roc1)/roc1

# 
emo::ji("tada")



