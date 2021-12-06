library(tidyverse)
library(magrittr)
library(lubridate)
library(forecast)
library(rjson)
############Simon
datos <- readRDS("AccidentesMDE.Rds") #LEYENDO RDS

#Generando fechas 
json <- fromJSON(file = "holidays.json")
fechas <- c()
for (i in 1:length(json$table)) {
  fechas[i] <- as.Date(json$table[[i]]$celebrationDay)
} 
detach("package:rjson", unload=TRUE)
#Las fechas se formatean como Año-Mes-Dia
datosts <- datos %>% #Contando accidentes por dia, formateando las fechas, asignando dia de la
  select(FECHA_ACCIDENTE) %>% #semana y ordenando por fecha
  mutate(FECHA_ACCIDENTE = substring(FECHA_ACCIDENTE, 1, 10)) %>%
  group_by(FECHA_ACCIDENTE) %>%
  summarise(ACCIDENTES_DIARIOS = n()) %>%
  ungroup() %>%
  mutate(FECHA_ACCIDENTE = as.Date(FECHA_ACCIDENTE, format = "%d/%m/%Y"),
         DIA_ACCIDENTE = factor(wday(FECHA_ACCIDENTE, label = T)),
         ACCIDENTES_DIARIOS = ts(ACCIDENTES_DIARIOS, start = c(2014, 185), deltat = 1/365)) %>%
  arrange(FECHA_ACCIDENTE) %>%
  mutate(PRECIP_MES = factor(if_else(month(FECHA_ACCIDENTE) %in% c(3:5, 9:11), 
                              "Lluvioso", "Seco")),
         FESTIVO = factor(if_else(FECHA_ACCIDENTE %in% fechas, "Si", "No")),
         SEMANA = factor(week(FECHA_ACCIDENTE)))

rm(list = c("json", "fechas", "i"))

#Generando quincenas
datosts$QUINCENA <- "0"
for (i in 1:nrow(datosts)) {
  if (day(datosts$FECHA_ACCIDENTE[i]) %in% c(15, 30)) {
    if (datosts$DIA_ACCIDENTE[i] != "dom" & datosts$FESTIVO[i] == "No") {
      datosts[i, 7] <- "Si"
    } else {
      datosts[i, 7] <- "No"
      if (datosts$FESTIVO[i] == "Si") {
        j = 1
        while(T) {
          if(datosts$DIA_ACCIDENTE[i-j] != "dom") {
            datosts[i-j, 7] <- "Si"
            break
          }
          j <- j-1
        } 
      } else {
        j = 1
        while(T) {
          if(datosts$DIA_ACCIDENTE[i-j] != "dom" & datosts$FESTIVO[i] == "No") {
            datosts[i-j, 7] <- "Si"
            break
          }
          j <- j-1
        }
      }
    }
  } else {
    datosts[i, 7] <- "No"
  }
}

datosts %<>%
  mutate(QUINCENA = as.factor(QUINCENA))

#Guardando la base de datos
#saveRDS(datosts, "basemodelo.Rds")

#Ploteando la serie completa
ts.plot <- datosts %>%
  mutate(Ano = year(FECHA_ACCIDENTE)) %>%
  filter(between(Ano, 2014, 2019)) %>%
  select(-Ano) %>%
  ggplot(aes(FECHA_ACCIDENTE, ACCIDENTES_DIARIOS)) +
  geom_path()

plotly::ggplotly(ts.plot)
#Creando entrenamiento y validacion como dataframes con dias
ts.train <- datosts %>%
  mutate(Ano = year(FECHA_ACCIDENTE)) %>%
  filter(between(Ano, 2014, 2017)) %>%
  select(-Ano, -FECHA_ACCIDENTE)

ts.test <- datosts %>%
  mutate(Ano = year(FECHA_ACCIDENTE)) %>%
  filter(between(Ano, 2018, 2019)) %>%
  select(-Ano, -FECHA_ACCIDENTE)

model.train <- ts.train %$%
  auto.arima(ACCIDENTES_DIARIOS, xreg = DIA_ACCIDENTE, lambda = -1, D = 7)

#mirando el RMSE en el train
mean((ts.train$ACCIDENTES_DIARIOS - fitted(model.train))^2)

#ploteando modelo original
ts.plot(ts.train$ACCIDENTES_DIARIOS, round(fitted(model.train)),
        col = c("black", "red"))

fores <- ts.test %$%
  forecast(model.train, h = nrow(ts.test), level = c(95),
           xreg = DIA_ACCIDENTE, lambda = -1, biasadj = T)

plot.ts(ts.test$ACCIDENTES_DIARIOS)
plot.ts(fitted(fores$model))


#Usando nn

model.train.nn <- ts.train %$%
  nnetar(ACCIDENTES_DIARIOS, xreg = DIA_ACCIDENTE, lambda = 1)

forc <- ts.test %$%
  forecast(model.train.nn, h = length(ts.test$ACCIDENTES_DIARIOS),
           xreg = DIA_ACCIDENTE)
###########Gaviria

train <- basemodelo %>%
  mutate(Ano = year(FECHA_ACCIDENTE)) %>%
  filter(between(Ano, 2014, 2017)) %>%
  select(-Ano)

test <- basemodelo %>%
  mutate(Ano = year(FECHA_ACCIDENTE)) %>%
  filter(between(Ano, 2018, 2019)) %>%
  select(-Ano)

#------rlm---------------------------------

modelo <- lm(ACCIDENTES_DIARIOS~DIA_ACCIDENTE+FESTIVO, data = train)

err <- function(mod, testset, trainset){
  predtest <- predict(mod, testset)
  predtrain <- predict(mod, trainset)
  msetest <- mean((predtest-testset$ACCIDENTES_DIARIOS)^2)
  msetrain <- mean((predtrain-trainset$ACCIDENTES_DIARIOS)^2)
  return(data.frame(ERR = (msetest-msetrain)/msetrain, RMSEtest = sqrt(msetest), RMSEtrain = sqrt(msetrain)))
}

err(modelo, test, train)

#----xgboost-------------------------------

#trainxg <- data.frame(numacc = train$ACCIDENTES_DIARIOS,
#                      dia = as.numeric(train$DIA_ACCIDENTE),
#                      fest = as.numeric(train$FESTIVO))
#
#testxg <- data.frame(numacc = test$ACCIDENTES_DIARIOS,
#                     dia = as.numeric(test$DIA_ACCIDENTE),
#                     fest = as.numeric(test$FESTIVO))
#train_mat <- 
#  trainxg %>% 
#  select(-numacc) %>% 
#  as.matrix() %>% 
#  xgb.DMatrix(data = ., label = trainxg$numacc)
#
#test_mat <- 
#  testxg %>% 
#  select(-numacc) %>% 
#  as.matrix() %>% 
#  xgb.DMatrix(data = ., label = testxg$numacc)
#
#mod2 <- xgboost(data = train_mat, 
#                objective = "count:poisson",
#                nrounds = 2000, max.depth = 1000, eta = 0.5)
#
#predxg <- predict(mod2, train_mat)
#
#
#
#sqrt(mean((train$ACCIDENTES_DIARIOS-predxg)^2))

#---------Random Forest-------------------------------

#mod3 <- randomForest(ACCIDENTES_DIARIOS~., data = train, ntree = 1000)
#
#err(mod3,test, train)


#---------rls-----------------------------------------

#mod4 <- glm(ACCIDENTES_DIARIOS~., family = "gaussian", data = train)
#
#err(mod4, test, train)

#------------knn------------ESTE-FUE---------------!!

# grid <- expand.grid(k = 6)
# 
# modelo.knn <- caret::train(ACCIDENTES_DIARIOS~DIA_ACCIDENTE+SEMANA,
#                      data = train, 
#                      method = "knn", 
#                      tuneGrid = grid)
# 
# err(modelo.knn, test, train)

#--------MODELO-MULTICLASE----------------------------

FECHAS <- as.Date(substring(AccidentesMDE$FECHA_ACCIDENTE,1,10), format = "%d/%m/%Y")
CLIMA <- as.factor(ifelse(month(FECHAS) %in% c(3:5, 9:11), "LLUVIOSO", "SECO"))
basemodelo2 <- data.frame(CLASE = as.factor(AccidentesMDE$CLASE_ACCIDENTE),
                          FECHA = FECHAS,
                          CLIMA = CLIMA)

proporciones <- colMeans(prop.table(table(basemodelo2$FECHA, basemodelo2$CLASE),1)) #This is the model modofoko

#-----MERGING-MODELO-MULTICLASE-Y-MODELO-PREDICTIVO----

#prediccionfinal <- function(DIA_ACCIDENTE, SEMANA){ #Esto es un ejemplo
#  pred <- predict(modelo, data.frame(DIA_ACCIDENTE = DIA_ACCIDENTE, SEMANA = as.factor(SEMANA)))
#  return(pred*proporciones)
#}
#
#
#prediccionfinal("dom", 10) #Número de accidentes de cada clase un domingo de la semana 10 del año 


#--------ESTRUCTURACION-DE-LAS-PREDICCIONES-----------

predict.train <- data.frame(FECHA = train$FECHA_ACCIDENTE,
                            SEMANA = paste(week(train$FECHA_ACCIDENTE),year(train$FECHA_ACCIDENTE), sep="-"),
                            MES =  paste(month(train$FECHA_ACCIDENTE),year(train$FECHA_ACCIDENTE), sep="-"),
                            ANIO = year(train$FECHA_ACCIDENTE),
                            ATROPELLO = round(proporciones[1]*predict(modelo,train)),
                            CAIDA.OCUPANTE = round(proporciones[2]*predict(modelo,train)),
                            CHOQUE = round(proporciones[3]*predict(modelo,train)),
                            INCENDIO = round(proporciones[4]*predict(modelo,train)),
                            OTRO = round(proporciones[5]*predict(modelo,train)),
                            VOLCAMIENTO = round(proporciones[6]*predict(modelo,train)),
                            MES.D = month(train$FECHA_ACCIDENTE),
                            SEMANA.D = week(train$FECHA_ACCIDENTE))

predict.test  <- data.frame(FECHA = test$FECHA_ACCIDENTE,
                            SEMANA = paste(week(test$FECHA_ACCIDENTE),year(test$FECHA_ACCIDENTE), sep="-"),
                            MES =  paste(month(test$FECHA_ACCIDENTE),year(test$FECHA_ACCIDENTE), sep="-"),
                            ANIO = year(test$FECHA_ACCIDENTE),
                            ATROPELLO = round(proporciones[1]*predict(modelo,test)),
                            CAIDA.OCUPANTE = round(proporciones[2]*predict(modelo,test)),
                            CHOQUE = round(proporciones[3]*predict(modelo,test)),
                            INCENDIO = round(proporciones[4]*predict(modelo,test)),
                            OTRO = round(proporciones[5]*predict(modelo,test)),
                            VOLCAMIENTO = round(proporciones[6]*predict(modelo,test)),
                            MES.D = month(test$FECHA_ACCIDENTE),
                            SEMANA.D = week(test$FECHA_ACCIDENTE))

saveRDS(predict.train, "predict_train.Rds")
saveRDS(predict.test, "predict_test.Rds")

#------GENERANDO-DATASET-PARA-2020----------------------------------

#base2020 <- AccidentesMDE %>% 
#  select(FECHA_ACCIDENTE) %>% #semana y ordenando por fecha
#  mutate(FECHA_ACCIDENTE = substring(FECHA_ACCIDENTE, 1, 10)) %>%
#  group_by(FECHA_ACCIDENTE) %>%
#  summarise(ACCIDENTES_DIARIOS = n()) %>%
#  ungroup() %>%
#  mutate(FECHA_ACCIDENTE = as.Date(FECHA_ACCIDENTE, format = "%d/%m/%Y"),
#         DIA_ACCIDENTE = factor(wday(FECHA_ACCIDENTE, label = T)),
#         ACCIDENTES_DIARIOS = ACCIDENTES_DIARIOS) %>%
#  arrange(FECHA_ACCIDENTE) %>%
#  mutate(SEMANA = factor(week(FECHA_ACCIDENTE))) %>%
#  filter(year(FECHA_ACCIDENTE) == 2020)


#------------OBSERVANDO-EL-AJUSTE-PARA-ESTE-AÑO--------------------------
base2020$FESTIVO <- basemodelo$FESTIVO[c(2010:2253)]
plotfit2020 <- ggplotly(ggplot(base2020, aes(FECHA_ACCIDENTE, ACCIDENTES_DIARIOS)) + 
               geom_point() +
               geom_line() + 
               geom_path(data = data.frame(FECHA_ACCIDENTE = base2020$FECHA_ACCIDENTE, 
                                           ACCIDENTES_DIARIOS = predict(modelo, base2020[,-c(1,2)])), colour = "#03fcb1") +
               geom_point(data = data.frame(FECHA_ACCIDENTE = base2020$FECHA_ACCIDENTE, 
                                            ACCIDENTES_DIARIOS = predict(modelo, base2020[,-c(1,2)])), colour = "#03fcb1") +
               labs(title = "Ajuste del modelo en el año 2020", 
                    caption = "Regresión Lineal", x = "Fecha", y = "Número de accidentes") +
               theme_minimal() + 
               theme(plot.title = element_text(hjust = 0.5)))

saveRDS(plotfit2020, "plotfit2020.Rds")

#Para el informe técnico
ggplotly(ggplot(basemodelo, aes(FECHA_ACCIDENTE, ACCIDENTES_DIARIOS)) + 
  geom_point() +
  geom_line() + 
  geom_path(data = data.frame(FECHA_ACCIDENTE = basemodelo$FECHA_ACCIDENTE, 
                              ACCIDENTES_DIARIOS = predict(modelo, basemodelo)), colour = "#03fcb1") +
  geom_point(data = data.frame(FECHA_ACCIDENTE = basemodelo$FECHA_ACCIDENTE, 
                               ACCIDENTES_DIARIOS = predict(modelo, basemodelo)), colour = "#03fcb1") +
  labs(title = "Ajuste del modelo", 
       caption = "Regresion", x = "Fecha", y = "Número de accidentes") +
  theme_minimal() + 
  theme(plot.title = element_text(hjust = 0.5)))

#Algunas métricas particulares

metrictimedata <- data.frame(fecha = test$FECHA_ACCIDENTE,
                             semana = week(test$FECHA_ACCIDENTE),
                             mes = month(test$FECHA_ACCIDENTE),
                             anio = year(test$FECHA_ACCIDENTE),
                             reales = test$ACCIDENTES_DIARIOS,                
                             predichos = predict(modelo, test))

#Para día

sqrt(mean((metrictimedata$reales-metrictimedata$predichos)^2))

#Para semana

metricsemdata <- metrictimedata %>% 
  group_by(semana) %>% 
  summarise(reales = sum(reales), predichos = sum(predichos))
  
sqrt(mean((metricsemdata$reales-metricsemdata$predichos)^2))

#Para mes

metricmonthdata <- metrictimedata %>%
  group_by(anio, mes) %>%
  summarise(reales = sum(reales), predichos = sum(predichos))
  
sqrt(mean((metricmonthdata$reales-metricmonthdata$predichos)^2))

#Exportando resultados temporales

saveRDS(data.frame(Dia = sqrt(mean((metrictimedata$reales-metrictimedata$predichos)^2)),
                   Semana = sqrt(mean((metricsemdata$reales-metricsemdata$predichos)^2)),
                   Mes = sqrt(mean((metricmonthdata$reales-metricmonthdata$predichos)^2))),
        "MetricasErrorTemporal.Rds")


## Métricas anteriores sobre Train
#
#metrictimedata2 <- data.frame(fecha = train$FECHA_ACCIDENTE,
#                             semana = week(train$FECHA_ACCIDENTE),
#                             mes = month(train$FECHA_ACCIDENTE),
#                             anio = year(train$FECHA_ACCIDENTE),
#                             reales = train$ACCIDENTES_DIARIOS,                
#                             predichos = predict(modelo, train))
#
##Para día
#
#sqrt(mean((metrictimedata2$reales-metrictimedata2$predichos)^2))
#
##Para semana
#
#metricsemdata2 <- metrictimedata2 %>% 
#  group_by(semana) %>% 
#  summarise(reales = sum(reales), predichos = sum(predichos))
#
#sqrt(mean((metricsemdata2$reales-metricsemdata2$predichos)^2))
#
##Para mes
#
#metricmonthdata2 <- metrictimedata2 %>%
#  group_by(anio, mes) %>%
#  summarise(reales = sum(reales), predichos = sum(predichos))
#
#sqrt(mean((metricmonthdata2$reales-metricmonthdata2$predichos)^2))

#Diario por tipo de accidente

TipoAccidente <- AccidentesMDE %>% #Contando accidentes por dia, formateando las fechas, asignando dia de la
  select(FECHA_ACCIDENTE, CLASE_ACCIDENTE) %>% #semana y ordenando por fecha
  mutate(FECHA_ACCIDENTE = substring(FECHA_ACCIDENTE, 1, 10)) %>%
  group_by(FECHA_ACCIDENTE) %>%
  mutate(FECHA_ACCIDENTE = as.Date(FECHA_ACCIDENTE, format = "%d/%m/%Y"),
         CLASE_ACCIDENTE = factor(CLASE_ACCIDENTE),
         ANIO = year(FECHA_ACCIDENTE)) %>%
  arrange(FECHA_ACCIDENTE) %>%
  filter(between(ANIO,2018,2019)) %>%
  select(-ANIO)

TipoAccidente <- table(TipoAccidente$FECHA_ACCIDENTE,TipoAccidente$CLASE_ACCIDENTE)
TipoAccidente <- data.frame(FECHA = date(rownames(TipoAccidente)),
                          ATROPELLO = TipoAccidente[,1],
                          CHOQUE = TipoAccidente[,2], 
                          CAIDA.OCUPANTE = TipoAccidente[,3],
                          OTRO = TipoAccidente[,4],
                          VOLCAMIENTO = TipoAccidente[,5], 
                          INCENDIO = TipoAccidente[,6])
row.names(TipoAccidente) <- NULL

#Atropello
sqrt(mean((TipoAccidente$ATROPELLO-predict_test$ATROPELLO)^2))

#Choque
sqrt(mean((TipoAccidente$CHOQUE-predict_test$CHOQUE)^2))

#Caida ocupante
sqrt(mean((TipoAccidente$CAIDA.OCUPANTE-predict_test$CAIDA.OCUPANTE)^2))

#Otro
sqrt(mean((TipoAccidente$OTRO-predict_test$OTRO)^2))

#Volcamiento
sqrt(mean((TipoAccidente$VOLCAMIENTO-predict_test$VOLCAMIENTO)^2))

#Incendio
sqrt(mean((TipoAccidente$INCENDIO-predict_test$INCENDIO)^2))

#Exportando datos por tipo de accidente a nivel diario

saveRDS(data.frame(Choque = sqrt(mean((TipoAccidente$CHOQUE-predict_test$CHOQUE)^2)),
                   Caida.ocupante = sqrt(mean((TipoAccidente$CAIDA.OCUPANTE-predict_test$CAIDA.OCUPANTE)^2)),
                   Otro = sqrt(mean((TipoAccidente$OTRO-predict_test$OTRO)^2)),
                   Volcamiento = sqrt(mean((TipoAccidente$VOLCAMIENTO-predict_test$VOLCAMIENTO)^2)),
                   Incendio = sqrt(mean((TipoAccidente$INCENDIO-predict_test$INCENDIO)^2))),
        "MetricasErrorTipoAccidente.Rds")
