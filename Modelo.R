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
  select(-Ano, -FECHA_ACCIDENTE)

test <- basemodelo %>%
  mutate(Ano = year(FECHA_ACCIDENTE)) %>%
  filter(between(Ano, 2018, 2019)) %>%
  select(-Ano, -FECHA_ACCIDENTE)

#------rlm---------------------------------

mod1 <- lm(ACCIDENTES_DIARIOS~DIA_ACCIDENTE+SEMANA, data = train)

err <- function(mod, testset, trainset){
  predtest <- predict(mod, testset)
  predtrain <- predict(mod, trainset)
  msetest <- mean((predtest-testset$ACCIDENTES_DIARIOS)^2)
  msetrain <- mean((predtrain-trainset$ACCIDENTES_DIARIOS)^2)
  return(data.frame(ERR = (msetest-msetrain)/msetest, RMSEtest = sqrt(msetest)))
}

err(mod1, test, train)

#----xgboost-------------------------------

trainxg <- data.frame(numacc = train$ACCIDENTES_DIARIOS,
                      dia = as.numeric(train$DIA_ACCIDENTE),
                      fest = as.numeric(train$FESTIVO))

testxg <- data.frame(numacc = test$ACCIDENTES_DIARIOS,
                     dia = as.numeric(test$DIA_ACCIDENTE),
                     fest = as.numeric(test$FESTIVO))
train_mat <- 
  trainxg %>% 
  select(-numacc) %>% 
  as.matrix() %>% 
  xgb.DMatrix(data = ., label = trainxg$numacc)

test_mat <- 
  testxg %>% 
  select(-numacc) %>% 
  as.matrix() %>% 
  xgb.DMatrix(data = ., label = testxg$numacc)

mod2 <- xgboost(data = train_mat, 
                objective = "count:poisson",
                nrounds = 2000, max.depth = 1000, eta = 0.5)

predxg <- predict(mod2, train_mat)



sqrt(mean((train$ACCIDENTES_DIARIOS-predxg)^2))

#---------Random Forest-------------------------------

mod3 <- randomForest(ACCIDENTES_DIARIOS~., data = train, ntree = 1000)

err(mod3,test, train)


#---------rls-----------------------------------------

mod4 <- glm(ACCIDENTES_DIARIOS~., family = "gaussian", data = train)

err(mod4, test, train)

#------------knn------------ESTE-FUE---------------!!

grid <- expand.grid(k = 6)

mod5 <- caret::train(ACCIDENTES_DIARIOS~DIA_ACCIDENTE+SEMANA,
                     data = train, 
                     method = "knn", 
                     tuneGrid = grid)

err(mod5, test, train)

