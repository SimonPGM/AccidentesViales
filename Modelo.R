library(tidyverse)
library(magrittr)
library(lubridate)
library(forecast)
############Simon
datos <- readRDS("datos_crudos.Rds") #LEYENDO RDS

#Las fechas se formatean como Año-Mes-Dia
datosts <- datos %>% #Contando accidentes por dia, formateando las fechas, asignando dia de la
  select(FECHA_ACCIDENTE) %>% #semana y ordenando por fecha
  mutate(FECHA_ACCIDENTE = substring(FECHA_ACCIDENTE, 1, 10)) %>%
  group_by(FECHA_ACCIDENTE) %>%
  summarise(ACCIDENTES_DIARIOS = n()) %>%
  ungroup() %>%
  mutate(FECHA_ACCIDENTE = as.Date(FECHA_ACCIDENTE, format = "%d/%m/%Y"),
         DIA_ACCIDENTE = as.numeric(wday(FECHA_ACCIDENTE)),
         ACCIDENTES_DIARIOS = ts(ACCIDENTES_DIARIOS, start = c(2014, 185), deltat = 1/365)) %>%
  arrange(FECHA_ACCIDENTE)


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
  select(-Ano)

ts.test <- datosts %>%
  mutate(Ano = year(FECHA_ACCIDENTE)) %>%
  filter(between(Ano, 2018, 2019)) %>%
  select(-Ano)

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
