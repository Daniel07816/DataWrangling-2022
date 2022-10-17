library(dplyr)
library(highcharter)
library(lubridate)
library(corrplot)
library(ggplot2)
library(GGally)
library(Hmisc)
library(corrplot)
library(PerformanceAnalytics)
require(plotly)

data = read.csv("c1.csv")

#---------------Limpieza de data---------------
data = data[-c(23,24,25,26,27,28)]
names(data) = tolower(names(data))
names(data) = gsub("_5", "", names(data))
names(data) = gsub("x", "", names(data))

data = data %>% mutate(
  camion = gsub("Q", "",camion),
  camion = gsub("-", 0,camion),
  pickup = gsub("Q", "",pickup),
  pickup = gsub("-", 0,pickup),
  moto = gsub("Q", "",moto),
  moto = gsub("-", 0,moto),
  
  directocamion = gsub("Q", "",directocamion),
  directocamion = gsub("-", 0,directocamion),
  directopickup = gsub("Q", "",directopickup),
  directopickup = gsub("-", 0,directopickup),
  directomoto = gsub("Q", "",directomoto),
  directomoto = gsub("-", 0,directomoto),
  
  fijocamion = gsub("Q", "",fijocamion),
  fijocamion = gsub("-", 0,fijocamion),
  fijopickup = gsub("Q", "",fijopickup),
  fijopickup = gsub("-", 0,fijopickup),
  fijomoto = gsub("Q", "",fijomoto),
  fijomoto = gsub("-", 0,fijomoto),
  
  factura = gsub("Q", "", factura)
)

data$fecha = dmy(data$fecha)
data$factura = as.numeric(data$factura)
data$camion = as.numeric(data$camion)
data$pickup = as.numeric(data$pickup)
data$moto = as.numeric(data$moto)
data$directocamion = as.numeric(data$directocamion)
data$directopickup = as.numeric(data$directopickup)
data$directomoto = as.numeric(data$directomoto)
data$fijocamion = as.numeric(data$fijocamion)
data$fijopickup = as.numeric(data$fijopickup)
data$fijomoto = as.numeric(data$fijomoto)
data$lat = as.numeric(data$lat)
data$long = as.numeric(data$long)
data$height = as.numeric(data$height)
data$origen = as.factor(data$origen)

data <- data %>%
  mutate(costo_clasificado = (camion + pickup + moto))

data <- data %>%
  mutate(marge_venta = (factura - (camion + pickup + moto)))

data2 <- data %>% arrange(data$fecha)

#------------------------------

highchart(type = "stock") %>% 
  hc_add_series(data2$factura, type = "line")

highchart(type = "stock") %>% 
  hc_add_series(data2$costo_clasificado, type = "line")

summary(data)

#Total marginado hasta el momento
sum(data$marge_venta)

#Tarifas promedio del 2017 por unidad
camiones = data.frame(data$camion,data$factura)
camiones = camiones %>% mutate(data.camion = na_if(camiones$data.camion,0))
camiones = camiones[!is.na(camiones$data.camion),]

pickups = data.frame(data$pickup,data$factura)
pickups = pickups %>% mutate(data.pickup = na_if(pickups$data.pickup,0))
pickups = pickups[!is.na(pickups$data.pickup),]

motos = data.frame(data$moto,data$factura)
motos = motos %>% mutate(data.moto = na_if(motos$data.moto,0))
motos = motos[!is.na(motos$data.moto),]

summary(camiones)
summary(pickups)
summary(motos)

#Codigo por cada centro
ubicaiones = data %>%
  group_by(cod,origen) %>%
  summarise(Origen = n())

ggplot(ubicaiones, aes(x = origen, y = Origen, fill = cod))+geom_bar(
  stat = "identity") + geom_text(aes(label = Origen), position = 
  position_stack(vjust = 0.4), size = 3)

#Mejores facturas por sector
facturas = data %>%
  select(factura, cod) %>% 
  group_by(cod) %>% 
  summarise(cantidad = n()) %>% 
  arrange(desc(cantidad))

marginacion = (data$marge_venta/data$factura)
mean(marginacion)

ggplot(facturas, aes(x = cod, y = cantidad)) + geom_bar(color = "red",
  fill = "red", stat = "identity")
