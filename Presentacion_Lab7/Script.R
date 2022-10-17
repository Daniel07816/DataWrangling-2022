library(dplyr)
library(highcharter)
library(lubridate)
library(corrplot)
library(ggplot2)

data = read.csv("c1.csv")

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

data <- data %>%
  mutate(costo_clasificado = (camion + pickup + moto))

data <- data %>%
  mutate(marge_venta = (factura - (camion + pickup + moto)))

data2 <- data %>% arrange(dmy(data$fecha))

#dataset adicional
df <- data.frame(data$cod, data$costo_clasificado, data$marge_venta, (data$costo_clasificado - data$marge_venta))
names(df) = c("cod", "costo", "margen", "diferencia")

#Orden de qué actividades son más requeridas
data %>% group_by(cod) %>% summarise(n = n()) %>% arrange(n) %>% 
  hchart(type = "column", hcaes(x = cod, y = n))

#Atividades ordenadas por ingresos (sin costos)
data %>% group_by(cod) %>% summarise(facturado = sum(factura)) %>% arrange(facturado) %>%
  hchart(type = "column", hcaes(x = cod, y = facturado))

#Actividades ordenadas por costos
data %>% group_by(cod) %>% summarise(costeado = sum(costo_clasificado)) %>% arrange(costeado) %>%
  hchart(type = "column", hcaes(x = cod, y = costeado))

#Outliers
boxplot(data$factura)

data2 %>% 
  ggplot(aes(x=fecha,y=factura)) +
  labs(title = "Movimiento de las facturas") +
  xlab("Date") + ylab("Price") +
  scale_color_manual(values = "Black")+
  geom_line()
