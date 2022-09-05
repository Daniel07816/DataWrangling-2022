library(tidyverse)
library(highcharter)
library(dplyr)
library(plotly)
library(ggplot2)

data = read_delim("H:/My Drive/Universidad/6to. Semestre/Data Wrangling/Lab4/tabla_completa.csv",
                  ",", escape_double = FALSE)
names(data)[4] = "DESPACHO"
data[4] = tolower(data$DESPACHO)
names(data)[5] = "ESTADO"
data[5] = tolower(data$ESTADO)
names(data)
data$ESTADO = gsub(" ","",data$ESTADO)
data$ESTADO = as.factor(data$ESTADO)
data$UBICACION = as.factor(data$UBICACION)
data$CREDITO = as.factor(data$CREDITO)

#Estados de todos los pedidos
data %>% select(ESTADO, UNIDAD) %>% group_by(ESTADO) %>% summarise(count=n())

#Clientes que mas compran
data %>%
  select(CLIENTE, COD_VIAJE) %>% 
  group_by(CLIENTE) %>% 
  summarise(viajes = n()) %>% 
  arrange(desc(viajes))

#Cantidad comprada por clientes
data %>% 
  select(CLIENTE, Q) %>% 
  group_by(CLIENTE) %>% 
  summarise(compras = sum(Q)) %>% 
  arrange(desc(compras))

#Ventas de la empresa mensuales
data %>%
  select(Q, MES) %>%
  group_by(MES) %>%
  summarise(ventas = sum(Q)) %>% 
  hchart("column", hcaes(x = MES, y = ventas)) %>% 
  hc_title(text = "<b>Ventas por mes</b>") %>% 
  hc_subtitle(text = "<i>mayo fue el mejor mes con Q60,075, marzo el mas bajo con Q48,466.75</i>")

#Ventas anuales
sum(data$Q)

#Cantidad de ventas por cada tipo de credito
data %>%
  select(CREDITO) %>%
  group_by(CREDITO) %>%
  summarise(total = n())

#Cantidad de ventas por cada tipo de credito por mes
cobrables = data %>%
  group_by(CREDITO,MES) %>%
  summarise(Cantidad = n()) %>%
  arrange(MES)

ggplot(cobrables, aes(x = MES, y = Cantidad, fill = CREDITO))+geom_bar(
  stat = "identity") + geom_text(aes(label = Cantidad), position = 
  position_stack(vjust = 0.4), size = 3)

#Estado por cada centro de distribucion
data %>%
  group_by(UBICACION,ESTADO) %>%
  summarise(Estado = n())

#Estado por cada piloto
pilotos = data %>%
  group_by(PILOTO,ESTADO) %>%
  summarise(Estado = n())
  
ggplot(pilotos, aes(x = PILOTO, y = Estado, fill = ESTADO))+geom_bar(
  stat = "identity") + geom_text(aes(label = Estado), position = 
   position_stack(vjust = 0.4), size = 3)

#Camiones mas eficientes
camiones = data %>%
  group_by(UNIDAD,ESTADO) %>%
  summarise(Estado = n())

ggplot(camiones, aes(x = UNIDAD, y = Estado, fill = ESTADO))+geom_bar(
  stat = "identity") + geom_text(aes(label = Estado), position = 
  position_stack(vjust = 0.4), size = 3)
