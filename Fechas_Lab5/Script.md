Laboratorio 5
================
Danniel Behar
2022-09-28

## Eclipse solar

En tiempo de Norte América, el eclipse total inició el 21 de agosto del
2017 a las 18:26:40.

Este mismo evento, sucederá un Saros después. Un Saros equivale a 223
Synodic Months Un Synodic Month equivale a 29 días con 12 horas, con 44
minutos y 3 segundos.

``` r
synodic = days(29) + hours(12) + minutes(44) + seconds(3)
saros = 223 * synodic
fecha_origen = mdy_hms("August 21, 2017, 18:26:40", tz = "UTC")
new_fecha = fecha_origen + saros
print("El proximo eclipse sera en esta fecha:")
```

    ## [1] "El proximo eclipse sera en esta fecha:"

``` r
new_fecha
```

    ## [1] "2035-09-02 02:09:49 UTC"

## Agrupaciones y operaciones con fechas

### ¿En qué meses existe una mayor cantidad de llamadas por código?

``` r
preg <- data %>% 
  mutate(mes = month(fecha_creacion)) %>% 
  filter(call == 1) %>% 
  select(mes, cod) %>% 
  group_by(mes,cod) %>% 
  summarise(cantidad = n()) %>%
  arrange(desc(cantidad))
```

    ## `summarise()` has grouped output by 'mes'. You can override using the `.groups`
    ## argument.

``` r
preg
```

    ## # A tibble: 12 x 3
    ## # Groups:   mes [12]
    ##      mes cod                          cantidad
    ##    <dbl> <chr>                           <int>
    ##  1     3 Actualización de Información      497
    ##  2     7 Actualización de Información      496
    ##  3     5 Actualización de Información      494
    ##  4    11 Actualización de Información      493
    ##  5    10 Actualización de Información      487
    ##  6    12 Actualización de Información      478
    ##  7     8 Actualización de Información      474
    ##  8     6 Actualización de Información      471
    ##  9     1 Actualización de Información      465
    ## 10     9 Actualización de Información      465
    ## 11     4 Actualización de Información      462
    ## 12     2 Actualización de Información      443

#### En marzo

### ¿Qué día de la semana es el más ocupado?

``` r
preg <- data %>% 
  group_by(wday(fecha_creacion,label = TRUE, abbr = FALSE)) %>% 
  summarise(cantidad = n()) %>% 
  arrange(desc(cantidad)) %>% 
  head(1)
preg
```

    ## # A tibble: 1 x 2
    ##   `wday(fecha_creacion, label = TRUE, abbr = FALSE)` cantidad
    ##   <ord>                                                 <int>
    ## 1 Sunday                                                38254

### ¿Qué mes es el más ocupado?

``` r
preg <- data %>%
  group_by(month(fecha_creacion)) %>% 
  summarise(cantidad = n()) %>% 
  arrange(desc(cantidad)) %>% 
  head(1)
preg
```

    ## # A tibble: 1 x 2
    ##   `month(fecha_creacion)` cantidad
    ##                     <dbl>    <int>
    ## 1                       3    22708

#### Marzo nuevamente

### ¿Existe una concentración o estacionalidad en la cantidad de llamadas?

``` r
preg <- data %>% 
  filter(call == 1) %>% 
  mutate(semana = floor_date(fecha_creacion, unit = "week")) %>% 
  group_by(semana) %>% 
  summarise(conteo = n())
preg = data.frame(preg)
kpss.test(preg[,2], null ="Trend") #si p>0.05 es estacionaria
```

    ## Warning in kpss.test(preg[, 2], null = "Trend"): p-value greater than printed p-
    ## value

    ## 
    ##  KPSS Test for Trend Stationarity
    ## 
    ## data:  preg[, 2]
    ## KPSS Trend = 0.11412, Truncation lag parameter = 3, p-value = 0.1

#### Existe estacionalidad en las llamadas

### ¿Cuántos minutos dura la llamada promedio?

``` r
preg <- data %>% 
  filter(call == 1) %>% 
  summarise(Duracion_Promedio = mean(difftime(fecha_final, fecha_creacion, "minutes")))
  
as.numeric(preg$Duracion_Promedio, units = "mins")
```

    ## [1] 14.5579

### Realice una tabla de frecuencias con el tiempo de llamada.

``` r
preg <- data %>% 
  filter(call == 1) %>% 
  mutate(duracion = (fecha_final - fecha_creacion)) %>% 
  select(duracion)
preg$duracion = round(as.numeric(preg$duracion)/60)
tabla  <- as.data.frame(table(cut(preg$duracion,
                                  seq(min(preg$duracion),
                                      max(preg$duracion),
                                      5)
                                      ))) 
tabla <- tabla %>% 
  rename("Tiempo en minutos" = Var1, 
         "Cantidad de llamadas" = Freq)
tabla
```

    ##   Tiempo en minutos Cantidad de llamadas
    ## 1             (0,5]                  956
    ## 2            (5,10]                  959
    ## 3           (10,15]                  920
    ## 4           (15,20]                  914
    ## 5           (20,25]                  932
    ## 6           (25,30]                  823

## Parte 3: Signo Zodiacal

#### Crear una funcion que reciba la fecha de nacimiento y retorne el signo zodiacal

``` r
signo <- function(anno, mes, dia) {
  
  anno <- as.numeric(anno)
  mes <- as.numeric(mes)
  dia <- as.numeric(dia)
  fecha <- make_date(anno, mes, dia)
  if (is.na(fecha)){
    return("Fecha invalida")
  }
  if (fecha <= make_date(anno, 1, 20)){
    return("Capricornio")
  } 
  else if (fecha <= make_date(anno, 2, 18)) {
    return("Acuario")
  } 
  else if (fecha <= make_date(anno, 3, 20)) {
    return("Picis")
  } 
  else if (fecha <= make_date(anno, 4, 20)) {
    return("Aries")
  }
  else if (fecha <= make_date(anno, 5, 21)) {
    return("Tauro")
  }
  else if (fecha <= make_date(anno, 6, 21)) {
    return("Geminis")
  }
  else if (fecha <= make_date(anno, 7, 22)) {
    return("Cancer")
  }
  else if (fecha <= make_date(anno, 8, 23)) {
    return("Leo")
  }
  else if (fecha <= make_date(anno, 9, 23)) {
    return("Virgo")
  }
  else if (fecha <= make_date(anno, 10, 23)) {
    return("Libra")
  }
  else if (fecha <= make_date(anno, 11, 22)) {
    return("Escorpion")
  }
  else if (fecha <= make_date(anno, 12, 21)) {
    return("Sagitario")
  } 
  else if (fecha >= make_date(anno, 12, 22)) {
    return("Capricornio")
  }
}
# Mi signo
signo(anno = 2001, mes = 10, dia = 15)
```

    ## [1] "Libra"

## Parte 4: Flights

### Añadir cuatro columnas más en formato hora y minuto

para: dep_time, arr_time, sched_dep_time, sched_arr_time

``` r
# se elimina los NA 
flights <- flights %>% 
  filter(!is.na(dep_time)) %>% 
  filter(!is.na(arr_time)) %>% 
  filter(!is.na(sched_dep_time)) %>% 
  filter(!is.na(sched_arr_time))

# Creacion de los nuevas columnas como caracteres
flights <- flights %>% 
  mutate(dep_time2 = as.character(dep_time),
         arr_time2 = as.character(arr_time), 
         sched_dep_time2 = as.character(sched_dep_time),
         sched_arr_time2 = as.character(sched_arr_time))
         
# se cambia el formato a cada columna para que parezca hora 
flights <- flights %>% 
  mutate(dep_time2 = case_when(nchar(dep_time2) == 1 ~ paste("00", paste("0",dep_time2,sep=""), sep = ":"),
                               nchar(dep_time2) == 2 ~ paste("00", dep_time2, sep = ":"),
                               nchar(dep_time2) == 3 ~ paste(substr(dep_time2, 1, 1)
                                                                ,substr(dep_time2, 2, 3), sep = ":"),
                               nchar(dep_time2) == 4 ~ paste(substr(dep_time2, 1, 2)
                                                                ,substr(dep_time2, 3, 4), sep = ":")
                               ))
flights <- flights %>% 
  mutate(arr_time2 = case_when(nchar(arr_time2) == 1 ~ paste("00", paste("0",arr_time2,sep=""), sep = ":"),
                               nchar(arr_time2) == 2 ~ paste("00", arr_time2, sep = ":"),
                               nchar(arr_time2) == 3 ~ paste(substr(arr_time2, 1, 1)
                                                                ,substr(arr_time2, 2, 3), sep = ":"),
                               nchar(arr_time2) == 4 ~ paste(substr(arr_time2, 1, 2)
                                                                ,substr(arr_time2, 3, 4), sep = ":")
                               ))
flights <- flights %>% 
  mutate(sched_dep_time2 = case_when(nchar(sched_dep_time2) == 1 ~ paste("00", paste("0",sched_dep_time2,sep=""), sep = ":"),
                               nchar(sched_dep_time2) == 2 ~ paste("00", sched_dep_time2, sep = ":"),
                               nchar(sched_dep_time2) == 3 ~ paste(substr(sched_dep_time2, 1, 1)
                                                                ,substr(sched_dep_time2, 2, 3), sep = ":"),
                               nchar(sched_dep_time2) == 4 ~ paste(substr(sched_dep_time2, 1, 2)
                                                                ,substr(sched_dep_time2, 3, 4), sep = ":")
                               ))
flights <- flights %>% 
  mutate(sched_arr_time2 = case_when(nchar(sched_arr_time2) == 1 ~ paste("00", paste("0",sched_arr_time2,sep=""), sep = ":"),
                               nchar(sched_arr_time2) == 2 ~ paste("00", sched_arr_time2, sep = ":"),
                               nchar(sched_arr_time2) == 3 ~ paste(substr(sched_arr_time2, 1, 1)
                                                                ,substr(sched_arr_time2, 2, 3), sep = ":"),
                               nchar(sched_arr_time2) == 4 ~ paste(substr(sched_arr_time2, 1, 2)
                                                                ,substr(sched_arr_time2, 3, 4), sep = ":")
                               ))
# Se transforman a horas 
flights <- flights %>% 
  mutate(dep_time2 = hm(dep_time2),
         arr_time2 = hm(arr_time2), 
         sched_dep_time2 = hm(sched_dep_time2),
         sched_arr_time2 = hm(sched_arr_time2))
# desplegue de las horas
flights %>% 
  select(dep_time2, arr_time2, sched_dep_time2, sched_arr_time2) %>% 
  head()
```

    ## # A tibble: 6 x 4
    ##   dep_time2 arr_time2 sched_dep_time2 sched_arr_time2
    ##   <Period>  <Period>  <Period>        <Period>       
    ## 1 5H 17M 0S 8H 30M 0S 5H 15M 0S       8H 19M 0S      
    ## 2 5H 33M 0S 8H 50M 0S 5H 29M 0S       8H 30M 0S      
    ## 3 5H 42M 0S 9H 23M 0S 5H 40M 0S       8H 50M 0S      
    ## 4 5H 44M 0S 10H 4M 0S 5H 45M 0S       10H 22M 0S     
    ## 5 5H 54M 0S 8H 12M 0S 6H 0M 0S        8H 37M 0S      
    ## 6 5H 54M 0S 7H 40M 0S 5H 58M 0S       7H 28M 0S

### Encuentre el delay total que existe en cada vuelo. El delay total se puede encontrar sumando el delay de la salida y el delay de la entrada.

``` r
flights <- flights %>% 
  mutate(delay_total = (dep_time2 - sched_dep_time2) + (arr_time2 - sched_arr_time2))
flights %>% 
  select(dep_time2, arr_time2, sched_dep_time2, sched_arr_time2, delay_total) %>% 
  head()
```

    ## # A tibble: 6 x 5
    ##   dep_time2 arr_time2 sched_dep_time2 sched_arr_time2 delay_total
    ##   <Period>  <Period>  <Period>        <Period>        <Period>   
    ## 1 5H 17M 0S 8H 30M 0S 5H 15M 0S       8H 19M 0S       13M 0S     
    ## 2 5H 33M 0S 8H 50M 0S 5H 29M 0S       8H 30M 0S       24M 0S     
    ## 3 5H 42M 0S 9H 23M 0S 5H 40M 0S       8H 50M 0S       1H -25M 0S 
    ## 4 5H 44M 0S 10H 4M 0S 5H 45M 0S       10H 22M 0S      -19M 0S    
    ## 5 5H 54M 0S 8H 12M 0S 6H 0M 0S        8H 37M 0S       -1H 29M 0S 
    ## 6 5H 54M 0S 7H 40M 0S 5H 58M 0S       7H 28M 0S       8M 0S
