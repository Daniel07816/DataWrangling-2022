Lab1
================
Daniel Behar
2022-08-03

## Parte 1

Al haber unificado todas las columnas y filas necesarias, me quedó un
total de: 9 Columnas 2180 Filas

``` r
print(UNIF)
```

    ## # A tibble: 2,180 x 9
    ##    COD_VIAJE CLIENTE    UBICACION CANTIDAD PILOTO     Q CREDITO UNIDAD FECHA[,1]
    ##        <dbl> <chr>          <dbl>    <dbl> <chr>  <dbl>   <dbl> <chr>  <chr>    
    ##  1  10000001 EL PINCHE~     76002     1200 Ferna~ 300        30 Camio~ 01-2018  
    ##  2  10000002 TAQUERIA ~     76002     1433 Hecto~ 358.       90 Camio~ 01-2018  
    ##  3  10000003 TIENDA LA~     76002     1857 Pedro~ 464.       60 Camio~ 01-2018  
    ##  4  10000004 TAQUERIA ~     76002      339 Angel~  84.8      30 Panel  01-2018  
    ##  5  10000005 CHICHARRO~     76001     1644 Juan ~ 411        30 Camio~ 01-2018  
    ##  6  10000006 UBIQUO LA~     76001     1827 Luis ~ 457.       30 Camio~ 01-2018  
    ##  7  10000007 CHICHARRO~     76002     1947 Ismae~ 487.       90 Camio~ 01-2018  
    ##  8  10000008 TAQUERIA ~     76001     1716 Juan ~ 429        60 Camio~ 01-2018  
    ##  9  10000009 EL GALLO ~     76002     1601 Ismae~ 400.       30 Camio~ 01-2018  
    ## 10  10000010 CHICHARRO~     76002     1343 Ferna~ 336.       90 Camio~ 01-2018  
    ## # ... with 2,170 more rows

## Parte 2

``` r
datos = generate_df(10)
print(datos)
```

    ##     a  b  c  d
    ## 1  10 20 25 40
    ## 2   7 15 27 39
    ## 3   6 20 28 31
    ## 4  10 15 24 37
    ## 5   2 19 28 31
    ## 6   7 16 26 35
    ## 7   2 19 23 39
    ## 8   6 19 23 31
    ## 9   2 15 22 39
    ## 10  9 17 22 40

``` r
lista = lapply(datos, FUN = mode)
print(lista)
```

    ## $a
    ## [1] 2
    ## 
    ## $b
    ## [1] 15
    ## 
    ## $c
    ## [1] 22
    ## 
    ## $d
    ## [1] 31

## Parte 3

Aca estoy imprimiendo el head de los datos que salieron de haber
exportado el .txt de carros de la SAT en Enero 2019. Esta tiene 11
columnas y 1,211,691 filas

``` r
head(carros)
```

    ##   ANIO_ALZA MES NOMBRE_DEPARTAMENTO NOMBRE_MUNICIPIO MODELO_VEHICULO
    ## 1      2007   5       HUEHUETENANGO    HUEHUETENANGO            2007
    ## 2      2007   5         EL PROGRESO        EL JICARO            2007
    ## 3      2007   5          SAN MARCOS             OCOS            2007
    ## 4      2007   5           ESCUINTLA         SAN JOSÉ            2006
    ## 5      2007   5             JUTIAPA           MOYUTA            2007
    ## 6      2007   5           GUATEMALA        FRAIJANES            1997
    ##            LINEA_VEHICULO TIPO_VEHICULO USO_VEHICULO MARCA_VEHICULO CANTIDAD  X
    ## 1                SPORT125          MOTO  MOTOCICLETA      ASIA HERO        1 NA
    ## 2 BT-50 DBL CAB 4X2 TURBO       PICK UP   PARTICULAR          MAZDA        1 NA
    ## 3                   JL125          MOTO  MOTOCICLETA         KINLON        1 NA
    ## 4               JL125T-15          MOTO  MOTOCICLETA        JIALING        1 NA
    ## 5                 JH100-2          MOTO  MOTOCICLETA        JIALING        1 NA
    ## 6  TACOMA XTRA CAB 4X4 V6       PICK UP   PARTICULAR         TOYOTA        1 NA
