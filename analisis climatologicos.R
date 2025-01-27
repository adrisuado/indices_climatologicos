#############################################################################################################################################
#
# Título: Procesamiento de datos climáticos
#
# Editor de código: RStudio
#
# Descripción:
# Este script carga datos raster de temperatura máxima, temperatura mínima y precipitación desde archivos .tif
# para la obtención de los siguientes indices climaticos para un área determinada por año:
#
#   - Indice de aridez de Martonne
#   - Indice termopluviometrico de Dantin-Revenga
#   - Indice de Continentalidad compensado por la latitud Currey
#
# Aclaraciones:
# 
# ----- Acerca de los datos -----
#
#   - Trabaja como base con los datos historicos del WorldClim que son mensuales (https://www.worldclim.org/data/worldclim21.html)
#   - Las imagenes con los datos climaticos deben de tener la extensión .tiff.
#   - El área de estudio es de libre elección. A modo de ejemplo se trabaja con los departamentos del Perú
#
#
# ----- Acerca de las carpetas a conectar -----
#
#   - Las carpetas deben tener la siguiente estructura:
#     ./TEMPERATURA MAX/ (archivos .tif para tmax)
#     ./TEMPERATURA MIN/ (archivos .tif para tmin)
#     ./PRECIPITACION/   (archivos .tif para precipitación)
#     ./AREA/ (archivos en formato .shp u otro de datos vectoriales georreferenciados)
#     ./RESULTADO/ (Carpeta por defecto donde se guardaran los productos obtenidos)
#
#
# ----- Acerca de los formatos de los archivos -----
#
#   wc2.1_reso_tmax_YYYY-MM.tif
#   wc2.1_reso_tmin_YYYY-MM.tif
#   wc2.1_reso_prec_YYYY-MM.tif
#
# Donde:
#
#   - Resolución de las imagenes. En WC existen 4 tipos.
#   - YYYY-MM año y mes que representa
#
#   Este es el formato de descarga, por lo que NO MODIFICAR NOMBRES
#
#
# - Nota:
#   Ajusta las rutas en caso de que las carpetas estén 
#   en una ubicación diferente.
#
#############################################################################################################################################

#Librerias necesarias

library(sp)
library(raster)
library(sf)
library(tidyverse)



#Ruta de la carpeta de trabajo (MODIFICAR A LA DE SU PC)

setwd("C:/Users/USER/Desktop/UNMSM/CICLO V/BIOGEOGRAFÍA/TRABAJO ANALISIS CLIMATOLOGICO/")



#Función para el analisis climatologico

indices_climatologicos <- function(area, re_wc, anos, repro_epsg = NULL, nombre_area = ""){
  
  area_geo <- st_transform(area, 4326)
  
  #Listas donde se almacenan los datos por año
  lista_tmax <- list()
  lista_tmin <- list()
  lista_prec <- list()
  
  # Función para formatear números con ceros iniciales
  
  format_number <- function(x) {
    sprintf("%02d", x)
  }
  
  # Bucle para los años y meses
  for (i in anos) {
    m <- i # Formatear el año
    
    for (k in 1:12) {
      n <- format_number(k) # Formatear el mes
      
      #rutas de carpetas con los datos de t y prec
      tmax <- raster(sprintf(paste("TEMPERATURA MAX/wc2.1_",re_wc,"_tmax_",i,"-%s.tif", sep = ""), n))
      tmin <- raster(sprintf(paste("TEMPERATURA MIN/wc2.1_",re_wc,"_tmin_",i,"-%s.tif", sep = ""),  n))
      preci <- raster(sprintf(paste("PRECIPITACION/wc2.1_",re_wc,"_prec_",i,"-%s.tif", sep = ""),  n))
      
      #Recortar con el área de interes
      
      tmax <- raster::crop(tmax, area_geo)
      tmax <- raster::mask(tmax, area_geo)
      
      tmin <- raster::crop(tmin, area_geo)
      tmin <- raster::mask(tmin, area_geo)
      
      preci <- raster::crop(preci, area_geo)
      preci <- raster::mask(preci, area_geo)
      
      #meter los 12 datos de un año en lista
      lista_tmax <- append(lista_tmax, list(tmax))
      lista_tmin <- append(lista_tmin, list(tmin)) 
      lista_prec <- append(lista_prec, list(preci)) 
    }
    
    # Stackear los 12 datos por año
    stack_tmax <- stack(lista_tmax)
    stack_tmin <- stack(lista_tmin)
    stack_prec <- stack(lista_prec)
    
    # Reducirlos por media (t) y suma (prec)
    tmax_anual <- calc(stack_tmax, fun = mean)
    tmin_anual <- calc(stack_tmin, fun = mean)
    prec_anual <- calc(stack_prec, fun = sum)
    tmean_anual <- (tmax_anual + tmin_anual) / 2  # Promedio de tmax y tmin reducidos
    
    #Obtención de un raster con los valores de las latitudes
    
    area_lat <- raster(tmean_anual)
    values(area_lat) <- abs(coordinates(tmean_anual)[,2])
    
    # Calculo de índices
    indice_martonee <- prec_anual / (tmean_anual * 10)
    indice_DR <- (tmean_anual * 100) / prec_anual
    indice_continentalidad <-  (tmax_anual - tmin_anual)/(1 + (area_lat/3))
    
    #Reclasificar
    
    #MATRICES DE RECLA
    recla_martonee <- c(0, 5, 1,
                        5, 10, 2,
                        10, 20, 3,
                        20, 30, 4,
                        30, 40, 5,
                        40, 1000000,6)
    
    recla_martonee <- matrix(recla_martonee, ncol = 3, nrow = 6, byrow = TRUE)
    
    recla_DR <- c(0, 2, 1,
                  2, 3, 2,
                  3, 6, 3,
                  6, 1000000, 4)
    
    recla_DR <- matrix(recla_DR, ncol = 3, nrow = 4, byrow = TRUE)
    
    
    recla_continental <- c(0,0.6,1,
                           0.6,1.1,2,
                           1.1,1.7,3,
                           1.7,2.3,4,
                           2.3,100,5)
    
    recla_continental <- matrix(recla_continental, ncol = 3, nrow = 5, byrow = TRUE)
    
    #uso de las matrices
    
    indice_martonee_recla <- reclassify(indice_martonee, recla_martonee)
    indice_DR_recla <- reclassify(indice_DR, recla_DR)
    indice_continentalidad_recla <- reclassify(indice_continentalidad, recla_continental)
  
    
    if (repro_epsg){
      indice_martonee_recla <- raster::projectRaster(indice_martonee_recla, crs = repro_epsg)
      indice_DR_recla <- raster::projectRaster(indice_DR_recla, crs = repro_epsg)
      indice_continentalidad_recla <- raster::projectRaster(indice_continentalidad_recla, crs = repro_epsg)
      
    }
    
    # Guardar los raster resultantes en una carpeta
    writeRaster(indice_martonee_recla, sprintf(paste("RESULTADO/INDICES/indice_martonee_", nombre_area,"_",i,".tif", sep = ""),m), overwrite = T)
    writeRaster(indice_DR_recla, sprintf(paste("RESULTADO/INDICES/indice_DR_", nombre_area,"_",i,".tif", sep = ""),m), overwrite = T)
    writeRaster(indice_continentalidad_recla, sprintf(paste("RESULTADO/INDICES/indice_continentalidad_", nombre_area,"_",i,".tif", sep = ""),m), overwrite = T)
    
    
    # Reiniciar las listas para calcular el siguiente año
    lista_tmax <- list()
    lista_tmin <- list()
    lista_prec <- list()
  }
}


#___________________________________________________EJEMPLO DE USO___________________________________

#Área de estudio


depa_piura <- read_sf("AREA/DEPARTAMENTOS.shp") %>% filter(DEPARTAMEN == "PIURA")

#EJECUTAR FUNCIÓN

indices_climatologicos(area = depa_piura, re_wc = "2.5m", anos = seq(2000, 2015, 3), repro_epsg = 32717, nombre_area = "Piura")

