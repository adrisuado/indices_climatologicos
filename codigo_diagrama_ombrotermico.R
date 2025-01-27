

#INSTALAR Y DESCARGAR LAS LIBRERIAS NECESARIAS

library(ggplot2)
library(reshape2)
library(tidyverse)
library(sp)
library(raster)
library(sf)
library(ncdf4)


#CARGAR DATOS PISCO

setwd("D:/PISCO")
data_pp <- brick("pp/data.nc")
data_tmax <- brick("tmax/data.nc")
data_tmin <- brick("tmin/data.nc")

setwd("C:/Users/USER/Desktop/UNMSM/CICLO V/BIOGEOGRAFÍA/TRABAJO ANALISIS CLIMATOLOGICO")


#CARGAR DEPARTAMENTOS

grafico_ombrotermico <- function(departamento_nombre, anos, pp_barras = F) {
  
  #Filtrar departamento
  departamento <- read_sf("DEPARTAMENTOS/DEPARTAMENTOS.shp") %>%
    st_transform(crs = 4326) %>%
    filter(DEPARTAMEN == toupper(departamento_nombre))
  
  
  #Asignar sistema de coords sin combar
  crs(data_pp) <- crs(data_tmax) <- crs(data_tmin) <- CRS("+init=EPSG:4326")
  
  #Extración de valores de la cuenca segun los rasters
  extrac_pp <- raster::extract(data_pp, departamento, fun = mean, na.rm = TRUE)
  extrac_tmax <- raster::extract(data_tmax, departamento, fun = mean, na.rm = TRUE)
  extrac_tmin <- raster::extract(data_tmin, departamento, fun = mean, na.rm = TRUE)
  
  #Trata de data para poder agrupar todo
  extrac_pp <- t(extrac_pp)
  extrac_tmax <- t(extrac_tmax)
  extrac_tmin <- t(extrac_tmin)
  
  
  rownames(extrac_pp) <- 1:525
  rownames(extrac_tmax) <- rownames(extrac_tmin)  <- 1:432
  
  data_depa <- data.frame(
    ANO = sort(c(rep(1981:2016, 12))),
    MES = rep(1:12, 36),
    PP = round(extrac_pp[1:432,], 2),
    TMAX = round(extrac_tmax[1:432,], 2),
    TMIN = round(extrac_tmin[1:432,], 2))
  
  data_depa <- data_depa %>%
    mutate(TMEAN = (TMAX + TMIN)/2) %>%
    mutate(MES = factor(MES, levels = 1:12, labels =  c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")),
           PP = PP/2)
  
  
  #CREA EL DIAGRAMA OMBROTERMICO
  
  for (i in anos){
    
    #Filtra año por año
    data_depa_grafico <- data_depa %>%
      filter(ANO == i) 
    
    if (pp_barras) {
      
      #Graficos de barras
      grafico <- ggplot(data = data_depa_grafico) +
        geom_col(aes(x = MES, y = PP, group = 1), size = 1, col = "black", fill = "blue") +
        geom_line(aes(x = MES, y = TMEAN, group = 1), size = 1, col = "red") +
        geom_point(aes(x = MES, y = TMEAN), size = 3, col = "red") +
        scale_y_continuous(
          name = "Temperature (°C)",
          sec.axis = sec_axis(~.*2, name = "Precipitation (mm)")
        ) +
        labs(title = paste("Diagrama Ombrotermico de Gaussen\n del departamento de", toupper(departamento_nombre),"en el año", i, sep = " "), x = "Mes", y = "Precipitación") +
        theme_bw()+
        theme(
          plot.title = element_text(size = 14, hjust = 0.5, face = "bold")
        )
      
      plot(grafico)
      
      ggsave(filename = paste("RESULTADO/DIAGRAMAS OMBROTERMICOS/diagramaO_barras_",tolower(departamento_nombre),"_",i,".png", sep = "" ), plot = grafico, width = unit(12, "cm"), height = unit(5, "cm"))
      
    } else {
      
      #Graficos de lineas
      grafico <- ggplot(data = data_depa_grafico) +
        geom_line(aes(x = MES, y = PP, group = 1), size = 1, col = "blue") +
        geom_point(aes(x = MES, y = PP), size = 3, col = "blue") +
        geom_line(aes(x = MES, y = TMEAN, group = 1), size = 1, col = "red") +
        geom_point(aes(x = MES, y = TMEAN), size = 3, col = "red") +
        scale_y_continuous(
          name = "Temperature (°C)",
          sec.axis = sec_axis(~.*2, name = "Precipitation (mm)")
        ) +
        labs(title = paste("Diagrama Ombrotermico de Gaussen\n del departamento de", toupper(departamento_nombre),"en el año", i, sep = " "), x = "Mes", y = "Precipitación") +
        theme_bw() +
        theme(
          plot.title = element_text(size = 14, hjust = 0.5, face = "bold")
        )
      
      plot(grafico)
      
      ggsave(filename = paste("RESULTADO/DIAGRAMAS OMBROTERMICOS/diagramaO_lineas_",tolower(departamento_nombre),"_",i,".png", sep = "" ), plot = grafico, width = unit(12, "cm"), height = unit(5, "cm"))
      
    }
  }
}


#___________________________PLOTEAR_____________________________________


grafico_ombrotermico("TACNA", anos = 1981:2016)

grafico_ombrotermico("PIURA", anos = 1981:2016, pp_barras = T)






