# Procesamiento de Datos Climáticos

## Descripción
Este proyecto utiliza datos climatologicos en distintos formatos para obtener información espacial y temporal de una determinada área de estudio.

Permite obtener índices climáticos anuales y diagramas ombrotermicos en un área determinada. Los índices generados son:

- Índice de aridez de Martonne
- Índice termopluviométrico de Dantin-Revenga
- Índice de continentalidad compensado por la latitud (Currey)

## Requisitos
- **Editor de código**: RStudio
- **Datos base**: Datos históricos de WorldClim (mensuales), disponibles en: [WorldClim 2.1](https://www.worldclim.org/data/worldclim21.html)
- **Extensión de los archivos climáticos**: `.tif`
- **Área de estudio**: Libre elección (ejemplo: departamentos del Perú)

## Estructura de Carpetas
El proyecto requiere la siguiente estructura de carpetas para su funcionamiento:

```plaintext
./TEMPERATURA MAX/    # Archivos .tif para temperatura máxima
./TEMPERATURA MIN/    # Archivos .tif para temperatura mínima
./PRECIPITACION/      # Archivos .tif para precipitación
./AREA/               # Archivos vectoriales georreferenciados (.shp u otros)
./RESULTADO/          # Carpeta donde se guardarán los productos obtenidos
