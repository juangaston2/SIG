library(rworldxtra)
library(raster)
library(sf)
library(tidyverse)

# Modelos con Rasters -----------------------------------------------------


data("countriesHigh")
Datos <- read_csv("https://raw.githubusercontent.com/derek-corcoran-barrios/derek-corcoran-barrios.github.io/master/Presentaciones_Espacial/Bombus.csv")
## Miremos los datos
Datos <- Datos %>% st_as_sf(coords = c(5, 6), crs = "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs +towgs84=0,0,0")
Mapa <- countriesHigh %>% st_as_sf() %>% st_crop(Datos)

## Exportar como un shp file
write_sf(Datos, "Datos.shp")


## Primer grafico: area de trabajo

ggplot(Mapa) + geom_sf()

## Grafico con puntos

ggplot() + 
  geom_sf(data = Mapa)+ 
  geom_sf(data = Datos, aes(color = species, size = Measurement))+ 
  theme_bw()+ 
  theme(legend.position = "bottom")


ggplot()+
  geom_sf(data = Mapa)+
  geom_sf(data = Datos, aes(size = Measurement))+
  facet_wrap(~species)


# Modelar ---------------------------------------------------

B_impatiens <- Datos %>% 
  dplyr::filter(species == "Bombus impatiens")

ggplot() + 
  geom_sf(data = Mapa) + 
  geom_sf(data = B_impatiens, aes(size = Measurement)) + 
  theme_bw()


## Obtener datos climaticos

BioClim <- getData("worldclim", var = "bio", res = 2.5) %>% 
  crop(B_impatiens)

plot(BioClim)

# Se trabaja solo con algunas

BioClim <- BioClim[[c(1, 7, 12, 15)]]
plot(BioClim)

BioClim

# Transformar en un data frame

Clima <- extract(BioClim, B_impatiens) %>% as.data.frame()
