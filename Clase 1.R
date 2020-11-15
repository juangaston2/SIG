library(sf)
library(tidyverse)
library(rworldxtra)
library(rgeos)

data("countriesHigh")

Mundo <- st_as_sf(countriesHigh)

ggplot() + geom_sf(data=Mundo)

ggplot() + 
  geom_sf(data = Mundo, aes(fill= REGION))
  
colnames(Mundo)

ggplot() + 
  geom_sf(data=Mundo, aes(fill = GDP_MD_EST))


Africa <- Mundo %>% 
  filter(continent == "Africa")

ggplot() + geom_sf(data = Africa, aes(fill = GDP_MD_EST))

## Subset por PBI

PBI_alto <- Mundo %>% 
  dplyr::filter(GDP_MD_EST >= mean(Mundo$GDP_MD_EST))

ggplot(PBI_alto) +
  geom_sf(aes(fill = GDP_MD_EST))


## Generar variables 

Africa <- Africa %>% mutate(Poblacion_mill = POP_EST/1e+6)

ggplot(Africa)+
  geom_sf(aes(fill = Poblacion_mill)) + 
  scale_fill_viridis_c()

Africa <- Africa %>% mutate(PIB_per_Capita = GDP_MD_EST/POP_EST)

ggplot(Africa)+
  geom_sf(aes(fill = PIB_per_Capita)) + 
  scale_fill_viridis_c()


Africa %>% 
  filter(PIB_per_Capita == max(PIB_per_Capita, na.rm =TRUE)) %>% 
  pull (NAME)


Africa <- Africa %>% dplyr::select(NAME, Poblacion_mill, PIB_per_Capita, GLOCAF)


write_sf(Africa, "Africa.shp")

Africa2 <- read_sf("Africa.shp")

ggplot(Africa2) + geom_sf(aes(fill= GLOCAF))+ theme_dark()


## Obtener informacion desde paquetes en R

library(raster)
Peru <- getData(name= "GADM", country = "PER", level=1) %>% 
  st_as_sf()

ggplot(Peru) + geom_sf(aes(fill=NAME_1)) + theme(legend.position = "none")


## Grafica puntos en dentro del mapa

df <- data.frame(lon = c(-74, -74), lat = c(-5, -4), Casa = c("Grande", "Chica")) %>% 
  st_as_sf(coords = c(1,2), crs = "+proj=longlat +datum=WGS84 +no_defs")

df


ggplot() + geom_sf(data = Peru) + geom_sf(data = df, aes(color=Casa)) 


## Raster

prec <- getData("worldclim", res = 10, var = "prec")

plot(prec, colNa = "Black")

invierno <- prec[[c(6,7,8)]]
plot(invierno, colNA = "black")


rasterVis::levelplot(invierno)

#Sumar precipitaciones

total_inv <- prec[[6]] + prec[[7]] + prec[[8]]
plot(total_inv, colNA = "black")

total_prec <- sum (prec)
plot(total_prec)


#Para cortar

Raster_Africa <- total_prec  %>%  crop(Africa)
plot(Raster_Africa )

Raster_Peru <- total_prec  %>%  crop(Peru) %>% mask(Peru)
plot(Raster_Peru)

extract(Raster_Peru, df)

df$prec <- extract(Raster_Peru, df)

Prec_casas <- extract(prec, df)
Prec_casas
Prec_casas %>%  as.data.frame() %>%  bind_cols(df)

##Escribir un raster

writeRaster(Raster_Africa, "RasterAfrica.grd", overwrite = T)

Raster_Africa2 <- raster("RasterAfrica.grd")


## Proyecciones 
 # https://projectionwizard.org/  

proy <- "+proj=laea +lon_0=21.796875 +lat_0=0 +datum=WGS84 +units=m +no_defs"
proy
Africa_igual <- projectRaster(Raster_Africa, crs = proy)

plot(Africa_igual)
plot(Raster_Africa)



## Grafico de shapefiles y rasters

Africa_DF <- Raster_Africa %>% as("SpatialPixelsDataFrame") %>% 
  as.data.frame()

head(Africa_DF)

ggplot() +geom_tile(data = Africa_DF, aes(x = x,y = y, fill = layer))+
  geom_sf(data = Africa, alpha = 0)
