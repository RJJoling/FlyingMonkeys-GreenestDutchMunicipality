# Team Name:               Team Members:               Date:
# Flying Monkeys           Robbert-Jan Joling          12-01-2015
#                          Damiano Luzzi

# load packages
library(rgdal)
library(raster)
library(downloader)


# Download and unzip data
download("https://github.com/GeoScripting-WUR/VectorRaster/raw/gh-pages/data/MODIS.zip",
         "data/MODIS.zip" , quiet = T, mode = "wb")
unzip("data/MODIS.zip", exdir = "data/MODIS")
NL.Municipalities <- getData('GADM',country='NLD', level=3)


# Retrieve appropriate file and convert to raster
file <- list.files("data/MODIS", pattern = glob2rx("*.grd"), full.names = T)
NDVI.raster <- brick(file)

# Match projection
NL.Municipalities <- spTransform(NL.Municipalities, CRS(proj4string(NDVI.raster)))

# Clip and crop raster to municipality vector
NDVI.mask <- mask(NDVI.raster, NL.Municipalities)
NDVI.crop <- crop(NDVI.mask, NL.Municipalities)

NDVI <- (NDVI.crop / 10000)

yearly.average <- mean(NDVI, na.rm = TRUE)

NL.NDVI.January <- extract(NDVI$January, NL.Municipalities, fun = median, df = T, sp = T)
NL.NDVI.August <- extract(NDVI$August, NL.Municipalities, fun = median, df = T, sp = T)
NL.NDVI.Year <- extract(yearly.average, NL.Municipalities, fun = median, df = T, sp = T)

greenest.city.January <- subset(NL.NDVI.January, January == max(NL.NDVI.January$January,
                                  na.rm = T), select = c(NAME_1, NAME_2, January))
greenest.city.August <- subset(NL.NDVI.August, August == max(NL.NDVI.August$August,
                                  na.rm = T), select = c(NAME_1, NAME_2, August))
greenest.city.Year <- subset(NL.NDVI.Year, layer == max(NL.NDVI.Year$layer, na.rm = T),
                                  select = c(NAME_1, NAME_2, layer))


spplot(NL.NDVI.January, zcol = 'January', col.regions = colorRampPalette(c("#E5FFCC", "#003300"))(20))
spplot(NL.NDVI.August, zcol = 'August', col.regions = colorRampPalette(c("#E5FFCC", "#003300"))(20))
spplot(NL.NDVI.Year, zcol = 'layer', col.regions = colorRampPalette(c("#E5FFCC", "#003300"))(20))


