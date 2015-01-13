# Team Name:               Team Members:               Date:
# Flying Monkeys           Robbert-Jan Joling          12-01-2015
#                          Damiano Luzzi

# load packages
library(rgdal)
library(raster)
library(downloader)
library(grid)

# Download and unzip data
download("https://github.com/GeoScripting-WUR/VectorRaster/raw/gh-pages/data/MODIS.zip",
         "data/MODIS.zip" , quiet = T, mode = "wb")
unzip("data/MODIS.zip", exdir = "data/MODIS")
NL.Municipalities <- getData('GADM',country='NLD', level=3, path = "data/")

# Retrieve appropriate file and convert to raster
file <- list.files("data/MODIS", pattern = glob2rx("*.grd"), full.names = T)
NDVI.raster <- brick(file)

# Match projection
NL.Municipalities <- spTransform(NL.Municipalities, CRS(proj4string(NDVI.raster)))

# Clip and crop raster to municipality vector
NDVI.mask <- mask(NDVI.raster, NL.Municipalities)
NDVI.crop <- crop(NDVI.mask, NL.Municipalities)

# Convert raster values to true NDVI values
NDVI <- (NDVI.crop / 10000)

# Calculate NDVI yearly average per city
yearly.average <- mean(NDVI, na.rm = T)

# Calculate average NDVI per city for January, August, and the whole year
NL.NDVI.January <- extract(NDVI$January, NL.Municipalities, fun = mean, df = T, sp = T)
NL.NDVI.August <- extract(NDVI$August, NL.Municipalities, fun = mean, df = T, sp = T)
NL.NDVI.Year <- extract(yearly.average, NL.Municipalities, fun = mean, df = T, sp = T)

# Find city with the highest NDVI value for January, August, and the whole year
greenest.city.January <- subset(NL.NDVI.January, January == max(NL.NDVI.January$January,
                                  na.rm = T), select = c(NAME_1, NAME_2, January))
greenest.city.August <- subset(NL.NDVI.August, August == max(NL.NDVI.August$August,
                                  na.rm = T), select = c(NAME_1, NAME_2, August))
greenest.city.Year <- subset(NL.NDVI.Year, layer == max(NL.NDVI.Year$layer, na.rm = T),
                                  select = c(NAME_1, NAME_2, layer))

# Visualisation
map1 <- spplot(NL.NDVI.January, main = list("January"), zcol = 'January',
               col.regions = colorRampPalette(c("#E5FFCC", "#003300"))(20))
map2 <- spplot(NL.NDVI.August, main = list("August"), zcol = 'August',
               col.regions = colorRampPalette(c("#E5FFCC", "#003300"))(20))
map3 <- spplot(NL.NDVI.Year, main = list("Yearly"), zcol = 'layer',
               col.regions = colorRampPalette(c("#E5FFCC", "#003300"))(20))

print(map1, position = c(0,.4,.5,1),more=T)
print(map2, position = c(.5,.4,1,1),more = T)
print(map3, position = c(0,0,1,.5))

grid.text("Greenest city in the Netherlands, by NDVI", x=unit(0.5, "npc"), y=unit(.95, "npc"), gp = gpar(fontsize=20))
grid.text(greenest.city.January$NAME_2, x=unit(0.15, "npc"), y=unit(0.55, "npc"), gp = gpar(fontsize=10))
grid.text(greenest.city.August$NAME_2, x=unit(0.65, "npc"), y=unit(0.55, "npc"), gp = gpar(fontsize=10))
grid.text(greenest.city.Year$NAME_2, x=unit(0.38, "npc"), y=unit(0.075, "npc"), gp = gpar(fontsize=10))
