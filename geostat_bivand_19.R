
## ---- echo=TRUE----------------------------------------------------------
library(sf)
packageVersion("sf")


## ---- echo=TRUE----------------------------------------------------------
sf_extSoftVersion()


## ---- echo=TRUE----------------------------------------------------------
st_crs(22525)


## ---- echo=TRUE----------------------------------------------------------
cat(system("projinfo EPSG:22525", intern=TRUE), sep="\n")


## ---- echo=TRUE----------------------------------------------------------
cat(system("projinfo -s EPSG:22525 -t EPSG:31985", intern=TRUE), sep="\n")


## ---- echo=TRUE----------------------------------------------------------
olinda <- st_read("data/olinda.gpkg", quiet=TRUE)
st_crs(olinda)


## ---- echo=TRUE----------------------------------------------------------
xy_c <- st_centroid(st_geometry(olinda[  1,]))
st_coordinates(xy_c)


## ---- echo=TRUE----------------------------------------------------------
st_coordinates(st_transform(st_transform(xy_c, 4326), 31985))


## ---- echo=TRUE----------------------------------------------------------
# without CA7072_003.gsb
st_coordinates(st_transform(xy_c, 31985))


## ---- echo=TRUE, eval=FALSE----------------------------------------------
## # with CA7072_003.gsb
## st_coordinates(st_transform(xy_c, 31985))
## #          X       Y
## # 1 295489.3 9120352


## ---- echo=TRUE, eval=FALSE----------------------------------------------
## # with CA7072_003.gsb
## xy_c1 <- xy_c
## st_crs(xy_c1) <- "+proj=utm +zone=25 +south +ellps=intl +units=m +nadgrids=CA7072_003.gsb"
## print(st_coordinates(st_transform(xy_c1, 31985)), digits=9)
## #            X          Y
## # 1 295486.396 9120350.62


## ---- echo=TRUE, eval=FALSE----------------------------------------------
## # with CA7072_003.gsb
## cat(system(paste0("echo ", paste(xy, collapse=" "), " | cs2cs EPSG:22525 EPSG:31985"), intern=TRUE))
## # 295486.40 9120350.62 0.00


## ---- echo=TRUE----------------------------------------------------------
xy <- st_coordinates(xy_c)
# without CA7072_003.gsb
cat(system(paste0("echo ", paste(xy, collapse=" "), " | cs2cs EPSG:22525 EPSG:31985"), intern=TRUE))


## ---- echo=TRUE, warning=FALSE-------------------------------------------
# without CA7072_003.gsb
xy_c2 <- xy_c
st_crs(xy_c2) <- "+proj=utm +zone=25 +south +ellps=intl +units=m +towgs84=-206.05,168.28,-3.82,0,0,0,0"
st_coordinates(st_transform(xy_c2, 31985))


## ---- echo=TRUE----------------------------------------------------------
# without CA7072_003.gsb
# -DACCEPT_USE_OF_DEPRECATED_PROJ_API_H
st_coordinates(lwgeom::st_transform_proj(xy_c, 31985))


## ---- echo=TRUE----------------------------------------------------------
rgeos::version_GEOS0()


## ---- echo=TRUE, warning=FALSE-------------------------------------------
cV_old_default <- ifelse(rgeos::version_GEOS0() >= "3.7.2", 0L, FALSE)
yy <- rgeos::readWKT(readLines("data/invalid.wkt"))
rgeos::gIsValid(yy, byid=TRUE, reason=TRUE)


## ---- echo=TRUE----------------------------------------------------------
sf::sf_extSoftVersion()


## ---- echo=TRUE----------------------------------------------------------
sf::st_is_valid(sf::st_as_sf(yy), reason=TRUE)


## ---- echo=TRUE, warning=FALSE-------------------------------------------
ply <- rgeos::readWKT(readLines("data/ply.wkt"))
oo <- try(rgeos::gIntersection(yy, ply, byid=TRUE, checkValidity=cV_old_default), silent=TRUE)
print(attr(oo, "condition")$message)

## ---- echo=TRUE----------------------------------------------------------
ooo <- try(sf::st_intersection(sf::st_as_sf(yy), sf::st_as_sf(ply)), silent=TRUE)
print(attr(oo, "condition")$message)


## ---- echo=TRUE----------------------------------------------------------
cV_new_default <- ifelse(rgeos::version_GEOS0() >= "3.7.2", 1L, TRUE)
try(rgeos::gIntersection(yy, ply, byid=TRUE, checkValidity=cV_new_default), silent=TRUE)


## ---- echo=TRUE----------------------------------------------------------
oo <- rgeos::gIntersection(yy, ply, byid=TRUE, checkValidity=2L)
rgeos::gIsValid(oo)


## ---- echo=TRUE----------------------------------------------------------
oo <- rgeos::gIntersection(rgeos::gBuffer(yy, byid=TRUE, width=0), ply, byid=TRUE, checkValidity=1L)
rgeos::gIsValid(oo)


## ---- echo=TRUE----------------------------------------------------------
ooo <- sf::st_intersection(sf::st_buffer(sf::st_as_sf(yy), dist=0), sf::st_as_sf(ply))
all(sf::st_is_valid(ooo))


## ---- echo=TRUE----------------------------------------------------------
buildings <- sf::st_read("data/buildings.gpkg", quiet=TRUE)
st_crs(buildings)


## ---- echo=TRUE----------------------------------------------------------
library(mapview)
mapview(buildings)


## ---- echo=TRUE----------------------------------------------------------
library(RSQLite)
db = dbConnect(SQLite(), dbname="data/buildings.gpkg")
dbReadTable(db, "gpkg_spatial_ref_sys")$definition[4]
dbDisconnect(db)


## ---- echo=TRUE----------------------------------------------------------
buildings1 <- rgdal::readOGR("data/buildings.shp", verbose=FALSE)
sp::proj4string(buildings1)


## ---- echo=TRUE----------------------------------------------------------
mapview(buildings1)


## ---- echo=TRUE, warning=FALSE-------------------------------------------
readLines("data/buildings.prj")


## ---- echo=TRUE, warning=FALSE-------------------------------------------
fixed <- "+proj=tmerc +lat_0=49 +lon_0=-2 +k=0.9996012717 +x_0=400000 +y_0=-100000 +ellps=airy +nadgrids=OSTN15_NTv2_OSGBtoETRS.gsb +units=m +no_defs"
st_crs(buildings) <- fixed
sp::proj4string(buildings1) <- sp::CRS(fixed)


## ---- echo=TRUE----------------------------------------------------------
mapview(buildings)


## ---- echo=TRUE----------------------------------------------------------
mapview(buildings1)


## ---- echo = TRUE, cache=TRUE, warning=FALSE, results="hide"-------------
cran <- available.packages()
library(ctv)
obj1 <- read.ctv(system.file("ctv/Spatial.ctv", package="ctv"))
obj2 <- read.ctv(system.file("ctv/SpatioTemporal.ctv", package="ctv"))
sp_ctv_pkgs <- sort(unique(c(obj1$packagelist$name, obj2$packagelist$name)))
pdb <- cran[cran[, "Package"] %in% sp_ctv_pkgs, ]


## ---- echo = TRUE, cache=TRUE, warning=FALSE-----------------------------
suppressPackageStartupMessages(library(miniCRAN))
suppressPackageStartupMessages(library(igraph))
suppressPackageStartupMessages(library(magrittr))
pg <- makeDepGraph(pdb[, "Package"], availPkgs = cran, suggests=TRUE, enhances=TRUE, includeBasePkgs = FALSE)


## ---- echo = TRUE, cache=TRUE--------------------------------------------
pr <- pg %>%
page.rank(directed = FALSE) %>%
use_series("vector") %>%
sort(decreasing = TRUE) %>%
as.matrix %>%
set_colnames("page.rank")


## ---- echo = TRUE, cache=TRUE--------------------------------------------
print(pr[1:30,], digits=4)

## ---- echo = FALSE, eval=TRUE, mysize=TRUE, size='\\tiny', cache=TRUE, results="hide"----
cutoff <- quantile(pr[, "page.rank"], probs = 0.2)
popular <- pr[pr[, "page.rank"] >= cutoff, ]
toKeep <- names(popular)
vids <- V(pg)[toKeep]
gs <- induced.subgraph(pg, vids = toKeep)
cl <- walktrap.community(gs, steps = 3)


## ---- echo = TRUE, cache=TRUE--------------------------------------------
topClusters <- table(cl$membership) %>%
sort(decreasing = TRUE) %>%
head(25)
cluster <- function(i, clusters, pagerank, n=10){
group <- clusters$names[clusters$membership == i]
pagerank[group, ] %>% sort(decreasing = TRUE) %>% head(n)
}
z <- lapply(names(topClusters)[1:15], cluster, clusters=cl, pagerank=pr, n=20)


## ---- echo = TRUE, cache=TRUE--------------------------------------------
z[[1]][1:20]


## ---- echo = TRUE, cache=TRUE--------------------------------------------
z[[2]][1:20]


## ---- echo = TRUE, cache=TRUE--------------------------------------------
z[[3]][1:20]


## ---- echo = TRUE, cache=TRUE--------------------------------------------
z[[4]][1:20]


## ---- echo=TRUE, warning=FALSE-------------------------------------------
library(RColorBrewer)
library(wordcloud)
opar <- par(mar=c(0,0,0,0)+0.1, mfrow=c(1,2))
for (i in 1:2) wordcloud(names(z[[i]]), freq=unname(z[[i]]), scale=rev(8*range(unname(z[[i]]))/max(unname(z[[4]]))))
par(opar)


## ---- echo=TRUE, warning=FALSE-------------------------------------------
opar <- par(mar=c(0,0,0,0)+0.1, mfrow=c(1,2))
for (i in 3:4) wordcloud(names(z[[i]]), freq=unname(z[[i]]), scale=rev(8*range(unname(z[[i]]))/max(unname(z[[4]]))))
par(opar)

