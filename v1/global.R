#Page de visualisation des hotels datasud

#Check if packages used by app are installed, installs those not installed yet----
#list.of.packages <- c("shiny", "shinydashboard","shinyjs","leaflet","ggvis","dplyr","RColorBrewer","raster","gstat","rgdal","Cairo")
#new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
#if(length(new.packages)) install.packages(new.packages)

# load packages ----
require(shiny)
require(shinydashboard)
require(shinyjs)
require(leaflet)
require(ggvis)
require(dplyr)
library(RColorBrewer)
require(raster)
require(gstat)
require(rgdal)
require(Cairo)
source("helper.r")
# hotels <- read.csv("data/hotels-region-sud-apidae-reference.csv",stringsAsFactors=FALSE)
hotels <- read.csv("../data/hotels-region-sud-apidae-reference_nobooking.csv",stringsAsFactors=FALSE) #version sans une des url du site booking.com comprenant des ,

rgc <-read.csv("../data/rgc2011.csv",stringsAsFactors=FALSE, sep = ';') # source : https://public.opendatasoft.com/explore/dataset/rgc2011/table/

#prepare data ---- # TODO : CREER UNE COLONNE id UNIQUE SI N'EXISTE PAS
#suppression des colonnes avec des NA
hotels <- hotels[,apply(hotels, 2, function(x) { sum(!is.na(x)) > 0 })]
hotels$Longitude <- as.numeric(hotels$Longitude)
hotels$Latitude <- as.numeric(hotels$Latitude)
hotels$Altitude <- as.numeric(hotels$Altitude)

# hotels with no localisation and validated geo-referencing
#hotels[which(is.na(hotels$Latitude) & hotels$Géolocalisation.validée=="1"),]
# we keep only the hotels with validated geo referencing
# hotels <- hotels[which(hotels$Géolocalisation.validée=="1"),]

# on remplace les altitudes manquantes par l'altitude min (d'après le RGC de l'IGN)
rgc$codeInsee <- ifelse(nchar(rgc$COM)==1,
                        paste0(rgc$DEP,'00',rgc$COM),
                        ifelse(nchar(rgc$COM)==2,paste0(rgc$DEP,'0',rgc$COM),
                               paste0(rgc$DEP,rgc$COM))
)
hotels$Code.inse <- ifelse(nchar(hotels$Code.inse)<5,
                           paste0('0',hotels$Code.inse),
                           hotels$Code.inse)

hotels <-merge(hotels,rgc[,c("codeInsee","ZMIN")],by.x="Code.inse",by.y="codeInsee",all.x=T)

hotels$Altitude <- ifelse(is.na(hotels$Altitude),hotels$ZMIN,hotels$Altitude)
# on supprime les hotels qui n'ont pas d'altitude
hotels <- hotels[which(!(is.na(hotels$Altitude)) & hotels$Géolocalisation.validée=="1"),]
# on attribue l'altitude moyenne aux hotels qui n'ont pas d'altitude
#alt_moy <- mean(hotels$Altitude,na.rm=T)
#completion altitude (à ameliorer !!!)
#hotels[which(is.na(hotels$Altitude) & hotels$Géolocalisation.validée=="1"),"Altitude"] <- alt_moy

#table(hotels$Classement.HOT)
#on attribue un classement aux hotels sans info de classement
hotels[which(hotels$Classement.HOT=="" ),"Classement.HOT"] <- "pas de classement"

#simplify data
hotels <- hotels[,c("id","Nom","Commune","Téléphone","Classement.HOT","Altitude","Longitude","Latitude")]

hotels <- nettoie_nom_colonnes(hotels)

# transform to SpatialPointDataFrame
coordinates(hotels) <- ~ Longitude + Latitude
proj4string(hotels) <- "+init=epsg:4326"

# test affichage carte
# library(rworldmap)
# newmap <- getMap(resolution = "low")
# plot(newmap, xlim = c(4, 7), ylim = c(42, 46), asp = 1)
# points(hotels, col = "red", cex = .01)

liste_classement <- unique(hotels$Classement.HOT)[order(unique(hotels$Classement.HOT))]

# create dataset and base layer ----
datasets <- list(
  'Hotels'=hotels
)

baselayers <- list(
  'Hotels'='DarkMatter (CartoDB)'
)
