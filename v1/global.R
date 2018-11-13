#Page de visualisation des hotels datasud

#Check if packages used by app are installed, installs those not installed yet----
list.of.packages <- c("shiny", "shinydashboard","leaflet","ggvis","dplyr","RColorBrewer","gstat","rgdal","rAmCharts")
unused.packages <-c("raster","Cairo","shinyjs","plotly")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)

# load packages ----
lapply(list.of.packages, require, character.only = T)
source("helper.R")

# load data ----
hotels <- read.csv("../data/hotels-region-sud-apidae-reference.csv",stringsAsFactors=FALSE)
# hotels <- read.csv("../data/hotels-region-sud-apidae-reference_nobooking.csv",stringsAsFactors=FALSE) #version sans une des url du site booking.com comprenant des ,

rgc <-read.csv("../data/rgc2011.csv",stringsAsFactors=FALSE, sep = ';') # source : https://public.opendatasoft.com/explore/dataset/rgc2011/table/

#prepare data ---- # TODO : CREER UNE COLONNE id UNIQUE SI N'EXISTE PAS
#suppression des colonnes avec des NA
hotels <- hotels[,apply(hotels, 2, function(x) { sum(!is.na(x)) > 0 })]
hotels <- nettoie_nom_colonnes(hotels)

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
hotels <- hotels[which(!(is.na(hotels$Altitude)) & hotels$Geolocalisation.validee=="1"),]

#table(hotels$Classement.HOT)
#on attribue un classement aux hotels sans info de classement
hotels[which(hotels$Classement.HOT=="" ),"Classement.HOT"] <- "pas de classement"

# rajout de faux prix pour tester les filtres quanti (sinon on n'a que l'altitude)
hotels$Prix <- sample(50:500, size = nrow(hotels), replace = TRUE)
#ajout des départements pour tester les filtres quanti
hotels <-merge(hotels,rgc[,c("codeInsee","DEP")],by.x="Code.inse",by.y="codeInsee",all.x=T)

#simplify data ----
hotels <- hotels[,c("id","Nom","Commune","Telephone","Classement.HOT","Altitude","Longitude","Latitude","url","Prix","DEP")]



# row.names(hotels) <- hotels$id - fait dans le cadre des pop ups, mais ça marche sans.

# creation des listes pour les filtres qualitatifs
colQuali = names(which(unlist(lapply(hotels, is.character)))) # selectionne les colonnes de type non numerique
colQualiUnique <- aggregate(values ~ ind, unique(stack(hotels[,colQuali])), length)# selectionne les colonnes de type non numerique

QualiChoicesHotels <- list()
for (i in colQualiUnique[colQualiUnique$values<15,"ind"]){ # on  selectionne seulement les variables quali ayant moins de 15 valeurs uniques différentes 
  QualiChoicesHotels[[i]] <- as.list(
    unique(hotels[,i])[order(unique(hotels[,i]))]
  ) 
}

liste_classement <- unique(hotels$Classement.HOT)[order(unique(hotels$Classement.HOT))]
choices = as.list(liste_classement)

# transform to SpatialPointDataFrame
coordinates(hotels) <- ~ Longitude + Latitude
proj4string(hotels) <- "+init=epsg:4326"


# create dataset and base layer ----
datasets <- list(
  'Hotels'=hotels
)

baselayers <- list(
  'Hotels'='DarkMatter (CartoDB)'
)

QualiChoices <- list(
  'Hotels'=QualiChoicesHotels
)
