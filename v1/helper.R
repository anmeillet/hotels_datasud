# fonction pour l'app shiny datasud

nettoie_nom_colonnes <- function(donnee_brute,code_commune='') {
  # s'il y a un code commune on peut rentrer le nom de la colonne et celle ci sera corrigée
    
  
  
    # recuperation des noms a changer
    nom_col <- names(donnee_brute)
    #nom_col <- tolower(gsub(" ","_",nom_col))
    nom_col <- gsub("_-_|__| _|_ ","_",nom_col)
    nom_col <- gsub("é|è|ê","e",nom_col)
    nom_col <- gsub("î","i",nom_col)
    nom_col <- gsub("n°_","",nom_col)
    
    #remplacer les euros s'il y en a
    donnee_brute <- data.frame(lapply(donnee_brute, function(x) {
      gsub(" €", "", x)
    }), stringsAsFactors = FALSE)
    
    #remplacer les % s'il y en a
    donnee_brute <- data.frame(lapply(donnee_brute, function(x) {
      gsub("%| %", "", x)
    }), stringsAsFactors = FALSE)
    
    #remplacer les , par des points s'il y en a
    donnee_brute <- data.frame(lapply(donnee_brute, function(x) {
      gsub(",", ".", x)
    }), stringsAsFactors = FALSE)
    
    names(donnee_brute)<-nom_col
    
    #change url name
    names(donnee_brute)[grepl("url|web",tolower(names(donnee_brute)))] <- "url"
    
    # correction code_commune
    if (code_commune != '') {
      donnee_brute[,code_commune] <- ifelse(nchar(donnee_brute[,code_commune])==4,paste0('0',donnee_brute[,code_commune]),donnee_brute[,code_commune])
    }
    
    return(donnee_brute)
  }
