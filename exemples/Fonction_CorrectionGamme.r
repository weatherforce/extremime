# Fonction
#' #---------------------------------------------------------------------
#' #' @title Correction de gamme avec Date
#' #' @author Author Kelly grassi 
#' #' @date   v21/04/2019
#' #' @description corrects the varriables according to the sensor range and/or the expert range
#' #' @param data
#' #' @param ExistGamme  T if  Range exist
#' #' @param filenames_Gamme filenames to the sensor range and/or the expert range
#' #' @param sep separator in sensor range and/or the expert range file
#' #' @param dec decimal in sensor range and/or the expert range file
#' #' @param NamesColumnPara column name that contains all parameter names
#' 
#' CorrectionGamme<-function(data, ExistGamme  = T, filenames_Gamme, sep = ",", dec  = ".", NamesColumnPara ){
#'   
#'   print('correction gamme capteur'); 
#'   # Chargement de la gamme 
#'   gamme.df= read.csv(file = filenames_Gamme, sep = sep ,dec = dec)  # Importation de la Gamme capteur/Expert ATT
#'   
#'   ## Fusionner les gammes Expert et Capteur ##
#'   
#'   indGammeMin=grep(pattern="min",names(gamme.df))
#'   indGammeMax=grep(pattern="max",names(gamme.df))
#'   
#'   # Funsion des min #
#'   gamme.df$min=apply(gamme.df[,indGammeMin],MARGIN=1,FUN=function(x){out=NA;if(sum(!is.na(x))>0) out=max(x,na.rm=T);out})
#'   # Par defaut je retourne des NA
#'   # si j'ai des valeurs connues non NA la somme du test !is.na(x) sera positive,je retourne alors la valeur max des min 
#'   
#'   # Fusion des max #
#'   gamme.df$max=apply(gamme.df[,indGammeMax],MARGIN=1,FUN=function(x){out=NA; if(sum(!is.na(x))>0) out=min(x,na.rm=T); out})
#'   # Par defaut je retourne des NA
#'   # si j'ai des valeurs connues non NA la somme du test !is.na(x) sera positive,je retourne alors la valeur min des max 
#'   
#'   ## Correction  ##
#'   
#'   for(indDebut in unique(data$date_debut)) { 
#'     print(paste('step 1:',indDebut))
#'     indUniqueGamme=grep(pattern=indDebut,gamme.df$date_debut)
#'     gamme1=gamme.df[indUniqueGamme,]
#'     
#'     for(indfin in unique(data$date_fin)) { 
#'       print(paste('step 2',indfin))
#'       indUniqueGamme=grep(pattern=indfin,gamme1$date_fin)
#'       gamme=gamme1[indUniqueGamme,]
#'       
#'       if(nrow(gamme) !=0){
#'         
#'         indNoms=grep(pattern=NamesColumnPara,names(gamme))            
#'         infoNoms=unlist(gamme[indNoms])
#'         
#'         for (nomPara in names(data)){
#'           print(nomPara) 
#'           indErrone=NULL
#'           indPara=grep(pattern=nomPara,infoNoms);                        #recherche du parametre dans la gamme
#'           print(indPara)
#'           test=(length(indPara)==1);                                     # test : verifie l'existence de ce parametre avant de tenter une correction
#'           if(test){
#'             gmin=gamme$min[indPara]                                   #recherche si valeur inferieure au seuil alors on met NA
#'             if(!is.na(gmin)){
#'               data[na.omit(data[,nomPara]<gmin) & na.omit(data$temps%in%vecDate),nomPara]=NA # substitution des valeurs > ou < à la gamme  et correspondant aux dates de gamme par des NA
#'             }
#'             gmax=gamme$max[indPara]                                   #recherche si valeur superieure au seuil alors on met NA
#'             if(!is.na(gmax)){
#'               
#'               data[na.omit(data[,nomPara]>gmax) & na.omit(data$temps%in%vecDate),nomPara]=NA # substitution des valeurs > ou < à la gamme  et correspondant aux dates de gamme par des NA
#'             }
#'             print('a été corrigé')
#'           }
#'         }
#'       }
#'     }
#'   }
#'   return(data)
#' }
#' 


#Version precedente

#---------------------------------------------------------------------
#' @title Correction de gamme SANS DATE
#' @author Author Kelly grassi
#' @date   v12/04/2019
#' @description corrects the varriables according to the sensor range and/or the expert range
#' @param data
#' @param filenames_Gamme filenames to the sensor range and/or the expert range
#' @param sep separator in sensor range and/or the expert range file
#' @param dec decimal in sensor range and/or the expert range file
#' @param NamesColumnPara column name that contains all parameter names

CorrectionGamme<-function(data, filenames_Gamme, sep = ",", dec  = ".", NamesColumnPara ){

  print('correction gamme capteur');
  # Chargement de la gamme
  gamme.df= read.csv(file = filenames_Gamme, sep = sep ,dec = dec)  # Importation de la Gamme capteur/Expert ATT


  ## Fusionner les gammes Expert et Capteur ##

  indGammeMin=grep(pattern="min",names(gamme.df))
  indGammeMax=grep(pattern="max",names(gamme.df))

  # Funsion des min #
  gamme.df$min=apply(gamme.df[,indGammeMin],MARGIN=1,FUN=function(x){out=NA;if(sum(!is.na(x))>0) out=max(x,na.rm=T);out})
  # Par defaut je retourne des NA
  # si j'ai des valeurs connues non NA la somme du test !is.na(x) sera positive,je retourne alors la valeur max des min

  # Fusion des max #
  gamme.df$max=apply(gamme.df[,indGammeMax],MARGIN=1,FUN=function(x){out=NA; if(sum(!is.na(x))>0) out=min(x,na.rm=T); out})
  # Par defaut je retourne des NA
  # si j'ai des valeurs connues non NA la somme du test !is.na(x) sera positive,je retourne alors la valeur min des max

  ## Correction  ##

  indNoms=grep(pattern=NamesColumnPara,names(gamme.df))                     # att!! Data_Name
  infoNoms=unlist(gamme.df[indNoms])

  for (nomPara in names(data)){
    print(nomPara)
    indErrone=NULL
    indPara=grep(pattern=nomPara,infoNoms);                        #recherche du parametre dans la gamme
    print(indPara)
    test=(length(indPara)==1);                                     # test : verifie l'existence de ce parametre avant de tenter une correction
    if(test){
      gmin=gamme.df$min[indPara]                                   #recherche si valeur inferieure au seuil alors on met NA
      if(!is.na(gmin)){indErrone=which(data[,nomPara]<gmin)}
      gmax=gamme.df$max[indPara]                                   #recherche si valeur superieure au seuil alors on met NA
      if(!is.na(gmax)){indErrone=c(indErrone,which(data[,nomPara]>gmax))}

      data[indErrone,nomPara]=NA                                # substitution des valeurs > ou < à la gamme par des NA
      print('a été corrigé')
    }
  }
  return(data)
}