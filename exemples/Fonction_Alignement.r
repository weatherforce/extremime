#TIME STEP
#---------------------------------------------------------------------
#' @title compute mean time step
#' @author Author Kelly grassi & Emilie Poisson Caillault 
#' @date   v12/04/2019
#' @description compute mean time step for each variable
#' @param data

TimeStep.mean<-function(data){
  
  ##Etude par parametre :mean
  indPara=(max(indMetaDonnees)+1):(ncol(data)-1);
  
  indPara=1:(ncol(data)-1)
  
  
  ecart.mean=apply(data[,indPara],MARGIN=2,FUN=function(x){
    print(head(x))
    indNA=which(is.na(x));
    
    if(length(indNA) == 0){
      indNA=NULL;
      temps=data$temps;
    }else{
      indNA=indNA
      temps=data$temps[-indNA];
    }
    
    if(length(indNA) == length(x)){
      deltat.median ='empty vector'
    }else{
      Ntemps=length(temps);
      deltat = abs(as.numeric(temps[2:Ntemps]-temps[1:(Ntemps-1)],units="secs"));  # calcul pas de temps (dt) (Valeur absolu et automatiquement en segonde)
      deltat.mean= mean(deltat,na.rm=T)/60;
      deltat.mean
    }
  })
  print("ecart moyen par parametres")
  print(ecart.mean)
  
  return(deltat.mean)
  
}


#---------------------------------------------------------------------
#' @title compute median time step
#' @author Author Kelly grassi & Emilie Poisson Caillault 
#' @date   v12/04/2019
#' @description compute median time step for each variable
#' @param data

TimeStep.median<-function(data){
  
  
  ##Etude par parametre :median
  indPara=1:(ncol(data)-1);
  ecart.median=apply(data[,indPara],MARGIN=2,FUN=function(x){
    indNA=which(is.na(x));
    
    if(length(indNA) == 0){
      indNA=NULL;
      temps=data$temps;
    }else{
      indNA=indNA
      temps=data$temps[-indNA];
    }
    if(length(indNA) == length(x)){
      deltat.median ='empty vector'
    }else{
      Ntemps=length(temps);
      deltat = abs(as.numeric(temps[2:Ntemps]-temps[1:(Ntemps-1)],units="secs"));  # calcul pas de temps (dt) (Valeur absolu et automatiquement en segonde)
      deltat.median= median(deltat,na.rm=T)/60;
      deltat.median
    }
  })
  print("ecart median par parametres")
  print(ecart.median)
  
  return(ecart.median)
}


#---------------------------------------------------------------------
#' @title time step
#' @author Author Kelly grassi & Emilie Poisson Caillault 
#' @date   v12/04/2019
#' @description define time step, auto or forced operation 
#' @param data

compute.TimeSetp<-function(ecart, dtAuto= T, dtForce=NULL){
  
  ecart= as.numeric(ecart)
  ecart= na.omit(ecart)
  tabAligne=c(1,10,20,30,60,60*24,60*24*7,60*24*7*30,60*24*7*30*12,60*24*2)                   # table d'alignement
  
  if(dtAuto == T){
    com= tabAligne %in% round(ecart)
    dtA=min(grep(pattern = "TRUE", com))
  } else {dtA=dtForce}
  
  return(dtA)
}


#ALIGNMENT

#---------------------------------------------------------------------
#' @title Time Alignment
#' @author Author Kelly grassi & Emilie Poisson Caillault  
#' @date   v12/04/2019
#' @description aggregates the data according to a regular time step
#' @param data
#' @param dtA time step 
#' 1: par 1 min
#' 2: par 10 min
#' 3: par 20 min
#' 4: par 30 minutes
#' 5: par heures
#' 6: par jours
#' 7: par semaine
#' 8: par mois
#' 9: par annees.
#' 10 : par 2 jours

TimeAlign<-function(data, dtA){
  #Fonction 
  print('ALignement temporel')
  
  
  ## par 2Jours ##
  
  if(dtA == 10){
    
    print('par 2jours')
    data$temps=strptime(data$temps,format="%Y-%m-%d",tz="GMT")       
    
    min.date=min(data$temps,na.rm=T)
    max.date=max(data$temps,na.rm=T)
    a=seq(min.date,max.date,by="2 day")                                            # Creation d'un vecteur model pour l'alignement
    d=format(a,"%F")
    
    df2=data.frame(temps=strptime(d,format="%Y-%m-%d",tz="GMT"))                            # Creation du tableau, en y incluant seulement le vecteur "a".
    df1=merge(data,df2, all=TRUE)                                               # fusion du tableau avec le tableau de donnée brute initial.
    
    classe=sapply(df1, FUN=class)
    indNumPara=which(classe == "numeric")                                             # indice paramètres numeriques
    indIntPara=which(classe =="integer")                                              # indice paramètres entiers
    indPara=c(indNumPara,indIntPara)                                                  # indice paramètres
    
    
    df1.D=aggregate(df1[,indPara],by=list(temps=as.character(df1$temps)),
                    FUN=function(x){ out=NA; if(sum(is.na(x))<length(x))
                    {out=max(x,na.rm=T);}; out})
    
    df1.D$temps=strptime(df1.D$temps,format="%Y-%m-%d",tz="GMT")
    dfa=df1.D
    
    rm(min.date); rm(max.date); rm(a); rm(df2); rm(df1); rm(indNumPara); rm(indIntPara); rm(indPara); rm(df1.D)   #Suppressions des données inutiles pour la suite.
  }
  
  
  
  ## Par années ## 
  if(dtA == 9){
    print('par années')
    
    data$temps=strptime(format(data$temps, "%Y-06-15"),format="%Y-%m-%d",tz="GMT")       # Toutes les donnees de l'annee sont assignees au 15/06 de l'annee  
    
    min.date=min(data$temps,na.rm=T)
    max.date=max(data$temps,na.rm=T)
    a=seq(min.date,max.date,by="1 years")                                             # Creation d'un vecteur model pour l'alignement
    
    df2=data.frame(temps=as.POSIXct(a),tz="GMT")                                               # Creation du tableau, en y incluant seulement le vecteur "a".
    df1=merge(data,df2, all=TRUE)                                                  # fusion du tableau avec le tableau de donnée brute initial.
    
    classe=sapply(df1, FUN=class)
    indNumPara=which(classe == "numeric")                                             # indice paramètres numeriques
    indIntPara=which(classe =="integer")                                              # indice paramètres entiers
    indPara=c(indNumPara,indIntPara)                                                  # indice paramètres
    
    df1.Y=aggregate(df1[,indPara],by=list(temps=as.character(df1$temps)),
                    FUN=function(x){ out=NA; if(sum(is.na(x))<length(x))
                    {out=max(x,na.rm=T);}; out})
    
    df1.Y$temps=strptime(df1.Y$temps,format="%Y-%m-%d",tz="GMT")
    dfa=df1.Y
    
    rm(min.date); rm(max.date); rm(a); rm(df2); rm(df1); rm(indNumPara); rm(indIntPara); rm(indPara); rm(df1.Y)    #Suppressions des données inutiles pour la suite.
  }
  
  ## par Mois ##
  if(dtA == 8){
    print('par mois')
    data$temps=strptime(format(data$temps, "%Y-%m-15"),format="%Y-%m-%d",tz="GMT")      # Toutes les donnee du mois sont assignees au jour 15                                    # en sortie le max de l'intervalle de temps
    
    min.date=min(data$temps,na.rm=T)
    max.date=max(data$temps,na.rm=T)
    a=seq(min.date,max.date,by="1 month")                                            # Creation d'un vecteur model pour l'alignement
    
    df2=data.frame(temps=as.POSIXct(a),tz="GMT")                                     # Creation du tableau, en y incluant seulement le vecteur "a".
    df1=merge(data,df2, all=TRUE)                                                 # fusion du tableau avec le tableau de donnée brute initial.
    
    classe=sapply(df1, FUN=class)
    indNumPara=which(classe == "numeric")                                             # indice paramètres numeriques
    indIntPara=which(classe =="integer")                                              # indice paramètres entiers
    indPara=c(indNumPara,indIntPara)                                                  # indice paramètres
    
    df1.M=aggregate(df1[,indPara],by=list(temps=as.character(df1$temps)),
                    FUN=function(x){ out=NA; if(sum(is.na(x))<length(x))
                    {out=max(x,na.rm=T);}; out})
    
    df1.M$temps=strptime(df1.M$temps,format="%Y-%m-%d",tz="GMT")
    dfa=df1.M
    
    rm(min.date); rm(max.date); rm(a); rm(df2); rm(df1); rm(indNumPara); rm(indIntPara); rm(indPara); rm(df1.M)   #Suppressions des données inutiles pour la suite.
  }
  
  ## par Semaine ##
  if(dtA == 7){
    print('par semaine')
    
    annees=format(data$temps,"%Y")
    numJour=as.numeric(strftime(data$temps, "%j"))                                                       # numéro du jour
    numJourSemaine=seq(1,364,by=7)                                                                          # vecteur numéro par semaine
    numJourT=sapply(numJour,function(x){index=max(which(x>=numJourSemaine));return(numJourSemaine[index])}) # remplace le numero de jour par le numero de la semaine correspondante
    data$temps=strptime(paste(numJourT,annees,sep=" "), "%j %Y",tz="GMT")
    
    min.date=min(data$temps,na.rm=T)
    max.date=max(data$temps,na.rm=T)
    d=seq(min.date,max.date,by="1 week")                                                                     # Creation d'un vecteur model pour l'alignement
    
    df2=data.frame(temps=strptime(d,format="%Y-%m-%d",tz="GMT"))                                             # Creation du tableau, en y incluant seulement le vecteur "a"
    df1=merge(data,df2, by=intersect(names(data),names(df2)),all=TRUE)                                 # fusion du tableau avec le tableau de donnée brute initial
    
    classe=sapply(df1, FUN=class)
    indNumPara=which(classe == "numeric")                                             # indice paramètres numeriques
    indIntPara=which(classe =="integer")                                              # indice paramètres entiers
    indPara=c(indNumPara,indIntPara)                                                  # indice paramètres
    
    df1.W=aggregate(df1[,indPara],by=list(temps=as.character(df1$temps)),
                    FUN=function(x){ out=NA; if(sum(is.na(x))<length(x))
                    {out=max(x,na.rm=T);}; out})
    
    df1.W$temps=strptime(df1.W$temps,format="%Y-%m-%d",tz="GMT")
    dfa=df1.W
    
    rm(df2); rm(df1); rm(indNumPara); rm(indIntPara); rm(indPara); rm(df1.W); rm(d);  rm(date)
  }
  
  ## par Jours ##
  
  if(dtA == 6){
    
    print('par jour')
    data$temps=strptime(data$temps,format="%Y-%m-%d",tz="GMT")       
    
    min.date=min(data$temps,na.rm=T)
    max.date=max(data$temps,na.rm=T)
    a=seq(min.date,max.date,by="1 day")                                            # Creation d'un vecteur model pour l'alignement
    d=format(a,"%F")
    
    df2=data.frame(temps=strptime(d,format="%Y-%m-%d",tz="GMT"))                            # Creation du tableau, en y incluant seulement le vecteur "a".
    df1=merge(data,df2, all=TRUE)                                               # fusion du tableau avec le tableau de donnée brute initial.
    
    classe=sapply(df1, FUN=class)
    indNumPara=which(classe == "numeric")                                             # indice paramètres numeriques
    indIntPara=which(classe =="integer")                                              # indice paramètres entiers
    indPara=c(indNumPara,indIntPara)                                                  # indice paramètres
    
    
    df1.D=aggregate(df1[,indPara],by=list(temps=as.character(df1$temps)),
                    FUN=function(x){ out=NA; if(sum(is.na(x))<length(x))
                    {out=max(x,na.rm=T);}; out})
    
    df1.D$temps=strptime(df1.D$temps,format="%Y-%m-%d",tz="GMT")
    dfa=df1.D
    
    rm(min.date); rm(max.date); rm(a); rm(df2); rm(df1); rm(indNumPara); rm(indIntPara); rm(indPara); rm(df1.D)   #Suppressions des données inutiles pour la suite.
  }
  
  ## par Heures ##
  if(dtA == 5){
    print('par heures')
    data$temps=strptime(format(data$temps, "%F %H:00:00"),format="%Y-%m-%d %H:%M:%S",tz="GMT")  #formatees telles que minutes et secondes considerees nulles 
    
    min.date=min(data$temps,na.rm=T)
    max.date=max(data$temps,na.rm=T)
    a=seq(min.date,max.date,by="1 hour")                                             # Creation d'un vecteur model pour l'alignement
    
    df2=data.frame(temps=strptime(a,format="%Y-%m-%d %H:%M:%S",tz="GMT"))                     # Creation du tableau, en y incluant seulement le vecteur "a".
    df1=merge(data,df2, all=TRUE)                                                 # fusion du tableau avec le tableau de donnée brute initial.
    
    classe=sapply(df1, FUN=class)
    indNumPara=which(classe == "numeric")                                             # indice paramètres numeriques
    indIntPara=which(classe =="integer")                                              # indice paramètres entiers
    indPara=c(indNumPara,indIntPara)                                                  # indice paramètres
    
    
    df1.H=aggregate(df1[,indPara],by=list(temps=as.character(df1$temps)),
                    FUN=function(x){ out=NA; if(sum(is.na(x))<length(x))
                    {out=max(x,na.rm=T);}; out})
    
    df1.H$temps=strptime(df1.H$temps,format="%Y-%m-%d %H:%M:%S",tz="GMT")
    dfa=df1.H
    
    rm(min.date); rm(max.date); rm(a); rm(df2); rm(df1); rm(indNumPara); rm(indIntPara); rm(indPara); rm(df1.H)   #Suppressions des données inutiles pour la suite.
  }
  
  ## Par 30 minute ##
  if(dtA == 4){
    
    print('par 30 minute')
    
    date=format(data$temps,"%F %H")
    minutes=as.numeric(format(data$temps,"%M"))
    minute=rep("45",length(minutes));
    minute[minutes<30]="15"                                                         # [hh:00, hh:29[= hh:15 et [hh:30, hh:59[= hh:45.
    d=paste(date,minute,"00",sep=":")                                               # d : nouveau vecteur date
    data$temps=strptime(d,format="%Y-%m-%d %H:%M:%S",tz="GMT")                   # formatees selon d 
    
    min.date=min(data$temps,na.rm=T)
    max.date=max(data$temps,na.rm=T)
    a=seq(min.date,max.date,by="30 mins")                                           # Creation d'un vecteur model pour l'alignement
    
    df2=data.frame(temps=strptime(a,format="%Y-%m-%d %H:%M:%S",tz="GMT"))           # Creation du tableau, en y incluant seulement le vecteur "a".
    df1=merge(data,df2, all=TRUE)                                                # fusion du tableau avec le tableau de donnée brute initial.
    
    classe=sapply(df1, FUN=class)
    indNumPara=which(classe == "numeric")                                            # indice paramètres numeriques
    indIntPara=which(classe =="integer")                                             # indice paramètres entiers
    indPara=c(indNumPara,indIntPara)                                                 # indice paramètres
    
    df30=aggregate(df1[,indPara],by=list(temps=as.character(df1$temps)),
                   FUN=function(x){ out=NA; if(sum(is.na(x))<length(x))
                   {out=max(x,na.rm=T);}; out})
    
    df30$temps=strptime(df30$temps,format="%Y-%m-%d %H:%M:%S",tz="GMT")
    dfa=df30
    
    rm(min.date); rm(max.date); rm(a); rm(df2); rm(df1); rm(indNumPara); rm(indIntPara); rm(indPara); rm(df30)   #Suppressions des données inutiles pour la suite.
  }
  
  ## Par 20 minute ##
  if(dtA == 3){
    
    print('par 20 minutes')
    
    date=format(data$temps,"%F %H")
    minutes=as.numeric(format(data$temps,"%M"))
    minute=rep("50",length(minutes));                                               # [hh:40, hh:59[= hh:50
    minute[minutes<40]="30"                                                         # [hh:20, hh:39[= hh:30. 
    minute[minutes<20]="10"                                                         # [hh:00, hh:19[= hh:10
    d=paste(date,minute,"00",sep=":")                                               # d : nouveau vecteur date
    data$temps=strptime(d,format="%Y-%m-%d %H:%M:%S",tz="GMT")                   # formatees selon d  
    # avec GMT (temps UTC) pour ne pas avoir de souci de conversion (CET<->CEST)
    
    min.date=min(data$temps,na.rm=T)
    max.date=max(data$temps,na.rm=T)
    a=seq(min.date,max.date,by="20 mins") # Creation d'un vecteur model pour l'alignement                                                
    
    
    df2=data.frame(temps=strptime(a,format="%Y-%m-%d %H:%M:%S",tz="GMT"))           # Creation du tableau, en y incluant seulement le vecteur "a".
    df1=merge(data,df2, all=TRUE)                                                # fusion du tableau avec le tableau de donnée brute initial.
    
    classe=sapply(df1, FUN=class)
    indNumPara=which(classe == "numeric")                                             # indice paramètres numeriques
    indIntPara=which(classe =="integer")                                              # indice paramètres entiers
    indPara=c(indNumPara,indIntPara)                                                  # indice paramètres
    
    df20=aggregate(df1[,indPara],by=list(temps=as.character(df1$temps)),
                   FUN=function(x){ out=NA; if(sum(is.na(x))<length(x))
                   {out=max(x,na.rm=T);}; out})
    
    df20$temps=strptime(df20$temps,format="%Y-%m-%d %H:%M:%S",tz="GMT")
    dfa=df20
    
    rm(min.date); rm(max.date); rm(a); rm(df2); rm(df1); rm(indNumPara); rm(indIntPara); rm(indPara); rm(df20)   #Suppressions des données inutiles pour la suite.
  }
  
  ## Par 10 minute ##
  if(dtA == 2){ 
    
    print('par 10 minute')
    date=format(data$temps,"%F %H")
    minutes=as.numeric(format(data$temps,"%M"))
    minute=rep("55",length(minutes));                                               # [hh:50, hh:59[= hh:55
    minute[minutes<50]="45"                                                         # [hh:40, hh:49[= hh:45. 
    minute[minutes<40]="35"                                                         # [hh:30, hh:39[= hh:35
    minute[minutes<30]="25"                                                         # [hh:20, hh:29[= hh:25
    minute[minutes<20]="15"                                                         # [hh:10, hh:19[= hh:15
    minute[minutes<20]="5"                                                          # [hh:00, hh:09[= hh:5
    d=paste(date,minute,"00",sep=":")                                               # d : nouveau vecteur date
    data$temps=strptime(d,format="%Y-%m-%d %H:%M:%S",tz="GMT")                            # formatees selon d 
    
    min.date=min(data$temps,na.rm=T)
    max.date=max(data$temps,na.rm=T)
    a=seq(min.date,max.date,by="10 mins")                                           # Creation d'un vecteur model pour l'alignement                                              
    
    df2=data.frame(temps=strptime(a,format="%Y-%m-%d %H:%M:%S",tz="GMT"))                    # Creation du tableau, en y incluant seulement le vecteur "a".
    df1=merge(data,df2, all=TRUE)                                                # fusion du tableau avec le tableau de donnée brute initial.
    
    classe=sapply(df1, FUN=class)
    indNumPara=which(classe == "numeric")                                             # indice paramètres numeriques
    indIntPara=which(classe =="integer")                                              # indice paramètres entiers
    indPara=c(indNumPara,indIntPara)                                                  # indice paramètres
    
    df10=aggregate(df1[,indPara],by=list(temps=as.character(df1$temps)),
                   FUN=function(x){ out=NA; if(sum(is.na(x))<length(x))
                   {out=max(x,na.rm=T);}; out})
    
    df10$temps=strptime(df10$temps,format="%Y-%m-%d %H:%M:%S",tz="GMT")
    dfa=df10
    
    rm(min.date); rm(max.date); rm(a); rm(df2); rm(df1); rm(indNumPara); rm(indIntPara); rm(indPara); rm(df10)   #Suppressions des données inutiles pour la suite.
  }
  
  ## Par 1min ## 
  if(dtA == 1){
    
    print('par minutes')
    data$temps=strptime(format(data$temps, "%F %H:%M:00"),format="%Y-%m-%d %H:%M:%S",tz="GMT")  #formatees telles que secondes considerees nulles 
    
    min.date=min(data$temps,na.rm=T)
    max.date=max(data$temps,na.rm=T)
    a=seq(min.date,max.date,by="1 mins")                                              # Creation d'un vecteur model pour l'alignement
    
    df2=data.frame(temps=strptime(a,format="%Y-%m-%d %H:%M:%S",tz="GMT"))                      # Creation du tableau, en y incluant seulement le vecteur "a".
    df1=merge(data,df2, all=TRUE)                                                  # fusion du tableau avec le tableau de donnée brute initial.
    
    classe=sapply(df1, FUN=class)
    indNumPara=which(classe == "numeric")                                             # indice paramètres numeriques
    indIntPara=which(classe =="integer")                                              # indice paramètres entiers
    indPara=c(indNumPara,indIntPara)                                                  # indice paramètres
    
    df1.min=aggregate(df1[,indPara],by=list(temps=as.character(df1$temps)),
                      FUN=function(x){ out=NA; if(sum(is.na(x))<length(x))
                      {out=max(x,na.rm=T);}; out})
    
    df1.min$temps=strptime(df1.min$temps,format="%Y-%m-%d %H:%M:%S",tz="GMT")
    dfa=df1.min
    
    rm(min.date); rm(max.date); rm(a); rm(df2); rm(df1); rm(indNumPara); rm(indIntPara); rm(indPara); rm(df1.min)   #Suppressions des données inutiles pour la suite.
  }
  
  ## par Mois ##
  if(dtA == 'SRN'){
    print('par mois')
    data$temps=strptime(format(data$temps, "%Y-%m-15"),format="%Y-%m-%d",tz="GMT")      # Toutes les donnee du mois sont assignees au jour 15                                    # en sortie le max de l'intervalle de temps
    
    min.date=min(data$temps,na.rm=T)
    max.date=max(data$temps,na.rm=T)
    a=seq(min.date,max.date,by="1 month")                                            # Creation d'un vecteur model pour l'alignement
    
    df2=data.frame(temps=as.POSIXct(a),tz="GMT")                                     # Creation du tableau, en y incluant seulement le vecteur "a".
    df1=merge(data,df2, all=TRUE)                                                 # fusion du tableau avec le tableau de donnée brute initial.
    
    classe=sapply(df1, FUN=class)
    indNumPara=which(classe == "numeric")                                             # indice paramètres numeriques
    indIntPara=which(classe =="integer")                                              # indice paramètres entiers
    indPara=c(indNumPara,indIntPara)
    
    selectPara1='Chla_microg_l'
    selectPara2='OxyDissolved_mg_l'

    indPara1=grep('Chla_microg_l', names(df1))                                           # indice paramètres pour critère merge 1
    indPara2=grep('OxyDissolved_mg_l', names(df1))                                            # indice paramètres pour critère merge 2
    
    df1.MaxChl=aggregate(df1[, indPara],by=list(temps=as.character(df1$temps)),
                         FUN=function(x){ out=NA; if(sum(is.na(x))<length(x))
                         {out=max(x,na.rm=T);}; out})
    
    df.max= merge(df1,df1.MaxChl, by= c('temps','Chla_microg_l'))
    df.max=df.max[,1:ncol(df1)] #recupere les namesFiles.x
    
    names(df.max)= c(names(df.max[,c(1,2)]),names(df1[,-c(1,indPara1)])) #1:indTemps , 3:indPara1
    
    df1.MinOx=aggregate(df1[, indPara],by=list(temps=as.character(df1$temps)),
                         FUN=function(x){ out=NA; if(sum(is.na(x))<length(x))
                         {out=min(x,na.rm=T);}; out})
    
    df.minmax= merge(df.max,df1.MinOx, by= c('temps','OxyDissolved_mg_l'),all.x = T , all.y = F)
    df.minmax=df.minmax[,1:ncol(df1)] #recupere les namesFiles.x
    
    names(df.minmax)= c(names(df.minmax[,c(1,2)]),names(df.max[,-c(1,indPara2)])) #1:indTemps , 3:indPara1
    
    #Supression empirique des derniers doublons
     df.minmax=df.minmax[!duplicated(df.minmax$temps,fromLast=F),] 
     dfa=df.minmax[,-length(df.minmax)]
     
    
    rm(min.date); rm(max.date); rm(a); rm(df2); rm(df1); rm(indNumPara); rm(indIntPara); rm(indPara); rm(df1.MaxChl); rm(df1.MinOx); rm()   #Suppressions des données inutiles pour la suite.
  }
  
  
  
  return(dfa)
}

