library("clValid")
library("class")
library("uHMM")

##  Fonction

#' Compute Kmeans with K-detection by Elbow criteria with reduced dichotomy process
#' @author Emilie Poisson Caillault v2019/04/16
#' @date   v17/04/2019
#' @description 
#' @param features data
#' @param Kmax max K medoid numbers for reduction
#' @param StopCriteria   explained variance for stop reduction. Default= 0.99 
#' @param graph graphic display
#' 
compute.KmeansAutoElbow<-function(features, Kmax, StopCriteria = 0.99, graph = FALSE) {
  #retrait des éléments/lignes identiques
  features=dplyr::distinct(as.data.frame(features))
  N = nrow(features)
  Kmax=min(Kmax,N-1)
  Within = rep(-1, Kmax)
  explainedVar = rep(-1, Kmax)
  fastCenters <- NULL
  start = 0
  end = Kmax;
  i = (end + start)/2
  while (abs(i - end) != 0) {
    fastCenters <- NULL
    fastCenters = i
    Res.km <- kmeans(features, centers = fastCenters, iter.max = 200, nstart = 5, algorithm = c("Hartigan-Wong"))
    if(Res.km$ifault==4){
      Res.km=kmeans(features, centers = fastCenters, iter.max = 200, algorithm = c("MacQueen"))
    }
    Within[i] <- Res.km$tot.withinss
    explainedVar[i] = Res.km$betweenss/Res.km$totss
    if (explainedVar[i] > StopCriteria) {
      end = i
      i = round((end + start)/2)
    }
    else {
      start = i
      i = round((end + start)/2)
    }
    if (abs(i - end) == 1) {
      end = i
    }
  }
  K = length(unique(Res.km$cluster))
  return(list(K = K, res.kmeans = Res.km))
}

#---------------------------------------------------------------------
#' @title Compute cluster numbers
#' @author Author Kelly grassi & Emilie Poisson Caillault 
#' @date   v17/04/2019
#' @description 
#' @param similarity similarity matrix
#' @param Kmax  max K medoid numbers for reduction
#' @param ev.min eigen values min Default 0.7
#' @param graphic plot

compute.nbClusterGap<-function (similarity, Kmax=NULL, ev.min=0.7,graphic=T) 
{
  d <- rowSums(abs(similarity))
  ds <- d^(-0.5)
  L <- ds * t(similarity * ds)
  e = eigen(L, symmetric = TRUE)
  if(!is.null(Kmax)) Kmax=nrow(similarity);
  Z = e$vector[, 1:Kmax]
  val = e$value[1:Kmax]
  if(graphic) plot(val,main="eigen values");
  if(!is.null(ev.min)) val=val[val>ev.min];
  Kprincipal=max(2,sum(trunc(val))+1);
  gap <- abs(diff(val));
  if(graphic) plot(val,main="gap");
  Kgap <- which.max(gap)
  return(list(gap = gap, KPEV=Kprincipal, Kgap = Kgap, valp = val,vp=Z, val= e$value, v=e$vector))
}

#---------------------------------------------------------------------
#' @title Compute cluster numbers
#' @author Emilie Poisson Caillault 
#' @date   v22/05/2019
#' @description
#' @param data dataframe (a row = a time sample, a column = a feature)
#' @param nK  K cluster numbers (fixed or NULL=automatically computed according to crit)
#' @param crit  cluster number computation method: "GAP" or "PEV"
#' @param ev.min eigen values accepted minimum 
#' @param Kech  maximum of sample number for reduction (by vector quantization)
#' @param StopCriteriaElbow criterion for minimizing intra-group distance and select final Kech
#' @param neighbours neighbour number for Zelnik and Perona similarity (Default = 7)
#' @param Xadd min size of added points for each Kech cluster to save a density information. (Default =11)
#' @param PAM  spectral-PAM (TRUE) or spectral kmeans (FALSE, by default)
#' @param info verbose during the process
#' @param scale true (by default) or false, data normalisation
#' @param sil silhouette score in spectral space
VFastSpectralNJW<-function(data, nK = NULL, crit="GAP", ev.min=0.9, 
                           Kech = 2500, StopCriteriaElbow = 0.99, neighbours = 7, 
                           Xadd = 11, PAM = FALSE,
                           info=FALSE, scale=T){
  
  #data verification
  if(sum(is.na(unlist(data)))>0) stop("na in dataset")
  if(sum(is.infinite(unlist(data)))>0) stop("Inf in dataset")
  if(sum(is.nan(unlist(data)))>0) stop("NaN in dataset")
  
  #delete column with no information sd=0
  data.sd=sapply(data,FUN=sd, na.rm=T)
  toRemove=which(data.sd==0)
  if(length(toRemove)>0) data=data[,-toRemove]
  rm(data.sd);rm(toRemove)
  
  #scale transform if request
  if (scale) data=scale(data);
  
  # si nb de points imposants quantification vectorielle par kmeans avec accroissement 
  # pour respecter les densités
  # spectral sur des echantillons
  dataSize=nrow(data)
  if (Kech<dataSize){
    KElbow=NULL;
    print(paste("vector quantization by kmeans, initial nb points=",dataSize))
    try({
      KElbow=compute.KmeansAutoElbow(features=data,Kmax=Kech,StopCriteria=StopCriteriaElbow,graph=F);
    })
    #if(class(KElbow) == "try-error") KElbow=uHMM::KmeansAutoElbow(features=data,Kmax=Kech,StopCriteria=StopCriteriaElbow,graph=F);
    
    nbProto=KElbow$K;
    if(info) { print(paste("Nb prototypes originaux selectionnes par Kmeans/ELbow= ",nbProto)) } 
    initialLabel=KElbow$res.kmeans$cluster
    sample=KElbow$res.kmeans$centers;
    
    #accroissement de la representativite de chaque sous-groupe
    #utile pour le calcul d'une similarité basé voisins
    K=nK
    proto.sample<-sample
    if (is.null(Xadd)){ Xadd = neighbours}
    if (nrow(proto.sample)<trunc(Kech/Xadd)){
      count<-as.matrix(table(initialLabel))
      weight<-count/(min(count))
      Xadd<-min(min(count),Xadd) # reduction du nombre de points ajoutes si le plus petit prototype a moins de Xadd points
      M<-data.frame(count=count,weight=trunc(weight*Xadd))
      
      nb.points<-apply(X = M, MARGIN = 1, FUN = min)
      total.points<-sum(nb.points)
      
      if(total.points>Kech){
        nb.points<-trunc(nb.points/total.points*Kech)
      }
      sample<-NULL
      for (i in 1:nbProto){
        classIndex<-which(initialLabel==i) #class_i<-data[classIndex,]
        n.index<-sample(classIndex,nb.points[i])     #kNN(class_i,t(as.matrix(proto.sample[i,])),K=n)
        sample<-rbind(sample,proto.sample[i,],data[n.index,])
      }
    }  
  }else{
    sample=data;
    proto.sample=data;
  }
  
  if(info) print(paste("nb points to classify= ",nrow(sample)))
        
  #sample data verification
  sample.sd=sapply(sample,FUN=sd, na.rm=T)
  toRemove=which(sample.sd==0)
  if(length(toRemove)>0) sample=sample[,-toRemove]
  rm(toRemove);rm(sample.sd)
  
  sample.dist=as.matrix(dist(sample));
  
  #Calcul de la matrice de similarite  des centres representatifs des donnees
  similarity=ZPGaussianSimilarity(data=sample, K=neighbours)
  v=eigen(similarity,symmetric=TRUE)
  if(length(which(v$values<(-0.1)))) {
    similarity=similarity%*%t(similarity) # Gram matrice pour faire du spectral car avec ZP non gram
  }
  
  #spectral kmeans sur les points representatifs
  #calcul du Gap, Kmax = le nombre de centres representatifs
  K=2
  if (is.null(nK)){
    gap=compute.nbClusterGap(similarity,ev.min=ev.min,Kmax=nrow(sample),graphic=info)
    if(crit=="GAP"){
      K=max(2,gap$Kgap)
    }else if(crit=="PEV"){
      K=max(2,gap$PEV)
    }
  }
  
  ######
  if(info) { print(paste("Detected K= ",K)) } 
  #calcul du spectral Kmeans
  labelSpectral=NULL
  if(PAM){
    if(info) { print("spectral kmedoids") } 
    spectral=spectralPamClusteringNg(similarity, K); 
    labelSpectral=spectral$label
  }else{
    if(info) { print("spectral kmeans") } 
    # vp sortant de gap
    Z=gap$vp[,1:K]
    rowNorm<-apply(Z,MARGIN=1,FUN=function(x) norm(matrix(x),"f"))
    Zn=Z/rowNorm
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                         
    #classification
    cl=kmeans(Zn,centers=K,iter.max = 100, nstart = 20)
    # case Hartigon with no convergence
    if(cl$ifault==4){
      cl=kmeans(Zn,centers=cl$centers, iter.max = 100, algorithm="MacQueen");
      print("MacQueen")
    } 
    labelSpectral=cl$cluster
  }
  
  # connexity computation
  Z=gap$vp[,1:K]
  silP=summary(silhouette(dist=dist(Z),x=labelSpectral))$clus.avg.widths
  sil=summary(silhouette(dist=dist(sample),x=labelSpectral))$clus.avg.widths
  #Labellisation des donnees par kppv avec les prototypes
  label=NULL;
  if (Kech<dataSize){
    label=knn(train=sample,test=data, labelSpectral,k=1, prob=FALSE)
    #correction des points dans l'espace d'entree 
    label=knn(train=data,test=data, label,k=3, prob=FALSE)
  }else{label=labelSpectral}
  
  out<-list(sample=sample,proto.sample=proto.sample,label=label, K=K, gap=gap,labelSpectral=labelSpectral,sil=sil,silP=silP)
  return(out)
}



#---------------------------------------------------------------------
#' @title Multi-level spectral clustering base on divisive cuts
#' @author Author Emilie Poisson Caillault & Kelly grassi
#' @date   v20/05/2019
#' @description
#' @param data dataframe a point by row
#' @param niveau  level of clustering
#' @param sil.min  min silouhette for each cluster
#' @param fig  plot
#' @param minPoints  min data by cluster
#' @param nNeighbours=7 neighbours for Kppv methode
#' @param Kech  sample numbers for reduction
#' @param crit "PEV"= OR "gap"=  
#' @param PAM T = "spectral kmedoids F= spectral kmeans
#' @param crit  methode determining the number of classes "GAP" or "PEV"
HSpectral<-function(data,niveau=2,sil.min=0.8,fig=T,
                    minPoints=5,nNeighbours=7,Kech=2500,
                    crit="PEV",PAM=T,info=T,...){
  
  #test NA and save id row to insert NA for not classified
  indRowNA=which(apply(is.na(data),MARGIN=1, FUN=any));
  
  #data verification
  df=NULL;
  if(sum(is.infinite(unlist(data)))>0) stop("Inf in dataset")
  if(sum(is.nan(unlist(data)))>0) stop("NaN in dataset")
  if(sum(is.na(unlist(data)))>0){
    warning("Na in dataset - a row with at least one NA are not considered for classification")
  }
  df=data;
  
  #remove column with no information sd=0
  data.sd=sapply(df,FUN=sd, na.rm=T)
  toRemove=which(data.sd==0)
  if(length(toRemove)>0) df=df[,-toRemove]
  rm(data.sd);
  
  #create label matrix(nb_level,nb_points) - 0 label for unclass  
  label=matrix(0,nrow=nrow(data),ncol =niveau+1);
  label[,1]=1;
  label[indRowNA,]=0;
  
  ind=1:nrow(df);
  labelSpectral=NULL;
  prec=0; # increment label number per level
  sil=0;
  
  for(niv in 2:(niveau+1)){
    sil.prec=sil
    print(sil.prec);
    sil=NULL;
    label.prec=label[,niv-1];
    lab=unique(label.prec)
    lab=lab[lab>0] #not considered 0-label : NA value in df's row
    print("----------------")
    print(paste("label repartition in niv=",niv-1))
    print(table(label.prec))
    print("----------------")
    prec=0;
    for(c in sort(lab,decreasing = F)){
      #research involved points with this lab 
      ind=which(c==label.prec);
      print(paste("niv=",niv, " - c=",c, "nbData=",length(ind), "sil=",sil.prec[c]))
      # df[df==0]=.Machine$double.ep
      
      # selection of involved points
      dataNiv=df[ind,];
      data.sd=sapply(dataNiv,FUN=sd,na.rm=T);
      toRemove=which(data.sd==0)
      if(length(toRemove)>0) dataNiv=dataNiv[,-toRemove]
      dataNiv=scale(dataNiv)
      rm(data.sd);rm(toRemove)
      
      #clustering
      labelSpectral=NULL;
      if( (ncol(dataNiv)>1) & (length(ind)>minPoints) & (sil.prec[c]<sil.min) ){
        nNeighbours=min(length(ind)-1,nNeighbours)
        cl=VFastSpectralNJW(dataNiv,neighbours = nNeighbours, crit=crit,PAM=PAM,Kech=min(Kech,nrow(dataNiv)))
        labelSpectral=as.numeric(cl$label);
        print(unique(labelSpectral));
        sil=c(sil,cl$sil);
        print("silhouette")
        print(sil)
      }else{
        print("no cut")
        sil=c(sil,sil.prec[c])
        labelSpectral=rep(1,length(ind));
      }
      labelSpectral=labelSpectral-min(labelSpectral)+1+prec;
      prec=max(labelSpectral);
      label[ind,niv]=labelSpectral;
    }# endfor c
    if(fig==T){jpeg(paste("niveau",niv,".jpg",sep=""));plot(data,col=label[,niv]+1);dev.off()}  
  }#endfor niv
  return(list(label=label,sil=sil));
}


