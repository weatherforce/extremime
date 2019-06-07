#---------------------------------------------------------------------
#' Compute size of consecutive hole for each time in time series 
#' return vector with same size. 0 if no hole other the size of hole
#' Author Emilie Poisson Caillault v25/01/2019
#' @param serie vector 
#' @return vector 
compute.sizeHole<-function(series){
  #find hole
  indice=which(is.na(series));
  N=length(indice)
  if(N>0){
    flagIndice=indice-c(-1,indice[-N]);
    sizeHole=1;
    consecutiveHole=rep(1,N);
    for(i in 1:N){
      #increment number of consecutive NA data
      if(flagIndice[i]==1){
        sizeHole=sizeHole+1;
        #update all indices concerned by same consecutive hole
        consecutiveHole[(i-(sizeHole-1)):i]=sizeHole;
      }else{
        sizeHole=1;
      }
    }
    sizeHole=rep(0,length(series));
    sizeHole[indice]=consecutiveHole;
  }else{
    sizeHole=0;
    print("no hole or no NA");
  }
  return(sizeHole);
}



#---------------------------------------------------------------------
#' indexes of missing hole use compute sizeHole 
#' return a dataframe with the position (pos) of the hole and its size  
#' Author Emilie Poisson Caillault v16/03/2015
#' @param serie vector 
#' @return dataframe if existing hole, else dataframe(pos=0,size=0)
compute.infoHole<-function(serie){
  vgap=compute.sizeHole(serie);
  index=NULL; i=0; posGap=NULL; sizeGap=NULL;
  if(sum(vgap)>0){
    indPos=which(vgap>0);
    index=data.frame(pos=indPos[1],size=vgap[indPos[1]]);
    i=1+vgap[indPos[1]];
    while(i<=length(indPos)){
      posGap=indPos[i];
      sizeGap=vgap[posGap]
      index[nrow(index)+1,]=c(posGap,sizeGap);
      i=i+sizeGap;
      posGap=NULL; sizeGap=NULL;
    }
  }else{
    index=data.frame(pos=0,size=0);
  }
  return(index);
}

#---------------------------------------------------------------------
#' hist of hole use compute sizeHole 
#' return the hist vector 
#' Author Emilie Poisson Caillault v16/03/2015
#' @param fig T or F, T=plot hist
#' @param serie vector 
#' @param verbose true or false to print some details
#' @return vector 
compute.histSizeHole<-function(series, fig=F,verbose=F){
  val=NULL;count=NULL;
  #find hole
  holeSize=compute.sizeHole(series);
  if(length(holeSize)>1){
    unique.holeSize=holeSize;
    N=length(series);
    val=NULL; count=NULL;
    for (i in 2:N){
      if(holeSize[i]>0)
        if (holeSize[i]==holeSize[i-1]) unique.holeSize[i]=-1;
    }
    #count
    maxi=max(unique.holeSize);
    mini=0;
    val=seq(mini,maxi,1);
    count=sapply(val,FUN=function(x){length(which(unique.holeSize==x))})
    if(fig){barplot(count,names.arg=val,xlab="hole size",ylab="frequency");
      if(verbose) {print(cbind(val,count))}
    }
  }else{
    if(verbose==T)
      print("no hole or no NA")
    val=0; count=length(series);
  }
  return(cbind(val,count));
}



#---------------------------------------------------------------------
#' compute information about missing data 
#' return a matrix of 4 index by parameters dataframe with the position (pos) of the hole and its size  
#' Author Emilie Poisson Caillault v25/01/2019
#' @param df vector or dataframe (parameter in column) 
#' @param v verbose true or false
#' @param fig show percent of na by column true or false
#' @return matrix 4 row index by parameters
infoNA<-function(df,v=T,fig=T){
  info=NULL;
  if(is.vector(df)) df=data.frame(df)
  info=sapply(df,FUN=function(x){
    nbNA=sum(is.na(x));
    PercentNA=0; largestGap=0;nbGap=0;infoGap=NULL;
    if(nbNA>0){
      PercentNA=100*nbNA/length(x);
      infoGap=compute.infoHole(x)
      largestGap=max(infoGap$size);
      if(infoGap$pos[1]>0) nbGap=nrow(infoGap);
    }
    return(c(nbNA,PercentNA,largestGap,nbGap))
  })
  rownames(info)=c("nbNA","PercentNA","largestGap","nbGap");
  if(v)
    if(fig) {
      plot(info['PercentNA',],main="% de NA by column",pch=3)
      abline(h=100,col="red")
    }
  if(v){print(info)}
  return(info);
}

#---------------------------------------------------------------------
#' @title data informations
#' @author Author Emilie Poisson Caillault , Chatelain Pierre 
#' @date   v02/05/2019
#' @description give indicator of data columns
#' @param df

function.infoData <- function(df){
  info.data <- NULL
 
  nb.nan <- sapply(df,FUN=function(x){sum(is.nan(x))})
  nb.na  <- sapply(df,FUN=function(x){sum(is.na(x))}) - nb.nan
  nb.inf <- sapply(df,FUN=function(x){sum(is.infinite(x))})
 
  my.sd <- function(x,...){
    x <- x[!is.na(x)]
    x <- x[!is.infinite(x)]
    x <- x[!is.nan(x)]
    sd(x)
  }
 
  flag.sd.zero  <- sapply(df,FUN=function(x){my.sd(x,na.rm=T)==0})
  flag.val.zero <- sapply(df,FUN=function(x){sum(abs(x),na.rm=T)==0})
 
  nb.outQC <- sapply(df,FUN=function(x){sum(abs(x)==9999.99,na.rm=T)})
 
  info.data=cbind(nb.na,nb.nan,nb.inf,nb.outQC,flag.sd.zero,flag.val.zero)
 
  return(info.data)
}

#---------------------------------------------------------------------
#' @title MetaData Identification
#' @author Author Kelly grassi 
#' @date   v12/04/2019
#' @description Finds index column of MetaData (unit varialble or Manual)
#' @param data
#' @param MetaDonnees = T find automatically unit variables
#' @param listeMetaData manual list Meta-data (Other Than a unit Variable)

compute.indMetadata<-function(data, MetaDonnees=T, listeMetaData){
  
  if(MetaDonnees == T){
    
    print('MetaDonnees=TRUE')
    N=apply(data,MARGIN=2, FUN=function(x){length(unique(na.omit(x)))})              # identification variable unitaire (que l'on peut directement concidere comme une métadonnée)
    indUnit=which(N==1)                                                                 # indice variable unitaire 
    
    indAdd=sapply(listeMetaData,FUN=function(x){grep(pattern=x,names(data), ignore.case=T)});
    ind=unique(c(indUnit,indAdd))
    
  }else{
    print('MetaDonnees=FALSE')
    ind=0
    print('ind=0')
  }
  return(ind)
}
