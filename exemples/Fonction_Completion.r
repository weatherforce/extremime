

#---------------------------------------------------------------------
#' Completion individual hole (no consecutive missing values) in time series 
#' Author Emilie Poisson Caillault v27/03/2019
#' @param serie vector 
#' @param verbose true or false to print some details 
#' @return vector
completion.singleHole<-function(series,verbose = F){
  completed=series;
  if(sum(is.na(series))==0){
    if (verbose) print("no completion");
    return(completed);
  }
  #compute size of Hole
  sizeHole=compute.sizeHole(series)
  #find individual hole
  indice=which(sizeHole==1);
  N=length(indice)
  if(N>0){
    #completion by mean
    debut=sapply(indice,function(x){max(1,x-1)})
    fin=sapply(indice,function(x){min(length(series),(x+1))})
    completed[indice]=rowMeans(cbind(series[debut],series[fin]),na.rm=TRUE);
  }
  return(completed);
}

#---------------------------------------------------------------------
#' Completion of time series by moving ponderated average according the hole size
#' Weigthed average according the place of the information, near it is higher missing data. 
#' Author Emilie Poisson Caillault v27/03/2019
#' @param serie vector
#' @param acceptedHole complete hole os size min or = to acceptedHole (default 10)
#' @param mult (default 10), if=0 standart weighting : 12..N.N..21 N=hole size else N*mult (as exponential)
#' @param confidence ponderation for NA completed value, if confidence >0 : weighted ponderation =1
#' @param maxWindow (default 10) max number of considered points to estimate hole. 
#' @param V = verbose 
#' @example t=1:500;s=sin(2*pi*t/100);s[10]=NA;s[1:2]=NA;s[480:500]=NA;s[210:240]=NA;
#' @example plot(s); sc=completion.adaptativeMovingAverage(s); points(sc,pch='+',col="red")
#' @return vector
completion.adaptativeMovingAverage<-function(series,acceptedHole=10,mult=10, confidence=1, maxWindow=10, V=FALSE){
  if(sum(is.na(series))==0) return(series);
  #fill individual hole is not already done
  completed=series;
  sizeHole=compute.infoHole(series); 
  if(sum(sizeHole[,2]==1)>0) completed=completion.singleHole(series);
  if(sum(is.na(completed))==0) return(completed);
  #compute size of Hole not filled
  sizeHole=compute.infoHole(completed);
  if(V){	
    print("hole information after single hole filling" )
    print(sizeHole)
  }
  sizeToFill=sizeHole[,2];
  positionHole=sizeHole[,1]
  indice=which(sizeToFill<=acceptedHole); # fill only missing value, no smoothing 
  N=length(indice)
  if(N==0) return(completed);
  #completion by moving average and taking account new inserted data
  for(i in indice){
      dateS.missing=positionHole[i] #start position of hole
      dateE.missing=positionHole[i]+sizeToFill[i]-1 #end position of hole
      s=sizeToFill[i];
      if(V) print(paste("----info hole i=",i, " : at ", dateS.missing, " to",dateE.missing))
      if(s==1){
        debut=max(1,dateS.missing-1)
        fin=min(length(series),dateS.missing+1)
        completed[date.missing]=mean(completed[debut:fin],na.rm=TRUE)
        hole.before=0;
      }else{
        fen=floor(min(maxWindow,length(series))/2)
        for(p in 1:s){
          dS=max(1,dateS.missing+p-1-fen);
          dE=min(dateS.missing+p-1+fen,length(series))
          ind.window=dS:dE;
          val.orig=series[dS:dE]
          val.window=completed[dS:dE];
          #create weigthed coef vector
          ponderation=rep(0,length(ind.window));
          Np=length(ponderation)
          q=(Np-fen+1):length(ponderation)
          if(mult>0){
            alpha=0.4;
            ponderation[fen:1]=alpha*(1-alpha)^(1:fen);
            ponderation=mult*ponderation/max(ponderation);
            ponderation[q]=alpha*(1-alpha)^(1:(length(q)));
            ponderation[q]=mult*ponderation[q]/max(ponderation[q]);
          }else{
            ponderation=1:length(ind.window);
            ponderation[q]=length(q):1;
          } 
          if (confidence) ponderation[is.na(val.orig)]=1;
          ponderation[is.na(val.window)]=0;
          completed[dateS.missing+p-1]=sum(ponderation*val.window,na.rm=TRUE)/sum(ponderation);
          if(V){
            print(paste("missing at ",p-1+dateS.missing))
            print("ponderation:");print(ponderation)
            print("series avant"); print(series[ind.window])
            print("series apres"); print(completed[ind.window])
          }
        }
      }
  }
  return(completed);
}

#---------------------------------------------------------------------
#' compute DTWBI algorithm on one signal
#' Author Emilie Poisson Caillault v28/03/2019
#' @param sig 
#' @param acceptedHole size of accepted hole, to complete
#' @param smallHole size of considered small hole, completed by movering average
#' @param thresh_cos_stop=0.8 to avoid filling
#' @param threshold_cos=0.995 for reduction computation, accepted cos between features
compute.DTWBI<-function(sig, acceptedHole, smallHole = 3*24, thresh_cos_stop=0.8,threshold_cos = 0.995,verbose=T,fig=F, ...){
  sigc=NULL;
  out<-sig;
  if(verbose){
    print(paste("number of data:",length(sig)));
    print(paste("NA total:",sum(is.na(sig))));
  }
  if(sum(is.na(sig))>0){
    # filling single hole
    sigc=completion.singleHole(sig);
    # then small hole : missing consecutives values with number < smallHole
    s1=completion.adaptativeMovingAverage(sigc,acceptedHole = smallHole,mult = 10,confidence = 1,maxWindow = 10 ,V = F);
    s2=completion.adaptativeMovingAverage(sigc[length(sig):1],acceptedHole = smallHole,mult = 10,confidence = 1,maxWindow = 10,V = F);
    s2=s2[length(s2):1]
    sigc=(s1+s2)/2;
    rm(s1);rm(s2);
    #missing values after isolated hole completion
    vgap=compute.sizeHole(sigc);
    #searching position and size of each hole
    infoHole=compute.infoHole(sigc);
    infoHole=infoHole[order(infoHole[,2],decreasing=F), ]
    
    #reject Hole with size>acceptedHole
    if(verbose) print(infoHole)
    infoHole=infoHole[infoHole$size<=acceptedHole,]
    
    # acceptable Hole to fill by DTWBI methods
    sigDTWBI=sigc;
    if(nrow(infoHole)>0){
      for(i in 1:nrow(infoHole)){
       if(verbose){print( paste("% completion",floor(100*i/nrow(infoHole))))}
       posHole=infoHole$pos[i]
       sizeHole=infoHole$size[i]
       resDTWBI=NULL;
       try({
         resDTWBI=imputeDTWBI(sigDTWBI, t_gap=posHole, T_gap=sizeHole,thresh_cos_stop=thresh_cos_stop,threshold_cos = threshold_cos,verbose=F);
         sigc[posHole:(posHole+sizeHole-1)]=resDTWBI$imputation_window;
       })
     }
    }
  }
  out<-sigc
  return(out);
}
