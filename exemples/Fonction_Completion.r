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


#' @title Estimating global features of a univariate signal
#' @author Camille Dezecache, Hong T. T. Phan, Emilie Poisson-Caillault v2019.04.18
#' @description Computes global features of a univariate signal, used as input for threshold and window definition in DTWBI algorithm.
#'  Features computed are:
#'  \itemize{
#'  \item{minx: }{minimum value of the input vector}
#'  \item{maxx: }{maximum value of the input vector}
#'  \item{avg: }{average value of the input vector}
#'  \item{medianx: }{median of the input vector}
#'  \item{sdx: }{standard deviation of the input vector}
#'  \item{momx: }{skewness}
#'  \item{nop: }{number of peaks of the input vector}
#'  \item{len: }{length of the input vector}
#'  \item{entro: }{measure of entropy}
#'  }
#' @param X signal
#' @return returns a matrix with one row, each column giving the value of corresponding estimated feature.
#' @importFrom entropy entropy
#' @importFrom e1071 skewness
#' @keywords internal
#' @examples
#' data(dataDTWBI)
#' X <- dataDTWBI[, 1]
#' gf <- .globalfeatures(X)

.findPeaks <-function(x, thresh = 0) {
  pks <- which(diff(sign(diff(x, na.pad = FALSE)), na.pad = FALSE) < 0) + 2
  if (!missing(thresh)) {
    (pks[x[pks] - x[pks + 1] > thresh]) || (pks[x[pks] - x[pks - 1] > thresh])
  }
  else pks
}

.globalfeatures<-function(X){
  minx <- min(X,na.rm=T); 
  maxx <- max(X,na.rm=T); 
  avg <- mean(X,na.rm=T) ; 
  medianx <- median(X,na.rm=T); 
  sdx <- sd(X,na.rm=T); 
  mom3 <- e1071::skewness(X,na.rm = T); 
  nop <- length(.findPeaks(X)); 
  len <- length(X)
  entro <- entropy::entropy(as.vector(table(X)), method="ML"); 
  out <- c(minx,maxx,avg,medianx,sdx,mom3,nop,len,entro)
  out <- format(out, digits=2, nsmall=2)
  return(out)
}

#' @title Global threshold for missing data imputation
#' @author Camille Dezecache, Hong T. T. Phan, Emilie Poisson-Caillault
#' @date 2019/04/15
#' @description Finds a threshold for univariate missing data imputation in a univariate vector.
#' @import dtw
#' @importFrom lsa cosine
#' @keywords internal
.DTW_threshold_global_univariate <- function(query, database, i_start, i_finish, step_threshold, threshold_cos, thresh_cos_stop, ...){
  if(sum(is.na(query))>0) return(NULL);
  # Initialization
  T_gap <- length(query)
  Cosine_threshold <- NULL;
  pos_i <- NULL;
  threshold_cos_temp <- threshold_cos
  gf_q <-as.numeric(.globalfeatures(query))
  ind_nan.q=which(is.nan(gf_q))
  while((length(Cosine_threshold)==0)&&(threshold_cos_temp>thresh_cos_stop)){
      i <- i_start
      # window browsing
      while(i<=i_finish){ 
        k <- i+T_gap-1
        ref <- database[i:k]
        cos_value=0;
        if(sum(is.na(ref))==0){
           gf_r <- as.numeric(.globalfeatures(ref))
           ind_nan <- c(ind_nan.q, which(is.nan(gf_r))) # Remove NaN in global features
           gf<-cbind(gf_r,gf_q)
           if(length(ind_nan)>0){ gf<-gf[-ind_nan,] }
           if( (nrow(gf)>1) & (ncol(gf)==2)){ cos_value <- cosine(gf)[1,2] }
           rm(gf);rm(gf_r);rm(ind_nan);
         }  
      
        #save window start index according cos criteria and cos value
        if(cos_value>=threshold_cos_temp){
          pos_i <- c(pos_i, i)
          Cosine_threshold <- c(Cosine_threshold, cos_value)
        }
        # next window start 
        i <- i+step_threshold
      }
    threshold_cos_temp <- threshold_cos_temp-0.01
  }
  if(length(pos_i)==0){warning("No similar window looks appropriate for imputation")}
  return(pos_i)
}

#' @title Finding similar windows to a query
#' @author Camille Dezecache, Hong T. T. Phan, Emilie Poisson-Caillault
#' @description This function finds similar windows to a query consisting of a univariate vector.
#' @import dtw
#' @importFrom lsa cosine
#' @keywords internal

.Finding_similar_window_univariate <- function(query, database, selected_qs, ...){
  id_similar=NULL;
  if (length(selected_qs)==0) return(id_similar)
  
  # Initialization
  T_gap <- length(query)
  Cost_dist <- c()
  pos_i <- selected_qs
  
  for (i in pos_i){
    k <- i+T_gap-1
    ref <- database[i:k]
    align <- dtw(query, ref, keep.internals=TRUE)
    cost <- align$normalizedDistance
    Cost_dist <- c(Cost_dist, cost)
  }
  
  min_cost <- min(Cost_dist)
  id_similar <- pos_i[which(Cost_dist==min_cost)]
  
  return(list(id_similar))
}

#' @title imput DTWBI
#' @author Camille Dezecache, Hong T. T. Phan, Emilie Poisson-Caillault
#' @date v17/04/2019
#' @description This function finds windows and complete a gap of a univariate vector.

imputeDTWBI<-function (data, t_gap, T_gap, DTW_method = "DTW", threshold_cos = NULL, 
    step_threshold = NULL, thresh_cos_stop = 0.8, verbose=F,...){
    
  method_options <- c("DTW", "DDTW", "AFBDTW", "dtw", "ddtw","afbdtw")
  if (DTW_method %in% method_options){
    if(verbose ==T) print(DTW_method)
  }else{ stop("Invalid DTW method")}
  
  IdGap <- t_gap:(t_gap + T_gap - 1)
  #if (sum(which(is.na(data[-IdGap]))) > 0) {
  #    stop("Dataset contains remaining NA outside main gap")}
  
  N <- length(data)
  if (T_gap >= 0.25*N) stop("Gap is too large to compute an appropriate imputation proposal")
  
  if (is.null(threshold_cos)){
        if (N > 10000) { threshold_cos = 0.9995
        }else { threshold_cos = 0.995}
  }
  
  if (is.null(step_threshold)){
        if (N > 10000) {step_threshold = 50
        }else if (N > 1000) {
            step_threshold = 10
        }else{ step_threshold = 2}
  }
  
  flag_tested=F;
  selected_qs <- NULL;
  id_similar_window=NULL;
  
  #test gap position : first half or second half in the data.
  #case 1: query after gap if t_gap<N/2
  if (t_gap < N/2) {
      flag_tested=T;
        query_a <- c()
        data_a <- data
        pos_start <- t_gap + T_gap
        ind <- pos_start:(pos_start + T_gap - 1)
        query_a <- data[ind]
        if (DTW_method == "DDTW" || DTW_method == "ddtw") {
            query_a <- local.derivative.ddtw(query_a)
            data_a <- local.derivative.ddtw(data_a)
            data_a[t_gap - 1] <- 0
            data_a[t_gap + T_gap] <- 0
        }
        i_start <- pos_start + T_gap
        i_finish <- length(data) - T_gap + 1
        
        selected_qs <- .DTW_threshold_global_univariate(query = query_a, 
            database = data_a, i_start, i_finish, step_threshold, 
            threshold_cos, thresh_cos_stop, ...)
        if(length(selected_qs)>0){
          if (DTW_method == "AFBDTW" || DTW_method == "afbdtw") {
              id_similar_window <- .Finding_similar_window_univariate_AFBDTW(query = query_a, 
                  database = data_a, selected_qs = selected_qs, 
                  ...)
          }else {
              id_similar_window <- .Finding_similar_window_univariate(query = query_a, 
                  database = data_a, selected_qs = selected_qs, 
                  ...)
          }
          if(length(id_similar_window)>0){
          id_simwin_begin <- id_similar_window[[1]]
          id_simwin_end <- id_simwin_begin + T_gap - 1
          similar_query_dtw <- data[id_simwin_begin:id_simwin_end]
          id_imp_end <- id_simwin_begin - 1
          id_imp_begin <- id_imp_end - T_gap + 1
          imp_value_dtw <- data[id_imp_begin:id_imp_end]
          data_imputed_proposal = data
          data_imputed_proposal[t_gap:(t_gap + T_gap - 1)] <- imp_value_dtw
          imputation <- list(output_vector = data_imputed_proposal, 
              input_vector = data, query = data[ind], pos_query = c(pos_start, 
                  (pos_start + T_gap - 1)), sim_window = similar_query_dtw, 
              pos_sim_window = c(id_simwin_begin, id_simwin_end), 
              imputation_window = imp_value_dtw, pos_imp_window = c(id_imp_begin, 
                  id_imp_end))
        }
        }
  }
  #case 2 and 3: query before if t_gap<N/2 or no selected qs for case 1
    if (t_gap >= N/2 | length(selected_qs)==0) {
        query_b <- c()
        pos_start <- t_gap - 1
        ind <- (pos_start - T_gap + 1):(pos_start)
        Researchbase_b <- data[1:pos_start]
        query_b <- data[ind]
        if (DTW_method == "DDTW" || DTW_method == "ddtw") {
            query_b <- local.derivative.ddtw(query_b)
            Researchbase_b <- local.derivative.ddtw(Researchbase_b)
        }
        i_start <- 1
        i_finish <- length(Researchbase_b) - 2 * T_gap
        selected_qs <- .DTW_threshold_global_univariate(query = query_b, 
            database = Researchbase_b, i_start = i_start, i_finish = i_finish, 
            step_threshold, threshold_cos, thresh_cos_stop)
        if(length(selected_qs)>0){
            if (DTW_method == "AFBDTW" || DTW_method == "afbdtw") {
              id_similar_window <- .Finding_similar_window_univariate_AFBDTW(query = query_b, 
                  database = Researchbase_b, selected_qs = selected_qs)
          }else {
              id_similar_window <- .Finding_similar_window_univariate(query = query_b, 
                  database = Researchbase_b, selected_qs = selected_qs)
          }
          if(length(id_similar_window)>0){
            id_simwin_begin <- id_similar_window[[1]]
            id_simwin_end <- id_simwin_begin + T_gap - 1
            similar_query_dtw <- Researchbase_b[id_simwin_begin:id_simwin_end]
            id_imp_begin <- id_simwin_end + 1
            id_imp_end <- id_imp_begin + T_gap - 1
            imp_value_dtw <- data[id_imp_begin:id_imp_end]
            data_imputed_proposal = data
            data_imputed_proposal[t_gap:(t_gap + T_gap - 1)] <- imp_value_dtw
            imputation <- list(output_vector = data_imputed_proposal, 
              input_vector = data, query = data[ind], pos_query = c((pos_start - 
                  T_gap + 1), pos_start), sim_window = similar_query_dtw, 
              pos_sim_window = c(id_simwin_begin, id_simwin_end), 
              imputation_window = imp_value_dtw, pos_imp_window = c(id_imp_begin, 
                  id_imp_end))
          }
          }
    }
    #case 4 
    if(flag_tested==F & length(selected_qs)==0){
      query_a <- c()
      data_a <- data
      pos_start <- t_gap + T_gap
      ind <- pos_start:(pos_start + T_gap - 1)
      query_a <- data[ind]
      if (DTW_method == "DDTW" || DTW_method == "ddtw") {
        query_a <- local.derivative.ddtw(query_a)
        data_a <- local.derivative.ddtw(data_a)
        data_a[t_gap - 1] <- 0
        data_a[t_gap + T_gap] <- 0
      }
      i_start <- pos_start + T_gap
      i_finish <- length(data) - T_gap + 1
      
      selected_qs <- .DTW_threshold_global_univariate(query = query_a, 
                                                    database = data_a, i_start, i_finish, step_threshold, 
                                                    threshold_cos, thresh_cos_stop, ...)
      if(length(selected_qs)>0){
        if (DTW_method == "AFBDTW" || DTW_method == "afbdtw") {
          id_similar_window <- .Finding_similar_window_univariate_AFBDTW(query = query_a, 
                   database = data_a, selected_qs = selected_qs, ...)
        }else{
        id_similar_window <- .Finding_similar_window_univariate(query = query_a, 
           database = data_a, selected_qs = selected_qs, ...)
        }
        if(length(id_similar_window)>0){
          id_simwin_begin <- id_similar_window[[1]]
          id_simwin_end <- id_simwin_begin + T_gap - 1
          similar_query_dtw <- data[id_simwin_begin:id_simwin_end]
          id_imp_end <- id_simwin_begin - 1
          id_imp_begin <- id_imp_end - T_gap + 1
          imp_value_dtw <- data[id_imp_begin:id_imp_end]
          data_imputed_proposal = data
          data_imputed_proposal[t_gap:(t_gap + T_gap - 1)] <- imp_value_dtw
          imputation <- list(output_vector = data_imputed_proposal, 
            input_vector = data, query = data[ind], pos_query = c(pos_start, (pos_start + T_gap - 1)), 
              sim_window = similar_query_dtw, 
                         pos_sim_window = c(id_simwin_begin, id_simwin_end), 
                         imputation_window = imp_value_dtw, 
                          pos_imp_window = c(id_imp_begin, id_imp_end))
        }
      }
    }
    if(length(selected_qs)==0 | length(id_similar_window)==0){
      if(verbose) warning("no available window according global features or DTW cost")
      sigNA=rep(NA,T_gap)
      imputation <- list(output_vector = sigNA, 
                     input_vector = data, query = data[ind], 
                     pos_query = c(pos_start, (pos_start + T_gap - 1)), 
                     sim_window = sigNA, 
                     pos_sim_window = c(NA, NA), 
                     imputation_window = sigNA, 
                     pos_imp_window = c(NA, NA))
    }
    return(imputation)
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
