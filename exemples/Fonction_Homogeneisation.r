
#---------------------------------------------------------------------
#' @title TimeFormat
#' @author Author Kelly grassi 
#' @date   v12/04/2019
#' @description add time colum with good format
#' @param data
#' @param FormatTemps Time Format
#' @param NbColumnTemps number of time columns (1 or 2)
#' @param NameColumnDate  names of column for Data (default = NULL)
#' @param NameColumnHours names of column  for hours (default = NULL)

format.Time<-function(data, FormatTemps, NbColumnTemps, NameColumnDate='NOcolumn', NameColumnHours='NOcolumn'){
  
  indDate=NULL
  date=NULL
  
  # 1 colonne temps (date + heure)
  if (NbColumnTemps == 1){ 
    indDate=grep(pattern=NameColumnDate,names(data), ignore.case=T); 
    date=as.character(data[,indDate]) 
  }
  
  # 2 colonnes temps (date puis heure séparé)
  if (NbColumnTemps == 2){
    
    indDate=grep(pattern=NameColumnDate,names(data), ignore.case=T);
    indHours=grep(pattern=NameColumnHours,names(data), ignore.case=T);
    date = paste(data[,indDate],data[,indHours],sep="") 
  }
  
  data$temps=strptime(date,format= FormatTemps,tz="GMT")
  
  return(data)
}

#---------------------------------------------------------------------
#' @title NA homogenization
#' @author Author Kelly grassi
#' @date   v12/04/2019
#' @description Replace missing values by NA
#' @param data
#' @param defNA if T remplace by NA
#' @param listNA list of missing value trait (inf, NaN ,)

NaTransform<-function(data, defNA = F, listNA){
  library(naniar)
  out<-c()
  if(defNA == T){
    indPara=1:(ncol(data)-1)
    data.na=replace_with_na_all(data[,indPara], condition = ~.x %in% listNA)
    data[,indPara]=data.na
    out<-data}else{
      
      out<-data
    }
  return(out)
}
