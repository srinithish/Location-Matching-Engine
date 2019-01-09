
### removes in a dataframe columns that have complete NAs

removeAllNAColumns = function(x){
  
  return ( x[sapply(x, function(x) !all(is.na(x)))] )
  
}


##' logic gets the first NA after the title
extractDFfromLineGuide = function(LineGuideDF_Copy,title,columnNameToFindIn = "Microstrategy"){
        #TO DO getv only if the element exists
        titleRowStart = which(LineGuideDF_Copy[[columnNameToFindIn]] == title)
        
        if (length(titleRowStart)== 1){
                firstNA = LineGuideDF_Copy[[columnNameToFindIn]] %>% 
                            .[titleRowStart:length(.)] %>% 
                                is.na() %>% which() %>% .[1]
          
                return(
                        LineGuideDF_Copy[(titleRowStart + 1) :(titleRowStart + firstNA -2),] 
        
                    )
        }
        else{return(NULL)}
}

##' corrects the column name as the first line of the passed dataframe
cleanUpExtractedDF <- function(x){
  
                      x = removeAllNAColumns(x)
                      colnames(x) = x[1,] # first line is transferred as column name
                      x = x[-1,]
                      return(x)
}

extractDFfromLineGuide_ForLayerName = function(LineGuideDF_Copy,title,columnNameToFindIn = "Microstrategy"){
                                    
                                    
                                          titleRowStart = which(LineGuideDF_Copy[[columnNameToFindIn]] == title)
                                          if (length(titleRowStart)== 1){    
                                                
                                                firstNA = LineGuideDF_Copy[[columnNameToFindIn]] %>% 
                                                  .[titleRowStart:length(.)] %>% 
                                                  is.na() %>% which() %>% .[1]
                                                
                                                return(
                                                  LineGuideDF_Copy[titleRowStart:(titleRowStart + firstNA -2),] 
                                                  
                                                )
                                    }
                                    else{return(NULL)}
}



