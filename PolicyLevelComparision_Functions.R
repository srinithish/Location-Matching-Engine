
placeSimilarColsSidebySide <- function(preDF,postDF,commonCols){
                              preCols = colnames(preDF)
                              postCols = colnames(postDF)
                              stackedUpCols = numeric(0)
                              
                              for(i in preCols){
                                    if(!i %in% commonCols){
                                      
                                         if(i %in% postCols){
                                        
                                              # commonisdTerm = gsub("(\\Q.\\E|_)(.*)$","",i)
                                              preColWithDelim = paste0(i,".x")
                                              postColWithDelim = paste0(i,".y")
                                              stackedUpCols = c(stackedUpCols,preColWithDelim,postColWithDelim)
                                         }
                                      
                                    
                                    }
                                
                              }
                                return(c(commonCols,stackedUpCols))
            
                              }