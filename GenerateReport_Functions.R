
IndicateChange = function(colName1,colName2,LocationDFtoOutput_Cop,sheet,startRow = 1){
                    colnames = colnames(LocationDFtoOutput_Cop)
                    
                    colNumber1 = which(colnames==colName1)
                    colNumber2 = which(colnames==colName2)
                   
                            for (i in (startRow:(nrow(LocationDFtoOutput_Cop)))){
                              
                                    if(LocationDFtoOutput_Cop[i,colNumber1] != LocationDFtoOutput_Cop[i,colNumber2] &&
                                       !is.na(LocationDFtoOutput_Cop[i,colNumber1]) && !is.na(LocationDFtoOutput_Cop[i,colNumber2])){
                                      
                                      addStyle(FinalReportWb,sheet,style = changeIndicatorStyle,
                                               rows = i+1,
                                               cols = colNumber1)
                                      
                                      addStyle(FinalReportWb,sheet,style = changeIndicatorStyle,
                                               rows = i+1,
                                               cols = colNumber2)
                                
                              }
                      
                            }
    
  
}

IndicateNumericChange = function(changeCol,changePctCol,DfContainingCol,sheet, changeNum,changePct,startRow =1 ){
                        colnames = colnames(DfContainingCol)
                        colNumber1 = which(colnames==changeCol)
                        colNumber2 = which(colnames==changePctCol)
                        
                        for (i in (startRow:(nrow(DfContainingCol)))){
                          
                                if((DfContainingCol[i,colNumber1] >= changeNum || DfContainingCol[i,colNumber1] <= -changeNum) && 
                                   (DfContainingCol[i,colNumber2] >= changePct ||DfContainingCol[i,colNumber2] <= -changePct) &&
                                   !is.na(DfContainingCol[i,colNumber1]) && !is.na(DfContainingCol[i,colNumber2])){
                                  
                                  addStyle(FinalReportWb,sheet,style = NumericChangeIndicatorStyle,
                                           rows = i+1,
                                           cols = colNumber1)
                                  
                                  addStyle(FinalReportWb,sheet,style = NumericChangeIndicatorStyle,
                                           rows = i+1,
                                           cols = colNumber2)
                                
                                }
                          
                        }
                        
  
  
                        }