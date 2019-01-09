# if() post against pre then 
##eliminate all non data available dfs before this
source("PolicyLevelComparision_Functions.R")

##For Policy Terms comparision
if(!any(grepl("No.data.available",c(colnames(PolicyLevelTermsPreDF),
                                    colnames(PolicyLevelTermsPostDF)))))
{
      PolicyLevelTermsCompDF = PolicyLevelTermsPreDF %>% 
        full_join(PolicyLevelTermsPostDF,by = c("Layer Name" = "Layer Name")) 
      
      PolicyLevelTermsCompDF = PolicyLevelTermsCompDF %>%
                               select(placeSimilarColsSidebySide(PolicyLevelTermsPreDF,
                                                                  PolicyLevelTermsPostDF,
                                                                  "Layer Name"))
}else{
      if(grepl("No.data.available",colnames(PolicyLevelTermsPreDF))){PolicyLevelTermsPreDF[1,] = NA}
      if(grepl("No.data.available",colnames(PolicyLevelTermsPostDF))){PolicyLevelTermsPostDF[1,] = NA}   
      PolicyLevelTermsCompDF = cbind(PolicyLevelTermsPreDF,PolicyLevelTermsPostDF)
}

colnames(PolicyLevelTermsCompDF) = colnames(PolicyLevelTermsCompDF) %>% 
                                    gsub("(.x)$","_Pre",.) %>% gsub("(.y)$","_Post",.)




##For TCP Comparision
##TO DO when the object is NULL handle the case
if(!any(grepl("No.data.available",c(colnames(TCPPreDF),colnames(TCPPostDF)))) 
   && !(is.null(TCPPreDF)||is.null(TCPPostDF)))
{
    TCPCompDF = TCPPreDF %>% full_join(TCPPostDF,by = "Layer Name")
    
    TCPCompDF = TCPCompDF %>%
      select(placeSimilarColsSidebySide(TCPPreDF,
                                        TCPPostDF,
                                        "Layer Name"))
}else{
      if(grepl("No.data.available",colnames(TCPPreDF))){TCPPreDF[1,] = NA}
      if(grepl("No.data.available",colnames(TCPPostDF))){TCPPostDF[1,] = NA}  
      TCPCompDF = cbind(TCPPreDF,TCPPostDF)
}

colnames(TCPCompDF) = colnames(TCPCompDF) %>% 
                                    gsub("(.x)$","_Pre",.) %>% gsub("(.y)$","_Post",.)



##For peril level terms
if(!any(grepl("No.data.available",c(colnames(PerilLevelTermsPreDF),
                                    colnames(PerilLevelTermsPostDF)))))
{
    PerilLevelTermsCompDF = PerilLevelTermsPreDF %>% 
                            full_join(PerilLevelTermsPostDF,by =c("Layer Name" = "Layer Name","Peril" = "Peril"))
    
    PerilLevelTermsCompDF = PerilLevelTermsCompDF %>%
      select(placeSimilarColsSidebySide(PerilLevelTermsPreDF,
                                        PerilLevelTermsPostDF,
                                        c("Layer Name","Peril")))
}else{
  
      if(grepl("No.data.available",colnames(PerilLevelTermsPreDF))){PerilLevelTermsPreDF[1,] = NA}
      if(grepl("No.data.available",colnames(PerilLevelTermsPostDF))){PerilLevelTermsPostDF[1,] = NA}
      PerilLevelTermsCompDF = cbind(PerilLevelTermsPreDF,PerilLevelTermsPostDF)
}
colnames(PerilLevelTermsCompDF) = colnames(PerilLevelTermsCompDF) %>% 
                                  gsub("(.x)$","_Pre",.) %>% gsub("(.y)$","_Post",.)





###For Peril sublimits
if(!any(grepl("No.data.available",c(colnames(PerilSublimitsPreDF),
                                    colnames(PerilSublimitsPostDF))))){

      PerilSublimitsCompDF = PerilSublimitsPreDF %>% 
                             full_join(PerilSublimitsPostDF,by = c("Peril"))
      
      PerilSublimitsCompDF = PerilSublimitsCompDF %>%
        select(placeSimilarColsSidebySide(PerilSublimitsPreDF,
                                          PerilSublimitsPostDF,
                                          "Peril"))
}else{
  
      if(grepl("No.data.available",colnames(PerilSublimitsPreDF))){PerilSublimitsPreDF[1,] = NA}
      if(grepl("No.data.available",colnames(PerilSublimitsPostDF))){PerilSublimitsPostDF[1,] = NA}
      PerilSublimitsCompDF = cbind(PerilSublimitsPreDF,PerilSublimitsPostDF)
}
colnames(PerilSublimitsCompDF) = colnames(PerilSublimitsCompDF) %>% 
                                  gsub("(.x)$","_Pre",.) %>% gsub("(.y)$","_Post",.)


##TO DO based on the cmparision change the x and y in Pre and Post comparisions


