require(dplyr)



####get from GitHub repository
# library(devtools)
# source_url("https://github.aig.net/raw/sk/Trials/master/AllTrialCodes.R")


source_github <- function(u) {
  # load package
  require(RCurl)
  
  # read script lines from website
  script <- getURL(u, ssl.verifypeer = FALSE)
  
  # parase lines and evealuate in the global environement
  eval(parse(text = script),envir = .GlobalEnv)
}



addressLocNameCleanUp <- function(dfHavingAddress){
          ##improvise this function for having anycolumn
          require(stringr)
  
          dfCleanedUp = dfHavingAddress %>% 
            mutate(cleanedUpAddress = 
                     tolower(gsub("[[:punct:]]|[[:space:]]","",Address)),
                   cleanedUpLocName = 
                     tolower(gsub("[[:punct:]]|[[:space:]]","",`Location Name`)))
          
          return(dfCleanedUp)
          #correct the order of the column if you need.
          }



normaliseSimilarityMatrix <- function(rawSimMatrix, method){
                    if(method == "jw"){
                      return  (100 * (1 -rawSimMatrix ))
                    }  
}

##' @param similarityMatrix Similarity matrix in 100 scale
##' @param MatchPctThreshold Matching percentage threshhold defaulted to 85
##' @Output Dataframe with matching indices, pre on the left and post on the right.
##TODO : 

getMatchingLocIDs = function(similarityMatrix,MatchPctThreshold = 85){
                    MatchDF = data.frame(LocID_Pre = character(),
                                         LocID_Post = character(),
                                         MatchPercentage = character(),
                                         MultipleWith = character(),
                                         stringsAsFactors = FALSE)
                    
                    
                 
                    # similarityMatrix =  AddressSimilarityMatrix
                        for (i in rownames(similarityMatrix))
                        {
                          
                          maxValueInRow = max(similarityMatrix[i,],na.rm = TRUE)
                          maxIndexCol = which(as.numeric(similarityMatrix[i,]) == maxValueInRow)
                          maxColName = colnames(similarityMatrix)[maxIndexCol]
                          maxValueInCol = max(similarityMatrix[,maxColName],na.rm = TRUE)
                            
                              if(length(maxIndexCol)>1){
                                
                                multipleWith = paste(maxColName,collapse = ",")
                                MatchDF = rbind(MatchDF,c(i,"Multiple",maxValueInRow,multipleWith),stringsAsFactors = FALSE)
                                
                              }else if (as.numeric(maxValueInRow) == as.numeric(maxValueInCol) && maxValueInRow >= MatchPctThreshold){
                                MatchDF = rbind(MatchDF,c(i,maxColName,maxValueInRow,""),stringsAsFactors = FALSE)
                                # print(MatchDF)
                              }else{
                                MatchDF = rbind(MatchDF,c(i,NA,maxValueInRow,""),stringsAsFactors = FALSE)
                                # print(MatchDF)
                              }
                          
                            
                          
                        
                        } 
                      colnames(MatchDF) = c("LocID_Pre","LocID_Post","MatchPercentage","MultipleWith") 
                      MatchDF$MatchPercentage = as.numeric(MatchDF$MatchPercentage)
                      
                    return(MatchDF)
                     
}


### function to concantenate the required cols in a dataframe.
##' @return a df with LocID, concantenated Columns
##' @param MultipleLocsDf is the subset of MatchLocs thats has multple matches in the Post Locs
concantColsPlusLocID <- function(LocationDetailsDF,MultipleAndUnmatchedLocs_Cop,...){
  
                  colsToConc = paste(as.list(...))
                          ##gives character vector
                  concantenatedCol = paste0(colsToConc,collapse = "")
  
                  concantRecursive = function(x){
                                  
                                   x[concantenatedCol] = ""  
                                   
                                   for(i in colsToConc){
                                     x[concantenatedCol] = paste0(x[[concantenatedCol]],x[[i]])
                                   }
                                 
                                 return(x) }
  
                  return(     ##main return
                              LocationDetailsDF %>% 
                              filter(LocID %in% MultipleAndUnmatchedLocs_Cop$LocID_Pre) %>% 
                              concantRecursive() %>% 
                              select(LocID,concantenatedCol)
                                
                        )
                              
                  }




###function to treat Multiple matched locations in Pre. 
##' @Param 
treatMultipleMatchLocs =function(MultipleLocs_Cop,leftOverLocs_Cop,
                                 LocationDetailsPreDF_Cop,LocationDetailsPostDF_Cop,
                                 MatchPctThreshold = 85){
                        ##multiple matches and concantenate address
                        ##left over locs conctantenate address
                        ##re attempt similarity matrix
                        ##call getMatchingLocIDs on the genrated similarity matrix
                        ##Rethink : think about only concantenating addresses for pre has matched multiple times
                        require(dplyr)
            
                      ##try wrapping the concatenation into funtictio simplifying the below
            PreLocNameAddress = LocationDetailsPreDF_Cop %>% 
                                filter(LocID %in% MultipleLocs_Cop$LocID_Pre) %>% 
                                (function(x){x$locNamePlusAdd = paste0(x$cleanedUpLocName,x$cleanedUpAddress)
                                 return(x)              
                                }) %>% 
                                select(LocID,locNamePlusAdd)
                                                    
            PostLocNameAddress = LocationDetailsPostDF_Cop %>% 
                                 filter(LocID %in% leftOverLocs_Cop) %>% ###note leftoverlocs
                                 (function(x){x$locNamePlusAdd = paste0(x$cleanedUpLocName,x$cleanedUpAddress)
                                  return(x)              
                                  }) %>% 
                                 select(LocID,locNamePlusAdd)  
            
            MultipleLocsSimilarityMatrix = stringdistmatrix(PreLocNameAddress$locNamePlusAdd,
                                                            PostLocNameAddress$locNamePlusAdd,
                                                            method = "jw")
            
            MultipleLocsSimilarityMatrix = normaliseSimilarityMatrix(MultipleLocsSimilarityMatrix,"jw")
            
            rownames(MultipleLocsSimilarityMatrix) = PreLocNameAddress$LocID
            colnames(MultipleLocsSimilarityMatrix) = PostLocNameAddress$LocID
            rematchMultipleLocs = getMatchingLocIDs(MultipleLocsSimilarityMatrix,MatchPctThreshold)
            
            return(rematchMultipleLocs)
  
}




treatMultipleMatchLocsVersionOcc =function(MultipleLocs_Cop,leftOverLocs_Cop,
                                 LocationDetailsPreDF_Cop,LocationDetailsPostDF_Cop,
                                 MatchPctThreshold = 85){
  ##Think if occupancy or Code shouold be concantenated
  ##Rethink : think about only concantenating addresses for pre has matched multiple times
                          require(dplyr)
                          
                          ##try wrapping the concatenation into funtictio simplifying the below
        PreLocNameAddress = LocationDetailsPreDF_Cop %>% 
                            filter(LocID %in% MultipleLocs_Cop$LocID_Pre) %>% 
                            (function(x){x$locNameAddOcc = paste0(x$cleanedUpLocName,x$cleanedUpAddress,x$Occupancy)
                              return(x)              
                            }) %>% 
                            select(LocID,locNameAddOcc)
                          
        PostLocNameAddress = LocationDetailsPostDF_Cop %>% 
                            filter(LocID %in% leftOverLocs_Cop) %>% ###note leftoverlocs
                            (function(x){x$locNameAddOcc = paste0(x$cleanedUpLocName,x$cleanedUpAddress,x$Occupancy)
                            return(x)              
                            }) %>% 
                            select(LocID,locNameAddOcc)  
                          
        MultipleLocsSimilarityMatrix = stringdistmatrix(PreLocNameAddress$locNameAddOcc,
                                                        PostLocNameAddress$locNameAddOcc,
                                                        method = "jw")
                          
        MultipleLocsSimilarityMatrix = normaliseSimilarityMatrix(MultipleLocsSimilarityMatrix,"jw")
                          
              rownames(MultipleLocsSimilarityMatrix) = PreLocNameAddress$LocID
              colnames(MultipleLocsSimilarityMatrix) = PostLocNameAddress$LocID
              rematchMultipleLocs = getMatchingLocIDs(MultipleLocsSimilarityMatrix,MatchPctThreshold)
              return(rematchMultipleLocs)
  
}

####updates all subsets of MatchLocs
updateAllSubsets <- function(){
  assign ("MatchedLocs", MatchLocs %>% filter(is.na(LocID_Post) == FALSE & LocID_Post != "Multiple"),envir = .GlobalEnv)
  assign ("UnmatchedLocs",MatchLocs %>% filter(is.na(LocID_Post) == TRUE),envir = .GlobalEnv)
  assign( "MultipleLocs", MatchLocs %>% filter(LocID_Post == "Multiple"),envir = .GlobalEnv)
  assign( "MultipleAndUnmatchedLocs", MatchLocs %>% filter(LocID_Post == "Multiple"|is.na(LocID_Post)),envir = .GlobalEnv)
  assign( "leftOverLocs", LocationDetailsPostDF$LocID[!(LocationDetailsPostDF$LocID %in% MatchedLocs$LocID_Post)],envir = .GlobalEnv)
}


##' @param MultipleLocsDf Subset of Match Locs.
##' @param colName the column name where the Multiple matches with in Post are presetn
##' @return Long format of Loc pre , Loc post , Match% and Multiple with Marked as Multiple
splitMultipleWith <- function(MultipleLocsCopy,colName){
                    #Check for MultipleLocs Empty or not
                    if(nrow(MultipleLocsCopy)){
                            require(tidyr)               
                            result =  str_split(MultipleLocsCopy[[colName]],",",simplify = TRUE)
                            result =  cbind(MultipleLocsCopy %>% select(-MultipleWith),result)
                            result = result %>% mutate(LocID_Pre = as.numeric(LocID_Pre))
                            result = gather(result, Condition, LocationsInPost,4:ncol(result), factor_key=TRUE)
                            result = result %>% arrange(LocID_Pre)
                            result = result %>% filter(LocationsInPost != "")
                            result = result %>% 
                                     select(LocID_Pre,LocationsInPost,MatchPercentage,LocID_Post) %>%
                                     rename(LocID_Post = LocationsInPost,MultipleWith = LocID_Post)
                            return(result)
                    }

}


##' validates the matched locations on country and then city
##' @param LocationDetailsPreDF  
##' @param LocationDetailsPostDF
##' @return FinalEnsembleLocsDF
validateMatches <- function(LocationDetailsPreDF_Copy,
                            LocationDetailsPostDF_Copy,
                            FinalEnsembleLocs_Copy){
  
       # add any more criteria to Verify here that require exact match
      verifyOn_InOrder = c("Country.x","State.x","City.x","Country.y","State.y","City.y")
  
          
                
      requiredDF =    FinalEnsembleLocs_Copy %>% 
                      mutate(LocID_Pre = as.numeric(LocID_Pre),
                      LocID_Post = as.numeric(LocID_Post)) %>%
                      inner_join(LocationDetailsPreDF_Copy,by = c("LocID_Pre" = "LocID")) %>% 
                      inner_join(LocationDetailsPostDF_Copy,by = c("LocID_Post" = "LocID")) %>% 
                      select(colnames(FinalEnsembleLocs_Copy),verifyOn_InOrder)
      
      ###extracting the subsets based on the Long Df
      requiredMatchedLocs = requiredDF %>% 
                            filter(is.na(LocID_Post) == FALSE & MultipleWith != "Multiple")
              
      requiredMultipleLocs = requiredDF %>% 
                             filter(is.na(LocID_Post) == FALSE & MultipleWith == "Multiple")
      
      requiredNALocs =    requiredDF %>% 
                          filter(is.na(LocID_Post) == TRUE)
        
     
        
      
      
      #discarding wrong matches based city and couuntry for matched Locs 
      
      requiredMacthedLocs = requiredMatchedLocs %>% 
                            mutate(LocID_Post = ifelse(complete.cases(Country.x,Country.y ) & Country.x != Country.y , NA, LocID_Post),
                                   LocID_Post = ifelse(complete.cases(State.x,State.y ) & State.x != State.y , NA, LocID_Post),
                                   LocID_Post = ifelse(complete.cases(City.x,City.y ) & City.x != City.y , NA, LocID_Post),
                                   MatchPercentage = ifelse(complete.cases(Country.x,Country.y ) & Country.x != Country.y , "", MatchPercentage),
                                   MatchPercentage = ifelse(complete.cases(State.x,State.y ) & State.x != State.y , "", MatchPercentage),
                                   MatchPercentage = ifelse(complete.cases(City.x,City.y ) & City.x != City.y , "", MatchPercentage)
                                   )
     
       ##discarding wrong matches based city and country for Multiple Locs
      requiredMultipleLocs = requiredMultipleLocs %>% 
                             mutate(MultipleWith = ifelse(complete.cases(Country.x,Country.y ) & Country.x != Country.y , "Reject", MultipleWith),
                                    MultipleWith = ifelse(complete.cases(State.x,State.y ) & State.x != State.y , "Reject", MultipleWith),
                                    MultipleWith = ifelse(complete.cases(City.x,City.y ) & City.x != City.y , "Reject", MultipleWith)
                                   
                                    ) %>% 
                             filter(MultipleWith != "Reject")
      
      ##Cleaning up the requiredMultipleLocs when Locs are not multiple
      requiredMultipleLocs = requiredMultipleLocs %>%
                             add_count(LocID_Pre) %>% 
                             mutate(MultipleWith = ifelse(n <=1 ,"",MultipleWith)) %>% 
                             select(colnames(requiredMultipleLocs))
                             
      
      return( 
              rbind(requiredMacthedLocs,requiredMultipleLocs,requiredNALocs) %>% 
              select(colnames(FinalEnsembleLocs_Copy))
             )
        
                  
  
}



