

library(readxl)
library(dplyr)
library(stringdist)
require(RCurl)
require(stringr)
library(tidyr)
library(shiny)
source("LocationMatchingAlgo_Functions.R")# to be changed to github
# source("//pngsfsdg04/AnalyticsCOE/@CATModeling/@CATAccountModeling/@Teams/@International/@APAC/Projects/Automation/Comparision Tool/Global Funtions/globalFunctions.R")

#Global Constants
# MatchThreshold = 85
ValidateMatchesYN = TRUE



##Uncomment the below to run directyl the script
# MSTRExcelWbPathPre = choose.files(caption = "Choose Pre-Quote export")
# MSTRExcelWbPathPost = choose.files(caption = "Choose Post-Bind export")


requiredSheets = c("Line Guide","Location Peril Region AAL","Location Details")

LocationDetailsPreDF = read_excel(MSTRExcelWbPathPre,sheet = requiredSheets[3],skip = 5 )
LocationDetailsPostDF = read_excel(MSTRExcelWbPathPost,sheet = requiredSheets[3],skip = 5 )


##TO DO: Handle unk_ locations groupthem and remove from the Dataframe
## append them last while comparision

LocationDetailsPreDF =  LocationDetailsPreDF %>% 
                        mutate(LocID = seq(1,nrow(LocationDetailsPreDF),1)) %>% 
                        select(LocID,everything()) 
                       
LocationDetailsPostDF = LocationDetailsPostDF %>% 
                       mutate(LocID = seq(1,nrow(LocationDetailsPostDF),1)) %>% 
                       select(LocID,everything()) 


##Clean up address strings

LocationDetailsPreDF = LocationDetailsPreDF %>% addressLocNameCleanUp()
LocationDetailsPostDF = LocationDetailsPostDF %>% addressLocNameCleanUp()

##TODo: Ensemble of different matching algorithms 
##TODO: Group locations if address,locname,occupancy is same and assign a groupid


AddressSimilarityMatrix = stringdistmatrix(LocationDetailsPreDF$cleanedUpAddress,
                                           LocationDetailsPostDF$cleanedUpAddress, 
                                           method = "jw")
# fix(LocationDetailsPreDF)



rownames(AddressSimilarityMatrix) = LocationDetailsPreDF$LocID
colnames(AddressSimilarityMatrix) = LocationDetailsPostDF$LocID

AddressSimilarityMatrix  = normaliseSimilarityMatrix(AddressSimilarityMatrix,"jw")
# AddressSimilarityMatrix = as.data.frame(AddressSimilarityMatrix)

MatchLocs = getMatchingLocIDs(AddressSimilarityMatrix,MatchThreshold)


##rematching locations
##TO DO: rethink logic
##     : Generalise and wrap into funcition(MatchLocs, AddressSimilarityMatrix)
a = updateAllSubsets()

##attempt rematch only if there are left over locs and for NAs
if(length(leftOverLocs)>0 & length(UnmatchedLocs$LocID_Pre) > 0){
  
  reMatchLocs = getMatchingLocIDs(AddressSimilarityMatrix[UnmatchedLocs$LocID_Pre,leftOverLocs],MatchThreshold)
    if (nrow(reMatchLocs)>0)
    {
      tempRematchTable = MatchLocs %>%  left_join(reMatchLocs,by = c("LocID_Pre" = "LocID_Pre")) 
      tempRematchTable[!complete.cases(tempRematchTable$LocID_Post.x),c("LocID_Post.x","MatchPercentage.x","MultipleWith.x")] = 
            tempRematchTable[!complete.cases(tempRematchTable$LocID_Post.x),c("LocID_Post.y","MatchPercentage.y","MultipleWith.y")] 
      
      MatchLocs = tempRematchTable %>% 
                  select(LocID_Pre,LocID_Post.x,MatchPercentage.x,MultipleWith.x) %>% 
                  rename(LocID_Post = LocID_Post.x,
                         MatchPercentage = MatchPercentage.x,
                         MultipleWith = MultipleWith.x )##complete transfering rematched locations
      
    }
}
      
    

##treat multiples matches
##' TODO :
##' 
##    Generate LocName+Address match similarity for multiples 
##    Generalise and wrap into funcition(MatchLocs, )

###Attempt address+LocName for both Multiple and Unmatched
##update all the subsets: Matched Locs 
a = updateAllSubsets()

if(nrow(MultipleLocs)>1 && length(leftOverLocs) > 1){
  rematchMultipleLocs = treatMultipleMatchLocs( MultipleAndUnmatchedLocs,
                                                leftOverLocs,
                                                LocationDetailsPreDF,
                                                LocationDetailsPostDF,
                                                MatchThreshold)
    if(nrow(rematchMultipleLocs)>1){
        
      tempRematchTable = MatchLocs %>%  left_join(rematchMultipleLocs,by = c("LocID_Pre" = "LocID_Pre")) 
      
      tempRematchTable[tempRematchTable$LocID_Post.x == "Multiple" | is.na(tempRematchTable$LocID_Post.x),c("LocID_Post.x","MatchPercentage.x","MultipleWith.x")] = 
        tempRematchTable[tempRematchTable$LocID_Post.x == "Multiple" | is.na(tempRematchTable$LocID_Post.x),c("LocID_Post.y","MatchPercentage.y","MultipleWith.y")] 
      
      MatchLocs = tempRematchTable %>% 
        select(LocID_Pre,LocID_Post.x,MatchPercentage.x,MultipleWith.x) %>% 
        rename(LocID_Post = LocID_Post.x,
               MatchPercentage = MatchPercentage.x,
               MultipleWith = MultipleWith.x )
      
      
    }


}


###Attempt address +Loc Name + Occupancy Desc and Occupancy code
##for both Multiple and UnmatchedLocs
a = updateAllSubsets()

if(nrow(MultipleLocs)>1 && length(leftOverLocs) > 1){
  rematchMultipleLocs = treatMultipleMatchLocsVersionOcc( MultipleAndUnmatchedLocs,
                                                          leftOverLocs,
                                                          LocationDetailsPreDF,
                                                          LocationDetailsPostDF,MatchThreshold)
  if(nrow(rematchMultipleLocs)>1){
    
        tempRematchTable = MatchLocs %>%  left_join(rematchMultipleLocs,by = c("LocID_Pre" = "LocID_Pre")) 
        
        tempRematchTable[tempRematchTable$LocID_Post.x == "Multiple" | is.na(tempRematchTable$LocID_Post.x),c("LocID_Post.y","MatchPercentage.y","MultipleWith.y")] = 
          tempRematchTable[tempRematchTable$LocID_Post.x == "Multiple" | is.na(tempRematchTable$LocID_Post.x),c("LocID_Post.y","MatchPercentage.y","MultipleWith.y")] 
        
        MatchLocs = tempRematchTable %>% 
          select(LocID_Pre,LocID_Post.x,MatchPercentage.x,MultipleWith.x) %>% 
          rename(LocID_Post = LocID_Post.x,
                 MatchPercentage = MatchPercentage.x,
                 MultipleWith = MultipleWith.x )
        
        
    }
}


##Any other dives in making the matches perfect goes here
##1st Construction , 2nd TIV matrix.
##TO DO Last : verify at least the matches by first city(if present in both) and then country








##converting the 'Multiple with' col to Long Format
MultipleLocs_Long = splitMultipleWith(MultipleLocs,"MultipleWith")


##Assemble Matched , Multiple and Unmatched
FinalEnsembleLocs = rbind(MatchedLocs,MultipleLocs_Long,UnmatchedLocs)


##TO DO in verification also keep a check on threshold pct for Multiple locs
if(ValidateMatchesYN){
    FinalEnsembleLocs = validateMatches(LocationDetailsPreDF,
                                        LocationDetailsPostDF,
                                         FinalEnsembleLocs)
}

##LocComparisionDf joining all the Pre,Post and MatchLocs
LocComparisionDF = FinalEnsembleLocs %>% 
                   mutate(LocID_Pre = as.numeric(LocID_Pre),LocID_Post = as.numeric(LocID_Post)) %>% 
                   full_join(LocationDetailsPreDF,by = c("LocID_Pre" = "LocID")) %>% 
                   full_join(LocationDetailsPostDF,by = c("LocID_Post" = "LocID"))

colnames(LocComparisionDF) = colnames(LocComparisionDF) %>% 
                             gsub("(.x)$","_Pre",.) %>% gsub("(.y)$","_Post",.)


