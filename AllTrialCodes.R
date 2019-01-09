print("hello")

library(devtools)
source_url("https://github.aig.net/raw/sk/Trials/master/AllTrialCodes.R")


myDf = mtcars
rm(a)
myDf %>% mutate(.,groupID = group_indices(.,cyl)) 
seq_along(c(17,11,17,18))

AddressSimilarityMatrix %>% class()
MatchLocs %>% View()

LocationDetailsPreDF %>% addressLocNameCleanUp() %>% head() %>% View()

LocationDetailsPreDF["Address"]

x = mtcars
myFunc = function(x,...){
  s = list(...)
  # print(x) 
  print(mtcars[s[[1]]])
  
}

HelloWorld <- function(...) {
  arguments <- list(...)
  paste(arguments)
}
HelloWorld("me")
myFunc(mtcars,"mpg")



LocationDetailsPreDF$LocID %in% length(MultipleLocs$LocID_Pre)

paste0(LocationDetailsPreDF$cleanedUpAddress,LocationDetailsPreDF$cleanedUpLocName)

locNamePlusAdd = "locNamePlusAdd"
cleanedUpLocName = "cleanedUpLocName"
cleanedUpAddress  ="cleanedUpAddress"

LocationDetailsPreDF %>% 
  filter(LocID %in% MultipleLocs$LocID_Pre) %>% 
  (function(x){x[locNamePlusAdd] = paste0(as.character(x[[cleanedUpLocName]]),as.character(x[[cleanedUpAddress]]))
  return(x)              
  }) %>% 
  select(LocID,locNamePlusAdd) %>% head() %>% View()

# class(as.character(mtcars["mpg"])
# 
# rm(PostLocNameAddresss)
# 
# 
# y= 5,


myFunc = function(x){
  y=6
  y <<- 7
  print(y)
}


myFunc(y)

1 - stringdist("1234","1245",method = "jw")

myFunc = function(x,...){
  print(...)
}

list(1,2,3,4) %>% length()
strin = myFunc(5,3,4)
class(strin)
strin[1]
paste0(strin,collapse = "")

mtcars %>% select("mpg")
concantColsPlusLocID(LocationDetailsPreDF,MultipleLocs = MultipleLocs,c("cleanedUpLocName","cleanedUpAddress"))


concantRecursive = function(x){
  
  x[concantenatedCol] = ""  
  
  for(i in colsToConc){
    x[concantenatedCol] = paste0(x[[i]],x[[concantenatedCol]])
  }
  
  return(x) }

concantRecursive(mt)

### do not delete
LocationDetailsPreDF %>% 
  filter(LocID %in% MultipleLocs$LocID_Pre) %>% 
  (function(x){x$locNamePlusAdd = paste0(x$cleanedUpLocName,x$cleanedUpAddress)
  return(x)              
  }) %>% 
  select(LocID,locNamePlusAdd)


stringdist("14 W 34th St, New York, NY, 10001","350 5th Ave, New York, NY, 10001",method = "jw")
  




#####


similarityMatrix =  AddressSimilarityMatrix
i = 5
maxValueInRow = max(similarityMatrix[i,],na.rm = TRUE)
maxIndexCol = which(as.numeric(similarityMatrix[i,]) == maxValueInRow)
maxColName = colnames(similarityMatrix)[maxIndexCol]
maxValueInCol = max(similarityMatrix[,maxColName],na.rm = TRUE)
class(maxColName)
paste(maxColName,collapse = ",")
rm(maxIndexCol,maxValueInRow,maxColName,maxValueInCol)

###gather and wide
olddata_wide <- read.table(header=TRUE, text='
                     subject sex control cond1 cond2
                           1   M     NA   NA    10.7
                           2   F     6.3  10.6  11.1
                           3   F     9.5  13.1  13.8
                           4   M    11.5  13.4  12.9
                           ')
olddata_wide$subject <- factor(olddata_wide$subject)
library(tidyr)
data_long <- gather(olddata_wide[-5], condition, measurement,3:, factor_key=TRUE)


####



before <- data.frame(attr = c(1,30,4,6), type=c('foo_and_bar_and_me','foo_and_bar_2'))  
out <- strsplit(as.character(before$type),'_and_') 
do.call(rbind, out)
rm(a)

data.frame(a = 1:10,b=1:5)
str_split("1,2,3",",",simplify = TRUE)

temp = splitMultipleWith(MultipleLocs,"MultipleWith")

mtcars %>% select(-mpg)
temp$LocID_Pre = as.numeric(temp$LocID_Pre)
del = gather(temp, condition, measurement,4:ncol(temp), factor_key=TRUE)
del = del %>% arrange(LocID_Pre)
del2 =  str_split(MultipleLocs[["MultipleWith"]],",",simplify = TRUE)
require(dplyr)
mtcars %>% select(mpg) %>% .[1:2,]

rm(maxIndexCol,maxValueInCol,maxValueInRow)

extractDFfromLineGuide(LineGuidePreDF,"Policy Level Terms") %>%
cleanUpExtractedDF() %>% View()

titleRowStart = which(LineGuidePreDF[["Microstrategy"]] == "Policy Level Terms")


firstNA = LineGuidePreDF[["Microstrategy"]] %>% 
          .[titleRowStart:length(.)] %>% 
            is.na() %>% which() %>% .[1]


2%%2

sapply(UnmatchedLocs,class)


colnames(LocComparisionDF) %>% gsub("(.x)$","_Pre",.) %>% gsub("(.y)$","_Post",.)


ifelse(c(1,2,3)%%2 == 0,TRUE,FALSE)


MatchedLocs %>% 
  mutate(LocID_Pre = as.numeric(LocID_Pre),
         LocID_Post = as.numeric(LocID_Post)) %>%
  
  inner_join(LocationDetailsPreDF,by = c("LocID_Pre" = "LocID")) %>% 
  inner_join(LocationDetailsPostDF,by = c("LocID_Post" = "LocID")) %>% select(c("LocID_Pre"),"LocID_Post")


mtcars %>% filter(complete.cases(mpg,cyl))

x = 10
for(i in seq(2,x,1)){
  if (x%%i==0 & i < x ){
    print("number is composite")
    break
  }else if(i==x){
    print("number is prime")
  }
  
}

verifyOn_InOrder = c("Country.x","City.x","Country.y","City.y")

require(dplyr)

requiredDF  = FinalEnsembleLocs %>% 
  mutate(LocID_Pre = as.numeric(LocID_Pre),
         LocID_Post = as.numeric(LocID_Post)) %>%
  inner_join(LocationDetailsPreDF,by = c("LocID_Pre" = "LocID")) %>% 
  inner_join(LocationDetailsPostDF,by = c("LocID_Post" = "LocID")) %>% 
  select(colnames(FinalEnsembleLocs),verifyOn_InOrder)


rm(requiredDF)
requiredDF %>% 
  filter(complete.cases(Country.x,Country.y)) %>% 
  filter(Country.x != Country.y)
a = 10
a = "a"
assign(a, 20,envir = .GlobalEnv)
rm(a)

row_number(c(1,1,2,3,3,4))
requiredMultipleLocs %>%
count(LocID_Pre) %>% ungroup()

View(mtcars)
require(dplyr)
mtcars %>% add_count(cyl) %>% View()


requiredMultipleLocs %>%
 add_count(LocID_Pre) %>% View()




runUrl("https://github.aig.net/raw/sk/Comparision-Tool-Shared/master/ComparisionToolUI.R")

shiny:: runApp("//pngsfsdg04/AnalyticsCOE/@CATModeling/@CATAccountModeling/@Teams/@International/@APAC/Projects/Automation/Comparision Tool/Nithish Module")

LocComparisionDF %>% arrange(LocID_Pre,LocID_Post,MultipleWith) %>% View()



fluidRow(
  column(3,plotOutput("AIGParticipationPlot")),
         
  column(3,plotOutput("PolicyLimitPlot"))
  
)
which(x = c(FALSE,FALSE))

mapply(sum, 1:4)
with(mtcars,mtcars[cyl >5,])

removeAllNAColumns(NULL)
length(dim(as.tibble(mtcars)))

gsub("(\\Q.\\E|_)(.*)$","","loc_Pre")
grep()
placeSimilarColsSidebySide(PolicyLevelTermsPreDF,PolicyLevelTermsPostDF,"Layer Name")
any(grepl("No.data.available",c(colnames(PerilLevelTermsPreDF),colnames(PerilLevelTermsPostDF))))


convertedCol  = LocationDFtoOutput %>% mutate(`Building Value_Pre` = gsub(",","",`Building Value_Pre`),
                                              `Building Value_Pre` = as.numeric(`Building Value_Pre`))
sapply(convertedCol,class)
rm(convertedCol)
withProgress(message = 'Making plot', value = 0,{
  source("LocationMatchingAlgo.R")
  source("LocationComparision.R")
  source("ReadingFromExcel.R")
  source("PolicyLevelComparision.R")}
)


require(ggplot2)
ggplot()+geom_col(mapping = aes(x = as.factor (c("Me","you")),y = c(30,30)))


mtcars %>% select(mpg)

FinalReportWb = createWorkbook()
SummaryWorksheet = addWorksheet(FinalReportWb,"Summary")
header = createStyle(fgFill = "#DCE6F1",halign = "Left",textDecoration = "bold")
rowNameStyle = createStyle(fgFill = "#D3D6D3",halign = "Left",textDecoration = "bold",
                           border = "TopBottomLeftRight")

writeData(FinalReportWb,"Summary",x = AccountDetailsPreDF,startCol = 2,startRow = 2,
          borders = "all",borderColour = "black",headerStyle = header,colNames = FALSE)
addStyle(FinalReportWb,"Summary",style = rowNameStyle , 2:nrow(AccountDetailsPreDF),cols = 2)
saveWorkbook(FinalReportWb,file = "Final.xlsx",overwrite = TRUE)

getwd()
which(colnames(LocationDFtoOutput)=="Occupancy_Pre")
colnames(LocationDFtoOutput)
attributes(c(1,2))


library(shiny)
runUrl("https://github.aig.net/sk/Comparision-Tool-Shared2/archive/master.tar.gz",launch.browser = TRUE,host = "0.0.0.0",port = 3168)

### http://10.199.55.136:3168


ggplot(mpg, aes(fl, fill = drv))

colnames(mtcars)



