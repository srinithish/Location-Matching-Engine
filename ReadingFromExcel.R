### load MSTR workbook
source("ReadingFromExcel_Functions.R")
# library(XLConnect)
library(readxl)


# MSTRExcelWbPathPre = choose.files(caption = "Choose Pre-Quote export")
# MSTRExcelWbPathPost = choose.files(caption = "Choose Post-Bind export")
requiredSheets = c("Line Guide","Location Peril Region AAL","Location Details")
# MSTRExcelWb = loadWorkbook(MSTRExcelWbPath,create = FALSE)

###load sheets


LineGuidePreDF = read_excel(MSTRExcelWbPathPre,sheet = "Line Guide")
LineGuidePostDF = read_excel(MSTRExcelWbPathPost,sheet = "Line Guide")

##extracting relavant DFs Pre
##TODO : Handle No Data Available rows

AccountDetailsPreDF =   LineGuidePreDF %>% 
                        extractDFfromLineGuide("Account Name") %>% 
                        removeAllNAColumns() %>% .[,1:2]

PolicyLevelTermsPreDF = LineGuidePreDF %>% 
                        extractDFfromLineGuide("Policy Level Terms") %>% 
                        cleanUpExtractedDF()

#handle layername differenelty
TCPPreDF  = LineGuidePreDF %>%
            extractDFfromLineGuide_ForLayerName("Layer Name") %>% 
             cleanUpExtractedDF()

PerilLevelTermsPreDF =  LineGuidePreDF %>% 
                        extractDFfromLineGuide("Peril Level Terms") %>% 
                        cleanUpExtractedDF()

PerilSublimitsPreDF =  LineGuidePreDF %>% 
                       extractDFfromLineGuide("Peril Sublimits") %>% 
                       cleanUpExtractedDF()

LocationGroupSublimitsPreDF = LineGuidePreDF %>% 
                              extractDFfromLineGuide("Location Group Sublimits") %>% 
                              cleanUpExtractedDF()



### Extract relavant DFs for Post

AccountDetailsPostDF =   LineGuidePostDF %>% 
                        extractDFfromLineGuide("Account Name") %>% 
                        removeAllNAColumns() %>% .[,1:2]


PolicyLevelTermsPostDF = LineGuidePostDF %>% 
                        extractDFfromLineGuide("Policy Level Terms") %>% 
                        cleanUpExtractedDF()

#handle layername differenelty
TCPPostDF  = LineGuidePostDF %>%
                   extractDFfromLineGuide_ForLayerName("Layer Name") %>% cleanUpExtractedDF()

PerilLevelTermsPostDF =  LineGuidePostDF %>% 
                        extractDFfromLineGuide("Peril Level Terms") %>% 
                        cleanUpExtractedDF()

PerilSublimitsPostDF =  LineGuidePostDF %>% 
                       extractDFfromLineGuide("Peril Sublimits") %>% 
                       cleanUpExtractedDF()

LocationGroupSublimitsPostDF = LineGuidePostDF %>% 
                              extractDFfromLineGuide("Location Group Sublimits") %>% 
                              cleanUpExtractedDF()


##QUestion: Difference between Location Details and Line Guide Location details
##Q : will the number of locations increase when there are more layers in line guide
##Q: 


