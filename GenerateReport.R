
require(openxlsx)
source("GenerateReport_Functions.R")
startRange = c(2,2)
#GOlbalValues
ValueIndicatingChange = 10
ValueIndicatingPctChange = 10






##have createstyle and have a get nextRow or currentRange

TableNameStyle = createStyle(fgFill = "#8FD8D8",halign = "Left",textDecoration = "bold",
                             border = "TopBottomLeftRight",fontSize = 11)

rowNameStyle = createStyle(fgFill = "#D3D6D3",halign = "Left",textDecoration = "bold",
                           border = "TopBottomLeftRight")

headerStyle = createStyle(fgFill = "#D3D6D3",halign = "Left",textDecoration = "bold",
                          border = "TopBottomLeftRight")

numberFormating = createStyle(numFmt = "0.00")

changeIndicatorStyle = createStyle(fgFill = "#ff9900")

NumericChangeIndicatorStyle = createStyle(fgFill = "#ff9900",numFmt = "0.00")


###Create Wb and SSheets AND anychanges to Wb
FinalReportWb = createWorkbook()
SummaryWorksheet = addWorksheet(FinalReportWb,"Summary")
LocationDetailsWorksheet = addWorksheet(FinalReportWb,"LocationComparision")


modifyBaseFont(FinalReportWb, fontSize = 8, fontColour = "black",
               fontName = "Helvetica")

setRowHeights(FinalReportWb, "Summary", rows = 1:200, heights = 15)



##Write Account Details Pre
#TableName:
writeData(FinalReportWb,"Summary",x = "PreQuote",
          startRow = startRange[1],
          startCol = startRange[2],
          borders = "all",borderColour = "black",colNames = TRUE,
          headerStyle = TableNameStyle )


mergeCells(FinalReportWb,"Summary",rows = startRange[1],
           cols = startRange[2]:(startRange[2]+ ncol(AccountDetailsPreDF)-1))

addStyle(FinalReportWb,"Summary",style = TableNameStyle,
         rows = startRange[1],
         cols = startRange[2]: (startRange[2]+ ncol(AccountDetailsPreDF)-1))

nextRange = c(startRange[1]+1,startRange[2])
#### Data:
writeData(FinalReportWb,"Summary",x = AccountDetailsPreDF,
          startRow = startRange[1]+1,
          startCol = startRange[2],
          borders = "all",borderColour = "black",colNames = FALSE)

addStyle(FinalReportWb,"Summary",style = rowNameStyle,
         rows = nextRange[1]:(nrow(AccountDetailsPreDF)+nextRange[1]-1),
         cols = nextRange[2])

##Write Account Details Post
#TableName:
nextRange = c(startRange[1],startRange[2]+ncol(AccountDetailsPreDF)+1)
writeData(FinalReportWb,"Summary",x = "Post-bind",
          startRow = nextRange[1],
          startCol = nextRange[2],
          borders = "all",borderColour = "black",colNames = TRUE,
          headerStyle = TableNameStyle )


mergeCells(FinalReportWb,"Summary",rows = nextRange[1],
           cols = nextRange[2]:(nextRange[2]+ ncol(AccountDetailsPostDF)-1))

addStyle(FinalReportWb,"Summary",style = TableNameStyle,
         rows = nextRange[1],
         cols = nextRange[2]:(nextRange[2]+ ncol(AccountDetailsPostDF)-1))

nextRange = c(nextRange[1]+1,nextRange[2])
#### Data:
writeData(FinalReportWb,"Summary",x = AccountDetailsPreDF,
          startRow = nextRange[1],
          startCol = nextRange[2],
          borders = "all",borderColour = "black",colNames = FALSE)

addStyle(FinalReportWb,"Summary",style = rowNameStyle,
         rows = nextRange[1]:(nrow(AccountDetailsPostDF)+nextRange[1]-1),
         cols = nextRange[2])




####################Plots##################################
plot(AIGParticipationPlotExp)

insertPlot(FinalReportWb,"Summary",
           width = 2.5,height = 3,
           startRow = 2,startCol = 7)


plot(PolicyLimitPlotExp)

insertPlot(FinalReportWb,"Summary",
           width = 3,height = 3,
           startRow = 2,startCol = 9)


plot(NoOfLocationsPlotExp)

insertPlot(FinalReportWb,"Summary",
            width = 3,height = 3,
            startRow = 18,startCol = 2)


############################################Vishals edit #################################


PlotsEndRange = c(33,2)
##########################################################

##Write Policy Level Term
#TableName:
nextRange = c(PlotsEndRange[1],PlotsEndRange[2])

writeData(FinalReportWb,"Summary",x = "Policy Level Terms",
          startRow = nextRange[1],
          startCol = nextRange[2],
          borders = "all",borderColour = "black",colNames = TRUE,
          headerStyle = TableNameStyle )


mergeCells(FinalReportWb,"Summary",rows = nextRange[1],
           cols = nextRange[2]:(nextRange[2]+ ncol(PolicyLevelTermsCompDF)-1))

addStyle(FinalReportWb,"Summary",style = TableNameStyle,
         rows = nextRange[1],
         cols = nextRange[2]:(nextRange[2]+ ncol(PolicyLevelTermsCompDF)-1))

nextRange = c(nextRange[1]+1,nextRange[2])
#### Data:
writeData(FinalReportWb,"Summary",x = PolicyLevelTermsCompDF,
          startRow = nextRange[1],
          startCol = nextRange[2],
          borders = "all",borderColour = "black",colNames = TRUE,
          headerStyle = headerStyle)



##Write Peril TCP
#TableName:
nextRange = c(nextRange[1]+nrow(PolicyLevelTermsCompDF)+2,nextRange[2])

writeData(FinalReportWb,"Summary",x = "Peril TCP",
          startRow = nextRange[1],
          startCol = nextRange[2],
          borders = "all",borderColour = "black",colNames = TRUE,
          headerStyle = TableNameStyle )


mergeCells(FinalReportWb,"Summary",rows = nextRange[1],
           cols = nextRange[2]:(nextRange[2]+ ncol(TCPCompDF)-1))

addStyle(FinalReportWb,"Summary",style = TableNameStyle,
         rows = nextRange[1],
         cols = nextRange[2]:(nextRange[2]+ ncol(TCPCompDF)-1))

nextRange = c(nextRange[1]+1,nextRange[2])
#### Data:
writeData(FinalReportWb,"Summary",x = TCPCompDF,
          startRow = nextRange[1],
          startCol = nextRange[2],
          borders = "all",borderColour = "black",colNames = TRUE,
          headerStyle = headerStyle)



##Write Peril Level Terms
#TableName:
nextRange = c(nextRange[1]+nrow(TCPCompDF)+2,nextRange[2])

writeData(FinalReportWb,"Summary",x = "Peril Level Term",
          startRow = nextRange[1],
          startCol = nextRange[2],
          borders = "all",borderColour = "black",colNames = TRUE,
          headerStyle = TableNameStyle )


mergeCells(FinalReportWb,"Summary",rows = nextRange[1],
           cols = nextRange[2]:(nextRange[2]+ ncol(PerilLevelTermsCompDF)-1))

addStyle(FinalReportWb,"Summary",style = TableNameStyle,
         rows = nextRange[1],
         cols = nextRange[2]:(nextRange[2]+ ncol(PerilLevelTermsCompDF)-1))

nextRange = c(nextRange[1]+1,nextRange[2])
#### Data:
writeData(FinalReportWb,"Summary",x = PerilLevelTermsCompDF,
          startRow = nextRange[1],
          startCol = nextRange[2],
          borders = "all",borderColour = "black",colNames = TRUE,
          headerStyle = headerStyle)



##Write Peril Sublimits
#TableName:
nextRange = c(nextRange[1]+nrow(PerilLevelTermsCompDF)+2,nextRange[2])

writeData(FinalReportWb,"Summary",x = "Peril Sublimits",
          startRow = nextRange[1],
          startCol = nextRange[2],
          borders = "all",borderColour = "black",colNames = TRUE,
          headerStyle = TableNameStyle )


mergeCells(FinalReportWb,"Summary",rows = nextRange[1],
           cols = nextRange[2]:(nextRange[2]+ ncol(PerilSublimitsCompDF)-1))

addStyle(FinalReportWb,"Summary",style = TableNameStyle,
         rows = nextRange[1],
         cols = nextRange[2]:(nextRange[2]+ ncol(PerilSublimitsCompDF)-1))

nextRange = c(nextRange[1]+1,nextRange[2])

#### Data:
writeData(FinalReportWb,"Summary",x = PerilSublimitsCompDF,
          startRow = nextRange[1],
          startCol = nextRange[2],
          borders = "all",borderColour = "black",colNames = TRUE,
          headerStyle = headerStyle)





#######################################end vishals edit##########################################












#############Locations Details###########################



writeData(FinalReportWb,"LocationComparision",x = LocationDFtoOutput,
          startRow = 1,startCol = 1,
          borders = "columns",borderColour = "black",colNames = TRUE,headerStyle = headerStyle)



##TO DO Optimise
for (i in 29:53){
      addStyle(FinalReportWb,"LocationComparision",style = numberFormating,
               rows = 2:(nrow(LocationDFtoOutput)+1),
               cols = i)
}

IndicateChange("Occupancy_Pre","Occupancy_Post",LocationDFtoOutput,sheet = "LocationComparision")

IndicateChange("Construction_Pre","Construction_Post",LocationDFtoOutput,"LocationComparision")

IndicateNumericChange("GroundUpAAL_Change","GroundUpAAL%Change",
                      LocationDFtoOutput,"LocationComparision",
                      ValueIndicatingChange,ValueIndicatingPctChange)

IndicateNumericChange("GrossAAL_Change","GrossAAL%Change",
                      LocationDFtoOutput,"LocationComparision",
                      ValueIndicatingChange,ValueIndicatingPctChange)

setColWidths(FinalReportWb,"Summary",cols = 1:100,widths = "auto",ignoreMergedCells = TRUE)
setColWidths(FinalReportWb,"LocationComparision",cols = 1:100,widths = "auto",ignoreMergedCells = TRUE)

outputFile = paste0(OutputDirectory,"/Final.xlsx")
saveWorkbook(FinalReportWb,file = outputFile,overwrite = TRUE)
