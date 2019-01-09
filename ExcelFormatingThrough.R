# Excel handling to
library(RDCOMClient)
library(RDCOM)
ExcelHandle = COMCreate("Excel.Application")
ExcelHandle[["Workbooks"]]$Open("//pngscitrix01/sk/Desktop/Automation/In Process/R Projects/ConnectR.xlsm")
ExcelHandle[["Visible"]] = TRUE
ExcelHandle$Activate()
mySheet = ExcelHandle[["Workbooks"]]$Item(1)$Worksheets("Sheet2")
print(mySheet[["name"]])
?get

for(i in list(folder[["Items"]])){
  
  print(i$item(i)[["Class"]])
}

dir.create("//pngscitrix01//sk//Desktop//Automation//In Process//R Projects//Testing2")

