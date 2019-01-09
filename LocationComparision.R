################################functions definitions goes here###############################################



##############################################################################################################



LocIDDetails = colnames(FinalEnsembleLocs)

AddressDetailsCols = c("Location#_Pre","Location#_Post","Location Name_Pre","Location Name_Post","Address_Pre","Address_Post",
                       "City_Pre","City_Post","State_Pre","State_Post","Country_Pre","Country_Post")

AALCols = c("Ground Up AAL_Pre","Ground Up AAL_Post","Gross AAL_Pre","Gross AAL_Post")

primaryModifiersCols = c("Construction_Pre","Construction_Post","Construction Code_Pre","Construction Code_Post",
                         "Construction Scheme_Pre","Construction Scheme_Post","Occupancy Code_Pre","Occupancy Code_Post",
                         "Occupancy Scheme_Pre","Occupancy Scheme_Post","Occupancy_Pre","Occupancy_Post")

CoverageValuesCols = c("Building Value_Pre","Building Value_Post","Content Value_Pre","Content Value_Post",
                       "BI Value_Pre","BI Value_Post","TIV_Pre","TIV_Post" )



LocationDFtoOutput = LocComparisionDF %>% 
                      arrange(LocID_Pre,LocID_Post,MultipleWith) %>% 
                      select(LocIDDetails,AddressDetailsCols,primaryModifiersCols,CoverageValuesCols,AALCols) 

LocationDFtoOutput = LocationDFtoOutput %>% mutate(`Building Value_Pre` = gsub(",","",`Building Value_Pre`),
                                                  `Building Value_Pre` = as.numeric(`Building Value_Pre`),
                                                  `Building Value_Post` = gsub(",","",`Building Value_Post`),
                                                  `Building Value_Post` = as.numeric(`Building Value_Post`),
                                                  `Content Value_Pre` = gsub(",","",`Content Value_Pre`),
                                                  `Content Value_Pre` = as.numeric(`Content Value_Pre`),
                                                  `Content Value_Post` = gsub(",","",`Content Value_Post`),
                                                  `Content Value_Post` = as.numeric(`Content Value_Post`),
                                                  `BI Value_Pre` = gsub(",","",`BI Value_Pre`),
                                                  `BI Value_Pre` = as.numeric(`BI Value_Pre`),
                                                  `BI Value_Post` = gsub(",","",`BI Value_Post`),
                                                  `BI Value_Post` = as.numeric(`BI Value_Post`)
                                                  )

##CoverageCols cover to numeric




##TODO only  if the Loc ID post is present
doComparisionIf = with(LocationDFtoOutput,(!is.na(LocID_Post) & LocID_Post != "" & !is.null(LocID_Post)))
                       
###Ask post - pre or pre - post
LocationDFtoOutput =  LocationDFtoOutput %>% 
                      mutate("GroundUpAAL_Change" = ifelse(doComparisionIf,`Ground Up AAL_Post` - `Ground Up AAL_Pre`,NA),
                             "GroundUpAAL%Change" = ifelse(doComparisionIf,100*(`Ground Up AAL_Post` - `Ground Up AAL_Pre`)/`Ground Up AAL_Pre`,NA),
                             "GrossAAL_Change" = ifelse(doComparisionIf,`Gross AAL_Post` - `Gross AAL_Pre`,NA),
                             "GrossAAL%Change" = ifelse(doComparisionIf,(`Gross AAL_Post` - `Gross AAL_Pre`)/`Gross AAL_Pre`,NA),
                             
                             "BuildingValue_Change" = ifelse(doComparisionIf,`Building Value_Pre` - `Building Value_Post`,NA),
                             "Building%Change" = ifelse(doComparisionIf,100*(`Building Value_Pre` - `Building Value_Post`)/`Building Value_Pre`,NA),
                             "ContentValue_Change" = ifelse(doComparisionIf,`Content Value_Pre` - `Content Value_Post`,NA),
                             "Content%Change" = ifelse(doComparisionIf,100*(`Content Value_Pre` - `Content Value_Post`)/`Content Value_Pre`,NA),
                             "BI_Change" = ifelse(doComparisionIf,`BI Value_Pre` - `BI Value_Post`,NA),
                             "BI%Change" = ifelse(doComparisionIf,100*(`BI Value_Pre` - `BI Value_Post`)/`BI Value_Pre`,NA),
                             "TIV_Change" = ifelse(doComparisionIf,`TIV_Pre` - `TIV_Post`,NA),
                             "TIV%Change" = ifelse(doComparisionIf,100*(`TIV_Pre` - `TIV_Post`)/`TIV_Pre`,NA)
                             )


LocationComparisionPlot_Out = renderLeaflet({
  leaflet() %>% addTiles() %>% addMarkers(lng = 77.729186,lat = 12.990320 ,popup = "Coming Soon")
})

# write.csv(LocationDFtoOutput,paste0(OutputDirectory,"/FinalOutputTrails_NoChange.csv"),na = "",row.names = FALSE)
##TODO : 
     



