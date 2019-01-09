
###AppControlTab

ControlTab =  tabPanel(
                      
                      "Control",
                      
                      br(),
                      
                      sliderInput("MatchPctThreshhold_Inp", "Location match % threshold",
                                  min = 0, max = 100,
                                  value = 85),
                      
                      tags$hr(),
                      
                      ### match intensity
                      
                      
                      fluidRow(
                        ###Match intensity
                        column(3,
                               radioButtons("MatchIntensity_Inp", "Match Intensity",
                                            choices = c("Superficial" = 1,
                                                        "Deep dive" = 2),
                                            selected  = 2)
                               
                               
                        ),
                        
                        ###Comparision Type
                        column(3,
                               radioButtons("ComparisionType_Inp", "Comparision type",
                                            choices = c("Pre-Post" = "PrePost",
                                                        "Year-on-Year" = "YoY"))
                               
                               
                        )
                        
                        
                        
                        
                      ),
                      
                      ##folder path to save the report at
                      textInput("FolderPath_Inp", label = h4("Save report at"), value = "C://Temp"),
                      
                      ###Download Button
                      
                      
                     
                      ##run button
                      actionButton("Run_Inp",h4("Run comparision"),width = '100%'),
                      
                      br(),
                      br(),
                      
                      downloadButton("finalReportDownload.xlsx", "Download Final Report"),
                      
                      
                      tags$hr(),
                      
                      h4(textOutput("ComparisionCompleted_Output")),
                      
                      h4(textOutput("message"))
                      )
