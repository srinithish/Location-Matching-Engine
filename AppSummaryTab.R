SummaryTab = tabPanel(
  
                      "Summary",
                      fluidRow(
                        column(6,plotOutput("AIGParticipationPlot")),
                        
                        column(6,plotOutput("PolicyLimitPlot"))
                        
                      ),
                      
                      fluidRow(
                        column(6,plotOutput("NoOfLocationsPlot"))
                        
                        
                        
                      )

                    )
