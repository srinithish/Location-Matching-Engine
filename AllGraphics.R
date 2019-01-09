
theme_update(plot.title = element_text(hjust = 0.5))

##Bargraph for AIG Participation
## To do change the space between the columns 
ParticipationPre = PolicyLevelTermsCompDF$`AIG Participation %_Pre`
ParticipationPost = PolicyLevelTermsCompDF$`AIG Participation %_Post`

AIGParticipationPlotExp  =  ggplot() + 
                            geom_col(mapping = aes(x = factor(c("Pre","Post"),levels = c("Pre","Post")),
                                                   y = c(ParticipationPre,ParticipationPost),
                                                   fill = factor(c("Pre","Post"),levels = c("Pre","Post"))),
                                     width = 0.25,position = position_dodge(width = 2)
                                    )+
                            guides(fill = FALSE)+
                            geom_text(label = c(ParticipationPre,ParticipationPost) )+
                            labs( x = "",y = "Participation %" ,title = "AIG Participation")

AIGParticipationPlot_Out = renderPlot({AIGParticipationPlotExp})

#### Bar graph for Limits 
## to do display value at the Bar top
LimitPre = PolicyLevelTermsCompDF$Limit_Pre
LimitPost = PolicyLevelTermsCompDF$Limit_Post

PolicyLimitPlotExp =    ggplot() + 
                        geom_col(mapping = aes(x = factor(c("Pre","Post"),levels = c("Pre","Post")),
                                               y = c(LimitPre,LimitPost),
                                               fill = factor(c("Pre","Post"),levels = c("Pre","Post"))),
                                 width = 0.25,position = position_dodge(width = 0.01)
                                 )+
                        guides(fill = FALSE)+
                        geom_text(label = c(LimitPost,LimitPost))+
                        labs( x = "",y = paste0("Limit in ", AccountDetailsPreDF$X__1[12]) ,title = "Limit")

PolicyLimitPlot_Out = renderPlot(PolicyLimitPlotExp)


######Bar graph for TCP
# TCPCompDFForGraph = rbind(cbind(PrePost = "Pre",TCPPreDF),
#                           cbind(PrePost = "Post",TCPPostDF))
# 
# TCPCompDFForGraph = TCPCompDFForGraph %>% 
#                     mutate(`Layer Name` = as.factor(`Layer Name`))
#     
#     for(i in colnames(TCPCompDFForGraph)){
#             if(i %in% c("EQ TCP","FL TCP","WS TCP")){
#               TCPCompDFForGraph[,i] = as.numeric(TCPCompDFForGraph[,i])
#               tempGraph = ggplot()+
#               assign(paste0(i,"Graph"),)
#             }
#     }
# 
# TCPCompDFForGraph = gather(TCPCompDFForGraph,c(`EQ TCP`,`FL TCP`,`WS TCP`),
#                             key = "Peril",value = "TCP",factor_key = TRUE)
# 
# ggplot(data = TCPCompDFForGraph) + 
# 
# class(TCPCompDFForGraph$`EQ TCP`)




####### Bar Graph for Locations 

NoOfLocationsPlotExp = ggplot() +
                       geom_col(mapping = aes(x = factor(c("Pre","Post"),levels = c("Pre","Post")),
                                               y = c(nrow(LocationDetailsPreDF),nrow(LocationDetailsPostDF)),
                                               fill = factor(c("Pre","Post"),levels = c("Pre","Post"))),   
                                                width = 0.25,position = position_dodge(width = 0.01)
                                               )+
                       guides(fill = FALSE)+
                       geom_text(aes( x = factor(c("Pre","Post"),levels = c("Pre","Post")),
                                      y = c(nrow(LocationDetailsPreDF),nrow(LocationDetailsPostDF)),
                                      label = c(nrow(LocationDetailsPreDF),nrow(LocationDetailsPostDF))),
                                      nudge_y = 15)+
                       labs( x = "",y = "Location Count" ,title = "No of Locations")

NoOfLocationsPlot_Out = renderPlot(NoOfLocationsPlotExp)






