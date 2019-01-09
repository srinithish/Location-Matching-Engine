
#TODO : get Command line args from runApp and passthese to them and run this before shiny

if (!require("dplyr")){
  install.packages("dplyr")
  
}
require("dplyr")

# if (!require(lubridate)){
#   install.packages("lubridate",dependencies = TRUE)
#   require(lubridate)   
# }

if (!require("devtools")){
  install.packages("devtools")
}
require("devtools")

if (!require("ggplot2")){
  install.packages("ggplot2")
  
}
require("ggplot2")

if (!require("stringr")){
  install.packages("stringr")
  
}
require("stringr")


if(!require("readxl")){
  install.packages("readxl",dependencies = TRUE)
  
}
require("readxl")

if(!require("tidyr")){
  install.packages("tidyr",dependencies = TRUE)
  
}
require("tidyr")




if (!require("RCurl")){
  install.packages("RCurl", dependencies = TRUE)
  
}

require("RCurl")


if (!require("stringdist")){
  install.packages("stringdist", dependencies = TRUE)
  
}
require("stringdist")

if (!require("shiny")){
  install.packages("shiny", dependencies = TRUE)
  
}
require("shiny")

if (!require("openxlsx")){
  install.packages("openxlsx", dependencies = TRUE)
  
}
require("openxlsx")