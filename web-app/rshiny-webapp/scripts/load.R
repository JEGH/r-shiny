
tryCatch({
  library(shiny)
}, error = function(e) {
  install.packages("shiny")
  library(shiny)
  
}
)

tryCatch({
  library(shinyTime)
}, error = function(e) {
  install.packages("shinyTime")
  library(shinyTime)
  
}
)


tryCatch({
  library(shinythemes)
}, error = function(e) {
  install.packages("shinythemes")
  library(shinythemes)
}
)



tryCatch({
  library(shinydashboard)
}, error = function(e) {
  install.packages("shinydashboard")
  library(shinydashboard)
  
}
)


tryCatch({
  
  library(plotly)
  
}, error = function(e) {
  install.packages("plotly")
  library(plotly)
}
)

tryCatch({
  
  library(markdown)
  
}, error = function(e) {
  install.packages("markdown")
  library(markdown)
}
)



tryCatch({
  library(ggthemes)
}, error = function(e) {
  install.packages("ggthemes")
  library(ggthemes)
}
)



tryCatch({
  library(flexdashboard)
}, error = function(e) {
  install.packages("flexdashboard")
  library(flexdashboard)
}
)


tryCatch({
  library(mongolite)
}, error = function(e) {
  install.packages("mongolite")
  library(mongolite)
}
)



tryCatch({
  library(shinyjs)
}, error = function(e) {
  install.packages("shinyjs")
  library(shinyjs)
}
)


tryCatch({
  library(RColorBrewer)
}, error = function(e) {
  install.packages("RColorBrewer")
  library(RColorBrewer)
}
)

tryCatch({
  library(reshape2)
}, error = function(e) {
  install.packages("reshape2")
  library(reshape2)
}
)

tryCatch({
  library(knitr)
}, error = function(e) {
  install.packages("knitr")
  library(knitr)
}

)

tryCatch({
  library(dplyr)
}, error = function(e) {
  install.packages("dplyr")
  library(dplyr)

}
)


tryCatch({
  library(RPostgreSQL)
}, error = function(e) {
  install.packages("RPostgreSQL")
  library(RPostgreSQL)
}
)


tryCatch({
  library(readxl)
}, error = function(e) {
  install.packages("readxl")
  library(readxl)
}
)

tryCatch({
  library(dplyr)
}, error = function(e) {
  install.packages("dplyr")
  library(dplyr)
}
)

tryCatch({
  library(sparklyr)
}, error = function(e) {
  install.packages("sparklyr")
}
)


tryCatch({
  library(compare)
}, error = function(e) {
  install.packages("compare")
  library(compare)
}
)

tryCatch({
  library(digest)
}, error = function(e) {
  install.packages("digest")
  library(digest)
}
)

tryCatch({
  library(Rcpp)
}, error = function(e) {
  install.packages("Rcpp")
  library(Rcpp)
}
)


tryCatch({
  library(DT)
}, error = function(e) {
  install.packages("DT")
  library(DT)
  
}
)






