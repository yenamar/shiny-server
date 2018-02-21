library(dplyr)
library(tidyr)
library(data.table)
    


function(input, output, session) {

source(paste0(getwd(),"/deep_learning.R"))
output$table_plot <- DT::renderDataTable(wb[,c(2,5,40,34,35,44)] %>%
                                           dplyr::mutate(Current_Value = round(Current_Value,2)),rownames=FALSE,options=list(pageLength=25))
  

session$onSessionEnded(function() {
  stopApp()
})

}


