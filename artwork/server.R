library(dplyr)
library(tidyr)
library(data.table)
    

  
function(input, output, session) {

  observeEvent(input$buttonRunAnalysis, {
    showNotification("Updating the predictions can take some time, depending on the size of the data...", type="message")
    
    source(paste0(getwd(),"/deep_learning.R"))
    
    
    output$table_plot <- DT::renderDataTable(wb[,c(2,5,40,34,35,44)] %>%
                                               dplyr::mutate(Current_Value = round(Current_Value,2)),rownames=FALSE,options=list(pageLength=25))
    
  })
  
  

session$onSessionEnded(function() {
  stopApp()
})

}


