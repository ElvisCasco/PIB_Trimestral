server <- function(input, output) {
  ##### Tabla #####
  output$table <- DT::renderDataTable(DT::datatable({
    data <- PIB_Trimestral
    if (input$archivo != "All") {
      data <- PIB_Trimestral[PIB_Trimestral$Enfoque_PIB == input$archivo,]
    }
    data
  }))
  ## Bajar datos en CSV
  thedata <- reactive(PIB_Trimestral)
  output$download <- downloadHandler(
    filename = function(){"Datos.csv"}, 
    content = function(fname){
      write.csv(thedata(), fname)
    }
  )
  PIB_Trimestral <- readRDS("RData/PIB_Trimestral.rds")
  ##### Grafico #####
}