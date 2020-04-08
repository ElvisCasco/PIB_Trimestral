library(DT)
library(shiny)
library(tidyverse)

# Archivos disponibles en <https://github.com/ElvisCasco/PIB_Anual>

source("R/getDataPIB.R")
# source("R/plotDataPIB.R")
getDataPIB()
PIB_Trimestral <- readRDS("RData/PIB_Trimestral.rds")
#plotDataPIB()
# Nombre_Archivo_Origen <- unique(PIB_Trimestral$Archivo_Origen)
Nombre_Enfoque_PIB <- unique(PIB_Trimestral$Enfoque_PIB)
Nombre_Tipo_Serie <- unique(PIB_Trimestral$Tipo_Serie)
Nombre_Tipo_Valor <- unique(PIB_Trimestral$Tipo_Valor)

ui <- fluidPage(
  ##### Tabla #####
  titlePanel("Tabla de Datos"),
  # Create a new Row in the UI for selectInputs
  fluidRow(
    column(4,
           selectInput("archivo",
                       "Enfoque del PIB:",
                       c("All",
                         as.character(Nombre_Enfoque_PIB)))
    )
  ),
  
  # Create a new row for the table.
  DT::dataTableOutput("table"),
  downloadButton("download", "Descargar"),
  fluidRow(column(7,dataTableOutput('datos')))
  ##### Grafico #####
)