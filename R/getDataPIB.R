getDataPIB <- function(nombre_datos = "PIB_Trimestral", ...) {
  
  #rm(list = ls())
  nombre_datos <- "PIB_Trimestral"

  # Estad&#237;stica Econ&#243;mica
  
  ## Sector Real
  
  ### PIB Trimestral
  
  #### Portal Cuentas Nacionales Trimestrales de Honduras

  ##### Enfoque PIB: Producci&#243;n #####
  
  ## Parametros de fecha
  fecha_fin <- "2019/12/2" # Debe cambiarse la fecha de acuerdo a datos disponibles
  
  ###### Valores Corrientes
  
  library(plotly)
  library(readxl)
  library(tidyverse)
  
  ## Tidy Data
  PIBProduccion_Corrientes <- readxl::read_excel("XLSData/ReportesDinamicosPIBProduccion.xls",
                                                 sheet = "VALORES CORRIENTES",
                                                 skip = 10) %>%
    stats::na.omit() %>%
    dplyr::mutate(N = 1:34,
                  Tipo_Serie = case_when(N %in% 1:17 ~ "Original",
                                         N %in% 18:34 ~ "Desestacionalizada"),
                  Enfoque_PIB = "Producci\u00F3n")  %>%
    dplyr::rename(Nombre_Serie = 1) %>%
    dplyr::select(N,Nombre_Serie,Tipo_Serie,Enfoque_PIB,everything()) %>%
    dplyr::select(-starts_with("..."))
  quarters <- c("N","Nombre_Serie","Tipo_Serie","Enfoque_PIB",
                as.character(as.POSIXct(seq(as.Date("2000/3/2"),
                                            as.Date(fecha_fin),
                                            by = "quarter"))))
  names(PIBProduccion_Corrientes) <- quarters
  PIBProduccion_Corrientes <- PIBProduccion_Corrientes %>%
    tidyr::gather("Fecha","Valor",5:ncol(.)) %>%
    dplyr::mutate(Valor = as.double(Valor),
                  Fecha = as.Date(Fecha),
                  Tipo_Valor = "Corrientes") %>%
    dplyr::select(N,Enfoque_PIB,Tipo_Serie,Tipo_Valor,Nombre_Serie,Fecha,Valor)
  
  ###### Valores Constantes
  ## Tidy Data
  PIBProduccion_Constantes <- readxl::read_excel("XLSData/ReportesDinamicosPIBProduccion.xls",
                                                 sheet = "VALORES CONSTANTES",
                                                 skip = 10) %>%
    stats::na.omit() %>%
    # filter(CONCEPTO != "CONCEPTO") %>%
    dplyr::mutate(N = 1:34,
                  Tipo_Serie = case_when(N %in% 1:17 ~ "Original",
                                         N %in% 18:34 ~ "Desestacionalizada"),
                  Enfoque_PIB = "Producci\u00F3n")  %>%
    dplyr::rename(Nombre_Serie = 1) %>%
    dplyr::select(N,Nombre_Serie,Tipo_Serie,Enfoque_PIB,everything()) %>%
    dplyr::select(-starts_with("..."))
  quarters <- c("N","Nombre_Serie","Tipo_Serie","Enfoque_PIB",
                as.character(as.POSIXct(seq(as.Date("2000/3/2"),
                                            as.Date(fecha_fin),
                                            by = "quarter"))))
  names(PIBProduccion_Constantes) <- quarters
  PIBProduccion_Constantes <- PIBProduccion_Constantes %>%
    tidyr::gather("Fecha","Valor",5:ncol(.)) %>%
    dplyr::mutate(Valor = as.double(Valor),
                  Fecha = as.Date(Fecha),
                  Tipo_Valor = "Constantes") %>%
    dplyr::select(N,Enfoque_PIB,Tipo_Serie,Tipo_Valor,Nombre_Serie,Fecha,Valor)
  
  ###### Serie Tendencia-Ciclo
  ## Tidy Data
  PIBProduccion_Tendencia_Ciclo <- readxl::read_excel("XLSData/ReportesDinamicosPIBProduccion.xls",
                                                      sheet = "SERIE TENDENCIA CICLO",
                                                      skip = 8) %>%
    stats::na.omit() %>%
    dplyr::mutate(N = 1:nrow(.),
                  Tipo_Serie = "Tendencia_Ciclo",
                  Enfoque_PIB = "Producci\u00F3n",
                  Nombre_Serie = "Tendencia_Ciclo",
                  Tipo_Valor = "Tendencia_Ciclo",
                  Fecha = seq(as.Date("2000/3/2"),
                              as.Date(fecha_fin),
                              by = "quarter"))  %>%
    dplyr::rename(Valor = `Serie Tendencia Ciclo`) %>%
    dplyr::select(N,Nombre_Serie,Tipo_Serie,Enfoque_PIB,everything()) %>%
    dplyr::select(-c(5,6))
  PIBProduccion_Tendencia_Ciclo <- PIBProduccion_Tendencia_Ciclo %>%
    dplyr::select(N,Enfoque_PIB,Tipo_Serie,Tipo_Valor,Nombre_Serie,Fecha,Valor)
  
  ##### Enfoque PIB: Gasto #####
  
  ###### Valores Corrientes
  ## Tidy Data
  PIBGasto_Corrientes <- readxl::read_excel("XLSData/ReportesDinamicosPIBGasto.xls",
                                            sheet = "VALORES CORRIENTES",
                                            skip = 10) %>%
    stats::na.omit() %>%
    # filter(CONCEPTO != "CONCEPTO") %>%
    dplyr::mutate(N = 1:14,
                  Tipo_Serie = case_when(N %in% 1:7 ~ "Original",
                                         N %in% 8:14 ~ "Desestacionalizada"),
                  Enfoque_PIB = "Gasto")  %>%
    dplyr::rename(Nombre_Serie = 1) %>%
    dplyr::select(N,Nombre_Serie,Tipo_Serie,Enfoque_PIB,everything()) %>%
    dplyr::select(-starts_with("..."))
  quarters <- c("N","Nombre_Serie","Tipo_Serie","Enfoque_PIB",
                as.character(as.POSIXct(seq(as.Date("2000/3/2"),
                                            as.Date(fecha_fin),
                                            by = "quarter"))))
  names(PIBGasto_Corrientes) <- quarters
  PIBGasto_Corrientes <- PIBGasto_Corrientes %>%
    tidyr::gather("Fecha","Valor",5:ncol(.)) %>%
    dplyr::mutate(Valor = as.double(Valor),
                  Fecha = as.Date(Fecha),
                  Tipo_Valor = "Corrientes") %>%
    dplyr::select(N,Enfoque_PIB,Tipo_Serie,Tipo_Valor,Nombre_Serie,Fecha,Valor)
  
  ###### Valores Constantes
  ## Tidy Data
  PIBGasto_Constantes <- readxl::read_excel("XLSData/ReportesDinamicosPIBGasto.xls",
                                            sheet = "VALORES CONSTANTES",
                                            skip = 10) %>%
    stats::na.omit() %>%
    dplyr::mutate(N = 1:14,
                  Tipo_Serie = case_when(N %in% 1:7 ~ "Original",
                                         N %in% 8:14 ~ "Desestacionalizada"),
                  Enfoque_PIB = "Gasto")  %>%
    dplyr::rename(Nombre_Serie = 1) %>%
    dplyr::select(N,Nombre_Serie,Tipo_Serie,Enfoque_PIB,everything()) %>%
    dplyr::select(-starts_with("..."))
  quarters <- c("N","Nombre_Serie","Tipo_Serie","Enfoque_PIB",
                as.character(as.POSIXct(seq(as.Date("2000/3/2"),
                                            as.Date(fecha_fin),
                                            by = "quarter"))))
  names(PIBGasto_Constantes) <- quarters
  PIBGasto_Constantes <- PIBGasto_Constantes %>%
    tidyr::gather("Fecha","Valor",5:ncol(.)) %>%
    dplyr::mutate(Valor = as.double(Valor),
                  Fecha = as.Date(Fecha),
                  Tipo_Valor = "Constantes") %>%
    dplyr::select(N,Enfoque_PIB,Tipo_Serie,Tipo_Valor,Nombre_Serie,Fecha,Valor)
  
  ###### Serie Tendencia-Ciclo
  ## Tidy Data
  PIBGasto_Tendencia_Ciclo <- readxl::read_excel("XLSData/ReportesDinamicosPIBGasto.xls",
                                                 sheet = "SERIE TENDENCIA CICLO",
                                                 skip = 8) %>%
    stats::na.omit() %>%
    # filter(CONCEPTO != "CONCEPTO") %>%
    dplyr::mutate(N = 1:nrow(.),
                  Tipo_Serie = "Tendencia_Ciclo",
                  Enfoque_PIB = "Gasto",
                  Nombre_Serie = "Tendencia_Ciclo",
                  Tipo_Valor = "Tendencia_Ciclo",
                  Fecha = seq(as.Date("2000/3/2"),
                              as.Date(fecha_fin),
                              by = "quarter"))  %>%
    dplyr::rename(Valor = `Serie Tendencia Ciclo`) %>%
    dplyr::select(N,Nombre_Serie,Tipo_Serie,Enfoque_PIB,everything()) %>%
    dplyr::select(-c(5,6))
  PIBGasto_Tendencia_Ciclo <- PIBGasto_Tendencia_Ciclo %>%
    dplyr::select(N,Enfoque_PIB,Tipo_Serie,Tipo_Valor,Nombre_Serie,Fecha,Valor)
  
  
  #### Unir Informes del Portal ####
  PIB_Trimestral <- rbind(PIBGasto_Constantes,PIBGasto_Corrientes,PIBGasto_Tendencia_Ciclo,
                          PIBProduccion_Constantes,PIBProduccion_Corrientes,PIBProduccion_Tendencia_Ciclo)
  rm(list=setdiff(ls(), "PIB_Trimestral"))
  saveRDS(PIB_Trimestral, file = "RData/PIB_Trimestral.rds")

}
