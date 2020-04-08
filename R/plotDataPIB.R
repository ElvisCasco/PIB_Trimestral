plotDataPIB <- function(Nombre_Archivo_Origen,
                        Nombre_Enfoque_PIB,
                        Nombre_Tipo_Serie,
                        Nombre_Tipo_Valor) {
  plotDin <- PIB_Anual %>%
    dplyr::filter(Archivo_Origen == Nombre_Archivo_Origen,
                  Enfoque_PIB == Nombre_Enfoque_PIB,
                  Tipo_Valor == Nombre_Tipo_Valor) %>%
    plotly::plot_ly(x = ~Fecha,
                    y = ~Valor,
                    type = 'scatter',
                    mode = 'lines',
                    color = ~Nombre_Serie) %>%
    layout(legend = list(x = 0.01, y = 0.99, bgcolor = "transparent"),
           xaxis = list(title = ""),
           yaxis = list(title = "Millones de Lempiras"),
           title = paste0("PIB Enfoque del ",
                          Nombre_Enfoque_PIB,
                          ", Serie ",Nombre_Tipo_Serie,
                          ", Valores ",Nombre_Tipo_Valor))
  plotDin
}