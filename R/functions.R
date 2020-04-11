##### Funciones personalizadas, curso Forecasting #####
ts_fred <- function(..., class = "data.frame") {
  symb <- c(...)
  dta.env <- new.env()
  suppressMessages(getSymbols(symb, env = dta.env, src = "FRED"))
  z <- data.table::rbindlist(lapply(as.list(dta.env), ts_dt), idcol = "id")
  tsbox:::as_class(class)(z)
}

nameMin <- function(Matx){
  # This is a useful function that returns the column and row names of a matrix
  # for the minimum value of the matrix
  ind <- which(Matx == min(Matx), arr.ind = TRUE)
  cname <- colnames(Matx)[[ind[2]]]
  rname <- rownames(Matx)[[ind[1]]]
  return(c(rname, cname))
}

ts_cumsum <- function(x){
  # Function to calculate cumulative sums of a time series
  return(ts_(cumsum)(x))
}

ts_rowMeans <- function(x){
  # Function to calculate means by row
  return(ts_(rowMeans)(x))
}

getForecastVariance <- function(fcst){
  # Function to extract forecast error variance from a forecast object
  # CI lower = y(t+h|t)-1.96*sig(h)
  # Therefore sig(h)^2 = [CI lower - y(t+h|t))/(-1.96)]^2
  # Get exact percentile (1.96 yield basically the same)
  z957 = qnorm(0.975, 0, 1)
  sigh2 = ((fcst$lower[,"95%"]-fcst$mean)/(-z957))^2
  return(sigh2)
}

##### Otras funciones personalizadas #####
## Funcion para crear valores CADF a partir de una lista
tabla_adf = function(DF, maxLags, lagSelection, ...){
  # Guarda ... argumentos como lista
  var_list = as.list(unlist(list(...)))
  # Subset DF basado en var_list
  if(length(var_list) == 0){
    subsetdf = DF
    var_list = as.list(names(DF))
  }else{
    subsetdf = select_(DF, .dots = var_list)
  }
  # Modelo para cada variable, guardando resultados en listas 
  tests = lapply(subsetdf, function(x, y){
    out1 = CADFtest(x, max.lag.y = maxLags, type = "drift", criterion = lagSelection)
    out2 = CADFtest(x, max.lag.y = maxLags, type = "trend", criterion = lagSelection)
    out3 = CADFtest(x, max.lag.y = maxLags, type =  "none", criterion = lagSelection)
    return(list(out1, out2, out3))
  })
  # Guardar cada rezago en un data.frame
  lag_df = lapply(tests, function(x){
    data.frame(
      drift = x[[1]]$max.lag.y[[1]],
      trend = x[[2]]$max.lag.y[[1]],
      none  = x[[3]]$max.lag.y[[1]]
    )
  })
  # Guardar cada t-estadistico en un data.frame
  est_df = lapply(tests, function(x){
    data.frame(
      drift = x[[1]]$statistic[[1]],
      trend = x[[2]]$statistic[[1]],
      none  = x[[3]]$statistic[[1]]
    )
  })
  # Guardar el p-value en un data.frame
  pvalue_df = lapply(tests, function(x){
    data.frame(
      drift = x[[1]]$p.value[[1]],
      trend = x[[2]]$p.value[[1]],
      none  = x[[3]]$p.value[[1]]
    )
  })  
  # Combinar en un solo dataframe
  lag_table = do.call(rbind, lag_df)
  est_table = do.call(rbind, est_df)
  pvalue_table = do.call(rbind, pvalue_df)
  tableADF = cbind(lag_table, est_table, pvalue_table)
  tableADF = round(tableADF,4)
  names(tableADF) <- c("sel.lag-Drift", "sel.lag-Trend", "sel.lag-None", 
                       "est.t-Drift", "est.t-Trend", "est.t-None", 
                       "p.value-Drift", "p.value-Trend", "p.value-None")
  # Tabla con formato  
  tableADF <- tableADF %>%
    rownames_to_column(var = "variables") %>%
    gather("Stats", "Valor", 2:10) %>%
    separate(Stats, c('Stat', 'Tipo'), sep="-") %>%
    separate(variables, c('transf', 'Nombre'), sep="_") %>%
    spread(Tipo, Valor) %>%
    arrange(desc(transf))
  
  tableADF <<- tableADF
  
  kable(tableADF, caption = "Resultados de Prueba CADF") %>%
    kable_styling(c("striped", "bordered")) %>%
    # kable_styling(c("bordered")) %>%
    add_header_above(c(" " = 1," " = 1," " = 1,"Tipo" = 3)) %>%
    collapse_rows(columns = 1:2, valign = "top") %>%
    footnote(general = c("est.t = t-test statistic",
                         "sel.lag = Max lag of the diff. dependent variable",
                         "p.value = p-value"))#, notation = ""))
}

## Funcion para estadisticos de coeficientes, modelos ARIMA 
table_coef = function(model, caption, ...){
  Betas = model$coef
  Error_Estandar = sqrt(diag(model$var.coef))
  t_Estadistic = model$coef/sqrt(diag(model$var.coef))
  p_value = 2*pt(-abs(t_Estadistic), df = model$nobs - 1)
  coefs <- as.data.frame(cbind(Betas,Error_Estandar,t_Estadistic,p_value))
  
  coefs <<- coefs
  
  kable(coefs,
        caption = caption) %>%
    kable_styling(c("striped", "bordered"))
}

## Funcion para estadisticos de modelos ARIMA 
table_stats = function(model, caption, ...){
  sigma_2 = model$sigma2
  logLik = model$loglik
  AIC = model$aic
  AICc = model$aicc
  BIC = model$bic
  stats <- as.data.frame(cbind(sigma_2, logLik, AIC, AICc, BIC))
  
  stats <<- stats
  
  kable(stats,
        caption = caption) %>%
    kable_styling(c("striped", "bordered"))
}

## Funcion para estadisticos de residuales modelo ARIMA 
table_resids = function(model, caption, ...){
  residuals = model$residuals
  original = model$x
  fitted = model$fitted
  PE = na.remove((original - fitted)/original)
  ME = mean(residuals, na.rm = T)
  RMSE = sqrt(mean(residuals^2, na.rm = T))
  MAE = mean(abs(residuals), na.rm = T)
  MPE = (100/length(PE))*sum(PE)
  MAPE = (100/length(PE))*sum(abs(PE))
  MASE = Metrics::mase(na.remove(original),na.remove(fitted),12)
  ACF1 = Arima(residuals, order = c(1, 0, 0), include.constant= TRUE)$coef[[1]]
  eval_resids <- as.data.frame(cbind(ME, RMSE, MAE, MPE, MAPE, MASE, ACF1))
  
  eval_resids <<- eval_resids
  
  kable(eval_resids,
        caption = caption) %>%
    kable_styling(c("striped", "bordered"))
}

## Funcion para reconocer formato: pdf o HTML
getOutputFormat <- function() {
  output <- rmarkdown:::parse_yaml_front_matter(
    readLines(knitr::current_input())
  )$output
  if (is.list(output)){
    return(names(output)[1])
  } else {
    return(output[1])
  }
}
