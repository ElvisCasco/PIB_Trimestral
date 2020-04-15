# PIB Trimestral de Honduras, Webpage BCH

En este sitio se presenta una forma de manipular los datos correspondientes a la p&#225;gina web del Banco Central de Honduras, creando un archivo unificado de los archivos descargados del [Portal Cuentas Nacionales Trimestrales de Honduras](https://see.bch.hn/portalPIBT/Login.aspx?ReturnUrl=%2fportalPIBT%2f).

El archivo `PIB_Trimestral.Rmd` contiene los c&#243;digos en RStudio para obtener los resultados a nivel de tabla y gr&#225;fico din&#225;mico de los datos del portal mencionado (exceptuando variaciones porcentuales). Para ejecutar c&#243;digos desde su computadora, puede descargar el contenido de este sitio con el bot&#243;n verde `Clone or download` ubicado arriba y a la derecha de este mensaje. Puede ver los resultados en html [aqu&#237;](https://rpubs.com/ElvisCasco/PIB_Trimestral).

Los archivos `ui.R` y `server.R` ejecutan un proceso que permite visualizar los datos en una tabla y descargar los datos de origen para dicha tabla en su computadora, en un archivo `.csv`. Los resultados din&#225;micos usando Shiny se encuentran en [este v&#237;nculo](https://elviscasco.shinyapps.io/PIB_Trimestral/).

Se incluye adicionalmente [un ejercicio](https://rpubs.com/ElvisCasco/Forecast_PIB_Trimestral_Honduras) utilizando el PIB Trimestral como referencia para evaluar modelos ARIMA y los resultados de pron&#243;stico dentro y fuera de muestra que se obtienen, además de todas las pruebas estad&#237;sticas pertinentes (archivo `Forecasting_PIB_Trimestral.Rmd` con los c&#243;digos); **Los resultados no representan ninguna postura institucional, solamente se publican con fines de mostrar una aplicaci&#243;n de pron&#243;stico con modelos ARIMA.**
