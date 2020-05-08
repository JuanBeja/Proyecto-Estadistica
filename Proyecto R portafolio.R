library(readxl)
datos_bloomberg <- read_excel("C:/Users/juanp/Downloads/datos bloomberg.xlsx")
View(datos_bloomberg)
Retornos <- log(( datos_bloomberg[2:1396, ])/datos_bloomberg [1:1395, ])
View(Retornos)
Retornos <- log10 ((datos_bloomberg [2:1396, ])/(datos_bloomberg [1:1395, ]))
View(Retornos)
METB <- mean (Retornos$ETB)
MAVAL <- mean (Retornos$AVAL)
MBANCOLO <- mean (Retornos$BANCOLO)
MBOGOTA <- mean (Retornos$BOGOTA)
MAVIANCA <- mean (Retornos$AVIANCA)
MGRUPOSURA <- mean (Retornos$GRUPOSURA)
MECOPETL <- mean (Retornos$ECOPETL)
MEXITO <- mean (Retornos$EXITO)
MCEMARGOS <- mean (Retornos$CEMARGOS)
MNUTRESA <- mean (Retornos$NUTRESA)
RIESGOETB <- sd (Retornos$ETB)
RIESGOAVIANCA <- sd (Retornos$AVIANCA)
RIESGOAVAL <- sd (Retornos$AVAL)
RIESGONUTRESA <- sd (Retornos$NUTRESA)
RIESGOCEMARGOS <- sd (Retornos$CEMARGOS)
RIESGOBANCOLO <- sd (Retornos$BANCOLO)
RIESGOBOGOTA <- sd (Retornos$BOGOTA)
RIESGOSURA <- sd (Retornos$GRUPOSURA)
RIESGOECOPETL <- sd (Retornos$ECOPETL)
RIESGOEXITO <- sd (Retornos$EXITO)
MATRIZCOV <- cov (Retornos)
View(MATRIZCOV)



  
    
  
  
  
