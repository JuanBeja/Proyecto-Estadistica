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

Retornos1 = c(METB,MAVAL,MBANCOLO,MBOGOTA,MAVIANCA,MGRUPOSURA,MECOPETL,MEXITO,MCEMARGOS,MNUTRESA)
Proporciones_A4 = c(0.35,0.98,-0.88,0.74,0.76,-0.50,-0.16,0.37,-0.41,-0.26)
Proporciones_A4Trans = cbind(Proporciones_A4)
View(Proporciones_A4Trans)
InterM <- read_excel("2020 1s/Estadistica/InterM.xlsx")
View(InterM)
InterM1=c(InterM,InterM,InterM,InterM,InterM,InterM,InterM,InterM,InterM,InterM)
Var_Portafolio = Proporciones_A4Trans*InterM1
var_A4 = sum(Var_Portafolio)
Desviacion_A4 = var_A4^(1/2)
Retorno_A4 = Retornos1*Proporciones_A4
View(Retorno_A4)



  
    
  
  
  
