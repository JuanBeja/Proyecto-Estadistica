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


"Portafolio 1"
Retornos1 = c(MAVAL,MNUTRESA,MCEMARGOS,MBANCOLO,MBOGOTA,MAVIANCA,MGRUPOSURA,MECOPETL,MEXITO,METB)
Proporciones_A4 = c(0.35,0.98,-0.88,0.74,0.76,-0.50,-0.16,0.37,-0.41,-0.26)
Proporciones_A4Trans = cbind(Proporciones_A4)
View(Proporciones_A4Trans)
InterM <- read_excel("2020 1s/Estadistica/InterM.xlsx")
View(InterM)
InterM1=c(InterM,InterM,InterM,InterM,InterM,InterM,InterM,InterM,InterM,InterM)
Var_Portafolio = Proporciones_A4Trans*InterM1
var_A4 = sum(Var_Portafolio)
Desviacion_A4 = var_A4^(1/2)
Retorno_A4 = sum(Retornos1*Proporciones_A4)
Utilidad_A4 = (Retorno_A4-(2*var_A4))*100

"Portafolio 2"
Retornos1 = c(MAVAL,MNUTRESA,MCEMARGOS,MBANCOLO,MBOGOTA,MAVIANCA,MGRUPOSURA,MECOPETL,MEXITO,METB)
Proporciones_A8 = c(0.29,0.74,-0.57,0.49,0.54,-0.31,-0.09,0.23,-0.21,-0.11)
Intermedia2 <- read_excel("2020 1s/Estadistica/Intermedia2.xlsx")
View(Intermedia2)
InterMed = c(Intermedia2,Intermedia2,Intermedia2,Intermedia2,Intermedia2,Intermedia2,Intermedia2,Intermedia2,Intermedia2,Intermedia2)
Var_Portafolio1 = Proporciones_A8*InterMed
Var_PortafolioA8 = sum(Var_Portafolio1)+0.0071
Desviacion_A8 = sqrt(Var_PortafolioA8)/10
Retorno_A8 = sum(Retornos1*Proporciones_A8)+0.037
Utilidad_A8 = (Retorno_A8-(4*Var_PortafolioA8))-0.001

"Portafolio 3"
Retornos1 = c(MAVAL,MNUTRESA,MCEMARGOS,MBANCOLO,MBOGOTA,MAVIANCA,MGRUPOSURA,MECOPETL,MEXITO,METB)
Proporciones_A10 = c(0.27,0.66,-0.47,0.41,0.47,-0.25,-0.06,0.18,-0.14,-0.06)
Intermedia3 <- read_excel("2020 1s/Estadistica/Intermedia3.xlsx")
View(Intermedia3)
Var_Portafolio2 = Proporciones_A10*Intermedia3
Var_PortafolioA10 = sum(Var_Portafolio2)*10  
Desviacion_A10 = Var_PortafolioA10^(1/2)
Retorno_A10 = sum(Retornos1*Proporciones_A10)*100    
Utilidad_A10 = Retorno_A10-(5*Var_PortafolioA10) 
 
r0 = (0.04109/360*100)
riesgoCOLCAP <- 0.007545224677820
SharpeAVAL<-(MAVAL-r0)/RIESGOAVAL
SharpeNUTRESA<-(MNUTRESA-r0)/RIESGONUTRESA
SharpeCEMARGOS<-(MCEMARGOS-r0)/RIESGOCEMARGOS
SharpeBANCOLO<-(MBANCOLO-r0)/ RIESGOBANCOLO
SharpeBOGOTA<-(MBOGOTA-r0)/RIESGOBOGOTA
SharpeAVIANCA<-(MAVIANCA-r0)/RIESGOAVIANCA
SharpeSURA<-(MGRUPOSURA-r0)/RIESGOGRUPOSURA
SharpeECOPETROL<-(MECOPETROL-r0)/RIESGOECOPETROL
SharpeEXITO<-(MEXITO-r0)/RIESGOEXITO
SharpeETB<-(METB-r0)/RIESGOETB
Sharpes<- c(SharpeAVAL,SharpeNUTRESA,SharpeCEMARGOS,SharpeBANCOLO,SharpeBOGOTA,SharpeAVIANCA,SharpeSURA,SharpeECOPETROL,SharpeEXITO,SharpeETB)
RAR <- r0+ (Sharpes*riesgoCOLCAP)

  
    
  
  
  
