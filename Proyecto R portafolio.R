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
Var_Portafolio = Proporciones_A4Trans*InterM
var_A4 = sum(Var_Portafolio)
Desviacion_A4 = var_A4^(1/2)
Retorno_A4 = sum(Retornos1*Proporciones_A4)
Utilidad_A4 = (Retorno_A4-(2*var_A4))*100

"Portafolio 2"
Retornos1 = c(MAVAL,MNUTRESA,MCEMARGOS,MBANCOLO,MBOGOTA,MAVIANCA,MGRUPOSURA,MECOPETL,MEXITO,METB)
Proporciones_A8 = c(0.29,0.74,-0.57,0.49,0.54,-0.31,-0.09,0.23,-0.21,-0.11)
Intermedia2 <- read_excel("2020 1s/Estadistica/Intermedia2.xlsx")
View(Intermedia2)
Var_Portafolio1 = Proporciones_A8*InterMedia2
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

#PORTAFOLIO MINIMA VARIANZA
Retornos1 = c(MAVAL,MNUTRESA,MCEMARGOS,MBANCOLO,MBOGOTA,MAVIANCA,MGRUPOSURA,MECOPETL,MEXITO,METB)
Proporciones_MV = c(0.2504,0.6203,-0.3720,0.3140,0.4051,-0.1996,-0.0668,0.1504,-0.0951,-0.0069)
IntermediaMV <- read_excel("2020 1s/Estadistica/IntermediaMV.xlsx")
View(IntermediaMV)
Var_PortafolioMV = sum(Proporciones_MV*IntermediaMV)
Desv_PortafolioMinV = Var_PortafolioMV^(1/2)
Retorno_MinV = sum(Retornos1*Proporciones_MV)

#Portafolio Ingenuo
Retornos1 = c(MAVAL,MNUTRESA,MCEMARGOS,MBANCOLO,MBOGOTA,MAVIANCA,MGRUPOSURA,MECOPETL,MEXITO,METB)
PIngen <- c(10/100,10/100,10/100,10/100,10/100,10/100,10/100,10/100,10/100,10/100)
Intermedia4 <- read_excel("2020 1s/Estadistica/Intermedia4.xlsx")
View(Intermedia4)
Var_PortafolioING = sum(PIngen*Intermedia4)
Desv_PortafolioING = sqrt(Var_PortafolioING)
Retorno_ING = sum(Retornos1*PIngen)

#fRONTERA EFICIENTE
DESVIACIONES = c(0.009,0.012,0.018,0.032,0.064,0.09,0.012)
RETORNOSFE = c(0.000398537,0.000597194,0.000980104,0.001854322,0.003834443,0.005439652,0.00729074)

 
r0 = (0.04109/360*100)
RetornoCOLCAP <- 0,0000934271003953
riesgoCOLCAP <- 0,008347988
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

Riesgo Diversificable
VarAVAL<- RIESGOAVAL^2
VarNUTRESA<-RIESGONUTRESA^2
VarCEMARGOS<-RIESGOCEMARGOS^2
VarBANCOLO<-RIESGOBANCOLO^2
VarBOGOTA<-RIESGOBOGOTA^2
VarAVIANCA<-RIESGOAVIANCA^2
VarGRUPOSURA<-RIESGOGRUPOSURA^2
VarECOPETROL<-RIESGOECOPETROL^2
VarEXITO<-RIESGOEXITO^2
VarETB<-RIESGOETB^2

Varianzas<-c(VarAVAL,VarNUTRESA,VarCEMARGOS,VarBANCOLO,VarBOGOTA,VarAVIANCA,VarGRUPOSURA,VarECOPETROL,VarEXITO,VarETB)
VarianzaMedia<-mean(Varianzas)
RD <-VarianzaMedia/10

#Riesgo no diversificable
V1<-c(MATRIZCOV[,1]-MATRIZCOV[1,1])
V1a<-c(MATRIZCOV[,1])
V1a[-1]

V2<-c(MATRIZCOV[,2]-MATRIZCOV[2,2])
V2a<-c(MATRIZCOV[,2])
V2a[-2]


V3 = C(MATRIZCOV[,3]-MATRIZCOV[3,3])
V3a<-c(MATRIZCOV[,3])
V3a[-3]

V4<-c(MATRIZCOV[,4]-MATRIZCOV[4,4])
V4a = c(MATRIZCOV[,4])
V4a[-4]

V5<-c(MATRIZCOV[,5]-MATRIZCOV[5,5])
V5a = c(MATRIZCOV[,5])
V5a[-5]

V6<-c(MATRIZCOV[,6]-MATRIZCOV[6,6])
V6a = c(MATRIZCOV[,6])
V6a[-6]

V7<-c(MATRIZCOV[,7]-MATRIZCOV[7,7])
V7a = c(MATRIZCOV[,7])
V7a[-7]

V8<-c(MATRIZCOV[,8]-MATRIZCOV[8,8])
V8a = c(MATRIZCOV[,8])
V8a[-8]

V9<-c(MATRIZCOV[,9]-MATRIZCOV[9,9])
V9a = c(MATRIZCOV[,9])
V9a[-9]

V10<-c(MATRIZCOV[,10]-MATRIZCOV[10,10])
V10a = c(MATRIZCOV[,10])
V10a[-10]

#Medias de cada vector
R1<-mean(V1a)
R2<-mean(V2a)
R3<-mean(V3a)
R4<-mean(V4a)
R5<-mean(V5a)
R6<-mean(V6a)
R7<-mean(V7a)
R8<-mean(V8a)
R9<-mean(V9a)
R10<-mean(V10a)


CovarMedia<-sum(R1,R2,R3,R4,R5,R6,R7,R8,R9,R10)

CovarianzaMedia<-CovarMedia/10

RiesNoDiver<-CovarianzaMedia*9/10

VarianzaP<-RD+RiesNoDiver

#Graficas retornos acciones
layout(matrix(c(1:10), nrow=2, byrow=FALSE))


plot(datos_bloomberg$AVAL,main = "Retorno Aval", xlab = "Dias", ylab = "Precios", col = "Red")
plot(datos_bloomberg$NUTRESA,main = "Retorno Nutresa", xlab = "Dias", ylab = "Precios", col = "Red")
plot(datos_bloomberg$CEMARGOS,main = "Retorno Cemargos", xlab = "Dias", ylab = "Precios", col = "Red")
plot(datos_bloomberg$BANCOLO,main = "Retorno Bancolombia", xlab = "Dias", ylab = "Precios", col = "Red")
plot(datos_bloomberg$BOGOTA,main = "Retorno Bogota", xlab = "Dias", ylab = "Precios", col = "Red")
plot(datos_bloomberg$AVIANCA,main = "Retorno Avianca", xlab = "Dias", ylab = "Precios", col = "Red")
plot(datos_bloomberg$GRUPOSURA,main = "Retorno Grupo Sura", xlab = "Dias", ylab = "Precios", col = "Red")
plot(datos_bloomberg$ECOPETROL,main = "Retorno Ecopetrol", xlab = "Dias", ylab = "Precios", col = "Red")
plot(datos_bloomberg$EXITO,main = "Retorno Exito", xlab = "Dias", ylab = "Precios", col = "Red")
plot(datos_bloomberg$ETB,main = "Retorno ETB", xlab = "Dias", ylab = "Precios", col = "Red")

#Grafica1
RIESGO =c(RIESGOETB,RIESGOAVIANCA,RIESGOAVAL,RIESGONUTRESA,RIESGOCEMARGOS,RIESGOBANCOLO,RIESGOBOGOTA,RIESGOSURA,RIESGOECOPETL,RIESGOEXITO )
RETORNO =c(METB,MAVIANCA,MAVAL,MNUTRESA,MCEMARGOS,MBANCOLO,MBOGOTA,MGRUPOSURA,MECOPETL,MEXITO)
plot(RIESGO,RETORNO,main = "RiesgoVSRetorno",pch =c(1,2,3,4,5,6,7,8,9,10))
legend("topright",legend = c("Etb","Avianca","Aval","Nutresa","Argos","Bancolombia","Bogtá","Sura","Ecopetrol","Éxito"),pch =c(1,2,3,4,5,6,7,8,9,10))

#Grafica2
RIESGOING =c(RIESGOETB,RIESGOAVIANCA,RIESGOAVAL,RIESGONUTRESA,RIESGOCEMARGOS,RIESGOBANCOLO,RIESGOBOGOTA,RIESGOSURA,RIESGOECOPETL,RIESGOEXITO,Desv_PortafolioING )
RETORNOING =c(METB,MAVIANCA,MAVAL,MNUTRESA,MCEMARGOS,MBANCOLO,MBOGOTA,MGRUPOSURA,MECOPETL,MEXITO,Desv_PortafolioING)
plot(RIESGOING,RETORNOING,main = "RiesgoVSRetorno",pch =c(1,2,3,4,5,6,7,8,9,10,11))
legend("topleft",legend = c("Etb","Avianca","Aval","Nutresa","Argos","Bancolombia","Bogtá","Sura","Ecopetrol","Éxito","PortIngenuo"),pch =c(1,2,3,4,5,6,7,8,9,10,11))

#Grafica3
RIESGOMIN =c(RIESGOETB,RIESGOAVIANCA,RIESGOAVAL,RIESGONUTRESA,RIESGOCEMARGOS,RIESGOBANCOLO,RIESGOBOGOTA,RIESGOSURA,RIESGOECOPETL,RIESGOEXITO,Desv_PortafolioING,Desv_PortafolioMinV )
RETORNOMIN =c(METB,MAVIANCA,MAVAL,MNUTRESA,MCEMARGOS,MBANCOLO,MBOGOTA,MGRUPOSURA,MECOPETL,MEXITO,Retorno_ING,Retorno_MinV)
plot(RIESGOMIN,RETORNOMIN,main = "RiesgoVSRetorno",pch =c(1,2,3,4,5,6,7,8,9,10,11,12))
legend("bottomright",legend = c("Etb","Avianca","Aval","Nutresa","Argos","Bancolombia","Bogtá","Sura","Ecopetrol","Éxito","PortIngenuo","VarMin"),pch =c(1,2,3,4,5,6,7,8,9,10,11,12))

#Grafica4
#VerifcarDatosConMafe
#Graficas8
#BlogspotAval
boxplot(Bloomberg[2:1396,1],main="Aval")
#BlogspotNutresa
boxplot(Bloomberg[2:1396,2],main="Nutresa")
#BlogspotArgos
boxplot(Bloomberg[2:1396,3],main="Argos")
#BlogspotBancolombia
boxplot(Bloomberg[2:1396,4],main="Bancolombia")
#BlogspotBogota
boxplot(Bloomberg[2:1396,5],main="Bogotá")
#BlogspotAvianca
boxplot(Bloomberg[2:1396,6],main="Avianca")
#BlogspotSura
boxplot(Bloomberg[2:1396,7],main="Sura")
#BlogspotEcopetrol
boxplot(Bloomberg[2:1396,8],main="Ecopetrol")
#BlogspotExito
boxplot(Bloomberg[2:1396,9],main="Exito")
#BlogspotEtb
boxplot(Bloomberg[2:1396,10],main="Etb")
#Todas
boxplot(Bloomberg[2:1396,1:10])
 #Resumen
summary(Bloomberg)
    
 
#Boxplot del retorno promedio de las acciones

layout(matrix(c(1:1), nrow=1, byrow=FALSE))

boxplot(Retornos1,main="Retornos promedio")
  
  
