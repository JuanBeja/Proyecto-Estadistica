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

#fRONTERA EFICIENTE
DESVIACIONES = c(0.009,0.012,0.018,0.032,0.064,0.09,0.012)
RETORNOSFE = c(0.000398537,0.000597194,0.000980104,0.001854322,0.003834443,0.005439652,0.00729074)

 
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

VarIngen <- c(25/100,25/100,25/100,25/100)*cov(Retornos[,1:4])
a<-c(25/100,25/100,25/100,25/100)
MatrizTrans1<-cbind(a)
Varingenuo<-Intermedia*MatrizTrans1
c(Varingenuo)
c(Varingenuo[1])
f<-c(Varingenuo[1])
unlist(Varingenuo)
unlist(Varingenuo[,1])
cbind(Varingenuo[1])
f2<-cbind(Varingenuo[1])
sum(f2)
Varianza<-f2
VarianzaIngenuo<-sum(Varianza)
RetornosEsp<-c(RTGS,RTGA,RTBC,RTBB)
RetornoIngen<-RetornosEsp*a
z<-cbind(RetornoIngen)
RetornoIngenuo<-sum(z)
VarGS<-RIGS^2
VarGA<-RIGA^2
VarBC<-RIBC^2
VarBB<-RIBB^2
Varianzas<-c(VarGS,VarGA,VarBC,VarBB)
VarianzaMedia<-mean(Varianzas)
Riesgodiver<-VarianzaMedia/4
MatCov<-cov(Retornos[,1:4])
V1<-c(MatCov[,1])
V2<-c(MatCov[,2]-MatCov[2,2])
V2a<-c(MatCov[,2])
V2a[-2]
V2<-V2a[-2]
v3a<-c(MatCov[,3])
V3<-v3a[-3]
V4<-c(MatCov[1:3,4])
V1<-V1[-1]
mean(mean(V1),mean(V2),mean(V3),mean(V4))
R<-mean(V1)
X<-mean(V1)
L<-mean(V1)
W<-mean(V1)
CovarMedia<-sum(R,X,L,W)
CovarianzaMedia<-CovarMedia/4
RiesNoDiver<-CovarianzaMedia*3/4
VarianzaP<-Riesgodiver+RiesNoDiver

  
    
  
  
  
