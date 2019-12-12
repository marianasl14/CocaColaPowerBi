getwd()
setwd("/Users/marianasantos/Desktop/CD")
Datos<-read.csv('DataSetProyectoFinal.csv')
Data<-read.csv('datosSINna.csv')
Data2<-read.csv('datosSINna.csv')

Datos$ValorVendido<-rowSums(Datos[,4:7])
ValorVendidoP<-aggregate(Datos$ValorVendido,by=list(Datos$Producto),FUN=sum,ra.rm=TRUE)
ValorVendidoC<-aggregate(Datos$ValorVendido,by=list(Datos$Cliente),FUN=sum,ra.rm=TRUE)
columnas<-c('Nombre','Venta Total')
colnames(ValorVendidoP)<-columnas
columnas<-c('Id','Venta Total')
colnames(ValorVendidoC)<-columnas

ValorVendidoP$Total<-ValorVendidoP$`Venta Total`*40
ValorVendidoC$Total<-ValorVendidoC$`Venta Total`*40

ProductoPareto<-ValorVendidoP[order(ValorVendidoP$Total,decreasing=TRUE),]
ClientePareto<-ValorVendidoC[order(ValorVendidoC$Total,decreasing=TRUE),]

ProductoPareto$Participacion<-(ProductoPareto$Total/sum(ProductoPareto[, 'Total'], na.rm = TRUE)*100)
ClientePareto$Participacion<-(ClientePareto$Total/sum(ClientePareto[, 'Total'], na.rm = TRUE)*100)

ProductoPareto$Pacum<-cumsum(ProductoPareto[,'Participacion'])
ClientePareto$Pacum<-cumsum(ClientePareto[,'Participacion'])

##A: 0-80
##B: 81-95
##C:96-100

ProductoPareto$Clasificacion[ProductoPareto$Pacum<=80]<-'A'
ProductoPareto$Clasificacion[ProductoPareto$Pacum<=95 &ProductoPareto$Pacum>81]<-'B'
ProductoPareto$Clasificacion[ProductoPareto$Pacum>96]<-'C'

ClientePareto$Clasificacion[ClientePareto$Pacum<=80]<-'A'
ClientePareto$Clasificacion[ClientePareto$Pacum<=95 &ClientePareto$Pacum>81]<-'B'
ClientePareto$Clasificacion[ClientePareto$Pacum>96]<-'C'

tablaC<-data.frame(table(ClientePareto$Clasificacion))
tablaC$Participacion<-tablaC$Freq/sum(tablaC$Freq)*100

tablaP<-data.frame(table(ProductoPareto$Clasificacion))
tablaP$Participacion<-tablaP$Freq/sum(tablaP$Freq)*100

tablaP$venta<-aggregate(ProductoPareto$Total,by=list(ProductoPareto$Clasificacion),FUN=sum,ra.rm=TRUE)
tablaP$PV[1]<-(536105523/sum(536105523,113510157,29215148))*100
tablaP$PV[2]<-(113510157/sum(536105523,113510157,29215148))*100
tablaP$PV[3]<-(29215148/sum(536105523,113510157,29215148))*100

tablaC$venta<-aggregate(ClientePareto$Total,by=list(ClientePareto$Clasificacion),FUN=sum,ra.rm=TRUE)
tablaC$PV[1]<-(552006233/sum(552006233,96603135,27601065))*100
tablaC$PV[2]<-(96603135/sum(552006233,96603135,27601065))*100
tablaC$PV[3]<-(27601065/sum(552006233,96603135,27601065))*100

acliente<-ClientePareto[ClientePareto$Clasificacion=='A',]
acliente<-head(acliente)
library(ggplot2)
ggplot(data=tablaP, aes(x=Clasificacion, y=PV)) +
  geom_bar(stat="identity", fill="steelblue")+
  theme_minimal()+
  geom_text(aes(label=round(PV)), vjust=1.6, color="white",
            position = position_dodge(0.9), size=3.5)


ggplot(data=tablaC, aes(x=Var1, y=PV)) +
geom_bar(stat="identity", fill="steelblue")+
theme_minimal()+geom_text(aes(label=round(PV)), vjust=1.6, color="white",
            position = position_dodge(0.9), size=3.5)



Data2<-read.csv('datosSINna.csv')
library(tidyverse)
library(corrplot)
library(gridExtra)
library(GGally)
library(knitr)

aggregate(Data, by=list(results$cluster), mean)

graph<-ggpairs(cbind(Data, Cluster=as.factor(results$cluster)),
        columns=1:6, aes(colour=Cluster, alpha=0.5),
        lower=list(continuous="points"),
        upper=list(continuous="blank"),
        axisLabels="none", switch="both") +
  theme_bw()


histo<-Data %>%
  gather(Attributes, value, 1:28) %>%
  ggplot(aes(x=value, fill=Attributes)) +
  geom_histogram(colour="black", show.legend=FALSE) +
  facet_wrap(~Attributes, scales="free_x") +
  labs(x="Values", y="Frequency",
       title="Productos - Histograms") +
  theme_bw()



matrixco<-corrplot(cor(Data2[,2:28]), type="upper", method="ellipse", tl.cex=0.9)




library("PerformanceAnalytics")
columnasIDs<-c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28)
colnames(Data2)<-columnasIDs
matrixcor<-as.data.frame(cor(Data2[,2:28]))
matrixcor[matrixcor$`7`<0,]
matrixcor[matrixcor$`9`<0,]
matrixcor[matrixcor$`25`<0,]

chart.Correlation(Data2[,20:28], histogram=TRUE, pch=19)


