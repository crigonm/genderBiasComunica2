### Esto es la aplicación de "Sesgos de género en la participación de 'Comunica2'"
# por Cristina González Moreno.
#
# UPV - 2021/2022

### Instalación de librerías ---------------------------------------------------
library(shiny)
library(shinydashboard)
library(shinythemes)
library(tidyverse)
library(datos)
library(ggplot2)
library(tidyr)
library(dplyr)
library(devtools)
library(plotly)
library(ggplot2)
library(coin)
library(effsize)
library(wordcloud)
library(RColorBrewer)

### Lectura del Excel y carga de datos -----------------------------------------
library(readxl)
pruebasDataset <- read_excel("pruebasDataset.xlsx")
View(pruebasDataset)

### Creación de variables a emplear --------------------------------------------

# Número total de preguntas respectivas
pregsTotal <- sum(pruebasDataset$`género pregunta`== "H" | pruebasDataset$`género pregunta`=="M" | pruebasDataset$`género pregunta`=="N", na.rm=TRUE)
pregsHombresTotal <- sum(pruebasDataset $`género pregunta` == "H", na.rm = TRUE)
pregsMujeresTotal <- sum(pruebasDataset $`género pregunta` == "M", na.rm = TRUE)

# Proporciones de preguntas (hombres/mujeres)
propHombresTotal <- (pregsHombresTotal/pregsTotal)*100
propMujeresTotal <- (pregsMujeresTotal/pregsTotal)*100

# Lista de los géneros de los ponentes
listaGenPonente <- pruebasDataset$`género ponente`[!is.na(pruebasDataset$ponente)]

# Proporción ponentes masculinos VS. femeninos
genPonente <- paste(listaGenPonente,collapse="")
nPonentesM <- str_count(genPonente, "M")
nPonentesH <- str_count(genPonente, "H")
nPonentesTotal <- nPonentesH+nPonentesM
propPonenteH <- (nPonentesH/nPonentesTotal)*100
propPonenteM <- (nPonentesM/nPonentesTotal)*100

# Número de conferencias total
nConf <- sum(pruebasDataset$ponente != "NA", na.rm = TRUE)
  
# Duración total tiempo hombres y mujeres
durTotal <- sum(na.omit(pruebasDataset$`duración (segs)`))
durH <- sum(na.omit(subset(pruebasDataset$`duración (segs)`, pruebasDataset$`género pregunta`=="H")))
durM <- sum(na.omit(subset(pruebasDataset$`duración (segs)`, pruebasDataset$`género pregunta`=="M")))
durNoId <- sum(na.omit(subset(pruebasDataset$`duración (segs)`, pruebasDataset$`género pregunta`=="N")))

# Lista de duraciones de hombres y mujeres
listaDurMujeres <- (pruebasDataset$`duración (segs)`[pruebasDataset$`género pregunta`=="M"])[!is.na(pruebasDataset$`duración (segs)`[pruebasDataset$`género pregunta`=="M"])]
listaDurHombres <- (pruebasDataset$`duración (segs)`[pruebasDataset$`género pregunta`=="H"])[!is.na(pruebasDataset$`duración (segs)`[pruebasDataset$`género pregunta`=="H"])]


# Duración media tiempo hombres y mujeres
durMediaH <- mean(listaDurHombres)
durMediaM <- mean(listaDurMujeres)

# Mujeres vs. hombres (duraciones)
durHsobreM <- durH/durM
durHsobreMmedia <- durMediaH/durMediaM

# Mediana de las duraciones de mujeres y hombres
medianaDurM <- median(listaDurMujeres)
medianaDurH <- median(listaDurHombres)

# Cantidad de mujeres y hombres hablando más de 60 segundos
durMujerMas60 <- length(subset(listaDurMujeres, listaDurMujeres>60))
durHombreMas60 <- length(subset(listaDurHombres, listaDurHombres>60))

# Número y proporción de conferencias con almenos una mujer ponente 
nConfM <- length(subset(str_count(listaGenPonente, "M"), str_count(listaGenPonente, "M")!="0"))
propConfM <- (nConfM / nConf) * 100

# Número y proporción de conferencias con todo hombres ponentes
nConfH <- nConf - nConfM
propConfH <- (nConfH / nConf) * 100

# Número y proporción de conferencias con todo mujeres ponentes
nConfSoloM <- length(subset(pruebasDataset$`género ponente`, !is.na(pruebasDataset$ponente) & !str_detect(pruebasDataset$`género ponente`,"H")))
propConfSoloM <- (nConfSoloM / nConf) * 100

# Número y proporción de conferencias con almenos un hombre ponente
nConf1Hmin <- length(subset(str_count(listaGenPonente, "H"), str_count(listaGenPonente, "H")!="0"))
propConf1Hmin <- (nConf1Hmin / nConf) * 100

# Lista de los años 
años <- pruebasDataset$año[!duplicated(pruebasDataset$año)]

# Lista id conferencias
idConf <- pruebasDataset$`id ponenciaXaño`[!duplicated(pruebasDataset$`id ponenciaXaño`)]

# Lista de géneros de las preguntas cuando los ponentes son solo hombres
listaGenPregPonenteH <- subset(pruebasDataset$`género pregunta`, !str_detect(pruebasDataset$`género ponente`,"M"))

# Lista de géneros de las preguntas cuando los ponentes son solo mujeres
listaGenPregPonenteM <- subset(pruebasDataset$`género pregunta`, !str_detect(pruebasDataset$`género ponente`,"H"))

# Proporción de mujeres/hombres si los ponentes son solo hombres
propMsoloPonH <- (length(subset(listaGenPregPonenteH, listaGenPregPonenteH=="M")) / length(na.omit(listaGenPregPonenteH))) * 100
propHsoloPonH <- (length(subset(listaGenPregPonenteH, listaGenPregPonenteH=="H")) / length(na.omit(listaGenPregPonenteH))) * 100

# Proporción de mujeres/hombres si los ponentes son solo mujeres
propMsoloPonM <- (length(subset(listaGenPregPonenteM, listaGenPregPonenteM=="M")) / length(na.omit(listaGenPregPonenteM))) * 100
propHsoloPonM <- (length(subset(listaGenPregPonenteM, listaGenPregPonenteM=="H")) / length(na.omit(listaGenPregPonenteM))) * 100

### Creación de tablas y gráficas a utilizar -----------------------------------

# Gráfica de conferencias con almenos una mujer / solo hombres como ponentes a lo largo de los años
listaNumConfs = vector("numeric",length(años))
listaNumConfsM = vector("numeric",length(años))
listaNumConfsH = vector("numeric",length(años))
listaNumConfs1Hmin = vector("numeric", length(años))

for (i in seq_along(años)) {
  listaNumConfs[[i]] <- sum(pruebasDataset$ponente != "NA" & pruebasDataset$año == años[i], na.rm = TRUE)
  listaNumConfsM[[i]] <- length(na.omit(subset(str_match(pruebasDataset$`género ponente`,"M"), pruebasDataset$ponente != "NA" & pruebasDataset$año == años[i])))
  listaNumConfsH[[i]] <- listaNumConfs[i] - listaNumConfsM[i]
  listaNumConfs1Hmin[[i]] <- length(na.omit(subset(str_match(pruebasDataset$`género ponente`,"H"), pruebasDataset$ponente != "NA" & pruebasDataset$año == años[i])))
}

tablaNumConfMH <- data.frame(
  años = rep(pruebasDataset$año[!duplicated(pruebasDataset$año)],2),
  genero = c(rep('Exclusivamente\nhombres',6),rep('Mínimo una\nmujer',6)), 
  nConfs = c(listaNumConfsH, listaNumConfsM)
)

grafNumConfMH <- ggplot(tablaNumConfMH, aes(x=años, y=nConfs, fill=genero)) +
  geom_bar(position = "dodge", stat = "identity") +
  scale_fill_manual(values=c("#80FF92", "#4590FF")) +
  theme_minimal() + 
  labs(x='Año de la conferencia', y='Número de ponencias', fill='Género de los\nponentes')

# Gráfica de mínimo un hombre vs. mínimo una mujer entre los ponentes
grafNumConfMinMH <- ggplot(data.frame(
  años = rep(años,2),
  gen = c(rep("Mínimo un\nhombre",6),rep("Mínimo una\nmujer",6)),
  nconfs = c(listaNumConfs1Hmin,listaNumConfsM)), aes(x= años, y=nconfs, fill=gen)) +
  geom_bar(position = "dodge", stat = "identity") +
  scale_fill_manual(values=c("#80FF92", "#4590FF")) +
  theme_minimal() + 
  labs(x='Año de la conferencia', y='Número de ponencias', fill='Género de los\nponentes')

# Total de primeras preguntas
nPrimPregs <- sum(pruebasDataset$pregunta==1)

# Primera persona en preguntar es un hombre (num y proporción)
primEsH <- sum(pruebasDataset$`género pregunta` == "H" & pruebasDataset$pregunta == 1)
propPrimEsH <- (primEsH / nPrimPregs) * 100

# Primera persona en preguntar es una mujer (num y proporción)
primEsM <- sum(pruebasDataset$`género pregunta` == "M" & pruebasDataset$pregunta == 1)
propPrimEsM <- (primEsM / nPrimPregs) * 100

# Primera persona en preguntar no es posible identificar su sexo
primEsN <- sum(pruebasDataset$`género pregunta` == "N" & pruebasDataset$pregunta == 1)
propPrimEsN <- (primEsN / nPrimPregs) * 100

# Donut chart, primeras preguntas
generoP = c("Hombre", "Mujer", "No identificado")
numsP = c(primEsH,primEsM,primEsN) 
tablaPrimPregs <- data.frame(generoP,numsP)

ymax <- cumsum(c(propPrimEsH,propPrimEsM,propPrimEsN))
ymin <- c(0, head(ymax, n=-1))
labelPosition <- (ymax + ymin) / 2
label <- paste0(generoP, "\n Preguntas: ", numsP)

# Gráfico del número de primeras preguntas según género
grafPrimPregs <- ggplot(tablaPrimPregs, aes(ymax=ymax, ymin=ymin, xmax=4, xmin=3, fill=generoP)) +
  geom_rect() +
  geom_text( x=2, aes(y=labelPosition, label=label, color=generoP), size=4) + # x here controls label position (inner / outer)
  #scale_fill_brewer(palette=3) +
  scale_color_manual(values = c("#20A94B","#4590FF","#7C2CE0")) +
  scale_fill_manual(values = c("#80FF92","#4590FF","#7C2CE0")) +
  coord_polar(theta="y") +
  xlim(c(1, 4)) +
  theme_void() +
  theme(legend.position = "none")

# Tabla de los géneros de las primeras y segundas preguntas
primPregs = vector("numeric",length(pruebasDataset$pregunta)-1)
segPregs = vector("numeric",length(pruebasDataset$pregunta)-1)

for(i in seq_along(pruebasDataset$pregunta)) {
  j <- i - 1
  
  if (j == 0) {
    # Do nothing
  } else if (pruebasDataset$pregunta[[i]]==1 & pruebasDataset$pregunta[[j]]==1) {
      primPregs[[j]] <- pruebasDataset$`género pregunta`[[j]]
      segPregs[[j]] <- NA
  } else if (pruebasDataset$pregunta[[i]]==2 ) {
      if (pruebasDataset$pregunta[[j]]==1) {
        primPregs[[j]] <- pruebasDataset$`género pregunta`[[j]]
        segPregs[[j]] <- pruebasDataset$`género pregunta`[[i]]
      }
  } else {
    primPregs[[j]] <- NA
    segPregs[[j]] <- NA
  }
}

primYsegsPregs <- data.frame(primPregs,segPregs)
primYsegsPregs <- primYsegsPregs[!is.na(primYsegsPregs$segPregs),]

# Si primero habla hombre -> número de segundas preguntas que son hombres (y proporción)
primHsegH <- sum(primYsegsPregs$primPregs=="H" & primYsegsPregs$segPregs=="H")
propPrimHsegH <- ( primHsegH / nrow(primYsegsPregs) ) * 100

# Si primero habla hombre -> número de segundas preguntas que son mujeres (y proporción)
primHsegM <- sum(primYsegsPregs$primPregs=="H" & primYsegsPregs$segPregs=="M")
propPrimHsegM <- ( primHsegM / nrow(primYsegsPregs) ) * 100

# Si primero habla mujer -> número de segundas preguntas que son hombres (y proporción)
primMsegH <- sum(primYsegsPregs$primPregs=="M" & primYsegsPregs$segPregs=="H")
propPrimMsegH <- ( primMsegH / nrow(primYsegsPregs) ) * 100

# Si primero habla mujer -> número de segundas preguntas que son mujeres (y proporción)
primMsegM <- sum(primYsegsPregs$primPregs=="M" & primYsegsPregs$segPregs=="M")
propPrimMsegM <- ( primMsegM / nrow(primYsegsPregs) ) * 100

primGenero = c(rep("Hombre",2), rep("Mujer",2))
segGenero = rep(c("Hombre","Mujer"),2)
nSegsPregs = c(primHsegH,primHsegM,primMsegH,primMsegM)


# Gráfico de barras primera cuestión H/M segunda H/M
tablaPrimSeg <- data.frame(primGenero,segGenero,nSegsPregs)

grafPrimSeg <- ggplot(tablaPrimSeg, aes(x=primGenero, y=nSegsPregs, fill=segGenero)) +
  geom_bar(position = "dodge", stat = "identity") +
  scale_fill_manual(values=c("#80FF92", "#4590FF")) +
  theme_minimal() + 
  labs(x='Género de la persona que realiza la primera pregunta o comentario', y='Número de segundas preguntas o comentarios', fill='Género')

# Si primero habla H/M -> número de siguientes preguntas de mujeres
mujPorConf = vector("numeric",length(idConf))
hombrePorConf = vector("numeric", length(idConf))
longConf = vector("numeric", length(idConf))

for (i in seq_along(idConf)) {
  mujPorConf[[i]] <- sum(str_count(subset(pruebasDataset$`género pregunta`,pruebasDataset$`id ponenciaXaño`==idConf[[i]] & pruebasDataset$pregunta!=1), "M"))
  hombrePorConf[[i]] <- sum(str_count(subset(pruebasDataset$`género pregunta`,pruebasDataset$`id ponenciaXaño`==idConf[[i]] & pruebasDataset$pregunta!=1), "H"))
  longConf[[i]] <- length(subset(pruebasDataset$`género pregunta`,pruebasDataset$`id ponenciaXaño`==idConf[[i]])) - 1
}

tablaMujerLongConf <- data.frame(año=subset(pruebasDataset$año,pruebasDataset$pregunta==1),na.omit(data.frame(idConf,mujPorConf,hombrePorConf,longConf)),gen=na.omit(subset(pruebasDataset$`género pregunta`,pruebasDataset$pregunta==1)))

propMujeresPrimH = vector("numeric",length(años))
propMujeresPrimM = vector("numeric",length(años))

for (i in seq_along(años)) {
  propMujeresPrimH[[i]] = 
    sum(subset(tablaMujerLongConf$mujPorConf, tablaMujerLongConf$año==años[[i]] & tablaMujerLongConf$gen=="H")) /
      sum(subset(tablaMujerLongConf$longConf, tablaMujerLongConf$año==años[[i]] & tablaMujerLongConf$gen=="H"))
  propMujeresPrimM[[i]] = 
    sum(subset(tablaMujerLongConf$mujPorConf, tablaMujerLongConf$año==años[[i]] & tablaMujerLongConf$gen=="M")) /
      sum(subset(tablaMujerLongConf$longConf, tablaMujerLongConf$año==años[[i]] & tablaMujerLongConf$gen=="M"))
  
  if (is.nan(propMujeresPrimH[[i]])) { propMujeresPrimH[[i]] <- 0 }
  if (is.nan(propMujeresPrimM[[i]])) { propMujeresPrimM[[i]] <- 0 }
}

tablaSigPregsManual <- data.frame(
  años = rep(años,2),
  genero = c(rep("Hombre",length(años)), rep("Mujer",length(años))), 
  propMujeres = c(propMujeresPrimH,propMujeresPrimM)
)

grafSigPregsManual <- ggplot(tablaSigPregsManual, aes(x = años, y = propMujeres, fill = genero)) +
  geom_bar(position = "dodge", stat = "identity") +
  scale_fill_manual(values=c("#80FF92", "#4590FF")) +
  theme_minimal() + 
  theme(text = element_text(size = 10)) +
  labs(x='Año de la conferencia', y='Proporción de siguientes preguntas hechas por mujeres', fill='Género de\nquien hizo\nla primera\npregunta')

tablaSigPregsM <- data.frame(
  genero = c("Hombre", "Mujer"),
  propMujeres = c(sum(subset(tablaMujerLongConf$mujPorConf, tablaMujerLongConf$gen=="H")) /
                    sum(subset(tablaMujerLongConf$longConf, tablaMujerLongConf$gen=="H")),
                  sum(subset(tablaMujerLongConf$mujPorConf, tablaMujerLongConf$gen=="M")) /
                    sum(subset(tablaMujerLongConf$longConf, tablaMujerLongConf$gen=="M"))))

grafSigPregsM <- ggplot(tablaSigPregsM, aes(x = genero, y = propMujeres)) + 
  geom_bar(position = "dodge", stat = "identity", fill = "#4590FF") +
  theme_minimal() + 
  theme(text = element_text(size = 10)) +
  labs(x='Género de quien realizó la primera pregunta', y='Proporción de siguientes preguntas hechas por mujeres')

# Gráfica de las conferencias por año (mujer vs. hombre)
# En términos absolutos
npregsH = vector("numeric",length(años))
npregsM = vector("numeric",length(años))
for (i in seq_along(años)) {
  npregsH[[i]] <- sum(pruebasDataset $`género pregunta` == "H" & pruebasDataset$año == años[i], na.rm = TRUE)
  npregsM[[i]] <- sum(pruebasDataset $`género pregunta` == "M" & pruebasDataset$año == años[i], na.rm = TRUE)
}

tablaAnualMH <- data.frame(
  años = rep(pruebasDataset$año[!duplicated(pruebasDataset$año)],2),
  genero = c(rep('Hombre',6),rep('Mujer',6)), 
  npregs = c(npregsH,npregsM)) 

grafAbsAnualMH <- ggplot(tablaAnualMH,aes(x=años, y=npregs, fill=genero)) + 
  geom_bar(position = "dodge", stat = "identity") +
  scale_fill_manual(values=c("#80FF92", "#4590FF")) +
  theme_minimal() + 
  labs(x='Año de la conferencia', y='Número de preguntas o comentarios', fill='Género')

# En términos relativos
grafRelAnualMH <- ggplot(tablaAnualMH, aes(x=años, y=npregs, fill=genero)) +
  geom_bar(position = "fill", stat = "identity") +
  scale_fill_manual(values=c("#80FF92", "#4590FF")) +
  theme_minimal() + 
  labs(x='Año de la conferencia', y='Proporción del total de preguntas o comentarios', fill='Género') +
  geom_hline(yintercept = 0.50, col = "red")

# Gráfica BOX WHISKER de las duraciones de hombres y mujeres
tablaDurMH <- data.frame(
  genero = c(rep("Mujer", length(listaDurMujeres)), rep("Hombre", length(listaDurHombres))),
  duraciones = c(listaDurMujeres, listaDurHombres)
)

grafDurMH <- ggplot(tablaDurMH, aes(x=genero, y=duraciones, colour=genero)) +
  geom_boxplot(outlier.shape = NA) +
  scale_color_manual(values = c("#80FF92", "#4590FF")) +
  geom_jitter() +
  theme_minimal() +
  labs(x='Género', y='Duración de la pregunta o comentario (segundos)', color='Género')

# Gráfica sobre las duraciones totales combinadas por años
listaDurCombM = vector("numeric",length(años))
listaDurCombH = vector("numeric",length(años))

for (i in seq_along(años)) {
  listaDurCombM[[i]] <- sum(na.omit(subset(pruebasDataset$`duración (segs)`, pruebasDataset$año==años[i] & pruebasDataset$`género pregunta`=="M")))
  listaDurCombH[[i]] <- sum(na.omit(subset(pruebasDataset$`duración (segs)`, pruebasDataset$año==años[i] & pruebasDataset$`género pregunta`=="H")))
}

tablaDurCombMH <- data.frame(
  años = rep(años, 2),
  genero = c(rep('Hombre', 6), rep('Mujer', 6)),
  durComb = c(listaDurCombH, listaDurCombM)
)

grafDurCombMH <- ggplot(tablaDurCombMH, aes(x=años, y=durComb, fill=genero)) +
  geom_bar(position = "dodge", stat = "identity") +
  scale_fill_manual(values=c("#80FF92", "#4590FF")) +
  theme_minimal() + 
  theme(text = element_text(size = 10)) +
  labs(x='Año de la conferencia', y='Duración combinada total de las preguntas (segundos)', fill='Género')

# Gráfica sobre el número de mujeres y hombres que hablan más de 60 segundos por años
listaDurMas60M = vector("numeric",length(años))
listaDurMas60H = vector("numeric",length(años))

for (i in seq_along(años)) {
  listaDurMas60M[[i]] <- length(subset(pruebasDataset$`duración (segs)`, pruebasDataset$`duración (segs)`>60 & pruebasDataset$año == años[i] & pruebasDataset$`género pregunta`=="M"))
  listaDurMas60H[[i]] <- length(subset(pruebasDataset$`duración (segs)`, pruebasDataset$`duración (segs)`>60 & pruebasDataset$año == años[i] & pruebasDataset$`género pregunta`=="H"))
}

tablaDurMas60MH <- data.frame(
  años = rep(años, 2),
  genero = c(rep('Hombre', 6), rep('Mujer', 6)),
  durMas60 = c(listaDurMas60H, listaDurMas60M)
)

grafDurMas60MH <- ggplot(tablaDurMas60MH, aes(x=años, y=durMas60, fill=genero)) +
  geom_bar(position = "dodge", stat = "identity") +
  scale_fill_manual(values=c("#80FF92", "#4590FF")) +
  theme_minimal() + 
  labs(x='Año de la conferencia', y='Número de preguntas o comentarios de más de 60s.', fill='Género')

# Gráfica de las proporciones de los ponentes por géneros
grafPropPonentes <- plot_ly(labels = ~gen, values = ~prop, legendgroup = ~gen,
  textinfo = 'label+percent',
  texttemplate = "%{percent:.1%}",
  hovertemplate = "%{label} <br> %{percent:.1%} <br> %{value}",
  marker = list(colors = c("#80FF92","#4590FF"))) %>%
  add_pie(data = data.frame(
    gen = c("Hombre","Mujer"),
    prop = c(propPonenteH,propPonenteM)
  ), name = "GP", domain = list(row = 0, column = 0))

# Gráfica de la proporción de los participantes según género de los ponentes (exclusivamente hombre/mujer)
grafpropXponX <- ggplot(data.frame(
  genP = c(rep("Hombre",2),rep("Mujer",2)),
  gen = c("Hombre","Mujer","Hombre","Mujer"),
  prop = c(propHsoloPonH,propMsoloPonH,propHsoloPonM,propMsoloPonM)), aes(x = genP, y = prop, fill=gen)) + 
  geom_bar(position = "fill", stat = "identity") +
  scale_fill_manual(values=c("#80FF92", "#4590FF")) +
  theme_minimal() + 
  labs(x='Género del ponente', y='Proporción de la participación', fill='Género de los\nparticipantes') +
  geom_hline(yintercept = 0.50, col = "red")

### Tests ----------------------------------------------------------------------

# Comprobación de normalidad en las duraciones de tiempo
# Shapiro-Wilk Test
duraciones <- pruebasDataset$`duración (segs)`[!is.na(pruebasDataset$`duración (segs)`)]

pruebaNormDurM <- shapiro.test(listaDurMujeres)
pruebaNormDurH <- shapiro.test(listaDurHombres)

plotnM <- function(listaDurMujeres,main="Histograma de frecuencias \ny distribución ",
                  xlab="Duraciones",ylab="Densidad",col="#4590FF") {
  min <- min(listaDurMujeres)
  max <- max(listaDurMujeres)
  media <- mean(listaDurMujeres)
  dt <- sd(listaDurMujeres)
  hist(listaDurMujeres,freq=F,main=main,xlab=xlab,ylab=ylab,col=col)
  curve(dnorm(x,media,dt), min, max,add = T,col="red")
}

histNormDurM <- plotnM(listaDurMujeres,main="Distribución duraciones de las mujeres")


plotnH <- function(listaDurHombres,main="Histograma de frecuencias \ny distribución ",
                  xlab="Duraciones",ylab="Densidad",col="#80FF92") {
  min <- min(listaDurHombres)
  max <- max(listaDurHombres)
  media <- mean(listaDurHombres)
  dt <- sd(listaDurHombres)
  hist(listaDurHombres,freq=F,main=main,xlab=xlab,ylab=ylab,col=col)
  curve(dnorm(x,media,dt), min, max,add = T,col="red")
}

histNormDurH <- plotnH(listaDurHombres,main="Distribución duraciones de los hombres")

# Comprobación de normalidad transformando a logaritmos
pruebaNormLogDurM <- shapiro.test(log(listaDurMujeres))
pruebaNormLogDurH <- shapiro.test(log(listaDurHombres))

logDurMujeres <- log(listaDurMujeres)
plotn <- function(logDurMujeres,main="Histograma de frecuencias \ny distribución ",
                  xlab="Duraciones",ylab="Densidad",col="#4590FF") {
  min <- min(logDurMujeres)
  max <- max(logDurMujeres)
  media <- mean(logDurMujeres)
  dt <- sd(logDurMujeres)
  hist(logDurMujeres,freq=F,main=main,xlab=xlab,ylab=ylab,col=col)
  curve(dnorm(x,media,dt), min, max,add = T,col="red")
}

histNormLogDurM <- plotn(logDurMujeres,main="Distribución duraciones de las mujeres")


logDurHombres <- log(listaDurHombres)
plotn <- function(logDurHombres,main="Histograma de frecuencias \ny distribución ",
                  xlab="Duraciones",ylab="Densidad",col="#80FF92") {
  min <- min(logDurHombres)
  max <- max(logDurHombres)
  media <- mean(logDurHombres)
  dt <- sd(logDurHombres)
  hist(logDurHombres,freq=F,main=main,xlab=xlab,ylab=ylab,col=col)
  curve(dnorm(x,media,dt), min, max,add = T,col="red")
}

histNormLogDurH <- plotn(logDurHombres,main="Distribución duraciones de los hombres")

# Comprobación de igualdad de varianzas 
pruebaVarLogDurMH <- var.test(logDurHombres,logDurMujeres)

# Comprobación de prueba T Student
pruebaMedLogDurMH <- t.test(logDurHombres,logDurMujeres)

# Tamaño de efecto
tam_efLogDurMH <- cohen.d(logDurHombres,logDurMujeres)


# Test chi-cuadrado para comprobar diferencias de proporciones

# Porcentaje ponentes 
pruebaPropGenPon <- prop.test(c(nPonentesH,nPonentesM),c(nPonentesTotal,nPonentesTotal), alternative = "two.sided", conf.level = 0.95, correct = FALSE)

# Ponentes mínimo un hombre vs. mínimo una mujer
pruebaPropPon1vs1 <- prop.test(c(nConf1Hmin,nConfM),c(nConf,nConf), alternative = "two.sided", conf.level = 0.95, correct = FALSE)

# Ponentes exclusivamente hombres vs. exclusivamente mujer
pruebaPropPonSoloM_H <- prop.test(c(nConfH,nConfSoloM),c(nConf,nConf), alternative = "two.sided", conf.level = 0.95, correct = FALSE)

# Proporción de mujeres según quien preguntó primero
PruebaPropMujSegun1Preg <- prop.test(x = c(mujPostH = sum(subset(tablaMujerLongConf$mujPorConf, tablaMujerLongConf$gen=="H")),
                                           mujPostM = sum(subset(tablaMujerLongConf$mujPorConf, tablaMujerLongConf$gen=="M"))),
                                    n = c(totalH = sum(subset(tablaMujerLongConf$longConf, tablaMujerLongConf$gen=="H")),
                                          totalM = sum(subset(tablaMujerLongConf$longConf, tablaMujerLongConf$gen=="M"))),
                                    correct = FALSE)

# Pruebas del 2016
pruebasPorGenero16 <- function() {

  durM16 <- subset(pruebasDataset$`duración (segs)`, pruebasDataset$`género pregunta`=="M" & pruebasDataset$año==2016)
  durH16 <- subset(pruebasDataset$`duración (segs)`, pruebasDataset$`género pregunta`=="H" & pruebasDataset$año==2016)
  
  testNormM16 <- shapiro.test(durM16)
  testNormH16 <- shapiro.test(durH16)
  
  testNormLogM16 <- shapiro.test(log(durM16))
  testNormLogH16 <- shapiro.test(log(durH16))
    
  testVarLog16 <- var.test(log(durH16),log(durM16))
  
  testMediaLog16 <- t.test(log(durH16),log(durM16), alternative = "greater")
  
  tam_efLog16 <- cohen.d(log(durH16),log(durM16))
  
}

# Pruebas del 2017
pruebasPorGenero17 <- function() {
  
  durM17 <- subset(pruebasDataset$`duración (segs)`, pruebasDataset$`género pregunta`=="M" & pruebasDataset$año==2017)
  durH17 <- subset(pruebasDataset$`duración (segs)`, pruebasDataset$`género pregunta`=="H" & pruebasDataset$año==2017)
  
  testNormM17 <- shapiro.test(durM17)
  testNormH17 <- shapiro.test(durH17)
  
  testNormLogM17 <- shapiro.test(log(durM17))
  testNormLogH17 <- shapiro.test(log(durH17))
  
  testVarLog17 <- var.test(log(durH17),log(durM17))
  
  testMediaLog17 <- t.test(log(durH17),log(durM17))
  
  tam_efLog17 <- cohen.d(log(durH17),log(durM17))
  
}

# Pruebas del 2018
pruebasPorGenero18 <- function() {
  
  durM18 <- subset(pruebasDataset$`duración (segs)`, pruebasDataset$`género pregunta`=="M" & pruebasDataset$año==2018)
  durH18 <- subset(pruebasDataset$`duración (segs)`, pruebasDataset$`género pregunta`=="H" & pruebasDataset$año==2018)
  
  testNormM18 <- shapiro.test(durM18)
  testNormH18 <- shapiro.test(durH18)
  
  testNormLogM18 <- shapiro.test(log(durM18))
  testNormLogH18 <- shapiro.test(log(durH18))
  
  testVarLog18 <- var.test(log(durH18),log(durM18))
  
  testMediaLog18 <- t.test(log(durH18),log(durM18))
  
  tam_efLog18 <- cohen.d(log(durH18),log(durM18))
  
}

# Pruebas del 2019
pruebasPorGenero19 <- function() {
  
  durM19 <- subset(pruebasDataset$`duración (segs)`, pruebasDataset$`género pregunta`=="M" & pruebasDataset$año==2019)
  durH19 <- subset(pruebasDataset$`duración (segs)`, pruebasDataset$`género pregunta`=="H" & pruebasDataset$año==2019)
  
  testNormM19 <- shapiro.test(durM19)
  testNormH19 <- shapiro.test(durH19)
  
  testNormLogM19 <- shapiro.test(log(durM19))
  testNormLogH19 <- shapiro.test(log(durH19))
  
  testVarLog19 <- var.test(log(durH19),log(durM19))
  
  testMediaLog19 <- t.test(log(durH19),log(durM19))
  
  tam_efLog19 <- cohen.d(log(durH19),log(durM19))
  
}

# Pruebas del 2021
pruebasPorGenero21 <- function() {
  
  durM21 <- na.omit(subset(pruebasDataset$`duración (segs)`, pruebasDataset$`género pregunta`=="M" & pruebasDataset$año==2021))
  durH21 <- na.omit(subset(pruebasDataset$`duración (segs)`, pruebasDataset$`género pregunta`=="H" & pruebasDataset$año==2021))
  
  testNormM21 <- shapiro.test(durM21)
  testNormH21 <- shapiro.test(durH21)
  
  testNormLogM21 <- shapiro.test(log(durM21))
  testNormLogH21 <- shapiro.test(log(durH21))
  
  testVarLog21 <- var.test(log(durH21),log(durM21))
  
  testMediaLog21 <- t.test(log(durH21),log(durM21))
  
  tam_efLog21 <- cohen.d(log(durH21),log(durM21))
  
}

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  # General
  output$nConf <- renderValueBox({valueBox(nConf, "Número de ponencias", icon = icon("glyphicon glyphicon-question-sign", lib = "glyphicon"), color = "navy")})
  output$nAños <- renderValueBox({valueBox(length(años), "Número de años", icon = icon("glyphicon glyphicon-asterisk", lib = "glyphicon"), color = "orange")})
  output$grafAbsAnualMH <- renderPlotly({ggplotly(grafAbsAnualMH)})
  output$grafRelAnualMH <- renderPlotly({ggplotly(grafRelAnualMH)})
  output$pregsHombresTotal <- renderValueBox({valueBox(pregsHombresTotal, "Número de preguntas hechas por hombres", icon = icon("mars"), color = "green")})
  output$pregsMujeresTotal <- renderValueBox({valueBox(pregsMujeresTotal, "Número de preguntas hechas por mujeres", icon = icon("venus"), color = "light-blue")})
  output$propPonenteH <- renderValueBox({valueBox(paste(round(propPonenteH,2), "%", sep = ""), "Proporción de hombres ponentes", icon = icon("mars"), color = "green")}) 
  output$propPonenteM <- renderValueBox({valueBox(paste(round(propPonenteM,2), "%", sep = ""), "Proporción de mujeres ponentes", icon = icon("venus"), color = "light-blue")})
  output$grafSigPregsMui <- renderPlotly({ggplotly(grafSigPregsM)})
  output$mediaDurHombres <- renderValueBox({valueBox(paste(round(durMediaH,2),"s."), "Media de la duración de los hombres", icon = icon("mars"), color = "green")})
  output$mediaDurMujeres <- renderValueBox({valueBox(paste(round(durMediaM,2),"s."), "Media de la duración de las mujeres", icon = icon("venus"), color = "light-blue")})
  output$grafdurMHui <- renderPlotly({ggplotly(grafDurMH)})
  
  # Por años
  output$grafDurMedMHui <- renderPlotly({
  
    # Mediana de las duraciones de mujeres y hombres
    listaMedM = vector("numeric",length(años))
    listaMedH = vector("numeric",length(años))
    
    for (i in seq_along(años)) {
      listaMedM[[i]] <- median(na.omit(subset(pruebasDataset$`duración (segs)`, pruebasDataset$año == años[i] & pruebasDataset$`género pregunta`=="M")))
      listaMedH[[i]] <- median(na.omit(subset(pruebasDataset$`duración (segs)`, pruebasDataset$año == años[i] & pruebasDataset$`género pregunta`=="H")))
    }
    
    tablaMedsAnualMH <- data.frame(
                          años = rep(años, 2),
                          genero = c(rep('Hombre', length(años)), rep('Mujer', length(años))),
                          meds = c(listaMedH, listaMedM)
                        )
    ggplotly(
      ggplot(tablaMedsAnualMH, aes(x=años, y=meds, fill=genero)) +
        geom_bar(position = "dodge", stat = "identity") +
        scale_fill_manual(values=c("#80FF92", "#4590FF")) +
        theme_minimal() + 
        labs(x='Año de la conferencia', y='Mediana de las duraciones', fill='Género')
    )
  })
  
  output$grafDurCombMHui <- renderPlotly({ggplotly(grafDurCombMH)})
  
  output$grafDurMas60MHui <- renderPlotly({ggplotly(grafDurMas60MH)})
  
  output$grafPrimPregsui <- renderPlotly({
    listaPrimPregM = vector("numeric",length(años))
    listaPrimPregH = vector("numeric",length(años))
    
    for (i in seq_along(años)) {
      listaPrimPregM[[i]] <- sum(pruebasDataset$`género pregunta` == "M" & pruebasDataset$pregunta == 1 & pruebasDataset$año==años[[i]])
      listaPrimPregH[[i]] <- sum(pruebasDataset$`género pregunta` == "H" & pruebasDataset$pregunta == 1 & pruebasDataset$año==años[[i]])
    }
    
    ggplotly(
      ggplot(
        data.frame(
          años = rep(años, 2),
          genero = c(rep('Hombre', length(años)), rep('Mujer', length(años))),
          listPrimPregs = c(listaPrimPregH, listaPrimPregM)),
        aes(x=años, y=listPrimPregs, fill=genero)) +
        geom_bar(position = "dodge", stat = "identity") +
        scale_fill_manual(values=c("#80FF92", "#4590FF")) +
        theme_minimal() + 
        labs(x='Año de la conferencia', y='Número de primeras preguntas o comentarios', fill='Género')
    )
  })
  
  output$grafPrimSegHui <- renderPlotly({
    listaPrimPregs = vector("numeric",length(pruebasDataset$pregunta)-1)
    listaSegPregs = vector("numeric",length(pruebasDataset$pregunta)-1)
    years = vector("numeric",length(pruebasDataset$pregunta)-1)
    
    for(i in seq_along(pruebasDataset$pregunta)) {
      j <- i - 1
      if (j == 0) {
        # Do nothing
      } else if (pruebasDataset$pregunta[[i]]==1 & pruebasDataset$pregunta[[j]]==1) {
        years[[j]] <- pruebasDataset$año[[j]]
        listaPrimPregs[[j]] <- pruebasDataset$`género pregunta`[[j]]
        listaSegPregs[[j]] <- NA
      } else if (pruebasDataset$pregunta[[i]]==2 ) {
          if (pruebasDataset$pregunta[[j]]==1) {
            years[[j]] <- pruebasDataset$año[[j]]
            listaPrimPregs[[j]] <- pruebasDataset$`género pregunta`[[j]]
            listaSegPregs[[j]] <- pruebasDataset$`género pregunta`[[i]]
          }
      } else {
        years [[j]] <- NA
        listaPrimPregs[[j]] <- NA
        listaSegPregs[[j]] <- NA
      } 
    }
    
    primYsegsPregsAnual <- data.frame(years,listaPrimPregs,listaSegPregs)
    primYsegsPregsAnual <- na.omit(primYsegsPregsAnual)
    
    primHsegHanual = vector("numeric",length(años))
    primHsegManual = vector("numeric",length(años))
    primMsegHanual = vector("numeric",length(años))
    primMsegManual = vector("numeric",length(años))
      
    for (i in seq_along(años)) {
      primHsegHanual[[i]] <- sum(primYsegsPregsAnual$listaPrimPregs=="H" & primYsegsPregsAnual$listaSegPregs=="H" & primYsegsPregsAnual$years==años[[i]])
      primHsegManual[[i]] <- sum(primYsegsPregsAnual$listaPrimPregs=="H" & primYsegsPregsAnual$listaSegPregs=="M" & primYsegsPregsAnual$years==años[[i]])
      primMsegHanual[[i]] <- sum(primYsegsPregsAnual$listaPrimPregs=="M" & primYsegsPregsAnual$listaSegPregs=="H" & primYsegsPregsAnual$years==años[[i]])
      primMsegManual[[i]] <- sum(primYsegsPregsAnual$listaPrimPregs=="M" & primYsegsPregsAnual$listaSegPregs=="M" & primYsegsPregsAnual$years==años[[i]])
    }
    
    nSegsPregsHAnual = c(primHsegHanual,primHsegManual)
    nSegsPregsMAnual = c(primMsegHanual,primMsegManual)
    
    tablaPrimSegHA <- data.frame(
      segGenero = rep(c(rep("Hombre",6),rep("Mujer",6))),
      nSegsPregs = nSegsPregsHAnual,
      años = rep(años,2))
    
    ggplotly(
      ggplot(tablaPrimSegHA, aes(x=años, y=nSegsPregs, fill=segGenero)) +
        geom_bar(position = "dodge", stat = "identity") +
        scale_fill_manual(values=c("#80FF92", "#4590FF")) +
        theme_minimal() + 
        theme(text = element_text(size = 9)) +
        labs(x='Año de la conferencia', y='Número de segundas preguntas después de un hombre', fill='Género')
    )
  })
  
  output$GrafPrimSegMui <- renderPlotly({
    tablaPrimSegMA <- data.frame(
      segGenero = rep(c(rep("Hombre",6), rep("Mujer",6))),
      nSegsPregs = nSegsPregsMAnual,
      años = rep(años,2))    
    
    ggplotly(
      ggplot(tablaPrimSegMA, aes(x=años, y=nSegsPregs, fill=segGenero)) +
        geom_bar(position = "dodge", stat = "identity") +
        scale_fill_manual(values=c("#80FF92", "#4590FF")) +
        theme_minimal() + 
        theme(text = element_text(size = 9)) +
        labs(x='Año de la conferencia', y='Número de segundas preguntas después de una mujer', fill='Género')
    )
  })
  
  output$grafSigPregsManualui <- renderPlotly({ggplotly(grafSigPregsManual)})
  
  # Por género
  output$selectGrafAnualMH <- renderPlotly({
    ggplotly(  
      ggplot(tablaAnualMH[str_detect(tablaAnualMH$años,input$selectAño),],aes(x=input$selectAño, y=npregs, fill=genero)) + 
        geom_bar(position = "dodge", stat = "identity") +
        scale_fill_manual(values=c("#80FF92", "#4590FF")) +
        theme_minimal() + 
        
        labs(x='Año de la conferencia', y='Número de preguntas o comentarios', fill='Género')
    )})
  
  output$selectGrafDurMH <- renderPlotly({
    listaDurMujeresA <- (pruebasDataset$`duración (segs)`[pruebasDataset$`género pregunta`=="M" & pruebasDataset$año==input$selectAño])[!is.na(pruebasDataset$`duración (segs)`[pruebasDataset$`género pregunta`=="M" & pruebasDataset$año==input$selectAño])]
    listaDurHombresA <- (pruebasDataset$`duración (segs)`[pruebasDataset$`género pregunta`=="H" & pruebasDataset$año==input$selectAño])[!is.na(pruebasDataset$`duración (segs)`[pruebasDataset$`género pregunta`=="H" & pruebasDataset$año==input$selectAño])]
    
    tablaDurMHanual <- data.frame(
      generoA = c(rep("Mujer", length(listaDurMujeresA)), rep("Hombre", length(listaDurHombresA))),
      duracionesA = c(listaDurMujeresA, listaDurHombresA)
    )
    
    ggplotly(
      ggplot(tablaDurMHanual, aes(x=generoA, y=duracionesA, colour=generoA)) +
        geom_boxplot(outlier.shape = NA) +
        scale_color_manual(values = c("#80FF92", "#4590FF")) +
        geom_jitter() +
        theme_minimal() +
        labs(x='Género', y='Duración de la pregunta o comentario (segundos)', color='Género')
    )
  })
  
  output$selectGrafDurMas60MH <- renderPlotly({
    ggplotly(
      ggplot(tablaDurMas60MH[str_detect(tablaDurMas60MH$años,input$selectAño),], aes(x=input$selectAño, y=durMas60, fill=genero)) +
        geom_bar(position = "dodge", stat = "identity") +
        scale_fill_manual(values=c("#80FF92", "#4590FF")) +
        theme_minimal() +
        theme(text = element_text(size = 10)) +
        labs(x='Año de la conferencia', y='Número de preguntas o comentarios de más de 60s.', fill='Género')
    )
  })
  
  output$selectGrafPrimSeg <- renderPlotly({
    primPregsA = vector("numeric",length(pruebasDataset$pregunta))
    segPregsA = vector("numeric",length(pruebasDataset$pregunta))
    
    for(i in seq_along(pruebasDataset$pregunta)) {
      j <- i - 1
      if (pruebasDataset$pregunta[[i]]==2 & pruebasDataset$año[[i]]==input$selectAño) {
        if (pruebasDataset$pregunta[[j]]==1) {
          primPregsA[[i]] <- pruebasDataset$`género pregunta`[[j]]
          segPregsA[[i]] <- pruebasDataset$`género pregunta`[[i]]
        }
      } else {
        primPregsA[[i]] <- NA
        segPregsA[[i]] <- NA
      }
    }
    
    primYsegsPregsA <- data.frame(primPregsA,segPregsA)
    primYsegsPregsA <- primYsegsPregsA[!is.na(primYsegsPregsA$primPregsA),]
    
    primHsegHa <- sum(primYsegsPregsA$primPregsA=="H" & primYsegsPregsA$segPregsA=="H")
    primHsegMa <- sum(primYsegsPregsA$primPregsA=="H" & primYsegsPregsA$segPregsA=="M")
    primMsegHa <- sum(primYsegsPregsA$primPregsA=="M" & primYsegsPregsA$segPregsA=="H")
    primMsegMa <- sum(primYsegsPregsA$primPregsA=="M" & primYsegsPregsA$segPregsA=="M")
    
    nSegsPregsA = c(primHsegHa,primHsegMa,primMsegHa,primMsegMa)
    
    tablaPrimSegA <- data.frame(primGenero,segGenero,nSegsPregsA)
    
    ggplotly(
      ggplot(tablaPrimSegA, aes(x=primGenero, y=nSegsPregsA, fill=segGenero)) +
        geom_bar(position = "dodge", stat = "identity") +
        scale_fill_manual(values=c("#80FF92", "#4590FF")) +
        theme_minimal() + 
        labs(x='Género de la persona que realiza\nla primera pregunta o comentario', y='Número de segundas preguntas o comentarios', fill='Género')
      
    )
  })
  
  output$selectGrafSigPregsM <- renderPlotly({
    ggplotly(
      ggplot(tablaSigPregsManual[str_detect(tablaSigPregsManual$años,input$selectAño),], aes(x = input$selectAño, y = propMujeres, fill = genero)) +
        geom_bar(position = "dodge", stat = "identity") +
        scale_fill_manual(values=c("#80FF92", "#4590FF")) +
        theme_minimal() + 
        theme(text = element_text(size = 10)) +
        labs(x='Año de la conferencia', y='Proporción de siguientes preguntas hechas por mujeres', fill='Género de\nquien hizo\nla primera\npregunta')
    )
  })
  
  output$propDurCombM <- renderValueBox({valueBox(paste(round(sum(tablaDurCombMH[str_detect(tablaDurCombMH$años,
    input$selectAño),]$durComb[tablaDurCombMH[str_detect(tablaDurCombMH$años,input$selectAño),]$genero=="Mujer"])/
    sum(tablaDurCombMH[str_detect(tablaDurCombMH$años,input$selectAño),]$durComb)*100,2), "%", sep = ""),
    "Porcentaje del tiempo hablado por mujeres",
    icon = icon("venus"),
    color = "light-blue")})
  
  output$selectGrafPrimPregs <- renderPlotly({
    primEsHa <- sum(pruebasDataset$`género pregunta` == "H" & pruebasDataset$pregunta == 1 & pruebasDataset$año == input$selectAño)
    primEsMa <- sum(pruebasDataset$`género pregunta` == "M" & pruebasDataset$pregunta == 1 & pruebasDataset$año == input$selectAño)
    primEsNa <- sum(pruebasDataset$`género pregunta` == "N" & pruebasDataset$pregunta == 1 & pruebasDataset$año == input$selectAño)

    tablaPrimPregsA <- data.frame(generoP,c(primEsHa,primEsMa,primEsNa))
    
    plot_ly(labels = ~generoP, values = ~c(primEsHa,primEsMa,primEsNa), legendgroup = ~generoP,
            textinfo = 'label+percent',
            texttemplate = "%{percent:.1%}",
            hovertemplate = "%{label} <br> %{percent:.1%} <br> %{value}",
            marker = list(colors = c("#80FF92","#4590FF","#7C2CE0"))) %>%
      add_pie(data = tablaPrimPregsA, name = "NP", domain = list(row = 0, column = 0))
  })
  
  # Ponentes
  output$nponentesHui <- renderValueBox({valueBox(nPonentesH,"Número de ponentes masculinos", icon = icon("mars"), color = "green")})
  output$nponentesMui <- renderValueBox({valueBox(nPonentesM,"Número de ponentes femeninos", icon = icon("venus"), color = "light-blue")})
  
  output$grafPonConfLollipop <- renderPlotly({
    ggplotly(
      ggplot(
        data.frame(
          x = c( "Mínimo una mujer", "Mínimo un hombre","Exclusivamente mujeres","Exclusivamente hombres"),
          y = c(nConfM,nConf1Hmin,nConfSoloM,nConfH)
        ), aes(x=x,y=y)) + 
        geom_segment(aes(x=x, xend=x, y=0, yend=y),color="skyblue",size=1) +
        geom_point(color = "blue", size = 4, alpha = 0.6) + 
        theme_light() + 
        coord_flip() +
        theme(
          panel.grid.major.y = element_blank(),
          panel.border = element_blank(),
          axis.ticks.y = element_blank()
        ) + 
        labs(x="",y="Número de conferencias")
    )
  })
  
  output$wordcloudH <- renderPlot({
    
    dfTemasSoloH <- data.frame(
      listaTemas = na.omit(subset(pruebasDataset$tema, !str_detect(pruebasDataset$`género ponente`,"M")))
    )
    
    dfTemasSoloH.freq <- table(unlist(dfTemasSoloH))
    
    cbindTemasH <- cbind.data.frame(Temas = names(dfTemasSoloH.freq), Frecuencia = as.integer(dfTemasSoloH.freq))
    
    par(mar = rep(0, 4))
    wordcloud(cbindTemasH$Temas, cbindTemasH$Frecuencia, scale = c(3.8,0.8), max.words = 70,
              min.freq = 1, colors = brewer.pal(8, "Set1"))
    
  })
  
  output$wordcloudM <- renderPlot({
    
    dfTemasSoloM <- data.frame(
      listaTemas = na.omit(subset(pruebasDataset$tema, !str_detect(pruebasDataset$`género ponente`,"H")))
    )
    
    dfTemasSoloM.freq <- table(unlist(dfTemasSoloM))
    
    cbindTemasM <- cbind.data.frame(Temas = names(dfTemasSoloM.freq), Frecuencia = as.integer(dfTemasSoloM.freq))
    
    par(mar = rep(0, 4))
    wordcloud(cbindTemasM$Temas, cbindTemasM$Frecuencia, scale = c(4,0.8), max.words = 70,
              min.freq = 1, colors = brewer.pal(8, "Set1"))
    
  })
  
  
  output$wordcloudMH <- renderPlot({
    
    dfTemas <- data.frame(
      listaTemas = pruebasDataset$tema[!is.na(pruebasDataset$tema)]
    )
    
    dfTemas.freq <- table(unlist(dfTemas))
    
    cbindTemas <- cbind.data.frame(Temas = names(dfTemas.freq), Frecuencia = as.integer(dfTemas.freq))
    
    par(mar = rep(0, 4))
    wordcloud(cbindTemas$Temas, cbindTemas$Frecuencia, scale = c(3.8,0.8), max.words = 70,
                      min.freq = 1, colors = brewer.pal(8, "Set1"))
  })
    
  output$grafNumConfMinMHuiP <- renderPlotly({
    ggplotly(grafNumConfMinMH)
  })
    
  output$grafNumConfMHuiP <- renderPlotly({
    ggplotly(grafNumConfMH)
  })
    
  output$grafPropXponXuiP <- renderPlotly({
    ggplotly(grafpropXponX)
  })
}
