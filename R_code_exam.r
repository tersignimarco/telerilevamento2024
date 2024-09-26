#Progetto di Marco Tersigni

#Questo progetto è incentrato sullo studio del lago Chad (o lago Tchad), grande 
#specchio d'acqua situato nella parte centro settentrionale del continente africano
#precisamente al confine tra Chad, Niger, Nigeria e Camerun.
#Il lago si forma circa 40'000 anni fa, raggiunge il suo picco di espansione circa 9'000 anni fa,
#dove si suddivide geologicamente in tre regioni, due delle quali oggi scomparse. Ad oggi rimane 
#solo il lago Chad, ma anch'esso non gode di buona salute.
#Ho scelto di elaborare questo progetto perché il lago Chad con la presenza di acqua e vegetazione
#rappresenta la prima oasi
#per le specie migratrici svernanti che dall'Europa attraversano il deserto del Sahara per
#poi passare i mesi freddi in Africa

#Settaggio della working directory e installazione di pacchetti
setwd("C:/Users/Marco/OneDrive/Desktop/TeleGeoEco")

library("RStoolbox") #remote sensing analysis
library("raster") #elaboratore di immagini
library("gridExtra") #combinazione di grafici e tabelle
library("ggplot2") #creazione di grafici

#Il focus del progetto è studiare l'andamento della presenza di acqua e vegetazione negli ultimi 40 anni
#utilizzando 4 immagini satellitari dell'anno 1987 (LandSat 5), 2003 (LandSat 7), 2013 (LandSat 7) e
#2021 (LandSat 8).
#Le immagini originali sono state editate con Adobe Illustrator.
Chad1987<-brick("C:/Users/Marco/OneDrive/Desktop/TeleGeoEco/LakeTchad/Chad1987.jpg")
Chad2003<-brick("C:/Users/Marco/OneDrive/Desktop/TeleGeoEco/LakeTchad/Chad2003.jpg")
Chad2013<-brick("C:/Users/Marco/OneDrive/Desktop/TeleGeoEco/LakeTchad/Chad2013.jpg")
Chad2021<-brick("C:/Users/Marco/OneDrive/Desktop/TeleGeoEco/LakeTchad/Chad2021.jpg")

#Classificazione non supervisionata: suddivisione delle immagini in 5 classi e colori
EnvChad1987<-unsuperClass(Chad1987, nClasses = 5)
EnvChad2003<-unsuperClass(Chad2003, nClasses = 5)
EnvChad2013<-unsuperClass(Chad2013, nClasses = 5)
EnvChad2021<-unsuperClass(Chad2021, nClasses = 5)

#Primo plottaggio per determinare la posizione nella legenda dell'area interessata da
#acqua e vegetazione
plot(EnvChad1987$map)
plot(EnvChad2003$map)
plot(EnvChad2013$map)
plot(EnvChad2021$map)

#Secondo plottaggio in cui si posizionana i colori nell'ordine corretto
color1987<-c("green", "grey", "white", "blue", "lightgrey")
color2003<-c("grey", "blue", "white", "green", "lightgrey")
color2013<-c("grey", "green", "white", "lightgrey", "blue")
color2021<-c("blue", "grey", "white", "green", "lightgrey")
par(mfrow=c(2, 2))
plot(EnvChad1987$map, col=color1987, main="Chad1987")
plot(EnvChad2003$map, col=color2003, main="Chad2003")
plot(EnvChad2013$map, col=color2013, main="Chad2013")
plot(EnvChad2021$map, col=color2021, main="Chad2021")

#Visulizzazione delle frequenze per determinare la grandezza, in pixels, delle aree interessate
freq(EnvChad1987$map)
freq(EnvChad2003$map)
freq(EnvChad2013$map)
freq(EnvChad2021$map)

#Area totale della mappa
AreaTOT<-203890

#Calcolo delle percentuali di ognuna delle 5 aree
Percent1987<-freq(EnvChad1987$map) / AreaTOT
Percent2003<-freq(EnvChad2003$map) / AreaTOT
Percent2013<-freq(EnvChad2013$map) / AreaTOT
Percent2021<-freq(EnvChad2021$map) / AreaTOT

#Percentuali di aree con acqua, aree con vegetazione e anni
Water<-c(22.7, 15.8, 18.3, 25.5)
NoWater<-c(77.3, 84.2, 81.7, 74.4)
Vegetation<-c(25.4, 28.8, 34.6, 33.3)
NoVegetation<-c(74.6, 71.2, 65.4, 66.7)
Years<-c(1987, 2003, 2013, 2021)

#Costruzione di un dataframe ed elaborazione di grafici sull'andamento di acqua, 
#andamento di vegetazione e l'unione di entrambi

DatiChad<-data.frame(Years, Water, NoWater, Vegetation, NoVegetation)
ggplot(DatiChad, aes(x=Years)) + geom_line(aes(y=Water), color="blue") + ggtitle("Water Trend")
ggplot(DatiChad, aes(x=Years)) + geom_line(aes(y=Vegetation), color="darkgreen") + ggtitle("Vegetation Trend")

ggplot(DatiChad, aes(x=Years)) + geom_line(aes(y=Water), color="blue") + geom_line(aes(y=Vegetation), color="darkgreen") + ylab("Water and Vegetation") + ggtitle("Water & Vegetation Trends")
