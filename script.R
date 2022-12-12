##Limpiar 
rm(list=ls())

##paquetes 
if(!require(pacman)) install.packages("pacman") ; require(pacman)
require(pacman)
p_load(rstudioapi, tidyverse, sf, rio, osmdata,leaflet, skimr,ggplot, ggplot2)
p_load(tidyverse,rio,skimr,viridis,osmdata,
       ggsn, ## scale bar
       raster,stars, ## datos raster
       ggmap, ## get_stamenmap
       sf, ## Leer/escribir/manipular datos espaciales
       leaflet) ## Visualizaciones dinámicas
path<- dirname(getActiveDocumentContext()$path)
setwd(path) 
dir()

Saber11 <- readRDS("C:/Users/Usuario/Downloads/Saber11.rds")
#### histogramas ####

  #Filtro 
Caribe<-Saber11%>%
  filter(DEPTO==8|DEPTO==13|DEPTO==20 |DEPTO==23| DEPTO==44| DEPTO==47 |DEPTO==70)

#Grafico
hist(Caribe$PUNT_GLOBAL_MEAN,
     main= "Distribución puntaje promedio Región Caribe",
     xlab="Puntaje promedio", 
     col="pink")
##
Pacifico<-Saber11%>%
  filter(DEPTO==19|DEPTO==27|DEPTO==52 |DEPTO==76)

hist(Pacifico$PUNT_GLOBAL_MEAN,
     main= "Distribución puntaje promedio Región Pacífico",
     xlab="Puntaje promedio", 
     col="pink")
##
Andina<-Saber11%>%
  filter(DEPTO==11|DEPTO==15|DEPTO==25 |DEPTO==54|DEPTO==68| DEPTO==73 |DEPTO==5|DEPTO==17|DEPTO==63|DEPTO==66 )

hist(Andina$PUNT_GLOBAL_MEAN,
     main= "Distribución puntaje promedio Región Andina",
     xlab="Puntaje promedio", 
     col="pink")

Orinoquia<-Saber11%>%
  filter(DEPTO==50|DEPTO==81|DEPTO==85|DEPTO==99|DEPTO==95)

hist(Orinoquia$PUNT_GLOBAL_MEAN,
     main= "Distribución puntaje promedio Región Orinoquia",
     xlab="Puntaje promedio", 
     col="pink")

Amazonia<-Saber11%>%
  filter(DEPTO==18|DEPTO==86|DEPTO==91|DEPTO==94|DEPTO==97)

hist(Amazonia$PUNT_GLOBAL_MEAN,
     main= "Distribución puntaje promedio Región Amazonica",
     xlab="Puntaje promedio", 
     col="pink")

Saber11$ones<-1
 
 Basedepto <- Saber11 %>%
  group_by(DEPTO) %>% 
  summarise(n_colegio=sum(ones))
 
setwd("~/GitHub/Trabajo_final/SHP_MGN2018_INTGRD_DEPTO") 

mapacol<-st_read(file.path("MGN_ANM_DPTOS.shp"))%>%
   st_transform(crs = 4326)

mapacol$DPTO_CCDGO<-as.numeric(mapacol$DPTO_CCDGO)

mapa_ncol_df<-merge(x = Basedepto,y=mapacol, by.x="DEPTO", by.y="DPTO_CCDGO",all.x=TRUE)
 
View(mapa_ncol_df)
    
ggplot(data=mapa_ncol_df, aes(geometry = geometry, fill=n_colegio))+
  geom_sf()+ 
  scale_fill_distiller(palette = "RdPu", na.value = "white", trans = "reverse" ) +
  labs(title = "Frecuencia colegios por regiones")


Basedepto2 <- Saber11 %>%
  group_by(DEPTO) %>% 
  summarise(puntaje_dep=mean(PUNT_GLOBAL_MEAN))

mapa_ncol2_df<-merge(x = Basedepto2,y=mapacol, by.x="DEPTO", by.y="DPTO_CCDGO",all.x=TRUE)

View(mapa_ncol2_df)

ggplot(data=mapa_ncol2_df, aes(geometry = geometry, fill=puntaje_dep))+
  geom_sf()+ 
  scale_fill_distiller(palette = "RdPu", na.value = "white", trans = "reverse" ) +
  labs(title = "Puntaje promedio por regiones")


Basedepto3 <- Saber11 %>%
  group_by(DEPTO) %>% 
  summarise(uniandes=sum(UNIANDES))

mapa_ncol3_df<-merge(x = Basedepto3,y=mapacol, by.x="DEPTO", by.y="DPTO_CCDGO",all.x=TRUE)

View(mapa_ncol3_df)

ggplot(data=mapa_ncol3_df, aes(geometry = geometry, fill=uniandes))+
  geom_sf()+ 
  scale_fill_distiller(palette = "RdPu", na.value = "white", trans = "reverse" ) +
  labs(title = "Numero estudiantes en Andes")

Basedepto4 <- Saber11 %>%
  group_by(DEPTO) %>% 
  summarise(uniandes=sum(UNIANDES))

mapa_ncol3_df<-merge(x = Basedepto3,y=mapacol, by.x="DEPTO", by.y="DPTO_CCDGO",all.x=TRUE)

View(mapa_ncol3_df)

ggplot(data=mapa_ncol3_df, aes(geometry = geometry, fill=uniandes))+
  geom_sf()+ 
  scale_fill_distiller(palette = "RdPu", na.value = "white", trans = "reverse" ) +
  labs(title = "Numero estudiantes en Andes")


library(readr)
Departamentos_y_municipios_de_Colombia <- read_csv("C:/Users/Usuario/Downloads/Departamentos_y_municipios_de_Colombia.csv")
View(Departamentos_y_municipios_de_Colombia)

Pacifi<-Departamentos_y_municipios_de_Colombia%>%
  filter(REGION=="Región Pacífico")

Poli_pacifico<-merge(x =Pacifi,y=mapacol, by.x="CÓDIGO DANE DEL DEPARTAMENTO", by.y="DPTO_CCDGO",all.x=TRUE)


Pacifico2<- Pacifico %>%
  group_by(MUNICIPIO) %>% 
  summarise(entra=sum(UNIANDES.LEAD,na.rm=T))


mapa_pacifico<-merge(x =Poli_pacifico,y=Pacifico2, by.x="CÓDIGO DANE DEL MUNICIPIO", by.y="MUNICIPIO",all.y=TRUE)
view(mapa_pacifico)

ggplot(data=mapa_pacifico, aes(geometry = geometry,fill=entra)) + 
  geom_sf()


  scale_fill_distiller(palette = "RdPu", na.value = "white", trans = "reverse" ) +
  labs(title = "Numero estudiantes en Andes")
  
  
  ##########
  Base_RF<- modelos %>%
    filter (ESTRATO==1|ESTRATO==2|ESTRATO==3)%>%
  
  Base_RF<- Base_RF %>%
    group_by(DEPTO) %>% 
    summarise(p_rf=sum(p_RF))
  
  mapa_p_RF<-merge(x =Base_RF,y=mapacol, by.x="DEPTO", by.y="DPTO_CCDGO",all.x=TRUE)

  ggplot(data=mapa_p_RF, aes(geometry = geometry, fill= p_rf))+
    geom_sf()+ 
    scale_fill_distiller(palette = "RdPu", na.value = "white", trans = "reverse" ) +
    labs(title = "RF")
  





