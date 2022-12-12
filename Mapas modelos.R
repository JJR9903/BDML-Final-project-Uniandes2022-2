#####MAPAS#####

    ##PACIFICO 
BaseP<- predictions_colegioss %>%
  filter (ESTRATO<=3)%>%
  filter(DEPTO==19|DEPTO==27|DEPTO==52 |DEPTO==76)

BaseP<- BaseP %>%
  group_by(DEPTO) %>% 
  summarise(entra=sum(UNIANDES.LEAD,na.rm=T))

Basep1<-merge(x =BaseP,y=mapacol, by.x="DEPTO", by.y="DPTO_CCDGO",all.x=TRUE)

ggplot(data=Basep1, aes(geometry = geometry, fill=entra))+
  geom_sf(color = "white")+ 
  scale_fill_distiller(palette = "RdPu", na.value = "white", trans = "reverse" ) +
  labs(title = "Posibilidad de ingreso", subtitle = "Región Pacifico")+ 
  theme(panel.border = element_rect(fill = "transparent"), plot.title = element_text(size=14, face="bold.italic"))+ 
  geom_sf_text(aes(label = entra), size = 3)


ggsave("Región Pacifico N.png")

##CARIBE 
BaseC<-predictions_colegioss %>%
  filter (ESTRATO<=3)%>%
  filter(DEPTO==8|DEPTO==13|DEPTO==20 |DEPTO==23| DEPTO==44| DEPTO==47 |DEPTO==70)

BaseC<- BaseC %>%
  group_by(DEPTO) %>% 
  summarise(entra=sum(UNIANDES.LEAD,na.rm=T))

Basec1<-merge(x =BaseC,y=mapacol, by.x="DEPTO", by.y="DPTO_CCDGO",all.x=TRUE)


ggplot(data=Basec1, aes(geometry = geometry, fill=entra))+
  geom_sf(color = "white")+ 
  scale_fill_distiller(palette = "RdPu", na.value = "white", trans = "reverse" ) +
  labs(title = "Estudiantes con posibilidad de ingreso ", subtitle = "Región Caribe") + 
  theme(panel.border = element_rect(fill = "transparent"), plot.title = element_text(size=14, face="bold.italic"))+ 
  geom_sf_text(aes(label = entra), size = 3)

ggsave("Región Caribe N.png")



      #### MAPAS predictions_colegioss ####

  ##PACIFICO 

   ## RF
Base_RF<- predictions_colegioss %>%
  filter (ESTRATO<=3)%>%
  filter(DEPTO==19|DEPTO==27|DEPTO==52 |DEPTO==76)

Base_RF<- Base_RF %>%
  group_by(DEPTO) %>% 
  summarise(p_rf=sum(p_RF))

mapa_p_RF<-merge(x =Base_RF,y=mapacol, by.x="DEPTO", by.y="DPTO_CCDGO",all.x=TRUE)

ggplot(data=mapa_p_RF, aes(geometry = geometry, fill=p_rf))+
  geom_sf(color="white")+ 
  scale_fill_distiller(palette = "RdPu", na.value = "white", trans = "reverse" ) +
  labs(title = "Estudiantes con posibilidad de ingreso", subtitle = "Región Pacifico, Random Forest") + 
  theme(panel.border = element_rect(fill = "transparent"), plot.title = element_text(size=14, face="bold.italic"))+ 
  geom_sf_text(aes(label = p_rf), size = 3)

ggsave("Región Pacifico,p_rf.png" )

##p_glmnet
Basep_glmnet<- predictions_colegioss %>%
  filter (ESTRATO<=3)%>%
  filter(DEPTO==19|DEPTO==27|DEPTO==52 |DEPTO==76)

Basep_glmnet<- Basep_glmnet %>%
  group_by(DEPTO) %>% 
  summarise(p_glmnet=sum(p_glmnet))

mapap_glmnet<-merge(x =Basep_glmnet,y=mapacol, by.x="DEPTO", by.y="DPTO_CCDGO",all.x=TRUE)

ggplot(data=mapap_glmnet, aes(geometry = geometry, fill=p_glmnet))+
  geom_sf(color="white")+ 
  scale_fill_distiller(palette = "RdPu", na.value = "white", trans = "reverse" ) +
  labs(title = "Estudiantes con posibilidad de ingreso", subtitle = "Región Pacifico, glmnet") + 
  theme(panel.border = element_rect(fill = "transparent"), plot.title = element_text(size=14, face="bold.italic"))+ 
  geom_sf_text(aes(label = p_glmnet), size = 3)

ggsave("Región Pacifico, glmne.png")

##p_RF_ranger
Base_RF_ran<- predictions_colegioss %>%
  filter (ESTRATO<=3)%>%
  filter(DEPTO==19|DEPTO==27|DEPTO==52 |DEPTO==76)

Base_RF_ran<- Base_RF_ran %>%
  group_by(DEPTO) %>% 
  summarise(p_rf_ranger=sum(p_RF_ranger))

mapa_RF_ran<-merge(x =Base_RF_ran,y=mapacol, by.x="DEPTO", by.y="DPTO_CCDGO",all.x=TRUE)

ggplot(data=mapa_RF_ran, aes(geometry = geometry, fill=p_rf_ranger))+
  geom_sf(color = "white")+ 
  scale_fill_distiller(palette = "RdPu", na.value = "white", trans = "reverse" ) +
  labs(title = "Estudiantes con posibilidad de ingreso", subtitle = "Región Pacifico, Random Forest Ranger") + 
  theme(panel.border = element_rect(fill = "transparent"), plot.title = element_text(size=14, face="bold.italic"))+ 
  geom_sf_text(aes(label = p_rf_ranger), size = 3)

ggsave("Región Pacifico, Random Forest Ranger.png" )

##p_xgb

Basep_xgb<- predictions_colegioss %>%
  filter (ESTRATO<=3)%>%
  filter(DEPTO==19|DEPTO==27|DEPTO==52 |DEPTO==76)

Basep_xgb<- Basep_xgb %>%
  group_by(DEPTO) %>% 
  summarise(p_xgb=sum(p_xgb))

mapap_xgb<-merge(x =Basep_xgb,y=mapacol, by.x="DEPTO", by.y="DPTO_CCDGO",all.x=TRUE)

ggplot(data=mapap_xgb, aes(geometry = geometry, fill=p_xgb))+
  geom_sf(color = "white")+ 
  scale_fill_distiller(palette = "RdPu", na.value = "white", trans = "reverse" ) +
  labs(title = "Estudiantes con posibilidad de ingreso", subtitle = "Región Pacifico, XGB") + 
  theme(panel.border = element_rect(fill = "transparent"), plot.title = element_text(size=14, face="bold.italic"))+ 
  geom_sf_text(aes(label =p_xgb), size = 3)

ggsave("Región Pacifico, XGB.png" )

##p_N_NET

Basep_N_NET<- predictions_colegioss %>%
  filter (ESTRATO<=3)%>%
  filter(DEPTO==19|DEPTO==27|DEPTO==52 |DEPTO==76)

Basep_N_NET<- Basep_N_NET %>%
  group_by(DEPTO) %>% 
  summarise(p_N_NET=sum(p_N_NET))

mapap_N_NET<-merge(x =Basep_N_NET,y=mapacol, by.x="DEPTO", by.y="DPTO_CCDGO",all.x=TRUE)

ggplot(data=mapap_N_NET, aes(geometry = geometry, fill=p_N_NET))+
  geom_sf(color = "white")+ 
  scale_fill_distiller(palette = "RdPu", na.value = "white", trans = "reverse" ) +
  labs(title = "Estudiantes con posibilidad de ingreso", subtitle = "Región Pacifico, Random Forest Ranger") + 
  theme(panel.border = element_rect(fill = "transparent"), plot.title = element_text(size=14, face="bold.italic"))+ 
  geom_sf_text(aes(label =p_N_NET), size = 3)

ggsave("Región Pacifico, p_N_NET.png" )

##p_avNNet

Basep_avNNet<- predictions_colegioss %>%
  filter (ESTRATO<=3)%>%
  filter(DEPTO==19|DEPTO==27|DEPTO==52 |DEPTO==76)

Basep_avNNet<- Basep_avNNet %>%
  group_by(DEPTO) %>% 
  summarise(p_avNNet=sum(p_avNNet))

mapap_avNNet<-merge(x =Basep_avNNet,y=mapacol, by.x="DEPTO", by.y="DPTO_CCDGO",all.x=TRUE)

ggplot(data=mapap_avNNet, aes(geometry = geometry, fill=p_avNNet))+
  geom_sf(color = "white")+ 
  scale_fill_distiller(palette = "RdPu", na.value = "white", trans = "reverse" ) +
  labs(title = "Estudiantes con posibilidad de ingreso", subtitle = "Región Pacifico, av NNET") + 
  theme(panel.border = element_rect(fill = "transparent"), plot.title = element_text(size=14, face="bold.italic"))+ 
  geom_sf_text(aes(label =p_avNNet), size = 3)

ggsave("Región Pacifico, p_avNNet.png" )


####CARIBE ####

   ##RF 
Base_RF<- predictions_colegioss %>%
  filter (ESTRATO<=3)%>%
  filter(DEPTO==8|DEPTO==13|DEPTO==20 |DEPTO==23| DEPTO==44| DEPTO==47 |DEPTO==70)

Base_RF<- Base_RF %>%
  group_by(DEPTO) %>% 
  summarise(p_rf=sum(p_RF))

mapa_p_RF<-merge(x =Base_RF,y=mapacol, by.x="DEPTO", by.y="DPTO_CCDGO",all.x=TRUE)

ggplot(data=mapa_p_RF, aes(geometry = geometry, fill=p_rf))+
  geom_sf(color="white")+ 
  scale_fill_distiller(palette = "RdPu", na.value = "white", trans = "reverse" ) +
  labs(title = "Estudiantes con posibilidad de ingreso", subtitle = "Región Caribe, Random Forest") + 
  theme(panel.border = element_rect(fill = "transparent"), plot.title = element_text(size=14, face="bold.italic"))+ 
  geom_sf_text(aes(label = p_rf), size = 3)
ggsave("Región Caribe, p_rf.png" )

##p_glmnet
Basep_glmnet<- predictions_colegioss %>%
  filter (ESTRATO<=3)%>%
  filter(DEPTO==8|DEPTO==13|DEPTO==20 |DEPTO==23| DEPTO==44| DEPTO==47 |DEPTO==70)

Basep_glmnet<- Basep_glmnet %>%
  group_by(DEPTO) %>% 
  summarise(p_glmnet=sum(p_glmnet))

mapap_glmnet<-merge(x =Basep_glmnet,y=mapacol, by.x="DEPTO", by.y="DPTO_CCDGO",all.x=TRUE)

ggplot(data=mapap_glmnet, aes(geometry = geometry, fill=p_glmnet))+
  geom_sf(color="white")+ 
  scale_fill_distiller(palette = "RdPu", na.value = "white", trans = "reverse" ) +
  labs(title = "Estudiantes con posibilidad de ingreso", subtitle = "Región Caribe, glmnet") + 
  theme(panel.border = element_rect(fill = "transparent"), plot.title = element_text(size=14, face="bold.italic"))+ 
  geom_sf_text(aes(label = p_glmnet), size = 3)

ggsave("Región Caribe, glmne.png")

##p_RF_ranger
Base_RF_ran<- predictions_colegioss %>%
  filter (ESTRATO<=3)%>%
  filter(DEPTO==8|DEPTO==13|DEPTO==20 |DEPTO==23| DEPTO==44| DEPTO==47 |DEPTO==70)

Base_RF_ran<- Base_RF_ran %>%
  group_by(DEPTO) %>% 
  summarise(p_rf_ranger=sum(p_RF_ranger))

mapa_RF_ran<-merge(x =Base_RF_ran,y=mapacol, by.x="DEPTO", by.y="DPTO_CCDGO",all.x=TRUE)

ggplot(data=mapa_RF_ran, aes(geometry = geometry, fill=p_rf_ranger))+
  geom_sf(color = "white")+ 
  scale_fill_distiller(palette = "RdPu", na.value = "white", trans = "reverse" ) +
  labs(title = "Estudiantes con posibilidad de ingreso", subtitle = "Región Caribe, Random Forest Ranger") + 
  theme(panel.border = element_rect(fill = "transparent"), plot.title = element_text(size=14, face="bold.italic"))+ 
  geom_sf_text(aes(label = p_rf_ranger), size = 3)

ggsave("Región Caribe, Random Forest Ranger.png" )

##p_xgb

Basep_xgb<- predictions_colegioss %>%
  filter (ESTRATO<=3)%>%
  filter(DEPTO==8|DEPTO==13|DEPTO==20 |DEPTO==23| DEPTO==44| DEPTO==47 |DEPTO==70)

Basep_xgb<- Basep_xgb %>%
  group_by(DEPTO) %>% 
  summarise(p_xgb=sum(p_xgb))

mapap_xgb<-merge(x =Basep_xgb,y=mapacol, by.x="DEPTO", by.y="DPTO_CCDGO",all.x=TRUE)

ggplot(data=mapap_xgb, aes(geometry = geometry, fill=p_xgb))+
  geom_sf(color = "white")+ 
  scale_fill_distiller(palette = "RdPu", na.value = "white", trans = "reverse" ) +
  labs(title = "Estudiantes con posibilidad de ingreso", subtitle = "Región Caribe, XGB") + 
  theme(panel.border = element_rect(fill = "transparent"), plot.title = element_text(size=14, face="bold.italic"))+ 
  geom_sf_text(aes(label =p_xgb), size = 3)

ggsave("Región Caribe, XGB.png" )

##p_N_NET

Basep_N_NET<- predictions_colegioss %>%
  filter (ESTRATO<=3)%>%
  filter(DEPTO==8|DEPTO==13|DEPTO==20 |DEPTO==23| DEPTO==44| DEPTO==47 |DEPTO==70)

Basep_N_NET<- Basep_N_NET %>%
  group_by(DEPTO) %>% 
  summarise(p_N_NET=sum(p_N_NET))

mapap_N_NET<-merge(x =Basep_N_NET,y=mapacol, by.x="DEPTO", by.y="DPTO_CCDGO",all.x=TRUE)

ggplot(data=mapap_N_NET, aes(geometry = geometry, fill=p_N_NET))+
  geom_sf(color = "white")+ 
  scale_fill_distiller(palette = "RdPu", na.value = "white", trans = "reverse" ) +
  labs(title = "Estudiantes con posibilidad de ingreso", subtitle = "Región Caribe, p_N_NET") + 
  theme(panel.border = element_rect(fill = "transparent"), plot.title = element_text(size=14, face="bold.italic"))+ 
  geom_sf_text(aes(label =p_N_NET), size = 3)

ggsave("Región Caribe, p_N_NET.png" )

##p_avNNet

Basep_avNNet<- predictions_colegioss %>%
  filter (ESTRATO<=3)%>%
  filter(DEPTO==8|DEPTO==13|DEPTO==20 |DEPTO==23| DEPTO==44| DEPTO==47 |DEPTO==70)

Basep_avNNet<- Basep_avNNet %>%
  group_by(DEPTO) %>% 
  summarise(p_avNNet=sum(p_avNNet))

mapap_avNNet<-merge(x =Basep_avNNet,y=mapacol, by.x="DEPTO", by.y="DPTO_CCDGO",all.x=TRUE)

ggplot(data=mapap_avNNet, aes(geometry = geometry, fill=p_avNNet))+
  geom_sf(color = "white")+ 
  scale_fill_distiller(palette = "RdPu", na.value = "white", trans = "reverse" ) +
  labs(title = "Estudiantes con posibilidad de ingreso", subtitle = "Región Caribe, av NNET") + 
  theme(panel.border = element_rect(fill = "transparent"), plot.title = element_text(size=14, face="bold.italic"))+ 
  geom_sf_text(aes(label =p_avNNet), size = 3)

ggsave("Región Caribe, p_avNNet.png" )
names (predictions_colegioss)




