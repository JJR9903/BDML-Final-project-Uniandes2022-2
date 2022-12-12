
rm(list = ls())

#Escoger directorio
dir_set <- function(){
  if(Sys.info()["user"]=="JuanJose"){
    setwd("/Users/JuanJose/Library/CloudStorage/OneDrive-UniversidaddelosAndes/Uniandes/9 Semestre - 1 PEG/Big Data/Proyecto Final")
  }
  else {
    setwd("")
  }
}

dir_set()

##
pacman::p_load(haven,tidyverse,data.table,DescTools,readxl,lubridate,caret,moments,bnstruct)
pacman::p_load(rio,glue,hexbin,patchwork,vip,ggrepel,stringi,tidytext,logr,fastDummies)



# read files  -------------------------------------------------------------

L=list.files(path= "icfes/Resultados Saber11",all.files=F, full.names=T)
L=L[order(L)]
L=L[-seq(1,15)]

#2014 - 2
i=L[1]
p2014_2<-read.delim(file = i, sep = ";")
setnames(p2014_2, old = names(p2014_2),  new =  toupper(names(p2014_2)) )



p2014_2<-p2014_2%>%
  mutate(ID=ESTU_CONSECUTIVO,
         HOMBRE=ifelse(ESTU_GENERO=="M",1,0),
         MUJER=ifelse(ESTU_GENERO=="F",1,0),
         ESTU_FECHANACIMIENTO=ifelse(as.numeric(round(difftime(as.Date("01/09/2014","%d/%m/%Y"),as.Date(ESTU_FECHANACIMIENTO,"%d/%m/%Y"), units = "days")/365.25))>100,NA,ESTU_FECHANACIMIENTO),
         ESTU_FECHANACIMIENTO=ifelse(as.numeric(round(difftime(as.Date("01/09/2014","%d/%m/%Y"),as.Date(ESTU_FECHANACIMIENTO,"%d/%m/%Y"), units = "days")/365.25))<=10,NA,ESTU_FECHANACIMIENTO),
         FECHA_NACIMIENTO=as.Date(ESTU_FECHANACIMIENTO,"%d/%m/%Y"),
         MES_NACIMIENTO = Month(FECHA_NACIMIENTO),
         ANO_NACIMIENTO = year(FECHA_NACIMIENTO),
         EDAD =as.numeric(round(difftime(as.Date("01/09/2014","%d/%m/%Y"),as.Date(ESTU_FECHANACIMIENTO,"%d/%m/%Y"), units = "days")/365.25)),
         PERIODO=as.numeric(PERIODO),
         DEPTO=as.numeric(ESTU_COD_RESIDE_DEPTO),
         DEPTO= ifelse(is.na(DEPTO),as.numeric(COLE_COD_DEPTO_UBICACION),DEPTO),
         MUNICIPIO=as.numeric(ESTU_COD_RESIDE_MCPIO),
         MUNICIPIO= ifelse(is.na(MUNICIPIO),as.numeric(COLE_COD_MCPIO_UBICACION),MUNICIPIO),
         EDUCACION_PADRE=case_when(FAMI_EDUCACIONPADRE==""~0,FAMI_EDUCACIONPADRE=="No Aplica"~0,FAMI_EDUCACIONPADRE=="Ninguno"~0,FAMI_EDUCACIONPADRE=="No sabe"~0,FAMI_EDUCACIONPADRE=="Primaria incompleta"~1,
                                   FAMI_EDUCACIONPADRE=="Primaria completa"~2,FAMI_EDUCACIONPADRE=="Secundaria (Bachillerato) incompleta"~3,FAMI_EDUCACIONPADRE=="Secundaria (Bachillerato) completa"~4,
                                   FAMI_EDUCACIONPADRE=="Técnica o tecnológica incompleta"~5,FAMI_EDUCACIONPADRE=="Educación profesional incompleta"~5,FAMI_EDUCACIONPADRE=="Técnica o tecnológica completa"~6,
                                   FAMI_EDUCACIONPADRE=="Educación profesional completa"~6,FAMI_EDUCACIONPADRE=="Postgrado"~7),
         
         EDUCACION_MADRE=case_when(FAMI_EDUCACIONMADRE==""~0,FAMI_EDUCACIONMADRE=="No Aplica"~0,FAMI_EDUCACIONMADRE=="Ninguno"~0,FAMI_EDUCACIONMADRE=="No sabe"~0,FAMI_EDUCACIONMADRE=="Primaria incompleta"~1,
                                   FAMI_EDUCACIONMADRE=="Primaria completa"~2,FAMI_EDUCACIONMADRE=="Secundaria (Bachillerato) incompleta"~3,FAMI_EDUCACIONMADRE=="Secundaria (Bachillerato) completa"~4,
                                   FAMI_EDUCACIONMADRE=="Técnica o tecnológica incompleta"~5,FAMI_EDUCACIONMADRE=="Educación profesional incompleta"~5,FAMI_EDUCACIONMADRE=="Técnica o tecnológica completa"~6,
                                   FAMI_EDUCACIONMADRE=="Educación profesional completa"~6,FAMI_EDUCACIONMADRE=="Postgrado"~7),
         ESTRATO=as.numeric(str_extract(FAMI_ESTRATOVIVIENDA,"[0-9]")),
         ESTRATO = ifelse(is.na(ESTRATO),0,ESTRATO),
         PERSONAS_HOGAR=case_when(FAMI_PERSONASHOGAR=="Una"~1,FAMI_PERSONASHOGAR=="Dos"~2,FAMI_PERSONASHOGAR=="Tres"~3,FAMI_PERSONASHOGAR=="Cuatro"~4,FAMI_PERSONASHOGAR=="Cinco"~5,FAMI_PERSONASHOGAR=="Seis"~6,
                                  FAMI_PERSONASHOGAR=="Siete"~7,FAMI_PERSONASHOGAR=="Ocho"~8,FAMI_PERSONASHOGAR=="Nueve"~9,FAMI_PERSONASHOGAR=="Diez"~10,FAMI_PERSONASHOGAR=="Once"~11,FAMI_PERSONASHOGAR=="Doce o más"~12),
         
         CUARTOS_HOGAR = case_when(FAMI_CUARTOSHOGAR=="Uno"~1,FAMI_CUARTOSHOGAR=="Dos"~2,FAMI_CUARTOSHOGAR=="Tres"~3,FAMI_CUARTOSHOGAR=="Cuatro"~4,FAMI_CUARTOSHOGAR=="Cinco"~5,FAMI_CUARTOSHOGAR=="Seis o mas"~6,FAMI_CUARTOSHOGAR=="Seis"~6,
                                   FAMI_CUARTOSHOGAR=="Siete"~6,FAMI_CUARTOSHOGAR=="Ocho"~6,FAMI_CUARTOSHOGAR=="Nueve"~6,FAMI_CUARTOSHOGAR=="Diez o más"~6),
         INTERNET = ifelse(FAMI_TIENEINTERNET=="Si",1,0),
         TV = ifelse(FAMI_TIENESERVICIOTV=="Si",1,0),
         COMPUTADOR = ifelse(FAMI_TIENECOMPUTADOR=="Si",1,0),
         TRANSPORTE = ifelse(FAMI_TIENEAUTOMOVIL=="Si",1,0),
         HORNO = ifelse(FAMI_TIENEMICROONDAS=="Si" | FAMI_TIENEHORNO=="Si",1,0),
         LAVADORA = ifelse(FAMI_TIENELAVADORA=="Si",1,0),
         
         ESTUDIANTE_TRABAJA=ifelse(ESTU_TRABAJAACTUALMENTE=="No" |ESTU_TRABAJAACTUALMENTE=="" ,0,1),
         
         COLEGIO = as.numeric(COLE_CODIGO_ICFES),
         COLE_FEMENINO = ifelse(COLE_GENERO=="FEMENINO",1,0),
         COLE_MASCULINO = ifelse(COLE_GENERO=="MASCULINO",1,0),
         COLE_MIXTO = ifelse(COLE_GENERO=="MIXTO",1,0),
         CALENDARIO_A = ifelse(COLE_CALENDARIO=="A",1,0),
         CALENDARIO_B = ifelse(COLE_CALENDARIO=="B",1,0),
         COLE_OFICIAL= ifelse(COLE_NATURALEZA=="OFICIAL",1,0),
         COLE_BILINGUE = ifelse(COLE_BILINGUE=="S",1,0),
         COLE_ACADEMICO= ifelse(COLE_CARACTER=="ACADÉMICO" | COLE_CARACTER=="TÉCNICO/ACADÉMICO",1,0),
         COLE_TECNICO= ifelse(COLE_CARACTER=="TÉCNICO" | COLE_CARACTER=="TÉCNICO/ACADÉMICO",1,0),
         JORNADA_COMPLETA=ifelse(COLE_JORNADA=="COMPLETA" |COLE_JORNADA=="UNICA",1,0),
         JORNADA_MANANA=ifelse(COLE_JORNADA=="MAÑANA",1,0),
         JORNADA_NOCHE=ifelse(COLE_JORNADA=="NOCHE",1,0),
         JORNADA_TARDE=ifelse(COLE_JORNADA=="TARDE",1,0),

         PUNT_GLOBAL=as.numeric(PUNT_GLOBAL)
  )%>%
  select(c(ID,HOMBRE,MUJER,FECHA_NACIMIENTO,MES_NACIMIENTO,ANO_NACIMIENTO,EDAD,PERIODO,DEPTO,MUNICIPIO,EDUCACION_PADRE,EDUCACION_MADRE,ESTRATO,PERSONAS_HOGAR,CUARTOS_HOGAR,INTERNET,TV,COMPUTADOR,TRANSPORTE,HORNO,LAVADORA,ESTUDIANTE_TRABAJA,COLEGIO,COLE_FEMENINO,COLE_MASCULINO,COLE_MIXTO,CALENDARIO_A,CALENDARIO_B,COLE_BILINGUE,COLE_OFICIAL,COLE_ACADEMICO,COLE_TECNICO,JORNADA_COMPLETA,JORNADA_MANANA,JORNADA_NOCHE,JORNADA_TARDE,PUNT_GLOBAL))

#p2014_2<-as.data.frame(knn.impute( as.matrix(p2014_2), k = 10))

p2014_2<-p2014_2%>%
  dummy_cols(select_columns = c("EDUCACION_PADRE","EDUCACION_MADRE"),remove_first_dummy = TRUE, ignore_na = TRUE, remove_selected_columns = TRUE)%>%
  rename(EDUCACION_PADRE_PRIMARIA_INCOMPLETA=EDUCACION_PADRE_1,EDUCACION_PADRE_PRIMARIA_COMPLETA=EDUCACION_PADRE_2,
         EDUCACION_PADRE_SECUNDARIA_INCOMPLETA=EDUCACION_PADRE_3,EDUCACION_PADRE_SECUNDARIA_COMPLETA=EDUCACION_PADRE_4,
         EDUCACION_PADRE_SUPERIOR_INCOMPLETA=EDUCACION_PADRE_5,EDUCACION_PADRE_SUPERIOR_COMPLETA=EDUCACION_PADRE_6,EDUCACION_PADRE_POSTGRADO=EDUCACION_PADRE_7,
         EDUCACION_MADRE_PRIMARIA_INCOMPLETA=EDUCACION_MADRE_1,EDUCACION_MADRE_PRIMARIA_COMPLETA=EDUCACION_MADRE_2,
         EDUCACION_MADRE_SECUNDARIA_INCOMPLETA=EDUCACION_MADRE_3,EDUCACION_MADRE_SECUNDARIA_COMPLETA=EDUCACION_MADRE_4,
         EDUCACION_MADRE_SUPERIOR_INCOMPLETA=EDUCACION_MADRE_5,EDUCACION_MADRE_SUPERIOR_COMPLETA=EDUCACION_MADRE_6,EDUCACION_MADRE_POSTGRADO=EDUCACION_MADRE_7)




#2015 - 1
i=L[2]
p2015_1<-read.csv(file = i)
setnames(p2015_1, old = names(p2015_1),  new =  toupper(names(p2015_1)) )

p2015_1<-p2015_1%>%
  mutate(ID=ESTU_CONSECUTIVO,
         HOMBRE=ifelse(ESTU_GENERO=="M",1,0),
         MUJER=ifelse(ESTU_GENERO=="F",1,0),
         ESTU_FECHANACIMIENTO=ifelse(as.numeric(round(difftime(as.Date("01/03/2015","%d/%m/%Y"),as.Date(ESTU_FECHANACIMIENTO,"%d/%m/%Y"), units = "days")/365.25))>100,NA,ESTU_FECHANACIMIENTO),
         ESTU_FECHANACIMIENTO=ifelse(as.numeric(round(difftime(as.Date("01/03/2015","%d/%m/%Y"),as.Date(ESTU_FECHANACIMIENTO,"%d/%m/%Y"), units = "days")/365.25))<=10,NA,ESTU_FECHANACIMIENTO),
         FECHA_NACIMIENTO=as.Date(ESTU_FECHANACIMIENTO,"%d/%m/%Y"),
         EDAD =as.numeric(round(difftime(as.Date("01/03/2015","%d/%m/%Y"),as.Date(ESTU_FECHANACIMIENTO,"%d/%m/%Y"), units = "days")/365.25)),
         MES_NACIMIENTO = Month(FECHA_NACIMIENTO),
         ANO_NACIMIENTO = year(FECHA_NACIMIENTO),
         PERIODO=as.numeric(PERIODO),
         DEPTO=as.numeric(ESTU_COD_RESIDE_DEPTO),
         MUNICIPIO=as.numeric(ESTU_COD_RESIDE_MCPIO),
         EDUCACION_PADRE=case_when(FAMI_EDUCACIONPADRE==""~0,FAMI_EDUCACIONPADRE=="No Aplica"~0,FAMI_EDUCACIONPADRE=="Ninguno"~0,FAMI_EDUCACIONPADRE=="No sabe"~0,FAMI_EDUCACIONPADRE=="Primaria incompleta"~1,
                                   FAMI_EDUCACIONPADRE=="Primaria completa"~2,FAMI_EDUCACIONPADRE=="Secundaria (Bachillerato) incompleta"~3,FAMI_EDUCACIONPADRE=="Secundaria (Bachillerato) completa"~4,
                                   FAMI_EDUCACIONPADRE=="Técnica o tecnológica incompleta"~5,FAMI_EDUCACIONPADRE=="Educación profesional incompleta"~5,FAMI_EDUCACIONPADRE=="Técnica o tecnológicacompleta"~6,
                                   FAMI_EDUCACIONPADRE=="Educación profesional completa"~6,FAMI_EDUCACIONPADRE=="Postgrado"~7),
         
         EDUCACION_MADRE=case_when(FAMI_EDUCACIONMADRE==""~0,FAMI_EDUCACIONMADRE=="No Aplica"~0,FAMI_EDUCACIONMADRE=="Ninguno"~0,FAMI_EDUCACIONMADRE=="No sabe"~0,FAMI_EDUCACIONMADRE=="Primaria incompleta"~1,
                                   FAMI_EDUCACIONMADRE=="Primaria completa"~2,FAMI_EDUCACIONMADRE=="Secundaria (Bachillerato) incompleta"~3,FAMI_EDUCACIONMADRE=="Secundaria (Bachillerato) completa"~4,
                                   FAMI_EDUCACIONMADRE=="Técnica o tecnológica incompleta"~5,FAMI_EDUCACIONMADRE=="Educación profesional incompleta"~5,FAMI_EDUCACIONMADRE=="Técnica o tecnológica completa"~6,
                                   FAMI_EDUCACIONMADRE=="Educación profesional completa"~6,FAMI_EDUCACIONMADRE=="Postgrado"~7),
         ESTRATO=as.numeric(str_extract(FAMI_ESTRATOVIVIENDA,"[0-9]")),
         ESTRATO = ifelse(is.na(ESTRATO),0,ESTRATO),
         PERSONAS_HOGAR=case_when(FAMI_PERSONASHOGAR=="Una"~1,FAMI_PERSONASHOGAR=="Dos"~2,FAMI_PERSONASHOGAR=="Tres"~3,FAMI_PERSONASHOGAR=="Cuatro"~4,FAMI_PERSONASHOGAR=="Cinco"~5,FAMI_PERSONASHOGAR=="Seis"~6,
                                  FAMI_PERSONASHOGAR=="Siete"~7,FAMI_PERSONASHOGAR=="Ocho"~8,FAMI_PERSONASHOGAR=="Nueve"~9,FAMI_PERSONASHOGAR=="Diez"~10,FAMI_PERSONASHOGAR=="Once"~11,FAMI_PERSONASHOGAR=="Doce o más"~12),
         
         CUARTOS_HOGAR = case_when(FAMI_CUARTOSHOGAR=="Uno"~1,FAMI_CUARTOSHOGAR=="Dos"~2,FAMI_CUARTOSHOGAR=="Tres"~3,FAMI_CUARTOSHOGAR=="Cuatro"~4,FAMI_CUARTOSHOGAR=="Cinco"~5,FAMI_CUARTOSHOGAR=="Seis o mas"~6,FAMI_CUARTOSHOGAR=="Seis"~6,
                                   FAMI_CUARTOSHOGAR=="Siete"~6,FAMI_CUARTOSHOGAR=="Ocho"~6,FAMI_CUARTOSHOGAR=="Nueve"~6,FAMI_CUARTOSHOGAR=="Diez o más"~6),
         INTERNET = ifelse(FAMI_TIENEINTERNET=="Si",1,0),
         TV = ifelse(FAMI_TIENETELEVISOR=="Si",1,0),
         COMPUTADOR = ifelse(FAMI_TIENECOMPUTADOR=="Si",1,0),
         TRANSPORTE = ifelse(FAMI_TIENEAUTOMOVIL=="Si",1,0),
         HORNO = ifelse(FAMI_TIENEMICROONDAS=="Si" | FAMI_TIENEHORNO=="Si",1,0),
         LAVADORA = ifelse(FAMI_TIENELAVADORA=="Si",1,0),
         ESTUDIANTE_TRABAJA=ifelse(ESTU_TRABAJAACTUALMENTE=="No" |ESTU_TRABAJAACTUALMENTE=="" ,0,1),
         
         COLEGIO = as.numeric(COLE_CODIGO_ICFES),
         COLE_FEMENINO = ifelse(COLE_GENERO=="FEMENINO",1,0),
         COLE_MASCULINO = ifelse(COLE_GENERO=="MASCULINO",1,0),
         COLE_MIXTO = ifelse(COLE_GENERO=="MIXTO",1,0),
         CALENDARIO_A = ifelse(COLE_CALENDARIO=="A",1,0),
         CALENDARIO_B = ifelse(COLE_CALENDARIO=="B",1,0),
         COLE_OFICIAL= ifelse(COLE_NATURALEZA=="OFICIAL",1,0),
         COLE_BILINGUE = ifelse(COLE_BILINGUE=="S",1,0),
         COLE_BILINGUE = ifelse(is.na(COLE_BILINGUE),0,COLE_BILINGUE),
         COLE_ACADEMICO= ifelse(COLE_CARACTER=="ACADÉMICO" | COLE_CARACTER=="TÉCNICO/ACADÉMICO",1,0),
         COLE_TECNICO= ifelse(COLE_CARACTER=="TÉCNICO" | COLE_CARACTER=="TÉCNICO/ACADÉMICO",1,0),
         JORNADA_COMPLETA=ifelse(COLE_JORNADA=="COMPLETA" |COLE_JORNADA=="UNICA",1,0),
         JORNADA_MANANA=ifelse(COLE_JORNADA=="MAÑANA",1,0),
         JORNADA_NOCHE=ifelse(COLE_JORNADA=="NOCHE",1,0),
         JORNADA_TARDE=ifelse(COLE_JORNADA=="TARDE",1,0),
         
         PUNT_GLOBAL=as.numeric(PUNT_GLOBAL)
  )%>%
  select(c(ID,HOMBRE,MUJER,FECHA_NACIMIENTO,MES_NACIMIENTO,ANO_NACIMIENTO,EDAD,PERIODO,DEPTO,MUNICIPIO,EDUCACION_PADRE,EDUCACION_MADRE,ESTRATO,PERSONAS_HOGAR,CUARTOS_HOGAR,INTERNET,TV,COMPUTADOR,TRANSPORTE,HORNO,LAVADORA,ESTUDIANTE_TRABAJA,COLEGIO,COLE_FEMENINO,COLE_MASCULINO,COLE_MIXTO,CALENDARIO_A,CALENDARIO_B,COLE_BILINGUE,COLE_OFICIAL,COLE_ACADEMICO,COLE_TECNICO,JORNADA_COMPLETA,JORNADA_MANANA,JORNADA_NOCHE,JORNADA_TARDE,PUNT_GLOBAL))

#p2015_1<-as.data.frame(knn.impute( as.matrix(p2015_1), k = 10))

p2015_1<-p2015_1%>%
  dummy_cols(select_columns = c("EDUCACION_PADRE","EDUCACION_MADRE"),remove_first_dummy = TRUE, ignore_na = TRUE, remove_selected_columns = TRUE)%>%
  rename(EDUCACION_PADRE_PRIMARIA_INCOMPLETA=EDUCACION_PADRE_1,EDUCACION_PADRE_PRIMARIA_COMPLETA=EDUCACION_PADRE_2,
         EDUCACION_PADRE_SECUNDARIA_INCOMPLETA=EDUCACION_PADRE_3,EDUCACION_PADRE_SECUNDARIA_COMPLETA=EDUCACION_PADRE_4,
         EDUCACION_PADRE_SUPERIOR_INCOMPLETA=EDUCACION_PADRE_5,EDUCACION_PADRE_SUPERIOR_COMPLETA=EDUCACION_PADRE_6,EDUCACION_PADRE_POSTGRADO=EDUCACION_PADRE_7,
         EDUCACION_MADRE_PRIMARIA_INCOMPLETA=EDUCACION_MADRE_1,EDUCACION_MADRE_PRIMARIA_COMPLETA=EDUCACION_MADRE_2,
         EDUCACION_MADRE_SECUNDARIA_INCOMPLETA=EDUCACION_MADRE_3,EDUCACION_MADRE_SECUNDARIA_COMPLETA=EDUCACION_MADRE_4,
         EDUCACION_MADRE_SUPERIOR_INCOMPLETA=EDUCACION_MADRE_5,EDUCACION_MADRE_SUPERIOR_COMPLETA=EDUCACION_MADRE_6,EDUCACION_MADRE_POSTGRADO=EDUCACION_MADRE_7)



#2015 - 2
i=L[3]
p2015_2<-read.delim(file = i, sep = ";")
setnames(p2015_2, old = names(p2015_2),  new =  toupper(names(p2015_2)) )

p2015_2<-p2015_2%>%
  mutate(ID=ESTU_CONSECUTIVO,
         HOMBRE=ifelse(ESTU_GENERO=="M",1,0),
         MUJER=ifelse(ESTU_GENERO=="F",1,0),
         ESTU_FECHANACIMIENTO=gsub("2094","1994",as.character(ESTU_FECHANACIMIENTO)),
         ESTU_FECHANACIMIENTO=gsub("2095","1995",as.character(ESTU_FECHANACIMIENTO)),
         ESTU_FECHANACIMIENTO=gsub("2096","1996",as.character(ESTU_FECHANACIMIENTO)),
         ESTU_FECHANACIMIENTO=gsub("2097","1997",as.character(ESTU_FECHANACIMIENTO)),
         ESTU_FECHANACIMIENTO=gsub("2098","1998",as.character(ESTU_FECHANACIMIENTO)),
         ESTU_FECHANACIMIENTO=gsub("2099","1999",as.character(ESTU_FECHANACIMIENTO)),
         ESTU_FECHANACIMIENTO=ifelse(as.numeric(round(difftime(as.Date("01/09/2015","%d/%m/%Y"),as.Date(ESTU_FECHANACIMIENTO,"%d/%m/%Y"), units = "days")/365.25))>100,NA,ESTU_FECHANACIMIENTO),
         ESTU_FECHANACIMIENTO=ifelse(as.numeric(round(difftime(as.Date("01/09/2015","%d/%m/%Y"),as.Date(ESTU_FECHANACIMIENTO,"%d/%m/%Y"), units = "days")/365.25))<=10,NA,ESTU_FECHANACIMIENTO),
         FECHA_NACIMIENTO=as.Date(ESTU_FECHANACIMIENTO,"%d/%m/%Y"),
         EDAD =as.numeric(round(difftime(as.Date("01/09/2015","%d/%m/%Y"),as.Date(ESTU_FECHANACIMIENTO,"%d/%m/%Y"), units = "days")/365.25)),
         MES_NACIMIENTO = Month(FECHA_NACIMIENTO),
         ANO_NACIMIENTO = year(FECHA_NACIMIENTO),
         PERIODO=as.numeric(PERIODO),
         DEPTO=as.numeric(ESTU_COD_RESIDE_DEPTO),
         MUNICIPIO=as.numeric(ESTU_COD_RESIDE_MCPIO),
         EDUCACION_PADRE=case_when(FAMI_EDUCACIONPADRE==""~0,FAMI_EDUCACIONPADRE=="No Aplica"~0,FAMI_EDUCACIONPADRE=="Ninguno"~0,FAMI_EDUCACIONPADRE=="No sabe"~0,FAMI_EDUCACIONPADRE=="Primaria incompleta"~1,
                                   FAMI_EDUCACIONPADRE=="Primaria completa"~2,FAMI_EDUCACIONPADRE=="Secundaria (Bachillerato) incompleta"~3,FAMI_EDUCACIONPADRE=="Secundaria (Bachillerato) completa"~4,
                                   FAMI_EDUCACIONPADRE=="Técnica o tecnológica incompleta"~5,FAMI_EDUCACIONPADRE=="Educación profesional incompleta"~5,FAMI_EDUCACIONPADRE=="Técnica o tecnológica completa"~6,
                                   FAMI_EDUCACIONPADRE=="Educación profesional completa"~6,FAMI_EDUCACIONPADRE=="Postgrado"~7),
         
         EDUCACION_MADRE=case_when(FAMI_EDUCACIONMADRE==""~0,FAMI_EDUCACIONMADRE=="No Aplica"~0,FAMI_EDUCACIONMADRE=="Ninguno"~0,FAMI_EDUCACIONMADRE=="No sabe"~0,FAMI_EDUCACIONMADRE=="Primaria incompleta"~1,
                                   FAMI_EDUCACIONMADRE=="Primaria completa"~2,FAMI_EDUCACIONMADRE=="Secundaria (Bachillerato) incompleta"~3,FAMI_EDUCACIONMADRE=="Secundaria (Bachillerato) completa"~4,
                                   FAMI_EDUCACIONMADRE=="Técnica o tecnológica incompleta"~5,FAMI_EDUCACIONMADRE=="Educación profesional incompleta"~5,FAMI_EDUCACIONMADRE=="Técnica o tecnológica completa"~6,
                                   FAMI_EDUCACIONMADRE=="Educación profesional completa"~6,FAMI_EDUCACIONMADRE=="Postgrado"~7),
         ESTRATO=as.numeric(str_extract(FAMI_ESTRATOVIVIENDA,"[0-9]")),
         ESTRATO = ifelse(is.na(ESTRATO),0,ESTRATO),
         PERSONAS_HOGAR=case_when(FAMI_PERSONASHOGAR=="Una"~1,FAMI_PERSONASHOGAR=="Dos"~2,FAMI_PERSONASHOGAR=="Tres"~3,FAMI_PERSONASHOGAR=="Cuatro"~4,FAMI_PERSONASHOGAR=="Cinco"~5,FAMI_PERSONASHOGAR=="Seis"~6,
                                  FAMI_PERSONASHOGAR=="Siete"~7,FAMI_PERSONASHOGAR=="Ocho"~8,FAMI_PERSONASHOGAR=="Nueve"~9,FAMI_PERSONASHOGAR=="Diez"~10,FAMI_PERSONASHOGAR=="Once"~11,FAMI_PERSONASHOGAR=="Doce o más"~12),
         
         CUARTOS_HOGAR = case_when(FAMI_CUARTOSHOGAR=="Uno"~1,FAMI_CUARTOSHOGAR=="Dos"~2,FAMI_CUARTOSHOGAR=="Tres"~3,FAMI_CUARTOSHOGAR=="Cuatro"~4,FAMI_CUARTOSHOGAR=="Cinco"~5,FAMI_CUARTOSHOGAR=="Seis o mas"~6,FAMI_CUARTOSHOGAR=="Seis"~6,
                                   FAMI_CUARTOSHOGAR=="Siete"~6,FAMI_CUARTOSHOGAR=="Ocho"~6,FAMI_CUARTOSHOGAR=="Nueve"~6,FAMI_CUARTOSHOGAR=="Diez o más"~6),
         INTERNET = ifelse(FAMI_TIENEINTERNET=="Si",1,0),
         TV = ifelse(FAMI_TIENESERVICIOTV=="Si",1,0),
         COMPUTADOR = ifelse(FAMI_TIENECOMPUTADOR=="Si",1,0),
         TRANSPORTE = ifelse(FAMI_TIENEAUTOMOVIL=="Si",1,0),
         HORNO = ifelse(FAMI_TIENEMICROONDAS=="Si" | FAMI_TIENEHORNO=="Si",1,0),
         LAVADORA = ifelse(FAMI_TIENELAVADORA=="Si",1,0),
         
         ESTUDIANTE_TRABAJA=ifelse(ESTU_TRABAJAACTUALMENTE=="No" |ESTU_TRABAJAACTUALMENTE=="" ,0,1),
         
         COLEGIO = as.numeric(COLE_CODIGO_ICFES),
         COLE_FEMENINO = ifelse(COLE_GENERO=="FEMENINO",1,0),
         COLE_MASCULINO = ifelse(COLE_GENERO=="MASCULINO",1,0),
         COLE_MIXTO = ifelse(COLE_GENERO=="MIXTO",1,0),
         CALENDARIO_A = ifelse(COLE_CALENDARIO=="A",1,0),
         CALENDARIO_B = ifelse(COLE_CALENDARIO=="B",1,0),
         COLE_OFICIAL= ifelse(COLE_NATURALEZA=="OFICIAL",1,0),
         COLE_BILINGUE = ifelse(COLE_BILINGUE=="S",1,0),
         COLE_ACADEMICO= ifelse(COLE_CARACTER=="ACADÉMICO" | COLE_CARACTER=="TÉCNICO/ACADÉMICO",1,0),
         COLE_TECNICO= ifelse(COLE_CARACTER=="TÉCNICO" | COLE_CARACTER=="TÉCNICO/ACADÉMICO",1,0),
         JORNADA_COMPLETA=ifelse(COLE_JORNADA=="COMPLETA" |COLE_JORNADA=="UNICA",1,0),
         JORNADA_MANANA=ifelse(COLE_JORNADA=="MAÑANA",1,0),
         JORNADA_NOCHE=ifelse(COLE_JORNADA=="NOCHE",1,0),
         JORNADA_TARDE=ifelse(COLE_JORNADA=="TARDE",1,0),
         
         PUNT_GLOBAL=as.numeric(PUNT_GLOBAL)
  )%>%
  select(c(ID,HOMBRE,MUJER,FECHA_NACIMIENTO,MES_NACIMIENTO,ANO_NACIMIENTO,EDAD,PERIODO,DEPTO,MUNICIPIO,EDUCACION_PADRE,EDUCACION_MADRE,ESTRATO,PERSONAS_HOGAR,CUARTOS_HOGAR,INTERNET,TV,COMPUTADOR,TRANSPORTE,HORNO,LAVADORA,ESTUDIANTE_TRABAJA,COLEGIO,COLE_FEMENINO,COLE_MASCULINO,COLE_MIXTO,CALENDARIO_A,CALENDARIO_B,COLE_BILINGUE,COLE_OFICIAL,COLE_ACADEMICO,COLE_TECNICO,JORNADA_COMPLETA,JORNADA_MANANA,JORNADA_NOCHE,JORNADA_TARDE,PUNT_GLOBAL))

#p2015_2<-as.data.frame(knn.impute( as.matrix(p2015_2), k = 10))

p2015_2<-p2015_2%>%
  dummy_cols(select_columns = c("EDUCACION_PADRE","EDUCACION_MADRE"),remove_first_dummy = TRUE, ignore_na = TRUE, remove_selected_columns = TRUE)%>%
  rename(EDUCACION_PADRE_PRIMARIA_INCOMPLETA=EDUCACION_PADRE_1,EDUCACION_PADRE_PRIMARIA_COMPLETA=EDUCACION_PADRE_2,
         EDUCACION_PADRE_SECUNDARIA_INCOMPLETA=EDUCACION_PADRE_3,EDUCACION_PADRE_SECUNDARIA_COMPLETA=EDUCACION_PADRE_4,
         EDUCACION_PADRE_SUPERIOR_INCOMPLETA=EDUCACION_PADRE_5,EDUCACION_PADRE_SUPERIOR_COMPLETA=EDUCACION_PADRE_6,EDUCACION_PADRE_POSTGRADO=EDUCACION_PADRE_7,
         EDUCACION_MADRE_PRIMARIA_INCOMPLETA=EDUCACION_MADRE_1,EDUCACION_MADRE_PRIMARIA_COMPLETA=EDUCACION_MADRE_2,
         EDUCACION_MADRE_SECUNDARIA_INCOMPLETA=EDUCACION_MADRE_3,EDUCACION_MADRE_SECUNDARIA_COMPLETA=EDUCACION_MADRE_4,
         EDUCACION_MADRE_SUPERIOR_INCOMPLETA=EDUCACION_MADRE_5,EDUCACION_MADRE_SUPERIOR_COMPLETA=EDUCACION_MADRE_6,EDUCACION_MADRE_POSTGRADO=EDUCACION_MADRE_7)




#2016 - 1
i=L[4]
p2016_1<-read.delim(file = i, sep = ";")
setnames(p2016_1, old = names(p2016_1),  new =  toupper(names(p2016_1)) )

p2016_1<-p2016_1%>%
  mutate(ID=ESTU_CONSECUTIVO,
         HOMBRE=ifelse(ESTU_GENERO=="M",1,0),
         MUJER=ifelse(ESTU_GENERO=="F",1,0),
         ESTU_FECHANACIMIENTO=ifelse(as.numeric(round(difftime(as.Date("01/03/2016","%d/%m/%Y"),as.Date(ESTU_FECHANACIMIENTO,"%d/%m/%Y"), units = "days")/365.25))>100,NA,ESTU_FECHANACIMIENTO),
         ESTU_FECHANACIMIENTO=ifelse(as.numeric(round(difftime(as.Date("01/03/2016","%d/%m/%Y"),as.Date(ESTU_FECHANACIMIENTO,"%d/%m/%Y"), units = "days")/365.25))<=10,NA,ESTU_FECHANACIMIENTO),
         FECHA_NACIMIENTO=as.Date(ESTU_FECHANACIMIENTO,"%d/%m/%Y"),
         EDAD =as.numeric(round(difftime(as.Date("01/03/2016","%d/%m/%Y"),as.Date(ESTU_FECHANACIMIENTO,"%d/%m/%Y"), units = "days")/365.25)),
         MES_NACIMIENTO = Month(FECHA_NACIMIENTO),
         ANO_NACIMIENTO = year(FECHA_NACIMIENTO),
         PERIODO=as.numeric(PERIODO),
         DEPTO=as.numeric(ESTU_COD_RESIDE_DEPTO),
         MUNICIPIO=as.numeric(ESTU_COD_RESIDE_MCPIO),
         EDUCACION_PADRE=case_when(FAMI_EDUCACIONPADRE==""~0,FAMI_EDUCACIONPADRE=="No Aplica"~0,FAMI_EDUCACIONPADRE=="Ninguno"~0,FAMI_EDUCACIONPADRE=="No sabe"~0,FAMI_EDUCACIONPADRE=="Primaria incompleta"~1,
                                   FAMI_EDUCACIONPADRE=="Primaria completa"~2,FAMI_EDUCACIONPADRE=="Secundaria (Bachillerato) incompleta"~3,FAMI_EDUCACIONPADRE=="Secundaria (Bachillerato) completa"~4,
                                   FAMI_EDUCACIONPADRE=="Técnica o tecnológica incompleta"~5,FAMI_EDUCACIONPADRE=="Educación profesional incompleta"~5,FAMI_EDUCACIONPADRE=="Técnica o tecnológica completa"~6,
                                   FAMI_EDUCACIONPADRE=="Educación profesional completa"~6,FAMI_EDUCACIONPADRE=="Postgrado"~7),
         
         EDUCACION_MADRE=case_when(FAMI_EDUCACIONMADRE==""~0,FAMI_EDUCACIONMADRE=="No Aplica"~0,FAMI_EDUCACIONMADRE=="Ninguno"~0,FAMI_EDUCACIONMADRE=="No sabe"~0,FAMI_EDUCACIONMADRE=="Primaria incompleta"~1,
                                   FAMI_EDUCACIONMADRE=="Primaria completa"~2,FAMI_EDUCACIONMADRE=="Secundaria (Bachillerato) incompleta"~3,FAMI_EDUCACIONMADRE=="Secundaria (Bachillerato) completa"~4,
                                   FAMI_EDUCACIONMADRE=="Técnica o tecnológica incompleta"~5,FAMI_EDUCACIONMADRE=="Educación profesional incompleta"~5,FAMI_EDUCACIONMADRE=="Técnica o tecnológica completa"~6,
                                   FAMI_EDUCACIONMADRE=="Educación profesional completa"~6,FAMI_EDUCACIONMADRE=="Postgrado"~7),
         ESTRATO=as.numeric(str_extract(FAMI_ESTRATOVIVIENDA,"[0-9]")),
         ESTRATO = ifelse(is.na(ESTRATO),0,ESTRATO),
         PERSONAS_HOGAR=case_when(FAMI_PERSONASHOGAR=="Una"~1,FAMI_PERSONASHOGAR=="Dos"~2,FAMI_PERSONASHOGAR=="Tres"~3,FAMI_PERSONASHOGAR=="Cuatro"~4,FAMI_PERSONASHOGAR=="Cinco"~5,FAMI_PERSONASHOGAR=="Seis"~6,
                                  FAMI_PERSONASHOGAR=="Siete"~7,FAMI_PERSONASHOGAR=="Ocho"~8,FAMI_PERSONASHOGAR=="Nueve"~9,FAMI_PERSONASHOGAR=="Diez"~10,FAMI_PERSONASHOGAR=="Once"~11,FAMI_PERSONASHOGAR=="Doce o más"~12),
         
         CUARTOS_HOGAR = case_when(FAMI_CUARTOSHOGAR=="Uno"~1,FAMI_CUARTOSHOGAR=="Dos"~2,FAMI_CUARTOSHOGAR=="Tres"~3,FAMI_CUARTOSHOGAR=="Cuatro"~4,FAMI_CUARTOSHOGAR=="Cinco"~5,FAMI_CUARTOSHOGAR=="Seis o mas"~6,FAMI_CUARTOSHOGAR=="Seis"~6,
                                   FAMI_CUARTOSHOGAR=="Siete"~6,FAMI_CUARTOSHOGAR=="Ocho"~6,FAMI_CUARTOSHOGAR=="Nueve"~6,FAMI_CUARTOSHOGAR=="Diez o más"~6),
         INTERNET = ifelse(FAMI_TIENEINTERNET=="Si",1,0),
         TV = ifelse(FAMI_TIENETELEVISOR=="Si",1,0),
         COMPUTADOR = ifelse(FAMI_TIENECOMPUTADOR=="Si",1,0),
         TRANSPORTE = ifelse(FAMI_TIENEAUTOMOVIL=="Si",1,0),
         HORNO = ifelse(FAMI_TIENEMICROONDAS=="Si" | FAMI_TIENEHORNO=="Si",1,0),
         LAVADORA = ifelse(FAMI_TIENELAVADORA=="Si",1,0),
         
         ESTUDIANTE_TRABAJA=ifelse(ESTU_TRABAJAACTUALMENTE=="No" |ESTU_TRABAJAACTUALMENTE=="" ,0,1),
         
         COLEGIO = as.numeric(COLE_CODIGO_ICFES),
         COLE_FEMENINO = ifelse(COLE_GENERO=="FEMENINO",1,0),
         COLE_MASCULINO = ifelse(COLE_GENERO=="MASCULINO",1,0),
         COLE_MIXTO = ifelse(COLE_GENERO=="MIXTO",1,0),
         CALENDARIO_A = ifelse(COLE_CALENDARIO=="A",1,0),
         CALENDARIO_B = ifelse(COLE_CALENDARIO=="B",1,0),
         COLE_OFICIAL= ifelse(COLE_NATURALEZA=="OFICIAL",1,0),
         COLE_BILINGUE = ifelse(COLE_BILINGUE=="S",1,0),
         COLE_ACADEMICO= ifelse(COLE_CARACTER=="ACADÉMICO" | COLE_CARACTER=="TÉCNICO/ACADÉMICO",1,0),
         COLE_TECNICO= ifelse(COLE_CARACTER=="TÉCNICO" | COLE_CARACTER=="TÉCNICO/ACADÉMICO",1,0),
         JORNADA_COMPLETA=ifelse(COLE_JORNADA=="COMPLETA" |COLE_JORNADA=="UNICA",1,0),
         JORNADA_MANANA=ifelse(COLE_JORNADA=="MAÑANA",1,0),
         JORNADA_NOCHE=ifelse(COLE_JORNADA=="NOCHE",1,0),
         JORNADA_TARDE=ifelse(COLE_JORNADA=="TARDE",1,0),
         
         PUNT_GLOBAL=as.numeric(PUNT_GLOBAL)
  )%>%
  select(c(ID,HOMBRE,MUJER,FECHA_NACIMIENTO,MES_NACIMIENTO,ANO_NACIMIENTO,EDAD,PERIODO,DEPTO,MUNICIPIO,EDUCACION_PADRE,EDUCACION_MADRE,ESTRATO,PERSONAS_HOGAR,CUARTOS_HOGAR,INTERNET,TV,COMPUTADOR,TRANSPORTE,HORNO,LAVADORA,ESTUDIANTE_TRABAJA,COLEGIO,COLE_FEMENINO,COLE_MASCULINO,COLE_MIXTO,CALENDARIO_A,CALENDARIO_B,COLE_BILINGUE,COLE_OFICIAL,COLE_ACADEMICO,COLE_TECNICO,JORNADA_COMPLETA,JORNADA_MANANA,JORNADA_NOCHE,JORNADA_TARDE,PUNT_GLOBAL))

#p2016_1<-as.data.frame(knn.impute( as.matrix(p2016_1), k = 10))

p2016_1<-p2016_1%>%
  dummy_cols(select_columns = c("EDUCACION_PADRE","EDUCACION_MADRE"),remove_first_dummy = TRUE, ignore_na = TRUE, remove_selected_columns = TRUE)%>%
  rename(EDUCACION_PADRE_PRIMARIA_INCOMPLETA=EDUCACION_PADRE_1,EDUCACION_PADRE_PRIMARIA_COMPLETA=EDUCACION_PADRE_2,
         EDUCACION_PADRE_SECUNDARIA_INCOMPLETA=EDUCACION_PADRE_3,EDUCACION_PADRE_SECUNDARIA_COMPLETA=EDUCACION_PADRE_4,
         EDUCACION_PADRE_SUPERIOR_INCOMPLETA=EDUCACION_PADRE_5,EDUCACION_PADRE_SUPERIOR_COMPLETA=EDUCACION_PADRE_6,EDUCACION_PADRE_POSTGRADO=EDUCACION_PADRE_7,
         EDUCACION_MADRE_PRIMARIA_INCOMPLETA=EDUCACION_MADRE_1,EDUCACION_MADRE_PRIMARIA_COMPLETA=EDUCACION_MADRE_2,
         EDUCACION_MADRE_SECUNDARIA_INCOMPLETA=EDUCACION_MADRE_3,EDUCACION_MADRE_SECUNDARIA_COMPLETA=EDUCACION_MADRE_4,
         EDUCACION_MADRE_SUPERIOR_INCOMPLETA=EDUCACION_MADRE_5,EDUCACION_MADRE_SUPERIOR_COMPLETA=EDUCACION_MADRE_6,EDUCACION_MADRE_POSTGRADO=EDUCACION_MADRE_7)





#2016 - 2
i=L[5]
p2016_2<-read.delim(file = i, sep = ";")
setnames(p2016_2, old = names(p2016_2),  new =  toupper(names(p2016_2)) )

p2016_2<-p2016_2%>%
  mutate(ID=ESTU_CONSECUTIVO,
         HOMBRE=ifelse(ESTU_GENERO=="M",1,0),
         MUJER=ifelse(ESTU_GENERO=="F",1,0),
         ESTU_FECHANACIMIENTO=ifelse(as.numeric(round(difftime(as.Date("01/09/2016","%d/%m/%Y"),as.Date(ESTU_FECHANACIMIENTO,"%d/%m/%Y"), units = "days")/365.25))>100,NA,ESTU_FECHANACIMIENTO),
         ESTU_FECHANACIMIENTO=ifelse(as.numeric(round(difftime(as.Date("01/09/2016","%d/%m/%Y"),as.Date(ESTU_FECHANACIMIENTO,"%d/%m/%Y"), units = "days")/365.25))<=10,NA,ESTU_FECHANACIMIENTO),
         FECHA_NACIMIENTO=as.Date(ESTU_FECHANACIMIENTO,"%d/%m/%Y"),
         EDAD =as.numeric(round(difftime(as.Date("01/09/2016","%d/%m/%Y"),as.Date(ESTU_FECHANACIMIENTO,"%d/%m/%Y"), units = "days")/365.25)),
         MES_NACIMIENTO = Month(FECHA_NACIMIENTO),
         ANO_NACIMIENTO = year(FECHA_NACIMIENTO),
         PERIODO=as.numeric(PERIODO),
         DEPTO=as.numeric(ESTU_COD_RESIDE_DEPTO),
         MUNICIPIO=as.numeric(ESTU_COD_RESIDE_MCPIO),
         EDUCACION_PADRE=case_when(FAMI_EDUCACIONPADRE==""~0,FAMI_EDUCACIONPADRE=="No Aplica"~0,FAMI_EDUCACIONPADRE=="Ninguno"~0,FAMI_EDUCACIONPADRE=="No sabe"~0,FAMI_EDUCACIONPADRE=="Primaria incompleta"~1,
                                   FAMI_EDUCACIONPADRE=="Primaria completa"~2,FAMI_EDUCACIONPADRE=="Secundaria (Bachillerato) incompleta"~3,FAMI_EDUCACIONPADRE=="Secundaria (Bachillerato) completa"~4,
                                   FAMI_EDUCACIONPADRE=="Técnica o tecnológica incompleta"~5,FAMI_EDUCACIONPADRE=="Educación profesional incompleta"~5,FAMI_EDUCACIONPADRE=="Técnica o tecnológica completa"~6,
                                   FAMI_EDUCACIONPADRE=="Educación profesional completa"~6,FAMI_EDUCACIONPADRE=="Postgrado"~7),
         
         EDUCACION_MADRE=case_when(FAMI_EDUCACIONMADRE==""~0,FAMI_EDUCACIONMADRE=="No Aplica"~0,FAMI_EDUCACIONMADRE=="Ninguno"~0,FAMI_EDUCACIONMADRE=="No sabe"~0,FAMI_EDUCACIONMADRE=="Primaria incompleta"~1,
                                   FAMI_EDUCACIONMADRE=="Primaria completa"~2,FAMI_EDUCACIONMADRE=="Secundaria (Bachillerato) incompleta"~3,FAMI_EDUCACIONMADRE=="Secundaria (Bachillerato) completa"~4,
                                   FAMI_EDUCACIONMADRE=="Técnica o tecnológica incompleta"~5,FAMI_EDUCACIONMADRE=="Educación profesional incompleta"~5,FAMI_EDUCACIONMADRE=="Técnica o tecnológica completa"~6,
                                   FAMI_EDUCACIONMADRE=="Educación profesional completa"~6,FAMI_EDUCACIONMADRE=="Postgrado"~7),
         ESTRATO=as.numeric(str_extract(FAMI_ESTRATOVIVIENDA,"[0-9]")),
         ESTRATO = ifelse(is.na(ESTRATO),0,ESTRATO),
         PERSONAS_HOGAR=case_when(FAMI_PERSONASHOGAR=="Una"~1,FAMI_PERSONASHOGAR=="Dos"~2,FAMI_PERSONASHOGAR=="Tres"~3,FAMI_PERSONASHOGAR=="Cuatro"~4,FAMI_PERSONASHOGAR=="Cinco"~5,FAMI_PERSONASHOGAR=="Seis"~6,
                                  FAMI_PERSONASHOGAR=="Siete"~7,FAMI_PERSONASHOGAR=="Ocho"~8,FAMI_PERSONASHOGAR=="Nueve"~9,FAMI_PERSONASHOGAR=="Diez"~10,FAMI_PERSONASHOGAR=="Once"~11,FAMI_PERSONASHOGAR=="Doce o más"~12),
         
         CUARTOS_HOGAR = case_when(FAMI_CUARTOSHOGAR=="Uno"~1,FAMI_CUARTOSHOGAR=="Dos"~2,FAMI_CUARTOSHOGAR=="Tres"~3,FAMI_CUARTOSHOGAR=="Cuatro"~4,FAMI_CUARTOSHOGAR=="Cinco"~5,FAMI_CUARTOSHOGAR=="Seis o mas"~6,FAMI_CUARTOSHOGAR=="Seis"~6,
                                   FAMI_CUARTOSHOGAR=="Siete"~6,FAMI_CUARTOSHOGAR=="Ocho"~6,FAMI_CUARTOSHOGAR=="Nueve"~6,FAMI_CUARTOSHOGAR=="Diez o más"~6),
         INTERNET = ifelse(FAMI_TIENEINTERNET=="Si",1,0),
         TV = ifelse(FAMI_TIENESERVICIOTV=="Si",1,0),
         COMPUTADOR = ifelse(FAMI_TIENECOMPUTADOR=="Si",1,0),
         TRANSPORTE = ifelse(FAMI_TIENEAUTOMOVIL=="Si",1,0),
         HORNO = ifelse(FAMI_TIENEMICROONDAS=="Si" | FAMI_TIENEHORNO=="Si",1,0),
         LAVADORA = ifelse(FAMI_TIENELAVADORA=="Si",1,0),
         
         ESTUDIANTE_TRABAJA=ifelse(ESTU_TRABAJAACTUALMENTE=="No" |ESTU_TRABAJAACTUALMENTE=="" ,0,1),
         
         COLEGIO = as.numeric(COLE_CODIGO_ICFES),
         COLE_FEMENINO = ifelse(COLE_GENERO=="FEMENINO",1,0),
         COLE_MASCULINO = ifelse(COLE_GENERO=="MASCULINO",1,0),
         COLE_MIXTO = ifelse(COLE_GENERO=="MIXTO",1,0),
         CALENDARIO_A = ifelse(COLE_CALENDARIO=="A",1,0),
         CALENDARIO_B = ifelse(COLE_CALENDARIO=="B",1,0),
         COLE_OFICIAL= ifelse(COLE_NATURALEZA=="OFICIAL",1,0),
         COLE_BILINGUE = ifelse(COLE_BILINGUE=="S",1,0),
         COLE_ACADEMICO= ifelse(COLE_CARACTER=="ACADÉMICO" | COLE_CARACTER=="TÉCNICO/ACADÉMICO",1,0),
         COLE_TECNICO= ifelse(COLE_CARACTER=="TÉCNICO" | COLE_CARACTER=="TÉCNICO/ACADÉMICO",1,0),
         JORNADA_COMPLETA=ifelse(COLE_JORNADA=="COMPLETA" |COLE_JORNADA=="UNICA",1,0),
         JORNADA_MANANA=ifelse(COLE_JORNADA=="MAÑANA",1,0),
         JORNADA_NOCHE=ifelse(COLE_JORNADA=="NOCHE",1,0),
         JORNADA_TARDE=ifelse(COLE_JORNADA=="TARDE",1,0),
         
         PUNT_GLOBAL=as.numeric(PUNT_GLOBAL)
  )%>%
  select(c(ID,HOMBRE,MUJER,FECHA_NACIMIENTO,MES_NACIMIENTO,ANO_NACIMIENTO,EDAD,PERIODO,DEPTO,MUNICIPIO,EDUCACION_PADRE,EDUCACION_MADRE,ESTRATO,PERSONAS_HOGAR,CUARTOS_HOGAR,INTERNET,TV,COMPUTADOR,TRANSPORTE,HORNO,LAVADORA,ESTUDIANTE_TRABAJA,COLEGIO,COLE_FEMENINO,COLE_MASCULINO,COLE_MIXTO,CALENDARIO_A,CALENDARIO_B,COLE_BILINGUE,COLE_OFICIAL,COLE_ACADEMICO,COLE_TECNICO,JORNADA_COMPLETA,JORNADA_MANANA,JORNADA_NOCHE,JORNADA_TARDE,PUNT_GLOBAL))

#p2016_2<-as.data.frame(knn.impute( as.matrix(p2016_2), k = 10))

p2016_2<-p2016_2%>%
  dummy_cols(select_columns = c("EDUCACION_PADRE","EDUCACION_MADRE"),remove_first_dummy = TRUE, ignore_na = TRUE, remove_selected_columns = TRUE)%>%
  rename(EDUCACION_PADRE_PRIMARIA_INCOMPLETA=EDUCACION_PADRE_1,EDUCACION_PADRE_PRIMARIA_COMPLETA=EDUCACION_PADRE_2,
         EDUCACION_PADRE_SECUNDARIA_INCOMPLETA=EDUCACION_PADRE_3,EDUCACION_PADRE_SECUNDARIA_COMPLETA=EDUCACION_PADRE_4,
         EDUCACION_PADRE_SUPERIOR_INCOMPLETA=EDUCACION_PADRE_5,EDUCACION_PADRE_SUPERIOR_COMPLETA=EDUCACION_PADRE_6,EDUCACION_PADRE_POSTGRADO=EDUCACION_PADRE_7,
         EDUCACION_MADRE_PRIMARIA_INCOMPLETA=EDUCACION_MADRE_1,EDUCACION_MADRE_PRIMARIA_COMPLETA=EDUCACION_MADRE_2,
         EDUCACION_MADRE_SECUNDARIA_INCOMPLETA=EDUCACION_MADRE_3,EDUCACION_MADRE_SECUNDARIA_COMPLETA=EDUCACION_MADRE_4,
         EDUCACION_MADRE_SUPERIOR_INCOMPLETA=EDUCACION_MADRE_5,EDUCACION_MADRE_SUPERIOR_COMPLETA=EDUCACION_MADRE_6,EDUCACION_MADRE_POSTGRADO=EDUCACION_MADRE_7)




#2017 - 1
i=L[6]
p2017_1<-read.delim(file = i, sep = ";")
setnames(p2017_1, old = names(p2017_1),  new =  toupper(names(p2017_1)) )

p2017_1<-p2017_1%>%
  mutate(ID=ESTU_CONSECUTIVO,
         HOMBRE=ifelse(ESTU_GENERO=="M",1,0),
         MUJER=ifelse(ESTU_GENERO=="F",1,0),
         ESTU_FECHANACIMIENTO=ifelse(as.numeric(round(difftime(as.Date("01/03/2017","%d/%m/%Y"),as.Date(ESTU_FECHANACIMIENTO,"%d/%m/%Y"), units = "days")/365.25))>100,NA,ESTU_FECHANACIMIENTO),
         ESTU_FECHANACIMIENTO=ifelse(as.numeric(round(difftime(as.Date("01/03/2017","%d/%m/%Y"),as.Date(ESTU_FECHANACIMIENTO,"%d/%m/%Y"), units = "days")/365.25))<=10,NA,ESTU_FECHANACIMIENTO),
         FECHA_NACIMIENTO=as.Date(ESTU_FECHANACIMIENTO,"%d/%m/%Y"),
         EDAD =as.numeric(round(difftime(as.Date("01/03/2017","%d/%m/%Y"),as.Date(ESTU_FECHANACIMIENTO,"%d/%m/%Y"), units = "days")/365.25)),
         MES_NACIMIENTO = Month(FECHA_NACIMIENTO),
         ANO_NACIMIENTO = year(FECHA_NACIMIENTO),
         PERIODO=as.numeric(PERIODO),
         DEPTO=as.numeric(ESTU_COD_RESIDE_DEPTO),
         MUNICIPIO=as.numeric(ESTU_COD_RESIDE_MCPIO),
         EDUCACION_PADRE=case_when(FAMI_EDUCACIONPADRE==""~0,FAMI_EDUCACIONPADRE=="No Aplica"~0,FAMI_EDUCACIONPADRE=="Ninguno"~0,FAMI_EDUCACIONPADRE=="No sabe"~0,FAMI_EDUCACIONPADRE=="Primaria incompleta"~1,
                                   FAMI_EDUCACIONPADRE=="Primaria completa"~2,FAMI_EDUCACIONPADRE=="Secundaria (Bachillerato) incompleta"~3,FAMI_EDUCACIONPADRE=="Secundaria (Bachillerato) completa"~4,
                                   FAMI_EDUCACIONPADRE=="Técnica o tecnológica incompleta"~5,FAMI_EDUCACIONPADRE=="Educación profesional incompleta"~5,FAMI_EDUCACIONPADRE=="Técnica o tecnológica completa"~6,
                                   FAMI_EDUCACIONPADRE=="Educación profesional completa"~6,FAMI_EDUCACIONPADRE=="Postgrado"~7),
         
         EDUCACION_MADRE=case_when(FAMI_EDUCACIONMADRE==""~0,FAMI_EDUCACIONMADRE=="No Aplica"~0,FAMI_EDUCACIONMADRE=="Ninguno"~0,FAMI_EDUCACIONMADRE=="No sabe"~0,FAMI_EDUCACIONMADRE=="Primaria incompleta"~1,
                                   FAMI_EDUCACIONMADRE=="Primaria completa"~2,FAMI_EDUCACIONMADRE=="Secundaria (Bachillerato) incompleta"~3,FAMI_EDUCACIONMADRE=="Secundaria (Bachillerato) completa"~4,
                                   FAMI_EDUCACIONMADRE=="Técnica o tecnológica incompleta"~5,FAMI_EDUCACIONMADRE=="Educación profesional incompleta"~5,FAMI_EDUCACIONMADRE=="Técnica o tecnológica completa"~6,
                                   FAMI_EDUCACIONMADRE=="Educación profesional completa"~6,FAMI_EDUCACIONMADRE=="Postgrado"~7),
         ESTRATO=as.numeric(str_extract(FAMI_ESTRATOVIVIENDA,"[0-9]")),
         ESTRATO = ifelse(is.na(ESTRATO),0,ESTRATO),
         PERSONAS_HOGAR=case_when(FAMI_PERSONASHOGAR=="1 a 2"~1,FAMI_PERSONASHOGAR=="Dos"~2,FAMI_PERSONASHOGAR=="3 a 4"~3,FAMI_PERSONASHOGAR=="Cuatro"~4,FAMI_PERSONASHOGAR=="5 a 6"~5,FAMI_PERSONASHOGAR=="Seis"~6,
                                  FAMI_PERSONASHOGAR=="7 a 8"~7,FAMI_PERSONASHOGAR=="Ocho"~8,FAMI_PERSONASHOGAR=="9 o más"~9,FAMI_PERSONASHOGAR=="Diez"~10,FAMI_PERSONASHOGAR=="Once"~11,FAMI_PERSONASHOGAR=="Doce o más"~12),
         
         CUARTOS_HOGAR = case_when(FAMI_CUARTOSHOGAR=="Uno"~1,FAMI_CUARTOSHOGAR=="Dos"~2,FAMI_CUARTOSHOGAR=="Tres"~3,FAMI_CUARTOSHOGAR=="Cuatro"~4,FAMI_CUARTOSHOGAR=="Cinco"~5,FAMI_CUARTOSHOGAR=="Seis o mas"~6,FAMI_CUARTOSHOGAR=="Seis"~6,
                                   FAMI_CUARTOSHOGAR=="Siete"~6,FAMI_CUARTOSHOGAR=="Ocho"~6,FAMI_CUARTOSHOGAR=="Nueve"~6,FAMI_CUARTOSHOGAR=="Diez o más"~6),
         INTERNET = ifelse(FAMI_TIENEINTERNET=="Si",1,0),
         TV = ifelse(FAMI_TIENESERVICIOTV=="Si",1,0),
         COMPUTADOR = ifelse(FAMI_TIENECOMPUTADOR=="Si",1,0),
         TRANSPORTE = ifelse(FAMI_TIENEAUTOMOVIL=="Si" | FAMI_TIENEMOTOCICLETA=="Si",1,0),
         HORNO = ifelse(FAMI_TIENEHORNOMICROOGAS=="Si" ,1,0),
         LAVADORA = ifelse(FAMI_TIENELAVADORA=="Si",1,0),
         
         ESTUDIANTE_TRABAJA=ifelse(ESTU_HORASSEMANATRABAJA=="0" |ESTU_HORASSEMANATRABAJA=="" ,0,1),
         
         COLEGIO = as.numeric(COLE_CODIGO_ICFES),
         COLE_FEMENINO = ifelse(COLE_GENERO=="FEMENINO",1,0),
         COLE_MASCULINO = ifelse(COLE_GENERO=="MASCULINO",1,0),
         COLE_MIXTO = ifelse(COLE_GENERO=="MIXTO",1,0),
         CALENDARIO_A = ifelse(COLE_CALENDARIO=="A",1,0),
         CALENDARIO_B = ifelse(COLE_CALENDARIO=="B",1,0),
         COLE_OFICIAL= ifelse(COLE_NATURALEZA=="OFICIAL",1,0),
         COLE_BILINGUE = ifelse(COLE_BILINGUE=="S",1,0),
         COLE_ACADEMICO= ifelse(COLE_CARACTER=="ACADÉMICO" | COLE_CARACTER=="TÉCNICO/ACADÉMICO",1,0),
         COLE_TECNICO= ifelse(COLE_CARACTER=="TÉCNICO" | COLE_CARACTER=="TÉCNICO/ACADÉMICO",1,0),
         JORNADA_COMPLETA=ifelse(COLE_JORNADA=="COMPLETA" |COLE_JORNADA=="UNICA",1,0),
         JORNADA_MANANA=ifelse(COLE_JORNADA=="MAÑANA",1,0),
         JORNADA_NOCHE=ifelse(COLE_JORNADA=="NOCHE",1,0),
         JORNADA_TARDE=ifelse(COLE_JORNADA=="TARDE",1,0),
         
         PUNT_GLOBAL=as.numeric(PUNT_GLOBAL)
  )%>%
  select(c(ID,HOMBRE,MUJER,FECHA_NACIMIENTO,MES_NACIMIENTO,ANO_NACIMIENTO,EDAD,PERIODO,DEPTO,MUNICIPIO,EDUCACION_PADRE,EDUCACION_MADRE,ESTRATO,PERSONAS_HOGAR,CUARTOS_HOGAR,INTERNET,TV,COMPUTADOR,TRANSPORTE,HORNO,LAVADORA,ESTUDIANTE_TRABAJA,COLEGIO,COLE_FEMENINO,COLE_MASCULINO,COLE_MIXTO,CALENDARIO_A,CALENDARIO_B,COLE_BILINGUE,COLE_OFICIAL,COLE_ACADEMICO,COLE_TECNICO,JORNADA_COMPLETA,JORNADA_MANANA,JORNADA_NOCHE,JORNADA_TARDE,PUNT_GLOBAL))

#p2017_1<-as.data.frame(knn.impute( as.matrix(p2017_1), k = 10))

p2017_1<-p2017_1%>%
  dummy_cols(select_columns = c("EDUCACION_PADRE","EDUCACION_MADRE"),remove_first_dummy = TRUE, ignore_na = TRUE, remove_selected_columns = TRUE)%>%
  rename(EDUCACION_PADRE_PRIMARIA_INCOMPLETA=EDUCACION_PADRE_1,EDUCACION_PADRE_PRIMARIA_COMPLETA=EDUCACION_PADRE_2,
         EDUCACION_PADRE_SECUNDARIA_INCOMPLETA=EDUCACION_PADRE_3,EDUCACION_PADRE_SECUNDARIA_COMPLETA=EDUCACION_PADRE_4,
         EDUCACION_PADRE_SUPERIOR_INCOMPLETA=EDUCACION_PADRE_5,EDUCACION_PADRE_SUPERIOR_COMPLETA=EDUCACION_PADRE_6,EDUCACION_PADRE_POSTGRADO=EDUCACION_PADRE_7,
         EDUCACION_MADRE_PRIMARIA_INCOMPLETA=EDUCACION_MADRE_1,EDUCACION_MADRE_PRIMARIA_COMPLETA=EDUCACION_MADRE_2,
         EDUCACION_MADRE_SECUNDARIA_INCOMPLETA=EDUCACION_MADRE_3,EDUCACION_MADRE_SECUNDARIA_COMPLETA=EDUCACION_MADRE_4,
         EDUCACION_MADRE_SUPERIOR_INCOMPLETA=EDUCACION_MADRE_5,EDUCACION_MADRE_SUPERIOR_COMPLETA=EDUCACION_MADRE_6,EDUCACION_MADRE_POSTGRADO=EDUCACION_MADRE_7)




#2017 - 2
i=L[7]
p2017_2<-read.delim(file = i, sep = ";")
setnames(p2017_2, old = names(p2017_2),  new =  toupper(names(p2017_2)) )

p2017_2<-p2017_2%>%
  mutate(ID=ESTU_CONSECUTIVO,
         HOMBRE=ifelse(ESTU_GENERO=="M",1,0),
         MUJER=ifelse(ESTU_GENERO=="F",1,0),
         ESTU_FECHANACIMIENTO=ifelse(as.numeric(round(difftime(as.Date("01/09/2017","%d/%m/%Y"),as.Date(ESTU_FECHANACIMIENTO,"%d/%m/%Y"), units = "days")/365.25))>100,NA,ESTU_FECHANACIMIENTO),
         ESTU_FECHANACIMIENTO=ifelse(as.numeric(round(difftime(as.Date("01/09/2017","%d/%m/%Y"),as.Date(ESTU_FECHANACIMIENTO,"%d/%m/%Y"), units = "days")/365.25))<=10,NA,ESTU_FECHANACIMIENTO),
         FECHA_NACIMIENTO=as.Date(ESTU_FECHANACIMIENTO,"%d/%m/%Y"),
         EDAD =as.numeric(round(difftime(as.Date("01/09/2017","%d/%m/%Y"),as.Date(ESTU_FECHANACIMIENTO,"%d/%m/%Y"), units = "days")/365.25)),
         MES_NACIMIENTO = Month(FECHA_NACIMIENTO),
         ANO_NACIMIENTO = year(FECHA_NACIMIENTO),
         PERIODO=as.numeric(PERIODO),
         DEPTO=as.numeric(ESTU_COD_RESIDE_DEPTO),
         MUNICIPIO=as.numeric(ESTU_COD_RESIDE_MCPIO),
         EDUCACION_PADRE=case_when(FAMI_EDUCACIONPADRE==""~0,FAMI_EDUCACIONPADRE=="No Aplica"~0,FAMI_EDUCACIONPADRE=="Ninguno"~0,FAMI_EDUCACIONPADRE=="No sabe"~0,FAMI_EDUCACIONPADRE=="Primaria incompleta"~1,
                                   FAMI_EDUCACIONPADRE=="Primaria completa"~2,FAMI_EDUCACIONPADRE=="Secundaria (Bachillerato) incompleta"~3,FAMI_EDUCACIONPADRE=="Secundaria (Bachillerato) completa"~4,
                                   FAMI_EDUCACIONPADRE=="Técnica o tecnológica incompleta"~5,FAMI_EDUCACIONPADRE=="Educación profesional incompleta"~5,FAMI_EDUCACIONPADRE=="Técnica o tecnológica completa"~6,
                                   FAMI_EDUCACIONPADRE=="Educación profesional completa"~6,FAMI_EDUCACIONPADRE=="Postgrado"~7),
         
         EDUCACION_MADRE=case_when(FAMI_EDUCACIONMADRE==""~0,FAMI_EDUCACIONMADRE=="No Aplica"~0,FAMI_EDUCACIONMADRE=="Ninguno"~0,FAMI_EDUCACIONMADRE=="No sabe"~0,FAMI_EDUCACIONMADRE=="Primaria incompleta"~1,
                                   FAMI_EDUCACIONMADRE=="Primaria completa"~2,FAMI_EDUCACIONMADRE=="Secundaria (Bachillerato) incompleta"~3,FAMI_EDUCACIONMADRE=="Secundaria (Bachillerato) completa"~4,
                                   FAMI_EDUCACIONMADRE=="Técnica o tecnológica incompleta"~5,FAMI_EDUCACIONMADRE=="Educación profesional incompleta"~5,FAMI_EDUCACIONMADRE=="Técnica o tecnológica completa"~6,
                                   FAMI_EDUCACIONMADRE=="Educación profesional completa"~6,FAMI_EDUCACIONMADRE=="Postgrado"~7),
         ESTRATO=as.numeric(str_extract(FAMI_ESTRATOVIVIENDA,"[0-9]")),
         ESTRATO = ifelse(is.na(ESTRATO),0,ESTRATO),
         PERSONAS_HOGAR=case_when(FAMI_PERSONASHOGAR=="1 a 2"~1,FAMI_PERSONASHOGAR=="Dos"~2,FAMI_PERSONASHOGAR=="3 a 4"~3,FAMI_PERSONASHOGAR=="Cuatro"~4,FAMI_PERSONASHOGAR=="5 a 6"~5,FAMI_PERSONASHOGAR=="Seis"~6,
                                  FAMI_PERSONASHOGAR=="7 a 8"~7,FAMI_PERSONASHOGAR=="Ocho"~8,FAMI_PERSONASHOGAR=="9 o más"~9,FAMI_PERSONASHOGAR=="Diez"~10,FAMI_PERSONASHOGAR=="Once"~11,FAMI_PERSONASHOGAR=="Doce o más"~12),
         
         CUARTOS_HOGAR = case_when(FAMI_CUARTOSHOGAR=="Uno"~1,FAMI_CUARTOSHOGAR=="Dos"~2,FAMI_CUARTOSHOGAR=="Tres"~3,FAMI_CUARTOSHOGAR=="Cuatro"~4,FAMI_CUARTOSHOGAR=="Cinco"~5,FAMI_CUARTOSHOGAR=="Seis o mas"~6,FAMI_CUARTOSHOGAR=="Seis"~6,
                                   FAMI_CUARTOSHOGAR=="Siete"~6,FAMI_CUARTOSHOGAR=="Ocho"~6,FAMI_CUARTOSHOGAR=="Nueve"~6,FAMI_CUARTOSHOGAR=="Diez o más"~6),
         INTERNET = ifelse(FAMI_TIENEINTERNET=="Si",1,0),
         TV = ifelse(FAMI_TIENESERVICIOTV=="Si",1,0),
         COMPUTADOR = ifelse(FAMI_TIENECOMPUTADOR=="Si",1,0),
         TRANSPORTE = ifelse(FAMI_TIENEAUTOMOVIL=="Si" | FAMI_TIENEMOTOCICLETA=="Si",1,0),
         HORNO = ifelse(FAMI_TIENEHORNOMICROOGAS=="Si" ,1,0),
         LAVADORA = ifelse(FAMI_TIENELAVADORA=="Si",1,0),
         
         ESTUDIANTE_TRABAJA=ifelse(ESTU_HORASSEMANATRABAJA=="0" |ESTU_HORASSEMANATRABAJA=="" ,0,1),
         
         COLEGIO = as.numeric(COLE_CODIGO_ICFES),
         COLE_FEMENINO = ifelse(COLE_GENERO=="FEMENINO",1,0),
         COLE_MASCULINO = ifelse(COLE_GENERO=="MASCULINO",1,0),
         COLE_MIXTO = ifelse(COLE_GENERO=="MIXTO",1,0),
         CALENDARIO_A = ifelse(COLE_CALENDARIO=="A",1,0),
         CALENDARIO_B = ifelse(COLE_CALENDARIO=="B",1,0),
         COLE_OFICIAL= ifelse(COLE_NATURALEZA=="OFICIAL",1,0),
         COLE_BILINGUE = ifelse(COLE_BILINGUE=="S",1,0),
         COLE_ACADEMICO= ifelse(COLE_CARACTER=="ACADÉMICO" | COLE_CARACTER=="TÉCNICO/ACADÉMICO",1,0),
         COLE_TECNICO= ifelse(COLE_CARACTER=="TÉCNICO" | COLE_CARACTER=="TÉCNICO/ACADÉMICO",1,0),
         JORNADA_COMPLETA=ifelse(COLE_JORNADA=="COMPLETA" |COLE_JORNADA=="UNICA",1,0),
         JORNADA_MANANA=ifelse(COLE_JORNADA=="MAÑANA",1,0),
         JORNADA_NOCHE=ifelse(COLE_JORNADA=="NOCHE",1,0),
         JORNADA_TARDE=ifelse(COLE_JORNADA=="TARDE",1,0),
         
         PUNT_GLOBAL=as.numeric(PUNT_GLOBAL)
  )%>%
  select(c(ID,HOMBRE,MUJER,FECHA_NACIMIENTO,MES_NACIMIENTO,ANO_NACIMIENTO,EDAD,PERIODO,DEPTO,MUNICIPIO,EDUCACION_PADRE,EDUCACION_MADRE,ESTRATO,PERSONAS_HOGAR,CUARTOS_HOGAR,INTERNET,TV,COMPUTADOR,TRANSPORTE,HORNO,LAVADORA,ESTUDIANTE_TRABAJA,COLEGIO,COLE_FEMENINO,COLE_MASCULINO,COLE_MIXTO,CALENDARIO_A,CALENDARIO_B,COLE_BILINGUE,COLE_OFICIAL,COLE_ACADEMICO,COLE_TECNICO,JORNADA_COMPLETA,JORNADA_MANANA,JORNADA_NOCHE,JORNADA_TARDE,PUNT_GLOBAL))

#p2017_2<-as.data.frame(knn.impute( as.matrix(p2017_2), k = 10))

p2017_2<-p2017_2%>%
  dummy_cols(select_columns = c("EDUCACION_PADRE","EDUCACION_MADRE"),remove_first_dummy = TRUE, ignore_na = TRUE, remove_selected_columns = TRUE)%>%
  rename(EDUCACION_PADRE_PRIMARIA_INCOMPLETA=EDUCACION_PADRE_1,EDUCACION_PADRE_PRIMARIA_COMPLETA=EDUCACION_PADRE_2,
         EDUCACION_PADRE_SECUNDARIA_INCOMPLETA=EDUCACION_PADRE_3,EDUCACION_PADRE_SECUNDARIA_COMPLETA=EDUCACION_PADRE_4,
         EDUCACION_PADRE_SUPERIOR_INCOMPLETA=EDUCACION_PADRE_5,EDUCACION_PADRE_SUPERIOR_COMPLETA=EDUCACION_PADRE_6,EDUCACION_PADRE_POSTGRADO=EDUCACION_PADRE_7,
         EDUCACION_MADRE_PRIMARIA_INCOMPLETA=EDUCACION_MADRE_1,EDUCACION_MADRE_PRIMARIA_COMPLETA=EDUCACION_MADRE_2,
         EDUCACION_MADRE_SECUNDARIA_INCOMPLETA=EDUCACION_MADRE_3,EDUCACION_MADRE_SECUNDARIA_COMPLETA=EDUCACION_MADRE_4,
         EDUCACION_MADRE_SUPERIOR_INCOMPLETA=EDUCACION_MADRE_5,EDUCACION_MADRE_SUPERIOR_COMPLETA=EDUCACION_MADRE_6,EDUCACION_MADRE_POSTGRADO=EDUCACION_MADRE_7)





#2018 - 1
i=L[8]
p2018_1<-read.delim(file = i, sep = ";")
setnames(p2018_1, old = names(p2018_1),  new =  toupper(names(p2018_1)) )

p2018_1<-p2018_1%>%
  mutate(ID=ESTU_CONSECUTIVO,
         HOMBRE=ifelse(ESTU_GENERO=="M",1,0),
         MUJER=ifelse(ESTU_GENERO=="F",1,0),
         ESTU_FECHANACIMIENTO=ifelse(as.numeric(round(difftime(as.Date("01/03/2018","%d/%m/%Y"),as.Date(ESTU_FECHANACIMIENTO,"%d/%m/%Y"), units = "days")/365.25))>100,NA,ESTU_FECHANACIMIENTO),
         ESTU_FECHANACIMIENTO=ifelse(as.numeric(round(difftime(as.Date("01/03/2018","%d/%m/%Y"),as.Date(ESTU_FECHANACIMIENTO,"%d/%m/%Y"), units = "days")/365.25))<=10,NA,ESTU_FECHANACIMIENTO),
         FECHA_NACIMIENTO=as.Date(ESTU_FECHANACIMIENTO,"%d/%m/%Y"),
         EDAD =as.numeric(round(difftime(as.Date("01/03/2018","%d/%m/%Y"),as.Date(ESTU_FECHANACIMIENTO,"%d/%m/%Y"), units = "days")/365.25)),
         MES_NACIMIENTO = Month(FECHA_NACIMIENTO),
         ANO_NACIMIENTO = year(FECHA_NACIMIENTO),
         PERIODO=as.numeric(PERIODO),
         DEPTO=as.numeric(ESTU_COD_RESIDE_DEPTO),
         MUNICIPIO=as.numeric(ESTU_COD_RESIDE_MCPIO),
         EDUCACION_PADRE=case_when(FAMI_EDUCACIONPADRE==""~0,FAMI_EDUCACIONPADRE=="No Aplica"~0,FAMI_EDUCACIONPADRE=="Ninguno"~0,FAMI_EDUCACIONPADRE=="No sabe"~0,FAMI_EDUCACIONPADRE=="Primaria incompleta"~1,
                                   FAMI_EDUCACIONPADRE=="Primaria completa"~2,FAMI_EDUCACIONPADRE=="Secundaria (Bachillerato) incompleta"~3,FAMI_EDUCACIONPADRE=="Secundaria (Bachillerato) completa"~4,
                                   FAMI_EDUCACIONPADRE=="Técnica o tecnológica incompleta"~5,FAMI_EDUCACIONPADRE=="Educación profesional incompleta"~5,FAMI_EDUCACIONPADRE=="Técnica o tecnológica completa"~6,
                                   FAMI_EDUCACIONPADRE=="Educación profesional completa"~6,FAMI_EDUCACIONPADRE=="Postgrado"~7),
         
         EDUCACION_MADRE=case_when(FAMI_EDUCACIONMADRE==""~0,FAMI_EDUCACIONMADRE=="No Aplica"~0,FAMI_EDUCACIONMADRE=="Ninguno"~0,FAMI_EDUCACIONMADRE=="No sabe"~0,FAMI_EDUCACIONMADRE=="Primaria incompleta"~1,
                                   FAMI_EDUCACIONMADRE=="Primaria completa"~2,FAMI_EDUCACIONMADRE=="Secundaria (Bachillerato) incompleta"~3,FAMI_EDUCACIONMADRE=="Secundaria (Bachillerato) completa"~4,
                                   FAMI_EDUCACIONMADRE=="Técnica o tecnológica incompleta"~5,FAMI_EDUCACIONMADRE=="Educación profesional incompleta"~5,FAMI_EDUCACIONMADRE=="Técnica o tecnológica completa"~6,
                                   FAMI_EDUCACIONMADRE=="Educación profesional completa"~6,FAMI_EDUCACIONMADRE=="Postgrado"~7),
         ESTRATO=as.numeric(str_extract(FAMI_ESTRATOVIVIENDA,"[0-9]")),
         ESTRATO = ifelse(is.na(ESTRATO),0,ESTRATO),
         PERSONAS_HOGAR=case_when(FAMI_PERSONASHOGAR=="1 a 2"~1,FAMI_PERSONASHOGAR=="Dos"~2,FAMI_PERSONASHOGAR=="3 a 4"~3,FAMI_PERSONASHOGAR=="Cuatro"~4,FAMI_PERSONASHOGAR=="5 a 6"~5,FAMI_PERSONASHOGAR=="Seis"~6,
                                  FAMI_PERSONASHOGAR=="7 a 8"~7,FAMI_PERSONASHOGAR=="Ocho"~8,FAMI_PERSONASHOGAR=="9 o más"~9,FAMI_PERSONASHOGAR=="Diez"~10,FAMI_PERSONASHOGAR=="Once"~11,FAMI_PERSONASHOGAR=="Doce o más"~12),
         
         CUARTOS_HOGAR = case_when(FAMI_CUARTOSHOGAR=="Uno"~1,FAMI_CUARTOSHOGAR=="Dos"~2,FAMI_CUARTOSHOGAR=="Tres"~3,FAMI_CUARTOSHOGAR=="Cuatro"~4,FAMI_CUARTOSHOGAR=="Cinco"~5,FAMI_CUARTOSHOGAR=="Seis o mas"~6,FAMI_CUARTOSHOGAR=="Seis"~6,
                                   FAMI_CUARTOSHOGAR=="Siete"~6,FAMI_CUARTOSHOGAR=="Ocho"~6,FAMI_CUARTOSHOGAR=="Nueve"~6,FAMI_CUARTOSHOGAR=="Diez o más"~6),
         INTERNET = ifelse(FAMI_TIENEINTERNET=="Si",1,0),
         TV = ifelse(FAMI_TIENESERVICIOTV=="Si",1,0),
         COMPUTADOR = ifelse(FAMI_TIENECOMPUTADOR=="Si",1,0),
         TRANSPORTE = ifelse(FAMI_TIENEAUTOMOVIL=="Si" | FAMI_TIENEMOTOCICLETA=="Si",1,0),
         HORNO = ifelse(FAMI_TIENEHORNOMICROOGAS=="Si" ,1,0),
         LAVADORA = ifelse(FAMI_TIENELAVADORA=="Si",1,0),
         
         ESTUDIANTE_TRABAJA=ifelse(ESTU_HORASSEMANATRABAJA=="0" |ESTU_HORASSEMANATRABAJA=="" ,0,1),
         
         COLEGIO = as.numeric(COLE_CODIGO_ICFES),
         COLE_FEMENINO = ifelse(COLE_GENERO=="FEMENINO",1,0),
         COLE_MASCULINO = ifelse(COLE_GENERO=="MASCULINO",1,0),
         COLE_MIXTO = ifelse(COLE_GENERO=="MIXTO",1,0),
         CALENDARIO_A = ifelse(COLE_CALENDARIO=="A",1,0),
         CALENDARIO_B = ifelse(COLE_CALENDARIO=="B",1,0),
         COLE_OFICIAL= ifelse(COLE_NATURALEZA=="OFICIAL",1,0),
         COLE_BILINGUE = ifelse(COLE_BILINGUE=="S",1,0),
         COLE_ACADEMICO= ifelse(COLE_CARACTER=="ACADÉMICO" | COLE_CARACTER=="TÉCNICO/ACADÉMICO",1,0),
         COLE_TECNICO= ifelse(COLE_CARACTER=="TÉCNICO" | COLE_CARACTER=="TÉCNICO/ACADÉMICO",1,0),
         JORNADA_COMPLETA=ifelse(COLE_JORNADA=="COMPLETA" |COLE_JORNADA=="UNICA",1,0),
         JORNADA_MANANA=ifelse(COLE_JORNADA=="MAÑANA",1,0),
         JORNADA_NOCHE=ifelse(COLE_JORNADA=="NOCHE",1,0),
         JORNADA_TARDE=ifelse(COLE_JORNADA=="TARDE",1,0),
         
         PUNT_GLOBAL=as.numeric(PUNT_GLOBAL)
  )%>%
  select(c(ID,HOMBRE,MUJER,FECHA_NACIMIENTO,MES_NACIMIENTO,ANO_NACIMIENTO,EDAD,PERIODO,DEPTO,MUNICIPIO,EDUCACION_PADRE,EDUCACION_MADRE,ESTRATO,PERSONAS_HOGAR,CUARTOS_HOGAR,INTERNET,TV,COMPUTADOR,TRANSPORTE,HORNO,LAVADORA,ESTUDIANTE_TRABAJA,COLEGIO,COLE_FEMENINO,COLE_MASCULINO,COLE_MIXTO,CALENDARIO_A,CALENDARIO_B,COLE_BILINGUE,COLE_OFICIAL,COLE_ACADEMICO,COLE_TECNICO,JORNADA_COMPLETA,JORNADA_MANANA,JORNADA_NOCHE,JORNADA_TARDE,PUNT_GLOBAL))

#p2018_1<-as.data.frame(knn.impute( as.matrix(p2018_1), k = 10))

p2018_1<-p2018_1%>%
  dummy_cols(select_columns = c("EDUCACION_PADRE","EDUCACION_MADRE"),remove_first_dummy = TRUE, ignore_na = TRUE, remove_selected_columns = TRUE)%>%
  rename(EDUCACION_PADRE_PRIMARIA_INCOMPLETA=EDUCACION_PADRE_1,EDUCACION_PADRE_PRIMARIA_COMPLETA=EDUCACION_PADRE_2,
         EDUCACION_PADRE_SECUNDARIA_INCOMPLETA=EDUCACION_PADRE_3,EDUCACION_PADRE_SECUNDARIA_COMPLETA=EDUCACION_PADRE_4,
         EDUCACION_PADRE_SUPERIOR_INCOMPLETA=EDUCACION_PADRE_5,EDUCACION_PADRE_SUPERIOR_COMPLETA=EDUCACION_PADRE_6,EDUCACION_PADRE_POSTGRADO=EDUCACION_PADRE_7,
         EDUCACION_MADRE_PRIMARIA_INCOMPLETA=EDUCACION_MADRE_1,EDUCACION_MADRE_PRIMARIA_COMPLETA=EDUCACION_MADRE_2,
         EDUCACION_MADRE_SECUNDARIA_INCOMPLETA=EDUCACION_MADRE_3,EDUCACION_MADRE_SECUNDARIA_COMPLETA=EDUCACION_MADRE_4,
         EDUCACION_MADRE_SUPERIOR_INCOMPLETA=EDUCACION_MADRE_5,EDUCACION_MADRE_SUPERIOR_COMPLETA=EDUCACION_MADRE_6,EDUCACION_MADRE_POSTGRADO=EDUCACION_MADRE_7)





#2018 - 2
i=L[9]
p2018_2<-read.delim(file = i, sep = ";")
setnames(p2018_2, old = names(p2018_2),  new =  toupper(names(p2018_2)) )

p2018_2<-p2018_2%>%
  mutate(ID=ESTU_CONSECUTIVO,
         HOMBRE=ifelse(ESTU_GENERO=="M",1,0),
         MUJER=ifelse(ESTU_GENERO=="F",1,0),
         ESTU_FECHANACIMIENTO=ifelse(as.numeric(round(difftime(as.Date("01/09/2018","%d/%m/%Y"),as.Date(ESTU_FECHANACIMIENTO,"%d/%m/%Y"), units = "days")/365.25))>100,NA,ESTU_FECHANACIMIENTO),
         ESTU_FECHANACIMIENTO=ifelse(as.numeric(round(difftime(as.Date("01/09/2018","%d/%m/%Y"),as.Date(ESTU_FECHANACIMIENTO,"%d/%m/%Y"), units = "days")/365.25))<=10,NA,ESTU_FECHANACIMIENTO),
         FECHA_NACIMIENTO=as.Date(ESTU_FECHANACIMIENTO,"%d/%m/%Y"),
         EDAD =as.numeric(round(difftime(as.Date("01/09/2018","%d/%m/%Y"),as.Date(ESTU_FECHANACIMIENTO,"%d/%m/%Y"), units = "days")/365.25)),
         MES_NACIMIENTO = Month(FECHA_NACIMIENTO),
         ANO_NACIMIENTO = year(FECHA_NACIMIENTO),
         PERIODO=as.numeric(PERIODO),
         DEPTO=as.numeric(ESTU_COD_RESIDE_DEPTO),
         MUNICIPIO=as.numeric(ESTU_COD_RESIDE_MCPIO),
         EDUCACION_PADRE=case_when(FAMI_EDUCACIONPADRE==""~0,FAMI_EDUCACIONPADRE=="No Aplica"~0,FAMI_EDUCACIONPADRE=="Ninguno"~0,FAMI_EDUCACIONPADRE=="No sabe"~0,FAMI_EDUCACIONPADRE=="Primaria incompleta"~1,
                                   FAMI_EDUCACIONPADRE=="Primaria completa"~2,FAMI_EDUCACIONPADRE=="Secundaria (Bachillerato) incompleta"~3,FAMI_EDUCACIONPADRE=="Secundaria (Bachillerato) completa"~4,
                                   FAMI_EDUCACIONPADRE=="Técnica o tecnológica incompleta"~5,FAMI_EDUCACIONPADRE=="Educación profesional incompleta"~5,FAMI_EDUCACIONPADRE=="Técnica o tecnológica completa"~6,
                                   FAMI_EDUCACIONPADRE=="Educación profesional completa"~6,FAMI_EDUCACIONPADRE=="Postgrado"~7),
         
         EDUCACION_MADRE=case_when(FAMI_EDUCACIONMADRE==""~0,FAMI_EDUCACIONMADRE=="No Aplica"~0,FAMI_EDUCACIONMADRE=="Ninguno"~0,FAMI_EDUCACIONMADRE=="No sabe"~0,FAMI_EDUCACIONMADRE=="Primaria incompleta"~1,
                                   FAMI_EDUCACIONMADRE=="Primaria completa"~2,FAMI_EDUCACIONMADRE=="Secundaria (Bachillerato) incompleta"~3,FAMI_EDUCACIONMADRE=="Secundaria (Bachillerato) completa"~4,
                                   FAMI_EDUCACIONMADRE=="Técnica o tecnológica incompleta"~5,FAMI_EDUCACIONMADRE=="Educación profesional incompleta"~5,FAMI_EDUCACIONMADRE=="Técnica o tecnológica completa"~6,
                                   FAMI_EDUCACIONMADRE=="Educación profesional completa"~6,FAMI_EDUCACIONMADRE=="Postgrado"~7),
         ESTRATO=as.numeric(str_extract(FAMI_ESTRATOVIVIENDA,"[0-9]")),
         ESTRATO = ifelse(is.na(ESTRATO),0,ESTRATO),
         PERSONAS_HOGAR=case_when(FAMI_PERSONASHOGAR=="1 a 2"~1,FAMI_PERSONASHOGAR=="Dos"~2,FAMI_PERSONASHOGAR=="3 a 4"~3,FAMI_PERSONASHOGAR=="Cuatro"~4,FAMI_PERSONASHOGAR=="5 a 6"~5,FAMI_PERSONASHOGAR=="Seis"~6,
                                  FAMI_PERSONASHOGAR=="7 a 8"~7,FAMI_PERSONASHOGAR=="Ocho"~8,FAMI_PERSONASHOGAR=="9 o más"~9,FAMI_PERSONASHOGAR=="Diez"~10,FAMI_PERSONASHOGAR=="Once"~11,FAMI_PERSONASHOGAR=="Doce o más"~12),
         
         CUARTOS_HOGAR = case_when(FAMI_CUARTOSHOGAR=="Uno"~1,FAMI_CUARTOSHOGAR=="Dos"~2,FAMI_CUARTOSHOGAR=="Tres"~3,FAMI_CUARTOSHOGAR=="Cuatro"~4,FAMI_CUARTOSHOGAR=="Cinco"~5,FAMI_CUARTOSHOGAR=="Seis o mas"~6,FAMI_CUARTOSHOGAR=="Seis"~6,
                                   FAMI_CUARTOSHOGAR=="Siete"~6,FAMI_CUARTOSHOGAR=="Ocho"~6,FAMI_CUARTOSHOGAR=="Nueve"~6,FAMI_CUARTOSHOGAR=="Diez o más"~6),
         INTERNET = ifelse(FAMI_TIENEINTERNET=="Si",1,0),
         TV = ifelse(FAMI_TIENESERVICIOTV=="Si",1,0),
         COMPUTADOR = ifelse(FAMI_TIENECOMPUTADOR=="Si",1,0),
         TRANSPORTE = ifelse(FAMI_TIENEAUTOMOVIL=="Si" | FAMI_TIENEMOTOCICLETA=="Si",1,0),
         HORNO = ifelse(FAMI_TIENEHORNOMICROOGAS=="Si" ,1,0),
         LAVADORA = ifelse(FAMI_TIENELAVADORA=="Si",1,0),
         
         ESTUDIANTE_TRABAJA=ifelse(ESTU_HORASSEMANATRABAJA=="0" |ESTU_HORASSEMANATRABAJA=="" ,0,1),
         
         COLEGIO = as.numeric(COLE_CODIGO_ICFES),
         COLE_FEMENINO = ifelse(COLE_GENERO=="FEMENINO",1,0),
         COLE_MASCULINO = ifelse(COLE_GENERO=="MASCULINO",1,0),
         COLE_MIXTO = ifelse(COLE_GENERO=="MIXTO",1,0),
         CALENDARIO_A = ifelse(COLE_CALENDARIO=="A",1,0),
         CALENDARIO_B = ifelse(COLE_CALENDARIO=="B",1,0),
         COLE_OFICIAL= ifelse(COLE_NATURALEZA=="OFICIAL",1,0),
         COLE_BILINGUE = ifelse(COLE_BILINGUE=="S",1,0),
         COLE_ACADEMICO= ifelse(COLE_CARACTER=="ACADÉMICO" | COLE_CARACTER=="TÉCNICO/ACADÉMICO",1,0),
         COLE_TECNICO= ifelse(COLE_CARACTER=="TÉCNICO" | COLE_CARACTER=="TÉCNICO/ACADÉMICO",1,0),
         JORNADA_COMPLETA=ifelse(COLE_JORNADA=="COMPLETA" |COLE_JORNADA=="UNICA",1,0),
         JORNADA_MANANA=ifelse(COLE_JORNADA=="MAÑANA",1,0),
         JORNADA_NOCHE=ifelse(COLE_JORNADA=="NOCHE",1,0),
         JORNADA_TARDE=ifelse(COLE_JORNADA=="TARDE",1,0),
         
         PUNT_GLOBAL=as.numeric(PUNT_GLOBAL)
  )%>%
  select(c(ID,HOMBRE,MUJER,FECHA_NACIMIENTO,MES_NACIMIENTO,ANO_NACIMIENTO,EDAD,PERIODO,DEPTO,MUNICIPIO,EDUCACION_PADRE,EDUCACION_MADRE,ESTRATO,PERSONAS_HOGAR,CUARTOS_HOGAR,INTERNET,TV,COMPUTADOR,TRANSPORTE,HORNO,LAVADORA,ESTUDIANTE_TRABAJA,COLEGIO,COLE_FEMENINO,COLE_MASCULINO,COLE_MIXTO,CALENDARIO_A,CALENDARIO_B,COLE_BILINGUE,COLE_OFICIAL,COLE_ACADEMICO,COLE_TECNICO,JORNADA_COMPLETA,JORNADA_MANANA,JORNADA_NOCHE,JORNADA_TARDE,PUNT_GLOBAL))

#p2018_2<-as.data.frame(knn.impute( as.matrix(p2018_2), k = 10))

p2018_2<-p2018_2%>%
  dummy_cols(select_columns = c("EDUCACION_PADRE","EDUCACION_MADRE"),remove_first_dummy = TRUE, ignore_na = TRUE, remove_selected_columns = TRUE)%>%
  rename(EDUCACION_PADRE_PRIMARIA_INCOMPLETA=EDUCACION_PADRE_1,EDUCACION_PADRE_PRIMARIA_COMPLETA=EDUCACION_PADRE_2,
         EDUCACION_PADRE_SECUNDARIA_INCOMPLETA=EDUCACION_PADRE_3,EDUCACION_PADRE_SECUNDARIA_COMPLETA=EDUCACION_PADRE_4,
         EDUCACION_PADRE_SUPERIOR_INCOMPLETA=EDUCACION_PADRE_5,EDUCACION_PADRE_SUPERIOR_COMPLETA=EDUCACION_PADRE_6,EDUCACION_PADRE_POSTGRADO=EDUCACION_PADRE_7,
         EDUCACION_MADRE_PRIMARIA_INCOMPLETA=EDUCACION_MADRE_1,EDUCACION_MADRE_PRIMARIA_COMPLETA=EDUCACION_MADRE_2,
         EDUCACION_MADRE_SECUNDARIA_INCOMPLETA=EDUCACION_MADRE_3,EDUCACION_MADRE_SECUNDARIA_COMPLETA=EDUCACION_MADRE_4,
         EDUCACION_MADRE_SUPERIOR_INCOMPLETA=EDUCACION_MADRE_5,EDUCACION_MADRE_SUPERIOR_COMPLETA=EDUCACION_MADRE_6,EDUCACION_MADRE_POSTGRADO=EDUCACION_MADRE_7)




#2019 - 2
i=L[10]
p2019_2<-read.delim(file = i, sep = ";")
setnames(p2019_2, old = names(p2019_2),  new =  toupper(names(p2019_2)) )

p2019_2<-p2019_2%>%
  mutate(ID=ESTU_CONSECUTIVO,
         HOMBRE=ifelse(ESTU_GENERO=="M",1,0),
         MUJER=ifelse(ESTU_GENERO=="F",1,0),
         ESTU_FECHANACIMIENTO=ifelse(as.numeric(round(difftime(as.Date("01/09/2019","%d/%m/%Y"),as.Date(ESTU_FECHANACIMIENTO,"%d/%m/%Y"), units = "days")/365.25))>100,NA,ESTU_FECHANACIMIENTO),
         ESTU_FECHANACIMIENTO=ifelse(as.numeric(round(difftime(as.Date("01/09/2019","%d/%m/%Y"),as.Date(ESTU_FECHANACIMIENTO,"%d/%m/%Y"), units = "days")/365.25))<=10,NA,ESTU_FECHANACIMIENTO),
         FECHA_NACIMIENTO=as.Date(ESTU_FECHANACIMIENTO,"%d/%m/%Y"),
         EDAD =as.numeric(round(difftime(as.Date("01/09/2019","%d/%m/%Y"),as.Date(ESTU_FECHANACIMIENTO,"%d/%m/%Y"), units = "days")/365.25)),
         MES_NACIMIENTO = Month(FECHA_NACIMIENTO),
         ANO_NACIMIENTO = year(FECHA_NACIMIENTO),
         PERIODO=as.numeric(PERIODO),
         DEPTO=as.numeric(ESTU_COD_RESIDE_DEPTO),
         MUNICIPIO=as.numeric(ESTU_COD_RESIDE_MCPIO),
         EDUCACION_PADRE=case_when(FAMI_EDUCACIONPADRE==""~0,FAMI_EDUCACIONPADRE=="No Aplica"~0,FAMI_EDUCACIONPADRE=="Ninguno"~0,FAMI_EDUCACIONPADRE=="No sabe"~0,FAMI_EDUCACIONPADRE=="Primaria incompleta"~1,
                                   FAMI_EDUCACIONPADRE=="Primaria completa"~2,FAMI_EDUCACIONPADRE=="Secundaria (Bachillerato) incompleta"~3,FAMI_EDUCACIONPADRE=="Secundaria (Bachillerato) completa"~4,
                                   FAMI_EDUCACIONPADRE=="Técnica o tecnológica incompleta"~5,FAMI_EDUCACIONPADRE=="Educación profesional incompleta"~5,FAMI_EDUCACIONPADRE=="Técnica o tecnológica completa"~6,
                                   FAMI_EDUCACIONPADRE=="Educación profesional completa"~6,FAMI_EDUCACIONPADRE=="Postgrado"~7),
         
         EDUCACION_MADRE=case_when(FAMI_EDUCACIONMADRE==""~0,FAMI_EDUCACIONMADRE=="No Aplica"~0,FAMI_EDUCACIONMADRE=="Ninguno"~0,FAMI_EDUCACIONMADRE=="No sabe"~0,FAMI_EDUCACIONMADRE=="Primaria incompleta"~1,
                                   FAMI_EDUCACIONMADRE=="Primaria completa"~2,FAMI_EDUCACIONMADRE=="Secundaria (Bachillerato) incompleta"~3,FAMI_EDUCACIONMADRE=="Secundaria (Bachillerato) completa"~4,
                                   FAMI_EDUCACIONMADRE=="Técnica o tecnológica incompleta"~5,FAMI_EDUCACIONMADRE=="Educación profesional incompleta"~5,FAMI_EDUCACIONMADRE=="Técnica o tecnológica completa"~6,
                                   FAMI_EDUCACIONMADRE=="Educación profesional completa"~6,FAMI_EDUCACIONMADRE=="Postgrado"~7),
         ESTRATO=as.numeric(str_extract(FAMI_ESTRATOVIVIENDA,"[0-9]")),
         ESTRATO = ifelse(is.na(ESTRATO),0,ESTRATO),
         PERSONAS_HOGAR=case_when(FAMI_PERSONASHOGAR=="1 a 2"~1,FAMI_PERSONASHOGAR=="Dos"~2,FAMI_PERSONASHOGAR=="3 a 4"~3,FAMI_PERSONASHOGAR=="Cuatro"~4,FAMI_PERSONASHOGAR=="5 a 6"~5,FAMI_PERSONASHOGAR=="Seis"~6,
                                  FAMI_PERSONASHOGAR=="7 a 8"~7,FAMI_PERSONASHOGAR=="Ocho"~8,FAMI_PERSONASHOGAR=="9 o más"~9,FAMI_PERSONASHOGAR=="Diez"~10,FAMI_PERSONASHOGAR=="Once"~11,FAMI_PERSONASHOGAR=="Doce o más"~12),
         
         CUARTOS_HOGAR = case_when(FAMI_CUARTOSHOGAR=="Uno"~1,FAMI_CUARTOSHOGAR=="Dos"~2,FAMI_CUARTOSHOGAR=="Tres"~3,FAMI_CUARTOSHOGAR=="Cuatro"~4,FAMI_CUARTOSHOGAR=="Cinco"~5,FAMI_CUARTOSHOGAR=="Seis o mas"~6,FAMI_CUARTOSHOGAR=="Seis"~6,
                                   FAMI_CUARTOSHOGAR=="Siete"~6,FAMI_CUARTOSHOGAR=="Ocho"~6,FAMI_CUARTOSHOGAR=="Nueve"~6,FAMI_CUARTOSHOGAR=="Diez o más"~6),
         INTERNET = ifelse(FAMI_TIENEINTERNET=="Si",1,0),
         TV = ifelse(FAMI_TIENESERVICIOTV=="Si",1,0),
         COMPUTADOR = ifelse(FAMI_TIENECOMPUTADOR=="Si",1,0),
         TRANSPORTE = ifelse(FAMI_TIENEAUTOMOVIL=="Si" | FAMI_TIENEMOTOCICLETA=="Si",1,0),
         HORNO = ifelse(FAMI_TIENEHORNOMICROOGAS=="Si" ,1,0),
         LAVADORA = ifelse(FAMI_TIENELAVADORA=="Si",1,0),
         
         ESTUDIANTE_TRABAJA=ifelse(ESTU_HORASSEMANATRABAJA=="0" |ESTU_HORASSEMANATRABAJA=="" ,0,1),
         
         COLEGIO = as.numeric(COLE_CODIGO_ICFES),
         COLE_FEMENINO = ifelse(COLE_GENERO=="FEMENINO",1,0),
         COLE_MASCULINO = ifelse(COLE_GENERO=="MASCULINO",1,0),
         COLE_MIXTO = ifelse(COLE_GENERO=="MIXTO",1,0),
         CALENDARIO_A = ifelse(COLE_CALENDARIO=="A",1,0),
         CALENDARIO_B = ifelse(COLE_CALENDARIO=="B",1,0),
         COLE_OFICIAL= ifelse(COLE_NATURALEZA=="OFICIAL",1,0),
         COLE_BILINGUE = ifelse(COLE_BILINGUE=="S",1,0),
         COLE_ACADEMICO= ifelse(COLE_CARACTER=="ACADÉMICO" | COLE_CARACTER=="TÉCNICO/ACADÉMICO",1,0),
         COLE_TECNICO= ifelse(COLE_CARACTER=="TÉCNICO" | COLE_CARACTER=="TÉCNICO/ACADÉMICO",1,0),
         JORNADA_COMPLETA=ifelse(COLE_JORNADA=="COMPLETA" |COLE_JORNADA=="UNICA",1,0),
         JORNADA_MANANA=ifelse(COLE_JORNADA=="MAÑANA",1,0),
         JORNADA_NOCHE=ifelse(COLE_JORNADA=="NOCHE",1,0),
         JORNADA_TARDE=ifelse(COLE_JORNADA=="TARDE",1,0),
         
         PUNT_GLOBAL=as.numeric(PUNT_GLOBAL)
  )%>%
  select(c(ID,HOMBRE,MUJER,FECHA_NACIMIENTO,MES_NACIMIENTO,ANO_NACIMIENTO,EDAD,PERIODO,DEPTO,MUNICIPIO,EDUCACION_PADRE,EDUCACION_MADRE,ESTRATO,PERSONAS_HOGAR,CUARTOS_HOGAR,INTERNET,TV,COMPUTADOR,TRANSPORTE,HORNO,LAVADORA,ESTUDIANTE_TRABAJA,COLEGIO,COLE_FEMENINO,COLE_MASCULINO,COLE_MIXTO,CALENDARIO_A,CALENDARIO_B,COLE_BILINGUE,COLE_OFICIAL,COLE_ACADEMICO,COLE_TECNICO,JORNADA_COMPLETA,JORNADA_MANANA,JORNADA_NOCHE,JORNADA_TARDE,PUNT_GLOBAL))

#p2019_2<-as.data.frame(knn.impute( as.matrix(p2019_2), k = 10))

p2019_2<-p2019_2%>%
  dummy_cols(select_columns = c("EDUCACION_PADRE","EDUCACION_MADRE"),remove_first_dummy = TRUE, ignore_na = TRUE, remove_selected_columns = TRUE)%>%
  rename(EDUCACION_PADRE_PRIMARIA_INCOMPLETA=EDUCACION_PADRE_1,EDUCACION_PADRE_PRIMARIA_COMPLETA=EDUCACION_PADRE_2,
         EDUCACION_PADRE_SECUNDARIA_INCOMPLETA=EDUCACION_PADRE_3,EDUCACION_PADRE_SECUNDARIA_COMPLETA=EDUCACION_PADRE_4,
         EDUCACION_PADRE_SUPERIOR_INCOMPLETA=EDUCACION_PADRE_5,EDUCACION_PADRE_SUPERIOR_COMPLETA=EDUCACION_PADRE_6,EDUCACION_PADRE_POSTGRADO=EDUCACION_PADRE_7,
         EDUCACION_MADRE_PRIMARIA_INCOMPLETA=EDUCACION_MADRE_1,EDUCACION_MADRE_PRIMARIA_COMPLETA=EDUCACION_MADRE_2,
         EDUCACION_MADRE_SECUNDARIA_INCOMPLETA=EDUCACION_MADRE_3,EDUCACION_MADRE_SECUNDARIA_COMPLETA=EDUCACION_MADRE_4,
         EDUCACION_MADRE_SUPERIOR_INCOMPLETA=EDUCACION_MADRE_5,EDUCACION_MADRE_SUPERIOR_COMPLETA=EDUCACION_MADRE_6,EDUCACION_MADRE_POSTGRADO=EDUCACION_MADRE_7)


# group by COLEGIO--------------------------------------------

p2014_2_COLEGIO<-p2014_2%>%
  mutate(UNIANDES=ifelse(PUNT_GLOBAL>=quantile(PUNT_GLOBAL, probs = 0.99),1,0))%>%
  group_by(COLEGIO)%>%
  summarise(MUJER=mean(MUJER, na.rm = T)*100,
            HOMBRE=mean(HOMBRE, na.rm = T)*100,
            MES_NACIMIENTO=Mode(MES_NACIMIENTO, na.rm = T)[1],
            ANO_NACIMIENTO=Mode(ANO_NACIMIENTO, na.rm = T)[1],
            EDAD=mean(EDAD, na.rm = T),
            PERIODO=Mode(PERIODO, na.rm = T)[1],
            DEPTO=Mode(DEPTO, na.rm = T)[1],
            MUNICIPIO=Mode(MUNICIPIO, na.rm = T)[1],
            
            ESTRATO=mean(ESTRATO, na.rm = T),
            PERSONAS_HOGAR=mean(PERSONAS_HOGAR,na.rm = T),
            CUARTOS_HOGAR=mean(CUARTOS_HOGAR,na.rm = T),
            INTERNET=mean(INTERNET,na.rm = T)*100,
            TV=mean(TV,na.rm = T)*100,
            COMPUTADOR=mean(COMPUTADOR,na.rm = T)*100,
            TRANSPORTE=mean(TRANSPORTE,na.rm = T)*100,
            HORNO=mean(HORNO,na.rm = T)*100,
            LAVADORA=mean(LAVADORA,na.rm = T)*100,
            ESTUDIANTE_TRABAJA=mean(ESTUDIANTE_TRABAJA,na.rm = T)*100,
            
            EDUCACION_PADRE_PRIMARIA_INCOMPLETA=mean(EDUCACION_PADRE_PRIMARIA_INCOMPLETA,na.rm = T)*100,
            EDUCACION_PADRE_PRIMARIA_COMPLETA=mean(EDUCACION_PADRE_PRIMARIA_COMPLETA,na.rm = T)*100,
            EDUCACION_PADRE_SECUNDARIA_INCOMPLETA=mean(EDUCACION_PADRE_SECUNDARIA_INCOMPLETA,na.rm = T)*100,
            EDUCACION_PADRE_SECUNDARIA_COMPLETA=mean(EDUCACION_PADRE_SECUNDARIA_COMPLETA,na.rm = T)*100,
            EDUCACION_PADRE_SUPERIOR_INCOMPLETA=mean(EDUCACION_PADRE_SUPERIOR_INCOMPLETA,na.rm = T)*100,
            EDUCACION_PADRE_POSTGRADO=mean(EDUCACION_PADRE_POSTGRADO,na.rm = T)*100,
            EDUCACION_MADRE_PRIMARIA_INCOMPLETA=mean(EDUCACION_MADRE_PRIMARIA_INCOMPLETA,na.rm = T)*100,
            EDUCACION_MADRE_PRIMARIA_COMPLETA=mean(EDUCACION_MADRE_PRIMARIA_COMPLETA,na.rm = T)*100,
            EDUCACION_MADRE_SECUNDARIA_INCOMPLETA=mean(EDUCACION_MADRE_SECUNDARIA_INCOMPLETA,na.rm = T)*100,
            EDUCACION_MADRE_SECUNDARIA_COMPLETA=mean(EDUCACION_MADRE_SECUNDARIA_COMPLETA,na.rm = T)*100,
            EDUCACION_MADRE_SUPERIOR_INCOMPLETA=mean(EDUCACION_MADRE_SUPERIOR_INCOMPLETA,na.rm = T)*100,
            EDUCACION_MADRE_POSTGRADO=mean(EDUCACION_MADRE_POSTGRADO,na.rm = T)*100,
            
            COLE_FEMENINO=mean(COLE_FEMENINO,na.rm = T)*100,
            COLE_MASCULINO=mean(COLE_MASCULINO,na.rm = T)*100,
            COLE_MIXTO=mean(COLE_MIXTO,na.rm = T)*100,
            CALENDARIO_A=mean(CALENDARIO_A,na.rm = T)*100,
            CALENDARIO_B=mean(CALENDARIO_B,na.rm = T)*100,
            COLE_BILINGUE=mean(COLE_BILINGUE,na.rm = T)*100,
            COLE_OFICIAL=mean(COLE_OFICIAL,na.rm = T)*100,
            COLE_ACADEMICO=mean(COLE_ACADEMICO,na.rm = T)*100,
            COLE_TECNICO=mean(COLE_TECNICO,na.rm = T)*100,
            JORNADA_COMPLETA=mean(JORNADA_COMPLETA,na.rm = T)*100,
            JORNADA_MANANA=mean(JORNADA_MANANA,na.rm = T)*100,
            JORNADA_NOCHE=mean(JORNADA_NOCHE,na.rm = T)*100,
            JORNADA_TARDE=mean(JORNADA_TARDE,na.rm = T)*100,
            
            PUNT_GLOBAL_MEAN=mean(PUNT_GLOBAL,na.rm = T),
            PUNT_GLOBAL_VAR=var(PUNT_GLOBAL,  na.rm = T),
            PUNT_GLOBAL_KURT=kurtosis(PUNT_GLOBAL, na.rm = T),
            UNIANDES = sum(UNIANDES, na.rm=T)
)


p2015_1_COLEGIO<-p2015_1%>%
  mutate(UNIANDES=ifelse(PUNT_GLOBAL>=quantile(PUNT_GLOBAL, probs = 0.89),1,0))%>%
  group_by(COLEGIO)%>%
  summarise(MUJER=mean(MUJER, na.rm = T)*100,
            HOMBRE=mean(HOMBRE, na.rm = T)*100,
            MES_NACIMIENTO=Mode(MES_NACIMIENTO, na.rm = T)[1],
            ANO_NACIMIENTO=Mode(ANO_NACIMIENTO, na.rm = T)[1],
            EDAD=mean(EDAD, na.rm = T),
            PERIODO=Mode(PERIODO, na.rm = T)[1],
            DEPTO=Mode(DEPTO, na.rm = T)[1],
            MUNICIPIO=Mode(MUNICIPIO, na.rm = T)[1],
            
            ESTRATO=mean(ESTRATO, na.rm = T),
            PERSONAS_HOGAR=mean(PERSONAS_HOGAR,na.rm = T),
            CUARTOS_HOGAR=mean(CUARTOS_HOGAR,na.rm = T),
            INTERNET=mean(INTERNET,na.rm = T)*100,
            TV=mean(TV,na.rm = T)*100,
            COMPUTADOR=mean(COMPUTADOR,na.rm = T)*100,
            TRANSPORTE=mean(TRANSPORTE,na.rm = T)*100,
            HORNO=mean(HORNO,na.rm = T)*100,
            LAVADORA=mean(LAVADORA,na.rm = T)*100,
            ESTUDIANTE_TRABAJA=mean(ESTUDIANTE_TRABAJA,na.rm = T)*100,
            
            EDUCACION_PADRE_PRIMARIA_INCOMPLETA=mean(EDUCACION_PADRE_PRIMARIA_INCOMPLETA,na.rm = T)*100,
            EDUCACION_PADRE_PRIMARIA_COMPLETA=mean(EDUCACION_PADRE_PRIMARIA_COMPLETA,na.rm = T)*100,
            EDUCACION_PADRE_SECUNDARIA_INCOMPLETA=mean(EDUCACION_PADRE_SECUNDARIA_INCOMPLETA,na.rm = T)*100,
            EDUCACION_PADRE_SECUNDARIA_COMPLETA=mean(EDUCACION_PADRE_SECUNDARIA_COMPLETA,na.rm = T)*100,
            EDUCACION_PADRE_SUPERIOR_INCOMPLETA=mean(EDUCACION_PADRE_SUPERIOR_INCOMPLETA,na.rm = T)*100,
            EDUCACION_PADRE_POSTGRADO=mean(EDUCACION_PADRE_POSTGRADO,na.rm = T)*100,
            EDUCACION_MADRE_PRIMARIA_INCOMPLETA=mean(EDUCACION_MADRE_PRIMARIA_INCOMPLETA,na.rm = T)*100,
            EDUCACION_MADRE_PRIMARIA_COMPLETA=mean(EDUCACION_MADRE_PRIMARIA_COMPLETA,na.rm = T)*100,
            EDUCACION_MADRE_SECUNDARIA_INCOMPLETA=mean(EDUCACION_MADRE_SECUNDARIA_INCOMPLETA,na.rm = T)*100,
            EDUCACION_MADRE_SECUNDARIA_COMPLETA=mean(EDUCACION_MADRE_SECUNDARIA_COMPLETA,na.rm = T)*100,
            EDUCACION_MADRE_SUPERIOR_INCOMPLETA=mean(EDUCACION_MADRE_SUPERIOR_INCOMPLETA,na.rm = T)*100,
            EDUCACION_MADRE_POSTGRADO=mean(EDUCACION_MADRE_POSTGRADO,na.rm = T)*100,
            
            COLE_FEMENINO=mean(COLE_FEMENINO,na.rm = T)*100,
            COLE_MASCULINO=mean(COLE_MASCULINO,na.rm = T)*100,
            COLE_MIXTO=mean(COLE_MIXTO,na.rm = T)*100,
            CALENDARIO_A=mean(CALENDARIO_A,na.rm = T)*100,
            CALENDARIO_B=mean(CALENDARIO_B,na.rm = T)*100,
            COLE_BILINGUE=mean(COLE_BILINGUE,na.rm = T)*100,
            COLE_OFICIAL=mean(COLE_OFICIAL,na.rm = T)*100,
            COLE_ACADEMICO=mean(COLE_ACADEMICO,na.rm = T)*100,
            COLE_TECNICO=mean(COLE_TECNICO,na.rm = T)*100,
            JORNADA_COMPLETA=mean(JORNADA_COMPLETA,na.rm = T)*100,
            JORNADA_MANANA=mean(JORNADA_MANANA,na.rm = T)*100,
            JORNADA_NOCHE=mean(JORNADA_NOCHE,na.rm = T)*100,
            JORNADA_TARDE=mean(JORNADA_TARDE,na.rm = T)*100,
            
            PUNT_GLOBAL_MEAN=mean(PUNT_GLOBAL,na.rm = T),
            PUNT_GLOBAL_VAR=var(PUNT_GLOBAL,  na.rm = T),
            PUNT_GLOBAL_KURT=kurtosis(PUNT_GLOBAL, na.rm = T),
            UNIANDES = sum(UNIANDES, na.rm=T)
  )


p2015_2_COLEGIO<-p2015_2%>%
  mutate(UNIANDES=ifelse(PUNT_GLOBAL>=quantile(PUNT_GLOBAL, probs = 0.99),1,0))%>%
  group_by(COLEGIO)%>%
  summarise(MUJER=mean(MUJER, na.rm = T)*100,
            HOMBRE=mean(HOMBRE, na.rm = T)*100,
            MES_NACIMIENTO=Mode(MES_NACIMIENTO, na.rm = T)[1],
            ANO_NACIMIENTO=Mode(ANO_NACIMIENTO, na.rm = T)[1],
            EDAD=mean(EDAD, na.rm = T),
            PERIODO=Mode(PERIODO, na.rm = T)[1],
            DEPTO=Mode(DEPTO, na.rm = T)[1],
            MUNICIPIO=Mode(MUNICIPIO, na.rm = T)[1],
            
            ESTRATO=mean(ESTRATO, na.rm = T),
            PERSONAS_HOGAR=mean(PERSONAS_HOGAR,na.rm = T),
            CUARTOS_HOGAR=mean(CUARTOS_HOGAR,na.rm = T),
            INTERNET=mean(INTERNET,na.rm = T)*100,
            TV=mean(TV,na.rm = T)*100,
            COMPUTADOR=mean(COMPUTADOR,na.rm = T)*100,
            TRANSPORTE=mean(TRANSPORTE,na.rm = T)*100,
            HORNO=mean(HORNO,na.rm = T)*100,
            LAVADORA=mean(LAVADORA,na.rm = T)*100,
            ESTUDIANTE_TRABAJA=mean(ESTUDIANTE_TRABAJA,na.rm = T)*100,
            
            EDUCACION_PADRE_PRIMARIA_INCOMPLETA=mean(EDUCACION_PADRE_PRIMARIA_INCOMPLETA,na.rm = T)*100,
            EDUCACION_PADRE_PRIMARIA_COMPLETA=mean(EDUCACION_PADRE_PRIMARIA_COMPLETA,na.rm = T)*100,
            EDUCACION_PADRE_SECUNDARIA_INCOMPLETA=mean(EDUCACION_PADRE_SECUNDARIA_INCOMPLETA,na.rm = T)*100,
            EDUCACION_PADRE_SECUNDARIA_COMPLETA=mean(EDUCACION_PADRE_SECUNDARIA_COMPLETA,na.rm = T)*100,
            EDUCACION_PADRE_SUPERIOR_INCOMPLETA=mean(EDUCACION_PADRE_SUPERIOR_INCOMPLETA,na.rm = T)*100,
            EDUCACION_PADRE_POSTGRADO=mean(EDUCACION_PADRE_POSTGRADO,na.rm = T)*100,
            EDUCACION_MADRE_PRIMARIA_INCOMPLETA=mean(EDUCACION_MADRE_PRIMARIA_INCOMPLETA,na.rm = T)*100,
            EDUCACION_MADRE_PRIMARIA_COMPLETA=mean(EDUCACION_MADRE_PRIMARIA_COMPLETA,na.rm = T)*100,
            EDUCACION_MADRE_SECUNDARIA_INCOMPLETA=mean(EDUCACION_MADRE_SECUNDARIA_INCOMPLETA,na.rm = T)*100,
            EDUCACION_MADRE_SECUNDARIA_COMPLETA=mean(EDUCACION_MADRE_SECUNDARIA_COMPLETA,na.rm = T)*100,
            EDUCACION_MADRE_SUPERIOR_INCOMPLETA=mean(EDUCACION_MADRE_SUPERIOR_INCOMPLETA,na.rm = T)*100,
            EDUCACION_MADRE_POSTGRADO=mean(EDUCACION_MADRE_POSTGRADO,na.rm = T)*100,
            
            COLE_FEMENINO=mean(COLE_FEMENINO,na.rm = T)*100,
            COLE_MASCULINO=mean(COLE_MASCULINO,na.rm = T)*100,
            COLE_MIXTO=mean(COLE_MIXTO,na.rm = T)*100,
            CALENDARIO_A=mean(CALENDARIO_A,na.rm = T)*100,
            CALENDARIO_B=mean(CALENDARIO_B,na.rm = T)*100,
            COLE_BILINGUE=mean(COLE_BILINGUE,na.rm = T)*100,
            COLE_OFICIAL=mean(COLE_OFICIAL,na.rm = T)*100,
            COLE_ACADEMICO=mean(COLE_ACADEMICO,na.rm = T)*100,
            COLE_TECNICO=mean(COLE_TECNICO,na.rm = T)*100,
            JORNADA_COMPLETA=mean(JORNADA_COMPLETA,na.rm = T)*100,
            JORNADA_MANANA=mean(JORNADA_MANANA,na.rm = T)*100,
            JORNADA_NOCHE=mean(JORNADA_NOCHE,na.rm = T)*100,
            JORNADA_TARDE=mean(JORNADA_TARDE,na.rm = T)*100,
            
            PUNT_GLOBAL_MEAN=mean(PUNT_GLOBAL,na.rm = T),
            PUNT_GLOBAL_VAR=var(PUNT_GLOBAL,  na.rm = T),
            PUNT_GLOBAL_KURT=kurtosis(PUNT_GLOBAL, na.rm = T),
            UNIANDES = sum(UNIANDES, na.rm=T)
  )


p2016_1_COLEGIO<-p2016_1%>%
  mutate(UNIANDES=ifelse(PUNT_GLOBAL>=quantile(PUNT_GLOBAL, probs = 0.89),1,0))%>%
  group_by(COLEGIO)%>%
  summarise(MUJER=mean(MUJER, na.rm = T)*100,
            HOMBRE=mean(HOMBRE, na.rm = T)*100,
            MES_NACIMIENTO=Mode(MES_NACIMIENTO, na.rm = T)[1],
            ANO_NACIMIENTO=Mode(ANO_NACIMIENTO, na.rm = T)[1],
            EDAD=mean(EDAD, na.rm = T),
            PERIODO=Mode(PERIODO, na.rm = T)[1],
            DEPTO=Mode(DEPTO, na.rm = T)[1],
            MUNICIPIO=Mode(MUNICIPIO, na.rm = T)[1],
            
            ESTRATO=mean(ESTRATO, na.rm = T),
            PERSONAS_HOGAR=mean(PERSONAS_HOGAR,na.rm = T),
            CUARTOS_HOGAR=mean(CUARTOS_HOGAR,na.rm = T),
            INTERNET=mean(INTERNET,na.rm = T)*100,
            TV=mean(TV,na.rm = T)*100,
            COMPUTADOR=mean(COMPUTADOR,na.rm = T)*100,
            TRANSPORTE=mean(TRANSPORTE,na.rm = T)*100,
            HORNO=mean(HORNO,na.rm = T)*100,
            LAVADORA=mean(LAVADORA,na.rm = T)*100,
            ESTUDIANTE_TRABAJA=mean(ESTUDIANTE_TRABAJA,na.rm = T)*100,
            
            EDUCACION_PADRE_PRIMARIA_INCOMPLETA=mean(EDUCACION_PADRE_PRIMARIA_INCOMPLETA,na.rm = T)*100,
            EDUCACION_PADRE_PRIMARIA_COMPLETA=mean(EDUCACION_PADRE_PRIMARIA_COMPLETA,na.rm = T)*100,
            EDUCACION_PADRE_SECUNDARIA_INCOMPLETA=mean(EDUCACION_PADRE_SECUNDARIA_INCOMPLETA,na.rm = T)*100,
            EDUCACION_PADRE_SECUNDARIA_COMPLETA=mean(EDUCACION_PADRE_SECUNDARIA_COMPLETA,na.rm = T)*100,
            EDUCACION_PADRE_SUPERIOR_INCOMPLETA=mean(EDUCACION_PADRE_SUPERIOR_INCOMPLETA,na.rm = T)*100,
            EDUCACION_PADRE_POSTGRADO=mean(EDUCACION_PADRE_POSTGRADO,na.rm = T)*100,
            EDUCACION_MADRE_PRIMARIA_INCOMPLETA=mean(EDUCACION_MADRE_PRIMARIA_INCOMPLETA,na.rm = T)*100,
            EDUCACION_MADRE_PRIMARIA_COMPLETA=mean(EDUCACION_MADRE_PRIMARIA_COMPLETA,na.rm = T)*100,
            EDUCACION_MADRE_SECUNDARIA_INCOMPLETA=mean(EDUCACION_MADRE_SECUNDARIA_INCOMPLETA,na.rm = T)*100,
            EDUCACION_MADRE_SECUNDARIA_COMPLETA=mean(EDUCACION_MADRE_SECUNDARIA_COMPLETA,na.rm = T)*100,
            EDUCACION_MADRE_SUPERIOR_INCOMPLETA=mean(EDUCACION_MADRE_SUPERIOR_INCOMPLETA,na.rm = T)*100,
            EDUCACION_MADRE_POSTGRADO=mean(EDUCACION_MADRE_POSTGRADO,na.rm = T)*100,
            
            COLE_FEMENINO=mean(COLE_FEMENINO,na.rm = T)*100,
            COLE_MASCULINO=mean(COLE_MASCULINO,na.rm = T)*100,
            COLE_MIXTO=mean(COLE_MIXTO,na.rm = T)*100,
            CALENDARIO_A=mean(CALENDARIO_A,na.rm = T)*100,
            CALENDARIO_B=mean(CALENDARIO_B,na.rm = T)*100,
            COLE_BILINGUE=mean(COLE_BILINGUE,na.rm = T)*100,
            COLE_OFICIAL=mean(COLE_OFICIAL,na.rm = T)*100,
            COLE_ACADEMICO=mean(COLE_ACADEMICO,na.rm = T)*100,
            COLE_TECNICO=mean(COLE_TECNICO,na.rm = T)*100,
            JORNADA_COMPLETA=mean(JORNADA_COMPLETA,na.rm = T)*100,
            JORNADA_MANANA=mean(JORNADA_MANANA,na.rm = T)*100,
            JORNADA_NOCHE=mean(JORNADA_NOCHE,na.rm = T)*100,
            JORNADA_TARDE=mean(JORNADA_TARDE,na.rm = T)*100,
            
            PUNT_GLOBAL_MEAN=mean(PUNT_GLOBAL,na.rm = T),
            PUNT_GLOBAL_VAR=var(PUNT_GLOBAL,  na.rm = T),
            PUNT_GLOBAL_KURT=kurtosis(PUNT_GLOBAL, na.rm = T),
            UNIANDES = sum(UNIANDES, na.rm=T)
  )

p2016_2_COLEGIO<-p2016_2%>%
  mutate(UNIANDES=ifelse(PUNT_GLOBAL>=quantile(PUNT_GLOBAL, probs = 0.99),1,0))%>%
  group_by(COLEGIO)%>%
  summarise(MUJER=mean(MUJER, na.rm = T)*100,
            HOMBRE=mean(HOMBRE, na.rm = T)*100,
            MES_NACIMIENTO=Mode(MES_NACIMIENTO, na.rm = T)[1],
            ANO_NACIMIENTO=Mode(ANO_NACIMIENTO, na.rm = T)[1],
            EDAD=mean(EDAD, na.rm = T),
            PERIODO=Mode(PERIODO, na.rm = T)[1],
            DEPTO=Mode(DEPTO, na.rm = T)[1],
            MUNICIPIO=Mode(MUNICIPIO, na.rm = T)[1],
            
            ESTRATO=mean(ESTRATO, na.rm = T),
            PERSONAS_HOGAR=mean(PERSONAS_HOGAR,na.rm = T),
            CUARTOS_HOGAR=mean(CUARTOS_HOGAR,na.rm = T),
            INTERNET=mean(INTERNET,na.rm = T)*100,
            TV=mean(TV,na.rm = T)*100,
            COMPUTADOR=mean(COMPUTADOR,na.rm = T)*100,
            TRANSPORTE=mean(TRANSPORTE,na.rm = T)*100,
            HORNO=mean(HORNO,na.rm = T)*100,
            LAVADORA=mean(LAVADORA,na.rm = T)*100,
            ESTUDIANTE_TRABAJA=mean(ESTUDIANTE_TRABAJA,na.rm = T)*100,
            
            EDUCACION_PADRE_PRIMARIA_INCOMPLETA=mean(EDUCACION_PADRE_PRIMARIA_INCOMPLETA,na.rm = T)*100,
            EDUCACION_PADRE_PRIMARIA_COMPLETA=mean(EDUCACION_PADRE_PRIMARIA_COMPLETA,na.rm = T)*100,
            EDUCACION_PADRE_SECUNDARIA_INCOMPLETA=mean(EDUCACION_PADRE_SECUNDARIA_INCOMPLETA,na.rm = T)*100,
            EDUCACION_PADRE_SECUNDARIA_COMPLETA=mean(EDUCACION_PADRE_SECUNDARIA_COMPLETA,na.rm = T)*100,
            EDUCACION_PADRE_SUPERIOR_INCOMPLETA=mean(EDUCACION_PADRE_SUPERIOR_INCOMPLETA,na.rm = T)*100,
            EDUCACION_PADRE_POSTGRADO=mean(EDUCACION_PADRE_POSTGRADO,na.rm = T)*100,
            EDUCACION_MADRE_PRIMARIA_INCOMPLETA=mean(EDUCACION_MADRE_PRIMARIA_INCOMPLETA,na.rm = T)*100,
            EDUCACION_MADRE_PRIMARIA_COMPLETA=mean(EDUCACION_MADRE_PRIMARIA_COMPLETA,na.rm = T)*100,
            EDUCACION_MADRE_SECUNDARIA_INCOMPLETA=mean(EDUCACION_MADRE_SECUNDARIA_INCOMPLETA,na.rm = T)*100,
            EDUCACION_MADRE_SECUNDARIA_COMPLETA=mean(EDUCACION_MADRE_SECUNDARIA_COMPLETA,na.rm = T)*100,
            EDUCACION_MADRE_SUPERIOR_INCOMPLETA=mean(EDUCACION_MADRE_SUPERIOR_INCOMPLETA,na.rm = T)*100,
            EDUCACION_MADRE_POSTGRADO=mean(EDUCACION_MADRE_POSTGRADO,na.rm = T)*100,
            
            COLE_FEMENINO=mean(COLE_FEMENINO,na.rm = T)*100,
            COLE_MASCULINO=mean(COLE_MASCULINO,na.rm = T)*100,
            COLE_MIXTO=mean(COLE_MIXTO,na.rm = T)*100,
            CALENDARIO_A=mean(CALENDARIO_A,na.rm = T)*100,
            CALENDARIO_B=mean(CALENDARIO_B,na.rm = T)*100,
            COLE_BILINGUE=mean(COLE_BILINGUE,na.rm = T)*100,
            COLE_OFICIAL=mean(COLE_OFICIAL,na.rm = T)*100,
            COLE_ACADEMICO=mean(COLE_ACADEMICO,na.rm = T)*100,
            COLE_TECNICO=mean(COLE_TECNICO,na.rm = T)*100,
            JORNADA_COMPLETA=mean(JORNADA_COMPLETA,na.rm = T)*100,
            JORNADA_MANANA=mean(JORNADA_MANANA,na.rm = T)*100,
            JORNADA_NOCHE=mean(JORNADA_NOCHE,na.rm = T)*100,
            JORNADA_TARDE=mean(JORNADA_TARDE,na.rm = T)*100,
            
            PUNT_GLOBAL_MEAN=mean(PUNT_GLOBAL,na.rm = T),
            PUNT_GLOBAL_VAR=var(PUNT_GLOBAL,  na.rm = T),
            PUNT_GLOBAL_KURT=kurtosis(PUNT_GLOBAL, na.rm = T),
            UNIANDES = sum(UNIANDES, na.rm=T)
  )

p2017_1_COLEGIO<-p2017_1%>%
  mutate(UNIANDES=ifelse(PUNT_GLOBAL>=quantile(PUNT_GLOBAL, probs = 0.89),1,0))%>%
  group_by(COLEGIO)%>%
  summarise(MUJER=mean(MUJER, na.rm = T)*100,
            HOMBRE=mean(HOMBRE, na.rm = T)*100,
            MES_NACIMIENTO=Mode(MES_NACIMIENTO, na.rm = T)[1],
            ANO_NACIMIENTO=Mode(ANO_NACIMIENTO, na.rm = T)[1],
            EDAD=mean(EDAD, na.rm = T),
            PERIODO=Mode(PERIODO, na.rm = T)[1],
            DEPTO=Mode(DEPTO, na.rm = T)[1],
            MUNICIPIO=Mode(MUNICIPIO, na.rm = T)[1],
            
            ESTRATO=mean(ESTRATO, na.rm = T),
            PERSONAS_HOGAR=mean(PERSONAS_HOGAR,na.rm = T),
            CUARTOS_HOGAR=mean(CUARTOS_HOGAR,na.rm = T),
            INTERNET=mean(INTERNET,na.rm = T)*100,
            TV=mean(TV,na.rm = T)*100,
            COMPUTADOR=mean(COMPUTADOR,na.rm = T)*100,
            TRANSPORTE=mean(TRANSPORTE,na.rm = T)*100,
            HORNO=mean(HORNO,na.rm = T)*100,
            LAVADORA=mean(LAVADORA,na.rm = T)*100,
            ESTUDIANTE_TRABAJA=mean(ESTUDIANTE_TRABAJA,na.rm = T)*100,
            
            EDUCACION_PADRE_PRIMARIA_INCOMPLETA=mean(EDUCACION_PADRE_PRIMARIA_INCOMPLETA,na.rm = T)*100,
            EDUCACION_PADRE_PRIMARIA_COMPLETA=mean(EDUCACION_PADRE_PRIMARIA_COMPLETA,na.rm = T)*100,
            EDUCACION_PADRE_SECUNDARIA_INCOMPLETA=mean(EDUCACION_PADRE_SECUNDARIA_INCOMPLETA,na.rm = T)*100,
            EDUCACION_PADRE_SECUNDARIA_COMPLETA=mean(EDUCACION_PADRE_SECUNDARIA_COMPLETA,na.rm = T)*100,
            EDUCACION_PADRE_SUPERIOR_INCOMPLETA=mean(EDUCACION_PADRE_SUPERIOR_INCOMPLETA,na.rm = T)*100,
            EDUCACION_PADRE_POSTGRADO=mean(EDUCACION_PADRE_POSTGRADO,na.rm = T)*100,
            EDUCACION_MADRE_PRIMARIA_INCOMPLETA=mean(EDUCACION_MADRE_PRIMARIA_INCOMPLETA,na.rm = T)*100,
            EDUCACION_MADRE_PRIMARIA_COMPLETA=mean(EDUCACION_MADRE_PRIMARIA_COMPLETA,na.rm = T)*100,
            EDUCACION_MADRE_SECUNDARIA_INCOMPLETA=mean(EDUCACION_MADRE_SECUNDARIA_INCOMPLETA,na.rm = T)*100,
            EDUCACION_MADRE_SECUNDARIA_COMPLETA=mean(EDUCACION_MADRE_SECUNDARIA_COMPLETA,na.rm = T)*100,
            EDUCACION_MADRE_SUPERIOR_INCOMPLETA=mean(EDUCACION_MADRE_SUPERIOR_INCOMPLETA,na.rm = T)*100,
            EDUCACION_MADRE_POSTGRADO=mean(EDUCACION_MADRE_POSTGRADO,na.rm = T)*100,
            
            COLE_FEMENINO=mean(COLE_FEMENINO,na.rm = T)*100,
            COLE_MASCULINO=mean(COLE_MASCULINO,na.rm = T)*100,
            COLE_MIXTO=mean(COLE_MIXTO,na.rm = T)*100,
            CALENDARIO_A=mean(CALENDARIO_A,na.rm = T)*100,
            CALENDARIO_B=mean(CALENDARIO_B,na.rm = T)*100,
            COLE_BILINGUE=mean(COLE_BILINGUE,na.rm = T)*100,
            COLE_OFICIAL=mean(COLE_OFICIAL,na.rm = T)*100,
            COLE_ACADEMICO=mean(COLE_ACADEMICO,na.rm = T)*100,
            COLE_TECNICO=mean(COLE_TECNICO,na.rm = T)*100,
            JORNADA_COMPLETA=mean(JORNADA_COMPLETA,na.rm = T)*100,
            JORNADA_MANANA=mean(JORNADA_MANANA,na.rm = T)*100,
            JORNADA_NOCHE=mean(JORNADA_NOCHE,na.rm = T)*100,
            JORNADA_TARDE=mean(JORNADA_TARDE,na.rm = T)*100,
            
            PUNT_GLOBAL_MEAN=mean(PUNT_GLOBAL,na.rm = T),
            PUNT_GLOBAL_VAR=var(PUNT_GLOBAL,  na.rm = T),
            PUNT_GLOBAL_KURT=kurtosis(PUNT_GLOBAL, na.rm = T),
            UNIANDES = sum(UNIANDES, na.rm=T)
  )

p2017_2_COLEGIO<-p2017_2%>%
  mutate(UNIANDES=ifelse(PUNT_GLOBAL>=quantile(PUNT_GLOBAL, probs = 0.99),1,0))%>%
  group_by(COLEGIO)%>%
  summarise(MUJER=mean(MUJER, na.rm = T)*100,
            HOMBRE=mean(HOMBRE, na.rm = T)*100,
            MES_NACIMIENTO=Mode(MES_NACIMIENTO, na.rm = T)[1],
            ANO_NACIMIENTO=Mode(ANO_NACIMIENTO, na.rm = T)[1],
            EDAD=mean(EDAD, na.rm = T),
            PERIODO=Mode(PERIODO, na.rm = T)[1],
            DEPTO=Mode(DEPTO, na.rm = T)[1],
            MUNICIPIO=Mode(MUNICIPIO, na.rm = T)[1],
            
            ESTRATO=mean(ESTRATO, na.rm = T),
            PERSONAS_HOGAR=mean(PERSONAS_HOGAR,na.rm = T),
            CUARTOS_HOGAR=mean(CUARTOS_HOGAR,na.rm = T),
            INTERNET=mean(INTERNET,na.rm = T)*100,
            TV=mean(TV,na.rm = T)*100,
            COMPUTADOR=mean(COMPUTADOR,na.rm = T)*100,
            TRANSPORTE=mean(TRANSPORTE,na.rm = T)*100,
            HORNO=mean(HORNO,na.rm = T)*100,
            LAVADORA=mean(LAVADORA,na.rm = T)*100,
            ESTUDIANTE_TRABAJA=mean(ESTUDIANTE_TRABAJA,na.rm = T)*100,
            
            EDUCACION_PADRE_PRIMARIA_INCOMPLETA=mean(EDUCACION_PADRE_PRIMARIA_INCOMPLETA,na.rm = T)*100,
            EDUCACION_PADRE_PRIMARIA_COMPLETA=mean(EDUCACION_PADRE_PRIMARIA_COMPLETA,na.rm = T)*100,
            EDUCACION_PADRE_SECUNDARIA_INCOMPLETA=mean(EDUCACION_PADRE_SECUNDARIA_INCOMPLETA,na.rm = T)*100,
            EDUCACION_PADRE_SECUNDARIA_COMPLETA=mean(EDUCACION_PADRE_SECUNDARIA_COMPLETA,na.rm = T)*100,
            EDUCACION_PADRE_SUPERIOR_INCOMPLETA=mean(EDUCACION_PADRE_SUPERIOR_INCOMPLETA,na.rm = T)*100,
            EDUCACION_PADRE_POSTGRADO=mean(EDUCACION_PADRE_POSTGRADO,na.rm = T)*100,
            EDUCACION_MADRE_PRIMARIA_INCOMPLETA=mean(EDUCACION_MADRE_PRIMARIA_INCOMPLETA,na.rm = T)*100,
            EDUCACION_MADRE_PRIMARIA_COMPLETA=mean(EDUCACION_MADRE_PRIMARIA_COMPLETA,na.rm = T)*100,
            EDUCACION_MADRE_SECUNDARIA_INCOMPLETA=mean(EDUCACION_MADRE_SECUNDARIA_INCOMPLETA,na.rm = T)*100,
            EDUCACION_MADRE_SECUNDARIA_COMPLETA=mean(EDUCACION_MADRE_SECUNDARIA_COMPLETA,na.rm = T)*100,
            EDUCACION_MADRE_SUPERIOR_INCOMPLETA=mean(EDUCACION_MADRE_SUPERIOR_INCOMPLETA,na.rm = T)*100,
            EDUCACION_MADRE_POSTGRADO=mean(EDUCACION_MADRE_POSTGRADO,na.rm = T)*100,
            
            COLE_FEMENINO=mean(COLE_FEMENINO,na.rm = T)*100,
            COLE_MASCULINO=mean(COLE_MASCULINO,na.rm = T)*100,
            COLE_MIXTO=mean(COLE_MIXTO,na.rm = T)*100,
            CALENDARIO_A=mean(CALENDARIO_A,na.rm = T)*100,
            CALENDARIO_B=mean(CALENDARIO_B,na.rm = T)*100,
            COLE_BILINGUE=mean(COLE_BILINGUE,na.rm = T)*100,
            COLE_OFICIAL=mean(COLE_OFICIAL,na.rm = T)*100,
            COLE_ACADEMICO=mean(COLE_ACADEMICO,na.rm = T)*100,
            COLE_TECNICO=mean(COLE_TECNICO,na.rm = T)*100,
            JORNADA_COMPLETA=mean(JORNADA_COMPLETA,na.rm = T)*100,
            JORNADA_MANANA=mean(JORNADA_MANANA,na.rm = T)*100,
            JORNADA_NOCHE=mean(JORNADA_NOCHE,na.rm = T)*100,
            JORNADA_TARDE=mean(JORNADA_TARDE,na.rm = T)*100,
            
            PUNT_GLOBAL_MEAN=mean(PUNT_GLOBAL,na.rm = T),
            PUNT_GLOBAL_VAR=var(PUNT_GLOBAL,  na.rm = T),
            PUNT_GLOBAL_KURT=kurtosis(PUNT_GLOBAL, na.rm = T),
            UNIANDES = sum(UNIANDES, na.rm=T)
  )

p2018_1_COLEGIO<-p2018_1%>%
  mutate(UNIANDES=ifelse(PUNT_GLOBAL>=quantile(PUNT_GLOBAL, probs = 0.89),1,0))%>%
  group_by(COLEGIO)%>%
  summarise(MUJER=mean(MUJER, na.rm = T)*100,
            HOMBRE=mean(HOMBRE, na.rm = T)*100,
            MES_NACIMIENTO=Mode(MES_NACIMIENTO, na.rm = T)[1],
            ANO_NACIMIENTO=Mode(ANO_NACIMIENTO, na.rm = T)[1],
            EDAD=mean(EDAD, na.rm = T),
            PERIODO=Mode(PERIODO, na.rm = T)[1],
            DEPTO=Mode(DEPTO, na.rm = T)[1],
            MUNICIPIO=Mode(MUNICIPIO, na.rm = T)[1],
            
            ESTRATO=mean(ESTRATO, na.rm = T),
            PERSONAS_HOGAR=mean(PERSONAS_HOGAR,na.rm = T),
            CUARTOS_HOGAR=mean(CUARTOS_HOGAR,na.rm = T),
            INTERNET=mean(INTERNET,na.rm = T)*100,
            TV=mean(TV,na.rm = T)*100,
            COMPUTADOR=mean(COMPUTADOR,na.rm = T)*100,
            TRANSPORTE=mean(TRANSPORTE,na.rm = T)*100,
            HORNO=mean(HORNO,na.rm = T)*100,
            LAVADORA=mean(LAVADORA,na.rm = T)*100,
            ESTUDIANTE_TRABAJA=mean(ESTUDIANTE_TRABAJA,na.rm = T)*100,
            
            EDUCACION_PADRE_PRIMARIA_INCOMPLETA=mean(EDUCACION_PADRE_PRIMARIA_INCOMPLETA,na.rm = T)*100,
            EDUCACION_PADRE_PRIMARIA_COMPLETA=mean(EDUCACION_PADRE_PRIMARIA_COMPLETA,na.rm = T)*100,
            EDUCACION_PADRE_SECUNDARIA_INCOMPLETA=mean(EDUCACION_PADRE_SECUNDARIA_INCOMPLETA,na.rm = T)*100,
            EDUCACION_PADRE_SECUNDARIA_COMPLETA=mean(EDUCACION_PADRE_SECUNDARIA_COMPLETA,na.rm = T)*100,
            EDUCACION_PADRE_SUPERIOR_INCOMPLETA=mean(EDUCACION_PADRE_SUPERIOR_INCOMPLETA,na.rm = T)*100,
            EDUCACION_PADRE_POSTGRADO=mean(EDUCACION_PADRE_POSTGRADO,na.rm = T)*100,
            EDUCACION_MADRE_PRIMARIA_INCOMPLETA=mean(EDUCACION_MADRE_PRIMARIA_INCOMPLETA,na.rm = T)*100,
            EDUCACION_MADRE_PRIMARIA_COMPLETA=mean(EDUCACION_MADRE_PRIMARIA_COMPLETA,na.rm = T)*100,
            EDUCACION_MADRE_SECUNDARIA_INCOMPLETA=mean(EDUCACION_MADRE_SECUNDARIA_INCOMPLETA,na.rm = T)*100,
            EDUCACION_MADRE_SECUNDARIA_COMPLETA=mean(EDUCACION_MADRE_SECUNDARIA_COMPLETA,na.rm = T)*100,
            EDUCACION_MADRE_SUPERIOR_INCOMPLETA=mean(EDUCACION_MADRE_SUPERIOR_INCOMPLETA,na.rm = T)*100,
            EDUCACION_MADRE_POSTGRADO=mean(EDUCACION_MADRE_POSTGRADO,na.rm = T)*100,
            
            COLE_FEMENINO=mean(COLE_FEMENINO,na.rm = T)*100,
            COLE_MASCULINO=mean(COLE_MASCULINO,na.rm = T)*100,
            COLE_MIXTO=mean(COLE_MIXTO,na.rm = T)*100,
            CALENDARIO_A=mean(CALENDARIO_A,na.rm = T)*100,
            CALENDARIO_B=mean(CALENDARIO_B,na.rm = T)*100,
            COLE_BILINGUE=mean(COLE_BILINGUE,na.rm = T)*100,
            COLE_OFICIAL=mean(COLE_OFICIAL,na.rm = T)*100,
            COLE_ACADEMICO=mean(COLE_ACADEMICO,na.rm = T)*100,
            COLE_TECNICO=mean(COLE_TECNICO,na.rm = T)*100,
            JORNADA_COMPLETA=mean(JORNADA_COMPLETA,na.rm = T)*100,
            JORNADA_MANANA=mean(JORNADA_MANANA,na.rm = T)*100,
            JORNADA_NOCHE=mean(JORNADA_NOCHE,na.rm = T)*100,
            JORNADA_TARDE=mean(JORNADA_TARDE,na.rm = T)*100,
            
            PUNT_GLOBAL_MEAN=mean(PUNT_GLOBAL,na.rm = T),
            PUNT_GLOBAL_VAR=var(PUNT_GLOBAL,  na.rm = T),
            PUNT_GLOBAL_KURT=kurtosis(PUNT_GLOBAL, na.rm = T),
            UNIANDES = sum(UNIANDES, na.rm=T)
  )

p2018_2_COLEGIO<-p2018_2%>%
  mutate(UNIANDES=ifelse(PUNT_GLOBAL>=quantile(PUNT_GLOBAL, probs = 0.99),1,0))%>%
  group_by(COLEGIO)%>%
  summarise(MUJER=mean(MUJER, na.rm = T)*100,
            HOMBRE=mean(HOMBRE, na.rm = T)*100,
            MES_NACIMIENTO=Mode(MES_NACIMIENTO, na.rm = T)[1],
            ANO_NACIMIENTO=Mode(ANO_NACIMIENTO, na.rm = T)[1],
            EDAD=mean(EDAD, na.rm = T),
            PERIODO=Mode(PERIODO, na.rm = T)[1],
            DEPTO=Mode(DEPTO, na.rm = T)[1],
            MUNICIPIO=Mode(MUNICIPIO, na.rm = T)[1],
            
            ESTRATO=mean(ESTRATO, na.rm = T),
            PERSONAS_HOGAR=mean(PERSONAS_HOGAR,na.rm = T),
            CUARTOS_HOGAR=mean(CUARTOS_HOGAR,na.rm = T),
            INTERNET=mean(INTERNET,na.rm = T)*100,
            TV=mean(TV,na.rm = T)*100,
            COMPUTADOR=mean(COMPUTADOR,na.rm = T)*100,
            TRANSPORTE=mean(TRANSPORTE,na.rm = T)*100,
            HORNO=mean(HORNO,na.rm = T)*100,
            LAVADORA=mean(LAVADORA,na.rm = T)*100,
            ESTUDIANTE_TRABAJA=mean(ESTUDIANTE_TRABAJA,na.rm = T)*100,
            
            EDUCACION_PADRE_PRIMARIA_INCOMPLETA=mean(EDUCACION_PADRE_PRIMARIA_INCOMPLETA,na.rm = T)*100,
            EDUCACION_PADRE_PRIMARIA_COMPLETA=mean(EDUCACION_PADRE_PRIMARIA_COMPLETA,na.rm = T)*100,
            EDUCACION_PADRE_SECUNDARIA_INCOMPLETA=mean(EDUCACION_PADRE_SECUNDARIA_INCOMPLETA,na.rm = T)*100,
            EDUCACION_PADRE_SECUNDARIA_COMPLETA=mean(EDUCACION_PADRE_SECUNDARIA_COMPLETA,na.rm = T)*100,
            EDUCACION_PADRE_SUPERIOR_INCOMPLETA=mean(EDUCACION_PADRE_SUPERIOR_INCOMPLETA,na.rm = T)*100,
            EDUCACION_PADRE_POSTGRADO=mean(EDUCACION_PADRE_POSTGRADO,na.rm = T)*100,
            EDUCACION_MADRE_PRIMARIA_INCOMPLETA=mean(EDUCACION_MADRE_PRIMARIA_INCOMPLETA,na.rm = T)*100,
            EDUCACION_MADRE_PRIMARIA_COMPLETA=mean(EDUCACION_MADRE_PRIMARIA_COMPLETA,na.rm = T)*100,
            EDUCACION_MADRE_SECUNDARIA_INCOMPLETA=mean(EDUCACION_MADRE_SECUNDARIA_INCOMPLETA,na.rm = T)*100,
            EDUCACION_MADRE_SECUNDARIA_COMPLETA=mean(EDUCACION_MADRE_SECUNDARIA_COMPLETA,na.rm = T)*100,
            EDUCACION_MADRE_SUPERIOR_INCOMPLETA=mean(EDUCACION_MADRE_SUPERIOR_INCOMPLETA,na.rm = T)*100,
            EDUCACION_MADRE_POSTGRADO=mean(EDUCACION_MADRE_POSTGRADO,na.rm = T)*100,
            
            COLE_FEMENINO=mean(COLE_FEMENINO,na.rm = T)*100,
            COLE_MASCULINO=mean(COLE_MASCULINO,na.rm = T)*100,
            COLE_MIXTO=mean(COLE_MIXTO,na.rm = T)*100,
            CALENDARIO_A=mean(CALENDARIO_A,na.rm = T)*100,
            CALENDARIO_B=mean(CALENDARIO_B,na.rm = T)*100,
            COLE_BILINGUE=mean(COLE_BILINGUE,na.rm = T)*100,
            COLE_OFICIAL=mean(COLE_OFICIAL,na.rm = T)*100,
            COLE_ACADEMICO=mean(COLE_ACADEMICO,na.rm = T)*100,
            COLE_TECNICO=mean(COLE_TECNICO,na.rm = T)*100,
            JORNADA_COMPLETA=mean(JORNADA_COMPLETA,na.rm = T)*100,
            JORNADA_MANANA=mean(JORNADA_MANANA,na.rm = T)*100,
            JORNADA_NOCHE=mean(JORNADA_NOCHE,na.rm = T)*100,
            JORNADA_TARDE=mean(JORNADA_TARDE,na.rm = T)*100,
            
            PUNT_GLOBAL_MEAN=mean(PUNT_GLOBAL,na.rm = T),
            PUNT_GLOBAL_VAR=var(PUNT_GLOBAL,  na.rm = T),
            PUNT_GLOBAL_KURT=kurtosis(PUNT_GLOBAL, na.rm = T),
            UNIANDES = sum(UNIANDES, na.rm=T)
  )

p2019_2_COLEGIO<-p2019_2%>%
  mutate(UNIANDES=ifelse(PUNT_GLOBAL>=quantile(PUNT_GLOBAL, probs = 0.99),1,0))%>%
  group_by(COLEGIO)%>%
  summarise(MUJER=mean(MUJER, na.rm = T)*100,
            HOMBRE=mean(HOMBRE, na.rm = T)*100,
            MES_NACIMIENTO=Mode(MES_NACIMIENTO, na.rm = T)[1],
            ANO_NACIMIENTO=Mode(ANO_NACIMIENTO, na.rm = T)[1],
            EDAD=mean(EDAD, na.rm = T),
            PERIODO=Mode(PERIODO, na.rm = T)[1],
            DEPTO=Mode(DEPTO, na.rm = T)[1],
            MUNICIPIO=Mode(MUNICIPIO, na.rm = T)[1],
            
            ESTRATO=mean(ESTRATO, na.rm = T),
            PERSONAS_HOGAR=mean(PERSONAS_HOGAR,na.rm = T),
            CUARTOS_HOGAR=mean(CUARTOS_HOGAR,na.rm = T),
            INTERNET=mean(INTERNET,na.rm = T)*100,
            TV=mean(TV,na.rm = T)*100,
            COMPUTADOR=mean(COMPUTADOR,na.rm = T)*100,
            TRANSPORTE=mean(TRANSPORTE,na.rm = T)*100,
            HORNO=mean(HORNO,na.rm = T)*100,
            LAVADORA=mean(LAVADORA,na.rm = T)*100,
            ESTUDIANTE_TRABAJA=mean(ESTUDIANTE_TRABAJA,na.rm = T)*100,
            
            EDUCACION_PADRE_PRIMARIA_INCOMPLETA=mean(EDUCACION_PADRE_PRIMARIA_INCOMPLETA,na.rm = T)*100,
            EDUCACION_PADRE_PRIMARIA_COMPLETA=mean(EDUCACION_PADRE_PRIMARIA_COMPLETA,na.rm = T)*100,
            EDUCACION_PADRE_SECUNDARIA_INCOMPLETA=mean(EDUCACION_PADRE_SECUNDARIA_INCOMPLETA,na.rm = T)*100,
            EDUCACION_PADRE_SECUNDARIA_COMPLETA=mean(EDUCACION_PADRE_SECUNDARIA_COMPLETA,na.rm = T)*100,
            EDUCACION_PADRE_SUPERIOR_INCOMPLETA=mean(EDUCACION_PADRE_SUPERIOR_INCOMPLETA,na.rm = T)*100,
            EDUCACION_PADRE_POSTGRADO=mean(EDUCACION_PADRE_POSTGRADO,na.rm = T)*100,
            EDUCACION_MADRE_PRIMARIA_INCOMPLETA=mean(EDUCACION_MADRE_PRIMARIA_INCOMPLETA,na.rm = T)*100,
            EDUCACION_MADRE_PRIMARIA_COMPLETA=mean(EDUCACION_MADRE_PRIMARIA_COMPLETA,na.rm = T)*100,
            EDUCACION_MADRE_SECUNDARIA_INCOMPLETA=mean(EDUCACION_MADRE_SECUNDARIA_INCOMPLETA,na.rm = T)*100,
            EDUCACION_MADRE_SECUNDARIA_COMPLETA=mean(EDUCACION_MADRE_SECUNDARIA_COMPLETA,na.rm = T)*100,
            EDUCACION_MADRE_SUPERIOR_INCOMPLETA=mean(EDUCACION_MADRE_SUPERIOR_INCOMPLETA,na.rm = T)*100,
            EDUCACION_MADRE_POSTGRADO=mean(EDUCACION_MADRE_POSTGRADO,na.rm = T)*100,
            
            COLE_FEMENINO=mean(COLE_FEMENINO,na.rm = T)*100,
            COLE_MASCULINO=mean(COLE_MASCULINO,na.rm = T)*100,
            COLE_MIXTO=mean(COLE_MIXTO,na.rm = T)*100,
            CALENDARIO_A=mean(CALENDARIO_A,na.rm = T)*100,
            CALENDARIO_B=mean(CALENDARIO_B,na.rm = T)*100,
            COLE_BILINGUE=mean(COLE_BILINGUE,na.rm = T)*100,
            COLE_OFICIAL=mean(COLE_OFICIAL,na.rm = T)*100,
            COLE_ACADEMICO=mean(COLE_ACADEMICO,na.rm = T)*100,
            COLE_TECNICO=mean(COLE_TECNICO,na.rm = T)*100,
            JORNADA_COMPLETA=mean(JORNADA_COMPLETA,na.rm = T)*100,
            JORNADA_MANANA=mean(JORNADA_MANANA,na.rm = T)*100,
            JORNADA_NOCHE=mean(JORNADA_NOCHE,na.rm = T)*100,
            JORNADA_TARDE=mean(JORNADA_TARDE,na.rm = T)*100,
            
            PUNT_GLOBAL_MEAN=mean(PUNT_GLOBAL,na.rm = T),
            PUNT_GLOBAL_VAR=var(PUNT_GLOBAL,  na.rm = T),
            PUNT_GLOBAL_KURT=kurtosis(PUNT_GLOBAL, na.rm = T),
            UNIANDES = sum(UNIANDES, na.rm=T)
  )

p2014_2_COLEGIO<-as.data.frame(knn.impute( as.matrix(p2014_2_COLEGIO), k = 10))
p2015_1_COLEGIO<-as.data.frame(knn.impute( as.matrix(p2015_1_COLEGIO), k = 10))
p2015_2_COLEGIO<-as.data.frame(knn.impute( as.matrix(p2015_2_COLEGIO), k = 10))
p2016_1_COLEGIO<-as.data.frame(knn.impute( as.matrix(p2016_1_COLEGIO), k = 10))
p2016_2_COLEGIO<-as.data.frame(knn.impute( as.matrix(p2016_2_COLEGIO), k = 10))
p2017_1_COLEGIO<-as.data.frame(knn.impute( as.matrix(p2017_1_COLEGIO), k = 10))
p2017_2_COLEGIO<-as.data.frame(knn.impute( as.matrix(p2017_2_COLEGIO), k = 10))
p2018_1_COLEGIO<-as.data.frame(knn.impute( as.matrix(p2018_1_COLEGIO), k = 10))
p2018_2_COLEGIO<-as.data.frame(knn.impute( as.matrix(p2018_2_COLEGIO), k = 10))
p2019_2_COLEGIO<-as.data.frame(knn.impute( as.matrix(p2019_2_COLEGIO), k = 10))


# lead UNIANDES -----------------------------------------------------------

p2014_2_COLEGIO<-p2014_2_COLEGIO%>%
  left_join(subset(p2015_2_COLEGIO,select= c(COLEGIO,UNIANDES)),by="COLEGIO",suffix=c("",".LEAD"))

p2015_1_COLEGIO<-p2015_1_COLEGIO%>%
  left_join(subset(p2016_1_COLEGIO,select= c(COLEGIO,UNIANDES)),by="COLEGIO",suffix=c("",".LEAD"))

p2015_2_COLEGIO<-p2015_2_COLEGIO%>%
  left_join(subset(p2016_2_COLEGIO,select= c(COLEGIO,UNIANDES)),by="COLEGIO",suffix=c("",".LEAD"))

p2016_1_COLEGIO<-p2016_1_COLEGIO%>%
  left_join(subset(p2017_1_COLEGIO,select= c(COLEGIO,UNIANDES)),by="COLEGIO",suffix=c("",".LEAD"))

p2016_2_COLEGIO<-p2016_2_COLEGIO%>%
  left_join(subset(p2017_2_COLEGIO,select= c(COLEGIO,UNIANDES)),by="COLEGIO",suffix=c("",".LEAD"))

p2017_1_COLEGIO<-p2017_1_COLEGIO%>%
  left_join(subset(p2018_1_COLEGIO,select= c(COLEGIO,UNIANDES)),by="COLEGIO",suffix=c("",".LEAD"))

p2017_2_COLEGIO<-p2017_2_COLEGIO%>%
  left_join(subset(p2018_2_COLEGIO,select= c(COLEGIO,UNIANDES)),by="COLEGIO",suffix=c("",".LEAD"))

p2018_1_COLEGIO<-p2018_1_COLEGIO%>%
  left_join(subset(p2019_2_COLEGIO,select= c(COLEGIO,UNIANDES)),by="COLEGIO",suffix=c("",".LEAD"))

p2018_2_COLEGIO<-p2018_2_COLEGIO%>%
  left_join(subset(p2019_2_COLEGIO,select= c(COLEGIO,UNIANDES)),by="COLEGIO",suffix=c("",".LEAD"))


####

p2014_2_COLEGIO<-as.data.frame(knn.impute( as.matrix(p2014_2_COLEGIO), k = 10))
p2015_1_COLEGIO<-as.data.frame(knn.impute( as.matrix(p2015_1_COLEGIO), k = 10))
p2015_2_COLEGIO<-as.data.frame(knn.impute( as.matrix(p2015_2_COLEGIO), k = 10))
p2016_1_COLEGIO<-as.data.frame(knn.impute( as.matrix(p2016_1_COLEGIO), k = 10))
p2016_2_COLEGIO<-as.data.frame(knn.impute( as.matrix(p2016_2_COLEGIO), k = 10))
p2017_1_COLEGIO<-as.data.frame(knn.impute( as.matrix(p2017_1_COLEGIO), k = 10))
p2017_2_COLEGIO<-as.data.frame(knn.impute( as.matrix(p2017_2_COLEGIO), k = 10))
p2018_1_COLEGIO<-as.data.frame(knn.impute( as.matrix(p2018_1_COLEGIO), k = 10))
p2018_2_COLEGIO<-as.data.frame(knn.impute( as.matrix(p2018_2_COLEGIO), k = 10))
p2019_2_COLEGIO<-as.data.frame(knn.impute( as.matrix(p2019_2_COLEGIO), k = 10))



# Append ------------------------------------------------------------------


Saber11<-bind_rows(p2014_2_COLEGIO,p2015_1_COLEGIO,p2015_2_COLEGIO,p2016_1_COLEGIO,p2016_2_COLEGIO,p2017_1_COLEGIO,p2017_2_COLEGIO,p2018_1_COLEGIO,p2018_2_COLEGIO)
Saber11<-as.data.frame(knn.impute( as.matrix(Saber11), k = 10))
Saber11<-bind_rows(Saber11,p2019_2_COLEGIO)



saveRDS(Saber11,file = "Saber11.rds")


# group by COLEGIO ESTRATO ------------------------------------------------

pruebas<-c("p2014_2","p2015_1","p2015_2","p2016_1","p2016_2","p2017_1","p2017_2","p2018_1","p2018_2","p2019_2")
for (g in pruebas){
  
  print(g)
  
  COLEGIO_ESTRATO<-get(g)%>%
    mutate(UNIANDES=ifelse(PUNT_GLOBAL>=quantile(PUNT_GLOBAL, probs = 0.99),1,0))%>%
    group_by(COLEGIO,ESTRATO)%>%
    summarise(MUJER=mean(MUJER, na.rm = T)*100,
              HOMBRE=mean(HOMBRE, na.rm = T)*100,
              MES_NACIMIENTO=Mode(MES_NACIMIENTO, na.rm = T)[1],
              ANO_NACIMIENTO=Mode(ANO_NACIMIENTO, na.rm = T)[1],
              EDAD=mean(EDAD, na.rm = T),
              PERIODO=Mode(PERIODO, na.rm = T)[1],
              DEPTO=Mode(DEPTO, na.rm = T)[1],
              MUNICIPIO=Mode(MUNICIPIO, na.rm = T)[1],
              
              PERSONAS_HOGAR=mean(PERSONAS_HOGAR,na.rm = T),
              CUARTOS_HOGAR=mean(CUARTOS_HOGAR,na.rm = T),
              INTERNET=mean(INTERNET,na.rm = T)*100,
              TV=mean(TV,na.rm = T)*100,
              COMPUTADOR=mean(COMPUTADOR,na.rm = T)*100,
              TRANSPORTE=mean(TRANSPORTE,na.rm = T)*100,
              HORNO=mean(HORNO,na.rm = T)*100,
              LAVADORA=mean(LAVADORA,na.rm = T)*100,
              ESTUDIANTE_TRABAJA=mean(ESTUDIANTE_TRABAJA,na.rm = T)*100,
              
              EDUCACION_PADRE_PRIMARIA_INCOMPLETA=mean(EDUCACION_PADRE_PRIMARIA_INCOMPLETA,na.rm = T)*100,
              EDUCACION_PADRE_PRIMARIA_COMPLETA=mean(EDUCACION_PADRE_PRIMARIA_COMPLETA,na.rm = T)*100,
              EDUCACION_PADRE_SECUNDARIA_INCOMPLETA=mean(EDUCACION_PADRE_SECUNDARIA_INCOMPLETA,na.rm = T)*100,
              EDUCACION_PADRE_SECUNDARIA_COMPLETA=mean(EDUCACION_PADRE_SECUNDARIA_COMPLETA,na.rm = T)*100,
              EDUCACION_PADRE_SUPERIOR_INCOMPLETA=mean(EDUCACION_PADRE_SUPERIOR_INCOMPLETA,na.rm = T)*100,
              EDUCACION_PADRE_POSTGRADO=mean(EDUCACION_PADRE_POSTGRADO,na.rm = T)*100,
              EDUCACION_MADRE_PRIMARIA_INCOMPLETA=mean(EDUCACION_MADRE_PRIMARIA_INCOMPLETA,na.rm = T)*100,
              EDUCACION_MADRE_PRIMARIA_COMPLETA=mean(EDUCACION_MADRE_PRIMARIA_COMPLETA,na.rm = T)*100,
              EDUCACION_MADRE_SECUNDARIA_INCOMPLETA=mean(EDUCACION_MADRE_SECUNDARIA_INCOMPLETA,na.rm = T)*100,
              EDUCACION_MADRE_SECUNDARIA_COMPLETA=mean(EDUCACION_MADRE_SECUNDARIA_COMPLETA,na.rm = T)*100,
              EDUCACION_MADRE_SUPERIOR_INCOMPLETA=mean(EDUCACION_MADRE_SUPERIOR_INCOMPLETA,na.rm = T)*100,
              EDUCACION_MADRE_POSTGRADO=mean(EDUCACION_MADRE_POSTGRADO,na.rm = T)*100,
              
              COLE_FEMENINO=mean(COLE_FEMENINO,na.rm = T)*100,
              COLE_MASCULINO=mean(COLE_MASCULINO,na.rm = T)*100,
              COLE_MIXTO=mean(COLE_MIXTO,na.rm = T)*100,
              CALENDARIO_A=mean(CALENDARIO_A,na.rm = T)*100,
              CALENDARIO_B=mean(CALENDARIO_B,na.rm = T)*100,
              COLE_BILINGUE=mean(COLE_BILINGUE,na.rm = T)*100,
              COLE_OFICIAL=mean(COLE_OFICIAL,na.rm = T)*100,
              COLE_ACADEMICO=mean(COLE_ACADEMICO,na.rm = T)*100,
              COLE_TECNICO=mean(COLE_TECNICO,na.rm = T)*100,
              JORNADA_COMPLETA=mean(JORNADA_COMPLETA,na.rm = T)*100,
              JORNADA_MANANA=mean(JORNADA_MANANA,na.rm = T)*100,
              JORNADA_NOCHE=mean(JORNADA_NOCHE,na.rm = T)*100,
              JORNADA_TARDE=mean(JORNADA_TARDE,na.rm = T)*100,
              
              PUNT_GLOBAL_MEAN=mean(PUNT_GLOBAL,na.rm = T),
              PUNT_GLOBAL_VAR=var(PUNT_GLOBAL,  na.rm = T),
              PUNT_GLOBAL_KURT=kurtosis(PUNT_GLOBAL, na.rm = T),
              UNIANDES = sum(UNIANDES, na.rm=T)
    )
  
  COLEGIO_ESTRATO<-as.data.frame(knn.impute( as.matrix(COLEGIO_ESTRATO), k = 10))
  
  assign(paste0(g,"_COLEGIO_ESTRATO"),COLEGIO_ESTRATO)
  
}



p2014_2_COLEGIO_ESTRATO<-p2014_2_COLEGIO_ESTRATO%>%
  left_join(subset(p2015_2_COLEGIO_ESTRATO,select= c(COLEGIO,UNIANDES,ESTRATO)),by=c("COLEGIO","ESTRATO"),suffix=c("",".LEAD"))

p2015_1_COLEGIO_ESTRATO<-p2015_1_COLEGIO_ESTRATO%>%
  left_join(subset(p2016_1_COLEGIO_ESTRATO,select= c(COLEGIO,UNIANDES,ESTRATO)),by=c("COLEGIO","ESTRATO"),suffix=c("",".LEAD"))

p2015_2_COLEGIO_ESTRATO<-p2015_2_COLEGIO_ESTRATO%>%
  left_join(subset(p2016_2_COLEGIO_ESTRATO,select= c(COLEGIO,UNIANDES,ESTRATO)),by=c("COLEGIO","ESTRATO"),suffix=c("",".LEAD"))

p2016_1_COLEGIO_ESTRATO<-p2016_1_COLEGIO_ESTRATO%>%
  left_join(subset(p2017_1_COLEGIO_ESTRATO,select= c(COLEGIO,UNIANDES,ESTRATO)),by=c("COLEGIO","ESTRATO"),suffix=c("",".LEAD"))

p2016_2_COLEGIO_ESTRATO<-p2016_2_COLEGIO_ESTRATO%>%
  left_join(subset(p2017_2_COLEGIO_ESTRATO,select= c(COLEGIO,UNIANDES,ESTRATO)),by=c("COLEGIO","ESTRATO"),suffix=c("",".LEAD"))

p2017_1_COLEGIO_ESTRATO<-p2017_1_COLEGIO_ESTRATO%>%
  left_join(subset(p2018_1_COLEGIO_ESTRATO,select= c(COLEGIO,UNIANDES,ESTRATO)),by=c("COLEGIO","ESTRATO"),suffix=c("",".LEAD"))

p2017_2_COLEGIO_ESTRATO<-p2017_2_COLEGIO_ESTRATO%>%
  left_join(subset(p2018_2_COLEGIO_ESTRATO,select= c(COLEGIO,UNIANDES,ESTRATO)),by=c("COLEGIO","ESTRATO"),suffix=c("",".LEAD"))

p2018_1_COLEGIO_ESTRATO<-p2018_1_COLEGIO_ESTRATO%>%
  left_join(subset(p2019_2_COLEGIO_ESTRATO,select= c(COLEGIO,UNIANDES,ESTRATO)),by=c("COLEGIO","ESTRATO"),suffix=c("",".LEAD"))

p2018_1_COLEGIO_ESTRATO<-p2018_1_COLEGIO_ESTRATO%>%
  left_join(subset(p2018_2_COLEGIO_ESTRATO,select= c(COLEGIO,UNIANDES,ESTRATO)),by=c("COLEGIO","ESTRATO"),suffix=c("",".LEAD"))


p2018_2_COLEGIO_ESTRATO<-p2018_2_COLEGIO_ESTRATO%>%
  left_join(subset(p2019_2_COLEGIO_ESTRATO,select= c(COLEGIO,UNIANDES,ESTRATO)),by=c("COLEGIO","ESTRATO"),suffix=c("",".LEAD"))


####

p2014_2_COLEGIO_ESTRATO<-as.data.frame(knn.impute( as.matrix(p2014_2_COLEGIO_ESTRATO), k = 10))
p2015_1_COLEGIO_ESTRATO<-as.data.frame(knn.impute( as.matrix(p2015_1_COLEGIO_ESTRATO), k = 10))
p2015_2_COLEGIO_ESTRATO<-as.data.frame(knn.impute( as.matrix(p2015_2_COLEGIO_ESTRATO), k = 10))
p2016_1_COLEGIO_ESTRATO<-as.data.frame(knn.impute( as.matrix(p2016_1_COLEGIO_ESTRATO), k = 10))
p2016_2_COLEGIO_ESTRATO<-as.data.frame(knn.impute( as.matrix(p2016_2_COLEGIO_ESTRATO), k = 10))
p2017_1_COLEGIO_ESTRATO<-as.data.frame(knn.impute( as.matrix(p2017_1_COLEGIO_ESTRATO), k = 10))
p2017_2_COLEGIO_ESTRATO<-as.data.frame(knn.impute( as.matrix(p2017_2_COLEGIO_ESTRATO), k = 10))


saveRDS(p2014_2_COLEGIO_ESTRATO,file = "p2014_2_COLEGIO_ESTRATO.rds")
saveRDS(p2015_1_COLEGIO_ESTRATO,file = "p2015_1_COLEGIO_ESTRATO.rds")
saveRDS(p2015_2_COLEGIO_ESTRATO,file = "p2015_2_COLEGIO_ESTRATO.rds")
saveRDS(p2016_1_COLEGIO_ESTRATO,file = "p2016_1_COLEGIO_ESTRATO.rds")
saveRDS(p2016_2_COLEGIO_ESTRATO,file = "p2016_2_COLEGIO_ESTRATO.rds")
saveRDS(p2017_1_COLEGIO_ESTRATO,file = "p2017_1_COLEGIO_ESTRATO.rds")
saveRDS(p2017_2_COLEGIO_ESTRATO,file = "p2017_2_COLEGIO_ESTRATO.rds")

saveRDS(p2018_1_COLEGIO_ESTRATO,file = "p2018_1_COLEGIO_ESTRATO.rds")
saveRDS(p2018_2_COLEGIO_ESTRATO,file = "p2018_2_COLEGIO_ESTRATO.rds")
saveRDS(p2019_2_COLEGIO_ESTRATO,file = "p2019_2_COLEGIO_ESTRATO.rds")


p2014_2_COLEGIO_ESTRATO<-read_rds(file = "p2014_2_COLEGIO_ESTRATO.rds")
p2015_1_COLEGIO_ESTRATO<-read_rds(file = "p2015_1_COLEGIO_ESTRATO.rds")
p2015_2_COLEGIO_ESTRATO<-read_rds(file = "p2015_2_COLEGIO_ESTRATO.rds")
p2016_1_COLEGIO_ESTRATO<-read_rds(file = "p2016_1_COLEGIO_ESTRATO.rds")
p2016_2_COLEGIO_ESTRATO<-read_rds(file = "p2016_2_COLEGIO_ESTRATO.rds")
p2017_1_COLEGIO_ESTRATO<-read_rds(file = "p2017_1_COLEGIO_ESTRATO.rds")
p2017_2_COLEGIO_ESTRATO<-read_rds(file = "p2017_2_COLEGIO_ESTRATO.rds")

p2018_1_COLEGIO_ESTRATO<-read_rds(file = "p2018_1_COLEGIO_ESTRATO.rds")
p2018_2_COLEGIO_ESTRATO<-read_rds(file = "p2018_2_COLEGIO_ESTRATO.rds")
p2019_2_COLEGIO_ESTRATO<-read_rds(file = "p2019_2_COLEGIO_ESTRATO.rds")

p2018_1_COLEGIO_ESTRATO<-as.data.frame(knn.impute( as.matrix(p2018_1_COLEGIO_ESTRATO), k = 10))
p2018_2_COLEGIO_ESTRATO<-as.data.frame(knn.impute( as.matrix(p2018_2_COLEGIO_ESTRATO), k = 10))
p2019_2_COLEGIO_ESTRATO<-as.data.frame(knn.impute( as.matrix(p2019_2_COLEGIO_ESTRATO), k = 10))




Saber11_COLEGIO_ESTRATO<-bind_rows(p2014_2_COLEGIO_ESTRATO,p2015_1_COLEGIO_ESTRATO,p2015_2_COLEGIO_ESTRATO,p2016_1_COLEGIO_ESTRATO,p2016_2_COLEGIO_ESTRATO,
                               p2017_1_COLEGIO_ESTRATO,p2017_2_COLEGIO_ESTRATO,p2018_1_COLEGIO_ESTRATO,p2018_2_COLEGIO_ESTRATO,p2019_2_COLEGIO_ESTRATO)

saveRDS(Saber11_COLEGIO_ESTRATO,file = "Saber11_COLEGIO_ESTRATO.rds")
