library(tidyverse)
library(data.table)
library(lubridate)
library(writexl)
library(gt)
library(eeptools)
library(iNZightTools)
library(scales)
library(naniar)
library(stargazer)
library(sandwich)
library(plm)
library(lmtest)
library(readxl)


####DATASET PRELIMINAR CANDIDATOS####

####variables####


vars_cand <- read_excel("Databases/candidate's data/VariablesElections2020.xlsx", 
                        sheet = "candidatos")
vars_cand_i = filter(vars_cand, Included == 1)

vars_munzona = read_excel("Databases/candidate's data/VariablesElections2020.xlsx", 
                          sheet = "munzona")
vars_munzona_i = filter(vars_munzona, Included == 1)

###Muestra: ACRE####

#Cargo los dataset para los diferentes años

ac_18_cand <- read_excel("Databases/candidate's data/consulta_cand_2018/consulta_cand_2018_AC.xlsx")

ac_20_cand <- read_excel("Databases/candidate's data/consulta_cand_2020/consulta_cand_2020_AC.xlsx")


#Investigación del dataset

glimpse(ac_18_cand)

unique(ac_18_cand$DS_ELEICAO)
unique(ac_20_cand$DS_ELEICAO)

unique(ac_18_cand$NM_TIPO_ELEICAO)
unique(ac_20_cand$NM_TIPO_ELEICAO)

unique(ac_18_cand$NR_TURNO)
unique(ac_20_cand$NR_TURNO)

unique(ac_18_cand$DS_CARGO)
unique(ac_20_cand$DS_CARGO)

#It seems better to compare between 2016 and 2020
#Cargo el dataset de 2016

ac_16_cand <- read_excel("Databases/candidate's data/consulta_cand_2016/consulta_cand_2016_AC.xlsx")

#Analizo el dataset

unique(ac_16_cand$DS_CARGO)

unique(ac_16_cand$TP_ABRANGENCIA)
unique(ac_20_cand$TP_ABRANGENCIA)

ls(ac_16_cand) 

length(setdiff(c(ls(ac_20_cand)), c(ls(ac_16_cand))))

#Cargo los datos de munzona

ac_20_munzona <- read_excel("Databases/candidate's data/votacao_candidato_munzona_2020/votacao_candidato_munzona_2020_AC.xlsx", 
                                                sheet = "votacao_candidato_munzona_2020_")
ac_16_munzona <- read_excel("Databases/candidate's data/votacao_candidato_munzona_2016/votacao_candidato_munzona_2016_AC.xlsx", 
                            sheet = "votacao_candidato_munzona_2016_")

unique(ac_20_cand$NM_TIPO_DESTINACAO_VOTOS)

setdiff(c(ls(ac_20_munzona)), c(ls(ac_20_cand)))
unique(ac_20_cand$DS_SITUACAO_CANDIDATO_TOT)
unique(ac_20_cand$DS_SITUACAO_CANDIDATO_URNA)

unique(ac_20_cand$DS_SIT_TOT_TURNO)

unique(ac_20_cand$DS_SITUACAO_CANDIDATURA)
unique(ac_20_cand$DS_DETALHE_SITUACAO_CAND)

unique(ac_20_cand$DS_SITUACAO_CANDIDATO_PLEITO)

unique(ac_20_cand$DS_SITUACAO_CANDIDATO_URNA)

unique(ac_20_cand$NR_FEDERACAO)
unique(ac_20_cand$ST_PREST_CONTAS)
unique(ac_20_cand$NM_URNA_CANDIDATO)


##ACRE 2020####

ac_20_cand_i = ac_20_cand[c(vars_cand_i$Variável)]

ac_20_munzona_i = ac_20_munzona[c(vars_munzona_i$Variável)]


##Exploro un poco las variables

names(ac_20_cand_i)


names(ac_20_munzona_i)

ac_20_cand_i %>% count(DS_COR_RACA)
ac_20_cand_i %>% count(DS_SITUACAO_CANDIDATURA)

#I could drop the candidates that are unfit to go to elections

summary(ac_20_munzona_i$QT_VOTOS_NOMINAIS)

count_cpf = ac_20_cand_i %>% count(NR_CPF_CANDIDATO)

unique(count_cpf$n)

unique(ac_20_munzona$NR_ZONA)
ac_20

ac_20_munzona_i = mutate(ac_20_munzona_i, identif_obs = 10000*CD_MUNICIPIO + 1000*NR_ZONA + SQ_CANDIDATO)

count_sqcand = ac_20_cand_i %>% count(SQ_CANDIDATO)
unique(count_sqcand$n)

#Another problem: Dif number of observations in munzona and candidates
#I'll merge the datsets

ac_20 = merge(ac_20_munzona_i,ac_20_cand_i,by=c("SQ_CANDIDATO","NR_TURNO"))

intersect = c(intersect(c(names(ac_20_cand_i)), c(names(ac_20_munzona_i))))

names(ac_20)

intersect_x <- paste(intersect, ".x", sep="")

ac_20 = ac_20[,!(names(ac_20) %in% c(intersect_x))]

names(ac_20)

vars_todo <- read_excel("Databases/candidate's data/VariablesElections2020.xlsx", 
                                     sheet = "todo")

vars_todo = filter(vars_todo, Included == 1)

intersect_y = paste(intersect, ".y", sep="")

intersect_y_bis =
  intersect_y[! intersect_y %in% c('NR_TURNO.y', 'SQ_CANDIDATO.y')]

intersect_bis = 
  intersect[! intersect %in% c('NR_TURNO', 'SQ_CANDIDATO')]
  
setnames(ac_20, old = c(intersect_y_bis), new = intersect_bis)

#Elimino las variables de FEDERACAO

unique(ac_20$NR_FEDERACAO)
unique(ac_20$NM_FEDERACAO)

ac_20 = ac_20[,!(names(ac_20) %in% c('NR_FEDERACAO', 'NM_FEDERACAO'))]

##ACRE 2016####

vars_cand_i = 
  filter(vars_cand_i, Variável != 'NR_FEDERACAO' & Variável != 'NM_FEDERACAO')

ac_16_cand_i = ac_16_cand[c(vars_cand_i$Variável)]

ac_16_munzona_i = ac_16_munzona[c(vars_munzona_i$Variável)]

#Exploro un poco

countsq_16_ac = ac_16_munzona_i %>% count(SQ_CANDIDATO)

unique(countsq_16_ac$n)

unique(ac_16_munzona_i$NR_TURNO)

rio_branco = filter(ac_16_munzona_i, NM_MUNICIPIO == 'RIO BRANCO')

#Hago el merging por 

names(ac_16_munzona_i)

names(ac_16_cand_i)

ac_16 = merge(ac_16_munzona_i,ac_16_cand_i,by=c("SQ_CANDIDATO","NR_TURNO"))
dim(ac_16)

rio_branco_2 = filter(ac_16, NM_MUNICIPIO == 'RIO BRANCO')

rio_branco_20 = filter(ac_20, NM_MUNICIPIO == 'RIO BRANCO')

ac_16 = ac_16[,!(names(ac_16) %in% c(intersect_x))]

setnames(ac_16, old = c(intersect_y_bis), new = intersect_bis)

ac_16 = ac_16[,!(names(ac_16) %in% c('NR_FEDERACAO', 'NM_FEDERACAO'))]

setdiff(c(names(ac_20)), c(names(ac_16)))

ac_20 = subset(ac_20, select = -c(identif_obs) )

###Ahora pego lo de 2020 abajo de lo de 2016

ac_16_20 = bind_rows(ac_16, ac_20) 

count_sq_2 = ac_16_20 %>% count(SQ_CANDIDATO)

unique(count_sq_2$n)

countsq_16_ac = ac_16 %>% count(SQ_CANDIDATO)

unique(countsq_16_ac$n)

countsq_20_ac = ac_20 %>% count(SQ_CANDIDATO)

unique(countsq_20_ac$n)

count_cpf = ac_16_20 %>% count(NR_CPF_CANDIDATO)

unique(count_cpf$n)

unique(ac_16$NR_CPF_CANDIDATO)
unique(ac_20$NR_CPF_CANDIDATO)
unique(ac_20$DS_COMPOSICAO_COLIGACAO)
unique(ac_20$TP_ABRANGENCIA)
unique(ac_20$CD_TIPO_ELEICAO)
unique(ac_20$VR_DESPESA_MAX_CAMPANHA)

####Código prolijo####





####TODO BRASIL####

brasil_cand_20 <- read_excel("Databases/candidate's data/consulta_cand_2020/consulta_cand_2020_BRASIL.csv.xlsx", 
                             sheet = "codes_filtered")
brasil_resultados_20 = read_excel("Databases/candidate's data/votacao_candidato_munzona_2020/votacao_candidato_munzona_2020_BRASIL.xlsx", 
                                  sheet = "Codes")
brasil_cand_16 = read_excel("Databases/candidate's data/consulta_cand_2016/consulta_cand_2016_BRASIL_1.xlsx", 
                            sheet = "codes")
brasil_resultados_16 = read_excel("Databases/candidate's data/votacao_candidato_munzona_2016/votacao_candidato_munzona_2016_BRASIL.xlsx", 
                                  sheet = "codes")

results_turno_1_16 = filter(brasil_resultados_16, NR_TURNO == 1)
rm(brasil_resultados_16)

results_turno_1_20 = filter(brasil_resultados_20, NR_TURNO == 1)
rm(brasil_resultados_20)

####Emprolijamiento####
###Mergeo los dataset

brasil_20 = 
  merge(brasil_resultados_20,brasil_cand_20,by=c("SQ_CANDIDATO","NR_TURNO"))
rm(brasil_resultados_20)
rm(brasil_cand_20)
brasil_16 = 
  merge(brasil_resultados_16,brasil_cand_16,by=c("SQ_CANDIDATO","NR_TURNO"))
rm(brasil_resultados_16)
rm(brasil_cand_16)

#saco los candidatos que no aparecen en ambas elecciones

brasil_20 = 
  subset(brasil_20, NR_CPF_CANDIDATO %in% c(brasil_16$NR_CPF_CANDIDATO))

brasil_16 = 
  subset(brasil_16, NR_CPF_CANDIDATO %in% c(brasil_20$NR_CPF_CANDIDATO))

cant_cand = length(c(unique(brasil_16$NR_CPF_CANDIDATO)))
length(c(unique(brasil_20$NR_CPF_CANDIDATO)))

##renombro las variables de ambos dataset


names(brasil_16)
names_16 = paste(c(names(brasil_16)), "_16", sep="")
names_16

names_20 = paste(c(names(brasil_20)), "_20", sep="")

setnames(brasil_16, old = c(names(brasil_16)), 
         new = c(names_16))
setnames(brasil_20, old = c(names(brasil_20)), 
         new = c(names_20))

colnames(brasil_16)[colnames(brasil_16) == "NR_CPF_CANDIDATO_16"] = "NR_CPF_CANDIDATO"
colnames(brasil_20)[colnames(brasil_20) == "NR_CPF_CANDIDATO_20"] = "NR_CPF_CANDIDATO"

###Cambio caracteristicas de algunas de las variables

#genero

brasil_16 <- brasil_16 %>% 
  mutate(female = case_when(CD_GENERO_16 == 4 ~ 1,
                            CD_GENERO_16 == 2 ~ 0),
         .after = CD_GENERO_16)

brasil_16 = subset(brasil_16, select = -c(CD_GENERO_16) )

colnames(brasil_16)[colnames(brasil_16) == "female"] = "CD_GENERO_16"


brasil_20 <- brasil_20 %>% 
  mutate(female = case_when(CD_GENERO_20 == 4 ~ 1,
                            CD_GENERO_20 == 2 ~ 0),
         .after = CD_GENERO_20)

brasil_20 = subset(brasil_20, select = -c(CD_GENERO_20) )

colnames(brasil_20)[colnames(brasil_20) == "female"] = "CD_GENERO_20"

#CoR raca. Cambio el 2 por el 3 y el 3 por el 2

brasil_16 <- brasil_16 %>% 
  mutate(race = case_when(CD_COR_RACA_16 == 1 ~ 1,
                          CD_COR_RACA_16 == 2 ~ 3,
                          CD_COR_RACA_16 == 3 ~ 2,
                          CD_COR_RACA_16 == 4 ~ 4,
                          CD_COR_RACA_16 == 5 ~ 5),
         .after = CD_COR_RACA_16)

brasil_16 = subset(brasil_16, select = -c(CD_COR_RACA_16) )

colnames(brasil_16)[colnames(brasil_16) == "race"] = "CD_COR_RACA_16"


brasil_20 <- brasil_20 %>% 
  mutate(race = case_when(CD_COR_RACA_20 == 1 ~ 1,
                          CD_COR_RACA_20 == 2 ~ 3,
                          CD_COR_RACA_20 == 3 ~ 2,
                          CD_COR_RACA_20 == 4 ~ 4,
                          CD_COR_RACA_20 == 5 ~ 5,
                          CD_COR_RACA_20 == 6 ~ 6),
         .after = CD_COR_RACA_20)

brasil_20 = subset(brasil_20, select = -c(CD_COR_RACA_20) )

colnames(brasil_20)[colnames(brasil_20) == "race"] = "CD_COR_RACA_20"

#reeleicao. S por 1 y N por 0

unique(brasil_16$ST_REELEICAO_16)
unique(brasil_20$ST_REELEICAO_20)

brasil_16 <- brasil_16 %>% 
  mutate(race = case_when(ST_REELEICAO_16 == "S" ~ 1,
                          ST_REELEICAO_16 == "N" ~ 0),
         .after = ST_REELEICAO_16)

brasil_16 = subset(brasil_16, select = -c(ST_REELEICAO_16) )

colnames(brasil_16)[colnames(brasil_16) == "race"] = "ST_REELEICAO_16"


brasil_20 <- brasil_20 %>% 
  mutate(race = case_when(ST_REELEICAO_20 == "S" ~ 1,
                          ST_REELEICAO_20 == "N" ~ 0),
         .after = ST_REELEICAO_20)

brasil_20 = subset(brasil_20, select = -c(ST_REELEICAO_20) )

colnames(brasil_20)[colnames(brasil_20) == "race"] = "ST_REELEICAO_20"

#ST DECLARAR BENS. 

unique(brasil_16$ST_DECLARAR_BENS_16)
unique(brasil_20$ST_DECLARAR_BENS_20)

brasil_16 <- brasil_16 %>% 
  mutate(race = case_when(ST_DECLARAR_BENS_16 == "S" ~ 1,
                          ST_DECLARAR_BENS_16 == "N" ~ 0),
         .after = ST_DECLARAR_BENS_16)

brasil_16 = subset(brasil_16, select = -c(ST_DECLARAR_BENS_16) )

colnames(brasil_16)[colnames(brasil_16) == "race"] = "ST_DECLARAR_BENS_16"

brasil_20 <- brasil_20 %>% 
  mutate(race = case_when(ST_DECLARAR_BENS_20 == "S" ~ 1,
                          ST_DECLARAR_BENS_20 == "N" ~ 0),
         .after = ST_DECLARAR_BENS_20)

brasil_20 = subset(brasil_20, select = -c(ST_DECLARAR_BENS_20) )

colnames(brasil_20)[colnames(brasil_20) == "race"] = "ST_DECLARAR_BENS_20"


###Mergeo los dataset de acuerdo al numnero de documento

brasil_preliminar = 
  merge(brasil_16,brasil_20,by=c("NR_CPF_CANDIDATO"))

####Contruyo el dataset

names16 = gsub('_16', '', names_16)
names16
names20 = gsub('_20', '', names_20)
names20



colnames(brasil_16)[colnames(brasil_16) == "NR_CPF_CANDIDATO"] = "NR_CPF_CANDIDATO_16"
colnames(brasil_20)[colnames(brasil_20) == "NR_CPF_CANDIDATO"] = "NR_CPF_CANDIDATO_20"
setnames(brasil_16, old = c(names_16), 
         new = c(names16))
setnames(brasil_20, old = c(names_20), 
         new = c(names20))

brasil = bind_rows(brasil_16, brasil_20)
rm(brasil_16)
rm(brasil_20)
rm(names_20)
rm(names_16)
rm(names16)
rm(names20)


#Agrego columna que sea edad (panel)

typeof(brasil$DT_NASCIMENTO[1])

summary(brasil$DT_NASCIMENTO)

brasil[brasil == "2968-12-12"] <- "1968-12-12"

brasil = mutate(brasil,
                DT_NASCIMENTO = as.Date(DT_NASCIMENTO, format= "%y-%m-%d"))

brasil = mutate(brasil,
                age = ifelse(ANO_ELEICAO == 2016,
                             as.numeric(difftime("2016-10-30", DT_NASCIMENTO,
                                                 unit="weeks"))/52.25,
                             as.numeric(difftime("2020-11-15", DT_NASCIMENTO,
                                                 unit="weeks"))/52.25))


#Sigo...

zonas = 
  brasil %>%
  group_by(NR_CPF_CANDIDATO) %>%
  summarise(cant_mun = length(unique(NR_ZONA)))

municip = 
  brasil %>%
  group_by(NR_CPF_CANDIDATO) %>%
  summarise(cant_mun = length(unique(CD_MUNICIPIO)))

nrow(filter(municip, cant_mun == 2))
nrow(municip) - nrow(filter(municip, cant_mun == 2))
cands_dos_mun = c(filter(municip, cant_mun == 2)$NR_CPF_CANDIDATO)

brasil_cands_dos_mun = filter(brasil, NR_CPF_CANDIDATO %in% cands_dos_mun)

charac_cands_dos_mun =
  brasil_cands_dos_mun %>%
  group_by(NR_CPF_CANDIDATO, ANO_ELEICAO, CD_MUNICIPIO) %>%
  summarise(votes = sum(QT_VOTOS_NOMINAIS))

nrow(charac_cands_dos_mun)/2

rm(brasil_cands_dos_mun)
rm(totales)
rm(zonas)
rm(municip)
rm(charac_cands_dos_mun)

names(brasil)


####Variables race switch####
#veo quienes cambiaron de raza

brasil_preliminar =
  mutate(brasil_preliminar, switched = ifelse(CD_COR_RACA_16 == CD_COR_RACA_20, 0, 1))

glimpse(brasil_preliminar)

switchers = c(unique(filter(brasil_preliminar, switched == 1)$NR_CPF_CANDIDATO))

n_switchers = length(switchers)

n_switchers/cant_cand

#hago lo mismo para saber quien cambio a que

brasil_preliminar =
  mutate(brasil_preliminar, r12 =
           ifelse(CD_COR_RACA_16 == 1 & CD_COR_RACA_20 == 2, 1, 0))
r12 = c(unique(filter(brasil_preliminar, r12 == 1)$NR_CPF_CANDIDATO))
brasil_preliminar = subset(brasil_preliminar, select = -c(r12) )

brasil_preliminar =
  mutate(brasil_preliminar, r13 =
           ifelse(CD_COR_RACA_16 == 1 & CD_COR_RACA_20 == 3, 1, 0))
r13 = c(unique(filter(brasil_preliminar, r13 == 1)$NR_CPF_CANDIDATO))
brasil_preliminar = subset(brasil_preliminar, select = -c(r13) )

brasil_preliminar =
  mutate(brasil_preliminar, r11 =
           ifelse(CD_COR_RACA_16 == 1 & CD_COR_RACA_20 == 1, 1, 0))
r11 = c(unique(filter(brasil_preliminar, r11 == 1)$NR_CPF_CANDIDATO))
brasil_preliminar = subset(brasil_preliminar, select = -c(r11) )

brasil_preliminar =
  mutate(brasil_preliminar, r14 =
           ifelse(CD_COR_RACA_16 == 1 & CD_COR_RACA_20 == 4, 1, 0))
r14 = c(unique(filter(brasil_preliminar, r14 == 1)$NR_CPF_CANDIDATO))
brasil_preliminar = subset(brasil_preliminar, select = -c(r14) )

brasil_preliminar =
  mutate(brasil_preliminar, r15 =
           ifelse(CD_COR_RACA_16 == 1 & CD_COR_RACA_20 == 5, 1, 0))
r15 = c(unique(filter(brasil_preliminar, r15 == 1)$NR_CPF_CANDIDATO))
brasil_preliminar = subset(brasil_preliminar, select = -c(r15) )

brasil_preliminar =
  mutate(brasil_preliminar, r21 =
           ifelse(CD_COR_RACA_16 == 2 & CD_COR_RACA_20 == 1, 1, 0))
r21 = c(unique(filter(brasil_preliminar, r21 == 1)$NR_CPF_CANDIDATO))
brasil_preliminar = subset(brasil_preliminar, select = -c(r21) )

brasil_preliminar =
  mutate(brasil_preliminar, r22 =
           ifelse(CD_COR_RACA_16 == 2 & CD_COR_RACA_20 == 2, 1, 0))
r22 = c(unique(filter(brasil_preliminar, r22 == 1)$NR_CPF_CANDIDATO))
brasil_preliminar = subset(brasil_preliminar, select = -c(r22) )

brasil_preliminar =
  mutate(brasil_preliminar, r23 =
           ifelse(CD_COR_RACA_16 == 2 & CD_COR_RACA_20 == 3, 1, 0))
r23 = c(unique(filter(brasil_preliminar, r23 == 1)$NR_CPF_CANDIDATO))
brasil_preliminar = subset(brasil_preliminar, select = -c(r23) )

brasil_preliminar =
  mutate(brasil_preliminar, r24 =
           ifelse(CD_COR_RACA_16 == 2 & CD_COR_RACA_20 == 4, 1, 0))
r24 = c(unique(filter(brasil_preliminar, r24 == 1)$NR_CPF_CANDIDATO))
brasil_preliminar = subset(brasil_preliminar, select = -c(r24) )

brasil_preliminar =
  mutate(brasil_preliminar, r25 =
           ifelse(CD_COR_RACA_16 == 2 & CD_COR_RACA_20 == 5, 1, 0))
r25 = c(unique(filter(brasil_preliminar, r25 == 1)$NR_CPF_CANDIDATO))
brasil_preliminar = subset(brasil_preliminar, select = -c(r25) )

brasil_preliminar =
  mutate(brasil_preliminar, r31 =
           ifelse(CD_COR_RACA_16 == 3 & CD_COR_RACA_20 == 1, 1, 0))
r31 = c(unique(filter(brasil_preliminar, r31 == 1)$NR_CPF_CANDIDATO))
brasil_preliminar = subset(brasil_preliminar, select = -c(r31) )

brasil_preliminar =
  mutate(brasil_preliminar, r32 =
           ifelse(CD_COR_RACA_16 == 3 & CD_COR_RACA_20 == 2, 1, 0))
r32 = c(unique(filter(brasil_preliminar, r32 == 1)$NR_CPF_CANDIDATO))
brasil_preliminar = subset(brasil_preliminar, select = -c(r32) )

brasil_preliminar =
  mutate(brasil_preliminar, r33 =
           ifelse(CD_COR_RACA_16 == 3 & CD_COR_RACA_20 == 3, 1, 0))
r33 = c(unique(filter(brasil_preliminar, r33 == 1)$NR_CPF_CANDIDATO))
brasil_preliminar = subset(brasil_preliminar, select = -c(r33) )

brasil_preliminar =
  mutate(brasil_preliminar, r34 =
           ifelse(CD_COR_RACA_16 == 3 & CD_COR_RACA_20 == 4, 1, 0))
r34 = c(unique(filter(brasil_preliminar, r34 == 1)$NR_CPF_CANDIDATO))
brasil_preliminar = subset(brasil_preliminar, select = -c(r34) )

brasil_preliminar =
  mutate(brasil_preliminar, r35 =
           ifelse(CD_COR_RACA_16 == 3 & CD_COR_RACA_20 == 5, 1, 0))
r35 = c(unique(filter(brasil_preliminar, r35 == 1)$NR_CPF_CANDIDATO))
brasil_preliminar = subset(brasil_preliminar, select = -c(r35) )

brasil_preliminar =
  mutate(brasil_preliminar, r41 =
           ifelse(CD_COR_RACA_16 == 4 & CD_COR_RACA_20 == 1, 1, 0))
r41 = c(unique(filter(brasil_preliminar, r41 == 1)$NR_CPF_CANDIDATO))
brasil_preliminar = subset(brasil_preliminar, select = -c(r41) )

brasil_preliminar =
  mutate(brasil_preliminar, r42 =
           ifelse(CD_COR_RACA_16 == 4 & CD_COR_RACA_20 == 2, 1, 0))
r42 = c(unique(filter(brasil_preliminar, r42 == 1)$NR_CPF_CANDIDATO))
brasil_preliminar = subset(brasil_preliminar, select = -c(r42) )

brasil_preliminar =
  mutate(brasil_preliminar, r43 =
           ifelse(CD_COR_RACA_16 == 4 & CD_COR_RACA_20 == 3, 1, 0))
r43 = c(unique(filter(brasil_preliminar, r43 == 1)$NR_CPF_CANDIDATO))
brasil_preliminar = subset(brasil_preliminar, select = -c(r43) )

brasil_preliminar =
  mutate(brasil_preliminar, r44 =
           ifelse(CD_COR_RACA_16 == 4 & CD_COR_RACA_20 == 4, 1, 0))
r44 = c(unique(filter(brasil_preliminar, r44 == 1)$NR_CPF_CANDIDATO))
brasil_preliminar = subset(brasil_preliminar, select = -c(r44) )

brasil_preliminar =
  mutate(brasil_preliminar, r45 =
           ifelse(CD_COR_RACA_16 == 4 & CD_COR_RACA_20 == 5, 1, 0))
r45 = c(unique(filter(brasil_preliminar, r45 == 1)$NR_CPF_CANDIDATO))
brasil_preliminar = subset(brasil_preliminar, select = -c(r45) )

brasil_preliminar =
  mutate(brasil_preliminar, r51 =
           ifelse(CD_COR_RACA_16 == 5 & CD_COR_RACA_20 == 1, 1, 0))
r51 = c(unique(filter(brasil_preliminar, r51 == 1)$NR_CPF_CANDIDATO))
brasil_preliminar = subset(brasil_preliminar, select = -c(r51) )

brasil_preliminar =
  mutate(brasil_preliminar, r52 =
           ifelse(CD_COR_RACA_16 == 5 & CD_COR_RACA_20 == 2, 1, 0))
r52 = c(unique(filter(brasil_preliminar, r52 == 1)$NR_CPF_CANDIDATO))
brasil_preliminar = subset(brasil_preliminar, select = -c(r52) )

brasil_preliminar =
  mutate(brasil_preliminar, r53 =
           ifelse(CD_COR_RACA_16 == 5 & CD_COR_RACA_20 == 3, 1, 0))
r53 = c(unique(filter(brasil_preliminar, r53 == 1)$NR_CPF_CANDIDATO))
brasil_preliminar = subset(brasil_preliminar, select = -c(r53) )

brasil_preliminar =
  mutate(brasil_preliminar, r54 =
           ifelse(CD_COR_RACA_16 == 5 & CD_COR_RACA_20 == 4, 1, 0))
r54 = c(unique(filter(brasil_preliminar, r54 == 1)$NR_CPF_CANDIDATO))
brasil_preliminar = subset(brasil_preliminar, select = -c(r54) )

brasil_preliminar =
  mutate(brasil_preliminar, r55 =
           ifelse(CD_COR_RACA_16 == 5 & CD_COR_RACA_20 == 5, 1, 0))
r55 = c(unique(filter(brasil_preliminar, r55 == 1)$NR_CPF_CANDIDATO))
brasil_preliminar = subset(brasil_preliminar, select = -c(r55) )

brasil_preliminar =
  mutate(brasil_preliminar, r16 =
           ifelse(CD_COR_RACA_16 == 1 & CD_COR_RACA_20 == 6, 1, 0))
r16 = c(unique(filter(brasil_preliminar, r16 == 1)$NR_CPF_CANDIDATO))
brasil_preliminar = subset(brasil_preliminar, select = -c(r16) )

brasil_preliminar =
  mutate(brasil_preliminar, r26 =
           ifelse(CD_COR_RACA_16 == 2 & CD_COR_RACA_20 == 6, 1, 0))
r26 = c(unique(filter(brasil_preliminar, r26 == 1)$NR_CPF_CANDIDATO))
brasil_preliminar = subset(brasil_preliminar, select = -c(r26) )

brasil_preliminar =
  mutate(brasil_preliminar, r36 =
           ifelse(CD_COR_RACA_16 == 3 & CD_COR_RACA_20 == 6, 1, 0))
r36 = c(unique(filter(brasil_preliminar, r36 == 1)$NR_CPF_CANDIDATO))
brasil_preliminar = subset(brasil_preliminar, select = -c(r36) )

brasil_preliminar =
  mutate(brasil_preliminar, r46 =
           ifelse(CD_COR_RACA_16 == 4 & CD_COR_RACA_20 == 6, 1, 0))
r46 = c(unique(filter(brasil_preliminar, r46 == 1)$NR_CPF_CANDIDATO))
brasil_preliminar = subset(brasil_preliminar, select = -c(r46) )

brasil_preliminar =
  mutate(brasil_preliminar, r56 =
           ifelse(CD_COR_RACA_16 == 5 & CD_COR_RACA_20 == 6, 1, 0))
r56 = c(unique(filter(brasil_preliminar, r56 == 1)$NR_CPF_CANDIDATO))
brasil_preliminar = subset(brasil_preliminar, select = -c(r56) )




rm(brasil_preliminar)



#Meto las variables de cambio de raza (panel)

brasil = mutate(brasil, switched = ifelse(NR_CPF_CANDIDATO %in% switchers,1,0))

brasil = mutate(brasil, r11 = ifelse(NR_CPF_CANDIDATO %in% rr11,1,0))

brasil = mutate(brasil, r12 = ifelse(NR_CPF_CANDIDATO %in% rr12,1,0))

brasil = mutate(brasil, r13 = ifelse(NR_CPF_CANDIDATO %in% rr13,1,0))

brasil = mutate(brasil, r21 = ifelse(NR_CPF_CANDIDATO %in% rr21,1,0))

brasil = mutate(brasil, r22 = ifelse(NR_CPF_CANDIDATO %in% rr22,1,0))

brasil = mutate(brasil, r23 = ifelse(NR_CPF_CANDIDATO %in% rr23,1,0))

brasil = mutate(brasil, r31 = ifelse(NR_CPF_CANDIDATO %in% rr31,1,0))

brasil = mutate(brasil, r32 = ifelse(NR_CPF_CANDIDATO %in% rr32,1,0))

brasil = mutate(brasil, r33 = ifelse(NR_CPF_CANDIDATO %in% rr33,1,0))

n_switchers / cant_cand

sum(brasil_long$r11)

#hacer con el dataset long 

brasil_long = mutate(brasil_long, switched = ifelse(NR_CPF_CANDIDATO %in% switchers,1,0))

brasil_long = mutate(brasil_long, r11 = ifelse(NR_CPF_CANDIDATO %in% rr11,1,0))

brasil_long = mutate(brasil_long, r12 = ifelse(NR_CPF_CANDIDATO %in% rr12,1,0))

brasil_long = mutate(brasil_long, r13 = ifelse(NR_CPF_CANDIDATO %in% rr13,1,0))

brasil_long = mutate(brasil_long, r21 = ifelse(NR_CPF_CANDIDATO %in% rr21,1,0))

brasil_long = mutate(brasil_long, r22 = ifelse(NR_CPF_CANDIDATO %in% rr22,1,0))

brasil_long = mutate(brasil_long, r23 = ifelse(NR_CPF_CANDIDATO %in% rr23,1,0))

brasil_long = mutate(brasil_long, r31 = ifelse(NR_CPF_CANDIDATO %in% rr31,1,0))

brasil_long = mutate(brasil_long, r32 = ifelse(NR_CPF_CANDIDATO %in% rr32,1,0))

brasil_long = mutate(brasil_long, r33 = ifelse(NR_CPF_CANDIDATO %in% rr33,1,0))


summary(brasil_long$r11)
summary(brasil_long$switched)
summary(brasil_long$r32)


####Dataset charac (ahora brasil)####

charac = 
brasil %>%
  group_by(NR_CPF_CANDIDATO, ANO_ELEICAO, NR_TURNO) %>%
  summarise(age = mean(age),
            educ_level = unique(CD_GRAU_INSTRUCAO)[1],
            gender = unique(CD_GENERO)[1],
            race = unique(CD_COR_RACA)[1],
            SQ_CANDIDATO = unique(SQ_CANDIDATO)[1],
            CD_TIPO_ELEICAO = unique(CD_TIPO_ELEICAO)[1],
            CD_CARGO = unique(CD_CARGO)[1],
            NR_PARTIDO=unique(NR_PARTIDO)[1],
            CD_NACIONALIDADE=unique(CD_NACIONALIDADE)[1],
            CD_SIT_TOT_TURNO=unique(CD_SIT_TOT_TURNO)[1],
            SG_UF_NASCIMENTO=unique(SG_UF_NASCIMENTO)[1],
            SG_UF=unique(SG_UF)[1],
            SQ_COLIGACAO=unique(SQ_COLIGACAO)[1],
            NM_MUNICIPIO_NASCIMENTO=unique(NM_MUNICIPIO_NASCIMENTO)[1],
            CD_OCUPACAO=unique(CD_OCUPACAO)[1],
            ST_REELEICAO=unique(ST_REELEICAO)[1],
            CD_MUNICIPIO=unique(CD_MUNICIPIO)[1],
            SG_UE=unique(SG_UE)[1],
            CD_SITUACAO_CANDIDATURA=unique(CD_SITUACAO_CANDIDATURA)[1],
            DS_COMPOSICAO_COLIGACAO=unique(DS_COMPOSICAO_COLIGACAO)[1],
            DT_NASCIMENTO=unique(DT_NASCIMENTO)[1],
            CD_ESTADO_CIVIL=unique(CD_ESTADO_CIVIL)[1],
            VR_DESPESA_MAX_CAMPANHA=unique(VR_DESPESA_MAX_CAMPANHA)[1],
            ST_DECLARAR_BENS=unique(ST_DECLARAR_BENS)[1],
            age=unique(age)[1],
            votes = sum(QT_VOTOS_NOMINAIS),
            switched=unique(switched)[1],
            NR_ZONA = n(), 
            r11 = mean(r11),
            r12 = mean(r12),
            r13 = mean(r13),
            r21 = mean(r21),
            r22 = mean(r22),
            r23 = mean(r23),
            r31 = mean(r31),
            r32 = mean(r32),
            r33 = mean(r33))

colnames(charac)[colnames(charac) == "NR_ZONA"] = "quantity_zones"

brasil = mutate(brasil, type_of_switch = case_when(r11 == 1 ~ "r11",
                                                   r12 == 1 ~ "r12",
                                                   r13 == 1 ~ "r13",
                                                   r21 == 1 ~ "r21",
                                                   r22 == 1 ~ "r22",
                                                   r23 == 1 ~ "r23",
                                                   r31 == 1 ~ "r31",
                                                   r32 == 1 ~ "r32",
                                                   r33 == 1 ~ "r33"))

brasil_long = mutate(brasil_long, type_of_switch = case_when(r11 == 1 ~ "r11",
                                                   r12 == 1 ~ "r12",
                                                   r13 == 1 ~ "r13",
                                                   r21 == 1 ~ "r21",
                                                   r22 == 1 ~ "r22",
                                                   r23 == 1 ~ "r23",
                                                   r31 == 1 ~ "r31",
                                                   r32 == 1 ~ "r32",
                                                   r33 == 1 ~ "r33"))
brasil_long %>%
  group_by(type_of_switch) %>%
  summarise(number = n())

##Sigo con lo que sigue...
#saco a aquellos que fueron a segundo turno

cands_sec_turn = c(filter(charac, CD_SIT_TOT_TURNO == 6)$NR_CPF_CANDIDATO)

charac_cands_sec_turn = filter(charac, NR_CPF_CANDIDATO %in% cands_sec_turn)

charac_cands_sec_turn_1 = 
  filter(charac_cands_sec_turn, CD_SIT_TOT_TURNO != 6) %>%
  select(NR_CPF_CANDIDATO, ANO_ELEICAO, CD_SIT_TOT_TURNO)
rm(charac_cands_sec_turn)

charac <- merge(charac, charac_cands_sec_turn_1,
                by=c("NR_CPF_CANDIDATO","ANO_ELEICAO"), all.x=TRUE)

charac$CD_SIT_TOT_TURNO.x <- 
  ifelse(is.na(charac$CD_SIT_TOT_TURNO.y), charac$CD_SIT_TOT_TURNO.x,
         charac$CD_SIT_TOT_TURNO.y)
charac$CD_SIT_TOT_TURNO.y <- NULL

unique(charac$CD_SIT_TOT_TURNO.x)
nrow(filter(charac, CD_SIT_TOT_TURNO.x == -1))
nrow(filter(charac, CD_SIT_TOT_TURNO.x == 6))

rm(charac_cands_sec_turn_1)

charac = charac[charac$NR_TURNO != 2, ]

#veo situación de los que cambiaron de municipio
#Cambio de municipio####

charac_cands_dos_mun = filter(charac, NR_CPF_CANDIDATO %in% cands_dos_mun)

perc_type_switch_two_mun = 
  charac_cands_dos_mun %>%
  group_by(type_of_switch) %>%
  summarise(n()/nrow(charac_cands_dos_mun))

perc_switchers_two_mun = 
  charac_cands_dos_mun %>%
  group_by(switched) %>%
  summarise(n()/nrow(charac_cands_dos_mun))

nrow(filter(charac_cands_dos_mun, switched == 1)) / nrow(charac_cands_dos_mun)
nrow(charac_cands_dos_mun) / nrow(charac)

rm(charac_cands_dos_mun)


###Separo en dos años

brasil = charac
rm(charac)

names(brasil)

names(brasil)[names(brasil) == 'CD_SIT_TOT_TURNO.x'] <- 'CD_SIT_TOT_TURNO'

names(brasil)[names(brasil) == 'CD_SIT_TOT_TURNO_16.x'] <- 'CD_SIT_TOT_TURNO_16'

names(brasil)[names(brasil) == 'CD_SIT_TOT_TURNO_20.x'] <- 'CD_SIT_TOT_TURNO_20'

charac_16 = filter(brasil, ANO_ELEICAO == 2016)
charac_20 = filter(brasil, ANO_ELEICAO == 2020)

##Hago el porcentaje de switched de los eleitos...
##Cosas varias####

cambio_raza_segun_resultado =
  charac_20 %>%
  group_by(CD_SIT_TOT_TURNO) %>%
  summarise(switched = mean(switched),
            r11 = mean(r11),
            r12 = mean(r12),
            r13 = mean(r13),
            r21 = mean(r21),
            r22 = mean(r22),
            r23 = mean(r23),
            r31 = mean(r31),
            r32 = mean(r32),
            r33 = mean(r33))

race_20 = 
  charac_20 %>%
  group_by(race) %>%
  summarise(n()/nrow(charac_20))

race_20_2 = 
  charac_20 %>%
  group_by(type_of_switch) %>%
  summarise(n()/nrow(charac_20))

##Cpntinuar por acá

charac_segun_race_16 = 
  charac_16 %>%
  group_by(race) %>%
  summarise(age = mean(age,  na.rm = TRUE),
            educ_level = mean(educ_level),
            gender = mean(gender))

charac_segun_race_16 = 
  filter(charac_segun_race_16, race %in% c(1, 2, 3, 4, 5, 6))

charac_segun_race_20 = 
  charac_20 %>%
  group_by(race) %>%
  summarise(age = mean(age,  na.rm = TRUE),
            educ_level = mean(educ_level),
            gender = mean(gender))

charac_segun_race_20 = 
  filter(charac_segun_race_20, race %in% c(1, 2, 3, 4, 5, 6))

#Agrupo según tipo de race switching

charac_segun_switch_20 = 
  charac_20 %>%
  group_by(type_of_switch) %>%
  summarise(age = mean(age,  na.rm = TRUE),
            educ_level = mean(educ_level),
            gender = mean(gender))

##porcentaje de blancos entre los que ganaron y perdieron en cada estado

perc_whites_segun_gano_16 =
  brasil_cor_normal_16 %>%
  filter(!is.na(won)) %>%
  group_by(SG_UF, won) %>%
  summarise(perc_whites = mean(white))

View(perc_whites_segun_gano_16)

###Vote share####

##2016

#cant de candidatos en el municpio

zones_per_cand_16 =
  results_turno_1_16 %>%
  group_by(SQ_CANDIDATO) %>%
  summarise(municip = length(unique(CD_MUNICIPIO)),
            zones = length(unique(NR_ZONA)))
rm(zones_per_cand_16)

cands_per_municipality_16 = 
  results_turno_1_16 %>%
  group_by(CD_MUNICIPIO) %>%
  summarise(quant_cands_mun_16 = length(unique(SQ_CANDIDATO)))

names(cands_per_municipality_16)[names(cands_per_municipality_16) ==
                                              'CD_MUNICIPIO'] <- 'CD_MUNICIPIO_16'

#cantidad de votantes en el municpio

voters_per_mun_16 = read_excel("Databases/electoral profile/perfil_comparecimento_abstencao_2016/voters_per_mun_16.xlsx", 
                               sheet = "Sheet1")

summary(voters_per_mun_16)

names(voters_per_mun_16)[names(voters_per_mun_16) ==
                           'CD_MUNICIPIO'] <- 'CD_MUNICIPIO_16'

##2020

#cantidad de candiadtos en el municpio

cands_per_municipality_20 = 
  results_turno_1_20 %>%
  group_by(CD_MUNICIPIO) %>%
  summarise(quant_cands_mun_20 = length(unique(SQ_CANDIDATO)))

names(cands_per_municipality_20)[names(cands_per_municipality_20) ==
                                              'CD_MUNICIPIO'] <- 'CD_MUNICIPIO_20'



#cantidad de votantes en el municpio

voters_per_mun_20 = read_excel("Databases/electoral profile/perfil_comparecimento_abstencao_2020/voters_per_mun.xlsx", 
                               sheet = "Sheet1")

summary(voters_per_mun_20)

names(voters_per_mun_20)[names(voters_per_mun_20) ==
                           'CD_MUNICIPIO'] <- 'CD_MUNICIPIO_20'


#Ahora lo mergeo al dataset wide

#cant de candidatos

names(cands_per_municipality_16)[names(cands_per_municipality_16) ==
                                              'CD_MUNICIPIO_16'] <- 'CD_MUNICIPIO'

names(cands_per_municipality_20)[names(cands_per_municipality_20) ==
                                              'CD_MUNICIPIO_20'] <- 'CD_MUNICIPIO'

brasil = merge(brasil, cands_per_municipality_16,
               by=c("CD_MUNICIPIO"), all.x=TRUE)

brasil = merge(brasil, cands_per_municipality_20,
               by=c("CD_MUNICIPIO"), all.x=TRUE)

brasil =
  mutate(brasil, quant_cands_mun = 
           ifelse(ANO_ELEICAO == 2016, quant_cands_mun_16, quant_cands_mun_20))

brasil = subset(brasil, select =
                  -c(quant_cands_mun_16, quant_cands_mun_20) )

#cant de votantes

names(voters_per_mun_16)[names(voters_per_mun_16) ==
                           'CD_MUNICIPIO_16'] <- 'CD_MUNICIPIO'

names(voters_per_mun_20)[names(voters_per_mun_20) ==
                           'CD_MUNICIPIO_20'] <- 'CD_MUNICIPIO'

brasil = merge(brasil, voters_per_mun_16,
               by=c("CD_MUNICIPIO"))

brasil = merge(brasil, voters_per_mun_20,
               by=c("CD_MUNICIPIO"))

brasil =
  mutate(brasil, quant_voters_mun = 
           ifelse(ANO_ELEICAO == 2016, quant_voters_mun_16, quant_voters_mun_20))

brasil =
  mutate(brasil, quant_aptos_mun = 
           ifelse(ANO_ELEICAO == 2016, quant_aptos_mun_16, quant_aptos_mun_20))


brasil = subset(brasil, select =
                  -c(quant_voters_mun_16, quant_voters_mun_20,
                     quant_aptos_mun_16, quant_aptos_mun_20) )

summary(brasil_16$quant_aptos_mun_16)
length(unique(brasil_16$CD_MUNICIPIO_16))
length(intersect(c(unique(brasil_16$CD_MUNICIPIO_16)), c(voters_per_mun_16$CD_MUNICIPIO)))
length(unique(voters_per_mun_16$CD_MUNICIPIO))
mean(unique(brasil_20$quant_aptos_mun_20))
mean(voters_per_mun_20$quant_aptos_mun_20)
setdiff(voters_per_mun_20$quant_aptos_mun_20, unique(brasil_20$quant_aptos_mun_20))

#No entiendo que esta pasando aca:

length(unique(brasil_20$CD_MUNICIPIO_20))
length(unique(voters_per_mun_20$CD_MUNICIPIO))
setdiff(voters_per_mun_20$quant_aptos_mun_20, unique(brasil_20$quant_aptos_mun_20))
setdiff(unique(brasil_20$quant_aptos_mun_20), voters_per_mun_20$quant_aptos_mun_20)

mean((brasil_20 %>%
  group_by(CD_MUNICIPIO_20) %>%
  summarise(mean_aptos = mean(quant_aptos_mun_20)))$mean_aptos)

#Entendí


##Calculo los vote share

brasil = mutate(brasil, vote_share = votes / quant_voters_mun)

summary(brasil$vote_share)

####Perc of candidates running twice (PCRT)####

length(unique(results_turno_1_16$SQ_CANDIDATO))

pcrt_16 = cant_cand / length(unique(results_turno_1_16$SQ_CANDIDATO))

pcrt_20 = cant_cand / length(unique(results_turno_1_20$SQ_CANDIDATO))

#Ahora lo hago por municipio

#2016


cands_rep_per_mun = 
  brasil %>%
  group_by(CD_MUNICIPIO, ANO_ELEICAO) %>%
  summarise(quant_cands_rep = n())


nrow(filter(brasil, CD_MUNICIPIO == 19 & ANO_ELEICAO == 2016))

summary(cands_rep_per_mun$cant_cands_rep)

brasil = merge(brasil, cands_rep_per_mun,
                        by=c("CD_MUNICIPIO", "ANO_ELEICAO"), all.x=TRUE)

summary(brasil$cant_cands_rep)

brasil = mutate(brasil, perc_cands_twice = quant_cands_rep / quant_cands_mun)

summary(brasil$perc_cands_twice)

#Switchers vs Non-switchers####

rm(charac_16)
rm(charac_20)

brasil_16 = filter(brasil, ANO_ELEICAO == 2016)
brasil_20 = filter(brasil, ANO_ELEICAO == 2020)

#switchers v non_switchers (solo para 2020)

characs_segun_switched = 
  brasil_20 %>%
  group_by(switched) %>%
  summarise(av_age = mean(age, na.rm = TRUE),
            sd_age = sd(age, na.rm = TRUE),
            av_educ_level = mean(CD_GRAU_INSTRUCAO),
            sd_educ_level = sd(CD_GRAU_INSTRUCAO),
            perc_women = mean(CD_GENERO),
            sd_perc_women = sd(CD_GENERO),
            perc_incumbent = mean(ST_REELEICAO),
            sd_perc_incumbent = sd(ST_REELEICAO),
            av_mun_aptos = mean(quant_aptos_mun),
            sd_mun_aptos = sd(quant_aptos_mun))

characs_segun_switched <- characs_segun_switched %>% 
  mutate(switched = ifelse(switched == 1,"Yes","No")) 

##P values tests of equality of distributions

dat_gender <-
  brasil_20 %>%
  group_by(switched) %>%
  summarise(men = sum(CD_GENERO),
            total = n())

pv_men =
  prop.test(
  x = dat_gender$men,
  n = dat_gender$total
)$p.value

dat_incumbent <-
  brasil_20 %>%
  group_by(switched) %>%
  summarise(incumbent = sum(ST_REELEICAO),
            total = n())

pv_incumb <-
  prop.test(
    x = dat_incumbent$incumbent,
    n = dat_incumbent$total
  )$p.value
pv_incumb

pv_age = (t.test(age ~ switched, data = brasil_20))$p.value

pv_voters = (t.test(quant_aptos_mun ~ switched, data = brasil_20))$p.value

pv_educ =(wilcox.test(CD_GRAU_INSTRUCAO~ switched,
                      data = brasil_20))$p.value

p_values = data.frame(switched = "p-values",
                      av_age = pv_age,
                      av_educ_level = pv_educ,
                      perc_women = pv_men,
                      perc_incumbent = pv_incumb,
                      av_mun_aptos = pv_voters)

characs_segun_switched = bind_rows(characs_segun_switched, p_values)

characs_segun_type_switch = 
  brasil_20 %>%
  group_by(type_of_switch) %>%
  summarise(av_age = mean(age, na.rm = TRUE),
            sd_age = sd(age, na.rm = TRUE),
            av_educ_level = mean(CD_GRAU_INSTRUCAO),
            sd_educ_level = sd(CD_GRAU_INSTRUCAO),
            perc_women = mean(CD_GENERO),
            sd_perc_women = sd(CD_GENERO),
            perc_incumbent = mean(ST_REELEICAO),
            sd_perc_incumbent = sd(ST_REELEICAO),
            av_vote_share = mean(vote_share),
            sd_vote_share = sd(vote_share),
            av_mun_aptos = mean(quant_aptos_mun),
            sd_mun_aptos = sd(quant_aptos_mun))

characs_segun_type_switch$type_of_switch =
  replace_na(characs_segun_type_switch$type_of_switch, "Others")

##Lo hago agregando pretos y pardos

#Meto variable type switch agregada

brasil_bis =
  brasil %>%
  mutate(type_switch_agreg = case_when(w11 == 1 ~ "w11",
                                       w10 == 1 ~ "w10",
                                       w01 == 1 ~ "w01",
                                       w00 == 1 ~ "w00"))

brasil = brasil_bis
rm(brasil_bis)

brasil_long =
  brasil_long %>%
  mutate(type_switch_agreg = case_when(w11 == 1 ~ "w11",
                                       w10 == 1 ~ "w10",
                                       w01 == 1 ~ "w01",
                                       w00 == 1 ~ "w00"))

characs_segun_type_switch_agreg = 
  brasil_20 %>%
  group_by(type_switch_agreg) %>%
  summarise(av_age = mean(age, na.rm = TRUE),
            sd_age = sd(age, na.rm = TRUE),
            av_educ_level = mean(CD_GRAU_INSTRUCAO),
            sd_educ_level = sd(CD_GRAU_INSTRUCAO),
            perc_men = mean(CD_GENERO),
            sd_perc_men = sd(CD_GENERO),
            perc_incumbent = mean(ST_REELEICAO),
            sd_perc_incumbent = sd(ST_REELEICAO),
            av_mun_aptos = mean(quant_aptos_mun),
            sd_mun_aptos = sd(quant_aptos_mun))

characs_segun_type_switch_agreg =
  characs_segun_type_switch_agreg %>%  na.omit()

characs_segun_type_switch_agreg =
  characs_segun_type_switch_agreg %>%
  mutate(type_switch_agreg = case_when(type_switch_agreg == "w11" ~ "11",
                                       type_switch_agreg == "w10" ~ "10",
                                       type_switch_agreg == "w01" ~ "01",
                                       type_switch_agreg == "w00" ~ "00"))

##P-values

#age

pv_age_11_10 =
  (t.test(age ~ w11,
          data = filter(brasil_20, w11 == 1 | w10 == 1)))$p.value

pv_age_11_01 =
  (t.test(age ~ w11,
          data = filter(brasil_20, w11 == 1 | w01 == 1)))$p.value

pv_age_11_00 =
  (t.test(age ~ w11,
          data = filter(brasil_20, w11 == 1 | w00 == 1)))$p.value

pv_age_10_01 =
  (t.test(age ~ w10,
          data = filter(brasil_20, w10 == 1 | w01 == 1)))$p.value

pv_age_10_00 =
  (t.test(age ~ w10,
          data = filter(brasil_20, w10 == 1 | w00 == 1)))$p.value

pv_age_01_00 =
  (t.test(age ~ w01,
          data = filter(brasil_20, w01 == 1 | w00 == 1)))$p.value

pv_age = c(pv_age_11_10, pv_age_11_01, pv_age_11_00, pv_age_10_01,
           pv_age_10_00, pv_age_01_00)

#Educ level

pv_educ_11_10 =
  (wilcox.test(CD_GRAU_INSTRUCAO~ w11,
               data = filter(brasil_20, w11 == 1 | w10 == 1)))$p.value

pv_educ_11_01 =
  (wilcox.test(CD_GRAU_INSTRUCAO~ w11,
               data = filter(brasil_20, w11 == 1 | w01 == 1)))$p.value

pv_educ_11_00 =
  (wilcox.test(CD_GRAU_INSTRUCAO~ w11,
               data = filter(brasil_20, w11 == 1 | w00 == 1)))$p.value

pv_educ_10_01 =
  (wilcox.test(CD_GRAU_INSTRUCAO~ w10,
               data = filter(brasil_20, w10 == 1 | w01 == 1)))$p.value

pv_educ_10_00 =
  (wilcox.test(CD_GRAU_INSTRUCAO~ w10,
               data = filter(brasil_20, w10 == 1 | w00 == 1)))$p.value

pv_educ_01_00 =
  (wilcox.test(CD_GRAU_INSTRUCAO~ w01,
               data = filter(brasil_20, w01 == 1 | w00 == 1)))$p.value

pv_educ = c(pv_educ_11_10, pv_educ_11_01, pv_educ_11_00, pv_educ_10_01,
           pv_educ_10_00, pv_educ_01_00)

#Men

dat =
  brasil_20 %>%
  group_by(type_switch_agreg) %>%
  summarise(x = sum(CD_GENERO),
            n = n())

dat =
  dat %>%
  mutate(p = x/n)

pv_men_11_10 = (prop.test(x = c(dat$x[4], dat$x[3]),
                          n = c(dat$n[4], dat$n[3])))$p.value

pv_men_11_01 = (prop.test(x = c(dat$x[4], dat$x[2]),
                          n = c(dat$n[4], dat$n[2])))$p.value

pv_men_11_00 = (prop.test(x = c(dat$x[4], dat$x[1]),
                          n = c(dat$n[4], dat$n[1])))$p.value

pv_men_10_01 = (prop.test(x = c(dat$x[3], dat$x[2]),
                          n = c(dat$n[3], dat$n[2])))$p.value

pv_men_10_00 = (prop.test(x = c(dat$x[3], dat$x[1]),
                          n = c(dat$n[3], dat$n[1])))$p.value

pv_men_01_00 = (prop.test(x = c(dat$x[2], dat$x[1]),
                          n = c(dat$n[2], dat$n[1])))$p.value

pv_men = c(pv_men_11_10, pv_men_11_01, pv_men_11_00, pv_men_10_01,
           pv_men_10_00, pv_men_01_00)

#incumbency

dat =
  brasil_20 %>%
  group_by(type_switch_agreg) %>%
  summarise(x = sum(ST_REELEICAO),
            n = n())

dat =
  dat %>%
  mutate(p = x/n)

pv_incumb_11_10 = (prop.test(x = c(dat$x[4], dat$x[3]),
                          n = c(dat$n[4], dat$n[3])))$p.value

pv_incumb_11_01 = (prop.test(x = c(dat$x[4], dat$x[2]),
                          n = c(dat$n[4], dat$n[2])))$p.value

pv_incumb_11_00 = (prop.test(x = c(dat$x[4], dat$x[1]),
                          n = c(dat$n[4], dat$n[1])))$p.value

pv_incumb_10_01 = (prop.test(x = c(dat$x[3], dat$x[2]),
                          n = c(dat$n[3], dat$n[2])))$p.value

pv_incumb_10_00 = (prop.test(x = c(dat$x[3], dat$x[1]),
                          n = c(dat$n[3], dat$n[1])))$p.value

pv_incumb_01_00 = (prop.test(x = c(dat$x[2], dat$x[1]),
                          n = c(dat$n[2], dat$n[1])))$p.value

pv_incumb = c(pv_incumb_11_10, pv_incumb_11_01, pv_incumb_11_00,
              pv_incumb_10_01,
           pv_incumb_10_00, pv_incumb_01_00)

#aptos

pv_quant_aptos_mun_11_10 =
  (t.test(quant_aptos_mun ~ w11,
          data = filter(brasil_20, w11 == 1 | w10 == 1)))$p.value

pv_quant_aptos_mun_11_01 =
  (t.test(quant_aptos_mun ~ w11,
          data = filter(brasil_20, w11 == 1 | w01 == 1)))$p.value

pv_quant_aptos_mun_11_00 =
  (t.test(quant_aptos_mun ~ w11,
          data = filter(brasil_20, w11 == 1 | w00 == 1)))$p.value

pv_quant_aptos_mun_10_01 =
  (t.test(quant_aptos_mun ~ w10,
          data = filter(brasil_20, w10 == 1 | w01 == 1)))$p.value

pv_quant_aptos_mun_10_00 =
  (t.test(quant_aptos_mun ~ w10,
          data = filter(brasil_20, w10 == 1 | w00 == 1)))$p.value

pv_quant_aptos_mun_01_00 =
  (t.test(quant_aptos_mun ~ w01,
          data = filter(brasil_20, w01 == 1 | w00 == 1)))$p.value

pv_quant_aptos_mun = c(pv_quant_aptos_mun_11_10, pv_quant_aptos_mun_11_01, pv_quant_aptos_mun_11_00, pv_quant_aptos_mun_10_01,
           pv_quant_aptos_mun_10_00, pv_quant_aptos_mun_01_00)

#Construyo tabla con los resultados

pvs_type_sw = data.frame(type_switch_agreg = c("11-10", "11-01",
                                               "11-00", "10-01", "10-00",
                                               "01-00"),
                         av_age = pv_age,
                         av_educ_level = pv_educ,
                         perc_men = pv_men,
                         perc_incumbent = pv_incumb,
                         av_mun_aptos = pv_quant_aptos_mun)

characs_segun_type_switch_agreg =
  bind_rows(characs_segun_type_switch_agreg, pvs_type_sw)


##Tables

#Switched

gt_characs_switched = 
  characs_segun_switched %>%
  gt(rowname_col = "switched")

gt_characs_switched = 
  gt_characs_switched %>%
  tab_spanner(label = "Age",
              columns = c(av_age, sd_age)) 

gt_characs_switched = 
  gt_characs_switched %>%
  tab_spanner(label = "Educational level",
              columns = c(av_educ_level, sd_educ_level))

gt_characs_switched = 
  gt_characs_switched %>%
  tab_spanner(label = "Proportion of Men",
              columns = c(perc_women, sd_perc_women))

gt_characs_switched = 
  gt_characs_switched %>%
  tab_spanner(label = "Proportion of incumbents",
              columns = c(perc_incumbent, sd_perc_incumbent))

gt_characs_switched = 
  gt_characs_switched %>%
  tab_spanner(label = "Apt voters in municipality",
              columns = c(av_mun_aptos, sd_mun_aptos))
  
gt_characs_switched =
  gt_characs_switched %>%
  tab_stubhead(label = md("**Switched race**"))

gt_characs_switched =
  gt_characs_switched %>%
  cols_label(av_age = "Mean",
             sd_age = "SD",
             av_educ_level = "Mean",
             sd_educ_level = "SD",
             perc_women = "Proportion",
             sd_perc_women = "SD",
             perc_incumbent = "Proportion",
             sd_perc_incumbent = "SD",
             av_mun_aptos = "Mean",
             sd_mun_aptos = "SD")

gt_characs_switched =
  gt_characs_switched %>%
  fmt_number(columns = 1:5,
             rows = 1:2,
             decimals = 2) %>%
  cols_align(align = "center",columns = everything())

gt_characs_switched =
  gt_characs_switched %>%
  fmt_number(columns = 10:11,
             rows = 1:2,
             decimals = 0)

gt_characs_switched =
  gt_characs_switched %>%
  fmt_percent(columns = 6:9,
              rows = 1:2,
             decimals = 2)


gt_characs_switched =
  gt_characs_switched %>%
  fmt_number(columns = everything(),
              rows = 3,
              decimals = 4) %>%
  sub_missing(columns = everything(),
              missing_text = "")

gt_characs_switched



#Type of switch

gt_characs_type_switch = 
  characs_segun_type_switch %>%
  gt(rowname_col = "type_of_switch")

gt_characs_type_switch = 
  gt_characs_type_switch %>%
  tab_spanner(label = "Age",
              columns = c(av_age, sd_age)) 

gt_characs_type_switch = 
  gt_characs_type_switch %>%
  tab_spanner(label = "Educational level",
              columns = c(av_educ_level, sd_educ_level))

gt_characs_type_switch = 
  gt_characs_type_switch %>%
  tab_spanner(label = "Proportion of men",
              columns = c(perc_women, sd_perc_women))

gt_characs_type_switch = 
  gt_characs_type_switch %>%
  tab_spanner(label = "Proportion of incumbents",
              columns = c(perc_incumbent, sd_perc_incumbent))

gt_characs_type_switch = 
  gt_characs_type_switch %>%
  tab_spanner(label = "Vote share",
              columns = c(av_vote_share, sd_vote_share))

gt_characs_type_switch = 
  gt_characs_type_switch %>%
  tab_spanner(label = "Apt voters in municipality",
              columns = c(av_mun_aptos, sd_mun_aptos))

gt_characs_type_switch =
  gt_characs_type_switch %>%
  tab_stubhead(label = md("**Type of switch**"))

gt_characs_type_switch =
  gt_characs_type_switch %>%
  cols_label(av_age = "Mean",
             sd_age = "SD",
             av_educ_level = "Mean",
             sd_educ_level = "SD",
             perc_women = "Proportion",
             sd_perc_women = "SD",
             perc_incumbent = "Proportion",
             sd_perc_incumbent = "SD",
             av_vote_share = "Mean",
             sd_vote_share = "SD",
             av_mun_aptos = "Mean",
             sd_mun_aptos = "SD")

gt_characs_type_switch =
  gt_characs_type_switch %>%
  fmt_number(columns = 1:5,
             decimals = 2) %>%
  cols_align(align = "center",columns = everything())

gt_characs_type_switch =
  gt_characs_type_switch %>%
  fmt_number(columns = 12:13,
             decimals = 0)

gt_characs_type_switch =
  gt_characs_type_switch %>%
  fmt_percent(columns = 6:11,
              decimals = 2)

gt_characs_type_switch

#Type of switch agreg

gt_type_sw_agreg =
  characs_segun_type_switch_agreg %>%
  gt(rowname_col = "type_switch_agreg") %>%
  cols_label(av_age = "Mean",
             sd_age = "SD",
             av_educ_level = "Mean",
             sd_educ_level = "SD",
             perc_men = "Proportion",
             sd_perc_men = "SD",
             perc_incumbent = "Proportion",
             sd_perc_incumbent = "SD",
             av_mun_aptos = "Mean",
             sd_mun_aptos = "SD") %>%
  tab_row_group(label = md("**p-values**"),
                rows = 5:10) %>%
  tab_row_group(label = md("**Type of switch**"),
                rows = 1:4) %>%
  tab_spanner(label = "Age",
              columns = 2:3) %>%
  tab_spanner(label = "Educational level",
              columns = 4:5) %>%
  tab_spanner(label = "Man",
              columns = 6:7) %>%
  tab_spanner(label = "Incumbent",
              columns = 8:9) %>%
  tab_spanner(label = "Eligible voters in municipality",
              columns = 10:11) %>%
  fmt_number(columns = 2:5,
             rows = 1:4) %>%
  fmt_percent(columns = 6:9,
              rows = 1:4) %>%
  fmt_number(columns = 10:11,
             rows = 1:4) %>%
  fmt_number(columns = 2:11,
             rows = 5:10,
             decimals = 4) %>%
  sub_missing(missing_text = "") %>%
  cols_align(align = c("center"))

  

gt_type_sw_agreg


##Histogramas
 
hist(brasil_20$vote_share)

hist( filter(brasil_20, CD_MUNICIPIO == 10006)$vote_share, breaks=20 )

##Agrego indicador de si se cambió municipio entre una elección y la otra

brasil = mutate(brasil, changed_mun =
                  ifelse(NR_CPF_CANDIDATO %in% cands_dos_mun, 1, 0))

rm(charac_segun_switch_20)

####Table 1 from Janus 2021####
Race = c("Branco", "Pardo", "Preto", "Amarelo", "Indígena", "Observations")
Branco <- c(length(r11), length(r21), length(r31),
            length(r41), length(r51), length(r11) + length(r21) + 
              length(r31) + length(r41) + length(r51))
Pardo <- c(length(rr12), length(r22), length(r32), length(r42),
           length(r52), length(r12) + length(r22) + 
             length(r32) + length(r42) + length(r52))
Preto <- c(length(r13), length(r23), length(rr33), length(r43),
           length(r53), length(r13) + length(r23) + 
             length(r33) + length(r43) + length(r53))
Amarelo <- c(length(r14), length(r24), length(r34), length(r44),
             length(r54), length(r14) + length(r24) + 
               length(r34) + length(r44) + length(r54))
Indígena <- c(length(r15), length(r25), length(r35), length(r45),
              length(r55), length(r15) + length(r25) + 
                length(r35) + length(r45) + length(r55))
Not_informed <- c(length(r16), length(r26), length(r36), length(r46),
                  length(r56), length(r16) + length(r26) + 
                    length(r36) + length(r46) + length(r56))

totales <- data.frame(Race, Branco,
                      Pardo,
                      Preto,
                      Amarelo,
                      Indígena,
                      Not_informed)

totales = 
  totales %>%
  mutate(Observations = Branco + Preto + Pardo + Amarelo +
           Indígena + Not_informed)

gt_totales = totales %>% gt(rowname_col = "Race")

gt_totales <- 
  gt_totales %>% tab_header(title = md("*2016 and 2020 elections: Race declaration*"))

gt_totales = tab_spanner(gt_totales, label = md("**Race in 2020**"),
                         columns = c(Branco, Pardo, Preto, Amarelo, Indígena, Not_informed))

gt_totales = tab_stubhead(gt_totales, label = md("**Race in 2016**"))

gt_janusz1_totales = gt_totales

rm(gt_totales)

gt_janusz1_totales

porcentajes <- data.frame(Race, Branco / cant_cand,
                          Pardo /cant_cand,
                          Preto /cant_cand,
                          Amarelo/cant_cand,
                          Indígena/cant_cand,
                          Not_informed/cant_cand)

setnames(porcentajes,
         old = c(names(porcentajes)),
         new = c("Race", "Branco", "Pardo", "Preto", "Amarelo",
                 "Indígena", "Not_informed"))

rm(Branco)
rm(Pardo)
rm(Preto)
rm(Amarelo)
rm(Indígena)

r1_ = c(filter(brasil_16, CD_COR_RACA == 1)$NR_CPF_CANDIDATO)
r2_ = c(filter(brasil_16, CD_COR_RACA == 2)$NR_CPF_CANDIDATO)
r3_ = c(filter(brasil_16, CD_COR_RACA == 3)$NR_CPF_CANDIDATO)
r4_ = c(filter(brasil_16, CD_COR_RACA == 4)$NR_CPF_CANDIDATO)
r5_ = c(filter(brasil_16, CD_COR_RACA == 5)$NR_CPF_CANDIDATO)

Branco = c(length(r11)/length(r1_),
           length(r21)/length(r2_),
           length(r31)/length(r3_),
           length(r41)/length(r4_),
           length(r51)/length(r5_), 73139)

Pardo = c(length(r12)/length(r1_),
          length(r22)/length(r2_),
          length(r32)/length(r3_),
          length(r42)/length(r4_),
          length(r52)/length(r5_), 59996)

Preto = c(length(r13)/length(r1_),
          length(r23)/length(r2_),
          length(r33)/length(r3_),
          length(r43)/length(r4_),
          length(r53)/length(r5_), 14208)

Amarelo = c(length(r14)/length(r1_),
            length(r24)/length(r2_),
            length(r34)/length(r3_),
            length(r44)/length(r4_),
            length(r54)/length(r5_), 512)

Indígena = c(length(r15)/length(r1_),
             length(r25)/length(r2_),
             length(r35)/length(r3_),
             length(r45)/length(r4_),
             length(r55)/length(r5_), 462)

Not_informed = c(length(r16)/length(r1_),
                 length(r26)/length(r2_),
                 length(r36)/length(r3_),
                 length(r46)/length(r4_),
                 length(r56)/length(r5_), 1631)

Observations = c(76316, 59548, 12904, 681, 499, 149948)

perc_bis <- data.frame(Race, Branco,
                      Pardo,
                      Preto,
                      Amarelo,
                      Indígena,
                      Not_informed,
                      Observations)

perc_bis = mutate(perc_bis, Total = Branco+Pardo+Preto+Amarelo+Indígena+Not_informed)
perc_bis =
  perc_bis %>% replace_with_na(replace = list(Total = 149948))


perc_bis$Branco = percent(perc_bis$Branco)
perc_bis$Pardo = percent(perc_bis$Pardo)
perc_bis$Preto = percent(perc_bis$Preto)
perc_bis$Amarelo = percent(perc_bis$Amarelo)
perc_bis$Indígena = percent(perc_bis$Indígena)
perc_bis$Not_informed = percent(perc_bis$Not_informed)
perc_bis$Total = percent(perc_bis$Total)


brancos_no_brancos = setdiff(c(r11,r12,r13,r14,r15,r16), r1_)
pardos_no_pardos = setdiff(c(r21,r22,r23,r24,r25,r26), r2_)
pretos_no_pretos = setdiff(c(r31,r32,r33,r34,r35,r36), r3_)

gt_janusz1_totales

gt_porcentajes = perc_bis %>%
  select(Race, Branco, Pardo, Preto, Amarelo, Indígena, Not_informed,
         Total, Observations) %>%
  gt(rowname_col = "Race")

gt_porcentajes <- 
  gt_porcentajes %>% tab_header(title = md("*2016 and 2020 elections: Race declaration*"))

gt_porcentajes = tab_spanner(gt_porcentajes, label = md("**Race in 2020**"),
                             columns = c(Branco, Pardo, Preto, Amarelo, Indígena, Not_informed))

gt_porcentajes = tab_stubhead(gt_porcentajes, label = md("**Race in 2016**"))

gt_janusz1_porcentajes = gt_porcentajes

rm(gt_porcentajes)

gt_janusz1_porcentajes =
gt_janusz1_porcentajes %>%
  fmt_percent(columns = 2:8,
              rows = 1:5)

gt_janusz1_porcentajes


##race switch per party####

switch_per_party_20 =
  brasil_20 %>%
  group_by(NR_PARTIDO) %>%
  summarise(tot_cands = n(),
            tot_voters = sum(votes),
            av_vote_share = mean(vote_share),
            av_switchers = mean(switched),
            r11 = mean(r11),
            r12 = mean(r12),
            r13 = mean(r13),
            r21 = mean(r21),
            r22 = mean(r22),
            r23 = mean(r23),
            r31 = mean(r31),
            r32 = mean(r32),
            r33 = mean(r33))

switch_per_top3_parties_20 = 
  head(arrange(switch_per_party_20,desc(tot_voters)), n = 6)

switch_per_party_16 =
  brasil_16 %>%
  group_by(NR_PARTIDO) %>%
  summarise(tot_cands = n(),
            tot_voters = sum(votes),
            av_vote_share = mean(vote_share),
            av_switchers = mean(switched),
            r11 = mean(r11),
            r12 = mean(r12),
            r13 = mean(r13),
            r21 = mean(r21),
            r22 = mean(r22),
            r23 = mean(r23),
            r31 = mean(r31),
            r32 = mean(r32),
            r33 = mean(r33))

switch_per_top3_parties_16 = 
  head(arrange(switch_per_party_16,desc(tot_voters)), n = 6)

rm(switch_per_party_16)
rm(switch_per_party_20)

#Names of the parties

party_names = read_excel("Databases/candidate's data/consulta_cand_2020/consulta_cand_2020_AC.xlsx", 
                         sheet = "Hoja2")

switch_per_top3_parties_16 = merge(switch_per_top3_parties_16, party_names)

switch_per_top3_parties_20 = merge(switch_per_top3_parties_20, party_names)

#Tabla 16

gt_sw_per_party_16 = 
  switch_per_top3_parties_16 %>%
  select(Party_name,tot_cands,av_vote_share, av_switchers, r11, r12, r13, r21, r22, r23, r31, r32, r33) %>%
  gt(rowname_col = "Party name") %>% 
  fmt_percent(columns = c(av_vote_share, av_switchers, r11, r12, r13, r21,
                          r22, r23, r31, r32, r33), decimals = 2) %>%
  cols_label(tot_cands = "Total candidates",
             av_vote_share = "Average vote share",
             av_switchers = "Average number of switchers",
             Party_name = md("**Party name**")) %>%
  tab_spanner(label = "Type of race switching",
              columns = 5:13) %>%
  tab_header(title = "Switching per party in 2016",
             subtitle = "Information about the six main parties of the election") %>%
  cols_align(align = c("center"))


gt_sw_per_party_16

gt_sw_per_party_20 = 
  switch_per_top3_parties_20 %>%
  select(Party_name,tot_cands,av_vote_share, av_switchers, r11, r12, r13, r21, r22, r23, r31, r32, r33) %>%
  gt(rowname_col = "Party name") %>% 
  fmt_percent(columns = c(av_vote_share, av_switchers, r11, r12, r13, r21,
                          r22, r23, r31, r32, r33), decimals = 2) %>%
  cols_label(tot_cands = "Total candidates",
             av_vote_share = "Average vote share",
             av_switchers = "Average number of switchers",
             Party_name = md("**Party name**")) %>%
  tab_spanner(label = "Type of race switching",
              columns = 5:11) %>%
  tab_header(title = "Switching per party in 2020",
             subtitle = "Information about the six main parties of the election") %>%
  cols_align(align = c("center"))


gt_sw_per_party_20

switch_per_party_20_bis = 
  brasil_20 %>%
  group_by(NR_PARTIDO) %>%
  summarise(total_candidates = n(),
            average_vote_share = mean(vote_share),
            average_number_of_switchers = mean(switched),
            w11 = mean(w11),
            w10 = mean(w10),
            w01 = mean(w01),
            w00 = mean(w00))

switch_per_party_20_bis = 
  head(arrange(switch_per_party_20_bis,desc(total_candidates)), n = 6)



##Characs party switchers####

#Construyo variable de si cambió de partido

brasil_long = brasil_long %>%
  mutate(switched_party = ifelse(NR_PARTIDO_16 != NR_PARTIDO_20, 1, 0))

#Characs of party switchers vs non_switchers

characs_party_switchers =
  brasil_long %>%
  group_by(switched_party) %>%
  summarise(Quantity = n(),
            Av_switchers = mean(switched),
            r11 = mean(r11),
            r12 = mean(r12),
            r13 = mean(r13),
            r21 = mean(r21),
            r22 = mean(r22),
            r23 = mean(r23),
            r31 = mean(r31),
            r32 = mean(r32),
            r33 = mean(r33))

characs_party_switchers = characs_party_switchers %>%
  mutate(switched_party = ifelse(switched_party == 1, "Yes", "No"))


gt_ch_party_switchers = characs_party_switchers %>%
  gt(rowname_col = "switched_party") %>%
  fmt_percent(columns = c(Av_switchers, r11, r12, r13, r21,
                          r22, r23, r31, r32, r33), decimals = 2) %>%
  tab_stubhead(label = md("**Switched party**"))

gt_ch_party_switchers = 
  gt_ch_party_switchers %>%
  cols_align(align = c("center")) %>%
  cols_label(Av_switchers = "Average number of switchers",
             Quantity = "Quantity of candidates") %>%
  tab_spanner(label = "Type of race switching",
              columns = c(r11,r12,r13,r21,r22,r23,r31,r32,r33)) 

gt_ch_party_switchers


#Meto la variable switched party en el dataset wide

brasil = brasil %>%
  mutate(switched_party = 
           ifelse(NR_CPF_CANDIDATO %in% 
                    filter(brasil_long, switched_party == 1)$NR_CPF_CANDIDATO, 1, 0))

###Chars mun switchers vs non-switchers####

names(brasil_long)
names(brasil)

characs_mun_switchers =
  brasil_long %>%
  group_by(changed_mun) %>%
  summarise(quant_cands = n(),
            Av_switchers = mean(switched),
            r11 = mean(r11),
            r12 = mean(r12),
            r13 = mean(r13),
            r21 = mean(r21),
            r22 = mean(r22),
            r23 = mean(r23),
            r31 = mean(r31),
            r32 = mean(r32),
            r33 = mean(r33))

characs_mun_switchers =
  characs_mun_switchers %>%
  mutate(changed_mun = ifelse(changed_mun==1, "Yes", "No"))

colnames(characs_mun_switchers)[colnames(characs_mun_switchers) ==
                                  "changed_mun"] = "Changed_municipality"

gt_ch_mun_sw =
  characs_mun_switchers %>%
  gt(rowname_col = "Changed_municipality") %>%
  tab_stubhead(label = md("**Changed municipality**")) %>%
  cols_label(quant_cands = "Quantity of candidates",
             Av_switchers = "Average number of switchers") %>%
  tab_spanner(label = "Type of race switch",
              columns = 4:12) %>%
  cols_align(align = c("center")) %>%
  fmt_percent(columns = 3:12)

gt_ch_mun_sw

##Creo variables blanco y negro

brasil_long =
  brasil_long %>%
  mutate(white_16 = case_when(race_16 == 1 ~ 1,
                              race_16 == 2 ~ 0,
                              race_16 == 3 ~ 0))

brasil_long =
  brasil_long %>%
  mutate(white_20 = case_when(race_20 == 1 ~ 1,
                              race_20 == 2 ~ 0,
                              race_20 == 3 ~ 0))

white = 
  brasil_long %>%
  select(NR_CPF_CANDIDATO, white_16, white_20)

brasil = merge(brasil, white, by = "NR_CPF_CANDIDATO")

brasil = 
  brasil %>%
  mutate(white = ifelse(ANO_ELEICAO == 2016, white_16, white_20))

brasil = brasil %>% select(-c(white_16, white_20))

##Creo las variables de cambio de raza usando la variable white

w11 = (brasil_long %>% filter(white_16 == 1 & white_20 == 1))$NR_CPF_CANDIDATO
w10 = (brasil_long %>% filter(white_16 == 1 & white_20 == 0))$NR_CPF_CANDIDATO
w01 = (brasil_long %>% filter(white_16 == 0 & white_20 == 1))$NR_CPF_CANDIDATO
w00 = (brasil_long %>% filter(white_16 == 0 & white_20 == 0))$NR_CPF_CANDIDATO

#Las meto en los datasets

brasil_long =
  brasil_long %>%
  mutate(w11 = ifelse(NR_CPF_CANDIDATO %in% w11, 1, 0))
brasil_long =
  brasil_long %>%
  mutate(w10 = ifelse(NR_CPF_CANDIDATO %in% w10, 1, 0))
brasil_long =
  brasil_long %>%
  mutate(w01 = ifelse(NR_CPF_CANDIDATO %in% w01, 1, 0))
brasil_long =
  brasil_long %>%
  mutate(w00 = ifelse(NR_CPF_CANDIDATO %in% w00, 1, 0))

brasil =
  brasil %>%
  mutate(w11 = ifelse(NR_CPF_CANDIDATO %in% w11, 1, 0))
brasil =
  brasil %>%
  mutate(w10 = ifelse(NR_CPF_CANDIDATO %in% w10, 1, 0))
brasil =
  brasil %>%
  mutate(w01 = ifelse(NR_CPF_CANDIDATO %in% w01, 1, 0))
brasil =
  brasil %>%
  mutate(w00 = ifelse(NR_CPF_CANDIDATO %in% w00, 1, 0))

####Share of white candidates per municipality####

find_mode <- function(x) {
  u <- unique(x)
  tab <- tabulate(match(x, u))
  u[tab == max(tab)]
}

#2020

cands_20 = read_excel("Databases/candidate's data/consulta_cand_2020/consulta_cand_2020_BRASIL.csv.xlsx", 
                      sheet = "codes_filtered") %>%
  select(NR_CPF_CANDIDATO, SQ_CANDIDATO, CD_COR_RACA)

cands_20 = 
merge(cands_20,
      results_turno_1_20 %>% select(SQ_CANDIDATO, CD_MUNICIPIO),
      by = "SQ_CANDIDATO")


cands_20 =
  cands_20 %>%
  group_by(NR_CPF_CANDIDATO) %>%
  summarise(race = find_mode(CD_COR_RACA),
            municipality = find_mode(CD_MUNICIPIO))

length(unique(cands_20$municipality))
unique(cands_20$race)
cands_20 = cands_20 %>% filter(NR_CPF_CANDIDATO != -4)

cands_20 =
  cands_20 %>%
  mutate(race = case_when(race == 1 ~ 1,
                          race == 2 ~ 3,
                          race == 3 ~ 2,
                          race == 4 ~ 4,
                          race == 5 ~ 5,
                          race == 6 ~ 6))

cands_20 = 
  cands_20 %>%
  mutate(white = ifelse(race == 1, 1, 0))

share_whites_per_mun_20 =
  cands_20 %>%
  group_by(CD_MUNICIPIO) %>%
  summarise(share_of_whites_20 = mean(white, na.rm = TRUE))

mean(share_whites_per_mun_20$share_of_whites_20)

#2016

cands_16 = read_excel("Databases/candidate's data/consulta_cand_2016/consulta_cand_2016_BRASIL_1.xlsx", 
                      sheet = "codes") %>%
  select(NR_CPF_CANDIDATO, SQ_CANDIDATO, CD_COR_RACA)

cands_16 = 
  merge(cands_16,
        results_turno_1_16 %>% select(SQ_CANDIDATO, CD_MUNICIPIO),
        by = "SQ_CANDIDATO")


cands_16 =
  cands_16 %>%
  group_by(NR_CPF_CANDIDATO) %>%
  summarise(race = find_mode(CD_COR_RACA),
            municipality = find_mode(CD_MUNICIPIO))

length(unique(cands_16$municipality))
unique(cands_16$race)
cands_16 = cands_16 %>% filter(race != -4)

cands_16 =
  cands_16 %>%
  mutate(race = case_when(race == 1 ~ 1,
                          race == 2 ~ 3,
                          race == 3 ~ 2,
                          race == 4 ~ 4,
                          race == 5 ~ 5,
                          race == 6 ~ 6))

cands_16 = 
  cands_16 %>%
  mutate(white = ifelse(race == 1, 1, 0))

share_whites_per_mun_16 =
  cands_16 %>%
  group_by(CD_MUNICIPIO) %>%
  summarise(share_of_whites_16 = mean(white, na.rm = TRUE))

mean(share_whites_per_mun_16$share_of_whites_16)

#junto los dataset

share_whites_per_mun = 
  merge(share_whites_per_mun_16, share_whites_per_mun_20, 
        by = "CD_MUNICIPIO")
summary(share_whites_per_mun)
cor(share_whites_per_mun$share_of_whites_16, share_whites_per_mun$share_of_whites_20)

rm(share_whites_per_mun_16)
rm(share_whites_per_mun_20)

#Meto esta info en los datasets

colnames(share_whites_per_mun)[colnames(share_whites_per_mun) == "municipality"] = "CD_MUNICIPIO"

brasil = merge(brasil, share_whites_per_mun, 
               by = "CD_MUNICIPIO")

brasil = 
  brasil %>%
  mutate(share_of_whites_mun = 
           ifelse(ANO_ELEICAO == 2016, share_of_whites_16, share_of_whites_20))

brasil = subset(brasil, select = -c(share_of_whites_16, share_of_whites_20) )

colnames(share_whites_per_mun_16)[colnames(share_whites_per_mun_16) == "CD_MUNICIPIO"] = "CD_MUNICIPIO_16"

colnames(share_whites_per_mun_20)[colnames(share_whites_per_mun_20) == "CD_MUNICIPIO"] = "CD_MUNICIPIO_20"

brasil_long = merge(brasil_long, share_whites_per_mun_16,
                    by = "CD_MUNICIPIO_16")

brasil_long = merge(brasil_long, share_whites_per_mun_20,
                    by = "CD_MUNICIPIO_20")

####Características demográficas####

###poblacion del municipio###

municipios = 
  



###Poblacion###

#2016

pop_est_16 = read_excel("Databases/regional data/estimativa_dou_2016_20160913.xlsx", 
                        sheet = "Municípios")

####Table 1 Cornwell####

##All candidates

cands_20 = read_excel("Databases/candidate's data/consulta_cand_2020/consulta_cand_2020_BRASIL.csv.xlsx", 
                      sheet = "codes_filtered") %>%
  select(NR_CPF_CANDIDATO, SQ_CANDIDATO, CD_COR_RACA, CD_GENERO,
         DT_NASCIMENTO, CD_GRAU_INSTRUCAO, ST_REELEICAO)

cands_16 = read_excel("Databases/candidate's data/consulta_cand_2016/consulta_cand_2016_BRASIL_1.xlsx", 
                      sheet = "codes") %>%
  select(NR_CPF_CANDIDATO, SQ_CANDIDATO, CD_COR_RACA, CD_GENERO,
         DT_NASCIMENTO, CD_GRAU_INSTRUCAO, ST_REELEICAO)

#2020

summary(cands_20)


cands_20 = mutate(cands_20,
                DT_NASCIMENTO = as.Date(DT_NASCIMENTO, format= "%y-%m-%d"))

cands_20 = 
  cands_20 %>%
  filter(NR_CPF_CANDIDATO >0 & CD_COR_RACA > 0 & CD_GENERO >0 &
           CD_GRAU_INSTRUCAO >0)

cands_20 = mutate(cands_20,
                age = as.numeric(difftime("2020-11-15", DT_NASCIMENTO,
                                          unit="weeks"))/52.25)

unique(cands_20$ST_REELEICAO)

cands_20 =
  cands_20 %>%
  mutate(ST_REELEICAO = ifelse(ST_REELEICAO == "S", 1, 0))

cands_20 =
  cands_20 %>%
  group_by(NR_CPF_CANDIDATO) %>%
  summarise(Race = find_mode(CD_COR_RACA),
            Male = find_mode(CD_GENERO),
            Age = find_mode(age),
            Education = find_mode(CD_GRAU_INSTRUCAO),
            Incumbent = find_mode(ST_REELEICAO))

unique(cands_20$Male)

cands_20 <- cands_20 %>% 
  mutate(Male = case_when(Male == 2 ~ 1,
                          Male == 4 ~ 0))
cands_20 =
  cands_20 %>% mutate(White = case_when(CD_COR_RACA == 1 ~ 1,
                                        CD_COR_RACA == 2 ~ 0,
                                        CD_COR_RACA == 3 ~ 0))

#2016

summary(cands_16)


cands_16 = mutate(cands_16,
                  DT_NASCIMENTO = as.Date(DT_NASCIMENTO, format= "%y-%m-%d"))

cands_16 = 
  cands_16 %>%
  filter(NR_CPF_CANDIDATO >0 & CD_COR_RACA > 0 & CD_GENERO >0 &
           CD_GRAU_INSTRUCAO >0)

cands_16 = mutate(cands_16,
                  age = as.numeric(difftime("2016-10-30", DT_NASCIMENTO,
                                            unit="weeks"))/52.25)

unique(cands_16$ST_REELEICAO)

cands_16 =
  cands_16 %>%
  mutate(ST_REELEICAO = ifelse(ST_REELEICAO == "S", 1, 0))

cands_16 =
  cands_16 %>%
  group_by(NR_CPF_CANDIDATO) %>%
  summarise(Race = find_mode(CD_COR_RACA),
            Male = find_mode(CD_GENERO),
            Age = find_mode(age),
            Education = find_mode(CD_GRAU_INSTRUCAO),
            Incumbent = find_mode(ST_REELEICAO))

cands_16 =
  cands_16 %>%
  mutate(Male = ifelse(Male == 2, 1, 0))

cands_16 =
  cands_16 %>% mutate(White = case_when(Race == 1 ~ 1,
                                        Race == 2 ~ 0,
                                        Race == 3 ~ 0))


summary(cands_16)

cands_16 = cands_16 %>% filter(Age > 0)

summary(cands_20)

##Empiezo a contruir la tabla

vars_table1 = c("11", "10", "01", "00", "white_16", "white_20",
                "male_16", "male_20", "age_16", "age_20", "educ_16",
                "educ_20", "incumb_16", "incumb_20", "obs_16", "obs_20")

All_candidates = c(NA, NA, NA, NA, mean(cands_16$white, na.rm = TRUE),
                   mean(cands_20$white, na.rm = TRUE), mean(cands_16$CD_GENERO),
                   mean(cands_20$CD_GENERO), mean(cands_16$age, na.rm = TRUE),
                   mean(cands_20$age, na.rm = TRUE), mean(cands_16$CD_GRAU_INSTRUCAO),
                   mean(cands_20$CD_GRAU_INSTRUCAO), mean(cands_16$ST_REELEICAO),
                   mean(cands_20$ST_REELEICAO), nrow(cands_16), nrow(cands_20))

table1_cornwell = data.frame(vars_table1, All_candidates)

brasil_long = 
  brasil_long %>%
  mutate(gender_16 = ifelse(gender_16 == 1, 0, 1))

brasil_long = 
  brasil_long %>%
  mutate(gender_20 = ifelse(gender_20 == 1, 0, 1))

brasil = 
  brasil %>%
  mutate(gender = ifelse(gender == 1, 0, 1))

#Lo mismo para los que participaron en ambas elecciones

Ran_twice = c(mean(brasil_long$w11), mean(brasil_long$w10),
              mean(brasil_long$w01),
              mean(brasil_long$w00), mean(brasil_long$white_16, na.rm = TRUE),
              mean(brasil_long$white_20, na.rm = TRUE), mean(brasil_long$CD_GENERO_16),
              mean(brasil_long$CD_GENERO_20), mean(brasil_long$age_16, na.rm = TRUE),
              mean(brasil_long$age_20, na.rm = TRUE), mean(brasil_long$CD_GRAU_INSTRUCAO_16),
              mean(brasil_long$CD_GRAU_INSTRUCAO_20), mean(brasil_long$ST_REELEICAO_16),
              mean(brasil_long$ST_REELEICAO_20),
              nrow(brasil_long), nrow(brasil_long))
table1_cornwell = data.frame(table1_cornwell, Ran_twice)

#11


cands_11 = brasil_long %>% filter(w11 == 1)

ch11 = c(mean(cands_11$w11), mean(cands_11$w10),
              mean(cands_11$w01),
              mean(cands_11$w00), mean(cands_11$white_16, na.rm = TRUE),
              mean(cands_11$white_20, na.rm = TRUE), mean(cands_11$CD_GENERO_16),
              mean(cands_11$CD_GENERO_20), mean(cands_11$age_16, na.rm = TRUE),
              mean(cands_11$age_20, na.rm = TRUE), mean(cands_11$CD_GRAU_INSTRUCAO_16),
              mean(cands_11$CD_GRAU_INSTRUCAO_20), mean(cands_11$ST_REELEICAO_16),
              mean(cands_11$ST_REELEICAO_20),
              nrow(cands_11), nrow(cands_11))
table1_cornwell = data.frame(table1_cornwell, ch11)

rm(ch11)
rm(cands_11)

#10

cands_10 = brasil_long %>% filter(w10 == 1)

ch10 = c(mean(cands_10$w11), mean(cands_10$w10),
         mean(cands_10$w01),
         mean(cands_10$w00), mean(cands_10$white_16, na.rm = TRUE),
         mean(cands_10$white_20, na.rm = TRUE), mean(cands_10$CD_GENERO_16),
         mean(cands_10$CD_GENERO_20), mean(cands_10$age_16, na.rm = TRUE),
         mean(cands_10$age_20, na.rm = TRUE), mean(cands_10$CD_GRAU_INSTRUCAO_16),
         mean(cands_10$CD_GRAU_INSTRUCAO_20), mean(cands_10$ST_REELEICAO_16),
         mean(cands_10$ST_REELEICAO_20),
         nrow(cands_10), nrow(cands_10))
table1_cornwell = data.frame(table1_cornwell, ch10)

rm(ch10)
rm(cands_10)

#01

cands_01 = brasil_long %>% filter(w01 == 1)

ch01 = c(mean(cands_01$w11), mean(cands_01$w10),
         mean(cands_01$w01),
         mean(cands_01$w00), mean(cands_01$white_16, na.rm = TRUE),
         mean(cands_01$white_20, na.rm = TRUE), mean(cands_01$CD_GENERO_16),
         mean(cands_01$CD_GENERO_20), mean(cands_01$age_16, na.rm = TRUE),
         mean(cands_01$age_20, na.rm = TRUE), mean(cands_01$CD_GRAU_INSTRUCAO_16),
         mean(cands_01$CD_GRAU_INSTRUCAO_20), mean(cands_01$ST_REELEICAO_16),
         mean(cands_01$ST_REELEICAO_20),
         nrow(cands_01), nrow(cands_01))
table1_cornwell = data.frame(table1_cornwell, ch01)

rm(ch01)
rm(cands_01)

#00

cands_00 = brasil_long %>% filter(w00 == 1)

ch00 = c(mean(cands_00$w11), mean(cands_00$w10),
         mean(cands_00$w01),
         mean(cands_00$w00), mean(cands_00$white_16, na.rm = TRUE),
         mean(cands_00$white_20, na.rm = TRUE), mean(cands_00$CD_GENERO_16),
         mean(cands_00$CD_GENERO_20), mean(cands_00$age_16, na.rm = TRUE),
         mean(cands_00$age_20, na.rm = TRUE), mean(cands_00$CD_GRAU_INSTRUCAO_16),
         mean(cands_00$CD_GRAU_INSTRUCAO_20), mean(cands_00$ST_REELEICAO_16),
         mean(cands_00$ST_REELEICAO_20),
         nrow(cands_00), nrow(cands_00))
table1_cornwell = data.frame(table1_cornwell, ch00)

rm(ch00)
rm(cands_00)

##Genro una tabla con los resultados

table1_cornwell[table1_cornwell == "white_16"] <- "2016"
table1_cornwell[table1_cornwell == "white_20"] <- "2020"
table1_cornwell[table1_cornwell == "male_16"] <- "2016"
table1_cornwell[table1_cornwell == "male_20"] <- "2020"
table1_cornwell[table1_cornwell == "age_16"] <- "2016"
table1_cornwell[table1_cornwell == "age_20"] <- "2020"
table1_cornwell[table1_cornwell == "educ_16"] <- "2016"
table1_cornwell[table1_cornwell == "educ_20"] <- "2020"
table1_cornwell[table1_cornwell == "incumb_16"] <- "2016"
table1_cornwell[table1_cornwell == "incumb_20"] <- "2020"
table1_cornwell[table1_cornwell == "obs_16"] <- "2016"
table1_cornwell[table1_cornwell == "obs_20"] <- "2020"

table1_cornwell %>%
   gt(rowname_col = "vars_table1") %>%
   cols_label(All_candidates = "All candidates",
              Ran_twice = "Ran twice",
              ch11 = "11",
              ch10 = "10",
              ch01 = "01",
              ch00 = "00") %>%
   cols_align(align = c("center")) %>%
   fmt_percent(columns = 2:7,
               rows = 1:8) %>%
   fmt_percent(columns = 2:7,
               rows = 13:14) %>%
   fmt_number(columns = 2:7,
              rows = 9:12) %>%
  tab_row_group(label = "Observations", 
                rows = 15:16) %>%
  tab_row_group(label = "Incumbent", 
                rows = 13:14) %>%
  tab_row_group(label = "Educational level", 
                rows = 11:12) %>%
  tab_row_group(label = "Age", 
                rows = 9:10) %>%
  tab_row_group(label = "Male", 
                rows = 7:8) %>%
  tab_row_group(label = "White", 
                rows = 5:6) %>%
  tab_row_group(label = "Race History", 
                rows = 1:4) %>%
  tab_spanner(label = "By Race history",
              columns = 4:7)
  
  
gt_cornwell_1  

gtsave(gt_cornwell_1, "cornwell_1.tex")

rm(cands_16)
rm(cands_20)
rm(cands_per_municipality_16)
rm(cands_per_municipality_20)
rm(cambio_raza_segun_resultado)
rm(charac_segun_race_16)
rm(charac_segun_race_20)
rm(characs_mun_switchers)
rm(characs_party_switchers)
rm(characs_segun_switched)
rm(characs_segun_type_switch)
rm(party_names)
rm(cands_rep_per_mun)
rm(perc_bis)
rm(perc_switchers_two_mun)
rm(perc_type_switch_two_mun)
rm(porcentajes)
rm(race_20)
rm(race_20_2)
rm(share_whites_per_mun)
rm(share_whites_per_mun_16)
rm(share_whites_per_mun_20)
rm(switch_per_top3_parties_16)
rm(switch_per_top3_parties_20)
rm(totales)
rm(voters_per_mun_16)
rm(voters_per_mun_20)
rm(white)


####Cross sectional racial gap####

names(brasil)

###replico el código de arriba

cands_20 <- read_excel("Databases/candidate's data/consulta_cand_2020/consulta_cand_2020_BRASIL.csv.xlsx", 
                       sheet = "codes_filtered")
cands_16 <- read_excel("Databases/candidate's data/consulta_cand_2016/consulta_cand_2016_BRASIL_1.xlsx", 
                            sheet = "codes")

##2020

length(unique(cands_20$NR_CPF_CANDIDATO))

cands_20 = mutate(cands_20,
                  DT_NASCIMENTO = as.Date(DT_NASCIMENTO, format= "%y-%m-%d"))

cands_20 = 
  cands_20 %>%
  filter(NR_CPF_CANDIDATO >0 & CD_COR_RACA > 0 & CD_GENERO >0 &
           CD_GRAU_INSTRUCAO >0)

summary(cands_20$NR_CPF_CANDIDATO)

cands_20 = mutate(cands_20,
                  age = as.numeric(difftime("2020-11-15", DT_NASCIMENTO,
                                            unit="weeks"))/52.25)

unique(cands_20$ST_REELEICAO)

cands_20 =
  cands_20 %>%
  mutate(ST_REELEICAO = ifelse(ST_REELEICAO == "S", 1, 0))

#Agrupo el dataset de resultados

result_per_cand_20 =
  results_turno_1_20 %>%
  group_by(SQ_CANDIDATO) %>%
  summarise(CD_MUNICIPIO = find_mode(CD_MUNICIPIO),
            quantity_of_zones = n(),
            votes = sum(QT_VOTOS_NOMINAIS))

#Mergeo los datsets

cands_20_bis = merge(cands_20, result_per_cand_20, by= "SQ_CANDIDATO")

cands_20 = cands_20_bis
rm(cands_20_bis)

unique(cands_20$NR_TURNO)

filter(cands_20, SQ_CANDIDATO %in% filter(cands_20, NR_TURNO == 2)$SQ_CANDIDATO)

#trabajo con el tema de los turno 2

cands_sec_turn = c(filter(cands_20, CD_SIT_TOT_TURNO == 6)$SQ_CANDIDATO)

charac_cands_sec_turn = filter(cands_20, SQ_CANDIDATO %in% cands_sec_turn)

charac_cands_sec_turn = 
  filter(charac_cands_sec_turn, CD_SIT_TOT_TURNO != 6) %>%
  select(SQ_CANDIDATO, CD_SIT_TOT_TURNO)

cands_20 <- merge(cands_20, charac_cands_sec_turn,
                by=c("SQ_CANDIDATO"), all.x=TRUE)

cands_20$CD_SIT_TOT_TURNO.x <- 
  ifelse(is.na(cands_20$CD_SIT_TOT_TURNO.y), cands_20$CD_SIT_TOT_TURNO.x,
         cands_20$CD_SIT_TOT_TURNO.y)
cands_20$CD_SIT_TOT_TURNO.y <- NULL

rm(charac_cands_sec_turn)

cands_20 = cands_20[cands_20$NR_TURNO != 2, ]

##2016

cands_16 = mutate(cands_16,
                  DT_NASCIMENTO = as.Date(DT_NASCIMENTO, format= "%y-%m-%d"))

cands_16 = 
  cands_16 %>%
  filter(NR_CPF_CANDIDATO >0 & CD_COR_RACA > 0 & CD_GENERO >0 &
           CD_GRAU_INSTRUCAO >0)

cands_16 = mutate(cands_16,
                  age = as.numeric(difftime("2016-11-15", DT_NASCIMENTO,
                                            unit="weeks"))/52.25)

unique(cands_16$ST_REELEICAO)

cands_16 =
  cands_16 %>%
  mutate(ST_REELEICAO = ifelse(ST_REELEICAO == "S", 1, 0))

#Agrupo el dataset de resultados

result_per_cand_16 =
  results_turno_1_16 %>%
  group_by(SQ_CANDIDATO) %>%
  summarise(CD_MUNICIPIO = find_mode(CD_MUNICIPIO),
            quantity_of_zones = n(),
            votes = sum(QT_VOTOS_NOMINAIS))

#Mergeo los datsets

cands_16_bis = merge(cands_16, result_per_cand_16, by= "SQ_CANDIDATO")

cands_16 = cands_16_bis
rm(cands_16_bis)

unique(cands_16$NR_TURNO)

filter(cands_16, SQ_CANDIDATO %in% filter(cands_16, NR_TURNO == 2)$SQ_CANDIDATO)

#trabajo con el tema de los turno 2

cands_sec_turn = c(filter(cands_16, CD_SIT_TOT_TURNO == 6)$SQ_CANDIDATO)

charac_cands_sec_turn = filter(cands_16, SQ_CANDIDATO %in% cands_sec_turn)

charac_cands_sec_turn = 
  filter(charac_cands_sec_turn, CD_SIT_TOT_TURNO != 6) %>%
  select(SQ_CANDIDATO, CD_SIT_TOT_TURNO)

cands_16 <- merge(cands_16, charac_cands_sec_turn,
                  by=c("SQ_CANDIDATO"), all.x=TRUE)

cands_16$CD_SIT_TOT_TURNO.x <- 
  ifelse(is.na(cands_16$CD_SIT_TOT_TURNO.y), cands_16$CD_SIT_TOT_TURNO.x,
         cands_16$CD_SIT_TOT_TURNO.y)
cands_16$CD_SIT_TOT_TURNO.y <- NULL

rm(charac_cands_sec_turn)

cands_16 = cands_16[cands_16$NR_TURNO != 2, ]

#Cambio algunas cosas de los datasets

cands_20 <- cands_20 %>% 
  mutate(CD_GENERO = case_when(CD_GENERO == 2 ~ 1,
                               CD_GENERO == 4 ~ 0))

cands_16 <- cands_16 %>% 
  mutate(CD_GENERO = case_when(CD_GENERO == 2 ~ 1,
                               CD_GENERO == 4 ~ 0))

cands_20 <- cands_20 %>% 
  mutate(CD_COR_RACA = case_when(CD_COR_RACA == 1 ~ 1,
                                 CD_COR_RACA == 2 ~ 3,
                                 CD_COR_RACA == 3 ~ 2,
                                 CD_COR_RACA == 4 ~ 4,
                                 CD_COR_RACA == 5 ~ 5,
                                 CD_COR_RACA == 6 ~ 6))

cands_16 <- cands_16 %>% 
  mutate(CD_COR_RACA = case_when(CD_COR_RACA == 1 ~ 1,
                                 CD_COR_RACA == 2 ~ 3,
                                 CD_COR_RACA == 3 ~ 2,
                                 CD_COR_RACA == 4 ~ 4,
                                 CD_COR_RACA == 5 ~ 5,
                                 CD_COR_RACA == 6 ~ 6))

cands_20 <- cands_20 %>% 
  mutate(ST_DECLARAR_BENS = case_when(ST_DECLARAR_BENS == "S" ~ 1,
                          ST_DECLARAR_BENS == "N" ~ 0))

cands_16 <- cands_16 %>% 
  mutate(ST_DECLARAR_BENS = case_when(ST_DECLARAR_BENS == "S" ~ 1,
                                      ST_DECLARAR_BENS == "N" ~ 0))

###creo variables white y vote share

##2020

cands_20 =
  cands_20 %>%
  mutate(white = case_when(CD_COR_RACA == 1 ~ 1,
                           CD_COR_RACA == 2 ~ 0,
                           CD_COR_RACA == 3 ~ 0))

cands_per_mun_20 =
  cands_20 %>% 
  group_by(CD_MUNICIPIO) %>%
  summarise(quant_cands_mun = n())

cands_20 = merge(cands_20, cands_per_mun_20, 
                     by = "CD_MUNICIPIO")



cands_20 = merge(cands_20, voters_per_mun_20,
                 by = "CD_MUNICIPIO")

cands_20 = 
  cands_20 %>%
  mutate(vote_share = votes / quant_voters_mun_20)

##2016

cands_16 =
  cands_16 %>%
  mutate(white = case_when(CD_COR_RACA == 1 ~ 1,
                           CD_COR_RACA == 2 ~ 0,
                           CD_COR_RACA == 3 ~ 0))

cands_per_mun_16 =
  cands_16 %>% 
  group_by(CD_MUNICIPIO) %>%
  summarise(quant_cands_mun = n())

cands_16 = merge(cands_16, cands_per_mun_16,
                 by = "CD_MUNICIPIO")

cands_16 = merge(cands_16, voters_per_mun_16,
                 by = "CD_MUNICIPIO")

cands_16 = 
  cands_16 %>%
  mutate(vote_share = votes / quant_voters_mun_16)

summary(cands_16$vote_share)
summary(cands_20$vote_share)

###Me quedo solo con los resultados de las elecciones ordincarias

cands_16 = 
  cands_16 %>%
  filter(CD_TIPO_ELEICAO == 2)

cands_20 = 
  cands_20 %>%
  filter(CD_TIPO_ELEICAO == 2)

###Saco a los que no son ni blancos ni negros y quienes participaron

cands_16_bn = cands_16 %>% filter(!is.na(white))

cands_20_bn = cands_20 %>% filter(!is.na(white))


###Hago las regresiones###

##Todos los candidatos

cornwell_table2_1 = 
  lm(vote_share ~ white, cands_16)


cornwell_table2_2 =
  lm(vote_share ~ white, cands_20)

names(brasil)

#Contruyo variable si ganó

cands_16 %>%
  group_by(CD_SIT_TOT_TURNO.x) %>%
  summarise(number = n())

cands_16 =
  cands_16 %>%
  mutate(won = case_when(CD_SIT_TOT_TURNO.x == 1 ~ 1,
                         CD_SIT_TOT_TURNO.x == 2 ~ 1,
                         CD_SIT_TOT_TURNO.x == 3 ~ 1,
                         CD_SIT_TOT_TURNO.x == 4 ~ 0,
                         CD_SIT_TOT_TURNO.x == 5 ~ 0))

cands_20 =
  cands_20 %>%
  mutate(won = case_when(CD_SIT_TOT_TURNO.x == 1 ~ 1,
                         CD_SIT_TOT_TURNO.x == 2 ~ 1,
                         CD_SIT_TOT_TURNO.x == 3 ~ 1,
                         CD_SIT_TOT_TURNO.x == 4 ~ 0,
                         CD_SIT_TOT_TURNO.x == 5 ~ 0))

#sigo regresando

cornwell_table2_3 =
  lm(won ~ white, cands_16)

cornwell_table2_4 =
  lm(won ~ white, cands_20)






####Revision variables brasil####

#Analisis preliminar

length(unique(brasil$NR_CPF_CANDIDATO) %in% unique(cands_16$NR_CPF_CANDIDATO))

cands_16_rt = 
  cands_16 %>%
  filter(NR_CPF_CANDIDATO %in% unique(brasil$NR_CPF_CANDIDATO))

length(unique(cands_16_rt$NR_CPF_CANDIDATO))

unique(cands_16_rt$NR_TURNO)

View(
  brasil_16 %>%
    group_by(CD_TIPO_ELEICAO) %>%
    summarise(veces = n())
)

unique(brasil$CD_TIPO_ELEICAO)
length(unique(brasil_16$NR_CPF_CANDIDATO))

nrow(
  cands_16_rt %>%
    filter(CD_TIPO_ELEICAO == 2)
)

length(unique(cands_16$SQ_CANDIDATO))
length(unique(cands_16$NR_CPF_CANDIDATO))
length(unique(cands_20$SQ_CANDIDATO))
length(unique(cands_20$NR_CPF_CANDIDATO))

#redefino el dataset brasil_16 y brasil_20

brasil_16_bis = cands_16 %>%
  filter(NR_CPF_CANDIDATO %in% c(cands_20$NR_CPF_CANDIDATO))


brasil_20_bis = cands_20 %>%
  filter(NR_CPF_CANDIDATO %in% c(cands_16$NR_CPF_CANDIDATO))

setdiff(names(brasil_16), names(brasil_16_bis))

summary(brasil_20_bis$vote_share)
summary(cands_20$vote_share)

summary(brasil_16$quant_aptos_mun)

summary(brasil_16_bis$NR_CPF_CANDIDATO)

summary(brasil_viejo$CD_SIT_TOT_TURN)

unique(brasil$CD_SIT_TOT_TURNO.x)

brasil %>%
  group_by(CD_COR_RACA) %>%
  summarise(number = n())

#Junto los datasets en uno solo

setdiff(names(brasil_16_bis), names(brasil_20_bis))

colnames(brasil_16_bis)[colnames(brasil_16_bis) == "quant_voters_mun_16"] = "quant_voters_mun"
colnames(brasil_16_bis)[colnames(brasil_16_bis) == "quant_aptos_mun_16"] = "quant_aptos_mun"
colnames(brasil_20_bis)[colnames(brasil_20_bis) == "quant_voters_mun_20"] = "quant_voters_mun"
colnames(brasil_20_bis)[colnames(brasil_20_bis) == "quant_aptos_mun_20"] = "quant_aptos_mun"

brasil_bis = bind_rows(brasil_16_bis, brasil_20_bis)

##Ahora uso el codigo de arriba para agregar las variables que faltan

names(brasil_bis)

brasil_viejo = brasil

rm(brasil)

brasil = brasil_bis
rm(brasil_bis)

brasil_16 = brasil_16_bis
brasil_20 = brasil_20_bis
rm(brasil_16_bis)
rm(brasil_20_bis)

brasil_long = 
  brasil_long %>%
  mutate(changed_mun = ifelse(CD_MUNICIPIO_16 != CD_MUNICIPIO_20, 1, 0))

summary(brasil_long$changed_mun)

brasil = 
  brasil %>%
  mutate(changed_mun =
           ifelse(NR_CPF_CANDIDATO %in% c(filter(brasil_long, changed_mun == 1)$NR_CPF_CANDIDATO), 1, 0))

summary(brasil$changed_mun)

cands_rep_mun =
  brasil_16 %>%
  group_by(CD_MUNICIPIO_16) %>%
  summarise(cands_rep_mun = n())

colnames(cands_rep_mun)[colnames(cands_rep_mun) == "CD_MUNICIPIO_16"] = "CD_MUNICIPIO"

brasil = merge(brasil, cands_rep_mun, by = "CD_MUNICIPIO")

colnames(cands_rep_mun)[colnames(cands_rep_mun) == "CD_MUNICIPIO"] = "CD_MUNICIPIO_16"

brasil_long = merge(brasil_long, cands_rep_mun, by = "CD_MUNICIPIO_16")

colnames(brasil_long)[colnames(brasil_long) == "cands_rep_mun"] = "cands_rep_mun_16"


colnames(cands_rep_mun)[colnames(cands_rep_mun) == "CD_MUNICIPIO_16"] = "CD_MUNICIPIO_20"

brasil_long = merge(brasil_long, cands_rep_mun, by = "CD_MUNICIPIO_20")

colnames(brasil_long)[colnames(brasil_long) == "cands_rep_mun"] = "cands_rep_mun_20"

brasil =
  brasil %>%
  mutate(perc_cands_rep_mun = cands_rep_mun / quant_cands_mun)

summary(brasil$perc_cands_rep_mun)

brasil_long =
  brasil_long %>%
  mutate(perc_cands_rep_mun_16 = cands_rep_mun_16 / quant_cands_mun_16)

brasil_long =
  brasil_long %>%
  mutate(perc_cands_rep_mun_20 = cands_rep_mun_20 / quant_cands_mun_20)

brasil_long = 
  brasil_long %>%
  mutate(switched_party = ifelse(NR_PARTIDO_16 != NR_PARTIDO_20, 1, 0))

summary(brasil_long$switched_party)

brasil =
  brasil %>%
  mutate(switched_party = 
           ifelse(NR_CPF_CANDIDATO %in% c(filter(brasil_long, switched_party == 1)$NR_CPF_CANDIDATO), 1, 0))

summary(brasil$switched_party)

summary(brasil$quantity_of_zones)

setdiff(names(brasil_viejo), names(brasil))
setdiff(names(brasil), names(brasil_viejo))

##Ahora candidatos que participaron en ambas elecciones

cornwell_table2_5 =
  lm(vote_share ~ white, brasil_16)

cornwell_table2_6 =
  lm(vote_share ~ white, brasil_20)

summary(cornwell_table2_6)

###Completo el dataset con todos los candidatos

setdiff(names(brasil_16), names(cands_16))

length(unique(cands_16$CD_MUNICIPIO))

colnames(share_whites_per_mun_16)[colnames(share_whites_per_mun_16) == "CD_MUNICIPIO_16"] = "CD_MUNICIPIO"
colnames(share_whites_per_mun_20)[colnames(share_whites_per_mun_20) == "CD_MUNICIPIO_20"] = "CD_MUNICIPIO"

cands_16 = merge(cands_16, share_whites_per_mun_16, by = "CD_MUNICIPIO")
cands_20 = merge(cands_20, share_whites_per_mun_20, by = "CD_MUNICIPIO")


colnames(cands_rep_mun)[colnames(cands_rep_mun) == "CD_MUNICIPIO_20"] = "CD_MUNICIPIO"

cands_16 = merge(cands_16, cands_rep_mun, by = "CD_MUNICIPIO")
cands_20 = merge(cands_20, cands_rep_mun, by = "CD_MUNICIPIO")


cands_16 =
  cands_16 %>%
  mutate(perc_cands_rep_mun = cands_rep_mun / quant_cands_mun)

summary(cands_16$perc_cands_rep_mun)

cands_20 =
  cands_20 %>%
  mutate(perc_cands_rep_mun = cands_rep_mun / quant_cands_mun)

####racial gap (continuacion)####

###vote_share

##all candidates

rm(cornwell_table2_1, cornwell_table2_2, cornwell_table2_3,
   cornwell_table2_4, cornwell_table2_5, cornwell_table2_6)

vs_w_n_16 = 
  lm(vote_share ~ white, data = cands_16)

summary(vs_w_n_16)

vs_w_n_20 =
  lm(vote_share ~ white, data = cands_20)

summary(vs_w_n_20)


vs_w_y_16 = 
  lm(vote_share ~ white + CD_CARGO + age + CD_GRAU_INSTRUCAO + 
       NR_PARTIDO + CD_NACIONALIDADE + CD_GENERO + ST_REELEICAO +
       quant_aptos_mun_16 + share_of_whites_16 +
       VR_DESPESA_MAX_CAMPANHA, data = cands_16)

summary(vs_w_y_16)

vs_w_y_20 = 
  lm(vote_share ~ white + CD_CARGO + age + CD_GRAU_INSTRUCAO + 
       NR_PARTIDO + CD_NACIONALIDADE + CD_GENERO + ST_REELEICAO +
       quant_aptos_mun_20 + share_of_whites_20 +
       VR_DESPESA_MAX_CAMPANHA, data = cands_20)

summary(vs_w_y_20)

#ran twice

vs_w_n_16_rt = 
  lm(vote_share ~ white, data = brasil_cor_normal_16)

summary(vs_w_n_16_rt)

vs_w_n_20_rt =
  lm(vote_share ~ white, data = brasil_cor_normal_20)

summary(vs_w_n_20_rt)


vs_w_y_16_rt = 
  lm(vote_share ~ white + CD_CARGO + age + CD_GRAU_INSTRUCAO + 
       NR_PARTIDO + CD_NACIONALIDADE + CD_GENERO + ST_REELEICAO +
       quant_aptos_mun + share_of_whites_mun +
       VR_DESPESA_MAX_CAMPANHA,
     data = brasil_cor_normal_16)

summary(vs_w_y_16_rt)

vs_w_y_20_rt = 
  lm(vote_share ~ white + CD_CARGO + age + CD_GRAU_INSTRUCAO + 
       NR_PARTIDO + CD_NACIONALIDADE + CD_GENERO + ST_REELEICAO +
       quant_aptos_mun + share_of_whites_mun +
       VR_DESPESA_MAX_CAMPANHA,
     data = brasil_cor_normal_20)

summary(vs_w_y_20_rt)

?stargazer

#stargazer tables#

m2 = vs_w_n_16
m3 =vs_w_n_20
m4=vs_w_y_16
m5=vs_w_y_20
m6=vs_w_n_16_rt
m7=vs_w_n_20_rt
m8=vs_w_y_16_rt
m9=vs_w_y_20_rt

cov2         <- vcovHC(m2, type = "HC1")
robust_se_2    <- sqrt(diag(cov2))

cov3         <- vcovHC(m3, type = "HC1")
robust_se_3    <- sqrt(diag(cov3))

cov4         <- vcovHC(m4, type = "HC1")
robust_se_4    <- sqrt(diag(cov4))

cov5         <- vcovHC(m5, type = "HC1")
robust_se_5    <- sqrt(diag(cov5))

cov6         <- vcovHC(m6, type = "HC1")
robust_se_6    <- sqrt(diag(cov6))

cov7         <- vcovHC(m7, type = "HC1")
robust_se_7    <- sqrt(diag(cov7))

cov8         <- vcovHC(m8, type = "HC1")
robust_se_8    <- sqrt(diag(cov8))

cov9         <- vcovHC(m9, type = "HC1")
robust_se_9    <- sqrt(diag(cov9))


stargazer(m2,
          m4,
          m6,
          m8,
          omit = c("share_of_whites_16",
                   "share_of_whites_mun", "CD_CARGO", "age",
                   "CD_GRAU_INSTRUCAO", "NR_PARTIDO",
                   "CD_NACIONALIDADE", "CD_GENERO",
                   "ST_REELEICAO", "quant_aptos_mun",
                   "quant_aptos_mun_16", "VR_DESPESA_MAX_CAMPANHA",
                   "share_of_whites_20"),
          covariate.labels = c("White in 2016"),
          keep.stat = c("n", "rsq"),
          se = list(robust_se_2, robust_se_4, robust_se_6, robust_se_8),
          add.lines = list(c("Controls", "N","Y","N","Y"),
                           c("Mean",
                             format(round(mean(cands_16$vote_share), 4)),
                             format(round(mean(cands_16$vote_share), 4)),
                             format(round(mean(brasil_cor_normal_16$vote_share), 4)),
                             format(round(mean(brasil_cor_normal_16$vote_share), 4)))),
          dep.var.labels = c("Vote share in 2016"),
          column.separate = c(2, 2),
          column.labels = c("All candidates", "Candidates who ran twice"),
          notes.align = c("l"),
          notes = "Models (1) and (2) are estimated using all the candidates that participated in the 2016 ordinary election. Models (3) and (4) are estimated using only the candidates that participated both in the 2016 and 2020 ordinary elections. Controls include: position to which the candidate is running, education, age, nationality, gender, party to which the candidate belongs, if the candidate is an incumbent, the maximum value of campaign expenses that the party declared for that candidate and the following characteristics of the municipality in which the candidate is running: share of white candidates and apt voters. Robust standard errors in parentheses.",
          out="cornwell_table2_16.tex")

stargazer(m3,
          m5,
          m7,
          m9,
          omit = c("share_of_whites_16",
                   "share_of_whites_mun", "CD_CARGO", "age",
                   "CD_GRAU_INSTRUCAO", "NR_PARTIDO",
                   "CD_NACIONALIDADE", "CD_GENERO",
                   "ST_REELEICAO", "quant_aptos_mun",
                   "quant_aptos_mun_16", "VR_DESPESA_MAX_CAMPANHA",
                   "share_of_whites_20"),
          covariate.labels = c("White in 2020"),
          se = list(robust_se_3, robust_se_5, robust_se_7, robust_se_9),
          keep.stat = c("n", "rsq"),
          add.lines = list(c("Controls", "N","Y","N","Y"),c("Mean",
                             format(round(mean(cands_20$vote_share), 4)),
                             format(round(mean(cands_20$vote_share), 4)),
                             format(round(mean(brasil_cor_normal_20$vote_share), 4)),
                             format(round(mean(brasil_cor_normal_20$vote_share), 4)))),
          dep.var.labels = c("Vote share in 2020"),
          column.separate = c(2, 2),
          column.labels = c("All candidates", "Candidates who ran twice"),
          notes.align = c("l"),
          notes = "Models (1) and (2) are estimated using all the candidates that participated in the 2020 ordinary election. Models (3) and (4) are estimated using only the candidates that participated both in the 2016 and 2020 ordinary elections. Controls include: position to which the candidate is running, education, age, nationality, gender, party to which the candidate belongs, if the candidate is an incumbent, the maximum value of campaign expenses that the party declared for that candidate and the following characteristics of the municipality in which the candidate is running: share of white candidates and apt voters. Robust standard errors in parentheses.",
          out="cornwell_table2_20.tex")

#end stargazer tables#

###Won election

##all candidates

won_w_n_16 = 
  lm(won ~ white, data = cands_16)

summary(won_w_n_16)

won_w_n_20 =
  lm(won ~ white, data = cands_20)

summary(won_w_n_20)

won_w_y_16 = 
  lm(won ~ white + CD_CARGO + age + CD_GRAU_INSTRUCAO + 
       NR_PARTIDO + CD_NACIONALIDADE + CD_GENERO + ST_REELEICAO +
       quant_aptos_mun_16 + share_of_whites_16 +
       VR_DESPESA_MAX_CAMPANHA, data = cands_16)

summary(won_w_y_16)

won_w_y_20 = 
  lm(won ~ white + CD_CARGO + age + CD_GRAU_INSTRUCAO + 
       NR_PARTIDO + CD_NACIONALIDADE + CD_GENERO + ST_REELEICAO +
       quant_aptos_mun_20 + share_of_whites_20 +
       VR_DESPESA_MAX_CAMPANHA, data = cands_20)

summary(won_w_y_20)

#ran twice

won_w_n_16_rt = 
  lm(won ~ white, data = brasil_cor_normal_16)

summary(won_w_n_16_rt)

won_w_n_20_rt =
  lm(won ~ white, data = brasil_cor_normal_20)

summary(won_w_n_20_rt)


won_w_y_16_rt = 
  lm(won ~ white + CD_CARGO + age + CD_GRAU_INSTRUCAO + 
       NR_PARTIDO + CD_NACIONALIDADE + CD_GENERO + ST_REELEICAO +
       quant_aptos_mun + share_of_whites_mun +
       VR_DESPESA_MAX_CAMPANHA,
     data = brasil_cor_normal_16)

summary(won_w_y_16_rt)

won_w_y_20_rt = 
  lm(won ~ white + CD_CARGO + age + CD_GRAU_INSTRUCAO + 
       NR_PARTIDO + CD_NACIONALIDADE + CD_GENERO + ST_REELEICAO +
       quant_aptos_mun + share_of_whites_mun +
       VR_DESPESA_MAX_CAMPANHA,
     data = brasil_cor_normal_20)

summary(won_w_y_20_rt)

#tabla stargazer#

m2= won_w_n_16
m3 =won_w_n_20
m4=won_w_y_16
m5=won_w_y_20
m6=won_w_n_16_rt
m7=won_w_n_20_rt
m8=won_w_y_16_rt
m9=won_w_y_20_rt

cov2         <- vcovHC(m2, type = "HC1")
robust_se_2    <- sqrt(diag(cov2))

cov3         <- vcovHC(m3, type = "HC1")
robust_se_3    <- sqrt(diag(cov3))

cov4         <- vcovHC(m4, type = "HC1")
robust_se_4    <- sqrt(diag(cov4))

cov5         <- vcovHC(m5, type = "HC1")
robust_se_5    <- sqrt(diag(cov5))

cov6         <- vcovHC(m6, type = "HC1")
robust_se_6    <- sqrt(diag(cov6))

cov7         <- vcovHC(m7, type = "HC1")
robust_se_7    <- sqrt(diag(cov7))

cov8         <- vcovHC(m8, type = "HC1")
robust_se_8    <- sqrt(diag(cov8))

cov9         <- vcovHC(m9, type = "HC1")
robust_se_9    <- sqrt(diag(cov9))

#2016

stargazer(m2,
          m4,
          m6,
          m8,
          omit = c("share_of_whites_16",
                   "share_of_whites_mun", "CD_CARGO", "age",
                   "CD_GRAU_INSTRUCAO", "NR_PARTIDO",
                   "CD_NACIONALIDADE", "CD_GENERO",
                   "ST_REELEICAO", "quant_aptos_mun",
                   "quant_aptos_mun_16", "VR_DESPESA_MAX_CAMPANHA",
                   "share_of_whites_20"),
          covariate.labels = c("White in 2016"),
          keep.stat = c("n", "rsq"),
          se = list(robust_se_2, robust_se_4, robust_se_6, robust_se_8),
          add.lines = list(c("Controls", "N","Y","N","Y"),
                           c("Mean",
                             format(round(mean(cands_16$won, na.rm = TRUE), 4)),
                             format(round(mean(cands_16$won, na.rm = TRUE), 4)),
                             format(round(mean(brasil_cor_normal_16$won, na.rm = TRUE), 4)),
                             format(round(mean(brasil_cor_normal_16$won, na.rm = TRUE), 4)))),
          dep.var.labels = c("Probability to win election in 2016"),
          column.separate = c(2, 2),
          column.labels = c("All candidates", "Candidates who ran twice"),
          notes.align = c("l"),
          notes = "Models (1) and (2) are estimated using all the candidates that participated in the 2016 ordinary election. Models (3) and (4) are estimated using only the candidates that participated both in the 2016 and 2020 ordinary elections. Controls include: position to which the candidate is running, education, age, nationality, gender, party to which the candidate belongs, if the candidate is an incumbent, the maximum value of campaign expenses that the party declared for that candidate and the following characteristics of the municipality in which the candidate is running: share of white candidates and apt voters. Robust standard errors in parentheses.",
          out="cornwell_table2_2_16.tex")

#2020

stargazer(m3,
          m5,
          m7,
          m9,
          omit = c("share_of_whites_16",
                   "share_of_whites_mun", "CD_CARGO", "age",
                   "CD_GRAU_INSTRUCAO", "NR_PARTIDO",
                   "CD_NACIONALIDADE", "CD_GENERO",
                   "ST_REELEICAO", "quant_aptos_mun",
                   "quant_aptos_mun_16", "VR_DESPESA_MAX_CAMPANHA",
                   "share_of_whites_20"),
          covariate.labels = c("White in 2020"),
          keep.stat = c("n", "rsq"),
          se = list(robust_se_3, robust_se_5, robust_se_7, robust_se_9),
          add.lines = list(c("Controls", "N","Y","N","Y"),
                           c("Mean",
                             format(round(mean(cands_20$won, na.rm = TRUE), 4)),
                             format(round(mean(cands_20$won, na.rm = TRUE), 4)),
                             format(round(mean(brasil_cor_normal_20$won, na.rm = TRUE), 4)),
                             format(round(mean(brasil_cor_normal_20$won, na.rm = TRUE), 4)))),
          dep.var.labels = c("Probability to win election in 2020"),
          column.separate = c(2, 2),
          column.labels = c("All candidates", "Candidates who ran twice"),
          notes.align = c("l"),
          notes = "Models (1) and (2) are estimated using all the candidates that participated in the 2020 ordinary election. Models (3) and (4) are estimated using only the candidates that participated both in the 2016 and 2020 ordinary elections. Controls include: position to which the candidate is running, education, age, nationality, gender, party to which the candidate belongs, if the candidate is an incumbent, the maximum value of campaign expenses that the party declared for that candidate and the following characteristics of the municipality in which the candidate is running: share of white candidates and apt voters. Robust standard errors in parentheses.",
          out="cornwell_table2_2_20.html")

#fin tabla stargazer#

?stargazer

####Vote share(Race History)####

###Meto variable que sea la diferencia de votos entre 16 y 20

brasil_long =
  brasil_long %>%
  mutate(diff_vote_share = vote_share_20 - vote_share_16)

summary(brasil_long$diff_vote_share)

diff_vote_share =
  brasil_long %>%
  select(NR_CPF_CANDIDATO, diff_vote_share)

brasil = merge(brasil, diff_vote_share, by = "NR_CPF_CANDIDATO")

winners_per_mun =
cands_20 %>%
  group_by(CD_MUNICIPIO) %>%
  summarise(quant_winners = sum(won))

summary(winners_per_mun$quant_winners)

cor(cands_20$vote_share, cands_20$won)

summary(brasil_20$vote_share)

summary((cands_20 %>%
  group_by(CD_MUNICIPIO) %>%
  summarise(vote_share = sum(vote_share)))$vote_share)

View(
cands_20 %>%
  group_by(NR_PARTIDO) %>%
  summarise(vote_share = mean(vote_share)))

summary((
cands_20 %>%
  group_by(CD_MUNICIPIO, NR_PARTIDO) %>%
  summarise(vote_share = mean(vote_share)) %>%
  group_by(CD_MUNICIPIO) %>%
  summarise(sum_vs = sum(vote_share)))$sum_vs)

cands_20 %>%
  group_by(NR_PARTIDO) %>%
  summarise()

names(cands_16)

cor(brasil_long$won_16, brasil_long$VR_DESPESA_MAX_CAMPANHA_16)

summary(brasil_16$won)

unique(brasil_16$CD_SIT_TOT_TURNO.x)

cor(cands_20$won, cands_20$vote_share)

mean(brasil_16$won, na.rm = TRUE)

length(unique(brasil_20$SG_UF))

###Llevo a cabo las regresiones

vs_rh_16 = 
  lm(vote_share ~ w11 + w10 + w01 + CD_CARGO + age + CD_GRAU_INSTRUCAO + 
       NR_PARTIDO + CD_NACIONALIDADE + CD_GENERO + ST_REELEICAO + 
       quant_aptos_mun + share_of_whites_mun + VR_DESPESA_MAX_CAMPANHA +
       SG_UF,
     data = brasil_cor_normal_16)


names(brasil_long)

summary(vs_rh_16)

vs_rh_20 = 
  lm(vote_share ~ w11 + w10 + w01 + CD_CARGO + age + CD_GRAU_INSTRUCAO + 
       NR_PARTIDO + CD_NACIONALIDADE + CD_GENERO + ST_REELEICAO +
       quant_aptos_mun + share_of_whites_mun + VR_DESPESA_MAX_CAMPANHA +
       SG_UF,
     data = brasil_cor_normal_20)

vs_rh_20_bis = 
  lm(vote_share_20 ~ w11 + w10 + w01 + CD_CARGO_20 + age_20 + CD_GRAU_INSTRUCAO_20 + 
       NR_PARTIDO_20 + CD_NACIONALIDADE_20 + CD_GENERO_20 + ST_REELEICAO_20 +
       quant_aptos_mun_20 + share_of_whites_20, data = brasil_long)

summary(vs_rh_20)
summary(vs_rh_20_bis)

diffvs_rh_20 = 
  lm(diff_vote_share ~ w11 + w10 + w01 + CD_CARGO + age + CD_GRAU_INSTRUCAO + 
       NR_PARTIDO + CD_NACIONALIDADE + CD_GENERO + ST_REELEICAO +
       quant_aptos_mun + share_of_whites_mun + VR_DESPESA_MAX_CAMPANHA +
       SG_UF,
     data = brasil_cor_normal_20)

summary(diffvs_rh_20)
coeftest(diffvs_rh_20, vcov = vcovHC(diffvs_rh_20, type = "HC0"))

diffvs_rh_20_bis =
  lm(diff_vote_share ~ w11 + w10 + w01 + CD_CARGO_20 + age_20 +
       CD_GRAU_INSTRUCAO_20 + NR_PARTIDO_20 + CD_NACIONALIDADE_20 +
       CD_GENERO_20 + ST_REELEICAO_20 + quant_aptos_mun_20 +
       share_of_whites_20 + VR_DESPESA_MAX_CAMPANHA_20 + 
       SG_UF_20, data = brasil_long)

summary(diffvs_rh_20_bis)

summary(lm(diff_vote_share ~ w11 + w10 + w01 + CD_CARGO + age + CD_GRAU_INSTRUCAO + 
     NR_PARTIDO + CD_NACIONALIDADE + CD_GENERO + ST_REELEICAO +
     quant_aptos_mun + share_of_whites_mun,
   data = brasil))

summary(lm(vote_share ~ w11 + w10 + w01 + CD_CARGO + age + CD_GRAU_INSTRUCAO + 
             NR_PARTIDO + CD_NACIONALIDADE + CD_GENERO + ST_REELEICAO +
             quant_aptos_mun + share_of_whites_mun,
           data = brasil))

summary(filter(brasil, ANO_ELEICAO == 2020)$quant_aptos_mun)
summary(brasil_long$quant_aptos_mun_20)

diffvs_rh_20_pooled = 
  lm(diff_vote_share ~ w11 + w10 + w01 + CD_CARGO + age + CD_GRAU_INSTRUCAO + 
       NR_PARTIDO + CD_NACIONALIDADE + CD_GENERO + ST_REELEICAO +
       quant_aptos_mun + share_of_whites_mun + VR_DESPESA_MAX_CAMPANHA +
       SG_UF,
     data = brasil)

summary(diffvs_rh_20_pooled)

coeftest(vs_rh_16, vcov = vcovHC(vs_rh_16, type = "HC1"))
coeftest(vs_rh_20, vcov = vcovHC(vs_rh_20, type = "HC1"))


###Codigo para tabla stargazer

##errores estandar robustos

cov1         <- vcovHC(vs_rh_16, type = "HC1")
robust_se_1    <- sqrt(diag(cov1))

cov2         <- vcovHC(vs_rh_20, type = "HC1")
robust_se_2    <- sqrt(diag(cov2))

cov3         <- vcovHC(diffvs_rh_20, type = "HC1")
robust_se_3    <- sqrt(diag(cov3))


##presentación

m1 = vs_rh_16
m2 = vs_rh_20
m3 = diffvs_rh_20

stargazer(m1,
          m2,
          m3,
          keep = c("w11", "w10", "w01"),
          covariate.labels = c("11: White / White", 
                               "10: White / Non-White",
                               "01: Non-White / White"),
          keep.stat = c("n", "rsq"),
          add.lines = list(c("Mean",
                             format(round(mean(brasil_cor_normal_16$vote_share), 4)),
                             format(round(mean(brasil_cor_normal_20$vote_share), 4)),
                             format(round(mean(brasil_cor_normal_20$diff_vote_share), 5)))),
          se = list(robust_se_1, robust_se_2, robust_se_3),
          column.labels = c("Vote share in 2016",
                            "Vote share in 2020",
                            "Difference in vote share"),
          dep.var.labels.include = FALSE,
          dep.var.labels=c("Vote Share", "Difference in vote share"),
          notes.align = c("l"),
          notes = "Column (1) corresponds to vote share in 2016 whereas column (2) corresponds to vote share in 2020. First model's controls include characteristics of the candidate in 2016 while the other models include characteristics of the candidate in 2020. Controls include:  position to which the candidate is running, education, age, nationality, gender, party to which the candidate belongs, if the candidate is an incumbent, the maximum value of campaign expeses declared by the party for that candidate and the following characteristics of the municipality in which the candidate is running: share of white candidates and apt voters. We also include state fixed effects. Robust standard errors presented in parentheses.",
          out="cornwell_table3_1_robse.tex")

###Fin codigo tabla stargazer

####Proba elected(race History)####

won_rh_16 = 
  lm(won ~ w11 + w10 + w01 + CD_CARGO + age + CD_GRAU_INSTRUCAO + 
       NR_PARTIDO + CD_NACIONALIDADE + CD_GENERO + ST_REELEICAO +
       quant_aptos_mun + share_of_whites_mun + VR_DESPESA_MAX_CAMPANHA +
       SG_UF,
     data = brasil_cor_normal_16)

summary(won_rh_16)

won_rh_20 = 
  lm(won ~ w11 + w10 + w01 + CD_CARGO + age + CD_GRAU_INSTRUCAO + 
       NR_PARTIDO + CD_NACIONALIDADE + CD_GENERO + ST_REELEICAO +
       quant_aptos_mun + share_of_whites_mun + VR_DESPESA_MAX_CAMPANHA +
       SG_UF,
    data = brasil_cor_normal_20)

summary(won_rh_20)

#Codigo tabla stargazer#

cov1         <- vcovHC(won_rh_16, type = "HC1")
robust_se_1    <- sqrt(diag(cov1))

cov2         <- vcovHC(won_rh_20, type = "HC1")
robust_se_2    <- sqrt(diag(cov2))

m1 = won_rh_16
m2 = won_rh_20

stargazer(m1,
          m2,
          keep = c("w11", "w10", "w01"),
          covariate.labels = c("11: White / White", 
                               "10: White / Non-White",
                               "01: Non-White / White"),
          keep.stat = c("n", "rsq"),
          add.lines = list(c("Mean",
                             format(round(mean(brasil_cor_normal_16$won, na.rm = TRUE), 4)),
                             format(round(mean(brasil_cor_normal_20$won), 4)))),
          se = list(robust_se_1, robust_se_2),
          dep.var.labels=c("Probability to win election"),
          notes.align = c("l"),
          dep.var.labels.include = FALSE,
          column.labels = c("Probability to win in 2016", "Probability to win in 2020"),
          notes = "Column (1) is the probability to win the election in 2016 while column (2) shows the probability to win the election in 2020. Controls include:  position to which the candidate is running, education, age, nationality, gender, party to which the candidate belongs, if the candidate is an incumbent, the maximum value of campaign expenses declared by the party for that candidate and the following characteristics of the municipality in which the candidate is running: share of white candidates and apt voters. We also include state fixed effects. The value of controls correspond to the year for which the probability of winning the election is being calculated. Robust standard errors presented in parentheses.",
          out="cornwell_table3_2_robse.tex")

#Fin codigo stargazer#

#Explanations of race history####

w11_explained_20 = 
  lm(w11 ~ age + I(age^2) + CD_GRAU_INSTRUCAO +
       CD_NACIONALIDADE + CD_GENERO + ST_REELEICAO +
       quant_aptos_mun + share_of_whites_mun + VR_DESPESA_MAX_CAMPANHA +
       SG_UF,
     data = brasil_cor_normal_20)

summary(w11_explained_20)

w10_explained_20 = 
  lm(w10 ~ age + I(age^2) + CD_GRAU_INSTRUCAO +
       CD_NACIONALIDADE + CD_GENERO + ST_REELEICAO +
       quant_aptos_mun + share_of_whites_mun + VR_DESPESA_MAX_CAMPANHA +
       SG_UF,
     data = brasil_cor_normal_20)

summary(w10_explained_20)

w01_explained_20 = 
  lm(w01 ~ age + I(age^2) + CD_GRAU_INSTRUCAO +
       CD_NACIONALIDADE + CD_GENERO + ST_REELEICAO +
       quant_aptos_mun + share_of_whites_mun + VR_DESPESA_MAX_CAMPANHA +
       SG_UF,
     data = brasil_cor_normal_20)

summary(w01_explained_20)

w00_explained_20 = 
  lm(w00 ~ age + I(age^2) + CD_GRAU_INSTRUCAO +
       CD_NACIONALIDADE + CD_GENERO + ST_REELEICAO +
       quant_aptos_mun + share_of_whites_mun + VR_DESPESA_MAX_CAMPANHA +
       SG_UF,
     data = brasil_cor_normal_20)

summary(w00_explained_20)

#Stargazer table#

cov1         <- vcovHC(w11_explained_20, type = "HC1")
robust_se_1    <- sqrt(diag(cov1))

cov2         <- vcovHC(w10_explained_20, type = "HC1")
robust_se_2    <- sqrt(diag(cov2))

cov3         <- vcovHC(w01_explained_20, type = "HC1")
robust_se_3    <- sqrt(diag(cov3))

cov4         <- vcovHC(w00_explained_20, type = "HC1")
robust_se_4    <- sqrt(diag(cov4))

m1 = w11_explained_20
m2 = w10_explained_20
m3 = w01_explained_20
m4 = w00_explained_20

stargazer(m1,
          m2,
          m3,
          m4,
          covariate.labels = c("Age", "Age Sq.", "Education level",
                               "Nationality", "Male", "Incumbent",
                               "Apt voters in municipality",
                               "Share of white candidates in municipality",
                               "Maximum party expenses for the candidate"),
          omit = c("SG_UF"),
          keep.stat = c("n", "rsq"),
          add.lines = list(c("Mean",
                             format(round(mean(brasil_cor_normal_20$w11), 4)),
                             format(round(mean(brasil_cor_normal_20$w10), 4)),
                             format(round(mean(brasil_cor_normal_20$w01), 4)),
                             format(round(mean(brasil_cor_normal_20$w00), 4)))),
          se = list(robust_se_1, robust_se_2, robust_se_3, robust_se_4),
          notes.align = c("l"),
          dep.var.labels=c("White / White", "White / Black",
                           "Black / White", "Black / Black"),
          notes = "This table reports possible explanations of the race history of the candidates. The values of the variables are their correposnding value in 2020. State fixed effects are being used.",
          out="cornwell_table5.tex")

#End stargazer table#

hist(brasil_cor_normal_20$CD_SIT_TOT_TURNO.x)

hist(brasil_cor_normal_20$vote_share)

brasil_long %>% filter(CD_GENERO_16 != CD_GENERO_20)

mean(cands_16$ST_REELEICAO)

####Observables(race history)####

##2020

brasil_cor_normal_20 =
  brasil_20 %>%
  filter(r11 == 1 | r12 == 1 | r13 == 1 | r21 == 1 | r22 == 1 | r23 == 1 |
         r31 == 1 | r32 == 1 | r33 == 1)

unique(brasil_cor_normal_20$CD_COR_RACA)

unique(brasil_cor_normal_20$type_of_switch)

gender_rh = lm(CD_GENERO ~ r11 + r12 + r13 + r21 + r22 + r23 + 
                 r31 + r32, data = brasil_cor_normal_20)
summary(gender_rh)

age_rh = lm(age ~ r11 + r12 + r13 + r21 + r22 + r23 + 
                 r31 + r32, data = brasil_cor_normal_20)
summary(age_rh)

incumb_rh = lm(ST_REELEICAO ~ r11 + r12 + r13 + r21 + r22 + r23 + 
                 r31 + r32, data = brasil_cor_normal_20)
summary(incumb_rh)

educ_rh = lm(CD_GRAU_INSTRUCAO ~ r11 + r12 + r13 + r21 + r22 + r23 + 
                 r31 + r32, data = brasil_cor_normal_20)
summary(educ_rh)

aptos_rh = lm(quant_aptos_mun ~ r11 + r12 + r13 + r21 + r22 + r23 + 
                 r31 + r32, data = brasil_cor_normal_20)
summary(aptos_rh)

stargazer(gender_rh,
          age_rh,
          incumb_rh,
          educ_rh,
          aptos_rh,
          title = "Observables in 2020 as a function of race history",
          covariate.labels = c("Branco / Branco", "Branco / Pardo",
                               "Branco / Preto",
                               "Pardo / Branco", "Pardo / Pardo",
                               "Pardo / Preto",
                               "Preto / Branco",
                               "Preto / Pardo"),
          keep.stat = c("n", "rsq"),
          add.lines = list(c("Mean",
                             format(round(mean(brasil_cor_normal_20$CD_GENERO), 4)),
                             format(round(mean(brasil_cor_normal_20$age, na.rm = TRUE), 4)),
                             format(round(mean(brasil_cor_normal_20$ST_REELEICAO), 4)),
                             format(round(mean(brasil_cor_normal_20$CD_GRAU_INSTRUCAO), 4)),
                             format(round(mean(brasil_cor_normal_20$quant_aptos_mun), 4)))),
          type="latex",
          dep.var.labels=c("Gender", "Age", "Incumbent", "Educational level",
                           "Eligible voters in municipality"),
          notes.align = c("l"),
          notes = "This table reports observable characteristics of the candidates in 2020 as a function of their race history",
          out="observable_race_hist_20.tex")




##2016

brasil_cor_normal_16 =
  brasil_16 %>%
  filter(r11 == 1 | r12 == 1 | r13 == 1 | r21 == 1 | r22 == 1 | r23 == 1 |
           r31 == 1 | r32 == 1 | r33 == 1)

names(brasil_16)

unique(brasil_cor_normal_16$CD_COR_RACA)

unique(brasil_cor_normal_16$type_of_switch)

gender_rh = lm(CD_GENERO ~ r11 + r12 + r13 + r21 + r22 + r23 + 
                 r31 + r32, data = brasil_cor_normal_16)
summary(gender_rh)

age_rh = lm(age ~ r11 + r12 + r13 + r21 + r22 + r23 + 
              r31 + r32, data = brasil_cor_normal_16)
summary(age_rh)

incumb_rh = lm(ST_REELEICAO ~ r11 + r12 + r13 + r21 + r22 + r23 + 
                 r31 + r32, data = brasil_cor_normal_16)
summary(incumb_rh)

educ_rh = lm(CD_GRAU_INSTRUCAO ~ r11 + r12 + r13 + r21 + r22 + r23 + 
               r31 + r32, data = brasil_cor_normal_16)
summary(educ_rh)

aptos_rh = lm(quant_aptos_mun ~ r11 + r12 + r13 + r21 + r22 + r23 + 
                r31 + r32, data = brasil_cor_normal_16)
summary(aptos_rh)

stargazer(gender_rh,
          age_rh,
          incumb_rh,
          educ_rh,
          aptos_rh,
          title = "Observables in 2016 as a function of race history",
          covariate.labels = c("Branco / Branco", "Branco / Pardo",
                               "Branco / Preto",
                               "Pardo / Branco", "Pardo / Pardo",
                               "Pardo / Preto",
                               "Preto / Branco",
                               "Preto / Pardo"),
          keep.stat = c("n", "rsq"),
          add.lines = list(c("Mean",
                             format(round(mean(brasil_cor_normal_16$CD_GENERO), 4)),
                             format(round(mean(brasil_cor_normal_16$age, na.rm = TRUE), 4)),
                             format(round(mean(brasil_cor_normal_16$ST_REELEICAO), 4)),
                             format(round(mean(brasil_cor_normal_16$CD_GRAU_INSTRUCAO), 4)),
                             format(round(mean(brasil_cor_normal_16$quant_aptos_mun), 4)))),
          type="latex",
          dep.var.labels=c("Gender", "Age", "Incumbent", "Educational level",
                           "Eligible voters in municipality"),
          notes.align = c("l"),
          notes = "This table reports observable characteristics of the candidates in 1616 as a function of their race history",
          out="observable_race_hist_16.tex")

?stargazer

45.142 + 1.003

###Aggregate pardos and pretos

##2016

unique(brasil_cor_normal_16$type_switch_agreg)

gender_rh = lm(CD_GENERO ~ w11 + w10 + w01, data = brasil_cor_normal_16)
summary(gender_rh)

age_rh = lm(age ~ w11 + w10 + w01, data = brasil_cor_normal_16)
summary(age_rh)

incumb_rh = lm(ST_REELEICAO ~ w11 + w10 + w01, data = brasil_cor_normal_16)
summary(incumb_rh)

educ_rh = lm(CD_GRAU_INSTRUCAO ~ w11 + w10 + w01, data = brasil_cor_normal_16)
summary(educ_rh)

aptos_rh = lm(quant_aptos_mun ~ w11 + w10 + w01, data = brasil_cor_normal_16)
summary(aptos_rh)

stargazer(gender_rh,
          age_rh,
          incumb_rh,
          educ_rh,
          aptos_rh,
          title = "Observables in 2016 as a function of race history",
          covariate.labels = c("White / White", "White / Non-White",
                               "Non-White / White"),
          keep.stat = c("n", "rsq"),
          add.lines = list(c("Mean",
                             format(round(mean(brasil_cor_normal_16$CD_GENERO), 4)),
                             format(round(mean(brasil_cor_normal_16$age, na.rm = TRUE), 4)),
                             format(round(mean(brasil_cor_normal_16$ST_REELEICAO), 4)),
                             format(round(mean(brasil_cor_normal_16$CD_GRAU_INSTRUCAO), 4)),
                             format(round(mean(brasil_cor_normal_16$quant_aptos_mun), 4)))),
          dep.var.labels=c("Gender", "Age", "Incumbent", "Educational level",
                           "Eligible voters in municipality"),
          notes.align = c("l"),
          notes = "This table reports observable characteristics of the candidates in 2016 as a function of their race history",
          out="observable_race_hist_agreg_16.tex")


##2020

gender_rh = lm(CD_GENERO ~ w11 + w10 + w01, data = brasil_cor_normal_20)
summary(gender_rh)

age_rh = lm(age ~ w11 + w10 + w01, data = brasil_cor_normal_20)
summary(age_rh)

incumb_rh = lm(ST_REELEICAO ~ w11 + w10 + w01, data = brasil_cor_normal_20)
summary(incumb_rh)

educ_rh = lm(CD_GRAU_INSTRUCAO ~ w11 + w10 + w01, data = brasil_cor_normal_20)
summary(educ_rh)

aptos_rh = lm(quant_aptos_mun ~ w11 + w10 + w01, data = brasil_cor_normal_20)
summary(aptos_rh)

stargazer(gender_rh,
          age_rh,
          incumb_rh,
          educ_rh,
          aptos_rh,
          title = "Observables in 2020 as a function of race history",
          covariate.labels = c("White / White", "White / Non-White",
                               "Non-White / White"),
          keep.stat = c("n", "rsq"),
          add.lines = list(c("Mean",
                             format(round(mean(brasil_cor_normal_20$CD_GENERO), 4)),
                             format(round(mean(brasil_cor_normal_20$age, na.rm = TRUE), 4)),
                             format(round(mean(brasil_cor_normal_20$ST_REELEICAO), 4)),
                             format(round(mean(brasil_cor_normal_20$CD_GRAU_INSTRUCAO), 4)),
                             format(round(mean(brasil_cor_normal_20$quant_aptos_mun), 4)))),
          dep.var.labels=c("Gender", "Age", "Incumbent", "Educational level",
                           "Eligible voters in municipality"),
          notes.align = c("l"),
          notes = "This table reports observable characteristics of the candidates in 2020 as a function of their race history",
          out="observable_race_hist_agreg_20.tex")

###Lo mismo pero agregando los controles
###(cada observable mas state fixed effects)

##complete race history

#2020

gender_rh = lm(CD_GENERO ~ r11 + r12 + r13 + r21 + r22 + r23 + 
                 r31 + r32 + age + ST_REELEICAO + CD_GRAU_INSTRUCAO +
                 quant_aptos_mun + SG_UF,
               data = brasil_cor_normal_20)
summary(gender_rh)

age_rh = lm(age ~ r11 + r12 + r13 + r21 + r22 + r23 + 
              r31 + r32 + CD_GENERO + ST_REELEICAO + CD_GRAU_INSTRUCAO +
              quant_aptos_mun + SG_UF, data = brasil_cor_normal_20)
summary(age_rh)

incumb_rh = lm(ST_REELEICAO ~ r11 + r12 + r13 + r21 + r22 + r23 + 
                 r31 + r32 + age + CD_GENERO + CD_GRAU_INSTRUCAO +
                 quant_aptos_mun + SG_UF, data = brasil_cor_normal_20)
summary(incumb_rh)

educ_rh = lm(CD_GRAU_INSTRUCAO ~ r11 + r12 + r13 + r21 + r22 + r23 + 
               r31 + r32 + age + ST_REELEICAO + CD_GENERO +
               quant_aptos_mun + SG_UF, data = brasil_cor_normal_20)
summary(educ_rh)

aptos_rh = lm(quant_aptos_mun ~ r11 + r12 + r13 + r21 + r22 + r23 + 
                r31 + r32 + age + ST_REELEICAO + CD_GRAU_INSTRUCAO +
                CD_GENERO + SG_UF, data = brasil_cor_normal_20)
summary(aptos_rh)

stargazer(gender_rh,
          age_rh,
          incumb_rh,
          educ_rh,
          aptos_rh,
          title = "Observables in 2020 as a function of race history",
          covariate.labels = c("Branco / Branco", "Branco / Pardo",
                               "Branco / Preto",
                               "Pardo / Branco", "Pardo / Pardo",
                               "Pardo / Preto",
                               "Preto / Branco",
                               "Preto / Pardo"),
          omit = c("CD_GENERO", "age", "ST_REELEICAO", "CD_GRAU_INSTRUCAO",
                   "quant_aptos_mun", "SG_UF"),
          keep.stat = c("n", "rsq"),
          add.lines = list(c("Mean",
                             format(round(mean(brasil_cor_normal_20$CD_GENERO), 4)),
                             format(round(mean(brasil_cor_normal_20$age, na.rm = TRUE), 4)),
                             format(round(mean(brasil_cor_normal_20$ST_REELEICAO), 4)),
                             format(round(mean(brasil_cor_normal_20$CD_GRAU_INSTRUCAO), 4)),
                             format(round(mean(brasil_cor_normal_20$quant_aptos_mun), 4)))),
          dep.var.labels=c("Gender", "Age", "Incumbent", "Educational level",
                           "Eligible voters in municipality"),
          notes.align = c("l"),
          notes = "This table reports observable characteristics of the candidates in 2020 as a function of their race history. Each model controls for the remaining observables and state fixed effects.",
          out="observable_race_hist_controls_20.tex")

#2016

gender_rh = lm(CD_GENERO ~ r11 + r12 + r13 + r21 + r22 + r23 + 
                 r31 + r32 + age + ST_REELEICAO + CD_GRAU_INSTRUCAO +
                 quant_aptos_mun + SG_UF,
               data = brasil_cor_normal_16)
summary(gender_rh)

age_rh = lm(age ~ r11 + r12 + r13 + r21 + r22 + r23 + 
              r31 + r32 + CD_GENERO + ST_REELEICAO + CD_GRAU_INSTRUCAO +
              quant_aptos_mun + SG_UF, data = brasil_cor_normal_16)
summary(age_rh)

incumb_rh = lm(ST_REELEICAO ~ r11 + r12 + r13 + r21 + r22 + r23 + 
                 r31 + r32 + age + CD_GENERO + CD_GRAU_INSTRUCAO +
                 quant_aptos_mun + SG_UF, data = brasil_cor_normal_16)
summary(incumb_rh)

educ_rh = lm(CD_GRAU_INSTRUCAO ~ r11 + r12 + r13 + r21 + r22 + r23 + 
               r31 + r32 + age + ST_REELEICAO + CD_GENERO +
               quant_aptos_mun + SG_UF, data = brasil_cor_normal_16)
summary(educ_rh)

aptos_rh = lm(quant_aptos_mun ~ r11 + r12 + r13 + r21 + r22 + r23 + 
                r31 + r32 + age + ST_REELEICAO + CD_GRAU_INSTRUCAO +
                CD_GENERO + SG_UF, data = brasil_cor_normal_16)
summary(aptos_rh)

stargazer(gender_rh,
          age_rh,
          incumb_rh,
          educ_rh,
          aptos_rh,
          title = "Observables in 2016 as a function of race history",
          covariate.labels = c("Branco / Branco", "Branco / Pardo",
                               "Branco / Preto",
                               "Pardo / Branco", "Pardo / Pardo",
                               "Pardo / Preto",
                               "Preto / Branco",
                               "Preto / Pardo"),
          omit = c("CD_GENERO", "age", "ST_REELEICAO", "CD_GRAU_INSTRUCAO",
                   "quant_aptos_mun", "SG_UF"),
          keep.stat = c("n", "rsq"),
          add.lines = list(c("Mean",
                             format(round(mean(brasil_cor_normal_16$CD_GENERO), 4)),
                             format(round(mean(brasil_cor_normal_16$age, na.rm = TRUE), 4)),
                             format(round(mean(brasil_cor_normal_16$ST_REELEICAO), 4)),
                             format(round(mean(brasil_cor_normal_16$CD_GRAU_INSTRUCAO), 4)),
                             format(round(mean(brasil_cor_normal_16$quant_aptos_mun), 4)))),
          dep.var.labels=c("Gender", "Age", "Incumbent", "Educational level",
                           "Eligible voters in municipality"),
          notes.align = c("l"),
          notes = "This table reports observable characteristics of the candidates in 2016 as a function of their race history. Each model controls for the remaining observables and state fixed effects.",
          out="observable_race_hist_controls_16.html")

##Agreggated race history

#2016

unique(brasil_cor_normal_16$type_switch_agreg)

gender_rh = lm(CD_GENERO ~ w11 + w10 + w01 + age + ST_REELEICAO + CD_GRAU_INSTRUCAO +
                 quant_aptos_mun + SG_UF, data = brasil_cor_normal_16)
summary(gender_rh)

age_rh = lm(age ~ w11 + w10 + w01 + CD_GENERO + ST_REELEICAO + CD_GRAU_INSTRUCAO +
              quant_aptos_mun + SG_UF, data = brasil_cor_normal_16)
summary(age_rh)

incumb_rh = lm(ST_REELEICAO ~ w11 + w10 + w01 + age + CD_GENERO + CD_GRAU_INSTRUCAO +
                 quant_aptos_mun + SG_UF, data = brasil_cor_normal_16)
summary(incumb_rh)

educ_rh = lm(CD_GRAU_INSTRUCAO ~ w11 + w10 + w01 + age + ST_REELEICAO + CD_GENERO +
               quant_aptos_mun + SG_UF, data = brasil_cor_normal_16)
summary(educ_rh)

aptos_rh = lm(quant_aptos_mun ~ w11 + w10 + w01 + age + ST_REELEICAO + CD_GRAU_INSTRUCAO +
              CD_GENERO + SG_UF, data = brasil_cor_normal_16)
summary(aptos_rh)

stargazer(gender_rh,
          age_rh,
          incumb_rh,
          educ_rh,
          aptos_rh,
          title = "Observables in 2016 as a function of race history",
          covariate.labels = c("White / White", "White / Non-White",
                               "Non-White / White"),
          omit = c("CD_GENERO", "age", "ST_REELEICAO", "CD_GRAU_INSTRUCAO",
                   "quant_aptos_mun", "SG_UF"),
          keep.stat = c("n", "rsq"),
          add.lines = list(c("Mean",
                             format(round(mean(brasil_cor_normal_16$CD_GENERO), 4)),
                             format(round(mean(brasil_cor_normal_16$age, na.rm = TRUE), 4)),
                             format(round(mean(brasil_cor_normal_16$ST_REELEICAO), 4)),
                             format(round(mean(brasil_cor_normal_16$CD_GRAU_INSTRUCAO), 4)),
                             format(round(mean(brasil_cor_normal_16$quant_aptos_mun), 4)))),
          dep.var.labels=c("Gender", "Age", "Incumbent", "Educational level",
                           "Eligible voters in municipality"),
          notes.align = c("l"),
          notes = "This table reports observable characteristics of the candidates in 2016 as a function of their race history. Each model controls for the remaining observables and state fixed effects.",
          out="observable_race_hist_agreg_controls_16.html")

#2020

unique(brasil_cor_normal_20$type_switch_agreg)

gender_rh = lm(CD_GENERO ~ w11 + w10 + w01 + age + ST_REELEICAO + CD_GRAU_INSTRUCAO +
                 quant_aptos_mun + SG_UF, data = brasil_cor_normal_20)
summary(gender_rh)

age_rh = lm(age ~ w11 + w10 + w01 + CD_GENERO + ST_REELEICAO + CD_GRAU_INSTRUCAO +
              quant_aptos_mun + SG_UF, data = brasil_cor_normal_20)
summary(age_rh)

incumb_rh = lm(ST_REELEICAO ~ w11 + w10 + w01 + age + CD_GENERO + CD_GRAU_INSTRUCAO +
                 quant_aptos_mun + SG_UF, data = brasil_cor_normal_20)
summary(incumb_rh)

educ_rh = lm(CD_GRAU_INSTRUCAO ~ w11 + w10 + w01 + age + ST_REELEICAO + CD_GENERO +
               quant_aptos_mun + SG_UF, data = brasil_cor_normal_20)
summary(educ_rh)

aptos_rh = lm(quant_aptos_mun ~ w11 + w10 + w01 + age + ST_REELEICAO + CD_GRAU_INSTRUCAO +
                CD_GENERO + SG_UF, data = brasil_cor_normal_20)
summary(aptos_rh)

stargazer(gender_rh,
          age_rh,
          incumb_rh,
          educ_rh,
          aptos_rh,
          title = "Observables in 2020 as a function of race history",
          covariate.labels = c("White / White", "White / Non-White",
                               "Non-White / White"),
          omit = c("CD_GENERO", "age", "ST_REELEICAO", "CD_GRAU_INSTRUCAO",
                   "quant_aptos_mun", "SG_UF"),
          keep.stat = c("n", "rsq"),
          add.lines = list(c("Mean",
                             format(round(mean(brasil_cor_normal_20$CD_GENERO), 4)),
                             format(round(mean(brasil_cor_normal_20$age, na.rm = TRUE), 4)),
                             format(round(mean(brasil_cor_normal_20$ST_REELEICAO), 4)),
                             format(round(mean(brasil_cor_normal_20$CD_GRAU_INSTRUCAO), 4)),
                             format(round(mean(brasil_cor_normal_20$quant_aptos_mun), 4)))),
          dep.var.labels=c("Gender", "Age", "Incumbent", "Educational level",
                           "Eligible voters in municipality"),
          notes.align = c("l"),
          notes = "This table reports observable characteristics of the candidates in 2020 as a function of their race history. Each model controls for the remaining observables and state fixed effects.",
          out="observable_race_hist_agreg_controls_20.html")

####Special regression####

###Creo variable que diga si fue blanco en 2016 y 2020

brasil =
  brasil %>%
  mutate(white_in_16 = ifelse(w11 == 1 | w10 == 1, 1, 0))

brasil =
  brasil %>%
  mutate(white_in_20 = ifelse(w11 == 1 | w01 == 1, 1, 0))

###Hago la regresión:

spreg_vs = lm(vote_share ~ white_in_16 + white_in_20 +
                I(white_in_16*white_in_20) + as.factor(CD_CARGO) + age +
                CD_GRAU_INSTRUCAO + 
                NR_PARTIDO + CD_NACIONALIDADE + CD_GENERO +
                ST_REELEICAO + 
                quant_aptos_mun + share_of_whites_mun +
                VR_DESPESA_MAX_CAMPANHA + as.factor(ANO_ELEICAO) +
                SG_UF, data = brasil)

summary(spreg_vs)

stargazer(spreg_vs,
          title = "Vote share as a function of race",
          keep = c("white_in_16", "white_in_20",
                   "I(white_in_16 * white_in_20)"),
          covariate.labels = c("White in 2016", "White in 2020",
                               "White in 2016 * White in 2020"),
          dep.var.labels = c("Vote share"),
          omit.stat = c("adj.rsq", "f", "ser"),
          add.lines = list(c("Mean", format(round(mean(brasil$vote_share), 4), nsmall = 2))),
          type = "html",
          notes = "This table presents a model for vote share as a function of race both in 2016 and 2020. Controls include: the position to which the candidate is running, age, educational level, party to which the candidate belongs, nationality, gender, incumbency, eligible voters in the municipality where the candidate is running and the maximum value of expenses declared by the party for that candidate. Also, state and year fixed effects are used.",
          notes.align = c("l"),
          out = "special_regression_1.html")

####Maximum cap explained####

###explanation cap in 2020

cap_explined = lm(VR_DESPESA_MAX_CAMPANHA ~ w11 + w10 + w01 + prefeito + age +
                CD_GRAU_INSTRUCAO + 
                NR_PARTIDO + CD_NACIONALIDADE + CD_GENERO +
                ST_REELEICAO + 
                quant_aptos_mun + share_of_whites_mun +
                SG_UF, data = brasil_cor_normal_20)

summary(cap_explined)
summary(brasil_long$prefeito_16)
summary(brasil_long$CD_GENERO_16)
length(unique(brasil_long$VR_DESPESA_MAX_CAMPANHA_16))

summary(brasil_long$CD_CARGO_16)
names(brasil)

stargazer(cap_explined,
          title = "Maximum cap explained",
          omit = c("SG_UF"),
          dep.var.labels = c("Maximum cap in 2020"),
          omit.stat = c("adj.rsq", "f", "ser"),
          add.lines = list(c("Mean",
                             format(round(mean(brasil_cor_normal_20$VR_DESPESA_MAX_CAMPANHA), 4),
                                    nsmall = 2))),
          type = "html",
          notes = "The dependent variable is the maximum value of expenses declared by the party for that candidate. The values of each variable are their values in 2020. State fixed effects are being used.",
          notes.align = c("l"),
          out = "cap_explained_20.html")

###Difference in cap between the two elections

brasil_long =
  brasil_long %>%
  mutate(diff_max_expenses =
           VR_DESPESA_MAX_CAMPANHA_20 - VR_DESPESA_MAX_CAMPANHA_16)

brasil_long_cor_normal = 
  brasil_long %>%
  filter(!is.na(type_of_switch))

names(brasil_long)

diff_cap_1 = lm(diff_max_expenses ~ w11 + w10 + w01,
              data = brasil_long_cor_normal)

diff_cap_2 = lm(diff_max_expenses ~ r11 + r12 + r13 + r21 + r22 + r23 + 
                  r31 + r32,
                data = brasil_long_cor_normal)

summary(diff_cap_1)

summary(diff_cap_2)

summary(brasil_long_cor_normal$diff_max_expenses)

brasil_long %>%
  filter(CD_MUNICIPIO_20 == 19) %>%
  group_by(NR_PARTIDO_20) %>%
  summarise(candidates = n())

brasil_long %>%
  group_by(CD_SIT_TOT_TURNO.x_20) %>%
  summarise(vote_s = mean(vote_share_20),
            number = n()/nrow(brasil_long))

unique(brasil_long$CD_CARGO_16)

brasil =
  brasil %>%
  mutate(prefeito = ifelse(CD_CARGO == 11, 1, 0))

brasil_long =
  brasil_long %>%
  mutate(prefeito_16 = ifelse(CD_CARGO_16 == 11, 1, 0))

brasil_long =
  brasil_long %>%
  mutate(prefeito_20 = ifelse(CD_CARGO_20 == 11, 1, 0))

perc_white_winners_per_mun =
brasil_long %>%
  filter(won_20 == 1) %>%
  group_by(CD_MUNICIPIO_20) %>%
  summarise(perc_white_winners = sum(white_20, na.rm = TRUE)/n(),
            winners =n())

summary(perc_white_winners_per_mun$perc_white_winners)
summary(perc_white_winners_per_mun$winners)

####Controls dif years####

###Vote share

vs16_rh20 = 
  lm(vote_share_16 ~ w11 + w10 + w01 + CD_CARGO_20 + age_20 +
       CD_GRAU_INSTRUCAO_20 + 
       NR_PARTIDO_20 + CD_NACIONALIDADE_20 + CD_GENERO_20 +
       ST_REELEICAO_20 + 
       quant_aptos_mun_20 + share_of_whites_20 +
       SG_UF_16,
     data = brasil_long_cor_normal)

summary(vs16_rh20)
summary(vs_rh_16)

vs20_rh16 = 
  lm(vote_share_20 ~ w11 + w10 + w01 + CD_CARGO_16 + age_16 +
       CD_GRAU_INSTRUCAO_16 + 
       NR_PARTIDO_16 + CD_NACIONALIDADE_16 + CD_GENERO_16 +
       ST_REELEICAO_16 + 
       quant_aptos_mun_16 + share_of_whites_16 +
       SG_UF_20,
     data = brasil_long_cor_normal)

summary(vs20_rh16)
summary(vs_rh_20)

diffvs_rh_16 = 
  lm(diff_vote_share ~ w11 + w10 + w01 + CD_CARGO_16 + age_16 +
       CD_GRAU_INSTRUCAO_16 + 
       NR_PARTIDO_16 + CD_NACIONALIDADE_16 + CD_GENERO_16 +
       ST_REELEICAO_16 +
       quant_aptos_mun_16 + share_of_whites_16 +
       SG_UF_16,
     data = brasil_long_cor_normal)

summary(diffvs_rh_16)
summary(diffvs_rh_20)

##Code table stargazer

cov1         <- vcovHC(vs16_rh20, type = "HC1")
robust_se_1    <- sqrt(diag(cov1))

cov2         <- vcovHC(vs20_rh16, type = "HC1")
robust_se_2    <- sqrt(diag(cov2))

cov3         <- vcovHC(diffvs_rh_16, type = "HC1")
robust_se_3    <- sqrt(diag(cov3))


##presentación

m1 = vs16_rh20
m2 = vs20_rh16
m3 = diffvs_rh_16

stargazer(m1,
          m2,
          m3,
          keep = c("w11", "w10", "w01"),
          covariate.labels = c("11: White / White", 
                               "10: White / Non-White",
                               "01: Non-White / White"),
          keep.stat = c("n", "rsq"),
          add.lines = list(c("Mean",
                             format(round(mean(brasil_cor_normal_16$vote_share), 4)),
                             format(round(mean(brasil_cor_normal_20$vote_share), 4)),
                             format(round(mean(brasil_cor_normal_20$diff_vote_share), 5)))),
          se = list(robust_se_1, robust_se_2, robust_se_3),
          column.labels = c("Vote share in 2016",
                            "Vote share in 2020",
                            "Difference in vote share"),
          dep.var.labels.include = FALSE,
          dep.var.labels=c("Vote Share", "Difference in vote share"),
          notes.align = c("l"),
          notes = "Column (1) corresponds to vote share in 2016 whereas column (2) corresponds to vote share in 2020. First model's controls include characteristics of the candidate in 2020 while the other models include characteristics of the candidate in 2016. Controls include:  position to which the candidate is running, education, age, nationality, gender, party to which the candidate belongs, if the candidate is an incumbent and the following characteristics of the municipality in which the candidate is running: share of white candidates and apt voters. We also include state fixed effects. Robust standard errors presented in parentheses.",
          out="cornwell_table3_1_controls_dif_years.tex")

##Fin codigo tabla stargazer

###Proba elected

won16_rh20 = 
  lm(won_16 ~ w11 + w10 + w01 + CD_CARGO_20 + age_20 + CD_GRAU_INSTRUCAO_20 + 
       NR_PARTIDO_20 + CD_NACIONALIDADE_20 + CD_GENERO_20 + ST_REELEICAO_20 +
       quant_aptos_mun_20 + share_of_whites_20 +
       SG_UF_16,
     data = brasil_long_cor_normal)

won20_rh16 = 
  lm(won_20 ~ w11 + w10 + w01 + CD_CARGO_16 + age_16 + CD_GRAU_INSTRUCAO_16 + 
       NR_PARTIDO_16 + CD_NACIONALIDADE_16 + CD_GENERO_16 + ST_REELEICAO_16 +
       quant_aptos_mun_16 + share_of_whites_16 +
       SG_UF_20,
     data = brasil_long_cor_normal)

##Codigo tabla stargazer#

cov1         <- vcovHC(won16_rh20, type = "HC1")
robust_se_1    <- sqrt(diag(cov1))

cov2         <- vcovHC(won20_rh16, type = "HC1")
robust_se_2    <- sqrt(diag(cov2))

m1 = won16_rh20
m2 = won20_rh16

stargazer(m1,
          m2,
          keep = c("w11", "w10", "w01"),
          covariate.labels = c("11: White / White", 
                               "10: White / Non-White",
                               "01: Non-White / White"),
          keep.stat = c("n", "rsq"),
          add.lines = list(c("Mean",
                             format(round(mean(brasil_cor_normal_16$won, na.rm = TRUE), 4)),
                             format(round(mean(brasil_cor_normal_20$won), 4)))),
          se = list(robust_se_1, robust_se_2),
          dep.var.labels=c("Probability to win election"),
          notes.align = c("l"),
          dep.var.labels.include = FALSE,
          column.labels = c("Probability to win in 2016", "Probability to win in 2020"),
          notes = "Column (1) is the probability to win the election in 2016 while column (2) shows the probability to win the election in 2020. The value of controls are their respective values in 2016 for the first model, while for the second model the 2020 values are used. Controls include:  position to which the candidate is running, education, age, nationality, gender, party to which the candidate belongs, if the candidate is an incumbent, the maximum value of campaign expenses declared by the party for that candidate and the following characteristics of the municipality in which the candidate is running: share of white candidates and apt voters. We also include state fixed effects. The value of controls correspond to the year for which the probability of winning the election is being calculated. Robust standard errors presented in parentheses.",
          out="cornwell_table3_2_controls_dif_years.html")

##Fin codigo tabla stargazer

####Controls 2020####

###Vote share

vs16_rh20 = 
  lm(vote_share_16 ~ w11 + w10 + w01 + CD_CARGO_20 + age_20 +
       CD_GRAU_INSTRUCAO_20 + 
       NR_PARTIDO_20 + CD_NACIONALIDADE_20 + CD_GENERO_20 +
       ST_REELEICAO_20 + 
       quant_aptos_mun_20 + share_of_whites_20 +
       SG_UF_20,
     data = brasil_long_cor_normal)

summary(vs16_rh20)
summary(vs_rh_16)

vs20_rh20 = 
  lm(vote_share_20 ~ w11 + w10 + w01 + CD_CARGO_20 + age_20 +
       CD_GRAU_INSTRUCAO_20 + 
       NR_PARTIDO_20 + CD_NACIONALIDADE_20 + CD_GENERO_20 +
       ST_REELEICAO_20 + 
       quant_aptos_mun_20 + share_of_whites_20 +
       SG_UF_20,
     data = brasil_long_cor_normal)

summary(vs20_rh16)
summary(vs_rh_20)

diffvs_rh_20 = 
  lm(diff_vote_share ~ w11 + w10 + w01 + CD_CARGO_20 + age_20 +
       CD_GRAU_INSTRUCAO_20 + 
       NR_PARTIDO_20 + CD_NACIONALIDADE_20 + CD_GENERO_20 +
       ST_REELEICAO_20 +
       quant_aptos_mun_20 + share_of_whites_20 +
       SG_UF_20,
     data = brasil_long_cor_normal)

summary(diffvs_rh_16)
summary(diffvs_rh_20)

names(brasil_long_cor_normal)

##Code table stargazer

cov1         <- vcovHC(vs16_rh20, type = "HC1")
robust_se_1    <- sqrt(diag(cov1))

cov2         <- vcovHC(vs20_rh20, type = "HC1")
robust_se_2    <- sqrt(diag(cov2))

cov3         <- vcovHC(diffvs_rh_20, type = "HC1")
robust_se_3    <- sqrt(diag(cov3))


##presentación

m1 = vs16_rh20
m2 = vs20_rh20
m3 = diffvs_rh_20

stargazer(m1,
          m2,
          m3,
          keep = c("w11", "w10", "w01"),
          covariate.labels = c("11: White / White", 
                               "10: White / Non-White",
                               "01: Non-White / White"),
          keep.stat = c("n", "rsq"),
          add.lines = list(c("Mean",
                             format(round(mean(brasil_cor_normal_16$vote_share), 4)),
                             format(round(mean(brasil_cor_normal_20$vote_share), 4)),
                             format(round(mean(brasil_cor_normal_20$diff_vote_share), 5)))),
          se = list(robust_se_1, robust_se_2, robust_se_3),
          column.labels = c("Vote share in 2016",
                            "Vote share in 2020",
                            "Difference in vote share"),
          dep.var.labels.include = FALSE,
          dep.var.labels=c("Vote Share", "Difference in vote share"),
          notes.align = c("l"),
          notes = "Column (1) corresponds to vote share in 2016 whereas column (2) corresponds to vote share in 2020. The values of controls are those of 2020. Controls include:  position to which the candidate is running, education, age, nationality, gender, party to which the candidate belongs, if the candidate is an incumbent and the following characteristics of the municipality in which the candidate is running: share of white candidates and apt voters. We also include state fixed effects. Robust standard errors presented in parentheses.",
          out="cornwell_table3_1_controls_2020.html")

##Fin codigo tabla stargazer

####Controls 2016####

###Vote share

vs16_rh16 = 
  lm(vote_share_16 ~ w11 + w10 + w01 + CD_CARGO_16 + age_16 +
       CD_GRAU_INSTRUCAO_16 + 
       NR_PARTIDO_16 + CD_NACIONALIDADE_16 + CD_GENERO_16 +
       ST_REELEICAO_16 + 
       quant_aptos_mun_16 + share_of_whites_16 +
       SG_UF_16,
     data = brasil_long_cor_normal)

summary(vs16_rh16)
summary(vs_rh_16)

vs20_rh16 = 
  lm(vote_share_20 ~ w11 + w10 + w01 + CD_CARGO_16 + age_16 +
       CD_GRAU_INSTRUCAO_16 + 
       NR_PARTIDO_16 + CD_NACIONALIDADE_16 + CD_GENERO_16 +
       ST_REELEICAO_16 + 
       quant_aptos_mun_16 + share_of_whites_16 +
       SG_UF_16,
     data = brasil_long_cor_normal)

summary(vs20_rh16)
summary(vs_rh_20)

diffvs_rh_16 = 
  lm(diff_vote_share ~ w11 + w10 + w01 + CD_CARGO_16 + age_16 +
       CD_GRAU_INSTRUCAO_16 + 
       NR_PARTIDO_16 + CD_NACIONALIDADE_16 + CD_GENERO_16 +
       ST_REELEICAO_16 +
       quant_aptos_mun_16 + share_of_whites_16 +
       SG_UF_16,
     data = brasil_long_cor_normal)

summary(diffvs_rh_16)
summary(diffvs_rh_20)

##Code table stargazer

cov1         <- vcovHC(vs16_rh16, type = "HC1")
robust_se_1    <- sqrt(diag(cov1))

cov2         <- vcovHC(vs20_rh16, type = "HC1")
robust_se_2    <- sqrt(diag(cov2))

cov3         <- vcovHC(diffvs_rh_16, type = "HC1")
robust_se_3    <- sqrt(diag(cov3))


##presentación

m1 = vs16_rh16
m2 = vs20_rh16
m3 = diffvs_rh_16

stargazer(m1,
          m2,
          m3,
          keep = c("w11", "w10", "w01"),
          covariate.labels = c("11: White / White", 
                               "10: White / Non-White",
                               "01: Non-White / White"),
          keep.stat = c("n", "rsq"),
          add.lines = list(c("Mean",
                             format(round(mean(brasil_cor_normal_16$vote_share), 4)),
                             format(round(mean(brasil_cor_normal_20$vote_share), 4)),
                             format(round(mean(brasil_cor_normal_20$diff_vote_share), 5)))),
          se = list(robust_se_1, robust_se_2, robust_se_3),
          column.labels = c("Vote share in 2016",
                            "Vote share in 2020",
                            "Difference in vote share"),
          dep.var.labels.include = FALSE,
          dep.var.labels=c("Vote Share", "Difference in vote share"),
          notes.align = c("l"),
          notes = "Column (1) corresponds to vote share in 2016 whereas column (2) corresponds to vote share in 2020. The values of controls are those of 2016. Controls include:  position to which the candidate is running, education, age, nationality, gender, party to which the candidate belongs, if the candidate is an incumbent and the following characteristics of the municipality in which the candidate is running: share of white candidates and apt voters. We also include state fixed effects. Robust standard errors presented in parentheses.",
          out="cornwell_table3_1_controls_2016.tex")

##Fin codigo tabla stargazer

brasil_long %>%
  select(race_16, race_20, w11, w10, w01, w00, white_16, white_20, type_switch_agreg)

names(brasil_long)

brasil_long %>%
  group_by(race_16) %>%
  summarise(n = n()/nrow(brasil_long))

brasil_long %>%
  group_by(race_20) %>%
  summarise(n = n()/nrow(brasil_long))

brasil_long %>%
  select(vote_share_16, vote_share_20, diff_vote_share)

lm(brasil_long_cor_normal$vote_share_20 ~ brasil_long_cor_normal$CD_GRAU_INSTRUCAO_16 +
     brasil_long_cor_normal$prefeito_16)

lm(brasil_long_cor_normal$vote_share_16 ~ brasil_long_cor_normal$CD_GRAU_INSTRUCAO_16 +
     brasil_long_cor_normal$prefeito_16)

lm(brasil_long_cor_normal$diff_vote_share ~ brasil_long_cor_normal$CD_GRAU_INSTRUCAO_16 +
     brasil_long_cor_normal$prefeito_16)


####Tables####

gt_ch_party_switchers
gt_characs_switched
gt_characs_type_switch
gt_janusz1_porcentajes
gt_janusz1_totales
gt_sw_per_party_16
gt_sw_per_party_20

as.character(as_latex(gt_characs_switched))

gtsave(gt_characs_switched, "characs_switched.tex")

gt_type_sw_agreg

gtsave(gt_type_sw_agreg, "characs_type_switch.tex")

gtsave(gt_janusz1_porcentajes, "janusz_percentages.tex")

gtsave(gt_janusz1_totales, "janusz_totals.tex")

####Eliminacion de variables####

rm(voters_per_mun_16)
rm(voters_per_mun_20)
rm(totales)
rm(table1_cornwell)
rm(switch_per_top3_parties_16)
rm(switch_per_top3_parties_20)
rm(share_whites_per_mun)
rm(share_whites_per_mun_16)
rm(share_whites_per_mun_20)
rm(results_turno_1_16)
rm(results_turno_1_20)
rm(result_per_cand_16)
rm(result_per_cand_20)
rm(porcentajes)
rm(perc_bis)
rm(party_names)
rm(diff_vote_share)
rm(characs_mun_switchers)
rm(characs_party_switchers)
rm(characs_segun_switched)
rm(characs_segun_type_switch)
rm(cands_rep_mun)
rm(cands_per_mun_16)
rm(cands_per_mun_20)
rm(brasil_viejo)

####datasets a excel####
write_xlsx(brasil,"Databases/candidate's data\\brazil.xlsx")
write_xlsx(brasil_long,"Databases/candidate's data\\brazil_long.xlsx")

write.csv(brasil,"Databases/candidate's data\\brazil_csv.csv", row.names = FALSE)
write.csv(brasil_long,"Databases/candidate's data\\brazil_wide_csv.xlsx", row.names = FALSE)
