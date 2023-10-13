library(tidyverse)
library(lubridate)
library(writexl)
library(readxl)
library(stats4)

####MAIN PANEL DATA####

#The OBJECTIVE now is to create a panel data with the candidates that
#participated in BOTH the 2016 and 2020 elections

####Prepare the date####

#I upload the raw datasets

#The information of the candidates in  2020:

brasil_cand_20 <- read_excel("candidate_info_20.xlsx", 
                             sheet = "data")

#The information of the candidates in  2016:

brasil_cand_16 = read_excel("candidate_info_16.xlsx", 
                            sheet = "data")

#The information of their results in the 2020 elections:

brasil_resultados_20 = read_excel("results_candidates_20.xlsx", 
                                  sheet = "data")

#The information of their results in the 2016 elections:

brasil_resultados_16 = read_excel("results_candidates_16.xlsx", 
                                  sheet = "data")

#Here I'll merge the information of the candidates with their results

brasil_16 = 
  merge(brasil_resultados_16,brasil_cand_16,by=c("SQ_CANDIDATO"))

brasil_20 = 
  merge(brasil_resultados_20,brasil_cand_20,by=c("SQ_CANDIDATO"))

#I keep just the candidates that participated in both elections

brasil_16 = 
  subset(brasil_16, NR_CPF_CANDIDATO %in% c(brasil_20$NR_CPF_CANDIDATO))

brasil_20 = 
  subset(brasil_20, NR_CPF_CANDIDATO %in% c(brasil_16$NR_CPF_CANDIDATO))

#Number of candidates:

cant_cand = length(c(unique(brasil_16$NR_CPF_CANDIDATO)))
length(c(unique(brasil_16$NR_CPF_CANDIDATO)))

####Data cleaning####

#Now I'll rename some variables and change some things

#First, I'll put a _16 at the end of the names pf the variables in the 2016
#dataset. The same for 2016.

names(brasil_16)
names_16 = paste(c(names(brasil_16)), "_16", sep="")
names_16

names_20 = paste(c(names(brasil_20)), "_20", sep="")

brasil_16 = setNames(object = brasil_16, names_16)
brasil_20 = setNames(object = brasil_20, names_20)

#I just keep the unique identifiers of the candidates the same so as to merge later

colnames(brasil_16)[colnames(brasil_16) == "NR_CPF_CANDIDATO_16"] = "NR_CPF_CANDIDATO"
colnames(brasil_20)[colnames(brasil_20) == "NR_CPF_CANDIDATO_20"] = "NR_CPF_CANDIDATO"


#Now I'll change some characteristics of the variables

#Gender

brasil_16 <- brasil_16 %>% 
  mutate(female_16 = case_when(CD_GENERO_16 == 4 ~ 1,
                               CD_GENERO_16 == 2 ~ 0))

brasil_16 = subset(brasil_16, select = -c(CD_GENERO_16))


brasil_20 <- brasil_20 %>% 
  mutate(female_20 = case_when(CD_GENERO_20 == 4 ~ 1,
                               CD_GENERO_20 == 2 ~ 0))

brasil_20 = subset(brasil_20, select = -c(CD_GENERO_20) )

#Race. 1: White. 2: Brown. 3: Black. 4: Asian. 5: Indigenous.

brasil_16 <- brasil_16 %>% 
  mutate(race = case_when(CD_COR_RACA_16 == 1 ~ 1,
                          CD_COR_RACA_16 == 2 ~ 3,
                          CD_COR_RACA_16 == 3 ~ 2,
                          CD_COR_RACA_16 == 4 ~ 4,
                          CD_COR_RACA_16 == 5 ~ 5))

brasil_16 = subset(brasil_16, select = -c(CD_COR_RACA_16) )


brasil_20 <- brasil_20 %>% 
  mutate(race = case_when(CD_COR_RACA_20 == 1 ~ 1,
                          CD_COR_RACA_20 == 2 ~ 3,
                          CD_COR_RACA_20 == 3 ~ 2,
                          CD_COR_RACA_20 == 4 ~ 4,
                          CD_COR_RACA_20 == 5 ~ 5))

brasil_20 = subset(brasil_20, select = -c(CD_COR_RACA_20) )

#Incumbent.

unique(brasil_16$ST_REELEICAO_16)
unique(brasil_20$ST_REELEICAO_20)

brasil_16 <- brasil_16 %>% 
  mutate(incumbent_16 = case_when(ST_REELEICAO_16 == "S" ~ 1,
                                  ST_REELEICAO_16 == "N" ~ 0))

brasil_16 = subset(brasil_16, select = -c(ST_REELEICAO_16) )


brasil_20 <- brasil_20 %>% 
  mutate(incumbent_20 = case_when(ST_REELEICAO_20 == "S" ~ 1,
                                  ST_REELEICAO_20 == "N" ~ 0))

brasil_20 = subset(brasil_20, select = -c(ST_REELEICAO_20) )

#If the candidate declared their personal goods 

unique(brasil_16$ST_DECLARAR_BENS_16)
unique(brasil_20$ST_DECLARAR_BENS_20)

brasil_16 <- brasil_16 %>% 
  mutate(declared_goods_16 = case_when(ST_DECLARAR_BENS_16 == "S" ~ 1,
                                       ST_DECLARAR_BENS_16 == "N" ~ 0))

brasil_16 = subset(brasil_16, select = -c(ST_DECLARAR_BENS_16) )

brasil_20 <- brasil_20 %>% 
  mutate(declared_goods_20 = case_when(ST_DECLARAR_BENS_20 == "S" ~ 1,
                                       ST_DECLARAR_BENS_20 == "N" ~ 0))

brasil_20 = subset(brasil_20, select = -c(ST_DECLARAR_BENS_20) )

#birth date

typeof(brasil_16$DT_NASCIMENTO[1])

brasil_16[brasil_16 == "2968-12-12"] <- "1968-12-12"

brasil_16 = mutate(brasil_16,
                   birth_date_16 = as.Date(DT_NASCIMENTO_16, format= "%y-%m-%d"))

brasil_20 = mutate(brasil_20,
                   birth_date_20 = as.Date(DT_NASCIMENTO_20, format= "%y-%m-%d"))

#Age

brasil_16 = mutate(brasil_16,
                   age_16 = as.numeric(difftime("2016-10-30", birth_date_16,
                                                unit="weeks"))/52.25)


brasil_20 = mutate(brasil_20,
                   age_20 = as.numeric(difftime("2020-11-15", birth_date_20,
                                                unit="weeks"))/52.25)


#Now I'll make each row unique for each candidate

brasil_16 = brasil_16 %>% filter(NR_TURNO_16 == 1)
brasil_16 = brasil_16 %>% filter(CD_TIPO_ELEICAO_16 == 2)

brasil_20 = brasil_20 %>% filter(NR_TURNO_20 == 1)
brasil_20 = brasil_20 %>% filter(CD_TIPO_ELEICAO_20 == 2)

brasil_16 = 
  brasil_16 %>%
  group_by(NR_CPF_CANDIDATO) %>%
  summarise(SQ_CANDIDATO_16 = unique(SQ_CANDIDATO_16)[1],
            year_16 = unique(ANO_ELEICAO_16)[1],
            date_of_election_16 = unique(DT_ELEICAO_16)[1],
            municip_16=unique(CD_MUNICIPIO_16)[1],
            age_16 = mean(age_16),
            educ_level_16 = unique(CD_GRAU_INSTRUCAO_16)[1],
            female_16 = unique(female_16)[1],
            race_16 = unique(race)[1],
            position_16 = unique(CD_CARGO_16)[1],
            party_number_16=unique(NR_PARTIDO_16)[1],
            nationality_16=unique(CD_NACIONALIDADE_16)[1],
            result_type_16=unique(CD_SIT_TOT_TURNO.y_16)[1],
            state_of_birth_16=unique(SG_UF_NASCIMENTO_16)[1],
            state_16=unique(SG_UF_16)[1],
            coalition_16=unique(SQ_COLIGACAO_16)[1],
            municipality_of_birth_16=unique(NM_MUNICIPIO_NASCIMENTO_16)[1],
            occupation_16=unique(CD_OCUPACAO_16)[1],
            incumbent_16=unique(incumbent_16)[1],
            electoral_unit_16=unique(SG_UE_16)[1],
            birth_date_16=unique(birth_date_16)[1],
            marital_status_16=unique(CD_ESTADO_CIVIL_16)[1],
            max_campaign_funds_16=unique(VR_DESPESA_MAX_CAMPANHA_16)[1],
            declared_goods_16=unique(declared_goods_16)[1],
            age_16 = unique(age_16)[1],
            votes_16 = sum(QT_VOTOS_NOMINAIS_16),
            quantity_zones_16 = n())

brasil_20 = 
  brasil_20 %>%
  group_by(NR_CPF_CANDIDATO) %>%
  summarise(SQ_CANDIDATO_20 = unique(SQ_CANDIDATO_20)[1],
            year_20 = unique(ANO_ELEICAO_20)[1],
            date_of_election_20 = unique(DT_ELEICAO_20)[1],
            municip_20=unique(CD_MUNICIPIO_20)[1],
            age_20 = mean(age_20),
            educ_level_20 = unique(CD_GRAU_INSTRUCAO_20)[1],
            female_20 = unique(female_20)[1],
            race_20 = unique(race)[1],
            position_20 = unique(CD_CARGO_20)[1],
            party_number_20=unique(NR_PARTIDO_20)[1],
            nationality_20=unique(CD_NACIONALIDADE_20)[1],
            result_type_20=unique(CD_SIT_TOT_TURNO.y_20)[1],
            state_of_birth_20=unique(SG_UF_NASCIMENTO_20)[1],
            state_20=unique(SG_UF_20)[1],
            coalition_20=unique(SQ_COLIGACAO_20)[1],
            municipality_of_birth_20=unique(NM_MUNICIPIO_NASCIMENTO_20)[1],
            occupation_20=unique(CD_OCUPACAO_20)[1],
            incumbent_20=unique(incumbent_20)[1],
            electoral_unit_20=unique(SG_UE_20)[1],
            birth_date_20=unique(birth_date_20)[1],
            marital_status_20=unique(CD_ESTADO_CIVIL_20)[1],
            max_campaign_funds_20=unique(VR_DESPESA_MAX_CAMPANHA_20)[1],
            declared_goods_20=unique(declared_goods_20)[1],
            age_20 = unique(age_20)[1],
            votes_20 = sum(QT_VOTOS_NOMINAIS_20),
            quantity_zones_20 = n())

####Merge####

#I do this according to their ID number (NR_CPF_CANDIDATO)

#First, I construct a long dataset

brasil_long = merge(brasil_16,brasil_20,by=c("NR_CPF_CANDIDATO"))

#And then a wide dataset

names_16 = names(brasil_16)
new_names_16 = gsub('_16', '', names_16)

names_20 = names(brasil_20)
new_names_20 = gsub('_20', '', names_20)

brasil_16 = setNames(object = brasil_16, new_names_16)
brasil_20 = setNames(object = brasil_20, new_names_20)


brasil_wide = bind_rows(brasil_16, brasil_20)


####Race variables####

unique(brasil_long$race_16)
unique(brasil_long$race_20)

#I recode the variable race for both the long and the wide dataset

brasil_long =
  brasil_long %>%
  mutate(race_20 = case_when(race_20 == 1 ~ 1,
                              race_20 == 2 ~ 2,
                              race_20 == 3 ~ 3,
                              race_20 == 4 ~ 4,
                              race_20 == 5 ~ 5,
                              is.na(race_20) ~ 6))

brasil_wide =
  brasil_wide %>%
  mutate(race = case_when(race == 1 ~ 1,
                          race == 2 ~ 2,
                          race == 3 ~ 3,
                          race == 4 ~ 4,
                          race == 5 ~ 5,
                          is.na(race) ~ 6))

#Now I'll construct all the race variables:

brasil_long =
  brasil_long %>%
  mutate(switched_race = ifelse(race_16 != race_20, 1, 0)) %>%
  mutate(r11 = ifelse(race_16 == 1 & race_20 == 1, 1, 0)) %>%
  mutate(r12 = ifelse(race_16 == 1 & race_20 == 2, 1, 0)) %>%
  mutate(r13 = ifelse(race_16 == 1 & race_20 == 3, 1, 0)) %>%
  mutate(r14 = ifelse(race_16 == 1 & race_20 == 4, 1, 0)) %>%
  mutate(r15 = ifelse(race_16 == 1 & race_20 == 5, 1, 0)) %>%
  mutate(r16 = ifelse(race_16 == 1 & race_20 == 6, 1, 0)) %>%
  mutate(r21 = ifelse(race_16 == 2 & race_20 == 1, 1, 0)) %>%
  mutate(r22 = ifelse(race_16 == 2 & race_20 == 2, 1, 0)) %>%
  mutate(r23 = ifelse(race_16 == 2 & race_20 == 3, 1, 0)) %>%
  mutate(r24 = ifelse(race_16 == 2 & race_20 == 4, 1, 0)) %>%
  mutate(r25 = ifelse(race_16 == 2 & race_20 == 5, 1, 0)) %>%
  mutate(r26 = ifelse(race_16 == 2 & race_20 == 6, 1, 0)) %>%
  mutate(r31 = ifelse(race_16 == 3 & race_20 == 1, 1, 0)) %>%
  mutate(r32 = ifelse(race_16 == 3 & race_20 == 2, 1, 0)) %>%
  mutate(r33 = ifelse(race_16 == 3 & race_20 == 3, 1, 0)) %>%
  mutate(r34 = ifelse(race_16 == 3 & race_20 == 4, 1, 0)) %>%
  mutate(r35 = ifelse(race_16 == 3 & race_20 == 5, 1, 0)) %>%
  mutate(r36 = ifelse(race_16 == 3 & race_20 == 6, 1, 0)) %>%
  mutate(r41 = ifelse(race_16 == 4 & race_20 == 1, 1, 0)) %>%
  mutate(r42 = ifelse(race_16 == 4 & race_20 == 2, 1, 0)) %>%
  mutate(r43 = ifelse(race_16 == 4 & race_20 == 3, 1, 0)) %>%
  mutate(r44 = ifelse(race_16 == 4 & race_20 == 4, 1, 0)) %>%
  mutate(r45 = ifelse(race_16 == 4 & race_20 == 5, 1, 0)) %>%
  mutate(r46 = ifelse(race_16 == 4 & race_20 == 6, 1, 0)) %>%
  mutate(r51 = ifelse(race_16 == 5 & race_20 == 1, 1, 0)) %>%
  mutate(r52 = ifelse(race_16 == 5 & race_20 == 2, 1, 0)) %>%
  mutate(r53 = ifelse(race_16 == 5 & race_20 == 3, 1, 0)) %>%
  mutate(r54 = ifelse(race_16 == 5 & race_20 == 4, 1, 0)) %>%
  mutate(r55 = ifelse(race_16 == 5 & race_20 == 5, 1, 0)) %>%
  mutate(r56 = ifelse(race_16 == 5 & race_20 == 6, 1, 0)) %>%
  mutate(type_of_switch = case_when(r11 == 1 ~ "r11",
                                    r12 == 1 ~ "r12",
                                    r13 == 1 ~ "r13",
                                    r14 == 1 ~ "r14",
                                    r15 == 1 ~ "r15",
                                    r16 == 1 ~ "r16",
                                    r21 == 1 ~ "r21",
                                    r22 == 1 ~ "r22",
                                    r23 == 1 ~ "r23",
                                    r24 == 1 ~ "r24",
                                    r25 == 1 ~ "r25",
                                    r26 == 1 ~ "r26",
                                    r31 == 1 ~ "r31",
                                    r32 == 1 ~ "r32",
                                    r33 == 1 ~ "r33",
                                    r34 == 1 ~ "r34",
                                    r35 == 1 ~ "r35",
                                    r36 == 1 ~ "r36",
                                    r41 == 1 ~ "r41",
                                    r42 == 1 ~ "r42",
                                    r43 == 1 ~ "r43",
                                    r44 == 1 ~ "r44",
                                    r45 == 1 ~ "r45",
                                    r46 == 1 ~ "r46",
                                    r51 == 1 ~ "r51",
                                    r52 == 1 ~ "r52",
                                    r53 == 1 ~ "r53",
                                    r54 == 1 ~ "r54",
                                    r55 == 1 ~ "r55",
                                    r56 == 1 ~ "r56")) %>%
  mutate(white_16 = case_when(race_16 == 1 ~ 1,
                              race_16 == 2 ~ 0,
                              race_16 == 3 ~ 0,
                              race_16 == 4 ~ 0,
                              race_16 == 5 ~ 0)) %>%
  mutate(white_20 = case_when(race_20 == 1 ~ 1,
                              race_20 == 2 ~ 0,
                              race_20 == 3 ~ 0,
                              race_20 == 4 ~ 0,
                              race_20 == 5 ~ 0,
                              race_20 == 6 ~ 0)) %>%
  mutate(w11 = ifelse(white_16 == 1 & white_20 == 1, 1, 0)) %>%
  mutate(w10 = ifelse(white_16 == 1 & white_20 == 0, 1, 0)) %>%
  mutate(w01 = ifelse(white_16 == 0 & white_20 == 1, 1, 0)) %>%
  mutate(w00 = ifelse(white_16 == 0 & white_20 == 0, 1, 0))


subset_brasil_long <- brasil_long %>% select(NR_CPF_CANDIDATO,
                                             r11,
                                             r12,
                                             r13,
                                             r14,
                                             r15,
                                             r16,
                                             r21,
                                             r22,
                                             r23,
                                             r24,
                                             r25,
                                             r26,
                                             r31,
                                             r32,
                                             r33,
                                             r34,
                                             r35,
                                             r36,
                                             r41,
                                             r42,
                                             r43,
                                             r44,
                                             r45,
                                             r46,
                                             r51,
                                             r52,
                                             r53,
                                             r54,
                                             r55,
                                             r56,
                                             type_of_switch,
                                             w11,
                                             w10,
                                             w01,
                                             w00)

brasil_wide = merge(brasil_wide, subset_brasil_long, by = "NR_CPF_CANDIDATO")

brasil_wide = brasil_wide %>% mutate(white = ifelse(race == 1, 1, 0))

####Results in elections (if candidate won and vote share)####

#Variable to indicate if the candidate won the election

brasil_wide =
  brasil_wide %>%
  mutate(won = case_when(result_type == 1 ~ 1,
                         result_type == 2 ~ 1,
                         result_type == 3 ~ 1,
                         result_type == 4 ~ 0,
                         result_type == 5 ~ 0,
                         result_type == 6 ~ 0,
                         result_type == -1 ~ 0,))

brasil_long =
  brasil_long %>%
  mutate(won_16 = case_when(result_type_16 == 1 ~ 1,
                         result_type_16 == 2 ~ 1,
                         result_type_16 == 3 ~ 1,
                         result_type_16 == 4 ~ 0,
                         result_type_16 == 5 ~ 0,
                         result_type_16 == 6 ~ 0,
                         result_type_16 == -1 ~ 0,))
brasil_long =
  brasil_long %>%
  mutate(won_20 = case_when(result_type_20 == 1 ~ 1,
                            result_type_20 == 2 ~ 1,
                            result_type_20 == 3 ~ 1,
                            result_type_20 == 4 ~ 0,
                            result_type_20 == 5 ~ 0,
                            result_type_20 == 6 ~ 0,
                            result_type_20 == -1 ~ 0,))

#Vote share: For candidate i: Vote_share_i = votes_i / quant_voters_municipality
#Then I have to obtain information on the electorate
#I upload a database with the quantity of voters per municipality 

voters_per_mun_16 = read_excel("voters_per_mun_16.xlsx")

voters_per_mun_20 = read_excel("voters_per_mun_20.xlsx")

voters_per_mun = merge(voters_per_mun_16, voters_per_mun_20, by = "municip")

voters_per_mun_16 <- voters_per_mun_16 %>% rename(municip_16 = municip)

voters_per_mun_20 <- voters_per_mun_20 %>% rename(municip_20 = municip)

#Now I merge it to the long dataset

brasil_long = merge(brasil_long, voters_per_mun_16, by="municip_16")

brasil_long = merge(brasil_long, voters_per_mun_20, by="municip_20")

#And now to the wide dataset

brasil_wide = merge(brasil_wide, voters_per_mun, by="municip")

brasil_wide = 
  brasil_wide %>% 
  mutate(quant_voters_mun = case_when(year == 2016 ~ quant_voters_mun_16,
                                      year == 2020 ~ quant_voters_mun_20))

brasil_wide = 
  brasil_wide %>% 
  mutate(quant_aptos_mun = case_when(year == 2016 ~ quant_aptos_mun_16,
                                     year == 2020 ~ quant_aptos_mun_20))

brasil_wide <- brasil_wide %>%
  select(-quant_voters_mun_16, -quant_aptos_mun_16,
         -quant_voters_mun_20, -quant_aptos_mun_20)

#Now I'll calculate the vote share:

brasil_long =
  brasil_long %>%
  mutate(vote_share_16 = votes_16 / quant_voters_mun_16)

brasil_long =
  brasil_long %>%
  mutate(vote_share_20 = votes_20 / quant_voters_mun_20)

brasil_wide =
  brasil_wide %>%
  mutate(vote_share = votes /quant_voters_mun)

summary(brasil_wide$vote_share)

rm(voters_per_mun)

####Other variables####

##Quantity of candidates in the municipality
#For this I'll use the dataset that has all the candidates.

names(brasil_resultados_16)
names(brasil_resultados_20)

brasil_resultados_16 = brasil_resultados_16 %>% filter(CD_ELEICAO == 220)

brasil_resultados_20 = brasil_resultados_20 %>% filter(CD_ELEICAO == 426)

cands_per_mun_16 = 
  brasil_resultados_16 %>%
  group_by(CD_MUNICIPIO) %>%
  summarise(quant_cands_mun_16 = length(unique(SQ_CANDIDATO)))

cands_per_mun_20 = 
  brasil_resultados_20 %>%
  group_by(CD_MUNICIPIO) %>%
  summarise(quant_cands_mun_20 = length(unique(SQ_CANDIDATO)))

#Now I merge it to the wide dataset

cands_per_mun_16 <- cands_per_mun_16 %>% rename(municip = CD_MUNICIPIO)

cands_per_mun_20 <- cands_per_mun_20 %>% rename(municip = CD_MUNICIPIO)

brasil_wide = merge(brasil_wide, cands_per_mun_16, by = "municip")
brasil_wide = merge(brasil_wide, cands_per_mun_20, by = "municip")

brasil_wide = 
  brasil_wide %>%
  mutate(quant_cands_mun = case_when(year == 2016 ~ quant_cands_mun_16,
                                     year == 2020 ~ quant_cands_mun_20)) %>%
  select(-quant_cands_mun_16, -quant_cands_mun_20)

#Now I merge it to the long dataset

cands_per_mun_16 <- cands_per_mun_16 %>% rename(municip_16 = municip)

cands_per_mun_20 <- cands_per_mun_20 %>% rename(municip_20 = municip)

brasil_long = merge(brasil_long, cands_per_mun_16, by="municip_16")

brasil_long = merge(brasil_long, cands_per_mun_20, by="municip_20")


rm(cands_per_mun_16)
rm(cands_per_mun_20)


##Percentage of candidates runniing twice 

cands_rep_per_mun = 
  brasil_wide %>%
  group_by(municip, year) %>%
  summarise(quant_cands_rep = n())

#For the wide dataset

brasil_wide = merge(brasil_wide, cands_rep_per_mun,
               by=c("municip", "year"), all.x=TRUE)

brasil_wide =
  brasil_wide %>%
  mutate(perc_cands_twice = quant_cands_rep / quant_cands_mun)

#For the long dataset

cands_rep_per_mun_16 = cands_rep_per_mun %>% filter(year == 2016)
cands_rep_per_mun_20 = cands_rep_per_mun %>% filter(year == 2020)

cands_rep_per_mun_16 = cands_rep_per_mun_16 %>% select(-year)
cands_rep_per_mun_20 = cands_rep_per_mun_20 %>% select(-year)

cands_rep_per_mun_16 = cands_rep_per_mun_16 %>% rename(municip_16 = municip)
cands_rep_per_mun_16 = cands_rep_per_mun_16 %>% rename(quant_cands_rep_16 = quant_cands_rep)

cands_rep_per_mun_20 = cands_rep_per_mun_20 %>% rename(municip_20 = municip)
cands_rep_per_mun_20 = cands_rep_per_mun_20 %>% rename(quant_cands_rep_20 = quant_cands_rep)

brasil_long = merge(brasil_long, cands_rep_per_mun_16, by="municip_16")
brasil_long = merge(brasil_long, cands_rep_per_mun_20, by="municip_20")

rm(cands_rep_per_mun, cands_rep_per_mun_16, cands_rep_per_mun_20)

##Candidate changed municpality

#Variable to see if the candidate changed municiálity between elections

brasil_long = brasil_long %>% mutate(changed_mun = ifelse(municip_16 != municip_20, 1, 0))

summary(brasil_long$changed_mun)

select_brasil = brasil_long %>% select(NR_CPF_CANDIDATO, changed_mun)

brasil_wide = merge(brasil_wide, select_brasil, by= "NR_CPF_CANDIDATO")

rm(select_brasil)

##Quantity of white candidates per municipality

names(brasil_cand_16)
names(brasil_cand_20)

brasil_cand_16 = 
  brasil_cand_16 %>% 
  filter(CD_TIPO_ELEICAO == 2 & NR_TURNO == 1 & !is.na(NR_CPF_CANDIDATO))

brasil_cand_20 = 
  brasil_cand_20 %>% 
  filter(CD_TIPO_ELEICAO == 2 & NR_TURNO == 1 & !is.na(NR_CPF_CANDIDATO))

race_brasil_16 = 
  merge(brasil_resultados_16,brasil_cand_16,by=c("SQ_CANDIDATO"))
  
race_brasil_20 = 
  merge(brasil_resultados_20,brasil_cand_20,by=c("SQ_CANDIDATO"))

#Now I create a dataframe with the quantity of white candidates per municipality in each year

race_brasil_16 =
  race_brasil_16 %>%
  group_by(NR_CPF_CANDIDATO, CD_MUNICIPIO, CD_COR_RACA) %>%
  summarise(n = n()) %>%
  select(-n) %>%
  group_by(CD_MUNICIPIO) %>%
  summarise(whites_per_mun_16 = sum(CD_COR_RACA == 1)) %>%
  rename(municip_16 = CD_MUNICIPIO)

race_brasil_20 =
  race_brasil_20 %>%
  group_by(NR_CPF_CANDIDATO, CD_MUNICIPIO, CD_COR_RACA) %>%
  summarise(n = n()) %>%
  select(-n) %>%
  group_by(CD_MUNICIPIO) %>%
  summarise(whites_per_mun_20 = sum(CD_COR_RACA == 1)) %>%
  rename(municip_20 = CD_MUNICIPIO)

#Now I merge it to the long dataset

brasil_long = merge(brasil_long, race_brasil_16, by="municip_16")
brasil_long = merge(brasil_long, race_brasil_20, by="municip_20")

#And now to the wide dataset

race_brasil_16 = race_brasil_16 %>% rename(municip = municip_16)
race_brasil_20 = race_brasil_20 %>% rename(municip = municip_20)

brasil_wide = merge(brasil_wide, race_brasil_16, by="municip")
brasil_wide = merge(brasil_wide, race_brasil_20, by="municip")

brasil_wide = 
  brasil_wide %>%
  mutate(whites_per_mun = case_when(year == 2016 ~ whites_per_mun_16,
                                    year == 2020 ~ whites_per_mun_20)) %>%
  select(-whites_per_mun_16, -whites_per_mun_20)

rm(race_brasil_16, race_brasil_20)

##Voter turnout

brasil_long =
  brasil_long %>% 
  mutate(voter_turnout_16 = quant_voters_mun_16 / quant_aptos_mun_16) %>%
  mutate(voter_turnout_20 = quant_voters_mun_20 / quant_aptos_mun_20)

brasil_wide =
  brasil_wide %>%
  mutate(voter_turnout = quant_voters_mun / quant_aptos_mun)


####Cleaning dataset with all candidates####
#The objective is to create a clean dataset with some information for all the 
#candidates in the 2016 and 2020 election

#First I merge the datasets

brasil_16_all_cands = 
  merge(brasil_cand_16, brasil_resultados_16,by=c("SQ_CANDIDATO"))

brasil_20_all_cands = 
  merge(brasil_cand_20, brasil_resultados_20,by=c("SQ_CANDIDATO"))

#Now I'll just keep the variable of interest

brasil_16_all_cands =
  brasil_16_all_cands %>%
  group_by(NR_CPF_CANDIDATO) %>%
  summarise(birth_16 = unique(DT_NASCIMENTO)[1],
            gender_16 = unique(CD_GENERO)[1],
            education_16 = unique(CD_GRAU_INSTRUCAO)[1],
            state_16 = unique(SG_UF)[1],
            incumbent_16 = unique(ST_REELEICAO)[1],
            nationality_16 = unique(CD_NACIONALIDADE)[1],
            votes_16 = sum(QT_VOTOS_NOMINAIS),
            race_16 = unique(CD_COR_RACA)[1],
            date_of_election_16 = unique(DT_ELEICAO)[1],
            result_type_16 = unique(CD_SIT_TOT_TURNO.y)[1],
            municip_16 = unique(CD_MUNICIPIO)[1],
            quantit_of_zones_16 = n())

brasil_20_all_cands =
  brasil_20_all_cands %>%
  group_by(NR_CPF_CANDIDATO) %>%
  summarise(birth_20 = unique(DT_NASCIMENTO)[1],
            gender_20 = unique(CD_GENERO)[1],
            education_20 = unique(CD_GRAU_INSTRUCAO)[1],
            state_20 = unique(SG_UF)[1],
            incumbent_20 = unique(ST_REELEICAO)[1],
            nationality_20 = unique(CD_NACIONALIDADE)[1],
            votes_20 = sum(QT_VOTOS_NOMINAIS),
            race_20 = unique(CD_COR_RACA)[1],
            date_of_election_20 = unique(DT_ELEICAO)[1],
            result_type_20 = unique(CD_SIT_TOT_TURNO.y)[1],
            municip_20 = unique(CD_MUNICIPIO)[1],
            quantit_of_zones_20 = n())

#I add the information about the voters

brasil_16_all_cands = merge(brasil_16_all_cands, voters_per_mun_16, by="municip_16")

brasil_20_all_cands = merge(brasil_20_all_cands, voters_per_mun_20, by="municip_20")

#I calculate the vote share

brasil_16_all_cands = 
  brasil_16_all_cands %>%
  mutate(vote_share_16 = votes_16 / quant_voters_mun_16)

brasil_20_all_cands = 
  brasil_20_all_cands %>%
  mutate(vote_share_20 = votes_20 / quant_voters_mun_20)

##I recode the variables

#Gender

brasil_16_all_cands <- brasil_16_all_cands %>% 
  mutate(gender_16 = case_when(gender_16 == 4 ~ 0,
                               gender_16 == 2 ~ 1))

brasil_20_all_cands <- brasil_20_all_cands %>% 
  mutate(gender_20 = case_when(gender_20 == 4 ~ 0,
                               gender_20 == 2 ~ 1))

#Race

brasil_16_all_cands <- brasil_16_all_cands %>% 
  mutate(race_16 = case_when(race_16 == 1 ~ 1,
                             race_16 == 2 ~ 3,
                             race_16 == 3 ~ 2,
                             race_16 == 4 ~ 4,
                             race_16 == 5 ~ 5))

brasil_20_all_cands <- brasil_20_all_cands %>% 
  mutate(race_20 = case_when(race_20 == 1 ~ 1,
                             race_20 == 2 ~ 3,
                             race_20 == 3 ~ 2,
                             race_20 == 4 ~ 4,
                             race_20 == 5 ~ 5))

#Incumbent

brasil_16_all_cands <- brasil_16_all_cands %>% 
  mutate(incumbent_16 = case_when(incumbent_16 == "S" ~ 1,
                                  incumbent_16 == "N" ~ 0))

brasil_20_all_cands <- brasil_20_all_cands %>% 
  mutate(incumbent_20 = case_when(incumbent_20 == "S" ~ 1,
                                  incumbent_20 == "N" ~ 0))

#Birth date

brasil_16_all_cands[brasil_16_all_cands == "2968-12-12"] <- "1968-12-12"

brasil_16_all_cands = mutate(brasil_16_all_cands,
                   birth_16 = as.Date(birth_16, format= "%y-%m-%d"))

brasil_20_all_cands = mutate(brasil_20_all_cands,
                             birth_20 = as.Date(birth_20, format= "%y-%m-%d"))

#Age

brasil_16_all_cands = mutate(brasil_16_all_cands,
                   age_16 = as.numeric(difftime("2016-10-30", birth_16,
                                                unit="weeks"))/52.25)

brasil_20_all_cands = mutate(brasil_20_all_cands,
                             age_20 = as.numeric(difftime("2020-11-15", birth_20,
                                                          unit="weeks"))/52.25)

#If candidate is white

brasil_16_all_cands = brasil_16_all_cands %>% mutate(white_16 = ifelse(race_16 == 1, 1, 0))

brasil_20_all_cands = brasil_20_all_cands %>% mutate(white_20 = ifelse(race_20 == 1, 1, 0))

#If candidate won

brasil_16_all_cands =
  brasil_16_all_cands %>%
  mutate(won_16 = case_when(result_type_16 == 1 ~ 1,
                            result_type_16 == 2 ~ 1,
                            result_type_16 == 3 ~ 1,
                            result_type_16 == 4 ~ 0,
                            result_type_16 == 5 ~ 0,
                            result_type_16 == 6 ~ 0,
                            result_type_16 == -1 ~ 0,))

brasil_20_all_cands =
  brasil_20_all_cands %>%
  mutate(won_20 = case_when(result_type_20 == 1 ~ 1,
                            result_type_20 == 2 ~ 1,
                            result_type_20 == 3 ~ 1,
                            result_type_20 == 4 ~ 0,
                            result_type_20 == 5 ~ 0,
                            result_type_20 == 6 ~ 0,
                            result_type_20 == -1 ~ 0,))



