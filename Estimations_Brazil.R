#NOTE 1: This script was in collaboration with the main researchers of the 
#project, so many thing are not of my authorship.

#NOTE 2: Please run the "code_panel_data.R" script before running this script

library(psych)
library(DescTools)
library(data.table)
library(xtable)
library(RCT)
library(knitr)
library(stargazer)
library(broom)
library(ggplot2)
library(hrbrthemes)
library(plotrix)
library(glmnet)
library(missForest)
library(randomForest)
library(dplyr)
library(lmtest)
library(tidyverse)
library(sandwich)
library(miceadds)
library(gt)

################################################################################ 
################################################################################ 
######################### Race switching in Brazil #############################
################################################################################ 
################################################################################ 

# This script performs the analysis for the project 
# Race switching in Brazil
# by GJ Clochard and J Flory



################################################################################ 
################################ Importing data ################################
################################################################################ 


data_brazil <- brasil_long

brazil_16 <- brasil_16_all_cands

brazil_20 <- brasil_20_all_cands

rm(brasil_long, brasil_16_all_cands, brasil_20_all_cands)


################################################################################ 
################################ Data cleaning #################################
################################################################################ 


data_brazil <- 
  data_brazil %>%
  mutate(
    birth_16 = birth_date_16,
    gender_16 = case_when(female_16 == 1 ~ 0,
                          female_16 == 0 ~ 1),
    education_16 = educ_level_16,
    state_16 = state_16,
    incumbent_16 = incumbent_16,
    nationality_16 = nationality_16,
    brazilian_16 = as.numeric(nationality_16 == 1),
    percent_16 = vote_share_16*100,
    city_size_16 = quant_aptos_mun_16,
    birth_20 = birth_date_20,
    gender_20 = case_when(female_20 == 1 ~ 0,
                          female_20 == 0 ~ 1),
    education_20 = educ_level_20,
    state_20 = state_20,
    incumbent_20 = incumbent_20,
    nationality_20 = nationality_20,
    brazilian_20 = as.numeric(nationality_20 == 1),
    percent_20 = vote_share_20*100,
    city_size_20 = quant_aptos_mun_20,
    diff_percent = percent_20 - percent_16,
    same_gender = as.numeric(gender_16 == gender_20),
    same_education = as.numeric(education_16 == education_20),
    same_birth = as.numeric(birth_20 == birth_16),
    same_nationality = as.numeric(nationality_16 == nationality_20),
    same_race = as.numeric(race_20==race_16),
    different_gender = as.numeric(gender_16 != gender_20),
    different_education = as.numeric(education_16 != education_20),
    different_birth = as.numeric(birth_20 != birth_16),
    different_nationality = as.numeric(nationality_16 != nationality_20),
    different_state = as.numeric(state_16!=state_20),
    different_race = as.numeric(race_20!=race_16),
    age_16 = ifelse(birth_16 == "2968-12-12", as.numeric(difftime("2016-10-30", "1968-12-12", unit="weeks"))/52.25, age_16),
    birth_16 = ifelse(birth_16 == "2968-12-12", "1968-12-12", birth_16),
    lower_educ = as.numeric(education_20<education_16),
    same_city = as.numeric(municip_20==municip_16),
    educ_16 = ifelse(education_16 <= 3,
                     0,
                     ifelse(education_16 <= 5 & education_16 > 3,
                            1,
                            ifelse(education_16 <= 7 & education_16 > 5,
                                   2,
                                   3)
                     )
    ),
    educ_20 = ifelse(education_20 <= 3,
                     0,
                     ifelse(education_20 <= 5 & education_20 > 3,
                            1,
                            ifelse(education_20 <= 7 & education_20 > 5,
                                   2,
                                   3)
                     )
    ),
    same_educ = as.numeric(educ_16==educ_20),
    lower_educ = as.numeric(educ_16>educ_20)
  )

brazil_16 <-
  brazil_16 %>%
  mutate(
    birth_16 = birth_16,
    gender_16 = gender_16,
    education_16 = education_16,
    state_16 = state_16,
    incumbent_16 = incumbent_16,
    nationality_16 = nationality_16,
    brazilian_16 = as.numeric(nationality_16 == 1),
    percent_16 = vote_share_16*100,
    city_size_16 = quant_aptos_mun_16,
    white_16 = white_16,
    vote_share_16 = vote_share_16,
    age_16 = age_16,
    won_16 = won_16,
    age_16 = ifelse(birth_16 == "2968-12-12", as.numeric(difftime("2016-10-30", "1968-12-12", unit="weeks"))/52.25, age_16),
    birth_16 = ifelse(birth_16 == "2968-12-12", "1968-12-12", birth_16),
    black_16 = as.numeric(race_16 == 3)
  )

brazil_20 <-
  brazil_20 %>%
  mutate(
    birth_20 = birth_20,
    gender_20 = gender_20,
    education_20 = education_20,
    state_20 = state_20,
    incumbent_20 = incumbent_20,
    nationality_20 = nationality_20,
    brazilian_20 = as.numeric(nationality_20 == 1),
    percent_20 = vote_share_20*100,
    city_size_20 = quant_aptos_mun_20,
    white_20 = white_20,
    vote_share_20 = vote_share_20,
    age_20 = age_20,
    won_20 = won_20,
    black_20 = as.numeric(race_20 == 3)
  )



################################################################################ 
############################ Definition of controls ############################
################################################################################ 


controls_16 <- 
  c("gender_16", "age_16", "incumbent_16", "brazilian_16",
    "city_size_16")

fixed_effects_16 <- 
  c("factor(state_16)", "factor(education_16)")

controls_20 <- 
  c("gender_20", "age_20", "incumbent_20", "brazilian_20",
    "city_size_20")

fixed_effects_20 <- 
  c("factor(state_20)", "factor(education_20)")

combined_controls <-
  c("gender_16", "gender_20",
    "age_16", "age_20",
    "incumbent_16", "incumbent_20",
    "brazilian_16", "brazilian_20",
    "city_size_16", "city_size_20"
    )

race_history <-
  c("w11", "w10", "w01")

moving_char <- 
  c("different_gender",
    "different_birth",
    "different_nationality" ,
    "different_state",
    "lower_educ",
    "different_race"
  )

labels_moving_char <-
  c("Different gender",
    "Different birth date",
    "Different nationality",
    "Different state of birth",
    "Lower education",
    "Different race"
  )

  

################################################################################ 
############################# Replication CRS 2017 #############################
################################################################################ 

############# Figure 1: Share of white candidates per city ############# 

##### For 2016

share_whites_per_mun_16 =
  brazil_16 %>%
  group_by(municip_16) %>%
  summarise(share_of_whites = sum(white_16)/n())

ggplot(share_whites_per_mun_16, aes(x=share_of_whites)) +
  geom_histogram(aes(y = (..count..)/sum(..count..)),
                 color="darkblue", fill="lightblue",
                 binwidth = (1/30)) +
  labs(x="Share of White candidates", y = "Fraction") +
  theme_minimal() + 
  theme(plot.title = element_text(hjust = 0.5))


##### For 2020

share_whites_per_mun_20 =
  brazil_20 %>%
  group_by(municip_20) %>%
  summarise(share_of_whites = sum(white_20)/n())

ggplot(share_whites_per_mun_20, aes(x=share_of_whites)) +
  geom_histogram(aes(y = (..count..)/sum(..count..)),
                 color="darkblue", fill="lightblue",
                 binwidth = (1/30)) +
  labs(x="Share of White candidates", y = "Fraction") +
  theme_minimal() + 
  theme(plot.title = element_text(hjust = 0.5))


############# Table 1: Descriptive statistics ############# 


###Construction of each column###

##Variables##

vars_table1 = c("11", "10", "01", "00", "white_16", "white_20",
                "male_16", "male_20", "age_16", "age_20", "educ_16",
                "educ_20", "incumb_16", "incumb_20", "obs_16", "obs_20")

##All candidates##

All_candidates = c(NA, NA, NA, NA, mean(brazil_16$white, na.rm = TRUE),
                   mean(brazil_20$white, na.rm = TRUE), mean(brazil_16$gender_16),
                   mean(brazil_20$gender_20, na.rm=TRUE), mean(brazil_16$age, na.rm = TRUE),
                   mean(brazil_20$age, na.rm = TRUE), mean(brazil_16$education_16),
                   mean(brazil_20$education_20), mean(brazil_16$incumbent_16),
                   mean(brazil_20$incumbent_20, na.rm=TRUE), nrow(brazil_16), nrow(brazil_20))

table1_cornwell = data.frame(vars_table1, All_candidates)

##Ran twice##

Ran_twice = c(mean(data_brazil$w11), mean(data_brazil$w10),
              mean(data_brazil$w01),
              mean(data_brazil$w00), mean(data_brazil$white_16, na.rm = TRUE),
              mean(data_brazil$white_20, na.rm = TRUE), mean(data_brazil$gender_16),
              mean(data_brazil$gender_20, na.rm=TRUE), mean(data_brazil$age_16, na.rm = TRUE),
              mean(data_brazil$age_20, na.rm = TRUE), mean(data_brazil$education_16),
              mean(data_brazil$education_20), mean(data_brazil$incumbent_16),
              mean(data_brazil$incumbent_20),
              nrow(data_brazil), nrow(data_brazil))
table1_cornwell = data.frame(table1_cornwell, Ran_twice)

##11##

cands_11 = data_brazil %>% filter(w11 == 1)

ch11 = c(mean(cands_11$w11), mean(cands_11$w10),
         mean(cands_11$w01),
         mean(cands_11$w00), mean(cands_11$white_16, na.rm = TRUE),
         mean(cands_11$white_20, na.rm = TRUE), mean(cands_11$gender_16),
         mean(cands_11$gender_20), mean(cands_11$age_16, na.rm = TRUE),
         mean(cands_11$age_20, na.rm = TRUE), mean(cands_11$education_16),
         mean(cands_11$education_20), mean(cands_11$incumbent_16),
         mean(cands_11$incumbent_20),
         nrow(cands_11), nrow(cands_11))
table1_cornwell = data.frame(table1_cornwell, ch11)

##10##

cands_10 = data_brazil %>% filter(w10 == 1)

ch10 = c(mean(cands_10$w11), mean(cands_10$w10),
         mean(cands_10$w01),
         mean(cands_10$w00), mean(cands_10$white_16, na.rm = TRUE),
         mean(cands_10$white_20, na.rm = TRUE), mean(cands_10$gender_16),
         mean(cands_10$gender_20), mean(cands_10$age_16, na.rm = TRUE),
         mean(cands_10$age_20, na.rm = TRUE), mean(cands_10$education_16),
         mean(cands_10$education_20), mean(cands_10$incumbent_16),
         mean(cands_10$incumbent_20),
         nrow(cands_10), nrow(cands_10))
table1_cornwell = data.frame(table1_cornwell, ch10)

##01##

cands_01 = data_brazil %>% filter(w01 == 1)

ch01 = c(mean(cands_01$w11), mean(cands_01$w10),
         mean(cands_01$w01),
         mean(cands_01$w00), mean(cands_01$white_16, na.rm = TRUE),
         mean(cands_01$white_20, na.rm = TRUE), mean(cands_01$gender_16),
         mean(cands_01$gender_20), mean(cands_01$age_16, na.rm = TRUE),
         mean(cands_01$age_20, na.rm = TRUE), mean(cands_01$education_16),
         mean(cands_01$education_20), mean(cands_01$incumbent_16),
         mean(cands_01$incumbent_20),
         nrow(cands_01), nrow(cands_01))
table1_cornwell = data.frame(table1_cornwell, ch01)

##00##

cands_00 = data_brazil %>% filter(w00 == 1)

ch00 = c(mean(cands_00$w11), mean(cands_00$w10),
         mean(cands_00$w01),
         mean(cands_00$w00), mean(cands_00$white_16, na.rm = TRUE),
         mean(cands_00$white_20, na.rm = TRUE), mean(cands_00$gender_16),
         mean(cands_00$gender_20), mean(cands_00$age_16, na.rm = TRUE),
         mean(cands_00$age_20, na.rm = TRUE), mean(cands_00$education_16),
         mean(cands_00$education_20), mean(cands_00$incumbent_16),
         mean(cands_00$incumbent_20),
         nrow(cands_00), nrow(cands_00))
table1_cornwell = data.frame(table1_cornwell, ch00)

###Presentation of the table###

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


############# Table 2: Racial vote gap ############# 

##### Version 1: Vote share, 2016

### Columns 1 & 2: all candidates 2016, without and with controls
### Columns 3 & 4: candidates who ran twice, vote 2016, without and with controls

### All candidates

## Without controls

lm1 <- 
  lm(
    as.formula(
      paste(
        "percent_16 ~ ", 
        " white_16 "
      )
    ), 
    data = brazil_16
  )

## With controls

lm2 <- 
  lm(
    as.formula(
      paste(
        "percent_16 ~ ", 
        paste(controls_16, collapse="+"), "+",
        paste(fixed_effects_16, collapse = "+"),
        "+ white_16 "
      )
    ), 
    data = brazil_16
  )


### Candidates who ran twice

## Without controls

lm3 <- 
  lm(
    as.formula(
      paste(
        "percent_16 ~ ", 
        " white_16 "
      )
    ), 
    data = data_brazil
  )

## With controls

lm4 <- 
  lm(
    as.formula(
      paste(
        "percent_16 ~ ", 
        paste(controls_16, collapse="+"), "+",
        paste(fixed_effects_16, collapse = "+"),
        "+ white_16 "
      )
    ), 
    data = data_brazil
  )


### Table exporting

collabels = c("All candidates", "All candidates", "Ran twice", "Ran twice")

covlabels = c("White 2016")

keeps = c("white_16")

notes = "The dependent variable is the vote share for the 2016 municipal election. * p<0.10, ** p<0.05, *** p<0.01. Standard errors in parentheses. Controls include age, level of education, gender, the number of eligible voters, incumbency, nationality and state fixed effects."

title = "Cross-sectional race gap in vote share, 2016"

label = "tab: race gap 2016 vote share"

stargazer(lm1, lm2, lm3, lm4,
          type='latex', 
          table.placement = "htbp",
          title = title,
          style = "all2",
          label = label,
          # align = TRUE,
          column.labels = collabels,
          covariate.labels = covlabels,
          keep = keeps,
          keep.stat = c("rsq", "n"),
          summary=FALSE,
          dep.var.caption = NULL,
          mean.sd = TRUE,
          digits = 3,
          notes = notes,
          out = paste(pathtables, "race_gap_vote_16.tex", sep="//"),
          notes.append = FALSE,
          notes.align = "l",
          header = FALSE
)


##### Version 2: Vote share, 2020

### Columns 1 & 2: all candidates 2020, without and with controls
### Columns 3 & 4: candidates who ran twice, vote 2020, without and with controls

### All candidates

## Without controls

lm1 <- 
  lm(
    as.formula(
      paste(
        "percent_20 ~ ", 
        " white_20 "
      )
    ), 
    data = brazil_20
  )

## With controls

lm2 <- 
  lm(
    as.formula(
      paste(
        "percent_20 ~ ", 
        paste(controls_20, collapse="+"), "+",
        paste(fixed_effects_20, collapse = "+"),
        "+ white_20 "
      )
    ), 
    data = brazil_20
  )


### Candidates who ran twice

## Without controls

lm3 <- 
  lm(
    as.formula(
      paste(
        "percent_20 ~ ", 
        " white_20 "
      )
    ), 
    data = data_brazil
  )

## With controls

lm4 <- 
  lm(
    as.formula(
      paste(
        "percent_20 ~ ", 
        paste(controls_20, collapse="+"), "+",
        paste(fixed_effects_20, collapse = "+"),
        "+ white_20 "
      )
    ), 
    data = data_brazil
  )


### Table exporting

collabels = c("All candidates", "All candidates", "Ran twice", "Ran twice")

covlabels = c("White 2020")

keeps = c("white_20")

notes = "The dependent variable is the vote share for the 2020 municipal election. * p<0.10, ** p<0.05, *** p<0.01. Standard errors in parentheses. Controls include age, level of education, gender, the number of eligible voters, incumbency, nationality and state fixed effects."

title = "Cross-sectional race gap in vote share, 2020"

label = "tab: race gap 2020 vote share"

stargazer(lm1, lm2, lm3, lm4,
          type='html', 
          table.placement = "htbp",
          title = title,
          style = "all2",
          label = label,
          # align = TRUE,
          column.labels = collabels,
          covariate.labels = covlabels,
          dep.var.labels.include = FALSE,
          keep = keeps,
          keep.stat = c("rsq", "n"),
          summary=FALSE,
          dep.var.caption = c(""),
          mean.sd = TRUE,
          digits = 3,
          notes = notes,
          out = "race_gap_vote_20.html",
          notes.append = FALSE,
          notes.align = "l",
          header = FALSE
)



##### Version 3: Proba win, 2016

### Columns 1 & 2: all candidates 2016, without and with controls
### Columns 3 & 4: candidates who ran twice, without and with controls

### All candidates

## Without controls

lm1 <- 
  glm(
    as.formula(
      paste(
        "won_16 ~ ", 
        " white_16 "
      )
    ), 
    data = brazil_16,
    family = binomial(link = "probit")
  )


## With controls

lm2 <- 
  glm(
    as.formula(
      paste(
        "won_16 ~ ", 
        paste(controls_16, collapse="+"), "+",
        paste(fixed_effects_16, collapse = "+"),
        " + white_16 "
      )
    ), 
    data = brazil_16,
    family = binomial(link = "probit")
  )


### Candidates who ran twice

## Without controls

lm3 <- 
  glm(
    as.formula(
      paste(
        "won_16 ~ ", 
        " white_16 "
      )
    ), 
    data = data_brazil,
    family = binomial(link = "probit")
  )


## With controls

lm4 <- 
  glm(
    as.formula(
      paste(
        "won_16 ~ ", 
        paste(controls_16, collapse="+"), "+",
        paste(fixed_effects_16, collapse = "+"),
        " + white_16 "
      )
    ), 
    data = data_brazil,
    family = binomial(link = "probit")
  )


### Table exporting

collabels = c("All candidates", "All candidates", "Ran twice", "Ran twice")

covlabels = c("White 2016")

keeps = c("white_16")

notes = "The dependent variable is an indicator of whether the candidate was elected in the 2016 municipal election. * p<0.10, ** p<0.05, *** p<0.01. Standard errors in parentheses. Controls include age, level of education, gender, the number of eligible voters, incumbency, nationality and state fixed effects."

title = "Cross-sectional race gap in the probability to be elected, 2016"

label = "tab: race gap 2016 proba win"

stargazer(lm1, lm2, lm3, lm4,
          type='latex', 
          table.placement = "htbp",
          title = title,
          style = "all2",
          label = label,
          # align = TRUE,
          column.labels = collabels,
          covariate.labels = covlabels,
          keep = keeps,
          keep.stat = c("rsq", "n"),
          summary=FALSE,
          dep.var.caption = NULL,
          mean.sd = TRUE,
          digits = 3,
          notes = notes,
          out = paste(pathtables, "race_gap_win_16.tex", sep="//"),
          notes.append = FALSE,
          notes.align = "l",
          header = FALSE
)


##### Version 4: Proba to win, 2020

### Columns 1 & 2: all candidates 2020, without and with controls
### Columns 3 & 4: candidates who ran twice, without and with controls

### All candidates

## Without controls

lm1 <- 
  glm(
    as.formula(
      paste(
        "won_20 ~ ", 
        " white_20 "
      )
    ), 
    data = brazil_20,
    family = binomial(link = "probit")
  )


## With controls

lm2 <- 
  glm(
    as.formula(
      paste(
        "won_20 ~ ", 
        paste(controls_20, collapse="+"), "+",
        paste(fixed_effects_20, collapse = "+"),
        " + white_20 "
      )
    ), 
    data = brazil_20,
    family = binomial(link = "probit")
  )


### Candidates who ran twice

## Without controls

lm3 <- 
  glm(
    as.formula(
      paste(
        "won_20 ~ ", 
        " white_20 "
      )
    ), 
    data = data_brazil,
    family = binomial(link = "probit")
  )


## With controls

lm4 <- 
  glm(
    as.formula(
      paste(
        "won_20 ~ ", 
        paste(controls_20, collapse="+"), "+",
        paste(fixed_effects_20, collapse = "+"),
        " + white_20 "
      )
    ), 
    data = data_brazil,
    family = binomial(link = "probit")
  )


### Table exporting

collabels = c("All candidates", "All candidates", "Ran twice", "Ran twice")

covlabels = c("White 2020")

keeps = c("white_20")

notes = "The dependent variable is an indicator of whether the candidate was elected in the 2020 municipal election. * p<0.10, ** p<0.05, *** p<0.01. Standard errors in parentheses. Controls include age, level of education, gender, the number of eligible voters, incumbency, nationality and state fixed effects."

title = "Cross-sectional race gap in the probability to be elected, 2020"

label = "tab: race gap 2020 proba win"

stargazer(lm1, lm2, lm3, lm4,
          type='latex', 
          table.placement = "htbp",
          title = title,
          style = "all2",
          label = label,
          # align = TRUE,
          column.labels = collabels,
          covariate.labels = covlabels,
          keep = keeps,
          keep.stat = c("rsq", "n"),
          summary=FALSE,
          dep.var.caption = NULL,
          mean.sd = TRUE,
          digits = 3,
          notes = notes,
          out = paste(pathtables, "race_gap_win_20.tex", sep="//"),
          notes.append = FALSE,
          notes.align = "l",
          header = FALSE
)


############# Table 3: Reduced form race history electoral ############# 

### Vote share in 2016

lm1 <-
  lm(
    as.formula(
      paste(
        "percent_16 ~ ", 
        paste(controls_16, collapse="+"), "+",
        paste(fixed_effects_16, collapse = "+"), "+",
        paste(race_history, collapse="+")
      )
    ), 
    data = data_brazil
  )
summary(lm1)
summary(data_brazil$percent_16)


### Vote share in 2020

lm2 <-
  lm(
    as.formula(
      paste(
        "percent_20 ~ ", 
        paste(controls_20, collapse="+"), "+",
        paste(fixed_effects_20, collapse = "+"), "+",
        paste(race_history, collapse="+")
      )
    ), 
    data = data_brazil
  )
summary(data_brazil$percent_20)


### Difference

lm3 <-
  lm(
    as.formula(
      paste(
        "diff_percent ~ ", 
        paste(controls_16, collapse="+"), "+",
        paste(fixed_effects_16, collapse = "+"), "+",
        paste(race_history, collapse="+")
      )
    ), 
    data = data_brazil
  )
summary(lm3)
summary(data_brazil$diff_percent)


### Table exporting

collabels = c("Vote share 2016", "Vote share 2020", "Difference")

covlabels = c("11: White/White", "10: White/Non-white", "01: Non-white/White")

keeps = c("w11", "w10", "w01")

notes = "In column 1, the dependent variable is the percentage of votes in the 2016 municipal election. In column 2, the dependent variable is the percentage of votes in the 2020 municipal election. In column 3, the dependent variable is the difference between the two percentages of votes. * p<0.10, ** p<0.05, *** p<0.01. Standard errors in parentheses. Controls include age, level of education, gender, the number of eligible voters, incumbency, nationality and state fixed effects."

title = "Link between race history and votes"

label = "tab: race-history and vote share"

stargazer(lm1, lm2, lm3,
          type='html', 
          table.placement = "htbp",
          title = title,
          style = "all2",
          label = label,
          # align = TRUE,
          column.labels = collabels,
          covariate.labels = covlabels,
          keep = keeps,
          keep.stat = c("rsq", "n"),
          summary=FALSE,
          dep.var.caption = NULL,
          mean.sd = TRUE,
          digits = 3,
          notes = notes,
          out = "race_history_vote.html",
          dep.var.labels = c("Vote Share 2016",
                             "Vote Share 2020",
                             "Difference in Vote Share"),
          dep.var.labels.include = FALSE,
          notes.append = FALSE,
          notes.align = "l",
          header = FALSE
)


############# Table 5+6: Reduced form race history as outcomes ############# 


### 11: Always White

lm1 <-
  glm(
    as.formula(
      paste(
        "w11 ~ ", 
        paste(combined_controls, collapse="+"), "+",
        "factor(education_16)"
      )
    ), 
    data = data_brazil,
    family = binomial(link = "probit")
  )
PseudoR2(lm1)

# lm1 <-
#   lm(
#     as.formula(
#       paste(
#         "w11 ~ ",
#         paste(combined_controls, collapse="+"), "+",
#         "factor(education_16)"
#         )
#     ),
#     data = data_brazil
#   )


### 10: From White

lm2 <-
  glm(
    as.formula(
      paste(
        "w10 ~ ", 
        paste(combined_controls, collapse="+"), "+",
        "factor(education_16)"
      )
    ), 
    data = data_brazil,
    family = binomial(link = "probit")
  )
PseudoR2(lm2)


### 01: To White

lm3 <-
  glm(
    as.formula(
      paste(
        "w01 ~ ", 
        paste(combined_controls, collapse="+"), "+",
        "factor(education_16)"
      )
    ), 
    data = data_brazil,
    family = binomial(link = "probit")
  )
PseudoR2(lm3)


### Table exporting

collabels = c("11: Always White", "10: White/Non-white", "01: Non-white/White")

covlabels = 
  c("Gender 2016", "Gender 2020",
    "Age 2016", "Age 2020",
    "Incumbent 2016", "Incumbent 2020",
    "Brazilian 2016", "Brazilian 2020",
    "City size 2016", "City size 2020"
  )

keeps = c(combined_controls, "factor(education_16)")

notes = "The dependent variables are race histories. * p<0.10, ** p<0.05, *** p<0.01. Standard errors in parentheses."

title = "Race histories explained"

label = "tab: race histories explained"

stargazer(lm1, lm2, lm3,
          type='html', 
          table.placement = "htbp",
          title = title,
          style = "all2",
          label = label,
          # align = TRUE,
          column.labels = collabels,
          covariate.labels = covlabels,
          # keep = keeps,
          keep.stat = c("rsq", "n"),
          summary=FALSE,
          dep.var.caption = NULL,
          dep.var.labels.include = FALSE,
          mean.sd = TRUE,
          digits = 3,
          notes = notes,
          out = "Race_histories_explained.html",
          notes.append = FALSE,
          notes.align = "l",
          header = FALSE
)

############# Table 9: Alternative specification ############# 

### Regression 

lm1 <- 
  lm(
    as.formula(
      paste(
        "percent_20 ~ ", 
        paste(combined_controls, collapse="+"), "+",
        paste(fixed_effects_16, collapse = "+"), "+",
        paste(fixed_effects_20, collapse = "+"), "+",
        "white_16 + w10 + w01 + percent_16"
      )
    ), 
    data = data_brazil
  )
summary(lm1)

### Table exporting

collabels = c("Votes 2020")

covlabels = c("10: White/Non-white", "01: Non-white/White", "White in 2016", "Vote percentage 2016")

keeps = c("w10", "w01", "white_16", "percent_16")

notes = "The dependent variable is the vote percentage in the 2020 municipal election. * p<0.10, ** p<0.05, *** p<0.01. Standard errors in parentheses. Controls include age, level of education, gender, the number of eligible voters, incumbency, nationality and state fixed effects."

title = "Alternative specification"

label = "tab: alternative specification"

stargazer(lm1, 
          type='latex', 
          table.placement = "htbp",
          title = title,
          style = "all2",
          label = label,
          # align = TRUE,
          column.labels = collabels,
          covariate.labels = covlabels,
          keep = keeps,
          keep.stat = c("rsq", "n"),
          summary=FALSE,
          dep.var.caption = NULL,
          mean.sd = TRUE,
          digits = 3,
          notes = notes,
          out = paste(pathtables, "Alternative_specification.tex", sep="//"),
          notes.append = FALSE,
          notes.align = "l",
          header = FALSE
)


############# Table A3: Different subsamples for main specification ############ 

### Regressions

## Main specification

lm1 <-
  lm(
    as.formula(
      paste(
        "diff_percent ~ ", 
        paste(controls_16, collapse="+"), "+",
        paste(fixed_effects_16, collapse = "+"), "+",
        paste(race_history, collapse="+")
      )
    ), 
    data = data_brazil
  )

## Only men

lm2 <-
  lm(
    as.formula(
      paste(
        "diff_percent ~ ", 
        paste(controls_16, collapse="+"), "+",
        paste(fixed_effects_16, collapse = "+"), "+",
        paste(race_history, collapse="+")
      )
    ), 
    data = subset(data_brazil, gender_16 == 1)
  )


## Only women

lm3 <-
  lm(
    as.formula(
      paste(
        "diff_percent ~ ", 
        paste(controls_16, collapse="+"), "+",
        paste(fixed_effects_16, collapse = "+"), "+",
        paste(race_history, collapse="+")
      )
    ), 
    data = subset(data_brazil, gender_16 == 0)
  )


## Higher than median age

mediane = median(subset(data_brazil, !is.na(age_16))$age_16)

lm4 <-
  lm(
    as.formula(
      paste(
        "diff_percent ~ ", 
        paste(controls_16, collapse="+"), "+",
        paste(fixed_effects_16, collapse = "+"), "+",
        paste(race_history, collapse="+")
      )
    ), 
    data = subset(data_brazil, age_16 >= mediane)
  )


## Lower than median age

lm5 <-
  lm(
    as.formula(
      paste(
        "diff_percent ~ ", 
        paste(controls_16, collapse="+"), "+",
        paste(fixed_effects_16, collapse = "+"), "+",
        paste(race_history, collapse="+")
      )
    ), 
    data = subset(data_brazil, age_16 <= mediane)
  )



## High share of white candidates

mediane = median(subset(data_brazil, !is.na(share_of_whites_16))$share_of_whites_16)

lm6 <-
  lm(
    as.formula(
      paste(
        "diff_percent ~ ", 
        paste(controls_16, collapse="+"), "+",
        paste(fixed_effects_16, collapse = "+"), "+",
        paste(race_history, collapse="+")
      )
    ), 
    data = subset(data_brazil, share_of_whites_16 >= mediane)
  )


## Low share of white candidates

lm7 <-
  lm(
    as.formula(
      paste(
        "diff_percent ~ ", 
        paste(controls_16, collapse="+"), "+",
        paste(fixed_effects_16, collapse = "+"), "+",
        paste(race_history, collapse="+")
      )
    ), 
    data = subset(data_brazil, share_of_whites_16 <= mediane)
  )


## Only incumbents in 2016

lm8 <-
  lm(
    as.formula(
      paste(
        "diff_percent ~ ", 
        paste(controls_16, collapse="+"), "+",
        paste(fixed_effects_16, collapse = "+"), "+",
        paste(race_history, collapse="+")
      )
    ), 
    data = subset(data_brazil, incumbent_16 == 0)
  )


### Table exporting

collabels = 
  c("Baseline", "Only men", "Only women", "Age >= median", "Age <= median",
    "Share white >= median", "Share white <= median", "Incumbents"
  )

covlabels = c("11: White/White", "10: White/Non-white", "01: Non-white/White")

keeps = c("w11", "w10", "w01")

notes = "For all columns, the dependent variable is the difference between the vote shares in the 2020 and 2016 municipal election. The columns differ in the sample used. In column 1, the complete sample is used. Only men are included in column 2, only women in column 3. Only candidates above (resp. below) the median age – approximately 45 years old – are included in column 4 (resp. 5). Only candidates in cities with a high (resp. low) share of white candidates are included in column 6 (resp. 7). Only candidates who were incumbents in 2016 were included in column 8. * p<0.10, ** p<0.05, *** p<0.01. Standard errors in parentheses. Controls include age, level of education, gender, the number of eligible voters, incumbency, nationality and state fixed effects."

title = "Reduced-form relationship between race history and vote share: Alternative samples"

label = "tab: race-history and vote share alternative"

stargazer(lm1, lm2, lm3, lm4, lm5, lm6, lm7, lm8,
          type='latex', 
          table.placement = "htbp",
          title = title,
          style = "all2",
          label = label,
          # align = TRUE,
          column.labels = collabels,
          covariate.labels = covlabels,
          keep = keeps,
          keep.stat = c("rsq", "n"),
          summary=FALSE,
          dep.var.caption = NULL,
          mean.sd = TRUE,
          digits = 3,
          notes = notes,
          out = paste(pathtables, "race_history_vote_alt_samples.tex", sep="//"),
          notes.append = FALSE,
          notes.align = "l",
          header = FALSE
)




################################################################################ 
############### Changing other characteristics between elections ###############
################################################################################ 

############# Table: Observable change ############# 

myDescriptives = function(x) {
  x = as.numeric(x)
  m = mean(x, na.rm = TRUE)
  sd = sd(x, na.rm = TRUE)
  N = length(x[!is.na(x)])
  return(c(m, sd, N))
}


colnames <- c("Mean", "SD", "N")

variableslist <- 
  list(
    moving_char
  )

labels <- 
  list(
    labels_moving_char
  )

datasets <- 
  list(
    "Characteristics" = data_brazil
  )

createDescriptiveTable(
  datasets,
  summary_function = myDescriptives,
  column_names = colnames,
  variable_names = variableslist,
  variable_labels = labels,
  arraystretch = 1.3,
  title = "Descriptive statistics",
  label = "tab: descriptive",
  file = paste(pathtables, "Changes_observables.tex", sep="//"),
  digits = c(3,3,0)
)
summary(data_brazil$different_state)

################################################################################ 
####################### Switches from black to non-black #######################
################################################################################ 

############# Data cleaning #####


data_black <- 
  data_brazil %>%
  mutate(
    b11 = as.numeric(race_16 == 3 & race_20 == 3),
    b10 = as.numeric(race_16 == 3 & race_20 != 3),
    b01 = as.numeric(race_16 != 3 & race_20 == 3),
    b00 = as.numeric(race_16 != 3 & race_20 != 3),
    black_16 = as.numeric(race_16 == 3),
    black_20 = as.numeric(race_20 == 3)
  )

race_history <- 
  c("b00", "b10", "b01")


############# Table 1: Descriptive statistics ############# 


###Construction of each column###

##Variables##

vars_table1 = c("B/B", "B/NB", "NB/B", "NB/NB", "black_16", "black_20",
                "male_16", "male_20", "age_16", "age_20", "educ_16",
                "educ_20", "incumb_16", "incumb_20", "obs_16", "obs_20")

##All candidates##

All_candidates = c(NA, NA, NA, NA, mean(brazil_16$white, na.rm = TRUE),
                   mean(brazil_20$white, na.rm = TRUE), mean(brazil_16$gender_16),
                   mean(brazil_20$gender_20), mean(brazil_16$age, na.rm = TRUE),
                   mean(brazil_20$age, na.rm = TRUE), mean(brazil_16$education_16),
                   mean(brazil_20$education_20), mean(brazil_16$incumbent_16),
                   mean(brazil_20$incumbent_20), nrow(brazil_16), nrow(brazil_20))

table1_cornwell = data.frame(vars_table1, All_candidates)

##Ran twice##

Ran_twice = c(mean(data_black$b11), mean(data_black$b10),
              mean(data_black$b01),
              mean(data_black$b00), mean(data_black$black_16, na.rm = TRUE),
              mean(data_black$black_20, na.rm = TRUE), mean(data_black$gender_16),
              mean(data_black$gender_20), mean(data_black$age_16, na.rm = TRUE),
              mean(data_black$age_20, na.rm = TRUE), mean(data_black$education_16),
              mean(data_black$education_20), mean(data_black$incumbent_16),
              mean(data_black$incumbent_20),
              nrow(data_black), nrow(data_black))
table1_cornwell = data.frame(table1_cornwell, Ran_twice)

##11##

cands_11 = data_black %>% filter(b11 == 1)

ch11 = c(mean(cands_11$b11), mean(cands_11$b10),
         mean(cands_11$b01),
         mean(cands_11$b00), mean(cands_11$black_16, na.rm = TRUE),
         mean(cands_11$black_20, na.rm = TRUE), mean(cands_11$gender_16),
         mean(cands_11$gender_20), mean(cands_11$age_16, na.rm = TRUE),
         mean(cands_11$age_20, na.rm = TRUE), mean(cands_11$education_16),
         mean(cands_11$education_20), mean(cands_11$incumbent_16),
         mean(cands_11$incumbent_20),
         nrow(cands_11), nrow(cands_11))
table1_cornwell = data.frame(table1_cornwell, ch11)

##10##

cands_10 = data_black %>% filter(b10 == 1)

ch10 = c(mean(cands_10$b11), mean(cands_10$b10),
         mean(cands_10$b01),
         mean(cands_10$b00), mean(cands_10$black_16, na.rm = TRUE),
         mean(cands_10$black_20, na.rm = TRUE), mean(cands_10$gender_16),
         mean(cands_10$gender_20), mean(cands_10$age_16, na.rm = TRUE),
         mean(cands_10$age_20, na.rm = TRUE), mean(cands_10$education_16),
         mean(cands_10$education_20), mean(cands_10$incumbent_16),
         mean(cands_10$incumbent_20),
         nrow(cands_10), nrow(cands_10))
table1_cornwell = data.frame(table1_cornwell, ch10)

##01##

cands_01 = data_black %>% filter(b01 == 1)

ch01 = c(mean(cands_01$b11), mean(cands_01$b10),
         mean(cands_01$b01),
         mean(cands_01$b00), mean(cands_01$black_16, na.rm = TRUE),
         mean(cands_01$black_20, na.rm = TRUE), mean(cands_01$gender_16),
         mean(cands_01$gender_20), mean(cands_01$age_16, na.rm = TRUE),
         mean(cands_01$age_20, na.rm = TRUE), mean(cands_01$education_16),
         mean(cands_01$education_20), mean(cands_01$incumbent_16),
         mean(cands_01$incumbent_20),
         nrow(cands_01), nrow(cands_01))
table1_cornwell = data.frame(table1_cornwell, ch01)

##00##

cands_00 = data_black %>% filter(b00 == 1)

ch00 = c(mean(cands_00$b11), mean(cands_00$b10),
         mean(cands_00$b01),
         mean(cands_00$b00), mean(cands_00$black_16, na.rm = TRUE),
         mean(cands_00$black_20, na.rm = TRUE), mean(cands_00$gender_16),
         mean(cands_00$gender_20), mean(cands_00$age_16, na.rm = TRUE),
         mean(cands_00$age_20, na.rm = TRUE), mean(cands_00$education_16),
         mean(cands_00$education_20), mean(cands_00$incumbent_16),
         mean(cands_00$incumbent_20),
         nrow(cands_00), nrow(cands_00))
table1_cornwell = data.frame(table1_cornwell, ch00)

###Presentation of the table###

table1_cornwell[table1_cornwell == "black_16"] <- "2016"
table1_cornwell[table1_cornwell == "black_20"] <- "2020"
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
             ch11 = "B/B",
             ch10 = "B/NB",
             ch01 = "NB/B",
             ch00 = "NB/NB") %>%
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
  tab_row_group(label = "Black",
                rows = 5:6) %>%
  tab_row_group(label = "Race History",
                rows = 1:4) %>%
  tab_spanner(label = "By Race history",
              columns = 4:7)


############# Table 2: Racial vote gap ############# 

##### Version 1: Vote share, 2016

### Columns 1 & 2: all candidates 2016, without and with controls
### Columns 3 & 4: candidates who ran twice, vote 2016, without and with controls

### All candidates

## Without controls

lm1 <- 
  lm(
    as.formula(
      paste(
        "percent_16 ~ ", 
        " black_16 "
      )
    ), 
    data = brazil_16
  )

## With controls

lm2 <- 
  lm(
    as.formula(
      paste(
        "percent_16 ~ ", 
        paste(controls_16, collapse="+"), "+",
        paste(fixed_effects_16, collapse = "+"),
        "+ black_16 "
      )
    ), 
    data = brazil_16
  )


### Candidates who ran twice

## Without controls

lm3 <- 
  lm(
    as.formula(
      paste(
        "percent_16 ~ ", 
        " black_16 "
      )
    ), 
    data = data_black
  )

## With controls

lm4 <- 
  lm(
    as.formula(
      paste(
        "percent_16 ~ ", 
        paste(controls_16, collapse="+"), "+",
        paste(fixed_effects_16, collapse = "+"),
        "+ black_16 "
      )
    ), 
    data = data_black
  )


### Table exporting

collabels = c("All candidates", "All candidates", "Ran twice", "Ran twice")

covlabels = c("Black 2016")

keeps = c("black_16")

notes = "The dependent variable is the vote share for the 2016 municipal election. * p<0.10, ** p<0.05, *** p<0.01. Standard errors in parentheses. Controls include age, level of education, gender, the number of eligible voters, incumbency, nationality and state fixed effects."

title = "Cross-sectional race gap black in vote share, 2016"

label = "tab: race gap black 2016 vote share"

stargazer(lm1, lm2, lm3, lm4,
          type='latex', 
          table.placement = "htbp",
          title = title,
          style = "all2",
          label = label,
          # align = TRUE,
          column.labels = collabels,
          covariate.labels = covlabels,
          keep = keeps,
          keep.stat = c("rsq", "n"),
          summary=FALSE,
          dep.var.caption = NULL,
          mean.sd = TRUE,
          digits = 3,
          notes = notes,
          out = paste(pathtables, "race_gap_black_vote_16.tex", sep="//"),
          notes.append = FALSE,
          notes.align = "l",
          header = FALSE
)


##### Version 2: Vote share, 2020

### Columns 1 & 2: all candidates 2020, without and with controls
### Columns 3 & 4: candidates who ran twice, vote 2020, without and with controls

### All candidates

## Without controls

lm1 <- 
  lm(
    as.formula(
      paste(
        "percent_20 ~ ", 
        " black_20 "
      )
    ), 
    data = brazil_20
  )

## With controls

lm2 <- 
  lm(
    as.formula(
      paste(
        "percent_20 ~ ", 
        paste(controls_20, collapse="+"), "+",
        paste(fixed_effects_20, collapse = "+"),
        "+ black_20 "
      )
    ), 
    data = brazil_20
  )


### Candidates who ran twice

## Without controls

lm3 <- 
  lm(
    as.formula(
      paste(
        "percent_20 ~ ", 
        " black_20 "
      )
    ), 
    data = data_black
  )

## With controls

lm4 <- 
  lm(
    as.formula(
      paste(
        "percent_20 ~ ", 
        paste(controls_20, collapse="+"), "+",
        paste(fixed_effects_20, collapse = "+"),
        "+ black_20 "
      )
    ), 
    data = data_black
  )


### Table exporting

collabels = c("All candidates", "All candidates", "Ran twice", "Ran twice")

covlabels = c("Black 2020")

keeps = c("black_20")

notes = "The dependent variable is the vote share for the 2020 municipal election. * p<0.10, ** p<0.05, *** p<0.01. Standard errors in parentheses. Controls include age, level of education, gender, the number of eligible voters, incumbency, nationality and state fixed effects."

title = "Cross-sectional race gap black in vote share, 2020"

label = "tab: race gap black 2020 vote share"

stargazer(lm1, lm2, lm3, lm4,
          type='latex', 
          table.placement = "htbp",
          title = title,
          style = "all2",
          label = label,
          # align = TRUE,
          column.labels = collabels,
          covariate.labels = covlabels,
          keep = keeps,
          keep.stat = c("rsq", "n"),
          summary=FALSE,
          dep.var.caption = NULL,
          mean.sd = TRUE,
          digits = 3,
          notes = notes,
          out = paste(pathtables, "race_gap_black_vote_20.tex", sep="//"),
          notes.append = FALSE,
          notes.align = "l",
          header = FALSE
)


##### Version 3: Proba win, 2016

### Columns 1 & 2: all candidates 2016, without and with controls
### Columns 3 & 4: candidates who ran twice, without and with controls

### All candidates

## Without controls

lm1 <- 
  glm(
    as.formula(
      paste(
        "won_16 ~ ", 
        " black_16 "
      )
    ), 
    data = brazil_16,
    family = binomial(link = "probit")
  )


## With controls

lm2 <- 
  glm(
    as.formula(
      paste(
        "won_16 ~ ", 
        paste(controls_16, collapse="+"), "+",
        paste(fixed_effects_16, collapse = "+"),
        " + black_16 "
      )
    ), 
    data = brazil_16,
    family = binomial(link = "probit")
  )


### Candidates who ran twice

## Without controls

lm3 <- 
  glm(
    as.formula(
      paste(
        "won_16 ~ ", 
        " black_16 "
      )
    ), 
    data = data_black,
    family = binomial(link = "probit")
  )


## With controls

lm4 <- 
  glm(
    as.formula(
      paste(
        "won_16 ~ ", 
        paste(controls_16, collapse="+"), "+",
        paste(fixed_effects_16, collapse = "+"),
        " + black_16 "
      )
    ), 
    data = data_black,
    family = binomial(link = "probit")
  )


### Table exporting

collabels = c("All candidates", "All candidates", "Ran twice", "Ran twice")

covlabels = c("Black 2016")

keeps = c("black_16")

notes = "The dependent variable is an indicator of whether the candidate was elected in the 2016 municipal election. * p<0.10, ** p<0.05, *** p<0.01. Standard errors in parentheses. Controls include age, level of education, gender, the number of eligible voters, incumbency, nationality and state fixed effects."

title = "Cross-sectional race gap black in the probability to be elected, 2016"

label = "tab: race gap black 2016 proba win"

stargazer(lm1, lm2, lm3, lm4,
          type='latex', 
          table.placement = "htbp",
          title = title,
          style = "all2",
          label = label,
          # align = TRUE,
          column.labels = collabels,
          covariate.labels = covlabels,
          keep = keeps,
          keep.stat = c("rsq", "n"),
          summary=FALSE,
          dep.var.caption = NULL,
          mean.sd = TRUE,
          digits = 3,
          notes = notes,
          out = paste(pathtables, "race_gap_black_win_16.tex", sep="//"),
          notes.append = FALSE,
          notes.align = "l",
          header = FALSE
)


##### Version 4: Proba to win, 2020

### Columns 1 & 2: all candidates 2020, without and with controls
### Columns 3 & 4: candidates who ran twice, without and with controls

### All candidates

## Without controls

lm1 <- 
  glm(
    as.formula(
      paste(
        "won_20 ~ ", 
        " black_20 "
      )
    ), 
    data = brazil_20,
    family = binomial(link = "probit")
  )


## With controls

lm2 <- 
  glm(
    as.formula(
      paste(
        "won_20 ~ ", 
        paste(controls_20, collapse="+"), "+",
        paste(fixed_effects_20, collapse = "+"),
        " + black_20 "
      )
    ), 
    data = brazil_20,
    family = binomial(link = "probit")
  )


### Candidates who ran twice

## Without controls

lm3 <- 
  glm(
    as.formula(
      paste(
        "won_20 ~ ", 
        " black_20 "
      )
    ), 
    data = data_black,
    family = binomial(link = "probit")
  )


## With controls

lm4 <- 
  glm(
    as.formula(
      paste(
        "won_20 ~ ", 
        paste(controls_20, collapse="+"), "+",
        paste(fixed_effects_20, collapse = "+"),
        " + black_20 "
      )
    ), 
    data = data_black,
    family = binomial(link = "probit")
  )


### Table exporting

collabels = c("All candidates", "All candidates", "Ran twice", "Ran twice")

covlabels = c("Black 2020")

keeps = c("black_20")

notes = "The dependent variable is an indicator of whether the candidate was elected in the 2020 municipal election. * p<0.10, ** p<0.05, *** p<0.01. Standard errors in parentheses. Controls include age, level of education, gender, the number of eligible voters, incumbency, nationality and state fixed effects."

title = "Cross-sectional race gap black in the probability to be elected, 2020"

label = "tab: race gap black 2020 proba win"

stargazer(lm1, lm2, lm3, lm4,
          type='latex', 
          table.placement = "htbp",
          title = title,
          style = "all2",
          label = label,
          # align = TRUE,
          column.labels = collabels,
          covariate.labels = covlabels,
          keep = keeps,
          keep.stat = c("rsq", "n"),
          summary=FALSE,
          dep.var.caption = NULL,
          mean.sd = TRUE,
          digits = 3,
          notes = notes,
          out = paste(pathtables, "race_gap_black_win_20.tex", sep="//"),
          notes.append = FALSE,
          notes.align = "l",
          header = FALSE
)


############# Table 3: Reduced form race history electoral ############# 

### Vote share in 2016

lm1 <-
  lm(
    as.formula(
      paste(
        "percent_16 ~ ", 
        paste(controls_16, collapse="+"), "+",
        paste(fixed_effects_16, collapse = "+"), "+",
        paste(race_history, collapse="+")
      )
    ), 
    data = data_black
  )


### Vote share in 2020

lm2 <-
  lm(
    as.formula(
      paste(
        "percent_20 ~ ", 
        paste(controls_20, collapse="+"), "+",
        paste(fixed_effects_20, collapse = "+"), "+",
        paste(race_history, collapse="+")
      )
    ), 
    data = data_black
  )


### Difference

lm3 <-
  lm(
    as.formula(
      paste(
        "diff_percent ~ ", 
        paste(controls_16, collapse="+"), "+",
        paste(fixed_effects_16, collapse = "+"), "+",
        paste(race_history, collapse="+")
      )
    ), 
    data = data_black
  )
summary(lm3)


### Table exporting

collabels = c("Vote share 2016", "Vote share 2020", "Difference")

covlabels = c("00: Non-Black/Non-Black", "10: Black/Non-Black", "01: Non-Black / Black")

keeps = c("b00", "b10", "b01")

notes = "In column 1, the dependent variable is the percentage of votes in the 2016 municipal election. In column 2, the dependent variable is the percentage of votes in the 2020 municipal election. In column 2, the dependent variable is the difference between the two percentages of votes. * p<0.10, ** p<0.05, *** p<0.01. Standard errors in parentheses. Controls include age, level of education, gender, the number of eligible voters, incumbency, nationality and state fixed effects."

title = "Reduced-form relationship between race history and vote share"

label = "tab: race-history black and vote share"

stargazer(lm1, lm2, lm3,
          type='latex', 
          table.placement = "htbp",
          title = title,
          style = "all2",
          label = label,
          # align = TRUE,
          column.labels = collabels,
          covariate.labels = covlabels,
          keep = keeps,
          keep.stat = c("rsq", "n"),
          summary=FALSE,
          dep.var.caption = NULL,
          mean.sd = TRUE,
          digits = 3,
          notes = notes,
          out = paste(pathtables, "race_history_black_vote.tex", sep="//"),
          notes.append = FALSE,
          notes.align = "l",
          header = FALSE
)


############# Table 5+6: Reduced form race history as outcomes ############# 


### 11: Always Black

lm1 <-
  lm(
    as.formula(
      paste(
        "b11 ~ ", 
        paste(combined_controls, collapse="+"), "+",
        paste(fixed_effects_16, collapse = "+"), "+",
        paste(fixed_effects_20, collapse = "+")
      )
    ), 
    data = data_black
  )

### 10: From Black

lm2 <-
  lm(
    as.formula(
      paste(
        "b10 ~ ", 
        paste(combined_controls, collapse="+"), "+",
        paste(fixed_effects_16, collapse = "+"), "+",
        paste(fixed_effects_20, collapse = "+")
      )
    ), 
    data = data_black
  )

### 01: To Black

lm3 <-
  lm(
    as.formula(
      paste(
        "b01 ~ ", 
        paste(combined_controls, collapse="+"), "+",
        paste(fixed_effects_16, collapse = "+"), "+",
        paste(fixed_effects_20, collapse = "+")
      )
    ), 
    data = data_black
  )

### Table exporting

collabels = c("11: Always Black", "10: Black/Non-Black ", "01: Non-Black/Black")

covlabels = 
  c("Gender 2016", "Gender 2020",
    "Age 2016", "Age 2020",
    "Incumbent 2016", "Incumbent 2020",
    "Brazilian 2016", "Brazilian 2020",
    "City size 2016", "City size 2020"
  )

keeps = c(combined_controls, "factor(education_16)", "factor(education_20)")

notes = "The dependent variables are race histories. * p<0.10, ** p<0.05, *** p<0.01. Standard errors in parentheses. All estimations include state and education level fixed effects."

title = "Race histories explained"

label = "tab: race histories black explained"

stargazer(lm1, lm2, lm3,
          type='latex', 
          table.placement = "htbp",
          title = title,
          style = "all2",
          label = label,
          # align = TRUE,
          column.labels = collabels,
          covariate.labels = covlabels,
          keep = keeps,
          keep.stat = c("rsq", "n"),
          summary=FALSE,
          dep.var.caption = NULL,
          mean.sd = TRUE,
          digits = 3,
          notes = notes,
          out = paste(pathtables, "Race_histories_black_explained.tex", sep="//"),
          notes.append = FALSE,
          notes.align = "l",
          header = FALSE
)

############# Table 9: Alternative specification ############# 

### Regression 

lm1 <- 
  lm(
    as.formula(
      paste(
        "percent_20 ~ ", 
        paste(combined_controls, collapse="+"), "+",
        paste(fixed_effects_16, collapse = "+"), "+",
        paste(fixed_effects_20, collapse = "+"), "+",
        "b10 + b01 + black_16 + percent_16"
      )
    ), 
    data = data_black
  )


### Table exporting

collabels = c("Vote share 2020")

covlabels = c("10: Black/Non-Black", "01: Non-Black / Black", "Black in 2016", "Vote share 2016")

keeps = c("b10", "b01", "black_16", "percent_16")

notes = "The dependent variable is the vote share in the 2020 municipal election. * p<0.10, ** p<0.05, *** p<0.01. Standard errors in parentheses. Controls include age, level of education, gender, the number of eligible voters, incumbency, nationality and state fixed effects."

title = "Alternative specification"

label = "tab: alternative specification black"

stargazer(lm1, 
          type='latex', 
          table.placement = "htbp",
          title = title,
          style = "all2",
          label = label,
          # align = TRUE,
          column.labels = collabels,
          covariate.labels = covlabels,
          keep = keeps,
          keep.stat = c("rsq", "n"),
          summary=FALSE,
          dep.var.caption = NULL,
          mean.sd = TRUE,
          digits = 3,
          notes = notes,
          out = paste(pathtables, "Alternative_specification_black.tex", sep="//"),
          notes.append = FALSE,
          notes.align = "l",
          header = FALSE
)


############# Table A3: Different subsamples for main specification ############ 

### Regressions

## Main specification

lm1 <-
  lm(
    as.formula(
      paste(
        "diff_percent ~ ", 
        paste(controls_16, collapse="+"), "+",
        paste(fixed_effects_16, collapse = "+"), "+",
        paste(race_history, collapse="+")
      )
    ), 
    data = data_black
  )

## Only men

lm2 <-
  lm(
    as.formula(
      paste(
        "diff_percent ~ ", 
        paste(controls_16, collapse="+"), "+",
        paste(fixed_effects_16, collapse = "+"), "+",
        paste(race_history, collapse="+")
      )
    ), 
    data = subset(data_black, gender_16 == 1)
  )


## Only women

lm3 <-
  lm(
    as.formula(
      paste(
        "diff_percent ~ ", 
        paste(controls_16, collapse="+"), "+",
        paste(fixed_effects_16, collapse = "+"), "+",
        paste(race_history, collapse="+")
      )
    ), 
    data = subset(data_black, gender_16 == 0)
  )


## Higher than median age

mediane = median(subset(data_black, !is.na(age_16))$age_16)

lm4 <-
  lm(
    as.formula(
      paste(
        "diff_percent ~ ", 
        paste(controls_16, collapse="+"), "+",
        paste(fixed_effects_16, collapse = "+"), "+",
        paste(race_history, collapse="+")
      )
    ), 
    data = subset(data_black, age_16 >= mediane)
  )


## Lower than median age

lm5 <-
  lm(
    as.formula(
      paste(
        "diff_percent ~ ", 
        paste(controls_16, collapse="+"), "+",
        paste(fixed_effects_16, collapse = "+"), "+",
        paste(race_history, collapse="+")
      )
    ), 
    data = subset(data_black, age_16 <= mediane)
  )



## High share of white candidates

mediane = median(subset(data_black, !is.na(share_of_whites_16))$share_of_whites_16)

lm6 <-
  lm(
    as.formula(
      paste(
        "diff_percent ~ ", 
        paste(controls_16, collapse="+"), "+",
        paste(fixed_effects_16, collapse = "+"), "+",
        paste(race_history, collapse="+")
      )
    ), 
    data = subset(data_black, share_of_whites_16 >= mediane)
  )


## Low share of white candidates

lm7 <-
  lm(
    as.formula(
      paste(
        "diff_percent ~ ", 
        paste(controls_16, collapse="+"), "+",
        paste(fixed_effects_16, collapse = "+"), "+",
        paste(race_history, collapse="+")
      )
    ), 
    data = subset(data_black, share_of_whites_16 <= mediane)
  )


## Only incumbents in 2016

lm8 <-
  lm(
    as.formula(
      paste(
        "diff_percent ~ ", 
        paste(controls_16, collapse="+"), "+",
        paste(fixed_effects_16, collapse = "+"), "+",
        paste(race_history, collapse="+")
      )
    ), 
    data = subset(data_black, incumbent_16 == 0)
  )


### Table exporting

collabels = 
  c("Baseline", "Only men", "Only women", "Age >= median", "Age <= median",
    "Share white >= median", "Share white <= median", "Incumbents"
  )

covlabels = c("11: Black/Black", "10: Black/Non-Black", "01: Non-Black/Black")

keeps = c("b11", "b10", "b01")

notes = "For all columns, the dependent variable is the difference between the vote shares in the 2020 and 2016 municipal election. The columns differ in the sample used. In column 1, the complete sample is used. Only men are included in column 2, only women in column 3. Only candidates above (resp. below) the median age – approximately 45 years old – are included in column 4 (resp. 5). Only candidates in cities with a high (resp. low) share of white candidates are included in column 6 (resp. 7). Only candidates who were incumbents in 2016 were included in column 8. * p<0.10, ** p<0.05, *** p<0.01. Standard errors in parentheses. Controls include age, level of education, gender, the number of eligible voters, incumbency, nationality and state fixed effects."

title = "Reduced-form relationship between race history and vote share: Alternative samples"

label = "tab: race-history and vote share alternative black"

stargazer(lm1, lm2, lm3, lm4, lm5, lm6, lm7, lm8,
          type='latex', 
          table.placement = "htbp",
          title = title,
          style = "all2",
          label = label,
          # align = TRUE,
          column.labels = collabels,
          covariate.labels = covlabels,
          keep = keeps,
          keep.stat = c("rsq", "n"),
          summary=FALSE,
          dep.var.caption = NULL,
          mean.sd = TRUE,
          digits = 3,
          notes = notes,
          out = paste(pathtables, "race_history_vote_alt_samples_black.tex", sep="//"),
          notes.append = FALSE,
          notes.align = "l",
          header = FALSE
)





################################################################################ 
############################## Any switch combined #############################
################################################################################ 

############# Cleaning: definition of same histories ############# 

data_switch <-
  select(data_brazil, 
         c('race_16', 'race_20', all_of(controls_16),
           'NR_CPF_CANDIDATO',
           'percent_16', 'percent_20', 'diff_percent',
           all_of(combined_controls),
           'state_16', 'educ_16', 'state_20', 'educ_20', 'education_16', 'education_20',
           'won_16', 'won_20', 'share_of_whites_16', 'share_of_whites_20',
           'switched'
         ))

data_switch <- 
  data_brazil %>%
  mutate(
    w11 = as.numeric(race_16 == 1 & race_20 == 1),
    m11 = as.numeric(race_16 == 2 & race_20 == 2),
    b11 = as.numeric(race_16 == 3 & race_20 == 3),
    o11 = as.numeric(race_16 > 3 & race_20 > 3),
  )

same_race <- 
  c("w11", "m11", "o11")


############# Table: Reduced form race history electoral ############# 

### Vote share in 2016

lm1 <-
  lm(
    as.formula(
      paste(
        "percent_16 ~ ", 
        paste(controls_16, collapse="+"), "+",
        paste(fixed_effects_16, collapse = "+"), "+",
        paste(same_race, collapse = "+"), "+",
        "switched " 
      )
    ), 
    data = data_switch
  )


### Vote share in 2020

lm2 <-
  lm(
    as.formula(
      paste(
        "percent_20 ~ ", 
        paste(controls_20, collapse="+"), "+",
        paste(fixed_effects_20, collapse = "+"), "+",
        paste(same_race, collapse = "+"), "+",
        "switched " 
      )
    ), 
    data = data_switch
  )


### Difference

lm3 <-
  lm(
    as.formula(
      paste(
        "diff_percent ~ ", 
        paste(controls_16, collapse="+"), "+",
        paste(fixed_effects_16, collapse = "+"), "+",
        paste(same_race, collapse = "+"), "+",
        "switched " 
      )
    ), 
    data = data_switch
  )
summary(lm3)


### Table exporting

collabels = c("Vote share 2016", "Vote share 2020", "Difference")

covlabels = c("White/White", "Mixed/Mixed", "Other/Other", "Any switch")

keeps = c(same_race, "switched")

notes = "In column 1, the dependent variable is the percentage of votes in the 2016 municipal election. In column 2, the dependent variable is the percentage of votes in the 2020 municipal election. In column 3, the dependent variable is the difference between the two percentages of votes. * p<0.10, ** p<0.05, *** p<0.01. Standard errors in parentheses. Controls include age, level of education, gender, the number of eligible voters, incumbency, nationality and state-fixed effects. The reference group is candidates with a race history Black/Black."

title = "Reduced-form relationship between race history and vote share for any race switch"

label = "tab: race-history and vote share switch"

stargazer(lm1, lm2, lm3,
          type='latex', 
          table.placement = "htbp",
          title = title,
          style = "all2",
          label = label,
          # align = TRUE,
          column.labels = collabels,
          covariate.labels = covlabels,
          keep = keeps,
          keep.stat = c("rsq", "n"),
          summary=FALSE,
          dep.var.caption = NULL,
          mean.sd = TRUE,
          digits = 3,
          notes = notes,
          out = paste(pathtables, "race_history_vote_switch.tex", sep="//"),
          notes.append = FALSE,
          notes.align = "l",
          header = FALSE
)


################################################################################ 
############################# Race gap in elections ############################
################################################################################ 

############# Vote shares as a function of race #####

### Vote share 2016, all candidates

lm1 <- 
  lm(
    as.formula(
      paste(
        "percent_16 ~ white_16 + ", 
        paste(controls_16, collapse="+"), "+",
        paste(fixed_effects_16, collapse = "+")
        )
    ), 
    data = brazil_16
  )

### Vote share 2016, candidates who ran twice

lm2 <- 
  lm(
    as.formula(
      paste(
        "percent_16 ~ white_16 +", 
        paste(controls_16, collapse="+"), "+",
        paste(fixed_effects_16, collapse = "+")
      )
    ), 
    data = data_brazil
  )

### Vote share 2020, all candidates

lm3 <- 
  lm(
    as.formula(
      paste(
        "percent_20 ~ white_20 + ", 
        paste(controls_20, collapse="+"), "+",
        paste(fixed_effects_20, collapse = "+")
      )
    ), 
    data = brazil_20
  )

### Vote share 2016, candidates who ran twice

lm4 <- 
  lm(
    as.formula(
      paste(
        "percent_20 ~ white_20 + ", 
        paste(controls_20, collapse="+"), "+",
        paste(fixed_effects_20, collapse = "+")
      )
    ), 
    data = data_brazil
  )


### Table exporting

collabels = c("All candidates", "Ran twice", "All candidates", "Ran twice")

covlabels = c("White 2016", "White 2020")

keeps = c("white_16", "white_20")

notes = "The dependent variable is the percentage of votes in municipal elections. In columns 1 and 2, the dependent variable is the vote percentage in 2016, in columns 3 and 4, the vote percentage for 2020. * p<0.10, ** p<0.05, *** p<0.01. Standard errors in parentheses. Controls include age, level of education, gender, the number of eligible voters, incumbency, nationality and state fixed effects."

title = "Cross-sectional race gap in votes"

label = "tab: race gap vote share"

stargazer(lm1, lm2, lm3, lm4,
          type='latex', 
          table.placement = "htbp",
          title = title,
          style = "all2",
          label = label,
          # align = TRUE,
          column.labels = collabels,
          covariate.labels = covlabels,
          keep = keeps,
          keep.stat = c("rsq", "n"),
          summary=FALSE,
          dep.var.caption = NULL,
          mean.sd = TRUE,
          digits = 3,
          notes = notes,
          out = paste(pathtables, "race_gap.tex", sep="//"),
          notes.append = FALSE,
          notes.align = "l",
          header = FALSE
)

summary(data_brazil$percent_16)
summary(data_brazil$percent_20)

summary(brazil_16$percent_16)
summary(brazil_20$percent_20)
