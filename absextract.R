rm(list = ls())

library(tidyverse)
library(kableExtra)
library(glmnet)
library(survey)
library(nnet)
library(VGAM)

# data 
# 1998
# bath: HCP540, dress: HCP541, eat: HCP542, toilet: HCP543, bladder: HCP544 
# age: PSN401 
# sex: PSN402
# illness codes: diabetes: 10, stroke: 56, heart diseases: 51, breathing: 81 - add evidence for the choice of diseases. (major courses of illness in the later age + top-ten courses of death - look at demography literature)
# age code: 13 (60-64),14 (65-69), 15 (70-74), 16 (75-79), 17 (80-84), 18 (85+)
# 13- 24, 

# Read data files in different forms and extract the required information

dataCON98 <- haven::read_sas("dsb98dis.sas7bdat")%>%as_tibble()%>%rename(LVLBATHE = "HCP540")%>%rename(LVLDRESS = "HCP541")%>%rename(LVLEATIN = "HCP542")%>%rename(LVLTOILT = "HCP543")%>%
  rename(LVLBOWEL = "HCP544")

dataPER98 <- haven::read_sas("dsb98psn.sas7bdat")%>%as_tibble()%>%rename(AGEPC = "PSN401")%>%rename(SEX = "PSN402")%>%
  mutate(AGEPC = case_when(
    AGEPC < 13 ~ "1",
    AGEPC == 13 ~ "24",
    AGEPC == 14 ~ "25",
    AGEPC == 15 ~ "26",
    AGEPC == 16 ~ "27",
    AGEPC == 17 ~ "28",
    AGEPC == 18 ~ "29",
    TRUE ~ NA_character_))%>%rename(CONDCODL = "CND4210A") %>% 
    mutate(CONDCODL = case_when(
      CONDCODL == 10 ~ "402",
      CONDCODL == 56 ~ "923",
      CONDCODL == 51 ~ "910",
      CONDCODL == 81 ~ "202",
      TRUE ~ NA_character_))%>% mutate(WT401= WT401/10000)%>% rename(FINWTP = "WT401")%>%
  mutate(FINWTP = ifelse(FINWTP == 0, mean(FINWTP[FINWTP != 0]), FINWTP))

# weight = PERS_WT

dataPER3 <- haven::read_sas("dac03per.sas7bdat")%>%as_tibble() %>% mutate(AGEPC = case_when(
  AGEPC < 13 ~ "1",
  AGEPC == 13 ~ "24",
  AGEPC == 14 ~ "25",
  AGEPC == 15 ~ "26",
  AGEPC == 16 ~ "27",
  AGEPC == 17 ~ "28",
  AGEPC == 18 ~ "29",
  TRUE ~ NA_character_))%>%rename(FINWTP = "PERS_WT")

dataCON3 <- haven::read_sas("dac03con.sas7bdat")%>%as_tibble()%>% rename(CONDCODL = "CODECODC")
dataCON3 <- dataCON3%>%mutate(CONDCODL = replace(CONDCODL, 1701, 202))

# person weight =PERS_WT

dataPER9 <- haven::read_sas("dac09per.sas7bdat")%>%as_tibble()%>% rename(AGEPC = "AGEEB")%>%
        mutate(AGEPC = case_when(AGEPC < 6 ~ "1",
                           AGEPC == 6 ~ "24",
                           AGEPC == 7 ~ "25",
                           AGEPC == 8 ~ "26",
                           AGEPC == 9 ~ "27",
                           AGEPC == 10 ~ "28",
                           AGEPC == 11 ~ "29",
                           AGEPC == 99 ~ "2",
                           TRUE ~ NA_character_))%>% rename(FINWTP = "PERS_WT")

dataCON9 <- haven::read_sas("dac09con.sas7bdat")%>%as_tibble()%>% rename(CONDCODL = "CONCODEB")
dataCON9<- dataCON9%>%mutate(CONDCODL = replace(CONDCODL, 1701, 202))

dataPER12 <- haven::read_sas("dac12per64.sas7bdat")%>%as_tibble()%>% rename(AGEPC = "AGEEB")%>%mutate(AGEPC = case_when(AGEPC < 6 ~ "1",
  AGEPC == 6 ~ "24",
  AGEPC == 7 ~ "25",
  AGEPC == 8 ~ "26",
  AGEPC == 9 ~ "27",
  AGEPC == 10 ~ "28",
  AGEPC == 11 ~ "29",
  AGEPC == 99 ~ "2",
  TRUE ~ NA_character_))%>% rename(FINWTP = "finwtp")

dataCON12 <- haven::read_sas("dac12con64.sas7bdat")%>%as_tibble()%>%rename(CONDCODL = "CONCODEB")
dataCON12<- dataCON12%>%mutate(CONDCODL = replace(CONDCODL, 1701, 202))

dataPER15 <- haven::read_sas("dac15per.sas7bdat")%>%as_tibble()
dataCON15 <- haven::read_sas("dac15con.sas7bdat")%>%as_tibble()%>% rename(CONDCODL = "CONCODEB")

dataPER18 <- read.csv("DAC18PER.csv")%>%as_tibble()
dataCON18 <- read.csv("DAC18CON.csv")%>%as_tibble()

# Create a list of file pairs

file_pairs <- list(list(x = dataPER98, y = dataCON98), list(x = dataCON3, y = dataPER3), list(x = dataCON9, y = dataPER9), list(x = dataCON12, y = dataPER12), list(x = dataCON15, y = dataPER15), list(x = dataCON18, y = dataPER18))

dataClean <- function(x, y) 
  
{
  # x: excel file for all conditions (DAC18CON.csv)
  # y: excel file for person (the main level) (DAC18PER.csv)
  
 cols_to_select <- c("SEX", "AGEPC", "FINWTP")
  
 if (all(cols_to_select %in% colnames(x)))
  
  {
    # condition is from dataPER98
    
    ID <- x%>%dplyr::select(c(ABSFID, ABSIID, ABSPID))
    df <- mutate_all(ID, as.numeric)
    conNew <- bind_cols(x, bind_cols(x%>%dplyr::select(c(ABSHID)),df)%>%mutate(Identifier = paste0(ABSHID, ABSFID, ABSIID, ABSPID))%>%
                          relocate(Identifier, .after = ABSHID)%>%select(Identifier)) 
    
    ConDF <- conNew %>%
      mutate(Health = CONDCODL%in% c(910, 402, 202, 923) + 0) %>%
      select(Identifier, Health, SEX, AGEPC, FINWTP)
  
  } else {
    
    ID <- x%>%dplyr::select(c(ABSFID, ABSIID, ABSPID))
    df <- mutate_all(ID, as.numeric)
    conNew <- bind_cols(x, bind_cols(x%>%dplyr::select(c(ABSHID)),df)%>%mutate(Identifier = paste0(ABSHID, ABSFID, ABSIID, ABSPID))%>%
                          relocate(Identifier, .after = ABSHID)%>%select(Identifier)) 
    
    ConDF <- conNew %>%
      mutate(Health = CONDCODL%in% c(910, 402, 202, 923) + 0) %>%
      select(Identifier, Health)
  
  }
  
  # disability 
  # disability in dataCON98 # dataPER98 - age, gender, conditions, 
  
 if (all(cols_to_select %in% colnames(y)))
    
  {
  
  ID0 <- y%>%dplyr::select(c(ABSFID, ABSIID, ABSPID))
  df0 <- mutate_all(ID0 , as.numeric)
  
  dataDis <- bind_cols(y, bind_cols(y%>%dplyr::select(c(ABSHID)),df0)%>%mutate(Identifier = paste0(ABSHID, ABSFID, ABSIID, ABSPID))%>%
                        relocate(Identifier, .after = ABSHID)%>%select(Identifier))%>%
    select(Identifier, SEX, AGEPC, LVLBATHE, LVLDRESS, LVLEATIN, LVLTOILT, LVLBOWEL, FINWTP)
  
  DisDF <- dataDis %>%
    mutate(across(c(LVLBATHE, LVLDRESS, LVLEATIN, LVLTOILT, LVLBOWEL), ~ as.integer(. > 2))) %>%
    mutate(disability = case_when(
      rowSums(across(c(LVLBATHE, LVLDRESS, LVLEATIN, LVLTOILT, LVLBOWEL))) > 1 ~ 1,
      TRUE ~ 0
    )) %>%
    select(Identifier, disability)
  
  mergedDF <- left_join(dataDis, ConDF, by = "Identifier") %>%
    left_join(DisDF, by = "Identifier") %>%
    na.omit() %>%
    group_by(Identifier, SEX, AGEPC, FINWTP) %>%
    summarize(disability = max(disability), Health = max(Health)) %>%
    ungroup()%>%
    mutate(HealDis = case_when(
      disability + Health > 1 ~ 1,
      TRUE ~ 0
    )) %>%
    select(Identifier, SEX, AGEPC, disability, Health, HealDis, FINWTP) %>%
    filter(AGEPC >= 24) %>%
    arrange(AGEPC) %>%
    rename(weight = FINWTP)%>%
    mutate(agegroup = case_when(
      AGEPC == 24 ~ "60-64",
      AGEPC == 25 ~ "65-69",
      AGEPC == 26 ~ "70-74",
      AGEPC == 27 ~ "75-79",
      AGEPC == 28 ~ "80-84",
      AGEPC == 29 ~ "85+",
      TRUE ~ NA_character_
    ))
  
  disability_prevalence <- mergedDF %>%
    group_by(SEX, AGEPC, agegroup) %>%
    summarise(total_weight = sum(weight),
              disabled_weight = sum(disability * weight)) %>%
    mutate(disability_prevalence = disabled_weight/total_weight)
  
  illness_prevalence <- mergedDF %>%
    group_by(SEX, AGEPC, agegroup) %>%
    summarise(total_weight = sum(weight),
              illness_weight = sum(Health*weight)) %>%
    mutate(illness_prevalence = illness_weight/total_weight)
  
  illDis_prevalence <- mergedDF %>%
    group_by(SEX, AGEPC, agegroup) %>%
    summarise(total_weight = sum(weight),
              illDis_weight = sum(HealDis*weight)) %>%
    mutate(illDis_prevalence = illDis_weight/total_weight)
  
  prevalance <-  bind_rows(illDis_prevalence%>% rename(prevalence= "illDis_prevalence") %>% mutate(status ="Both"),
                           illness_prevalence%>% rename(prevalence= "illness_prevalence") %>% mutate(status ="Ill"),
                           disability_prevalence%>% rename(prevalence= "disability_prevalence") %>% mutate(status ="Disabled")) %>% ungroup()%>% select(c(SEX, agegroup,prevalence,status))
  
  return(list(data = mergedDF, dis_prev_rate = disability_prevalence, ill_prev_rate = illness_prevalence, illDis_prev_rate = illDis_prevalence, prev = prevalance))
  
  }
  
  else {
    
    ID0 <- y%>%dplyr::select(c(ABSFID, ABSIID, ABSPID))
    df0 <- mutate_all(ID0, as.numeric)
    
    dataDis <- bind_cols(y, bind_cols(y%>%dplyr::select(c(ABSHID)), df0)%>%mutate(Identifier = paste0(ABSHID, ABSFID, ABSIID, ABSPID))%>%
                           relocate(Identifier, .after = ABSHID)%>%select(Identifier))%>%
      select(Identifier, LVLBATHE, LVLDRESS, LVLEATIN, LVLTOILT, LVLBOWEL)
    
    DisDF <- dataDis %>%
      mutate(across(c(LVLBATHE, LVLDRESS, LVLEATIN, LVLTOILT, LVLBOWEL), ~ as.integer(. > 2))) %>%
      mutate(disability = case_when(
        rowSums(across(c(LVLBATHE, LVLDRESS, LVLEATIN, LVLTOILT, LVLBOWEL))) > 1 ~ 1,
        TRUE ~ 0
      ))%>%
      select(Identifier, disability)
    
    mergedDF <- left_join(dataDis, ConDF, by = "Identifier") %>%
      left_join(DisDF, by = "Identifier") %>%
      na.omit() %>%
      group_by(Identifier, SEX, AGEPC, FINWTP) %>%
      summarize(disability = max(disability), Health = max(Health)) %>%
      ungroup()%>%
      mutate(HealDis = case_when(
        disability + Health > 1 ~ 1,
        TRUE ~ 0
      )) %>%
      select(Identifier, SEX, AGEPC, disability, Health, HealDis, FINWTP) %>%
      filter(AGEPC >= 24) %>%
      arrange(AGEPC)%>%
      rename(weight = FINWTP)%>%
      mutate(agegroup = case_when(
        AGEPC == 24 ~ "60-64",
        AGEPC == 25 ~ "65-69",
        AGEPC == 26 ~ "70-74",
        AGEPC == 27 ~ "75-79",
        AGEPC == 28 ~ "80-84",
        AGEPC == 29 ~ "85+",
        TRUE ~ NA_character_
      ))
    
    disability_prevalence <- mergedDF %>%
      group_by(SEX, AGEPC, agegroup) %>%
      summarise(total_weight = sum(weight),
                disabled_weight = sum(disability * weight)) %>%
      mutate(disability_prevalence = disabled_weight/total_weight)
    
    illness_prevalence <- mergedDF %>%
      group_by(SEX, AGEPC, agegroup) %>%
      summarise(total_weight = sum(weight),
                illness_weight = sum(Health*weight)) %>%
      mutate(illness_prevalence = illness_weight/total_weight)
    
    illDis_prevalence <- mergedDF %>%
      group_by(SEX, AGEPC, agegroup) %>%
      summarise(total_weight = sum(weight),
                illDis_weight = sum(HealDis*weight)) %>%
      mutate(illDis_prevalence = illDis_weight/total_weight)

    prevalance <-  bind_rows(illDis_prevalence%>% rename(prevalence= "illDis_prevalence") %>% mutate(status ="Both"),
      illness_prevalence%>% rename(prevalence= "illness_prevalence") %>% mutate(status ="Ill"),
      disability_prevalence%>% rename(prevalence= "disability_prevalence") %>% mutate(status ="Disabled")) %>% select(c(SEX, agegroup,prevalence,status))

    return(list(data = mergedDF, dis_prev_rate = disability_prevalence, ill_prev_rate = illness_prevalence, illDis_prev_rate = illDis_prevalence, prev = prevalance))
    
  }
  
}

# Apply the dataClean function to each pair and store the results in a list

results <- lapply(file_pairs, function(pair) dataClean(pair[[1]], pair[[2]]))
years <- c("1998", "2003", "2009", "2012", "2015", "2018")
resDF <- bind_rows(lapply(1:length(years), function(x) {
  data <- results[[x]]$data %>% mutate(AGEPC = as.character(AGEPC)) %>% mutate(year = years[x])
  as.data.frame(data)}))%>% ungroup()%>%select(c(SEX, agegroup, weight, disability, Health, HealDis, year))%>% as_tibble()%>%mutate(trend = as.numeric(year) - 1998) %>% mutate(trend2 = (trend)^2)%>%mutate(trend3 = (trend)^3/1000)%>%mutate(trend4 = (trend)^4/10000)%>%mutate(trend5 = (trend)^5/100000)
resDF$agegroup <- as.factor(resDF$agegroup)
resDF <- resDF%>%mutate(purehealth = case_when(rowSums(across(c(disability, Health, HealDis))) == 0 ~ 1,
    TRUE ~ 0))

resDF  <- resDF%>%mutate(disability = ifelse(disability == 1 & Health == 1, 0, disability), HealDis = ifelse(disability == 1 & Health == 1, 1, HealDis),
    Health = ifelse(disability == 1 & Health == 1, 0, Health))

resDF  <- resDF%>%mutate(Health = ifelse(HealDis == 1 & Health == 1, 0, Health))

# collect data for each health state 

d <- resDF %>% select(c(SEX,agegroup, weight, disability, trend, year)) %>% rename(status = "disability") %>% mutate(health = "disability")
H <- resDF %>% select(c(SEX,agegroup, weight, Health, trend, year))%>% rename(status = "Health")%>% mutate(health = "ill")
dH <- resDF %>% select(c(SEX,agegroup, weight, HealDis, trend, year))%>% rename(status = "HealDis")%>% mutate(health = "id")
pH <- resDF %>% select(c(SEX,agegroup, weight, purehealth, trend, year))%>%rename(status = "purehealth")%>% mutate(health = "ph")
dDF <- bind_rows(d, H, dH, pH)
dDF$health <- factor(dDF$health)

# prepare data for multinomial distribution 

suDF <- resDF
suDF$purehealth <- ifelse(suDF$purehealth == 1, 4, 0) 
suDF$Health <- ifelse(suDF$Health == 1, 2, 0)
suDF$HealDis <- ifelse(suDF$HealDis == 1, 3, 0)
finsuDF <- suDF%>%mutate(status = rowSums(across(c(disability, Health, HealDis, purehealth))))
finsuDF$status <- factor(finsuDF$status)

df <- finsuDF %>%
  mutate(status = case_when(
    status == "4" ~ "health",
    status == "3" ~ "illDis",
    status == "2" ~ "ill",
    status == "1" ~ "Dis",
    TRUE ~  status))

survey_design <- svydesign(ids = ~1, weights = ~weight, data = finsuDF)
fit <- vglm(status ~ SEX + agegroup  + trend, multinomial(refLevel = "health"), df, weights = weight/mean(weight, na.rm=T))
ss0 <- summary(fit)

fitted_values <- predict(fit, type = "response")
finsuDF$fitted_values <- fitted_values
prevalence_data0 <- aggregate(fitted_values ~ agegroup + SEX + year, data = df, FUN = mean)
prDF0 <- prevalence_data0%>% as_tibble()%>% rename(Gender = SEX) %>% mutate(Gender = ifelse(Gender == 1, "Male", "Female"))%>%rename(pid1 = "Dis")%>%rename(pid2 = "ill")%>%rename(pid3 = "illDis")%>%rename(pid4 = "health")
prDF0$Gender <- factor(prDF0$Gender , c("Male", "Female"))

# plotting prevalance rate

ggplot(prDF0) + geom_line(aes(x= agegroup, y = pid2, group = year, colour = factor(year)), size = 1) +
  labs(x = "Age Group", y = "Prevalence", color = "Gender") +
  theme(legend.key.size = unit(0.5, "cm"), strip.text = element_text(size=10),
        axis.text.y = element_text(angle = 90, hjust = 0.3), axis.text.x = element_text(angle = 90, hjust = 0.3), axis.title.x = element_text(vjust = - 0.5),
        legend.text = element_text(size = 10), legend.key.width = unit(0.8,"cm"), strip.background =element_rect(fill="white"),
        panel.grid.major = element_blank(), panel.background = element_blank(), axis.line = element_line(colour = "black"),
        legend.position="bottom") + facet_wrap(~Gender, nrow = 2, strip.position = "top")

ggplot(prDF0) + geom_line(aes(x= agegroup, y = pid1, group = year, colour = factor(year)), size = 1) +
  labs(x = "Age Group", y = "Prevalence", color = "Gender") +
  theme(legend.key.size = unit(0.5, "cm"), strip.text = element_text(size=10),
        axis.text.y = element_text(angle = 90, hjust = 0.3), axis.text.x = element_text(angle = 90, hjust = 0.3), axis.title.x = element_text(vjust = - 0.5),
        legend.text = element_text(size = 10), legend.key.width = unit(0.8,"cm"), strip.background =element_rect(fill="white"),
        panel.grid.major = element_blank(), panel.background = element_blank(), axis.line = element_line(colour = "black"),
        legend.position="bottom") + facet_wrap(~Gender, nrow = 2, strip.position = "top")

ggplot(prDF0) + geom_line(aes(x= agegroup, y = pid3, group = year, colour = factor(year)), size = 1) +
  labs(x = "Age Group", y = "Prevalence", color = "Gender") +
  theme(legend.key.size = unit(0.5, "cm"), strip.text = element_text(size=10),
        axis.text.y = element_text(angle = 90, hjust = 0.3), axis.text.x = element_text(angle = 90, hjust = 0.3), axis.title.x = element_text(vjust = - 0.5),
        legend.text = element_text(size = 10), legend.key.width = unit(0.8,"cm"), strip.background =element_rect(fill="white"),
        panel.grid.major = element_blank(), panel.background = element_blank(), axis.line = element_line(colour = "black"),
        legend.position="bottom") + facet_wrap(~Gender, nrow = 2, strip.position = "top")

ggplot(prDF0) + geom_line(aes(x= agegroup, y = pid4, group = year, colour = factor(year)), size = 1) +
  labs(x = "Age Group", y = "Prevalence", color = "Gender") +
  theme(legend.key.size = unit(0.5, "cm"), strip.text = element_text(size=10),
        axis.text.y = element_text(angle = 90, hjust = 0.3), axis.text.x = element_text(angle = 90, hjust = 0.3), axis.title.x = element_text(vjust = - 0.5),
        legend.text = element_text(size = 10), legend.key.width = unit(0.8,"cm"), strip.background =element_rect(fill="white"),
        panel.grid.major = element_blank(), panel.background = element_blank(), axis.line = element_line(colour = "black"),
        legend.position="bottom") + facet_wrap(~Gender, nrow = 2, strip.position = "top")

# transition rates

ratesDF <- read.csv("ratesdDF.csv")%>%as_tibble()%>%select(-X)
state1 <- read.csv("dismale.csv")%>%as_tibble() %>% rename(pid1 = "prevalence")
state2 <- read.csv("hmale.csv")%>%as_tibble()%>%rename(pid2 = "prevalence")
state3 <- read.csv("hdismale.csv")%>%as_tibble()%>% rename(pid3 = "prevalence")
state4 <- read.csv("multi.csv")%>%as_tibble()%>% select(-X)%>%rename(pid1 = "disability")%>%rename(pid2 = "id")%>%rename(pid3 = "ill")%>%rename(etax = "ph")%>% rename(SEX= "Gender")

Time <- 2018

state <- state1%>% mutate(pid2 = state2 %>% pull(pid2))%>%mutate(pid3 = state3 %>% pull(pid3))
rates <- ratesDF%>%filter(Gender=="Male")%>%filter(year==Time)%>% select(-Gender)
stateDF0 <- state%>%filter(year==Time)%>% mutate(rate = rates%>%pull(Rates))%>%mutate(etax = (1- (pid1 + pid2 + pid3)))%>% select(-X)
stateDF9 <- state4%>%filter(SEX=="Male")%>%filter(year==Time)%>% mutate(rate = rates%>%pull(Rates))
stateDF <- read.csv("finmulti.csv")%>%as_tibble()%>%select(-X)%>%rename(etax = "pid4")%>%filter(Gender=="Male")%>%filter(year==Time)%>% mutate(rate = rates%>%pull(Rates))

data1 <- read.csv("data1.csv")%>%as_tibble()%>%select(-X)

k1 = 0.2047; k2 = 0.2848; k3 = 0.2942; k4 = 0.8486; k5 = 0.8671; k6 = 0.9637; k7 = 0.1582 

j1 <- 0.942; j2 <- 0.4811; j3 <- 0.3820; j4 <- 0.99

# extrapolate 

x_new <- 87

fit_exponential <- function(data, x_new) 
  
{
  fit <- lm(log(y) ~ x, data = data)
  a <- coef(fit)[2]
  b <- coef(fit)[1]
  
  predict_exp <- function(x) {
    exp(a * x + b)
  }
  y_pred <- predict_exp(x_new)
  
  return(y_pred)
}

x <- c(62, 67, 72, 77, 82, 87)

phd3 <- function(data, kxd1d2, kxd1d3, kxd2d3, kxd1d, kxd2d, kxd3d) 
  
{  
  qx <- data$rate
  pid1 <- data$pid1
  pid2 <- data$pid2
  pid3 <-  data$pid3
  pid4 <-  c(data$pid3, j3)[-1]  
  etax <- data$etax
  
  numerator <- (pid4*(1 - qx) + pid3*((1 + kxd3d) * qx - 1))
  denominator <- (etax + pid1*(1 + kxd1d3) + pid2*(1 + kxd2d3))
  
  y <- numerator/denominator
  
  return(y)
}

result <- phd3(stateDF,k1,k2,k3,k4,k5,k6)
phd33 <- data.frame(tran = result, age = stateDF$agegroup, year = stateDF$year, exactage = x) %>% as_tibble()
y_pred <- fit_exponential(head(phd33, 5)%>%select(c(tran, exactage))%>%rename(y ="tran")%>%rename(x = "exactage"), x_new)
phd3 <- rbind(head(phd33, 5)%>%select(-exactage), data.frame(tran = y_pred[1], age = "85+", year = phd33$year[1])%>% as_tibble())

phd2 <- function(data, kxd1d2, kxd1d3, kxd2d3, kxd1d,  kxd2d, kxd3d) 
  
{
  qx <- data$rate
  pid1 <- data$pid1
  pid2 <- data$pid2
  pid3 <- data$pid3
  pid4 <- c(data$pid2, j2)[-1]  
  etax <- data$etax
  phd <- phd3$tran
  
  numerator <- (pid4*(1 - qx) +  pid2*((1 + kxd2d)*qx + (1 + kxd1d3)*phd -1))
  denominator <- (etax + pid1*(1 + kxd1d2))
  
  y <- numerator/denominator
  
  return(y)
  
}

result1 <- phd2(stateDF, k1,k2,k3,k4,k5, k6)
phd22 <- data.frame(tran = result1, age = stateDF$agegroup, year = stateDF$year, exactage = x) %>% as_tibble()
y_pred2 <- fit_exponential(head(phd22, 5)%>%select(c(tran, exactage))%>%rename(y ="tran")%>%rename(x = "exactage"), x_new)
phd2 <- rbind(head(phd22, 5)%>%select(-exactage), data.frame(tran = y_pred2[1], age = "85+", year = phd22$year[1])%>% as_tibble())

pxhd <- function(data, kxhd) 
  
{
  qx <- data$rate
  pid1 <- data$pid1
  pid2 <- data$pid2
  pid3 <- data$pid3
  pid4 <- c(data$pid2, j2)[-1]  
  etax <- data$etax
  phd3 <- phd3$tran
  phd2 <- phd2$tran
  
  numerator <- qx*(1 - kxhd)
  
  return(numerator)
}

result12 <- pxhd(stateDF,k7)
phd0 <- data.frame(tran = result12, age = stateDF$agegroup, year = stateDF$year, exactage = x) %>% as_tibble()
y_pred0 <- fit_exponential(head(phd0, 5)%>%select(c(tran, exactage))%>%rename(y ="tran")%>%rename(x = "exactage"), x_new)
phd <- rbind(head(phd0, 5)%>%select(-exactage), data.frame(tran = y_pred0[1], age = "85+", year = phd0$year[1])%>% as_tibble())

phd1 <- function(data, kxd1d2, kxd1d3, kxd2d3, kxd1d,  kxd2d, kxd3d)
  
{
  qx <- data$rate
  pid1 <- data$pid1
  pid2 <- data$pid2
  pid3 <- data$pid3
  pid4 <- c(data$pid1, j1)[-1]  
  etax <- data$etax
  phd3 <- phd3$tran
  phd2 <- phd2$tran
  phd <- phd$tran
  
  numerator <- pid4*(1 - qx) + pid1*((1 + kxd1d)*qx + (1 + kxd1d3)*phd3 + (1 + kxd1d2)*phd2 - 1)
  denominator <- etax
  
  y <- numerator/denominator
  
  return(y)
  
}

result2 <- phd1(stateDF, k1, k2, k3, k4, k5, k6)
phd11 <- data.frame(tran = result2, age = stateDF$agegroup, year = stateDF$year, exactage = x) %>% as_tibble()
y_pred3 <- fit_exponential(head(phd11, 5)%>%select(c(tran, exactage))%>%rename(y ="tran")%>%rename(x = "exactage"), x_new)
phd1 <- rbind(head(phd11, 5)%>%select(-exactage), data.frame(tran = y_pred3[1], age = "85+", year = phd11$year[1])%>% as_tibble())

pxhh <- function(data) 
  
{
  qx <- data$rate
  pid1 <- data$pid1
  pid2 <- data$pid2
  pid3 <- data$pid3
  etax <- data$etax
  pid4 <- c(data$etax, j4)[-1]
  phd3 <- phd3$tran
  phd2 <- phd2$tran
  phd1 <- phd1$tran
  
  numerator <- pid4*(1 - qx)
  denominator <- etax
  
  pxhh <- numerator/denominator
  
  return(pxhh)
}

result6 <- pxhh(stateDF)
phh24 <- data.frame(tran = result6, age = stateDF$agegroup, year = stateDF$year, exactage = x) %>% as_tibble()
y_pred24 <- fit_exponential(head(phh24, 5)%>%select(c(tran, exactage))%>%rename(y ="tran")%>%rename(x = "exactage"), x_new)
phh <- rbind(head(phh24, 5)%>%select(-exactage), data.frame(tran = y_pred24[1], age = "85+", year = phh24$year[1])%>% as_tibble())

pxd1d1 <- function(data) 
  
{
  qx <- data$rate
  pid1 <- data$pid1
  pid2 <- data$pid2
  pid3 <- data$pid3
  pid4 <- c(data$pid1, j1)[-1]  
  etax <- data$etax
  phd3 <- phd3$tran
  phd2 <- phd2$tran
  phd1 <- phd1$tran
  
  numerator <- pid4*(1 - qx) - etax*phd1
  denominator <-  pid1
  
  pxd1d1 <- numerator/denominator
  
  return(pxd1d1)
}

result3 <- pxd1d1(stateDF)
pd1d11 <- data.frame(tran = result3, age = stateDF$agegroup, year = stateDF$year, exactage = x) %>% as_tibble()
y_pred4 <- fit_exponential(head(pd1d11, 5)%>%select(c(tran, exactage))%>%rename(y ="tran")%>%rename(x = "exactage"), x_new)
pd1d1 <- rbind(head(pd1d11, 5)%>%select(-exactage), data.frame(tran = y_pred4[1], age = "85+", year =pd1d11$year[1])%>% as_tibble())

pxd2d2 <- function(data, kxd1d2) 
  
{
  
  qx <- data$rate
  pid1 <- data$pid1
  pid2 <- data$pid2
  pid3 <- data$pid3
  pid4 <- c(data$pid2, j2)[-1]  
  etax <- data$etax
  phd3 <- phd3$tran
  phd2 <- phd2$tran
  phd1 <- phd1$tran
  
  numerator <- pid4*(1 - qx) - etax*phd2 - pid1*phd2 - pid1*(1 + kxd1d2)*phd2
  denominator <-  pid2
  
  pxd2d2 <- numerator/denominator
  
  return(pxd2d2)
}

result4 <- pxd2d2(stateDF, k1)
pd2d22 <- data.frame(tran = result4, age = stateDF$agegroup, year = stateDF$year, exactage = x) %>% as_tibble()
y_pred5 <- fit_exponential(head(pd2d22, 5)%>%select(c(tran, exactage))%>%rename(y ="tran")%>%rename(x = "exactage"), x_new)
pd2d2 <- rbind(head(pd2d22, 5)%>%select(-exactage), data.frame(tran = y_pred5[1], age = "85+", year = pd2d22$year[1])%>% as_tibble())

pxd3d3 <- function(data, kxd1d3, kxd2d3) 
  
{
  qx <- data$rate
  pid1 <- data$pid1
  pid2 <- data$pid2
  pid3 <- data$pid3
  pid4 <- c(data$pid3, j3)[-1]  
  etax <- data$etax
  phd3 <- phd3$tran
  phd2 <- phd2$tran
  phd1 <- phd1$tran
  
  numerator <- pid4*(1 - qx) - etax*phd3 - pid2*(1 + kxd2d3)*phd3 - pid1*(1 + kxd1d3)*phd3
  denominator <-  pid3
  
  pxd3d3 <- numerator/denominator
  
  return(pxd3d3)
}

result5 <- pxd3d3(stateDF, k2, k3)
pd3d33 <- data.frame(tran = result5, age = stateDF$agegroup, year = stateDF$year, exactage = x) %>% as_tibble()
y_pred6 <- fit_exponential(head(pd3d33, 5)%>%select(c(tran, exactage))%>%rename(y ="tran")%>%rename(x = "exactage"), x_new)
pd3d3 <- rbind(head(pd3d33, 5)%>%select(-exactage), data.frame(tran = y_pred6[1], age = "85+", year = pd3d33$year[1])%>% as_tibble())

pxd3d <- function(data, kxd3d) 
  
{
  qx <- data$rate
  pid1 <- data$pid1
  pid2 <- data$pid2
  pid3 <- data$pid3
  etax <- data$etax
  pid4 <- c(data$etax, j4)[-1] 
  phd3 <- phd3$tran
  phd2 <- phd2$tran
  phd1 <- phd1$tran
  
  numerator <- (1 + kxd3d)*qx
  
  return(numerator)
}

result7 <- pxd3d(stateDF, k6)
pd3d0 <- data.frame(tran = result7, age = stateDF$agegroup, year = stateDF$year, exactage = x) %>% as_tibble()
y_pred7 <- fit_exponential(head(pd3d0, 5)%>%select(c(tran, exactage))%>%rename(y ="tran")%>%rename(x = "exactage"), x_new)
pd3d <- rbind(head(pd3d0, 5)%>%select(-exactage), data.frame(tran = y_pred7[1], age = "85+", year =pd3d0$year[1])%>% as_tibble())

pxd2d <- function(data, kxd2d) 
  
{
  qx <- data$rate
  pid1 <- data$pid1
  pid2 <- data$pid2
  pid3 <- data$pid3
  etax <- data$etax
  pid4 <- c(data$etax, j4)[-1] 
  phd3 <- phd3$tran
  phd2 <- phd2$tran
  phd1 <- phd1$tran
  
  numerator <- (1 + kxd2d)*qx
  
  return(numerator)
}

result8 <- pxd2d(stateDF, k5)
pd2d0 <- data.frame(tran = result8, age = stateDF$agegroup, year = stateDF$year, exactage = x) %>% as_tibble()
y_pred8 <- fit_exponential(head(pd2d0, 5)%>%select(c(tran, exactage))%>%rename(y ="tran")%>%rename(x = "exactage"), x_new)
pd2d <- rbind(head(pd2d0, 5)%>%select(-exactage), data.frame(tran = y_pred8[1], age = "85+", year = pd2d0$year[1])%>% as_tibble())

pxd1d2 <- function(data, kxd1d2) 
  
{
  qx <- data$rate
  pid1 <- data$pid1
  pid2 <- data$pid2
  pid3 <- data$pid3
  pid4 <- c(data$pid2, j2)[-1]  
  etax <- data$etax
  phd3 <- phd3$tran
  phd2 <- phd2$tran
  phd1 <- phd1$tran
  
  numerator <- (1 + kxd1d2)*phd2
  
  return(numerator)
}

result9 <- pxd1d2(stateDF, k1)
pd1d22 <- data.frame(tran = result9, age = stateDF$agegroup, year = stateDF$year, exactage = x) %>% as_tibble()
y_pred9 <- fit_exponential(head(pd1d22, 5)%>%select(c(tran, exactage))%>%rename(y ="tran")%>%rename(x = "exactage"), x_new)
pd1d2 <- rbind(head(pd1d22, 5)%>%select(-exactage), data.frame(tran = y_pred9[1], age = "85+", year = pd1d22$year[1])%>% as_tibble())

pxd1d <- function(data, kxd1d) 
  
{
  qx <- data$rate
  pid1 <- data$pid1
  pid2 <- data$pid2
  pid3 <- data$pid3
  etax <- data$etax
  pid4 <- c(data$etax, j4)[-1] 
  phd3 <- phd3$tran
  phd2 <- phd2$tran
  phd1 <- phd1$tran
  
  numerator <- (1 + kxd1d)*qx
  
  return(numerator)
}

result10 <- pxd1d(stateDF, k4)
pd1d0 <- data.frame(tran = result10, age = stateDF$agegroup, year = stateDF$year, exactage = x) %>% as_tibble()
y_pred10 <- fit_exponential(head(pd1d0, 5)%>%select(c(tran, exactage))%>%rename(y ="tran")%>%rename(x = "exactage"), x_new)
pd1d <- rbind(head(pd1d0, 5)%>%select(-exactage), data.frame(tran = y_pred10[1], age = "85+", year =pd1d0$year[1])%>% as_tibble())

pxd1d3 <- function(data, kxd1d3) 
  
{
  
  qx <- data$rate
  pid1 <- data$pid1
  pid2 <- data$pid2
  pid3 <- data$pid3
  pid4 <- c(data$pid2, j2)[-1]  
  etax <- data$etax
  phd3 <- phd3$tran
  phd2 <- phd2$tran
  phd1 <- phd1$tran
  
  numerator <- (1 + kxd1d3)*phd3
  
  return(numerator)
}

result11 <- pxd1d3(stateDF, k2)
pd1d33 <- data.frame(tran = result11, age = stateDF$agegroup, year = stateDF$year, exactage = x) %>% as_tibble()
y_pred11 <- fit_exponential(head(pd1d33, 5)%>%select(c(tran, exactage))%>%rename(y ="tran")%>%rename(x = "exactage"), x_new)
pd1d3 <- rbind(head(pd1d33, 5)%>%select(-exactage), data.frame(tran = y_pred11[1], age = "85+", year =pd1d0$year[1])%>% as_tibble())

pxd2d3 <- function(data, kxd2d3) 
  
{
  qx <- data$rate
  pid1 <- data$pid1
  pid2 <- data$pid2
  pid3 <- data$pid3
  pid4 <- c(data$pid2, j2)[-1]  
  etax <- data$etax
  phd3 <- phd3$tran
  phd2 <- phd2$tran
  phd1 <- phd1$tran
  
  numerator <- (1 + kxd2d3)*phd3
  
  return(numerator)
}

result15 <- pxd2d3(stateDF, k3)
pd2d33 <- data.frame(tran = result15, age = stateDF$agegroup, year = stateDF$year, exactage = x) %>% as_tibble()

y_pred12 <- fit_exponential(head(pd2d33, 5)%>%select(c(tran, exactage))%>%rename(y ="tran")%>%rename(x = "exactage"), x_new)
pd2d3 <- rbind(head(pd2d33, 5)%>%select(-exactage), data.frame(tran = y_pred12[1], age = "85+", year = pd2d33$year[1])%>% as_tibble())

pd3dDF <- pd3d3%>%mutate(tran = 1 - tran) %>% mutate(var="pd3d")

phdDF <- bind_cols(phd%>%rename(phd = "tran") %>% select(phd), phd1 %>% rename(phd1 = "tran")%>% select(phd1), phd2%>% rename(phd2 = "tran")%>% select(phd2), phd3 %>% rename(phd3 ="tran")%>% select(phd3)) %>% mutate(phh = 1 - (phd1 + phd2 + phd3 + phd)) 
phdD <- pd3d3 %>% mutate(tran = phdDF %>% pull(phh))

pd1d1DF <- bind_cols(pd1d2%>%rename(pd1d2 = "tran")%>%select(pd1d2),  pd1d3%>%rename(pd1d3 = "tran")%>% select(pd1d3), pd1d%>%rename(pd1d = "tran")%>% select(pd1d)) %>% mutate(pd1d1 = 1- (pd1d2 + pd1d3 + pd1d)) 
pd1d1D <- pd3d3 %>% mutate(tran = pd1d1DF %>% pull(pd1d1))

phdDF <- bind_cols(phh%>%rename(phh = "tran")%>%select(phh),  phd1%>%rename(phd1 = "tran")%>%select(phd1),  phd2%>%rename(phd2 = "tran")%>%select(phd2),  phd3%>%rename(phd3 = "tran")%>%select(phd3)) %>% mutate(phd = 1- (phh + phd1 + phd2 +  phd3)) 

pd1d1D <- pd3d3 %>% mutate(tran = pd1d1DF %>% pull(pd1d1))

data <- bind_rows(phd%>%mutate(var="phd"), pd1d1D%>%mutate(var="pd1d1"), phh%>% mutate(var="phh"), pd2d%>% mutate(var="pd2d"), pd3d%>% mutate(var="pd3d"), pd2d2%>% mutate(var="pd2d2"), pd1d%>% mutate(var="pd1d"), pd3d3%>% mutate(var="pd3d3"), phd1%>% mutate(var="phd1"), phd2 %>% mutate(var = "phd2"), phd3 %>% mutate(var = "phd3"), pd1d2 %>% mutate(var = "pd1d2"), pd1d3%>%mutate(var="pd1d3"), pd2d3%>%mutate(var="pd2d3")) %>% mutate(year = as.character(year))

