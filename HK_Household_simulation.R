library(readxl)
library(tidyr)
library(dplyr)
library(readr)
library(ggplot2)
library(forcats)

rm(list=ls())
setwd("D:/Edward/Simulation")

# Age distribution of Hong Kong (from 2016 By-Census) by HH size and HH type
HK <- read_excel("hk census_household size and age_Ddate20210812.xlsx", 
                 sheet="D108e", 
                 range="C9:AD122", 
                 col_names = c('Age', paste("Sex",3:20), 1:8,'n')) %>%
  select(Age, `1`:`8`, n) %>%
  filter(Age != "Sub-Total") %>%
  mutate(hh_size=rep(1:6, each=18),
         age=fct_collapse(Age,
                          '0 - 9'=c('0 - 4','5 - 9'),
                          '10 - 19'=c('10 - 14','15 - 19'),
                          '20 - 29'=c('20 - 24','25 - 29'),
                          '30 - 39'=c('30 - 34','35 - 39'),
                          '40 - 49'=c('40 - 44','45 - 49'),
                          '50 - 59'=c('50 - 54','55 - 59'),
                          '60 - 69'=c('60 - 64','65 - 69'),
                          '70 - 79'=c('70 - 74','75 - 79'),
                          '80+'=c('80 - 84','85+')))
print(HK, n=Inf)

HH_age <- read_excel("hk census_household size and age_Ddate20210812.xlsx", 
                     sheet="D108e", 
                     range="AD123:AD140", 
                     col_names = 'n') %>%
  mutate(age=rep(1:9, each=2)) %>%
  group_by(age) %>%
  summarise(n=sum(n)) %>%
  mutate(p=n/sum(n))
HH_age

# 1. Select household size
HH_size <- read_excel("hk census_household size and age_Ddate20210812.xlsx", 
                      sheet="D103ae", 
                      range="F6:F11", 
                      col_names = 'n') %>%
  mutate(hh_size=1:6, p=n/sum(n)) %>%
  select(hh_size, p)
HH_size

# 2. Age range of household head
HH_head <- read_excel("hk census_household size and age_Ddate20210812.xlsx", 
                      sheet="D121e", 
                      range="D293:J302", 
                      col_names = c('Age', 1:6)) %>%
  pivot_longer(2:7, names_to="hh_size") %>%
  mutate(age=recode(Age,
                    "0 - 24"=1,
                    "25 - 29"=2,
                    "30 - 34"=3,
                    "35 - 39"=4,
                    "40 - 44"=5,
                    "45 - 49"=6,
                    "50 - 54"=7,
                    "55 - 59"=8,
                    "60 - 64"=9,
                    "65+"=10,
  )) %>%
  group_by(hh_size) %>%
  mutate(p=value/sum(value)) %>%
  arrange(hh_size)
HH_head

# 3. Age range of HH head by household type
HH_type <- read_excel("hk census_household size and age_Ddate20210812.xlsx", 
                      sheet="D121e", range="C7:J269", 
                      col_names = c('Sex','Age', 1:6)) %>%
  fill(Sex) %>%
  filter(Sex=="Sub-Total" & Age!="Sub-Total") %>%
  mutate(hh_type=rep(1:8, each=10)) %>%
  select(-Sex) %>%
  pivot_longer(2:7, names_to="hh_size", values_to="n") %>%
  mutate(n=ifelse(n=="-", 0, n)) %>%
  type_convert() %>%
  group_by(Age, hh_size) %>% # probabilities in each age/hh_size must add to 1
  mutate(p=if_else(n>0, n/sum(n), 0)) %>% ungroup()
HH_type

# hh_types
# 1=couple, 2=couple+child, 3=parent+child, 4=couple+parent, 
# 5=couple+parent+child, 6=other, 7=single, 8=complex

ggplot(HH_type, aes(x=hh_size, y=p, fill=hh_type)) +
  facet_wrap(~Age) +
  geom_col() +
  scale_x_continuous(breaks=1:6)

HH_types <- group_by(HH_type, hh_type) %>%
  summarise(n=sum(n), .groups='drop') %>%
  mutate(p=n/sum(n))

# 4. Age of partner if type in {2, 4, 5}
# No data, so assume difference in age of household head and partner is normally distributed with mean 0 and sd 2

# 5. Age of child(ren)
# No data on age, so assume that children are younger than the parent by 30 years +/- 5 years 
# based on median age of women at first childbirth.
# 30 years is 3 age groups.
# Can use the census to help with generating probabilities of 0, 1, 2, and 3 chidlren according to household size and type.

HH_child <- read_excel("hk census_household size and age_Ddate20210812.xlsx", 
                       sheet="D105e", range="C7:H75", 
                       col_names = c('hh_type', 'hh_size', 0:3)) %>%
  fill(hh_type) %>%
  filter(hh_size!="Sub-Total" & hh_type!="Sub-Total") %>%
  mutate(hh_type=sub('.+\\((\\d)\\)', '\\1', hh_type)) %>%
  pivot_longer(3:6, names_to="child", values_to="n") %>%
  mutate(n=ifelse(n=="-", 0, n),
         hh_size=recode(hh_size, "6 and over"='6')) %>%
  type_convert() %>%
  filter(n>0) %>%
  group_by(hh_type, hh_size) %>% # probabilities in each hh_type/hh_size must add to 1
  mutate(p=if_else(n>0, n/sum(n), 0)) %>%
  select(-n) 

print(HH_child, n=Inf)

# Households with members aged 65 and over
HH_elder <- read_excel("hk census_household size and age_Ddate20210812.xlsx", 
                       sheet="D106e", range="C7:H75", 
                       col_names = c('hh_type', 'hh_size', 0:3)) %>%
  fill(hh_type) %>%
  filter(hh_size!="Sub-Total" & hh_type!="Sub-Total") %>%
  mutate(hh_type=sub('.+\\((\\d)\\)', '\\1', hh_type)) %>%
  pivot_longer(3:6, names_to="elder", values_to="n") %>%
  mutate(n=ifelse(n=="-", 0, n),
         hh_size=recode(hh_size, "6 and over"='6')) %>%
  type_convert() %>%
  filter(n>0) %>%
  group_by(hh_type, hh_size) %>% # probabilities in each hh_type/hh_size must add to 1
  mutate(p=if_else(n>0, n/sum(n), 0)) %>%
  select(-n) 

print(HH_elder, n=Inf)

# Algorithm
source("simulate.R")
#debug(simulate)

HK_HH <- simulate(2000, HH_size, HH_type, HH_head, HH_child, HH_elder, HH_age, age.threshold=0)

p_size <- tabulate(rowSums(HK_HH$hh)) / sum(tabulate(rowSums(HK_HH$hh)))
tibble(HH_size, p_size, diff=round(p_size-HH_size$p, 4))
sum((HH_size$p - p_size)^2)

p_age <- colSums(HK_HH$hh) / sum(colSums(HK_HH$hh))
tibble(HH_age, p_age, diff=round(p_age-HH_age$p, 3))
sum((HH_age$p - p_age)^2)

table(HK_HH$type)
p_type <- tabulate(HK_HH$type)/sum(tabulate(HK_HH$type))
tibble(HH_types, p_type, diff=round(p_type-HH_types$p, 3))
sum((HH_types$p - p_type)^2)

write.csv(HK_HH$hh, file="Simulation.csv")
write.csv(unlist(HK_HH$type), file="Simulation_types.csv")
save.image("Simulate.Rda")
# End