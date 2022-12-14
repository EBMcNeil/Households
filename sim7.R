library(readxl)
library(tidyr)
library(dplyr)
library(readr)
library(ggplot2)
library(forcats)

rm(list=ls())
setwd("D:/Edward/Simulation")

load("Simulate.Rda")

# Assess age-mixing patterns
hh_1 <- colSums(HK_HH$hh[rowSums(HK_HH$hh)==1,])
hh_1_p <- hh_1/sum(hh_1)

filter(HK, hh_size==1) %>%
  group_by(age) %>%
  summarise(n=sum(n), .groups = 'drop') %>%
  mutate(p_pop=n/sum(n), p_sample=hh_1_p, diff=p_sample-p_pop)
# The most common age group for hh of size 1 in HK is 50-59 (20.6%), which is similar to pop (21.7%)

hh_2 <- colSums(HK_HH$hh[rowSums(HK_HH$hh)==2,])
hh_2_p <- hh_2/sum(hh_2)

filter(HK, hh_size==2) %>%
  group_by(age) %>%
  summarise(n=sum(n), .groups = 'drop') %>%
  mutate(p_pop=n/sum(n), p_sample=hh_2_p, diff=p_sample-p_pop)
# The most common age groups for hh of size 2 are 50-59 (19.1%) and 60-69 (16.5%), which are similar to pop (18.6% and 18.8%)

Sim <- tibble(as.data.frame(HK_HH$hh))
head(Sim)

hh_hk <- mutate(Sim, hh_size=rowSums(across(everything())),
                hid=1:nrow(Sim)) %>%
  pivot_longer(-c(hid, hh_size), names_to="age") %>%
  filter(value>0) %>%
  uncount(weights=value) %>%
  group_by(hh_size, hid) %>%
  mutate(pid=row_number()) %>%
  pivot_wider(id_cols=c(hid, hh_size), names_from=pid, names_prefix="age_", values_from=age) 
hh_hk

filter(hh_hk, hh_size==2) %>%
  group_by(age_1, age_2) %>%
  summarise(n=n(), .groups='drop') %>%
  arrange(-n) %>%
  mutate(p=n/sum(n))
# Most common pattern in households of size 2 is 50-59{2} (10.1%)

filter(hh_hk, hh_size==3) %>%
  group_by(age_1, age_2, age_3) %>%
  summarise(n=n(), .groups='drop') %>%
  arrange(-n) %>%
  mutate(p=n/sum(n))
# Most common pattern in households of size 3 is 50-59{2} + 20-29{1} (6.5%)
# i.e., a couple aged 50-59 living with a single child aged 20-29

filter(hh_hk, hh_size==4) %>%
  group_by(age_1, age_2, age_3, age_4) %>%
  summarise(n=n(), .groups='drop') %>%
  arrange(-n) %>%
  mutate(p=n/sum(n))
# Most common pattern in households of size 4 is 30-39{2} + 0-9{2} (4.9%)
# i.e., a couple aged 30-39 living with 2 children aged 0-9

filter(hh_hk, hh_size==5) %>%
  group_by(age_1, age_2, age_3, age_4, age_5) %>%
  summarise(n=n(), .groups='drop') %>%
  arrange(-n) %>%
  mutate(p=n/sum(n))
# Most common pattern in households of size 5 is 30-39{2} + 0-9{3} (4.4%)
# i.e., a couple aged 30-39 living with 2 children aged 0-9

filter(hh_hk, hh_size==6) %>%
  group_by(age_1, age_2, age_3, age_4, age_5, age_6) %>%
  summarise(n=n(), .groups='drop') %>%
  arrange(-n) %>%
  mutate(p=n/sum(n))
# Most common pattern in households of size 6 is 30-39{2} + 70-79{1} + 0-9{2} (2.6%)
# i.e., a couple aged 30-39 living with 2 children aged 0-9 and a single parent aged 70-79 yrs


# Read in the UK data
UK <- read_excel("Simulate 10000 households with HK demographic data_ABM_2.xlsx",
                 sheet="baseline_household_demographic",
                 range="B1:K10001")
UK <- mutate(UK, hid=1:10000); UK

# Distribution of hh sizes among 10,000 households
hh_UK <- count(UK, hh_size) %>%
  mutate(p=n/sum(n)) # Unequal distribution, and very dissimilar to HK.
hh_UK
# Assume these are the proportions in the population?

mutate(Sim, hh_size=rowSums(across(everything()))) %>%
  count(hh_size) %>%
  mutate(p=n/sum(n))

# hh with members aged 0-9
count(UK, hh_size, a_0_9) %>% 
  xtabs(n~hh_size+a_0_9, data=.) %>%
  addmargins()


filter(UK, hh_size==1) %>%
  pivot_longer(starts_with("a"), names_to="age") %>%
  group_by(age) %>%
  summarise(n=sum(value), .groups='drop') %>%
  mutate(p=n/sum(n))
# The most common age group for hh of size 1 in the UK is 60-69 (17.0%)

# Age-mixing patterns
filter(UK, hh_size==2) %>%
  pivot_longer(starts_with("a"), names_to="age") %>%
  filter(value>0) %>%
  uncount(weights=value) %>%
  group_by(hh_size, hid) %>%
  mutate(pid=row_number()) %>%
  pivot_wider(id_cols=c(hid, hh_size), names_from=pid, names_prefix="age_", values_from=age) %>%
  group_by(age_1, age_2) %>%
  summarise(n=n(), .groups='drop') %>%
  arrange(-n) %>%
  mutate(p=n/sum(n))
# The most common age group for hh of size 2 in the UK is 60-69{2} (14.7%)

filter(UK, hh_size==3) %>%
  pivot_longer(starts_with("a"), names_to="age") %>%
  filter(value>0) %>%
  uncount(weights=value) %>%
  group_by(hh_size, hid) %>%
  mutate(pid=row_number()) %>%
  pivot_wider(id_cols=c(hid, hh_size), names_from=pid, names_prefix="age_", values_from=age) %>%
  group_by(age_1, age_2, age_3) %>%
  summarise(n=n(), .groups='drop') %>%
  arrange(-n) %>%
  mutate(p=n/sum(n))
# The most common age group for hh of size 3 in the UK is 30-39{2} + 0-9{1} (8.6%)

filter(UK, hh_size==4) %>%
  pivot_longer(starts_with("a"), names_to="age") %>%
  filter(value>0) %>%
  uncount(weights=value) %>%
  group_by(hh_size, hid) %>%
  mutate(pid=row_number()) %>%
  pivot_wider(id_cols=c(hid, hh_size), names_from=pid, names_prefix="age_", values_from=age) %>%
  group_by(age_1, age_2, age_3, age_4) %>%
  summarise(n=n(), .groups='drop') %>%
  arrange(-n) %>%
  mutate(p=n/sum(n))
# The most common age groups for hh of size 4 in the UK are 30-39{2} + 0-9{2} (11.9%) and 40-49{2} + 10-19{2} (11.1%)

filter(UK, hh_size==5) %>%
  pivot_longer(starts_with("a"), names_to="age") %>%
  filter(value>0) %>%
  uncount(weights=value) %>%
  group_by(hh_size, hid) %>%
  mutate(pid=row_number()) %>%
  pivot_wider(id_cols=c(hid, hh_size), names_from=pid, names_prefix="age_", values_from=age) %>%
  group_by(age_1, age_2, age_3, age_4, age_5) %>%
  summarise(n=n(), .groups='drop') %>%
  arrange(-n) %>%
  mutate(p=n/sum(n))
# The most common age group for hh of size 5 in the UK is 30-39{2} + 0-9{3} (6.5%) and 40-49{2} + 10-19{3} (5.9%)

filter(UK, hh_size==6) %>%
  pivot_longer(starts_with("a"), names_to="age") %>%
  filter(value>0) %>%
  uncount(weights=value) %>%
  group_by(hh_size, hid) %>%
  mutate(pid=row_number()) %>%
  pivot_wider(id_cols=c(hid, hh_size), names_from=pid, names_prefix="age_", values_from=age) %>%
  group_by(age_1, age_2, age_3, age_4, age_5, age_6) %>%
  summarise(n=n(), .groups='drop') %>%
  arrange(-n) %>%
  mutate(p=n/sum(n))
# The most common age groups for hh of size 6 in the UK are 30-39{2} + 0-9{2} + 10-19{2} (4.2%) and 40-49{2} + 10-19{3} (5.9%)
