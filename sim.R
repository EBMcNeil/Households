#
# R file to import Hong Kong census data ("hk census_household size and age_Ddate20210812.xlsx".
# Uses data in sheet D108e.
# Generates the distributional proportions for various parameters:
# Namely, 
# household size (HH_size), 
# household type by size (HH_type), 
# age of household head (HH_head), 
# age of household member, 
# number of children (HH_child) by type and size of household,
# number of elderly (HH_elder) by type and size of household.
#

# Load required R packages
library(readxl) 
library(tidyr) 
library(dplyr) 
library(readr) 
library(ggplot2) 
library(forcats) 

rm(list=ls()) 
setwd("F:/Edward/Simulation") 


# Import the age distribution of Hong Kong (from 2016 By-Census) by HH size and HH type 
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

# Import the age distribution of Hong Kong by age group
HH_age <- read_excel("hk census_household size and age_Ddate20210812.xlsx",  
                     sheet="D108e",  
                     range="AD123:AD140",  
                     col_names = 'n') %>% 
  mutate(age=rep(1:9, each=2)) %>% 
  group_by(age) %>% 
  summarise(n=sum(n), .groups='drop') %>% 
  mutate(p=n/sum(n)) 
HH_age 


# 1. Household sizes (sheet D103ae)
HH_size <- read_excel("hk census_household size and age_Ddate20210812.xlsx",  
                      sheet="D103ae",  
                      range="F6:F11",  
                      col_names = 'n') %>% 
  mutate(hh_size=1:6, p=n/sum(n)) %>% 
  select(hh_size, p) 

HH_size 


# 2. Age range of household head (sheet D121e)
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
  mutate(p=if_else(n>0, n/sum(n), 0))  


HH_type 

# hh_types (according to census)
# 1=couple, 2=couple+child, 3=parent+child, 4=couple+parent,  
# 5=couple+parent+child, 6=other, 7=single, 8=complex 


# Visualize the distribution of household types and size by age group
ggplot(HH_type, aes(x=hh_size, y=p, fill=factor(hh_type))) + 
  facet_wrap(~Age, labeller='label_both') + 
  geom_col() + 
  scale_x_continuous(breaks=1:6) + 
  labs(fill="Household type") 


# Proportion of household types in Hong Kong
HH_types <- group_by(HH_type, hh_type) %>% 
  summarise(n=sum(n), .groups='drop') %>% 
  mutate(p=n/sum(n)) 

HH_types # type 2 (couple+child) is the most common in Hong Kong 


# 4. Age of partner, if household type = {2, 4, 5} 
# AFAIK, the census does not provide this data, so we assume that the difference in age between the household head and partner is normally distributed with a mean of 0 and sd 2 yrs


# 5. Age of child(ren) 
# Use census data to determine the distribution of ages for each combination of hh type and size
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
  group_by(hh_type, hh_size) %>% 
  # probabilities in each hh_type/hh_size must add to 1 
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



# Load the algorithm to simulate households
source("simulate.R") 

# Run based on above probabilities generated by the census (takes around 8-10 minutes for n=10,000)
# HK_HH <- simulate(n=10000, HH_size, HH_type, HH_head, HH_child, HH_elder, HH_age, age.threshold=0.005) 


# Save to a file
# saveRDS(HK_HH, file="Simulate_2022.Rds")


# Load a previously saved simulation result #### 
HK_HH <- readRDS("Simulate_2022.Rds") 


# We can check marginal proportions and compare simulation results with the actual population 
# Household sizes (population proportions are in HH_size$p) 
p_size <- tabulate(rowSums(HK_HH$hh)) / sum(tabulate(rowSums(HK_HH$hh))) 
tibble(HH_size, p_size, diff=round(p_size-HH_size$p, 4)) 


# Sum of squared residuals
sum((HH_size$p - p_size)^2) 


# Household member ages (population proportions are in HH_age$p) 
p_age <- colSums(HK_HH$hh) / sum(colSums(HK_HH$hh)) 
tibble(HH_age, p_age, diff=round(p_age-HH_age$p, 3)) 


sum((HH_age$p - p_age)^2) 


# Household types 
p_type <- tabulate(unlist(sapply(HK_HH$type, unlist))) 
p_type <- p_type/sum(p_type) 
tibble(HH_types, p_type, diff=round(p_type-HH_types$p, 3)) 


sum((HH_types$p - p_type)^2) 


# Assess most common age groups in each household of sizes 1-6 
# Households of size 1 
hh_1 <- colSums(HK_HH$hh[rowSums(HK_HH$hh)==1,]) 
hh_1_p <- hh_1/sum(hh_1) 

filter(HK, hh_size==1) %>% 
  group_by(age) %>% 
  summarise(n=sum(n), .groups = 'drop') %>% 
  mutate(p_pop=n/sum(n), p_sample=hh_1_p, diff=p_sample-p_pop) %>% print()  %>% 
  summarise(SS=sum(diff^2)) 





# Households of size 2 
hh_2 <- colSums(HK_HH$hh[rowSums(HK_HH$hh)==2,]) 
hh_2_p <- hh_2/sum(hh_2) 

filter(HK, hh_size==2) %>% 
  group_by(age) %>% 
  summarise(n=sum(n), .groups = 'drop') %>% 
  mutate(p_pop=n/sum(n), p_sample=hh_2_p, diff=p_sample-p_pop) %>% print()  %>% 
  summarise(SS=sum(diff^2)) 



# Households of size 3 
hh_3 <- colSums(HK_HH$hh[rowSums(HK_HH$hh)==3,]) 
hh_3_p <- hh_3/sum(hh_3) 

filter(HK, hh_size==3) %>% 
  group_by(age) %>% 
  summarise(n=sum(n), .groups = 'drop') %>% 
  mutate(p_pop=n/sum(n), p_sample=hh_3_p, diff=p_sample-p_pop) %>% print()  %>% 
  summarise(SS=sum(diff^2)) 




# Households of size 4 
hh_4 <- colSums(HK_HH$hh[rowSums(HK_HH$hh)==4,]) 
hh_4_p <- hh_3/sum(hh_4) 

filter(HK, hh_size==4) %>% 
  group_by(age) %>% 
  summarise(n=sum(n), .groups = 'drop') %>% 
  mutate(p_pop=n/sum(n), p_sample=hh_4_p, diff=p_sample-p_pop) %>% print()  %>% 
  summarise(SS=sum(diff^2)) 




# Households of size 5 
hh_5 <- colSums(HK_HH$hh[rowSums(HK_HH$hh)==5,]) 
hh_5_p <- hh_5/sum(hh_5) 

filter(HK, hh_size==5) %>% 
  group_by(age) %>% 
  summarise(n=sum(n), .groups = 'drop') %>% 
  mutate(p_pop=n/sum(n), p_sample=hh_5_p, diff=p_sample-p_pop) %>% print()  %>% 
  summarise(SS=sum(diff^2)) 





# Households of size 6 
hh_6 <- colSums(HK_HH$hh[rowSums(HK_HH$hh)==6,]) 
hh_6_p <- hh_6/sum(hh_6) 

filter(HK, hh_size==6) %>% 
  group_by(age) %>% 
  summarise(n=sum(n), .groups = 'drop') %>% 
  mutate(p_pop=n/sum(n), p_sample=hh_6_p, diff=p_sample-p_pop) %>% print()  %>% 
  summarise(SS=sum(diff^2)) 




# Age mixing patterns #### 
Sim <- tibble(as.data.frame(HK_HH$hh)) 
head(Sim) 



hh_hk <- mutate(Sim,  
                hh_size=rowSums(across(everything())), 
                hid=1:nrow(Sim)) %>% 
  pivot_longer(-c(hid, hh_size), names_to="age") %>% 
  filter(value>0) %>% 
  uncount(weights=value) %>% 
  group_by(hh_size, hid) %>% 
  mutate(pid=row_number()) %>% 
  pivot_wider(id_cols=c(hid, hh_size), names_from=pid, names_prefix="age_", values_from=age)  
hh_hk 



# Households of size 2 
filter(hh_hk, hh_size==2) %>% 
  group_by(age_1, age_2) %>% 
  summarise(n=n(), .groups='drop') %>% 
  mutate(p=n/sum(n)) %>% 
  arrange(-n)  



# Households of size 3
filter(hh_hk, hh_size==3) %>% 
  group_by(age_1, age_2, age_3) %>% 
  summarise(n=n(), .groups='drop') %>% 
  arrange(-n) %>% 
  mutate(p=n/sum(n)) 



# Households of size 4
filter(hh_hk, hh_size==4) %>% 
  group_by(age_1, age_2, age_3, age_4) %>% 
  summarise(n=n(), .groups='drop') %>% 
  arrange(-n) %>% 
  mutate(p=n/sum(n)) 




# Households of size 5
filter(hh_hk, hh_size==5) %>% 
  group_by(age_1, age_2, age_3, age_4, age_5) %>% 
  summarise(n=n(), .groups='drop') %>% 
  arrange(-n) %>% 
  mutate(p=n/sum(n)) # %>% print(n=Inf) 



# Households of size 6
filter(hh_hk, hh_size==6) %>% 
  group_by(age_1, age_2, age_3, age_4, age_5, age_6) %>% 
  summarise(n=n(), .groups='drop') %>% 
  arrange(-n) %>% 
  mutate(p=n/sum(n)) #%>% print(n=Inf) 



# Compare with the UK data

# Read in the UK data 
UK <- read_excel("Simulate 10000 households with HK demographic data_ABM_2.xlsx", 
                sheet="baseline_household_demographic", 
                 range="B1:K10001") 

UK <- mutate(UK, hid=1:10000); UK 

filter(UK, hh_size==1) %>% 
  pivot_longer(starts_with("a"), names_to="age") %>% 
  group_by(age) %>% 
  summarise(n=sum(value), .groups='drop') %>% 
  mutate(p=n/sum(n)) 

# The most common age group for hh of size 1 in the UK is 60-69 (17.0%) 


