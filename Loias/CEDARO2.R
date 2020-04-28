
library(tidyverse)
library(readstata13)

loiasis <- read.dta13("C:/Users/EDDY/Desktop/newworkcermel/CEDARO_analysis2/Loiasis_dataset.dta")

n_partic <- 
            loiasis  %>%
            mutate(np = str_detect(id,"id")) %>%
            count(np)  %>%
            mutate(variable = "Participant",
                      res = as.character(n)) %>% 
  select(variable, res)

med_age <- 
          loiasis %>% 
          summarise(med = median(age),
            q1 = quantile(age, 0.25),
            q2 = quantile(age, 0.75),
            res = paste0(med, " (", q1, " - ", q2, ")")) %>% 
          mutate(variable = "Median age (range)") %>% 
          select(variable, res)

totsex <- 
         loiasis  %>%
         count(sex) %>% 
         mutate(perc = round( n * 100 / sum(n), 1),
         res = paste0(n, " (", perc, "%)"),
         variable = sex)  %>%  
         select(variable, res)

loisdef <- 
  loiasis  %>%
  count(eyewormmf) %>% 
  mutate(perc = round( n * 100 / sum(n), 1),
         res = paste0(n, " (", perc, "%)"),
         variable = case_when(eyewormmf == "No" ~ "Loiasis (No)",
                              eyewormmf == "Yes" ~ "Loiasis (Yes)")) %>% 
  select(variable, res)

pos_raplo <- 
          loiasis  %>%
          count(mf,eyeworm) %>% 
          slice(2:4) %>% 
          mutate(perc = round( n * 100 / sum(n), 1),
          res = paste0(n, " (", perc, "%)"),
          variable = case_when(eyeworm == "Yes" ~ "RAPLOA +")) %>% 
          select(variable, res) %>%
          slice(1)

pruri <- 
        loiasis  %>%
  group_by(itch) %>%
  summarise(nb = n()) %>%
  mutate(perc = round(nb*100 / sum(nb),1),
         percent = paste0(nb, " (", perc, "%)")) %>%
  select(itch, n_percent = percent)

twice <- 
       loiasis  %>%
        group_by(anyexposure) %>%
        summarise(nb = n()) %>%
        mutate(perc = round(nb*100 / sum(nb),1),
           percent = paste0(nb, " (", perc, "%)")) %>%
         select(anyexposure, n_percent = percent)

calabsw_recr <- 
         loiasis %>% 
         mutate(calabsw = str_detect(edema, "edema")) %>% 
         count(calabsw) %>%
         mutate(variable = "Calabar swelling recruted",
                 res = as.character(n)) %>% 
         select(variable, res) %>%
         slice(1)


medage_calab <- 
          loiasis %>% 
          filter(edema %in% c("No","Yes")) %>% 
          summarise(med = median(age),
            q3 = quantile(age, 0.25),
            q4 = quantile(age, 0.75),
            res = paste0(med, " (", q3, " - ", q4, ")")) %>% 
  mutate(variable = "Median age (range)") %>% 
  select(variable, res)

calab_lois <-
   loiasis %>% 
   count(edema,eyewormmf) %>%
      slice(1:4)
  

tr_age <-
        loiasis %>%
        select(age) %>%
        mutate(age_group = case_when(
                           age < 6 ~ '< 6',
                           age >= 6 & age <= 17  ~ '6-17',
                           age >= 18  & age <= 34  ~ '18-34',
                           age >= 35 & age <= 49  ~ '35-49',
                           age >= 50 & age <= 64  ~ '50-64',
                           age >= 65 ~ '65+',
                           TRUE ~ 'other')) %>%
  count(age_group)

expo <-  loiasis  %>%
  group_by(exposureforest) %>%
  summarise(nb = n()) %>%
  mutate(perc = round(nb*100 / sum(nb),1),
         percent = paste0(nb, " (", perc, "%)")) %>%
  select(exposureforest, n_percent = percent)

raplodef <- loiasis  %>%
  select(eyeworm) %>%
  group_by(eyeworm) %>%
  summarise(nb = n()) %>%
  mutate(perc = round(nb*100 / sum(nb),1),
         percent = paste0(nb, " (", perc, "%)")) %>%
  select(eyeworm, n_percent = percent) %>%
  slice(2)

microf <- loiasis  %>%
  select(mf) %>%
  group_by(mf) %>%
  summarise(nb = n()) %>%
  mutate(perc = round(nb*100 / sum(nb),1),
         percent = paste0(nb, " (", perc, "%)")) %>%
  select(mf, n_percent = percent) %>%
  slice(2)

raplo_microf <- loiasis  %>%
  filter(mf == "Yes" |eyeworm == "Yes") %>%
  group_by(eyeworm) %>%
  count(eyeworm) %>%
  mutate(perc = round(sum(n)*100/947, 1),
         percent = paste0(n, " (", perc, "%)")) %>%
  select(eyeworm, n_percent = percent)
  #slice(2)

#select(mf, eyeworm) | %>%
#