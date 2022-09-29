# packages ----------------------------------------------------------------
lib <- c("tidyverse", "readxl", "writexl", "vegan","flextable", "webshot", 
         "ggfortify", "ggrepel", "devtools", "ggpubr", "grid", "knitr", "gginference", "scales")
lapply(lib, library, character.only = TRUE)

rm(lib)

# data import -------------------------------------------------------------

setwd(".../TFM")

sdata <- read_xlsx("./data/seed_data.xlsx", sheet = "seeds") #individual seed data
cdata1 <- read_xlsx("./data/seed_data.xlsx", sheet = "faeces") #sample date
family_species <- read_xlsx("./data/family_species.xlsx") #species traits info

# sample weight ---------------------------------------------------

#average sample weight

avg_weight <- cdata1 %>% 
  filter(locality %in% c("El_Rocio", "Hato_Blanco")) %>% 
  filter(!str_detect(collection_date, pattern = "2022")) %>%
  mutate(average_weight = mean(weight)) %>% 
  group_by(locality) %>% 
  mutate(locality_average_weight = mean(weight)) %>% 
  select(c("locality", "average_weight", "locality_average_weight")) %>% 
  unique()

#sample weight standard deviation

sd_weight <- cdata1 %>% 
  filter(locality %in% c("El_Rocio", "Hato_Blanco")) %>% 
  filter(!str_detect(collection_date, pattern = "2022")) %>%
  mutate(sd_weight = sd(weight)) %>% 
  group_by(locality) %>% 
  mutate(locality_sd_weight = sd(weight)) %>% 
  select(c("locality", "sd_weight", "locality_sd_weight")) %>% 
  unique()

#minimum sample weight

min_weight <- cdata1 %>% 
  filter(locality %in% c("El_Rocio", "Hato_Blanco")) %>% 
  filter(!str_detect(collection_date, pattern = "2022")) %>%
  group_by(locality) %>% 
  slice_min(weight)

#maximum sample weight

max_weight <- cdata1 %>% 
  filter(locality %in% c("El_Rocio", "Hato_Blanco")) %>% 
  filter(!str_detect(collection_date, pattern = "2022")) %>%
  group_by(locality) %>% 
  slice_max(weight)

# storage time ------------------------------------------------------------

#median sample storage time

med_st <- cdata1 %>% 
  filter(str_detect(collection_date, pattern = "2021")) %>% 
  mutate(storage_time = extraction_date - collection_date) %>% 
  group_by(locality) %>% 
  summarise(median_storage_time = median(as.double(storage_time), na.rm = TRUE))

#minimum sample storage time

min_st <- cdata1 %>% 
  filter(str_detect(collection_date, pattern = "2021")) %>% 
  mutate(storage_time = extraction_date - collection_date) %>% 
  group_by(locality) %>% 
  slice_min(storage_time)

#maximum sample storage time

max_st <- cdata1 %>% 
  filter(str_detect(collection_date, pattern = "2021")) %>% 
  mutate(storage_time = extraction_date - collection_date) %>% 
  group_by(locality) %>% 
  slice_max(storage_time)

# counts ------------------------------------------------------------------

#number of seeds in November samples

n_seeds_ERHB <- sdata %>% 
  filter(str_detect(collection_date, pattern = "2021")) %>% 
  filter(species != "not_a_seed") %>%
  count()

#number of seeds in the February sample

n_seeds_EC <- sdata %>% 
  filter(locality == "El_Cornejo") %>% 
  filter(species != "not_a_seed") %>%
  count()

#number of species in November samples

sp_EC <- sdata %>% 
  filter(locality == "El_Cornejo") %>% 
  filter(species != "not_a_seed") %>%
  group_by(species, extraction_date) %>% 
  count()

#median number of seeds per sample in November samples

median_nseeds <- cdata1 %>% 
  filter(locality %in% c("El_Rocio", "Hato_Blanco")) %>% 
  filter(!str_detect(collection_date, pattern = "2022")) %>%
  mutate(median_nseeds = median(n_seeds)) %>% 
  group_by(locality) %>% 
  mutate(locality_median_nseeds = median(n_seeds)) %>% 
  select(c("locality", "median_nseeds", "locality_median_nseeds")) %>% 
  unique()

#quartile deviation of the number of seeds in the November samples

Q_nseeds <- cdata1 %>% 
  filter(locality %in% c("El_Rocio", "Hato_Blanco")) %>% 
  filter(!str_detect(collection_date, pattern = "2022")) %>% 
  mutate(Q1 = quantile(n_seeds)[2],
         Q3 = quantile(n_seeds)[4],
         Q = (Q3 - Q1)/2) %>% 
  group_by(locality) %>% 
  mutate(Q1_l = quantile(n_seeds)[2],
         Q3_l = quantile(n_seeds)[4],
         Q_l = (Q3_l - Q1_l)/2) %>% 
  select(c("locality", "Q", "Q_l")) %>% 
  unique()

#minimum number of seeds per sample in November samples

min_nseeds <- cdata1 %>% 
  filter(locality %in% c("El_Rocio", "Hato_Blanco")) %>% 
  filter(!str_detect(collection_date, pattern = "2022")) %>%
  group_by(locality) %>% 
  slice_min(n_seeds)

#maximum number of seeds per sample in November samples

max_nseeds <- cdata1 %>% 
  filter(locality %in% c("El_Rocio", "Hato_Blanco")) %>% 
  filter(!str_detect(collection_date, pattern = "2022")) %>%
  group_by(locality) %>% 
  slice_max(n_seeds)

#number of samples from November

samples <- cdata1 %>% 
  filter(locality %in% c("El_Rocio", "Hato_Blanco")) %>% 
  filter(!str_detect(collection_date, pattern = "2022")) %>%
  count()

#November samples with seeds

  #number

samples_with_seeds_T <- cdata1 %>% 
  filter(locality %in% c("El_Rocio", "Hato_Blanco")) %>% 
  filter(!str_detect(collection_date, pattern = "2022")) %>%
  filter(n_seeds>0) %>% 
  count()
 
  #proportion

n_samples_seeds_T <- samples_with_seeds_T/samples  

#number of samples from November per locality

s_p_l <- cdata1 %>% 
  filter(locality %in% c("El_Rocio", "Hato_Blanco")) %>% 
  filter(!str_detect(collection_date, pattern = "2022")) %>%
  group_by(locality) %>% 
  count()

#November samples with seeds per locality

  #number

samples_with_seeds_l <- cdata1 %>% 
  filter(locality %in% c("El_Rocio", "Hato_Blanco")) %>% 
  filter(!str_detect(collection_date, pattern = "2022")) %>%
  group_by(locality) %>% 
  filter(n_seeds>0) %>% 
  count()

  #proportion

n_samples_seeds_l <- samples_with_seeds_l %>% 
  left_join(s_p_l, by = "locality") %>% 
  group_by(locality) %>% 
  summarise(samples_with_seeds = n.x/n.y)