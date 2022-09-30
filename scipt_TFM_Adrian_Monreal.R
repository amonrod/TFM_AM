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
# community database ------------------------------------------------------

cdata2 <- cdata1 %>% 
  mutate(storage_time = extraction_date - collection_date) %>% 
  mutate(period = collection_date) %>% 
  mutate(period = as.character(period)) %>% 
  mutate(period = gsub(pattern = "2021-11-25", replacement = "NOV", period)) %>% 
  mutate(period = gsub(pattern = "2022-02-08", replacement = "FEB", period)) %>% 
  mutate(period = gsub(pattern = "2022-02-10", replacement = "FEB", period)) %>% 
  mutate(loc_p_s = paste0(locality, "_", period, "_", sample)) %>% 
  arrange(locality, sample)

#richness per sample

r_s <- sdata %>% 
  filter(species != "not_a_seed") %>% 
  filter(species != "missing") %>% 
  mutate(period = collection_date) %>% 
  mutate(period = as.character(period)) %>% 
  mutate(period = gsub(pattern = "2021-11-25", replacement = "NOV", period)) %>% 
  mutate(period = gsub(pattern = "2022-02-08", replacement = "FEB", period)) %>% 
  mutate(period = gsub(pattern = "2022-02-10", replacement = "FEB", period)) %>% 
  mutate(loc_p_s = paste0(locality, "_", period, "_", sample)) %>% 
  dplyr::select(c(loc_p_s, species)) %>% 
  unique(.) %>% 
  count(loc_p_s) %>% 
  rename("richness" = "n")

#species abundance per sample

sp_a <- sdata %>% 
  mutate(period = collection_date) %>% 
  mutate(period = as.character(period)) %>% 
  mutate(period = gsub(pattern = "2021-11-25", replacement = "NOV", period)) %>% 
  mutate(period = gsub(pattern = "2022-02-08", replacement = "FEB", period)) %>% 
  mutate(period = gsub(pattern = "2022-02-10", replacement = "FEB", period)) %>% 
  group_by(locality, sample, period, species) %>% 
  count(.) %>% 
  mutate(loc_p_s = paste0(locality, "_", period, "_", sample)) %>% 
  dplyr::select(c(loc_p_s, n)) %>% 
  pivot_wider(., names_from = "species", values_from = n) %>%
  replace(is.na(.), 0) 

cdata <- cdata2 %>% 
  merge(., sp_a, by = c("loc_p_s", "sample", "period", "locality"), all.x = T) %>%
  merge(., r_s, by = "loc_p_s", all.x = T) %>%
  dplyr::select(-c(loc_p_s, collection_date, extraction_date, observations)) %>%
  dplyr::select(-c(not_a_seed, missing)) %>%
  replace(is.na(.), 0)

# table 1 -----------------------------------------------------------------

spinf <- read_xlsx("./data/species_info.xlsx")

Table_1 <- spinf %>% 
  dplyr::select(-c("dormancy", "reported_Donana", "synonims", "status", "complete_name")) %>% 
  rename(Family = family) %>% 
  rename(Species = species) %>% 
  rename(`Dispersal syndrome` = dispersal_syndrome) %>% 
  rename(`Ellenberg F` = ellenberg_moisture_value) %>% 
  rename(Weed = crop_weed) %>% 
  rename(`Herbicide resistance` = herbicide_resistance) %>% 
  rename(`Previously reported in geese diet` = previously_reported) %>% 
  rename(`Length (mm)` = length) %>% 
  rename(`Flowering period` = flowering) %>% 
  mutate(Species = gsub(pattern = "_", replacement = " ", Species),
         Species = gsub(pattern = "sp", replacement = "sp.", Species),
         Species = gsub(pattern = "Elatine campylosp.erma", replacement = "Elatine campylosperma", Species),
         `Length (mm)` = round(`Length (mm)`, digits = 2)) %>%
  arrange(Family, Species) %>% 
  flextable() %>% 
  align(., align = "center", part = "all") %>% 
  bold(., bold = T, part = "header") %>% 
  italic(., j = "Species", italic = T, part = "body") %>% 
  italic(i = c(1, 3, 4),j = "Species", italic = F, part = "body" ) %>%
  compose(i =  4, j = "Species", value = as_paragraph(as_i("Carex"), " sp.")) %>% 
  compose(i = 1, j = "Species", value = as_paragraph(as_i("Eryngium"), " sp."))

Table_1

save_as_image(Table_1, "./fig/Table_1.png")

# table 2 -----------------------------------------------------------------

#number of seeds of each species per locality

n_s_l1 <- sdata %>% 
  filter(locality != "El_Cornejo") %>% 
  group_by(locality, species) %>% 
  count() %>% 
  filter(species != "not_a_seed") %>% 
  pivot_wider(names_from = locality, values_from = n) %>% 
  replace(is.na(.), 0)

#number of samples with each species

samples_with_each_sp <- sdata %>% 
  filter(locality != "El_Cornejo") %>% 
  mutate(period = collection_date) %>% 
  mutate(period = as.character(period)) %>% 
  mutate(period = gsub(pattern = "2021-11-25", replacement = "NOV", period)) %>% 
  mutate(period = gsub(pattern = "2022-02-08", replacement = "FEB", period)) %>% 
  mutate(period = gsub(pattern = "2022-02-10", replacement = "FEB", period)) %>% 
  mutate(loc_p = paste0(locality, "_", period)) %>% 
  mutate(loc_p_s = paste0(locality, "_", period, "_", sample)) %>% 
  dplyr::select(species, loc_p, loc_p_s) %>% 
  unique() %>% 
  group_by(species, loc_p) %>% 
  count() %>% 
  filter(species != "not_a_seed") %>% 
  pivot_wider(names_from = loc_p, values_from = n) %>% 
  replace(is.na(.), 0)

#maximum number of seeds of each species per sample

max_seed_sample <- sdata %>% 
  filter(locality != "El_Cornejo") %>% 
  mutate(period = collection_date) %>% 
  mutate(period = as.character(period)) %>% 
  mutate(period = gsub(pattern = "2021-11-25", replacement = "NOV", period)) %>% 
  mutate(period = gsub(pattern = "2022-02-08", replacement = "FEB", period)) %>% 
  mutate(period = gsub(pattern = "2022-02-10", replacement = "FEB", period)) %>% 
  mutate(loc_p = paste0(locality, "_", period)) %>% 
  mutate(loc_p_s = paste0(locality, "_", period, "_", sample)) %>%
  dplyr::select(species, loc_p, loc_p_s) %>%
  group_by(species, loc_p, loc_p_s) %>% 
  count() %>% 
  group_by(species, loc_p) %>% 
  slice_max(order_by = n) %>% 
  dplyr::select(-loc_p_s) %>% 
  unique() %>% 
  filter(species != "not_a_seed") %>% 
  pivot_wider(names_from = loc_p, values_from = n) %>% 
  replace(is.na(.), 0)

#table 2

Table_2.1 <- max_seed_sample %>% 
  left_join(samples_with_each_sp, by = "species") %>% 
  left_join(family_species, ., by = "species") %>%
  left_join(n_s_l1, by = "species") %>% 
  rename(Species = species) %>%
  rename(Family = family) %>% 
  rename(`Max/Sample` = El_Rocio_NOV.x) %>% 
  rename(`Max/Sample ` = Hato_Blanco_NOV.x) %>% 
  rename(`N.S.T.` = El_Rocio_NOV.y) %>% 
  rename(`N.S.T. ` = Hato_Blanco_NOV.y) %>% 
  rename(N = El_Rocio) %>% 
  rename(`N ` = Hato_Blanco) %>% 
  relocate(c("Max/Sample ", "N.S.T. "), .after = "N.S.T.") %>% 
  relocate("N", .after = Species) %>% 
  relocate("N ", .after = "N.S.T.") %>%
  mutate(Species = gsub(pattern = "_", replacement = " ", Species),
         Species = gsub(pattern = "sp", replacement = "sp.", Species),
         Species = gsub(pattern = "Elatine campylosp.erma", replacement = "Elatine campylosperma", Species),
         Species = gsub(pattern = "missing", replacement = "Unidentified", Species)) %>%
  group_by(Species) %>% 
  mutate("Total" = sum(N, `N `),
         Family = gsub(pattern = "-", replacement = "zzz", Family)) %>% 
  arrange(Family, Species) %>% 
  mutate(Family = gsub(pattern = "zzz", replacement = " ", Family))  

Table_2 <- Table_2.1 %>% 
  flextable() %>% 
  add_header_row(
    x = ., top = F, values = c(" ", "El Rocío", "Hato Blanco", " "),
    colwidths = c(2, 3, 3, 1)) %>% 
  add_footer(x = ., Family = "Total", 
             `N` = sum(Table_1.1$`N`),
             `N ` = sum(Table_1.1$`N `),
             `Total` = sum(Table_1.1$`Total`)) %>% 
  align(., align = "center", part = "all") %>% 
  italic(., j = "Species", italic = T, part = "body") %>% 
  italic(i = c(1, 3, 4, 18),j = "Species", italic = F, part = "body" ) %>% 
  bold(., bold = T, part = "header") %>% 
  bold(., bold = T, part = "footer") %>% 
  vline(., j = c(2, 5, 8), part = "all") %>% 
  compose(i =  4, j = "Species", value = as_paragraph(as_i("Carex"), " sp.")) %>% 
  compose(i = 1, j = "Species", value = as_paragraph(as_i("Eryngium"), " sp."))

Table_2

save_as_image(Table_2, "./fig/Table_2.png")

# figure 4 ---------------------------------------------------------

abu1 <- sdata %>% 
  filter(str_detect(collection_date, pattern = "2021")) %>%
  filter(species != "not_a_seed") %>% 
  mutate(period = collection_date) %>% 
  mutate(period = as.character(period)) %>% 
  mutate(period = gsub(pattern = "2021-11-25", replacement = "NOV", period)) %>%
  mutate(loc_p_s = paste0(locality, "_", period, "_", sample)) %>%
  mutate(locality = gsub(pattern = "_", replacement = " ", locality),
         locality = gsub(pattern = "Rocio", replacement = "Rocío", locality)) %>% 
  group_by(locality, species) %>% 
  count() %>% 
  mutate(species = gsub(pattern = "_", replacement = " ", species),
         species = gsub(pattern = "sp", replacement = "sp.", species),
         code = n,
         code = replace(code, n < 100 & locality == "El Rocío", 0),
         code = replace(code, n > 700, 1000),
         species = gsub(pattern = "missing", replacement = "Unidentified", species)) %>% 
  group_by(code) %>% 
  mutate(n = sum(n),
         species = replace(species, code == 0, "Others")) %>% 
  ungroup() %>% 
  unique() %>% 
  dplyr::select(-code)

ggplot(abu1, aes(x = locality, y = n, fill = species)) +
  geom_bar(position = "fill", stat = "identity") +
  labs(x = "Locality",
       y = "Proportion of seeds") +  
  theme_classic() +
  guides(fill = guide_legend(title="Species")) +
  scale_fill_discrete(breaks = c("Bolboschoenus maritimus",
                                 "Sporobolus aculeatus",
                                 "Juncus bufonius",
                                 "Schoenoplectus litoralis",
                                 "Others"),
                      labels = c(expression(italic("Bolboschoenus maritimus")), 
                                 expression(italic("Sporobolus aculeatus")),
                                 expression(italic("Juncus bufonius")),
                                 expression(italic("Schoenoplectus litoralis")),
                                 "Others")) +                               
  theme(text = element_text(size = 15),
        legend.text.align = 0) 

ggsave(
  filename = "./fig/stacked_barplot.png",
  plot = last_plot(),
  dpi = 300,
  width = 20,
  height = 20,
  units = "cm"
)
# figure 5 -----------------------------------------------------------------

#abundance

abu <- cdata1 %>% 
  filter(str_detect(collection_date, pattern = "2021")) %>% 
  mutate(locality = gsub(pattern = "_", replacement = " ", locality),
         locality = gsub(pattern = "Rocio", replacement = "Rocío", locality))

g_abu <- ggplot(abu, aes(x = locality, y = n_seeds, fill = locality)) +
  geom_boxplot(show.legend = F) +
  labs(x = "Locality",
       y = "Seed abundance per sample") +
  scale_y_continuous(breaks = seq(0, 150, by = 25)) +
  theme_classic() +
  theme(text = element_text(size = 15)) 

g_abu

#richness

ric <- cdata %>% 
  filter(locality %in% c("El_Rocio", "Hato_Blanco")) %>% 
  mutate(locality = gsub(pattern = "_", replacement = " ", locality),
         locality = gsub(pattern = "Rocio", replacement = "Rocío", locality))

g_ric <- ggplot(ric, aes(x = locality, y = richness, fill = locality)) +
  geom_boxplot(show.legend = F) +
  labs(x = "Locality",
       y = "Richness per sample") +
  theme_classic() +
  scale_y_continuous(breaks = seq(0, 6, by = 1)) +
  theme(text = element_text(size = 15),
        legend.position = "none")

g_ric

#both

g_abu_ric <- ggarrange(g_abu + rremove("xlab"), g_ric + rremove("xlab"), 
                       labels = c("A", "B"),
                       ncol = 2, nrow = 1)

annotate_figure(g_abu_ric, bottom = textGrob("Locality", gp = gpar(cex = 1.3)))

ggsave(
  filename = "./fig/boxplot.png",
  plot = last_plot(),
  dpi = 300,
  width = 20,
  height = 20,
  units = "cm"
)

# figure 6 ----------------------------------------------------------------

library(vegan)

par(mfrow = c( 1, 2))

cdata_c_ER <- cdata %>% 
  dplyr::select(-c("period", "n_seeds", "weight", "storage_time")) %>% 
  filter(., locality == "El_Rocio") %>% 
  dplyr::select(-c("sample", "locality"))

curve1 <- specaccum(cdata_c_ER, method = "exact")

plot(curve1)

cdata_c_HB <- cdata %>% 
  dplyr::select(-c("period", "n_seeds", "weight", "storage_time")) %>% 
  filter(., locality == "Hato_Blanco") %>% 
  dplyr::select(-c("sample", "locality"))

curve2 <- specaccum(cdata_c_HB, method = "exact")

plot(curve2)

par(mfrow = c(1,1))

df_ER <- tibble(
  code = "El Rocío",
  sites = curve1$sites,
  richness = curve1$richness,
  sd = curve1$sd)

df_HB <- tibble(
  code = "Hato Blanco",
  sites = curve2$sites,
  richness = curve2$richness,
  sd = curve2$sd)

df_rar <- rbind(df_ER, df_HB)

ggplot(data = df_rar, aes(sites, richness, group = code, color = code, fill = code)) +
  geom_line()+
  geom_ribbon(aes(ymin = richness - sd, ymax = richness + sd), alpha = 0.5) +
  labs(x = "Number of samples",
       y = "Observed richness",
       fill = "Locality",
       color = "Locality") +
  theme_classic() +
  theme(text = element_text(size = 15))

ggsave(
  filename = "./fig/specaccum.png",
  plot = last_plot(),
  dpi = 300,
  width = 25,
  height = 20,
  units = "cm"
)

# glm (table 3) ---------------------------------------------------------------------

lib <- c("MASS", "DHARMa", "AICcmodavg")

lapply(lib, library, character.only = TRUE)

rm(lib)

#Negative binomial abundance

glm_data <- filter(cdata, locality != "El_Cornejo")

glm_data_ric <- filter(glm_data, n_seeds > 0)

glm1_nb_2 <- glm.nb(n_seeds ~ locality + weight, link = log, data = glm_data)

simulationOutput_glm1_nb_2<-simulateResiduals(fittedModel=glm1_nb_2)

plot(simulationOutput_glm1_nb_2)

par(mfcol=c(1,3))

hist(resid(glm1_nb_2)) 

qqnorm(residuals(glm1_nb_2))

qqline(residuals(glm1_nb_2))

anova(glm1_nb_2)

summary(glm1_nb_2)

AICc(glm1_nb_2)

#table glm abundance

summary_coefs_glm1 <- as.data.frame(coef(summary(glm1_nb_2)))

summary_coefs_glm_abu <- summary_coefs_glm1 %>%  
  rownames_to_column(., var = "Level") %>% 
  mutate(Level = c("Intercept", "Hato Blanco", "Weight"),) %>% 
  rename(ß = Estimate,
          SE = `Std. Error`,
          p = `Pr(>|z|)`) %>% 
  mutate(ß = round(ß, digits = 3),
          SE = round(SE, digits = 3),
          `z value` = round(`z value`, digits = 3),
          p = round(p, digits = 3),
          p = replace(p, p < 0.001, "<0.001"))

table_glm_abu <- summary_coefs_glm_abu %>% 
  flextable(.) %>% 
  bold(., part = "header") %>% 
  align(., align = "center", part = "all") %>% 
  align(., align = "left", part = "header") %>% 
  add_header_lines(., "A.
                   Abundance")

table_glm_abu

save_as_image(table_glm_abu, "./fig/Table_abu.png")

#Poisson richness

glm1_po <- glm(richness ~ locality, data = glm_data_ric, family = "poisson")

simulationOutput_glm1_po<-simulateResiduals(fittedModel=glm1_po) 

plot(simulationOutput_glm1_po)

hist(resid(glm1_po)) 

qqnorm(residuals(glm1_po))

qqline(residuals(glm1_po))

anova(glm1_po)

summary(glm1_po)

AICc(glm1_po)

#table richness

summary_coefs_glm2 <- as.data.frame(coef(summary(glm1_po)))

summary_coefs_glm_ric <- summary_coefs_glm2 %>%  
  #rbind(., summary_coefs_glm2) %>% 
  rownames_to_column(., var = "Level") %>% 
  mutate(Level = c("Intercept", "Hato Blanco"),) %>% 
  rename(ß = Estimate,
          SE = `Std. Error`,
          p = `Pr(>|z|)`) %>% 
  mutate(ß = round(ß, digits = 3),
          SE = round(SE, digits = 3),
          `z value` = round(`z value`, digits = 3),
          p = round(p, digits = 3),
          p = replace(p, p < 0.001, "<0.001"))

table_glm_ric <- summary_coefs_glm_ric %>% 
  flextable(.) %>% 
  bold(., part = "header") %>% 
  align(., align = "center", part = "all") %>% 
  align(., align = "left", part = "header") %>% 
  add_header_lines(., "B.
                   Richness")

table_glm_ric

save_as_image(table_glm_ric, "./fig/Table_ric.png")

# germination analysis of November samples (table 4) ----------------------

cdata2 <- cdata1 %>% 
  mutate(storage_time = extraction_date - collection_date) %>% 
  mutate(period = collection_date) %>% 
  mutate(period = as.character(period)) %>% 
  mutate(period = gsub(pattern = "2021-11-25", replacement = "NOV", period)) %>% 
  mutate(period = gsub(pattern = "2022-02-08", replacement = "FEB", period)) %>% 
  mutate(period = gsub(pattern = "2022-02-10", replacement = "FEB", period)) %>% 
  mutate(loc_p_s = paste0(locality, "_", period, "_", sample)) %>% 
  arrange(locality, sample)

w_s <- cdata2[, c("loc_p_s", "weight")]

gdata1 <- sdata %>% 
  filter(species != "not_a_seed") %>% 
  mutate(storage_time = extraction_date - collection_date) %>%
  mutate(germination_time = germination_date - incubation_date) %>%
  mutate(period = collection_date) %>% 
  mutate(period = as.character(period)) %>% 
  mutate(period = gsub(pattern = "2021-11-25", replacement = "NOV", period)) %>% 
  mutate(period = gsub(pattern = "2022-02-08", replacement = "FEB", period)) %>% 
  mutate(period = gsub(pattern = "2022-02-10", replacement = "FEB", period)) %>%
  mutate(loc_p_s = paste0(locality, "_", period, "_", sample)) %>% 
  merge(., w_s, by = "loc_p_s", all.x = T) %>%
  dplyr::select(-c("collection_date", "extraction_date", 
                   "incubation_date", "germination_date", "observations"))
#germination rate

  #number of seeds per sample

n_s <- gdata1 %>% 
  count(.,loc_p_s, name = "N")

n_s_sp <- gdata1 %>% 
  count(.,loc_p_s, species, name = "N_sp")

w_s <- cdata2[, c("loc_p_s", "weight")]

g_rate <- gdata1 %>% 
  dplyr::select(., code, sample, germination, loc_p_s, species) %>% 
  group_by(loc_p_s) %>%
  mutate(n = sum(germination)) %>% 
  merge(., n_s, by = "loc_p_s") %>% 
  mutate(total_germination_rate = n/N) %>% 
  ungroup(.) %>% 
  group_by(loc_p_s, species) %>% 
  mutate(n_sp = sum(germination)) %>% 
  merge(., n_s_sp, by = c("loc_p_s", "species")) %>% 
  mutate(species_germination_rate = n_sp/N_sp)

germination_rate <- g_rate %>% 
  dplyr::select(., loc_p_s, species, species_germination_rate) %>% 
  unique(.)  

gdata <- gdata1 %>% 
  merge(., g_rate, by = c("code", "sample", "loc_p_s", "germination", "species")) %>% 
  dplyr::select(-c("n", "n_sp", "N", "N_sp","loc_p_s"))

n_s_l <- gdata1 %>% 
  filter(locality != "El_Cornejo") %>% 
  count(.,locality, species, name = "N_diaspores")

n_t <- gdata1 %>% 
  filter(locality != "El_Cornejo") %>% 
  count(., species, name = "Total_diaspores")

n_g <- gdata1 %>% 
  filter(locality != "El_Cornejo") %>% 
  dplyr::select(.,germination, species) %>% 
  group_by(species) %>%
  mutate(Total_germinated = sum(germination)) %>% 
  dplyr::select(-germination) %>% 
  unique()

avg_germination_time <- gdata %>% 
  filter(locality != "El_Cornejo") %>% 
  dplyr::select(., germination, species, germination_time) %>% 
  subset(., germination == 1) %>% 
  dplyr::select(-germination) %>% 
  group_by(species) %>% 
  summarise(average_germination_time = mean(as.double(germination_time), na.rm = TRUE))

med_germination_time <- gdata %>% 
  filter(locality != "El_Cornejo") %>% 
  dplyr::select(., germination, species, germination_time) %>% 
  subset(., germination == 1) %>% 
  dplyr::select(-germination) %>% 
  group_by(species) %>% 
  summarise(median_germination_time = median(as.double(germination_time), na.rm = TRUE))

table_germination <- gdata1 %>% 
  dplyr::select(.,germination, locality, species) %>% 
  filter(locality != "El_Cornejo") %>% 
  group_by(locality, species) %>%
  mutate(N_germinated = sum(germination)) %>% 
  left_join(family_species, ., by = "species") %>% 
  left_join(., n_s_l, by = c("locality", "species")) %>% 
  dplyr::select(-germination) %>% 
  unique() %>% 
  pivot_wider(., names_from = locality, values_from = c("N_germinated", "N_diaspores")) %>% 
  replace(is.na(.), 0) %>% 
  left_join(., n_t, by = "species") %>% 
  left_join(., n_g, by = "species") %>% 
  left_join(., med_germination_time, by = "species") %>% 
  mutate(germination_rate = Total_germinated/Total_diaspores) %>%
  filter(species != "not_a_seed") %>% 
  mutate(family = gsub(pattern = "-", replacement = "zzz", family),
         species = gsub(pattern = "missing", replacement = "Unidentified", species)) %>% 
  arrange(family, species) %>% 
  mutate(family = gsub(pattern = "zzz", replacement = " ", family)) %>% 
  rename("Family" = "family", "Species" = "species") %>% 
  mutate(germination_rate = round(germination_rate, digits = 3)) %>% 
  mutate(Species = gsub(pattern = "_", replacement = " ", Species),
         Species = gsub(pattern = "sp", replacement = "sp.", Species)) %>% 
  mutate(median_germination_time = as.character(median_germination_time)) %>% 
  mutate(median_germination_time = replace(median_germination_time, list = is.na(median_germination_time), values = "-")) %>%
  relocate(., c("Total_diaspores", "Total_germinated"), .after = "Species") %>% 
  relocate(., "N_diaspores_El_Rocio", .before = "N_germinated_El_Rocio") %>% 
  relocate(., "N_diaspores_Hato_Blanco", .before = "N_germinated_Hato_Blanco") %>% 
  rename("Total seeds" = "Total_diaspores",
         "Total germinated seeds" = "Total_germinated",
         "Number of seeds" = "N_diaspores_El_Rocio",
         "Number of germinated seeds" = "N_germinated_El_Rocio",
         "Number of seeds " = "N_diaspores_Hato_Blanco",
         "Number of germinated seeds " = "N_germinated_Hato_Blanco",
         "Median germination time (days)" = "median_germination_time",
         "Germinability" = "germination_rate") 

Germination_Table <- table_germination %>% 
  flextable(.) %>% 
  add_header_row(
    x = ., top = F, values = c(" ", "El Rocío", "Hato Blanco", " "),
    colwidths = c(4, 2, 2, 2)) %>% 
  add_footer(x = ., Family = "T", `Total seeds` = sum(table_germination$`Total seeds`),
             `Total germinated seeds` = sum(table_germination$`Total germinated seeds`),
             `Number of seeds` = sum(table_germination$`Number of seeds`),
             `Number of germinated seeds` = sum(table_germination$`Number of germinated seeds`),
             `Number of seeds ` = sum(table_germination$`Number of seeds `),
             `Number of germinated seeds ` = sum(table_germination$`Number of germinated seeds `) ) %>% 
  align(., align = "center", part = "all") %>% 
  italic(., j = "Species", italic = T, part = "body") %>% 
  italic(., i = c(1, 3, 4, 18), j = "Species", italic = F, part = "body") %>% 
  bold(., bold = T, part = "header") %>% 
  bold(., bold = T, part = "footer") %>% 
  vline(., j = c(4, 6, 8), part = "all") %>% 
  # font(., fontaname = , part = "all") %>% 
  set_formatter(Germinability = function(x) sprintf( "%.1f%%", x*100 )) %>% 
  compose(i =  4, j = "Species", value = as_paragraph(as_i("Carex"), " sp.")) %>% 
  compose(i = 1, j = "Species", value = as_paragraph(as_i("Eryngium"), " sp.")) %>% 
  compose(i = 6, j = "Germinability", value = as_paragraph("0.9 %", as_sub("1"))) %>% 
  compose(i = 7, j = "Germinability", value = as_paragraph("33.3 %", as_sub("2"))) %>% 
  compose(i = 18, j = "Germinability", value = as_paragraph("-"))


Germination_Table

save_as_image(Germination_Table, "./fig/Table_ger.png")


# proportion test (figure 7) ----------------------------------------------

elpa <- sdata %>% 
  filter(locality == "El_Cornejo", species == "Eleocharis_palustris")

elpa_T <- elpa %>%
  group_by(extraction_date) %>% 
  count(name = "N")

elpa_bin <- elpa %>% 
  dplyr::select(c("extraction_date", "germination", "species")) %>% 
  group_by(extraction_date, germination, species) %>% 
  count() %>% 
  left_join(elpa_T, by = "extraction_date") %>% 
  mutate(proportion = n/N,
         extraction_date = gsub(pattern = "2022-02-08", replacement = "0", extraction_date),
         extraction_date = gsub(pattern = "2022-04-12", replacement = "64", extraction_date),
         germination = gsub(pattern = 0, replacement = "No", germination),
         germination = gsub(pattern = 1, replacement = "Yes", germination)) %>% 
  rename(Germinated = germination)

boma <- sdata %>% 
  filter(locality == "El_Cornejo", species == "Bolboschoenus_maritimus")

boma_T <- boma %>%
  group_by(extraction_date) %>% 
  count(name = "N")

boma_bin <- boma %>% 
  dplyr::select(c("extraction_date", "germination", "species")) %>% 
  group_by(extraction_date, germination, species) %>% 
  count() %>% 
  left_join(boma_T, by = "extraction_date") %>% 
  mutate(proportion = n/N,
         extraction_date = gsub(pattern = "2022-02-08", replacement = "0", extraction_date),
         extraction_date = gsub(pattern = "2022-04-12", replacement = "64", extraction_date),
         germination = gsub(pattern = 0, replacement = "No", germination),
         germination = gsub(pattern = 1, replacement = "Yes", germination)) %>% 
  rename(Germinated = germination)

#proportion test Eleocharis

elpa_prop <- elpa_bin %>%
  ungroup() %>% 
  filter(Germinated == "Yes") 

boma_prop <- boma_bin %>%
  ungroup() %>% 
  filter(Germinated == "Yes")

elpa_prop_test <- prop.test(x = elpa_prop$n, n = elpa_prop$N, p = NULL, alternative = "two.sided",
                            correct = TRUE)
elpa_prop_test

elpa_prop0 <- elpa_prop[1,]

elpa_prop_test0 <- prop.test(x = elpa_prop0$n, n = elpa_prop0$N, p = NULL, alternative = "two.sided",
                             correct = TRUE)

elpa_prop64 <- elpa_prop[2,]

elpa_prop_test64 <- prop.test(x = elpa_prop64$n, n = elpa_prop64$N, p = NULL, alternative = "two.sided",
                              correct = TRUE)

#confidence intervals for the barplot

ci_elpa0 <- as.data.frame(elpa_prop_test0$conf.int)

ci_elpa64 <- as.data.frame(elpa_prop_test64$conf.int)

ci_elpa <- cbind(ci_elpa0, ci_elpa64)

#proportion test Bolboschoenus

boma_prop_test <- prop.test(x = boma_prop$n, n = boma_prop$N, p = NULL, alternative = "two.sided",
                            correct = TRUE)

boma_prop_test

boma_prop0 <- boma_prop[1,]

boma_prop_test0 <- prop.test(x = boma_prop0$n, n = boma_prop0$N, p = NULL, alternative = "two.sided",
                             correct = TRUE)

boma_prop64 <- boma_prop[2,]

boma_prop_test64 <- prop.test(x = boma_prop64$n, n = boma_prop64$N, p = NULL, alternative = "two.sided",
                              correct = TRUE)
#confidence intervals

ci_boma0 <- as.data.frame(boma_prop_test0$conf.int)

ci_boma64 <- as.data.frame(boma_prop_test64$conf.int)

ci_boma <- cbind(ci_boma0, ci_boma64)

#barplot

boma_elpa_prop <- boma_prop %>% 
  rbind(elpa_prop) %>% 
  rename(Species = species) %>% 
  mutate(Species = gsub(pattern = "_", replacement = " ", Species))


ci_boma_elpa <- cbind(ci_boma, ci_elpa)

ggplot(boma_elpa_prop, aes(x = extraction_date, y = proportion, group = Species, fill = Species)) +
  geom_bar(stat = "identity", position = "dodge") +
  geom_errorbar(aes(ymin = c(ci_boma_elpa[1,1], ci_boma_elpa[1,2], ci_boma_elpa[1,3], 
                             ci_boma_elpa[1,4]), ymax = c(ci_boma_elpa[2,1], ci_boma_elpa[2,2],
                                                          ci_boma_elpa[2,3], ci_boma_elpa[2,4])),
                position = position_dodge(0.9), width = 0.25) +
  labs(x = "Extraction time (days)",
       y = "Proportion of germinated seeds") +
  geom_text(data = boma_elpa_prop, aes(label = N, group = Species), 
            position = position_dodge(width = 0.9),
            vjust = -6) +
  ylim(0, 1) +
  theme_classic()+
  theme(text = element_text(size = 15),
        legend.text = element_text(face = "italic"))

ggsave(
  filename = "./fig/fig_germination.png",
  plot = last_plot()
)


# survival curves (figure 8) ----------------------------------------------

library(survival)
library(survminer)

elpa_gt <- sdata %>% 
  filter(locality == "El_Cornejo", species == "Eleocharis_palustris") %>% 
  mutate(germination_time = germination_date - incubation_date,
         germination_time = as.double(germination_time)) %>% 
  filter(germination == 1)

boma_gt <- sdata %>% 
  filter(locality == "El_Cornejo", species == "Bolboschoenus_maritimus") %>% 
  mutate(germination_time = germination_date - incubation_date,
         germination_time = as.double(germination_time)) %>% 
  filter(germination == 1)

km_fit_elpa <- survfit(Surv(germination_time) ~ extraction_date, data = elpa_gt)
summary(km_fit_elpa)

km_fit_boma <- survfit(Surv(germination_time) ~ extraction_date, data = boma_gt)
summary(km_fit_boma)

g_elpa <- ggsurvplot(km_fit_elpa,
                     conf.int = TRUE,
                     legend.title = "Storage time",
                     legend.labs = c("0 days (33 seeds)", "64 days (131 seeds)"),
                     ylab = "Probability of not germinating",
                     title = expression(italic("Eleocharis palustris")),
                     censor = 124,
                     ggtheme = theme_classic()+
                       theme(text = element_text(size = 15)))

g_elpa

ggsave(
  filename = "./fig/surv_elpa.png",
  plot = last_plot(),
  dpi = 300,
  width = 20,
  height = 20,
  units = "cm")

#Log-rank test

surv_diff_elpa <- survdiff(Surv(germination_time) ~ extraction_date, data = elpa_gt)
surv_diff_elpa


# references --------------------------------------------------------------

write_bib(.packages(), "./packages.bib")
