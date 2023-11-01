# This code is to combine data from different sources used for NJ municipality-level association studies
# Xiang Ren, 2021-12-14

rm(list = ls())
setwd("/projectsp/f_panosg/COVID-19/Association_Study/ForManuscript/Data/")

needed_packages <- c("ggplot2", "readr", "dplyr")
lapply(needed_packages, require, character.only = TRUE)

# load data
data_heatmap <- read_csv("data_NJMunicipality_heatmap.csv")
data_box <- read_csv("data_NJMunicipality_box_raw.csv")
nata_2014 <- read_csv("nata_2014.csv")
power_plant_2019 <- read_csv("power_plant_2019.csv")
job_2018 <- read_csv("job_2018.csv")
shop_2018 <- read_csv("shop_2018.csv")

# combine data
data_combine <- data_box[, c(1:11, 48:54, 60:84, 107:124)] %>%
  left_join(nata_2014, by = c("Town.Fips", "Town.Name.Wheat" = "Town.Name", "County.Name")) %>%
  left_join(power_plant_2019, by = c("Town.Fips", "Town.Name.Wheat" = "Town.Name", "County.Name")) %>%
  left_join(job_2018, by = c("Town.Fips", "Town.Name.Wheat" = "Town.Name", "County.Name")) %>%
  left_join(shop_2018, by = c("Town.Fips", "Town.Name.Wheat" = "Town.Name", "County.Name")) %>%
  bind_cols(NO2 = data_heatmap$`NO2 Average Conc.`, DOT.Noise.Level = data_heatmap$`DOT Noise Level`) # 565*101

data_clean <- data_combine %>%
  mutate(Job.Pct.NYC = Job.Count.NYC/Population*100, Job.Pct.Cnty = Job.Count.Cnty/Population*100,
         Job.Pct.Wholesale = Job.Count.Wholesale/Population*100, Job.Pct.Retail = Job.Count.Retail/Population*100,
         Job.Pct.Transport = Job.Count.Transport/Population*100, Job.Pct.Healthcare = Job.Count.Healthcare/Population*100,
         Job.Pct.Foodservice = Job.Count.Foodservice/Population*100, Job.Pct.HRisk = Job.Count.HRisk/Population*100) %>%
  mutate(Case.Rate = Town.Case/Population*10^4, Death.Rate = Town.Death/Population*10^5, Fatality.Rate = Town.Death/Town.Case*10^2,
         Case.rate.NonLTC = (Town.Case - LTC.Case.R)/Population*10^4, Death.rate.NonLTC = (Town.Death - LTC.Death.R)/Population*10^5) %>%
  mutate(PD = Population/Town.Area) %>%
  select(c(1:9, 24, 11, 111, 114, 10, 110, 113, 112, 52, 115, 22, 21, 20, 19, 25:29, 30, 31, 33, 36, 38, 34, 37, 39, 41, 40, 32, 49, 43, 
           104:109, 98:99, 103, 102, 46, 58:61, 57, 13, 100, 12, 62:83, 16:18, 15, 14, 84:89, 101))

data_clean[data_clean$Over65.Pct > 99, c(1:11, 14)]
data_clean[data_clean$Job.Pct.Healthcare > 99, c(1:11, 14)]
data_clean[data_clean$Job.Pct.HRisk > 99, c(1:11, 14)]
data_clean[is.na(data_clean$Restaurant.PerCapita), c(1:11, 14)]
data_clean[is.na(data_clean$Job.Pct.NYC), c(1:11, 14)]
data_clean[which.max(data_clean$Cancer.Risk), c(1:11, 14)]

write_csv(data_clean, file = "data_municipality_raw.csv")






















































