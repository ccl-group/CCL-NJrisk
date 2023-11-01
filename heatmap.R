# This code is to plot heatmap for COVID-19 association studies
# Xiang Ren, 2021-12-20

rm(list = ls())
setwd("/projectsp/f_panosg/COVID-19/Association_Study/ForManuscript/Data/")

needed_packages <- c("ggplot2", "readr", "dplyr", "ggcorrplot", "heatmaply")
lapply(needed_packages, require, character.only = TRUE)

data_all <- read_csv("data_municipality_raw.csv")

data_select <- data_all %>%
  select("# COVID19 Deaths" = "Town.Death", "COVID19 Death Rates" = "Death.Rate", "Death Rates (Exclude LTC)" = "Death.rate.NonLTC",
         "# COVID19 Cases" = "Town.Case", "COVID19 Case Rates" = "Case.Rate", "Case Rates (Exclude LTC)" = "Case.rate.NonLTC",
         "COVID19 Fatality Rates" = "Fatality.Rate", "# LTC Beds" = "LTC.Beds.NoPediatric",
         "Population Density" = "PD", 
         "% Population (Age<15)" = "Under15.Pct", "% Population (Age 15-44)" = "Age15.44.Pct", "% Population (Age 45-64)" = "Age45.64.Pct",
         "% Population (Age>64)" = "Over65.Pct", 
         "% Population (White)" = "White.Pct", "% Population (Black)" = "Black.Pct", "% Population (Asian)" = "Asian.Pct",
         "% Population (Hispanic)" = "Hispanic.Pct", "% Minority" = "Minority.Pct",
         "% Below High School Edu." = "BelowHS.Pct", "% Linguistic Isolate" = "Linguistic.Iso.Pct", "% Below Poverty Level" = "LowIncome.Pct",
         "Gini Index" = "Gini.Idx", "Median Gross Rent" = "Med.Gross.Rent", "Median Household Income" = "Med.House.Income",
         "Median House Value" = "Med.House.Value", "% House Built Before 1960" = "HouseBelow1960.Pct", "% High Occupancy Residence" = "CrowdHouse.Pct",
         "% Group Quarter Residence" = "GroupQ.Pct", "% Population (Disability)" = "Disability.Pct", "% Uninsured" = "Uninsured.Pct",
         "% Unemployed" = "Unemployed.Pct", 
         "% Occupation (Wholesale)" = "Job.Pct.Wholesale", "% Occupation (Retail)" = "Job.Pct.Retail", "% Occupation (Transportation)" = "Job.Pct.Transport",
         "% Occupation (Health Care)" = "Job.Pct.Healthcare", "% Occupation (Food Service)" = "Job.Pct.Foodservice", "% Occupation (High Risk)" = "Job.Pct.HRisk",
         "# Restaurants Per Capita" = "Restaurant.PerCapita", "# Supermarkets Per Capita" = "Grocery.PerCapita",
         "% Commute (To Diff. Counties)" = "Job.Pct.Cnty", "% Commute (To NY City)" = "Job.Pct.NYC", "% Commute (Public Transport)" = "Public.Trans.Pct",
         "SVI (Socioeconomic)" = "SVI.Socioeconomic", "SVI (Disability)" = "SVI.Disability", "SVI (Minority & Language)" = "SVI.Minority",
         "SVI (Housing & Transport)" = "SVI.House", "SVI (Overall)" = "SVI.Overall",
         "PM25 Average Conc." = "PM25", "NO2 Average Conc." = "NO2", "Ozone Seasonal DM8HA" = "Ozone",
         "Acrolein" = "Acrolein.Conc", "Acetaldehyde" = "Acetaldehyde.Conc", "Formaldehyde" = "Formaldehyde.Conc",
         "Diesel PM" = "Diesel.PM10.Conc", "Naphthalene" = "Naphthalene.Conc", "Acrylic Acid" = "Acrylic.Acid.Conc",
         "Acrylonitrile" = "Acrylonitrile.Conc", "Beryllium" = "Beryllium.Conc", "Chlorine" = "Chlorine.Conc",
         "Chromhex" = "Chromhex.Conc", "Ethylene Glycol" = "Ethylene.Glycol.Conc", "Hexamethylene Diisocyanate" = "Hexamethylene.Diisocyanate.Conc",
         "Hydrochloric Acid" = "Hydrochloric.Acid.Conc", "Maleic Anhydride" = "Maleic.Anhydride.Conc", "Methylene Chloride" = "Methylene.Chloride.Conc",
         "Methyl Bromide" = "Methyl.Bromide.Conc", "Nickel" = "Nickel", "Propionaldehyde" = "Propionaldehyde.Conc",
         "2,4-Toluene Diisocyanate" = "Toluene.Diiso.Conc", "4,4P-Methylenediphenyl Diiso." = "Methylenediphenyl.Diiso.Conc",
         "Inhalation Cancer Risk" = "Cancer.Risk", "Respiratory Hazard Index" = "Resp.Risk",
         "NPL Site Proximity" = "NPL.Prox", "RMP Facility Proximity" = "RMP.Prox", "TSDF Facility Proximity" = "TSDF.Prox",
         "Proximity to TWWD" = "Wastewater.Prox", "Traffic Proximity" = "Traffic.Prox",
         "Proximity to Natural Gas EGU" = "PP.Prox.Gas", "Proximity to Petroleum EGU" = "PP.Prox.Petro", "Proximity to Biomass EGU" = "PP.Prox.Biom", 
         "Proximity to Coal EGU" = "PP.Prox.Coal", "Proximity to Nuclear EGU" = "PP.Prox.Nucl", "Proximity to WFFEGU" = "PP.Prox.WFF",
         "DOT Noise Level" = "DOT.Noise.Level")

# original heatmap
corr_matrix <- round(cor(data_select, method = "spearman", use = "pairwise.complete.obs"), 3)
ggcorrplot(corr_matrix)
# ggcorrplot(corr_matrix, hc.order = TRUE)
ggsave(filename = "/projectsp/f_panosg/COVID-19/Association_Study/ForManuscript/Figure/heatmap_municipality.png", width = 18, height = 18, units = "in", dpi = 300)

# ordered heatmap
idx <- sort(corr_matrix[1, ], decreasing = TRUE, index.return = TRUE)$ix
corr_matrix <- corr_matrix[, idx]
corr_matrix <- corr_matrix[idx, ]
ggcorrplot(corr_matrix)
ggsave(filename = "/projectsp/f_panosg/COVID-19/Association_Study/ForManuscript/Figure/heatmap_municipality_ordered.png", width = 18, height = 18, units = "in", dpi = 300)





