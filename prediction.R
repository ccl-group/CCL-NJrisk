# This code is to predict covid-19 deaths at municipality level in NJ
# Xiang Ren, 2022-1-5

rm(list = ls())
setwd("/projectsp/f_panosg/COVID-19/Association_Study/ForManuscript/")

needed_packages <- c("dplyr", "readr", "tidyr", "tibble", "ggplot2", "stringr", "car", "INLA", "ggrepel", "sf")
lapply(needed_packages, require, character.only = TRUE)

fmla.function <- function(variable, model_type) {  # formula
  if (model_type %in% c("poisson", "nbinomial")) {
    as.formula(paste("Town.Death ~ ", paste(variable, collapse = "+")))
  } else if (model_type %in% c("poisson.iid.region", "nbinomial.iid.region")) {
    update(as.formula(paste("Town.Death ~ ", paste(variable, collapse = "+"))), . ~ . + 
             f(Region.Name, model = "iid"))
  } else if (model_type %in% c("poisson.iid.county", "nbinomial.iid.county")) {
    update(as.formula(paste("Town.Death ~ ", paste(variable, collapse = "+"))), . ~ . + 
             f(County.Name, model = "iid"))
  } else if (model_type %in% c("poisson.iid", "nbinomial.iid")) {
    update(as.formula(paste("Town.Death ~ ", paste(variable, collapse = "+"))), . ~ . + 
             f(OBJECTID, model = "iid"))
  } else if (model_type %in% c("poisson.bym", "nbinomial.bym")) {
    update(as.formula(paste("Town.Death ~ ", paste(variable, collapse = "+"))), . ~ . + 
             f(OBJECTID, model = "bym", graph = "Data/municipality_adjacency.graph"))
  }
}

mod.function <- function(fmla, data, model_type, link) {
  model_family <- str_split(model_type, "[.]", simplify = TRUE)[1]
  if (model_family == "poisson") { 
    return(inla(fmla, data = data, family = "poisson", offset = log(Population),
                control.compute = list(dic = TRUE, cpo = TRUE),
                control.predictor = list(link = link)))
  } else if (model_family == "nbinomial") {
    return(inla(fmla, data = data, family = "nbinomial", offset = log(Population),
                control.compute = list(dic = TRUE, cpo = TRUE, config = TRUE),
                control.predictor = list(link = link)))
  }
}

## modify data for modeling
data_raw <- read_csv("Data/data_municipality_model.csv")  # 565*68
PD_cut <- quantile(data_raw$PD, c(0.2, 0.4, 0.6, 0.8))  # quantile cut for PD
data_select <- arrange(data_raw, OBJECTID) %>%
  mutate(Acetaldehyde.Conc = Acetaldehyde.Conc * 10,  # 0.01HQ, rescale three air toxic
         Diesel.PM10.Conc = Diesel.PM10.Conc / 10,  # 0.1HQ
         Formaldehyde.Conc = Formaldehyde.Conc * 10) %>%  # 0.01HQ
  mutate(PD = case_when(PD <= PD_cut[1] ~ 0,
                        (PD > PD_cut[1]) & (PD <= PD_cut[2]) ~ 1,
                        (PD > PD_cut[2]) & (PD <= PD_cut[3]) ~ 2,
                        (PD > PD_cut[3]) & (PD <= PD_cut[4]) ~ 3,
                        TRUE ~ 4)) 
link <- rep(NA, nrow(data_select))
link[which(is.na(data_select$Town.Death))] <- 1 

xref_poisson <- c("Over65.Pct", "Minority.Pct", "BelowHS.Pct", "Med.Gross.Rent", "PD", "Job.Pct.HRisk", "PM25",
                  "Ozone", "CrowdHouse.Pct", "Unemployed.Pct")  # consider 10 variables in reference model
xref_nbinomial <- c("Over65.Pct", "Minority.Pct", "BelowHS.Pct", "Med.Gross.Rent", "PD", "Job.Pct.HRisk", "PM25",
                    "Ozone", "CrowdHouse.Pct", "Unemployed.Pct")

xname_scale <- setdiff(names(data_select), c("Town.Death", "PM25", "NO2", "Ozone", "Acrolein.Conc", "Acetaldehyde.Conc", "Formaldehyde.Conc", "Diesel.PM10.Conc", "Naphthalene.Conc",
                                             "Population", "Centroid.X", "Centroid.Y", "County.Name", "Region.Name", "OBJECTID"))
data_select[, xname_scale] <- as.data.frame(apply(data_select[, xname_scale], 2, function(x) scale(x)))

model_name <- c("poisson", "poisson.iid.region", "poisson.bym")
model_name <- c("nbinomial", "nbinomial.iid.region", "nbinomial.bym")

for (i in 1:length(model_name)) {
  fmla <- fmla.function(xref_nbinomial, model_name[i])  # xref_nbinomial
  mod <- mod.function(fmla, data_select, model_name[i], link)
  saveRDS(mod, paste0("Data/Output/ModelPredict/", model_name[i], "_predict_565samples.rds", sep = ""))
  print(i)
}

## county level prediction with Gaussian approximated interval
# six geostatistical models
model_name <- c("poisson", "poisson.iid.region", "poisson.bym",
                "nbinomial", "nbinomial.iid.region", "nbinomial.bym")
for (i in 1:6)  {  # load six model objects
  assign(model_name[i], readRDS(paste0("Data/Output/ModelPredict/", model_name[i], "_predict_565samples.rds", sep = "")))
}

data_predict <- cbind(data_select, 
                      poisson = poisson$summary.fitted.values$mean, nbinomial = nbinomial$summary.fitted.values$mean,
                      poisson.iid.region = poisson.iid.region$summary.fitted.values$mean, nbinomial.iid.region = nbinomial.iid.region$summary.fitted.values$mean,
                      poisson.bym = poisson.bym$summary.fitted.values$mean, nbinomial.bym = nbinomial.bym$summary.fitted.values$mean,
                      poisson_var = poisson$summary.fitted.values$sd^2, nbinomial_var = nbinomial$summary.fitted.values$sd^2,
                      poisson.iid.region_var = poisson.iid.region$summary.fitted.values$sd^2, nbinomial.iid.region_var = nbinomial.iid.region$summary.fitted.values$sd^2,
                      poisson.bym_var = poisson.bym$summary.fitted.values$sd^2, nbinomial.bym_var = nbinomial.bym$summary.fitted.values$sd^2) %>%  #  mean and variance
  filter(Population > 10) %>%
  mutate(poisson.bym_var = replace(poisson.bym_var, poisson.bym_var > 100000, 0), 
         nbinomial.bym_var = replace(nbinomial.bym_var, nbinomial.bym_var > 100000, 0)) %>%
  group_by(County.Name) %>%
  summarise(sum.deaths = sum(Town.Death),
            poisson_mean = sum(poisson), poisson.iid.region_mean = sum(poisson.iid.region), poisson.bym_mean = sum(poisson.bym),
            nbinomial_mean = sum(nbinomial), nbinomial.iid.region_mean = sum(nbinomial.iid.region), nbinomial.bym_mean = sum(nbinomial.bym),
            poisson_sd = sqrt(sum(poisson_var)), poisson.iid.region_sd = sqrt(sum(poisson.iid.region_var)), poisson.bym_sd = sqrt(sum(poisson.bym_var)),
            nbinomial_sd = sqrt(sum(nbinomial_var)), nbinomial.iid.region_sd = sqrt(sum(nbinomial.iid.region_var)), nbinomial.bym_sd = sqrt(sum(nbinomial.bym_var)))

# two ML models
data_ml <- read_csv("Data/Output/ModelPredict/MachineLearning/data_predict_ml.csv") %>%
  mutate(rf = rf.pred * Population, xgboost = xgboost.pred * Population) 
data_predict_ml <- cbind(data_select, data_ml[, c("rf", "xgboost")]) %>%
  filter(Population > 10) %>%
  group_by(County.Name) %>%
  summarise(rf_mean = sum(rf), xgboost_mean = sum(xgboost))

# combine
data_predict <- left_join(data_predict, data_predict_ml, by = "County.Name")  # 21*16
county_death <- read_csv("Data/county_death.csv") %>%  # NJ county deaths on 9/24
  left_join(data_predict, by = c("county" = "County.Name")) %>%
  arrange(county)  # county predictions and observations

data_predict_final <- filter(county_death, county != "Unknown") %>%
  mutate(poisson_lwr = poisson_mean - 1.96*poisson_sd, poisson.iid.region_lwr = poisson.iid.region_mean - 1.96*poisson.iid.region_sd, poisson.bym_lwr = poisson.bym_mean - 1.96*poisson.bym_sd,
         nbinomial_lwr = nbinomial_mean - 1.96*nbinomial_sd, nbinomial.iid.region_lwr = nbinomial.iid.region_mean - 1.96*nbinomial.iid.region_sd, nbinomial.bym_lwr = nbinomial.bym_mean - 1.96*nbinomial.bym_sd,
         rf_lwr = NA, xgboost_lwr = NA,
         poisson_upr = poisson_mean + 1.96*poisson_sd, poisson.iid.region_upr = poisson.iid.region_mean + 1.96*poisson.iid.region_sd, poisson.bym_upr = poisson.bym_mean + 1.96*poisson.bym_sd,
         nbinomial_upr = nbinomial_mean + 1.96*nbinomial_sd, nbinomial.iid.region_upr = nbinomial.iid.region_mean + 1.96*nbinomial.iid.region_sd, nbinomial.bym_upr = nbinomial.bym_mean + 1.96*nbinomial.bym_sd,
         rf_upr = NA, xgboost_upr = NA) %>%
  select(-c(14:19)) %>%
  pivot_longer(cols = "poisson_mean":"xgboost_upr", names_to = "Metric", values_to = "Value") %>%
  separate(Metric, into = c("Model", "Metric.Name"), sep = "_") %>%
  mutate(Value = round(Value, 0)) %>%
  pivot_wider(names_from = "Metric.Name", values_from = "Value") %>%
  mutate(Model = factor(Model)) %>%
  mutate(Model = recode_factor(Model, "poisson" = "Poisson Regression", "poisson.iid.region" = "Poisson Mixed Effect Model", "poisson.bym" = "Poisson Besag-York-Mollie Spatial Model", "rf" = "Random Forest",
                               "nbinomial" = "Negative Binomial Regression", "nbinomial.iid.region" = "Negative Binomial Mixed Effect Model", "nbinomial.bym" = "Negative Binomial Besag-York-Mollie Spatial Model", "xgboost" = "Extreme Gradient Boosting"))

data_interest <- filter(data_select, !is.na(Town.Death)) %>%  # calculate missing data in each county
  group_by(County.Name) %>%
  summarise(Value.Num = n()) %>%
  right_join({
    group_by(data_select, County.Name) %>%
      summarise(., Town.Num = n())
  }, by = "County.Name") %>%
  mutate(Ratio = Value.Num/Town.Num) %>%
  arrange(desc(Ratio))
county_complete <- data_interest$County.Name[which(data_interest$Ratio == 1)]  # counties with complete record deaths
county_missing <- setdiff(data_interest$County.Name, county_complete)
data_complete <- filter(data_predict_final, county %in% county_complete)  # split into two labeled with different colors
data_missing <- filter(data_predict_final, county %in% county_missing)

ggplot(data = data_complete, aes(x = deaths, y = mean)) +
  geom_point(color = "dodgerblue3", alpha = 0.6) +
  geom_errorbar(aes(ymin = lwr, ymax = upr), width = 50, size = 0.5, color = "dodgerblue3", alpha = 0.6) +
  geom_point(data = data_missing, aes(x = deaths, y = mean), color = "goldenrod3", inherit.aes = FALSE) +
  geom_errorbar(data = data_missing, aes(x= deaths, ymin = lwr, ymax = upr), width = 50, size = 0.5, color = "goldenrod3", inherit.aes = FALSE) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", alpha = 0.7) +
  facet_wrap(~ Model, nrow = 2) +
  geom_text_repel(data = data_predict_final, aes(label = county), size = 2.5, segment.size = 0.1, vjust = 0.5, hjust = 0.5, col = "black") +
  scale_x_continuous(name = "# Confirmed Deaths", limits = c(0, 2600), breaks = seq(0, 2600, by = 500)) +
  scale_y_continuous(name = "# Predicted Deaths", limits = c(0, 2600), breaks = seq(0, 2600, by = 500)) +
  theme_bw() +
  theme(strip.text = element_text(face = "bold"))
ggsave("Figure/Scatterplot/scatterplot_county-prediction_all-models.png", width = 13, height = 6.8, units = "in", dpi = 300)

# machine learning results (for test purpose)
data_ml <- read_csv("Data/Output/ModelPredict/MachineLearning/data_predict_ml.csv") %>%
  mutate(rf = xgboost.pred * Population) 
plot(data_ml$Town.Death,data_ml$rf)  # visualize municipality-level plot
abline(0, 1)
data_predict <- cbind(data_select, rf = data_ml$rf) %>%
  filter(Population > 10) %>%
  group_by(County.Name) %>%
  summarise(sum.deaths = sum(Town.Death), rf = sum(rf))

county_death <- read_csv("Data/county_death.csv") %>%  # NJ county deaths on 9/24
  left_join(data_predict, by = c("county" = "County.Name")) %>%
  arrange(county) %>%  # county predictions and observations
  filter(county != "Unknown")

data_interest <- filter(data_select, !is.na(Town.Death)) %>%  # calculate missing data in each county
  group_by(County.Name) %>%
  summarise(Value.Num = n()) %>%
  right_join({
    group_by(data_select, County.Name) %>%
      summarise(., Town.Num = n())
  }, by = "County.Name") %>%
  mutate(Ratio = Value.Num/Town.Num) %>%
  arrange(desc(Ratio))
county_complete <- data_interest$County.Name[which(data_interest$Ratio == 1)]  # counties with complete record deaths
county_missing <- setdiff(data_interest$County.Name, county_complete)

data_complete <- filter(county_death, county %in% county_complete)  # split into two labeled with different colors
data_missing <- filter(county_death, county %in% county_missing)

ggplot(data = data_complete, aes(x = deaths, y = rf)) +
  geom_point(color = "dodgerblue3", alpha = 0.6) +
  geom_point(data = data_missing, aes(x = deaths, y = rf), color = "goldenrod3", inherit.aes = FALSE) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", alpha = 0.7) +
  geom_text_repel(data = county_death, aes(label = county), size = 2.5, segment.size = 0.1, vjust = 0.5, hjust = 0.5, col = "black") +
  scale_x_continuous(name = "# Confirmed Deaths", limits = c(0, 2600), breaks = seq(0, 2600, by = 500)) +
  scale_y_continuous(name = "# Predicted Deaths", limits = c(0, 2600), breaks = seq(0, 2600, by = 500)) +
  theme_bw()

## map of death rates
municipality_poly <- st_read("Data/Municipal_Boundaries_of_NJ_Shapefile/NJ_Municipal_Boundaries_3424.shp") %>%
  select(c(1:5, 24))
data_shp <- st_drop_geometry(municipality_poly) %>%
  mutate(OBJECTID1 = OBJECTID, County.Name1 = str_to_title(COUNTY), Town.Name.Standard1 = MUN_LABEL) %>%
  select(OBJECTID1, County.Name1, Town.Name.Standard1)
data_deaths <- cbind(data_select, 
                     poisson = poisson$summary.fitted.values$mean, nbinomial = nbinomial$summary.fitted.values$mean,
                     poisson.iid.region = poisson.iid.region$summary.fitted.values$mean, nbinomial.iid.region = nbinomial.iid.region$summary.fitted.values$mean,
                     poisson.bym = poisson.bym$summary.fitted.values$mean, nbinomial.bym = nbinomial.bym$summary.fitted.values$mean,
                     rf = data_ml$rf, xgboost = data_ml$xgboost) %>%
  mutate(death_rate = Town.Death/Population*10^5,
         poisson_rate = poisson/Population*10^5, nbinomial_rate = nbinomial/Population*10^5,
         poisson.iid.region_rate = poisson.iid.region/Population*10^5, nbinomial.iid.region_rate = nbinomial.iid.region/Population*10^5,
         poisson.bym_rate = poisson.bym/Population*10^5, nbinomial.bym_rate = nbinomial.bym/Population*10^5,
         rf_rate = rf/Population*10^5, xgboost_rate = xgboost/Population*10^5) %>%
  bind_cols(data_shp) %>%
  select(-c(2:62, 67))
data_deaths[which(data_deaths$Population < 10), c(7:23)] <- 0  # remove three townships with population < 10

model_name <- paste0(c("poisson", "poisson.iid.region", "poisson.bym", "nbinomial", "nbinomial.iid.region", "nbinomial.bym", "rf", "xgboost", "death"), "_rate", sep = "")
col.br <- colorRampPalette(c("lightgoldenrodyellow", "lightgoldenrod1", "lightgoldenrod2", "orange", "darkorange1", "orangered", "red", "red3", "darkred"))(10)  # color palette

for (i in 1:length(model_name)) {  
  data_deaths_sf <- bind_cols(municipality_poly, data_deaths) %>%
    mutate(variable_focus = get(model_name[i])) %>%
    mutate(category = case_when(is.na(variable_focus) ~ "a",
                                (variable_focus < 25) & (variable_focus >= 0) ~ 'b',
                                (variable_focus < 50) & (variable_focus >= 25) ~ 'c',
                                (variable_focus < 75) & (variable_focus >= 50) ~ 'd',
                                (variable_focus < 100) & (variable_focus >= 75) ~ 'e',
                                (variable_focus < 125) & (variable_focus >= 100) ~ 'f',
                                (variable_focus < 150) & (variable_focus >= 125) ~ 'g',
                                (variable_focus < 175) & (variable_focus >= 150) ~ 'h',
                                (variable_focus < 200) & (variable_focus >= 175) ~ 'i',
                                (variable_focus < 250) & (variable_focus >= 200) ~ 'j',
                                TRUE ~ 'k'))
  ggplot() +
    geom_sf(data = data_deaths_sf, aes(fill = category), size = 0.1, color = "black", inherit.aes = FALSE) +
    scale_fill_manual(values = c('a' = "lightgrey", 'b' = col.br[1], 'c' = col.br[2], 'd' = col.br[3],
                                 'e' = col.br[4], 'f' = col.br[5], 'g' = col.br[6], 'h' = col.br[7],
                                 'i' = col.br[8], 'j' = col.br[9], 'k' = col.br[10]),
                      labels = c('No Data','0-25', '25-50', '50-75', '75-100', '100-125',
                                 '125-150', '150-175', '175-200', '200-250', '>250')) +
    theme_void() +
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
          axis.ticks = element_blank(), axis.text = element_blank(),
          legend.title = element_text()) +
    labs(fill = "Death Rates\n(per 100k)")
  ggsave(paste0("Figure/Map/death-rate_", model_name[i], ".png"), width = 6, height = 8, units = "in", dpi = 300)
}

## machine learning results (for test purpose)
municipality_poly <- st_read("Data/Municipal_Boundaries_of_NJ_Shapefile/NJ_Municipal_Boundaries_3424.shp") %>%
  select(c(1:5, 24))
data_shp <- st_drop_geometry(municipality_poly) %>%
  mutate(OBJECTID1 = OBJECTID, County.Name1 = str_to_title(COUNTY), Town.Name.Standard1 = MUN_LABEL) %>%
  select(OBJECTID1, County.Name1, Town.Name.Standard1)
data_deaths <- cbind(data_select, rf = data_ml$rf) %>%
  mutate(death_rate = Town.Death/Population*10^5, rf_rate = rf/Population*10^5) %>%
  bind_cols(data_shp) %>%
  select(-c(2:62, 67))

col.br <- colorRampPalette(c("lightgoldenrodyellow", "lightgoldenrod1", "lightgoldenrod2", "orange", "darkorange1", "orangered", "red", "red3", "darkred"))(10)  # color palette

data_deaths_sf <- bind_cols(municipality_poly, data_deaths) %>%
  mutate(variable_focus = rf_rate) %>%
  mutate(category = case_when(is.na(variable_focus) ~ "a",
                              (variable_focus < 25) & (variable_focus >= 0) ~ 'b',
                              (variable_focus < 50) & (variable_focus >= 25) ~ 'c',
                              (variable_focus < 75) & (variable_focus >= 50) ~ 'd',
                              (variable_focus < 100) & (variable_focus >= 75) ~ 'e',
                              (variable_focus < 125) & (variable_focus >= 100) ~ 'f',
                              (variable_focus < 150) & (variable_focus >= 125) ~ 'g',
                              (variable_focus < 175) & (variable_focus >= 150) ~ 'h',
                              (variable_focus < 200) & (variable_focus >= 175) ~ 'i',
                              (variable_focus < 250) & (variable_focus >= 200) ~ 'j',
                              TRUE ~ 'k'))
ggplot() +
  geom_sf(data = data_deaths_sf, aes(fill = category), size = 0.1, color = "black", inherit.aes = FALSE) +
  scale_fill_manual(values = c('a' = "lightgrey", 'b' = col.br[1], 'c' = col.br[2], 'd' = col.br[3],
                               'e' = col.br[4], 'f' = col.br[5], 'g' = col.br[6], 'h' = col.br[7],
                               'i' = col.br[8], 'j' = col.br[9], 'k' = col.br[10]),
                    labels = c('No Data','0-25', '25-50', '50-75', '75-100', '100-125',
                               '125-150', '150-175', '175-200', '200-250', '>250')) +
  theme_void() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        axis.ticks = element_blank(), axis.text = element_blank(),
        legend.title = element_text()) +
  labs(fill = "Death Rates\n(per 100,000)")
ggsave(paste0("Figure/Map/death-rate_", model_name[i], ".png"), width = 6, height = 8, units = "in", dpi = 300)
