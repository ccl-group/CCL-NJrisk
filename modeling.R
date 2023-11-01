# This code is to construct reference model and quantify associations of COVID-19 deaths
# Xiang Ren, 2021-12-28

rm(list = ls())
setwd("/projectsp/f_panosg/COVID-19/Association_Study/ForManuscript/")

needed_packages <- c("dplyr", "readr", "tidyr", "tibble", "ggplot2", "stringr", "car", "INLA")
lapply(needed_packages, require, character.only = TRUE)

## modify data for modeling
data_model <- read_csv("Data/data_municipality_model.csv") %>%
  filter(Population > 10) %>%  # remove three municipalities with population below 10, 562*68
  drop_na(Town.Death) %>%  # remove missing data in Town.Death
  mutate(Restaurant.PerCapita = replace(Restaurant.PerCapita, is.na(Restaurant.PerCapita), quantile(Restaurant.PerCapita, 0.5, na.rm = TRUE)),
         Grocery.PerCapita = replace(Grocery.PerCapita, is.na(Grocery.PerCapita), quantile(Grocery.PerCapita, 0.5, na.rm = TRUE)),
         Job.Pct.NYC = replace(Job.Pct.NYC, is.na(Job.Pct.NYC), quantile(Job.Pct.NYC, 0.5, na.rm = TRUE)))  # replace mising data in three columns with median, 355*68

PD_cut <- quantile(data_model$PD, c(0.2, 0.4, 0.6, 0.8))  # quantile cut for PD
Med.House.Income_cut <- quantile(data_model$Med.House.Income, c(0.2, 0.4, 0.6, 0.8))  # quantile cut for Med.House.Income
NPL.Prox_cut <- quantile(data_model$NPL.Prox, c(0.2, 0.4, 0.6, 0.8))  # quantile cut for NPL.Prox, analyze separately
RMP.Prox_cut <- quantile(data_model$RMP.Prox, c(0.2, 0.4, 0.6, 0.8))  # quantile cut for RMP.Prox
TSDF.Prox_cut <- quantile(data_model$TSDF.Prox, c(0.2, 0.4, 0.6, 0.8))  # quantile cut for TSDF.Prox
Wastewater.Prox_cut <- quantile(data_model$Wastewater.Prox, c(0.2, 0.4, 0.6, 0.8))  # quantile cut for Wastewater.Prox
Traffic.Prox_cut <- quantile(data_model$Traffic.Prox, c(0.2, 0.4, 0.6, 0.8))  # quantile cut for Traffic.Prox
PP.Prox.WFF_cut <- quantile(data_model$PP.Prox.WFF, c(0.2, 0.4, 0.6, 0.8))  # quantile cut for PP.Prox.WFF
PP.Prox.Gas_cut <- quantile(data_model$PP.Prox.Gas, c(0.2, 0.4, 0.6, 0.8))  # quantile cut for PP.Prox.Gas
PP.Prox.Petro_cut <- quantile(data_model$PP.Prox.Petro, c(0.2, 0.4, 0.6, 0.8))  # quantile cut for PP.Prox.Petro
PP.Prox.Biom_cut <- quantile(data_model$PP.Prox.Biom, c(0.2, 0.4, 0.6, 0.8))  # quantile cut for PP.Prox.Biom
PP.Prox.Coal_cut <- quantile(data_model$PP.Prox.Coal, c(0.2, 0.4, 0.6, 0.8))  # quantile cut for PP.Prox.Coal
PP.Prox.Nucl_cut <- quantile(data_model$PP.Prox.Nucl, c(0.2, 0.4, 0.6, 0.8))  # quantile cut for PP.Prox.Nucl
data_select <- data_model %>%
  mutate(Acetaldehyde.Conc = Acetaldehyde.Conc * 10,  # 0.01HQ, rescale three air toxic
         Diesel.PM10.Conc = Diesel.PM10.Conc / 10,  # 0.1HQ
         Formaldehyde.Conc = Formaldehyde.Conc * 10) %>%  # 0.01HQ
  mutate(PD = case_when(PD <= PD_cut[1] ~ 0,
                        (PD > PD_cut[1]) & (PD <= PD_cut[2]) ~ 1,
                        (PD > PD_cut[2]) & (PD <= PD_cut[3]) ~ 2,
                        (PD > PD_cut[3]) & (PD <= PD_cut[4]) ~ 3,
                        TRUE ~ 4)) %>%  # quantile transformation for PD
  # mutate(Med.House.Income = case_when(Med.House.Income <= Med.House.Income_cut[1] ~ 0,
  #                       (Med.House.Income > Med.House.Income_cut[1]) & (Med.House.Income <= Med.House.Income_cut[2]) ~ 1,
  #                       (Med.House.Income > Med.House.Income_cut[2]) & (Med.House.Income <= Med.House.Income_cut[3]) ~ 2,
  #                       (Med.House.Income > Med.House.Income_cut[3]) & (Med.House.Income <= Med.House.Income_cut[4]) ~ 3,
  #                       TRUE ~ 4)) %>% 
  mutate(NPL.Prox = case_when(NPL.Prox <= NPL.Prox_cut[1] ~ 0,
                        (NPL.Prox > NPL.Prox_cut[1]) & (NPL.Prox <= NPL.Prox_cut[2]) ~ 1,
                        (NPL.Prox > NPL.Prox_cut[2]) & (NPL.Prox <= NPL.Prox_cut[3]) ~ 2,
                        (NPL.Prox > NPL.Prox_cut[3]) & (NPL.Prox <= NPL.Prox_cut[4]) ~ 3,
                        TRUE ~ 4)) %>%
  mutate(RMP.Prox = case_when(RMP.Prox <= RMP.Prox_cut[1] ~ 0,
                        (RMP.Prox > RMP.Prox_cut[1]) & (RMP.Prox <= RMP.Prox_cut[2]) ~ 1,
                        (RMP.Prox > RMP.Prox_cut[2]) & (RMP.Prox <= RMP.Prox_cut[3]) ~ 2,
                        (RMP.Prox > RMP.Prox_cut[3]) & (RMP.Prox <= RMP.Prox_cut[4]) ~ 3,
                        TRUE ~ 4)) %>%
  mutate(TSDF.Prox = case_when(TSDF.Prox <= TSDF.Prox_cut[1] ~ 0,
                        (TSDF.Prox > TSDF.Prox_cut[1]) & (TSDF.Prox <= TSDF.Prox_cut[2]) ~ 1,
                        (TSDF.Prox > TSDF.Prox_cut[2]) & (TSDF.Prox <= TSDF.Prox_cut[3]) ~ 2,
                        (TSDF.Prox > TSDF.Prox_cut[3]) & (TSDF.Prox <= TSDF.Prox_cut[4]) ~ 3,
                        TRUE ~ 4)) %>%
  mutate(Wastewater.Prox = case_when(Wastewater.Prox <= Wastewater.Prox_cut[1] ~ 0,
                        (Wastewater.Prox > Wastewater.Prox_cut[1]) & (Wastewater.Prox <= Wastewater.Prox_cut[2]) ~ 1,
                        (Wastewater.Prox > Wastewater.Prox_cut[2]) & (Wastewater.Prox <= Wastewater.Prox_cut[3]) ~ 2,
                        (Wastewater.Prox > Wastewater.Prox_cut[3]) & (Wastewater.Prox <= Wastewater.Prox_cut[4]) ~ 3,
                        TRUE ~ 4)) %>%
  mutate(Traffic.Prox = case_when(Traffic.Prox <= Traffic.Prox_cut[1] ~ 0,
                        (Traffic.Prox > Traffic.Prox_cut[1]) & (Traffic.Prox <= Traffic.Prox_cut[2]) ~ 1,
                        (Traffic.Prox > Traffic.Prox_cut[2]) & (Traffic.Prox <= Traffic.Prox_cut[3]) ~ 2,
                        (Traffic.Prox > Traffic.Prox_cut[3]) & (Traffic.Prox <= Traffic.Prox_cut[4]) ~ 3,
                        TRUE ~ 4)) %>%
  mutate(PP.Prox.WFF = case_when(PP.Prox.WFF <= PP.Prox.WFF_cut[1] ~ 0,
                        (PP.Prox.WFF > PP.Prox.WFF_cut[1]) & (PP.Prox.WFF <= PP.Prox.WFF_cut[2]) ~ 1,
                        (PP.Prox.WFF > PP.Prox.WFF_cut[2]) & (PP.Prox.WFF <= PP.Prox.WFF_cut[3]) ~ 2,
                        (PP.Prox.WFF > PP.Prox.WFF_cut[3]) & (PP.Prox.WFF <= PP.Prox.WFF_cut[4]) ~ 3,
                        TRUE ~ 4)) #%>%
  # mutate(PP.Prox.Gas = case_when(PP.Prox.Gas <= PP.Prox.Gas_cut[1] ~ 0,
  #                       (PP.Prox.Gas > PP.Prox.Gas_cut[1]) & (PP.Prox.Gas <= PP.Prox.Gas_cut[2]) ~ 1,
  #                       (PP.Prox.Gas > PP.Prox.Gas_cut[2]) & (PP.Prox.Gas <= PP.Prox.Gas_cut[3]) ~ 2,
  #                       (PP.Prox.Gas > PP.Prox.Gas_cut[3]) & (PP.Prox.Gas <= PP.Prox.Gas_cut[4]) ~ 3,
  #                       TRUE ~ 4)) %>%
  # mutate(PP.Prox.Petro = case_when(PP.Prox.Petro <= PP.Prox.Petro_cut[1] ~ 0,
  #                       (PP.Prox.Petro > PP.Prox.Petro_cut[1]) & (PP.Prox.Petro <= PP.Prox.Petro_cut[2]) ~ 1,
  #                       (PP.Prox.Petro > PP.Prox.Petro_cut[2]) & (PP.Prox.Petro <= PP.Prox.Petro_cut[3]) ~ 2,
  #                       (PP.Prox.Petro > PP.Prox.Petro_cut[3]) & (PP.Prox.Petro <= PP.Prox.Petro_cut[4]) ~ 3,
  #                       TRUE ~ 4)) %>%
  # mutate(PP.Prox.Biom = case_when(PP.Prox.Biom <= PP.Prox.Biom_cut[1] ~ 0,
  #                       (PP.Prox.Biom > PP.Prox.Biom_cut[1]) & (PP.Prox.Biom <= PP.Prox.Biom_cut[2]) ~ 1,
  #                       (PP.Prox.Biom > PP.Prox.Biom_cut[2]) & (PP.Prox.Biom <= PP.Prox.Biom_cut[3]) ~ 2,
  #                       (PP.Prox.Biom > PP.Prox.Biom_cut[3]) & (PP.Prox.Biom <= PP.Prox.Biom_cut[4]) ~ 3,
  #                       TRUE ~ 4)) %>%
  # mutate(PP.Prox.Coal = case_when(PP.Prox.Coal <= PP.Prox.Coal_cut[1] ~ 0,
  #                       (PP.Prox.Coal > PP.Prox.Coal_cut[1]) & (PP.Prox.Coal <= PP.Prox.Coal_cut[2]) ~ 1,
  #                       (PP.Prox.Coal > PP.Prox.Coal_cut[2]) & (PP.Prox.Coal <= PP.Prox.Coal_cut[3]) ~ 2,
  #                       (PP.Prox.Coal > PP.Prox.Coal_cut[3]) & (PP.Prox.Coal <= PP.Prox.Coal_cut[4]) ~ 3,
  #                       TRUE ~ 4)) %>%
  # mutate(PP.Prox.Nucl = case_when(PP.Prox.Nucl <= PP.Prox.Nucl_cut[1] ~ 0,
  #                       (PP.Prox.Nucl > PP.Prox.Nucl_cut[1]) & (PP.Prox.Nucl <= PP.Prox.Nucl_cut[2]) ~ 1,
  #                       (PP.Prox.Nucl > PP.Prox.Nucl_cut[2]) & (PP.Prox.Nucl <= PP.Prox.Nucl_cut[3]) ~ 2,
  #                       (PP.Prox.Nucl > PP.Prox.Nucl_cut[3]) & (PP.Prox.Nucl <= PP.Prox.Nucl_cut[4]) ~ 3,
  #                       TRUE ~ 4))
data_cor <- as.data.frame(cor(data_select[, 1:62]))  # Pearson correlation matrix

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

mod.function <- function(fmla, data, model_type) {  # run model
  model_family <- str_split(model_type, "[.]", simplify = TRUE)[1]
  if (model_family == "poisson") { 
    return(inla(fmla, data = data, family = "poisson", offset = log(Population),
                control.compute = list(dic = TRUE, cpo = TRUE)))
  } else if (model_family == "nbinomial") {
    return(inla(fmla, data = data, family = "nbinomial", offset = log(Population),
                control.compute = list(dic = TRUE, cpo = TRUE)))
  }
}

vif.function <- function(data) {  # variance inflation factor
  return(vif(lm(Town.Death ~ ., data = data)))
}

## construct reference model
model_name <- c("poisson", "poisson.iid.county", "poisson.bym", "nbinomial", "nbinomial.iid.county", "nbinomial.bym")  # six model types
model_name <- c("poisson", "poisson.iid.region", "poisson.iid.county", "poisson.iid", "poisson.bym", "nbinomial", "nbinomial.iid.region", "nbinomial.iid.county", "nbinomial.iid", "nbinomial.bym")  
# xname_poisson <- c("Over65.Pct", "Minority.Pct", "BelowHS.Pct", "Med.Gross.Rent", "PD", "Job.Pct.HRisk", "PM25",
#                    "Ozone", "CrowdHouse.Pct", "Unemployed.Pct", "Job.Pct.Cnty")  # consider 11 or 10 variables in reference model
# xname_nbinomial <- c("Over65.Pct", "Minority.Pct", "BelowHS.Pct", "Med.Gross.Rent", "PD", "Job.Pct.HRisk", "PM25",
#                      "Ozone", "CrowdHouse.Pct", "Unemployed.Pct", "Job.Pct.Cnty")
xname_poisson <- c("Over65.Pct", "Minority.Pct", "BelowHS.Pct", "Med.Gross.Rent", "PD", "Job.Pct.HRisk", "PM25",
                   "Ozone", "CrowdHouse.Pct", "Unemployed.Pct")  # consider 11 or 10 variables in reference model
xname_nbinomial <- c("Over65.Pct", "Minority.Pct", "BelowHS.Pct", "Med.Gross.Rent", "PD", "Job.Pct.HRisk", "PM25",
                     "Ozone", "CrowdHouse.Pct", "Unemployed.Pct")

xname <- list()  # variables for six reference models
for (i in 1:length(model_name)) {
  if (str_split(model_name[i], "[.]", simplify = TRUE)[1] == "poisson") {
    xname[model_name[i]] = list(xname_poisson)
  } else if (str_split(model_name[i], "[.]", simplify = TRUE)[1] == "nbinomial")
    xname[model_name[i]] = list(xname_nbinomial)
}

xname_scale <- setdiff(names(data_select), c("Town.Death", "PM25", "NO2", "Ozone", "Acrolein.Conc", "Acetaldehyde.Conc", "Formaldehyde.Conc", "Diesel.PM10.Conc", "Naphthalene.Conc",
                                             "Population", "Centroid.X", "Centroid.Y", "County.Name", "Region.Name", "OBJECTID"))
data_select[, xname_scale] <- as.data.frame(apply(data_select[, xname_scale], 2, function(x) scale(x)))

mod_all <- list()
for (i in 1:length(model_name)) {
  fmla <- fmla.function(xname[[model_name[i]]], model_name[i])
  mod <- mod.function(fmla, data_select, model_name[i])
  #mod_cpo <- mod  # uncomment if inla.cpo fails
  mod_cpo <- inla.cpo(mod, mc.cores = 50)  # , force = TRUE
  mod_all[model_name[i]] <- list(list(variable = xname[[model_name[i]]],
                                      coefficent = data.frame(summary(mod)[["fixed"]]) %>%
                                        rownames_to_column(var = "Variable") %>%
                                        mutate(MRR = round(exp(mean), 3) , MRR_0.025 = round(exp(X0.025quant), 3), MRR_0.975 = round(exp(X0.975quant), 3)) %>%
                                        dplyr::select(Variable, MRR, MRR_0.025, MRR_0.975),
                                      dic = round(mod$dic$dic, 2),
                                      cpo = round(-2*sum(log(mod_cpo$cpo$cpo), na.rm = TRUE), 2),
                                      cpo.failure = sum(mod_cpo$cpo$failure)))
  print(i)
}

vis.coef <- function(data) {  # regression coefficients of the first 9 variables for six models
  coef <- data.frame()
  for (i in 1:length(model_name)) {
    for (j in 1:10) {  # first 10 variables
      coef <- bind_rows(coef, data.frame(Model = model_name[i],
                                         Variable = data[[model_name[i]]]$coefficent$Variable[j+1],
                                         Number = as.character(nrow(data[[model_name[i]]]$coefficent)-1),  # number of covariates
                                         Mean = data[[model_name[i]]]$coefficent$MRR[j+1],
                                         Lower = data[[model_name[i]]]$coefficent$MRR_0.025[j+1],
                                         Upper = data[[model_name[i]]]$coefficent$MRR_0.975[j+1],
                                         DIC = data[[model_name[i]]]$dic,
                                         CPO = data[[model_name[i]]]$cpo)) 
    }
  }
  return(coef)
}

data_visualize <- vis.coef(mod_all) %>%
  mutate(Mean_change = (Mean-1)*100, Lower_change = (Lower-1)*100, Upper_change = (Upper-1)*100)
distinct(data_visualize, Model, DIC, CPO)  # dic and cpo for six models
# write_csv(data_visualize, "Data/Output/ModelConstruct/coef_10variable_6refmodels.csv")

## calculate regression coefficient for each variable
model_name <- c("poisson", "poisson.iid.region", "poisson.bym", "nbinomial", "nbinomial.iid.region", "nbinomial.bym")  
model_name <- c("poisson", "poisson.iid.region", "poisson.iid.county", "poisson.iid", "poisson.bym", "nbinomial", "nbinomial.iid.region", "nbinomial.iid.county", "nbinomial.iid", "nbinomial.bym")  
xref_poisson <- c("Over65.Pct", "Minority.Pct", "BelowHS.Pct", "Med.Gross.Rent", "PD", "Job.Pct.HRisk", "PM25",
                   "Ozone", "CrowdHouse.Pct", "Unemployed.Pct")  # consider 11 or 10 variables in reference model
xref_nbinomial <- c("Over65.Pct", "Minority.Pct", "BelowHS.Pct", "Med.Gross.Rent", "PD", "Job.Pct.HRisk", "PM25",
                     "Ozone", "CrowdHouse.Pct", "Unemployed.Pct")
xtest <- names(data_select)[c(2:62)]  # all variables to be quantified
xname_scale <- setdiff(names(data_select), c("Town.Death", "PM25", "NO2", "Ozone", "Acrolein.Conc", "Acetaldehyde.Conc", "Formaldehyde.Conc", "Diesel.PM10.Conc", "Naphthalene.Conc",
                                             "Population", "Centroid.X", "Centroid.Y", "County.Name", "Region.Name", "OBJECTID"))
data_select[, xname_scale] <- as.data.frame(apply(data_select[, xname_scale], 2, function(x) scale(x)))

cor.remove <- function(xname_add, xname_ref) {  # remove inter-correlated variables
  cor_select <- abs(data_cor[xname_add, xname_ref, drop = FALSE])
  return(c(xname_add, names(cor_select )[cor_select <= 0.6]))
}

data_summary <- data_coef <- data.frame(Variable = xtest)
for (i in 1:length(model_name)) {
  tmp <- mclapply(1:length(xtest), function(j) {  # add a variable each time
    model_family <- str_split(model_name[i], "[.]", simplify = TRUE)[1]
    if (model_family == "poisson") { 
      xname <- cor.remove(xname_add = xtest[j], xname_ref = xref_poisson)
      fmla <- fmla.function(xname, model_name[i])
    } else if (model_family == "nbinomial") {
      xname <- cor.remove(xname_add = xtest[j], xname_ref = xref_nbinomial)
      fmla <- fmla.function(xname, model_name[i])
    }
    mod <- mod.function(fmla, data_select, model_name[i])  # new model for each variable
    tmp1 <- list(variable = xtest[j],
                 coefficent = data.frame(summary(mod)[["fixed"]]) %>%
                   rownames_to_column(var = "Variable") %>%
                   filter(Variable == xtest[j]) %>%
                   mutate(MRR = round(exp(mean), 3) , MRR_0.025 = round(exp(X0.025quant), 3), MRR_0.975 = round(exp(X0.975quant), 3)))  
  }, mc.cores = 50)
  
  tmp2 <- setNames(data.frame(matrix(NA, length(xtest), 1)), model_name[i])  # combine mean, upper and lower
  tmp3 <- setNames(data.frame(matrix(NA, length(xtest), 3)), as.vector(str_split(paste0(model_name[i], c("_mean", "_lower", "_upper"), collapse = " "), " ", simplify = TRUE)))  # separate mean, upper and lower
  for (k in 1:length(xtest)) {
    tmp2[k, 1] <- paste0(tmp[[k]][["coefficent"]]$MRR, " (", tmp[[k]][["coefficent"]]$MRR_0.025, ", ", tmp[[k]][["coefficent"]]$MRR_0.975, ")", sep = "")
    tmp3[k, 1] <- (tmp[[k]][["coefficent"]]$MRR-1) * 100
    tmp3[k, 2] <- (tmp[[k]][["coefficent"]]$MRR_0.025-1) * 100
    tmp3[k, 3] <- (tmp[[k]][["coefficent"]]$MRR_0.975-1) * 100
  }
  
  data_summary <- cbind(data_summary, tmp2)
  data_coef <- cbind(data_coef, tmp3)
  
  print(i)
}

data_summary1 <- data_summary
data_summary1$Variable <- c("% Elderly (>=65)", "% White", "% Below High School Education", "Median Gross Rent", "Population Density (Quantile)",
                            "Job.sum5.Pct" = "% High Risk Occupation", "PM25 Average Concentration", "Ozone Seasonal DM8HA", "% High Occupancy Residence",
                            "% Unemployment", "% Commute (To Different Counties)",
                            "Acrolein (0.1 HQ)", "Acetaldehyde (0.01 HQ)", "Diesel PM (0.1 HQ)", "Formaldehyde (0.01 HQ)", "Naphthalene (0.01 HQ)",
                            "Respiratory Hazard Index", "Inhalation Cancer Risk",
                            "% Black", "% Asian", "% Hispanic",
                            "% Population (Age<15)", "% Population (Age 45-64)", "% Population (Age 15-44)", "Median Age",
                            "% Below Poverty Level", "Median Household Income", "Income Per Capita", "Gini Index", "Median House Value",
                            "% Linguistic Isolate", "% Disability", "% Commute (Public Transport)", "% House Built Before 1960", "% Group Quarter Residence",
                            "Traffic Proximity", "Proximity to TWWD", "NPL Site Proximity", "RMP Facility Proximity", "TSDF Facility Proximity", "Proximity to Power Plant (Fossil Fuel)",
                            "% Uninsured", "% Medicare", "% Medicaid", "SVI (Overall)", "Noise Level")
names(data_summary1) <- c("Covariate",
                          "Poisson Regression", "Poisson Mixed Effect Model", "Poisson Besag-York-Mollie Spatial Model",
                          "Negative Binomial Regression", "Negative Binomial Mixed Effect Model", "Negative Binomial Besag-York-Mollie Spatial Model")

write_csv(data_summary, file = "Data/Output/ModelConstruct/coef_allvariable_6refmodel_10variable.csv")
write_csv(data_coef, file = "Data/Output/ModelConstruct/coef_allvariable_6refmodel_10variable_separate.csv")
write_csv(data_summary, file = "Data/Output/ModelConstruct/coef_allvariable_6refmodel_10variable_quantile.csv")
write_csv(data_coef, file = "Data/Output/ModelConstruct/coef_allvariable_6refmodel_10variable_separate_quantile.csv")
write_csv(data_summary, file = "Data/Output/ModelConstruct/coef_allvariable_6refmodel_10variable_final.csv")
write_csv(data_coef, file = "Data/Output/ModelConstruct/coef_allvariable_6refmodel_10variable_separate_final.csv")

# bar plot
data_all <- read_csv("Data/Output/ModelConstruct/coef_allvariable_6refmodel_10variable_separate_final.csv") %>%
  pivot_longer(cols = -c("Variable"), names_to = "Metric", values_to = "Value") %>%
  separate(col = Metric, into = c("Model", "Metric"), sep = "_") %>%
  pivot_wider(names_from = "Metric", values_from = "Value") %>%
  select(Model, Variable, Mean_change = mean, Lower_change = lower, Upper_change = upper) %>%
  mutate(Number = NA, Mean = NA, Lower = NA, Upper = NA, DIC = NA, CPO = NA, .before = Mean_change) %>%
  # filter(Variable %in% c("PD", "Minority.Pct", "LowIncome.Pct", "BelowHS.Pct", "CrowdHouse.Pct", "Job.Pct.HRisk", "PM25", "NO2", "Formaldehyde.Conc")) %>%
  # filter(Variable %in% c("Under15.Pct", "Age15.44.Pct", "Age45.64.Pct", "Over65.Pct")) %>%
  # filter(Variable %in% c("White.Pct", "Black.Pct", "Asian.Pct", "Hispanic.Pct")) %>%
  filter(Variable %in% c("PP.Prox.Gas", "PP.Prox.Petro", "PP.Prox.Biom", "PP.Prox.Coal", "PP.Prox.Nucl", "PP.Prox.WFF")) %>%
  filter(!Model %in% c("poisson.iid.county", "poisson.iid", "nbinomial.iid.county", "nbinomial.iid"))
data_all <- read_csv("Data/Output/ModelConstruct/coef_allvariable_6refmodel_10variable_separate_quantile.csv") %>%
  pivot_longer(cols = -c("Variable"), names_to = "Metric", values_to = "Value") %>%
  separate(col = Metric, into = c("Model", "Metric"), sep = "_") %>%
  pivot_wider(names_from = "Metric", values_from = "Value") %>%
  select(Model, Variable, Mean_change = mean, Lower_change = lower, Upper_change = upper) %>%
  mutate(Number = NA, Mean = NA, Lower = NA, Upper = NA, DIC = NA, CPO = NA, .before = Mean_change) %>%
  # filter(Variable %in% c("NPL.Prox", "RMP.Prox", "TSDF.Prox", "Wastewater.Prox", "Traffic.Prox", "PP.Prox.WFF")) %>%
  filter(Variable %in% c("PP.Prox.Gas", "PP.Prox.Petro", "PP.Prox.Biom", "PP.Prox.Coal", "PP.Prox.Nucl", "PP.Prox.WFF")) %>%
  filter(!Model %in% c("poisson.iid.county", "poisson.iid", "nbinomial.iid.county", "nbinomial.iid"))
data_all$Variable <- factor(data_all$Variable, 
                                  levels = c("PD", "Minority.Pct", "LowIncome.Pct", "BelowHS.Pct", "CrowdHouse.Pct", "Job.Pct.HRisk", "PM25", "NO2", "Formaldehyde.Conc"))
data_all$Variable <- factor(data_all$Variable,
                                  levels = c("Under15.Pct", "Age15.44.Pct", "Age45.64.Pct", "Over65.Pct"))
data_all$Variable <- factor(data_all$Variable,
                            levels = c("White.Pct", "Black.Pct", "Asian.Pct", "Hispanic.Pct"))
data_all$Variable <- factor(data_all$Variable,
                            levels = c("NPL.Prox", "RMP.Prox", "TSDF.Prox", "Wastewater.Prox", "Traffic.Prox", "PP.Prox.WFF"))
data_all$Variable <- factor(data_all$Variable,
                            levels = c("PP.Prox.Gas", "PP.Prox.Petro", "PP.Prox.Biom", "PP.Prox.Coal", "PP.Prox.Nucl", "PP.Prox.WFF"))
data_all$Model <- factor(data_all$Model, 
                               levels = c("poisson", "poisson.iid.region", "poisson.bym", "nbinomial", "nbinomial.iid.region", "nbinomial.bym"))

library(RColorBrewer)
col.poisson <- colorRampPalette(c("lightskyblue1", "dodgerblue3"))(3)
col.nbinomial <- colorRampPalette(c("lightgoldenrodyellow", "darkgoldenrod3"))(3)
ggplot(data = data_all, aes(x = Model, y = Mean_change, fill = Model)) +
  geom_bar(stat = "identity") +
  geom_errorbar(aes(ymin = Lower_change, ymax = Upper_change), width = 0.2, size = 1.1) +
  geom_abline(slope= 0, intercept = 0, linetype= "dashed") +
  scale_fill_manual(values = c("poisson" = col.poisson[1], "poisson.iid.region" = col.poisson[2], "poisson.bym" = col.poisson[3],
                               "nbinomial" = col.nbinomial[1], "nbinomial.iid.region" = col.nbinomial[2], "nbinomial.bym" = col.nbinomial[3]),
                    labels = c("Poisson Regression", "Poisson Mixed Effect Model", "Poisson Besag-York-Mollie Spatial Model",
                               "Negative Binomial Regression", "Negative Binomial Mixed Effect Model", "Negative Binomial Besag-York-Mollie Spatial Model")) +
  # facet_wrap(facets = ~ Variable, nrow = 3, ncol = 3,
  #            labeller = labeller(Variable = c("PD" = "Population Density (Quantile)",
  #                                             "Minority.Pct" = "% Minority",
  #                                             "LowIncome.Pct" = "% Below Poverty Level",
  #                                             "BelowHS.Pct" = "% Below High School Education",
  #                                             "CrowdHouse.Pct" = "% High Occupancy Residence",
  #                                             "Job.Pct.HRisk" = "% High Risk Occupation",
  #                                             "PM25" = "PM25 Average Concentration",
  #                                             "NO2" = "NO2 Average Concentration",
  #                                             "Formaldehyde.Conc" = "Formaldehyde (0.01 HQ)"
  #            ))) +
  
  # facet_wrap(facets = ~ Variable, nrow = 2, ncol = 2,
  #            labeller = labeller(Variable = c("Under15.Pct" = "% Population (Age<15)",
  #                                             "Age15.44.Pct" = "% Population (Age 15-44)",
  #                                             "Age45.64.Pct" = "% Population (Age 45-64)",
  #                                             "Over65.Pct" = "% Population (Age>64)"
  #            ))) +
  
  # facet_wrap(facets = ~ Variable, nrow = 2, ncol = 2,
  #            labeller = labeller(Variable = c("White.Pct" = "% Population (White)",
  #                                             "Black.Pct" = "% Population (Black)",
  #                                             "Asian.Pct" = "% Population (Asian)",
  #                                             "Hispanic.Pct" = "% Population (Hispanic)"
  #            ))) +
  
  # facet_wrap(facets = ~ Variable, nrow = 2, ncol = 3,
  #            labeller = labeller(Variable = c("NPL.Prox" = "NPL Site Proximity",
  #                                             "RMP.Prox" = "RMP Facility Proximity",
  #                                             "TSDF.Prox" = "TSDF Facility Proximity",
  #                                             "Wastewater.Prox" = "Proximity to TWWD",
  #                                             "Traffic.Prox" = "Traffic Proximity",
  #                                             "PP.Prox.WFF" = "Proximity to WFFEGU"
  #                                             
  #            ))) +
  
  facet_wrap(facets = ~ Variable, nrow = 2, ncol = 3,
             labeller = labeller(Variable = c("PP.Prox.Gas" = "Proximity to Natural Gas EGU",
                                              "PP.Prox.Petro" = "Proximity to Petroleum EGU",
                                              "PP.Prox.Biom" = "Proximity to Biomass EGU",
                                              "PP.Prox.Coal" = "Proximity to Coal EGU",
                                              "PP.Prox.Nucl" = "Proximity to Nuclear EGU",
                                              "PP.Prox.WFF" = "Proximity to WFFEGU"
             ))) +

  scale_x_discrete(name = "") + 
  # scale_y_continuous(name = "Mortality Rate Ratios (% Change)", limits = c(-20, 70), breaks = seq(-20, 70, by = 10)) +
  scale_y_continuous(name = "Mortality Rate Ratios (% Change)", limits = c(-40, 55), breaks = seq(-40, 50, by = 10)) +
  theme_bw() +
  theme(axis.text.x = element_blank(), axis.ticks.x = element_blank(),
        strip.text = element_text(face = "bold"),
        legend.position = "bottom", legend.title = element_blank(),
        panel.grid.major.x = element_blank(), panel.grid.minor = element_blank()) +
  guides(fill = guide_legend(nrow = 2, byrow = TRUE))

ggsave("Figure/Barplot/barplot_9variables.png", width = 9, height = 7, units = "in", dpi = 300)
ggsave("Figure/Barplot/barplot_4ages.png", width = 7, height = 5.5, units = "in", dpi = 300)
ggsave("Figure/Barplot/barplot_4races.png", width = 7, height = 5.5, units = "in", dpi = 300)
ggsave("Figure/Barplot/barplot_6proximity.png", width = 9, height = 6, units = "in", dpi = 300)




































































