# This code is to select variables/models for COVID-19 association studies
# Xiang Ren, 2021-12-23

rm(list = ls())
setwd("/projectsp/f_panosg/COVID-19/Association_Study/ForManuscript/Data/")

needed_packages <- c("dplyr", "readr", "tidyr", "tibble", "ggplot2", "stringr", "car", "INLA")
lapply(needed_packages, require, character.only = TRUE)

## generate data for modeling (not run again)
# data_old <- read_csv("/projects/f_panosg/COVID-19/Association_Study/SecondPhase/Data/data_NJMunicipality_Modeling_secondphase.csv")
object_id <- read_csv("/projects/f_panosg/COVID-19/Association_Study/SecondPhase/Data/data_NJMunicipality_Modeling_secondphase.csv") %>%  # old data for modeling
  select(c("Centroid.X", "Centroid.Y", "County.Name", "OBJECTID"))  # 565*4
data_model <- read_csv("data_municipality_raw.csv") %>%
  mutate(Acetaldehyde.Conc = Acetaldehyde.Conc/9*10,
         Acrolein.Conc = Acrolein.Conc/0.035*10,
         Diesel.PM10.Conc = Diesel.PM10.Conc/5*10^2,
         Formaldehyde.Conc = Formaldehyde.Conc/9.8*10,
         Naphthalene.Conc = Naphthalene.Conc/3*10^2) %>%  # hazard quotient for five air toxic
  left_join(object_id, by = c("Centroid.X", "Centroid.Y", "County.Name")) %>%  # include objectid for geospatial modeling
  select(c(11, 58:65, 81:94, 19:57, 6:9, 95, 10))  # 565*68
# write_csv(data_model, file = "data_municipality_model.csv")

## modify data for modeling
data_model <- read_csv("data_municipality_model.csv") %>%
  filter(Population > 10) %>%  # remove three municipalities with population below 10, 562*68
  drop_na(Town.Death) %>%  # remove missing data in Town.Death
  mutate(Restaurant.PerCapita = replace(Restaurant.PerCapita, is.na(Restaurant.PerCapita), quantile(Restaurant.PerCapita, 0.5, na.rm = TRUE)),
         Grocery.PerCapita = replace(Grocery.PerCapita, is.na(Grocery.PerCapita), quantile(Grocery.PerCapita, 0.5, na.rm = TRUE)),
         Job.Pct.NYC = replace(Job.Pct.NYC, is.na(Job.Pct.NYC), quantile(Job.Pct.NYC, 0.5, na.rm = TRUE)))  # replace mising data in three columns with median, 355*68

PD_cut <- quantile(data_model$PD, c(0.2, 0.4, 0.6, 0.8))  # quantile cut for PD
data_select <- data_model %>%
  mutate(Acetaldehyde.Conc = Acetaldehyde.Conc * 10,  # 0.01HQ, rescale three air toxic
         Diesel.PM10.Conc = Diesel.PM10.Conc / 10,  # 0.1HQ
         Formaldehyde.Conc = Formaldehyde.Conc * 10) %>%  # 0.01HQ
  mutate(PD = case_when(PD <= PD_cut[1] ~ 0,
                        (PD > PD_cut[1]) & (PD <= PD_cut[2]) ~ 1,
                        (PD > PD_cut[2]) & (PD <= PD_cut[3]) ~ 2,
                        (PD > PD_cut[3]) & (PD <= PD_cut[4]) ~ 3,
                        TRUE ~ 4))  # quantile transformation for PD
data_cor <- as.data.frame(cor(data_select[, 1:62]))  # Pearson correlation matrix

for (i in 1:62) {
  ggplot(data = data_model, aes(x = get(names(data_model)[i]))) +
    geom_histogram(breaks = seq(min(data_model[, i]), max(data_model[, i]), length.out = 30)) +
    labs(x = names(data_model)[i]) +
    theme_bw()
  ggsave(filename = paste0("/projectsp/f_panosg/COVID-19/Association_Study/ForManuscript/Figure/Histogram/", names(data_model)[i], ".png", sep = ""), width = 5, height = 4, units = "in", dpi = 300)
}

## variable division
cor_summary <- list()  # list of dataframes with highly correlated variables with each variable
for (i in 1:nrow(data_cor)) {
  idx <- which((data_cor[, i] != 1) & (abs(data_cor[, i]) >= 0.6))
  value <- round(data_cor[idx, i], 2) %>%
    setNames(., rownames(data_cor)[idx]) %>%
    as.data.frame()
  cor_summary[[names(data_cor)[i]]] <- value
}

xname_init <- c("Over65.Pct", "Minority.Pct", "BelowHS.Pct", "Med.Gross.Rent", "PD", "Job.Pct.HRisk", "PM25")  # initial set with 7 variables
xname_cand <- setdiff(setdiff(names(data_select), xname_init), c("Town.Death", "Population", "Centroid.X", "Centroid.Y", "County.Name", "Region.Name", "OBJECTID",
                                                                 "Acrolein.Conc", "Acetaldehyde.Conc", "Formaldehyde.Conc", "Naphthalene.Conc", "Diesel.PM10.Conc",
                                                                 "PP.Prox.Gas", "PP.Prox.Petro", "PP.Prox.Biom", "PP.Prox.Coal", "PP.Prox.Nucl",
                                                                 "Black.Pct", "Asian.Pct", "Hispanic.Pct", "White.Pct",
                                                                 "Job.Pct.Wholesale", "Job.Pct.Retail", "Job.Pct.Transport", "Job.Pct.Healthcare", "Job.Pct.Foodservice",
                                                                 "SVI.Socioeconomic", "SVI.Disability", "SVI.Minority", "SVI.House",
                                                                 "Restaurant.PerCapita", "Grocery.PerCapita",
                                                                 "Age45.64.Pct", "Age15.44.Pct", "Under15.Pct",
                                                                 "HouseBelow1960.Pct"))  # "Resp.Risk", "Cancer.Risk", "Linguistic.Iso.Pct", "Unemployed.Pct", "Job.Pct.NYC", candidate set with 25 variables
xname_scale <- setdiff(names(data_select), c("Town.Death", "PM25", "NO2", "Ozone", "Acrolein.Conc", "Acetaldehyde.Conc", "Formaldehyde.Conc", "Diesel.PM10.Conc", "Naphthalene.Conc",
                                             "Population", "Centroid.X", "Centroid.Y", "County.Name", "Region.Name", "OBJECTID"))
data_select[, xname_scale] <- as.data.frame(apply(data_select[, xname_scale], 2, function(x) scale(x)))

## variable selection
model_type <- "nbinomial"  # select model type
fmla.function <- function(variable, model_type) {  # formula for different model types
  if (model_type %in% c("poisson", "nbinomial")) {
    as.formula(paste("Town.Death ~ ", paste(variable, collapse = "+")))
  } else if (model_type %in% c("poisson.iid", "nbinomial.iid")) {
    update(as.formula(paste("Town.Death ~ ", paste(variable, collapse = "+"))), . ~ . + 
             f(OBJECTID, model = "iid"))
  } else if (model_type %in% c("poisson.bym", "nbinomial.bym")) {
    update(as.formula(paste("Town.Death ~ ", paste(variable, collapse = "+"))), . ~ . + 
             f(OBJECTID, model = "bym", graph = "municipality_adjacency.graph"))
  } else if (model_type == "poisson.iid.prec1") {
    update(as.formula(paste("Town.Death ~ ", paste(variable, collapse = "+"))), . ~ . + 
             f(OBJECTID, model = "iid", hyper = list(Prec = list(prior = "loggamma", param = c(1, 0.1)))))
  } else if (model_type == "poisson.iid.prec2") {
    update(as.formula(paste("Town.Death ~ ", paste(variable, collapse = "+"))), . ~ . + 
             f(OBJECTID, model = "iid", hyper = list(Prec = list(prior = "loggamma", param = c(2000, 10)))))
  }
}

model_family <- str_split(model_type, "[.]", simplify = TRUE)[1]
mod.function <- function(fmla, data) {  # run model for different family types
  if (model_family == "poisson") { 
    return(inla(fmla, data = data, family = "poisson", offset = log(Population),
                control.compute = list(dic = TRUE, cpo = TRUE)))
  } else if (model_family == "nbinomial") {
    return(inla(fmla, data = data, family = "nbinomial", offset = log(Population),
                control.compute = list(dic = TRUE, cpo = TRUE)))
  }
}

vif.function <- function(data) {  # calculate variance inflation factor
  if (ncol(data) <= 2) {
    return(NA)
  } else {
    return(vif(lm(Town.Death ~ ., data = data)))
  }
}

cor.remove <- function(xname_init, xname_cand) {  # select candidate variables with correlation below 0.6 to initial variables
  cor_select <- abs(data_cor[xname_init, xname_cand, drop = FALSE]) %>%
    apply(., 2, max)
  return(names(cor_select )[which(cor_select <= 0.6)])
}

iter.summary <- function(x) {  # return summary report for each iteration
  data_summary <- data.frame()
  for (i in 1:(length(x)-1)) {
    if (class(try(x[[i]])) == "try-error") {  # for aliased coefficients in the model
      data_summary <- bind_rows(data_summary, data.frame(vif = 100000,
                                                         dic = 100000,
                                                         cpo = 100000,
                                                         cpo.failure = 100000))
    } else {
      data_summary <- bind_rows(data_summary, data.frame(vif = x[[i]][["vif"]][x[[i]][["variable.add"]]],
                                                         dic = x[[i]][["dic"]],
                                                         cpo = x[[i]][["cpo"]],
                                                         cpo.failure = x[[i]][["cpo.failure"]]))
    }
    
  }
  data_summary <- bind_rows(data_summary, data.frame(vif = NA,
                                                     dic = x[[length(x)]][["dic"]],
                                                     cpo = x[[length(x)]][["cpo"]],
                                                     cpo.failure = x[[length(x)]][["cpo.failure"]]))
  rownames(data_summary)[length(x)] <- "Base"
  return(data_summary)
}

iter_all <- list()
iter_optim_path <- data.frame()
for (i in 1:length(xname_cand)) {
  xname_cand <- cor.remove(xname_init, xname_cand)  # determine candidate variables with correlation below 0.6
  tmp <- mclapply(1:length(xname_cand), function(j) {  # regression result for each candidate variable
    fmla_add <- fmla.function(c(xname_init, xname_cand[j]), model_type)
    mod_add <- mod.function(fmla_add, data_select)
    tmp1 <- list(variable.add = xname_cand[j], 
                 vif = round(vif.function(data_select[, c("Town.Death", xname_init, xname_cand[j])]), 2),
                 coefficent = data.frame(summary(mod_add)[["fixed"]]) %>%
                   rownames_to_column(var = "Variable") %>%
                   mutate(MRR = round(exp(mean), 3) , MRR_0.025 = round(exp(X0.025quant), 3), MRR_0.975 = round(exp(X0.975quant), 3)) %>%
                   dplyr::select(Variable, sd, MRR, MRR_0.025, MRR_0.975),
                 dic = round(mod_add$dic$dic, 2),
                 cpo = round(-2*sum(log(mod_add$cpo$cpo)), 2),
                 cpo.failure = sum(mod_add$cpo$failure))
  }, mc.cores = 48)
  
  fmla_base <- fmla.function(xname_init, model_type)  # regression result for current base model
  mod_base <- mod.function(fmla_base, data_select)
  tmp[[length(xname_cand)+1]] <- list(variable.add = xname_init,
                                      vif = round(vif.function(data_select[, c("Town.Death", xname_init)]), 2),
                                      coefficent = data.frame(summary(mod_base)[["fixed"]]) %>%
                                        rownames_to_column(var = "Variable") %>%
                                        mutate(MRR = round(exp(mean), 3) , MRR_0.025 = round(exp(X0.025quant), 3), MRR_0.975 = round(exp(X0.975quant), 3)) %>%
                                        dplyr::select(Variable, sd, MRR, MRR_0.025, MRR_0.975),
                                      dic = round(mod_base$dic$dic, 2),
                                      cpo = round(-2*sum(log(mod_base$cpo$cpo)), 2),
                                      cpo.failure = sum(mod_base$cpo$failure))
  
  data_summary1 <- iter.summary(tmp)  # return dataframe format
  data_summary <- filter(data_summary1, cpo <= data_summary1$cpo[length(tmp)]) %>%  # select variables with cpo smaller than current base model
    arrange(cpo)
  
  iter_all[i] <- list(tmp)
  iter_optim_path <- rbind(iter_optim_path, data.frame(Iter = i, Variable.Order = which(rownames(data_summary1) == rownames(data_summary)[1])))  # select the variable with minimum cpo
  saveRDS(list(iter_all = iter_all, iter_optim_path = iter_optim_path), file = paste("Output/ModelSelection/iter-result_", model_type, "_minority.pct.rds", sep = ""))
  if (nrow(data_summary) < 2) {
    break
  }
  xname_cand <- setdiff(xname_cand, rownames(data_summary)[1])  # update xname_cand and xname_init
  xname_init <- c(xname_init, rownames(data_summary)[1])
  
  print(i)
}



















