## Run analysis, write model results using SAM model

## Before: data as matrix
## After:
## data file data_sam.rds in the /model directory
## config SAM file model.cfg in the /model directory
## fit SAM file sam_model_fit.rds in the /model directory

library("stockassessment")

mkdir("model")

###-----------------------------------------------------------------------------
### Make data for SAM
###-----------------------------------------------------------------------------
n_landings_at_age_sam <- flr2sam(flr_stock@landings.n)
n_discards_at_age_sam <- flr2sam(flr_stock@discards.n)
n_catch_at_age_sam <- flr2sam(flr_stock@catch.n)

wt_landings_sam <- flr2sam(flr_stock@landings.wt)
wt_discards_sam <- flr2sam(flr_stock@discards.wt)
wt_catch_sam <- flr2sam(flr_stock@catch.wt)
wt_stock_sam <- flr2sam(flr_stock@stock.wt)

maturity_sam <- flr2sam(flr_stock@mat)
natural_mortality_sam <- flr2sam(flr_stock@m)

###-----------------------------------------------------------------------------
###
land_frac_sam <- n_landings_at_age_sam / n_catch_at_age_sam
# land_frac_sam[is.na(land_frac_sam)] <- 0

# Proportion of F before Spawning
prop_f_sam <- matrix(0.0,
                     nrow = dim(n_landings_at_age_sam)[1],
                     ncol = dim(n_landings_at_age_sam)[2],
                     dimnames = list(min_assessment_year:data_year,
                                     min_age:max_age))

#Proportion of M before Spawning
prop_m_sam <- matrix(0.0,
                     nrow = dim(n_landings_at_age_sam)[1],
                     ncol = dim(n_landings_at_age_sam)[2],
                     dimnames = list(min_assessment_year:data_year,
                                     min_age:max_age))

###-----------------------------------------------------------------------------
data_sam <- setup.sam.data(
  surveys = sole_indices_sam_list, # tuning fleets
  residual.fleet = n_catch_at_age_sam, # catch numbers-at-age
  prop.mature = maturity_sam, # maturity ogive
  prop.f = prop_f_sam, # proportion fished before spawning
  prop.m = prop_m_sam, # proportion natural mortality before spawning
  natural.mortality = natural_mortality_sam, # natural mortality
  land.frac = land_frac_sam, # landing proportion
  stock.mean.weight = wt_stock_sam, # stock weights
  catch.mean.weight = wt_catch_sam, # catch weights
  dis.mean.weight = wt_discards_sam,# discard weights
  land.mean.weight = wt_landings_sam # landing weights
)

saveRDS(data_sam,
        file = "model/data_sam.rds")

###-----------------------------------------------------------------------------
### SAM config as validated during the 2024 FLATFISH Benchmark
###-----------------------------------------------------------------------------
n_age <- length(min_age:max_age)

# Create a default parameter configuration object
conf <- defcon(data_sam)

# //1// MinAge, maxAge, maxAgePlusGroup
conf$minAge <- min_age
conf$maxAge <- max_age

# //3// Correlation between F-at-age
# Correlation of fishing mortality across ages: independent
conf$corFlag <- 0

# //4// Number of parameters in the survey processes -
# coupling of the survey catchability parameters (1st row not used)
conf$keyLogFpar[2, ] <- c(0, 1, 2, 3, 4, 4, -1)
conf$keyLogFpar[3, 1] <- max(conf$keyLogFpar[2, ]) + 1

# //5// Density dependent catchability power parameters (if any)
conf$keyQpow[3, 1] <- 0

# //8// Coupling of the variance parameters on the observations
conf$keyVarObs
conf$keyVarObs[1, ] <- rep(0, n_age )
conf$keyVarObs[2, ] <- c(1, 1, 2, 3, 4, 4, -1)
conf$keyVarObs[3, 1] <- max(conf$keyVarObs[2, ]) + 1

# //15// Define the fbar range
conf$fbarRange <- c(3, 7)

# //16// Biomass or catch survey for the tuning series
# 2 = FSB index
conf$keyBiomassTreat <- c(-1, -1, 2)

# //17// Set to 0 as default
conf$fleetTypes <-  c(0, 0, 0)

## save config
saveConf(conf,
         file = "model/model.cfg",
         overwrite = TRUE)

###-----------------------------------------------------------------------------
### SAM fit
###-----------------------------------------------------------------------------
par <- defpar(data_sam, conf)

fit_sam <- sam.fit(data_sam,
                   conf, par,
                   fullDerived = TRUE)

print(fit_sam$opt$convergence)
print(fit_sam$opt$message)

### save model results
saveRDS(fit_sam,
        file = "model/sam_model_fit.rds")

### save list of data, config and model results
saveRDS(list(data_sam = data_sam,
             conf_sam = conf,
             fit_sam = fit_sam),
        file = "model/sam_ouputs.rds")
