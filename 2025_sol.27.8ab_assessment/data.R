## Preprocess data, write TAF data tables

## Before: stock.RDS and indices.RDS
## After: catage.csv, id_FR_SABLES.csv, id_FR_ROCHELLE.csv, id_FR_BB_IN_Q4.csv,
## id_FR_BB_OFF_Q2.csv, id_FR_ORHAGO.csv, latage.csv, maturity.csv, natmort.csv,
## wcatch.csv, wlandings.csv, wstock.csv

library("icesTAF")
library("tidyverse")
source("utilities_data.R")
icesTAF::taf.bootstrap()

### Write tables to data directory
mkdir("data")

### Time serie range
assessment_year <- 2025
data_year <- assessment_year - 1
min_assessment_year <- 1984

### Age range
min_age <- 2
max_age <- 8 # plus group

###-----------------------------------------------------------------------------
### Load data

### Load stock data as FLR object
flr_stock <- readRDS(file = "bootstrap/data/stock.rds")

### Load indices data as FLR object
id_fr_trawler <- readRDS("bootstrap/data/sole_lpues_ott_otb.rds")
id_fr_bts_viii <- readRDS("bootstrap/data/sole_indices_bts_viii.rds")
sole_indices_sam_list <- list(`FR-BTS-VIII` = id_fr_bts_viii,
                              `FR-TRAWLER` = id_fr_trawler)
attr(sole_indices_sam_list$`FR-TRAWLER`, "time") <- c(0.1, 2/12)  # February
attr(sole_indices_sam_list$`FR-BTS-VIII`, "time") <- c(11/12, 1) # November - December

write.taf(id_fr_trawler, file = "id_fr_trawler.csv", dir = "data")
write.taf(id_fr_bts_viii,  file = "id_fr_bts_viii.csv", dir = "data")

###-----------------------------------------------------------------------------
### Reference points from 2024 FLATFISH Benchmark
ref_points <- list(Fpa = 0.43,
                   Flim = 0.62,
                   Fmsy = 0.31,
                   Fmsyupper = 0.38,
                   Fmsylower = 0.20,
                   Blim = 7470,
                   Bpa = 10460,
                   MSYBtrigger = 10460)

df_ref_points_f <- data.frame(variable = c("Fpa", "Flim", "Fmsy", "Fmsyupper", "Fmsylower"),
                              value = c(0.43, 0.62, 0.31, 0.38, 0.20))

df_ref_points_biomass <- data.frame(variable = c("Blim", "Bpa", "MSYBtrigger"),
                                    value = c(7470, 10460, 10460))

###-----------------------------------------------------------------------------
### Prepare tables for export
##-----------------------------------------------------------------------------


latage <- flr2taf(flr_stock@landings.n)
datage <- flr2taf(flr_stock@discards.n)
catage <- flr2taf(flr_stock@catch.n)
wlandings <- flr2taf(flr_stock@landings.wt)
wcatch <- flr2taf(flr_stock@catch.wt)
wdiscards <- flr2taf(flr_stock@discards.wt)
wstock <- flr2taf(flr_stock@stock.wt)
maturity <- flr2taf(flr_stock@mat)
natmort <- flr2taf(flr_stock@m)

df_catch_obs <- flr2sam(flr_stock@catch) %>%
  as.data.frame() %>%
  rownames_to_column() %>%
  rename(Year = rowname,
         Catch = Value)


write.taf(plus(latage), file = "latage.csv", dir = "data")
write.taf( plus(catage),  file = "catage.csv", dir = "data")
write.taf( plus(datage),  file = "datage.csv", dir = "data")
write.taf(plus(wlandings),  file = "wlandings.csv", dir = "data")
write.taf(plus(wcatch),  file = "wcatch.csv", dir = "data")
write.taf(plus(wstock),  file = "wstock.csv", dir = "data")
write.taf(plus(maturity), file = "maturity.csv", dir = "data")
write.taf(plus(natmort), file = "natmort.csv", dir = "data")
write.taf(flr2taf(id_fr_bts_viii), file = "id_fr_bts_viii.csv", dir = "data")
write.taf(flr2taf(id_fr_trawler), file = "id_fr_trawler.csv", dir = "data")
write.taf(df_catch_obs, file = "df_catch_obs.csv", dir = "data")
saveRDS(df_catch_obs, file = "data/df_catch_obs.rds")

###-----------------------------------------------------------------------------
### load TAC value from data_year and assessment_year for short time forecasting
df_TAC <- read.csv(file = "bootstrap/data/df_TAC.csv")
df_advice_MAP <- read.csv(file = "bootstrap/data/df_advice_MAP.csv")
