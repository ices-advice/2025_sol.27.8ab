###  Prepare tables for report

###  Before: catage_full.csv, catch_sop.csv, datage_full.csv, ibts1.csv,
###          ibts3.csv, latage_full.csv, maturity.csv, natmort.csv,
###          wcatch_full.csv, wdiscards_full.csv, wlandings_full.csv (data),
###          catage_fit.csv, catch_est.csv, fatage.csv, multiplier.csv,
###          natage.csv, summary.csv (output)
###  After:  catage.csv, catage_fit.csv, catch_est.csv, catch_sop.csv, datage.csv,
###          fatage.csv, ibts1.csv, ibts3.csv, latage.csv, maturity.csv,
###          multiplier.csv, natage.csv, natmort.csv, summary.csv, wcatch.csv,
###          wdiscards.csv, wlandings.csv (report)

###  latage (trim age, round, transpose)
latage <- read.taf("data/latage.csv")
names(latage)[ncol(latage)] <- "+gp"
latage <- round(latage)
latage <- tt(latage, TRUE)
write.taf(latage, dir = "report")

###  catage (trim age, round, transpose)
catage <- read.taf("data/catage.csv")
names(catage)[ncol(catage)] <- "+gp"
catage <- round(catage)
catage <- tt(catage, TRUE)
write.taf(catage, dir = "report")

###  wlandings (trim age, round, transpose)
wlandings <- read.taf("data/wlandings.csv")
names(wlandings)[ncol(wlandings)] <- "+gp"
wlandings <- round(wlandings, 3)
wlandings <- tt(wlandings, TRUE)
write.taf(wlandings, dir = "report")

###  wcatch (trim age, round, transpose)
wcatch <- read.taf("data/wcatch.csv")
names(wcatch)[ncol(wcatch)] <- "+gp"
wcatch <- round(wcatch, 3)
wcatch <- tt(wcatch, TRUE)
write.taf(wcatch, dir = "report")

###  catch.sop (round)
# catch_sop <- read.taf("data/catch_sop.csv")
# catch_sop <- round(catch_sop)
# write.taf(catch.sop, dir = "report")

###  maturity (round)
maturity <- read.taf("data/maturity.csv")
maturity <- round(maturity, 3)
write.taf(maturity, dir = "report")

###  natmort (round)
natmort <- read.taf("data/natmort.csv")
natmort <- round(natmort, 3)
write.taf(natmort, dir = "report")

###  id_fr_trawler (round)
id_fr_trawler <- read.taf("data/id_fr_trawler.csv")
id_fr_trawler <- round(id_fr_trawler, 2)
write.taf(id_fr_trawler, dir = "report")

###  id_fr_bts_viii (round)
id_fr_bts_viii <- read.taf("data/id_fr_bts_viii.csv")
id_fr_bts_viii <- round(id_fr_bts_viii, 2)
write.taf(id_fr_bts_viii, dir = "report")

###-----------------------------------------------------------------------------
###  outputs tables
###-----------------------------------------------------------------------------
###  fatage (average, round)
fatage <- read.taf("output/fatage.csv")
fatage <- round(fatage, 3)
write.taf(fatage, dir = "report")

###  natage (sum, round)
natage <- read.taf("output/natage.csv")
natage$Total <- rowSums(natage)
natage <- round(natage)
write.taf(natage, dir = "report")

### summary table for report and advice sheet (table 6)
year_vec <- as.data.frame(sam_stock@catch) %>%
  pull(year) %>%
  unique()

summuary_table <- data.frame(Year = year_vec,
                             `Recruitment (age 2)` = round(as.vector(sam_stock@stock.n["2",]), digits = 0),
                             SSB = round(as.vector(ssb(sam_stock)), digits = 0),
                             # Catch = round(as.vector(catch(sam_stock)), digits = 0),
                             Landings = round(as.vector(landings(sam_stock)), digits = 0),
                             `Discards **` = round(as.vector(discards(sam_stock)), digits = 0),
                             `F ages 3–7` = round(as.vector(fbar(sam_stock)), digits = 2))

summuary_table_assessment_year <- data.frame(Year = assessment_year,
                                             `Recruitment (age 2)` = recruits_assessment_year,
                                             SSB = ssb_assessment_year,
                                             # Catch = NA,
                                             Landings = NA,
                                             `Discards **` = NA,
                                             `F ages 3–7` = NA)

summuary_table <- bind_rows(summuary_table, summuary_table_assessment_year)
write.taf(summuary_table, dir = "report")

###-----------------------------------------------------------------------------
### Read data sam and config sam to make tables
data_sam <- read_rds(file = "model/data_sam.rds")
conf <- stockassessment::loadConf(dat = data_sam,
                                  file = "model/model.cfg")

###-----------------------------------------------------------------------------
### Catcahbility parameters
df_catchability <- data.frame(Estimats = coef(fit_sam)[names( coef(fit_sam)) == "logFpar"],
                              SD = attr(coef(fit_sam),"sd")[names(coef(fit_sam)) == "logFpar"],
                              Age = conf$keyLogFpar[2, 1:length(coef(fit_sam)[names( coef(fit_sam)) == "logFpar"])] + 2,
                              Index = c(rep("FR BTS VIII", 5), "FR TRAWLER"))

df_catchability$Age[5] <- "6 7" ## BTS-VIII age 6 and 7 share the same Q
df_catchability$Age[6] <- "FSB" ## No age for FR-TRAWLER but a FSB index

saveRDS(df_catchability, file = "report/df_catchability.rds")

###-----------------------------------------------------------------------------
df_summuary_table <- data.frame(Year = year_vec,
                                `Recruitment` = round(as.vector(sam_stock@stock.n["2",]), digits = 0),
                                SSB = round(as.vector(ssb(sam_stock)), digits = 0),
                                Catch = round(as.vector(catch(sam_stock)), digits = 0),
                                Landings = round(as.vector(landings(sam_stock)), digits = 0),
                                `Discards` = round(as.vector(discards(sam_stock)), digits = 0),
                                `F` = round(as.vector(fbar(sam_stock)), digits = 2))

saveRDS(df_summuary_table, file = "report/df_summuary_table.rds")

###-----------------------------------------------------------------------------
### Report table with confidence intervals (Table 9 advice sheet)
df_summuary_table_conf <- data.frame(Year = c(year_vec, assessment_year),
                                     Low_Recruitment = c(df_rec$Low, NA),
                                     Recruitment = c(df_rec$Estimate, recruits_assessment_year),
                                     High_Recruitment = c(df_rec$High, NA),
                                     Low_StockSize = c(df_ssb$Low, df_sam_stf_ssb$low),
                                     StockSize = c(df_ssb$Estimate, df_sam_stf_ssb$median),
                                     High_StockSize = c(df_ssb$High, df_sam_stf_ssb$high),
                                     Landings = c(landings(flr_stock)[, ac(1984:(assessment_year - 1))],NA),
                                     Discards = c(discards(flr_stock)[, ac(1984:(assessment_year - 1))],NA),
                                     # Low_TBiomass = c(df_tsb$Low, df_sam_stf_tsb$low),
                                     # TBiomass = c(df_tsb$Estimate, df_sam_stf_tsb$median),
                                     # High_TBiomass = c(df_tsb$High, df_sam_stf_tsb$high),
                                     Low_FishingPressure = c(df_fbar$Low, NA),
                                     FishingPressure = c(df_fbar$Estimate, NA),
                                     High_FishingPressure = c(df_fbar$High, NA),
                                     Catches = c(catch(sam_stock)[,ac(1984:(assessment_year - 1))], NA)
                                    ) %>%
  mutate(across(contains("Recruitment"), ~ as.character(round(., digits = 0))),
         across(contains("Catches"), ~ as.character(round(., digits = 0))),
         across(contains("StockSize"), ~ as.character(round(., digits = 0))),
         across(contains("Landings"), ~ as.character(round(., digits = 0))),
         across(contains("Discards"), ~ as.character(round(., digits = 0))),
         across(contains("FishingPressure"), ~ as.character(round(., digits = 2)))) %>%
  mutate_if(is.numeric, round)

saveRDS(df_summuary_table_conf, file = "report/df_summuary_table_conf.rds")
write.taf(df_summuary_table_conf, dir = "report")

###-----------------------------------------------------------------------------
### Report table with confidence intervals at 90% (Table 9 advice sheet)
df_summuary_table_conf <- data.frame(Year = c(year_vec, assessment_year),
                                     Low_Recruitment = c(df_rec$Low, NA),
                                     Recruitment = c(df_rec$Estimate, recruits_assessment_year),
                                     High_Recruitment = c(df_rec$High, NA),
                                     Low_StockSize = c(df_ssb$Low, df_sam_stf_ssb$low),
                                     StockSize = c(df_ssb$Estimate, df_sam_stf_ssb$median),
                                     High_StockSize = c(df_ssb$High, df_sam_stf_ssb$high),
                                     Landings = c(landings(flr_stock)[, ac(1984:(assessment_year - 1))],NA),
                                     Discards = c(discards(flr_stock)[, ac(1984:(assessment_year - 1))],NA),
                                     # Low_TBiomass = c(df_tsb$Low, df_sam_stf_tsb$low),
                                     # TBiomass = c(df_tsb$Estimate, df_sam_stf_tsb$median),
                                     # High_TBiomass = c(df_tsb$High, df_sam_stf_tsb$high),
                                     Low_FishingPressure = c(df_fbar$Low, NA),
                                     FishingPressure = c(df_fbar$Estimate, NA),
                                     High_FishingPressure = c(df_fbar$High, NA),
                                     Catches = c(catch(sam_stock)[,ac(1984:(assessment_year - 1))], NA)
) %>%
  mutate(across(contains("Recruitment"), ~ as.character(round(., digits = 0))),
         across(contains("Catches"), ~ as.character(round(., digits = 0))),
         across(contains("StockSize"), ~ as.character(round(., digits = 0))),
         across(contains("Landings"), ~ as.character(round(., digits = 0))),
         across(contains("Discards"), ~ as.character(round(., digits = 0))),
         across(contains("FishingPressure"), ~ as.character(round(., digits = 2)))) %>%
  mutate_if(is.numeric, round)

saveRDS(df_summuary_table_conf, file = "report/df_summuary_table_conf.rds")
write.taf(df_summuary_table_conf, dir = "report")
