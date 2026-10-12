### -----------------------------------------------------------------------------
###   Short time forecasting
### -----------------------------------------------------------------------------

library("stockassessment")
library("TMB")
set.seed(12345)

### -----------------------------------------------------------------------------
### Set parameters for forecasting
last_projection_year <- assessment_year + 2
last_stock_year <- range(sam_stock)[["maxyear"]]
intermediate_year <- assessment_year
forcasting_year <- seq(assessment_year, assessment_year + 2)
advice_year <- assessment_year + 1

### set TAC: Last advised TAC
## advice made on catches (increasing with the DR)
TAC_data_year <- filter(df_TAC, year == (assessment_year - 1)) %>%
  pull(TAC)

### catch in working_year from Table 7 of the advice sheet
TAC_assessment_year <- filter(df_TAC, year == (assessment_year)) %>%
  pull(TAC)

### extract f bar status quo
F_bar_status_quo <- fbar(sam_stock)[, ac(last_stock_year+ (-2:0))] %>%
  mean() %>%
  round(., digits = 2)

### TAC constraints
TAC_Constraint <- round(TAC_assessment_year)

###-----------------------------------------------------------------------------
## Forecast settings
stock_weights_avg_years <- last_stock_year + (-2:0)
SSB_final <- ssbtable(fit_sam)[fit_sam$data$years == last_stock_year, 1]
nr_sims <- 1000
deterministic_forecast <- FALSE

###-----------------------------------------------------------------------------
### set recruitment after WGBIE decision see stock annex updated in 2024
Recr_start_year <- assessment_year - 5
Recr_end_year <- assessment_year - 2
Recr_years <- Recr_start_year:Recr_end_year

###-----------------------------------------------------------------------------
### F intermediate year to compute SSB in assessment_year + 1 for making STF
### and choose the right F to make projection

stf_intermediate_year <- stockassessment::forecast(fit_sam,
                                                   catchval.exact = c(NA, NA, NA), # in case of TAC constr option: fill in 'catch_int_year'
                                                   fscale = c(NA, NA, NA),    # insert 1 here is same as put Flast in fval -> no not the case
                                                   fval = c(F_bar_status_quo, F_bar_status_quo, F_bar_status_quo),
                                                   nosim = nr_sims,
                                                   year.base = last_stock_year,
                                                   ave.years = stock_weights_avg_years,
                                                   rec.years = Recr_years,
                                                   deterministic = deterministic_forecast,
                                                   useSWmodel = FALSE,
                                                   useCWmodel = FALSE,
                                                   useMOmodel = FALSE,
                                                   useNMmodel = FALSE,
                                                   processNoiseF = FALSE,
                                                   customWeights = NULL,
                                                   customSel = NULL,
                                                   lagR = FALSE,
                                                   splitLD = TRUE,
                                                   addTSB = FALSE,
                                                   label = "Intermediate_year")

df_stf_intermediate_year <- attributes(stf_intermediate_year)$shorttab %>%
  as.data.frame() %>%
  rownames_to_column(var = "Variable") %>%
  mutate(Variable = str_split(Variable, ":") %>%
           sapply(., function(x) x[2])) %>%
  pivot_longer(-Variable, names_to = "Year")

ssb_assessment_year <-  df_stf_intermediate_year %>%
  filter(Variable == "ssb", Year == (assessment_year)) %>%
  pull(value)

ssb_assessment_year_plus1 <-  df_stf_intermediate_year %>%
  filter(Variable == "ssb", Year == (assessment_year + 1)) %>%
  pull(value)

recruits_assessment_year <-  df_stf_intermediate_year %>%
  filter(Variable == "rec", Year == (assessment_year)) %>%
  pull(value)

ssb_intermediate_year <- df_stf_intermediate_year %>%
  filter(Variable == "ssb", Year == (assessment_year + 1)) %>%
  pull(value)

f_intermediate_year <- df_stf_intermediate_year %>%
  filter(Variable == "fbar", Year == (assessment_year + 1)) %>%
  pull(value)

projected_catch_assessment_year <- df_stf_intermediate_year %>%
  filter(Variable == "catch", Year == (assessment_year)) %>%
  pull(value)

projected_landings_assessment_year <- df_stf_intermediate_year %>%
  filter(Variable == "Land", Year == (assessment_year)) %>%
  pull(value)

projected_discards_assessment_year <- df_stf_intermediate_year %>%
  filter(Variable == "Discard", Year == (assessment_year)) %>%
  pull(value)

### Check parameters for projections
if (projected_catch_assessment_year > TAC_data_year) {
  print("run TAC constraint forecast")
  var_intermediate_year <- "TAC"
  value_intermediate_year <- TAC_data_year
} else {
  print("Fsq is ok")
  var_intermediate_year <- "F"
  value_intermediate_year <- f_intermediate_year
}
### Because only 1t of difference between :
### projected_catch_assessment_year > TAC_data_year
### Keep F status quo
if(assessment_year == 2025) {
  print("Fsq is ok")
  var_intermediate_year <- "F"
  value_intermediate_year <- f_intermediate_year
}


### Following Advice on fishing opportunities (2021) https://doi.org/10.17895/ices.advice.7720
### if SSB in assessment year < MSY_Btrigger, then use reduce fmsy
if (ssb_intermediate_year > ref_points$MSYBtrigger) {
  F_bar_advice_ices <- ref_points$Fmsy
  Flower_bar_advice_ices <- ref_points$Fmsylower
  Fupper_bar_advice_ices <- ref_points$Fmsyupper
} else {
  cat("ssb_intermediate_year lower than ref_points$MSYBtrigger \n use lower F")
  F_bar_advice_ices <- round(ref_points$Fmsy * ssb_intermediate_year / ref_points$MSYBtrigger, digits = 2)
  Flower_bar_advice_ices <- round(ref_points$Fmsylower * ssb_intermediate_year / ref_points$MSYBtrigger, digits = 2)
  Fupper_bar_advice_ices <- round(ref_points$Fmsyupper * ssb_intermediate_year / ref_points$MSYBtrigger, digits = 2)
}


###-----------------------------------------------------------------------------
### save stf parameters for report and advice sheet
saveRDS(list(Recr_start_year = Recr_start_year,
             Recr_end_year = Recr_end_year,
             F_bar_status_quo = F_bar_status_quo,
             ssb_assessment_year_plus1 = ssb_assessment_year_plus1,
             TAC_Constraint = TAC_Constraint,
             var_intermediate_year = var_intermediate_year,
             value_intermediate_year = value_intermediate_year,
             stock_weights_avg_years = stock_weights_avg_years),
        file = paste0("output/stf_parameters.rds"))

################################################################################
### Scenarios
################################################################################

###----------------------------------------------------------------------------
### EU MAP - Fmsy
### save for SAG
stf_EU_MAP <- stockassessment::forecast(fit_sam,
                                        catchval.exact = c(NA, NA, NA, NA), # in case of TAC constr option: fill in 'catch_int_year'
                                        fscale = c(NA, NA, NA, NA), # insert 1 here is same as put Flast in fval -> no not the case
                                        fval = c(F_bar_status_quo, F_bar_status_quo,
                                                 F_bar_advice_ices, F_bar_advice_ices),
                                        nosim = nr_sims,
                                        year.base = last_stock_year,
                                        ave.years = stock_weights_avg_years,
                                        rec.years = Recr_years,
                                        deterministic = deterministic_forecast,
                                        useSWmodel = FALSE,
                                        useCWmodel = FALSE,
                                        useMOmodel = FALSE,
                                        useNMmodel = FALSE,
                                        processNoiseF = FALSE,
                                        customWeights = NULL,
                                        customSel = NULL,
                                        lagR = FALSE,
                                        splitLD = TRUE,
                                        addTSB = TRUE,
                                        savesim = TRUE,
                                        label = "EU_MAP")
### save results
saveRDS(stf_EU_MAP,
        file = paste0("output/stf_EU_MAP.rds"))

###----------------------------------------------------------------------------
## EU MAP - Fmsy-lower
### save for SAG
stf_EU_MAP_lower <- stockassessment::forecast(fit_sam,
                                              catchval.exact = c(NA,NA,NA,NA), # in case of TAC constr option: fill in 'catch_int_year'
                                              fscale = c(NA,NA,NA,NA), # insert 1 here is same as put Flast in fval -> no not the case
                                              fval = c(F_bar_status_quo, F_bar_status_quo,
                                                       Flower_bar_advice_ices, Flower_bar_advice_ices),
                                              nosim = nr_sims,
                                              year.base = last_stock_year,
                                              ave.years = stock_weights_avg_years,
                                              rec.years = Recr_years,
                                              deterministic = deterministic_forecast,
                                              useSWmodel = FALSE,
                                              useCWmodel = FALSE,
                                              useMOmodel = FALSE,
                                              useNMmodel = FALSE,
                                              processNoiseF = FALSE,
                                              customWeights = NULL,
                                              customSel = NULL,
                                              lagR = FALSE,
                                              splitLD = TRUE,
                                              addTSB = FALSE,
                                              savesim = TRUE,
                                              label = "EU_MAP_lower")

### save results
saveRDS(stf_EU_MAP_lower,
        file = paste0("output/stf_EU_MAP_lower.rds"))

###----------------------------------------------------------------------------
## EU MAP - Fmsy-upper
### save for SAG
stf_EU_MAP_upper <- stockassessment::forecast(fit_sam,
                                              catchval.exact = c(NA,NA,NA,NA), # in case of TAC constr option: fill in 'catch_int_year'
                                              fscale = c(NA,NA,NA,NA), # insert 1 here is same as put Flast in fval -> no not the case
                                              fval = c(F_bar_status_quo, F_bar_status_quo,
                                                       Fupper_bar_advice_ices, Fupper_bar_advice_ices),
                                              nosim = nr_sims,
                                              year.base = last_stock_year,
                                              ave.years = stock_weights_avg_years,
                                              rec.years = Recr_years,
                                              deterministic = deterministic_forecast,
                                              useSWmodel = FALSE,
                                              useCWmodel = FALSE,
                                              useMOmodel = FALSE,
                                              useNMmodel = FALSE,
                                              processNoiseF = FALSE,
                                              customWeights = NULL,
                                              customSel = NULL,
                                              lagR = FALSE,
                                              splitLD = TRUE,
                                              addTSB = FALSE,
                                              savesim = TRUE,
                                              label = "EU_MAP_upper")


### save results
saveRDS(stf_EU_MAP_upper, file = paste0("output/stf_EU_MAP_upper.rds"))

###----------------------------------------------------------------------------
## FMSY
### save for SAG
stf_FMSY <- stockassessment::forecast(fit_sam,
                                      catchval.exact = c(NA,NA,NA,NA), # in case of TAC constr option: fill in 'catch_int_year'
                                      fscale = c(NA,NA,NA,NA), # insert 1 here is same as put Flast in fval -> no not the case
                                      fval = c(F_bar_status_quo, F_bar_status_quo,
                                               ref_points$Fmsy, ref_points$Fmsy),
                                      nosim = nr_sims,
                                      year.base = last_stock_year,
                                      ave.years = stock_weights_avg_years,
                                      rec.years = Recr_years,
                                      deterministic = deterministic_forecast,
                                      useSWmodel = FALSE,
                                      useCWmodel = FALSE,
                                      useMOmodel = FALSE,
                                      useNMmodel = FALSE,
                                      processNoiseF = FALSE,
                                      customWeights = NULL,
                                      customSel = NULL,
                                      lagR = FALSE,
                                      splitLD = TRUE,
                                      addTSB = FALSE,
                                      savesim = TRUE,
                                      label = "Fmsy")
### save results
saveRDS(stf_FMSY,
        file = paste0("output/stf_FMSY.rds"))

###----------------------------------------------------------------------------
## FMSY
### save for SAG
stf_FMSY_lower <- stockassessment::forecast(fit_sam,
                                            catchval.exact = c(NA,NA,NA,NA), # in case of TAC constr option: fill in 'catch_int_year'
                                            fscale = c(NA,NA,NA,NA), # insert 1 here is same as put Flast in fval -> no not the case
                                            fval = c(F_bar_status_quo, F_bar_status_quo,
                                                     ref_points$Fmsylower, ref_points$Fmsylower),
                                            nosim = nr_sims,
                                            year.base = last_stock_year,
                                            ave.years = stock_weights_avg_years,
                                            rec.years = Recr_years,
                                            deterministic = deterministic_forecast,
                                            useSWmodel = FALSE,
                                            useCWmodel = FALSE,
                                            useMOmodel = FALSE,
                                            useNMmodel = FALSE,
                                            processNoiseF = FALSE,
                                            customWeights = NULL,
                                            customSel = NULL,
                                            lagR = FALSE,
                                            splitLD = TRUE,
                                            addTSB = FALSE,
                                            savesim = TRUE,
                                            label = "Fmsy_lower")
### save results
saveRDS(stf_FMSY_lower,
        file = paste0("output/stf_FMSY_lower.rds"))


###----------------------------------------------------------------------------
## FMSY
### save for SAG
stf_FMSY_upper <- stockassessment::forecast(fit_sam,
                                            catchval.exact = c(NA,NA,NA,NA), # in case of TAC constr option: fill in 'catch_int_year'
                                            fscale = c(NA,NA,NA,NA), # insert 1 here is same as put Flast in fval -> no not the case
                                            fval = c(F_bar_status_quo, F_bar_status_quo,
                                                     ref_points$Fmsyupper, ref_points$Fmsyupper),
                                            nosim = nr_sims,
                                            year.base = last_stock_year,
                                            ave.years = stock_weights_avg_years,
                                            rec.years = Recr_years,
                                            deterministic = deterministic_forecast,
                                            useSWmodel = FALSE,
                                            useCWmodel = FALSE,
                                            useMOmodel = FALSE,
                                            useNMmodel = FALSE,
                                            processNoiseF = FALSE,
                                            customWeights = NULL,
                                            customSel = NULL,
                                            lagR = FALSE,
                                            splitLD = TRUE,
                                            addTSB = FALSE,
                                            savesim = TRUE,
                                            label = "Fmsy_upper")
### save results
saveRDS(stf_FMSY_upper,
        file = paste0("output/stf_FMSY_upper.rds"))



###----------------------------------------------------------------------------
## F = 0
### save for SAG
stf_F_0  <-  stockassessment::forecast(fit_sam,
                                       catchval.exact = c(NA,NA,NA,NA), # in case of TAC constr option: fill in 'catch_int_year'
                                       fscale = c(NA,NA,0,0),
                                       fval = c(F_bar_status_quo, F_bar_status_quo,
                                                NA, NA),
                                       nosim = nr_sims,
                                       year.base = last_stock_year,
                                       ave.years = stock_weights_avg_years,
                                       rec.years = Recr_years,
                                       deterministic = deterministic_forecast,
                                       useSWmodel = FALSE,
                                       useCWmodel = FALSE,
                                       useMOmodel = FALSE,
                                       useNMmodel = FALSE,
                                       processNoiseF = FALSE,
                                       customWeights = NULL,
                                       customSel = NULL,
                                       lagR = FALSE,
                                       splitLD = TRUE,
                                       addTSB = FALSE,
                                       savesim = TRUE,
                                       label = "F_0")

### save results
saveRDS(stf_F_0,
        file = paste0("output/stf_F_0.rds"))

###----------------------------------------------------------------------------
## F = F_pa
### save for SAG
stf_F_PA  <- stockassessment::forecast(fit_sam,
                                       catchval.exact = c(NA,NA,NA,NA), # in case of TAC constr option: fill in 'catch_int_year'
                                       fscale = c(NA,NA,NA,NA), # insert 1 here is same as put Flast in fval -> no not the case
                                       fval = c(F_bar_status_quo, F_bar_status_quo,
                                                ref_points$Fpa, ref_points$Fpa),
                                       nosim = nr_sims,
                                       year.base = last_stock_year,
                                       ave.years = stock_weights_avg_years,
                                       rec.years = Recr_years,
                                       deterministic = deterministic_forecast,
                                       useSWmodel = FALSE,
                                       useCWmodel = FALSE,
                                       useMOmodel = FALSE,
                                       useNMmodel = FALSE,
                                       processNoiseF = FALSE,
                                       customWeights = NULL,
                                       customSel = NULL,
                                       lagR = FALSE,
                                       splitLD = TRUE,
                                       addTSB = FALSE,
                                       savesim = TRUE,
                                       label = "Fpa")
### save results
saveRDS(stf_F_PA,
        file = paste0("output/stf_F_PA.rds"))

###----------------------------------------------------------------------------
## F = F_lim
### save for SAG
stf_F_lim  <- stockassessment::forecast(fit_sam,
                                        catchval.exact = c(NA,NA,NA,NA), # in case of TAC constr option: fill in 'catch_int_year'
                                        fscale = c(NA,NA,NA,NA), # insert 1 here is same as put Flast in fval -> no not the case
                                        fval = c(F_bar_status_quo, F_bar_status_quo,
                                                 ref_points$Flim, ref_points$Flim),
                                        nosim = nr_sims,
                                        year.base = last_stock_year,
                                        ave.years = stock_weights_avg_years,
                                        rec.years = Recr_years,
                                        deterministic = deterministic_forecast,
                                        useSWmodel = FALSE,
                                        useCWmodel = FALSE,
                                        useMOmodel = FALSE,
                                        useNMmodel = FALSE,
                                        processNoiseF = FALSE,
                                        customWeights = NULL,
                                        customSel = NULL,
                                        lagR = FALSE,
                                        splitLD = TRUE,
                                        addTSB = FALSE,
                                        savesim = TRUE,
                                        label = "Flim")

### save results
saveRDS(stf_F_lim,
        file = paste0("output/stf_F_lim.rds"))

###----------------------------------------------------------------------------
## SSB = B_lim
### save for SAG
stf_SSB_Blim <-  stockassessment::forecast(fit_sam,
                                           catchval.exact = c(NA, NA, NA, NA),
                                           fscale = c(NA, NA, NA, NA),
                                           nextssb = c(NA, NA, ref_points$Blim, NA),
                                           fval = c(F_bar_status_quo, F_bar_status_quo,
                                                    NA, ref_points$Fmsy),
                                           nosim = nr_sims,
                                           year.base = last_stock_year,
                                           ave.years = stock_weights_avg_years,
                                           rec.years = Recr_years,
                                           deterministic = deterministic_forecast,
                                           useSWmodel = FALSE,
                                           useCWmodel = FALSE,
                                           useMOmodel = FALSE,
                                           useNMmodel = FALSE,
                                           processNoiseF = FALSE,
                                           customWeights = NULL,
                                           customSel = NULL,
                                           lagR = FALSE,
                                           splitLD = TRUE,
                                           addTSB = FALSE,
                                           savesim = TRUE,
                                           label = "Blim")

### save results
saveRDS(stf_SSB_Blim,
        file = paste0("output/stf_SSB_Blim.rds"))

###----------------------------------------------------------------------------
## SSB = B_pa
### save for SAG
stf_SSB_Bpa <- stockassessment::forecast(fit_sam,
                                         catchval.exact = c(NA, NA, NA, NA),
                                         fscale = c(NA, NA, NA, NA),
                                         nextssb = c(NA, NA, ref_points$Bpa, NA),
                                         fval = c(F_bar_status_quo, F_bar_status_quo,
                                                  NA, ref_points$Fmsy),
                                         nosim = nr_sims,
                                         year.base = last_stock_year,
                                         ave.years = stock_weights_avg_years,
                                         rec.years = Recr_years,
                                         deterministic = deterministic_forecast,
                                         useSWmodel = FALSE,
                                         useCWmodel = FALSE,
                                         useMOmodel = FALSE,
                                         useNMmodel = FALSE,
                                         processNoiseF = FALSE,
                                         customWeights = NULL,
                                         customSel = NULL,
                                         lagR = FALSE,
                                         splitLD = TRUE,
                                         addTSB = FALSE,
                                         savesim = TRUE,
                                         label = "Bpa")

### save results
saveRDS(stf_SSB_Bpa,
        file = paste0("output/stf_SSB_Bpa.rds"))

###----------------------------------------------------------------------------
## SSB = B_MSYBtrigger
### save for SAG
if (ref_points$Bpa !=  ref_points$MSYBtrigger) {
  stf_SSB_MSYBtrigger <- stockassessment::forecast(fit_sam,
                                                   catchval.exact = c(NA, NA, NA, NA),
                                                   fscale = c(NA, NA, NA, NA),
                                                   nextssb = c(NA, NA, ref_points$MSYBtrigger, NA),
                                                   fval = c(F_bar_status_quo, F_bar_status_quo,
                                                            NA, ref_points$Fmsy),
                                                   nosim = nr_sims,
                                                   year.base = last_stock_year,
                                                   ave.years = stock_weights_avg_years,
                                                   rec.years = Recr_years,
                                                   deterministic = deterministic_forecast,
                                                   useSWmodel = FALSE,
                                                   useCWmodel = FALSE,
                                                   useMOmodel = FALSE,
                                                   useNMmodel = FALSE,
                                                   processNoiseF = FALSE,
                                                   customWeights = NULL,
                                                   customSel = NULL,
                                                   lagR = FALSE,
                                                   splitLD = TRUE,
                                                   addTSB = FALSE,
                                                   savesim = TRUE,
                                                   label = "msyBtrigger")
  ### save results
  saveRDS(stf_SSB_MSYBtrigger,
          file = paste0("output/stf_SSB_MSYBtrigger.rds"))
}

###-----------------------------------------------------------------------------
### SSB as intermediate year
stf_ssb_intermediate <- stockassessment::forecast(fit_sam,
                                                  catchval.exact = c(NA, NA, NA, NA),
                                                  fscale = c(NA, NA, NA, NA),
                                                  nextssb = c(NA, NA, ssb_intermediate_year, NA),
                                                  fval = c(F_bar_status_quo, F_bar_status_quo,
                                                           NA, ref_points$Fmsy),
                                                  nosim = nr_sims,
                                                  year.base = last_stock_year,
                                                  ave.years = stock_weights_avg_years,
                                                  rec.years = Recr_years,
                                                  deterministic = deterministic_forecast,
                                                  useSWmodel = FALSE,
                                                  useCWmodel = FALSE,
                                                  useMOmodel = FALSE,
                                                  useNMmodel = FALSE,
                                                  processNoiseF = FALSE,
                                                  customWeights = NULL,
                                                  customSel = NULL,
                                                  lagR = FALSE,
                                                  splitLD = TRUE,
                                                  addTSB = FALSE,
                                                  savesim = TRUE,
                                                  label = "ssb_intermediate")
### save results
saveRDS(stf_ssb_intermediate, file = paste0("output/stf_ssb_intermediate.rds"))

###----------------------------------------------------------------------------
## F = F of assessment year
### save for SAG
stf_F_assessment_year <- stockassessment::forecast(fit_sam,
                                                   catchval.exact = c(NA, NA, NA, NA),
                                                   fscale = c(NA, NA, NA, NA),
                                                   nextssb = c(NA, NA, NA, NA),
                                                   fval = c(F_bar_status_quo, F_bar_status_quo,
                                                            f_intermediate_year, ref_points$Fmsy),
                                                   nosim = nr_sims,
                                                   year.base = last_stock_year,
                                                   ave.years = stock_weights_avg_years,
                                                   rec.years = Recr_years,
                                                   deterministic = deterministic_forecast,
                                                   useSWmodel = FALSE,
                                                   useCWmodel = FALSE,
                                                   useMOmodel = FALSE,
                                                   useNMmodel = FALSE,
                                                   processNoiseF = FALSE,
                                                   customWeights = NULL,
                                                   customSel = NULL,
                                                   lagR = FALSE,
                                                   splitLD = TRUE,
                                                   addTSB = FALSE,
                                                   savesim = TRUE,
                                                   label = "f_intermediate_year")
### save results
saveRDS(stf_F_assessment_year,
        file = paste0("output/stf_F_assessment_year.rds"))


###----------------------------------------------------------------------------
## equal TAC
### save for SAG
stf_landings_TAC_data_year <- stockassessment::forecast(fit_sam,
                                                        landval  = c(NA, NA, TAC_Constraint , NA),
                                                        fscale = c(NA, NA, NA, NA),
                                                        nextssb = c(NA, NA, NA, NA),
                                                        fval = c(F_bar_status_quo, F_bar_status_quo,
                                                                 NA, ref_points$Fmsy),
                                                        nosim = nr_sims,
                                                        year.base = last_stock_year,
                                                        ave.years = stock_weights_avg_years,
                                                        rec.years = Recr_years,
                                                        deterministic = deterministic_forecast,
                                                        useSWmodel = FALSE,
                                                        useCWmodel = FALSE,
                                                        useMOmodel = FALSE,
                                                        useNMmodel = FALSE,
                                                        processNoiseF = FALSE,
                                                        customWeights = NULL,
                                                        customSel = NULL,
                                                        lagR = FALSE,
                                                        splitLD = TRUE,
                                                        addTSB = FALSE,
                                                        savesim = TRUE,
                                                        label = "catch_TAC_data_year")
# ### save results
saveRDS(stf_landings_TAC_data_year,
        file = paste0("output/stf_landings_TAC_data_year.rds"))

###----------------------------------------------------------------------------
## catch equal to TAC
### save for SAG
stf_catch_TAC_assessment_year <- stockassessment::forecast(fit_sam,
                                                           catchval.exact = c(NA, NA, TAC_Constraint, NA),
                                                           fscale = c(NA, NA, NA, NA),
                                                           nextssb = c(NA, NA, NA, NA),
                                                           fval = c(F_bar_status_quo, F_bar_status_quo,
                                                                    NA, ref_points$Fmsy),
                                                           nosim = nr_sims,
                                                           year.base = last_stock_year,
                                                           ave.years = stock_weights_avg_years,
                                                           rec.years = Recr_years,
                                                           deterministic = deterministic_forecast,
                                                           useSWmodel = FALSE,
                                                           useCWmodel = FALSE,
                                                           useMOmodel = FALSE,
                                                           useNMmodel = FALSE,
                                                           processNoiseF = FALSE,
                                                           customWeights = NULL,
                                                           customSel = NULL,
                                                           lagR = FALSE,
                                                           splitLD = TRUE,
                                                           addTSB = FALSE,
                                                           savesim = TRUE,
                                                           label = "catch_TAC_assessment_year")
### save results
saveRDS(stf_catch_TAC_assessment_year,
        file = paste0("output/stf_catch_TAC_assessment_year.rds"))

###----------------------------------------------------------------------------
## Probability of (SSB2026<Blim)=5%
### save for SAG
f_for_ssb_below_blim <- 0.35
stf_ssb_below_blim  <- stockassessment::forecast(fit_sam,
                                        catchval.exact = c(NA,NA,NA,NA), # in case of TAC constr option: fill in 'catch_int_year'
                                        fscale = c(NA,NA,NA,NA), # insert 1 here is same as put Flast in fval -> no not the case
                                        fval = c(F_bar_status_quo, F_bar_status_quo,
                                                 f_for_ssb_below_blim, f_for_ssb_below_blim),
                                        nosim = nr_sims,
                                        year.base = last_stock_year,
                                        ave.years = stock_weights_avg_years,
                                        rec.years = Recr_years,
                                        deterministic = deterministic_forecast,
                                        useSWmodel = FALSE,
                                        useCWmodel = FALSE,
                                        useMOmodel = FALSE,
                                        useNMmodel = FALSE,
                                        processNoiseF = FALSE,
                                        customWeights = NULL,
                                        customSel = NULL,
                                        lagR = FALSE,
                                        splitLD = TRUE,
                                        addTSB = FALSE,
                                        savesim = TRUE,
                                        label = "Flim")

### save results
saveRDS(stf_F_lim,
        file = paste0("output/stf_F_lim.rds"))
