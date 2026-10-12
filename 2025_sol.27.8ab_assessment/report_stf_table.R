################################################################################
### Set Management options table. Annual catch scenarios
################################################################################

###-----------------------------------------------------------------------------
df_sam_stf_output <- attributes(stf_EU_MAP)$tab %>%
  as.data.frame() %>%
  rownames_to_column(var = "Year") %>%
  pivot_longer(-Year, names_to = "Variable") %>%
  mutate(stat = str_split(Variable, ":") %>%
           sapply(., function(x) x[2]),
         Variable = str_split(Variable, ":") %>%
           sapply(., function(x) x[1]))

df_sam_stf_output_assessment_year <- df_sam_stf_output %>%
  filter(Year == assessment_year) %>%
  pivot_wider(names_from = stat, values_from = value)

df_sam_stf_rec <- df_sam_stf_output_assessment_year %>%
  filter(Variable == "rec")

df_sam_stf_ssb <- df_sam_stf_output_assessment_year %>%
  filter(Variable == "ssb")

df_sam_stf_tsb <- df_sam_stf_output_assessment_year %>%
  filter(Variable == "tsb")

df_sam_stf_f <- df_sam_stf_output_assessment_year %>%
  filter(Variable == "f")


################################################################################
### STF assumptions
################################################################################

### empty data frame to store results
assumptions_table_var_names <- c(paste0("Fage 3–7 (", assessment_year, ")"),
                                 paste0("SSB (", assessment_year + 1, ")"),
                                 paste0("R_{age2} (", assessment_year, "–", assessment_year + 1, ")"),
                                 paste0("Catch (", assessment_year, ")"),
                                 paste0("Projected landings (", assessment_year, ")"),
                                 paste0("Projected discards (", assessment_year, ")"))


assumptions_table_var_values <- c(f_intermediate_year,
                                  ssb_intermediate_year,
                                  df_sam_stf_rec$median,
                                  projected_catch_assessment_year,
                                  projected_landings_assessment_year,
                                  projected_discards_assessment_year)

assumptions_table_var_notes <- c(glue("Average selection pattern from {min(stock_weights_avg_years)} to {max(stock_weights_avg_years)}."),
                                 "Short-term forecast (STF); tonnes",
                                 glue("Median recruitment, resampled from the years {min(Recr_years)}-{max(Recr_years)}; thousands"),
                                 "STF; tonnes",
                                 glue("STF; assuming average landings ratio by age {min(stock_weights_avg_years)}-{max(stock_weights_avg_years)}; tonnes"),
                                 glue("STF; assuming average landings ratio by age {min(stock_weights_avg_years)}-{max(stock_weights_avg_years)}; tonnes"))


stf_advice_assumptions_table <- data.frame(Variables = assumptions_table_var_names,
                                           Values = assumptions_table_var_values,
                                           Notes = assumptions_table_var_notes)

saveRDS(stf_advice_assumptions_table,
        file = paste0("report/stf_advice_assumptions_table.rds"))
write.csv(stf_advice_assumptions_table,
          file = paste0("report/stf_advice_assumptions_table.csv"))

################################################################################
### STF assumptions for comparison
################################################################################
stf_advice_assumptions_table_comparison <- extract_sam_stf_for_advice_comparison(stf_EU_MAP,
                                                                                 assessment_year = assessment_year)

saveRDS(stf_advice_assumptions_table_comparison,
        file = paste0("report/stf_advice_assumptions_table_comparison.rds"))
write.csv(stf_advice_assumptions_table_comparison,
          file = paste0("report/stf_advice_assumptions_table_comparison.csv"))

################################################################################
### STF results
################################################################################
# Set some column names
stf_advice_table_names <- c("Basis",
                            paste0("Total catch (", assessment_year  + 1, ")"),
                            paste0("Projected landings (", assessment_year + 1, ")"),
                            paste0("Projected discards (", assessment_year + 1, ")"),
                            # paste0("F_{total} (", assessment_year  + 1, ")"),
                            paste0("F projected landings (", assessment_year + 1, ")"),
                            paste0("SSB (", assessment_year + 2, ")"),
                            "% SSB change #",
                            "% TAC change ##",
                            "% Advice change ^^^",
                            glue("Probability SSB < Blim in {last_projection_year} ^^^^"))

### empty data frame to store results
stf_advice_table <- data.frame()

###-----------------------------------------------------------------------------
### EU MAP
df_stf_EU_MAP <- extract_sam_stf_for_advice_table(stf_EU_MAP,
                                                  TAC_ref = TAC_assessment_year,
                                                  Advice_ref = TAC_assessment_year,
                                                  SSB_ref = ssb_intermediate_year,
                                                  col_names = stf_advice_table_names)

stf_advice_table <- bind_rows(stf_advice_table, df_stf_EU_MAP)

### EU MAP lower
Fmsylow_last_advice <- df_advice_MAP %>%
  filter(year == assessment_year & FMSY == "FMSY_lower") %>%
  pull(TAC)

df_stf_EU_MAP_lower <- extract_sam_stf_for_advice_table(stf_EU_MAP_lower,
                                                        TAC_ref = TAC_assessment_year,
                                                        Advice_ref = Fmsylow_last_advice,
                                                        SSB_ref = ssb_intermediate_year,
                                                        col_names = stf_advice_table_names)

stf_advice_table <- bind_rows(stf_advice_table, df_stf_EU_MAP_lower)

### EU MAP upper
if (nrow(filter(df_advice_MAP, year == assessment_year &
                FMSY == "FMSY_upper")) > 0) {
  Fmsyupper_last_advice <- df_advice_MAP %>%
    filter(year == assessment_year & FMSY == "FMSY_upper") %>%
    pull(TAC)


  df_stf_EU_MAP_upper <- extract_sam_stf_for_advice_table(stf_EU_MAP_upper,
                                                          TAC_ref = TAC_assessment_year,
                                                          Advice_ref = Fmsyupper_last_advice,
                                                          SSB_ref = ssb_intermediate_year,
                                                          col_names = stf_advice_table_names)

  stf_advice_table <- bind_rows(stf_advice_table, df_stf_EU_MAP_upper)
}
###-----------------------------------------------------------------------------
### other scenario

### F = FMSY
df_stf_FMSY <- extract_sam_stf_for_advice_table(stf_FMSY,
                                                TAC_ref = TAC_assessment_year,
                                                Advice_ref = TAC_assessment_year,
                                                SSB_ref = ssb_intermediate_year,
                                                col_names = stf_advice_table_names)

stf_advice_table <- bind_rows(stf_advice_table, df_stf_FMSY)

### F = FMSY_lower
df_stf_FMSY_lower <- extract_sam_stf_for_advice_table(stf_FMSY_lower,
                                                      TAC_ref = TAC_assessment_year,
                                                      Advice_ref = TAC_assessment_year,
                                                      SSB_ref = ssb_intermediate_year,
                                                      col_names = stf_advice_table_names)

stf_advice_table <- bind_rows(stf_advice_table, df_stf_FMSY_lower)

### F = FMSY_upper
df_stf_FMSY_upper <- extract_sam_stf_for_advice_table(stf_FMSY_upper,
                                                      TAC_ref = TAC_assessment_year,
                                                      Advice_ref = TAC_assessment_year,
                                                      SSB_ref = ssb_intermediate_year,
                                                      col_names = stf_advice_table_names)

stf_advice_table <- bind_rows(stf_advice_table, df_stf_FMSY_upper)

### F = 0
df_stf_F_0 <- extract_sam_stf_for_advice_table(stf_F_0,
                                               TAC_ref = TAC_assessment_year,
                                               Advice_ref = TAC_assessment_year,
                                               SSB_ref = ssb_intermediate_year,
                                               col_names = stf_advice_table_names)

stf_advice_table <- bind_rows(stf_advice_table, df_stf_F_0)

### F = F_pa
df_stf_F_PA <- extract_sam_stf_for_advice_table(stf_F_PA,
                                                TAC_ref = TAC_assessment_year,
                                                Advice_ref = TAC_assessment_year,
                                                SSB_ref = ssb_intermediate_year,
                                                col_names = stf_advice_table_names)

stf_advice_table <- bind_rows(stf_advice_table, df_stf_F_PA)

# ### F = F_lim
# df_stf_F_lim <- extract_sam_stf_for_advice_table(stf_F_lim,
#                                              scenario_name = "F_lim",
#                                              TAC_ref = TAC_assessment_year,
#                                              Advice_ref = TAC_assessment_year,
#                                              SSB_ref = ssb_intermediate_year,
#                                              col_names = stf_advice_table_names)
#
# stf_advice_table <- bind_rows(stf_advice_table, df_stf_F_lim)


### SSB = B_lim
df_stf_SSB_Blim <- extract_sam_stf_for_advice_table(stf_SSB_Blim,
                                                    TAC_ref = TAC_assessment_year,
                                                    Advice_ref = TAC_assessment_year,
                                                    SSB_ref = ssb_intermediate_year,
                                                    col_names = stf_advice_table_names)

stf_advice_table <- bind_rows(stf_advice_table, df_stf_SSB_Blim)

### SSB = B_pa
df_stf_SSB_Bpa <- extract_sam_stf_for_advice_table(stf_SSB_Bpa,
                                                   TAC_ref = TAC_assessment_year,
                                                   Advice_ref = TAC_assessment_year,
                                                   SSB_ref = ssb_intermediate_year,
                                                   col_names = stf_advice_table_names)

stf_advice_table <- bind_rows(stf_advice_table, df_stf_SSB_Bpa)


### SSB = ssb intermediate

df_stf_SSB_intermediate <- extract_sam_stf_for_advice_table(stf_ssb_intermediate,
                                                            TAC_ref = TAC_assessment_year,
                                                            Advice_ref = TAC_assessment_year,
                                                            SSB_ref = ssb_intermediate_year,
                                                            col_names = stf_advice_table_names)

stf_advice_table <- bind_rows(stf_advice_table, df_stf_SSB_intermediate)

### F = f data year
df_stf_F_assessment_year <- extract_sam_stf_for_advice_table(stf_F_assessment_year,
                                                             TAC_ref = TAC_assessment_year,
                                                             Advice_ref = TAC_assessment_year,
                                                             SSB_ref = ssb_intermediate_year,
                                                             col_names = stf_advice_table_names)

stf_advice_table <- bind_rows(stf_advice_table, df_stf_F_assessment_year)

### Landings = TAC assessment year
df_stf_landings_TAC_data_year <- extract_sam_stf_for_advice_table(stf_landings_TAC_data_year,
                                                                  TAC_ref = TAC_assessment_year,
                                                                  Advice_ref = TAC_assessment_year,
                                                                  SSB_ref = ssb_intermediate_year,
                                                                  col_names = stf_advice_table_names)

stf_advice_table <- bind_rows(stf_advice_table, df_stf_landings_TAC_data_year)

### Catch = TAC assessment year
df_stf_catch_TAC_assessment_year <- extract_sam_stf_for_advice_table(stf_catch_TAC_assessment_year,
                                                                     TAC_ref = TAC_assessment_year,
                                                                     Advice_ref = TAC_assessment_year,
                                                                     SSB_ref = ssb_intermediate_year,
                                                                     col_names = stf_advice_table_names)

stf_advice_table <- bind_rows(stf_advice_table, df_stf_catch_TAC_assessment_year)

###----------------------------------------------------------------------------
## Probability of (SSB2026<Blim)=5%

### Landings = TAC assessment year
df_stf_ssb_below_blim <- extract_sam_stf_for_advice_table(stf_ssb_below_blim,
                                                          TAC_ref = TAC_assessment_year,
                                                          Advice_ref = TAC_assessment_year,
                                                          SSB_ref = ssb_intermediate_year,
                                                          col_names = stf_advice_table_names)


stf_advice_table <- bind_rows(stf_advice_table, df_stf_ssb_below_blim)
###-----------------------------------------------------------------------------
### table formating
stf_advice_table <- stf_advice_table %>%
  mutate(across(contains("%"), ~ as.character(round(., digits = 1))),
         across(contains("Probability"), ~ as.character(round(., digits = 2))),
         across(contains("F projected"), ~ as.character(round(., digits = 3)))) %>%
  mutate_if(is.numeric, round)

saveRDS(stf_advice_table, file = paste0("report/stf_advice_table.rds"))
write.taf(stf_advice_table, file = paste0("report/stf_advice_table.csv"))

