### export data to Standard Assessment Graph website using icesSAG package

## Before: Run icesTAF::source.all()
## After: create info and fishdata to be used for SAG upload using personnal token

library("icesSAG")

info <- icesSAG::stockInfo(StockCode = "sol.27.8ab",
                           AssessmentYear = assessment_year,
                           ContactPerson = "jean.baptiste.lecomte@ifremer.fr",
                           StockCategory = "1",
                           ModelType = "A",
                           ModelName = "SAM",
                           Purpose = "Advice",
                           RecruitmentAge = 2,
                           CatchesLandingsUnits = "t",
                           RecruitmentUnits = "NE3",
                           StockSizeUnits = "t",
                           Fage = "3-7",
                           MSYBtrigger = ref_points[["MSYBtrigger"]],
                           FMSY = ref_points[["Fmsy"]],
                           Blim = ref_points[["Blim"]],
                           Bpa = ref_points[["Bpa"]],
                           # Flim = ref_points[["Flim"]],
                           Fpa = ref_points[["Fpa"]],
                           ConfidenceIntervalDefinition = "90%")

Year <- (range(sam_ass_stock)[['minyear']]:assessment_year)
Year_rec <- (range(sam_ass_stock)[['minyear']]:(assessment_year - 1))



###-----------------------------------------------------------------------------
### Fish data for SAG
fishdata <- icesSAG::stockFishdata(Year,
                          Low_Recruitment = c(df_rec$Low, NA),
                          Recruitment = c(df_rec$Estimate, df_sam_stf_rec$median),
                          High_Recruitment = c(df_rec$High, NA),
                          Low_StockSize = c(df_ssb$Low, df_sam_stf_ssb$low),
                          StockSize = c(df_ssb$Estimate, df_sam_stf_ssb$median),
                          High_StockSize = c(df_ssb$High, df_sam_stf_ssb$high),
                          Low_TBiomass = c(df_tsb$Low, df_sam_stf_tsb$low),
                          TBiomass = c(df_tsb$Estimate, df_sam_stf_tsb$median),
                          High_TBiomass = c(df_tsb$High, df_sam_stf_tsb$high),
                          Low_FishingPressure = c(df_fbar$Low, NA),
                          FishingPressure = c(df_fbar$Estimate, NA),
                          High_FishingPressure = c(df_fbar$High, NA),
                          Catches = round(c(catch(flr_stock)[,ac(1984:(assessment_year - 1))],NA), digits = 0),
                          Landings = round(c(landings(flr_stock)[, ac(1984:(assessment_year - 1))],NA), digits = 0),
                          Discards = round(c(discards(flr_stock)[, ac(1984:(assessment_year - 1))],NA), digits = 0))

###-----------------------------------------------------------------------------
writeSAGxml(info, fishdata, file = "upload_for_sage.xml")
icesSAG::uploadStock(file = "upload_for_sage.xml",
                     upload = FALSE,
                     verbose = TRUE)

saveRDS(list(info = info,
             fishdata = fishdata),
        "report/sag_objects.rds"  )
