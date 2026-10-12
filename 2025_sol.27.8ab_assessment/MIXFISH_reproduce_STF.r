# library------------------------------------------------------------

library("FLCore")
library("FLash")
remotes::install_github(repo = "ices-tools-dev/mixfishtools",
                        force = FALSE)
library("mixfishtools")
library("tidyr")
library("ggplot2")
library("ggplotFL")
library("knitr")

### -----------------------------------------------------------------------------
### Set parameters for forecasting
yrAssess <- last_stock_year # final year of assessment data
yrNow <- assessment_year # intermediate year
yrTAC <- advice_year # advice year
yrTACp1 <- last_projection_year # advice year +1 (needed to get SSB at end of yrTAC)

## stock  ------------------------------------------------------------
### general-----------
Fmsy <- filter(df_ref_points_f, variable == "Fmsy") %>%
  pull(value)

### from SSA STF to be updated------------
FyearNow <-  fbar(sam_stock)[, ac(last_stock_year+ (-2:0))] %>%
  mean() %>%
  round(., digits = 2)

FyearTAC <-  f_intermediate_year

RyearNow <- df_sam_stf_rec$median
RyearTAC <- df_sam_stf_rec$median

## WGBIE results to be updated-------------------------------------------------------
# Reported output from single stock headline advice
# fill manually for more control and checks
stfRef <- data.frame(
  model = "WGBIE",
  year = c(2024:2027),
  catch = c(catch(sam_ass_stock)[, ac(last_stock_year)], 2490, 2482, NA), # comes from Table 8, Table 1, Table 2
  landings = c(landings(sam_ass_stock)[, ac(last_stock_year)] , 2471, 2460, NA), # comes from Table 8, Table 1, Table 2
  fbar = c(0.29, 0.28, 0.28, NA), # comes from Table 9, Table 1, Table 2
  ssb = c(9524, 9634, 9485, 9380) # comes from Table 9, Table 1, Table 2
)

#load FLR object------------------------------------------------------------
stk <- sam_ass_stock



#extend object to be updated if different assumption about number of years/differnt years for the average and f rescaling------------------------------------------------------------
stkProj <- stf(object = stk,
               nyears = 3,
               wts.nyears = 3,
               fbar.nyears = 3,
               f.rescale = FALSE,
               disc.nyears = 3)

## control ------------------------------------------------------------
df <- as.data.frame(stkProj)
df <- subset(df, slot %in% c("landings.wt", "discards.wt", "catch.wt", "m", "mat", "harvest") & year > (yrAssess-20))
df$forecast <- df$year %in% c(yrNow, yrTAC, yrTACp1)

ggplot(df) + aes(x = year, y = data, group = age, color = forecast) +
  facet_wrap(~slot, scales = "free_y") +
  geom_line(show.legend = F) +
  scale_color_manual(values = c(8,1)) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5)) +
  theme_bw()


# STF to be changed if needed ------------------------------------------------------------
# stf control (Fsq, followed by 2 years at Fmsy)

ctrl <- fwdControl(
  data.frame(
    year = c(yrNow, yrTAC, yrTACp1),
    val = c(FyearNow, FyearTAC, FyearTAC),
    quantity = c("f"),
    relYear = c(NA, NA, NA)
  )
)

# stock-recruitment model (manual input within a geometric mean model)
srPar <- FLPar(c(RyearNow, RyearTAC, RyearTAC),
               dimnames = list(params="a",
                               year = c(yrNow, yrTAC, yrTACp1),
                               iter = 1))
srMod <- FLSR(model = "geomean", params = srPar)

# projection
stkProj <- fwd(object = stkProj,
               control = ctrl,
               sr = srMod)

# plot
L <- FLStocks(list(assessment = stkProj,
                   forecast = stkProj[,ac(yrAssess:yrTACp1)]))
plot(L) +
  # aes(linetype = stock) +
  scale_color_manual(values = c(8,1)) +
  # scale_linetype_manual(values = c(2,1)) +
  theme_bw()

# Control ------------------------------

stfDet <- data.frame(
  model = "FLR",
  year = ac(yrAssess:yrTACp1),
  catch = c(catch(stkProj[,ac(yrAssess:yrTACp1)])),
  landings = c(landings(stkProj[,ac(yrAssess:yrTACp1)])),
  fbar = c(fbar(stkProj[,ac(yrAssess:yrTACp1)])),
  ssb = c(ssb(stkProj[,ac(yrAssess:yrTACp1)]))
)

df <- merge(stfRef, stfDet, all = T)
df <- pivot_longer(df, cols = c(catch, landings, fbar, ssb),
                   names_to = "variable", values_to = "value")
df <- df |>
  filter(
    (variable %in% c("catch", "landings", "fbar") & year <= yrTAC) |
      (variable %in% c("ssb") & year <= yrTACp1))


df2 <- pivot_wider(df, names_from = model, values_from = value)
df2$percErr <- round((df2$FLR - df2$WGBIE)/df2$WGBIE * 100, 1)

ggplot(df) + aes(x = year, y = value, group = model, color = model, shape = model) +
  facet_wrap(~variable, scales = "free_y") +
  geom_line() +
  geom_point(size = 3, stroke = 1) +
  scale_shape_discrete(solid = F) +
  coord_cartesian(ylim = c(0, NA)) +
  theme_bw()


kable(df2, digits = 3)


ggplot(df2) + aes(x = year, y = percErr) +
  facet_wrap(~variable) +
  geom_col(fill = 4) +
  geom_hline(yintercept = 0, linetype = 1) +
  geom_hline(yintercept = c(-10,10), linetype = 3) +
  theme_bw()
