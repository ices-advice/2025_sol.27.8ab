###  Prepare figures for report

###  Before: tab_retro_analysis.csv
###  After: retro_analysis.png

library("icesTAF")

### -----------------------------------------------------------------------------
### Before loading ggplot2, make consistency plots for tunning fleets
### plot index time series
id_fr_bts_viii <- readRDS("bootstrap/data/sole_indices_bts_viii.rds")
df_id_fr_bts_viii <- id_fr_bts_viii %>%
  as.data.frame() %>%
  rownames_to_column(var = "Year") %>%
  pivot_longer(-Year,
               names_to = "Age",
               values_to = "Estimate") %>%
  mutate(Year = as_factor(Year),
         Age  = as_factor(Age))

### Transform into FLR object for consistency plot
flr_id_fr_bts_viii <-
  flr_df_to_index(df_id_fr_bts_viii,
                  name = "BTS-VIII",
                  startf = 11/12,
                  endf = 1)

flr_id_fr_bts_viii_consistency <- FLCore::plot(flr_id_fr_bts_viii[[1]],
                                               type = c("splom"))

saveRDS(flr_id_fr_bts_viii_consistency,
        file = "output/flr_id_fr_bts_viii_consistency.rds")
taf.png("bts_viii_consistency", res = 300)
print(flr_id_fr_bts_viii_consistency)
dev.off()

library("ggplot2")
library("patchwork")
library("ggplotFL")

### -----------------------------------------------------------------------------
df_catage <- flr2taf(flr_stock@landings.n) %>%
  mutate(Category = "Catch")

df_latage <- flr2taf(flr_stock@landings.n) %>%
  mutate(Category = "Landings")

df_datage <- flr2taf(flr_stock@discards.n) %>%
  mutate(Category = "Discards")

df_n_at_age_landings_discards <- bind_rows(df_latage, df_datage) %>%
  pivot_longer(cols = -c("Year", "Category"),
               names_to = "Age",
               values_to = "Numbers")

plot_n_at_age_year <- ggplot() +
  geom_col(data = df_n_at_age_landings_discards,
           aes(x = Age, y = Numbers, fill = Category)) +
  facet_wrap(~Year) +
  theme(strip.text = element_text(size = 7),
        strip.background = element_rect(colour = "black",
                                        linewidth = 1))
saveRDS(plot_n_at_age_year, "report/plot_n_at_age_year.rds")

### save retrospective analysis plot
taf.png("plot_n_at_age_year", res = 300)
plot(plot_n_at_age_year)
dev.off()

### -----------------------------------------------------------------------------
### plot index time series
indices <- FLCore::as.data.frame(flr_id_fr_bts_viii) %>%
  filter(slot == "index")
taf.png("TSLPUE", res = 300)
xyplot(log(data) ~ year | cname,
       data = indices, type = "b",
       scales = list(alternating = 1,
                     y = list(relation = "free", cex = 1.5),
                     x = list(cex = 1.2)),
       par.strip.text = list(cex = 1),
       groups = as.numeric(as.character(age)),
       auto.key = list(space = 'bottom', columns = 5, cex = 0.7),
       cex.lab = 1,
       xlab = "year", ylab = "log standardised indices")
dev.off()

### -----------------------------------------------------------------------------
### plot obs resids
taf.png("residuals_obs", res = 300)
plot(residuals_obs)
dev.off()

plot_residuals_obs <- ggplot(df_residuals_obs, aes(Year, as.factor(Age),
                                                   size = Residuals_abs,
                                                   fill = Value)) +
  geom_point(
    shape = 21,
    color = "black"
  ) +
  scale_size(name = NULL, range = c(0, 10)) +
  ylab("Age") +
  xlab("Year") +
  scale_fill_discrete(name = NULL) +
  theme(legend.position = "bottom", legend.box = "vertical") +
  facet_wrap(~ Fleet, nrow = length(attributes(residuals_obs)$fleetNames),
             scales = "free_y")
saveRDS(plot_residuals_obs,    "report/plot_residuals_obs.rds")


### -----------------------------------------------------------------------------
### plot processs resids
taf.png("residuals_process", res = 300)
plot(residuals_process)
dev.off()

plot_residuals_process <- ggplot(df_residuals_process,
                                 aes(Year, as.factor(Age),
                                     size = Residuals_abs,
                                     fill = Value)) +
  geom_point(
    shape = 21,
    color = "black"
  ) +
  scale_size(name = NULL, range = c(0, 10)) +
  ylab("Age") +
  xlab("Year") +
  scale_fill_discrete(name = NULL) +
  theme(legend.position = "bottom", legend.box = "vertical") +
  facet_wrap(~ Process, nrow = length(attributes(residuals_obs)$fleetNames),
             scales = "free_y")
saveRDS(plot_residuals_process,    "report/plot_residuals_process.rds")


### -----------------------------------------------------------------------------
### plot retro analysis

### save retrospective analysis plot
taf.png("retro_analysis", res = 300)
ggplot(data = list_df_retro$df_retro,
       aes(x = Year,
           y = Estimate,
           color = id_retro,
           group = id_retro)) +
  geom_line() +
  geom_point() +
  geom_ribbon(aes(ymin = Low, ymax = High), alpha = 0.1) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1)) +
  scale_x_discrete(breaks = levels(df_fit_summary$Year)[seq(1, length(levels(df_fit_summary$Year)), by = 5)]) +
  facet_wrap(~ Var, scales = "free", nrow = 3) +
  theme(legend.position="none")
dev.off()

### save retrospective analysis plot
if(!is.null(lo_fit_sam)) {
  taf.png("lo_analysis", res = 300)
  plot(lo_fit_sam)
  dev.off()
}
### -----------------------------------------------------------------------------
### bubble plot
taf.png("buble_catch", res = 300)
ggplot(catch.n(sam_stock), aes(year, as.factor(age), size = data)) +
  geom_point(shape = 21) +
  scale_size(range = c(1, 20)) +
  ylab("Age") +
  xlab("Year") +
  theme(legend.position = "none")
dev.off()

### -----------------------------------------------------------------------------
### bubble plot
taf.png("buble_landings", res = 300, width = 2000, height = 2000)
ggplot(landings.n(sam_stock), aes(year, as.factor(age), size = data)) +
  geom_point(
    shape = 21,
    # fill = "#56B4E9",
    color = "black"
  ) +
  scale_size(range = c(1, 20), name = "Landings") +
  ylab("Age") +
  xlab("Year") +
  theme(legend.position = "bottom")
dev.off()

### -----------------------------------------------------------------------------
### fbar with quantiles
taf.png("fbar", res = 300)
plot(rlnorm(1000, fbar(sam_stock), 0.15),
     probs = c(0.05, 0.10, 0.25, 0.50, 0.75, 0.90, 0.95)
)
dev.off()

### -----------------------------------------------------------------------------
### fbar with quantiles
plot_prop_biomass_at_age <- ggplot() +
  geom_col(data = df_B_at_age_stock, aes(x = Year, y = Prop_total_biomass, fill = Age)) +
  ylab("Proportion of biomass at age to total biomass")
saveRDS(plot_prop_biomass_at_age, "report/plot_prop_biomass_at_age.rds")

taf.png("fbar", res = 300)
plot_prop_biomass_at_age
dev.off()

### -----------------------------------------------------------------------------
### fbar SSB R Catch
###-----------------------------------------------------------------------------
plot_catch <-  ggplot() +
  geom_line(data = df_catch,
            aes(x = Year, y = Estimate,
                group = 1)) +
  geom_point(data = df_catch,
             aes(x = Year, y = Estimate,
                 group = 1)) +
  geom_ribbon(data = df_catch,
              aes(x = Year,
                  y = Estimate,
                  ymin = Low,
                  ymax = High,
                  group = 1), alpha = 0.1) +
  geom_point(data = df_catch_obs, aes(x = Year, y = Catch), shape = 4) +
  ylab("Catch") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1)) +
  scale_x_discrete(breaks = levels(df_fit_summary$Year)[seq(1, length(levels(df_fit_summary$Year)), by = 5)])

taf.png("catch", res = 300)
print(plot_catch)
dev.off()

saveRDS(plot_catch,
        "report/plot_catch.rds")
###-----------------------------------------------------------------------------
plot_rec <- ggplot(data = filter(df_fit_summary,
                                 Variable == "Rec"),
                   aes(x = Year, y = Estimate,
                       group = 1)) +
  geom_line() +
  geom_point() +
  geom_ribbon(aes(ymin = Low, ymax = High), alpha = 0.1) +
  ylab("Recruitment (age-2)") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1)) +
  scale_x_discrete(breaks = levels(df_fit_summary$Year)[seq(1, length(levels(df_fit_summary$Year)), by = 5)])

taf.png("rec", res = 300)
print(plot_rec)
dev.off()

saveRDS(plot_rec,
        "report/plot_rec.rds")
###-----------------------------------------------------------------------------
plot_fbar <- ggplot(data = filter(df_fit_summary,
                                  Variable == "Fbar"),
                    aes(x = Year, y = Estimate,
                        group = 1)) +
  geom_line() +
  geom_point()  +
  geom_ribbon(aes(ymin = Low, ymax = High), alpha = 0.1) +
  ylab(glue("F-bar ({fbar_min}-{fbar_max})")) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1)) +
  scale_x_discrete(breaks = levels(df_fit_summary$Year)[seq(1, length(levels(df_fit_summary$Year)), by = 5)])

taf.png("fbar", res = 300)
print(plot_fbar)
dev.off()

saveRDS(plot_fbar,
        "report/plot_fbar.rds")

###-----------------------------------------------------------------------------
plot_ssb <- ggplot(data = filter(df_fit_summary,
                                 Variable == "SSB"),
                   aes(x = Year, y = Estimate,
                       group = 1)) +
  geom_line() +
  geom_point() +
  geom_ribbon(aes(ymin = Low, ymax = High), alpha = 0.1) +
  ylab("SSB") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1)) +
  scale_x_discrete(breaks = levels(df_fit_summary$Year)[seq(1, length(levels(df_fit_summary$Year)), by = 5)])

taf.png("ssb", res = 300)
print(plot_ssb)
dev.off()

saveRDS(plot_ssb,
        "report/plot_ssb.rds")

###-----------------------------------------------------------------------------
plot_fbar_ref <- ggplot(data = filter(df_fit_summary,
                                      Variable == "Fbar"),
                        aes(x = Year, y = Estimate,
                            group = 1)) +
  geom_line() +
  geom_point()  +
  geom_ribbon(aes(ymin = Low, ymax = High), alpha = 0.1) +
  ylab("F-bar") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1),
        legend.title = element_blank()) +
  geom_hline(data = df_ref_points_f,
             aes(yintercept = value, color = variable),
             linewidth = 1) +
  scale_x_discrete(breaks = levels(df_fit_summary$Year)[seq(1, length(levels(df_fit_summary$Year)), by = 5)])

taf.png("plot_fbar_ref", res = 300)
print(plot_fbar_ref)
dev.off()

saveRDS(plot_fbar_ref,
        "report/plot_fbar_ref.rds")



plot_ssb_ref <- ggplot(data = filter(df_fit_summary,
                                     Variable == "SSB"),
                       aes(x = Year, y = Estimate,
                           group = 1)) +
  geom_line() +
  geom_point() +
  geom_ribbon(aes(ymin = Low, ymax = High), alpha = 0.1) +
  ylab("SSB") +
  geom_hline(data = df_ref_points_biomass,
             aes(yintercept = value, color = variable),
             linewidth = 1) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1),
        legend.title = element_blank()) +
  scale_x_discrete(breaks = levels(df_fit_summary$Year)[seq(1, length(levels(df_fit_summary$Year)), by = 5)])

taf.png("plot_ssb_ref", res = 300)
print(plot_ssb_ref)
dev.off()

saveRDS(plot_ssb_ref,
        "report/plot_ssb_ref.rds")

###-----------------------------------------------------------------------------
### Summary plot
plot_summary <- (plot_catch + plot_rec +
                   plot_fbar + plot_ssb) +
  plot_layout(nrow = 2,
              byrow = TRUE,
              guides = "collect") &
  theme(legend.position = "bottom")

taf.png("summary", res = 300)
print(plot_summary)
dev.off()

saveRDS(plot_summary,
        "report/plot_summary.rds")

###-----------------------------------------------------------------------------
plot_summary_ssb_fbar_ref <- (plot_fbar_ref + plot_ssb_ref) +
  plot_layout(nrow = 1,
              byrow = TRUE,
              guides = "collect") &
  theme(legend.position = "bottom")

taf.png("summary_ssb_fbar_ref", res = 300)
print(plot_summary_ssb_fbar_ref)
dev.off()


saveRDS(plot_summary_ssb_fbar_ref,
        "report/plot_summary_ssb_fbar_ref.rds")

###-----------------------------------------------------------------------------
plot_summary_ref <- (plot_catch + plot_rec +
                       plot_fbar_ref + plot_ssb_ref) +
  plot_layout(nrow = 2,
              byrow = TRUE,
              guides = "collect") &
  theme(legend.position = "bottom")

taf.png("plot_summary_ref", res = 300)
print(plot_summary_ref)
dev.off()

saveRDS(plot_summary_ref,
        "report/plot_summary_ref.rds")

###-----------------------------------------------------------------------------
### Plot catchability
taf.png("catchability", res = 300)
qtableplot(qtable(fit_sam))
dev.off()

###-----------------------------------------------------------------------------
### Data fit
### Catch
taf.png("fitplot_catch", res = 300)
fitplot(fit_sam, fleets = 1)
dev.off()

### Fleets
taf.png("fitplot_bts_viii", res = 300)
fitplot(fit_sam, fleets = 2)
dev.off()

taf.png("fitplot_lpue_trawler", res = 300)
fitplot(fit_sam, fleets = 3)
dev.off()

###-----------------------------------------------------------------------------
### F at age
plot_f_at_age <- ggplot(df_f_at_age, aes(Year, FatAge)) +
  geom_line(aes(color = Age, group = Age)) +
  theme_bw() +
  geom_text(label = df_f_at_age$Age,
            aes(colour = Age)) +
  theme(legend.position = "none") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))

saveRDS(plot_f_at_age,
        "report/plot_f_at_age.rds")

taf.png("f_at_age", res = 300)
print(plot_f_at_age)
dev.off()

###-----------------------------------------------------------------------------
###
plot_observation_error <- ggplot() +
  geom_col(data = df_observation_error,
           aes(x = fct_reorder(name, sd), y = sd, fill = fleet) ) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1)) +
  ylab("Observation error") +
  xlab("")

saveRDS(plot_observation_error,
        "report/plot_observation_error.rds")


taf.png("observation_error", res = 300)
print(plot_observation_error)
dev.off()

###-----------------------------------------------------------------------------
###
taf.png("srplot", res = 200, width = 1600, height = 1200)
print(srplot(fit_sam))
dev.off()
