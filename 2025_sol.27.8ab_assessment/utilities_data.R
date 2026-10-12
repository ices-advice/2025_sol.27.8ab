################################################################################
### Functions used to shape data
################################################################################

#' Title total_stock
#' Adds up totals for stock objects -
# computes landings, discards, catch, stock totals and sets units
#'
#' @param stock
#' @param units
#'
#' @return
#' @export
#'
#' @examples
total_stock <- function(stock_flr, units_flr) {
  FLCore::landings(stock_flr) <- FLCore::computeLandings(stock_flr)
  FLCore::discards(stock_flr) <- FLCore::computeDiscards(stock_flr)
  FLCore::catch.n(stock_flr)  <- FLCore::landings.n(stock_flr) + FLCore::discards.n(stock_flr)
  FLCore::catch.wt(stock_flr) <- (FLCore::landings.n(stock_flr) * FLCore::landings.wt(stock_flr) +
                                    FLCore::discards.n(stock_flr) * FLCore::discards.wt(stock_flr)) / FLCore::catch.n(stock_flr)
  # Make Catch=Landings if no discards
  if (sum(FLCore::catch.n(stock_flr), na.rm = TRUE) == 0) {
    FLCore::catch.n(stock_flr)  <- FLCore::landings.n(stock_flr)
    FLCore::catch.wt(stock_flr)  <- FLCore::landings.wt(stock_flr)
  }
  FLCore::catch(stock_flr) <- FLCore::computeCatch(stock_flr)
  FLCore::stock(stock_flr) <- FLCore::computeStock(stock_flr)
  FLCore::units(stock_flr)[1:17] <- as.list(c(rep(units_flr, 4), "NA", "NA", "f", "NA", "NA"))
  return(stock_flr)
}
###-----------------------------------------------------------------------------
remotes::install_gitlab(
  repo = "emh-lab/emhutils",
  host = "https://gitlab.ifremer.fr",
  force = FALSE,
  quiet = FALSE
)
library("emhUtils")
###-----------------------------------------------------------------------------

#' Data frame to FLR index
#'
#' @param df_index a data frame
#' @param name name of the index
#' @param type type of the index
#' @param startf starts of the index
#' @param endf ends of the index
#'
#' @return
#' @export
#'
#' @examples
flr_df_to_index <- function(df_index,
                            name = "Survey",
                            type = "Survey tunning fleet",
                            startf = 0,
                            endf = 1) {

  df_index_export <- df_index %>%
    dplyr::ungroup() %>%
    dplyr::select(Age, Year, Estimate) %>%
    tidyr::pivot_wider(id_cols = Age,
                names_from = Year,
                values_from = Estimate) %>%
    dplyr::select(-Age)

  index_year_range <- range(emhUtils::as_numeric_factor(df_index$Year))
  index_age_range <- range(emhUtils::as_numeric_factor(df_index$Age))

  ### transform into a fl quant object
  id_fr_orhago <- FLCore::FLQuant(as.matrix(df_index_export),
                          dimnames = list(age = index_age_range[1]:index_age_range[2],
                                          year = index_year_range[1]:index_year_range[2]))

  ###-----------------------------------------------------------------------------
  ### merge into the flr indices object
  id_fr_orhago_fl <- FLCore::FLIndex(index = id_fr_orhago,
                             name = name,
                             type = type)
  id_fr_orhago_fl <- FLCore::FLIndices(id_fr_orhago_fl)
  id_fr_orhago_fl@names <- name
  range(id_fr_orhago_fl@.Data[[1]])["startf"] <- startf
  range(id_fr_orhago_fl@.Data[[1]])["endf"] <- endf

  return(id_fr_orhago_fl)
}
