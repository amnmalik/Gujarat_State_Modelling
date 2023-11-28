#' module_gcamindia_L210.agg_onshore_wind_resource_india
#'
#' Calculate supply sector, subsector, and technology information for the agg_elect_td_ind sector
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs.
#' @details Calculate shareweights, cost, price elasticity, calibrated, and other data for the wind resource sector
#' @importFrom assertthat assert_that
#' @importFrom dplyr filter mutate select
#' @importFrom tidyr gather spread
#' @author AM Feb21
module_gcamindia_L210.agg_onshore_wind_resource_india <- function(command, ...) {
  if(command == driver.DECLARE_INPUTS) {
    return(c(FILE = "gcam-india/A22.india_agg_wind_resource_structure"))
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(c("L210.india_state_RenewRsrc_agg_india"))
  } else if(command == driver.MAKE) {

    all_data <- list(...)[[1]]

    # Silence package notes
     region <- renewresource <- smooth.renewable.subresource <- maxSubResource <- mid.price <-
       curve.exponent <- output.unit <- price.unit <- market<- NULL


    # Load required inputs
     A22.india_agg_wind_resource_structure <- get_data(all_data, "gcam-india/A22.india_agg_wind_resource_structure")

    # ===================================================

     L210.india_state_RenewRsrc_agg_india <- A22.india_agg_wind_resource_structure %>%
       mutate(region = "India") %>%
       select(LEVEL2_DATA_NAMES[["WindResNew"]]) %>%
       unique()

    # ===================================================


       L210.india_state_RenewRsrc_agg_india %>%
      add_title("Smth renew res infortation for india wind sector") %>%
      add_units("Unitless") %>%
      add_comments("Smth renew res infortation for india wind sector") %>%
      add_legacy_name("L210.india_state_RenewRsrc_agg_india") %>%
      add_precursors("gcam-india/A22.india_agg_wind_resource_structure") ->
         L210.india_state_RenewRsrc_agg_india


    return_data(L210.india_state_RenewRsrc_agg_india)
  } else {
    stop("Unknown command")
  }
}
