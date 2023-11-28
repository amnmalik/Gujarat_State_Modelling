#' module_gcamindia_LA1321.cement
#'
#' Allocate across the states national cement production, input-output cofficients, and energy inputs to cement production
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs,
#' a vector of output names, or (if \code{command} is "MAKE") all
#' the generated outputs: \code{L1321.out_Mt_india_state_cement_Yh}, \code{L1321.IO_GJkg_india_state_cement_F_Yh}, \code{L1321.in_india_EJ_state_cement_F_Y}.
#' @details The tables for cement production, i.e., out, and energy inputs, i.e., in, were calculated by applying state shares to national data.
#' @details The state shares were determined by the states' relative values of cement shipments.
#' @details The input-out coefficients were downscaled to the states in proportation to the national data.
#' @importFrom assertthat assert_that
#' @importFrom dplyr filter mutate select
#' @importFrom tidyr gather spread
#' @author AM Dec20
module_gcamindia_LA1321.cement <- function(command, ...) {
  if(command == driver.DECLARE_INPUTS) {
    return(c(FILE = "gcam-india/A32.india_state_cement_GVA",
             "L1321.out_Mt_R_cement_Yh",
             "L1321.IO_GJkg_R_cement_F_Yh",
             "L1321.in_EJ_R_cement_F_Y"))
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(c("L1321.out_Mt_india_state_cement_Yh",
             "L1321.IO_GJkg_india_state_cement_F_Yh",
             "L1321.in_EJ_india_state_cement_F_Y"))
  } else if(command == driver.MAKE) {

    all_data <- list(...)[[1]]

    . <- NULL   # silence package check notes

    # Load required inputs
    A32.india_state_cement_GVA <- get_data(all_data, "gcam-india/A32.india_state_cement_GVA")
    L1321.out_Mt_R_cement_Yh <- get_data(all_data, "L1321.out_Mt_R_cement_Yh")
    L1321.IO_GJkg_R_cement_F_Yh <- get_data(all_data, "L1321.IO_GJkg_R_cement_F_Yh")
    L1321.in_EJ_R_cement_F_Y <- get_data(all_data, "L1321.in_EJ_R_cement_F_Y")

    # ===================================================

    variable <- value_state <- value_share <- value_national <- NAICS_code <- state <-
      VoS_thousUSD <- sector <- fuel <- year <- value <- GCAM_region_ID <- NULL   # silence package check notes

    #State shares of cement production economic value
    Cement_share_state <- A32.india_state_cement_GVA %>%
      mutate(value_share = Value) %>%
      select(state, value_share)

    # Creating a list of states
    # This will be helpful when expanding the national table into state-level
    # Note that not every state has cement manufacturing
    state_list <- unique(Cement_share_state$state)

    # This section is calculating state-level data by multiplying the state share by the USA component in the global data
    # This will generate an output table: Cement production by state / historical year
    L1321.out_Mt_R_cement_Yh %>%
      filter(GCAM_region_ID == gcam.INDIA_CODE) %>% # Filtering for the USA component
      repeat_add_columns(tibble(state = state_list)) %>% # Expanding the table to the state-level
      left_join_error_no_match(Cement_share_state, by = "state") %>% # Adding the state share we calculated above
      mutate(value = value * value_share) %>% # Multiplying the national amount with the state share
      select(state, sector, year, value) ->
      L1321.out_Mt_state_cement_Yh

    # This section is downscaling the national input/output (IO) coefficients to the state level
    # Assuming all states have the same IO coefficients for heat, electricity, and limestone
    # This will generate an output table: Input-output coefficients of cement production by state / fuel / historical year
    L1321.IO_GJkg_R_cement_F_Yh %>%
      filter(GCAM_region_ID == gcam.INDIA_CODE) %>%
      repeat_add_columns(tibble(state = state_list)) %>%
      select(state, sector, fuel, year, value) ->
      L1321.IO_GJkg_state_cement_F_Yh

    # Calculating energy inputs to cement production by state
    # Again, this section is downscaling the national data to the state level, using the state share calculated above
    # Note that this assumes the same fuel blend in all states
    # This will generate an output table: Energy inputs to cement production by state / fuel / historical year
    L1321.in_EJ_R_cement_F_Y %>%
      filter(GCAM_region_ID == gcam.INDIA_CODE) %>%
      repeat_add_columns(tibble(state = state_list)) %>%
      left_join_error_no_match(Cement_share_state, by = "state") %>%
      mutate(value = value_share * value) %>%
      select(state, sector, fuel, year, value) ->
      L1321.in_EJ_state_cement_F_Y

    # ===================================================

    L1321.out_Mt_state_cement_Yh %>%
      add_title("Cement production by state / historical year ") %>%
      add_units("Mt") %>%
      add_comments("downscaling national data using state shares") %>%
      add_comments("these state shares were calculated to be proportional to the their values of cement shipments") %>%
      add_legacy_name("L1321.out_Mt_state_cement_Yh") %>%
      add_precursors("gcam-india/A32.india_state_cement_GVA", "L1321.out_Mt_R_cement_Yh") ->
      L1321.out_Mt_india_state_cement_Yh

    L1321.IO_GJkg_state_cement_F_Yh %>%
      add_title("Input-output coefficients of cement production by state / fuel / historical year") %>%
      add_units("GJ/kg and kg/kg") %>%
      add_comments("downscaling national data assuming the same IO coefficients for each respective fuel") %>%
      add_legacy_name("L1321.IO_GJkg_state_cement_F_Yh") %>%
      add_precursors("L1321.IO_GJkg_R_cement_F_Yh") ->
      L1321.IO_GJkg_india_state_cement_F_Yh

    L1321.in_EJ_state_cement_F_Y %>%
      add_title("Energy inputs to cement production by state / fuel / historical year") %>%
      add_units("EJ") %>%
      add_comments("downscaling national data using state shares") %>%
      add_comments("these state shares were calculated to be proportional to the their values of cement shipments") %>%
      add_legacy_name("L1321.in_EJ_state_cement_F_Y") %>%
      add_precursors("gcam-india/A32.india_state_cement_GVA", "L1321.in_EJ_R_cement_F_Y") ->
      L1321.in_EJ_india_state_cement_F_Y

    return_data(L1321.out_Mt_india_state_cement_Yh, L1321.IO_GJkg_india_state_cement_F_Yh, L1321.in_EJ_india_state_cement_F_Y)
  } else {
    stop("Unknown command")
  }
}
