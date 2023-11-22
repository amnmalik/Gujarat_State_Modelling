#' module_socio_India_update
#'
#' To update the data for socioeconomics
#'
#' @author

module_socio_India_update <- function(command, ...) {

  if(command == driver.DECLARE_INPUTS) {
    return(c(FILE = "gcam-india/India_BaseGDP",
             FILE = "gcam-india/India_LaborProd_rate",
             FILE = "gcam-india/India_LaborForce_HighGR",
             FILE = "gcam-india/India_Population",
             FILE = "gcam-india/A10.india_pop_rural_urban_share",
             FILE = "gcam-india/A10.india_GDP_rural_urban_share",
             FILE = "gcam-india/India_GDP"))
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(c("L102.India_BaseGDP",
             "L102.India_LaborProd_rate",
             "L102.India_LaborForce",
             "L102.India_Population",
             "L100.india_state_pop_ruralurban",
             "L100.india_state_pcGDP_thous90usd_ruralurban"))
  } else if(command == driver.MAKE) {

    all_data <- list(...)[[1]]

    socioeconomics_India_update.xml <- NULL  # silence package check notes

    # Load required inputs
    India_BaseGDP <- get_data(all_data, "gcam-india/India_BaseGDP")
    India_LaborProd_rate <- get_data(all_data, "gcam-india/India_LaborProd_rate")
    India_LaborForce <- get_data(all_data, "gcam-india/India_LaborForce_HighGR")
    India_Population <- get_data(all_data, "gcam-india/India_Population")
    A10.india_pop_rural_urban_share <- get_data(all_data, "gcam-india/A10.india_pop_rural_urban_share") %>% gather_years
    A10.india_GDP_rural_urban_share <- get_data(all_data, "gcam-india/A10.india_GDP_rural_urban_share") %>% gather_years
    India_GDP <- get_data(all_data, "gcam-india/India_GDP") %>% gather_years()

    #Mutating Data

    L102.India_BaseGDP <- India_BaseGDP %>% mutate (region = region) %>%
      mutate(baseGDP = baseGDP*1)

    L102.India_LaborProd_rate <- India_LaborProd_rate %>% mutate (region = region)%>%
      mutate(laborproductivity = laborproductivity*1)

    L102.India_LaborForce <- India_LaborForce %>% mutate (region = region)%>%
      mutate(laborforce = laborforce*1)

    L102.India_Population <- India_Population %>% mutate (region = region)%>%
      mutate(totalPop = totalPop*1)

    #Population Rural Urban Commercial estimations

    L100.india_pop_rural <- L102.India_Population %>%
      mutate(sector = 'resid rural') %>% select(region, sector, year, value = totalPop) %>%
      filter(year %in% MODEL_BASE_YEARS) %>%
      left_join(A10.india_pop_rural_urban_share, by = c("sector","year")) %>%
      mutate(rural_pop = value.x * value.y) %>%
      select (region, sector, year, rural_pop)

      L100.india_pop_urban <- L102.India_Population %>%
      mutate(sector = 'resid urban') %>% select(region, sector, year, value = totalPop) %>%
      filter(year %in% MODEL_BASE_YEARS) %>%
      left_join_keep_first_only(L100.india_pop_rural %>%
                                  mutate(sector = 'resid urban'), by = c("sector", "year")) %>%
      mutate(urban_pop = value - rural_pop) %>%
      select (region=region.x, sector, year, urban_pop)
      L100.india_pop_rural <- L100.india_pop_rural %>% rename (population = rural_pop)
      L100.india_pop_urban <- L100.india_pop_urban %>% rename (population = urban_pop)

      L100.india_pop_com <- L102.India_Population %>% gather_years() %>% mutate(sector = 'comm') %>%
        filter(year %in% MODEL_BASE_YEARS) %>% select (region, sector, year, population = totalPop)

      L100.india_state_pop_ruralurban <- bind_rows(L100.india_pop_rural,L100.india_pop_urban,L100.india_pop_com) %>%
        mutate(value = population * CONV_ONES_THOUS) %>% rename (pop = population) %>%
        select (region, sector, year, pop)


    #GDP Rural Urban Commercial estimations

      L100.india_GDP_rural <- A10.india_GDP_rural_urban_share %>%
        left_join_keep_first_only(India_GDP, by = c('year'))%>%
        mutate(value = value.x * value.y) %>%
        select (region, sector, year, value)

      L100.india_GDP_urban <- L100.india_GDP_rural %>%
        left_join_error_no_match(India_GDP, by = 'year') %>%
        mutate(value = value.y - value.x) %>%
        select (-sector) %>%
        mutate (sector = 'resid urban') %>%
        mutate (region = region.x) %>%
        select (region = region.x, sector, year, value)

      L100.india_GDP_comm <- India_GDP %>%
        mutate (sector = 'comm')

      L100.india_state_GDP_ruralurban <- bind_rows(L100.india_GDP_rural,L100.india_GDP_urban,L100.india_GDP_comm) %>%
        filter(year %in% MODEL_BASE_YEARS)

      #GDP per capita estimations

      L100.india_state_pcGDP_thous90usd_ruralurban <- L100.india_state_GDP_ruralurban %>%
        left_join_keep_first_only(L100.india_state_pop_ruralurban, by = c('region', 'sector', 'year')) %>%
        mutate (pcGDP = value / pop) %>%
        select(region, sector, year, pcGDP)


    # Produce output
    L102.India_BaseGDP %>%
      add_title("Base GDP for India") %>%
      add_units("Million 1990 USD") %>%
      add_comments("Update for India") %>%
      add_legacy_name("L102.India_BaseGDP") %>%
      add_precursors("gcam-india/India_BaseGDP")  ->
      L102.India_BaseGDP

    L102.India_LaborProd_rate %>%
      add_title("Labor prodctivity rate update for India") %>%
      add_units("Unitless") %>%
      add_comments("Update for India") %>%
      add_legacy_name("L102.India_LaborProd_rate") %>%
      add_precursors("gcam-india/India_LaborProd_rate")  ->
      L102.India_LaborProd_rate

    L102.India_LaborForce %>%
      add_title("Labor Force update for India") %>%
      add_units("Unitless") %>%
      add_comments("Update for India") %>%
      add_legacy_name("L102.India_LaborForce") %>%
      add_precursors("gcam-india/India_LaborForce_HighGR")  ->
      L102.India_LaborForce

    L102.India_Population %>%
      add_title("Population update for India") %>%
      add_units("thausands") %>%
      add_comments("Update for India") %>%
      add_legacy_name("L102.India_Population") %>%
      add_precursors("gcam-india/India_Population")  ->
      L102.India_Population

    L100.india_state_pop_ruralurban %>%
      add_title("Population rural and urban update for India") %>%
      add_units("thausands") %>%
      add_comments("Update for India") %>%
      add_legacy_name("L100.india_state_pop_ruralurban") %>%
      add_precursors("gcam-india/A10.india_pop_rural_urban_share")  ->
      L100.india_state_pop_ruralurban

    L100.india_state_pcGDP_thous90usd_ruralurban %>%
      add_title("GDP per capita update for India") %>%
      add_units("thausands") %>%
      add_comments("Update for India") %>%
      add_legacy_name("L100.india_state_pcGDP_thous90usd_ruralurban") %>%
      add_precursors("gcam-india/A10.india_GDP_rural_urban_share",
                     "gcam-india/India_GDP")  ->
      L100.india_state_pcGDP_thous90usd_ruralurban

    return_data(L102.India_BaseGDP,L102.India_LaborProd_rate,L102.India_LaborForce,L102.India_Population,L100.india_state_pop_ruralurban,L100.india_state_pcGDP_thous90usd_ruralurban)
  } else {
    stop("Unknown command")
  }
}
