#' module_socio_batch_India_update_xml
#'
#' To update the data for socioeconomics
#'
#' @author Malyan_Ankur_CEEW (26Aug20)

module_socio_batch_India_update_xml <- function(command, ...) {

  if(command == driver.DECLARE_INPUTS) {
    return(c(FILE = "L102.India_BaseGDP",
             FILE = "L102.India_LaborProd_rate",
             FILE = "L102.India_LaborForce",
             FILE = "L102.India_Population"))
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(c(XML = "socioeconomics_India_update.xml"))
  } else if(command == driver.MAKE) {

    all_data <- list(...)[[1]]

    socioeconomics_India_update.xml <- NULL  # silence package check notes

    # Load required inputs
    L102.India_BaseGDP <- get_data(all_data, "L102.India_BaseGDP")
    L102.India_LaborProd_rate <- get_data(all_data, "L102.India_LaborProd_rate")
    L102.India_LaborForce <- get_data(all_data, "L102.India_LaborForce")
    L102.India_Population <- get_data(all_data, "L102.India_Population")

    # Produce output
    create_xml("socioeconomics_India_update.xml") %>%
      add_xml_data(L102.India_Population, "Pop") %>%
      add_xml_data(L102.India_BaseGDP, "BaseGDP") %>%
      add_xml_data(L102.India_LaborForce, "LaborForce") %>%
      add_xml_data(L102.India_LaborProd_rate, "LaborProductivity") %>%
      add_precursors("L102.India_Population", "L102.India_BaseGDP", "L102.India_LaborForce", "L102.India_LaborProd_rate") ->
      socioeconomics_India_update.xml

    return_data(socioeconomics_India_update.xml)
  } else {
    stop("Unknown command")
  }
}
