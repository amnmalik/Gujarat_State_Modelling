#' module_gcamindia_batch_cement_India_xml
#'
#' Construct XML data structure for \code{cement_India.xml}.
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs,
#' a vector of output names, or (if \code{command} is "MAKE") all
#' the generated outputs: \code{cement_India.xml}. The corresponding file in the
#' original data system was \code{batch_cement_India_xml.R} (gcamusa XML).
module_gcamindia_batch_cement_India_xml <- function(command, ...) {
  if(command == driver.DECLARE_INPUTS) {
    return(c("L210.DeleteUnlimitRsrc_Indialimestone",
             "L210.india_state_UnlimitRsrc_limestone",
             "L210.india_state_UnlimitRsrcPrice_limestone",
             "L2321.DeleteSupplysector_Indiacement",
             "L2321.Supplysector_cement_India",
             "L2321.FinalEnergyKeyword_cement_India",
             "L2321.SubsectorLogit_cement_India",
             "L2321.SubsectorShrwtFllt_cement_India",
             "L2321.SubsectorInterp_cement_India",
             "L2321.StubTech_cement_India",
             "L2321.PerCapitaBased_cement_India",
             "L2321.PriceElasticity_cement_India",
             "L2321.IncomeElasticity_cement_gcam3_India",
             "L2321.DeleteFinalDemand_Indiacement",
             "L2321.StubTechProd_cement_India",
             "L2321.StubTechCoef_cement_India",
             "L2321.StubTechCalInput_cement_heat_India",
             "L2321.StubTechMarket_cement_India",
             "L2321.BaseService_cement_India"))
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(c(XML = "cement_India.xml"))
  } else if(command == driver.MAKE) {

    all_data <- list(...)[[1]]

    # Load required inputs
    L210.DeleteUnlimitRsrc_Indialimestone <- get_data(all_data, "L210.DeleteUnlimitRsrc_Indialimestone")
    L210.india_state_UnlimitRsrc_limestone <- get_data(all_data, "L210.india_state_UnlimitRsrc_limestone")
    L210.india_state_UnlimitRsrcPrice_limestone <- get_data(all_data, "L210.india_state_UnlimitRsrcPrice_limestone")
    L2321.DeleteSupplysector_Indiacement <- get_data(all_data, "L2321.DeleteSupplysector_Indiacement")
    L2321.FinalEnergyKeyword_cement_India <- get_data(all_data, "L2321.FinalEnergyKeyword_cement_India")
    L2321.SubsectorLogit_cement_India <- get_data(all_data, "L2321.SubsectorLogit_cement_India")
    L2321.SubsectorShrwtFllt_cement_India <- get_data(all_data, "L2321.SubsectorShrwtFllt_cement_India")
    L2321.SubsectorInterp_cement_India <- get_data(all_data, "L2321.SubsectorInterp_cement_India")
    L2321.StubTech_cement_India <- get_data(all_data, "L2321.StubTech_cement_India")
    L2321.PerCapitaBased_cement_India <- get_data(all_data, "L2321.PerCapitaBased_cement_India")
    L2321.PriceElasticity_cement_India <- get_data(all_data, "L2321.PriceElasticity_cement_India")
    L2321.IncomeElasticity_cement_gcam3_India <- get_data(all_data, "L2321.IncomeElasticity_cement_gcam3_India")
    L2321.DeleteFinalDemand_Indiacement <- get_data(all_data, "L2321.DeleteFinalDemand_Indiacement")
    L2321.Supplysector_cement_India <- get_data(all_data, "L2321.Supplysector_cement_India")
    L2321.StubTechProd_cement_India <- get_data(all_data, "L2321.StubTechProd_cement_India")
    L2321.StubTechCoef_cement_India <- get_data(all_data, "L2321.StubTechCoef_cement_India")
    L2321.StubTechCalInput_cement_heat_India <- get_data(all_data, "L2321.StubTechCalInput_cement_heat_India")
    L2321.StubTechMarket_cement_India <- get_data(all_data, "L2321.StubTechMarket_cement_India")
    L2321.BaseService_cement_India <- get_data(all_data, "L2321.BaseService_cement_India")

    # ===================================================

    # Produce outputs
    create_xml("cement_India.xml") %>%
      add_xml_data(L210.DeleteUnlimitRsrc_Indialimestone, "DeleteUnlimitRsrc") %>%
      add_xml_data(L210.india_state_UnlimitRsrc_limestone, "UnlimitRsrc") %>%
      add_xml_data(L210.india_state_UnlimitRsrcPrice_limestone, "UnlimitRsrcPrice") %>%
      add_xml_data(L2321.DeleteSupplysector_Indiacement, "DeleteSupplysector") %>%
      add_xml_data(L2321.DeleteFinalDemand_Indiacement, "DeleteFinalDemand") %>%
      add_logit_tables_xml(L2321.Supplysector_cement_India, "Supplysector") %>%
      add_xml_data(L2321.FinalEnergyKeyword_cement_India, "FinalEnergyKeyword") %>%
      add_logit_tables_xml(L2321.SubsectorLogit_cement_India, "SubsectorLogit") %>%
      add_xml_data(L2321.SubsectorShrwtFllt_cement_India, "SubsectorShrwtFllt") %>%
      add_xml_data(L2321.SubsectorInterp_cement_India, "SubsectorInterp") %>%
      add_xml_data(L2321.StubTech_cement_India, "StubTech") %>%
      add_xml_data(L2321.PerCapitaBased_cement_India, "PerCapitaBased") %>%
      add_xml_data(L2321.PriceElasticity_cement_India, "PriceElasticity") %>%
      add_xml_data(L2321.IncomeElasticity_cement_gcam3_India, "IncomeElasticity") %>%
      add_xml_data(L2321.StubTechProd_cement_India, "StubTechProd") %>%
      add_xml_data(L2321.StubTechCoef_cement_India, "StubTechCoef") %>%
      add_xml_data(L2321.StubTechCalInput_cement_heat_India, "StubTechCalInput") %>%
      add_xml_data(L2321.StubTechMarket_cement_India, "StubTechMarket") %>%
      add_xml_data(L2321.BaseService_cement_India, "BaseService") %>%
      add_precursors("L210.DeleteUnlimitRsrc_Indialimestone",
                     "L210.india_state_UnlimitRsrc_limestone",
                     "L210.india_state_UnlimitRsrcPrice_limestone",
                     "L2321.DeleteSupplysector_Indiacement",
                     "L2321.Supplysector_cement_India",
                     "L2321.FinalEnergyKeyword_cement_India",
                     "L2321.SubsectorLogit_cement_India",
                     "L2321.SubsectorShrwtFllt_cement_India",
                     "L2321.SubsectorInterp_cement_India",
                     "L2321.StubTech_cement_India",
                     "L2321.PerCapitaBased_cement_India",
                     "L2321.PriceElasticity_cement_India",
                     "L2321.IncomeElasticity_cement_gcam3_India",
                     "L2321.DeleteFinalDemand_Indiacement",
                     "L2321.StubTechProd_cement_India",
                     "L2321.StubTechCoef_cement_India",
                     "L2321.StubTechCalInput_cement_heat_India",
                     "L2321.StubTechMarket_cement_India",
                     "L2321.BaseService_cement_India") ->
      cement_India.xml

    return_data(cement_India.xml)
  } else {
    stop("Unknown command")
  }
}
