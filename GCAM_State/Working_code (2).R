x <- driver(stop_after = "module_gcamindia_batch_HDDCDD_constdds_xml")
x <- driver(stop_before = "module_gcamindia_L244.building_INDIA")

debug(gcamdata:::module_gcamindia_L244.building_INDIA)
gcamdata:::module_gcamindia_L244.building_INDIA ("MAKE",x)
library(devtools)
devtools::load_all(".")
library(gcamdata)
library(drake)

all_data <- driver_drake(return_inputs_of = "module_energy_L1326.aluminum")
x <- driver_drake(stop_after = "module_gcamindia_batch_building_India_xml")

#electricity india XML level 3
a <- driver(stop_after = "module_gcamindia_batch_cement_India_xml")
#recreate electd india xml
a <- driver(stop_after = "module_gcamindia_batch_electd_xml")
#recreate en_transformation xml
a <- driver(stop_after = "module_energy_batch_en_transformation_xml")
#recreate en_transformation_india xml
a <- driver(stop_after = "module_gcamindia_batch_en_transformation_xml")
#electricity  XML level 3
a <- driver(stop_after = "module_energy_batch_electricity_xml")
#resource xml level 3 recreate
a <- driver(stop_after = "module_energy_batch_resources_xml")
#resource india xml level 3 recreate
a <- driver(stop_after = "module_gcamindia_batch_resources_xml")
#building XML level 3
a <- driver(stop_after = "module_gcamindia_batch_building_India_xml")
driver_drake()

install.packages("remotes")
remotes::install_version("tibble","2.1.1")
driver()

#Rebuilding the changes in the package
  #1 Go to build and click on more
  #2 Click clean and rebuild after any changes to input files
  #3 If there are any changes to codes, from the more drop-down first document and then clean and rebuild

#For running the whole gcam-data-system
driver()

#If any changes are required in the codes or to explore how each command in a chunk is working, enter debugg mode
  #1 Run the driver before that chunk to generate the input filesx
      #driver loads the files into the variable 'x'
devtools::load_all(".")
library(gcamdata)

x <- driver(stop_before = "module_gcamindia_LB123.electricity")

#2 To enter debugg mode, run the below written commands together
debug(gcamdata:::module_gcamindia_LB123.electricity)
gcamdata:::module_gcamindia_LB123.electricity("MAKE",x)

driver(stop_after="zchunk_batch_en_distribution_xml")

x <- driver(stop_before = "module_india_LA154.Transport")

debug(gcamdata:::module_energy_LA154.transportation_UCD)
gcamdata:::module_energy_LA154.transportation_UCD ("MAKE",x)
devtools::load_all(".")
library(gcamdata)
a <- driver(stop_after = "module_batch_transportation_India_xml")

 #producing test outputs
a <- driver(stop_after="module_aglu_L221.land_input_1")
a <- driver(stop_after = "module_aglu_batch_land_input_1_xml")
a <- driver(stop_after = "module_aglu_batch_land_input_2_xml")
a <- driver(stop_after = "module_aglu_batch_land_input_3_IRR_xml")
a <- driver(stop_after = "module_aglu_batch_land_input_4_IRR_MGMT_xml")
a <- driver(stop_after = "module_aglu_batch_land_input_5_IRR_MGMT_xml")
a <- driver(stop_after = "module_aglu_batch_protected_land_input_2_xml")
a <- driver(stop_after = "module_aglu_batch_protected_land_input_3_xml")



a <- driver(stop_after = "module_aglu_batch_demand_input_xml")
a <- driver(stop_after = "module_aglu_batch_bio_trade_xml")
a <- driver(stop_after = "module_aglu_batch_ag_trade_xml")
a <- driver(stop_after = "module_emissions_batch_all_aglu_emissions_IRR_MGMT_xml")
a <- driver(stop_after = "module_emissions_batch_all_unmgd_emissions_xml")
a <- driver(stop_after = "module_emissions_batch_all_protected_unmgd_emissions_xml")

a <- driver(stop_after = "module_gcamindia_batch_building_India_xml")
a <- driver(stop_after = "module_gcamindia_batch_electricity_xml")
a <- driver(stop_after = "module_gcamindia_batch_cement_India_xml")
#Writing Output
write.csv(L122.LC_bm2_R_HarvCropLand_Yh_GLU, "L122.LC_bm2_R_HarvCropLand_Yh_GLU.csv")


#dstrace function
gcamdata::dstrace("L122.FeedOut_Mt_R_C_Yh", direction = "both", gcam_data_map = GCAM_DATA_MAP)
driver_drake()
