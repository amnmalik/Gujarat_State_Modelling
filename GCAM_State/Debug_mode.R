x <- driver(stop_before = "module_energy_LA1231.elec_tech")

debug(gcamdata:::module_data_Maddison_population)
gcamdata:::module_data_Maddison_population ("MAKE",x)
devtools::load_all(".")
library(gcamdata)
a <- driver(stop_after = "module_gcamindia_batch_electricity_xml")
