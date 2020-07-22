############################################################
############################################################
##
## wrangle school test data, prep for analysis
##  
##
## By: Hunter York, hyork@uw.edu
############################################################
############################################################
## This file pulls in learning outcomes data, standardizes
## it, and exports it as a file named "data_long."
## No analytical methods are used in this script save for
## simple arithmetic
############################################################
############################################################

############################################################
## Setup
############################################################

library(stringr)
library(data.table)
library(sf)
library(dplyr)

#
data_append <- list.files("/home/j/WORK/01_covariates/02_inputs/education/update_2020/geospatial_final_project/reference", 
                          full.names = T, pattern = "achievement") %>% 
  lapply(read.csv) %>% rbindlist(fill = T)

#melt
data_melt <- melt(data_append, id.vars = c("STNAM", "LEAID"), measure.vars = names(data_append)[names(data_append) %like% "MTH|RL"])
data_append <- NULL
#data_melt[, value := as.numeric(value)]
data_melt <- data_melt[!is.na(value) & value != ""]

#create vars
data_melt[variable %like% "MTH", subject := "math"]
data_melt[variable %like% "RL", subject := "reading"]
data_melt[,prefix := tstrsplit(variable, "_", keep = 1)]
data_melt[tolower(variable) %like% "numvalid", measure := "numvalid"]
data_melt[tolower(variable) %like% "pctprof", measure := "pctprof"]
data_melt[, grade := tstrsplit(variable, "_", keep = 2)]
data_melt[, grade := gsub("MTH|RLA", "", grade)]
data_melt[, grade := as.numeric(substr(grade, 1,2))]
data_melt[variable %like% "HS", grade := 12]
data_melt[, year := as.numeric(str_sub(variable,-4,-1))]
data_melt[,variable := NULL]

#if a range is provided, thke the mean
data_melt[measure == "pctprof" & value %like% "-", upper := tstrsplit(value, "-", keep = 2, type.convert = T)]
data_melt[measure == "pctprof" & value %like% "-", lower := tstrsplit(value, "-", keep = 1, type.convert = T)]
data_melt[measure == "pctprof" & value %like% "-", value := (upper + lower)/ 2]
data_melt[measure == "pctprof" & value %like% "GE", value := (100 + as.numeric(gsub("GE", "", value)))/2]
data_melt[measure == "pctprof" & value %like% "GT", value := (100 + as.numeric(gsub("GT", "", value)) + 1)/2]
data_melt[measure == "pctprof" & value %like% "LE", value := (0 + as.numeric(gsub("LE", "", value)))/2]
data_melt[measure == "pctprof" & value %like% "LT", value := (0 + as.numeric(gsub("LT", "", value)) - 1)/2]
data_melt[, c("upper", "lower") := NULL]
data_melt[, value := as.numeric(value)]
#fwrite(data_melt, "/home/j/WORK/01_covariates/02_inputs/education/update_2020/geospatial_final_project/outputs/data_melt.csv")


# #subset to all races, white, black, and asian
# data_melt <- data_melt[prefix %in% c("ALL", "MWH", "MBL", "MAS")]
# data_melt[, UNSDLEA := as.numeric(str_sub(LEAID, 1, -6))]
# data_melt[, STATEFP := as.numeric(str_sub(LEAID, 1, -6))]
# data_melt[, UNSDLEA := as.numeric(str_sub(LEAID, -5, -1))]
# data_wide <- dcast(data_melt[year < 1500],  STNAM + STATEFP + UNSDLEA +LEAID + prefix + subject + grade + year ~ measure, value.var = "value")
# 
# 
# #take the mean across all years and all grades (grade == 0) and across reading and learning
# avg_prof_by_race <- data_wide[grade == 0 & prefix != "ALL",.(avg_prof = mean(as.numeric(pctprof), na.rm = T)), by = .(STNAM, LEAID,UNSDLEA, STATEFP, prefix)]
# avg_prof_by_race_wide <- dcast(avg_prof_by_race, STNAM + LEAID + UNSDLEA + STATEFP ~ prefix, value.var = "avg_prof")
# avg_prof_by_race_wide[, abs_dev_mbl := MWH - MBL]
# avg_prof_by_race_wide[, abs_dev_mas := MWH - MAS]
# prof_by_race_dev <- melt(avg_prof_by_race_wide, id.vars = c("STNAM", "LEAID", "UNSDLEA", "STATEFP"), measure.vars = c("abs_dev_mbl", "abs_dev_mas"))
# 
# #merge on shapefile
# shp <- read_sf("/home/j/WORK/01_covariates/02_inputs/education/update_2020/geospatial_final_project/reference/schooldistrict_sy1819_tl19.shp")
# shp$STATEFP <- as.numeric(shp$STATEFP)
# shp$UNSDLEA <- as.numeric(shp$UNSDLEA)
# 
# plot_data <- left_join(data.frame(prof_by_race_dev), shp[,c("UNSDLEA", "STATEFP", "NAME", "geometry")], by = c("UNSDLEA", "STATEFP"))
# 
# 
# #saveRDS(plot_data, '/home/j/WORK/01_covariates/02_inputs/education/update_2020/geospatial_final_project/reference/avg_prof_by_leaid_year_race.rds')
# 
# ggplot(plot_data) + 
#   geom_sf(aes(fill = value, geometry = geometry), lwd = 0) + 
#   coord_sf(datum = NA) + 
#   scale_fill_gradient2("Average Percent Proficient", low = "red",mid = "white", high = "green") + 
#   facet_wrap(~variable, ncol = 1) + 
#   theme_bw()
#   
# 
# 
# #slopes 
# avg_prof_by_leaid_year[, year_start := as.numeric(substr(year,1,2))] 
# avg_prof_by_leaid_year[, l1avg_prof := lag(avg_prof), by = .(LEAID)]
# avg_prof_by_leaid_year[, slope := l1avg_prof - avg_prof]
# avg_prof_by_leaid_year[, avg_slope := mean(slope, na.rm = T), by = .(LEAID)]
# 
# plot_data<- merge(plot_data, unique(avg_prof_by_leaid_year[,.(LEAID, avg_slope)]), by = "LEAID")
# 
# ggplot(plot_data[STNAM %in% c("MISSISSIPPI", "ALABAMA", "TENNESSEE")]) + 
#   geom_sf(aes(fill = avg_slope, geometry = geometry)) + 
#   coord_sf(datum = NA) + 
#   scale_fill_gradient2("Average ROC, %/year", low = "red",mid= "white", high = "green", midpoint = 0)
# 
