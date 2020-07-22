##geospatial smoothing learning model
library(stringr)
library(rlang)
library(data.table)
library(sf)
library(dplyr)
library(INLA)
library(spdep)
library(boot)
library(ggplot2)
library(gridExtra)
##

task_id <- as.integer(Sys.getenv("SGE_TASK_ID"))
codes <- fread("/home/j/WORK/01_covariates/02_inputs/education/update_2020/geospatial_final_project/ref/codes.csv")
c.subset <- codes[task_id, prefix]

data <- fread(paste0("/home/j/WORK/01_covariates/02_inputs/education/update_2020/geospatial_final_project/outputs/data_wide_", c.subset, ".csv"))

#
subset_cb <- data.table(prefix = c("ALL", "MAM", "MAS", "MBL", "MHI", "MTR", "MWH", "F", "M", "CWD", "ECD", "LEP", "HOM", "MIG"),
                        subset = c("All Students", "American Indian/Alaska Native", "Asian/Pacific Islander",
                                   "Black", "Hispanic", "Two or More Races", "White", "Female", "Male", "Children With Disabilities",
                                   "Economically Disadvantaged", "Limited English Proficient", "Homeless Enrolled Students", "Migrant Students"))

data <- merge(data, subset_cb)

#load school district shapefile
shp <- read_sf("/home/j/WORK/01_covariates/02_inputs/education/update_2020/geospatial_final_project/reference/schooldistrict_sy1819_tl19.shp")
shp$STATEFP <- as.numeric(shp$STATEFP)
shp$UNSDLEA <- as.numeric(shp$UNSDLEA)
shp$ELSDLEA <- as.numeric(shp$ELSDLEA)
shp <- st_transform(shp, crs = "+proj=longlat +datum=NAD83 +no_defs")
shp_dt <- data.table(shp)
shp$seq_id <- 1:nrow(shp)
#rename special school districts to be the same as normal school districts
shp$UNSDLEA[is.na(shp$UNSDLEA)] <- shp$ELSDLEA[is.na(shp$UNSDLEA)]
shp <- shp[shp$STATEFP <= 51 & !shp$STATEFP %in% c(72, 2, 15),]

#merge on shapefile
data_shp <- merge(data, shp[,c("STATEFP", "UNSDLEA", "seq_id")], by = c("STATEFP", "UNSDLEA"), all = T)
head(data)

#load state shapefile for some borders
states_shp <- read_sf("/home/j/WORK/01_covariates/02_inputs/education/update_2020/geospatial_final_project/reference/cb_2018_us_state_20m.shp")
states_shp <- st_transform(states_shp, st_crs(shp)$proj4string)

c.grade <- 0
c.year <- 2011
c.desc <- subset_cb[prefix == c.subset, subset]
#plot all vals

for(c.grade in unique(data$grade)){
  for(c.year in unique(data$year)){
    jpeg(paste0("/home/j/WORK/01_covariates/02_inputs/education/update_2020/geospatial_final_project/maps/", c.subset,"_",c.grade, "_", c.year, ".jpeg"), height = 700, width = 500)
    print(c.grade)
    print(c.year)
    
    gg1 <- ggplot(data_shp[grade == c.grade & subject == "math" & year == c.year]) + 
      geom_sf(aes(fill = pctprof, geometry = geometry), lwd = 0, color = NA) +
      scale_fill_continuous(breaks = seq(0,100, 20), limits = c(0,100), type = "viridis") +
      geom_sf(data = states_shp[states_shp$STATEFP == 1,], aes(geometry = geometry),fill = NA, color = "white") +
      ggtitle(paste0(c.desc,"\nMath", ", Grade: ", c.grade, ", Year:", c.year)) + xlab("Longitude") + ylab("Latitude")+ theme_bw() +
      labs(fill = "Percent of the Population Achieving Proficiency") + 
      theme(legend.position = "bottom") 
    gg2 <- ggplot(data_shp[grade == c.grade & subject == "reading" & year == c.year]) + 
      geom_sf(aes(fill = pctprof, geometry = geometry), lwd = 0, color = NA) +
      scale_fill_continuous(breaks = seq(0,100, 20), limits = c(0,100), type = "viridis") +
      geom_sf(data = states_shp[states_shp$STATEFP == 1,], aes(geometry = geometry),fill = NA, color = "white") +
      ggtitle(paste0(c.desc, "\nReading, ", ", Grade: ", c.grade, ", Year:", c.year)) + xlab("Longitude") + ylab("Latitude")+ theme_bw() +
      labs(fill = "Percent of the Population Achieving Proficiency") + 
      theme(legend.position = "bottom") 
    grid.arrange(gg1, gg2, nrow = 2)
    dev.off()
  }
}
