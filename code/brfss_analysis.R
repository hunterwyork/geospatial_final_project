############################################################
############################################################
##
## Aggregate school district learning data to counties
## then regress on CDC county health data
##  
##
## By: Hunter York, hyork@uw.edu
############################################################
############################################################
############################################################
library(Hmisc)
library(stringr)
library(data.table)
library(lme4)
library(sf)
library(ggplot2)
library(GWmodel)
library(lwgeom)
library(gridExtra)

### load learning data and regress on brfss
#load learning data
learning <- readRDS("C:/Users/hyork/Desktop/ed_thesis/south_learning.rds")
learning <- st_as_sf(learning)
learning <- st_transform(learning, crs = "+proj=longlat +datum=NAD83 +no_defs")
learning_dt <- data.table(learning)

##pull pops 
pops <- fread("C:/Users/hyork/Desktop/ed_thesis/nhgis0006_ds173_2010_sd_uni.csv")
pops <- pops[STATEA %in% unique(learning_dt$STATEFP)]
pops_long <- melt(pops, measure.vars = patterns("^ID"))
#standardize race
pops_long[variable %like% "IDM", race := "white"]
pops_long[variable %like% "IDF", race := "black"]
pops_long[variable %like% "IDH", race := "asian"]
#standardize ages
pops_long[as.numeric(str_sub(variable, 4,6)) %in% 1:105, sex := "males"]
pops_long[as.numeric(str_sub(variable, 4,6)) %in% 1:105, age := as.numeric(str_sub(variable, 4,6)) - 3]
pops_long[as.numeric(str_sub(variable, 4,6)) %in% 106:209, sex := "females"]
pops_long[as.numeric(str_sub(variable, 4,6)) %in% 106:209, age := as.numeric(str_sub(variable, 4,6)) - 107]
#subset to schooling years
pops_long <- pops_long[age %in% 5:18]
pops_long[, value := as.numeric(value)]
pops_sum <- pops_long[,.(population = sum(value, na.rm = T)), by = .(race, SDUNI, SDUNIA, STATEA)]
setnames(pops_sum, c("SDUNIA", "STATEA"), c("UNSDLEA", "STATEFP"))
#cast learning long to merge on pops
learning_long <- melt(learning_dt[,.(UNSDLEA,STATEFP, GEOID, NAME,MWH_final, MBL_final, MAS_final)], measure.vars = c("MWH_final", "MBL_final", "MAS_final"))
learning_long[variable %like% "MWH", race := "white"]
learning_long[variable %like% "MBL", race := "black"]
learning_long[variable %like% "MAS", race := "asian"]
#merge on pops
learning_long <- merge(learning_long, pops_sum, by = c("race", "UNSDLEA", "STATEFP"), all = T)
learning_long[, population_fill := quantile(population,.5, na.rm = T), by = .(race, STATEFP)]
learning_long[is.na(population), population := population_fill] %>% .[,population_fill := NULL]
#now geolocate school districts within counties
counties_shp <- read_sf("C:/Users/hyork/Desktop/ed_thesis/tl_2017_us_county.shp")
counties_shp <- st_transform(counties_shp, st_crs(learning)$proj4string)
counties_shp <- counties_shp[as.numeric(counties_shp$STATEFP) %in% learning_long$STATEFP,]
counties_shp <- st_make_valid(counties_shp)

shp <- learning[,c("UNSDLEA", "STATEFP", "STNAM", "geometry")]
shp <- st_make_valid(shp)
shp <- shp[!is.na(shp$UNSDLEA),]
shp2 <- st_overlaps(counties_shp, shp)

shp_dt <- data.table(shp)
counties_dt <- data.table(counties_shp)
counties_map <- data.table()

OvPerc <- function(base_pol, sel_pol) {
  base_area <- st_area(base_pol)
  intersections <- st_area(st_intersection(base_pol, sel_pol))
  return(intersections/base_area)
}
get_percent <- function(c.UNSDLEA, c.STATEFP, c.geoid){
  base_shp <- shp[shp$UNSDLEA == c.UNSDLEA & shp$STATEFP == c.STATEFP & !is.na(shp$UNSDLEA), ]
  county_shp <- counties_shp[counties_shp$GEOID == c.geoid,]
  as.numeric(OvPerc(base_shp, county_shp))[!is.infinite(as.numeric(OvPerc(base_shp, county_shp))) & !is.nan(as.numeric(OvPerc(base_shp, county_shp)))]
}


for(i in 1:length(counties_dt$GEOID)){
  print(i)
  c.geoid <- counties_dt[i, GEOID]
  shp_index_list <- shp2[[i]]
  unsdleas_list <- shp_dt[shp_index_list, UNSDLEA]
  st_list <- shp_dt[shp_index_list, STATEFP]
  if(length(unsdleas_list) == 0){unsdleas_list <- c(NA)}
  if(length(st_list) == 0){st_list <- c(NA)}
  dt <- data.table(UNSDLEA = as.numeric(unsdleas_list), geoid = c.geoid, STATEFP = st_list)
  for(ii in 1:nrow(dt)){
    print(ii)
    dt[ii, percent_overlap := get_percent(UNSDLEA, STATEFP, geoid)]
  }
  counties_map <- rbind(counties_map, dt, fill = T)
}

#save file
#this file has the mappings of school districts to counties and the percent of the school district which lies within each county
#write.csv(counties_map, "C:/Users/hyork/Desktop/ed_thesis/counties_map.csv")
counties_map <- fread("C:/Users/hyork/Desktop/ed_thesis/counties_map.csv")
counties_map <- counties_map[percent_overlap > .5]

#now merge onto map
learning_long <- merge(learning_long, counties_map, by = c("UNSDLEA", "STATEFP"), all.x = T)

#now weighted average the learning data by cbsa/race
county_sum <- learning_long[,.(learning_prof = weighted.mean(value, population, na.rm = T),
                               population = sum(population, na.rm = T)), by = .(race, geoid)]
county_sum <- county_sum[!is.na(geoid)]
setnames(county_sum, "geoid", "GEOID")
#merge on shapefile and plot
counties_shp$GEOID <- as.numeric(counties_shp$GEOID)
sum_shp <- merge(county_sum, counties_shp, by = c( "GEOID"))

#load state shapefile for some borders
states_shp <- read_sf("C:/Users/hyork/Desktop/ed_thesis/cb_2018_us_state_20m.shp")
states_shp <- st_transform(states_shp, st_crs(learning)$proj4string)
states_shp <- states_shp[as.numeric(states_shp$STATEFP) %in% learning_long$STATEFP,]


gg1 <- ggplot(sum_shp[race == "white"]) + 
  geom_sf(aes(geometry = geometry, fill = learning_prof)) + theme_bw() + xlab("Longitude") + 
  ylab("Latitude") + 
  labs(fill = "Percent of the Population Achieving Proficiency") + 
  theme(legend.position = "bottom") + 
  scale_fill_continuous(limits = c(0,100), breaks = seq(0,100,20), type = "viridis") + ggtitle("White Americans") +
  geom_sf(data = states_shp, aes(geometry = geometry),fill = NA, color = "white")
gg2 <- ggplot(sum_shp[race == "black"]) + 
  geom_sf(aes(geometry = geometry, fill = learning_prof))+ theme_bw() + xlab("Longitude") + 
  ylab("Latitude") + 
  labs(fill = "Percent of the Population Achieving Proficiency") + 
  theme(legend.position = "bottom") + 
  scale_fill_continuous(limits = c(0,100), breaks = seq(0,100,20), type = "viridis") + ggtitle("Black Americans")+
  geom_sf(data = states_shp, aes(geometry = geometry),fill = NA, color = "white")
gg3 <- ggplot(sum_shp[race == "asian"]) + 
  geom_sf(aes(geometry = geometry, fill = learning_prof)) + theme_bw() + xlab("Longitude") + 
  ylab("Latitude") + 
  labs(fill = "Percent of the Population Achieving Proficiency") + 
  theme(legend.position = "bottom") + 
  scale_fill_continuous(limits = c(0,100), breaks = seq(0,100,20), type = "viridis") + ggtitle("Asian Americans")+
  geom_sf(data = states_shp, aes(geometry = geometry),fill = NA, color = "white")
pdf("C:/Users/hyork/Desktop/ed_thesis/county_agg_learning.pdf", width = 14, height = 8)
grid.arrange(gg1, gg2,nrow = 1)
dev.off()

##########################################
## analyze cdc healthoutcome data
## here referred to as brfss since that is 
## the primary source of many of its health
## outcomes measures
##########################################
#load brfss data
brfss <- fread("C:/Users/hyork/Desktop/ed_thesis/Analytic_data2019.csv")
codebook <- data.table(cbind(names(brfss), t(brfss[1,])))
names(brfss) <- unlist(brfss[1,])
brfss <- brfss[-1,]

# only keep columns needed
brfss <- brfss[,.(statecode, fipscode, v001_race_black, v001_race_white, v037_race_white,
                  v037_race_black, v014_race_white, v014_race_black,
                  v021_rawvalue, v024_race_black, v024_race_white,
                  v147_race_black, v147_race_white,
                  v141_rawvalue)]

setnames(brfss, c("STATEFP", "GEOID", "premature_death_bl", "premature_death_wh",
                  "low_birthwt_bl", "low_birthwt_wh",
                  "teen_births_bl", "teen_births_wh", 
                  "hs_grad_rate",
                  "children_in_pov_bl", "children_in_pov_wh",
                  "le_bl", "le_wh",
                  "res_seg"))

brfss <- brfss[as.numeric(STATEFP) %in% as.numeric(learning_long$STATEFP)]

#cast long by race
brfss_long <- melt(brfss, id.vars = c("STATEFP", "GEOID", "hs_grad_rate", "res_seg"))
brfss_long[, var := gsub("_wh|_bl", "", variable)]
brfss_long[, race := str_sub(variable, -2, -1)]
brfss_long[, variable := NULL]
brfss_long[,c("hs_grad_rate", "res_seg", "value") := lapply(.SD, as.numeric), .SDcols = c("hs_grad_rate", "res_seg", "value")]

#cast wide by measure
brfss_wide <- dcast(brfss_long, ... ~ var, value.var = "value")
brfss_wide[race == "wh", race := "white"]
brfss_wide[race == "bl", race := "black"]

#merge on learning_data
brfss_wide[, GEOID := as.integer(GEOID)]
brfss_wide <- merge(brfss_wide, county_sum, by = c("GEOID", "race"), all.x = T)
brfss_wide[is.na(learning_prof)] ## missing data are just state totals, so no worries there
brfss_wide <- brfss_wide[!is.na(learning_prof)]

#make some diagnostic scatters
ggplot(brfss_wide) + 
  geom_point(aes(x = learning_prof, y = premature_death, color = race))
ggplot(brfss_wide) + 
  geom_point(aes(x = learning_prof, y = children_in_pov, color = race))
ggplot(brfss_wide) + 
  geom_point(aes(x = learning_prof, y = res_seg, color = race))
ggplot(brfss_wide) + 
  geom_point(aes(x = learning_prof, y = le, color = race))
ggplot(brfss_wide) + 
  geom_point(aes(x = learning_prof, y = teen_births, color = race))
ggplot(brfss_wide) + 
  geom_point(aes(x = learning_prof, y = hs_grad_rate, color = race))

#plot map
brfss_wide_shape <- merge(brfss_wide, sum_shp, by = c("GEOID", "race", "learning_prof", "population", "STATEFP"), all.x = T)
ggplot(brfss_wide_shape) +
  geom_sf(aes(geometry = geometry, fill = premature_death)) + facet_wrap(~race)


#merge on shapefile
brfss_wide_spdf <- as(st_as_sf(brfss_wide_shape[complete.cases(brfss_wide_shape[,1:12])]), "Spatial")

#now run a linear model, using just fixed effects for simplicity
model_lm <- lm(premature_death ~ race + hs_grad_rate + res_seg + children_in_pov + learning_prof, data = brfss_wide_spdf)
summary(model_lm)
stargazer::stargazer(model_lm, out = "C:/Users/hyork/Desktop/ed_thesis/lm_model_output.html", type = "html")

#now run GWR
bw.ans <- bw.gwr(premature_death ~ race  + hs_grad_rate + res_seg + children_in_pov + learning_prof,
                 data = brfss_wide_spdf, approach = "CV", kernel = "bisquare")

gwr.res <- gwr.basic(premature_death ~ race  + hs_grad_rate + res_seg + children_in_pov + learning_prof,
                     data = brfss_wide_spdf, bw = bw.ans, kernel = "bisquare") #, adaptive = TRUE, F123.test = TRUE)
gwr.res

#plot learning prof histogram over all counties
hist(gwr.res$SDF$learning_prof, main = "", col = "purple",
     xlab = "Learning Proficiency", ylab = "")
abline(v = model_lm$coef[8], col = "green")

gg1 <- ggplot(st_as_sf(gwr.res$SDF)) + 
  geom_sf(aes(geometry = geometry, fill = learning_prof))+ theme_bw() + xlab("Longitude") + 
  ylab("Latitude") + 
  labs(fill = "Beta Value") + 
  theme(legend.position = "right") + 
  scale_fill_continuous( type = "viridis") + 
  ggtitle("Estimated Beta Value for Learning Proficiency's\nAssociation With Premature Mortality") +
  geom_sf(data = states_shp, aes(geometry = geometry),fill = NA, color = "Black")

gg2 <- ggplot(st_as_sf(gwr.res$SDF)) + 
  geom_sf(aes(geometry = geometry, fill = racewhite))+ theme_bw() + xlab("Longitude") + 
  ylab("Latitude") + 
  labs(fill = "Beta Value") + 
  theme(legend.position = "right") + 
  scale_fill_continuous( type = "viridis") + 
  ggtitle("Estimated Beta Value for Race's Association With\nPremature Mortality Where White = 1, Black = 0") +
  geom_sf(data = states_shp, aes(geometry = geometry),fill = NA, color = "Black")

gg3 <- ggplot(st_as_sf(gwr.res$SDF)) + 
  geom_sf(aes(geometry = geometry, fill = children_in_pov))+ theme_bw() + xlab("Longitude") + 
  ylab("Latitude") + 
  labs(fill = "Beta Value") + 
  theme(legend.position = "right") + 
  scale_fill_continuous( type = "viridis") + 
  ggtitle("Estimated Beta Value for Percentage of the Population's\nChildren in Poverty's Association With Premature Mortality") +
  geom_sf(data = states_shp, aes(geometry = geometry),fill = NA, color = "Black")

pdf("C:/Users/hyork/Desktop/ed_thesis/gwr_betas.pdf", width = 21, height = 8)
grid.arrange(gg1, gg2, gg3, nrow = 1)
dev.off()
