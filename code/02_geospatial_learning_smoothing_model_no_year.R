############################################################
############################################################
##
## run bym2 model on school district learning data to fill
## in missing data
##  
##
## By: Hunter York, hyork@uw.edu
############################################################
############################################################
############################################################
############################################################
##geospatial smoothing learning model
library(stringr)
library(rlang)
library(data.table)
library(sf)
library(dplyr)
library(INLA)
library(spdep)
library(boot)
##

#pull in data and subset to the deep south
data_melt <- fread("/home/j/WORK/01_covariates/02_inputs/education/update_2020/geospatial_final_project/outputs/data_melt.csv")
data_melt[, UNSDLEA := as.numeric(str_sub(LEAID, 1, -6))]
data_melt[, STATEFP := as.numeric(str_sub(LEAID, 1, -6))]
data_melt[, UNSDLEA := as.numeric(str_sub(LEAID, -5, -1))]
#
subset_cb <- data.table(prefix = c("ALL", "MAM", "MAS", "MBL", "MHI", "MTR", "MWH", "F", "M", "CWD", "ECD", "LEP", "HOM", "MIG"),
           subset = c("All Students", "American Indian/Alaska Native", "Asian/Pacific Islander",
                      "Black", "Hispanic", "Two or More Races", "White", "Female", "Male", "Children With Disabilities",
                      "Economically Disadvantaged", "Limited English Proficient", "Homeless Enrolled Students", "Migrant Students"))

data_melt[, year := as.numeric(substr(year,1,2)) + 2000]

#delete duplicated entries for 2016
data_melt <- data_melt[!duplicated(data_melt[,.(STNAM, LEAID, subject, prefix, measure,grade, year, UNSDLEA, STATEFP)])]
data_wide <- dcast(data_melt,  STNAM + STATEFP + UNSDLEA +LEAID + prefix + subject + grade + year  ~ measure, value.var = "value")
#fwrite(data_wide, "/home/j/WORK/01_covariates/02_inputs/education/update_2020/geospatial_final_project/outputs/data_wide.csv")

for(c.subset in unique(data_wide$prefix)){
  print(c.subset)
  fwrite(data_wide[prefix == c.subset], paste0("/home/j/WORK/01_covariates/02_inputs/education/update_2020/geospatial_final_project/outputs/data_wide_", c.subset, ".csv"))
}

#load school district shapefile
shp <- read_sf("C:/Users/hyork/Desktop/ed_thesis/schooldistrict_sy1819_tl19.shp")
shp <- shp[shp$STATEFP %in% unique(data_melt$STATEFP),]
shp$STATEFP <- as.numeric(shp$STATEFP)
shp$UNSDLEA <- as.numeric(shp$UNSDLEA)
shp$ELSDLEA <- as.numeric(shp$ELSDLEA)
shp <- st_transform(shp, crs = "+proj=longlat +datum=NAD83 +no_defs")
shp_dt <- data.table(shp)
shp$seq_id <- 1:nrow(shp)
#rename special school districts to be the same as normal school districts
shp$UNSDLEA[is.na(shp$UNSDLEA)] <- shp$ELSDLEA[is.na(shp$UNSDLEA)]

data_melt[, year := as.numeric(substr(year,1,2))]
data_wide <- dcast(data_melt[year < 16],  STNAM + STATEFP + UNSDLEA +LEAID + prefix + subject + grade + year  ~ measure, value.var = "value")

#now average over all years/grades/subjects for simplicity
avg_prof_by_race <- data_wide[grade == 0 & prefix != "ALL",.(avg_prof = mean(as.numeric(pctprof), na.rm = T),
                                                             numvalid = sum(as.numeric(numvalid), na.rm = T)), by = .(STNAM, LEAID,UNSDLEA, STATEFP, prefix)]
avg_prof_wide <- dcast(avg_prof_by_race, STNAM + LEAID + UNSDLEA + STATEFP ~ prefix, value.var = c("avg_prof", "numvalid"))


#merge on shapefile
avg_prof_wide <- merge(avg_prof_wide, shp[,c("STATEFP", "UNSDLEA", "seq_id")], by = c("STATEFP", "UNSDLEA"), all.y = T)


#standardize in logit space
avg_prof_wide[, MBL := avg_prof_MBL]
avg_prof_wide[, MAS := avg_prof_MAS]
avg_prof_wide[, MWH := avg_prof_MWH]

avg_prof_wide[, logit_MWH := logit(MWH/100)]
avg_prof_wide[, logit_MBL := logit(MBL/100)]
avg_prof_wide[, logit_MAS := logit(MAS/100)]

#make sure there's no erroneous units like guam that snuck in
avg_prof_wide <- avg_prof_wide[STATEFP %in% 1:51]
avg_prof_wide[, STATEFP_STD := as.numeric(as.factor(STATEFP))]

#make border file
nb.map <- poly2nb(shp)
nb2INLA("south.graph", nb.map)

re_lev_n <- shp$seq_id %>% range() %>% diff()+ 1

fit_one <- inla(logit_MWH ~ 1 + f(STATEFP_STD, model = "iid") + f(seq_id,
                                  model = "bym2", graph = "./south.graph",
                                  scale.model = T, 
                                  constr = T),
                data = avg_prof_wide[!is.na(logit_MWH)],
                family = "gaussian")

##pull res
fit_one_res <- data.table(fit_one$summary.random$seq_id)
fit_one_res[ID %in% (re_lev_n + 1):(2*re_lev_n), spatial := 1]
fit_one_res[is.na(spatial), spatial := 0]
fit_one_res[spatial == 1, seq_id := ID - re_lev_n]
fit_one_res[spatial == 0, seq_id := ID ]
fit_one_res <- dcast(fit_one_res[,.(mean, seq_id, spatial)], ... ~ spatial, value.var = "mean")
setnames(fit_one_res, c("1", "0"), c("spatial", "non_spatial"))
fit_one_res_state <- data.table(fit_one$summary.random$STATEFP_STD)[,.(ID, mean)]
setnames(fit_one_res_state, c("ID", "mean"), c("STATEFP_STD","state_mean"))

#make a prediction dataset to have a complete set of data for missing school districts
pred_df <- data.table(expand.grid(seq_id = 1:re_lev_n))
pred_df <- merge(pred_df, avg_prof_wide[,.(STATEFP,STATEFP_STD, UNSDLEA, STNAM, seq_id, MWH, MBL, MAS)], by = c("seq_id"), all.x = T)
pred_df <- merge(pred_df, fit_one_res[,.(seq_id, spatial, non_spatial)], by = c("seq_id"), all.x = T)
pred_df <- merge(pred_df, fit_one_res_state, by = "STATEFP_STD")
pred_df[, pred_MWH_spat := inv.logit(fit_one$summary.fixed$mean + spatial + state_mean)]
pred_df[, pred_MWH_nonspat := inv.logit(fit_one$summary.fixed$mean +non_spatial + state_mean) ]
ggplot(pred_df) + geom_point(aes(x = MWH, y = pred_MWH_spat))
ggplot(pred_df) + geom_point(aes(x = MWH, y = pred_MWH_nonspat))

#fill in imputed values where orig values missing
pred_df[,logit_MWH := logit(as.numeric(MWH)/100)]
pred_df[is.na(logit_MWH), logit_MWH := logit(pred_MWH_spat)]
pred_df[, logit_MBL := logit(as.numeric(MBL)/100)]


#now fit black data
fit_two <- inla(logit_MBL ~ 1 + I(logit_MWH) + f(STATEFP_STD, model = "iid") + f(seq_id,
                                                 model = "bym2",
                                                 graph = "./south.graph", scale.model = T), data = pred_df,
                family = "gaussian")

#fill in for places missing white and black
fit_two_res <- data.table(fit_two$summary.random$seq_id)
fit_two_res[ID %in% (re_lev_n + 1):(2*re_lev_n), spatial := 1]
fit_two_res[is.na(spatial), spatial := 0]
fit_two_res[spatial == 1, seq_id := ID - re_lev_n]
fit_two_res[spatial == 0, seq_id := ID ]
fit_two_res <- dcast(fit_two_res[,.(mean, seq_id, spatial)], ... ~ spatial, value.var = "mean")
setnames(fit_two_res, c("1", "0"), c("spatial", "non_spatial"))

fit_one_res_state <- data.table(fit_one$summary.random$STATEFP_STD)[,.(ID, mean)]
setnames(fit_one_res_state, c("ID", "mean"), c("STATEFP_STD","state_mean"))

pred_df[, spatial := NULL] %>% .[, non_spatial := NULL] %>% .[, state_mean := NULL]
pred_df <- merge(pred_df, fit_two_res[,.(seq_id, spatial, non_spatial)], by = c("seq_id"), all.x = T)
pred_df <- merge(pred_df, fit_one_res_state, by = "STATEFP_STD")
pred_df[, pred_MBL_spat := inv.logit(fit_two$summary.fixed$mean[1] + spatial + (logit_MWH*fit_two$summary.fixed$mean[2])+ state_mean)]
pred_df[, pred_MBL_nonspat := inv.logit(fit_two$summary.fixed$mean[1] +non_spatial + (logit_MWH*fit_two$summary.fixed$mean[2])+ state_mean) ]
ggplot(pred_df) + geom_point(aes(x = MBL, y = pred_MBL_spat))
ggplot(pred_df) + geom_point(aes(x = MBL, y = pred_MBL_nonspat))

pred_df[, logit_MAS := logit(as.numeric(MAS)/100)]


# fit asian data for future analyses
fit_three <- inla(logit_MAS ~ 1 + I(logit_MWH) + f(STATEFP_STD, model = "iid") + f(seq_id,
                                                                                 model = "bym2",
                                                                                 graph = "./south.graph", scale.model = T), data = pred_df,
                family = "gaussian")

#fill in for places missing white and black
fit_three_res <- data.table(fit_three$summary.random$seq_id)
fit_three_res[ID %in% (re_lev_n + 1):(2*re_lev_n), spatial := 1]
fit_three_res[is.na(spatial), spatial := 0]
fit_three_res[spatial == 1, seq_id := ID - re_lev_n]
fit_three_res[spatial == 0, seq_id := ID ]
fit_three_res <- dcast(fit_three_res[,.(mean, seq_id, spatial)], ... ~ spatial, value.var = "mean")
setnames(fit_three_res, c("1", "0"), c("spatial", "non_spatial"))

fit_one_res_state <- data.table(fit_one$summary.random$STATEFP_STD)[,.(ID, mean)]
setnames(fit_one_res_state, c("ID", "mean"), c("STATEFP_STD","state_mean"))

pred_df[, spatial := NULL] %>% .[, non_spatial := NULL] %>% .[, state_mean := NULL]
pred_df <- merge(pred_df, fit_three_res[,.(seq_id, spatial, non_spatial)], by = c("seq_id"), all.x = T)
pred_df <- merge(pred_df, fit_one_res_state, by = "STATEFP_STD")
pred_df[, pred_MAS_spat := inv.logit(fit_three$summary.fixed$mean[1] + spatial + (logit_MWH*fit_three$summary.fixed$mean[2])+ state_mean)]
pred_df[, pred_MAS_nonspat := inv.logit(fit_three$summary.fixed$mean[1] +non_spatial + (logit_MWH*fit_three$summary.fixed$mean[2])+ state_mean) ]
ggplot(pred_df) + geom_point(aes(x = MAS, y = pred_MAS_spat))
ggplot(pred_df) + geom_point(aes(x = MAS, y = pred_MAS_nonspat))



#fill in missing data with predictions (a very small subset of locations)
pred_df[, MWH_final := MWH]
pred_df[is.na(MWH), MWH_final := pred_MWH_spat * 100]
pred_df[, MBL_final := MBL]
pred_df[is.na(MBL), MBL_final := pred_MBL_spat * 100]
pred_df[, MAS_final := MAS]
pred_df[is.na(MAS), MAS_final := pred_MAS_spat * 100]

pred_df[is.na(MAS), MAS_pred := 1]
pred_df[is.na(MBL), MBL_pred := 1]
pred_df[is.na(MWH), MWH_pred := 1]
pred_df[is.na(MAS_pred), MAS_Pred := 0]
pred_df[is.na(MWH_pred), MWH_Pred := 0]
pred_df[is.na(MBL_pred), MBL_Pred := 0]

pred_df_shp <- merge(pred_df, shp, by = c("seq_id", "UNSDLEA", "STATEFP"), all = T)

saveRDS(pred_df_shp, "C:/Users/hyork/Desktop/ed_thesis/south_learning.rds")

#load state shapefile for some borders
states_shp <- read_sf("C:/Users/hyork/Desktop/ed_thesis/cb_2018_us_state_20m.shp")
states_shp <- st_transform(states_shp, st_crs(learning)$proj4string)
states_shp <- states_shp[as.numeric(states_shp$STATEFP) %in% learning_long$STATEFP,]



#plot all vals
gg1 <- ggplot(pred_df_shp) + 
  geom_sf(aes(fill = MWH_final, geometry = geometry), lwd = 0, color = NA) +
  scale_fill_continuous(breaks = seq(0,100, 20), limits = c(0,100), type = "viridis") +
  geom_sf(data= pred_df_shp[MWH_pred == 1],aes(geometry = geometry),fill = NA, color = "red")+
  geom_sf(data = states_shp, aes(geometry = geometry),fill = NA, color = "white") +
  ggtitle("White Americans") + xlab("Longitude") + ylab("Latitude")+ theme_bw() +
  labs(fill = "Percent of the Population Achieving Proficiency") + 
  theme(legend.position = "bottom") 

gg2 <- ggplot(pred_df_shp) + 
  geom_sf(aes(fill = MBL_final, geometry = geometry), lwd = 0, color = NA) + 
  scale_fill_continuous(breaks = seq(0,100, 20), limits = c(0,100), type = "viridis") +
  geom_sf(data= pred_df_shp[MBL_pred == 1],aes(geometry = geometry),fill = NA, color = "red")+
  geom_sf(data = states_shp, aes(geometry = geometry),fill = NA, color = "white") +
  ggtitle("Black Americans") + xlab("Longitude") + ylab("Latitude") + theme_bw() +
  labs(fill = "Percent of the Population Achieving Proficiency") + 
  theme(legend.position = "bottom") 
  


gg3 <- ggplot(pred_df_shp) + 
  geom_sf(aes(fill = MAS_final, geometry = geometry), lwd = 0, color = NA) + 
  scale_fill_continuous(breaks = seq(0,100, 20), limits = c(0,100), type = "viridis") +
  geom_sf(data= pred_df_shp[MAS_pred == 1],aes(geometry = geometry),fill = NA, color = "red")+
  geom_sf(data = states_shp, aes(geometry = geometry),fill = NA, color = "white") +
  ggtitle("Asian Americans") + xlab("Longitude") + ylab("Latitude")+ theme_bw() +
  labs(fill = "Percent of the Population Achieving Proficiency") + 
  theme(legend.position = "bottom") 

pdf("C:/Users/hyork/Desktop/ed_thesis/district_learning_smoothed.pdf", width = 14, height = 8)
grid.arrange(gg1, gg2,nrow = 1)
dev.off()
