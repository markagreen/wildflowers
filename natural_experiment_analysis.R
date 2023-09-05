#####################################
### Evaluating UGBS interventions ###
#####################################

# Aim:

# Libraries
library(data.table)
library(ggplot2)
library(viridis)
library(microsynth)
library(sf)
sf_use_s2(FALSE)

### Apologies - Mark needs to tidy code ###

# Load
lsoa <- fread("../DSH Outputs/Geolytix_Green_1278/2ndFeb23_MarkGreen_1278_cmm_lsoa.csv")
lsoa$lad_name <- substr(lsoa$name, 1, 2) # Get a lazy version of Local Authority name ID  
lads <- lsoa[, list(unique_devs = sum(unique_devs), n_hex = sum(n_hex)), by = c("lad_name", "month")] # Aggregate to LADs
lads$mean_LAD_rate <- lads$unique_devs / lads$n_hex # Calculate rate
lads$adjust <- 4 / lads$mean_LAD_rate 
lsoa <- merge(lsoa, lads[,c(1,2,6)], by = c("lad_name", "month"), all.x = TRUE)
lsoa$adjust_count <- lsoa$unique_devs * lsoa$adjust
sum(lsoa$adjust_count[lsoa$lad_name == "Li"])

# Load data
gpsp <- fread("../DSH Outputs/Geolytix_Green_1278/2ndFeb23_MarkGreen_1278_cmm_grsp2_monthly.csv") # Load monthly counts of greenspace visits
gpsp$unique_devs_rate <- gpsp$unique_devs / gpsp$n_hex # Calculate rate

lad_values <- fread("../DSH Outputs/Geolytix_Green_1278/2ndFeb23_MarkGreen_1278_cmm_grsp2_month_totals_LADs.csv")
lad_values$unique_devs[is.na(lad_values$unique_devs)] <- 0 # Fill in missing data
lad_values$mean_LAD_rate <- lad_values$unique_devs / lad_values$n_hex # Calculate rate

# Calculate monthly adjusted values
ggplot(lad_values, aes(x = month, y = mean_LAD_rate, group = name.x, color = name.x)) +
  geom_point() +
  ylab("Mean rate") +
  ylim(0,6.5) # plot mean values per month
lad_values$adjust <- 4 / lad_values$mean_LAD_rate 
gpsp <- merge(gpsp, lad_values[, c(2,3,7,8)], by = c("name.x", "month"), all.x = TRUE)
gpsp$adjust_count <- gpsp$unique_devs * gpsp$adjust
sum(gpsp$adjust_count[gpsp$name.x == "Liverpool"])

# Calculate z-score
stdev <- gpsp[, list(std_dev_udrate = sd(unique_devs_rate)), by = c("name.x", "month")] # Calculate standard deviation per green space values
gpsp <- merge(gpsp, stdev, by = c("name.x", "month"), all.x = TRUE)
gpsp$unique_devs_zscore <- (gpsp$unique_devs_rate - gpsp$mean_LAD_rate) / gpsp$std_dev_udrate # Calculate z-score
rm(stdev, lad_values) # Tidy

# Compute time variable as numeric
gpsp$time <- NA
gpsp$time[gpsp$month == "2021-08"] <- 1
gpsp$time[gpsp$month == "2021-09"] <- 2
gpsp$time[gpsp$month == "2021-10"] <- 3
gpsp$time[gpsp$month == "2021-11"] <- 4
gpsp$time[gpsp$month == "2021-12"] <- 5
gpsp$time[gpsp$month == "2022-01"] <- 6
gpsp$time[gpsp$month == "2022-02"] <- 7
gpsp$time[gpsp$month == "2022-03"] <- 8
gpsp$time[gpsp$month == "2022-04"] <- 9
gpsp$time[gpsp$month == "2022-05"] <- 10
gpsp$time[gpsp$month == "2022-06"] <- 11
gpsp$time[gpsp$month == "2022-07"] <- 12

# Add in covariates
ndvi <- read_sf("../OS Green Space layer/NDVI/cheshire_and_merseyside20212022ndvi.shp") # NDVI
ndvi$area <- st_area(ndvi) # Calculate size of green spaces

# Bus stops (proxy for accessibility)
bus <- read_sf("../Shapefiles/bus_stops.geojson") # Load in bus stop locations (downloaded from OSM)
bus <- bus[bus$bus == "yes" & !is.na(bus$bus),] # Subset only bus stops
bus <- st_transform(bus, crs = 27700) # Change CRS
ndvi_buff <- st_buffer(ndvi, dist = 100) # Generate 100m buffer around greenspaces
ndvi_buff <- st_join(ndvi_buff, bus, join = st_intersects) # Point-in-polygon
ndvi_buff <- ndvi_buff[, 1] # Drop variables
ndvi_buff <- as.data.table(ndvi_buff) # Change format for next step
ndvi_buff <- ndvi_buff[, list(bus_stops = .N), by = c("id.x")] # Aggregate to count number of bus stops per park
ndvi <- merge(ndvi, ndvi_buff, by.x = "id", by.y = "id.x", all.x = TRUE) # Join onto main file
ndvi <- st_transform(ndvi, crs = 4326) # To match CRS of H3
summary(ndvi)
rm(bus, ndvi_buff)

# IMD
# Neighbourhood deprivation
lsoa_lcr <- read_sf("../../Food adverts/Predicting adverts/Covariates/lcr_deprivation/data/Index_of_Multiple_Deprivation_IMD/Combined_Authorities/E47000004/shapefiles/E47000004.shp") # LSOAs for Liverpool City Region
lsoa_lcr <- st_transform(lsoa_lcr, crs = 4326) # To match CRS of H3
imd <- read.csv("../../Food adverts/Predicting adverts/Covariates/lcr_deprivation/data/Index_of_Multiple_Deprivation_IMD/Combined_Authorities/E47000004/tables/E47000004_2019.csv") # 2019 IMD values
imd$pop_density <- (imd$TotPop / imd$st_areasha) * 100 # Create population density variable per square km
imd <- imd[, c("lsoa11cd", "IMDScore", "IMD_Decile", "pop_density")] # Subset variables
lsoa_lcr <- merge(lsoa_lcr, imd, by = "lsoa11cd", all.x = TRUE) # Join onto main dataset
rm(imd)
ndvi <- st_intersection(ndvi, lsoa_lcr) # Spatial join 
ndvi <- ndvi[!duplicated(ndvi["id"]),] # Randomly select one
gpsp <- merge(gpsp, ndvi[, c("id", "function_", "X2021ndvime", "X2022ndvime", "IMDScore", "IMD_Decile", "pop_density", "bus_stops", "area")], by = c("id"), all.x = TRUE, allow.cartesian=TRUE)

# Load in list of wildflower sites
wildflowers <- read_sf("../Location data/scouseflowerhouse.shp")
wildflowers <- st_transform(wildflowers, crs = 27700)
wildflowers$area <- st_area(wildflowers) # Estimate area size
ndvi <- read_sf("../OS Green Space layer/NDVI/cheshire_and_merseyside20212022ndvi.shp")
test <- st_join(ndvi, wildflowers, left = FALSE) # Spatial join
test <- data.frame(test[, c("id", "year")]) # Drops all vars
test$geometry <- NULL 
test <- test[!duplicated(test["id"]),]
gpsp <- merge(gpsp, test, by = c("id"), all.x = TRUE)
gpsp$wildflowers[is.na(gpsp$wildflowers)] <- 0

# Create binary variables for green space type (not all have interventions in them)
gpsp$cemetry <- 0 # Create blank variable 
gpsp$cemetry[gpsp$function_ == "Cemetery"] <- 1 # Update if matches variable type
gpsp$allotment <- 0 # Repeat... 
gpsp$allotment[gpsp$function_ == "Allotments Or Community Growing Spaces"] <- 1
gpsp$bowling <- 0 # Cemetry 
gpsp$bowling[gpsp$function_ == "Bowling Green"] <- 1
gpsp$golf <- 0 # Cemetry 
gpsp$golf[gpsp$function_ == "Golf Course"] <- 1
gpsp$other_sports <- 0 # Cemetry 
gpsp$other_sports[gpsp$function_ == "Other Sports Facility"] <- 1
gpsp$play <- 0 
gpsp$play[gpsp$function_ == "Play Space"] <- 1
gpsp$play_field <- 0 
gpsp$play_field[gpsp$function_ == "Playing Field"] <- 1
gpsp$park <- 0 
gpsp$park[gpsp$function_ == "Public Park Or Garden"] <- 1
gpsp$religion <- 0 
gpsp$religion[gpsp$function_ == "Religious Grounds"] <- 1
gpsp$tennis <- 0 
gpsp$tennis[gpsp$function_ == "Tennis Court"] <- 1

# See if repeated GPSP IDs


# All wildflower sites
gpsp$treated <- 0 # Create blank variable to denote if treated or not
gpsp$treated[gpsp$year >= 2019 & (gpsp$month == "2022-05" | gpsp$month == "2022-06" | gpsp$month == "2022-07")] <- 1 # Denote is when wildflowers bloom and months after
temp <- gpsp[gpsp$name.x == "Liverpool" | (gpsp$name.x == "Knowsley" & gpsp$year >= 2019)]
# temp <- temp[temp$id != "E56DE6BE-F3EE-13A9-E053-AAEFA00A0D0E" & temp$id != "E56DE6C1-FC5A-13A9-E053-AAEFA00A0D0E" & temp$id != "E56DE6C5-497E-13A9-E053-AAEFA00A0D0E" & temp$id != "E56DE6DD-CC6E-13A9-E053-AAEFA00A0D0E" & temp$id != "E56DE71E-1DD7-13A9-E053-AAEFA00A0D0E" & temp$id != "E56DE71E-BA66-13A9-E053-AAEFA00A0D0E" & temp$id != "E56DE733-58EE-13A9-E053-AAEFA00A0D0E" & temp$id != "E56DE755-6B63-13A9-E053-AAEFA00A0D0E" & temp$id != "E56DE845-3933-13A9-E053-AAEFA00A0D0E" & temp$id != "E56DE84A-B54F-13A9-E053-AAEFA00A0D0E",] # drop as repeated twice
temp$id.num <- as.numeric(as.factor(temp$id))
id <- data.frame(table(temp$id))
temp <- merge(temp, id, by.x = "id", by.y = "Var1", all.x = TRUE)
# temp <- temp[temp$Freq == 12] # drop as repeated twice

# Calculate mean area size of green spaces
summary(temp$area[temp$name.x== "Liverpool"])
summary(temp$area[temp$treated == 1])

# Microsynth
# Define components of model
match.out <- c("adjust_count") # Outcomes
cov.var <- c("X2021ndvime", "IMDScore", "pop_density", "bus_stops", "area", "cemetry", "allotment", "bowling", "golf", "other_sports", "play", "play_field", "park", "religion", "tennis") # Covariates

# Estimate model
model1 <- microsynth(temp, # Dataset
                   idvar = "id", # Unique ID of input (here this is green space ID)
                   timevar = "time", # Time variable
                   intvar = "treated", # Which variable identifies areas with intervention
                   start.pre = 1, # Where to start the pre-intervention period
                   end.pre = 9, # Where the intervention begins
                   end.post = 12, # End of data
                   match.out = match.out, # Variable names for time-varying variables 
                   match.covar = cov.var, # Time invariant covariates
                   result.var = match.out, # Save results for these outcome variables
                   omnibus.var = match.out, # Outcome variables used to calculate omnibus statistic
                   period = 1, # Granularity of data for estimating - 1 means estimate for each month
                   confidence = 0.95, # For confidence intervals
                   test = "twosided", # When calculating p-values, use this hypothesis test
                   n.cores = 1, # CPU cores for parrallelization (set to 1 if perm is 250, else can use all 8 cores if not specify below)
                   perm = 250, # Number of permutations
                   jack = F, # Jacknife
                   ret.stats = TRUE # Return all statistics
)
model1 # print model output

plot_microsynth(model1, xlab.tc = "Time point", ylab.tc = "Number of unique visits", xlab.diff = "Time point", main.tc = "Visits to a green space", main.diff = "Visits to a green space", all = NULL) # Quick plot of results
plot_microsynth(model1, xlab.tc = "Time point", ylab.tc = "Number of unique visits", xlab.diff = "Time point", main.tc = "Visits to a green space", main.diff = "Visits to a green space", all = NULL, file = "./perm_figure.jpeg") # Quick plot of results and save
model1$svyglm.stats # Print GLM model

# Make nice plot for paper
df1 <- data.frame(t(model1$Plot.Stats$Treatment)) 
df1$Intervention <- "Wildflower sites"
df1$time <- 1:12
# df1$date <- c("2021-08-31", "2021-09-30", "2021-10-31", "2021-11-30", "2021-12-31", "2022-01-31", "2022-02-28", "2022-03-31", "2022-04-30", "2022-05-31", "2022-06-30", "2022-07-31")
df1$date <- c("2021-08-01", "2021-09-01", "2021-10-01", "2021-11-01", "2021-12-01", "2022-01-01", "2022-02-01", "2022-03-01", "2022-04-01", "2022-05-01", "2022-06-01", "2022-07-01") # Give first of month for plotting purposes - data are monthly totals and looks best this way (plus r throws a wobbly if code as month)
df1$date <- as.Date(df1$date)

df2 <- data.frame(t(model1$Plot.Stats$Control)) # t = transpose 
df2$Intervention <- "Synthetic control"
df2$time <- 1:12
# df2$date <- c("2021-08-31", "2021-09-30", "2021-10-31", "2021-11-30", "2021-12-31", "2022-01-31", "2022-02-28", "2022-03-31", "2022-04-30", "2022-05-31", "2022-06-30", "2022-07-31")
df2$date <- c("2021-08-01", "2021-09-01", "2021-10-01", "2021-11-01", "2021-12-01", "2022-01-01", "2022-02-01", "2022-03-01", "2022-04-01", "2022-05-01", "2022-06-01", "2022-07-01")
df2$date <- as.Date(df2$date)

df <- rbind(df1, df2)


fig1 <- ggplot(df, aes(x = date, y = adjust_count, group = Intervention, color = Intervention)) + # Create plot
  geom_vline(xintercept = as.numeric(as.Date("2022-04-01")), linetype = "dashed") + # Intervention line
  # geom_point() + # Or just geom_path() to give just a connected line
  geom_line(linewidth=1) +
  scale_color_viridis(option = "D", discrete = TRUE, begin = 0.2, end = 0.8) +
  ylim(0, max(df$adjust_count)) +
  ylab("Number of unqiue visits to green space") +
  xlab("Month") +
  scale_x_date(date_labels = "%b", date_breaks = "month", name = "Month")
fig1 # Print
ggsave(fig1, filename = "./figure1.jpeg") # Save
ggsave(fig1, filename = "./figure1_hr.jpeg", dpi = 300) # Save



# Just 2022 sites (models says 4 sites, but should be 10?)

# Just new ones
gpsp$treated <- 0 # Create blank variable to denote if treated or not
gpsp$treated[gpsp$year >= 2022 & (gpsp$month == "2022-05" | gpsp$month == "2022-06" | gpsp$month == "2022-07")] <- 1 # Denote is the golf course and months after was closed
temp2 <- gpsp[gpsp$name.x == "Liverpool" | (gpsp$name.x == "Knowsley" & gpsp$year >= 2019)] # Subset locations
temp2 <- temp2[(is.na(temp2$year) | temp2$year == 2022)] # Subset dates
#temp <- temp[, c("id", "month", "adjust_count", "treated")] # subset required data
df <- data.frame(table(temp2$id))
temp2 <- merge(temp2, df, by.x = "id", by.y = "Var1", all.x = T)
# temp <- temp[temp$Freq == 12] # drop ones that are repeated twice
temp2$Freq <- NULL

# Microsynth

match.out <- c("adjust_count") # Outcomes
cov.var <- c("X2021ndvime", "IMDScore", "pop_density", "bus_stops", "area", "cemetry", "allotment", "bowling", "golf", "other_sports", "play", "play_field", "park", "religion", "tennis") # Covariates

model2 <- microsynth(temp2, 
                     idvar = "id", 
                     timevar = "time", 
                     intvar = "treated", 
                     start.pre = 1, 
                     end.pre = 9, 
                     end.post = 12, 
                     match.out = match.out, 
                     match.covar = cov.var, 
                     result.var = match.out, 
                     omnibus.var = match.out,
                     confidence = 0.95,
                     test = "twosided",
                     n.cores = 8
)
model2 # Gives estimate of intervention effect at end of intervention

plot_microsynth(model2) # Plot results
model2$svyglm.stats # Print GLM model


df1 <- data.frame(t(model2$Plot.Stats$Treatment)) 
df1$Intervention <- "Wildflower sites"
df1$time <- 1:12
# df1$date <- c("2021-08-31", "2021-09-30", "2021-10-31", "2021-11-30", "2021-12-31", "2022-01-31", "2022-02-28", "2022-03-31", "2022-04-30", "2022-05-31", "2022-06-30", "2022-07-31")
df1$date <- c("2021-08-01", "2021-09-01", "2021-10-01", "2021-11-01", "2021-12-01", "2022-01-01", "2022-02-01", "2022-03-01", "2022-04-01", "2022-05-01", "2022-06-01", "2022-07-01") # Give first of month for plotting purposes - data are monthly totals and looks best this way (plus r throws a wobbly if code as month)
df1$date <- as.Date(df1$date)

df2 <- data.frame(t(model2$Plot.Stats$Control)) # t = transpose 
df2$Intervention <- "Synthetic control"
df2$time <- 1:12
# df2$date <- c("2021-08-31", "2021-09-30", "2021-10-31", "2021-11-30", "2021-12-31", "2022-01-31", "2022-02-28", "2022-03-31", "2022-04-30", "2022-05-31", "2022-06-30", "2022-07-31")
df2$date <- c("2021-08-01", "2021-09-01", "2021-10-01", "2021-11-01", "2021-12-01", "2022-01-01", "2022-02-01", "2022-03-01", "2022-04-01", "2022-05-01", "2022-06-01", "2022-07-01")
df2$date <- as.Date(df2$date)

df_2 <- rbind(df1, df2)


ggplot(df_2, aes(x = date, y = adjust_count, group = Intervention, color = Intervention)) +
  geom_vline(xintercept = as.numeric(as.Date("2022-04-01")), linetype = "dashed") + # Intervention line
  # geom_point() + # Or just geom_path() to give just a connected line
  geom_line(linewidth=1) +
  scale_color_viridis(option = "D", discrete = TRUE, begin = 0.2, end = 0.8) +
  ylim(0, max(df_2$adjust_count)) +
  ylab("Number of unqiue visits to green space") +
  xlab("Month") +
  scale_x_date(date_labels = "%b", date_breaks = "month", name = "Month")

