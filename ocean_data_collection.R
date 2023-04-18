# shipwrecks
library(readr)
library(ggplot2)
library(tidyverse)
shipwrecks <- read_csv("raw_data/shipwrecks.csv")
shipwrecks$sank <- 1

# lat/long filtering
shipwrecks <- shipwrecks[complete.cases(shipwrecks[, c("LATITUDE", "LONGITUDE")]), ]

# grabbing relevant variables
shipwrecks <- shipwrecks %>%
  select(
    `YEAR BUILT`, `WHERE BUILT`, `DATE LOST`, YEAR, MNTH, DAY,
    `LOCATION LOST`, LATITUDE, LONGITUDE, `CAUSE OF LOSS`,
    `GROSS TONNAGE`, `HOME (HAILING) PORT`, `DEPARTURE PORT`,
    `DESTINATION PORT`, `# CREW`, `LIVES LOST`, `NATURE OF CARGO`,
    `LOST`, sank
  ) %>%
  mutate(age = YEAR - `YEAR BUILT`) # AGE OF SHIP

# loading in ocean data
oceans <- read.csv("raw_data/ocean_data.csv")

measurement_values <- data.frame(oceans[1, ])

oceans[oceans == "NaN"] <- NA
oceans <- oceans[-1, ]
oceans[, -1] <- data.frame(lapply(oceans[, -1], as.numeric))

complete_oceans <- oceans[complete.cases(oceans), ]

latitude <- seq(-89, 89, 1)
longitude <- seq(-179, 179, 1)

totals <- expand.grid(latitude = latitude, longitude = longitude)

vars <- colnames(complete_oceans)
for (i in 4:length(vars)) {
  data_to_interp <- complete_oceans %>% select(latitude, longitude, vars[i])
  model <- lm(data_to_interp[, 3] ~ latitude + longitude, data = data_to_interp)
  pred <- predict(model, newdata = totals[, 1:2])
  totals <- cbind(totals, pred)
}

names(totals) <- vars[2:length(vars)]

new_names <- c(
  "Latitude",
  "Longitude",
  "AirTempSurface",
  "Cloudiness",
  "LatentHeat",
  "HumidityMinusTemp",
  "Humidity",
  "HeatParameter",
  "Humidity",
  "Pressure",
  "SeaAirTempDiff",
  "SeaSurfaceTemp",
  "SensibleHeatTransEastward",
  "ZonalLatentHeatParameter",
  "UWindStress",
  "LatentHeatTransEastward",
  "UWind",
  "SensibleHeatTransNorthward",
  "MeridonalLatentHeatParameter",
  "VWindStress",
  "LatentHeatTransNorthward",
  "VWind",
  "ScalarWind",
  "ScalarWindCubed"
)
names(totals) <- new_names

#calcite

calcite <- read_csv("raw_data/calcite.csv")
measurement_values_calcite <- data.frame(calcite[1, ])

calcite[calcite == "NaN"] <- NA
calcite <- calcite[-1, ]
calcite[, -1] <- data.frame(lapply(calcite[, -1], as.numeric))
calcite <- calcite[complete.cases(calcite),]

calc_long <- expand.grid(latitude = latitude, longitude = longitude)
calcite <- calcite %>% select(latitude, longitude, pic)
model <- lm(pic ~ latitude + longitude, data = calcite)
pred <- predict(model, newdata = calc_long)
totals <- cbind(totals, pred)
names(totals)[25] <- "calcite"

# currents
currents <- read_csv("raw_data/currents.csv")
measurement_values_currents <- data.frame(currents[1, ])

currents[currents == "NaN"] <- NA
currents <- currents[-1, ]
currents[, -1] <- data.frame(lapply(currents[, -1], as.numeric))
currents <- currents[complete.cases(currents),]

calc_long <- expand.grid(latitude = latitude, longitude = longitude)
curr <- currents %>% select(latitude, longitude, u_current)
model <- lm(u_current ~ latitude + longitude, data = curr)
pred <- predict(model, newdata = calc_long)
totals <- cbind(totals, pred)
names(totals)[26] <- "u_current"

calc_long <- expand.grid(latitude = latitude, longitude = longitude)
curr <- currents %>% select(latitude, longitude, v_current)
model <- lm(v_current ~ latitude + longitude, data = curr)
pred <- predict(model, newdata = calc_long)
totals <- cbind(totals, pred)
names(totals)[27] <- "v_current"

# ice
ice <- read_csv("raw_data/ice.csv")
measurement_values_ice <- data.frame(ice[1, ])

ice[ice == "NaN"] <- NA
ice <- ice[-1, ]
ice[, -1] <- data.frame(lapply(ice[, -1], as.numeric))
ice <- ice[complete.cases(ice),]

ice <- ice %>% group_by(latitude, longitude) %>% summarize(ice_concentration = mean(cn))

calc_long <- expand.grid(latitude = latitude, longitude = longitude)
ice <- ice %>% select(latitude, longitude, ice_concentration)
model <- lm(ice_concentration ~ latitude + longitude, data = ice)
pred <- predict(model, newdata = calc_long)
totals <- cbind(totals, pred)
names(totals)[28] <- "ice"

# pressure
pressure <- read.csv("raw_data/pressure.csv")
measurement_values_pressure <- data.frame(pressure[1, ])

pressure[pressure == "NaN"] <- NA
pressure <- pressure[-1, ]
pressure[, -1] <- data.frame(lapply(pressure[, -1], as.numeric))
pressure <- pressure[complete.cases(pressure),]

calc_long <- expand.grid(latitude = latitude, longitude = longitude)
pressure <- pressure %>% select(latitude, longitude, p_msl)
model <- lm(p_msl ~ latitude + longitude, data = pressure)
pred <- predict(model, newdata = calc_long)
totals <- cbind(totals, pred)
names(totals)[29] <- "pressure"

# salt
salt <- read.csv("raw_data/salt.csv")
measurement_values_salt <- data.frame(salt[1, ])

salt[salt == "NaN"] <- NA
salt <- salt[-1, ]
salt[, -1] <- data.frame(lapply(salt[, -1], as.numeric))
salt <- salt[complete.cases(salt),]
names(salt) <- c("Depth", "latitude", "longitude",
                 "Temperature", "Salt", "SurfaceVelocity",
                 "PotentialDensity", "UCurrent", "VCurrent")
salt <- aggregate(salt[, 4:9], by = list(salt$latitude, salt$longitude), mean)
names(salt) <- c("latitude", "longitude", "Temperature",
                              "Salt", "SurfaceVelocity", "PotentialDensity",
                              "UCurrent", "VCurrent")


calc_long <- expand.grid(latitude = latitude, longitude = longitude)
salt_var <- salt %>% select(latitude, longitude, Temperature)
model <- lm(Temperature ~ latitude + longitude, data = salt_var)
pred <- predict(model, newdata = calc_long)
totals <- cbind(totals, pred)
names(totals)[30] <- "Temperature"

calc_long <- expand.grid(latitude = latitude, longitude = longitude)
salt_var <- salt %>% select(latitude, longitude, SurfaceVelocity)
model <- lm(SurfaceVelocity ~ latitude + longitude, data = salt_var)
pred <- predict(model, newdata = calc_long)
totals <- cbind(totals, pred)
names(totals)[31] <- "SurfaceVelocity"

calc_long <- expand.grid(latitude = latitude, longitude = longitude)
salt_var <- salt %>% select(latitude, longitude, Temperature)
model <- lm(Temperature ~ latitude + longitude, data = salt_var)
pred <- predict(model, newdata = calc_long)
totals <- cbind(totals, pred)
names(totals)[32] <- "Temperature"

calc_long <- expand.grid(latitude = latitude, longitude = longitude)
salt_var <- salt %>% select(latitude, longitude, PotentialDensity)
model <- lm(PotentialDensity ~ latitude + longitude, data = salt_var)
pred <- predict(model, newdata = calc_long)
totals <- cbind(totals, pred)
names(totals)[33] <- "PotentialDensity"

calc_long <- expand.grid(latitude = latitude, longitude = longitude)
salt_var <- salt %>% select(latitude, longitude, UCurrent)
model <- lm(UCurrent ~ latitude + longitude, data = salt_var)
pred <- predict(model, newdata = calc_long)
totals <- cbind(totals, pred)
names(totals)[34] <- "UCurrent"

calc_long <- expand.grid(latitude = latitude, longitude = longitude)
salt_var <- salt %>% select(latitude, longitude, VCurrent)
model <- lm(VCurrent ~ latitude + longitude, data = salt_var)
pred <- predict(model, newdata = calc_long)
totals <- cbind(totals, pred)
names(totals)[35] <- "VCurrent"

# sea ice concentration
sea_ice <- read.csv("raw_data/sea_ice_concentration.csv")

measurement_values_seaice <- data.frame(sea_ice[1, ])

sea_ice[sea_ice == "NaN"] <- NA
sea_ice <- sea_ice[-1, ]
sea_ice[, -1] <- data.frame(lapply(sea_ice[, -1], as.numeric))
sea_ice <- sea_ice[complete.cases(sea_ice),]

calc_long <- expand.grid(latitude = latitude, longitude = longitude)
model <- lm(sic ~ latitude + longitude, data = sea_ice)
totals[,36] <- predict(model, newdata = calc_long)
names(totals)[36] <- "SeaIceConcentration"

#surface winds 10m
surface10 <- read.csv("raw_data/SurfaceWinds10m.csv")
measurement_values_surface10 <- data.frame(surface10[1, ])

surface10[surface10 == "NaN"] <- NA
surface10 <- surface10[-1, ]
surface10[, -1] <- data.frame(lapply(surface10[, -1], as.numeric))
surface10 <- surface10[complete.cases(surface10),]

surface10 <- aggregate(surface10[, 4:9], by = list(surface10$latitude, surface10$longitude), mean)
names(surface10)[1:2] <- c("latitude", "longitude")

calc_long <- expand.grid(latitude = latitude, longitude = longitude)
model <- lm(uv_mag_mean ~ latitude + longitude, data = surface10)
totals[,36] <- predict(model, newdata = calc_long)
names(totals)[36] <- "uv_magnitude10"

calc_long <- expand.grid(latitude = latitude, longitude = longitude)
model <- lm(taux_mean ~ latitude + longitude, data = surface10)
totals[,37] <- predict(model, newdata = calc_long)
names(totals)[37] <- "ZonalSurfaceWindStress10"

calc_long <- expand.grid(latitude = latitude, longitude = longitude)
model <- lm(tauy_mean ~ latitude + longitude, data = surface10)
totals[,38] <- predict(model, newdata = calc_long)
names(totals)[38] <- "MeridonalSurfaceWindStress10"

calc_long <- expand.grid(latitude = latitude, longitude = longitude)
model <- lm(curl ~ latitude + longitude, data = surface10)
totals[,39] <- predict(model, newdata = calc_long)
names(totals)[39] <- "curl10"

# surface20
surface20 <- read.csv("raw_data/surfacewinds20m.csv")
measurement_values_surface20 <- data.frame(surface20[1, ])

surface20[surface20 == "NaN"] <- NA
surface20 <- surface20[-1, ]
surface20[, -1] <- data.frame(lapply(surface20[, -1], as.numeric))
surface20 <- surface20[complete.cases(surface20),]

surface20 <- aggregate(surface20[, 4:9], by = list(surface20$latitude, surface20$longitude), mean)
names(surface20)[1:2] <- c("latitude", "longitude")

calc_long <- expand.grid(latitude = latitude, longitude = longitude)
model <- lm(uv_mag_mean ~ latitude + longitude, data = surface20)
totals[,40] <- predict(model, newdata = calc_long)
names(totals)[40] <- "uv_magnitude20"

calc_long <- expand.grid(latitude = latitude, longitude = longitude)
model <- lm(taux_mean ~ latitude + longitude, data = surface20)
totals[,41] <- predict(model, newdata = calc_long)
names(totals)[41] <- "ZonalSurfaceWindStress20"

calc_long <- expand.grid(latitude = latitude, longitude = longitude)
model <- lm(tauy_mean ~ latitude + longitude, data = surface20)
totals[,42] <- predict(model, newdata = calc_long)
names(totals)[42] <- "MeridonalSurfaceWindStress20"

calc_long <- expand.grid(latitude = latitude, longitude = longitude)
model <- lm(curl ~ latitude + longitude, data = surface20)
totals[,43] <- predict(model, newdata = calc_long)
names(totals)[43] <- "curl20"

write.csv(totals, "ocean_data.csv")
