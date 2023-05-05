
# Reading in data
library(readr)
library(tidyverse)
oceans <- read.csv("ocean_data.csv")

# adjusting ship data
shipwrecks <- read_csv("shipwrecks.csv")
shipwrecks$sank <- 1

# lat/long filtering
shipwrecks <- shipwrecks[complete.cases(shipwrecks[, c("LATITUDE", "LONGITUDE")]), ]

# grabbing relevant variables
ships <- shipwrecks %>%
  select(
    `YEAR BUILT`, `WHERE BUILT`, `DATE LOST`, YEAR, MNTH, DAY,
    `LOCATION LOST`, LATITUDE, LONGITUDE, `CAUSE OF LOSS`,
    `GROSS TONNAGE`, `HOME (HAILING) PORT`, `DEPARTURE PORT`,
    `DESTINATION PORT`, `# CREW`, `LIVES LOST`, `NATURE OF CARGO`,
    `LOST`, sank
  ) %>%
  mutate(age = YEAR - `YEAR BUILT`) # AGE OF SHIP

wrecks <- ships %>%
  select(sank, LONGITUDE, LATITUDE, YEAR)

oceans <- oceans[,-1]

names(wrecks) <- c("sank", "Longitude", "Latitude", "Year")

wrecks$Longitude <- wrecks$Longitude %>% as.numeric() 
wrecks$Longitude <- wrecks$Longitude <- round(wrecks$Longitude / 0.05) * 0.05
wrecks$Latitude <- wrecks$Latitude %>% as.numeric()
wrecks$Latitude <- wrecks$Latitude <- round(wrecks$Latitude / 0.05) * 0.05

wrecks <- wrecks[complete.cases(wrecks),]

## EDA
### Quick little year plot
years <- seq(1753, 2020, 1)
n_wrecks <- rep(NA, length(years))

for (i in 1:length(years)){
  n_wrecks[i] <- wrecks %>% filter(Year == years[i]) %>% nrow()
}

wrecks_by_year <- data.frame(years, n_wrecks)
wrecks_by_year %>% arrange(-n_wrecks) %>% head(5) 
# 1942: 33 (WWII)
# 1886: 20 (?)
# 1918: 19 (WWI)
# 1884: 14 (Sino-French War)
# 1889: 12

library(hrbrthemes)
ggplot(wrecks_by_year, aes(x = years, y = n_wrecks)) +
  geom_step() +
  theme_ipsum() +
  labs(x = "Year",
       y = "Number of Shipwrecks",
       title = "Shipwrecks Over Time",
       subtitle = "Rises can be seen during periods of global conflict.")

## Areas where ships have sunk
sink <- merge(wrecks[1:3], oceans, by = c("Longitude", "Latitude"), all.y = T)
rows_na <- naniar::where_na(sink) %>% as.data.frame() %>% select(row) %>% unique()
sink[rows_na$row, 3] <- 0
sink$sank <- factor(sink$sank)

sink <- sink %>%
  filter(Longitude > -80) %>%
  filter(Longitude < -65) %>%
  filter(Latitude  < 50) %>%
  filter(Latitude > 25)

write.csv(sink, "sink.csv")

## plot the region
ggplot(data = sink, aes(x = Longitude, y = Latitude, color = sank)) +
  geom_point(shape = 22, aes(size = ifelse(sank == 1, 1.2, 1))) +
  scale_color_manual(values = c(NA, "firebrick"), 
                     labels = c("None", "1+"),
                     name = "Number of Wrecks") +
  theme_ipsum() +
  theme(legend.position = "bottom") +
  guides(size = "none")