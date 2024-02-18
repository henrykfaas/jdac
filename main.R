# ----- JDAC Henryk 10 Feb -----


# ----- Packages -----
# renv::install("tidyverse")
# renv::install(c("httr", "jsonlite"))
# renv::install("mapview")
# renv::install("sf")
# renv::install("janitor")
# renv::install("reshape2")
# renv::install("here")

#renv::init() 
#renv::snapshot()
renv::restore()

library(reshape2)
library(janitor)
library(tidyverse)
library(httr)
library(jsonlite)
library(mapview)
library(sf)
library(leaflet)
library(here)


# ---- Functions ----


# --- load data ----

# ---- access API from TFL Bike Points
res = GET("https://api.tfl.gov.uk/BikePoint/")

# convert raw unicode to character vector, then convert JSON into list
data = fromJSON(rawToChar(res$content))

# variables
names(data)

# select from bikepoints data only longitude , Latitude, name, and id
data_bp <- data %>% select(id, commonName, lat, lon)

# ---- plot bikepoints over map of London ----

# create a blank map
m <- leaflet() 

# mark points with blue circles,
m <- addCircleMarkers(m, lat=data_bp$lat, lng=data_bp$lo, label=data_bp$id, radius = 5, color = "blue")

# add an underlying basemap
m <- addProviderTiles(m, providers$OpenTopoMap) 

# center the view on London
m <- setView(m, -0.119,51.525, zoom = 12) 

# plot map with bike points 
m

# --- Index of deprivation ----
file_iod <- "data/IoD/IoD2019_Scores.csv"
filename_iod <- file.path(here::here(), filename_iod)

#filename_iod <- "./data/IoD/IoD2019_Scores.csv"

# Loading
df_iod <- read.csv(filename_iod)

# clean up variable names
df_iod <- clean_names(df_iod)

# variables iod
names(df_iod)

unique(df_iod$local_authority_district_name_2019)

# ---- London boroughs ----
file_london <- "data/boroughs/london_boroughs.csv"
filename_london <- file.path(here::here(), file_london)

filename_london <- "./data/boroughs/london_boroughs.csv"

# Loading
df_london <- read.csv(filename_london)

#df_london_temp <- df_london_temp %>% select(borough)
london_boroughs <- unique(df_london$borough)

# retain only London districts in iod data 
df_iod_london <- df_iod[df_iod$local_authority_district_name_2019 %in% london_boroughs, ]

names(df_iod_london)

# select variable for indicator
df_iod_london <- df_iod_london %>% select(local_authority_district_name_2019, income_score_rate, health_deprivation_and_disability_score)

# average over local authorities
df_iod_london_avg <- df_iod_london %>% group_by(local_authority_district_name_2019) %>%
  summarize(income = mean(income_score_rate),
            health = mean(health_deprivation_and_disability_score))

# sort aggregate score health
sorted_df_iod <- df_iod_london_avg[order(df_iod_london_avg$health), ]

# plot health score
sorted_df_iod %>% ggplot(aes(x=reorder(local_authority_district_name_2019, health), y = health)) + 
  geom_bar(stat = "identity") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1)) +
  labs(title="Health score by borough", x = element_blank(), y = "Score")


# ---- Plot health and income

df.long<-melt(df_iod_london_avg )

ggplot(df.long,aes(x = reorder(local_authority_district_name_2019, value),y = value,fill=variable))+
  geom_bar(stat="identity",position="dodge") + 
  theme_light() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1)) +
  labs(title="Health & Income score by borough", x = element_blank(), y = "Score") + 
  theme(legend.title = element_blank()) 


