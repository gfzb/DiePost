###########################################################################################################
#### 
#### Erfüllt die Post die verordneten Erreichbarkeitsauflagen (VPG Art. 33 Abs. 4)? 
#### Ein datengetriebener Überprüfungsansatz
#### Grünenfelder Zumbach GmbH - Sozialforschung und Beratung
#### David Zumbach, 30.11./01.12.2017
####
###########################################################################################################

# Load Packages -------------------------------------------------------------------------------------------
if (!require("pacman")) install.packages("pacman")
pacman::p_load(readr, dplyr, purrr, tidyr, RSwissMaps, geosphere, jsonlite, devtools, extrafont, ggplot2, ggmap)

# Prepare Geodata of Swiss Post Services  -----------------------------------------------------------------

# Load data
dt <- read_delim("zugangspunkte-post.csv", delim = ";", escape_double = FALSE, trim_ws = TRUE)

# Traditional Branches (Poststellen)
branches <- dt %>%
  select(Id, address_zip, address_city, geopoint, service_de) %>%
  spread(service_de, service_de) %>%
  filter(

    # Core Business
    !is.na(`Briefmarken`)&
      !is.na(`Briefe und Pakete versenden`)&
      !is.na(`Briefe und Pakete empfangen`)&
      
      # Money
      !is.na(`Bargeldbezug`)&
      !is.na(`Einzahlungen`)&
      !is.na(`Bareinzahlungen in CHF und EUR`)&
      !is.na(`Change`)
    
    ) %>%
  separate(geopoint, c("lat", "lon"), sep = ", ") %>%
  mutate(
    lat = as.numeric(lat), 
    lon = as.numeric(lon),
    address_city = paste0(address_zip, " ", address_city)
    ) %>%
  select(address_city, lon, lat) %>%
  distinct() %>%
  mutate(type = "Branch")

# Agencies (Postagenturen)
agencies <- dt %>%
  select(poityp_de, address_zip, address_city, geopoint) %>%
  filter(poityp_de == "Filiale") %>%
  separate(geopoint, c("lat", "lon"), sep = ", ") %>%
  mutate(
    lat = as.numeric(lat), 
    lon = as.numeric(lon),
    address_city = paste0(address_zip, " ", address_city)
  ) %>%
  filter(!lat %in% branches$lat) %>%
  select(address_city, lon, lat) %>%
  distinct() %>%
  mutate(type = "Agency")

# Home-delivery service (Hausservice)
hdservices <- dt %>%
  select(poityp_de, address_zip, address_city, geopoint) %>%
  filter(poityp_de == "Hausservice") %>%
  separate(geopoint, c("lat", "lon"), sep = ", ") %>%
  mutate(
    lat = as.numeric(lat), 
    lon = as.numeric(lon),
    address_city = paste0(address_zip, " ", address_city)
  ) %>%
  filter(!lat %in% branches$lat&!lat %in% agencies$lat) %>%
  select(address_city, lon, lat) %>%
  distinct() %>%
  mutate(type = "Home-delivery service")

# All in one dataset
all <- rbind.data.frame(branches, agencies, hdservices)

# Clean up
rm(list = setdiff(ls(), c("all")))

# EDA
all %>%
  mutate(type = factor(type, levels = c("Branch", "Agency", "Home-delivery service"))) %>%
  ggplot(aes(type)) + geom_bar() + theme_minimal()

# Prepare Random Sample of Swiss Addresses ----------------------------------------------------------------

# Load data
housekeys <- read_delim("hausnummer-und-hauskey.csv", delim = ";", escape_double = FALSE, trim_ws = TRUE)
streets <- read_delim("strassenbezeichnungen.csv", delim = ";", escape_double = FALSE, trim_ws = TRUE)
postalcodes <- read_delim("plz-verzeichnis.csv", delim = ";", escape_double = FALSE, trim_ws = TRUE)
municipalities <- read_delim("politische-gemeinden.csv", delim = ";", escape_double = FALSE, trim_ws = TRUE)

# Join streets and house numbers
housekeys <- housekeys %>%
  mutate(
    HNR = ifelse(is.na(HNRA), HNR, paste0(HNR, HNRA)),
    HNR = gsub("NA", "", HNR, fixed = T)
    ) %>%
  select(STRID, HNR)

streets <- streets %>%
  select(STRID, STRBEZ2L, ONRP)

addresses <- left_join(housekeys, streets) %>%
  select(-STRID)

rm(housekeys, streets)

# Join street/house numbers with postal code
postalcodes <- postalcodes %>%
  select(ONRP, POSTLEITZAHL, ORTBEZ27, BFSNR)

addresses <- left_join(addresses, postalcodes) %>%
  select(-ONRP)

rm(postalcodes)

# Join street/house numbers/postal code with political municipality
municipalities <- municipalities %>%
  select(BFSNR, GEMEINDENAME)

addresses <- left_join(addresses, municipalities)

rm(municipalities)

# Draw Inhabitants-per-Address-Weighted Random Sample ------------------------------------------------------

# Generate inhabitants per address ratios per municipality
## Load data
population <- read_delim("bfs.csv", ";", quote = "\\\"", escape_double = FALSE, trim_ws = TRUE, skip = 2)
names(population) <- c("year", "BFSNR", "type", "population")

population <- population %>%
  mutate(BFSNR = as.numeric(gsub("\\D", "", BFSNR))) %>%
  filter(!is.na(BFSNR)) %>%
  select(BFSNR, population)

## Summarise addresses per municipality
ad_mun <- addresses %>%
  group_by(BFSNR) %>%
  summarise(
    addresses = n(),
    name = nth(GEMEINDENAME, 1)
    ) %>%
  # we exclude FL
  filter(BFSNR < 7000)

## Join population and number of addresses
ad_mun <- full_join(ad_mun, population)

## Find population size of new municipalities (wikipedia)
ad_mun$population[ad_mun$name == "Estavayer"] <- 6291 
ad_mun$population[ad_mun$name == "Cheyres-Châbles"] <- 2222 
ad_mun$population[ad_mun$name == "Riviera"] <- 4132
ad_mun$population[ad_mun$name == "Goms"] <- 4421 
ad_mun$population[ad_mun$name == "Crans-Montana"] <- 10711 
ad_mun <- ad_mun %>% filter(!is.na(name))
rm(population)

## Calculate inhabitants per address ratio
ad_mun <- ad_mun %>%
  mutate(ratio = population/addresses) %>%
  select(BFSNR, ratio)

mun.plot(ad_mun$BFSNR, ad_mun$ratio, 2017)

# Join addressess with ratio (and google Chavannes-près-Renens and Sonogno ;-)
addresses <- left_join(addresses, ad_mun) %>%
  select(-BFSNR) %>%
  filter(!is.na(ratio))

rm(ad_mun)
  
# Draw random sample with respect to variable ratio
set.seed(1313)
ind <- c(1:nrow(addresses)) 
ind <- sample(ind, 2500, prob = addresses$ratio)
addresses <- addresses[ind,]

# Geocoding of Sample Addresses Using Google Maps' Geocoding API ------------------------------------------

# Base settings
key <- "get-your-own-key-:-)"

# Prepare adresses for query
addresses <- addresses %>%
  mutate(
    adr = paste(STRBEZ2L, HNR, POSTLEITZAHL, ORTBEZ27, sep = " "),
    adr = ifelse(is.na(HNR), paste(STRBEZ2L, POSTLEITZAHL, ORTBEZ27, sep = " "), adr),
    adr = gsub(" ", "+", adr)
  )

# Query function
geocoding_google <- function(address, key){
  
  gd <- fromJSON(readLines(paste0("https://maps.googleapis.com/maps/api/geocode/json?address=", address, "&key=", key)))
  lon <- ifelse(length(gd$results$geometry$location$lng)>0, gd$results$geometry$location$lng, NA)
  lat <- ifelse(length(gd$results$geometry$location$lat)>0, gd$results$geometry$location$lat, NA)
  coord <- paste0(lon, ",", lat) 
  coord
  
}

# API Query (takes a while)
addresses$coordinates <- NULL
for(i in 1:length(addresses$adr)){
  
  addresses$coordinates[i] <- geocoding_google(addresses$adr[i], key)
  print(i)
  
}

addresses <- addresses %>%
  separate(coordinates, c("lon", "lat"), ",") %>%
  mutate(
    lon = as.numeric(lon),
    lat = as.numeric(lat)
  )

## coordinates_addresses <- map_chr(addresses_v, ~geocoding_google(.x, key))

# Missing values (and obviously false coordinates)
missings <- addresses %>%
  filter(is.na(lon) | lon < 5.9 | lon > 10.5 | 
           is.na(lat) | lat < 45.7 | lat > 47.9)

addresses <- addresses %>%
  filter(!is.na(lon), !lon < 5.9, !lon > 10.5, 
         !is.na(lat), !lat < 45.7, !lat > 47.9)

write.table(missings, "missings.csv", row.names = F, sep = ";", dec = ".")
print(paste0("Missing values: ", 100*nrow(missings)/2500, "%"))

# Look missing values up to avoid bias 
## https://map.search.ch and https://podcast.paravan.ch/?page_id=2278
missings_cor <- read_delim("missings_cor.csv", delim = ";", escape_double = FALSE, trim_ws = TRUE)
rm(missings)

# Update addresses
addresses <- bind_rows(addresses, missings_cor)

# Spatial distribution of sample
map <- get_map(location = "Switzerland", maptype = "terrain", source = "google", language = "de-CH", color = "bw", zoom = 7)
ggmap(map) + geom_point(data = addresses, aes(x = lon, y = lat), size = 1, color = "black") +
  geom_point(data = missings_cor, aes(x = lon, y = lat), size = 1, color = "red")

rm(missings_cor)

# Calculate Beeline Distances Between Addresses and Swiss Post Services (Preselection Distance API) ------

# Distance Function (adapted from goo.gl/N8kUhA)
distance_km <- function(lon1, lat1, lon2, lat2) {
  
  lonlat1 = map2(lon1, lat1, ~c(.x, .y))
  lonlat2 = map2(lon2, lat2, ~c(.x, .y))
  
  map2_dbl(lonlat1, lonlat2, ~distHaversine(.x, .y))/1000

}

# Calculate Matrix (rows: Post Services / cols: Addresses)
distances <- map2(addresses$lon, addresses$lat, ~distance_km(.x, .y, all$lon, all$lat)) %>%
  map_call(cbind)

# Get Travel Time to the 5 Closest Swiss Post Services Using Google Maps' Distance Matrix API  ------------

# Create service info variable
all <- all %>%
  mutate(service_info = paste0(address_city, "_", type))

# API key
key <- "huh, don't-you-understand"

# Set departure time
departure_time <- as.integer(as.POSIXct("2017-12-05 09:30:00", tz = "MET"))

# Prepare variables (one set for each type of transportation: Walking, Public Transportation, Car, Bike)
addresses$walking_traveltime_1 <- NA
addresses$walking_destination_1 <- NA
addresses$walking_traveltime_2 <- NA
addresses$walking_destination_2 <- NA
addresses$walking_traveltime_3 <- NA
addresses$walking_destination_3 <- NA
addresses$walking_traveltime_4 <- NA
addresses$walking_destination_4 <- NA
addresses$walking_traveltime_5 <- NA
addresses$walking_destination_5 <- NA

addresses$transit_traveltime_1 <- NA
addresses$transit_destination_1 <- NA
addresses$transit_traveltime_2 <- NA
addresses$transit_destination_2 <- NA
addresses$transit_traveltime_3 <- NA
addresses$transit_destination_3 <- NA
addresses$transit_traveltime_4 <- NA
addresses$transit_destination_4 <- NA
addresses$transit_traveltime_5 <- NA
addresses$transit_destination_5 <- NA

addresses$driving_traveltime_1 <- NA
addresses$driving_destination_1 <- NA
addresses$driving_traveltime_2 <- NA
addresses$driving_destination_2 <- NA
addresses$driving_traveltime_3 <- NA
addresses$driving_destination_3 <- NA
addresses$driving_traveltime_4 <- NA
addresses$driving_destination_4 <- NA
addresses$driving_traveltime_5 <- NA
addresses$driving_destination_5 <- NA

addresses$bicycling_traveltime_1 <- NA
addresses$bicycling_destination_1 <- NA
addresses$bicycling_traveltime_2 <- NA
addresses$bicycling_destination_2 <- NA
addresses$bicycling_traveltime_3 <- NA
addresses$bicycling_destination_3 <- NA
addresses$bicycling_traveltime_4 <- NA
addresses$bicycling_destination_4 <- NA
addresses$bicycling_traveltime_5 <- NA
addresses$bicycling_destination_5 <- NA

# Loop over every addresses and its 5 closest Post Services 
for(i in 1:nrow(addresses)){
  
  # Coordinates of branch under scrutiny
  origin_lon <- addresses$lon[i]
  origin_lat <- addresses$lat[i]
  
  # Neighbouring branches (based on Haversine distance)
  n <- head(sort(distances[,i]), na.rm = T, 6)
  n <- n[-1]
  
  # Prepare destination variable for query
  destinations <- vector(length = 5)
  destinations_coordinates <- NULL
  
  for(j in 1:5){
    
    dest <- paste0(all$lat[which(n[j]==distances[,i])], ",", all$lon[which(n[j]==distances[,i])])
    destinations_coordinates <- paste0(destinations_coordinates, "|", dest)
    destinations[j] <- all$service_info[which(n[j]==distances[,i])]
    
  }
  
  destinations_coordinates <- substr(destinations_coordinates, 2, nchar(destinations_coordinates))
  
  ## Query for WALKING time
  walking <- fromJSON(readLines(paste0("https://maps.googleapis.com/maps/api/distancematrix/json?units=metric&origins=",
                                       origin_lat, ",", origin_lon, "&destinations=", destinations_coordinates,
                                       "&mode=walking&departure_time=", departure_time, "&key=", key)))
  
  walking_time <- walking$rows$elements[[1]]$duration$value
  
  ### Extract parameters for closest branches based on travel time
  if(length(walking_time[!is.na(walking_time)]) == 1){
    
    addresses$walking_traveltime_1[i] <- nth(walking_time, 1, order_by = walking_time)
    addresses$walking_destination_1[i] <- destinations[which(walking_time == nth(walking_time, 1, order_by = walking_time))]
    
  }
  if(length(walking_time[!is.na(walking_time)]) == 2){
    
    addresses$walking_traveltime_1[i] <- nth(walking_time, 1, order_by = walking_time)
    addresses$walking_destination_1[i] <- destinations[which(walking_time == nth(walking_time, 1, order_by = walking_time))]
    addresses$walking_traveltime_2[i] <- nth(walking_time, 2, order_by = walking_time)
    addresses$walking_destination_2[i] <- destinations[which(walking_time == nth(walking_time, 2, order_by = walking_time))]
    
  }
  if(length(walking_time[!is.na(walking_time)]) == 3){
    
    addresses$walking_traveltime_1[i] <- nth(walking_time, 1, order_by = walking_time)
    addresses$walking_destination_1[i] <- destinations[which(walking_time == nth(walking_time, 1, order_by = walking_time))]
    addresses$walking_traveltime_2[i] <- nth(walking_time, 2, order_by = walking_time)
    addresses$walking_destination_2[i] <- destinations[which(walking_time == nth(walking_time, 2, order_by = walking_time))]
    addresses$walking_traveltime_3[i] <- nth(walking_time, 3, order_by = walking_time)
    addresses$walking_destination_3[i] <- destinations[which(walking_time == nth(walking_time, 3, order_by = walking_time))]
    
  }
  if(length(walking_time[!is.na(walking_time)]) == 4){
    
    addresses$walking_traveltime_1[i] <- nth(walking_time, 1, order_by = walking_time)
    addresses$walking_destination_1[i] <- destinations[which(walking_time == nth(walking_time, 1, order_by = walking_time))]
    addresses$walking_traveltime_2[i] <- nth(walking_time, 2, order_by = walking_time)
    addresses$walking_destination_2[i] <- destinations[which(walking_time == nth(walking_time, 2, order_by = walking_time))]
    addresses$walking_traveltime_3[i] <- nth(walking_time, 3, order_by = walking_time)
    addresses$walking_destination_3[i] <- destinations[which(walking_time == nth(walking_time, 3, order_by = walking_time))]
    addresses$walking_traveltime_4[i] <- nth(walking_time, 4, order_by = walking_time)
    addresses$walking_destination_4[i] <- destinations[which(walking_time == nth(walking_time, 4, order_by = walking_time))]
    
  }
  if(length(walking_time[!is.na(walking_time)]) == 5){
    
    addresses$walking_traveltime_1[i] <- nth(walking_time, 1, order_by = walking_time)
    addresses$walking_destination_1[i] <- destinations[which(walking_time == nth(walking_time, 1, order_by = walking_time))]
    addresses$walking_traveltime_2[i] <- nth(walking_time, 2, order_by = walking_time)
    addresses$walking_destination_2[i] <- destinations[which(walking_time == nth(walking_time, 2, order_by = walking_time))]
    addresses$walking_traveltime_3[i] <- nth(walking_time, 3, order_by = walking_time)
    addresses$walking_destination_3[i] <- destinations[which(walking_time == nth(walking_time, 3, order_by = walking_time))]
    addresses$walking_traveltime_4[i] <- nth(walking_time, 4, order_by = walking_time)
    addresses$walking_destination_4[i] <- destinations[which(walking_time == nth(walking_time, 4, order_by = walking_time))]
    addresses$walking_traveltime_5[i] <- nth(walking_time, 5, order_by = walking_time)
    addresses$walking_destination_5[i] <- destinations[which(walking_time == nth(walking_time, 5, order_by = walking_time))]
    
  }
  
  ## Query for PUBLIC TRANSPORT time
  transit <- fromJSON(readLines(paste0("https://maps.googleapis.com/maps/api/distancematrix/json?units=metric&origins=",
                                       origin_lat, ",", origin_lon, "&destinations=", destinations_coordinates,
                                       "&mode=transit&departure_time=", departure_time, "&key=", key)))
  
  transit_time <- transit$rows$elements[[1]]$duration$value
  
  ### Extract parameters for closest branches based on travel time
  if(length(transit_time[!is.na(transit_time)]) == 1){
    
    addresses$transit_traveltime_1[i] <- nth(transit_time, 1, order_by = transit_time)
    addresses$transit_destination_1[i] <- destinations[which(transit_time == nth(transit_time, 1, order_by = transit_time))]
    
  }
  if(length(transit_time[!is.na(transit_time)]) == 2){
    
    addresses$transit_traveltime_1[i] <- nth(transit_time, 1, order_by = transit_time)
    addresses$transit_destination_1[i] <- destinations[which(transit_time == nth(transit_time, 1, order_by = transit_time))]
    addresses$transit_traveltime_2[i] <- nth(transit_time, 2, order_by = transit_time)
    addresses$transit_destination_2[i] <- destinations[which(transit_time == nth(transit_time, 2, order_by = transit_time))]
    
  }
  if(length(transit_time[!is.na(transit_time)]) == 3){
    
    addresses$transit_traveltime_1[i] <- nth(transit_time, 1, order_by = transit_time)
    addresses$transit_destination_1[i] <- destinations[which(transit_time == nth(transit_time, 1, order_by = transit_time))]
    addresses$transit_traveltime_2[i] <- nth(transit_time, 2, order_by = transit_time)
    addresses$transit_destination_2[i] <- destinations[which(transit_time == nth(transit_time, 2, order_by = transit_time))]
    addresses$transit_traveltime_3[i] <- nth(transit_time, 3, order_by = transit_time)
    addresses$transit_destination_3[i] <- destinations[which(transit_time == nth(transit_time, 3, order_by = transit_time))]
    
  }
  if(length(transit_time[!is.na(transit_time)]) == 4){
    
    addresses$transit_traveltime_1[i] <- nth(transit_time, 1, order_by = transit_time)
    addresses$transit_destination_1[i] <- destinations[which(transit_time == nth(transit_time, 1, order_by = transit_time))]
    addresses$transit_traveltime_2[i] <- nth(transit_time, 2, order_by = transit_time)
    addresses$transit_destination_2[i] <- destinations[which(transit_time == nth(transit_time, 2, order_by = transit_time))]
    addresses$transit_traveltime_3[i] <- nth(transit_time, 3, order_by = transit_time)
    addresses$transit_destination_3[i] <- destinations[which(transit_time == nth(transit_time, 3, order_by = transit_time))]
    addresses$transit_traveltime_4[i] <- nth(transit_time, 4, order_by = transit_time)
    addresses$transit_destination_4[i] <- destinations[which(transit_time == nth(transit_time, 4, order_by = transit_time))]
    
  }
  if(length(transit_time[!is.na(transit_time)]) == 5){
    
    addresses$transit_traveltime_1[i] <- nth(transit_time, 1, order_by = transit_time)
    addresses$transit_destination_1[i] <- destinations[which(transit_time == nth(transit_time, 1, order_by = transit_time))]
    addresses$transit_traveltime_2[i] <- nth(transit_time, 2, order_by = transit_time)
    addresses$transit_destination_2[i] <- destinations[which(transit_time == nth(transit_time, 2, order_by = transit_time))]
    addresses$transit_traveltime_3[i] <- nth(transit_time, 3, order_by = transit_time)
    addresses$transit_destination_3[i] <- destinations[which(transit_time == nth(transit_time, 3, order_by = transit_time))]
    addresses$transit_traveltime_4[i] <- nth(transit_time, 4, order_by = transit_time)
    addresses$transit_destination_4[i] <- destinations[which(transit_time == nth(transit_time, 4, order_by = transit_time))]
    addresses$transit_traveltime_5[i] <- nth(transit_time, 5, order_by = transit_time)
    addresses$transit_destination_5[i] <- destinations[which(transit_time == nth(transit_time, 5, order_by = transit_time))]
    
  }
  
  ## Query for DRIVING time
  driving <- fromJSON(readLines(paste0("https://maps.googleapis.com/maps/api/distancematrix/json?units=metric&origins=",
                                       origin_lat, ",", origin_lon, "&destinations=", destinations_coordinates,
                                       "&departure_time=", departure_time, "&key=", key)))
  
  driving_time <- driving$rows$elements[[1]]$duration_in_traffic$value
  
  ### Extract parameters for closest branches based on travel time
  if(length(driving_time[!is.na(driving_time)]) == 1){
    
    addresses$driving_traveltime_1[i] <- nth(driving_time, 1, order_by = driving_time)
    addresses$driving_destination_1[i] <- destinations[which(driving_time == nth(driving_time, 1, order_by = driving_time))]
    
  }
  if(length(driving_time[!is.na(driving_time)]) == 2){
    
    addresses$driving_traveltime_1[i] <- nth(driving_time, 1, order_by = driving_time)
    addresses$driving_destination_1[i] <- destinations[which(driving_time == nth(driving_time, 1, order_by = driving_time))]
    addresses$driving_traveltime_2[i] <- nth(driving_time, 2, order_by = driving_time)
    addresses$driving_destination_2[i] <- destinations[which(driving_time == nth(driving_time, 2, order_by = driving_time))]
    
  }
  if(length(driving_time[!is.na(driving_time)]) == 3){
    
    addresses$driving_traveltime_1[i] <- nth(driving_time, 1, order_by = driving_time)
    addresses$driving_destination_1[i] <- destinations[which(driving_time == nth(driving_time, 1, order_by = driving_time))]
    addresses$driving_traveltime_2[i] <- nth(driving_time, 2, order_by = driving_time)
    addresses$driving_destination_2[i] <- destinations[which(driving_time == nth(driving_time, 2, order_by = driving_time))]
    addresses$driving_traveltime_3[i] <- nth(driving_time, 3, order_by = driving_time)
    addresses$driving_destination_3[i] <- destinations[which(driving_time == nth(driving_time, 3, order_by = driving_time))]
    
  }
  if(length(driving_time[!is.na(driving_time)]) == 4){
    
    addresses$driving_traveltime_1[i] <- nth(driving_time, 1, order_by = driving_time)
    addresses$driving_destination_1[i] <- destinations[which(driving_time == nth(driving_time, 1, order_by = driving_time))]
    addresses$driving_traveltime_2[i] <- nth(driving_time, 2, order_by = driving_time)
    addresses$driving_destination_2[i] <- destinations[which(driving_time == nth(driving_time, 2, order_by = driving_time))]
    addresses$driving_traveltime_3[i] <- nth(driving_time, 3, order_by = driving_time)
    addresses$driving_destination_3[i] <- destinations[which(driving_time == nth(driving_time, 3, order_by = driving_time))]
    addresses$driving_traveltime_4[i] <- nth(driving_time, 4, order_by = driving_time)
    addresses$driving_destination_4[i] <- destinations[which(driving_time == nth(driving_time, 4, order_by = driving_time))]
    
  }
  if(length(driving_time[!is.na(driving_time)]) == 5){
    
    addresses$driving_traveltime_1[i] <- nth(driving_time, 1, order_by = driving_time)
    addresses$driving_destination_1[i] <- destinations[which(driving_time == nth(driving_time, 1, order_by = driving_time))]
    addresses$driving_traveltime_2[i] <- nth(driving_time, 2, order_by = driving_time)
    addresses$driving_destination_2[i] <- destinations[which(driving_time == nth(driving_time, 2, order_by = driving_time))]
    addresses$driving_traveltime_3[i] <- nth(driving_time, 3, order_by = driving_time)
    addresses$driving_destination_3[i] <- destinations[which(driving_time == nth(driving_time, 3, order_by = driving_time))]
    addresses$driving_traveltime_4[i] <- nth(driving_time, 4, order_by = driving_time)
    addresses$driving_destination_4[i] <- destinations[which(driving_time == nth(driving_time, 4, order_by = driving_time))]
    addresses$driving_traveltime_5[i] <- nth(driving_time, 5, order_by = driving_time)
    addresses$driving_destination_5[i] <- destinations[which(driving_time == nth(driving_time, 5, order_by = driving_time))]
    
  }
  
  ## Query for BICYCLING time
  bicycling <- fromJSON(readLines(paste0("https://maps.googleapis.com/maps/api/distancematrix/json?units=metric&origins=",
                                         origin_lat, ",", origin_lon, "&destinations=", destinations_coordinates,
                                         "&mode=bicycling&departure_time=", departure_time, "&key=", key)))
  
  bicycling_time <- bicycling$rows$elements[[1]]$duration$value
  
  ### Extract parameters for closest branches based on travel time
  if(length(bicycling_time[!is.na(bicycling_time)]) == 1){
    
    addresses$bicycling_traveltime_1[i] <- nth(bicycling_time, 1, order_by = bicycling_time)
    addresses$bicycling_destination_1[i] <- destinations[which(bicycling_time == nth(bicycling_time, 1, order_by = bicycling_time))]
    
  }
  if(length(bicycling_time[!is.na(bicycling_time)]) == 2){
    
    addresses$bicycling_traveltime_1[i] <- nth(bicycling_time, 1, order_by = bicycling_time)
    addresses$bicycling_destination_1[i] <- destinations[which(bicycling_time == nth(bicycling_time, 1, order_by = bicycling_time))]
    addresses$bicycling_traveltime_2[i] <- nth(bicycling_time, 2, order_by = bicycling_time)
    addresses$bicycling_destination_2[i] <- destinations[which(bicycling_time == nth(bicycling_time, 2, order_by = bicycling_time))]
    
  }
  if(length(bicycling_time[!is.na(bicycling_time)]) == 3){
    
    addresses$bicycling_traveltime_1[i] <- nth(bicycling_time, 1, order_by = bicycling_time)
    addresses$bicycling_destination_1[i] <- destinations[which(bicycling_time == nth(bicycling_time, 1, order_by = bicycling_time))]
    addresses$bicycling_traveltime_2[i] <- nth(bicycling_time, 2, order_by = bicycling_time)
    addresses$bicycling_destination_2[i] <- destinations[which(bicycling_time == nth(bicycling_time, 2, order_by = bicycling_time))]
    addresses$bicycling_traveltime_3[i] <- nth(bicycling_time, 3, order_by = bicycling_time)
    addresses$bicycling_destination_3[i] <- destinations[which(bicycling_time == nth(bicycling_time, 3, order_by = bicycling_time))]
    
  }
  if(length(bicycling_time[!is.na(bicycling_time)]) == 4){
    
    addresses$bicycling_traveltime_1[i] <- nth(bicycling_time, 1, order_by = bicycling_time)
    addresses$bicycling_destination_1[i] <- destinations[which(bicycling_time == nth(bicycling_time, 1, order_by = bicycling_time))]
    addresses$bicycling_traveltime_2[i] <- nth(bicycling_time, 2, order_by = bicycling_time)
    addresses$bicycling_destination_2[i] <- destinations[which(bicycling_time == nth(bicycling_time, 2, order_by = bicycling_time))]
    addresses$bicycling_traveltime_3[i] <- nth(bicycling_time, 3, order_by = bicycling_time)
    addresses$bicycling_destination_3[i] <- destinations[which(bicycling_time == nth(bicycling_time, 3, order_by = bicycling_time))]
    addresses$bicycling_traveltime_4[i] <- nth(bicycling_time, 4, order_by = bicycling_time)
    addresses$bicycling_destination_4[i] <- destinations[which(bicycling_time == nth(bicycling_time, 4, order_by = bicycling_time))]
    
  }
  if(length(bicycling_time[!is.na(bicycling_time)]) == 5){
    
    addresses$bicycling_traveltime_1[i] <- nth(bicycling_time, 1, order_by = bicycling_time)
    addresses$bicycling_destination_1[i] <- destinations[which(bicycling_time == nth(bicycling_time, 1, order_by = bicycling_time))]
    addresses$bicycling_traveltime_2[i] <- nth(bicycling_time, 2, order_by = bicycling_time)
    addresses$bicycling_destination_2[i] <- destinations[which(bicycling_time == nth(bicycling_time, 2, order_by = bicycling_time))]
    addresses$bicycling_traveltime_3[i] <- nth(bicycling_time, 3, order_by = bicycling_time)
    addresses$bicycling_destination_3[i] <- destinations[which(bicycling_time == nth(bicycling_time, 3, order_by = bicycling_time))]
    addresses$bicycling_traveltime_4[i] <- nth(bicycling_time, 4, order_by = bicycling_time)
    addresses$bicycling_destination_4[i] <- destinations[which(bicycling_time == nth(bicycling_time, 4, order_by = bicycling_time))]
    addresses$bicycling_traveltime_5[i] <- nth(bicycling_time, 5, order_by = bicycling_time)
    addresses$bicycling_destination_5[i] <- destinations[which(bicycling_time == nth(bicycling_time, 5, order_by = bicycling_time))]
    
  }
  
  # Clean up
  rm(driving, walking, bicycling, transit)
  
  print(i)
  
}

# Clean up
rm(list = setdiff(ls(), c("addresses", "all", "distances")))

# Prepare DataViz ----------------------------------------------------------------------------------------

# Install awtools to use its wonderful theme_a (https://github.com/awhstin/awtools)
devtools::install_github('awhstin/awtools')
library(awtools)

# DataViz Pt. 1: Travel Times to closest Service (walking/transit) ---------------------------------------

# Type to Closest Service
addresses %>%
  gather(parameter, value, c(20, 40)) %>%
  select(adr, GEMEINDENAME, parameter, value, walking_destination_1, transit_destination_1) %>%
  separate(walking_destination_1, c("gmd1", "type1"), "_") %>%
  separate(transit_destination_1, c("gmd2", "type2"), "_") %>%
  mutate(type = ifelse(parameter == "walking_traveltime_1", type1, type2)) %>%
  select(-gmd1, -gmd2, -type1, -type2) %>%
  group_by(adr) %>%
  arrange(value) %>%
  slice(1) %>%
  mutate(
    type = ifelse(type == "Branch", "Poststelle", type),
    type = ifelse(type == "Agency", "Postagentur", type),
    type = ifelse(type == "Home-delivery service", "Hausservice", type),
    type = factor(type, levels = c("Poststelle", "Postagentur", "Hausservice"))
  ) %>%
  ggplot(aes(type)) + geom_bar(fill = "#face3a") +
  a_theme() +
  theme(text = element_text(family = "Arial")) +
  labs(
    title = "Das Schweizer Postnetz – Welchen\nPostservice erreichen die Haushalte\nheute am schnellsten?",
    subtitle = "Zufallsstichprobe: 2'500 Schweizer Postadressen\nWegzeiten simuliert für Dienstag, den 5. Dezember 2017, um 9:30 Uhr.",
    x = "",
    y = "",
    caption = "Daten: Open Data Portal of Swiss Post | Wegzeiten: Google Maps Distance Matrix API"
  )

ggsave("1_typ.png", dpi = 2000, width = 7)

# Travel Time to Closest Service
addresses %>%
  gather(parameter, value, c(20, 40)) %>%
  select(adr, GEMEINDENAME, parameter, value, walking_destination_1, transit_destination_1) %>%
  separate(walking_destination_1, c("gmd1", "type1"), "_") %>%
  separate(transit_destination_1, c("gmd2", "type2"), "_") %>%
  mutate(type = ifelse(parameter == "walking_traveltime_1", type1, type2)) %>%
  select(-gmd1, -gmd2, -type1, -type2) %>%
  group_by(adr) %>%
  arrange(value) %>%
  slice(1) %>%
  ggplot(aes(value)) + 
  geom_vline(aes(xintercept = nth(value, floor(0.5*length(value[!is.na(value)])), 
                                  order_by = value)), color = "gray50") +
  geom_text(aes(x = nth(value, floor(0.5*length(value[!is.na(value)])), 
                        order_by = value), y=27.5, label="50%"), color = "gray50", size=4, hjust=-0.15) +
  geom_vline(aes(xintercept = nth(value, floor(0.9*length(value[!is.na(value)])), 
                                  order_by = value)), color = "gray50") +
  geom_text(aes(x = nth(value, floor(0.9*length(value[!is.na(value)])), 
                        order_by = value), y=27.5, label="90%"), color = "gray50", size=4, hjust=-0.15) +
  geom_histogram(fill = "#face3a", bins = 500) +
  scale_x_time(limits = c(0, 2400)) +
  a_theme() +
  theme(text = element_text(family = "Arial")) +
  labs(
    title = "...und wie lange sind sie unterwegs,\nwenn sie dabei zu Fuss oder mit dem\nöffentlichen Verkehr anreisen?",
    subtitle = "Zufallsstichprobe: 2'500 Schweizer Postadressen\nWegzeiten simuliert für Dienstag, den 5. Dezember 2017, um 9:30 Uhr.",
    x = "Wegzeit (hh:mm:ss)",
    y = "",
    caption = "Daten: Open Data Portal of Swiss Post | Wegzeiten: Google Maps Distance Matrix API"
  ) +
  scale_y_continuous(limits = c(0, 30))

ggsave("2_dauer.png", dpi = 2000, width = 7)

# Braches and agencies (walking/publ.tr.)
addresses %>%
  gather(parameter, value, c(20, 40)) %>%
  select(adr, GEMEINDENAME, parameter, value, walking_destination_1, transit_destination_1) %>%
  separate(walking_destination_1, c("gmd1", "type1"), "_") %>%
  separate(transit_destination_1, c("gmd2", "type2"), "_") %>%
  mutate(type = ifelse(parameter == "walking_traveltime_1", type1, type2)) %>%
  select(-gmd1, -gmd2, -type1, -type2) %>%
  group_by(adr) %>%
  arrange(value) %>%
  slice(1) %>%
  filter(type == "Agency" | type == "Branch") %>%
  ggplot(aes(value)) + 
  geom_vline(aes(xintercept = nth(value, floor(0.5*length(value[!is.na(value)])), 
                                  order_by = value)), color = "gray50") +
  geom_text(aes(x = nth(value, floor(0.5*length(value[!is.na(value)])), 
                        order_by = value), y=27.5, label="50%"), color = "gray50", size=4, hjust=1.15) +
  geom_vline(aes(xintercept = nth(value, floor(0.9*length(value[!is.na(value)])), 
                                  order_by = value)), color = "gray50") +
  geom_text(aes(x = nth(value, floor(0.9*length(value[!is.na(value)])), 
                        order_by = value), y=27.5, label="90%"), color = "gray50", size=4, hjust=-0.15) +
  geom_histogram(fill = "#face3a", bins = 500) +
  scale_x_time(limits = c(0, 2400)) +
  a_theme() +
  theme(text = element_text(family = "Arial")) +
  labs(
    title = "Die Erreichbarkeit von Schweizer\nPoststellen und Postagenturen",
    subtitle = "Zufallsstichprobe: 2'500 Schweizer Postadressen\nWegzeiten simuliert für Dienstag, den 5. Dezember 2017, um 9:30 Uhr.",
    x = "Wegzeit (hh:mm:ss)",
    y = "",
    caption = "Daten: Open Data Portal of Swiss Post | Wegzeiten: Google Maps Distance Matrix API"
  ) +
  scale_y_continuous(limits = c(0, 30))

ggsave("3_poststellen.png", dpi = 2000, width = 7)

# Home-delivery service (walking/publ.tr.)
addresses %>%
  gather(parameter, value, c(20, 40)) %>%
  select(adr, GEMEINDENAME, parameter, value, walking_destination_1, transit_destination_1) %>%
  separate(walking_destination_1, c("gmd1", "type1"), "_") %>%
  separate(transit_destination_1, c("gmd2", "type2"), "_") %>%
  mutate(type = ifelse(parameter == "walking_traveltime_1", type1, type2)) %>%
  select(-gmd1, -gmd2, -type1, -type2) %>%
  group_by(adr) %>%
  arrange(value) %>%
  slice(1) %>%
  filter(!type == "Agency" & !type == "Branch") %>%
  ggplot(aes(value)) + 
  geom_vline(aes(xintercept = nth(value, floor(0.5*length(value[!is.na(value)])), 
                                  order_by = value)), color = "gray50") +
  geom_text(aes(x = nth(value, floor(0.5*length(value[!is.na(value)])), 
                        order_by = value), y=27.5, label="50%"), color = "gray50", size=4, hjust=1.15) +
  geom_vline(aes(xintercept = nth(value, floor(0.9*length(value[!is.na(value)])), 
                                  order_by = value)), color = "gray50") +
  geom_text(aes(x = nth(value, floor(0.9*length(value[!is.na(value)])), 
                        order_by = value), y=27.5, label="90%"), color = "gray50", size=4, hjust=-0.15) +
  geom_histogram(fill = "#face3a", bins = 500) +
  scale_x_time(limits = c(0, 2400)) +
  a_theme() +
  theme(text = element_text(family = "Arial")) +
  labs(
    title = "Die Erreichbarkeit des Hausservice\nder Schweizerischen Post",
    subtitle = "Zufallsstichprobe: 2'500 Schweizer Postadressen\nWegzeiten simuliert für Dienstag, den 5. Dezember 2017, um 9:30 Uhr.",
    x = "Wegzeit (hh:mm:ss)",
    y = "",
    caption = "Daten: Open Data Portal of Swiss Post | Wegzeiten: Google Maps Distance Matrix API"
  ) +
  scale_y_continuous(limits = c(0, 30))

ggsave("4_hausservice.png", dpi = 2000, width = 7)

# DataViz Pt. 2: Map -------------------------------------------------------------------------------------

# Download Swiss Map
map <- get_map(location = "Switzerland", maptype = "terrain", source = "google", language = "de-CH", color = "bw", zoom = 7)

times <- addresses %>%
  gather(parameter, value, c(20, 40)) %>%
  select(lon, lat, parameter, value, walking_destination_1, transit_destination_1) %>%
  separate(walking_destination_1, c("gmd1", "type1"), "_") %>%
  separate(transit_destination_1, c("gmd2", "type2"), "_") %>%
  mutate(type = ifelse(parameter == "walking_traveltime_1", type1, type2)) %>%
  select(-gmd1, -gmd2, -type1, -type2) %>%
  group_by(lon, lat) %>%
  arrange(value) %>%
  slice(1) %>%
  mutate(
    type = ifelse(type == "Branch", "Poststelle", type),
    type = ifelse(type == "Agency", "Postagentur", type),
    type = ifelse(type == "Home-delivery service", "Hausservice", type),
    type = factor(type, levels = c("Poststelle", "Postagentur", "Hausservice")),
    cat = cut(as.numeric(value), breaks = c(0, 600, 1199, 100000), labels = c("< 10 Minuten", "10-20 Minuten", "> 20 Minuten"))
  )

ggmap(map) + 
  geom_point(data = times, aes(x = lon, y = lat, color = cat), size = 0.3) +  
  labs(
    title = "Das Schweizer Postnetz – Welchen\nPostservice die Haushalte erreichen\nund wie schnell.",
    subtitle = "Zufallsstichprobe: 2'500 Schweizer Postadressen\nWegzeiten simuliert für Dienstag, den 5. Dezember 2017, um 9:30 Uhr.",
    caption = "Daten: Open Data Portal of Swiss Post |\nWegzeiten: Google Maps Distance Matrix API"
    ) +
  scale_color_manual(values = c("#99b898", "#fecea8", "#ff847c")) +
  a_theme() + 
  theme(
    text = element_text(family = "Arial"),
    axis.title = element_blank(),
    axis.ticks = element_blank(),
    axis.text.x = element_blank(),
    legend.position = "none",
    axis.text.y = element_blank()
    ) +
  guides(colour = guide_legend(override.aes = list(size=5))) +
  facet_grid(type~cat)

ggsave("5_karte.png", dpi = 1000, width = 7)