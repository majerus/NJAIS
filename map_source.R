
library(rvest)
library(stringr)
library(ggmap)
library(leaflet)
library(rgdal) 
library(sp)

# download NJ mapping data 
#url<-"http://data.ci.newark.nj.us/dataset/db87f66a-6d79-4933-9011-f392fdce7eb8/resource/95db8cad-3a8c-41a4-b8b1-4991990f07f3/download/njcountypolygonv2.geojson"
#downloaddir<-"/Users/majerus/Desktop/NJAIS"
#destname<-"/Users/majerus/Desktop/NJAIS/nj.geojson"
#download.file(url, destname)
# nj_counties <- RJSONIO::fromJSON("/Users/majerus/Desktop/NJAIS/nj.geojson") # read in geojson 
# 
# read in geojson from online source 
nj_counties <- RJSONIO::fromJSON("http://data.ci.newark.nj.us/dataset/db87f66a-6d79-4933-9011-f392fdce7eb8/resource/95db8cad-3a8c-41a4-b8b1-4991990f07f3/download/njcountypolygonv2.geojson")

# # scrape in names of NJAIS independent schools
# x <- html("http://www.njais.org/page.cfm?p=365")
# 
# school <-
#   x %>%
#   html_nodes("strong") %>%
#   html_text() 
# 
# data <- as.data.frame(school)
# 
# data$state <- 'New Jersey' # create state variable 
# 
# # clean school names data 
# data$school <- as.character(data$school)
# 
# data$school <- ifelse(data$school == 'Golda Och Academy (formerly Solomon Schechter Day School of Essex and Union)',
#                       'Golda Och Academy',
#                       data$school)
# 
# data$school <- ifelse(str_detect(data$school, '(Fomerly Hebrew Academy of Morris County)'),
#                       'Gottesman RTW Academy',
#                       data$school)
# 
# data <- subset(data, data$school!='')
# data <- subset(data, data$school!=' ')
# 
# # geocode schools 
# geocodes <- geocode(paste(data$school, data$state))
# 
# data <- cbind(data, geocodes)
# write.csv(data, '/Users/majerus/Desktop/NJAIS/data/new_jersey_indepedent_schools_geocoded.csv')

data <- read.csv('/Users/majerus/Desktop/NJAIS/data/new_jersey_indepedent_schools_geocoded.csv')
# create leaflet map
leaflet(data) %>%
  addTiles() %>%
  setView(-74.38277,  40.45135, zoom = 8) %>%
  addGeoJSON(nj_counties) %>%
  addCircles(data$lon, data$lat, color = '#ff0000', radius=150, popup=data$school)

