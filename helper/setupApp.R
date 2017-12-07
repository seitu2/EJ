#set up libraries
options(stringsAsFactors=FALSE)
#if(!require('pacman')){install.packages('pacman')}
#require(pacman)
#libraries <- c('shiny','rgdal','rgeos','maptools','dplyr','tidyr','tmap','readxl',"SOAR",
#               'leaflet','sp','httr','XML','RCurl','rvest','gsubfn','acs','datasets','tidyverse','rmapshaper','DT')
#p_load(libraries,character.only=TRUE)

require(rgdal);require(rgeos);require(maptools);require(tidyverse);require(DT);require(leaflet);require(ggmap);require(acs)


#non-interactive constants
miles.to.meters <- 1609.344
wgs84 <-"+proj=longlat +datum=NAD83 +no_defs +ellps=GRS80 +towgs84=0,0,0"



###read in spatial data
ny.tracts.wgs84.simple<-readRDS(file="./data/ny.tracts.wgs84.simple.rds")
ny.tracts.wgs84<-readRDS(file="./data/nytracts.rds")
ny.tracts.pts.wgs84<-readRDS(file="./data/nytractspts.rds")

#create list of MPOs
geocode.source <- 'google' #google or dsk
mpos.census.tracts <-data.frame(ny.tracts.pts.wgs84[,c('geoid','mpo')])
mpos.list <-unique(c('All',mpos.census.tracts[!is.na( mpos.census.tracts$mpo ),'mpo']))

#ead in ACS shells
language.spoken.at.home <- readRDS(file='./data/language.spoken.at.home.rds')
age.groups <- readRDS(file='./data/age.groups.rds')
household.income.groups <- readRDS(file='./data/household.income.groups.rds')

#read in acs data;
lep.state<-readRDS(file='./data/lep.state.rds')
age.state<-readRDS(file='./data/age.state.rds')
household.income.state <- readRDS(file='./data/household.income.state.rds')


