options(warn = -1, scipen = 999, tigris_use_cache = T)
library(broom)
library(choroplethr)
library(dplyr)
library(forcats)
library(ggplot2)
library(gridExtra)
library(ggmap)
library(hrbrthemes)
library(htmlwidgets)
library(magrittr)
library(lubridate)
library(leaflet)
library(magrittr)
library(maptools)
library(purrr)
library(sp)#old
library(sf)
library(reshape2)
library(rgdal)
library(raster)
library(rgeos)
library(readr)
library(readr)
library(tidycensus)
library(tidyverse)
library(tigris)
library(tmap)
library(tmaptools)
library(usmap)
library(viridisLite)
library(viridis)

#update.packages()

#----------------------------------------------- Read in the data -------------------------------------------------
#read the data file (has to in the directory you're working in)
gainsville_df <- readr::read_csv("311_Service_Requests__myGNV_.csv")

#Get levels of the data frame
#gainsville_df %>% dplyr::mutate_all(as.factor) %>% purrr:map(levels)

#select columns of interest
gainsville_df <- gainsville_df %>% dplyr::select(ID, `Reporter Display`,
                                                 `Request Type`,
                                                 Description,
                                                 Latitude,
                                                 Longitude,
                                                 `Service Request Date`,
                                                 Acknowledged,
                                                 `Minutes to acknowledge`,
                                                 `Assigned To:`, 
                                                 `Last updated`,
                                                 Closed,
                                                 `Minutes to close`,
                                                 )


#NA replacement based on the class type {optional, more info is needed from the team}

# for(i in colnames(gainsville_df)){
#   if(class(gainsville_df[[i]])=="numeric"){
#     gainsville_df[[i]][which(is.na(gainsville_df[[i]]))] <- 99999
#   }
#   else if(class(gainsville_df[[i]])=="character"){
#     gainsville_df[[i]][which(is.na(gainsville_df[[i]]))] <- "XXXXX"
#   }
# }

#------------------------------------------------ Categorizing ---------------------------------------------------------

#Reporter Display (Case Owners)-> same users with different display, will be time consuming for Categorizing
#Minutes colums are too varied if you want to change them to days or hours

# Varities of dates and time
gainsville_df[c("Service Request Date","Acknowledged",
                "Last updated", "Closed")] <- lapply(gainsville_df[
                                                     c("Service Request Date",
                                                       "Acknowledged",
                                                       "Last updated", 
                                                       "Closed")] ,
                                                     lubridate::mdy_hm)

gainsville_df <- within(gainsville_df,`Assigned To:`[`Request Type` %in% 
                                                      c("Dead Animal (Public Property)",
                                                        "Emergency - Flooding",
                                                        "Emergency - Other",
                                                        "Emergency - Traffic",
                                                        "Emergency - Tree or Brush Debris",
                                                        "General Police Enforcement",
                                                        "Graffiti",
                                                        "Mosquito Control",
                                                        "Noise Complaint",
                                                        "Sewer/Wastewater",
                                                        "Traffic Enforcement",
                                                        "Request to General Manager of Utilities",
                                                        "Vehicle (Abandoned/Non-Operational)")] <- "Health and Public Safety")

gainsville_df <- within(gainsville_df,`Assigned To:`[`Request Type` %in% 
                                                     c("Building (Abandoned/Damaged)",
                                                       "Garbage Pick Up & Recycling",
                                                       "General Code Issue",
                                                       "Grass (Overgrown)",
                                                       "Request to City Auditor's Office",
                                                       "Trash/Debris (Private Property)",
                                                       "Trash/Debris (Public Property)",
                                                       "Tree Planting Suggestion",
                                                       "Tree/Limbs (Private Property)",
                                                       "Tree/Limbs (Public Property)")] <- "Neighborhood")

gainsville_df <- within(gainsville_df,`Assigned To:`[`Request Type` %in% 
                                                       c("Flooding",
                                                         "Maintenance Ditch/Drainage/Flooding",
                                                         "Other",
                                                         "Park Repair or Clean Up",
                                                         "Residential Rental Maintenance",
                                                         "Right-of-Way Maintenance",
                                                         "Road Repair",
                                                         "Sidewalk Repair",
                                                         "Street Sign",
                                                         "Street Sweeping",
                                                         "Streetlight/Lamp",
                                                         "Traffic/Pedestrian Signal",
                                                         "Water Pipe Leaks/Spills")] <- "Public Services")

gainsville_df <- within(gainsville_df,`Assigned To:`[`Request Type` %in% 
                                                       c("Parking Enforcement",
                                                         "Parking in Yard (Other Than Driveway)",
                                                         "Parking Meter Malfunction",
                                                         "Request for New Streetlight/Lamp")] <- "Transportation")

gainsville_df[c("Reporter Display","Request Type","Assigned To:")] <- lapply(
                                                                      gainsville_df[
                                                                      c("Reporter Display",
                                                                        "Request Type",
                                                                        "Assigned To:")] ,
                                                                      as.factor)

# lapply(gainsville_df, class) #look through class of columns
# as.data.frame(colnames(gainsville_df)) #look through column indices

#------------------------------------------ Assigned to Category------------------------------------------------------
#Category searched from  Gainsville 311 site

category_names_df <- data.frame(table(as.factor(gainsville_df$`Assigned To:`)))
colnames(category_names_df) <- c("Branch", "Frequencies")
#View(category_names_df)

#------------------------------------------ Request Type Category ------------------------------------------------------

#levels(gainsville_df$`Request Type`)
issue_type_df <- data.frame(table(as.factor(gainsville_df$`Request Type`)))
colnames(issue_type_df) <- c("Issue Category", "Frequencies")

issue_type_df <- dplyr::filter(issue_type_df, Frequencies > median(issue_type_df$Frequencies)) #Cutoffs
#View(category_names_df)

# NEED THIS IN CASE SHE NEEDS CALLS
# dat <- new_df %>% 
#   group_by(Request.Type) %>%
#   summarise(no_rows = length(Request.Type))

#------------------------------------------ Case Owners Category ------------------------------------------------------

#levels(gainsville_df$`Reporter Display`)
case_owners_df <- data.frame(table(as.factor(gainsville_df$`Reporter Display`)))
colnames(case_owners_df) <- c("Case Owner", "Frequencies")

case_owners_df <- dplyr::filter(case_owners_df, Frequencies > median(case_owners_df$Frequencies))
#View(case_owners_df)


#-------------------------------------------- Pre-plots for Analysis ----------------------------------------------------------------

#Service requests from Sep 2014 - Jan 2020

#scatter plot
ggplot2::ggplot(data= dplyr::tibble(lubridate::date(gainsville_df$`Service Request Date`)) %>%
                  dplyr::count(lubridate::date(gainsville_df$`Service Request Date`)),
                  aes(`lubridate::date(gainsville_df$\`Service Request Date\`)`, log(n, base=10), alpha= log(n,base = 10)))+ 
          theme_bw()+
          geom_point() +
          geom_smooth(method='loess') +          
          xlim(as.Date("2014-01-01", format= "%Y-%m-%d"), as.Date("2020-01-01", format= "%Y-%m-%d"))+
          ylim(0.0, 2.5)+
          theme(plot.title = element_text(hjust = 0.5), legend.position = "none")+
          labs(x="Days",y= "Service Requests")+
          ggtitle("311 Service Requests by Days")+
          ggsave("311_service_request_by_date.png",dpi = 600, height = 5.00, width = 6.0)

#Branch Work load from Sep 2014 - Jan 2020, thickness is frequency, diamond is the mean frequency for checking the box plot average

#Box plot
ggplot2::ggplot(data = gainsville_df, aes(`Assigned To:`, lubridate::date(`Service Request Date`), fill= `Assigned To:`, alpha= 0.5))+
          geom_violin(trim = F)+
          stat_summary(fun.y=mean, geom="point", shape=18, size=4)+
          ylim(as.Date("2014-01-01", format= "%Y-%m-%d"), as.Date("2020-01-01", format= "%Y-%m-%d"))+
          scale_fill_viridis(discrete = T)+
          theme_bw()+
          theme(plot.title = element_text(hjust = 0.5), legend.position = "none")+
          labs(x="311 Gainsville Branch",y= "Year", caption = "Note: Width represents call volume with Diamond being the Mean")+
          ggtitle("Branch Work Load by Year")+
          coord_flip()+
          ggsave("311_branch_busyness_by_year.png",dpi = 600, height = 5.00, width = 6.0)

#Request types from Sep 2014 - Jan 2020

#Bar chart  (Make this into a bubble graph)
ggplot2::ggplot(data= gainsville_df %>%
                  group_by(`Request Type`) %>%
                  summarise(num_calls = length(`Request Type`))
                , aes(x= `Request Type`, y= num_calls, fill= `Request Type`))+
         geom_bar(stat = "identity")+
         theme_bw()+
         theme(plot.title = element_text(hjust = 0.5), legend.position = "none",axis.text.x = element_text(angle = 90, vjust=0.5))+
         labs(x="Requested Services",y= "No. of Calls")+
         ggtitle("Number of Calls per Requested Service")+
         scale_fill_viridis(discrete = T)+
         ggsave("311_requests_per_call.png",dpi = 600, height = 5.00, width = 10)


# Year groups, calls per issues by group

#faceted by year, bar charts
ggplot2::ggplot(data= gainsville_df %>%
                  subset(lubridate::year(`Service Request Date`) != 2014) %>%
                  group_by(`Request Type`, `Assigned To:`, `Service Request Date`) %>%
                  summarise(num_calls = length(`Request Type`))
                , aes(x= `Request Type`, y= num_calls, fill= `Assigned To:`))+
          geom_bar(stat = "identity")+
          theme_bw()+
          theme(plot.title = element_text(hjust = 0.5), legend.position = "top",axis.text.x = element_text(angle = 90, vjust=0.5, hjust = 0.5))+
          labs(x="Requested Services",y= "No. of Calls", fill= "Assinged To:")+
          ggtitle("Number of Calls per Requested Service by year")+
          facet_wrap(~lubridate::year(`Service Request Date`))+
          scale_fill_viridis(discrete = T)+
          ggsave("311_requests_per_call_yr.png",dpi = 600, height = 9.00, width = 18)


#--------------------------------------------------- K-means Klustering ---------------------------------------------------------

#summary(is.na(gainsville_df$Latitude)) #see for NAs in latitude
#summary(is.na(gainsville_df$Longitude)) #see for NAs in longitude



#----------------------------------------------------- tidycensus -------------------------------------------------------------


#tidycensus::census_api_key("21adc0b3d6e900378af9b7910d04110cdd38cd75", install = T, overwrite = T)

census <- tidycensus::load_variables(2010, "sf1", cache = T)
variable <- tidycensus::load_variables(2010, "acs5", cache = TRUE)

alachua <- tidycensus::get_acs(state = "FL", county = "Alachua",
                               geography = "tract", geometry = T,
                               variables = "B19013_001")


################################################## Shape file reading #####################################################

data_file_location <- choose.files() #This has to be the border shape file, zones will be from tidycensus
gainsville_bound <- sf::st_read(data_file_location, stringsAsFactors = F)
data_file_location_2 <- choose.files() #This has to be the border shape file, zones will be from tidycensus
gainsville_zones <- sf::st_read(data_file_location_2, stringsAsFactors = F)

########################## Previous method won't work because of different units in shapefile ###################################################

tracts <- rgdal::readOGR(data_file_location_2) #proj4strings is here
tract_df <- as.data.frame(fortify(gainsville_bound))


tracts@data$ID <- rownames(tracts@data)
tracts_points <- fortify(tracts, region = "ID")

names(tracts_points)[6] <- "ID"

tracts_df <- left_join(tracts_points, tracts@data, by = "ID")

gainsville_sp <- sp::SpatialPoints(coord= gainsville_df[, c("Latitude", "Longitude")], proj4string = tracts@proj4string)

gainsville_df <- cbind(gainsville_df, over(x= gainsville_sp, y= tracts))

######################################################This works, but what is the unit type exactly?############################################
#incomplete
boundry_plot <- ggplot2::ggplot(data= gainsville_bound)+
                          geom_sf(aes(geometry= geometry))+
                          geom_sf(data= gainsville_zones, aes(geometry= geometry))
boundry_plot
########################################################################################################################

#Get the census codes
coord <- data.frame(lat= gainsville_df$Latitude, long= gainsville_df$Longitude)

# Run this code below if you have 1:15 mins to kill, otherwise read from a file
coord$`Census Code` <- apply(coord, 1, function(row) tigris::call_geolocator_latlon(row['lat'], row['long']))
colnames(coord) <- c("Latitude", "Longitude", "Census Code")

#Census code: 120010011003032-The first two being the state, next three the county, and the following six the tract.

#Get the tract
coord$Tract <- substr(coord$`Census Code`, start= 6, stop= 11)

#readr::write_csv(coord,"contains_latlon_cenCodes_cenTract.csv")
#coord <- readr::read_csv("contains_latlon_cenCodes_cenTract.csv")

#Merge two data frames
gainsville_df[names(coord)] <- coord

#Change classes again for newly added columns
gainsville_df[c("Census Code","Tract")] <- lapply(
                                                  gainsville_df[
                                                    c("Census Code","Tract")] ,
                                                  as.numeric)

#str(gainsville_df)

# Apply the color ranks based on the population of county
MapPalette <- colorQuantile(palette = "viridis", domain = alachua$estimate, n= 10)

#plot the county with tidycensus, and add markers
alachua_draft_plot <-alachua %>%
                          st_transform(crs= "+init=epsg:4326") %>%
                          leaflet(width = "100%") %>%
                          addProviderTiles(provider = "Stamen.TonerLines") %>%
                          addPolygons(popup = ~str_extract(NAME, "^([^,]*)"),
                                      stroke= F,
                                      smoothFactor = 0,
                                      fillOpacity = 0.7,
                                      color= ~MapPalette(estimate)) %>%
                          addLegend("bottomright",
                                    pal= MapPalette,
                                    values= ~estimate,
                                    title= "Alachua County Population",
                                    opacity = 1) %>%
                          addCircleMarkers(data= gainsville_df,
                                           lat= ~Latitude,
                                           lng= ~Longitude,
                                           popup = gainsville_df$`Request Type`,
                                           weight = 1,
                                           radius = 0.6,
                                           opacity= 0.5,
                                           color= 'midnightblue',
                                           fill = ~Tract)

htmlwidgets::saveWidget(alachua_draft_plot, "dynamic_alachua.html")

