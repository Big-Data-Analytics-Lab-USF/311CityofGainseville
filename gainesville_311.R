options(warn = -1, scipen = 999, tigris_use_cache = T, tigris_class = "sf")
#install.packages("digest", dependencies = TRUE, INSTALL_opts = '--no-lock')
library(acs)
library(broom)
library(choroplethr)
library(cluster)
library(censusr)
library(censusapi)
library(dplyr)
library(devtools)
library(flextable)
library(forcats)
library(factoextra)
library(ggplot2)
library(gridExtra)
library(ggmap)
library(hrbrthemes)
library(htmlwidgets)
library(installr)
#installr::uninstall.packages(c(""))
library(janitor)
library(magrittr)
library(lubridate)
library(leaflet)
library(leaflet.extras)
library(maps)
library(maptools)
library(magrittr)
library(purrr)
library(sp)
library(sf)
library(stringr)
library(spdep)
library(reshape2)
library(RCurl)
library(rgdal)
library(raster)
library(RColorBrewer)
library(rgeos)
library(readr)
library(RANN)
library(spatialEco)
library(tidycensus)
library(tidyverse)
library(tigris)
library(tibble)
library(tmap)
library(totalcensus)
library(tmaptools)
library(usmap)
library(usethis)
library(viridisLite)
library(viridis)
library(XML)

#update.packages()

#----------------------------------------------- Read in the data -------------------------------------------------

#read the data file (has to in the directory you're working in)
gainsville_df <- readr::read_csv("311_Service_Requests__myGNV_.csv")

#Get levels of the data frame
#gainsville_df %>% dplyr::mutate_all(as.factor) %>% purrr:map(levels)

#read multiple files and store into a dataframe, then branch out to make variables
# multi_files_data <- sapply(c("first_file.csv","second_file.csv"), readr::read_csv, simplify=FALSE) %>% 
#                         bind_rows(.id = "id")


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

#----------------------------------------------- Apply sf to the data ----------------------------------------------

#make a data frame just for latitude, logitude, and location. Change location to the Spatial Points
gnv_latlon <- readr::read_csv("311_Service_Requests__myGNV_.csv") %>% 
                      dplyr::select(ID,
                                    Latitude,
                                    Longitude,
                                    Location) %>%
                      dplyr::mutate(Location = gsub(x= Location, pattern = "POINT \\(|\\)", replacement = "")) %>% 
                      tidyr::separate(col = "Location", into = c("lon", "lat"), sep = " ") %>% 
                      sf::st_as_sf(coords = c(4,5)) %>% 
                      sf::st_set_crs(4326)

#Read in the corrupted shapefile, extract the latitude and logitudes only, and make polygon with them
gnv_poly <-  sf::st_read("C:\\Users\\ThinkPad\\OneDrive\\Documents\\Project_311\\Supplements\\GIS_cgbound\\cgbound.shp") %>% 
                sf::st_transform(crs = 4326) %>% 
                sf::st_polygonize() %>% 
                sf::st_union()


#----------------------------------------------------- See outliers before ---------------------------------------------------

#Add a coloum, in this column we see which spatials points fall within the polygon
dplyr::mutate(gnv_latlon,
        check = as.vector(sf::st_intersects(x = gnv_latlon, y = gnv_poly, sparse = FALSE))) -> outliers_before

#plot these spatials points in respect to the polygon
ggplot2::ggplot() +
          geom_sf(data= gnv_poly) +
          geom_point(data= outliers_before, aes(x= Longitude, y= Latitude, color= check), alpha= 0.6)+
          scale_color_manual(values= c("sienna3","deepskyblue4"))+
          theme_bw()+
          labs(title = "Spatial Outliers- Before cleaning")+
          guides(color= guide_legend(title= "Within polygon?"))+
          theme(legend.position = "top", plot.title = element_text(hjust = 0.5),
                legend.background = element_blank(),
                legend.box.background = element_rect(colour = "black"))+
          ggsave("outliers_after_before.png", height = 8.0, width = 8.0, units = "in")
  

#----------------------------------------------------- See outliers after ---------------------------------------------------
sf::st_filter(x= outliers_before, y= gnv_poly, predicate= st_intersects) -> outliers_after

ggplot2::ggplot() +
          geom_sf(data= gnv_poly) +
          geom_sf()+
          geom_point(data= outliers_after, aes(x= Longitude, y= Latitude, color= check), alpha= 0.6)+
          scale_color_manual(values= c("deepskyblue4"))+
          theme_bw()+
          labs(title = "Spatial Outliers- After cleaning")+
          guides(color= guide_legend(title= "Within polygon?"))+
          theme(legend.position = "top", plot.title = element_text(hjust = 0.5),
                legend.background = element_blank(),
                legend.box.background = element_rect(colour = "black"))+
          ggsave("outliers_after_cleaning.png", height = 8.0, width = 8.0, units = "in")


#------------------------------------------ Remove outliers in main frame ----------------------------------------

#Nullify the column because we're going to add with people of gainesville only.
gainsville_df[ ,c('Latitude', 'Longitude')] <- list(NULL)

match_these_df <- outliers_after[, 1:3]
st_geometry(match_these_df) <- NULL

gainsville_df <- merge(match_these_df, gainsville_df, by = "ID")

####---RM

#Removed the geometry column from the main frame for now, use it for future workaround....
#gainsville_df <- merge(outliers_after, gainsville_df, by = "ID")

####---RM_END
#------------------------------------------------ Categorizing -------------------------------------------------------

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

#Chagen the the category levels of request types
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

# NEED THIS IN CASE DR. Loni NEEDS CALLS
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

# category based scatter plot
ggplot2::ggplot(data= gainsville_df %>%
                        group_by(lubridate::date(`Service Request Date`), `Assigned To:`) %>%
                          count(),
                aes(`lubridate::date(\`Service Request Date\`)`, log(n, base=10), alpha= log(n, base= 10)))+ 
          theme_bw()+
          geom_point(aes(color = `Assigned To:`)) +
          geom_smooth(method='loess') +          
          xlim(as.Date("2014-01-01", format= "%Y-%m-%d"), as.Date("2020-01-01", format= "%Y-%m-%d"))+
          ylim(0.0, 2.5)+
          scale_alpha(guide = 'none')+
          theme(plot.title = element_text(hjust = 0.5), legend.position = "top")+
          scale_color_manual(values = c("indianred4", "greenyellow", "royalblue4", "gold"))+
          labs(x="Days",y= "Service Requests (Percentage)")+
          ggtitle("311 Service Requests by Days per Categories")+
          ggsave("311_cat_based_yr_call.png",dpi = 600, height = 5.00, width = 8.0)


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


#----------------------------------------------------- fetching the census stuff ------------------------------------------------

#tidycensus::census_api_key("21adc0b3d6e900378af9b7910d04110cdd38cd75", install = T, overwrite = T)

census <- tidycensus::load_variables(2010, "sf1", cache = T) #Summary of all census
variable <- tidycensus::load_variables(2010, "acs5", cache = T) #get the variable from ACS: American Community Survey

#Get the alachua county census data GEOID- STATE+COUNTY+TRACT- 2+3+6
#variables placement:
#Total Population > Median Income > Per-Capita > Ratio of income to Poverty level of family > Employement >
alachua <- tidycensus::get_acs(state = "FL", county = "Alachua", 
                               geography = "tract", geometry = T,
                               variables = c("B01003_001", "B07011_001", "B19301_001", "B17026_001", "B23001_001"))

#Now split the dataframe into list of unique variables
alachua <- alachua %>% group_split(variable)

#**because estimate is the common number among each variable, it will be tricky and unreadble to write a complex code**
#Follow the variable placemement label for naming new dataframe

####################################### REMOVE EXTRANEOUS TRACTS FOR NOW ####################################################
remove_these_tracts <- c("12001110800", "12001002220", "12001002219", "12001002217", "12001001908", "12001001813", "12001001811",
                         "12001001802", "12001002204", "12001002101", "12001001702", "12001001701", "12001001400", "12001000700")

alachua_population <- alachua[[1]]
alachua_population <- alachua_population[ ! alachua_population$GEOID %in% remove_these_tracts, ]

alachua_median_income <- alachua[[2]]
alachua_median_income <- alachua_median_income[ ! alachua_median_income$GEOID %in% remove_these_tracts, ]

alachua_per_capita <- alachua[[3]]
alachua_per_capita <- alachua_per_capita[ ! alachua_per_capita$GEOID %in% remove_these_tracts, ]

alachua_per_poverty_level <- alachua[[4]]
alachua_per_poverty_level <- alachua_per_poverty_level[ ! alachua_per_poverty_level$GEOID %in% remove_these_tracts, ]

alachua_employment <- alachua[[5]]
alachua_employment <- alachua_employment[ ! alachua_employment$GEOID %in% remove_these_tracts, ]


############################################################################################################################
# alachua_population <- alachua[[1]] 
# alachua_median_income <- alachua[[2]] 
# alachua_per_capita <- alachua[[3]] 
# alachua_per_poverty_level <- alachua[[4]] 
# alachua_employment <- alachua[[5]] 

#------------------------------------------------------------- Tidycensus manipulation ---------------------------------------
#CAUTION: DO NOT WRITE SHAPEFILES TO CSV, IT WILL GET CORRUPT!
coord <- readr::read_csv("contains_latlon_cenCodes_cenTract.csv") 

#If you've read the coord file above, then SKIP running the block section ---SKIP till ---SKIP_END

####---SKIP

#Get the census codes
coord <- data.frame(lat= gainsville_df$Latitude, long= gainsville_df$Longitude)

# Run this code below if you have 1.25 mins to kill, otherwise read from a file, (may cause erratic behaviours)
##Do not hit stop to compilation, this process takes some time.
coord$`Census Code` <- apply(coord[1:nrow(coord), ], 1, function(row) tigris::call_geolocator_latlon(row['lat'], row['long']))
colnames(coord) <- c("Latitude", "Longitude", "Census Code")

#GeoID: eg. 120010011003032-The first 11 digits represt geo id in the tidyverse.

#Get the geographical ID
coord$`Geo ID` <- substr(coord$`Census Code`, start = 1, stop = 11)

#Census code: eg. 120010011003032-The first two being the state, next three the county, and the following six the tract.

#Get the tract
coord$Tract <- substr(coord$`Census Code`, start= 6, stop= 11)

#save the features per tract, match the coord & alachua variable GEOIDs, if they match get estimate, and save
coord$Population <- alachua_population$estimate[match(coord$`Geo ID`, alachua_population$GEOID)]
coord$`Median Income` <- alachua_median_income$estimate[match(coord$`Geo ID`, alachua_median_income$GEOID)]
coord$`Per Capita` <- alachua_per_capita$estimate[match(coord$`Geo ID`, alachua_per_capita$GEOID)]
coord$`Under Poverty` <- alachua_per_poverty_level$estimate[match(coord$`Geo ID`, alachua_per_poverty_level$GEOID)]
coord$`Employed` <- alachua_employment$estimate[match(coord$`Geo ID`, alachua_employment$GEOID)]

#drop NA when necessary
#coord <- coord %>% drop_na()

readr::write_csv(coord,"contains_latlon_cenCodes_cenTract.csv")
#coord <- readr::read_csv("contains_latlon_cenCodes_cenTract.csv")

####---SKIP_END

#Get the tract shapes, since shapes are consistent, it doesn't matter which list we choose. We will go with population
coord$Geomtry <- alachua[[1]][["geometry"]][match(coord$`Geo ID`, alachua[[1]][["GEOID"]])]

#Merge two data frames
gainsville_df[names(coord)] <- coord

# #Updated merge
# gainsville_df <- semi_join(gainsville_df, coord)
# gainsville_df[names(coord)] <- coord

#Change classes again for newly added columns
gainsville_df[c("Census Code","Tract", "Geo ID", "Population",
                "Median Income","Per Capita","Under Poverty",
                "Employed")] <- lapply(gainsville_df[c("Census Code","Tract", "Geo ID", "Population",
                                                      "Median Income","Per Capita","Under Poverty",
                                                      "Employed")] ,
                                                  as.numeric)
#View(coord)

#Follow the SKIP labels again, and SKIP the code compilation
####---SKIP

# #-----------------------------------------------------(OUTDATED) See outliers before (OUTDATED)---------------------------------------------------
# #Make a copy
# outliers_before <- gainsville_df
# 
# #get the spatial coordinates with sp
# sp::coordinates(outliers_before) <- ~Longitude+Latitude
# 
# #get the outliers with outliers function, and store them in their zscore block
# outliers_before$zscore <- spatialEco::outliers(outliers_before$Tract)
# 
# grDevices::png("Outliers_before_cleaned.png")
# #examine the outliers with the spplot function (zcore is required)
# sp::spplot(outliers_before, "zscore", col.regions= rev(RColorBrewer::brewer.pal(3, "Greys")), 
#            main= "Outliers before cleaning",
#            sub= "* not to scale",
#            alpha= 0.7)
# 
# grDevices::dev.off()
# 
# #------------------------------------------(OUTDATED)Remove outliers with earth distance (OUTDATED)----------------------------------------
# 
# # This is a made up function to remove the outliers outside of a circle, works great.
# outlierRemoval <<- function (long, lati, meanLong, meanLati) {
#   rad <- pi/180 #earth axis
#   a1 <- lati * rad
#   a2 <- long * rad
#   b1 <- meanLati * rad #Converting standard coordinates to radians
#   b2 <- meanLong * rad
#   dlon <- b2 - a2
#   dlat <- b1 - a1
#   a <- (sin(dlat/2))^2 + cos(a1) * cos(b1) * (sin(dlon/2))^2 #Haversine formula
#   c <- 2 * atan2(sqrt(a), sqrt(1 - a)) 
#   R <- 6378.145 #earth's constant
#   d <- R * c #converting the distance back to standard coordinate
#   return(d)
# }
# 
# #gainsville_df <- purrr::map_df(gainsville_df, rev) #reverse the column
# 
# #try sending in reverse or look at the distance and pick another distance windows cutoff
# gainsville_df$Distance <- outlierRemoval(gainsville_df$Longitude, gainsville_df$Latitude, 
#                                          mean(gainsville_df$Longitude), mean(gainsville_df$Latitude))
# 
# gainsville_df <- gainsville_df[gainsville_df$Distance <= 5.95,] # Filter those above 5095m
# 
# 
# #View(gainsville_df)
# 
# #-----------------------------------------------------(OUTDATED) See outliers after (OUTDATED)---------------------------------------------------
# #Make a copy
# outliers_after <- gainsville_df
# 
# #get the spatial coordinates with sp
# sp::coordinates(outliers_after) <- ~Longitude+Latitude
# 
# #get the outliers with outliers function, and store them in their zscore block
# outliers_after$zscore <- spatialEco::outliers(outliers_after$Tract)
# 
# grDevices::png("Outliers_after_cleaned.png")
# #examine the outliers with the spplot function (zcore is required)
# sp::spplot(outliers_after, "zscore", col.regions=  rev(RColorBrewer::brewer.pal(3, "Greys")), 
#            main= "Outliers after cleaning",
#            sub= "* not to scale",
#            alpha= 0.7)
# grDevices::dev.off()

####---SKIP_END


#--------------------------------------------------------------- Re-adjust the variables --------------------------------------------------------

#str(gainsville_df)

#Change the coord GEO ID to chars IFNIF if you've read the coord file earlier
coord[c("Geo ID")] <- lapply(coord[c("Geo ID")], as.character)

#comes handy in polar plots

#Modify the alachua variables with main frames's Geo ID to project only the gainsville coordinates
alachua_population <- alachua_population %>% filter(GEOID %in% unique(gainsville_df$`Geo ID`))
alachua_median_income <- alachua_median_income %>% filter(GEOID %in% unique(gainsville_df$`Geo ID`))
alachua_per_capita <- alachua_per_capita %>% filter(GEOID %in% unique(gainsville_df$`Geo ID`))
alachua_per_poverty_level <- alachua_per_poverty_level %>% filter(GEOID %in% unique(gainsville_df$`Geo ID`))
alachua_employment <- alachua_employment %>% filter(GEOID %in% unique(gainsville_df$`Geo ID`))


#--------------------------------------------------------------- Map -----------------------------------------------------------------------------

# Apply the color ranks based on the population of gainsville (used to be whole alachua)

#Change this when you have K-means cluster

#Reverse the map palette
#MapPalettes::map_palette("bruiser", n=10)
MapPalette <- leaflet::colorQuantile(palette = "RdYlBu", domain = alachua_population$estimate, n= 10, reverse = F) #"Greys"
pal <- leaflet::colorFactor(palette = colorRampPalette(c("lightgreen", "peru","deeppink4","slateblue4"))(length(gainsville_df$`Assigned To:`)),
                            domain = gainsville_df$`Assigned To:`)

#plot the county with tidycensus, and add markers
alachua_draft_plot <- alachua_population %>%
                          st_transform(crs= "+init=epsg:4326") %>%
                          leaflet() %>%
                          addFullscreenControl() %>%
                          addProviderTiles(provider = "Wikimedia") %>%
                          addPolygons(data= gnv_poly,
                                      color= "black",
                                      weight= 2) %>%
                          addPolygons(popup = ~str_extract(NAME, "^([^,]*)"),
                                      stroke= F,
                                      smoothFactor = 0,
                                      fillOpacity = 0.7,
                                      color= ~MapPalette(estimate)) %>%
                          addLegend("bottomright",
                                    pal= MapPalette,
                                    values= ~estimate,
                                    title= "Population Density by Tract (%)",
                                    opacity = 1) %>%
                          addCircleMarkers(data= gainsville_df,
                                           lat= ~Latitude,
                                           lng= ~Longitude,
                                           popup = gainsville_df$`Request Type`,
                                           weight = 1,
                                           radius = 0.6,
                                           #opacity= 0.5,
                                           color= ~pal(`Assigned To:`)) %>% 
                          addLegend("topright", 
                                    pal = pal, values = gainsville_df$`Assigned To:`, 
                                    title = "Responsible Branch")

htmlwidgets::saveWidget(alachua_draft_plot, "dynamic_gainsville_pop_category_type.html")

# Plot testing here!!

#Clean the gainsville_df so only gainsville data remains
#gainsville_df %<>% tidyr::drop_na("Census Code")

######################################################### ADDED tigris class "sf"################################################
fl <-  tracts("FL", cb = TRUE)


ggplot(fl) + geom_sf()

cb <- core_based_statistical_areas(cb = TRUE)

pdx <- filter(cb, grepl("Gainesville, FL", NAME))

p1 <- fl[pdx,]

ggplot() + 
  geom_sf(data = p1) + 
  geom_sf(data = pdx, fill = NA, color = "red")

w1 <- st_within(fl, pdx)

print(length(w1))

w2 <- map_lgl(w1, function(x) {
  if (length(x) == 1) {
    return(TRUE)
  } else {
    return(FALSE)
  }
})

p2 <- fl[w2,]

ggplot() + 
  geom_sf(data = p2) + 
  geom_sf(data = pdx, fill = NA, color = "red")

##################################################################################################################################


ggplot() + 
  geom_sf(data = gainsville_df,aes(geometry= Geomtry, fill= Population), alpha= 0.2) +
  coord_sf(crs = "+init=epsg:4326")+ #crs = 26911
  geom_sf(data= gnv_poly, alpha= 0.1)+
  scale_fill_viridis(discrete = F, option= "magma")+
  theme_bw()



#--------------------------------------------------- K-means clustering ---------------------------------------------------------

req_type_tract_df <- data.frame(gainsville_df$`Request Type`, gainsville_df$Tract)
assign_to_tract_df <- data.frame(gainsville_df$`Assigned To:`, gainsville_df$Tract)

#Make the frequency chart per tract for REQUEST TYPES
freq_chart_summarise <- req_type_tract_df %>%
                          group_by(gainsville_df..Request.Type.,gainsville_df.Tract) %>% 
                          summarise(n=n())
# Cast the chart
tracts_per_req <- reshape2::dcast(freq_chart_summarise, gainsville_df.Tract~gainsville_df..Request.Type., value.var = "n", fill = 0)

#Get the columns totals
totals_tracts_per_req <- tracts_per_req[,-1] %>%
                                  adorn_totals("col")

#Paste the total to request type chart
tracts_per_req$Total <- totals_tracts_per_req$Total

#add the population column by taking "Population" column as a list, matching the tracts, and putting the mathed pop. to short dataframe
tracts_per_req["Population"] <- lapply("Population", 
                                       function(x) gainsville_df[[x]][match(tracts_per_req$gainsville_df.Tract, 
                                                                            gainsville_df$Tract)])

# NORMALIZE the requests per tract
normalizeFunc_tracts_per_req <<-function(v) { 
  
    p = tracts_per_req$Population 
    v * p / sum(v * p) 
}

tracts_per_req <- dplyr::mutate_at(tracts_per_req, 
                                         .vars = 2:41, 
                                         normalizeFunc_tracts_per_req)

#write to the file
readr::write_csv(tracts_per_req,"normalized_tracts_requests.csv")

#View(tracts_per_req)

#Make the frequency chart per tract for ASSIGNED TO
freq_chart_summarise <- assign_to_tract_df %>%
                          group_by(gainsville_df..Assigned.To..,gainsville_df.Tract) %>% 
                          summarise(n=n())

# Cast the chart again
tracts_per_assigned <- reshape2::dcast(freq_chart_summarise, gainsville_df.Tract~gainsville_df..Assigned.To.., value.var = "n", fill = 0)

#Get the columns totals
totals_tracts_per_assigned <- tracts_per_assigned[,-1] %>%
                                   adorn_totals("col")

#Paste the total to Assigned To chart
tracts_per_assigned$Total <- totals_tracts_per_assigned$Total

#add the population column by taking "Population" column as a list, matching the tracts, and putting the mathed pop. to short dataframe
tracts_per_assigned["Population"] <- lapply("Population", 
                                       function(x) gainsville_df[[x]][match(tracts_per_assigned$gainsville_df.Tract, 
                                                                            gainsville_df$Tract)])

#NORMALIZE the issue types per tract
normalizeFunc_tracts_per_assigned <<-function(v) { 
  
  p = tracts_per_assigned$Population 
  v * p / sum(v * p) 
}

tracts_per_assigned <- dplyr::mutate_at(tracts_per_assigned, 
                                   .vars = 2:4, 
                                   normalizeFunc_tracts_per_assigned)

#write to the file
readr::write_csv(tracts_per_assigned,"normalized_tracts_assignedTo.csv")

#View(tracts_per_assigned)

#incase of removal is needed
# tracts_per_assigned <- tracts_per_assigned[tracts_per_assigned$Total > 1, ]
# tracts_per_req <- tracts_per_req[tracts_per_req$Total > 1, ]

tracts_per_req[c("gainsville_df.Tract")] <- lapply(tracts_per_req[c("gainsville_df.Tract")], as.numeric)

#Within-cluster sum of square to get a good cluster
factoextra::fviz_nbclust(tracts_per_req[ ,2:41], kmeans, method = "wss")+
                  theme_bw()+
                  theme(plot.title = element_text(hjust = 0.5))+
                  ggsave("twss_cluster_plot.png",dpi = 600)

#Average Silhouette Method (for checking the quality of clusters)
factoextra::fviz_nbclust(tracts_per_req[ ,2:41], kmeans, method = "silhouette")+
                  theme_bw()+
                  theme(plot.title = element_text(hjust = 0.5))+
                  ggsave("asm_plot.png",dpi = 600)

# a non-supervise clustering using k-means
kmc <- stats::kmeans(tracts_per_req[ ,2:41], centers = 4, nstart= 25)

#factoextra::fviz_cluster(kmc, data= tracts_per_req[ ,2:41]) #this gives you a nightmare, keep it commented

#put the k-means cluster group after the tract column
tracts_per_req <- tibble::add_column(tracts_per_req, `Cluster Group`= kmc$cluster, .after = "gainsville_df.Tract")

#make a new column in main frame, match the tract from main tracts to the tracts of cluster group and assign group
gainsville_df$`Cluster Group` <- tracts_per_req$`Cluster Group`[match(gainsville_df$Tract, tracts_per_req$gainsville_df.Tract)]



#--------------------------------------------------------------- Map with K-Means ---------------------------------------------------------------------

#convert cluster group column to a factor
gainsville_df[c("Cluster Group")] <- lapply(gainsville_df[c("Cluster Group")], as.factor)

# Cluster graph with Request types and Assinged To:

MapPalette <- leaflet::colorQuantile(palette = "RdYlBu", domain = alachua_population$estimate, n= 10, reverse = F) 
pal <- leaflet::colorFactor(palette = colorRampPalette(c("orangered4", "lightpink3","firebrick1","royalblue4"))(length(gainsville_df$`Cluster Group`)),
                            domain = gainsville_df$`Cluster Group`)

#plot the county with tidycensus, and add markers
alachua_draft_plot_cluster <- alachua_population %>%
                          st_transform(crs= "+init=epsg:4326") %>%
                          leaflet() %>%
                          addProviderTiles(provider = "Wikimedia") %>%
                          addFullscreenControl() %>%
                          addPolygons(popup = ~str_extract(NAME, "^([^,]*)"),
                                      stroke= F,
                                      smoothFactor = 0,
                                      fillOpacity = 0.7,
                                      color= ~MapPalette(estimate)) %>%
                          addPolygons(data= gnv_poly,
                                      color= "black",
                                      weight= 2) %>%
                          addLegend("bottomright",
                                    pal= MapPalette,
                                    values= ~estimate,
                                    title= "Population Density by Tract (%)",
                                    opacity = 1) %>%
                          addCircleMarkers(data= gainsville_df,
                                           lat= ~Latitude,
                                           lng= ~Longitude,
                                           popup = gainsville_df$`Request Type`,
                                           weight = 1,
                                           radius = 0.6,
                                           #opacity= 0.5,
                                           color= ~pal(`Cluster Group`)) %>% 
                          addLegend("topright", 
                                    pal = pal, values = gainsville_df$`Cluster Group`, 
                                    title = "Cluster Group")
alachua_draft_plot_cluster
htmlwidgets::saveWidget(alachua_draft_plot_cluster, "dynamic_gainsville_clustered_issuetype.html")

#####################################################
ggplot() + 
  geom_sf(data = gainsville_df,aes(geometry= Geomtry,fill= as.numeric(gainsville_df$`Cluster Group`)), alpha= 0.2) +
  coord_sf(crs = "+init=epsg:4326")+ #crs = 26911
  geom_sf(data= gnv_poly, alpha= 0.1)+
  scale_fill_viridis(discrete = F, option= "plasma")+
  theme_bw()


#---------------------------------------------------------- K-means centrality bar plot ------------------------------------------------------------------

#grouped bar graph of clusters
ggplot2::ggplot(data= gainsville_df %>%
                  group_by(`Request Type`, `Cluster Group`) %>%
                  summarise(num_calls = length(`Request Type`))
                , aes(x= `Cluster Group`, y= num_calls, fill= `Request Type`))+
            geom_bar(position = "dodge",stat = "identity", colour="black")+
            theme_bw()+
            theme(plot.title = element_text(hjust = 0.5), legend.position = "top", legend.direction = "horizontal", legend.spacing.x = unit(1.0, 'cm'))+
            guides(fill = guide_legend(ncol = 5))+
            labs(x= "Cluster Groups", y="No. of Requested Services")+
            ggtitle("Service Requests per Cluster")+
            scale_fill_viridis(option = "viridis",discrete = T)+
            ggsave("311_cluster_barPlot.png",dpi = 600, height = 8.00, width = 18.1)

#----------------------------------------------------------- Polar plots for extra census variables ----------------------------------------

#readr::write_csv(variable,"all_variables.csv")



#----------------------------------------------------------- Descriptive Statistics --------------------------------------------------------

#top 10 request types and their percentage portion (Pie chart would be better and capture all the data)
top_10_req <- gainsville_df %>%
                dplyr::group_by(`Request Type`) %>%
                dplyr::summarise(Total= n()) %>%
                dplyr::arrange(desc(Total)) %>%
                dplyr::mutate(Percentage= round(Total*100/sum(Total), digits = 2))%>%
                dplyr::top_n(n= 10, wt= `Request Type`)

#change the column names
colnames(top_10_req) <- c("Request", "Total", "Percentage")

#make a flex table for top 10 request types and their percentage portion
ft_top_10_req <- flextable::flextable(top_10_req) %>%
                  #flextable::theme_box()%>%
                  flextable::autofit()%>%
                  flextable::save_as_html(path = "ft_top_10_req.html")


#top 10 category types and their percentage portion (Pie chart would be better and capture all the data)
top_10_cat <- gainsville_df %>%
                  dplyr::group_by(`Request Type`, `Assigned To:`) %>%
                  dplyr::summarise(Total= n()) %>%
                  dplyr::arrange(desc(Total)) %>%
                  dplyr::ungroup() %>%
                  dplyr::mutate(Percentage= round(Total*100/sum(Total), digits = 2))%>%
                  dplyr::top_n(n= 10, wt= `Request Type`)

#change the column names
colnames(top_10_cat) <- c("Request", "Branch", "Total", "Percentage")

#make a flex table for top 10 request types and their percentage portion
ft_top_10_cat <- flextable::flextable(top_10_cat) %>%
                    #flextable::theme_box()%>%
                    flextable::autofit()%>%
                    flextable::save_as_html(path = "ft_top_10_cat.html")


# number of requests by categories per year
req_by_cat_per_yr <-gainsville_df %>%
                    dplyr::group_by(
                            lubridate::year(`Service Request Date`),
                            `Assigned To:`,
                            `Request Type`) %>%
                    dplyr::summarise(Total= n()) %>% 
                    dplyr::arrange(`lubridate::year(\`Service Request Date\`)`) #Very important to put \ {escape chars.}

#Change the hideous column names
colnames(req_by_cat_per_yr) <- c("Year", "Branch", "Request", "Total")

#make a flex table for requests by categories per year
ft_req_by_cat_per_yr <- flextable::flextable(req_by_cat_per_yr) %>%
                          flextable::merge_v(j= ~Year+Branch) %>%
                          flextable::add_footer_lines(values= c(colSums(req_by_cat_per_yr[,"Total"], na.rm = F))) %>%
                          flextable::theme_box()%>%
                          flextable::autofit() %>%
                          flextable::save_as_html(path = "ft_req_by_cat_per_yr.html")

###Point rest here!
