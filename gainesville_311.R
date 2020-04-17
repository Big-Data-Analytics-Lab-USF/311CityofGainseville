set.seed(786)
options(warn = -1, scipen = 999, tigris_use_cache = T, tigris_class = "sf")
#install.packages("units", dependencies = TRUE, INSTALL_opts = '--no-lock')
library(acs)
library(broom)
library(choroplethr)
library(cluster)
library(censusr)
library(censusapi)
library(dplyr)
library(devtools)
#devtools::install_github("timelyportfolio/d3radarR")
library(d3radarR)
library(extrafont)
library(flextable)
library(forcats)
library(factoextra)
library(ggplot2)
#devtools::install_github("ricardo-bion/ggradar", dependencies = TRUE)
library(ggradar)
library(gridExtra)
library(GGally)
library(ggforce)
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
library(purrrlyr)
library(sp)
library(sf)
library(scales)
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
          ggsave("outliers_mapped_BEFOREcleaning.png", height = 8.0, width = 8.0, units = "in")
  

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
          ggsave("outliers_mapped_AFTERcleaning.png", height = 8.0, width = 8.0, units = "in")


#------------------------------------------ Remove outliers in main frame ----------------------------------------

#Nullify the column because we're going to add with people of gainesville only.
gainsville_df[ ,c('Latitude', 'Longitude')] <- list(NULL)

match_these_df <- outliers_after[, 1:3]
sf::st_geometry(match_these_df) <- NULL

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

# Varities of factors
gainsville_df[c("Reporter Display","Request Type","Assigned To:")] <- lapply(
                                                                      gainsville_df[
                                                                      c("Reporter Display",
                                                                        "Request Type",
                                                                        "Assigned To:")] ,
                                                                      as.factor)

# lapply(gainsville_df, class) #look through class of columns
# as.data.frame(colnames(gainsville_df)) #look through column indices

####---SKIP

#------------------------------------------ Assigned to Category------------------------------------------------------
#Category searched from  Gainsville 311 site

# category_names_df <- data.frame(table(as.factor(gainsville_df$`Assigned To:`)))
# colnames(category_names_df) <- c("Branch", "Frequencies")

#------------------------------------------ Request Type Category ------------------------------------------------------

#levels(gainsville_df$`Request Type`)
# issue_type_df <- data.frame(table(as.factor(gainsville_df$`Request Type`)))
# colnames(issue_type_df) <- c("Issue Category", "Frequencies")
# 
# issue_type_df <- dplyr::filter(issue_type_df, Frequencies > median(issue_type_df$Frequencies)) #Cutoffs

# NEED THIS IN CASE DR. Loni NEEDS CALLS
## dat <- new_df %>%
##  group_by(Request.Type) %>%
##  summarise(no_rows = length(Request.Type))

#------------------------------------------ Case Owners Category ------------------------------------------------------

#levels(gainsville_df$`Reporter Display`)
# case_owners_df <- data.frame(table(as.factor(gainsville_df$`Reporter Display`)))
# colnames(case_owners_df) <- c("Case Owner", "Frequencies")
# 
# case_owners_df <- dplyr::filter(case_owners_df, Frequencies > median(case_owners_df$Frequencies))


####---SKIP_END

#------------------------------------------------------ Truly Distinct colors-------------------------------------------------

getDistinctColors <- function(n) {
  qual_col_pals <- RColorBrewer::brewer.pal.info[RColorBrewer::brewer.pal.info$category == 'qual', ]
  col_vector <- unique(unlist(mapply(RColorBrewer::brewer.pal, qual_col_pals$maxcolors, rownames(qual_col_pals))))
  stopifnot (n <= length(col_vector))
  xxx <- grDevices::col2rgb(col_vector)
  dist_mat <- as.matrix(dist(t(xxx)))
  diag(dist_mat) <- 1e10
  while (length(col_vector) > n) {
    minv <- apply (dist_mat,1,function(x)min(x))
    idx <- which(minv==min(minv))[1]
    dist_mat <- dist_mat[-idx, -idx]
    col_vector <- col_vector[-idx]
  }
  return(col_vector)
}

#-------------------------------------------- Pre-plots for Analysis ----------------------------------------------------------------

#Service requests from Sep 2014 - Jan 2020

# category based scatter plot
ggplot2::ggplot(data= gainsville_df %>%
                        group_by(lubridate::date(`Service Request Date`), `Assigned To:`) %>%
                        count(),
                aes(`lubridate::date(\`Service Request Date\`)`, log(n, base=10), alpha= log(n, base= 10)))+ 
          theme_bw()+
          guides(color = guide_legend(override.aes = list(alpha = 1, size= 1.8)))+
          geom_point(aes(color = `Assigned To:`)) +
          geom_smooth(method='loess') +          
          xlim(as.Date("2014-01-01", format= "%Y-%m-%d"), as.Date("2020-01-01", format= "%Y-%m-%d"))+
          ylim(0.0, 2.5)+
          scale_alpha(guide = 'none')+
          theme(plot.title = element_text(hjust = 0.5), legend.position = "top")+
          scale_color_manual(values = c("indianred4", "greenyellow", "royalblue4", "gold"))+
          labs(x="Days",y= "Service Requests (log10)")+
          ggtitle("311 Service Requests by Days per Categories")+
          ggsave("analysis_service_requests_by_days.png",dpi = 600, height = 5.00, width = 8.0)


#Branch Work load from Sep 2014 - Jan 2020, thickness is frequency, diamond is the mean frequency for checking the box plot average

#Violin plot
ggplot2::ggplot(data = gainsville_df, aes(`Assigned To:`, lubridate::date(`Service Request Date`), fill= `Assigned To:`, alpha= 0.5))+
          geom_violin(trim = F)+ 
          stat_summary(fun.y=mean, geom="point", shape=18, size=4)+
          ylim(as.Date("2014-01-01", format= "%Y-%m-%d"), as.Date("2020-01-01", format= "%Y-%m-%d"))+
          scale_fill_viridis(discrete = T)+
          theme_bw()+
          theme(plot.title = element_text(hjust = 0.5), legend.position = "none")+
          labs(x="311 Gainsville Branch",y= "Months", caption = "Note: Width represents call volume with Diamond being the Mean")+
          ggtitle("Branch Work Load Comparison by Month")+
          coord_flip()+
          ggsave("analysis_branch_work_load_comparison.png",dpi = 600, height = 5.00, width = 6.0)

#Request types from Sep 2014 - Jan 2020

#Bar chart  (Make this into a bubble graph)
ggplot2::ggplot(data= gainsville_df %>%
                  group_by(`Request Type`) %>%
                  summarise(num_calls = length(`Request Type`))
                , aes(x= `Request Type`, y= num_calls, fill= `Request Type`))+
         geom_bar(stat = "identity", alpha= 0.9)+
         theme_bw()+
         theme(plot.title = element_text(hjust = 0.5), legend.position = "none",axis.text.x = element_text(angle = 90, vjust=0.5))+
         labs(x="Requested Services",y= "No. of Calls")+
         ggtitle("Number of Calls per Requested Service")+
         scale_fill_manual(values = getDistinctColors(length(unique(gainsville_df$`Request Type`))))+
         ggsave("analysis_service_calls_by_requests.png",dpi = 600, height = 5.00, width = 10)


# Year groups, calls per issues by group

#faceted by year, bar charts
ggplot2::ggplot(data= gainsville_df %>%
                  subset(lubridate::year(`Service Request Date`) != 2014) %>%
                  group_by(`Request Type`, `Assigned To:`, `Service Request Date`) %>%
                  summarise(num_calls = length(`Request Type`))
                , aes(x= `Request Type`, y= num_calls, fill= `Assigned To:`))+
          geom_bar(stat = "identity", alpha= 0.9)+
          theme_bw()+
          theme(plot.title = element_text(hjust = 0.5), legend.position = "top",axis.text.x = element_text(angle = 90, vjust=0.5, hjust = 0.5))+
          labs(x="Requested Services",y= "No. of Calls", fill= "Assinged To:", caption= "*Years with less than one requested service have been removed!")+
          ggtitle("Number of Calls per Requested Service by year")+
          facet_wrap(~lubridate::year(`Service Request Date`))+
          scale_fill_manual(values = getDistinctColors(length(unique(gainsville_df$`Assigned To:`))))+
          ggsave("analysis_number_calls_per_requests_per_yr.png",dpi = 600, height = 9.00, width = 18)


#----------------------------------------------------- fetching the census stuff ------------------------------------------------

#tidycensus::census_api_key("21adc0b3d6e900378af9b7910d04110cdd38cd75", install = T, overwrite = T)

#census <- tidycensus::load_variables(2010, "sf1", cache = T) #Summary of all census
variable <- tidycensus::load_variables(2018, "acs5", cache = T) #get the variable from ACS: American Community Survey

#Get the alachua county census data GEOID- STATE+COUNTY+TRACT- 2+3+6
#variables placement:
#Total Population > Median Income (past 12 months) > Per-Capita (past 12 months) > Below 100% poverty level > White (by birth) >
#Hispanic or latino (by birth) > Black (by birth) > Other races (By birth) > High School Graduate (US) > Bachelors (US) >
#Graduate or Professional Degree (US) > US Citizen (by birth) > US Citizen (Naturalized) > Not a US Citizen
#Unemployed Vets (18-34 yrs) > Unemployed Non-Vets (18-34 yrs) > Employed Vets (18-34 yrs) > Employed Non-Vets (18-34 yrs) > 
#Vets Labor Force (18-34 yrs) > Non-vets labor force (18-34 yrs) > Enrolled in Undergraduates > Enrolled in 9-12 > Enrolled in Graduate

alachua <- tidycensus::get_acs(state = "FL", county = "Alachua", 
                               geography = "tract", geometry = T,
                               variables = c("B01003_001", "B06011_001", "B19301_001", "B06012_002", "B06004A_001",
                                             "B06004I_001", "B06004B_001", "B06004F_001", "B06009_003", "B06009_005",
                                             "B06009_006", "B05001_002", "B05001_005", "B05001_006",
                                             "B21005_006", "B21005_011", "B21005_005", "B21005_010", 
                                             "B21005_004", "B21005_009", "B14007_017", "B14001_007", "B14007_018"),
                               year = 2018)


#!!CAUTION BEFORE YOU ADD A NEW VARIABLE, MAKE SURE TO DOUBLE CHECK THE INDEX PLACEMENTS BECAUSE SPLIT FUNCTION SORTS THE TABLE TOO!!
#Now split the dataframe into list of unique variables 
alachua <- alachua %>% dplyr::group_split(variable)

####---SKIP

#Because estimate is the common number among each variable, it will be tricky and unreadble to write a complex code**
#Follow the variable placemement label for naming new dataframe

# alachua_population <- alachua[[1]]
# alachua_us_citizens <- alachua[[2]]
# alachua_natu_us_citizens <- alachua[[3]]
# alachua_not_us_citizens <- alachua[[4]]
# alachua_white_pop <- alachua[[5]]
# alachua_black_pop <- alachua[[6]]
# alachua_others_pop <- alachua[[7]]
# alachua_latino_pop <- alachua[[8]]
# alachua_high_grads <- alachua[[9]]
# alachua_batch_grads <- alachua[[10]]
# alachua_grads_post_grads <- alachua[[11]]
# alachua_median_income <- alachua[[12]]
# alachua_poverty_level <- alachua[[13]]
# alachua_high_enrollment <- alachua[[14]]
# alachua_batch_enrollment <- alachua[[15]]
# alachua_grads_post_enrollment <- alachua[[16]]
# alachua_per_capita <- alachua[[17]]
# alachua_labor_vets <- alachua[[18]]
# alachua_employed_vets <- alachua[[19]]
# alachua_unemployed_vets <- alachua[[20]]
# alachua_labor_non_vets <- alachua[[21]]
# alachua_employed_non_vets <- alachua[[22]]
# alachua_unemployed_non_vets <- alachua[[23]]

####---SKIP_END

#--------------------- Remove the outer spanning tracts manually (update, st_intersection doesn't work in our case) -----------------------------

remove_these_tracts <- c("12001110800", "12001002220", "12001002219", "12001002217", "12001001908", "12001001813", "12001001811",
                         "12001001802", "12001002204", "12001002101", "12001001702", "12001001701", "12001001400", "12001000700",
                         "12001001814", "12001001805", "12001001806", "12001001801")

#too lazy to apply a loop for new variables creation
alachua_population <- alachua[[1]]
alachua_population <- alachua_population[ ! alachua_population$GEOID %in% remove_these_tracts, ]

alachua_us_citizens <- alachua[[2]]
alachua_us_citizens <- alachua_us_citizens[ ! alachua_us_citizens$GEOID %in% remove_these_tracts, ]

alachua_natu_us_citizens <- alachua[[3]]
alachua_natu_us_citizens <- alachua_natu_us_citizens[ ! alachua_natu_us_citizens$GEOID %in% remove_these_tracts, ]

alachua_not_us_citizens <- alachua[[4]]
alachua_not_us_citizens <- alachua_not_us_citizens[ ! alachua_not_us_citizens$GEOID %in% remove_these_tracts, ]

alachua_white_pop <- alachua[[5]]
alachua_white_pop <- alachua_white_pop[ ! alachua_white_pop$GEOID %in% remove_these_tracts, ]

alachua_black_pop <- alachua[[6]]
alachua_black_pop <- alachua_black_pop[ ! alachua_black_pop$GEOID %in% remove_these_tracts, ]

alachua_others_pop <- alachua[[7]]
alachua_others_pop <- alachua_others_pop[ ! alachua_others_pop$GEOID %in% remove_these_tracts, ]

alachua_latino_pop <- alachua[[8]]
alachua_latino_pop <- alachua_latino_pop[ ! alachua_latino_pop$GEOID %in% remove_these_tracts, ]

alachua_high_grads <- alachua[[9]]
alachua_high_grads <- alachua_high_grads[ ! alachua_high_grads$GEOID %in% remove_these_tracts, ]

alachua_batch_grads <- alachua[[10]]
alachua_batch_grads <- alachua_batch_grads[ ! alachua_batch_grads$GEOID %in% remove_these_tracts, ]

alachua_grads_post_grads <- alachua[[11]]
alachua_grads_post_grads <- alachua_grads_post_grads[ ! alachua_grads_post_grads$GEOID %in% remove_these_tracts, ]

alachua_median_income <- alachua[[12]]
alachua_median_income <- alachua_median_income[ ! alachua_median_income$GEOID %in% remove_these_tracts, ]

alachua_poverty_level <- alachua[[13]]
alachua_poverty_level <- alachua_poverty_level[ ! alachua_poverty_level$GEOID %in% remove_these_tracts, ]

alachua_high_enrollment <- alachua[[14]]
alachua_high_enrollment <- alachua_high_enrollment[ ! alachua_high_enrollment$GEOID %in% remove_these_tracts, ]

alachua_batch_enrollment <- alachua[[15]]
alachua_batch_enrollment <- alachua_batch_enrollment[ ! alachua_batch_enrollment$GEOID %in% remove_these_tracts, ]

alachua_grads_post_enrollment <- alachua[[16]]
alachua_grads_post_enrollment <- alachua_grads_post_enrollment[ ! alachua_grads_post_enrollment$GEOID %in% remove_these_tracts, ]

alachua_per_capita <- alachua[[17]]
alachua_per_capita <- alachua_per_capita[ ! alachua_per_capita$GEOID %in% remove_these_tracts, ]

alachua_labor_vets <- alachua[[18]]
alachua_labor_vets <- alachua_labor_vets[ ! alachua_labor_vets$GEOID %in% remove_these_tracts, ]

alachua_employed_vets <- alachua[[19]]
alachua_employed_vets <- alachua_employed_vets[ ! alachua_employed_vets$GEOID %in% remove_these_tracts, ]

alachua_unemployed_vets <- alachua[[20]]
alachua_unemployed_vets <- alachua_unemployed_vets[ ! alachua_unemployed_vets$GEOID %in% remove_these_tracts, ]

alachua_labor_non_vets <- alachua[[21]]
alachua_labor_non_vets <- alachua_labor_non_vets[ ! alachua_labor_non_vets$GEOID %in% remove_these_tracts, ]

alachua_employed_non_vets <- alachua[[22]]
alachua_employed_non_vets <- alachua_employed_non_vets[ ! alachua_employed_non_vets$GEOID %in% remove_these_tracts, ]

alachua_unemployed_non_vets <- alachua[[23]]
alachua_unemployed_non_vets <- alachua_unemployed_non_vets[ ! alachua_unemployed_non_vets$GEOID %in% remove_these_tracts, ]


#------------------------------------------------------------- Tidycensus manipulation ---------------------------------------
#CAUTION: DO NOT WRITE SHAPEFILES TO CSV, IT WILL GET CORRUPT!
coord <- readr::read_csv("latlon_to_cenCodes.csv") 

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
coord$GEOID <- substr(coord$`Census Code`, start = 1, stop = 11)

#Census code: eg. 120010011003032-The first two being the state, next three the county, and the following six the tract.

#Get the tract
coord$Tract <- substr(coord$`Census Code`, start= 6, stop= 11)

#make a copy just so we can refer back
coord_shell <- coord

#save the features per tract, match the coord & alachua variable GEOIDs, if they match get estimate, and save
coord$Population <- alachua_population$estimate[match(coord$GEOID, alachua_population$GEOID)]
coord$`Median Income` <- alachua_median_income$estimate[match(coord$GEOID, alachua_median_income$GEOID)]
coord$`Per Capita` <- alachua_per_capita$estimate[match(coord$GEOID, alachua_per_capita$GEOID)]
coord$`Under Poverty` <- alachua_poverty_level$estimate[match(coord$GEOID, alachua_poverty_level$GEOID)]
coord$Caucasians <- alachua_white_pop$estimate[match(coord$GEOID, alachua_white_pop$GEOID)]
coord$Latinos <- alachua_latino_pop$estimate[match(coord$GEOID, alachua_latino_pop$GEOID)]
coord$`African Americans` <- alachua_black_pop$estimate[match(coord$GEOID, alachua_black_pop$GEOID)]
coord$Others <- alachua_others_pop$estimate[match(coord$GEOID, alachua_others_pop$GEOID)]
coord$`High School Graduates` <- alachua_high_grads$estimate[match(coord$GEOID, alachua_high_grads$GEOID)]
coord$`Bachelors Degree`  <- alachua_batch_grads$estimate[match(coord$GEOID, alachua_batch_grads$GEOID)]
coord$`Graduate or PhD Degree` <- alachua_grads_post_grads$estimate[match(coord$GEOID, alachua_grads_post_grads$GEOID)]
coord$`US Citizens by Birth` <- alachua_us_citizens$estimate[match(coord$GEOID, alachua_us_citizens$GEOID)]
coord$`US Citizens via Naturalization` <- alachua_natu_us_citizens$estimate[match(coord$GEOID, alachua_natu_us_citizens$GEOID)]
coord$`Unemployed Veterns` <- alachua_unemployed_vets$estimate[match(coord$GEOID, alachua_unemployed_vets$GEOID)]
coord$`Unemployed Non-Veterns` <- alachua_unemployed_non_vets$estimate[match(coord$GEOID, alachua_unemployed_non_vets$GEOID)]
coord$`Employed Veterns` <- alachua_employed_vets$estimate[match(coord$GEOID, alachua_employed_vets$GEOID)]
coord$`Employed Non-Veterns` <- alachua_employed_non_vets$estimate[match(coord$GEOID, alachua_employed_non_vets$GEOID)]
coord$`Veterns Labor Force` <- alachua_labor_vets$estimate[match(coord$GEOID, alachua_labor_vets$GEOID)]
coord$`Non-Veterns Labor Force` <- alachua_labor_non_vets$estimate[match(coord$GEOID, alachua_labor_non_vets$GEOID)]
coord$`Non US Citizens` <- alachua_not_us_citizens$estimate[match(coord$GEOID, alachua_not_us_citizens$GEOID)]
coord$`High School Enrollment` <- alachua_high_enrollment$estimate[match(coord$GEOID, alachua_high_enrollment$GEOID)]
coord$`Undergraduate Enrollment` <- alachua_batch_enrollment$estimate[match(coord$GEOID, alachua_batch_enrollment$GEOID)]
coord$`Graduate or PhD Enrollment` <- alachua_grads_post_enrollment$estimate[match(coord$GEOID, alachua_grads_post_enrollment$GEOID)]

#drop NA from the tracts which we have removed earliers
coord <- coord %>% tidyr::drop_na()

#readr::write_csv(coord_copy,"COPYcontains_latlon_cenCodes_cenTract.csv")

#readr::write_csv(coord,"latlon_to_cenCodes.csv")
#coord <- readr::read_csv("latlon_to_cenCodes.csv")

####---SKIP_END

#Change the coord GEOID to chars IFNIF if you've read the coord file earlier
coord[c("GEOID")] <- lapply(coord[c("GEOID")], as.character)


#join any of the alachua geometry with coord frame, here we picked population because it is most common (OUTDATED)
# coord <- dplyr::left_join(alachua_population, coord, by = "GEOID")

#coord <- coord %>% tidyr::drop_na()

#reduce columns which we don't need (OUTDATED)
#coord %<>% dplyr::select (-c(NAME, variable, estimate, moe))

#Get the tract shapes, since shapes are consistent, it doesn't matter which list we choose. We will go with population
coord$geometry <- alachua[[1]][["geometry"]][match(coord$GEOID, alachua[[1]][["GEOID"]])]

#Merge two data frames (OUTDATED)
#gainsville_df[names(coord)] <- coord

# Updated: Merge two data frames
gainsville_df <- dplyr::semi_join(gainsville_df, coord)
gainsville_df[names(coord)] <- coord

#Change classes again for newly added columns
gainsville_df[c("Census Code","Tract", "GEOID", "Population",
                "Median Income","Per Capita","Under Poverty",
                "Caucasians", "Latinos", "African Americans","Others",
                "High School Graduates", "Bachelors Degree", "Graduate or PhD Degree",
                "US Citizens by Birth", "US Citizens via Naturalization",
                "Unemployed Veterns", "Unemployed Non-Veterns",
                "Employed Veterns", "Employed Non-Veterns",
                "Veterns Labor Force", "Non-Veterns Labor Force","Non US Citizens",
                "High School Enrollment", "Undergraduate Enrollment",
                "Graduate or PhD Enrollment")] <- lapply(gainsville_df[c("Census Code","Tract", "GEOID", "Population",
                                                                                      "Median Income","Per Capita","Under Poverty",
                                                                                      "Caucasians", "Latinos", "African Americans","Others",
                                                                                      "High School Graduates", "Bachelors Degree", "Graduate or PhD Degree",
                                                                                      "US Citizens by Birth", "US Citizens via Naturalization",
                                                                                      "Unemployed Veterns", "Unemployed Non-Veterns",
                                                                                      "Employed Veterns", "Employed Non-Veterns",
                                                                                      "Veterns Labor Force", "Non-Veterns Labor Force", "Non US Citizens",
                                                                                      "High School Enrollment", "Undergraduate Enrollment",
                                                                                      "Graduate or PhD Enrollment")] ,
                                                        as.numeric)

#tibble view to quickly see each datatypes of column
#gainsville_df %>% as_tibble()

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

#comes handy in polar plots

#Modify the alachua variables with main frames's Geo ID to project only the gainsville coordinates

alachua_population <- alachua_population %>% dplyr::filter(GEOID %in% unique(gainsville_df$GEOID))
alachua_median_income <- alachua_median_income %>% dplyr::filter(GEOID %in% unique(gainsville_df$GEOID))
alachua_per_capita <- alachua_per_capita %>% dplyr::filter(GEOID %in% unique(gainsville_df$GEOID))
alachua_poverty_level <- alachua_poverty_level %>% dplyr::filter(GEOID %in% unique(gainsville_df$GEOID))
alachua_white_pop <- alachua_white_pop %>% dplyr::filter(GEOID %in% unique(gainsville_df$GEOID))
alachua_latino_pop <- alachua_latino_pop %>% dplyr::filter(GEOID %in% unique(gainsville_df$GEOID))
alachua_black_pop <- alachua_black_pop %>% dplyr::filter(GEOID %in% unique(gainsville_df$GEOID))
alachua_others_pop <- alachua_others_pop %>% dplyr::filter(GEOID %in% unique(gainsville_df$GEOID))
alachua_high_grads <- alachua_high_grads %>% dplyr::filter(GEOID %in% unique(gainsville_df$GEOID))
alachua_batch_grads <- alachua_batch_grads %>% dplyr::filter(GEOID %in% unique(gainsville_df$GEOID))
alachua_grads_post_grads <- alachua_grads_post_grads %>% dplyr::filter(GEOID %in% unique(gainsville_df$GEOID))
alachua_us_citizens <- alachua_us_citizens %>% dplyr::filter(GEOID %in% unique(gainsville_df$GEOID))
alachua_natu_us_citizens <- alachua_white_pop %>% dplyr::filter(GEOID %in% unique(gainsville_df$GEOID))
alachua_unemployed_vets <- alachua_unemployed_vets %>% dplyr::filter(GEOID %in% unique(gainsville_df$GEOID))
alachua_unemployed_non_vets <- alachua_unemployed_non_vets %>% dplyr::filter(GEOID %in% unique(gainsville_df$GEOID))
alachua_employed_vets <- alachua_employed_vets %>% dplyr::filter(GEOID %in% unique(gainsville_df$GEOID))
alachua_employed_non_vets <- alachua_employed_non_vets %>% dplyr::filter(GEOID %in% unique(gainsville_df$GEOID))
alachua_labor_vets <- alachua_labor_vets %>% dplyr::filter(GEOID %in% unique(gainsville_df$GEOID))
alachua_labor_non_vets <- alachua_labor_non_vets %>% dplyr::filter(GEOID %in% unique(gainsville_df$GEOID))
alachua_not_us_citizens <- alachua_not_us_citizens %>% dplyr::filter(GEOID %in% unique(gainsville_df$GEOID))
alachua_high_enrollment <- alachua_high_enrollment %>% dplyr::filter(GEOID %in% unique(gainsville_df$GEOID))
alachua_batch_enrollment <- alachua_batch_enrollment %>% dplyr::filter(GEOID %in% unique(gainsville_df$GEOID))
alachua_grads_post_enrollment <- alachua_grads_post_enrollment %>% dplyr::filter(GEOID %in% unique(gainsville_df$GEOID))
#--------------------------------------------------------------- Map -----------------------------------------------------------------------------

# Apply the color ranks based on the population of gainsville (used to be whole alachua)

#Change this when you have K-means cluster

# Interactive graph
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

htmlwidgets::saveWidget(alachua_draft_plot, "dynamic_geoLocation_reqServices_map.html")

#Clean the gainsville_df so only gainsville data remains (I forgot why I had this, Current tag: OUTDATED)
#gainsville_df %<>% tidyr::drop_na("Census Code")

#Calculate the geographic distance between two (sets of) points on the WGS ellipsoid, need it for a TRUE scale in map in km (NOT YET IMPLEMENTED)
pointDistance_frame <- raster::pointDistance(gainsville_df[, c("Longitude", "Latitude")], lonlat=TRUE)
true_scale <- pointDistance_frame[nrow(pointDistance_frame), 1]

#Static Graph (ignore the warning that layer already exist. geom_sf is buggy)
ggplot2::ggplot() + 
            geom_sf(data = gainsville_df, aes(geometry= geometry), alpha= 0.2) +
            coord_sf(crs = "+init=epsg:4326")+
            geom_sf(data= gnv_poly, alpha= 0.1)+
            geom_point(data = gainsville_df,aes(x= Longitude, y= Latitude, color= `Assigned To:`), alpha= 0.4, size= 1.0)+
            #stat_density_2d(data = gainsville_df, aes(x= Longitude, y= Latitude, fill = `Assigned To:`),alpha= 0.5, geom = "polygon")+
            scale_color_brewer(palette = "Set1")+
            theme_bw()+
            labs(title = "Geo-Location Distribution of Requested Services per Tract*", 
                 caption = "*Tracts spanning outside of Gainesville boundary have been removed.")+
            theme(plot.title = element_text(hjust = 0.5), legend.position = "top", 
                  plot.caption= element_text(face="italic"))+
            guides(color = guide_legend(override.aes = list(alpha = 1, size= 1.8)))+
            ggsave("static_geoLocation_reqServices_map.png",dpi = 100, height = 10, width = 10, units = "in")
  
#--------------------------------------------------- K-means clustering ---------------------------------------------------------

req_type_tract_df <- data.frame(gainsville_df$`Request Type`, gainsville_df$Tract)
assign_to_tract_df <- data.frame(gainsville_df$`Assigned To:`, gainsville_df$Tract)

#Make the frequency chart per tract for REQUEST TYPES
freq_chart_summarise <- req_type_tract_df %>% 
                            dplyr::group_by(gainsville_df..Request.Type.,gainsville_df.Tract) %>% 
                            dplyr::summarise(n=n())

# Cast the chart (we melt the data per Tract meaning sum every entry requests tract)
tracts_per_req <- reshape2::dcast(freq_chart_summarise, gainsville_df.Tract~gainsville_df..Request.Type., value.var = "n", fill = 0)

#Get the columns totals
totals_tracts_per_req <- tracts_per_req[,-1] %>%
                                janitor::adorn_totals("col")

#Paste the total to request type chart
tracts_per_req$Total <- totals_tracts_per_req$Total

#add the population column by taking "Population" column as a list, matching the tracts, and putting the matched pop. to short dataframe
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
readr::write_csv(tracts_per_req,"normalized_serviceRequests_tracts.csv")

#View(tracts_per_req)

#Make the frequency chart per tract for ASSIGNED TO
freq_chart_summarise <- assign_to_tract_df %>%
                          group_by(gainsville_df..Assigned.To..,gainsville_df.Tract) %>% 
                          summarise(n=n())

# Cast the chart again
tracts_per_assigned <- reshape2::dcast(freq_chart_summarise, gainsville_df.Tract~gainsville_df..Assigned.To.., value.var = "n", fill = 0)

#Get the columns totals
totals_tracts_per_assigned <- tracts_per_assigned[,-1] %>%
                                    janitor::adorn_totals("col")

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
                                   .vars = 2:5, 
                                   normalizeFunc_tracts_per_assigned)

#write to the file
readr::write_csv(tracts_per_assigned,"normalized_assignedTo_tracts.csv")

#View(tracts_per_assigned)

# ~~~incase of removal is needed~~~
# tracts_per_assigned <- tracts_per_assigned[tracts_per_assigned$Total > 1, ]
# tracts_per_req <- tracts_per_req[tracts_per_req$Total > 1, ]

tracts_per_req[c("gainsville_df.Tract")] <- lapply(tracts_per_req[c("gainsville_df.Tract")], as.numeric)

#Within-cluster sum of square to get a good cluster
factoextra::fviz_nbclust(tracts_per_req[ ,2:41], kmeans, method = "wss")+
                  theme_bw()+
                  theme(plot.title = element_text(hjust = 0.5))+
                  ggsave("kmeans_elbow_plot.png",dpi = 600)

#Average Silhouette Method (for checking the quality of clusters)
factoextra::fviz_nbclust(tracts_per_req[ ,2:41], kmeans, method = "silhouette")+
                  theme_bw()+
                  theme(plot.title = element_text(hjust = 0.5))+
                  ggsave("kmeans_averageSilhouette_plot.png",dpi = 600)

# a non-supervise clustering using k-means
kmc <- stats::kmeans(tracts_per_req[ ,2:41], centers = 6, nstart= 25) 

#factoextra::fviz_cluster(kmc, data= tracts_per_req[ ,2:41]) #this gives you a nightmare, keep it commented

#put the k-means cluster group after the tract column
tracts_per_req <- tibble::add_column(tracts_per_req, ClusterGroup= kmc$cluster, .after = "gainsville_df.Tract")

#make a new column in main frame, match the tract from main tracts to the tracts of cluster group and assign group
gainsville_df$ClusterGroup <- tracts_per_req$ClusterGroup[match(gainsville_df$Tract, tracts_per_req$gainsville_df.Tract)]

#--------------------------------------------------------------- Map with K-Means ---------------------------------------------------------------------

#Copy the default (Population) tidycensus data and call it kmc because we will be adding tracts on it
# we have to do this to prevent issues with layers, which are causing the ClusterGroup to become continous
alachua_kmc <- alachua_population

#add cluster association for every GEOID
alachua_kmc$ClusterGroup <- gainsville_df$ClusterGroup[match(alachua_kmc$GEOID ,gainsville_df$GEOID)] 


# Cluster graph with Request types and Assinged To:

pal <- leaflet::colorFactor(palette = colorRampPalette(c("turquoise", "mediumseagreen", "salmon3","gold", "darkmagenta","lightslateblue"))(length(alachua_kmc$ClusterGroup)),
                            domain = alachua_kmc$ClusterGroup)

#plot the county with tidycensus, and add markers
alachua_draft_plot_cluster <- alachua_kmc %>%
                              st_transform(crs= "+init=epsg:4326") %>%
                              leaflet() %>%
                              addProviderTiles(provider = "Wikimedia") %>%
                              addFullscreenControl() %>%
                              addPolygons(data= gnv_poly,
                                          color= "black",
                                          weight= 2) %>%
                              addLegend("topright", 
                                        pal = pal, values = gainsville_df$ClusterGroup, 
                                        title = "Cluster Group") %>%
                              addPolygons(popup = ~str_extract(NAME, "^([^,]*)"),
                                          stroke= F,
                                          smoothFactor = 0,
                                          fillOpacity = 0.7,
                                          color= ~pal(ClusterGroup))
                                

htmlwidgets::saveWidget(alachua_draft_plot_cluster, "dynamic_kmeans_map.html")


#Static Graph of K clusters (again ignore the warning, CHANGE COLOR MANUALS based on k-means selection)
ggplot2::ggplot() + 
        geom_sf(data = subset(alachua_kmc, !is.na(ClusterGroup)), aes(fill = factor(ClusterGroup)), alpha= 0.7) +
        #coord_sf(crs = "+init=epsg:4326")+
        geom_sf(data= gnv_poly, alpha= 0.1)+
        #stat_density_2d(data = gainsville_df, aes(x= Longitude, y= Latitude, fill = `Assigned To:`),alpha= 0.5, geom = "polygon")+
        theme_bw()+
        scale_fill_manual(values = c("palevioletred4", "mediumseagreen", "salmon3","gold", "darkmagenta","lightslateblue"), name="Cluster Group:")+
        labs(title = "K-means Cluster Distribution per Tract*", 
             caption = "*Tracts spanning outside of Gainesville boundary have been removed.")+
        theme(plot.title = element_text(hjust = 0.5),  
              plot.caption= element_text(face="italic"),
              legend.position = "top")+
        ggsave("static_kmeans_map.png",dpi = 100, height = 10, width = 10, units = "in")
 
#---------------------------------------------------------- K-means centrality bar plot ------------------------------------------------------------------

#grouped bar graph of clusters
ggplot2::ggplot(data= gainsville_df %>%
                  group_by(`Request Type`, ClusterGroup) %>%
                  summarise(num_calls = length(`Request Type`))%>%
                  mutate(percentage= num_calls/ sum(num_calls))
                , aes(x= ClusterGroup, y= round(percentage, 2), fill= `Request Type`))+
            geom_bar(position = "dodge",stat = "identity", colour="black", alpha= 0.7)+
            theme_bw()+
            theme(plot.title = element_text(hjust = 0.5), legend.position = "top", legend.direction = "horizontal", legend.spacing.x = unit(1.0, 'cm'))+
            guides(fill = guide_legend(ncol = 5))+
            labs(x= "Cluster Groups", y="Requested Services")+
            ggtitle("Service Requests per Cluster")+
            scale_x_discrete(limits=c("1","2","3","4", "5", "6"))+
            scale_y_continuous(labels = scales::percent_format(accuracy = 1))+
            scale_fill_manual(values= getDistinctColors(length(unique(gainsville_df$`Request Type`))))+
            ggsave("kmeans_cluster_barPlot.png",dpi = 600, height = 8.00, width = 18.1, units = "in")


#----------------------------------------------------------- Descriptive Statistics --------------------------------------------------------

#extract the core statistics; doing this so we can write portion of the data to file
gnv_social_stats <- gainsville_df[ ,c("Tract", "Assigned To:", "Request Type", "ClusterGroup", "Population",
                                     "Median Income","Per Capita","Under Poverty",
                                     "Caucasians", "Latinos", "African Americans","Others",
                                     "High School Graduates", "Bachelors Degree", "Graduate or PhD Degree",
                                     "US Citizens by Birth", "US Citizens via Naturalization", "Non US Citizens",
                                     "Unemployed Veterns", "Unemployed Non-Veterns",
                                     "Employed Veterns", "Employed Non-Veterns",
                                     "Veterns Labor Force", "Non-Veterns Labor Force",
                                     "High School Enrollment", "Undergraduate Enrollment",
                                     "Graduate or PhD Enrollment") ] 

#write to file
readr::write_csv(gnv_social_stats,"gnv_social_stats.csv")


#read to file
#gnv_social_stats <- readr::read_csv("gnv_social_stats.csv")

#top 10 category types and their percentage portion
top_10_cat <- gainsville_df %>%
                  dplyr::group_by(`Request Type`, `Assigned To:`) %>%
                  dplyr::summarise(Total= n()) %>%
                  dplyr::arrange(desc(Total)) %>%
                  dplyr::ungroup() %>%
                  dplyr::mutate(Percentage= round(Total*100/sum(Total), digits = 2)) %>%
                  dplyr::top_n(n= 10, wt= `Request Type`)

#change the column names
colnames(top_10_cat) <- c("Request", "Branch", "Total", "Percentage")

#make a flex table for top 10 request types and their percentage portion
ft_top_10_cat <- flextable::flextable(top_10_cat) %>%
                    #flextable::theme_box()%>%
                    flextable::autofit()%>%
                    flextable::save_as_html(path = "descriptiveStats_top_10_categories.html")


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
                          flextable::save_as_html(path = "descriptiveStats_requests_by_categories_per_year.html")

#----------------------------------------------------------- % Polar coordinates ---------------------------------------------------------

#read to file
#gnv_social_stats <- readr::read_csv("gnv_social_stats.csv")

#change the Cluster Group to factor
gnv_social_stats[c("ClusterGroup")] <- lapply(gnv_social_stats[c("ClusterGroup")], as.factor)

#reduce data to work with polar plots, here we keep the rows with unique Tracts because much of the rows are there due to "Request Types"
true_gnv_social_stats <- gnv_social_stats %>% 
                            dplyr::distinct(Tract, .keep_all = TRUE) %>% 
                            dplyr::select (-c(`Assigned To:`, `Request Type`))

#make a new dataframe only consisting of percentage of Races and Non US Cititzens
percent_gnv_social_stats <- true_gnv_social_stats[ , 
                                                   c("Caucasians", "Latinos", "African Americans", "Others", "Non US Citizens")
                                                   ]*100/true_gnv_social_stats[["Population"]]

#add tracts in the first column
percent_gnv_social_stats <- tibble::add_column(percent_gnv_social_stats, Tract = true_gnv_social_stats$Tract, .before = "Caucasians")

#add clusters after the first column
percent_gnv_social_stats <- tibble::add_column(percent_gnv_social_stats, Cluster = true_gnv_social_stats$ClusterGroup, .after = "Tract")

#get US citizens density in percentage
percent_gnv_social_stats$`US Citizens` <- (true_gnv_social_stats[ , c("US Citizens by Birth")]+
                                             true_gnv_social_stats[ , c("US Citizens via Naturalization")]
                                           )*100/true_gnv_social_stats[["Population"]]

#get unemployment rate
percent_gnv_social_stats$`Unemplyment Rate` <- (true_gnv_social_stats[ , c("Unemployed Veterns")]+
                                                true_gnv_social_stats[ , c("Unemployed Non-Veterns")]
                                                )*100/(true_gnv_social_stats[["Veterns Labor Force"]]+
                                                         true_gnv_social_stats[["Non-Veterns Labor Force"]]
                                                       )

#get employment rate
percent_gnv_social_stats$`Employment Rate` <- (true_gnv_social_stats[ , c("Employed Veterns")]+
                                                  true_gnv_social_stats[ , c("Employed Non-Veterns")]
                                                )*100/(true_gnv_social_stats[["Veterns Labor Force"]]+
                                                         true_gnv_social_stats[["Non-Veterns Labor Force"]]
                                                        )
#get poverty rate
percent_gnv_social_stats$`Poverty Rate` <- true_gnv_social_stats[ , c("Under Poverty")]*100/true_gnv_social_stats[["Population"]]

#get highschool graduation rate
percent_gnv_social_stats$`High School Graduates` <- true_gnv_social_stats[ , c("High School Graduates")
                                                                           ]*100/true_gnv_social_stats[["Population"]]

#get undergraduate degree rate
percent_gnv_social_stats$`Undergraduate Graduates` <- true_gnv_social_stats[ , c("Bachelors Degree")
                                                                             ]*100/true_gnv_social_stats[["Population"]]

#get graduate phd graduation rate
percent_gnv_social_stats$`Professional Graduates` <- true_gnv_social_stats[ , c("Graduate or PhD Degree")
                                                                            ]*100/true_gnv_social_stats[["Population"]]

# to get the Median income in a true "percentage" form, we have to apply an Excel equivalent =PERCENTRANK.EXC function in r
# since there is none, we can make one by looking at the info popup. Reason why RANK and no simple PERCENTILE is because we
# want to compare relative to the each clusters/tracts.
percentilerank_exclusive <<- function(x){
  rx<- rle(sort(x))
  smaller<- cumsum(c(!0, rx$lengths))[seq(length(rx$lengths))]
  larger<- rev(cumsum(c(0, rev(rx$lengths))))
  rxpr<- smaller/(smaller+larger)
  return(rxpr[match(x, rx$values)])
}

#alternate dplyr's inclusive: percent_rank(x), exclusive: percent_rank(c(-Inf, x, Inf))[-c(1, length(x) + 2)]
# because I spent some time making mine, I will go with mine, because I like to show off.

percent_gnv_social_stats$`Median Income Rate` <- percentilerank_exclusive(true_gnv_social_stats$`Median Income`)

#write the stats to file
readr::write_csv(percent_gnv_social_stats,"percent_gnv_social_stats.csv")

#read
# percent_gnv_social_stats <- readr::read_csv("percent_gnv_social_stats.csv")

#------------------------------------------------------------ Radar Charts ---------------------------------------------------------------

#Get the raw values from the table
polar_stats <- percent_gnv_social_stats[ ,2:ncol(percent_gnv_social_stats)]

#Trun the values into percentage form and reduce each clusters into single group
polar_stats %<>%dplyr::group_by(Cluster) %>%  dplyr::summarise_all(mean)

#change the column names to appropriate convention
colnames(polar_stats) <- c("Cluster", "Whites","Latinos", "Blacks","Other Race","Aliens",
                           "U.S. Citizens", "Unemployment", "Employment", "Poverty", "High School Graduates",
                           "College Graduates", "Has MSc or PhD Degree", "Median Income Quantile")

#interactive version of the polar plot. 
#This long approach was done to prevent a bug that was stopping us from hiding clusters
json_data <- jsonlite::fromJSON(
  '
  [
    {
      "key":"Cluster 1",
      "values":[
        {  "axis":"Whites", "value":0.623826773 }, {  "axis":"Latinos", "value":0.117403414 },
        {  "axis":"Blacks", "value":0.214937205 }, {  "axis":"Other Race", "value":0.009908185 },
        {  "axis":"Aliens", "value":0.095237020 }, {  "axis":"U.S. Citizens", "value":0.878745393 },
        {  "axis":"Unemployment", "value":0.098075796 }, {  "axis":"Employment", "value":0.901924204 },
        {  "axis":"Poverty", "value":0.335676655 }, {  "axis":"High School Graduates", "value":0.112008343 },
        {  "axis":"College Graduates", "value":0.113980160 }, {  "axis":"Has MSc or PhD Degree", "value":0.107165461 },
        {  "axis":"Median Income Quantile", "value":0.004530075 }
        ]
    },
    {
      "key":"Cluster 2",
      "values":[
        {  "axis":"Whites", "value":0.913330951 }, {  "axis":"Latinos", "value":0.159220872 },
        {  "axis":"Blacks", "value":0.021622588 }, {  "axis":"Other Race", "value":0.010007148 },
        {  "axis":"Aliens", "value":0.013402430 }, {  "axis":"U.S. Citizens", "value":0.979092209 },
        {  "axis":"Unemployment", "value":0.068590093 }, {  "axis":"Employment", "value":0.931409907 },
        {  "axis":"Poverty", "value":0.443352395 }, {  "axis":"High School Graduates", "value":0.043781272 },
        {  "axis":"College Graduates", "value":0.108291637 }, {  "axis":"Has MSc or PhD Degree", "value":0.208363117 },
        {  "axis":"Median Income Quantile", "value":0.003214286 }
        ]
    },
    {
      "key":"Cluster 3",
      "values":[
        {  "axis":"Whites", "value":0.606452587 }, {  "axis":"Latinos", "value":0.086579529 },
        {  "axis":"Blacks", "value":0.283162315 }, {  "axis":"Other Race", "value":0.004611622 },
        {  "axis":"Aliens", "value":0.027311675 }, {  "axis":"U.S. Citizens", "value":0.945785930 },
        {  "axis":"Unemployment", "value":0.058056568 }, {  "axis":"Employment", "value":0.941943432 },
        {  "axis":"Poverty", "value":0.184890789 }, {  "axis":"High School Graduates", "value":0.138145773 },
        {  "axis":"College Graduates", "value":0.168840895 }, {  "axis":"Has MSc or PhD Degree", "value":0.142019575 },
        {  "axis":"Median Income Quantile", "value":0.007946429 }
        ]
    },
    {
      "key":"Cluster 4",
      "values":[
        {  "axis":"Whites", "value":0.828023127 }, {  "axis":"Latinos", "value":0.130274326 },
        {  "axis":"Blacks", "value":0.081190798 }, {  "axis":"Other Race", "value":0.007380982 },
        {  "axis":"Aliens", "value":0.091524173 }, {  "axis":"U.S. Citizens", "value":0.891745602 },
        {  "axis":"Unemployment", "value":0.025797373 }, {  "axis":"Employment", "value":0.974202627 },
        {  "axis":"Poverty", "value":0.379874523 }, {  "axis":"High School Graduates", "value":0.064337557 },
        {  "axis":"College Graduates", "value":0.103825809 }, {  "axis":"Has MSc or PhD Degree", "value":0.118464756 },
  {  "axis":"Median Income Quantile", "value":0.005000000 }
        ]
    },
    {
      "key":"Cluster 5",
      "values":[
        {  "axis":"Whites", "value":0.750861940 }, {  "axis":"Latinos", "value":0.101783841 },
        {  "axis":"Blacks", "value":0.162044671 }, {  "axis":"Other Race", "value":0.007495128 },
        {  "axis":"Aliens", "value":0.031329636 }, {  "axis":"U.S. Citizens", "value":0.955179134 },
        {  "axis":"Unemployment", "value":0.088663968 }, {  "axis":"Employment", "value":0.911336032 },
        {  "axis":"Poverty", "value":0.534852346 }, {  "axis":"High School Graduates", "value":0.091740369 },
        {  "axis":"College Graduates", "value":0.024434118 }, {  "axis":"Has MSc or PhD Degree", "value":0.048718333 },
        {  "axis":"Median Income Quantile", "value":0.001071429 }
        ]
    },
    {
      "key":"Cluster 6",
      "values":[
        {  "axis":"Whites", "value":0.751498743 }, {  "axis":"Latinos", "value":0.093598917 },
        {  "axis":"Blacks", "value":0.193772965 }, {  "axis":"Other Race", "value":0.013923806 },
        {  "axis":"Aliens", "value":0.064784374 }, {  "axis":"U.S. Citizens", "value":0.920518275 },
        {  "axis":"Unemployment", "value":0.037442396 }, {  "axis":"Employment", "value":0.962557604 },
        {  "axis":"Poverty", "value":0.312898859 }, {  "axis":"High School Graduates", "value":0.100174048 },
        {  "axis":"College Graduates", "value":0.196867144 }, {  "axis":"Has MSc or PhD Degree", "value":0.175594663 },
        {  "axis":"Median Income Quantile", "value":0.007857143 }
        ]
    }
  ]
  ',
  simplifyDataFrame = F
)

d3_radar <- d3radarR::d3radar(json_data, height = 800, width = 800)

htmlwidgets::saveWidget(d3_radar, "dynamic_d3_radar.html")

###
# standard 2d polar plot with ggradar
standard_radar <- polar_stats

colnames(standard_radar)[colnames(standard_radar) == "Cluster"] <- "group"

standard_radar[ ,2:length(standard_radar)] <-  standard_radar[ ,2:length(standard_radar)] / 100

radar %<>% mutate_if(is.factor, as.character) %>%tidyr::as_tibble() 

ggradar::ggradar(radar, font.radar = "Arial", axis.label.size = 3,
                 legend.title = "Cluster: ",  grid.label.size = 4, group.point.size= 3,
        group.line.width= 1, )+
  theme_bw()+
  labs(title = "Comparison of Social Standards between each Cluster",
       subtitle = "Statistics Values in %")+
  theme(plot.title = element_text(hjust = 0.5), legend.position = "top", 
        legend.title = element_text("Cluster: "),
        plot.subtitle = element_text(hjust = 0.5, face="italic"),
        axis.ticks = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_blank())+
  guides(color = guide_legend(ncol = 6))+
  ggsave("static_standard_radar.png", width = 10, height = 10, units = "in", dpi= 300)


###
#parallel coordinates to compare clusters
parallel_coordinate <- polar_stats
parallel_coordinate[ ,2:ncol(parallel_coordinate)] <- parallel_coordinate[ ,2:ncol(parallel_coordinate)]/100

GGally::ggparcoord(parallel_coordinate,
           columns = 2:ncol(parallel_coordinate), groupColumn = 1, order = "anyClass",
           scale= "uniminmax",
           showPoints = T, 
           alphaLines = 0.9) + 
  scale_color_brewer(palette = "Dark2") +
  theme_bw()+
  labs(title= "Parallel Coordinates to Distinguish each Cluster",
       subtitle = "Standardize to Min = 0 & Max = 1",
       x= "Variables",
       y= "Value")+
  theme(legend.position="top",
        legend.title = element_text("Cluster "),
        plot.title = element_text(size= 13, hjust= 0.5),
        plot.subtitle = element_text(hjust = 0.5, face="italic"),
        legend.background = element_rect(fill="ghostwhite",
                                         size=0.5, linetype="solid", 
                                         colour ="slategray4")) +
  guides(fill = guide_legend(ncol = 6),
         color= guide_legend(ncol = 6))+
  ggsave("static_parallel_coordinates_radar.png", width = 18, height = 10, units = "in", dpi= 300)


## 3/30/2020