options(warn = -1)
library(dplyr)
library(magrittr)
library(ggplot2)
library(reshape2)
library(gridExtra)
library(readr)
library(lubridate)
library(magrittr)
#update.packages()

#----------------------------------------------- Read in the data -------------------------------------------------
#read the data file (has to in the directory you're working in)
gainsville_df <- readr::read_csv("311_Service_Requests__myGNV_.csv")

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

# levels(gainsville_df$`Assigned To:`)
# levels(gainsville_df$`Assigned To:`)[c(5:8,25,30:31,36:44,53)] <- c("Health and Public Safety")
# levels(gainsville_df$`Assigned To:`)
# levels(gainsville_df$`Assigned To:`)[c(3,4,6:12,14:29,33:38,41,43,44, 46:48)] <- c("Neighborhoods")
# #levels(gainsville_df$`Assigned To:`)
# levels(gainsville_df$`Assigned To:`)[c(1,2,5,7:10)] <- c("Public Services")
# #levels(gainsville_df$`Assigned To:`)
# levels(gainsville_df$`Assigned To:`)[c(4:6)] <- c("Transportation")

category_names_df <- data.frame(table(as.factor(gainsville_df$`Assigned To:`)))
colnames(category_names_df) <- c("Branch", "Frequencies")
#View(category_names_df)

#------------------------------------------ Request Type Category ------------------------------------------------------

#levels(gainsville_df$`Request Type`)
issue_type_df <- data.frame(table(as.factor(gainsville_df$`Request Type`)))
colnames(issue_type_df) <- c("Issue Category", "Frequencies")

issue_type_df <- dplyr::filter(issue_type_df, Frequencies > median(issue_type_df$Frequencies)) #Cutoffs
#View(category_names_df)

#------------------------------------------ Case Owners Category ------------------------------------------------------

#levels(gainsville_df$`Reporter Display`)
case_owners_df <- data.frame(table(as.factor(gainsville_df$`Reporter Display`)))
colnames(case_owners_df) <- c("Case Owner", "Frequencies")

case_owners_df <- dplyr::filter(case_owners_df, Frequencies > median(case_owners_df$Frequencies))
#View(case_owners_df)


#---------------------------------------- K-means Klustering ------------------------------------------------------------


