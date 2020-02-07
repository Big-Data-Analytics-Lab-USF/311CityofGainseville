options(warn = -1)
library(dplyr)
library(magrittr)
library(ggplot2)
library(reshape2)
library(gridExtra)
library(readr)
library(lubridate)
library(magrittr)
#install.packages("forcats", dependencies = T)
library(forcats)
library(readr)
library(viridisLite)
library(viridis)
library(hrbrthemes)
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


#-------------------------------------------- Pre-plots ----------------------------------------------------------------

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
          ggtitle("311 Service Requests by Days")

ggsave("311_service_request_by_date.png",dpi = 600, height = 5.00, width = 6.0)

#Branch Work load from Sep 2014 - Jan 2020, thickness is frequency, diamond is the mean frequency for checking the box plot average

#Box plot
ggplot2::ggplot(data = gainsville_df, aes(`Assigned To:`, lubridate::date(`Service Request Date`), colour= `Assigned To:`))+
          geom_boxplot(notch=F, varwidth = T)+
          stat_summary(fun.y=mean, geom="point", shape=18, size=4)+
          ylim(as.Date("2014-01-01", format= "%Y-%m-%d"), as.Date("2020-01-01", format= "%Y-%m-%d"))+
          scale_color_brewer(palette="Dark2")+
          theme_bw()+
          theme(plot.title = element_text(hjust = 0.5), legend.position = "none")+
          labs(x="311 Gainsville Branch",y= "Year")+
          ggtitle("Branch Busyness by Year")

ggsave("311_branch_busyness_by_year.png",dpi = 600, height = 5.00, width = 6.0)

#Request types from Sep 2014 - Jan 2020

#Bar chart
ggplot2::ggplot(data= gainsville_df %>%
                  group_by(`Request Type`) %>%
                  summarise(num_calls = length(`Request Type`))
                , aes(x= `Request Type`, y= num_calls, fill= `Request Type`))+
        geom_bar(stat = "identity")+
        theme_bw()+
        #geom_errorbar(aes(ymin=len-sd, ymax=len+sd), width=.2,position=position_dodge(.9))+
        theme(plot.title = element_text(hjust = 0.5), legend.position = "none",axis.text.x = element_text(angle = 90))+
        labs(x="Requested Service",y= "No. of Calls")+
        ggtitle("Number of Calls per Requested Service")+
        scale_fill_viridis(discrete = T)

ggsave("311_requests_per_call.png",dpi = 600, height = 5.00, width = 10)



#---------------------------------------- K-means Klustering ------------------------------------------------------------

#summary(is.na(gainsville_df$Latitude)) #see for NAs in latitude
#summary(is.na(gainsville_df$Longitude)) #see for NAs in longitude

# NEED THIS IN CASE SHE NEEDS CALLS
# dat <- new_df %>% 
#   group_by(Request.Type) %>%
#   summarise(no_rows = length(Request.Type))







