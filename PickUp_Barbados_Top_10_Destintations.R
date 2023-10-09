library(tidyverse)
library(ggplot2)
library(mdsr)
library(lubridate)
library(forcats)

ride_data

ride_data_clean<-ride_data%>%
  select(drop_address, drop_lat_long)

ride_data_clean


ride_data_with_abbrev_lat<-ride_data_clean%>%
  mutate(
    abbrev_lat_long=paste(substring(drop_lat_long,1,6),substring(drop_lat_long,12,16))
  )
  
ride_data_with_abbrev_lat

tallied_rides_df<-ride_data_with_abbrev_lat%>%
  group_by(abbrev_lat_long)%>%
  summarize(freq=n(), abbrev_lat_long, drop_address)%>% #Put in and take out drop adress as needed to see add or top 10.
  unique()
 
tallied_rides_df

tallied_rides_df_ordered<-tallied_rides_df%>%
arrange(desc(freq))

tallied_rides_df_ordered

tallied_rides_df_ordered_filtered<-tallied_rides_df_ordered%>%
  filter(abbrev_lat_long!="13.080 59.48")

tallied_rides_df_ordered_filtered

tallied_rides_df_ordered_filtered_1<-tallied_rides_df_ordered%>%
  filter(abbrev_lat_long==popular_1st)

tallied_rides_df_ordered_filtered_1

print(tallied_rides_df_ordered_filtered_1, n=100)

tallied_rides_df_ordered_filtered_2<-tallied_rides_df_ordered%>%
  filter(abbrev_lat_long==popular_2nd)

tallied_rides_df_ordered_filtered_2

tallied_rides_df_ordered_filtered_3<-tallied_rides_df_ordered%>%
  filter(abbrev_lat_long=="13.067 59.57")

tallied_rides_df_ordered_filtered_3
print(tallied_rides_df_ordered_filtered_3, n=100)

tallied_rides_df_ordered_filtered_4<-tallied_rides_df_ordered%>%
  filter(abbrev_lat_long==popular_4th)

tallied_rides_df_ordered_filtered_4
print(tallied_rides_df_ordered_filtered_4, n=100)

tallied_rides_df_ordered_filtered_5<-tallied_rides_df_ordered%>%
  filter(abbrev_lat_long==popular_5th)

tallied_rides_df_ordered_filtered_5
print(tallied_rides_df_ordered_filtered_5, n=100)

tallied_rides_df_ordered_filtered_6<-tallied_rides_df_ordered%>%
  filter(abbrev_lat_long==popular_6th)

tallied_rides_df_ordered_filtered_6

print(tallied_rides_df_ordered_filtered_6, n=100)

tallied_rides_df_ordered_filtered_7<-tallied_rides_df_ordered%>%
  filter(abbrev_lat_long==popular_7th)

tallied_rides_df_ordered_filtered_7

print(tallied_rides_df_ordered_filtered_7, n=100)

tallied_rides_df_ordered_filtered_8<-tallied_rides_df_ordered%>%
  filter(abbrev_lat_long==popular_8th)

tallied_rides_df_ordered_filtered_8

print(tallied_rides_df_ordered_filtered_8, n=100)

tallied_rides_df_ordered_filtered_9<-tallied_rides_df_ordered%>%
  filter(abbrev_lat_long==popular_9th)

tallied_rides_df_ordered_filtered_9

print(tallied_rides_df_ordered_filtered_9, n=100)

tallied_rides_df_ordered_filtered_10<-tallied_rides_df_ordered%>%
  filter(abbrev_lat_long==popular_10th)

tallied_rides_df_ordered_filtered_10

print(tallied_rides_df_ordered_filtered_10, n=100)

popular_1st<-"13.074 59.59"#Hastings main road (Lanterns area) 397 
popular_2nd<-"13.079 59.61"#Hilton hotel area 245
popular_3rd<-"13.067 59.57"#Gap--cove/primo/Mcnaughtys(Nightclubs) 239 
popular_4th<-"13.071 59.58"#Worthing area 230 esso gas station/cave shepherd/Champers-area/worthing square/ east worthing
popular_5th<-"13.074 59.58"#Worthing area Cheffette/quayside/accra beach hotel 205/west wrothing
popular_6th<-"13.066 59.56"#Dover area (Sandals etc.) 184
popular_7th<-"13.068 59.57"#St.Lawrence Gap area-moreso restaurants such as Primo, Cafesol, castaways, yellow bird hotel 184 
popular_8th<-"13.063 59.54"#Oistins Fish market area for sure 179
popular_9th<-"13.076 59.60"#sugarbay/courtyard marriot area
popular_10th<-"13.069 59.57"#Divi south winds area

Location_Description<-c("Hastings (Lanterns Area)", "Hilton hotel Area", "Gap Nightclubs Area",
                        "East Worthing (Worthing square area)", "West Worthing (Quayside area)", "Dover (Sandals area)",
                        "Gap restaurant area", "Oistins Fish Market", "Sugar Bay / Marriot area", "Divi South Winds area")

Location_Drop_Freq<-c(397,245,239,230,205,184,184,179, 178, 172)

Top_ten_popular_locations_df<-data.frame(Location_Description,Location_Drop_Freq)
Top_ten_popular_locations_df

Top_Ten_Plot<-ggplot(Top_ten_popular_locations_df,aes(x=fct_reorder(Location_Description, Location_Drop_Freq),
                                                      y=Location_Drop_Freq,fill=Location_Description))+
  geom_col()+
  labs(title = "Top 10 Most Popular Locations For PickUP",
       x = "Destination", y = "Number of Tips",
       tag = "PickUP Barbados", caption = "Powered by Luis Sebastian Weekes")+
  scale_x_discrete(labels=c('10', '9', '8', '7', '6','5','4','3','2','1'))

Top_Ten_Plot

