#### run following install fore each library needed just once
# install.packages("dplyr")

#### setting libraries we will need later
library(rworldmap)
library(ggmap)
library(mapproj)
library(dplyr)
library(leaflet)

#### setting working directory
setwd("INSERT YOUR WORK DIRECTORY HERE")

#### reading in Beacon St. data
`compiled_full` <- read.csv("chelsea_compiled.csv")
isd <- read.csv("ISD.csv")
#View(`isd`)

compiled<-compiled_full[compiled_full$RISKSCORE>5,]
#View(`compiled`)
#View(`compiled_full`)

#### creating new vars
compiled_full$str_risk4<-ifelse(compiled_full$RISKSCORE!=0,"High Risk","Low Risk")




#################
######## MAPPING #########
#################

#### find a good address to center map on
map.center <- geocode("53-55 Washington Ave. Chelsea, MA 02150")

#### grabbing map and setting zoom
# try maptype= toner, watercolor
# source='stamen', 'google','osm','cloudmade'
chelsea <- get_map(c(lon=map.center$lon, lat=map.center$lat),
                   source='osm', zoom=15, col="bw")
compiledMap <- ggmap(chelsea,
                     extent = 'device', legend='topleft')

#### building the report
# renaming variables
compiled_full<-rename(compiled_full, Risk_Area=risk1, 
                      Service_Calls=risk2,
                      Crime=risk3,
                      Psych_Risk=PSYCHOSCORE,
                      Notifications=risk4,
                      Balance_Due=risk5,
                      Financ_Risk=FINANCIALSCORE,
                      Potential_Vacancy=risk6,
                      Vacancy=risk7,
                      History_Vacancy=risk8,
                      Num_Tickets=risk9,
                      Fine_Total=risk10,
                      Absentee_LL=risk11,
                      High_Water_Use=risk12,
                      Physical_Risk=PHYSICALSCORE)


riskarea <- compiled_full[compiled_full$Risk_Area==1,]
servicecalls <- compiled_full[compiled_full$Service_Calls>0,]
crime <- compiled_full[compiled_full$Crime>0,]
psychrisk <- compiled_full[compiled_full$Psych_Risk>0.3,]
notifications <- compiled_full[compiled_full$Notifications>0,]
balancedue <- compiled_full[compiled_full$Balance_Due>0,]
financrisk <- compiled_full[compiled_full$Financ_Risk>0,]
potentialvacancy <- compiled_full[compiled_full$Potential_Vacancy>0,]
vacancy <- compiled_full[compiled_full$Vacancy>0,]
historyvacancy <- compiled_full[compiled_full$History_Vacancy>0,]
numtickets <- compiled_full[compiled_full$Num_Tickets>0,]
finetotal <- compiled_full[compiled_full$Fine_Total>0,]
absenteell <- compiled_full[compiled_full$Absentee_LL>0,]
highwater <- compiled_full[compiled_full$High_Water_Use>0,]
physicalrisk <- compiled_full[compiled_full$Physical_Risk>0,]
suggested <- compiled_full[compiled_full$RISKSCORE > 4.6,]
suggested90th <- compiled_full[compiled_full$RISKSCORE > 6,]

#mean(compiled_full$RISKSCORE[compiled_full$problemproperty=="ISD"])]
#mean riskscore of isd properties is 4.611111
#compiled_full$count <- 1
#sum(compiled_full$count[compiled_full$problemproperty=="ISD"], na.rm=TRUE)
#sum(compiled_full$count[compiled_full$RISKSCORE > 4.6], na.rm=TRUE)
#benchmark <- quantile(compiled_full$RISKSCORE,.9,na.rm = TRUE)
#sum(compiled_full$count[compiled_full$RISKSCORE > benchmark], na.rm=TRUE)



map <- leaflet(compiled_full) %>%
  # Center map
  setView(-71.033111,42.395009, zoom = 15) %>%
  # Base groups
  addTiles(group = "OSM (default)") %>%
  addProviderTiles("Stamen.Toner", group = "Toner") %>%
  addProviderTiles("Stamen.TonerLite", group = "Toner Lite") %>%
  # Overlay groups
  addCircleMarkers(data=riskarea,~longit, ~lat,popup = ~as.character(location), radius = 5, stroke = F, color = c('red'),group = "Risk_Area") %>%
  addCircleMarkers(data=servicecalls,~longit, ~lat,popup = ~as.character(location), radius = 5, stroke = F, color = c('red'),fillOpacity = 0.5,group = "Service_Calls") %>%
  addCircleMarkers(data=crime,~longit, ~lat,popup = ~as.character(location), radius = 5, stroke = F, color = c('red'),fillOpacity = 0.5,group = "Crime") %>%
  addCircleMarkers(data=psychrisk,~longit, ~lat,popup = ~as.character(location), radius = 5, stroke = F, color = c('blue'),fillOpacity = 0.5,group = "Psych_Risk") %>%
  addCircleMarkers(data=notifications,~longit, ~lat,popup = ~as.character(location), radius = 5, stroke = F, color = c('red'),fillOpacity = 0.5,group = "Notifications") %>%
  addCircleMarkers(data=balancedue,~longit, ~lat,popup = ~as.character(location), radius = 5, stroke = F, color = c('red'),fillOpacity = 0.5,group = "Balance_Due") %>%
  addCircleMarkers(data=financrisk,~longit, ~lat,popup = ~as.character(location), radius = 5, stroke = F, color = c('blue'),fillOpacity = 0.5,group = "Financ_Risk") %>%
  addCircleMarkers(data=potentialvacancy,~longit, ~lat,popup = ~as.character(location), radius = 5, stroke = F, color = c('red'),fillOpacity = 0.5,group = "Potential_Vacancy") %>%
  addCircleMarkers(data=vacancy,~longit, ~lat,popup = ~as.character(location), radius = 5, stroke = F, color = c('red'),fillOpacity = 0.5,group = "Vacancy") %>%
  addCircleMarkers(data=historyvacancy,~longit, ~lat,popup = ~as.character(location), radius = 5, stroke = F, color = c('red'),fillOpacity = 0.5,group = "History_Vacancy") %>%
  addCircleMarkers(data=numtickets,~longit, ~lat,popup = ~as.character(location), radius = 5, stroke = F, color = c('red'),fillOpacity = 0.5,group = "Num_Tickets") %>%
  addCircleMarkers(data=finetotal,~longit, ~lat,popup = ~as.character(location), radius = 5, stroke = F, color = c('red'),fillOpacity = 0.5,group = "Fine_Total") %>%
  addCircleMarkers(data=absenteell,~longit, ~lat,popup = ~as.character(location), radius = 5, stroke = F, color = c('red'),fillOpacity = 0.5,group = "Absentee_LL") %>%
  addCircleMarkers(data=highwater,~longit, ~lat,popup = ~as.character(location), radius = 5, stroke = F, color = c('red'),fillOpacity = 0.5,group = "High_Water_Use") %>%
  addCircleMarkers(data=physicalrisk,~longit, ~lat,popup = ~as.character(location), radius = 5, stroke = F, color = c('blue'),fillOpacity = 0.5,group = "Physical_Risk") %>%
  addCircleMarkers(data=isd,~Longit, ~Lat, popup = ~as.character(Address), radius = 5, stroke = F, color = c('purple'),fillOpacity = 0.5,group = "ISD_Properties") %>%
  addCircleMarkers(data=suggested,~longit, ~lat, popup = ~as.character(location), radius = 5, stroke = F, color = c('green'),fillOpacity = 0.5,group = "Suggested") %>%
  addCircleMarkers(data=suggested90th,~longit, ~lat, popup = ~as.character(location), radius = compiled_full$RISKSCORE, stroke = F, color = c('green'),fillOpacity = 0.5,group = "Suggested_90th") %>%

  # Layers control
  addLayersControl(
    baseGroups = c("OSM (default)", "Toner", "Toner Lite"),
    overlayGroups = c("Risk_Area", "Service_Calls","Crime","Psych_Risk","Notifications","Balance_Due","Financ_Risk","Potential_Vacancy","Vacancy","History_Vacancy","Num_Tickets","Fine_Total","Absentee_LL","High_Water_Use","Physical_Risk","ISD_Properties","Suggested","Suggested_90th"),
    options = layersControlOptions(collapsed = TRUE)
  )%>% 
  hideGroup("Risk_Area")%>% hideGroup("Psych_Risk")%>% hideGroup("Financ_Risk") %>% hideGroup("Physical_Risk")%>% hideGroup("Service_Calls")%>% hideGroup("Crime") %>% 
  hideGroup("Notifications") %>% hideGroup("Balance_Due") %>% hideGroup("Potential_Vacancy") %>% 
  hideGroup("Vacancy") %>% hideGroup("History_Vacancy") %>% hideGroup("Num_Tickets") %>% 
  hideGroup("Fine_Total") %>% hideGroup("Absentee_LL") %>% hideGroup("High_Water_Use")
map
