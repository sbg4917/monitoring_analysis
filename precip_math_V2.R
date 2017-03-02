# This is designed to calculate the runoff in separate sections of the RMSC campus where we have placed monitoring equipment. 
# Ultmately it will be used to determine what rainstorms would theoretically cause the system to overflow or reach capacity 
# and compare this to the data actually gathered from the site
############ TO DO #############################
# convert from inches^3 to a more reasonable unit?
# at some point make it so you can input dates and load files from that < append to master file (function)

# what is the volume of each catchment area? 
# tree trench baffle: located in DA-P4-2
# monitoring well: located DA-P2-1 ?

setwd('/home/sarah/Documents/Academic_Stuff/Capstone')

######### Read in data  ##########
weather_historic <- read.csv('historic_weather_data.csv') #temporarily using NOAA data as we do not have enough logged from the Wunderground station
# remove values less than zero as these indicate no data recorded
weather_historic <- subset(weather_historic, weather_historic$PRCP >= 0, select = c(DATE, PRCP))
#reformat date -- possibly only needed for NOAA data
weather_historic$DATE <- as.character(weather_historic$DATE)
weather_historic$DATE <- as.Date(weather_historic$DATE, "%Y %m %d")

# Read in HOBO data
HOBOdata <- read.csv('HOBOdata.csv', col.names = c('line', 'Date', '0.4m Water Level', '0.4m Temperature', 'Monitoring Well Temperature', 'Monnitoring Well Water Level', 'Tree Trench Temperature', 'Tree Trench Water Level')) 
HOBOdata$Date <- strptime(HOBOdata$Date, format = "%m/%d/%Y %H:%M:%S", tz = 'UTM')

# read in pendant data, convert to relative conductance (RC = current signal / maximum signal *100)
bioretention <- read.csv('Bioretention2_9_17.csv', col.names = c('ID','Date','temp','RC'))
bioretention$RC <-(bioretention$RC /330000) *100
# bioretention$Date <- as.character(bioretention$Date)
bioretention$Date <- strptime(bioretention$Date, format = "%m/%d/%Y %H:%M:%S", tz = 'UTM')

####### Constants ########
# area [in^2] (may not be correct-- double check)
pervious_areas <- matrix( c(6020, 4190, 2258), dimnames = list(c('tree trench', 'monioring well','bioretention'))) *12^2
impervious_areas <- matrix( c(2042, 0, 1584), dimnames = list(c('tree trench','monitoring well', 'bioretention'))) *12^2
pervious_pavement_area <- matrix(c(285, 16882, 0), dimnames = list(c('tree trench', 'monitoring well','bioretention'))) * 12 ^2
  

# s values [in]
S_values <- matrix ( c(10.40816327, 0.309278351, 12.22222222), dimnames = list (c('S pervious','s impervious','s pervious pavement'), c('S value')))
 #S_impervious <- 0.309278351
 #S_pervious_pavement <- 12.22222222 #find area in each drainage site

################## calculate theoretical runoff for each area ############################
weather_historic$runoff_pervious <- ifelse(weather_historic$PRCP > (0.2 * S_values[1]), weather_historic$runoff_pervious <- ((weather_historic$PRCP - 0.2 * S_values[1]) ^ 2 ) / (weather_historic$PRCP + 0.8* S_values[1]), weather_historic$runoff_pervious <- 0)
weather_historic$runoff_impervious <- ifelse(weather_historic$PRCP > (0.2 * S_values[2]), weather_historic$runoff_impervious <- ((weather_historic$PRCP - 0.2 * S_values[2]) ^ 2 ) / (weather_historic$PRCP + 0.8* S_values[2]), weather_historic$runoff_impervious <- 0)
weather_historic$runoff_pervious_pavement <- ifelse(weather_historic$PRCP > (0.2 * S_values[3]), weather_historic$runoff_pervious_pavement <- ((weather_historic$PRCP - 0.2 * S_values[3]) ^ 2 ) / (weather_historic$PRCP + 0.8* S_values[3]), weather_historic$runoff_pervious_pavement <- 0)

# calculate runoff for each area [in^]
weather_historic$tree_trench <- weather_historic$runoff_pervious * pervious_areas[1] + weather_historic$runoff_impervious * impervious_areas[1] + weather_historic$runoff_pervious_pavement * pervious_pavement_area[1] 
weather_historic$monitoring_well <- weather_historic$runoff_pervious* pervious_areas[2] + weather_historic$runoff_impervious * impervious_areas[2] +  weather_historic$runoff_pervious_pavement * pervious_pavement_area[2] 
weather_historic$bioretention <-  weather_historic$runoff_pervious* pervious_areas[3] + weather_historic$runoff_impervious * impervious_areas[3] +  weather_historic$runoff_pervious_pavement * pervious_pavement_area[3]

######### CALCULATE VOLUME OF WATER PRESENT ###################
HOBOdata$monitoring.well.vol <- HOBOdata$Monnitoring.Well.Water.Level *39.3701* 3.14 *6 # 6 in. diameter, multiply by 39.3701 to convert to in. from meters

x = nrow(bioretention)
for(i in 1:x){
  if(bioretention$RC[i] != 0){
  bioretention$volume[i] <- 18*3.14*24 #outflow is 18" up, pipe diameter = 24"
  }
  else{
    bioretention$volume[i] <- 0 
  }
}

############# Plots ##################

# plot expected runoff and actual data values
plot(weather_historic$DATE, weather_historic$bioretention, col = 'blue', ylab = "Runoff [in^3]", xlab = 'Date')
limy <- par("usr")
par(new = T)
plot(bioretention$Date, bioretention$volume, col = 'red', axes = F, xlab = NA, ylab = NA, ylim = c(0, limy[4]))
legend('topright',inset = 0.05, c('calculated runoff','actual runoff'), fill = c('blue','red'))


plot(weather_historic$DATE, weather_historic$monitoring_well, col = 'blue', xlab = 'Date', ylab = 'runoff [in^3]')
limy <- par("usr")
par(new = T)
plot(HOBOdata$Date, HOBOdata$monitoring.well.vol, col = 'red', axes = F, xlab = NA, ylab = NA, ylim = c(0, limy[4]) )
legend('topright',inset = 0.05, c('calculated runoff','actual runoff'), fill = c('blue','red'))
