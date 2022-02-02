library('rnoaa')
library(lubridate)

require(devtools)

options(noaakey = "ESLHtORWGoywtryTvPwuijdzGjqrGiJq") # http://www.ncdc.noaa.gov/cdo-web/token

load("station_data.Rdata")

# Get available stations
# station_data <- ghcnd_stations() # Takes a while to run and you can load form the available R object 
# save(station_data, file="station_data.RData")

get_nearby_stations <- function(d, lat, lon) {
    # Define the GPS coordinates of a fire event 
    df <- data.frame(id=c(d), latitude=c(lat), longitude=c(lon), stringsAsFactors=FALSE)

    # Get nearby stations that can provide the mean average temperature, the max temperature, the min temperature and the precipitation
    nearby_stations <- meteo_nearby_stations(lat_lon_df=df, station_data=station_data, radius=1000, var="all", year_min=2014, year_max=2015)

    return(nearby_stations)
}

maximum_getter <- function(nearby_stations, actual_date) {
    for(i in 1:length(nearby_stations[[1]]$id)) {
        weather_data <- ghcnd_search(nearby_stations[[1]]$id[i], var=c("TMAX"), date_min=actual_date, date_max=actual_date)
        weather_frame <- do.call(rbind.data.frame, weather_data['tmax'])
        
        temperature_max = weather_frame$tmax[1]

        if((!is.na(temperature_max)) && (length(temperature_max) != 0)) {
            return(temperature_max)
        }
    }

    return(NA)
}
minimum_getter <- function(nearby_stations, actual_date) {
    for(i in 1:length(nearby_stations[[1]]$id)) {
        weather_data <- ghcnd_search(nearby_stations[[1]]$id[i], var=c("TMIN"), date_min=actual_date, date_max=actual_date)
        weather_frame <- do.call(rbind.data.frame, weather_data['tmin'])
        
        temperature_min = weather_frame$tmin[1]

        if((!is.na(temperature_min)) && (length(temperature_min) != 0)) {
            return(temperature_min)
        }
    }
    
    return(NA)
}
precipitation_getter <- function(nearby_stations, actual_date) {
    for(i in 1:length(nearby_stations[[1]]$id)){
        weather_data <- ghcnd_search(nearby_stations[[1]]$id[i], var=c("PRCP"), date_min=actual_date, date_max=actual_date)
        weather_frame <- do.call(rbind.data.frame, weather_data['prcp'])
        
        precipitation = weather_frame$prcp[1]
        
        if(!is.na(precipitation) && (length(precipitation) != 0)) {
            return(precipitation)
        }
    }

    return(NA)
}
average_getter <- function(nearby_stations, actual_date) {
    for(i in 1:length(nearby_stations[[1]]$id)){
        weather_data <- ghcnd_search(nearby_stations[[1]]$id[i], var=c("TAVG"), date_min=actual_date, date_max=actual_date)
        weather_frame <- do.call(rbind.data.frame, weather_data['tavg'])
        
        temperature_avg = weather_frame$tavg[1]
        
        if(!is.na(temperature_avg) && (length(temperature_avg) != 0)) {
            return(temperature_avg)
        }
    }

    return(NA)
}