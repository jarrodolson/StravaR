##============================================================================
## StravaR - API Integration between R and and Strava.com
## Copyright (C) 2016  Jarrod Olson - jarrod.olson@outlook.com
##
## This program is free software: you can redistribute it and/or modify
## it under the terms of the GNU General Public License as published by
## the Free Software Foundation, either version 3 of the License, or
## (at your option) any later version.
##
## This program is distributed in the hope that it will be useful,
## but WITHOUT ANY WARRANTY; without even the implied warranty of
## MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
## GNU General Public License for more details.
##
## You should have received a copy of the GNU General Public License
## along with this program.  If not, see <http://www.gnu.org/licenses/>.
##=============================================================================

##Contains scripts for parsing data

#'Parse Activity Stream
#'
#'Parses an activity stream object and returns it as a data frame.
#'
#'@param response An httr response object for a stream, unparsed
#'@return data A dataframe object with all available fields
#'@export
parseActivityStream <- function(response){
  ##Should make into a tidy dataframe
  core <- httr::content(response)
  columns <- c()
  for (d in 1:length(core)){
    temp <- core[[d]]$data
    columns <- c(columns, core[[d]]$type)
    if (core[[d]]$type != 'latlng'){
      temp <- as.numeric(temp)
    }
    if (d==1){
      data <- data.frame(row.names = 1:length(temp))
      data[[core[[d]]$type]] <- temp
    } else {
      data[[core[[d]]$type]] <- temp
    }
  }
  temp1 <- c()
  temp2 <- c()
  for (r in 1:nrow(data)){
    temp1 <- c(temp1, data$latlng[[r]][[1]])
    temp2 <- c(temp2, data$latlng[[r]][[2]])
  }
  data$lat <- temp1
  data$lng <- temp2
  data
}

#'Parse Athlete Summary Data
#'
#'Parses Athlete Summary response data, either summary or detailed from Strava
#'JSON API. Dynamically determines the level of detail from JSON object.
#'
#'@param response An httr response object from an authenticated call to
#'    Strava JSON API
#'@return out A single line data frame object with features from json object
#'@examples
#'##Not a real token
#'\dontrun{me <- retrieveAthlete('abcdefghijk')}
#'\dontrun{me_df <- parseAthleteSummary(me)}
#'@export
parseAthleteSummary <- function(response){
  data <- httr::content(response)
  if (data$resource_state >= 2) {
    out2 <- data.frame(id = data$id, username = data$username,
                       firstname = data$firstname,
                       lastname = data$lastname,
                       profile_medium = data$profile_medium,
                       profile = data$profile, city = data$city,
                       state = data$state, country = data$country,
                       sex = data$sex,
                       friend = ifelse(is.null(data$friend), NA, data$friend),
                       following = ifelse(is.null(data$following), NA, data$following),
                       premium = data$premium,
                       created_at = convertStravaDate(data$created_at),
                       updated_at = convertStravaDate(data$updated_at))
  }
  if (data$resource_state == 3) {
    defaultSport <- ifelse(data$athlete_type == 0, "cyclist", "runner")
    out3 <- data.frame(follower_count = data$follower_count,
                       friend_count = data$friend_count,
                       mutual_friend_count = data$mutual_friend_count,
                       athlete_type = defaultSport,
                       date_preference = data$date_preference,
                       measurement_preference = data$measurement_preference,
                       email = data$email,
                       ftp = ifelse(is.null(data$ftp), NA, data$ftp),
                       weight = data$weight
    )
    out <- cbind(out2, out3)
  }
  
  if (data$resource_state == 2) {
    ##To facilitate future rbind calls
    out23 <- data.frame(follower_count = NA, friend_count = NA,
                        mutual_friend_count = NA, athlete_type = NA,
                        date_preference = NA, measurement_preference = NA,
                        email = NA, ftp = NA, weight = NA)
    out <- cbind(out2, out23)
  }
  
  out
}

#'Either Create Dataframe or Bind, based on row
#'
#'@param d Integer indicating whether this is the first instance of the data
#'    frame, or a later version
#'@return ou A data frame object
createOrBind <- function(d, row, out){
  if (d == 1) {
    out = row
  } else {
    out <- rbind(out, row)
  }
  out
}

#'Parse Athlete's Bike Summary Data
#'
#'Parses Athlete's Bike Summary data, drawn from Athlete JSON object from Strava
#'JSON API.
#'
#'@param response An httr response object from an authenticated call to
#'    Strava JSON API
#'@return out A data frame with one row per bike object with features from json
#'    object
#'@examples
#'##Not a real token
#'\dontrun{me <- retrieveAthlete('abcdefghijk')}
#'\dontrun{my_bikes <- parseAthleteBikeSummary(me)}
#'@export
parseAthleteBikeSummary <- function(response){
  ownerID = httr::content(response)$id
  data = httr::content(response)$bikes
  out <- NULL
  for (d in 1:length(data)) {
    temp <- data[[d]]
    row = data.frame(owner_id = ownerID, bike_id = temp$id,
                     bike_name = temp$name, bike_distance = temp$distance)
    out = createOrBind(d, row)
  }
  out
}

#'Parse Athlete's Club Summary Data
#'
#'Parses Athlete's Club Summary data, drawn from Athlete JSON object from Strava
#'JSON API.
#'
#'@param response An httr response object from an authenticated call to
#'    Strava JSON API
#'@return out A data frame with one row per club object with features from json
#'    object
#'@examples
#'##Not a real token
#'\dontrun{me <- retrieveAthlete('abcdefghijk')}
#'\dontrun{my_clubs <- parseAthleteClubSummary(me)}
#'@export
parseAthleteClubSummary <- function(response){
  ownerID = httr::content(response)$id
  data = httr::content(response)$clubs
  out <- NULL
  for (d in 1:length(data)) {
    temp <- data[[d]]
    row = data.frame(owner_id = ownerID, club_id = temp$id,
                     club_name = temp$name,
                     club_profile_medium = temp$profile_medium,
                     club_profile = temp$profile, club_sport = temp$sport_type,
                     club_member_count = temp$member_count,
                     club_featured = temp$featured)
    out = createOrBind(d, row, out)
  }
  out
}

#'Parse Athlete's Shoe Summary Data
#'
#'Parses Athlete's Shoe Summary data, drawn from Athlete JSON object from Strava
#'JSON API.
#'
#'@param response An httr response object from an authenticated call to
#'    Strava JSON API
#'@return out A data frame with one row per shoe object with features from json
#'    object
#'@examples
#'##Not a real token
#'\dontrun{me <- retrieveAthlete('abcdefghijk')}
#'\dontrun{my_shoes <- parseAthleteShoeSummary(me)}
#'@export
parseAthleteShoeSummary <- function(response){
  ownerID = httr::content(response)$id
  data = httr::content(response)$shoes
  out <- NULL
  for (d in 1:length(data)) {
    temp <- data[[d]]
    row = data.frame(owner_id = ownerID, shoe_id = temp$id,
                     show_primary = temp$primary, shoe_name = temp$name,
                     shoe_distance = temp$distance)
    out = createOrBind(d, row, out)
  }
  out
}

#'Parse Athlete Stats
#'
#'Parses a given athletes stats, returning as a data frame. Note that the
#'max_distance and max_climb are global, so they would take the maximum of
#'ride, run, or swim.
#'
#'@param response An httr response object from an authenticated call to
#'    Strava JSON API
#'@param athlete_id Athlete ID for user of interest
#'@return out A data frame with one row per statistic reported with features
#'    from json object
#'@examples
#'##Not a real token
#'\dontrun{me <- retrieveAthlete('abcdefghijk')}
#'\dontrun{me_stats <- retrieveAthleteStats('abcdefghijk', me$id[1])}
#'\dontrun{my_stats_df <- parseAthleteStats(me_stats, me$id[1])}
#'@export
parseAthleteStats <- function(response, athlete_id){
  data <- httr::content(response)
  totals <- c("recent_ride_totals", "recent_run_totals",
              "recent_swim_totals", "ytd_ride_totals",
              "ytd_run_totals", "ytd_swim_totals", "all_ride_totals",
              "all_run_totals", "all_swim_totals")
  out <- NULL
  for (t in 1:length(totals)){
    #print(t)
    #print(el)
    #print(elLi)
    el <- totals[[t]]
    elLi <- strsplit(el, "_")[[1]]
    temp <- data[[el]]
    #print(temp)
    if (elLi[[1]] == 'recent'){
      achievements <- temp$achievement_count
    } else {achievements <- NA}
    row = data.frame(athlete_id = athlete_id, time = elLi[[1]],
                     ride_type = elLi[[2]], stat_category = elLi[[3]],
                     count = temp$count, distance = temp$count,
                     moving_time = temp$moving_time,
                     elapsed_time = temp$elapsed_time,
                     elevation_gain = temp$elevation_gain,
                     achievement_count = achievements,
                     max_distance = data$biggest_ride_distance,
                     max_climb = data$biggest_climb_elevation_gain)
    out <- createOrBind(t, row, out)
  }
  out
}

#'Converts Strava Date-Time Object to POSIXct
#'
#'@param stravaDateVector A character vector formatted by Strava
#'@return A Posixct date-time object
convertStravaDate <- function(stravaDateVector){
  as.POSIXct(strptime(stravaDateVector, "%FT%X"))
}


handleNull <- function(val){
  ifelse(is.null(val), NA, val)
}

#'Parse Athlete Activity list
#'
#'Iterates through an Athlete Activity List and passes to
#'\link{parseAthleteActivity}.
#'
#'@param response An httr response object from an authenticated call to
#'    Strava JSON API
#'@return out A data frame with one row per activity with features from json
#'    object
#'@export
parseAthleteActivityList <- function(response){
  data <- httr::content(response)
  out <- NULL
  for (r in 1:length(data)){
    row <- parseAthleteActivity(data[[r]])
    out <- createOrBind(r, row, out)
  }
  out
}

#'Parse Athlete Activity
#'
#'Parses through activity objects and returns as a single line data frame.
#'
#'@param activityObj A named list activity object from Strava JSON API
#'@return out A data frame with one row with attributes from Strava API JSON
#'@export
parseAthleteActivity <- function(activityObj){
  row <- data.frame(id = activityObj$id,
                    external_id = handleNull(activityObj$external_id),
                    upload_id = handleNull(activityObj$upload_id),
                    athlete_id = activityObj$athlete$id,
                    name = activityObj$name,
                    description = handleNull(activityObj$description),
                    distance = activityObj$distance,
                    moving_time = activityObj$moving_time,
                    elapsed_time = activityObj$elapsed_time,
                    total_elevation_gain = handleNull(activityObj$total_elevation_gain),
                    elev_high = handleNull(activityObj$elev_high),
                    elev_low = handleNull(activityObj$elev_low),
                    type = activityObj$type,
                    start_date = convertStravaDate(handleNull(activityObj$start_date)),
                    start_date_local = convertStravaDate(activityObj$start_date_local),
                    timezone = activityObj$timezone,
                    start_lat = handleNull(activityObj$start_latitude),
                    start_lng = handleNull(activityObj$start_longitude),
                    end_lat = handleNull(activityObj$end_latlng[[1]]),
                    end_lng = handleNull(activityObj$end_latlng[[2]]),
                    achievement_count = activityObj$achievement_count,
                    kudos_count = activityObj$kudos_count,
                    comment_count = activityObj$comment_count,
                    athlete_count = activityObj$athlete_count,
                    photo_count = activityObj$photo_count,##count of instrgrams
                    map_id = handleNull(activityObj$map$id),
                    trainer = activityObj$trainer,
                    commute = activityObj$commute,
                    manual = activityObj$manual,
                    private = activityObj$private,
                    device_name = handleNull(activityObj$device_name),
                    embed_token = handleNull(activityObj$embed_token),
                    flagged = activityObj$flagged,
                    workout_type = handleNull(activityObj$workout_type),
                    gear_id = activityObj$gear_id,
                    avg_speed = activityObj$average_speed,
                    max_speed = activityObj$max_speed,
                    avg_cadence = handleNull(activityObj$average_watts),
                    avg_temp = handleNull(activityObj$average_temp),
                    avg_watts = handleNull(activityObj$average_watts),
                    max_watts = handleNull(activityObj$max_watts),
                    avg_watts_wgtd = handleNull(activityObj$weighted_average_watts),
                    kilojoules = handleNull(activityObj$kilojoules),
                    device_watts = activityObj$device_watts,
                    has_heartrate = activityObj$has_heartrate,
                    avg_heartrate = handleNull(activityObj$average_heartrate),
                    max_heartrate = handleNull(activityObj$max_heartrate),
                    calories = handleNull(activityObj$calories),
                    suffer_score = handleNull(activityObj$suffer_score),
                    has_kudoed = activityObj$has_kudoed,
                    splits_metric = handleNull(activityObj$splits_metric),
                    splits_standard = handleNull(activityObj$splits_standard),
                    best_efforts = handleNull(activityObj$best_efforts)
  )
  row
}