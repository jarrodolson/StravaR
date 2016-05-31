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

## Contains commands to retrieve data

#'Check HTTR Response for Errors
#'
#'\code{checkErrors} raises a warning if there was a problem with the response
#'status code (i.e. it was not 200)
#'
#'@param response An httr response object
checkErrors <- function(response){
  status <- httr::http_status(response)
  if (status$category != "Success"){
    msg <- paste("API Call Failed. Full message is:",
                 "Category::", status$category,
                 "; Reason::", status$reason,
                 "; Message::", status$message)
    warning(msg)
  }
}

#' Make Query Call
#' 
#' Takes a query, attaches the access token, checks for errors, checks for rate
#' limits and returns the response object.
#' 
#' @param query An appropriate API call, without access token for Strava API
#' @param token A character string of the user's access token
#' @return result An httr response object from the Strava API call
makeQuery <- function(query, token){
  result <- httr::GET(query, query = list('access_token' = token))
  checkErrors(result)
  rate <- checkRateLimit(result)
  result
}

#'Retrieve Athlete Information from API
#'
#'\code{retrieveAthlete} fetches athlete information from the Strava API, checks
#'for errors and the current rate limit, and return the result (full response).
#'Will raise warnings if the response status is not 200, and also if the user
#'has used 90% of their API query limit (short term or long term).
#'
#'@param token String value for user access token to API
#'@param athlete_id (optional) Athlete ID, if retreiving non-authenticated user
#'@return result An httr response object, unparsed
#'@examples
#'##Not a real token
#'\dontrun{me <- retrieveAthlete('abcdefghijk')}
#'\dontrun{someoneElse <- retrieveAthlete('abcdefghijk', athlete_id='1234')}
#'@export
retrieveAthlete <- function(token, athlete_id = NULL){
  base <- 'https://www.strava.com/api/v3/'
  if (is.null(athlete_id)){
    query <- paste0(base, "athlete")
  } else {
    query <- paste0(base, "athletes/", athlete_id)
  }
  makeQuery(query, token)
}

#'Retrieves Athlete Stats
#'
#'@param token String value for user access token to API
#'@param athlete_id Athlete ID for user of interest
#'@return result An httr response object, unparsed
#'@export
retrieveAthleteStats <- function(token, athlete_id){
  query <- paste0('https://www.strava.com/api/v3/athletes/', athlete_id, '/stats')
  makeQuery(query, token)
}

#'Retrieve Activity Stream
#'
#'Takes an activity id and returns the stream (raw data) of the activity
#'
#'@param token String value for user access token to API
#'@param activity_id (optional) Athlete ID, if retreiving non-authenticated user
#'@return result An httr response object, unparsed
#'@export
retrieveActivityStream <- function(token, activity_id){
  query <- paste0('https://www.strava.com/api/v3/activities/',
                  activity_id,
                  '/streams/time,latlng,distance,altitude,velocity_smooth,heartrate,cadence,watts,temp,moving,grade_smooth')
  makeQuery(query, token)
}

#'Retrieve All Activity Streams Into Data Frame
#'
#'Reads through an activity data frame, making a request to API for each
#'activity id, and combines it into a tidy data frame with a unique record
#'for each observation and activity id.
#'
#'@param token String value for user access token to API
#'@param activity_df A dataframe object with all available fields
#'@return data A dataframe object with all available fields, stacking by
#'  activity id
#'@examples
#'token <- readToken('path/to/token.txt')
#'activity_response <- retrieveAthleteActivities(token)
#'activity_df <- parseAthleteActivities(activity_response)
#'activity_stream_df <- retrieveAllActivityStreamsIntoDf(token, activity_df)
#'@export
retrieveAllActivityStreamsIntoDf <- function(token, activity_df){
  cat("Retrieving activity streams, this could take a while...")
  data <- NULL
  for (r in 1:nrow(activity_df)){
    if (!is.na(activity_df$start_lat[r])){
      activity_id <- activity_df$id[r]
      temp_stream <- retrieveActivityStream(token, activity_id)
      temp_stream_df <- parseActivityStream(temp_stream)
      temp_stream_df$activity_id <- activity_id
      if (is.null(data)){
        data <- temp_stream_df
      } else {
        data <- rbind(data, temp_stream_df)
      }
    } else {
      warning(
        paste("One record skipped due to missing lat/lng data:", activity_id)
      )
    }
  }
  data
}

#'Retrieve Athlete Activities
#'
#'Retrieves athlete activities, in one hit. If it will be more than 300
#'activities, the function will need to be modified.
#'
#'@param token String value for user access token to API
#'@param before (optional) Integer value representing seconds since epoch
#'@param after (optional) Integer value representing seconds since epoch
#'@return result An httr response object, unparsed
#'@export
retrieveAthleteActivities <- function(token, before = NULL, after = NULL){
  query <- "https://www.strava.com/api/v3/athlete/activities"
  params <- list("access_token" = token, "per_page" = 200)
  if (!is.null(before) & !is.null(after)){
    stop("before and after are not both allowed in the api call")
  }
  if (!is.null(before)) {
    params <- c(params, list("before" = before))
  }
  if (!is.null(after)) {
    params <- c(params, list("after" = after))
  }
  result <- httr::GET(query, query = params)
  checkErrors(result)
  rate <- checkRateLimit(result)
  result
}

#'Retrieve Specific Athlete Activity
#'
#'Using activity_id, downloads the detailed resource_state for an activity
#'
#'@param token String value for user access token to API
#'@param activity_id Unique identifier for the activity, as an integer or string
#'@return An unparsed HTTR response object
#'@export
retrieveAthleteActivitySpecific <- function(token, activity_id){
  query <- paste0('https://www.strava.com/api/v3/activities/', activity_id)
  makeQuery(query, token)
}

#'Update Activities with Detail
#'
#'Uses activity summary data frame and then queries each indiidual activity
#'to get a detailed (and if changed, updated) record for that activity. It
#'replaces the old records and returns it as a dataframe.
#'
#'@param token String value for user access token to API
#'@param activity_df Dataframe containing parsed activities
#'@return activity_df Dataframe with updated, detailed parsed activities
#'@export
updateActivitiesWithDetail <- function(token, activity_df){
  out <- NULL
  for (r in 1:nrow(activity_df)){
    activity_id <- activity_df$id[[r]]
    new <- retrieveAthleteActivitySpecific(token, activity_id)
    new_parsed <- parseAthleteActivity(httr::content(new))
    if (is.null(out)){
      out <- new_parsed
    } else {
      out <- rbind(out, new_parsed)
    }
  }
  out
}