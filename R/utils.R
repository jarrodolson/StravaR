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

##Contains utility functions useful or interacting with Strava data

#'Convert Meters Per Second to Miles Per Hour
#'
#'@param vect Numeric vector measured in meters per second
#'@export
convertMetersPerSecondToMph <- function(vect){
  ##Starts in meters/second
  vect*2.23694##google conversion ratio
}

#'Convert Meters to Miles
#'
#'@param vect Numeric vector measured in meters
#'@export
convertMetersToMiles <- function(vect){
  ##starts in meters
  vect*0.000621371##google conversion ratio
}

#'Convert Celsius to Fahrenheit
#'
#'@param vect Numeric vector in celsius degrees
#'@export
convertCelsiusToFahrenheit <- function(vect){
  ##starts in celsius
  vect*1.8+32
}

#'Check Rate Limit Status
#'
#'Raises a warning if the user has used more than 90% of short or
#'long term rate limit
#'
#'@param response An httr response object
#'@export
checkRateLimit <- function(response){
  limitLi <- as.numeric(
    strsplit(httr::headers(response)[["x-ratelimit-limit"]], ",")[[1]]
  )
  usageLi <- as.numeric(
    strsplit(httr::headers(response)[["x-ratelimit-usage"]], ",")[[1]]
  )
  shortTerm <- (usageLi[1]/limitLi[1])*100
  longTerm <- (usageLi[1]/limitLi[1])*100
  if (shortTerm > 90){
    warning(paste("You have used more than 90% of your short term limit.",
                  "See: http://strava.github.io/api/#access"))
  }
  if (longTerm > 90) {
    warning(paste("You have used more than 90% of your long term limit.",
                  "See: http://strava.github.io/api/#access"))
  }
  list('short' = shortTerm, "long" = longTerm)
}

#'Read User Access Token From File
#'
#'@param filename String containing path to file containing plain text token
#'@return token String value for the token
#'
#'@examples
#'\dontrun{token <- readToken(/path/to/file.txt)}
#'@export
readToken <- function(filename){
  token <- scan(filename, what = "character", strip.white = TRUE)
  token
}