% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/StravaR.R
\name{parseAthleteStats}
\alias{parseAthleteStats}
\title{Parse Athlete Stats}
\usage{
parseAthleteStats(response, athlete_id)
}
\arguments{
\item{response}{An httr response object from an authenticated call to
Strava JSON API}

\item{athlete_id}{Athlete ID for user of interest}
}
\value{
out A data frame with one row per statistic reported with features
   from json object
}
\description{
Parses a given athletes stats, returning as a data frame. Note that the
max_distance and max_climb are global, so they would take the maximum of
ride, run, or swim.
}
\examples{
##Not a real token
\dontrun{me <- retrieveAthlete('abcdefghijk')}
\dontrun{me_stats <- retrieveAthleteStats('abcdefghijk', me$id[1])}
\dontrun{my_stats_df <- parseAthleteStats(me_stats, me$id[1])}
}

