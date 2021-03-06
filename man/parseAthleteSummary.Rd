% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/StravaR.R
\name{parseAthleteSummary}
\alias{parseAthleteSummary}
\title{Parse Athlete Summary Data}
\usage{
parseAthleteSummary(response)
}
\arguments{
\item{response}{An httr response object from an authenticated call to
Strava JSON API}
}
\value{
out A single line data frame object with features from json object
}
\description{
Parses Athlete Summary response data, either summary or detailed from Strava
JSON API. Dynamically determines the level of detail from JSON object.
}
\examples{
##Not a real token
\dontrun{me <- retrieveAthlete('abcdefghijk')}
\dontrun{me_df <- parseAthleteSummary(me)}
}

