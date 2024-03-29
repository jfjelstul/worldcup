% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/stadiums.R
\docType{data}
\name{stadiums}
\alias{stadiums}
\title{Stadiums}
\format{
A data frame with 8 variables:
\describe{
\item{key_id}{\code{integer}. The unique ID number for the observation.}
\item{stadium_id}{\code{text}. The unique ID number for the stadium. Has
the format \code{S-###}, where the number is a count that is assigned with
the data sorted by country, then by city, then by the name of the stadium.}
\item{stadium_name}{\code{text}. The name of the stadium.}
\item{city_name}{\code{text}. The city in which the match was played.}
\item{country_name}{\code{text}. The name of the country in which the
stadium is located.}
\item{stadium_capacity}{\code{integer}. The approximate capacity of the
stadium.}
\item{stadium_wikipedia_link}{\code{text}. The Wikipedia link for the
stadium.}
\item{city_wikipedia_link}{\code{text}. The Wikipedia link for the city in
which the match was played.}
}
}
\usage{
stadiums
}
\description{
This dataset records all stadiums that have hosted a World Cup match. There
is one observation per stadium. It includes the country and city of the
stadium, the approximate capacity of the stadium, and the link to the
Wikipedia pages for the city and the stadium. There are 8 variables and 240
observations.
}
\keyword{datasets}
