% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/tournament_standings.R
\docType{data}
\name{tournament_standings}
\alias{tournament_standings}
\title{Tournament standings}
\format{
A data frame with 7 variables:
\describe{
\item{key_id}{\code{integer}. The unique ID number for the observation.}
\item{tournament_id}{\code{text}. The unique ID number for the tournament.
References \code{tournament_id} in the \code{tournaments} dataset.}
\item{tournament_name}{\code{text}. The name of the tournament.}
\item{position}{\code{integer}. The place of the team in the final
standings.}
\item{team_id}{\code{text}. The unique ID number for the team. References
\code{team_id} in the \code{teams} dataset.}
\item{team_name}{\code{text}. The name of the team.}
\item{team_code}{\code{text}. The 3-letter code for the team.}
}
}
\usage{
tournament_standings
}
\description{
This dataset records the final standings for each tournament. There is one
observation per position per tournament. The top four teams are ranked. In
most tournaments, these are the winner of the final, the loser of the
final, the winner of the third-place match, and the loser of the
third-place match. There are 7 variables and 120 observations.
}
\keyword{datasets}
