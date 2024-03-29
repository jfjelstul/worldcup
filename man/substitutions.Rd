% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/substitutions.R
\docType{data}
\name{substitutions}
\alias{substitutions}
\title{Substitutions}
\format{
A data frame with 24 variables:
\describe{
\item{key_id}{\code{integer}. The unique ID number for the observation.}
\item{substitution_id}{\code{text}. The unique ID number for the
substitution. Has the format \code{S-####}, where the number is a counter
that is assigned with the data sorted by the match ID, then the minute of
the substitution, then whether the player is going off.}
\item{tournament_id}{\code{text}. The unique ID number for the tournament.
References \code{tournament_id} in the \code{tournaments} dataset.}
\item{tournament_name}{\code{text}. The name of the tournament.}
\item{match_id}{\code{text}. The unique ID number for the match in which
the substitution occurred. References \code{match_id} in the \code{matches}
dataset.}
\item{match_name}{\code{text}. The name of the match in which the
substitution occurred.}
\item{match_date}{\code{date}. The date of the match in the format
\code{YYYY-MM-DD}.}
\item{stage_name}{\code{enum}. The stage of the tournament in which the
match occurred. The possible values are: \code{first round}, \code{second
round}, \code{group stage}, \code{round of sixteen}, \code{quarter-finals},
\code{semi-finals}, \code{third place match}, \code{final}. Note that not
all values are applicable to all tournaments.}
\item{group_name}{\code{text}. The name of the group.}
\item{team_id}{\code{text}. The unique ID number for the team of the player
who was substituted. References \code{team_id} in the \code{teams}
dataset.}
\item{team_name}{\code{text}. The name of the team of the player who was
substituted.}
\item{team_code}{\code{text}. The 3-letter code for the team of the player
who was substituted.}
\item{home_team}{\code{boolean}. Whether the team was the home team. Coded
\code{1} if the team was the home team and \code{0} otherwise.}
\item{away_team}{\code{boolean}. Whether the team was the away team. Coded
\code{1} if the team was the away team and \code{0} otherwise.}
\item{player_id}{\code{text}. The unique ID number for the player who was
substituted. References \code{player_id} in the \code{players} dataset.}
\item{family_name}{\code{text}. The family name of the player who was
substituted.}
\item{given_name}{\code{text}. The given name of the player who was
substituted.}
\item{shirt_number}{\code{integer}. The shirt number of the player who was
substituted.}
\item{minute_label}{\code{text}. The minute of the match in which the
substitution occurred in the format \code{#'} or \code{#'+#'}.}
\item{minute_regulation}{\code{integer}. The minute of regulation time in
which the substitution occurred.}
\item{minute_stoppage}{\code{integer}. The minute of stoppage time in which
the substitution occurred. Coded \code{0} if the substitution did not occur
during stoppage time.}
\item{match_period}{\code{enum}. The period of the match in which the
substitution occurred. The possible values are: \code{first half},
\code{first half, stoppage time}, \code{second half}, \code{second half,
stoppage time}, \code{extra time, first half}, \code{extra time, first
half, stoppage time}, \code{extra time, second half}, \code{extra time,
second half, stoppage time}, \code{after extra time}.}
\item{going_off}{\code{boolean}. Whether the player was going off the
field. Coded \code{1} if the player was going off and \code{0} otherwise.}
\item{coming_on}{\code{boolean}. Whether the player was coming on the
field. Coded \code{1} if the player was coming on and \code{0} otherwise.}
}
}
\usage{
substitutions
}
\description{
This dataset records all substitutions since 1970. FIFA match reports do
not include information about substitutions before 1970. There is one
observation per player per substitution. It indicates the minute of the
substitution, the player who went off, and the player who came on. There
are 24 variables and 10222 observations.
}
\keyword{datasets}
