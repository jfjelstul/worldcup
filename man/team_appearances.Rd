% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/team_appearances.R
\docType{data}
\name{team_appearances}
\alias{team_appearances}
\title{Team appearances}
\format{
A data frame with 36 variables:
\describe{
\item{key_id}{\code{integer}. The unique ID number for the observation.}
\item{tournament_id}{\code{text}. The unique ID number for the tournament.
References \code{tournament_id} in the \code{tournaments} dataset.}
\item{tournament_name}{\code{text}. The name of the tournament.}
\item{match_id}{\code{text}. The unique ID number for the match. References
\code{match_id} in the \code{matches} dataset.}
\item{match_name}{\code{text}. The name of the match.}
\item{stage_name}{\code{enum}. The stage of the tournament in which the
match occurred. The possible values are: \code{first round}, \code{second
round}, \code{group stage}, \code{round of sixteen}, \code{quarter-finals},
\code{semi-finals}, \code{third place match}, \code{final}. Note that not
all values are applicable to all tournaments.}
\item{group_name}{\code{text}. The name of the group.}
\item{group_stage}{\code{boolean}. Whether the match is a group stage
match. Coded \code{1} if the match is a group stage match and \code{0}
otherwise.}
\item{knockout_stage}{\code{boolean}. Whether the match is a knockout stage
match. Coded \code{1} if the match is a knockout stage match and \code{0}
otherwise.}
\item{replayed}{\code{boolean}. Whether the match was replayed. Coded
\code{1} if the match was replayed and \code{0} otherwise.}
\item{replay}{\code{boolean}. Whether the match was a replay. Coded
\code{1} if the match was a replay and \code{0} otherwise.}
\item{match_date}{\code{date}. The date of the match in the format
\code{YYYY-MM-DD}.}
\item{match_time}{\code{integer}. The time of the match in the format
\code{HH:MM}.}
\item{stadium_id}{\code{text}. The unique ID number for the stadium.
References \code{stadium_id} in the \code{stadiums} dataset.}
\item{stadium_name}{\code{text}. The name of the stadium.}
\item{city_name}{\code{text}. The city in which the match was played.}
\item{country_name}{\code{text}. The name of the country in which the match
was played.}
\item{team_id}{\code{text}. The unique ID number for the team. References
\code{team_id} in the \code{teams} dataset.}
\item{team_name}{\code{text}. The name of the team.}
\item{team_code}{\code{text}. The 3-letter code for the team.}
\item{opponent_id}{\code{text}. The unique ID number for the team's
opponent. References \code{team_id} in the \code{teams} dataset.}
\item{opponent_name}{\code{text}. The name of the team's opponent.}
\item{opponent_code}{\code{text}. The 3-letter code for the team's
opponent.}
\item{home_team}{\code{boolean}. Whether the team was the home team. Coded
\code{1} if the team was the home team and \code{0} otherwise.}
\item{away_team}{\code{boolean}. Whether the team was the away team. Coded
\code{1} if the team was the away team and \code{0} otherwise.}
\item{goals_for}{\code{integer}. The number of goals scored by the team.}
\item{goals_against}{\code{integer}. The number of goals scored against the
team.}
\item{goal_differential}{\code{integer}. The team's goal differential.}
\item{extra_time}{\code{boolean}. Whether the match went to extra time.
Coded \code{1} if the match went to extra time and \code{0} otherwise.}
\item{penalty_shootout}{\code{boolean}. Whether the match ended in a
penalty shootout. Coded \code{1} if the match ended in a penalty shootout
and \code{0} otherwise.}
\item{penalties_for}{\code{integer}. The number of penalties scored by the
opponent, if the match ended in a penalty shootout. Coded \code{0} if there
was not a shootout.}
\item{penalties_against}{\code{integer}. The number of penalties scored by
the team, if the match ended in a penalty shootout. Coded \code{0} if there
was not a shootout.}
\item{result}{\code{enum}. The result of the match. The possible values
are: \code{home team win}, \code{away team win}, \code{draw},
\code{replayed}.}
\item{win}{\code{boolean}. Whether the team won the match. Coded \code{1}
if the team won the match and \code{0} otherwise.}
\item{lose}{\code{boolean}. Whether the team lost the match. Coded \code{1}
if the team lost the match and \code{0} otherwise.}
\item{draw}{\code{boolean}. Whether the match ended in a draw. Coded
\code{1} of the match ended in a draw and \code{0} otherwise.}
}
}
\usage{
team_appearances
}
\description{
This dataset records all team appearances. There is one observation per
team per match per tournament. It includes whether the team is the home
team or the away team, the number of goals for and against, the goal
difference, whether there was a penalty shootout, penalties for and against
(if applicable), and whether the team wins, loses, or draws. There are 37
variables and 2496 observations.
}
\keyword{datasets}
