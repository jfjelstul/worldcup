################################################################################
# Joshua C. Fjelstul, Ph.D.
# worldcup R package
# automatically generated by the codebookr R package
################################################################################

#' Referee appointments
#' 
#' This dataset records all referee appointments. There is one observation per
#' referee per tournament. This dataset only includes the main referee, not
#' assistant referees, fourth officials, or video assistant referees.There are
#' 10 variables and 668 observations.
#' 
#' @format A data frame with 10 variables:
#' \describe{
#' \item{key_id}{\code{integer}. The unique ID number for the observation.}
#' \item{tournament_id}{\code{text}. The unique ID number for the tournament.
#' References \code{tournament_id} in the \code{tournaments} dataset.}
#' \item{tournament_name}{\code{text}. The name of the tournament.}
#' \item{referee_id}{\code{text}. The unique ID number for the referee.
#' References \code{referee_id} in the \code{referees} dataset.}
#' \item{family_name}{\code{text}. The family name of the referee.}
#' \item{given_name}{\code{text}. The given name fo the referee.}
#' \item{country_name}{\code{text}. The name of the referee's home country.}
#' \item{confederation_id}{\code{text}. The unique ID number for the
#' confederation. References \code{confederation_id} in the
#' \code{confederations} dataset.}
#' \item{confederation_name}{\code{text}. The name of the confederation.}
#' \item{confederation_code}{\code{text}. The abbreviation for the
#' confederation.}
#' }
"referee_appointments"

################################################################################
# end R script
################################################################################

