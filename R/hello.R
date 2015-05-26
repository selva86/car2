# Hello, world!
#
# This is an example function named 'hello'
# which prints 'Hello, world!'.
#
# You can learn more about package authoring with RStudio at:
#
#   http://r-pkgs.had.co.nz/
#
# Some useful keyboard shortcuts for package authoring:
#
#   Build and Reload Package:  'Ctrl + Shift + B'
#   Check Package:             'Ctrl + Shift + E'
#   Test Package:              'Ctrl + Shift + T'

#' @title CalcConcordance
#' @description Calculate concordance and discordance percentages for a logit model
#' @details Calculate the percentage of concordant and discordant pairs for a given logit model.
#' @aliases CalcConcordance, CalcDiscordance, CalcConcordanceDiscordance
#' @author Selva Prabhakaran
#' @export CalcConcordance
#' @param logitMod A logit model
#' @return a list containing percentage of concordant pairs, percentage discordant pairs, percentage ties and No. of pairs.
#' @examples
#' accept <- c (1, 0, 1, 0, 1, 1, 0, 0, 0,1, 0, 1, 0, 0, 1)
#' acad   <- c (66, 60, 80, 60, 52, 60, 47, 90, 75, 35, 46, 75, 66, 54, 76)
#' sports <- c (2.6,4.6,4.5, 3.3, 3.13, 4, 1.9, 3.5, 1.2, 1.8, 1, 5.1, 3.3, 5.2, 4.9)
#' rank   <- c (3, 3, 1, 4, 4, 2, 4, 4, 4, 3, 3, 3, 2, 2, 1)
#' inputData  <- data.frame (accept, acad , sports, rank) # assemble the data frame
#' logitModel <- glm(accept ~ ., family="binomial", data = inputData )
#' CalcConcordance(logitModel)
CalcConcordance <- function (logitMod){
  fitted <- data.frame (cbind (logitMod$y, logitMod$fitted.values)) # actuals and fitted
  colnames(fitted) <- c('response','score') # rename columns
  ones <- fitted[fitted$response==1, ] # Subset ones
  zeros <- fitted[fitted$response==0, ] # Subsetzeros
  totalPairs <- nrow (ones) * nrow (zeros) # calculate total number of pairs to check
  conc <- sum (c (vapply (ones$score, function(x) {((x > zeros$score))}, FUN.VALUE=logical(nrow(zeros)))))
  disc <- totalPairs - conc

  # Calc concordance, discordance and ties
  concordance <- conc/totalPairs
  discordance <- disc/totalPairs
  tiesPercent <- (1-concordance-discordance)
  return(list("Concordance"=concordance, "Discordance"=discordance,
              "Tied"=tiesPercent, "Pairs"=totalPairs))
}

#' @title SomersD
#' @description Calculate the Somers D statistic for a given logit model
#' @details For a given logit model, Somer's D is calculated as the number of concordant pairs less number of discordant pairs divided by total number of pairs.
#' @aliases SomersDistance
#' @author Selva Prabhakaran
#' @export SomersD
#' @param logitMod A logit model
#' @return The Somers D statistic, which tells how many more concordant than discordant pairs exist divided by total number of pairs.
#' @examples
#' accept <- c (1, 0, 1, 0, 1, 1, 0, 0, 0,1, 0, 1, 0, 0, 1)
#' acad   <- c (66, 60, 80, 60, 52, 60, 47, 90, 75, 35, 46, 75, 66, 54, 76)
#' sports <- c (2.6,4.6,4.5, 3.3, 3.13, 4, 1.9, 3.5, 1.2, 1.8, 1, 5.1, 3.3, 5.2, 4.9)
#' rank   <- c (3, 3, 1, 4, 4, 2, 4, 4, 4, 3, 3, 3, 2, 2, 1)
#' inputData  <- data.frame (accept, acad , sports, rank) # assemble the data frame
#' logitModel <- glm(accept ~ ., family="binomial", data = inputData )
#' SomersD(logitMod=logitModel)
SomersD <- function(logitMod){
  conc_disc <- CalcConcordance(logitMod)
  return (conc_disc$Concordance - conc_disc$Discordance)
}
