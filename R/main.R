# You can learn more about package authoring with RStudio at:
#
#   http://r-pkgs.had.co.nz/
#
# Some useful keyboard shortcuts for package authoring:
#
#   Build and Reload Package:  'Ctrl + Shift + B'
#   Check Package:             'Ctrl + Shift + E'
#   Test Package:              'Ctrl + Shift + T'

#' @title calcConcordance
#' @description Calculate concordance and discordance percentages for a logit model
#' @details Calculate the percentage of concordant and discordant pairs for a given logit model.
#' @author Selva Prabhakaran
#' @export calcConcordance
#' @param logitMod A logit model
#' @return a list containing percentage of concordant pairs, percentage discordant pairs, percentage ties and No. of pairs.
#' @examples
#' accept <- c (1, 0, 1, 0, 1, 1, 0, 0, 0,1, 0, 1, 0, 0, 1)
#' acad   <- c (66, 60, 80, 60, 52, 60, 47, 90, 75, 35, 46, 75, 66, 54, 76)
#' sports <- c (2.6,4.6,4.5, 3.3, 3.13, 4, 1.9, 3.5, 1.2, 1.8, 1, 5.1, 3.3, 5.2, 4.9)
#' rank   <- c (3, 3, 1, 4, 4, 2, 4, 4, 4, 3, 3, 3, 2, 2, 1)
#' inputData  <- data.frame (accept, acad , sports, rank) # assemble the data frame
#' logitModel <- glm(accept ~ ., family="binomial", data = inputData )
#' calcConcordance(logitModel)
calcConcordance <- function (logitMod){
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


#' @title somersD
#' @description Calculate the Somers D statistic for a given logit model
#' @details For a given logit model, Somer's D is calculated as the number of concordant pairs less number of discordant pairs divided by total number of pairs.
#' @author Selva Prabhakaran
#' @export somersD
#' @param logitMod A logit model
#' @return The Somers D statistic, which tells how many more concordant than discordant pairs exist divided by total number of pairs.
#' @examples
#' accept <- c (1, 0, 1, 0, 1, 1, 0, 0, 0,1, 0, 1, 0, 0, 1)
#' acad   <- c (66, 60, 80, 60, 52, 60, 47, 90, 75, 35, 46, 75, 66, 54, 76)
#' sports <- c (2.6,4.6,4.5, 3.3, 3.13, 4, 1.9, 3.5, 1.2, 1.8, 1, 5.1, 3.3, 5.2, 4.9)
#' rank   <- c (3, 3, 1, 4, 4, 2, 4, 4, 4, 3, 3, 3, 2, 2, 1)
#' inputData  <- data.frame (accept, acad , sports, rank) # assemble the data frame
#' logitModel <- glm(accept ~ ., family="binomial", data = inputData )
#' somersD(logitMod=logitModel)
somersD <- function(logitMod){
  conc_disc <- CalcConcordance(logitMod)
  return (conc_disc$Concordance - conc_disc$Discordance)
}


# Misclassification Error
#' @title misClassError
#' @description Calculate the percentage misclassification error for this logit model's fitted values.
#' @details For a given logit model, misclassfication error is the number of mismatches between the predicted and actuals direction of the binary y variable.
#' @author Selva Prabhakaran
#' @export misClassError
#' @param logitMod A logit model
#' @param threshold If predicted value is above the threshold, it will be considered as an event (1), else it will be a non-event (0). Defaults to 0.5.
#' @return The misclassification error, which tells what proportion of predicted direction did not match with the actuals.
#' @examples
#' accept <- c (1, 0, 1, 0, 1, 1, 0, 0, 0,1, 0, 1, 0, 0, 1)
#' acad   <- c (66, 60, 80, 60, 52, 60, 47, 90, 75, 35, 46, 75, 66, 54, 76)
#' sports <- c (2.6,4.6,4.5, 3.3, 3.13, 4, 1.9, 3.5, 1.2, 1.8, 1, 5.1, 3.3, 5.2, 4.9)
#' rank   <- c (3, 3, 1, 4, 4, 2, 4, 4, 4, 3, 3, 3, 2, 2, 1)
#' inputData  <- data.frame (accept, acad , sports, rank) # assemble the data frame
#' logitModel <- glm(accept ~ ., family="binomial", data = inputData )
#' misClassError(logitMod=logitModel)
misClassError <- function(logitMod, threshold=0.5){
  predicted_dir <- ifelse(logitMod$fitted.values < threshold, 0, 1)
  actual_dir <- logitMod$y
  return(sum(predicted_dir != actual_dir)/length(actual_dir))
}


# Sensitivity
#' @title sensitivity
#' @description Calculate the sensitivity for a given logit model.
#' @details For a given logit model, sensitivity is defined as number of observations with the event AND predicted to have the event divided by the number of observations with the event. It can be used as an indicator to gauge how sensitive is your model in detecting the occurence of events, especially when you are not so concerned about predicting the non-events as true.
#' @author Selva Prabhakaran
#' @export sensitivity
#' @param logitMod A logit model
#' @param threshold If predicted value is above the threshold, it will be considered as an event (1), else it will be a non-event (0). Defaults to 0.5.
#' @return The sensitivity of the logit model, which is, the number of observations with the event AND predicted to have the event divided by the nummber of observations with the event.
#' @examples
#' accept <- c (1, 0, 1, 0, 1, 1, 0, 0, 0,1, 0, 1, 0, 0, 1)
#' acad   <- c (66, 60, 80, 60, 52, 60, 47, 90, 75, 35, 46, 75, 66, 54, 76)
#' sports <- c (2.6,4.6,4.5, 3.3, 3.13, 4, 1.9, 3.5, 1.2, 1.8, 1, 5.1, 3.3, 5.2, 4.9)
#' rank   <- c (3, 3, 1, 4, 4, 2, 4, 4, 4, 3, 3, 3, 2, 2, 1)
#' inputData  <- data.frame (accept, acad , sports, rank) # assemble the data frame
#' logitModel <- glm(accept ~ ., family="binomial", data = inputData )
#' sensitivity(logitMod=logitModel)
sensitivity <- function(logitMod, threshold=0.5){
  predicted_dir <- ifelse(logitMod$fitted.values < threshold, 0, 1)
  actual_dir <- logitMod$y
  no_with_and_predicted_to_have_event <- sum(actual_dir == 1 & predicted_dir == 1)
  no_with_event <- sum(actual_dir == 1)
  return(no_with_and_predicted_to_have_event/no_with_event)
}

# Specificity
#' @title specificity
#' @description Calculate the specificity for a given logit model.
#' @details For a given logit model, specificity is defined as number of observations without the event AND predicted to not have the event divided by the number of observations without the event. Specificity is particularly useful when you are extra careful not to predict a non event as an event, like in spam detection where you dont want to classify a genuine mail as spam(event) where it may be somewhat ok to occasionally classify a spam as a genuine mail(a non-event).
#' @author Selva Prabhakaran
#' @export specificity
#' @param logitMod A logit model
#' @param threshold If predicted value is above the threshold, it will be considered as an event (1), else it will be a non-event (0). Defaults to 0.5.
#' @return The specificity of the logit model, which is, the number of observations without the event AND predicted to not have the event divided by the nummber of observations without the event.
#' @examples
#' accept <- c (1, 0, 1, 0, 1, 1, 0, 0, 0,1, 0, 1, 0, 0, 1)
#' acad   <- c (66, 60, 80, 60, 52, 60, 47, 90, 75, 35, 46, 75, 66, 54, 76)
#' sports <- c (2.6,4.6,4.5, 3.3, 3.13, 4, 1.9, 3.5, 1.2, 1.8, 1, 5.1, 3.3, 5.2, 4.9)
#' rank   <- c (3, 3, 1, 4, 4, 2, 4, 4, 4, 3, 3, 3, 2, 2, 1)
#' inputData  <- data.frame (accept, acad , sports, rank) # assemble the data frame
#' logitModel <- glm(accept ~ ., family="binomial", data = inputData )
#' specificity(logitMod=logitModel)
specificity <- function(logitMod, threshold=0.5){
  predicted_dir <- ifelse(logitMod$fitted.values < threshold, 0, 1)
  actual_dir <- logitMod$y
  no_without_and_predicted_to_not_have_event <- sum(actual_dir != 1 & predicted_dir != 1)
  no_without_event <- sum(actual_dir != 1)
  return(no_without_and_predicted_to_not_have_event/no_without_event)
}

# youdensIndex
#' @title youdensIndex
#' @description Calculate the specificity for a given logit model.
#' @details For a given logit model, Youden's index is calculated as sensitivity + specificity - 1
#' @author Selva Prabhakaran
#' @export youdensIndex
#' @param logitMod A logit model
#' @param threshold If predicted value is above the threshold, it will be considered as an event (1), else it will be a non-event (0). Defaults to 0.5.
#' @return The youdensIndex of the logit model, which is calculated as Sensitivity + Specificity - 1
#' @examples
#' accept <- c (1, 0, 1, 0, 1, 1, 0, 0, 0,1, 0, 1, 0, 0, 1)
#' acad   <- c (66, 60, 80, 60, 52, 60, 47, 90, 75, 35, 46, 75, 66, 54, 76)
#' sports <- c (2.6,4.6,4.5, 3.3, 3.13, 4, 1.9, 3.5, 1.2, 1.8, 1, 5.1, 3.3, 5.2, 4.9)
#' rank   <- c (3, 3, 1, 4, 4, 2, 4, 4, 4, 3, 3, 3, 2, 2, 1)
#' inputData  <- data.frame (accept, acad , sports, rank) # assemble the data frame
#' logitModel <- glm(accept ~ ., family="binomial", data = inputData )
#' youdensIndex(logitMod=logitModel)
youdensIndex <- function(logitMod, threshold=0.5){
  Sensitivity <- sensitivity(logitMod, threshold = threshold)
  Specificity <- specificity(logitMod, threshold = threshold)
  return(Sensitivity + Specificity - 1)
}


# confusionMatrix
#' @title confusionMatrix
#' @description Calculate the confusion matrix for the fitted values for a logistic regression model.
#' @details For a given logit model, the confusion matrix showing the count of predicted events and non-events against actual events and non events.
#' @author Selva Prabhakaran
#' @export confusionMatrix
#' @param logitMod A logit model
#' @param threshold If predicted value is above the threshold, it will be considered as an event (1), else it will be a non-event (0). Defaults to 0.5.
#' @return For a given logit model, returns the confusion matrix showing the count of predicted events and non-events against actual events and non events.
#' @examples
#' accept <- c (1, 0, 1, 0, 1, 1, 0, 0, 0,1, 0, 1, 0, 0, 1)
#' acad   <- c (66, 60, 80, 60, 52, 60, 47, 90, 75, 35, 46, 75, 66, 54, 76)
#' sports <- c (2.6,4.6,4.5, 3.3, 3.13, 4, 1.9, 3.5, 1.2, 1.8, 1, 5.1, 3.3, 5.2, 4.9)
#' rank   <- c (3, 3, 1, 4, 4, 2, 4, 4, 4, 3, 3, 3, 2, 2, 1)
#' inputData  <- data.frame (accept, acad , sports, rank) # assemble the data frame
#' logitModel <- glm(accept ~ ., family="binomial", data = inputData )
#' confusionMatrix(logitMod=logitModel)
confusionMatrix <- function(logitMod, threshold=0.5){
  predicted_dir <- ifelse(logitMod$fitted.values < threshold, 0, 1)
  actual_dir <- logitMod$y
  return (as.data.frame.matrix(table(predicted_dir, actual_dir)))
}

# kappaCohen
#' @title kappaCohen
#' @description Calculate the Cohen's kappa statistic for a given logit model.
#' @details For a given logit model, Cohen's kappa is calculated. Cohen's kappa is calculated as (probabiliity of agreement - probability of expected) / (1-(probability of expected)))
#' @author Selva Prabhakaran
#' @export kappaCohen
#' @param logitMod A logit model
#' @param threshold If predicted value is above the threshold, it will be considered as an event (1), else it will be a non-event (0). Defaults to 0.5.
#' @return The Cohen's kappa of the logit model
#' @examples
#' accept <- c (1, 0, 1, 0, 1, 1, 0, 0, 0,1, 0, 1, 0, 0, 1)
#' acad   <- c (66, 60, 80, 60, 52, 60, 47, 90, 75, 35, 46, 75, 66, 54, 76)
#' sports <- c (2.6,4.6,4.5, 3.3, 3.13, 4, 1.9, 3.5, 1.2, 1.8, 1, 5.1, 3.3, 5.2, 4.9)
#' rank   <- c (3, 3, 1, 4, 4, 2, 4, 4, 4, 3, 3, 3, 2, 2, 1)
#' inputData  <- data.frame (accept, acad , sports, rank) # assemble the data frame
#' logitModel <- glm(accept ~ ., family="binomial", data = inputData )
#' kappaCohen(logitMod=logitModel)
kappaCohen <- function(logitMod, threshold=0.5){
  conf <- confusionMatrix(logitMod)
  prob_agreement <- conf[1, 1] + conf[2, 2]
  prob_expected <- sum(conf[2, ])/sum(conf) * sum(conf[, 2])/sum(conf)   # probability of actual 'yes' * probability of predicting 'yes'.
  return((prob_agreement - prob_expected)/(1-(prob_expected)))
}

# Compute specificity and sensitivity
getSpecSens<- function(logitmod, threshold=threshold){
  return(list(1-specificity(logitMod = logitmod, threshold=threshold),
              sensitivity(logitMod = logitMod, threshold=threshold)))
}


# plotROC
#' @title plotROC
#' @description Plot the Receiver Operating Characteristics(ROC) Curve based on ggplot2
#' @details For a given logit model, A ROC curve is plotted using the ggplot2 framework along the the area under the curve.
#' @author Selva Prabhakaran
#' @export plotROC
#' @param logitMod A logit model
#' @return Plots the ROC curve
#' @import ggplot2
#' @examples
#' accept <- c (1, 0, 1, 0, 1, 1, 0, 0, 0,1, 0, 1, 0, 0, 1)
#' acad   <- c (66, 60, 80, 60, 52, 60, 47, 90, 75, 35, 46, 75, 66, 54, 76)
#' sports <- c (2.6,4.6,4.5, 3.3, 3.13, 4, 1.9, 3.5, 1.2, 1.8, 1, 5.1, 3.3, 5.2, 4.9)
#' rank   <- c (3, 3, 1, 4, 4, 2, 4, 4, 4, 3, 3, 3, 2, 2, 1)
#' inputData  <- data.frame (accept, acad , sports, rank) # assemble the data frame
#' logitModel <- glm(accept ~ ., family="binomial", data = inputData )
#' plotROC(logitMod=logitModel)
plotROC <- function(logitMod){
  # create the x and y axis values in a df
  df <- as.data.frame(matrix(numeric(51*2), ncol=2))# initialise
  names(df) <- c("One_minus_specificity", "sensitivity")  # give col names.
  rowcount = 1
  for (threshold in seq(1, 0, by=-0.02)){
    df[rowcount, ] <- getSpecSens(logitmod=logitMod, threshold=threshold)
    rowcount <- rowcount + 1
  }

  AREAROC <- aROC(logitMod)  # compute area under ROC

  df <- data.frame(df, Threshold=seq(1, 0, by=-0.02))  # append threshold

  df$Threshold.show <- rep(NA, nrow(df))
  # Adding Thresholds to show.
  for (rownum in c(2:nrow(df))){
    if(df[rownum, 1] != df[rownum-1, 1]  |  df[rownum, 2] != df[rownum-1, 2]){
      df$Threshold.show[rownum] <-  df$Threshold[rownum]
    }
  }

  # Plot it
  bp <- ggplot(df, aes(One_minus_specificity, sensitivity, label=Threshold.show))
  bp + geom_ribbon(color="#3399FF", fill="#3399FF", aes(ymin=0, ymax=sensitivity)) +
    labs(title="ROC", x="1-Specificity", y="Sensitivity") +
    annotate("text", label=paste("Area Under ROC:", round(AREAROC, 2)), x=0.75, y=0.35, colour="white") +
    geom_text(aes(size=0.5))
}


# Compute auROC
# aROC
#' @title aROC
#' @description Calculate the area uder ROC curve statistic for a given logit model.
#' @details For a given logit model, the area under the ROC curve shows how well the model performs at capturing the false events and false non-events. An best case model will have an area of 1. However that would be unrealistic, so the closer the aROC to 1, the better is the model.
#' @author Selva Prabhakaran
#' @export aROC
#' @param logitMod A logit model
#' @return The area under the ROC curve for a given logit model.
#' @examples
#' accept <- c (1, 0, 1, 0, 1, 1, 0, 0, 0,1, 0, 1, 0, 0, 1)
#' acad   <- c (66, 60, 80, 60, 52, 60, 47, 90, 75, 35, 46, 75, 66, 54, 76)
#' sports <- c (2.6,4.6,4.5, 3.3, 3.13, 4, 1.9, 3.5, 1.2, 1.8, 1, 5.1, 3.3, 5.2, 4.9)
#' rank   <- c (3, 3, 1, 4, 4, 2, 4, 4, 4, 3, 3, 3, 2, 2, 1)
#' inputData  <- data.frame (accept, acad , sports, rank) # assemble the data frame
#' logitModel <- glm(accept ~ ., family="binomial", data = inputData )
#' aROC(logitMod=logitModel)
aROC <- function(logitMod){
  # create the x and y axis values in a df
  df <- as.data.frame(matrix(numeric(51*2), ncol=2))# initialise
  names(df) <- c("One_minus_specificity", "sensitivity")  # give col names.
  rowcount = 1
  for (threshold in seq(1, 0, by=-0.02)){
    df[rowcount, ] <- getSpecSens(logitmod=logitMod, threshold=threshold)
    rowcount <- rowcount + 1
  }

  df <- data.frame(df, Threshold=seq(1, 0, by=-0.02))  # append threshold

  # Compute aROC.
  auROC <- 0  # initialise
  for(point in c(2:nrow(df))) {
    x1 <- df[point-1, 1]
    x2 <- df[point, 1]
    y1 <- df[point-1, 2]
    y2 <- df[point, 2]
    # cat("x1, x2, y1, y2:", x1, x2, y1, y2)

    # compute rect_area
    rect_x <- x2 - x1
    rect_y <- y1
    rect_area <- rect_x * rect_y
    # cat("rect_x, rect_y, rect_area:", rect_x, rect_y, rect_area)

    # compute area of head triangle
    triangle_area <- rect_x * (y2-y1) * 0.5
    currArea <- rect_area + triangle_area
    auROC <- auROC + currArea
  }
  totalArea <- (max(df[, 1]) * max(df[, 2]))
  return(auROC/totalArea)  # auROC/totalArea
}

# Compute WOE
library(ISLR)
data("OJ")
head(OJ)
X <- factor(as.character(OJ$STORE))  # prepares X
Y <- as.character(OJ$Purchase)
valueOfGood <- "CH"  # input argument

getWOETable <- function(X=X, Y=Y, valueOfGood=1){
  yClasses <- unique(Y)
  if(length(yClasses) == 2) {  # ensure it is binary
    # covert good's to 1 and bad's to 0.
    Y[which(Y==valueOfGood)] <- 1
    Y[which(!(Y=="1"))] <- 0
    Y <- as.numeric(Y)
    df <- data.frame(X, Y)

    # Create WOE table
    woeTable <- as.data.frame(matrix(numeric(nlevels(X) * 8), nrow=nlevels(X), ncol=8))
    names(woeTable) <- c("CAT", "GOODS", "BADS", "TOTAL", "PCT_G", "PCT_B", "WOE", "IV")
    woeTable$CAT <- levels(X)  # load categories to table.

    # Load the number of goods and bads within each category.
    for(catg in levels(X)){  # catg => current category
      woeTable[woeTable$CAT == catg, c(3, 2)] <- table(Y[X==catg])  # assign the good and bad count for current category.
      woeTable[woeTable$CAT == catg, "TOTAL"] <- sum(X==catg)
    }

    woeTable$PCT_G <- woeTable$GOODS/sum(woeTable$GOODS)  # compute % good
    woeTable$PCT_B <- woeTable$BADS /sum(woeTable$BADS)  # compute % bad
    woeTable$WOE <- log(woeTable$PCT_G / woeTable$PCT_B)  # compute WOE
    woeTable$IV <- (woeTable$PCT_G - woeTable$PCT_B) * woeTable$WOE  # compute IV
    attr(woeTable, "iValue") <- sum(woeTable$IV)  # assign iv as attribute..
    return(woeTable)
  } else {
    cat("WOE can't be computed because the Y is not binary.")
  }
}

getWOETable(X, Y, valueOfGood = "CH")

# iv



