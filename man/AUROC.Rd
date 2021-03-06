% Generated by roxygen2 (4.1.1): do not edit by hand
% Please edit documentation in R/Main.R
\name{AUROC}
\alias{AUROC}
\title{AUROC}
\usage{
AUROC(actuals, predictedScores)
}
\arguments{
\item{actuals}{The actual binary flags for the response variable. It can take values of either 1 or 0, where 1 represents the 'Good' or 'Events' while 0 represents 'Bad' or 'Non-Events'.}

\item{predictedScores}{The prediction probability scores for each observation.}
}
\value{
The area under the ROC curve for a given logit model.
}
\description{
Calculate the area uder ROC curve statistic for a given logit model.
}
\details{
For a given actuals and predicted probability scores, the area under the ROC curve shows how well the model performs at capturing the false events and false non-events. An best case model will have an area of 1. However that would be unrealistic, so the closer the aROC to 1, the better is the model.
}
\examples{
data('ActualsAndScores')
AUROC(actuals=ActualsAndScores$Actuals, predictedScores=ActualsAndScores$PredictedScores)
}
\author{
Selva Prabhakaran \email{selva86@gmail.com}
}

