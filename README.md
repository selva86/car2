# car2
Extends capabilities of 'car' to include companion functions for logistic regression

To install:
   The latest development version: library(devtools); install.github("car2", "selva86");

The following functions are covered in the package:

1. AUROC: Computes Area under ROC curve.
2. Concordance: Calculates the concordance and discordance statistic.
3. confusionMatrix: Compute the Good / Bad match matrix.
4. IV: Compute the information value for a given categorical variable.
5. kappaCohen: Computes the kappaCohen statistic, an indicator of the predictive power of the model.
6. misClassError: Computes the percentage misclassification error.
7. plotROC: Plots a beautiful ROC curve using the ggplot2 framework.
8. sensitivity: Computes the True Positive rate.
9. SimData: A data with binary response variable (1/0) and a categorical X variable, used to demo the calculation of WOE.
10. somersD: Computes somersD statistic which equals to Concordance - Discordance.
11. Specificity: Computes the specificity of the given Actuals (1/0's) and the predicted probability scores and given threshold. It representative of prediction coverage of non-events, i.e. what portion of the non-events were predicted correctly.
12. WOE: The weights of evidence of a given categorical X variable againsts as given binary response (1/0's).
13. WOETable: Compute the WOE table showing the WOE and IV values for each group in the categorical variable.
14. youdensIndex: Computes the youden's Index statistic.
