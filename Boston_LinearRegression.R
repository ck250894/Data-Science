# Load standard libraries
library(tidyverse)
library(broom)
library(ggplot2)
library(MASS) # Modern applied statistics functions

library(MASS)
df <- Boston


ncol(df)
nrow(df)
colSums(is.na(df))
str(df)


#There are no Null values in the boston dataset. 

#The following variables are in the dataset. 
# 1. crim - per capita crime rate by town.
# 2. zn - proportion of residential land zoned for lots over 25,000 sq.ft.
# 3. indus -  proportion of non-retail business acres per town. 
# 4. chas - Charles River dummy variable (= 1 if tract bounds river; 0 otherwise).
# 5. nox - nitrogen oxides concentration (parts per 10 million).
# 6. rm - average number of rooms per dwelling.
# 7. age -  proportion of owner-occupied units built prior to 1940.
# 8. dis - weighted mean of distances to five Boston employment centres.
# 9. rad - index of accessibility to radial highways.
# 10. tax - full-value property-tax rate per $10,000.
# 11. ptratio - pupil-teacher ratio by town.
# 12. black - 1000(Bk - 0.63)^2 where Bk is the proportion of blacks by town.
# 13. lstat - lower status of the population (percent).
# 14. medv - median value of owner-occupied homes in $1000s.

# Sources - 
# Harrison, D. and Rubinfeld, D.L. (1978) Hedonic prices and the demand for clean air. J. Environ. Economics and Management 5, 81–102.
# Belsley D.A., Kuh, E. and Welsch, R.E. (1980) Regression Diagnostics. Identifying Influential Data and Sources of Collinearity. New York: Wiley.
# https://www.kaggle.com/c/boston-housing

# The target variable is medv. 

predict_to_response <- function(predictor, df, title)
{
  fit_predictor <- lm(medv ~ predictor, df)
  print(fit_predictor)
  print(summary(fit_predictor))
  X_Vs_Y <- ggplot(data=df, aes(x=predictor,y=df$medv)) + geom_point(color="red",size=2) + ggtitle(title)
  X_Vs_Y <- X_Vs_Y + stat_smooth(method = "lm", se=FALSE, color="blue")
  print(X_Vs_Y)
  #Residual Vs the Response variable(predicted variable)
  residuals <-  resid(fit_predictor)
  residualplot <- ggplot(data = data.frame(x = df$medv, y = residuals), aes(x = x, y = y)) + 
    geom_point(color = 'red', size = 5) + stat_smooth(method = "lm", se = FALSE, color = 'blue')
  print(residualplot)
  if(!exists('coeffs_uni'))
  {
    coeffs_uni <- tidy(fit_predictor)
  }
  else{
    coeff_add <- tidy(fit_predictor)
    coeffs_uni <- rbind(coeffs_uni,coeff_add)
  }
  
  return(as.data.frame(coeffs_uni))
  
}

a <- predict_to_response(df$crim,df, "Crime Vs Medv") #R-squared = 0.1508
b <- predict_to_response(df$zn,df, "Zn vs Medv") #R-squared = 0.1299
c <- predict_to_response(df$indus,df, "Indus vs Medv") #R-squared = 0.234
d <- predict_to_response(df$chas,df,"Chas vs Medv") #R-squared = 0.03072  - Dummy Variable
e <- predict_to_response(df$nox,df,"Nox vs Medv") #R-squared = 0.1826
f <- predict_to_response(df$rm,df,"Rm vs Medv") #R-squared = 0.4835
g <- predict_to_response(df$age,df,"Age vs Medv") #R-squared =0.1421
h <- predict_to_response(df$dis,df,"Dis vs Medv") #R-squared = 0.06246
i <- predict_to_response(df$rad,df,"rad vs Medv") #R-squared = 0.1456
j <- predict_to_response(df$tax,df,"Tax vs Medv") #R-squared = 0.2195
k <- predict_to_response(df$ptratio,df,"Ptratio vs Medv") #R-squared = 0.2578
l <- predict_to_response(df$black,df,"Black vs Medv") #R-squared = 0.1112
m <- predict_to_response(df$lstat,df,"Lstat vs Medv") #R-squared = 0.5441



# Rm(Average number of rooms per dwelling) and lstat(lower status of the population (percent)) have a good R-squared value, and hence have a statistically significant association.  


#names(df)
#df2 <- subset(df,select=c("crim", "zn"  ,    "indus"  , "chas" ,   "nox" ,    "rm" ,     "age"   ,  "dis" ,    "rad"  ,   "tax"  ,   "ptratio", "black"   ,"lstat"))

fit_predictor_2 <- lm(df$medv ~.,df)
print(fit_predictor_2)
print(summary(fit_predictor_2))


# The predictor variables indus and age are not significant, hence we can use them to reject the null hypothesis. 



coefficient_of <- c('Crime','Zn','Indus','Chas','Nox','Rm','Age','Dis','rad','Tax','Ptratio','Black','Lstat')
coeffs_value <- c(a[2,2],b[2,2],c[2,2],d[2,2],e[2,2],f[2,2],g[2,2],h[2,2],i[2,2],j[2,2],k[2,2],l[2,2],m[2,2])

coefficient_of <- list(coefficient_of)
coeffs_value <- list(coeffs_value)

coeff_df <- do.call("cbind",coefficient_of)
coeff_val <- do.call("cbind",coeffs_value)
coeff_df <- cbind(coeff_df,coeff_val)
coeff_df <- as.data.frame(coeff_df)

coeffs_multi <- tidy(fit_predictor_2)
coeff_multi <-  as.data.frame(coeffs_multi)
coeff_multi_df <- coeff_multi[,1:2]
coeff_multi_df <- coeff_multi_df[2:14,]
#Resetting Index value
rownames(coeff_multi_df) <- NULL


#univariate Coeffecient
print(coeff_df)
#Multivariate coeffecients
print(coeff_multi_df)

coeff_df <- cbind(coeff_df,coeff_multi_df$estimate)

plot(x=coeff_df$V2,y=coeff_df$`coeff_multi_df$estimate`)

```

```{r Non-Linear Relation}


non_linear <- function(predictor, df, title)
{
  model <- lm(medv ~ predictor + I(predictor^2) + I(predictor^3), df)
  nonlinear <- ggplot(data=df, aes(x=predictor,y=df$medv)) + geom_point(color="red",size=2) + ggtitle(title)
  nonlinear <- nonlinear + stat_smooth(method = "lm", se=FALSE, color="blue")
  print(nonlinear)
  
  nonlinearresidual <-  resid(model)
  NL_residualplot <- ggplot(data = data.frame(x = df$medv, y = nonlinearresidual), aes(x = x, y = y)) + 
    geom_point(color = 'red', size = 5) + stat_smooth(method = "lm", se = FALSE, color = 'blue')
  print(NL_residualplot)
}

non_linear(df$crim,df,"Crime")
non_linear(df$zn,df, "Zn vs Medv") 
non_linear(df$indus,df, "Indus vs Medv") 
non_linear(df$chas,df,"Chas vs Medv")
non_linear(df$nox,df,"Nox vs Medv")
non_linear(df$rm,df,"Rm vs Medv")
non_linear(df$age,df,"Age vs Medv") 
non_linear(df$dis,df,"Dis vs Medv") 
non_linear(df$rad,df,"rad vs Medv")
non_linear(df$tax,df,"Tax vs Medv")
non_linear(df$ptratio,df,"Ptratio vs Medv") 
non_linear(df$black,df,"Black vs Medv")
non_linear(df$lstat,df,"Lstat vs Medv") 


#There is a non-linear relationship in some of the predictors. 
#Reference - https://stats.stackexchange.com/questions/64927/how-do-i-plot-an-exponential-curve-in-r



stepwise <- step(lm(df$medv ~.,data=df),direction='both')

#Reference - https://stats.stackexchange.com/questions/214682/stepwise-regression-in-r-how-does-it-work

summary(stepwise)
AIC(stepwise)
BIC(stepwise)
#For the model in 4 - multi variate
AIC(fit_predictor_2)
BIC(fit_predictor_2)

#The stepwise model has less AIC & BIC than the AIC & BIC for Multi Variate model. We aim to make the AIC & BIC less, hence the stepwise model is better than the multi variate model. 
#However, the R squared values for the Multivariate and Stepwise function is the same because the variables indus and age are non-significant, so even if we keep them in the multivariate model, they are not affecting our R Squared value. 


residual_stepwise <- residuals(stepwise)
qqnorm(residual_stepwise,main = "Stepwise Residual")

plotResiduals <- ggplot(data = data.frame(x = df$medv, y = residual_stepwise), aes(x = x, y = y)) + geom_point(color = 'blue', size = 5) + stat_smooth(method = "lm", se = FALSE, color = 'red') + labs(title = "Stepwise Residual Plot Vs Median Value", y = 'Stepwise Residual', x = 'Median Value')
plotResiduals

#The statistical assumption is that the residual for the stepwise model is normally distributed, which can be shown by the graph. There are outliers in the residual. I can spot 7 outliers.The model is not optimised. had I used forward selection, I believe the model would have had better results than the stepwise model.  

