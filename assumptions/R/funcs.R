install.packages('lmtest')
library(lmtest)

#' A Assumptions Checking Function
#'
#' This functions check the assumptions of linear regression
#' @param mod Input a fitted linear model
#' @keywords linear, regression
#' @export
#' @examples check_as(model), check_as(lm(y ~.))
#' check_as()

check_as = function(mod){
  res = mod$residuals
  nrm = shapiro.test(res)
  bpt = bptest(mod)
  if (nrm$p.value > 0.05){
    print('Normality is not violated')
  }
  else{
    print('Normality is violated')
  }
  if (bpt$p.value > 0.05){
    print('Residuals are homogenious')
  }
  else{
    print('Heteroscedasticity is detected')
  }
}



#' A Graphic Summary Function
#'
#' This functions displays key graphs of linear regression
#' @param mod Input a fitted linear model
#' @keywords linear, regression, graph
#' @export
#' @examples summary_disp(model), summary_disp(lm(y ~ .))
#' summary_disp()

summary_disp = function(modd) {
  #x = as.numeric(modd$model[,1])
  #y = as.numeric(modd$model[,2])
  #plot(x,y)
  #lines(modd$fitted.values)
  boxplot(modd$residuals)
  plot(modd)
}
