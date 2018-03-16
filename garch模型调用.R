上install.packages("rgarch")
library(rgarch)
#######sgarch(1,1)-norm模型的模型设定
myspec=ugarchspec(variance.model = list(model = "sGARCH", garchOrder = c(1, 1), submodel = NULL, external.regressors = NULL, variance.targeting = FALSE), 
                  mean.model = list(armaOrder = c(1, 1), include.mean = TRUE, archm = FALSE, archpow = 1, arfima = FALSE, external.regressors = NULL, archex = FALSE), distribution.model = "norm")