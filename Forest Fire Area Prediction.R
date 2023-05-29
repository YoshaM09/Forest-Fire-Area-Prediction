fires <- read.table('forestfires.csv', header = TRUE, sep = ',')
fit_basic = lm(formula = area~X+Y+FFMC+DMC+DC+ISI+temp+RH+wind+rain, data = fires)

# We observed that rain has 0's more than 90% which is why it may not conntribute to the model 
# So remove rain as obvious garbage. 
summary(fit_basic)# With only numerical data - Base model
# Residual Standard Error = 63.64 
# R2 = 0.016
# Adjusted R2 = 0.0005
anova(fit_basic)

# simple scatter plot to see every predictor with y 
plot(fires$X,fires$area, xlab = "X", ylab = "Area")
plot(fires$Y,fires$area, xlab = "Y", ylab = "Area")
plot(fires$FFMC,fires$area, xlab = "FFMC", ylab = "Area")
plot(fires$DMC,fires$area, xlab = "DMC", ylab = "Area")
plot(fires$DC,fires$area, xlab = "DC", ylab = "Area")
plot(fires$ISI,fires$area, xlab = "ISI", ylab = "Area")
plot(fires$temp,fires$area, xlab = "temp", ylab = "Area")
plot(fires$RH,fires$area, xlab = "RH", ylab = "Area")
plot(fires$wind,fires$area, xlab = "wind", ylab = "Area")
plot(fires$rain,fires$area, xlab = "rain", ylab = "Area")

# After plotting each value looks like many outliers that could be removed
# OUTLIER TESTING (Refer lecture 4 - outliers, Lecture 6 - Leverage and influence)

plot(fitted.values(fit_basic), rstandard(fit_basic), xlab = "fitted values", ylab = "Studentized residuals")
# Not able to understand much from residual plot as there are outliers 

#### Find the outliers ##### 
# Check standardized residuals greater than 3
sort(abs(rstandard(fit_basic)), decreasing = TRUE) > 3

# Observation 239, 416, 480, 238, 237 could be POTENTIAL outliers 

# Create hat matrix and check h_ii entries to find observations with largest leverage
X = as.matrix(cbind(1, fires$X, fires$Y, fires$FFMC, fires$DMC, fires$DC, fires$ISI, 
                    fires$temp, fires$RH, fires$wind, fires$rain))
H = X%*%solve(t(X)%*%X)%*%t(X)
H.order <- cbind(c(1:517), diag(H))
order <- order(H.order[,2])
H.order <- H.order[order,]

# Observation 300, 313, 23, 380, 500 have value greater than 0.10

indicator <- c(rep(1,517))
indicator[300] <- 2 # red
indicator[313] <- 3 # green
indicator[23] <- 4 # green
indicator[380] <- 5 # green
indicator[500] <- 6 # green
plot(fires$X, fires$area, xlab = "X", ylab = "Area", col = indicator)
plot(fires$Y, fires$area, xlab = "Y", ylab = "Area", col = indicator)
plot(fires$FFMC, fires$area, xlab = "FFMC", ylab = "Area", col = indicator)
plot(fires$DMC, fires$area, xlab = "DMC", ylab = "Area", col = indicator)
plot(fires$DC, fires$area, xlab = "DC", ylab = "Area", col = indicator)
plot(fires$ISI, fires$area, xlab = "ISI", ylab = "Area", col = indicator)
plot(fires$temp, fires$area, xlab = "temp", ylab = "Area", col = indicator)
plot(fires$RH, fires$area, xlab = "RH", ylab = "Area", col = indicator)
plot(fires$wind, fires$area, xlab = "wind", ylab = "Area", col = indicator)
plot(fires$rain, fires$area, xlab = "rain", ylab = "Area", col = indicator)

# Remove each observation and test the summary of model
fires_1 <- fires[-300,]
fit.1 <- lm(formula = area~X+Y+FFMC+DMC+DC+ISI+temp+RH+wind+rain, data = fires_1)
summary(fit.1)
fires_2 <- fires_1[-312,]
fit.2 <- lm(formula = area~X+Y+FFMC+DMC+DC+ISI+temp+RH+wind+rain, data = fires_2)
summary(fit.2)
fires_3 <- fires_2[-23,]
fit.3 <- lm(formula = area~X+Y+FFMC+DMC+DC+ISI+temp+RH+wind+rain, data = fires_3)
summary(fit.3)
fires_4 <- fires_3[-377,]
fit.4 <- lm(formula = area~X+Y+FFMC+DMC+DC+ISI+temp+RH+wind+rain, data = fires_4)
summary(fit.4)
fires_5 <- fires_4[-496,]
fit.5 <- lm(formula = area~X+Y+FFMC+DMC+DC+ISI+temp+RH+wind+rain, data = fires_5)
summary(fit.5)


plot(fires_5$X, fires_5$area, xlab = "X", ylab = "Area", col = indicator)
plot(fires_5$Y, fires_5$area, xlab = "Y", ylab = "Area", col = indicator)
plot(fires_5$FFMC, fires_5$area, xlab = "FFMC", ylab = "Area", col = indicator)
plot(fires_5$DMC, fires_5$area, xlab = "DMC", ylab = "Area", col = indicator)
plot(fires_5$DC, fires_5$area, xlab = "DC", ylab = "Area", col = indicator)
plot(fires_5$ISI, fires_5$area, xlab = "ISI", ylab = "Area", col = indicator)
plot(fires_5$temp, fires_5$area, xlab = "temp", ylab = "Area", col = indicator)
plot(fires_5$RH, fires_5$area, xlab = "RH", ylab = "Area", col = indicator)
plot(fires_5$wind, fires_5$area, xlab = "wind", ylab = "Area", col = indicator)
plot(fires_5$rain, fires_5$area, xlab = "rain", ylab = "Area", col = indicator)
#### End of find outliers #####

fit_full = lm(formula = area~X+Y+month+day+FFMC+DMC+DC+ISI+temp+RH+wind+rain, data = fires)
summary(fit_full)
# With numerical data + categorical variables (month,day)
# R2 improved 

#### 
# possible leverage points: 481,514,515,282,76,277,279,80,5,131,278,98,212,510,13,200,
# the most leverage( increasing order): 300, 313, 23, 280,500

# leverage plot indicator
lev_points <- 497:517
indicator <- c(rep(1,517))
indicator[lev_points] <- 2 # all red


# looking at the points with high leverage on the fit (made red)
plot(fitted.values(fit_basic), rstudent(fit_basic), xlab = "fitted values", ylab = "Studentized residuals", col = indicator)
abline(0,0)

# looking at leverage points on a transformed model (made red)
fit_log = lm(formula = (log(area+1))~X+Y+FFMC+DMC+DC+ISI+temp+RH+wind+rain, data = fires)
plot(fitted.values(fit_log), rstudent(fit_log), xlab = "fitted values", ylab = "Studentized residuals", col = indicator)
abline(0,0)

summary(fit_basic)

# Multicollinearity
X = as.matrix(cbind(fires$X, fires$Y, fires$FFMC, fires$DMC, fires$DC, fires$ISI, 
                    fires$temp, fires$RH, fires$wind, fires$rain))
solve(cor(X))
# Not concern for multicollinearity, nothing above 5 in diagonal
# slight negative correlation (-1.15) between x4 and x5, slight positive correlation (1.388) between x7 and x8
det(cor(X))

# Influence: 
# Cook's distance
d <- cooks.distance(fit_basic)
d[order(d, decreasing = TRUE)]

# 500, 416, 239

# dfbetas cut off: 0.0879599
dfb<-dfbetas(fit_basic)
tail(dfb,25)
# Rain - 500
# Wind - 237,238,378
# RH - 480, 514
# Temp - 235
# Isi - 416, 23, 380
# DC - 239,
# DMC - 416, 378,393,421
# FFMC - 416, 380
# Y - 237, 378, 480, 238
# X -  232, 238
# Inter - 380, 416


# dffits cut off: 0.29173
dff <- dffits(fit_basic)
dff
sort(abs(dff), decreasing = TRUE)
# 237, 238, 378, 421, 480, 500, 416, 239
# 416 affects most of the predictors
# 500 classifies as an outlier, high influence from cook's distance and dffits, huge influence on rain from dfbetas

summary(fit_basic)
anova(fit_basic)

# Removing outling leverage and influencial points
fires_1 <- fires[-416,]
fit.1 <- lm(formula = area~X+Y+FFMC+DMC+DC+ISI+temp+RH+wind+rain, data = fires_1)
summary(fit.1)
anova(fit.1)

par(mfrow=c(1,2))
plot(fitted.values(fit_basic), rstudent(fit_basic), xlab = "fitted values", ylab = "Studentized residuals", main = "with 416")
plot(fitted.values(fit.1), rstudent(fit.1), xlab = "fitted values", ylab = "Studentized residuals", main = "without 416")

fires_2 <- fires_1[-239,]
fit.2 <- lm(formula = area~X+Y+FFMC+DMC+DC+ISI+temp+RH+wind+rain, data = fires_2)
summary(fit.2)
anova(fit.2)

par(mfrow=c(1,3))
plot(fitted.values(fit_basic), rstudent(fit_basic), xlab = "fitted values", ylab = "Studentized residuals", main = "base")
plot(fitted.values(fit.1), rstudent(fit.1), xlab = "fitted values", ylab = "Studentized residuals", main = "without 416")
plot(fitted.values(fit.2), rstudent(fit.2), xlab = "fitted values", ylab = "Studentized residuals", main = "without 239 & 416")

# comparing with and without obs. 239
fires_239 <- fires[-239,]
fit.239 <- lm(formula = area~X+Y+FFMC+DMC+DC+ISI+temp+RH+wind+rain, data = fires_239)
summary(fit.239)
anova(fit.239)

par(mfrow=c(1,3))
plot(fitted.values(fit_basic), rstudent(fit_basic), xlab = "fitted values", ylab = "Studentized residuals", main = "base")
plot(fitted.values(fit.239), rstudent(fit.239), xlab = "fitted values", ylab = "Studentized residuals", main = "without 239")
plot(fitted.values(fit.1), rstudent(fit.1), xlab = "fitted values", ylab = "Studentized residuals", main = "without 416")

# comparing with and without obs. 378
fires_378 <- fires[-378,]
fit.378 <- lm(formula = area~X+Y+FFMC+DMC+DC+ISI+temp+RH+wind+rain, data = fires_378)
summary(fit.378)
anova(fit.378)


fires_3 <- fires_2[-378,]
fit.3<- lm(formula = area~X+Y+FFMC+DMC+DC+ISI+temp+RH+wind+rain, data = fires_3)
summary(fit.3)
anova(fit.3)

par(mfrow=c(2,3))
plot(fitted.values(fit_basic), rstudent(fit_basic), xlab = "fitted values", ylab = "Studentized residuals", main = "base")
plot(fitted.values(fit.378), rstudent(fit.378), xlab = "fitted values", ylab = "Studentized residuals", main = "without 378")
plot(fitted.values(fit.239), rstudent(fit.239), xlab = "fitted values", ylab = "Studentized residuals", main = "without 239")
plot(fitted.values(fit.1), rstudent(fit.1), xlab = "fitted values", ylab = "Studentized residuals", main = "without 416")
plot(fitted.values(fit.2), rstudent(fit.2), xlab = "fitted values", ylab = "Studentized residuals", main = "without 239 & 416")
plot(fitted.values(fit.3), rstudent(fit.3), xlab = "fitted values", ylab = "Studentized residuals", main = "without 239 & 416 & 378")

par(mfrow=c(1,2))
plot(fitted.values(fit.2), rstudent(fit.2), xlab = "fitted values", ylab = "Studentized residuals", main = "without 239 & 416")
plot(fitted.values(fit.3), rstudent(fit.3), xlab = "fitted values", ylab = "Studentized residuals", main = "without 239 & 416 & 378")


# trying log transformation on the area value with deleted points
sort(abs(rstudent(fit_basic)), decreasing = TRUE)

fit.log <- lm(log(area+1)~X+Y+FFMC+DMC+DC+ISI+temp+RH+wind+rain, data = fires_3)
summary(fit.log)
anova(fit.log)

# seeing how obs. 500 changes
lev_points <- 497:517
indicator <- c(rep(1,517))
indicator[500] <- 2 # all red

par(mfrow=c(1,2))
plot(fitted.values(fit.log), rstudent(fit.log), xlab = "fitted values", ylab = "Studentized residuals", main = "fit.log removed 239 & 416 & 378 ", col = indicator)
abline(0,0)
plot(fitted.values(fit_log), rstudent(fit_log), xlab = "fitted values", ylab = "Studentized residuals", main= "fit log base", col = indicator)
abline(0,0)

# Continuing with log transformed data and outlier removal
fires_4 <- fires_3[-23,]
fit.4<- lm(log(area+1)~X+Y+FFMC+DMC+DC+ISI+temp+RH+wind+rain, data = fires_4)
summary(fit.4)
anova(fit.4)
par(mfrow=c(1,3))
plot(fitted.values(fit.log), rstudent(fit.log), xlab = "fitted values", ylab = "Studentized residuals", main = "fit.log removed 239 & 416 & 378 ", col = indicator)
abline(0,0)
plot(fitted.values(fit_log), rstudent(fit_log), xlab = "fitted values", ylab = "Studentized residuals", main= "fit log base", col = indicator)
abline(0,0)
plot(fitted.values(fit.4), rstudent(fit.4), xlab = "fitted values", ylab = "Studentized residuals", main = "without 239 & 416 & 378 & 23")

fires_5 <- fires_4[-377,]
fit.5<- lm(log(area+1)~X+Y+FFMC+DMC+DC+ISI+temp+RH+wind+rain, data = fires_5)
summary(fit.5)
anova(fit.5)
par(mfrow=c(2,3))
plot(fitted.values(fit.log), rstudent(fit.log), xlab = "fitted values", ylab = "Studentized residuals", main = "fit.log removed 239 & 416 & 378 ", col = indicator)
abline(0,0)
plot(fitted.values(fit_log), rstudent(fit_log), xlab = "fitted values", ylab = "Studentized residuals", main= "fit log base", col = indicator)
abline(0,0)

par(mfrow=c(1,2))
plot(fitted.values(fit.4), rstudent(fit.4), xlab = "fitted values", ylab = "Studentized residuals", main = "without 239 & 416 & 378 & 23")
plot(fitted.values(fit.5), rstudent(fit.5), xlab = "fitted values", ylab = "Studentized residuals", main = "without 239 & 416 & 378 & 377 & 23")

# Fit 5 seems most of important outliers are gone

# Checking with more outliers/leverage/influential points

fires_6 <- fires_5[-237,]
fit.6<- lm(log(area+1)~X+Y+FFMC+DMC+DC+ISI+temp+RH+wind+rain, data = fires_6)
summary(fit.6)
anova(fit.6)
par(mfrow=c(1,3))
plot(fitted.values(fit.4), rstudent(fit.4), xlab = "fitted values", ylab = "Studentized residuals", main = "without 239 & 416 & 378 & 23",col = indicator)
plot(fitted.values(fit.5), rstudent(fit.5), xlab = "fitted values", ylab = "Studentized residuals", main = "without 239 & 416 & 378 & 377 & 23",col=indicator)
plot(fitted.values(fit.6), rstudent(fit.6), xlab = "fitted values", ylab = "Studentized residuals", main = "without 239 & 416 & 378 & 377 & 23 & 500",col=indicator)

fires_7 <- fires_6[-238,]
fit.7<- lm(log(area+1)~X+Y+FFMC+DMC+DC+ISI+temp+RH+wind+rain, data = fires_7)
summary(fit.7)
anova(fit.7)

fires_8 <- fires_7[-480,]
fit.8<- lm(log(area+1)~X+Y+FFMC+DMC+DC+ISI+temp+RH+wind+rain, data = fires_8)
summary(fit.8)
anova(fit.8)

par(mfrow=c(2,3))
plot(fitted.values(fit.4), rstudent(fit.4), xlab = "fitted values", ylab = "Studentized residuals", main = "without 239 & 416 & 378 & 23",col = indicator)
plot(fitted.values(fit.5), rstudent(fit.5), xlab = "fitted values", ylab = "Studentized residuals", main = "without 239 & 416 & 378 & 377 & 23",col=indicator)
plot(fitted.values(fit.6), rstudent(fit.6), xlab = "fitted values", ylab = "Studentized residuals", main = "without 239 & 416 & 378 & 377 & 23 & 237",col=indicator)
plot(fitted.values(fit.7), rstudent(fit.7), xlab = "fitted values", ylab = "Studentized residuals", main = "without 239 & 416 & 378 & 23 & 237 & 238",col = indicator)
plot(fitted.values(fit.8), rstudent(fit.8), xlab = "fitted values", ylab = "Studentized residuals", main = "without 239 & 416 & 378 & 377 & 23& 237 & 238 & 480",col=indicator)

# not significant changes from fits 5-8
# use fit.5/fires_5 going forward
# final data set without obs. : 416, 239, 378, 23, 377

summary(fit.5)
anova(fit.5)


# Transformations
# boxcox, have to shift area by 1 for bc to work because it needs values > 0
# post removal
fit_preBC <- lm((area+1)~X+Y+FFMC+DMC+DC+ISI+temp+RH+wind+rain, data = fires_5)
summary(fit_preBC)
library(MASS)
bc <- boxcox(fit_preBC)
# lambda ~ -1/2
y <-(1/(sqrt(fires_5$area+1)))
fit_bc <- lm(y~X+Y+FFMC+DMC+DC+ISI+temp+RH+wind+rain, data = fires_5)
summary(fit_bc)
anova(fit_bc)

# Comparing fitted values vs. rstudent residuals
par(mfrow=c(1,4))
plot(fitted.values(fit_basic), rstudent(fit_basic), xlab = "fitted values", ylab = "Studentized residuals",main="Base (Pre-outlier removal)")
plot(fitted.values(fit_preBC), rstudent(fit_preBC), xlab = "fitted values", ylab = "Studentized residuals",main="Area+1 (Post Removal)")
plot(fitted.values(fit_bc), rstudent(fit_bc), xlab = "fitted values", ylab = "Studentized residuals",main="Boxcox on (Area+1)")
plot(fitted.values(fit.5), rstudent(fit.5), xlab = "fitted values", ylab = "Studentized residuals",main="Log(Area+1)")


# Residual Analysis
# Comparing QQ-plots
par(mfrow=c(1,4))
qqnorm(residuals(fit_basic), main = "Base Normal QQ-Plot (Pre-outlier removal)")
qqline(residuals(fit_basic))
qqnorm(residuals(fit_preBC), main = "Area+1 Normal QQ-Plot (Post Removal)")
qqline(residuals(fit_preBC))
qqnorm(residuals(fit_bc), main = "Boxcox Normal QQ-Plot")
qqline(residuals(fit_bc))
qqnorm(residuals(fit.5), main = "Log(area+1) Normal QQ-Plot")
qqline(residuals(fit.5))
# log(area+1) transformation looks ok


#####################################################
# Variable selection using exhaustive, forward, and backward
#####################################################
summary(fit.5)
library(leaps)

all_forward <-regsubsets(x=cbind(fires_5$X,fires_5$Y, fires_5$FFMC, fires_5$DMC,fires_5$DC, fires_5$ISI, fires_5$temp, fires_5$RH , fires_5$wind, fires_5$rain), y= I(log(fires_5$area + 1)), method='forward')

Cp <- summary(all_forward)$cp
AdjR2 <- summary(all_forward)$adjr2
SSRes <- summary(all_forward)$rss
R2 <- summary(all_forward)$rsq
Matrix <- summary(all_forward)$which
# with some trickery...
p <- apply(Matrix,1, sum)
MSRes <- SSRes/(512-p)
# Make a nice table
output <- cbind(p, Matrix, SSRes, R2, AdjR2, MSRes, Cp)
colnames(output)[3:6] <- c("x1", "x2", "x3", "x4")
output



all_ex <-regsubsets(x=cbind(fires_5$X,fires_5$Y, fires_5$FFMC, fires_5$DMC,fires_5$DC, fires_5$ISI, fires_5$temp, fires_5$RH , fires_5$wind, fires_5$rain), y= I(log(fires_5$area + 1)), method = "exhaustive", 
                    all.best = TRUE, nbest=2 )

Cp <- summary(all_ex)$cp
AdjR2 <- summary(all_ex)$adjr2
SSRes <- summary(all_ex)$rss
R2 <- summary(all_ex)$rsq
Matrix <- summary(all_ex)$which
# with some trickery...
p <- apply(Matrix,1, sum)
MSRes <- SSRes/(512-p)
# Make a nice table
output_ex <- cbind(p, Matrix, SSRes, R2, AdjR2, MSRes, Cp)
colnames(output_ex)[3:12] <- c("X", "Y", "FFMC", "DMC", 'DC', 'ISI', 'temp', 'RH', 'wind', 'rain')
output_ex[5]


## Testing start 
# 3 4           1  1      1    1 
check_fit <- lm(log(area+1)~X+DC+wind, data = fires_5)
summary(check_fit)
## Testing end