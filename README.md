# Forest-Fire-Area-Prediction
<img width="327" alt="Screenshot 2023-05-30 at 8 57 17 AM" src="https://github.com/YoshaM09/Forest-Fire-Area-Prediction/assets/105993890/f4dbcc36-ff3b-44f3-b2ff-2d8967cf86a0">

### Purpose:

 To create a regression model to predict the area burned in hectares
(ha) by the forest fires in the northeast region of Portugal. 

### Problem Statement:

Forest fires are a major environmental issue, creating economic and ecological damage while endangering human lives. Fast detection is a key element for controlling such phenomenon. To achieve this, one alternative is to use automatic tools based on local sensors, such as provided by meteorological stations. In effect, meteorological conditions (e.g., temperature, wind) are known to influence forest fires and several fire indexes, such as the forest Fire Weather Index (FWI). The dataset that I used is the forest fire dataset which is based on the forest fires in Montesinho Natural Park which is a protected area located in the northeastern Portugal. 

### How the problem was efficiently solved:

Utilized forest fire dataset to perform regression analysis and identify most significant variables affecting and causing forest fires, eliminated multicollinearity with VIF and verified linear assumptions via QQ and residual plots.

### Approach:

#### 1. Data analysis and visualizations: 
This dataset has 517 observations and consists of two categorical predictors namely month and day and 10 numerical predictors. The initial step for preparing the data was to check the model assumptions (without categorical variables) by plotting the scatter plots for all the predictors against the response variable Area burned, the Residuals vs. the fitted values, and the QQ-plots to perform residual analysis. 
#### 2. Outlier detection: 
Some observation on the scatter plot were far away from most of the data points and could be potential outliers so the next step was to detect the outliers. A hat matrix was created to check the diagonal elements of the hat matrix to find observations with largest leverage. Observations that had values greater than 0.10 had large leverage. Cook’s distance along with dfbetas and dffits were used to identify the influence points and concluded that the observations with the most influence. 
#### 3. Multicollinearity: 
To check multicollinearity in data I used the correlation matrix
for pair-wise correlation and Variance Inflation Factors. 
#### 4. Variable transformation: 
There was still a large cluster and a skew towards zero after outlier removal, I tried to find a variable transformation that would best fit the data. Shifted the response up by one then applied the boxcox method 
#### 5. Variable Selection: 
First I start with forward variable selection where I found that ten variables were selected. However, with the ten variables the metrics do not seem to be good. So, I move on to perform backward selection which yields same results as forward variable selection. Further, I perform exhaustive search and sort the models based on the following importance: Adjusted R2, MSRES and Mallow’s Cp. 

### Results:

* Plotting the predictors against the response variable (Area burned) did not signal any obvious predictor transformation but did show a skew towards zero hectares burned. 
* Observations 500, 416, and 239 were the influence points. 
* It was found that the slight expected pairwise correlation existed because the Initial Spread Index can depend on the other predictors. There was no large pairwise correlation that would have caused a problem. 
Boxcox transformation inverted the data but gave a good variation in the plot comparing the fitted values to the studentized residuals. 
* The regression model analysis discovered only certain variables are statistically significant in predicting the area burnt by forest fires. Not all Fire Watch Index and Atmospheric variables completely explain the variation in the area of forest fire.  
* Predictors seem to be insufficient in explaining variability in area burnt by forest fires indicating presence of other influential factors and need for a more comprehensive dataset

### Tools & Tech: R, RStudio

### Dataset link:
https://www.kaggle.com/datasets/ishandutta/forest-fires-data-set-portugal
