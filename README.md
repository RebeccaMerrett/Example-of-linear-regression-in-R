# Example of linear regression in R
#Set your working directory path to whatever folder you like.<br>
setwd("C:\\Users\\Rebecca Merrett\\Documents\\r-workspace\\RebeccaSpace")<br>
getwd()<br>
data(airquality)<br>
head(airquality)<br>
#Check for missing values and see if need to treat them accordingly. Also if there are categorical type variables (including age groups), need to set as factors.<br>
#I noticed I could not get Rsquared on testset when included missing values.<br>
airquality <- na.exclude(airquality)<br>
#na.exclude leaves out rows with missing values, but keeps track of where they were so that when making predictions a vector will be the same length of the actual response.<br>
#Use scatterplot matrix to find correlations between variables. Ozone on y axis has strong positive correlation with Temp on x axis.<br>
pairs(airquality)<br>
#lm plot of predictor along x horizontal axis and response (what want to predict) along y vertical axis.<br>
#The regression line will go with the best fit to the data, with grey band around it meaning uncertainty in fit.<br>
#This is to first see if data is in any way suitable to be fitted in linear form.
require(ggplot2)<br>
ggplot(airquality, aes(x=Temp, y=Ozone)) + geom_point() + geom_smooth(method="lm") + labs(x="Temperature", y="Ozone")<br>
#Split data into train (70%) and test (30%), and build the model on the training set.<br>
airqualitySplit <- sort(sample(nrow(airquality),nrow(airquality)*.7))<br>
airqualityTrain <- airquality[airqualitySplit,]<br>
airqualityTest <- airquality[-airqualitySplit,]<br>
OzoneModel <- lm(formula = Ozone ~ Temp, data = airqualityTrain)<br>
summary(OzoneModel)<br>
#Look at the residuals (difference between actual and predicted values) and plot them, which will produce 4 plots.<br>
#One is Normal Q-Q plot to diagnose if residuals are far off the normal distribution line or not, lack of linearity showing it’s not good fit.<br>
plot(OzoneModel)<br>
#Might want to do an ANOVA/F-test to see if adding more predictors makes a difference or not, looking at lm model with say all predictors.<br>
#This 'anova(Model1, Model2)' comparison looks at p-value against significance 0.05 to accept or reject null hypothesis of a noticeable difference or not.<br>
#Now test the model against the test set, and minus the first column or response (Ozone).<br>
ModelPredictions <- predict(OzoneModel, newdata=airqualityTest[ ,-1],type="response", se.fit=TRUE)<br>
head(ModelPredictions$fit)<br>
head(airqualityTest$Ozone)<br>
actual <- airqualityTest$Ozone<br>
Rsquared <- 1-sum((actual-ModelPredictions$fit)^2)/sum((actual-mean(actual))^2)<br>
Rsquared
