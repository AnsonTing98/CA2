# Read the csv file that already been modified and prepared in CA 1
# into dataframe named stroke
stroke_data <- read.csv("Stroke-modified.csv")

# Check the structure of data
str(stroke_data)

# Remove the X col which is the index of csv
stroke_data <- subset(stroke_data, select = -c(X))

# Remove gender = "Other"
# and smoking_status = "unknown"
# both values are meaningless in the prediction
stroke_data <- subset(stroke_data, gender != "Other")
stroke_data <- subset(stroke_data, smoking_status != "unknown")

# Convert all the categorize variables to factor
attach(stroke_data)
stroke_data$gender <- factor(gender)
stroke_data$ever_married <- factor(ever_married)
stroke_data$work_type <- factor(work_type)
stroke_data$Residence_type <- factor(Residence_type)
stroke_data$smoking_status <- factor(smoking_status)
stroke_data$hypertension_factor <- factor(hypertension_factor)
stroke_data$heart_disease_factor <- factor(heart_disease_factor)
stroke_data$stroke_factor <- factor(stroke_factor)

# Convert the int variables to numeric
stroke_data$heart_disease <- as.numeric(heart_disease)
stroke_data$hypertension <- as.numeric(hypertension)
stroke_data$stroke <- as.numeric(stroke)

# Convert the date the Date variable
stroke_data$Date <- as.Date(Date, "%Y-%m-%d")
detach(stroke_data)

# Correlation with pairs panels
library(psych)
pairs.panels(stroke_data[c(2, 3, 6, 7, 8, 9, 10, 11, 15, 16, 17)],
             smooth = TRUE,
             scale = FALSE,
             density = TRUE,
             ellipses = TRUE,
             method = "spearman",
             pch = 21,
             lm = FALSE, 
             cor = TRUE,
             jiggle = FALSE,
             factor = 2,
             hist.col = 4,
             stars = TRUE,
             ci = TRUE) 

# set the par parameter
opar <- par(no.readonly = TRUE)

# 2 rows and 2 columns
par(mfrow = c(2,2))
attach(stroke_data)
# plotting the box plot 
# the dependent variable - stroke 
# with the continuous independent variable - 
# age, avg_glucose_level, bmi
plot(x = stroke_factor,
     y = age,
     horizontal=TRUE,
     main = "Stroke by age", 
     xlab = "Age", 
     ylab = "Stroke")

plot(x = stroke_factor, 
     y = avg_glucose_level, 
     horizontal = TRUE, 
     main = "Stroke by average glucose level", 
     xlab = "Blood glucose level (Average)", 
     ylab = "Stroke")

plot(x = stroke_factor, 
     y = bmi, 
     horizontal = TRUE, 
     main = "Stroke by bmi", 
     xlab = "BMI", 
     ylab = "Stroke")
detach(stroke_data)
par(opar)

par(mfrow = c(2,2))
# Normality of age
with(stroke_data, {
  qqnorm(age, 
         main = "Normality analysis of age data")
  qqline(age)
})

# Q-Q plot for distribution of age - the healthy patient
with(stroke_data, {qqnorm(age[stroke_factor == "healthy"], 
                          main = "Q-Q plot - Age of healthy patient") 
  qqline(age[stroke_factor == "healthy"], col = "red")})

# Q-Q plot for distribution of age - the stroke patient
with(stroke_data, {qqnorm(age[stroke_factor == "stroke"], 
                          main = "Q-Q plot - Age of stroke patient") 
  qqline(age[stroke_factor == "stroke"], col = "red")})
par(opar)

par(mfrow = c(2,2))
# Normality of avg_gluose_level
with(stroke_data, {
  qqnorm(avg_glucose_level, 
         main = "Normality analysis of average glucose level")
  qqline(avg_glucose_level)
})

# Q-Q plot for distribution of average glucose level - the healthy patient
with(stroke_data, {qqnorm(avg_glucose_level[stroke_factor == "healthy"], 
                          main = "Q-Q plot - Average glucose level of healthy patient") 
  qqline(avg_glucose_level[stroke_factor == "healthy"], col = "red")})

# Q-Q plot for distribution of average glucose level - the stroke patient
with(stroke_data, {qqnorm(avg_glucose_level[stroke_factor == "stroke"], 
                          main = "Q-Q plot - Average glucose level of stroke patient") 
  qqline(avg_glucose_level[stroke_factor == "stroke"], col = "red")})
par(opar)

par(mfrow = c(2,2))
# Normality of bmi
with(stroke_data, {
  qqnorm(bmi, 
         main = "Normality analysis of bmi")
  qqline(bmi)
})

# Q-Q plot for distribution of bmi - the healthy patient
with(stroke_data, {qqnorm(bmi[stroke_factor == "healthy"], 
                          main = "Q-Q plot - BMI of healthy patient") 
  qqline(bmi[stroke_factor == "healthy"], col = "red")})

# Q-Q plot for distribution of bmi - the stroke patient
with(stroke_data, {qqnorm(bmi[stroke_factor == "stroke"], 
                          main = "Q-Q plot - BMI of stroke patient") 
  qqline(bmi[stroke_factor == "stroke"], col = "red")})
par(opar)

# Check for normality using Shapiro test
# If p-value is lower than 0.05
# then data is not normally distributed
paste("The p-value of age:", shapiro.test(stroke_data$age)$p.value)

paste("The p-value of age - stroke patient:", with(stroke_data, 
                                                   tapply(age, stroke_factor, 
                                                          shapiro.test)
                                                   )$stroke$p.value)

paste("The p-value of age - healthy patient:", with(stroke_data, 
                                                     tapply(age, stroke_factor, 
                                                            shapiro.test)
                                                     )$healthy$p.value)

paste("The p-value of BMI:", shapiro.test(stroke_data$bmi)$p.value)

paste("The p-value of BMI - stroke patient:", with(stroke_data, 
                                                   tapply(bmi, stroke_factor, 
                                                          shapiro.test)
                                                   )$stroke$p.value)

paste("The p-value of BMI - healthy patient:", with(stroke_data, 
                                                    tapply(bmi, stroke_factor, 
                                                           shapiro.test)
                                                    )$healthy$p.value)

paste("The p-value of average glucose level:", 
      shapiro.test(stroke_data$avg_glucose_level)$p.value)

paste("The p-value of average glucose level - stroke patient:", 
      with(stroke_data, 
           tapply(avg_glucose_level, stroke_factor, 
                  shapiro.test))$stroke$p.value)

paste("The p-value of average glucose level - healthy patient:", 
      with(stroke_data, 
           tapply(avg_glucose_level, stroke_factor, 
                  shapiro.test))$healthy$p.value)

par(mfrow = c(2,2))
attach(stroke_data)
# Outliers
# Use the boxplot to show the outliers of the continuous variables
boxplot(age, 
        main = "age", 
        sub = paste("Outlier count: ", length(boxplot.stats(age)$out)))

boxplot(bmi, 
        main = "bmi", 
        sub = paste("Outlier count: ", length(boxplot.stats(bmi)$out)))

boxplot(avg_glucose_level, 
        main = "Average glucose level", 
        sub = paste("Outlier count: ", 
                    length(boxplot.stats(avg_glucose_level)$out)))
detach(stroke_data)
par(opar)

# Record the outliers 
bmi_outliers <- sort(boxplot.stats(stroke_data$bmi)$out)
glucose_outliers <- sort(boxplot.stats(stroke_data$avg_glucose_level)$out)

bmi_outliers
median(stroke_data$bmi)
# Median of bmi is 29.1
# then remove the 11.5, 71.9, 78.0 and 92.0 outliers

glucose_outliers
median(stroke_data$avg_glucose_level)
# Median of avg_glucose_level is 92.35
# then remove the some far outliers 
# 266.59, 267.60, 267.61, 267.76 and 271.74

# Remove some outliers of bmi variables
stroke_data <- subset(stroke_data, bmi != 11.5 &
                        bmi != 71.9 & 
                        bmi != 78.0 & 
                        bmi != 92.0)

# Remove some outliers of avg_glucose_level variables
stroke_data <- subset(stroke_data, avg_glucose_level != 266.59 & 
                        avg_glucose_level != 267.60 & 
                        avg_glucose_level != 267.61 & 
                        avg_glucose_level != 267.76 & 
                        avg_glucose_level != 271.74)

# set the seed to 1
# to make sure building the model 
# with same simulations
set.seed(1)
# Split the data into 70% training, 30% testing
no_rows_data <- nrow(stroke_data)
sample_data <- sample(1:no_rows_data, 
                      size = round(0.7 * no_rows_data), 
                      replace = FALSE)

# Record 70% as training data
# and 30% as testing data
training_data <- stroke_data[sample_data, ]
testing_data <- stroke_data[-sample_data, ]

# Build the default model
# just to see the significant of the variables
# family = binomial as stroke or healthy
model <- glm(stroke_factor ~ gender + age + hypertension_factor + 
               heart_disease_factor + ever_married + 
               work_type + Residence_type + avg_glucose_level + 
               bmi + smoking_status, 
             data = stroke_data, family = binomial)
summary(model)

# Make decision with the significant result
# to build model2
model2 <- glm(stroke_factor ~ age + hypertension + heart_disease + 
                avg_glucose_level + bmi, 
             data = training_data, family = binomial)
summary(model2)

library(MASS)
library(car)
# Use confint() function
# to get the 95% confident
# of the interval of 2.5% and 97.5%
# with 1% changes in the independent variables
confint(model2)

# coef() is to get the model coefficient
# and use exp() to calculate exponential values of coefficient
exp(coef(model2))

# Plotting the residual plot with the independent variable
# set type = 'rstudent' for studentised residuals
residualPlots(model2, 
              type = 'rstudent')

par(mfrow = c(2, 2))
# plot the residuals vs fitted plot,
# normal Q-Q plot,
# scale-location plot,
# residuals vs leverage
plot(model)
par(opar)

# Q-Q plot of model2
qqnorm(resid(model2))
qqline(resid(model2))

# Show the histogram of
# studentised residuals,
# normal curve, kernel-density curve,
# and rug plot
student_fit_model2 <- rstudent(model2)
hist(student_fit_model2,
     breaks=10,
     freq=FALSE,
     ylim = c(0.0, 3.0),
     xlab="Studentized Residual", main="Distribution of Errors")
rug(jitter(student_fit_model2), col="brown")
curve(dnorm(x, 
            mean=mean(student_fit_model2), 
            sd=sd(student_fit_model2)),
      add=TRUE, 
      col="blue", 
      lwd=2)
lines(density(student_fit_model2)$x, 
      density(student_fit_model2)$y, 
      col="red", 
      lwd=2, 
      lty=2)
legend("topright", 
       legend = c( "Normal Curve", "Kernel Density Curve"), 
       lty=1:2, 
       col=c("blue","red"), 
       cex=.7)

# Show outlier of the model with outlierTest()
outlierTest(model2)

# Remove the outliers 
# that show in residuals plot
stroke_data <- stroke_data[rownames(stroke_data) != 150 & 
                             rownames(stroke_data) != 100 & 
                             rownames(stroke_data) != 174 &
                             rownames(stroke_data) != 190 & 
                             rownames(stroke_data) != 78, ]

# Rebuild the model after removed outliers
set.seed(1)
# Split the data into 70% training, 30% testing
no_rows_data <- nrow(stroke_data)
sample_data <- sample(1:no_rows_data, 
                      size = round(0.7 * no_rows_data), 
                      replace = FALSE)

# Record 70% as training data
# and 30% as testing data
training_data <- stroke_data[sample_data, ]
testing_data <- stroke_data[-sample_data, ]

# Build the model 3 with stroke ~
# age + hypertension + heart_disease + avg_glucose_level + bmi
# set family as binomial
model3 <- glm(stroke_factor ~ age + hypertension + heart_disease + 
                avg_glucose_level + bmi, 
              data = training_data, family = binomial)
summary(model3)

# Histogram of model 3
student_fit_model3 <- rstudent(model3)
hist(student_fit_model3,
     breaks=10,
     freq=FALSE,
     ylim = c(0.0, 3.0),
     xlab="Studentized Residual", main="Distribution of Errors")
rug(jitter(student_fit_model3), col="brown")
curve(dnorm(x, 
            mean=mean(student_fit_model3), 
            sd=sd(student_fit_model3)),
      add=TRUE, 
      col="blue", 
      lwd=2)
lines(density(student_fit_model3)$x, 
      density(student_fit_model3)$y, 
      col="red", 
      lwd=2, 
      lty=2)
legend("topright", 
       legend = c( "Normal Curve", "Kernel Density Curve"), 
       lty=1:2, 
       col=c("blue","red"), 
       cex=.7)

# Check the outliers of model 3
outlierTest(model3)

# set a cut-off with formula 4/(n-k-1)
# where n = sample size and k = number of predictor variables
cutoff <- 4/(nrow(training_data) - length(model3$coefficients) - 1)
# Plot the Cook's distance plot
# with a red line of cu-toff
plot(model3, which = 4, cook.levels = cutoff)
abline(h = cutoff, lty = 2, col = 'red')

# Plot the Added-Variable 
# for each independent variables
avPlots(model3)

# plot the influence plot
# of the model3 
# to check studentised residuals versus hat-values
# which can use to improve the model
influencePlot(model3, 
              main = "Influence Plot", 
              sub = "Circle size is proportional to Cook's distance", 
              col = "red", 
              lwd = 0.5)

# the absoulute studentlised residuals verses
# fitted values
# with suggested power transformation
spreadLevelPlot(model3)

# Use vif() to check whether the variables indicate
# a multicollinearity problem
vif(model3)
# If the value > 2,
# the variable indicate a multicollinearity problem
sqrt(vif(model3)) > 2

library(pscl)
# Use pscl package
# Pseudo R^2 to measure predictive power of model
# McFadden value close to 1 as good model,
# otherwise it not predictive power which close to 0
pR2(model3)

library(MASS)
# Use stepwise regression algorithms 
# to check the coefficients of variables
# with backward stepwise regression
fit_test <- glm(stroke_factor ~ age + hypertension_factor + 
                  heart_disease_factor + avg_glucose_level + bmi, 
                data = training_data, family = binomial)
stepAIC(fit_test, direction = "backward")

library(leaps)
# Use leaps package
# to regsubsets the model with nbest = 4
# and apply BIC of scale of plot
model_leaps <- regsubsets(stroke ~ age + hypertension + heart_disease + 
                            avg_glucose_level + bmi, 
                          data = training_data, nbest = 4)
plot(model_leaps, scale = "bic")

# Build the final model
# with independent variables
# age + hypertension + heart_disease + avg_glucose_level
final_model <- glm(stroke_factor ~ age + hypertension_factor + 
                     heart_disease_factor + avg_glucose_level, 
                   data = training_data, family = binomial)
summary(final_model)

# Predict with testing data
# set type to "response" to get the probability
predicted_result <- predict(final_model, testing_data, type = "response")

# Create actuals stroke variable from testing data  vs 
# predicted result of testing data as dataframe
actuals_predictions <- data.frame(cbind(Actuals = testing_data$stroke_factor, 
                                        Predicted = predicted_result))

# Check the correlation accuracy of the predictions
correlation_accuracy <- cor(actuals_predictions)
correlation_accuracy

# Set a cut-off value
# probability >= 0.5 as stroke
# otherwise healthy
predicted_result <- ifelse(predicted_model >= 0.5, "stroke", "healthy")

# show the accuracy of the testing data prediction
testing_accuracy <- mean(predicted_result == testing_data$stroke_factor)
paste("The accuracy for testing data:", testing_accuracy)

library(ROCR)
# Use ROCR package
# to plot the ROC Curve to show the accuracy of model
prod <- predict(final_model, training_data, type = "response")
pred <- prediction(prod, training_data$stroke_factor)
perf <- performance(pred, measure = "tpr", x.measure = "fpr")
plot(perf)

# Calculate the AUC value
# if AUC >= 0.8 indicate model does good job in discriminating
# between the two categories which comprise response variable.
auc <- performance(pred, measure = "auc")
auc <- auc@y.values[[1]]
paste("The AUC values:", auc)

# Scenario for prediction
# Scenario 1 -
# age = 50, hypertension = yes, heart_disease = yes, avg_glucose_level = 150
df <- data.frame(age = 50, 
                 hypertension_factor = factor("yes"), 
                 heart_disease_factor = factor("yes"), 
                 avg_glucose_level = 150)
predicted_stroke <- predict(final_model, df, type = "response")
predicted_stroke

# Scenario 2 -
# age = 80, hypertension = yes, heart_disease = yes, avg_glucose_level = 150
df <- data.frame(age = 80, 
                 hypertension_factor = factor("yes"), 
                 heart_disease_factor = factor("yes"), 
                 avg_glucose_level = 150)
predicted_stroke <- predict(final_model, df, type = "response")
predicted_stroke

# Scenario 3 -
# age = 80, hypertension = yes, heart_disease = yes, avg_glucose_level = 240
df <- data.frame(age = 80, 
                 hypertension_factor = factor("yes"), 
                 heart_disease_factor = factor("yes"), 
                 avg_glucose_level = 240)
predicted_stroke <- predict(final_model, df, type = "response")
predicted_stroke

# Scenario 4 -
# age = 20, hypertension = no, heart_disease = yes, avg_glucose_level = 90
df <- data.frame(age = 20, 
                 hypertension_factor = factor("no"), 
                 heart_disease_factor = factor("yes"), 
                 avg_glucose_level = 90)
predicted_stroke <- predict(final_model, df, type = "response")
predicted_stroke

# Scenario 5 -
# age = 20, hypertension = yes, heart_disease = no, avg_glucose_level = 90
df <- data.frame(age = 20, 
                 hypertension_factor = factor("yes"), 
                 heart_disease_factor = factor("no"), 
                 avg_glucose_level = 90)
predicted_stroke <- predict(final_model, df, type = "response")
predicted_stroke

# Scenario 6 -
# age = 20, hypertension = yes, heart_disease = yes, avg_glucose_level = 240
df <- data.frame(age = 18, 
                 hypertension_factor = factor("yes"), 
                 heart_disease_factor = factor("yes"), 
                 avg_glucose_level = 300)
predicted_stroke <- predict(final_model, df, type = "response")
predicted_stroke

# Scenario 7 -
# age = 90, hypertension = no, heart_disease = no, avg_glucose_level = 90
df <- data.frame(age = 90, 
                 hypertension_factor = factor("no"), 
                 heart_disease_factor = factor("no"), 
                 avg_glucose_level = 90)
predicted_stroke <- predict(final_model, df, type = "response")
predicted_stroke
