#https://www.kaggle.com/datasets/mohansacharya/graduate-admissions

getwd()
# Renamed variables for better readability
setwd("C:/Users/User/Desktop/Personal Projects/Graduate Admission Prediction")

library(tidyverse)
library(GGally)
library(caret)
library(performance)
library(MLmetrics)
library(lmtest)
library(car)
library(corrplot)

admission <- read.csv("Admission_Predict_Ver1.1.csv")
View(admission)

admission <- admission %>%
  mutate(University_Rating = as.factor(University.Rating)) %>%
  select(-Serial.No.)

anyNA(admission)
plot(admission, col="darkblue")


ggpairs(admission,lower = list(continuous = wrap('points', colour = "darkblue")),
        diag = list(continuous = wrap("barDiag", colour = "red")))

set.seed(1234)
inTrain <- createDataPartition(admission$Chance.of.Admit, p = 0.9, list = FALSE)
train <- admission[inTrain,]
test <- admission[-inTrain,]

adm_model <- lm(Chance.of.Admit ~ ., train)
summary(adm_model)

hist(resid(adm_model), col = 'darkblue', main = "Residual Disribution", xlab = "Residuals")


model_backward <- step(object = adm_model, direction = "backward")
model_backward$call

RMSE(y_pred = model_backward$fitted.values, y_true = admission$Chance_of_Admit)

bptest(model_backward)

plot(x = model_backward$fitted.values, y = model_backward$residuals, 
     xlab = "Fitted Values", ylab = "Residuals", 
     main = "Residual Plot", col = "blue")

abline(h = 0, col = "red", lty = 2)

adm_test <- predict(adm_model, test)
test$predicted_adm_model <- adm_test

test <- test %>%
  rename(Predicted_Values = predicted_adm_model)

test %>% 
  select(Predicted_Values, Chance.of.Admit) %>%
  head()

test %>%
  ggplot(aes(x = Chance.of.Admit, y = Predicted_Values)) +
  geom_point(alpha = 0.8, color = "darkblue") +
  geom_abline(intercept = 0, slope = 1, color = "blue", size = 1.5) +
  theme_minimal()
