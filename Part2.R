install.packages("ModelMetrics")
library(ModelMetrics)

library(rpart)
library(rpart.plot)

income_train <- read.csv('https://raw.githubusercontent.com/paramshah4/data101_tutorial/main/files/dataset/incomeTrain2023.csv')
income_test <- read.csv('https://raw.githubusercontent.com/paramshah4/data101_tutorial/main/files/dataset/IncomeTest2023_Kaggle.csv')
submission <- read.csv("/Users/savani_naman/Documents/submission.csv")

income_train$CollegeLocation <- income_train$College_location
income_train$SquareLi <- income_train$LinkedIN ^ 2
income_test$SquareLi <- income_test$LinkedIN ^ 2

# Linear Regression Model for Major=="Other"
model1 <- lm(Salary ~ SquareLi, data = income_train[income_train$Major == "Other", ])

# Linear Regression Model for Major=="Buisness" and DOB%%2==0
model201 <- lm(Salary ~ GPA, data=income_train[income_train$Major == "Buisness" & income_train$DOB%%2 == 0, ])
model202 <- lm(Salary ~ GPA, data=income_train[income_train$Major == "Buisness" & income_train$DOB%%2 == 1, ])

# Linear Regression Models for all other Majors with GPA
model3 <- lm(Salary ~ GPA, data = income_train[income_train$Major == "Humanities", ])
model4 <- lm(Salary ~ GPA, data = income_train[income_train$Major == "STEM", ])
model5 <- lm(Salary ~ GPA, data = income_train[income_train$Major == "Vocational", ])
model6 <- lm(Salary ~ GPA, data = income_train[income_train$Major == "Professional", ])

# Predictions
pred1 <- predict(model1, newdata = income_train[income_train$Major == "Other", ])
pred201 <- predict(model201, newdata = income_train[income_train$Major == "Buisness" & income_train$DOB%%2 == 0, ])
pred202 <- predict(model202, newdata = income_train[income_train$Major == "Buisness" & income_train$DOB%%2 == 1, ])
pred3 <- predict(model3, newdata = income_train[income_train$Major == "Humanities", ])
pred4 <- predict(model4, newdata = income_train[income_train$Major == "STEM", ])
pred5 <- predict(model5, newdata = income_train[income_train$Major == "Vocational", ])
pred6 <- predict(model6, newdata = income_train[income_train$Major == "Professional", ])



# Combine Predictions
decision <- rep(0, nrow(income_train))
decision[income_train$Major == "Other"] <- pred1
decision[income_train$Major == "Buisness" & income_train$DOB%%2 == 0] <- pred201
decision[income_train$Major == "Buisness" & income_train$DOB%%2 == 1] <- pred202
decision[income_train$Major == "Humanities"] <- pred3
decision[income_train$Major == "STEM"] <- pred4
decision[income_train$Major == "Vocational"] <- pred5
decision[income_train$Major == "Professional"] <- pred6



# Calculate Mean Squared Error
mse <- mean((decision - income_train$Salary)^2)
mse 

mean_mse <- mse(income_train$Salary, decision)
mean_mse

# Let's write the submission file

pred1_final <- predict(model1, newdata = income_test[income_test$Major == "Other", ])
pred201_final <- predict(model201, newdata = income_test[income_test$Major == "Buisness" & income_train$DOB%%2 == 0, ])
pred202_final <- predict(model202, newdata = income_test[income_test$Major == "Buisness" & income_train$DOB%%2 == 1, ])
pred3_final <- predict(model3, newdata = income_test[income_test$Major == "Humanities", ])
pred4_final <- predict(model4, newdata = income_test[income_test$Major == "STEM", ])
pred5_final <- predict(model5, newdata = income_test[income_test$Major == "Vocational", ])
pred6_final <- predict(model6, newdata = income_test[income_test$Major == "Professional", ])

final_decision <- rep(0, nrow(income_test))
final_decision[income_test$Major == "Other"] <- pred1_final
final_decision[income_test$Major == "Buisness" & income_train$DOB%%2 == 0] <- pred201_final
final_decision[income_test$Major == "Buisness" & income_train$DOB%%2 == 1] <- pred202_final
final_decision[income_test$Major == "Humanities"] <- pred3_final
final_decision[income_test$Major == "STEM"] <- pred4_final
final_decision[income_test$Major == "Vocational"] <- pred5_final
final_decision[income_test$Major == "Professional"] <- pred6_final

ids<-c(1:nrow(income_test))

submission$sample_Salary<-final_decision
submission
write.csv(submission, '/Users/savani_naman/Documents/submission.csv', row.names=FALSE)

