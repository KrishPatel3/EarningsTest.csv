# Load necessary libraries
library(rpart)

# Step 1: Load EarningsTrain.csv dataset
EarningsTrain <- read.csv("EarningsTrain.csv")

# Step 2: Explore the data
summary(EarningsTrain)

# Step 3: Preprocess the data if necessary

# Step 4: Build predictive model using rpart
# Example: Using all predictors
model <- rpart(Earnings ~ ., data = EarningsTrain)

# Step 5: Evaluate model performance
EarningsTrain$pred.Earnings <- predict(model, EarningsTrain)
MSE <- mean((EarningsTrain$Earnings - EarningsTrain$pred.Earnings)^2)
cat("MSE on training data:", MSE, "\n")

# Step 6: Apply trained model to EarningsTest data
EarningsTest <- read.csv("EarningsTest.csv")
EarningsTest$predEarnings <- predict(model, EarningsTest)

# Step 7: Save modified EarningsTest.csv file
write.csv(EarningsTest, "Earnings_Test.csv", row.names = FALSE)

# Step 8: Calculate MSE using predictions and actual values from training data
MSE_train <- mean((EarningsTrain$Earnings - EarningsTrain$pred.Earnings)^2)
cat("MSE on training data:", MSE_train, "\n")

