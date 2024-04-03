library(rpart)
library(rpart.plot)

train<-read.csv('https://raw.githubusercontent.com/paramshah4/data101_tutorial/main/files/dataset/movies2023Tr.csv')

head(train)
summary(train)
nrow(train)

test<-read.csv('https://raw.githubusercontent.com/paramshah4/data101_tutorial/main/files/dataset/movies2023Test_Student.csv')

head(test)
summary(test)
nrow(test)

submission<-read.csv('https://raw.githubusercontent.com/paramshah4/data101_tutorial/main/files/dataset/submissionMovies2023.csv')

head(submission)
summary(submission)

# Let's run some data analysis on the two numerical variables
summary(train$Audience)
summary(test$Audience)


# Since the income and audience have different values for testing and training, 
# Let's attempt to normalize this relationship through a ration

train$income_by_drama <- train$Income/train$Audience
test$income_by_drama <- test$Income/test$Audience

# If we take the summary now
summary(train$income_by_drama)
summary(test$income_by_drama)


train$drama <- train$Genre == "Drama"
train$R <- train$Content == "R"

test$drama <- test$Genre == "Drama"
test$R <- test$Content == "R"

head(train)


n2tree <- rpart(RATING ~ income_by_drama+drama+R, data = train, method = "class")
n2tree
rpart.plot(n2tree)
pr3d <- predict(n2tree, train, type="class")
mean(train$RATING==pr3d)

# With just three variables, our accuracy jumps to a 94.9%

# Let's a build a prediction model for the testing data
pr4d <- predict(n2tree, test, type="class")

submission$RATING<-pr4d
write.csv(submission, '/Users/savani_naman/submission.csv', row.names=FALSE)

