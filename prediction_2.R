library(rpart)
library(rpart.plot)
hire <- read.csv("HireRTrain1-1.csv")

test <- read.csv("test_challenge2.csv")
submission <- read.csv("sample_submission2.csv")

tree <- rpart(Hired ~ Coding+TikTokFOLLOWERS+TwitterFOLLOWERS+TwitterFOLLOWING, data = hire, 
              method = "class",control = rpart.control(cp = 0.005))

rpart.plot(tree)

pred <- predict(tree, hire, type = "class")
head(pred)

#cross validation
mean(hire$Hired==pred)
CrossValidation::cross_validate(hire,tree,5,0.7)

model1 <- rpart(Hired~., data = hire[hire$TikTokFOLLOWERS>=4710 & hire$Coding!="Weak",]);
model2 <- rpart(Hired~., data = hire[hire$TikTokFOLLOWERS>=4710 & hire$Coding=="Weak" & hire$TikTokFOLLOWERS>=7954,]);
model3 <- rpart(Hired~., data = hire[hire$TikTokFOLLOWERS<4710 & hire$TwitterFOLLOWERS>=6881 & hire$TwitterFOLLOWING<4335,])
model4 <- rpart(Hired~., data = hire[hire$TikTokFOLLOWERS>=4710,]);

model1
model2
model3
model4

pred1 <- predict(model1, newdata=hire[hire$TikTokFOLLOWERS >= 4710 & hire$Coding != "Weak",], type = "class")
pred2 <- predict(model2, newdata=hire[hire$TikTokFOLLOWERS >= 4710 & hire$Coding == "Weak" & hire$TikTokFOLLOWERS >= 7954,], type = "class")
pred3 <- predict(model3, newdata=hire[hire$TikTokFOLLOWERS < 4710 & hire$TwitterFOLLOWERS >= 6881 & hire$TwitterFOLLOWING < 4335,], type = "class")
pred4 <- predict(model4, newdata=hire[hire$TikTokFOLLOWERS >= 4710,], type = "class")
myprediction <- hire

decision <- rep('No',nrow(myprediction))
decision[myprediction$TikTokFOLLOWERS >= 4710 & myprediction$Coding != "Weak"] <- as.character(pred1)
decision[myprediction$TikTokFOLLOWERS >= 4710 & myprediction$Coding == "Weak" & myprediction$TikTokFOLLOWERS >= 7954] <- as.character(pred2)
decision[myprediction$TikTokFOLLOWERS < 4710 & myprediction$TwitterFOLLOWERS >= 6881 & myprediction$TwitterFOLLOWING < 4335] <- as.character(pred3)
decision[myprediction$TikTokFOLLOWERS >= 4710] <- as.character(pred4)

myprediction$Hired <- decision

error <- mean(hire$Hired != myprediction$Hired)
error

#submission
myprediction <- test
decision <- rep('No',nrow(myprediction))
decision[myprediction$TikTokFOLLOWERS >= 4710 & myprediction$Coding != "Weak"] <- 'Yes'
decision[myprediction$TikTokFOLLOWERS >= 4710 & myprediction$Coding == "Weak" & myprediction$TikTokFOLLOWERS >= 7954] <- 'Yes'
decision[myprediction$TikTokFOLLOWERS < 4710 & myprediction$TwitterFOLLOWERS >= 6881 & myprediction$TwitterFOLLOWING < 4335] <- 'Yes'
submission$Prediction <- decision
write.csv(submission,'submission.csv',row.names = FALSE)
