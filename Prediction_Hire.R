hire <- read.csv("HireTrainApr10.csv")

test <- read.csv("test_challenge1.csv")
submission <- read.csv("sample_submission_challenge1.csv")

#chisq test
data1 <- table(hire$Coding, hire$Hired)
chisq.test(data1)

data2 <- table(hire$Impression, hire$Hired)
chisq.test(data2)

data3 <- table(hire$Major, hire$Hired)
chisq.test(data3)

data4 <- table(hire$College, hire$Hired)
chisq.test(data4)

#get to know data
colnames(hire)
nrow(hire)
summary(hire)
unique(hire$Coding)
unique(hire$Impression)
unique(hire$Major)
unique(hire$College)
unique(hire$Hired)
table(hire$Coding)
table(hire$Impression)
table(hire$Major)
table(hire$College)
table(hire$Hired)

#subsetting
table(hire[hire$Coding =='Excellent',]$Hired)
table(hire[hire$Coding =='OK',]$Hired)
table(hire[hire$Coding =='Weak',]$Hired)

table(hire[hire$Impression == 'Nerdy',]$Hired)
table(hire[hire$Impression == 'Outgoing',]$Hired)
table(hire[hire$Impression == 'Confident',]$Hired)
table(hire[hire$Impression == 'Shy',]$Hired)

table(hire[hire$Impression == 'Nerdy' & hire$College == 'Redbrick',]$Hired)
table(hire[hire$Impression == 'Nerdy' & hire$College == 'PJIT',]$Hired)
table(hire[hire$Impression == 'Nerdy' & hire$College == 'BYU',]$Hired)
table(hire[hire$Impression == 'Nerdy' & hire$College == 'BestCollege',]$Hired)
table(hire[hire$Impression == 'Nerdy' & hire$College == 'Peters',]$Hired)

table(hire[hire$Impression == 'Outgoing' & hire$College == 'Redbrick',]$Hired)
table(hire[hire$Impression == 'Outgoing' & hire$College == 'PJIT',]$Hired)
table(hire[hire$Impression == 'Outgoing' & hire$College == 'BYU',]$Hired)
table(hire[hire$Impression == 'Outgoing' & hire$College == 'BestCollege',]$Hired)
table(hire[hire$Impression == 'Outgoing' & hire$College == 'Peters',]$Hired)

table(hire[hire$Impression == 'Confident' & hire$College == 'Redbrick',]$Hired)
table(hire[hire$Impression == 'Confident' & hire$College == 'PJIT',]$Hired)
table(hire[hire$Impression == 'Confident' & hire$College == 'BYU',]$Hired)
table(hire[hire$Impression == 'Confident' & hire$College == 'BestCollege',]$Hired)
table(hire[hire$Impression == 'Confident' & hire$College == 'Peters',]$Hired)

myprediction <- hire
decision <- rep('No',nrow(myprediction))
decision[myprediction$Coding!="Weak"] <- 'Yes'
decision[myprediction$Impression=="Nerdy" & myprediction$College=="Redbrick"] <- 'Yes'
decision[myprediction$Impression=="Outgoing" & myprediction$College=="Peters"] <- 'Yes'
decision[myprediction$Impression=="Confident" & myprediction$College=="PJIT"] <- 'Yes'
myprediction$Hired <- decision
error <- mean(hire$Hired != myprediction$Hired)
error

#Cross validation
v<-sample(1:nrow(hire))
v[1:5]
hireScrambled<-hire[v, ]
hireSample<-hireScrambled[nrow(hireScrambled)-1800:nrow(hireScrambled), ]
mypred<-hireSample

decision <- rep('No',nrow(mypred))
decision[mypred$Coding!="Weak"] <- 'Yes'
decision[mypred$Impression=="Nerdy" & mypred$College=="Redbrick"] <- 'Yes'
decision[mypred$Impression=="Outgoing" & mypred$College=="Peters"] <- 'Yes'
decision[mypred$Impression=="Confident" & mypred$College=="PJIT"] <- 'Yes'

mypred$Hired <- decision[1:nrow(hireSample)]
error <- mean(hireSample$Hired != mypred$Hired)
error

#submission
myprediction <- test
decision <- rep('No',nrow(myprediction))
decision[myprediction$Coding!="Weak"] <- 'Yes'
decision[myprediction$Impression=="Nerdy" & myprediction$College=="Redbrick"] <- 'Yes'
decision[myprediction$Impression=="Outgoing" & myprediction$College=="Peters"] <- 'Yes'
decision[myprediction$Impression=="Confident" & myprediction$College=="PJIT"] <- 'Yes'
submission$Prediction <- decision
write.csv(submission,'submission.csv',row.names = FALSE)
