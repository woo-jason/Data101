install.packages("devtools")
devtools::install_github("devanshagr/PermutationTestSecond")
devtools::install_github("devanshagr/PermutationTestManual")
devtools::install_github("https://github.com/rishirich/odds")
devtools::install_github("janish-parikh/ZTest")

Hire <- read.csv("HireWide.csv")

ZTest::z_test_from_agg(mean_a = 6.468047, mean_b = 6.676305, sd_a = 1.041859, 
                       sd_b = 0.9132732, n_a = 128, n_b = 2850)

ZTest::z_test_from_data(Hire,"content","Age","PG","R")

Prior<-nrow(Hire[Hire$Hired =='Yes',])/nrow(Hire)
Prior
PriorOdds<-round(Prior/(1-Prior),2)
PriorOdds
TruePositive<-round(nrow(Hire[Hire$Coding=='Excellent'& Hire$Hired=='Yes',])/nrow(Hire[Hire$Hired=='Yes',]),2)
TruePositive
FalsePositive<-round(nrow(Hire[Hire$Coding=='Excellent'& Hire$Hired!='Yes',])/nrow(Hire[Hire$Hired!='Yes',]),2)
FalsePositive
LikelihoodRatio<-round(TruePositive/FalsePositive,2)
LikelihoodRatio
PosteriorOdds <-LikelihoodRatio * PriorOdds
PosteriorOdds
Posterior <-PosteriorOdds/(1+PosteriorOdds)
Posterior

odds::likelihoods (Hire, 'Impression', 'Major')

mean(Hire[Hire$Coding=='Excellent',]$Age) 
mean(Hire[Hire$Coding=='Weak',]$Age) 

Permutation(Hire, "Coding", "Age",10000, "Excellent", "Weak") 

PriorOdds <- 1:4

Prior<-nrow(Hire[Hire$Hired =='Yes',])/nrow(Hire)
Prior
PriorOdds<-0.25
PriorOdds
TruePositive<-0.2
TruePositive
FalsePositive<-0.1
FalsePositive
LikelihoodRatio<-round(TruePositive/FalsePositive,2)
LikelihoodRatio
PosteriorOdds <-LikelihoodRatio * PriorOdds
PosteriorOdds
Posterior <-PosteriorOdds/(1+PosteriorOdds)
Posterior
