movies <- read.csv("IMDB Top 250 Movies.csv")

colors <- c('red', 'blue','cyan', 'yellow', 'green')

avg_rating <- mean(movies$rating) #8.31
pg_13 <- table(movies$certificate == "PG-13") #37
pg <- table(movies$certificate == "PG") #35
r <- table(movies$certificate == "R") #97

boxplot(rating~certificate,data=movies,
        xlab="Certificate",ylab="Rating", main="Certificate vs Rating",col=colors,border="black")

avg_pg13_rating <- mean(movies[movies$certificate == "PG-13",]$rating) #8.37 - above average
avg_r_rating <- mean(movies[movies$certificate == "R",]$rating) #8.33 - above average below PG-13
avg_pg_rating <- mean(movies[movies$certificate == "PG",]$rating) #8.29 - above average below R/PG-13

PermutationTestSecond::Permutation(movies,"certificate","rating",10000,"PG","PG-13")

PermutationTestSecond::Permutation(movies,"certificate","rating",10000,"PG","R")
