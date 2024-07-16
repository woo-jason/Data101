movies <- read.csv("Movies2022F-4.csv")

#1
moviesA <- movies[movies$content == 'PG',]
tapply(moviesA$imdb_score, moviesA$genre, mean)
ZTest::z_test_from_data(moviesA,"genre","imdb_score","Comedy","Drama")
PermutationTestSecond::Permutation(moviesA,"genre","imdb_score",1000,"Family","Drama")

#2
moviesB <- movies[movies$content == 'PG-13',]
tapply(moviesB$imdb_score, moviesB$genre, mean)
ZTest::z_test_from_data(moviesB,"genre","imdb_score","Sci-Fi","History")
PermutationTestSecond::Permutation(moviesB,"genre","imdb_score",1000,"Sci-Fi","History")

#3
sd(movies$imdb_score[(movies$Budget == 'Medium' & movies$genre == 'Action')])
mean(movies$imdb_score[(movies$Budget == 'Medium' & movies$genre == 'Action')])
sum(movies$Budget == 'Medium' & movies$genre == 'Action')

sd(movies$imdb_score[(movies$Budget == 'Low' & movies$genre == 'Comedy')])
mean(movies$imdb_score[(movies$Budget == 'Low' & movies$genre == 'Comedy')])
sum(movies$Budget == 'Low' & movies$genre == 'Comedy')

ZTest::z_test_from_agg(mean_a = 6.465654, mean_b = 6.76466, sd_a = 0.9543413, 
                       sd_b = 1.198237, n_a = 1146, n_b = 103)

#4
moviesC <- movies[movies$genre == 'Action',]
tapply(moviesC$imdb_score, moviesC$content, mean)
ZTest::z_test_from_data(moviesC,"content","imdb_score","PG","R")

#5
sd(movies$imdb_score[(movies$Budget == 'Medium' & movies$content == 'R')])
mean(movies$imdb_score[(movies$Budget == 'Medium' & movies$content == 'R')])
sum(movies$Budget == 'Medium' & movies$content == 'R')

sd(movies$imdb_score[(movies$Budget == 'High' & movies$content == 'G')])
mean(movies$imdb_score[(movies$Budget == 'High' & movies$content == 'G')])
sum(movies$Budget == 'High' & movies$content == 'G')

ZTest::z_test_from_agg(mean_a = 6.468047, mean_b = 6.676305, sd_a = 1.041859, 
                       sd_b = 0.9132732, n_a = 128, n_b = 2850)

#6
sd(movies$imdb_score[(movies$Gross == 'Medium' & movies$genre == 'Action')])
mean(movies$imdb_score[(movies$Gross == 'Medium' & movies$genre == 'Action')])
sum(movies$Gross == 'Medium' & movies$genre == 'Action')

sd(movies$imdb_score[(movies$Gross == 'Medium' & movies$genre == 'Comedy')])
mean(movies$imdb_score[(movies$Gross == 'Medium' & movies$genre == 'Comedy')])
sum(movies$Gross == 'Medium' & movies$genre == 'Comedy')

ZTest::z_test_from_agg(mean_a = 6.317036, mean_b = 6.36696, sd_a = 1.092991, 
                       sd_b = 0.9825533, n_a = 1805, n_b = 944)
