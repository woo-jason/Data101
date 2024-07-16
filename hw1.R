getwd()

airbnb <- read.csv('airbnb.csv')
movies <- read.csv('Movies2022F-4.csv')

names(airbnb)

table(airbnb$neighbourhood_group)

airbnb_manhattan <- subset(airbnb, neighbourhood_group== 'Manhattan')
manhattan_15 <- airbnb_manhattan[airbnb_manhattan$floor > 15,]
answer <- nrow(manhattan_15)

min(airbnb_manhattan$price)

airbnb_queens <- subset(airbnb, neighbourhood_group== 'Queens')

min(airbnb_queens[airbnb_queens$floor > 12,]$price)

ZTest::z_test_from_agg(mean_a = 2.1, mean_b = 2.8, sd_a = 3, 
                       sd_b = 1, n_a = 50, n_b = 100)
