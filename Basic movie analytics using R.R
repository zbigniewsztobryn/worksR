
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

readRDS('./data/movies.rds') -> movies
readRDS('./data/ratings.rds') -> ratings
readRDS('./data/users.rds') -> users

year <- movies$title
movies["year"] <- year

library(stringr) #obrobka tekstu
library(dplyr)
library(tibble)

movies$title <- str_sub(movies$title,1, -8)

movies$year <- str_sub(movies$year,-5, -2)

nona_movies <- movies[complete.cases(movies$year,movies$movieID),]

nona_movies$year <- as.integer(nona_movies$year)

nona_movies <- nona_movies[complete.cases(nona_movies$year),]

str(nona_movies)

nona_movies %>% count(year) -> movies_by_year

names(movies_by_year)[names(movies_by_year) == "n"] <- "total_movies"

movies_by_year[with(movies_by_year, order(total_movies, year,decreasing = TRUE)), ] -> sorted_by_year

head(sorted_by_year,5)

users  %>% count(Gender) -> gender_quant

sum(gender_quant[2])
gender_quant$n[1]
gender_quant$n[2]

male_percentage <- gender_quant$n[2]*100/sum(gender_quant[2])
male_percentage

female_percentage <- gender_quant$n[1]*100/sum(gender_quant[2])
female_percentage

movies_and_ratings <- ratings %>% inner_join(nona_movies,by="movieID")

aggregate(
  movies_and_ratings$Rating, list(
    movies_and_ratings$title
    ), mean) -> ratings_calculated

names(ratings_calculated)[names(ratings_calculated) == "Group.1"] <- "title"

movies_and_ratings %>% count(title) -> rating_quant

ranking <- ratings_calculated %>% inner_join(rating_quant,by="title")

names(ranking)[names(ranking) == "x"] <- "rank_mean"
names(ranking)[names(ranking) == "n"] <- "rank_quant"

ranking <- filter(ranking, rank_quant > 100)

ranking[with(ranking, order(rank_mean,decreasing = TRUE)), ] -> ranking

ranking_1 <- head(ranking,1)

select (ranking_1,c(title))

ranking_100 <- head(ranking,100)

select(ranking_100,-c(rank_quant))

full_df <- movies_and_ratings %>% inner_join(users,by="userID")

male_rates <- filter(full_df, Gender == 'M')
female_rates <- filter(full_df, Gender == 'F')

rating_by_gender <- function(gender_rate){
  
  gender_rate <- select(gender_rate, c(Rating, title))
  rates_count <- gender_rate %>% count(title)
  
  rates_mean <-aggregate(
    gender_rate$Rating, list(
      gender_rate$title
    ), mean)
  
  names(rates_count)[names(rates_count) == "n"] <- "votes_quant"
  names(rates_mean)[names(rates_mean) == "x"] <- "rank"
  names(rates_mean)[names(rates_mean) == "Group.1"] <- "title"

  gender_ranking <- rates_mean %>% inner_join(rates_count,by="title")
  gender_ranking <- filter(gender_ranking, votes_quant > 100)
  gender_ranking <- gender_ranking[with(gender_ranking, order(rank,decreasing = TRUE)), ]
  
  result <- head(gender_ranking, 10)
  result <- select(result, -c(votes_quant))
  
  return(result)
}

male_top_10   <- rating_by_gender(male_rates)
female_top_10 <-rating_by_gender(female_rates)

male_top_10
female_top_10

clean_df <- select(full_df, c(title, year, Age,))
clean_df %>% count(Age)

clean_df <- filter(clean_df, Age != 1)

age_groups <- clean_df %>% count(Age)
age_groups$Age

movie_by_year <- function(var_age){
  age_str <- sprintf('Dla grupy wiekowej %s. usredniony rok producji filmu wynosi:',var_age)
  result <- paste(c(age_str,round(
                                  mean(
                                    filter(clean_df, Age == var_age)$year),
                                      digits=0)))
  print(result)
}

for (i in age_groups$Age){
  movie_by_year(i)
}

movies_and_gender <- select(full_df, c(title,Gender))

popular_gender_3 <- function(gender,gender_str){
    movies_and_gender <- filter(movies_and_gender,Gender == gender)
    count_by_gender <- movies_and_gender %>% count(title)
    sorted <- count_by_gender[with(count_by_gender, order(n, decreasing = TRUE)),]
    print(sprintf('Trzy najpopularniejsze filmy wsrod %s to:',gender_str))
    print(head(sorted,3)$title)
    
}

popular_gender_3('M','mezczyzn')
popular_gender_3('F','kobiet')

