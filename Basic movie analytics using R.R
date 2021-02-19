#ustawienie sciezki roboczej w miejscu pliku
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

#import plikow
readRDS('./data/movies.rds') -> movies
readRDS('./data/ratings.rds') -> ratings
readRDS('./data/users.rds') -> users

#tworzenie nowej kolumny "year" jako duplikat
year <- movies$title
movies["year"] <- year

#import pakietow
library(stringr) #obrobka tekstu
library(dplyr)
library(tibble)

# obrobka kolumny 'year' i 'title'
movies$title <- str_sub(movies$title,1, -8)

#####ODPOWIEDZ (a.
movies$year <- str_sub(movies$year,-5, -2)

#probowalem wczesniej takiej petli, ale nie dziala - nie wiem czemu:
#for (i in length(movies$year)){
#  movies$year[i] <- str_sub(movies$year[i],-6,-1)
#}

#zwraca tabele bez wartosci NaN
#####ODPOWIEDZ (b.
nona_movies <- movies[complete.cases(movies$year,movies$movieID),]

#zamiana kolumny 'year' na typ int
nona_movies$year <- as.integer(nona_movies$year)

#ponowne identyfikowanie wart NaN w kolumnie 'year'
nona_movies <- nona_movies[complete.cases(nona_movies$year),]

#struktura tabli - liczba obiektow i wystepujace typy wartosci

#####ODPOWIEDZ (c. #sprawdzam ilosc elementów uzywajac str()
str(nona_movies)

#zliczam filmy w poszczegolnych latach
nona_movies %>% count(year) -> movies_by_year

#zamiana nazwy kolumny 'n' na bardziej czytelna
names(movies_by_year)[names(movies_by_year) == "n"] <- "total_movies"

#sortuje tabele wg lat z najwieksza liczba filmow
movies_by_year[with(movies_by_year, order(total_movies, year,decreasing = TRUE)), ] -> sorted_by_year

#####ODPOWIEDZ (d. wyciagam top 5. wartosci
head(sorted_by_year,5)


users  %>% count(Gender) -> gender_quant

#suma uzytkownikow
sum(gender_quant[2])

#liczba kobiet
gender_quant$n[1]

#liczba mezczyzn
gender_quant$n[2]

#####ODPOWIEDZ (e.
#odsetek mezczyzn
male_percentage <- gender_quant$n[2]*100/sum(gender_quant[2])
male_percentage
#odsetek kobiet
female_percentage <- gender_quant$n[1]*100/sum(gender_quant[2])
female_percentage

#lacze dwie tabele z filmami i ocenami
movies_and_ratings <- ratings %>% inner_join(nona_movies,by="movieID")

#grupuje je po ocenach
aggregate(
  movies_and_ratings$Rating, list(
    movies_and_ratings$title
    ), mean) -> ratings_calculated

#dla porzadku nazywam kolumne
names(ratings_calculated)[names(ratings_calculated) == "Group.1"] <- "title"

#problem jest taki, ¿e istnieja filmy z ocenami 5.0, ale mala liczba glosow
#aby wyeliminowaæ problem zlicze liczbe glosow
#nastepnie zignoruje te filmy, ktore maja ich mniej niz 100 

#zliczam ilosc glosow
movies_and_ratings %>% count(title) -> rating_quant

#lacze dwie listy aby uworzyc tabele ze srednia ocena i iloscia glosow
ranking <- ratings_calculated %>% inner_join(rating_quant,by="title")

#porzadkuje nazwy kolumn
names(ranking)[names(ranking) == "x"] <- "rank_mean"
names(ranking)[names(ranking) == "n"] <- "rank_quant"

#odrzucam filmy ktore maja mniej niz 100 ocen
ranking <- filter(ranking, rank_quant > 100)

#sortuje je
ranking[with(ranking, order(rank_mean,decreasing = TRUE)), ] -> ranking

#####ODPOWIEDZ (f. najlepszy film wrzechczasow
ranking_1 <- head(ranking,1)

#chce zwrocic sam tytul
select (ranking_1,c(title))

#####ODPOWIEDZ (g. tworze ranking top 100
ranking_100 <- head(ranking,100)

#pomijam kolumne z iloscia glosow dla porzadku
select(ranking_100,-c(rank_quant))

# tworze wspolna tabele z ocenami i id uzytkownikow
full_df <- movies_and_ratings %>% inner_join(users,by="userID")

#tabela dla mezczyzn
male_rates <- filter(full_df, Gender == 'M')
#tabela dla kobiet
female_rates <- filter(full_df, Gender == 'F')

#funkcja, ktora wybiera top 10 filmow z podzialem na plec
rating_by_gender <- function(gender_rate){
  
  #wybieram potrzebne kolumny z tabeli
  gender_rate <- select(gender_rate, c(Rating, title))
  
  #zliczam glosy
  rates_count <- gender_rate %>% count(title)
  
  #zliczam srednia
  rates_mean <-aggregate(
    gender_rate$Rating, list(
      gender_rate$title
    ), mean)
  
  #porzadkuje nazwy kolumn
  names(rates_count)[names(rates_count) == "n"] <- "votes_quant"
  names(rates_mean)[names(rates_mean) == "x"] <- "rank"
  names(rates_mean)[names(rates_mean) == "Group.1"] <- "title"
  
  #tworze tabele ze srednia i iloscia glosow
  gender_ranking <- rates_mean %>% inner_join(rates_count,by="title")
  
  #odrzucam filmy < 100 ocen
  gender_ranking <- filter(gender_ranking, votes_quant > 100)
  
  #sortuje
  gender_ranking <- gender_ranking[with(gender_ranking, order(rank,decreasing = TRUE)), ]
  
  #wybieram top 10
  result <- head(gender_ranking, 10)
  result <- select(result, -c(votes_quant))
  
  #zwracam wynik
  return(result)
}

#wywoluje funkcje podstawiajac odpowiednie tabele
male_top_10   <- rating_by_gender(male_rates)
female_top_10 <-rating_by_gender(female_rates)

#####ODPOWIEDZ (h. wywoluje liste top 10 dla obu plci
male_top_10
female_top_10

#tworze instancje z potrzebnymi informacjami
clean_df <- select(full_df, c(title, year, Age,))

#zliczam wartosci
clean_df %>% count(Age)


#w tabeli sa oceny od osób majacych 1 rok zycia
#odrzucam je jako nieadekwatne - moga pochodzic od osob ktore nie chcialy udostepniac swojego wieku
#widac tez klarowne grupy bez gradientu wieku
clean_df <- filter(clean_df, Age != 1)

#wyciagam grupy wiekowe z tabeli
age_groups <- clean_df %>% count(Age)
age_groups$Age

#=funkcja ktora filtruje dane, wyciaga srednia i zaokragla
movie_by_year <- function(var_age){
  age_str <- sprintf('Dla grupy wiekowej %s. usredniony rok producji filmu wynosi:',var_age)
  result <- paste(c(age_str,round(
                                  mean(
                                    filter(clean_df, Age == var_age)$year),
                                      digits=0)))
  print(result)
}

#####ODPOWIEDZ (i. petla wywoluje odpowiedzi dla kazedj grupy wiekowej
for (i in age_groups$Age){
  movie_by_year(i)
}

#wybieram interesujace mnie kolumny
movies_and_gender <- select(full_df, c(title,Gender))

#funkcja
popular_gender_3 <- function(gender,gender_str){
    movies_and_gender <- filter(movies_and_gender,Gender == gender)
    count_by_gender <- movies_and_gender %>% count(title)
    sorted <- count_by_gender[with(count_by_gender, order(n, decreasing = TRUE)),]
    print(sprintf('Trzy najpopularniejsze filmy wsrod %s to:',gender_str))
    print(head(sorted,3)$title)
    
}

#####ODPOWIEDZ (j. wywoluje funkcje podajac zmienne tekstowe
popular_gender_3('M','mezczyzn')
popular_gender_3('F','kobiet')

