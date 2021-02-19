#ustawienie sciezki roboczej w miejscu pliku
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

#funkcja sprawdzajaca czy wybrany pakiet jest zainstalowany
pkgTest <- function(x)
{
  if (!require(x,character.only = TRUE))
  {
    install.packages(x,dep=TRUE)
    if(!require(x,character.only = TRUE)) stop("Package not found")
  }
}

#sprawdzenie i instalacja pakietu
pkgTest("ggplot2movies")
pkgTest("randomcoloR")



#import pakietu
library(ggplot2)
library(dplyr)
library(ggplot2movies)
library(plyr)
library(randomcoloR)

#funkcja do drukowania dwoch wykresow
multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
  library(grid)
  
  # Make a list from the ... arguments and plotlist
  plots <- c(list(...), plotlist)
  
  numPlots = length(plots)
  
  # If layout is NULL, then use 'cols' to determine layout
  if (is.null(layout)) {
    # Make the panel
    # ncol: Number of columns of plots
    # nrow: Number of rows needed, calculated from # of cols
    layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                     ncol = cols, nrow = ceiling(numPlots/cols))
  }
  
  if (numPlots==1) {
    print(plots[[1]])
    
  } else {
    # Set up the page
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))
    
    # Make each plot, in the correct location
    for (i in 1:numPlots) {
      # Get the i,j matrix positions of the regions that contain this subplot
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))
      
      print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                      layout.pos.col = matchidx$col))
    }
  }
}

movies -> df

#tabel pomocnicza ze liczba wystapien poszczegolnych kategorii
df_cat_count<- data.frame(
  categ = c("Action", "Animation", "Comedy","Drama", "Documentary", "Romance","Short"),
  number = c(sum(df$Action),
             sum(df$Animation),
             sum(df$Comedy),
             sum(df$Drama),
             sum(df$Documentary),
             sum(df$Romance),
             sum(df$Short))
)

#drukowanie wykresu
pie_plot <- ggplot(df_cat_count, aes(x="", y=number, fill=categ))+
            geom_bar(width = 1, stat = "identity")+
            coord_polar("y", start=0)+
            #geom_text(aes(label = paste(round(number / sum(number) * 100, 1), "%")),
            geom_text(aes(label = paste(number)),
            position = position_stack(vjust = 0.5), size = 3)+
            labs(x = '', 
                 y= '',
                 title='Number of movies by category',
                 legend= "Unempl.",
                 caption= 'diag. [3.1]')+
            scale_fill_discrete(name = "Categories:")

pie_plot

#ocenie jak budzet ma sie do oceny
#wybieram interesujace kolumny
rating_to_budget <- select(df,c(budget,rating,votes,year,title))

#odrzucam wiersze z wartosciami NaN
rating_to_budget <- na.omit(rating_to_budget)

#odrzucam filmy, ktore maja mniej niz 1000 ocen - 
#sa to filmy zbyt malo popularne aby je uwzgledniac w tej zaleznosci
rating_to_budget <- filter(rating_to_budget, votes > 1000)

#kresle wykres budzetu do oceny filmu z uwzglednieniem popularnosci filmu
#ktore oznaczam kolorem i wielkoscia
rating_plot <- ggplot(rating_to_budget,
                      aes(x = budget, y = rating,color=votes ,size=votes)) +
                      geom_point() +
                      scale_size(range = c(2,4))+
                      scale_colour_gradient(
                                            low = "#0000FF",
                                            high = "#FF0000"
                                          )+
                      labs(x = 'Budget', 
                           y= 'Rating',
                           title='Rating to budget relation',
                           caption= 'diag. [4.1]')

rating_plot

#Inflacja sprawia, ze trudno jest jednoznacznie porownac budzety na przestrzeni
#lat, biorac pod uwage jedynie wartosci nominalne.
#Aby uwzglednic kluczowa w tym przypadku inflacje posluze sie wzpolczynnikiem CPI 
#ktory pozwala mi okreslic wartosc dolara z dowolnego roku (w odniesieniu do r. 2020)

#pobieram tabele ze wspolczynnikami CPI dla poszczegolnych lat i obrabiam dane
library('lubridate')

monthly_cpi <-
  read.table("http://research.stlouisfed.org/fred2/data/CPIAUCSL.txt",
             skip = 53, header = TRUE)
monthly_cpi$cpi_year <- year(monthly_cpi$DATE)
yearly_cpi <- select(monthly_cpi,c(VALUE, cpi_year))
yearly_cpi <- aggregate(yearly_cpi$VALUE, list(yearly_cpi$cpi_year), mean)

#zanim zlacze tabele podmieniam nazwe kolumny
names(yearly_cpi)[names(yearly_cpi) == "Group.1"] <- "year"
names(yearly_cpi)[names(yearly_cpi) == "x"] <- "cpi"


#lacze kolumny aby uzyskac wspolczynnik CPI w tabeli dla kazdego filmu
rating_to_budget <- yearly_cpi %>% inner_join(rating_to_budget,by="year")

#wyliczam zmienne aby podstawic je do wzoru:
#Real Amount = Dollar amount × Ending-period CPI ÷ Beginning-period CPI

#Ending-period CPI:
year_end_period <- filter(yearly_cpi, year == 2020)
year_end_period <- year_end_period$cpi

#dodaje kolumne z kwota wyliczona wg podanego wzoru
rating_to_budget <- rating_to_budget %>%
  mutate(real_budget = budget*year_end_period/cpi)

#tworze zaktualizowany wykres w oparciu o realne wartosci budzetow
updated_rating_plot <- ggplot(rating_to_budget,
                      aes(x = real_budget, y = rating,color=votes ,size=votes)) +
                      geom_point() +
                      scale_size(range = c(2,4))+
                      scale_colour_gradient(
                        low = "#0000FF",
                        high = "#FF0000")+
                      labs(x = 'Budget (with CPI)', 
                         y= 'Rating',
                         title='Rating to budget (CPI included)',
                         caption= 'diag. [4.2]')
updated_rating_plot

#wykres wyglada ciekawie - szczegolnie widoczna jedna wartosc odstajaca

sorted <- rating_to_budget[with(rating_to_budget, order(real_budget,decreasing = TRUE)), ]
head(sorted, 5)

#Po sprawdzeniu dowiadujemy sie, ze ta odstajaca wartosc to film "Kleopatra"
#Okazuje sie, ¿e gdybysmy chcieli nakreciæ taki film dzisiaj,
#to po uwzglednieniu inflacji kosztowalaby ok 371 mln $. Niezle!
#Przy czym wartosc nominalna wskazuje 44mln$ - dosc mylace

#jeszcze raz sprawdzam jak wykres wyglada bez skrajnych wartosci

rating_to_budget <- filter(rating_to_budget, real_budget < 3e+8)
updated_rating_plot <- ggplot(rating_to_budget,
                              aes(x = real_budget, y = rating,color=votes ,size=votes)) +
                              geom_point() +
                              scale_size(range = c(2,4)) +
                              scale_colour_gradient(
                                low = "#0000FF",
                                high = "#FF0000"
                              )+
                              labs(x = 'Budget (with CPI)',
                                   y= 'Rating',
                                   title='Rating to budget (CPI included)',
                                   caption= 'diag. [4.2]')
updated_rating_plot

#teraz tylko porownam z ciekawosci jak wyglada wykres przed 
#i po uwzglednieniu inflacji
multiplot(rating_plot,updated_rating_plot)


#### NASTEPNA CZESC ZADANIA

#wybieram kolumny do pracy i je obrabiam
budget_in_time <- select(df, c(year, 
                               budget, 
                               Action, 
                               Animation, 
                               Comedy,
                               Drama,
                               Documentary,
                               Romance,
                               Short
                               ))
budget_in_time <- na.omit(budget_in_time)

budget_in_time <- yearly_cpi %>% inner_join(budget_in_time,by="year")
budget_in_time <- budget_in_time %>%
  mutate(real_budget = round_any((budget*year_end_period/cpi),1000))


#ggplot(filter(budget_in_time, Action == 1), aes(x = year, y = real_budget)) +
  #geom_point()
  #facet_wrap(~Action)

#Probowalem zestawic wykresy ze soba za pomoca facet_wrap, ale przez to, ze
#zliczenie kategorii znajduje sie w wielu roznych kolumnach mialem z tym 
#problem. Rozwiazanie jest pewnie latwe, ale niestety go nie znalazlem

#Zrobilem wiec funkcje dla rysowania wykresu w zaleznosci od kategorii 
plot_movie <- function(categ,categ_str){
    rand_color <- randomColor(1, luminosity ='random')
    myplot <- ggplot(filter(budget_in_time, categ == 1),
         aes(x = year, y = real_budget)) +
    geom_point(color=rand_color) +
    labs(x = 'Year',
         y= 'Budget',
         title=sprintf('Budget in time for %s movies', categ_str),
         caption= 'diag. [4.2]')
    return(myplot)
}


#drukuje je po kolei (wykresy uwzgledniaja w budzetach czynnik inflacji)
plot_movie(budget_in_time$Action,'action')
plot_movie(budget_in_time$Animation,'animation')
plot_movie(budget_in_time$Comedy,'comedy')
plot_movie(budget_in_time$Drama,'drama')
plot_movie(budget_in_time$Documentary,'documentary')
plot_movie(budget_in_time$Romance,'romance')
plot_movie(budget_in_time$Short,'short')

