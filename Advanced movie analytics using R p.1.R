#ustawienie sciezki roboczej w miejscu pliku
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

#import pakietu
library(ggplot2)
library(dplyr)

economics%>% as_tibble() -> data

#znalazlem funkcje w internecie pozwalajaca drukowac dwa wykresy 
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

#wykres wydatkow konsumpcyjnych w przedziale czasowym 
diagr_pce <- (ggplot(data, aes(x=date, y=pce, size=pop))+
              geom_smooth(se=FALSE, size=1.5, color="#FF7F50", linetype = "dashed")+
              geom_line(size=0.2,color="#FF7F50"))+
              labs(title= 'Private consumption expences in time',
                   x = "Date", 
                   y= "PCE [bln]",
                   caption = "diag. [1.1.1]")

#wykres liczby bezrobotnych w przedziale czasowym
diagr_unemploy <- (ggplot(data, aes(x=date, y=unemploy,size=pop))+
                    geom_smooth(se=FALSE, size=1.5,linetype = "dashed")+
                    geom_line(size=0.2,color='blue'))+
                    labs(title= 'Unemployed number in time',
                         x = "Date", 
                         y= "Unempl. [x1000]",
                         caption = 'diag. [1.1.2]')

#laczenie dwoch wykresow
multiplot(diagr_pce, diagr_unemploy)

#wykres odsetka bezrobotnych w przedziale czasowym
diagr_unemploy_rate <- (ggplot(data, aes(x=date, y=unemploy*100/pop))+
                     geom_smooth(se=FALSE, size=1.5,linetype = "dashed",color='orange')+
                     geom_line(size=0.2,color='orange'))+
                     labs(title= 'Unemployment rate in time',
                          x = "Date", 
                          y= "Unemloyment rate [%]",
                          caption= 'diag. [1.2]')

diagr_unemploy_rate

#Wydaje mi sie ze zrobilem wszystko dobrze, ale zweryfikowalem informacje w internecie
#i w USA dla 1982r. wskaznik bezrobocia wynosi ok 10%, a mi wychodzi ok. 5%.
#Czy wynika to z tego, ze ja licze cala populacje, a w internecie podaje sie zazwyczaj
#wzskaznik wsrod osob zdolnych do pracy?

#wykres babelkowy stopy oszczednosci do wydatkow na konsumpcje indywid.
#kolorem oznaczono mediane okresu bezrobocia wyrazona w miesiacach
ggplot(data, aes(x=pce, y=psavert, color=uempmed)) +
        geom_point(size=1)+
        labs(title= 'Prv. savings rate to PCE',
         x= "PCE [bln$]", 
         y= "Prv. savings rate[%]", 
         colour= "Unempl.",
         caption= 'diag. [2.1]')

# Gospodarka podega cyklom koniunkturalnym 
# Sa to wachania ktore okresla sie takimi wsaznikami jak m.in. dynamika PKB 
# wieklkoscia importu/exportu, ale tez stopa bezrobocia
# jednoczesnie dlugotrwala tendencja jest przewaznie rosnaca.
# M.in na wykresie 1.2 widaæ takie cykle 

#Bezrobocie w USA bylo dotychczas najwyzsze w oklicach 1982 z powodu wielkiego krysysu
#wywolanego iranskim kryzysem energetycznym z 1979r. Rosnace ceny ropy spowodowaly
#m.in. w USA nawet dwucyfrowa inflacje i spore wyhamowanie gospodarki.


