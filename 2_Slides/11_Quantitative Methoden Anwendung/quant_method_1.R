####Setup####
  #Packete Installieren - nur einmal 
  install.packages("tidyverse")  #Tidyverse erlaubt uns die pipes zu Nutzen, und ggplot2 für Abbildungen
  install.packages("car")    #Daten Säubern - Achtung: Konflikt mit recode im dplyr Packet
  install.packages('haven') #Stata Daten Lesen und nutzen
  install.packages('stargazer') #Tabellen Herstellen 
  
  #Packete Aktivieren
  library(tidyverse)
  library(car)
  library(haven)
  library(stargazer)
  library(labelled)
  
  ##Arbeitsverzeichniss Definieren - nicht notwendig wenn es ein R-Studio Projekt ist

  
  
####Daten Einlesen####
    
  #Daten von Github herunterladen - später selber finden
  #Quelle: https://www.europeansocialsurvey.org/data/download.html?r=9 Deutschland 
  
  
  DATA<-read_sav("https://github.com/cgnguyen/R_intro/blob/master/pstva.sav?raw=true")
  
  #Daten Anschauen
  view(DATA)
  
  summary(DATA$netustm)
  

  
  
####Deskriptive Variablen####

  ####*Metrisch####
  #Alle Werte
  DATA$imwbcnt  
  
  #Zusammenfassung
  summary(DATA$imwbcnt)
  
  summary(DATA$agea)
  
  ####*Faktor 1####
  DATA$mnactic
  
  summary(DATA$mnactic) #Das funktioniert noch nicht - gleich mehr
  
  
  

  
####Daten Säubern####
    #data_cleaning.R mit mehr Beispielen
  
  #Hauptaktivität in Faktor Umwandeln 
  DATA$mnactic_clean<-as_factor(DATA$mnactic) # as_factor nutzt die Labels in Stata/SPSS für   Faktor Variablen in R
  
  
  summary(DATA$mnactic_clean) 
  DATA$mnactic_clean<-droplevels(DATA$mnactic_clean)
  summary(DATA$mnactic_clean) 
  
  #Geschlecht in Faktor umwandeln
  DATA$gender<-as_factor(DATA$gndr)

  #Hauptaktivität - Recoden und Vereinfachen
  DATA$mnactic_simple<-car::recode(as.numeric(DATA$mnactic),"
                                  1='Paid Work';
                                  2='Education'; 
                                  3='Unemployed';
                                  4='Unemployed';
                                  5='Other';
                                  6='Retired';
                                  7='Other';
                                  8='Other'; 
                                  9='Other';
                                  66=NA;
                                  77=NA;
                                  88=NA;
                                  99=NA;")
  DATA$mnactic_simple<-as.factor(DATA$mnactic_simple) 
  DATA$mnactic_simple<-relevel(DATA$mnactic_simple, ref="Paid Work")
  
  summary(DATA$mnactic_simple)
  
####Abbildungen####
  
  ####*Streudiagramm  - Zusammenhang Alter und Einschätzung von Migrant*innen####
  #Version 1
  DATA %>%
    ggplot(aes(x = agea, y = imwbcnt))+
    geom_point()
  
  #Version 2
  DATA %>%
    ggplot(aes(x=agea, y=imwbcnt))+
    geom_point(alpha=0.3,position= position_jitter())+
    theme_bw()+
    xlab("Alter")+ylab("Migrant*innen machen das Leben im Land schlechter / besser")+
    ggtitle("Streudiagramm Alter und Einschätzung  von Migrant*Innen")
  
  #Version 3 - Regressionslinie
  DATA %>%
    ggplot(aes(x=agea, y=imwbcnt))+
    geom_point(alpha=0.3,position= position_jitter())+
    theme_bw()+
    xlab("Alter")+ylab("Migrant*innen machen das Leben im Land schlechter / besser")+
    geom_smooth(method="lm")

  #Abbildung speichern
  fig_1<-
    DATA %>%
      ggplot(aes(x=agea, y=imwbcnt))+
      geom_point(alpha=0.3,position= position_jitter())+
      theme_bw()+
      xlab("Alter")+ylab("Migrant*innen machen das Leben im Land schlechter / besser")+
      geom_smooth(method="lm")
  
  #Abbildung auf dem Computer speichern
  ggsave(fig_1, file="abbildung1.png")
  

  ####*Streudiagramm: Links-Rechts Ausprägung und Zustimmung im Durschnitt####
    #Durschnitt berechnen und "missing values" entfernen
     temp_data<- 
        DATA %>% 
          group_by(lrscale)%>%
          summarize(imwbcnt=mean(imwbcnt, na.rm=T)) %>%
          filter(!is.na(lrscale))
  
    temp_data
      
    #Die Daten in eine Graphische Abbildung packen
    temp_data %>%
      ggplot(aes(x=lrscale, y=imwbcnt))+
      geom_point()+ 
      xlab("Links-Rechts Ausprägung")+
      ylab("Migrant*innen machen das Leben im Land schlechter / besser")+
      theme_bw()
  

  ####*Balkendiagramme####
  #Durschnitt nach Hauptaktivität 
    DATA %>% 
      group_by(mnactic_simple)%>%
        summarize(imwbcnt=mean(imwbcnt, na.rm=T)) %>%
        filter(!is.na(mnactic_simple))%>%
        ggplot(aes(x= mnactic_simple, y=imwbcnt))+
        geom_bar(stat="identity")+ 
        xlab("Hauptaktivität")+
        ylab("Migrant*innen machen das Leben im Land schlechter / besser")+
        theme_bw()
    
    
    #Ost/West
    DATA %>% 
      group_by(ost)%>%
      summarize(imwbcnt=mean(imwbcnt, na.rm=T)) %>%
      filter(!is.na(imwbcnt)) %>%
      ggplot(aes(x= ost, y=imwbcnt))+
      geom_col()+ 
      xlab("Region")+
      ylab("Migrant*innen machen das Leben im Land schlechter / besser")+
      theme_bw()
    
    #Bundesland 2
    DATA$bundesland<-as_factor(DATA$bundesland)
    DATA %>% 
      group_by(bundesland)%>%
      summarize(imwbcnt=mean(imwbcnt, na.rm=T)) %>%
      filter(!is.na(imwbcnt)) %>%
      ggplot(aes(x= reorder(bundesland, -imwbcnt), y=imwbcnt))+
      geom_col()+ 
      xlab("Bundesland")+
      ylab("Migrant*innen machen das Leben im Land schlechter / besser")+
      theme_bw()+
      theme(axis.text.x = element_text(angle = 90))
    
    
 
    
    
    