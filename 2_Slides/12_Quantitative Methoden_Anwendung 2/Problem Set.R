  summary(DATA$afd)
  
  mean(DATA$afd, na.rm=T)
  median(DATA$afd, na.rm=T)


####Zusammenhang Alter und AfD Zugehörigkeit####
  DATA %>%
    ggplot(aes(x=agea, y=afd))+
    geom_point(alpha=0.3, position= position_jitter())+
    theme_bw()+
    xlab("Alter")+ylab("AfD Zugehörigkeit")+
    geom_smooth(method="lm")

  


  ####*Streudiagramm: Links-Rechts Ausprägung und Zustimmung im Durschnitt####
  #Durschnitt Zustimmung und Links-Rechts Ausprägung - Streudiagramm
    #Durschnitt berechnen und "missing values" entfernen
     temp_data<- 
        DATA %>% 
          group_by(lrscale)%>%
          summarize(afd=mean(afd, na.rm=T))
    
  
    temp_data
      
    #Die Daten in eine Graphische Abbildung packen
    temp_data %>%
      ggplot(aes(x=lrscale, y=afd))+
      geom_col()+ 
      xlab("Links-Rechts Ausprägung")+
      ylab("AfD Zugehörigkeit")+
      theme_bw()
  

  ####*Balkendiagramme####
  #Durschnitt nach Hauptaktivität 
    DATA %>% 
      group_by(mnactic_simple)%>%
        summarize(afd=mean(afd, na.rm=T)) %>%
        filter(!is.na(mnactic_simple)) %>%
        ggplot(aes(x= mnactic_simple, y=afd))+
        geom_col()+ 
        xlab("Hauptaktivität")+
        ylab("AfD Zugehörigkeit")+
        theme_bw()
    
    
    DATA$ost<-as_factor(DATA$ost)
    
    
    #Ost/West
    DATA %>% 
      group_by(ost)%>%
      summarize(afd=mean(afd, na.rm=T)) %>%
      filter(!is.na(afd)) %>%
      ggplot(aes(x= ost, y=afd))+
      geom_col()+ 
      xlab("Region")+
      ylab("AfD Zugehörigkeit")+
      theme_bw()
    
    DATA$bundesland<-as_factor(DATA$bundesland)
    
 
    

    
    #Bundesland 2
    DATA %>% 
      group_by(bundesland)%>%
      summarize(afd=mean(afd, na.rm=T)) %>%
      filter(!is.na(afd)) %>%
      ggplot(aes(x= reorder(bundesland, -afd), y=afd))+
      geom_col()+ 
      xlab("Bundesland")+
      ylab("AfD Zugehörigkeit")+
      theme_bw()+
      theme(axis.text.x = element_text(angle = 90))
    
    
####BiVariate Regression####
    DATA$ost<-as_factor(DATA$ost)
    DATA$gndr<-as.factor(DATA$gndr)
    
    mod_1<-lm(afd~lrscale, data=DATA)
    summary(mod_1)
    
    mod_2<-lm(afd~agea, data=DATA)
    summary(mod_2)
    
    mod_3<-lm(afd~ost, data=DATA)
    summary(mod_3)
    
    
    mod_3<-lm(afd~bundesland, data=DATA)
    summary(mod_3)
    
    mod_4<-lm(afd~gender, data=DATA)
    summary(mod_4)


####Multivariate Regression####
    mod_5<-lm(afd~lrscale+agea+ost+eduyrs+mnactic_simple+imwbcnt+trstprl, 
              data=DATA)
    
    summary(mod_5)  
    
    

####Eigene Ideen###### 
    #Arbeitsplatzverlust / Langzeitarbeitslosigkeit 
    DATA$uemp3m
    DATA$langzeit<-as_factor(DATA$uemp3m)
    
    summary(  DATA$langzeit)
    
    mod_6<-lm(afd~lrscale+agea+ost+eduyrs+mnactic_simple+imwbcnt+trstprl+
                langzeit, data=DATA)
    
    summary(mod_6)  
    
    
    #Afd Neigung der Eltern 
    
    
    #Feministische Einstellungen / LGBTQ
    
    
    
    #Religionszugehörigkeit 
    
    
    #Partizipation 
    
    
    
    #Direktdemokratie 
    
    
    
    #veganismus 
    
    
    
    
    #Nähe zur Grenze
    
    
    
    
    #Einstellung zum Euro 
    
    
    
    #Migrationshintergrund
    DATA<-
      DATA %>% 
      mutate(
        mig = as.factor(case_when(
          brncntr == 2 ~ "Yes",
          facntr == 2 | mocntr == 2 ~ "Yes",
          is.na(brncntr)  ~ NA_character_,
          TRUE ~ "No")))
    
   summary(DATA$mig)
   
   
    mod_7<-lm(afd~lrscale+agea+ost+eduyrs+mnactic_simple+imwbcnt+trstprl+
                mig, data=DATA)
    
    summary(mod_7)  
    
    
    #Gruppen 
    DATA_mig<-DATA%>%filter(mig=="Yes")
    DATA_nomig<-DATA%>%filter(mig=="No")
    
    mod__mig<-lm(afd~lrscale+agea+ost+eduyrs+mnactic_simple+imwbcnt+trstprl, 
              data=DATA_mig)
    
    mod__nomig<-lm(afd~lrscale+agea+ost+eduyrs+mnactic_simple+imwbcnt+trstprl, 
              data=DATA_nomig)
    
    summary(mod__mig)  
    
    summary(mod__nomig)
    
    screenreg(list(mod__mig,mod__nomig))
    