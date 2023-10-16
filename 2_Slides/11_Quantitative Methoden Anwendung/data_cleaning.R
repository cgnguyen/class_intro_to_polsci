####Daten Säubern####
  
  ####*Setup - ESS Packet aktivieren und Daten herunterladen####  
  library(essurvey) #Bibliothek für ESS download
  set_email("cgnguyen@gmail.com")
  
  DATA<-import_country("France", rounds=4, format = "stata")

  ####*Daten Säubern####
  DATA<-zap_missing(DATA)
  
  #Ost oder West Deutschland 
    #Namen aus dem Regions Packet-  DEG0 zu DEG für Thüringen 
    library(regions)
    name_regions<-
      google_nuts_matchtable%>%
        filter(country_code =="DE")%>%
        mutate(bundesland=as.factor(google_region_name),
               region=code_2016)%>%
        select(region,bundesland)%>%
        mutate(region=dplyr::recode(region,
                                        "DEG0"="DEG"))

    
    DATA<-DATA%>%
      left_join(name_regions, by="region")
    
    #Ost und West definition 
    ost_vec<-c("Berlin","Mecklenburg-Vorpommern","Saxony","Saxony-Anhalt","Brandenburg","Thuringia")
    west_vec<-c("Baden-Württemberg","Bavaria","Bremen","Hamburg","Hesse","Lower Saxony",
                "North Rhine-Westphalia","Rhineland-Palatinate","Saarland","Schleswig-Holstein")    
    
    DATA<-
      DATA%>%
        mutate(ost = as.factor(case_when(bundesland %in% ost_vec ~ 'Neues Bundesland',
                               bundesland %in% west_vec ~ 'Altes Bundesland')))
      

    #Parteizugehörigkeit/ Affinität Definieren für die AfD 
      #Exkludiere Beobachtungen ohne Parteiaffinität 
      DATA<-subset(DATA, !is.na(clsprty))
      
      #Inverse Skala für Parteizugehörigkeit
      DATA<-
        DATA %>%
          mutate(prtdgcl=unclass((DATA$prtdgcl)-5)*-1)
                   
      
      #Stärke der Parteizugehörigkeit für die AfD = Stärke der Parteizugehörigkeit für AfD Nahe = ansonsten 0
      DATA<-DATA %>%
        mutate(afd = case_when(prtclede==6 ~prtdgcl,
                               TRUE ~ 0))
                 
    
      
    ##Turn standard factors into labeled variables - necessary for export 

        
    ####*Daten Speichern####
 
    
    write_sav(DATA,"pstva.sav")
              