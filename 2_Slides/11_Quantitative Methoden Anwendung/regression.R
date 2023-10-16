
####BiVariate Regression####
mod_1<-lm(imwbcnt~lrscale, data=DATA)
summary(mod_1)

mod_2<-lm(imwbcnt~agea, data=DATA)
summary(mod_2)

mod_3<-lm(imwbcnt~ost, data=DATA)
summary(mod_3)

mod_4<-lm(imwbcnt~bundesland, data=DATA)
summary(mod_4)

mod_5<-lm(imwbcnt~mnactic_simple, data=DATA)
summary(mod_5)


####Multivariate Regression####
mod_5<-lm(imwbcnt~lrscale+agea+gender+ost+eduyrs+mnactic_simple, 
          data=DATA)

summary(mod_5)

####Exportieren 1####
stargazer(mod_5, type="html", out="ergebniss.html") 


stargazer(mod_5, type="html", out="ergebniss1.html",
          covariate.labels= c("Links-Rechts","Alter","Weiblich","Neues Bundesland",
                              "Bildungsjahre","Im Studium/Ausbildung","Andere Tätigkeit",
                              "In Rente","Arbeitslos",NA))









mod_5<-lm(afd~lrscale+agea+gender+ost+eduyrs+mnactic_simple+trstplt,
          data=DATA)


summary(mod_5)





#Soziale Zugeh?rigkeit - Klasse ---------------------------------

summary(d$class8)

d$class<-as_factor(d$class8)

mod_1<-lm(afd~class, data=d)

summary(mod_1)

mod_2<-lm(afd~class+
            agea+gender+ost+eduyrs+mnactic_simple, data=d)

summary(mod_2)

mod_3<-lm(afd~class+
            agea+gender+ost+eduyrs+mnactic_simple+
            lrscale+stfdem, data=d)

summary(mod_3)



#

DATA %>%
  ggplot(aes(x=netusoft, y=afd))+
  geom_point(position= position_jitter(),alpha=0.2)+ 
  xlab("Internet Nutzung")+
  ylab("AFD-Verbundenheit")+
  geom_smooth(method="lm")+
  ggtitle("Balken zu Internetnutzung und Afd-Verbundenheit")+
  theme_bw()


DATA %>%
  group_by(netusoft,ost)%>%
  summarize(afd=mean(afd,na.rm=T))%>%
  ggplot(aes(x=netusoft, y=afd,fill=ost))+
  geom_col(position=position_dodge())+ 
  xlab("Internet Nutzung")+
  ylab("AFD-Verbundenheit")+
  
  ggtitle("Balken zu Internetnutzung und Afd-Verbundenheit")+
  theme_bw()



