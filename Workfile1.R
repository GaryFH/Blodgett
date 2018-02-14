##Blodgett work file

library(dplyr)
library(ggplot2)


dfBonde1<-tbl_df(read.csv("Diarrhrea Trend Bonde Dispensary 2012-2015 (outside study area).csv"))
dfKatito1<-tbl_df(read.csv("Diarrhrea Trend Katito Health Center 2012-2015 (-2012) .csv" ))
dfKibogo1<-tbl_df(read.csv("Diarrhrea Trend Kibogo Dispensary 2012-2015.csv" ))
dfPapOnditi1<-tbl_df(read.csv("Diarrhrea Trend Pap Onditi Hospital Manthly Ave 2009-2016.csv" ))
dfRae1<-tbl_df(read.csv("Diarrhrea Trend Rae Dispensary 2012-2015 (outside study area) .csv" ))
dfDissertation1<-tbl_df(read.csv("Dissertation Full Data from the Questionaire 9-26-17_Katy.csv"))




gplot<-ggplot(data=df66,aes(x=x,y=y))+ #theme_minimal()+
  geom_point(aes(color=ColorChoice),size=1,alpha=.99)+
  #geom_point(aes(size=FourthVariable),alpha=.4,color="yellow")+
  #geom_smooth(data=df66,aes(x=x,y=y, color=ColorChoice), method = lm,se=F)+
  labs(title=paste("data:",z666,z7[1],z7[2],z7[3],z7[4],z7[5],z7[6],z7[7]))+
  #labs(x=paste(z3,z7[1],z7[2],z7[3],z7[4],z7[5],z7[6],z7[7]),y=z2)
  labs(x=z3,y=z2)

#8/12/2017 home survey/testing


#THINK WATER IS CLEAN
## "Q5f..do.you.think.water.is.safe.to.drink.." 

#SELF REPORT
# "Q4..do.you.treat.your.water.."
#"Q5i..method.of.treatment." 
#"Q5b..how.often.do.you.use.this.method."  

#WATER VESSEL
# "Q5d..type.of.vessel."

#EVIDENCE FROM TREATED WATER COLLECTED FROM HOME VESSEL
#"HvColiresult..Home.vessel.drinking.water.test.result."  
#"Hvchloresidtest"
#"Hvpetrifrest" 

#EVIDENCE FROM  RAW WATER
#"Wscoliresult..raw.water.source.test.result."
 "Wspetriftest"
 
 aab<-select(dfDiss,HvColiresult..Home.vessel.drinking.water.test.result., Wscoliresult..raw.water.source.test.result.) 
 aac<-select(dfDiss,Hvpetrifrest, Wspetriftest,Hvchloresidtest)
 
# bbb<-filter(dfDiss,Q5a..how.long.have.you.been.using.the.method.=="Add bleach/chlorine")
 
 aad<-select(dfDiss,Q5f..do.you.think.water.is.safe.to.drink..,Hvpetrifrest, Wspetriftest,Hvchloresidtest,Q5a..how.long.have.you.been.using.the.method.)
 
 ##No chlorine was available before 2012
 
 aad2<-filter(aad,(Q5a..how.long.have.you.been.using.the.method.<=60))
 
 
 
 #what type of vessel 
 unique(dfDiss$Q5d..type.of.vessel.)
 ##all spigot owners have solar cookers
 bbc2<-filter(dfDiss,Q5d..type.of.vessel.=="Ceramic narrow mouth with spigot")
 aad2<-select(bbc2,Q5f..do.you.think.water.is.safe.to.drink..,Hvpetrifrest, Wspetriftest,Hvchloresidtest,Q5a..how.long.have.you.been.using.the.method.)
 

 bbc3<-filter(dfDiss,Q5i..method.of.treatment.=="Solar pasteurisation")
 
 
 df %>%
   mutate(g = ifelse(a == 2 | a == 5 | a == 7 | (a == 1 & b == 4), 2,
                     ifelse(a == 0 | a == 1 | a == 4 | a == 3 |  c == 4, 3, NA)))
 
 
aaba<-c(sum(df9$BeforeTrainingWaterQuality),sum(df9$Home_SafeColiNumber))
barplot(aaba)
 
 
 