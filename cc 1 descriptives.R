# Pull data from mysql server to do some basic
# descriptive statistics on cases vs controls
# dataset is studya
library(RODBC)
odbcch <- odbcConnect("rindex2012")
# sqlTables(odbcch)
sqls <- "SELECT s.ccstudynum,
       s.studygroup,
       s.studynum,
       #e.DOB,
       e.AdmitDT,
       dayofweek(e.AdmitDT) as AdmitDOW,
       dayofweek(e.DCDT) as DCDOW,
       dayofweek(s.edt) as eDOW,
       # s.edt,
       floor(hour(s.edt)/4)*4 as HourGroup,
       #e.DCDT,
       e.AdmitAge,
       e.LOS,
       e.gender,
       e.StudyMonth,
       cr.grouping AS race,
       cd.grouping AS dispo,
       cc1_codes.grouping AS service
  FROM    (   (   (   cc1_encs e
                   INNER JOIN
                      cc1_codes cd
                   ON (e.dispocode = cd.codenum))
               INNER JOIN
                  cc1_studya s
               ON (s.studynum = e.studynum))
           INNER JOIN
              cc1_codes cr
           ON (e.racecode = cr.codenum))
       INNER JOIN
          cc1_codes cc1_codes
       ON (e.servicecode = cc1_codes.codenum)
ORDER BY s.ccstudynum ASC, s.studygroup ASC;"
studya <- sqlQuery(odbcch, sqls)
odbcClose(odbcch)
dow=c("Sun", "Mon","Tue","Wed","Thr","Fri","Sat")
studya$AdmitDOWf <- factor(studya$AdmitDOW, labels=dow)
studya$DCDOWf <- factor(studya$DCDOW, labels=dow)
studya$eDOWf <- factor(studya$eDOW, labels=dow)

studya$IsCase <- studya$studygroup == "Case"

trim <- quantile(studya$LOS, c(0.01, 0.99))
studya$LOSt <- studya$LOS
studya$LOSt[(studya$LOSt<trim[1])|(studya$LOSt>trim[2])] <- NA

#Descriptive statistics by group
library(psych)
library(ggplot2)
library(plyr)
library(epitools)
library(survival)  # For conditional logistic regression
library(epicalc)   # for idr.display

str(studya)
summary(studya)
describe.by(studya, group=studya$studygroup)

# LOS
x=studya$LOS
ggplot(studya, aes(x=LOS, fill=studygroup))+
  geom_density(alpha=0.5)+xlim(0,60)

ggplot(studya, aes(x=LOS, fill=studygroup))+
  geom_density()+
  facet_grid(studygroup~.)+
  xlim(0,60)

t.test(x~studya$studygroup)
wilcox.test(studya$LOS~studya$studygroup)
summary(x)
#m1<-glm(IsCase~x, family="poisson", data=studya);summary(m1)
#idr.display(m1,decimal=4)
m1<-clogit(IsCase~x+strata(ccstudynum), data=studya, method="exact")
summary(m1);exp(coef(m1));exp(confint(m1))


# Admit Age - not significant
ggplot(studya, aes(x=AdmitAge, fill=studygroup))+
  geom_density(alpha=0.5)+xlim(0,300)
wilcox.test(studya$AdmitAge~studya$studygroup)

# Admit DOW - not significant
x <- studya$AdmitDOWf
epitab(xtabs(~x+studya$studygroup), correction=T, rev="c")

# Event DOW - not significant
x <- studya$eDOWf
mytable <- table(x, studya$studygroup)
mytable
prop.table(mytable,2) # Column percentages
summary(mytable)
epitab(xtabs(~x+studya$studygroup), correction=T, rev="c")

# DC DOW - more control dc on Monday, maybe
x <- studya$DCDOWf
mytable <- table(x, studya$studygroup)
mytable
prop.table(mytable,2) # Column percentages
summary(mytable)
epitab(xtabs(~x+studya$studygroup), correction=T, rev="c")
m1<-clogit(IsCase~x+strata(ccstudynum), data=studya, method="exact")
summary(m1);exp(coef(m1));exp(confint(m1))
mydf <- count(studya, vars=c('studygroup','DCDOWf'))
mydf <- ddply(mydf, .(studygroup), transform, p = freq/sum(freq))
ggplot(studya, aes(DCDOWf, fill=studygroup))+
  geom_bar(position="dodge")
ggplot(mydf, aes(DCDOWf, p, fill=studygroup))+
  geom_bar(stat="identity", position="dodge")
ggplot(mydf, aes(DCDOWf, p))+
  geom_bar(stat="identity")+facet_grid(~studygroup)


# dispo - 
x <- studya$dispo
mytable <- table(x, studya$studygroup)
mytable
prop.table(mytable,2) # Column percentages
summary(mytable)
epitab(xtabs(~x+studya$studygroup), correction=T, rev="c")
m1<-clogit(IsCase~x+strata(ccstudynum), data=studya, method="exact")
summary(m1);exp(coef(m1));exp(confint(m1))
mydf <- count(studya, vars=c('studygroup','dispo'))
mydf <- ddply(mydf, .(studygroup), transform, p = freq/sum(freq))
ggplot(studya, aes(dispo, fill=studygroup))+
  geom_bar(position="dodge")
ggplot(mydf, aes(dispo, p, fill=studygroup))+
  geom_bar(stat="identity", position="dodge")
ggplot(mydf, aes(dispo, p))+
  geom_bar(stat="identity")+facet_grid(~studygroup)

# gender - NS
x <- studya$gender
mytable <- table(studya$gender, studya$studygroup)
mytable
prop.table(mytable,2) # Column percentages
summary(mytable)
m1<-clogit(IsCase~x+strata(ccstudynum), data=studya, method="exact")
summary(m1);exp(coef(m1));exp(confint(m1))

# race - NS
mytable <- table(studya$race, studya$studygroup)
mytable
prop.table(mytable,2) # Column percentages
summary(mytable)

# HourGroup - cases higher 12,16, and 20
x <- studya$HourGroup
mytable <- table(x, studya$studygroup)
mytable
prop.table(mytable,2) # Column percentages
summary(mytable)
epitab(xtabs(~x+studya$studygroup), correction=T, rev="c")
m1<-clogit(IsCase~x+strata(ccstudynum), data=studya, method="exact")
summary(m1);exp(coef(m1));exp(confint(m1))
mydf <- count(studya, vars=c('studygroup','dispo'))
mydf <- ddply(mydf, .(studygroup), transform, p = freq/sum(freq))
ggplot(studya, aes(dispo, fill=studygroup))+
  geom_bar(position="dodge")
ggplot(mydf, aes(dispo, p, fill=studygroup))+
  geom_bar(stat="identity", position="dodge")
ggplot(mydf, aes(dispo, p))+
  geom_bar(stat="identity")+facet_grid(~studygroup)
