# Levin 2013-01-21, case control variables and Rothman Index
# Pull data from mysql server to do some basic
# statistics on cases vs controls
# dataset is studya
library(RODBC)
odbcch <- odbcConnect("rindex2012")
sqls <- "SELECT * from cc1_vars1"
vars1 <- sqlQuery(odbcch, sqls)
odbcClose(odbcch)

library(plyr)
library(reshape2)
library(Hmisc)
library(ggplot2)
library(epicalc)
attach(vars1)

x <- LastMin
h <- hist(x)

#trim <- quantile(x, c(0.025, 0.975))
#trim
#binwidth <- (trim[2]-trim[1])/5
#binwidth
#vars1$xb <- 
#  trunc(pmin(pmax(x,trim[1]),trim[2])/binwidth)*binwidth

vars1$xb <- ave(x, cut2(x, g=20), FUN=median)
vars1$xb2 <- cut2(x, g=20, levels.mean=TRUE)
hist(vars1$xb)
h <- ddply(vars1, c("xb"), summarise, 
      cases=sum(studygroup=="Case")/length(studygroup),
      count=length(studygroup)
           )
qplot(xb, cases, data=h, geom="line")

# Resshape vars1
str(vars1)
vars2 <- dcast(vars1, ccstudynum+studygroup+studynum ~ MappedAs, value.var="Present")
str(vars2)
attach(vars2)

# Feedtube OR=1.278 (p=0.005)
x <- FeedTube
mytable <- table(x, studygroup)
mytable
prop.table(mytable,2) # Column percentages
summary(mytable)
m1<-clogit((studygroup=='Case')~x+strata(ccstudynum), data=vars2, method="exact")
summary(m1);exp(coef(m1));exp(confint(m1))

# Recently received vasoactive drug OR=1.46, p=0.0027
x <- Vaso
mytable <- table(x, studygroup)
mytable
prop.table(mytable,2) # Column percentages
summary(mytable)
m1<-clogit((studygroup=='Case')~x+strata(ccstudynum), data=vars2, method="exact")
summary(m1);exp(coef(m1));exp(confint(m1))

# Recently had something cultured OR=1.609 p<0.0001
x <- vars2$Cult
mytable <- table(x, studygroup);mytable
prop.table(mytable,2) # Column percentages
summary(mytable)
m1<-clogit((studygroup=='Case')~x+strata(ccstudynum), data=vars2, method="exact")
summary(m1);exp(coef(m1));exp(confint(m1))

# Recently had central line charted, NS
x <- vars2$Line
mytable <- table(x, studygroup);mytable
prop.table(mytable,2) # Column percentages
summary(mytable)
m1<-clogit((studygroup=='Case')~x+strata(ccstudynum), data=vars2, method="exact")
summary(m1);exp(coef(m1));exp(confint(m1))

# Create multivariable model
m1<-clogit((studygroup=='Case')~Vaso+Cult+FeedTube+TPN+
  strata(ccstudynum), data=vars2, method="exact")
summary(m1);exp(coef(m1));exp(confint(m1))

# Use epicalc to show ROC curve
m1<-glm((studygroup=='Case')~Vaso+Cult+FeedTube+TPN, data=vars2, family=binomial)
#m1<-glm((studygroup=='Case')~Cult, data=vars2, family=binomial)
logistic.display(m1)
lroc(m1, title=TRUE, auc.coords=c(.5,.1), cex.main=1, 
     cex.lab=1.5, col.lab="blue", cex.axis=1.3, lwd=3)

