#date and time handling
#basic C
library(dplyr)
library(ggplot2)
library(lubridate)


#Code	Value
%d	Day of the month (decimal number)
%m	Month (decimal number)
%b	Month (abbreviated)
%B	Month (full name)
%y	Year (2 digit)
%Y	Year (4 digit)


#in the context of birthdates
as.Date(birthdates$Conception, format='%d-%b')
Z<-as.Date(birthdates$Conception, format='%d-%b')

#lubridate commands - we would use this to create cannonical 
now()
day(Z)
date(Z)
month(Z)
#more on this next week....

#we can then add this new data back into the original frame
birthdates<-mutate(birthdates, conception.date=as.Date(birthdates$Conception, format='%d-%b'))
birthdates<-mutate(birthdates, birth.date2=as.Date(birthdates$Birth.Date, format='%d-%b'))
View(birthdates)


#a preview for next week...

ggplot(birthdates, aes(conception.date, Births))+geom_density2d()
ggplot(birthdates, aes(birth.date2, Births))+geom_density2d()
ggplot(birthdates, aes(birth.date2, Births))+geom_rug()


ggplot(birthdates, aes(conception.date, Births))+geom_bin2d()
ggplot(birthdates, aes(conception.date, Births))+geom_hex(aes(color=birthdates$Rank))
ggplot(birthdates, aes(birth.date2, Births))+geom_hex(aes(color=birthdates$Rank))

#jitter effect series
ggplot(birthdates, aes(conception.date, Births))+geom_jitter()
ggplot(birthdates, aes(conception.date, Births))+geom_jitter(aes(size=birthdates$Rank))
ggplot(birthdates, aes(conception.date, Births))+geom_jitter(aes(color=birthdates$Rank))
ggplot(birthdates, aes(conception.date, Births))+geom_jitter(aes(alpha=birthdates$Rank))
#try this - rebuilds graph along the rank
ggplot(birthdates, aes(conception.date, Births))+geom_jitter(aes(y=birthdates$Rank))

#discrete-continuous
ggplot(birthdates, aes(conception.date, Births))+geom_violin(scale="area")


