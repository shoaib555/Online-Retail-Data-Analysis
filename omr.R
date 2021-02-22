rm(list=ls())
od=read.csv("data.csv",na.strings = c("NaN",NA,""))
dim(od)
str(od)
summary(od)
colSums(is.na(od))

library(dplyr)
od%>%filter(is.na(Description) & is.na(CustomerID))%>%head(10)
od=na.omit(od)

od%>%group_by(InvoiceNo,Description,Quantity)%>%filter(n() > 1)%>%head(10)
od%>%group_by(InvoiceNo,Description,Quantity)%>%filter(n() == 1)->od

od=as.data.frame(od)
str(od)
od$Time=format(as.POSIXct(od$InvoiceDate,format="%m/%d/%Y %H:%M"),format="%H:%M")
od$Date=format(as.Date(od$InvoiceDate,format="%m/%d/%Y %H:%M"),format="%m-%d-%Y")

library(lubridate)
od$month=month(mdy(od$Date),label = T)
od$day=day(mdy(od$Date))
od$wday=wday(mdy(od$Date),label = T)


od$Time=as.numeric(sub(":",".",od$Time))

od$day_bin[od$Time>=4.00 & od$Time < 12.00] = "Morning"
od$day_bin[od$Time>=12.00 & od$Time < 16.00] = "Afternoon"
od$day_bin[od$Time>=16.00 & od$Time < 19.00] = "Evening"
od$day_bin[od$Time>=19.00 & od$Time <= 24.00] = "Night"


od$day_bin=as.factor(od$day_bin)

table(od$day_bin)

od$Sales=od$UnitPrice*od$Quantity
summary(od)

library(ggplot2)
library(RColorBrewer)
od%>%group_by(day_bin)%>%summarize(n=sum(Sales),count=n(),p=round(count/length(od$Sales)*100,2))%>%ggplot(aes(x=reorder(day_bin,-p),y=p,fill=day_bin))+geom_bar(stat='identity',position = position_stack())+
  labs(x="Time of Day",y="Total Sales")+ggtitle("Total sales by time of Day")+guides(fill=F)+scale_fill_brewer(palette = "YlGn")+
  geom_text(aes(x=day_bin,label=paste(format(p),"%")),vjust=-0.3)



od%>%group_by(Country)%>%summarize(n=sum(Sales),count=n(),p=round(count/length(od$Sales)*100,2))%>%top_n(10,n)%>%
  ggplot(aes(x=reorder(Country,-p),y=count,fill=Country))+
  geom_bar(stat='identity',position = position_stack())+
  labs(x="Country",y="Total Sales")+ggtitle("Top 10 countries by Sales")+
  guides(fill=F)+theme(axis.text.x = element_text(angle = 45, hjust = 1))+
  geom_text(aes(x=Country,label=paste(format(p),"%")),vjust=-0.3)
  



od%>%group_by(Description)%>%summarize(n=sum(Sales),count=n(),p=round(count/length(od$Sales)*100,3))%>%top_n(20,n)%>%ggplot(aes(x=reorder(Description,-p),y=count,fill=Description))+geom_bar(stat='identity',position = position_stack())+
  labs(x="Items",y="Total Sales")+ggtitle("Top 20 Items by Sales")+guides(fill=F)+theme(axis.text.x = element_text(angle = 90, hjust = 1))+
  geom_text(aes(x=Description,label=paste(format(p),"%")),vjust=-0.3)

od%>%group_by(day)%>%summarize(n=sum(Sales))%>%ggplot(aes(x=reorder(day,-n),y=n,fill=day))+geom_bar(stat='identity',position = position_stack())+
  labs(x="Day",y="Total Sales")+ggtitle("Total sales by day")+guides(fill=F)+theme(axis.text.x = element_text(angle = 45, hjust = 1))

od%>%group_by(day)%>%summarise(Total = sum(Sales))%>%ggplot(aes(day,Total,col=Total))+geom_line(aes(group=1))+geom_point(size=2)+scale_x_continuous(breaks = c(1:31))+ylab("Total Sales")+ggtitle("Total sales by day")

od%>%group_by(wday)%>%summarize(n=sum(Sales),count=n(),p=round(count/length(od$Sales)*100,2))%>%ggplot(aes(x=reorder(wday,-count),y=count,fill=wday))+geom_bar(stat='identity',position = position_stack())+
  labs(x="Day of the week",y="Total Sales")+ggtitle("Total sales by day of a week")+guides(fill=F)+
  theme(axis.text.x = element_text(angle = 0, hjust = 1))+
  geom_text(aes(x=wday,label=paste(format(p),"%")),vjust=-0.3)
  


od%>%group_by(month)%>%summarize(n=sum(Sales),count=n(),p=round(count/length(od$Sales)*100,2))%>%ggplot(aes(x=reorder(month,-count),y=count,fill=month))+geom_bar(stat='identity',position = position_stack())+
  labs(x="Month",y="Total Sales")+ggtitle("Total sales by Month")+guides(fill=F)+geom_text(aes(x=month,label=paste(format(p),"%")),vjust=-0.3)


uk=od%>%filter(Country=="United Kingdom")


uk%>%group_by(Description)%>%summarize(n=sum(Sales),count=n(),p=round(count/length(od$Sales)*100,3))%>%top_n(20,n)%>%ggplot(aes(x=reorder(Description,-p),y=count,fill=Description))+geom_bar(stat='identity',position = position_stack())+
  labs(x="Items",y="Total Sales")+ggtitle("Top 20 Items by Sales in UK")+guides(fill=F)+theme(axis.text.x = element_text(angle = 90, hjust = 1))+
  geom_text(aes(x=Description,label=paste(format(p),"%")),vjust=-0.3)



od=od[-grep("C", od$InvoiceNo), ]
od%>%filter(Country=="United Kingdom")->uk

library(arules)
library(arulesViz) 
library(RColorBrewer)
uk$InvoiceNo=as.factor(uk$InvoiceNo)
str(uk)
uk.agg=split(uk$Description,uk$InvoiceNo)
uk.agg2=list()
for( i in 1:length(uk.agg)){
  uk.agg2[[i]]=unique(uk.agg[[i]])
}

head(uk.agg2,10)
txn=as(uk.agg2,"transactions")
summary(txn)

itemFrequencyPlot(txn,topN=20,main="Top 20 items by order absolute frequency",col=brewer.pal(8,'Pastel2'),type="absolute")
itemFrequencyPlot(txn,topN=20,main="Top 20 items by order relative frequency",col=brewer.pal(8,'Pastel2'),type="relative")

arules1=apriori(data=txn,parameter = list(support=0.001,confidence=0.8,maxlen=3))
summary(arules1)
inspect(arules1[1:10])


subrule=head(sort(arules1,by="lift"),20)
subrule=subrule[quality(subrule)$confidence>0.4]
subrule=sort(subrule,by="support")
print(subrule)
inspect(subrule)
plot(arules1,control = list(col="RED",jitter=0))
plot(arules1,control = list(col=brewer.pal(11,"Spectral")))

plot(subrule,method = "graph",engine = "htmlwidget")
rules=as(subrule,"data.frame")
print(rules)
rules$LHSP=rules$support/rules$confidence
rules$RHSP=rules$confidence/rules$lift
rules%>%arrange(desc(lift))->ruless

arules2=apriori(data=txn,parameter = list(support=0.002,confidence=0.8,maxlen=3))
subrule1=head(sort(arules2,by="lift"),20)
subrule1=subrule1[quality(subrule1)$confidence>0.4]
subrule1=sort(subrule1,by="support")
inspect(subrule1)
plot(subrule1,method = "graph",engine = "htmlwidget")
rules1=as(subrule1,"data.frame")

rules1$LHSP=rules1$support/rules1$confidence
rules1$RHSP=rules1$confidence/rules1$lift
rules1%>%arrange(desc(lift))->rules2


####RFM

uk$Date=as.Date(uk$Date, format="%m-%d-%Y")

uk%>%group_by(CustomerID)%>%mutate(Recency=as.numeric(as.Date("2012-01-01")-max(Date)),Frequency=n_distinct(InvoiceNo),Monetary=sum(Sales)/n_distinct(InvoiceNo))%>%filter(Monetary>0)->UKRFM

summary(UKRFM[,c(16,17,18)])
UKRFM=as.data.frame(UKRFM)
UKRFM$Recency=as.numeric(UKRFM$Recency)

UKRFM$R_Score[UKRFM$Recency>=23 & UKRFM$Recency <=27] = "4"
UKRFM$R_Score[UKRFM$Recency>=28 & UKRFM$Recency <=39] = "3"
UKRFM$R_Score[UKRFM$Recency>=40 & UKRFM$Recency <=67] = "2"
UKRFM$R_Score[UKRFM$Recency>=68 & UKRFM$Recency <=396] = "1"





str(UKRFM)
table(UKRFM$R_Score)


UKRFM$F_Score[UKRFM$Frequency>=1 & UKRFM$Frequency <=3] = "1"
UKRFM$F_Score[UKRFM$Frequency>=4 & UKRFM$Frequency <=7] = "2"
UKRFM$F_Score[UKRFM$Frequency>=8 & UKRFM$Frequency <=15] = "3"
UKRFM$F_Score[UKRFM$Frequency>=16 & UKRFM$Frequency <=210] = "4"



table(UKRFM$F_Score)




UKRFM$M_Score[UKRFM$Monetary>=3.45 & UKRFM$Monetary <=223.80] = "1"
UKRFM$M_Score[UKRFM$Monetary>=223.81 & UKRFM$Monetary <=340.75] = "2"
UKRFM$M_Score[UKRFM$Monetary>=340.71 & UKRFM$Monetary <=497.75] = "3"
UKRFM$M_Score[UKRFM$Monetary>=497.76 & UKRFM$Monetary <=84236.25] = "4"

table(UKRFM$M_Score)

UKRFM$R_Score=as.numeric(UKRFM$R_Score)
UKRFM$F_Score=as.numeric(UKRFM$F_Score)
UKRFM$M_Score=as.numeric(UKRFM$M_Score)

UKRFM%>%mutate(RFM_Score=R_Score+F_Score+M_Score)->UKRFM
table(UKRFM$RFM_Score)

UKRFM%>%mutate(RFM_Score = as.numeric(paste0(R_Score,F_Score, M_Score)))->UKRFM

UKRFM%>%filter(RFM_Score==444)%>%distinct(CustomerID,.keep_all = T)->Best_Customers
UKRFM%>%filter(F_Score==4)%>%distinct(CustomerID,.keep_all = T)->Loyal_customers
UKRFM%>%filter(M_Score==4)%>%distinct(CustomerID,.keep_all = T)->Big_Spenders
UKRFM%>%filter(RFM_Score==244)%>%distinct(CustomerID,.keep_all = T)->Almost_Lost_Customers
UKRFM%>%filter(RFM_Score==111)%>%distinct(CustomerID,.keep_all =T )->One_off_Customers

q()


