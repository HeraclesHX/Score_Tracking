# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #
# ProjectName:  Xi'an Jassen PET
# Purpose:      Format the data for the plot described in PPT
# programmer:   Xin Huang
# Date:         06-20-2017
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #
library(zoo)
library(plyr) 
library(openxlsx)
library(dplyr)
library(ggplot2)
library(lubridate)
library(gtable)
library(grid)
library(tidyr)
##-- formating data
##-- update data
to_use_data<-unique(eda_dat[,c(1,5,7,8,9,10,11,13)])
to_use_data<-data.frame('date'=as.yearmon(to_use_data$most.recent.modify.date),to_use_data)
to_use_data$hcp.major<-as.numeric(to_use_data$hcp.major)
data1<-to_use_data %>% group_by(most.recent.modify.date,doctorid)%>%
  dplyr::summarise(hcp.major=max(hcp.major))
data1.tmp<-merge(data1,to_use_data)
data2<-data1.tmp %>% group_by(date,doctorid)%>%
  dplyr::summarise(most.recent.modify.date=max(most.recent.modify.date))
newdata<-merge(data1.tmp,data2)




## part1
## p10
p10_tmp1<-eda_dat[,c(1,2,3,5,7,8,9,13)] ##8,9,13
p10_tmp2<-data.frame('year'=year(p10_tmp1$most.recent.modify.date),
                     'month'=month(p10_tmp1$most.recent.modify.date),
                     'date'=as.yearmon(p10_tmp1$most.recent.modify.date),
                     'doctor.id'=p10_tmp1$doctorid)
tbl_df(p10_tmp2)
x<-tbl_df(p10_tmp2) %>% 
  group_by(date) %>%
  dplyr::summarize(counts=n_distinct(doctor.id))
x<-data.frame(x,difference=c(0,diff(x$counts)))

p <- ggplot(data=x,aes(x=factor(date),y=counts,group=F,colour='skyblue'))
p <- p + geom_line() +  geom_point() 
p <- p + geom_bar(aes(y=difference),stat="identity",colour=F,fill='skyblue',alpha=0.5)
p <- p + scale_y_continuous(sec.axis = sec_axis(~.*1, name = "increase"))
#p <- p + scale_colour_manual(values = c("skyblue", "skyblue"))
p <- p + labs(y = "Counts",
              x = "Date") + 
  scale_colour_discrete(#values = c("red", "skyblue"),
                        breaks=c("counts", "difference"),
                        labels=c("counts", "difference")) #+
  #scale_shape_discrete(
                       #breaks=c("counts", "difference"),
                       #labels=c("counts", "difference"))

p+theme(legend.position="bottom")+guides(fill=guide_legend(title=NULL))

## p11
p11_tmp1<-data.frame('date'=as.yearmon(p10_tmp1$most.recent.modify.date),
                     p10_tmp1)
head(p11_tmp1)

P11_p1<-p11_tmp1%>%
  group_by(date,region)%>%
  dplyr::summarise(counts=n_distinct(doctorid))

#month_summ<-dplyr::summarise(group_by(data.frame(P11_p1),date),sum=sum(counts))
#p11_tmp2<-merge(month_summ,P11_p1)
#p11_tmp3<-data.frame(p11_tmp2,'percentage'=p11_tmp2$counts/p11_tmp2$sum)
#i-th month
i<-1
data<-P11_p1[P11_p1$date==unique(P11_p1$date)[i],]
data = data[order(data$counts, decreasing = TRUE),] 
myLabel = as.vector(paste( round(data$counts / sum(data$counts) * 100, 0), "%", sep = "") )


p11_first = ggplot(data[order(data$counts, decreasing = TRUE),] , aes(x = "", y = counts, fill = region)) +
  geom_bar(stat = "identity", width = 0.8)+
  coord_polar(theta="y") +
  theme_bw() +
  labs(x = "", y = "", title = "") +  
  theme(axis.ticks = element_blank()) +  
  theme(legend.position = "right") +  
  theme(axis.text.x = element_blank()) +
  scale_fill_brewer(palette=1) +
  geom_text(aes(y = counts/2 + c(0, cumsum(counts)[-length(counts)]), x = sum(counts)/24, label = myLabel), size = 3)  

P11_p4<-p11_tmp1%>%
  group_by(date,department)%>%
  dplyr::summarise(counts=n_distinct(doctorid))

#month_summ<-dplyr::summarise(group_by(data.frame(P11_p1),date),sum=sum(counts))
#p11_tmp2<-merge(month_summ,P11_p1)
#p11_tmp3<-data.frame(p11_tmp2,'percentage'=p11_tmp2$counts/p11_tmp2$sum)
#i-th month
i<-1
data<-P11_p4[P11_p4$date==unique(P11_p4$date)[i],]
data = data[order(data$counts, decreasing = TRUE),] 
myLabel = as.vector(paste( round(data$counts / sum(data$counts) * 100, 0), "%", sep = "") )


p11_fourth = ggplot(data[order(data$counts, decreasing = TRUE),] , aes(x = "", y = counts, fill = department,order=counts)) +
  geom_bar(stat = "identity", width = 0.8)+
  coord_polar(theta="y") +
  theme_bw() +
  labs(x = "", y = "", title = "") +  
  theme(axis.ticks = element_blank()) +  
  theme(legend.position = "right") +  
  theme(axis.text.x = element_blank()) +
  scale_fill_brewer(palette=1) +
  geom_text(aes(y = counts/2 + c(0, cumsum(counts)[-length(counts)]), x = sum(counts)/24, label = myLabel), size = 3)  

## p12
p12_tmp1<-data.frame('date'=factor(as.yearmon(p10_tmp1$most.recent.modify.date)),p10_tmp1)
p12_tmp1<-p12_tmp1%>% group_by(date,hcp.major)%>%
  dplyr::summarise(counts=n_distinct(doctorid))

p <- ggplot(data=p12_tmp1,aes(x=factor(date),y=counts,group=hcp.major,colour=hcp.major)) +
   geom_line() +  geom_point() +
  theme(legend.title = element_blank(),legend.position = 'top')

p2<- ggplot(data=p12_tmp1[order(p12_tmp1$counts, decreasing = T),] ,aes(x=factor(date),y=counts,fill=hcp.major)) +
  geom_bar(stat='identity') +
   labs(x = '', y = '', title = '')
  theme(legend.title = element_blank(),legend.position = 'left')
   
## p13
  p13_tmp1 <- p12_tmp1 %>% group_by(date, hcp.major, region) %>%
    dplyr::summarise(counts = n_distinct(doctorid))
  data <- p13_tmp1[p13_tmp1$date == unique(p13_tmp1$date)[i], ]
  data$hcp.major <- factor(data$hcp.major)
  data$region <- factor(data$region)
  tmp <- dplyr::summarise(group_by(data, hcp.major), total = sum(counts))
  total <-
    data.frame(
      'date' = rep(factor(unique(data$date)), nrow(tmp)),
      'hcp.major' = as.character(tmp$hcp.major),
      'region' = as.character(rep('全国', nrow(tmp))),
      'counts' = tmp$total
    )
  data1 <- rbind(data, total)
  
  p1 <-
    ggplot(data = data , aes(
      x = factor(region),
      y = counts,
      fill = hcp.major
    )) +
    geom_bar(stat = 'identity') +
    labs(x = '', y = '', title = '') +
    theme(legend.title = element_blank(), legend.position = 'left')
######？？？？？未完#####

## p14  ??要不要按每月来
## p1
p14_tmp1<-data.frame(eda_dat[grep('Q15',eda_dat$questions),c(1,5,7,11)])
p14_tmp1<-data.frame('date'=factor(as.yearmon(p14_tmp1$most.recent.modify.date)),p14_tmp1)
p14_tmp2<-separate_rows(p14_tmp1,answers,sep=';')


p14_tmp2<-p14_tmp2 %>% 
  group_by(date,answers, hcp.major) %>% 
  dplyr::summarize(counts = n_distinct(doctorid)) %>% 
  do(plyr::rbind.fill(., data_frame(date = first(.$date),
                                    answers=first(.$answers),
                                    hcp.major = c("All"),
                                    counts=sum(.$counts))))
summ<-p14_tmp1%>%group_by(date,hcp.major)%>%dplyr::summarise(total=n())%>%
  do(plyr::rbind.fill(., data_frame(date = first(.$date),
                                    hcp.major = c("All"),
                                    total=sum(.$total))))

tmp<-merge(p14_tmp2,summ)

## 取第i个月的
p14_tmp3<-data.frame(tmp,'percentage'=round(tmp$counts/tmp$total*100,0))
ggplot(data=p14_tmp3[p14_tmp3$date=='7月 2016',],aes(x=factor(hcp.major),y=percentage,group=answers,colour=answers)) +
   geom_line() +  geom_point() +
  theme(legend.title = element_blank(),legend.position = 'top')


##p2
p14_tmp1<-data.frame(eda_dat[grep('Q16',eda_dat$questions),c(1,5,7,11)])
p14_tmp1<-data.frame('date'=factor(as.yearmon(p14_tmp1$most.recent.modify.date)),p14_tmp1)
p14_tmp2<-separate_rows(p14_tmp1,answers,sep=';')


p14_tmp2<-p14_tmp2 %>% 
  group_by(date,answers, hcp.major) %>% 
  dplyr::summarize(counts = n_distinct(doctorid)) %>% 
  do(plyr::rbind.fill(., data_frame(date = first(.$date),
                                    answers=first(.$answers),
                                    hcp.major = c("All"),
                                    counts=sum(.$counts))))
summ<-p14_tmp1%>%group_by(date,hcp.major)%>%dplyr::summarise(total=n())%>%
  do(plyr::rbind.fill(., data_frame(date = first(.$date),
                                    hcp.major = c("All"),
                                    total=sum(.$total))))

tmp<-merge(p14_tmp2,summ)

## 取第i个月的
p14_tmp3<-data.frame(tmp,'percentage'=round(tmp$counts/tmp$total*100,0))
ggplot(data=p14_tmp3[p14_tmp3$date=='7月 2016',],aes(x=factor(hcp.major),y=percentage,group=answers,colour=answers)) +
   geom_line() +  geom_point() +
  theme(legend.title = element_blank(),legend.position = 'top')








## part2
## p15
p15_tmp1<-unique(eda_dat[,c(1,5,7,9,10,13)])
p15_tmp1<-data.frame('date'=as.yearmon(p15_tmp1$most.recent.modify.date),p15_tmp1)
p15_tmp1$hcp.major<-as.numeric(p15_tmp1$hcp.major)

data<- p15_tmp1%>% group_by(date,doctorid) %>%
  dplyr::summarise(change=(hcp.major[which(date==max(date))]-hcp.major[which(date==max(date))]))
View(p15_tmp1)                            
x<-function(data){
  data$hcp.major[which(data$date==max(data$date))]-data$hcp.major[which(data$date==max(data$date))]
}                           


###
##
data<-newdata%>%spread(date,hcp.major)

