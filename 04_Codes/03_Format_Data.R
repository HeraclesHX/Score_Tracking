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
to_use_data<-unique(eda_dat[,c(1,5,7,8,9,10,11,13)])  #first distinct over all elements
unique(unique(eda_dat[,c(1,5,7,8,9,10,11,13)])$region)
to_use_data<-data.frame('date'=as.yearmon(to_use_data$most.recent.modify.date),to_use_data)

to_use_data$hcp.major<-as.numeric(to_use_data$hcp.major)
data1<-to_use_data %>% group_by(most.recent.modify.date,doctorid)%>%
  dplyr::summarise(hcp.major=max(hcp.major))  #second distinct over day
data1.tmp<-merge(data1,to_use_data)
data2<-data1.tmp %>% group_by(date,doctorid)%>%    #third distinct over month
  dplyr::summarise(most.recent.modify.date=max(most.recent.modify.date))
newdata<-merge(data1.tmp,data2)

# newdata$date<-as.character(newdata$date)
# newdata$date
## current data.frame 
# most.recent.modify.date doctorid     date   hcp.major  hospital            department       questions                                      answers               region    
#      2016-07-27     10001159     7月 2016    1      江西省皮肤病专科医院   皮肤内科     Q4:治疗甲真菌病过程中最常用的处方原则是？  C.口服+外用联合使用 浙闽湘鄂赣

## variables available 
# [1] "most.recent.modify.date" 
# [2] "doctorid"               
# [3] "date"                    
# [4] "hcp.major"              
# [5] "hospital"                
# [6] "department"             
# [7] "questions"               
# [8] "answers"                
# [9] "region"    
#   





## part1
## p10
p10_tmp1<-tbl_df(newdata) %>% 
  group_by(date) %>%
  dplyr::summarize(counts=n_distinct(doctorid))
p10_tmp2<-data.frame(p10_tmp1,difference=c(0,diff(p10_tmp1$counts)))
View(p10_tmp2)
p <- ggplot(data=p10_tmp2,aes(x=factor(date),y=counts,group=F,colour='skyblue'))
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
View(newdata)
p15_tmp1<-unique(newdata[,c(2,3,4,5,6,9)])
nrow(p15_tmp1)  # in total 3200 observations
length(unique(p15_tmp1$doctorid))  #in total 2642 doctors been interviewed 
data<-p15_tmp1%>%spread(date,hcp.major)
nrow(data)     # confirmed  2642 observations after spread()


## use locf to impute missing hcp.majors and get lag between nearest two months
View(data)
x<-t(data[,5:ncol(data)])  ## 2642 observations confirmed & in total 12months
ncol(x)
nrow(x)
y<-na.locf(x)
z<-diff(y)  ## 11 lags confirmed & 2642 observations confirmed &
nn<-cbind(data[,1:4],t(z))
View(nn)
head(nn,1)

## nn 
 #   doctorid   hospital              X8月.2016 X9月.2016  X10月.2016 X11月.2016 X12月.2016 X1月.2017 X2月.2017
 # 10000574 中山大学附属第一医院        NA         0          0       0            0           0         0
         

## number of progressed doctors in every month
pro<-function(data){
  length(which(data>0))
}
no.pro<-apply(t(z),2,pro)
p15_tmp1<-data.frame(p10_tmp2[2:nrow(p10_tmp2),1:2],no.pro)
p15_tmp1<-data.frame(p15_tmp1,'percentage'=round(p15_tmp1$no.pro/p15_tmp1$counts*100,0))
## picture1
p <- ggplot(data=p15_tmp1,aes(x=factor(date),y=no.pro,group=F,colour='skyblue'))
p <- p + geom_line() +  geom_point() 
p <- p + geom_bar(aes(y=percentage),stat="identity",colour=F,fill='skyblue',alpha=0.5)
p <- p + scale_y_continuous(sec.axis = sec_axis(~.*1, name = "percentage"))
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


## p16 观念进阶医生的分布情况
## 单月 suppose 8月.2016
p16_tmp1<-nn
p16_tmp1[is.na(p16_tmp1)]<-0
p16_tmp1<-nn[,-c(6:ncol(nn))]
View(p16_tmp1)
colnames(p16_tmp1)
unique(p16_tmp1$region)

##大区等级
p16_tmp2<-p16_tmp1%>% group_by(region)%>%
  summarise(no.pro=pro(`8月 2016`))

##每月每个区域受访医生
doc.no.region<-newdata%>%group_by(date,region)%>%dplyr::summarise(counts=n_distinct(doctorid))
View(doc.no.region)
p16_tmp3<-merge(p16_tmp2,doc.no.region[doc.no.region$date=='8月 2016',])
p16_tmp3<-data.frame('region'=p16_tmp3$region,'progressed'=p16_tmp3$no.pro,'not.progressed'=(p16_tmp3$counts-p16_tmp3$no.pro))
View(p16_tmp3)
p16_tmp3<-gather(p16_tmp3,type,value,-region)
p16_tmp3<-p16_tmp3%>%group_by(type)%>%
  do(plyr::rbind.fill(., data_frame(type = first(.$type),
                                    region = c("All"),
                                    value=sum(.$value))))
ggplot(data=p16_tmp3 ,aes(x=factor(region),y=value,fill=type)) +
  geom_bar(stat='identity') +
  labs(x = '', y = '', title = '')+
theme(legend.title = element_blank(),legend.position = 'left')



##科室分布
p16_tmp4<-p16_tmp1%>% group_by(department)%>%
  summarise(no.pro=pro(`8月 2016`))

##每月每个科室受访医生
doc.no.dep<-newdata%>%group_by(date,department)%>%dplyr::summarise(counts=n_distinct(doctorid))
length(unique(doc.no.dep$department))

p16_tmp4<-merge(p16_tmp4,doc.no.dep[doc.no.dep$date=='8月 2016',])
p16_tmp4<-data.frame('department'=p16_tmp4$department,'progressed'=p16_tmp4$no.pro,'not.progressed'=(p16_tmp4$counts-p16_tmp4$no.pro))
View(p16_tmp5)
p16_tmp4<-gather(p16_tmp4,type,value,-department)
p16_tmp4<-p16_tmp4%>%group_by(type)%>%
  do(plyr::rbind.fill(., data_frame(type = first(.$type),
                                    department = c("All"),
                                    value=sum(.$value))))
ggplot(data=p16_tmp4 ,aes(x=factor(department),y=value,fill=type)) +
  geom_bar(stat='identity') +
  labs(x = '', y = '', title = '')+
  theme(legend.title = element_blank(),legend.position = 'left')


##p17
## 观念进阶医生对斯皮仁诺优势的认可程度（Q15）
## 单月 suppose 11月.2016
##进阶医生
number.progressed<-length(which(nn$`11月 2016`>0))
p17_tmp1<-newdata[newdata$date=='11月 2016'&newdata$doctorid%in%nn[which(nn$`11月 2016`>0),]$doctorid,]
p17_tmp1<-p17_tmp1[grep('Q15',p17_tmp1$questions),]
p17_tmp1<-separate_rows(p17_tmp1,answers,sep=';')
p17_tmp1<-p17_tmp1%>%group_by(answers)%>%dplyr::summarise(counts=n())
p17_tmp1<-data.frame('type'='progressed',p17_tmp1,'total'=number.progressed,'percentage'=round((p17_tmp1$counts/number.progressed*100),0))
##全部受访医生
number.all<-length(unique(newdata[which(newdata$date=='11月 2016'),]$doctorid))
p17_tmp2<-newdata[which(newdata$date=='11月 2016'),]
p17_tmp2<-p17_tmp2[grep('Q15',p17_tmp2$questions),]
p17_tmp2<-separate_rows(p17_tmp2,answers,sep=';')
p17_tmp2<-p17_tmp2%>%group_by(answers)%>%dplyr::summarise(counts=n())
p17_tmp2<-data.frame('type'='all',p17_tmp2,'total'=number.all,'percentage'=round((p17_tmp2$counts/number.all*100),0))

p17_tmp3<-rbind(p17_tmp1,p17_tmp2)
ggplot(data = p17_tmp3, mapping = aes(x = factor(answers), y = percentage, fill = type)) + 
  geom_bar(stat= 'identity', position = 'dodge') +
  coord_flip()

## 观念进阶医生对斯皮仁诺推广活动的认可程度（Q16）
## 单月 suppose 11月.2016
##进阶医生
number.progressed<-length(which(nn$`11月 2016`>0))
p17_tmp4<-newdata[newdata$date=='11月 2016'&newdata$doctorid%in%nn[which(nn$`11月 2016`>0),]$doctorid,]
p17_tmp4<-p17_tmp4[grep('Q16',p17_tmp4$questions),]
p17_tmp4<-separate_rows(p17_tmp4,answers,sep=';')
p17_tmp4<-p17_tmp4%>%group_by(answers)%>%dplyr::summarise(counts=n())
p17_tmp4<-data.frame('type'='progressed',p17_tmp4,'total'=number.progressed,'percentage'=round((p17_tmp4$counts/number.progressed*100),0))
##全部受访医生
number.all<-length(unique(newdata[which(newdata$date=='11月 2016'),]$doctorid))
p17_tmp5<-newdata[which(newdata$date=='11月 2016'),]
p17_tmp5<-p17_tmp5[grep('Q16',p17_tmp5$questions),]
p17_tmp5<-separate_rows(p17_tmp5,answers,sep=';')
p17_tmp5<-p17_tmp5%>%group_by(answers)%>%dplyr::summarise(counts=n())
p17_tmp5<-data.frame('type'='all',p17_tmp5,'total'=number.all,'percentage'=round((p17_tmp5$counts/number.all*100),0))

p17_tmp6<-rbind(p17_tmp4,p17_tmp5)
ggplot(data = p17_tmp6, mapping = aes(x = factor(answers), y = percentage, fill = type)) + 
  geom_bar(stat= 'identity', position = 'dodge') +
  coord_flip()





## part3
##p19
## 不同观念级别的医生出席会议的平均次数
## 取每个医生最近一次拜访得到的观念
View(newdata)
## 每个医生最近一次拜访的时间及其ID
meeting_dat<-data.frame('date'=as.yearmon(meeting_dat$imeeting.time),meeting_dat)
recent.time<-newdata%>%group_by(doctorid)%>%dplyr::summarise(most.recent.modify.date=max(most.recent.modify.date))
get.hcp<-unique(newdata[,c(1,2,3,4,9)])
p19_tmp1<-merge(recent.time,get.hcp)
View(p19_tmp1)
p19_tmp1<-p19_tmp1[,c(1,4,5)]
p19_tmp2<-meeting_dat[,c(7,8,13)]
p19_tmp2<-merge(p19_tmp1,p19_tmp2)
p19_tmp3<-p19_tmp2%>%group_by(imeeting.type,hcp.major,doctorid)%>%
  dplyr::summarise(counts=n())%>%
  dplyr::summarise(average=mean(counts))
p19_all1<-p19_tmp2%>%group_by(imeeting.type,doctorid)%>%
  dplyr::summarise(counts=n())%>%
  dplyr::summarise(average=mean(counts))
p19_tmp4<-plyr::rbind.fill(p19_tmp3, data_frame(imeeting.type = p19_all1$imeeting.type,
                                  hcp.major = c("All"),
                                  average=p19_all1$average))
p19_tmp4$average<-round(p19_tmp4$average,1)

ggplot(data = p19_tmp4, mapping = aes(x = factor(imeeting.type), y = average, fill = hcp.major)) + 
  geom_bar(stat= 'identity', position = 'dodge')

## 不同观念医生接受拜访的平均次数
call_dat<-data.frame('date'=as.yearmon(call_dat$call.date),call_dat)
rbind(c(1:ncol(call_dat)),colnames(call_dat))
p19_tmp5<-call_dat[,c(1,4)]
p19_tmp6<-merge(p19_tmp5,p19_tmp1)
View(p19_tmp6)
p19_tmp7<-p19_tmp6%>%group_by(region,hcp.major,doctorid)%>%
  dplyr::summarise(counts=n())%>%
  dplyr::summarise(average=mean(counts))
p19_all2<-p19_tmp6%>%group_by(region,doctorid)%>%
  dplyr::summarise(counts=n())%>%
  dplyr::summarise(average=mean(counts))
p19_tmp8<-plyr::rbind.fill(p19_tmp7, data_frame(region = p19_all2$region,
                                                hcp.major = c("All"),
                                                average=p19_all2$average))
p19_tmp8$average<-round(p19_tmp8$average,1)

ggplot(data = p19_tmp8, mapping = aes(x = factor(region), y = average, fill = hcp.major)) + 
  geom_bar(stat= 'identity', position = 'dodge')


##p20
## 每月不同观念医生出席会议的平均次数
## 单月 suppose 1月.2017
p20_tmp1<-meeting_dat[,c(1,7,8,13)]
get.hcp2<-unique(newdata[,c(2,3,4,9)])

p20_tmp1<-merge(p20_tmp1,get.hcp2)
View(newdata)

p20_tmp1<-p20_tmp1%>%group_by(date,hcp.major,doctorid)%>%dplyr::summarise(counts=n())
p20_tmp2<-p20_tmp1%>%group_by(date,hcp.major)%>%dplyr::summarise(average=mean(counts))
p20_tmp3<-p20_tmp1%>%group_by(date)%>%dplyr::summarise(average=mean(counts))
p20_tmp4<-plyr::rbind.fill(p20_tmp2, data_frame(date = p20_tmp3$date,
                                                hcp.major = c("All"),
                                                average = p20_tmp3$average))
p20_tmp4$average<-round(p20_tmp4$average,1)

ggplot(data = p20_tmp4, mapping = aes(x = factor(date), y = average, colour = hcp.major)) + 
  geom_line()+geom_point()

## 每月不同观念医生接受拜访的平均次数
## 单月 suppose 1月.2017
p20_tmp5<-call_dat[,c(1,4)]
View(call_dat)
p20_tmp5<-merge(p20_tmp5,get.hcp2)


p20_tmp5<-p20_tmp5%>%group_by(date,hcp.major,doctorid)%>%dplyr::summarise(counts=n())
#
p20_tmp6<-p20_tmp5%>%group_by(date,hcp.major)%>%dplyr::summarise(average=mean(counts))
p20_tmp7<-p20_tmp5%>%group_by(date)%>%dplyr::summarise(average=mean(counts))
p20_tmp8<-plyr::rbind.fill(p20_tmp6, data_frame(date=p20_tmp7$date,
                                                hcp.major = c("All"),
                                                average=p20_tmp7$average))
#

p20_tmp8$average<-round(p20_tmp8$average,1)

ggplot(data = p20_tmp8, mapping = aes(x = factor(date), y = average, colour = hcp.major)) + 
  geom_point() +geom_line()


