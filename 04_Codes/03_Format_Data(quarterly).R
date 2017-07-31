# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #
# ProjectName:  Xi'an Jassen PET
# Purpose:      Format the data for the plot described in PPT
# programmer:   Anqi Chen
# Date:         07-31-2017
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #

install.packages('plotly')
library(zoo)
library(plyr) 
library(openxlsx)
library(dplyr)
library(ggplot2)
library(lubridate)
library(gtable)
library(grid)
library(tidyr)
library(plotly)







##-- formating data
##-- update data
## 数据处理：保留每个医生每月最新的一次记录
#first distinct over all elements
to_use_data<-unique(eda_dat[,c(1,5,7,8,9,10,11,13)]) 
to_use_data<-data.frame('quarter'=paste(year(to_use_data$most.recent.modify.date),
                                        quarters(to_use_data$most.recent.modify.date)),to_use_data)
to_use_data$hcp.major<-as.numeric(to_use_data$hcp.major)
##  取每日hcp.major最大的记录
data1<-to_use_data %>% group_by(most.recent.modify.date,doctorid)%>%
  dplyr::summarise(hcp.major=max(hcp.major))  #second distinct over day
data1.tmp<-merge(data1,to_use_data)
## 取每月最新的记录
data2<-data1.tmp %>% group_by(quarter,doctorid)%>%    #third distinct over month
  dplyr::summarise(most.recent.modify.date=max(most.recent.modify.date))
newdata<-merge(data1.tmp,data2)
#View(newdata)

# newdata$quarter<-as.character(newdata$quarter)
# newdata$quarter
## current data.frame 
# most.recent.modify.date doctorid     quarter   hcp.major  hospital            department       questions                                      answers               region    
#      2016-07-27     10001159           Q3         1      江西省皮肤病专科医院   皮肤内科     Q4:治疗甲真菌病过程中最常用的处方原则是？   C.口服+外用联合使用        浙闽湘鄂赣

## variables available 
# [1] "most.recent.modify.date" 
# [2] "doctorid"               
# [3] "quarter"                    
# [4] "hcp.major"              
# [5] "hospital"                
# [6] "department"             
# [7] "questions"               
# [8] "answers"                
# [9] "region"    
#   





## part1 受访医生概况




## p10
## 每季度受访医生数目递增情况及总体分析人数
p10_tmp1<-tbl_df(newdata) %>% 
  group_by(quarter) %>%
  dplyr::summarize(counts=n_distinct(doctorid))
p10_tmp2<-data.frame(p10_tmp1,difference=c(0,diff(p10_tmp1$counts)))
#View(p10_tmp2)
p <- ggplot(data=p10_tmp2,aes(x=factor(quarter),y=counts,group=F,colour='skyblue'))
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
## 每季度受访医生的总体分布情况
## suppose quarter i

## picture 1 大区分布
P11_p1<-newdata[newdata$quarter==i,]%>%
  group_by(region)%>%
  dplyr::summarise(counts=n_distinct(doctorid))
View(P11_p1)

#quarter i
i<-'2016 Q3'
p10_tmp3<-P11_p1[P11_p1$quarter==i,]


P11_p1 <- newdata[newdata$quarter==i,] %>%
  group_by(region) %>%
  dplyr::summarize(counts = n_distinct(doctorid)) %>%
  plot_ly(labels = ~region, values = ~counts) %>%
  add_pie(hole = 0.6) %>%
  layout(title = "Donut charts using Plotly",  showlegend = F,
         xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
         yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))

# Create a shareable link to your chart
# Set up API credentials: https://plot.ly/r/getting-started
p11_first = plotly_POST(P11_p1, filename="pie/donut")
p11_first


## picture 4 科室分布
P11_p4<-newdata%>%
  group_by(quarter,department)%>%
  dplyr::summarise(counts=n_distinct(doctorid))

# Quarter i
data<-P11_p4[P11_p4$quarter==i,]

P11_p4 <- newdata[newdata$quarter==i,] %>%
  group_by(region) %>%
  dplyr::summarize(counts = n_distinct(doctorid)) %>%
  plot_ly(labels = ~region, values = ~counts) %>%
  add_pie(hole = 0.6) %>%
  layout(title = "Donut charts using Plotly",  showlegend = F,
         xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
         yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))

# Create a shareable link to your chart
# Set up API credentials: https://plot.ly/r/getting-started
p11_fourth = plotly_POST(P11_p4, filename="pie/donut")
p11_fourth





## p12 每月各个观念级别的受访医生数目及比例变化情况
p12_tmp1<-newdata%>% group_by(quarter,hcp.major)%>%
  dplyr::summarise(counts=n_distinct(doctorid))

p12_first <- ggplot(data=p12_tmp1,aes(x=factor(quarter),y=counts,group=hcp.major,colour=hcp.major)) +
  geom_line() +  geom_point() +
  theme(legend.title = element_blank(),legend.position = 'top')
p12_first

p12_second <- ggplot(data=p12_tmp1[order(p12_tmp1$counts, decreasing = T),] ,aes(x=factor(quarter),y=counts,fill=hcp.major)) +
  geom_bar(stat='identity') +
  labs(x = '', y = '', title = '') +
theme(legend.title = element_blank(),legend.position = 'left')
p12_second







## p13 每季度不同观念级别医生的分布情况
## picture 1 大区分布  
p13_tmp1 <- newdata 
p13_tmp1$quarter<-as.character(p13_tmp1$quarter)
p13_tmp2 <- p13_tmp1 %>% group_by(quarter,hcp.major,region) %>%
  dplyr::summarise(counts = n_distinct(doctorid)) %>%
  do(plyr::rbind.fill(., data_frame(quarter = first(.$quarter),
                                    hcp.major=first(.$hcp.major),
                                    region = '全国',
                                    counts=sum(.$counts))))

## quarter i
p13_tmp3 <- p13_tmp2[p13_tmp2$quarter == i, ]

p13_first <-
  ggplot(data = p13_tmp3 , aes(
    x = factor(region),
    y = counts,
    fill = hcp.major
  )) +
  geom_bar(stat = 'identity') +
  labs(x = '', y = '', title = '') +
  theme(legend.title = element_blank(), legend.position = 'left')
p13_first


## picture 4 科室分布
p13_tmp4 <- p13_tmp1 %>% group_by(quarter,hcp.major,department) %>%
  dplyr::summarise(counts = n_distinct(doctorid)) %>%
  do(plyr::rbind.fill(., data_frame(quarter = first(.$quarter),
                                    hcp.major=first(.$hcp.major),
                                    department = '全部科室',
                                    counts=sum(.$counts))))
## quarter i
p13_tmp5 <- p13_tmp4[p13_tmp4$quarter == i, ]

p13_fourth <-
  ggplot(data = p13_tmp5 , aes(
    x = factor(department),
    y = counts,
    fill = hcp.major
  )) +
  geom_bar(stat = 'identity') +
  labs(x = '', y = '', title = '') +
  theme(legend.title = element_blank(), legend.position = 'left')
p13_fourth





## p14  不同观念级别医生对斯皮仁诺及其推广活动的接受和认可情况
## picture 1 对斯皮仁诺优势的认可情况
p14_tmp1<-newdata[grep('Q15',newdata$questions),]
p14_tmp1$quarter<-factor(p14_tmp1$quarter)
p14_tmp2<-separate_rows(p14_tmp1,answers,sep=';')

p14_tmp3<-p14_tmp2 %>% 
  group_by(quarter,answers, hcp.major) %>% 
  dplyr::summarize(counts = n_distinct(doctorid)) %>% 
  do(plyr::rbind.fill(., data_frame(quarter = first(.$quarter),
                                    answers=first(.$answers),
                                    hcp.major = c("All"),
                                    counts=sum(.$counts))))

summ<-p14_tmp1 %>% group_by(quarter,hcp.major)%>%dplyr::summarise(total=n_distinct(doctorid))%>%
  do(plyr::rbind.fill(., data_frame(quarter = first(.$quarter),
                                    hcp.major = c("All"),
                                    total=sum(.$total))))
p14_tmp4<-merge(p14_tmp3,summ)

## quarter i
p14_tmp5 <- data.frame(p14_tmp4,'percentage'=round(p14_tmp4$counts/p14_tmp4$total*100,0))
p14_tmp5 <- p14_tmp5[p14_tmp5$quarter==i,]
ggplot(data=p14_tmp5,aes(x=factor(hcp.major),y=percentage,group=answers,colour=answers)) +
  geom_line() +  geom_point() +
  theme(legend.title = element_blank(),legend.position = 'top')


##picture 2 对斯皮仁诺推广活动接受情况
p14_tmp6<-newdata[grep('Q16',newdata$questions),]
p14_tmp6$quarter<-factor(p14_tmp6$quarter)
p14_tmp7<-separate_rows(p14_tmp6,answers,sep=';')

p14_tmp8<-p14_tmp7 %>% 
  group_by(quarter,answers, hcp.major) %>% 
  dplyr::summarize(counts = n_distinct(doctorid)) %>% 
  do(plyr::rbind.fill(., data_frame(quarter = first(.$quarter),
                                    answers=first(.$answers),
                                    hcp.major = c("All"),
                                    counts=sum(.$counts))))

summ2<-p14_tmp6 %>% group_by(quarter,hcp.major)%>%dplyr::summarise(total=n_distinct(doctorid))%>%
  do(plyr::rbind.fill(., data_frame(quarter = first(.$quarter),
                                    hcp.major = c("All"),
                                    total=sum(.$total))))
p14_tmp9<-merge(p14_tmp8,summ2)

## Quarter i
p14_tmp10 <- data.frame(p14_tmp9,'percentage'=round(p14_tmp9$counts/p14_tmp9$total*100,0))
p14_tmp10 <- p14_tmp10[p14_tmp10$quarter==i,]
ggplot(data=p14_tmp10,aes(x=factor(hcp.major),y=percentage,group=answers,colour=answers)) +
  geom_line() +  geom_point() +
  theme(legend.title = element_blank(),legend.position = 'top')




## part2


## p15 每季度进阶医生的总体及在某一特定维度中的变化情况
#View(newdata)
rbind(1:ncol(newdata),colnames(newdata))
p15_tmp1 <- unique(newdata[,c(2,3,4,5,6,9)])
nrow(p15_tmp1)  # in total 3200 observations
length(unique(p15_tmp1$doctorid))  #in total 2642 doctors been interviewed 
## 将日期列转为行变量
p15_tmp2 <- p15_tmp1 %>% spread(quarter,hcp.major)
colnames(p15_tmp2)
nrow(p15_tmp2)     # confirmed  2642 observations after spread()


## use locf to impute missing hcp.majors and get lag between nearest two months
#View(p15_tmp2)
rbind(1:ncol(p15_tmp2),colnames(p15_tmp2))
x <- t(p15_tmp2[,5:ncol(p15_tmp2)])  ## 2642 observations confirmed & in total 12months
View(x)
ncol(x)
nrow(x)
## 
y <- na.locf(x)
z <- diff(y)  ## 11 lags confirmed & 2642 observations confirmed &
nn <- cbind(p15_tmp2[,1:4],t(z))
nn<-nn%>% gather(quarter,value,-c(doctorid,hospital,department,region))
View(nn)

## nn 
#    doctorid             hospital   department  region     quarter    value
# 1 10000574   中山大学附属第一医院     皮肤科    粤桂琼    2017 Q1      0       



## number of progressed doctors in every month
pro<-function(data){
  length(which(data>0))
}
no.pro<-apply(t(z),2,pro)
p15_tmp1<-data.frame(p10_tmp2[2:nrow(p10_tmp2),1:2],no.pro)
p15_tmp1<-data.frame(p15_tmp1,'percentage'=round(p15_tmp1$no.pro/p15_tmp1$counts*100,0))
## picture1
p <- ggplot(data=p15_tmp1,aes(x=factor(quarter),y=no.pro,group=F,colour='skyblue'))
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
## Quarter i 
i<-'2016 Q4'
p16_tmp1<-nn%>% gather(quarter,value,-c(doctorid,hospital,department,region))
## Picture 1 大区等级
p16_tmp1<-nn[nn$quarter==i,]%>% group_by(region)%>%
  summarise(no.pro=pro(value))

##每个区域受访医生
doc.no.region<-newdata%>%group_by(quarter,region)%>%dplyr::summarise(counts=n_distinct(doctorid))
View(doc.no.region)
p16_tmp2<-merge(p16_tmp1,doc.no.region[doc.no.region$quarter==i,])
p16_tmp2<-data.frame('region'=p16_tmp2$region,'progressed'=p16_tmp2$no.pro,'not.progressed'=(p16_tmp2$counts-p16_tmp2$no.pro))
View(p16_tmp2)
p16_tmp3<-gather(p16_tmp2,type,value,-region)
p16_tmp3<-p16_tmp3%>%group_by(type)%>%
  do(plyr::rbind.fill(., data_frame(type = first(.$type),
                                    region = c("全国"),
                                    value=sum(.$value))))
ggplot(data=p16_tmp3 ,aes(x=factor(region),y=value,fill=type)) +
  geom_bar(stat='identity') +
  labs(x = '', y = '', title = '')+
  theme(legend.title = element_blank(),legend.position = 'left')



## Picture 4 科室分布
p16_tmp4<-p16_tmp1%>% group_by(department)%>%
  summarise(no.pro=pro(value))

##每个科室受访医生
doc.no.dep<-newdata%>%group_by(quarter,department)%>%dplyr::summarise(counts=n_distinct(doctorid))
p16_tmp4<-merge(p16_tmp4,doc.no.dep[doc.no.dep$quarter==i,])
p16_tmp4<-data.frame('department'=p16_tmp4$department,'progressed'=p16_tmp4$no.pro,'not.progressed'=(p16_tmp4$counts-p16_tmp4$no.pro))
View(p16_tmp4)
p16_tmp4<-gather(p16_tmp4,type,value,-department)
p16_tmp4<-p16_tmp4%>%group_by(type)%>%
  do(plyr::rbind.fill(., data_frame(type = first(.$type),
                                    department = c("全部科室"),
                                    value=sum(.$value))))
ggplot(data=p16_tmp4 ,aes(x=factor(department),y=value,fill=type)) +
  geom_bar(stat='identity') +
  labs(x = '', y = '', title = '')+
  theme(legend.title = element_blank(),legend.position = 'left')




##p17
## Picture 1 观念进阶医生对斯皮仁诺优势的认可程度（Q15）
## Quarter i
##进阶医生
number.progressed<-length(which(nn$quarter==i&nn$value>0))
p17_tmp1<-merge(newdata,nn[which(nn$quarter==i&nn$value>0),c(1,5)])
p17_tmp1<-p17_tmp1[grep('Q15',p17_tmp1$questions),]
p17_tmp1<-separate_rows(p17_tmp1,answers,sep=';')
p17_tmp1<-p17_tmp1%>%group_by(answers)%>%dplyr::summarise(counts=n())
p17_tmp1<-data.frame('type'='progressed',p17_tmp1,'total'=number.progressed,'percentage'=round((p17_tmp1$counts/number.progressed*100),0))
##全部受访医生
number.all<-length(unique(newdata[which(newdata$quarter==i),]$doctorid))
p17_tmp2<-newdata[which(newdata$quarter==i),]
p17_tmp2<-p17_tmp2[grep('Q15',p17_tmp2$questions),]
p17_tmp2<-separate_rows(p17_tmp2,answers,sep=';')
p17_tmp2<-p17_tmp2%>%group_by(answers)%>%dplyr::summarise(counts=n())
p17_tmp2<-data.frame('type'='all',p17_tmp2,'total'=number.all,'percentage'=round((p17_tmp2$counts/number.all*100),0))

p17_tmp3<-rbind(p17_tmp1,p17_tmp2)
ggplot(data = p17_tmp3, mapping = aes(x = factor(answers), y = percentage, fill = type)) + 
  geom_bar(stat= 'identity', position = 'dodge') +
  coord_flip()


## Picture 2 观念进阶医生对斯皮仁诺推广活动的认可程度（Q16）
## quarter i
##进阶医生
number.progressed<-length(which(nn$quarter==i&nn$value>0))
p17_tmp4<-merge(newdata,nn[which(nn$quarter==i&nn$value>0),c(1,5)])
p17_tmp4<-p17_tmp4[grep('Q16',p17_tmp4$questions),]
p17_tmp4<-separate_rows(p17_tmp4,answers,sep=';')
p17_tmp4<-p17_tmp4%>%group_by(answers)%>%dplyr::summarise(counts=n())
p17_tmp4<-data.frame('type'='progressed',p17_tmp4,'total'=number.progressed,'percentage'=round((p17_tmp4$counts/number.progressed*100),0))
##全部受访医生
number.all<-length(unique(newdata[which(newdata$quarter==i),]$doctorid))
p17_tmp5<-newdata[which(newdata$quarter==i),]
p17_tmp5<-p17_tmp5[grep('Q16',p17_tmp5$questions),]
p17_tmp5<-separate_rows(p17_tmp5,answers,sep=';')
p17_tmp5<-p17_tmp5%>%group_by(answers)%>%dplyr::summarise(counts=n())
p17_tmp5<-data.frame('type'='all',p17_tmp5,'total'=number.all,'percentage'=round((p17_tmp5$counts/number.all*100),0))

p17_tmp6<-rbind(p17_tmp4,p17_tmp5)
ggplot(data = p17_tmp6, mapping = aes(x = factor(answers), y = percentage, fill = type)) + 
  geom_bar(stat= 'identity', position = 'dodge') +
  coord_flip()





## part3 推广活动与受访医生


## p19
## Picture 1 不同观念级别的医生出席会议的平均次数
## 取每个医生最近一次拜访得到的观念
## 每个医生最近一次拜访的时间及其ID
meeting_dat<-data.frame('quarter'=quarters(meeting_dat$imeeting.time),meeting_dat)
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

## Picture 2 不同观念医生接受拜访的平均次数
call_dat<-data.frame('quarter'=quarters(call_dat$call.date),call_dat)
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


## p20
## Picture 1 季度不同观念医生出席会议的平均次数
## Quarter i
p20_tmp1 <- meeting_dat[,c(1,7,8,13)]
get.hcp2 <- unique(newdata[,c(2,3,4,9)])

p20_tmp1 <- merge(p20_tmp1,get.hcp2)
View(newdata)

p20_tmp1 <- p20_tmp1%>%group_by(quarter,hcp.major,doctorid)%>%dplyr::summarise(counts=n())
p20_tmp2 <- p20_tmp1%>%group_by(quarter,hcp.major)%>%dplyr::summarise(average=mean(counts))
p20_tmp3 <- p20_tmp1%>%group_by(quarter)%>%dplyr::summarise(average=mean(counts))
p20_tmp4 <- plyr::rbind.fill(p20_tmp2, data_frame(quarter = p20_tmp3$quarter,
                                                  hcp.major = c("All"),
                                                  average = p20_tmp3$average))
p20_tmp4$average<-round(p20_tmp4$average,1)

ggplot(data = p20_tmp4, mapping = aes(x = factor(quarter), y = average, colour = hcp.major)) + 
  geom_line(aes(colour=hcp.major, group=hcp.major))+geom_point()

## Picture 2 每月不同观念医生接受拜访的平均次数
## Quarter i
p20_tmp5<-call_dat[,c(1,4)]
View(call_dat)
p20_tmp5<-merge(p20_tmp5,get.hcp2)


p20_tmp5<-p20_tmp5%>%group_by(quarter,hcp.major,doctorid)%>%dplyr::summarise(counts=n())

p20_tmp6<-p20_tmp5%>%group_by(quarter,hcp.major)%>%dplyr::summarise(average=mean(counts))
p20_tmp7<-p20_tmp5%>%group_by(quarter)%>%dplyr::summarise(average=mean(counts))
p20_tmp8<-plyr::rbind.fill(p20_tmp6, data_frame(quarter=p20_tmp7$quarter,
                                                hcp.major = c("All"),
                                                average=p20_tmp7$average))

p20_tmp8$average<-round(p20_tmp8$average,1)

ggplot(data = p20_tmp8, mapping = aes(x = factor(quarter), y = average, colour = hcp.major)) + 
  geom_point() +geom_line(aes(colour=hcp.major, group=hcp.major))







## part4 推广活动对观念进阶医生的影响


## p22 每月观念进阶医生参与推广活动情况
## picture 1 每月观念进阶医生出席会议的平均次数
## 要增加一个会议类型选项
p22_tmp1 <- meeting_dat[,c(1,7,8,13)]

##进阶医生
x<-nn[which(nn$value>0),]
p22_tmp2 <- p22_tmp1%>%group_by(quarter,doctorid)%>%dplyr::summarise(counts=n())
p22_tmp2$quarter<-as.character(p22_tmp2$quarter)
x$doctorid<-as.character(x$doctorid)
p22_tmp3 <- merge(p22_tmp2,x)


p22_tmp4 <- p22_tmp3%>%group_by(quarter)%>%dplyr::summarise(average=mean(counts))
p22_tmp4$average<-round(p22_tmp4$average,1)
p22_tmp4<-data.frame('type'='进阶医生',p22_tmp4)

##全部受访医生
rbind(1:ncol(newdata),colnames(newdata))
p22_tmp5<-unique(newdata[,c(2,3)])
p22_tmp5$quarter<-as.character(p22_tmp5$quarter)
p22_tmp6<-merge(p22_tmp2,p22_tmp5)
p22_tmp7<-p22_tmp6%>%group_by(quarter)%>%dplyr::summarise(average=mean(counts))

p22_tmp8<-plyr::rbind.fill(p22_tmp4, data_frame(type='全部受访医生',
                                                quarter=p22_tmp7$quarter,
                                                average=p22_tmp7$average))
ggplot(data = p22_tmp8, mapping = aes(x = factor(quarter), y = average, color = type)) + 
  geom_point()+geom_line(aes(colour=type, group=type))


## picture 2 每月观念进阶医生接受拜访的平均次数（可用于各类维度）
View(call_dat)
p22_2_tmp1 <- call_dat[,c(1,4)]

##进阶医生
p22_2_tmp2 <- p22_2_tmp1%>%group_by(quarter,doctorid)%>%dplyr::summarise(counts=n())
p22_2_tmp2$quarter<-as.character(p22_2_tmp2$quarter)
p22_2_tmp3 <- merge(p22_2_tmp2,x)


p22_2_tmp4 <- p22_2_tmp3%>%group_by(quarter)%>%dplyr::summarise(average=mean(counts))
p22_2_tmp4$average<-round(p22_2_tmp4$average,1)
p22_2_tmp4<-data.frame('type'='进阶医生',p22_2_tmp4)

##全部受访医生
p22_2_tmp5<-merge(p22_2_tmp2,p22_tmp5)
p22_2_tmp6<-p22_2_tmp5%>%group_by(quarter)%>%dplyr::summarise(average=mean(counts))

p22_2_tmp7<-plyr::rbind.fill(p22_2_tmp4, data_frame(type='全部受访医生',
                                                    quarter=p22_2_tmp6$quarter,
                                                    average=p22_2_tmp6$average))
ggplot(data = p22_2_tmp7, mapping = aes(x = factor(quarter), y = average, color = type)) + 
  geom_point()+geom_line(aes(colour=type, group=type))





