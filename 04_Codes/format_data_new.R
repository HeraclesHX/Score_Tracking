# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #
# ProjectName:  Xi'an Jassen PET
# Purpose:      Format the data for the plot described in PPT
# programmer:   Anqi Chen
# Date:         07-31-2017
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #

library(zoo)
library(plyr) 
library(openxlsx)
library(dplyr)
library(lubridate)
library(gtable)
library(grid)
library(tidyr)
library(plotly)


##-- formating data
##-- update data
## 数据处理：保留每个医生每月最新的一次记录
#first distinct over all elements
meeting_dat<-data.frame('quarter'=paste(year(meeting_dat$imeeting.time),quarters(meeting_dat$imeeting.time)),'imeeting.type'=meeting_dat$imeeting.type,'doctorid'=meeting_dat$doctorid)
call_dat<-data.frame('quarter'=paste(year(call_dat$call.date),quarters(call_dat$call.date)),
'call.date'=call_dat$call.date,'doctor.tier'=call_dat$doctor.tier,'doctorid'=call_dat$doctorid)

eda_dat<-eda_dat[eda_dat$target.department=="Y",]
to_use_data <- unique(eda_dat[,c(1,5,7,8,9,10,11,12,13)]) 
to_use_data <- data.frame('quarter'=paste(year(to_use_data$most.recent.modify.date),
                                          quarters(to_use_data$most.recent.modify.date)),to_use_data)
to_use_data$hcp.major <- as.numeric(to_use_data$hcp.major)

newdata<- to_use_data %>%
  select(quarter,hcp.major,most.recent.modify.date,doctorid,department,region) %>%
  distinct() %>%
  arrange(doctorid,most.recent.modify.date) %>%
  group_by(doctorid,quarter) %>%
  filter(row_number()==n())

doctor.tier.info<-call_dat %>%
  select(doctorid,call.date,quarter,doctor.tier) %>%
  arrange(doctorid,call.date) %>%
  group_by(doctorid,quarter) %>%
  filter(row_number()==n()) %>%
  group_by(doctorid) %>%
  filter(row_number()==n()) %>%
  select(doctorid,doctor.tier)

doctor.tier.info$doctorid<-as.character(doctor.tier.info$doctorid)
##match doctor.tier.infor with eda data
newdata<- newdata %>%
  left_join(doctor.tier.info,by='doctorid')


## part1 受访医生概况
## p10
## 每季度受访医生数目递增情况及总体分析人数
doc.general.quarter<-tbl_df(newdata) %>% 
  group_by(quarter) %>%
  dplyr::summarize(counts=n()) %>%
  dplyr::mutate(difference=c(0,diff(counts)))


p10 <- 
  plot_ly(doc.general.quarter,x = ~quarter, y = ~difference, name = "新增", type = "bar") %>%
  add_trace(y = ~counts, name = '总数',mode='lines',type='scatter',yaxis = "y2") %>%
  layout(
    title = "受访医生总数及每季度新增人数",
    yaxis2 = list(
      tickfont = list(color = "red"),
      overlaying = "y",
      side = "right",
      title = ""),
    xaxis = list(title="")
  )

install.packages('RSelenium')
library(RSelenium)
library(webshot)
rD <- RSelenium::rsDriver(browser = 'internet explorer')
export(p10,'y.jpeg')

## p11
## 每季度受访医生的总体分布情况
## picture 1 大区分布
i <- '2016 Q3'
doc.region.quarter <- newdata %>%
  group_by(quarter,region) %>%
  dplyr::summarise(counts=n())

p11_first <- 
  plot_ly(doc.region.quarter[doc.region.quarter$quarter==i,],labels = ~region, values = ~counts) %>%
  add_pie(hole = 0.6) %>%
  layout(title = "大区分布",  showlegend = T,
         xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
         yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))


## picture 3 医生等级分布
doc.tier.quarter <- newdata %>%
  group_by(quarter,doctor.tier) %>%
  dplyr::summarise(counts=n())
 
p11_second <- 
  plot_ly(doc.tier.quarter[doc.tier.quarter$quarter==i,],labels = ~doctor.tier, values = ~counts) %>%
  add_pie(hole = 0.6) %>%
  layout(title = "医生等级分布",  showlegend = T,
         xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
         yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))


## picture 4 科室分布
doc.department.quarter <- newdata %>%
  group_by(quarter,department) %>%
  dplyr::summarise(counts=n())

p11_fourth <- 
  plot_ly(doc.department.quarter[doc.department.quarter$quarter==i,],labels = ~department, values = ~counts) %>%
  add_pie(hole = 0.6) %>%
  layout(title = "科室分布",  showlegend = T,
         xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
         yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))



## p12 每季度各个观念级别的受访医生数目及比例变化情况
doc.hcp.quarter <- newdata %>% 
  group_by(quarter,hcp.major) %>%
  dplyr::summarise(counts=n())

p12_first <-
  plot_ly(ungroup(doc.hcp.quarter), x = ~factor(quarter), y = ~counts, type = 'scatter', mode='lines',color=~factor(hcp.major)) %>%
  layout(title = "每季度医生观念级别变化情况",  showlegend = T,
         xaxis = list(title = "",showgrid = T, zeroline = T, showticklabels = T),
         yaxis = list(title = "",showgrid = T, zeroline = T, showticklabels = T))



p12_second <- 
  plot_ly(ungroup(doc.hcp.quarter), x = ~factor(quarter), y = ~counts, type = 'bar',color=~factor(hcp.major)) %>%
  layout(title = "每季度医生观念级别变化情况",  showlegend = T,
         xaxis = list(title = "",showgrid = T, zeroline = T, showticklabels = T),
         yaxis = list(title = "",showgrid = T, zeroline = T, showticklabels = T), 
         barmode = 'stack')



## p13 每季度不同观念级别医生的分布情况
## picture 1 大区分布  
doc.region.hcp.quarter <- newdata %>% 
  group_by(quarter,hcp.major,region) %>%
  dplyr::summarise(counts = n()) %>%
  do(plyr::rbind.fill(., data_frame(quarter = first(.$quarter),
                                    hcp.major=first(.$hcp.major),
                                    region = '全国',
                                    counts=sum(.$counts))))
## quarter i
p13_first <- 
  plot_ly(ungroup(doc.region.hcp.quarter[doc.region.hcp.quarter$quarter == i, ]), x = ~factor(region), y = ~counts,color = ~factor(hcp.major), type = 'bar') %>%
  layout(title = "大区分布",  showlegend = T,
         xaxis = list(title = "",showgrid = T, zeroline = T, showticklabels = T),
         yaxis = list(title = "",showgrid = T, zeroline = T, showticklabels = T), 
         barmode = 'stack')

## picture 3 医生等级分布
doc.tier.hcp.quarter <- newdata %>%
  group_by(quarter,hcp.major,doctor.tier) %>%
  dplyr::summarise(counts=n()) %>%
  do(plyr::rbind.fill(.,data.frame(quarter=first(.$quarter),
                                   doctor.tier="全部",
                                   hcp.major=first(.$hcp.major),
                                   counts=sum(.$counts))))
## quarter j
j<-'2017 Q1'
p13_third <- 
  plot_ly(ungroup(doc.tier.hcp.quarter[doc.tier.hcp.quarter$quarter == j, ]), x = ~factor(doctor.tier), y = ~counts,color = ~factor(hcp.major), type = 'bar') %>%
  layout(title = "医生等级分布",  showlegend = T,
         xaxis = list(title = "",showgrid = T, zeroline = T, showticklabels = T),
         yaxis = list(title = "",showgrid = T, zeroline = T, showticklabels = T), 
         barmode = 'stack')

## picture 4 科室分布
doc.department.hcp.quarter <- newdata %>% 
  group_by(quarter,hcp.major,department) %>%
  dplyr::summarise(counts = n()) %>%
  do(plyr::rbind.fill(., data_frame(quarter = first(.$quarter),
                                    hcp.major=first(.$hcp.major),
                                    department = '全部科室',
                                    counts=sum(.$counts))))
## quarter i
p13_fourth <- 
  plot_ly(ungroup(doc.department.hcp.quarter[doc.department.hcp.quarter$quarter == i, ]), x = ~factor(department), y = ~counts,color=~factor(hcp.major), type = 'bar') %>%
  layout(title = "科室分布",  showlegend = T,
         xaxis = list(title = "",showgrid = T, zeroline = T, showticklabels = T),
         yaxis = list(title = "",showgrid = T, zeroline = T, showticklabels = T), 
         barmode = 'stack')



## p14  不同观念级别医生对斯皮仁诺及其推广活动的接受和认可情况
## picture 1 对斯皮仁诺优势的认可情况
diff.hcp.perception.question15<-to_use_data %>%
  select(quarter,most.recent.modify.date,doctorid,questions,answers,hcp.major) %>%
  filter(substr(question,1,2)=='Q15') %>% 
  arrange(doctorid,most.recent.modify.date) %>%
  filter(row_number()==n()) %>%
  group_by(quarter,hcp.major) %>%
  mutate(total=n()) %>%
  separate_rows(answers,sep=';') %>%
  group_by(quarter,answers,hcp.major) %>%
  dplyr::summarize(counts = n()) %>%
  do(plyr::rbind.fill(., data_frame(quarter = first(.$quarter),
                                    answers=first(.$answers),
                                    hcp.major = c("全部受访医生"),
                                    counts=sum(.$counts),
                                    total=first(.$total)))) %>%
  mutate(percentage=round(counts/total*100,0))


p14_first <- plot_ly(diff.hcp.perception.question15[diff.hcp.perception.question15$quarter==i,], x = ~factor(hcp.major), y = ~percentage, type = 'scatter', mode = 'lines', color = ~answers) %>%
  layout(title = '对斯皮仁诺优势的认可情况',
         xaxis = list(title = ''),
         yaxis = list (title = ''))



##picture 2 对斯皮仁诺推广活动接受情况
diff.hcp.perception.question16<-to_use_data %>%
  select(quarter,most.recent.modify.date,doctorid,questions,answers,hcp.major) %>%
  filter(substr(question,1,2)=='Q16') %>% 
  arrange(doctorid,most.recent.modify.date) %>%
  filter(row_number()==n()) %>%
  group_by(quarter,hcp.major) %>%
  mutate(total=n()) %>%
  separate_rows(answers,sep=';') %>%
  group_by(quarter,answers,hcp.major) %>%
  dplyr::summarize(counts = n()) %>%
  do(plyr::rbind.fill(., data_frame(quarter = first(.$quarter),
                                    answers=first(.$answers),
                                    hcp.major = c("全部受访医生"),
                                    counts=sum(.$counts),
                                    total=first(.$total)))) %>%
  mutate(percentage=round(counts/total*100,0))

P14_second <- plot_ly(diff.hcp.perception.question16[diff.hcp.perception.question16$quarter==i,], x = ~factor(hcp.major), y = ~percentage, type = 'scatter', mode = 'lines', color = ~answers) %>%
  layout(title = '对斯皮仁诺推广活动接受情况',
         xaxis = list(title = ''),
         yaxis = list (title = ''))




## part2

##identify progressed doctors
x <- newdata %>%
  ungroup() %>%
  spread(quarter,hcp.major) %>%
  select(`2016 Q3`,`2016 Q4`,`2017 Q1`,`2017 Q2`) %>%
  t(.) %>%
  na.locf(.) %>%
  ifelse(is.na(.),1,.) %>%
  diff(.) %>%
  t(.)

colnames(newdata)

y <- newdata %>%
  ungroup() %>%
  spread(quarter,hcp.major) %>%
  select(doctorid,department,region,doctor.tier) %>%
  cbind(.,x) %>%
  gather(quarter,value,-c(doctorid,department,region,doctor.tier)) %>%
  data.frame(.,'type'='progress') %>%
  do(plyr::rbind.fill(.,data.frame(doctorid=newdata$doctorid,
                                   department=newdata$department,
                                   region=newdata$region,
                                   doctor.tier=newdata$doctor.tier,
                                   quarter=newdata$quarter,
                                   value=1,
                                   type='all')))
  

## p15 每季度进阶医生的总体及在某一特定维度中的变化情况
#View(newdata)


## number of progressed doctors in every month
pro<-function(data){
  length(which(data>0))
}

doc.quarter.advanced<-y %>% group_by(quarter,type) %>%
  dplyr::summarise(counts=pro(value)) %>%
  spread(type,counts) %>% 
  mutate(percentage=round(no.pro/counts*100,0))


## picture1
p15_first <- plot_ly(doc.quarter.advanced,x = ~quarter, y = ~progress, name = "进阶医生数", type = "bar") %>%
  add_trace(y = ~percentage, name = '进阶医生比例',mode='lines',type='scatter',yaxis = "y2") %>%
  layout(
    title = "每季度的总体观念进阶医生",
    yaxis2 = list(
      tickfont = list(color = "red"),
      overlaying = "y",
      side = "right",
      title = ""),
    xaxis = list(title="")
  )



## p16 观念进阶医生的分布情况
## Quarter i 
i<-'2016 Q4'
## Picture 1 大区等级
doc.quarter.region.advanced <- y %>% 
  group_by(quarter,type,region) %>%
  summarise(counts=pro(value)) %>%
  do(plyr::rbind.fill(., data_frame(quarter=first(.$quarter),
                                    region = c("全国"),
                                    counts=sum(.$counts),
                                    type=first(.$type)))) %>%
  spread(type,counts) %>%
  mutate(not.progress=all-progress)%>%
  gather(type,value,progress,not.progress)
  



p16_first <- plot_ly(doc.quarter.region.advanced[doc.quarter.region.advanced$quarter==i,], x = ~region, y = ~value,color=~type, type = 'bar') %>%
  layout(title = "大区分布",  showlegend = T,
         xaxis = list(title = "",showgrid = T, zeroline = T, showticklabels = T),
         yaxis = list(title = "",showgrid = T, zeroline = T, showticklabels = T), 
         barmode = 'stack')

## picture 3 医生等级分布
doc.quarter.tier.advanced <- y %>% 
  group_by(quarter,type,doctor.tier) %>%
  summarise(counts=pro(value)) %>%
  do(plyr::rbind.fill(., data_frame(quarter=first(.$quarter),
                                    doctor.tier = c("所有医生"),
                                    counts=sum(.$counts),
                                    type=first(.$type)))) %>%
  spread(type,counts) %>%
  mutate(not.progress=all-progress)%>%
  gather(type,value,progress,not.progress)

p16_third <- plot_ly(doc.quarter.tier.advanced[doc.quarter.tier.advanced$quarter==i,], x = ~doctor.tier, y = ~value,color=~type, type = 'bar') %>%
  layout(title = "医生等级",  showlegend = T,
         xaxis = list(title = "",showgrid = T, zeroline = T, showticklabels = T),
         yaxis = list(title = "",showgrid = T, zeroline = T, showticklabels = T), 
         barmode = 'stack')

## Picture 4 科室分布
doc.quarter.department.advanced <- y %>% 
  group_by(quarter,type,department) %>%
  summarise(counts=pro(value)) %>%
  do(plyr::rbind.fill(., data_frame(quarter=first(.$quarter),
                                    department = c("全部科室"),
                                    counts=sum(.$counts),
                                    type=first(.$type)))) %>%
  spread(type,counts) %>%
  mutate(not.progress=all-progress)%>%
  gather(type,value,progress,not.progress)

p16_fourth <- plot_ly(doc.quarter.department.advanced[doc.quarter.department.advanced$quarter==i,], x = ~department, y = ~value,color=~type, type = 'bar') %>%
  layout(title = "科室分布",  showlegend = T,
         xaxis = list(title = "",showgrid = T, zeroline = T, showticklabels = T),
         yaxis = list(title = "",showgrid = T, zeroline = T, showticklabels = T), 
         barmode = 'stack')




##p17
## Picture 1 观念进阶医生对斯皮仁诺优势的认可程度（Q15）
## Quarter i
##进阶医生
x1<-to_use_data %>%
  select(quarter,most.recent.modify.date,doctorid,questions,answers,hcp.major) %>%
  filter(substr(questions,1,3)=='Q15') %>% 
  arrange(doctorid,most.recent.modify.date) %>%
  group_by(doctorid,quarter) %>%
  filter(row_number()==n()) %>%
  group_by(quarter,hcp.major) %>%
  mutate(total=n()) %>%
  separate_rows(answers,sep=';') %>%
  ungroup() %>%
  select(quarter,doctorid,answers)

perception.advanced.question15<-y %>%
  group_by(quarter,type) %>%
  mutate(total=pro(value)) %>%
  ungroup() %>%
  left_join(.,x1,by=c('quarter','doctorid')) %>%
  filter(quarter!='2016 Q3',!is.na(answers)) %>%
  group_by(quarter,type,answers,total) %>%
  dplyr::summarise(counts=pro(value)) %>%
  mutate(percentage=round(counts/total*100,0))
  
p17_first <- plot_ly(perception.advanced.question15, x = ~percentage, y = ~answers, type = 'bar',color=~type, orientation = 'h') %>%
  layout(xaxis = list(title = "", tickangle = -45),
         yaxis = list(title = ""),
         margin = list(b = 100),
         barmode = 'group')


## Picture 2 观念进阶医生对斯皮仁诺推广活动的认可程度（Q16）
## quarter i
x2<-to_use_data %>%
  select(quarter,most.recent.modify.date,doctorid,questions,answers,hcp.major) %>%
  filter(substr(questions,1,3)=='Q16') %>% 
  arrange(doctorid,most.recent.modify.date) %>%
  group_by(doctorid,quarter) %>%
  filter(row_number()==n()) %>%
  group_by(quarter,hcp.major) %>%
  mutate(total=n()) %>%
  separate_rows(answers,sep=';') %>%
  ungroup() %>%
  select(quarter,doctorid,answers)

perception.advanced.question16<-y %>%
  group_by(quarter,type) %>%
  mutate(total=pro(value)) %>%
  ungroup() %>%
  left_join(.,x1,by=c('quarter','doctorid')) %>%
  filter(quarter!='2016 Q3',!is.na(answers)) %>%
  group_by(quarter,type,answers,total) %>%
  dplyr::summarise(counts=pro(value)) %>%
  mutate(percentage=round(counts/total*100,0))


p17_second <- plot_ly(perception.advanced.question16, x = ~percentage, y = ~answers, type = 'bar',color=~type, orientation = 'h') %>%
  layout(xaxis = list(title = "", tickangle = -45),
         yaxis = list(title = ""),
         margin = list(b = 100),
         barmode = 'group')




## part3 推广活动与受访医生
## p19
## Picture 1 不同观念级别的医生出席会议的平均次数
colnames(meeting_dat)
doc.meeting.info<- newdata %>%
  select(quarter,doctorid,hcp.major) %>%
  filter(substr(quarter,1,4)=='2017')%>%
  inner_join(meeting_dat,by=c('doctorid','quarter')) %>%
  ungroup() %>%
  group_by(quarter,imeeting.type,doctorid) %>%
  mutate(all.doc=n()) %>%
  ungroup() %>%
  group_by(quarter,hcp.major,imeeting.type,doctorid) %>%
  mutate(hcp.doc=n()) %>%
  group_by(quarter,hcp.major,imeeting.type) %>%
  mutate(hcp.average=mean(hcp.doc)) %>%
  group_by(quarter,imeeting.type) %>%
  mutate(all.average=mean(all.doc)) %>%
  ungroup() %>%
  select(quarter,imeeting.type,hcp.major,hcp.average,all.average) %>%
  distinct() %>%
  mutate(hcp.major=as.character(hcp.major)) %>%
  plyr::rbind.fill(.[,1:4],
                   data.frame(
                     'quarter' = .$quarter,
                     'hcp.major' = 'all',
                     'imeeting.type' = .$imeeting.type,
                     'hcp.average' = .$all.average
                   )) %>%
  select(quarter,hcp.major,imeeting.type,hcp.average) %>%
  distinct()
 

p19_first <- plot_ly(doc.meeting.info, x = ~imeeting.type, y = ~hcp.average, type = 'bar',color=~factor(hcp.major)) %>%
  layout(xaxis = list(title = "", tickangle = -45),
         yaxis = list(title = ""),
         margin = list(b = 100),
         barmode = 'group')


## Picture 2 不同观念医生接受拜访的平均次数
call_dat$doctorid<-as.character(call_dat$doctorid)
doc.call.info<- newdata %>%
  select(quarter,doctorid,hcp.major,region) %>%
  filter(substr(quarter,1,4)=='2017')%>%
  mutate(hcp.major=as.character(hcp.major)) %>%
  inner_join(call_dat,by=c('doctorid','quarter')) %>%
  group_by(quarter,region,hcp.major) %>%
  dplyr::summarise(each.total=n_distinct(doctorid),
                   all.total=n(),
                   average=all.total/each.total) %>%
  group_by(quarter, hcp.major) %>%
  do(plyr::rbind.fill(., data.frame(quarter = first(.$quarter),
                                    hcp.major = first(.$hcp.major),
                                    region = "Total",
                                    each.total = sum(.$each.total, na.rm = TRUE),
                                    all.total = sum(.$all.total, na.rm = TRUE),
                                    avg_call_cnt = sum(.$all.total, na.rm = TRUE) /
                                      sum(.$each.total, na.rm = TRUE))))
  
doc.call.info.hcp <- newdata %>%
  select(quarter,doctorid,region) %>%
  filter(substr(quarter,1,4)=='2017')%>%
  inner_join(call_dat,by=c('doctorid','quarter')) %>%
  group_by(Quarter, region) %>%
  summarise(each.total = sum(each.total),
            all.total = sum(all.total),
            average = all.total / each.total)

doc.call.info$hcp.major <- "All"
doc.call.info <- rbind(doc.call.info.hcp,
                                   doc.call.info)

p19_second <- plot_ly(doc.call.info, x = ~region, y = ~average, type = 'bar',color=~factor(hcp.major)) %>%
  layout(xaxis = list(title = "", tickangle = -45),
         yaxis = list(title = ""),
         margin = list(b = 100),
         barmode = 'group')


## p21 每季度观念进阶医生参与推广活动情况
## picture 1 每季度观念进阶医生出席会议的平均次
View(y)
adv.y<-y%>%
  select(doctorid,region,quarter,value,type) %>%
  filter(value>=1)
  
adv.doc.meeting.info<- adv.y %>%
  filter(substr(quarter,1,4)=='2017')%>%
  inner_join(meeting_dat,by=c('doctorid','quarter')) %>%
  group_by(quarter,imeeting.type,type) %>%
  dplyr::summarise(each.total=n_distinct(doctorid),
                   all.total=n(),
                   average=all.total/each.total) 
  

p21_first <- plot_ly(adv.doc.meeting.info[adv.doc.meeting.info$quarter==j,], x = ~imeeting.type, y = ~average, type = 'bar',color=~type) %>%
  layout(xaxis = list(title = "", tickangle = -45),
         yaxis = list(title = ""),
         margin = list(b = 100),
         barmode = 'group')

## picture 2 观念进阶医生接受拜访的平均次数（可用于各类维度）
adv.doc.call.info<- adv.y %>%
  filter(substr(quarter,1,4)=='2017')%>%
  inner_join(call_dat,by=c('doctorid','quarter')) %>%
  group_by(quarter,region,type) %>%
  dplyr::summarise(each.total=n_distinct(doctorid),
                   all.total=n(),
                   average=all.total/each.total) %>%
  group_by(quarter, type) %>%
  do(plyr::rbind.fill(., data.frame(quarter = first(.$quarter),
                                    type = first(.$type),
                                    region = "Total",
                                    each.total = sum(.$each.total, na.rm = TRUE),
                                    all.total = sum(.$all.total, na.rm = TRUE),
                                    average = sum(.$all.total, na.rm = TRUE) /
                                      sum(.$each.total, na.rm = TRUE))))

View(adv.doc.call.info)
p21_second <- plot_ly(adv.doc.call.info[adv.doc.call.info$quarter==j,], x = ~region, y = ~average, type = 'bar',color=~type) %>%
  layout(xaxis = list(title = "", tickangle = -45),
         yaxis = list(title = ""),
         margin = list(b = 100),
         barmode = 'group')






