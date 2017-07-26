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
##-- formating data
## p10
p10_tmp1<-eda_dat[,c(5,7)] ##8,9,13
p10_tmp2<-data.frame('year'=year(p10_tmp1$most.recent.modify.date),
                     'month'=month(p10_tmp1$most.recent.modify.date),
                     'date'=as.yearmon(p10_tmp1$most.recent.modify.date),
                     'doctor.id'=p10_tmp1$doctorid)
tbl_df(p10_tmp2)
x<-p10_tmp2 %>% 
  group_by(date) %>%
  summarise(counts=n_distinct(doctor.id))
x<-data.frame(x,difference=c(0,diff(x$counts)))

p <- ggplot(data=x,aes(x=factor(date),y=counts,group=F,colour='skyblue'))
p <- p + geom_line() +  geom_point() 
p <- p + geom_bar(aes(y=difference),stat="identity",colour=F,fill='skyblue',alpha=0.5)
p <- p + scale_y_continuous(sec.axis = sec_axis(~.*1, name = "increase"))
p <- p + scale_colour_manual(values = c("blue", "red"))
p <- p + labs(y = "Counts",
              x = "Date")
p



