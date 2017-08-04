library(webshot)
library(ReporteRs)
library(magrittr)




allin<-list(page10 = list(number_of_dataset=2,
                          plot_type='bar',
                          dataset_names=c('doc.general.quarter','doc.general.quarter'),
                          dataset_1=c('quarter','difference','受访医生总数及每季度新增人数'),
                          dataset_2=c('quarter','counts','受访医生总数及每季度新增人数'),
                          layout='two charts horizantal'),
            page11 = list(number_of_dataset=3,
                          plot_type='donut pie',
                          dataset_names=c("doc.region.quarter[doc.region.quarter$quarter=='2017 Q1',]",
                                          "doc.tier.quarter[doc.tier.quarter$quarter=='2017 Q1',]",
                                          "doc.department.quarter[doc.department.quarter$quarter=='2017 Q1',]"),
                          dataset_1=c('region','counts','大区分布'),
                          dataset_2=c('doctor.tier','counts','医生等级分布'),
                          dataset_3=c('department','counts','科室分布'),
                          layout="four charts"))
            


roll_up<-function(data){
  mydoc<-addSlide(mydoc,slide.layout = data$layout)
  n<-data$number_of_dataset
  i=0
  x<-rep(1,n)
  y<-rep(1,n)
  ti<-rep(1,n)
  p<-rep(1,n)
  while (i<n){
    i=i+1
    x[i]<-(data[i+3])[1]
    y[i]<-(data[i+3])[2]
    ti[i]<-(data[i+3])[3]
    if (data$plot_type=='bar'){
        p[i]<-(plot_ly(data.frame(data$dataset_names[i]),x = ~x[i], y = ~y[i],  type = "bar") %>%
          layout(
            title = ti[i],
            yaxis2 = list(
              tickfont = list(color = "red"),
              overlaying = "y",
              side = "right",
              title = ""),
            xaxis = list(title=""))) 
        } else if (data$plot_type=='donut pie') {
        p[i]<-(plot_ly(data.frame(data$dataset_names[i]),labels = ~x[i], values = ~y[i]) +
        add_pie(hole = 0.6) +
        layout(title = ti[i],  showlegend = T,
               xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
               yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE)))
            }
    export(p[i],paste(data$dataset_names[i],'pptx',sep='.'))
   mydoc<-addImage(doc=mydoc,filename=paste(data$dataset_names[i],'pptx',sep='.'))
  }
  }
  
    

mydoc<-pptx(title = "title",template = "Xi'an.pptx")

lapply(allin,roll_up)

