library(webshot)
library(ReporteRs)
library(magrittr)


roll_up<-function(data){
  # data = allin$page11
  mydoc<-addSlide(mydoc,slide.layout = data$layout)
  n<-data$number_of_dataset
  i=0
  #p<-rep(1,n)
  while (i<n){
    i=i+1
    tmp_dat <- as.data.frame(get(data$dataset_names[i]),
                             stringsAsFactors = FALSE)
    
    if ((data$plot_type)[i]=='bar'){
      p<-plot_ly(x = tmp_dat[, data[[i+3]]$x],
                 y = tmp_dat[, data[[i+3]]$y], 
                 type = 'bar', 
                 # text = text,
                 marker = list(color = 'rgb(158,202,225)',
                               line = list(color = 'rgb(8,48,107)', 
                                           width = 1.5))) %>%
        layout(title = data[[i+3]]$title_plot,
               xaxis = list(title = data[[i+3]]$x_axis),
               yaxis = list(title = data[[i+3]]$y_axis),
               annotations = list(x = tmp_dat[, data[[i+3]]$x],
                                  y = tmp_dat[, data[[i+3]]$y],
                                  text = tmp_dat[, data[[i+3]]$y],
                                  xanchor = 'center',
                                  yanchor = 'bottom',
                                  showarrow = FALSE),
               autosize = T,
               margin =  list(
                 l = 100,
                 r = 50,
                 b = 100,
                 t = 100,
                 pad = 4
               ))
    } else if (data$plot_type[i]=='pie') {
      p <- plot_ly(labels = tmp_dat[, data[[i+3]]$x],
                   values = tmp_dat[, data[[i+3]]$y]) %>%
        add_pie(hole = 0.6) %>%
        layout(title = data[[i+3]]$title,  showlegend = F,
               xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
               yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE)
               )
    } else if (data$plot_type[i]=="line"){
      p<-plot_ly(x = tmp_dat[, data[[i+3]]$x],
                 y = tmp_dat[, data[[i+3]]$y],
                 type = "scatter",
                 mode = 'linesmarker',
                 linetype = tmp_dat[, data[[i+3]]$linetype],
                 color = tmp_dat[, data[[i+3]]$color]) %>%
        layout(title = data[[i+3]]$title_plot,
               xaxis = list(title = data[[i+3]]$x_axis),
               yaxis = list(title = data[[i+3]]$y_axis),
               autosize = T,
               margin =  list(
                 l = 100,
                 r = 50,
                 b = 100,
                 t = 100,
                 pad = 4
               ))
    } else if (data$plot_type[i]=="stacked bar") {
      p<-plot_ly(x = tmp_dat[, data[[i+3]]$x],
                 y = tmp_dat[, data[[i+3]]$y],
                type = "bar",
                color = tmp_dat[, data[[i+3]]$color]) %>%
        layout(title = data[[i+3]]$title_plot,
               xaxis = list(title = data[[i+3]]$x_axis),
               yaxis = list(title = data[[i+3]]$y_axis),
               barmode = "stack",
               autosize = T,
               margin =  list(
                 l = 100,
                 r = 50,
                 b = 100,
                 t = 100,
                 pad = 4
               ))
    } else if (data$plot_type[i]=="bar + line") {
      p<-plot_ly(x = tmp_dat[, data[[i+3]]$x1],
                 y = tmp_dat[, data[[i+3]]$y1],
                 type = "bar",
                 name = data[[i+3]]$name1) %>%
        add_trace(x = tmp_dat[, data[[i+3]]$x2],
                  y = tmp_dat[, data[[i+3]]$y2],
                  type = "scatter",
                  mode = "lines",
                  yaxis = "y2",
                  name = data[[i+3]]$name2) %>%
        layout(yaxis2 = list(name = data[[i+3]]$y2_axis,
                             overlaying = "y",
                             side = "right"),
               yaxis = list(name = data[[i+3]]$y1_axis),
               xaxis = list(name = data[[i+3]]$x_axis),
               title = data[[i+3]]$title_plot,
               autosize = T,
               margin =  list(
                 l = 100,
                 r = 50,
                 b = 100,
                 t = 100,
                 pad = 4
               ))
    } else if (data$plot_type[i]=="horizantal bar"){
      p<-plot_ly(x = tmp_dat[, data[[i+3]]$x],
                 y = tmp_dat[, data[[i+3]]$y],
                 type = "bar",
                 color = tmp_dat[, data[[i+3]]$color],
                 orientation = "h") %>%
        layout(title = data[[i+3]]$title_plot,
               xaxis = list(title = data[[i+3]]$x_axis),
               yaxis = list(title = data[[i+3]]$y_axis),
               barmode = "group",
               autosize = T,
               margin =  list(
                 l = 250,
                 r = 50,
                 b = 100,
                 t = 100,
                 pad = 4
               ))
    } else if (data$plot_type[i]=="multiple bar") {
      p<-plot_ly(x = tmp_dat[, data[[i+3]]$x],
                 y = tmp_dat[, data[[i+3]]$y],
                 type = "bar",
                 color = tmp_dat[, data[[i+3]]$color]) %>%
        layout(title = data[[i+3]]$title_plot,
               xaxis = list(title = data[[i+3]]$x_axis),
               yaxis = list(title = data[[i+3]]$y_axis),
               barmode = "group",
               autosize = T,
               margin =  list(
                 l = 100,
                 r = 50,
                 b = 150,
                 t = 100,
                 pad = 4
               ))
    }
    export(p, paste(data$dataset_names[i],'png',sep='.'))
    mydoc<-addImage(doc=mydoc,filename=paste(data$dataset_names[i],'png',sep='.'))
  }
}

setwd("D:/Rachel/WorkMaterial/Poject/Xi'an Jassen PET/test")
mydoc<-pptx(title = "title",template = "Xi'an.pptx")

lapply(allin,roll_up)

writeDoc(mydoc,file='firsttry.pptx')














######
doc_cnt_qtr$Quarter <- paste(doc_cnt_qtr$Year, doc_cnt_qtr$Quarter, sep = "_")
doc_cnt_qtr$count_change <- doc_cnt_qtr$doc_cnt - lag(doc_cnt_qtr$doc_cnt, 1)


input=list(year="2017",
           quarter="Q1",
           item="A",
           field="tier",
           meeting_type="城市会 City Meeting",
           region="Total",
           level="A"
           )

tmp_doc_perception_score_tier_qtr <- 
  doc_perception_score_tier_qtr %>%
    filter(doctor.tier == input$level) %>%
    mutate(Quarter = paste(Year, Quarter, sep = "_"))


tmp_doc_region_dist_qtr <- 
  doc_region_dist_qtr %>%
    filter(Year == input$year, Quarter == input$quarter)


tmp_doc_level_dist_qtr <- 
  doc_tier_dist_qtr %>%
    filter(Year == input$year, Quarter == input$quarter) %>%
    mutate(Quarter = paste(Year, Quarter, sep = "_"))


tmp_doc_department_dist_qtr <- 
  doc_department_dist_qtr %>%
    filter(Year == input$year, Quarter == input$quarter) %>%
    mutate(Quarter = paste(Year, Quarter, sep = "_"))

tmp_doc_perception_score_region_qtr <- 
  doc_perception_score_region_qtr %>%
    filter(Year == input$year,
           Quarter == input$quarter)


tmp1_doc_perception_score_tier_qtr <- 
  doc_perception_score_tier_qtr %>%
    filter(Year == input$year,
           Quarter == input$quarter)


tmp_doc_perception_score_department_qtr <- 
  doc_perception_score_department_qtr %>%
    filter(Year == input$year,
           Quarter == input$quarter)

tmp_eda_dat_15_q <- 
  eda_dat_15_q %>%
    filter(Year == input$year,
           Quarter == input$quarter) %>%
    mutate(Quarter = paste(Year, Quarter, sep = "_"))


tmp_eda_dat_16_q <- 
  eda_dat_16_q %>%
    filter(Year == input$year,
           Quarter == input$quarter) %>%
    mutate(Quarter = paste(Year, Quarter, sep = "_"))



  tmp <- get(paste("smmy_psc_qtr_smmy_", input$field, sep = ""))
  tmp1 <- unique(unlist(tmp[, 1]))
  smmy_psc_qtr_smmy_field <- ({ if (is.null(input$item)) {
    tmp
  } else {
    tmp[unlist(tmp[, 1]) == input$item, ]
  }
  })
  
  tmp_smmy_psc_qtr_smmy_region <-
    smmy_psc_qtr_smmy_region %>%
      group_by(Quarter) %>%
      do(plyr::rbind.fill(., data.frame(Quarter = first(.$Quarter),
                                        region = "Total",
                                        adv = sum(.$adv, na.rm = TRUE),
                                        total = sum(.$total, na.rm = TRUE)))) %>%
      ungroup(Quarter) %>%
      mutate(Year = substr(Quarter, 1, 4),
             Quarter = substr(Quarter, 6, 7),
             adv_ratio = adv / total,
             `Physican with Progression` = adv_ratio) %>%
      filter(Year == input$year,
             Quarter == input$quarter) %>%
      mutate(`No Change` = 1 - `Physican with Progression`) %>%
      mutate(Quarter = paste(Year, Quarter, sep = "")) %>%
      select(-total, -adv, -Year, -adv_ratio) %>%
      gather(item, ratio, -region, -Quarter)
 
  
  tmp_smmy_psc_qtr_smmy_department <- 
    smmy_psc_qtr_smmy_department %>%
      group_by(Quarter) %>%
      do(plyr::rbind.fill(., data.frame(Quarter = first(.$Quarter),
                                        department = "Total",
                                        adv = sum(.$adv, na.rm = TRUE),
                                        total = sum(.$total, na.rm = TRUE)))) %>%
      ungroup() %>%
      mutate(Year = substr(Quarter, 1, 4),
             Quarter = substr(Quarter, 6, 7),
             adv_ratio = adv / total,
             `Physican with Progression` = adv_ratio) %>%
      filter(Year == input$year,
             Quarter == input$quarter) %>%
      mutate(`No Change` = 1 - `Physican with Progression`) %>%
      mutate(Quarter = paste(Year, Quarter, sep = "")) %>%
      select(-total, -adv, -Year, -adv_ratio) %>%
      gather(item, ratio, -department, -Quarter)
  
  
  
    tmp <- smmy_psc_qtr_smmy_tier %>%
      group_by(Quarter) %>%
      do(plyr::rbind.fill(., data.frame(Quarter = first(.$Quarter),
                                        doctor.tier = "Total",
                                        adv = sum(.$adv, na.rm = TRUE),
                                        total = sum(.$total, na.rm = TRUE)))) %>%
      ungroup() %>%
      mutate(Year = substr(Quarter, 1, 4),
             Quarter = substr(Quarter, 6, 7),
             adv_ratio = adv / total,
             `Physican with Progression` = adv_ratio) %>%
      filter(Year == input$year,
             Quarter == input$quarter) %>%
      mutate(`No Change` = 1 - `Physican with Progression`) %>%
      mutate(Quarter = paste(Year, Quarter, sep = "")) %>%
      select(-total, -adv, -Year, -adv_ratio) %>%
      gather(item, ratio, -doctor.tier, -Quarter)
    
    tmp$doctor.tier <- factor(tmp$doctor.tier, 
                              levels = c("Total", "A", "B", "C", "U", NA))
    
    tmp_smmy_psc_qtr_smmy_tier <-tmp
    
    tmp_eda_dat_15_q_adv <- 
      eda_dat_15_q_adv %>%
        filter(Year == input$year,
               Quarter == input$quarter) %>%
        group_by(Year, Quarter, answers, doc_cnt_total) %>%
        do(plyr::rbind.fill(., data.frame(Year = first(.$Year),
                                          Quarter = first(.$Quarter),
                                          answers = first(.$answers),
                                          adv_flag = 2,
                                          doc_cnt_total = first(.$doc_cnt_total),
                                          doc_cnt = sum(.$doc_cnt)
        ))) %>%
        filter(!is.na(adv_flag)) %>%
        mutate(flag = ifelse(adv_flag == 1, "Physicians with Progress",
                             "Overall Physicians"),
               doc_cnt_pct = ifelse(adv_flag == 1, doc_cnt / doc_cnt_total_adv,
                                    doc_cnt / doc_cnt_total))
    
    
    tmp_eda_dat_16_q_adv <- 
      eda_dat_16_q_adv %>%
        filter(Year == input$year,
               Quarter == input$quarter) %>%
        group_by(Year, Quarter, answers, doc_cnt_total) %>%
        do(plyr::rbind.fill(., data.frame(Year = first(.$Year),
                                          Quarter = first(.$Quarter),
                                          answers = first(.$answers),
                                          adv_flag = 2,
                                          doc_cnt_total = first(.$doc_cnt_total),
                                          doc_cnt = sum(.$doc_cnt)
        ))) %>%
        filter(!is.na(adv_flag)) %>%
        mutate(flag = ifelse(adv_flag == 1, "Physicians with Progress",
                             "Overall Physicians"),
               doc_cnt_pct = ifelse(adv_flag == 1, doc_cnt / doc_cnt_total_adv,
                                    doc_cnt / doc_cnt_total))
    
    
    tmp_eda_dat_tgt_with_meeting_all <- 
      eda_dat_tgt_with_meeting_all %>%
        filter(Year == input$year,
               Quarter == input$quarter)
    
    
    tmp_eda_dat_tgt_with_call_all <- 
      eda_dat_tgt_with_call_all %>%
        filter(Year == input$year,
               Quarter == input$quarter)
    
    
    tmp1_eda_dat_tgt_with_meeting_all <- 
      eda_dat_tgt_with_meeting_all %>%
        ungroup() %>%
        mutate(Quarter = paste(Year, Quarter, sep = "")) %>%
        filter(imeeting.type == input$meeting_type)
    
    
    tmp1_eda_dat_tgt_with_call_all <- 
      eda_dat_tgt_with_call_all %>%
        ungroup() %>%
        mutate(Quarter = paste(Year, Quarter, sep = "")) %>%
        filter(region == input$region)
    
    tmp_eda_dat_tgt_with_meeting_adv_all <- 
      eda_dat_tgt_with_meeting_adv_all %>%
        filter(Year == input$year,
               Quarter == input$quarter)
   
    
    tmp_eda_dat_tgt_with_call_adv_all <- 
      eda_dat_tgt_with_call_adv_all %>%
        filter(Year == input$year,
               Quarter == input$quarter)
    
    tmp1_eda_dat_tgt_with_meeting_adv_all <- 
      eda_dat_tgt_with_meeting_adv_all %>%
        ungroup() %>%
        mutate(Quarter = paste(Year, Quarter, sep = "")) %>%
        filter(imeeting.type == input$meeting_type)
    
    tmp1_eda_dat_tgt_with_call_adv_all <- 
      eda_dat_tgt_with_call_adv_all %>%
        ungroup() %>%
        mutate(Quarter = paste(Year, Quarter, sep = "")) %>%
        filter(region == input$region)
    
    
  
