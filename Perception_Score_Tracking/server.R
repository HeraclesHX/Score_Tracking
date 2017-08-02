library(shiny)
library(shinythemes)
library(plotly)
library(dplyr)
library(shinydashboard)
library(tidyr)

load("./Data/.RData")
function(input, output, session) {
  
  observe({
    tmp_year <- unique(doc_region_dist_qtr$Year)
    tmp_quarter <- unique(doc_region_dist_qtr$Quarter)
    tmp_level <- as.character(unique(doc_perception_score_tier_qtr$doctor.tier))
    # tmp_region <- unique(eda_dat_tgt_with_call_all$region)
    tmp_region <- c("沪鲁苏皖", "京津冀黑辽吉", "粤桂琼", "浙闽湘鄂赣", "NULL", "Total")
    tmp_imeeting <- unique(eda_dat_tgt_with_meeting_all$imeeting.type)
    tmp_imeeting <- tmp_imeeting[!is.na(tmp_imeeting)]
    updateSelectizeInput(session,
                         'year',
                         choices = tmp_year[rank(tmp_year)], 
                         selected = "2016",
                         server = TRUE)
    updateSelectizeInput(session,
                         'quarter',
                         choices = tmp_quarter[rank( tmp_quarter)],
                         selected = "Q3",
                         server = TRUE)
    updateSelectizeInput(session,
                         'level',
                         choices = tmp_level,
                         selected = "A",
                         server = TRUE)
    updateSelectizeInput(session,
                         'year1',
                         choices = tmp_year[rank(tmp_year)], 
                         selected = "2016",
                         server = TRUE)
    updateSelectizeInput(session,
                         'quarter1',
                         choices = tmp_quarter[rank(tmp_quarter)],
                         selected = "Q3",
                         server = TRUE)
    updateSelectizeInput(session,
                         'year2',
                         choices = tmp_year[rank(tmp_year)], 
                         selected = "2016",
                         server = TRUE)
    updateSelectizeInput(session,
                         'quarter2',
                         choices = tmp_quarter[rank(tmp_quarter)],
                         selected = "Q3",
                         server = TRUE)
    
    updateSelectizeInput(session,
                         'year3',
                         choices = tmp_year[rank(tmp_year)], 
                         selected = "2016",
                         server = TRUE)
    updateSelectizeInput(session,
                         'quarter3',
                         choices = tmp_quarter[rank(tmp_quarter)],
                         selected = "Q4",
                         server = TRUE)
    
    updateSelectizeInput(session,
                         'year4',
                         choices = tmp_year[rank(tmp_year)], 
                         selected = "2016",
                         server = TRUE)
    
    updateSelectizeInput(session,
                         'quarter4',
                         choices = tmp_quarter[rank(tmp_quarter)],
                         selected = "Q4",
                         server = TRUE)
    
    updateSelectizeInput(session,
                         'year5',
                         choices = c("2017"), 
                         selected = "2017",
                         server = TRUE)
    
    updateSelectizeInput(session,
                         'quarter5',
                         choices = c("Q1", "Q2", "Q3", "Q4"),
                         selected = "Q1",
                         server = TRUE)
    
    updateSelectizeInput(session,
                         'region1',
                         choices = tmp_region,
                         selected = NULL,
                         server = TRUE)
    
    updateSelectizeInput(session,
                         'meeting_type',
                         choices = tmp_imeeting[rank(tmp_imeeting)],
                         selected = NULL,
                         server = TRUE)
    
    updateSelectizeInput(session,
                         'year6',
                         choices = c("2017"), 
                         selected = "2017",
                         server = TRUE)
    
    updateSelectizeInput(session,
                         'quarter6',
                         choices = c("Q1", "Q2", "Q3", "Q4"),
                         selected = "Q1",
                         server = TRUE)
    
    updateSelectizeInput(session,
                         'region2',
                         choices = tmp_region,
                         selected = NULL,
                         server = TRUE)
    
    updateSelectizeInput(session,
                         'meeting_type1',
                         choices = tmp_imeeting[rank(tmp_imeeting)],
                         selected = NULL,
                         server = TRUE)
    
  })
  
  ##- do with action link
  observeEvent(input$link_to_1.1, {
    newvalue <- "1.1 Tracking of Number of Survey Physicians"
    updateTabItems(session, "panels", newvalue)
  })
  
  observeEvent(input$link_to_1.2, {
    newvalue <- "1.2 Distributions of Survey Physicians"
    updateTabItems(session, "panels", newvalue)
  })
  
  observeEvent(input$link_to_1.3, {
    newvalue <- "1.3 Tracking of Physicians Score and Ratio"
    updateTabItems(session, "panels", newvalue)
  })
  
  observeEvent(input$link_to_1.4, {
    newvalue <- "1.4 Distributions of Overall Physicians Scores"
    updateTabItems(session, "panels", newvalue)
  })
  
  observeEvent(input$link_to_1.5, {
    newvalue <- "1.5 Attitude to Sporanox and Promotional Activities"
    updateTabItems(session, "panels", newvalue)
  })
  
  observeEvent(input$link_to_2.1, {
    newvalue <- "2.1 Perception Progression in Physician"
    updateTabItems(session, "panels", newvalue)
  })
  
  observeEvent(input$link_to_2.2, {
    newvalue <- "2.2 Distribution of Physician with Scores Progression Variation"
    updateTabItems(session, "panels", newvalue)
  })
  
  observeEvent(input$link_to_2.3, {
    newvalue <- "2.3 Attitude to Sporanox and Promotional Activities for Physicians with Score Progression"
    updateTabItems(session, "panels", newvalue)
  })
  
  observeEvent(input$link_to_3.1, {
    newvalue <- "3.1 Overview of Promotion Activities for Physicians at Different Level"
    updateTabItems(session, "panels", newvalue)
  })
  
  observeEvent(input$link_to_3.2, {
    newvalue <- "3.2 Tracking of Promotion Activities for Physicians at Different Level"
    updateTabItems(session, "panels", newvalue)
  })
  
  observeEvent(input$link_to_4.1, {
    newvalue <- "4.1 Relationship of Promotion Activities and Physician Perception Progression"
    updateTabItems(session, "panels", newvalue)
  })
  observeEvent(input$link_to_4.2, {
    newvalue <- "4.2 Tracking of Promotion Activities for Physicians with Progression and Overall"
    updateTabItems(session, "panels", newvalue)
  })
  
  #- back to home page actionlink
  observeEvent(input$link_to_home_page_1.1, {
    newvalue <- "HOME PAGE"
    updateTabItems(session, "panels", newvalue)
  })
  
  observeEvent(input$link_to_home_page_1.2, {
    newvalue <- "HOME PAGE"
    updateTabItems(session, "panels", newvalue)
  })
  
  observeEvent(input$link_to_home_page_1.3, {
    newvalue <- "HOME PAGE"
    updateTabItems(session, "panels", newvalue)
  })
  
  observeEvent(input$link_to_home_page_1.4, {
    newvalue <- "HOME PAGE"
    updateTabItems(session, "panels", newvalue)
  })
  
  observeEvent(input$link_to_home_page_1.5, {
    newvalue <- "HOME PAGE"
    updateTabItems(session, "panels", newvalue)
  })
  
  observeEvent(input$link_to_home_page_2.1, {
    newvalue <- "HOME PAGE"
    updateTabItems(session, "panels", newvalue)
  })
  
  
  observeEvent(input$link_to_home_page_2.2, {
    newvalue <- "HOME PAGE"
    updateTabItems(session, "panels", newvalue)
  })
  
  observeEvent(input$link_to_home_page_2.3, {
    newvalue <- "HOME PAGE"
    updateTabItems(session, "panels", newvalue)
  })
  
  observeEvent(input$link_to_home_page_3.1, {
    newvalue <- "HOME PAGE"
    updateTabItems(session, "panels", newvalue)
  })
  
  observeEvent(input$link_to_home_page_3.2, {
    newvalue <- "HOME PAGE"
    updateTabItems(session, "panels", newvalue)
  })
  
  observeEvent(input$link_to_home_page_4.1, {
    newvalue <- "HOME PAGE"
    updateTabItems(session, "panels", newvalue)
  })
  
  observeEvent(input$link_to_home_page_4.2, {
    newvalue <- "HOME PAGE"
    updateTabItems(session, "panels", newvalue)
  })
  
  ##- below charts are for Summary of Count of Physicians by Quarter
  output$total_doc_bar <- renderPlotly({
    doc_cnt_qtr$Quarter <- paste(doc_cnt_qtr$Year, doc_cnt_qtr$Quarter, sep = "_")
    
    
    p <- plot_ly(doc_cnt_qtr, x = ~ Quarter, y = ~ doc_cnt, 
                 type = 'bar', 
                 # text = text,
                 marker = list(color = 'rgb(158,202,225)',
                               line = list(color = 'rgb(8,48,107)', 
                                           width = 1.5))) %>%
      layout(title = "Total Physicians' Count by Quarter",
             xaxis = list(title = "Quarter"),
             yaxis = list(title = "Physician Count"),
             annotations = list(x = doc_cnt_qtr$Quarter,
                                y = doc_cnt_qtr$doc_cnt,
                                text = doc_cnt_qtr$doc_cnt,
                                xanchor = 'center',
                                yanchor = 'bottom',
                                showarrow = FALSE),
             autosize = T,
             margin =  list(
               l = 150,
               r = 150,
               b = 100,
               t = 100,
               pad = 4
             ))
    
    p
  })
  
  output$new_doc_bar <- renderPlotly({
    doc_cnt_qtr$Quarter <- paste(doc_cnt_qtr$Year, doc_cnt_qtr$Quarter, sep = "_")
    doc_cnt_qtr$count_change <- doc_cnt_qtr$doc_cnt - lag(doc_cnt_qtr$doc_cnt, 1)
    
    p <- plot_ly(doc_cnt_qtr, x = ~ Quarter, y = ~ count_change, 
                 type = 'bar', 
                 # text = text,
                 marker = list(color = 'rgb(158,202,225)',
                               line = list(color = 'rgb(8,48,107)', 
                                           width = 1.5))) %>%
      layout(title = "Counts Change of Physicians by Quarter",
             xaxis = list(title = "Quarter"),
             yaxis = list(title = "Change of Physician Count"),
             annotations = list(x = doc_cnt_qtr$Quarter,
                                y = doc_cnt_qtr$count_change,
                                text = doc_cnt_qtr$count_change,
                                xanchor = 'center',
                                yanchor = 'bottom',
                                showarrow = FALSE),
             autosize = T,
             margin =  list(
               l = 150,
               r = 150,
               b = 100,
               t = 100,
               pad = 4
             ))
    
    p
  })
  
  ##- below charts are for Distributions of Survey Physicians
  
  tmp_doc_region_dist_qtr <- reactive({
    doc_region_dist_qtr %>%
      filter(Year == input$year, Quarter == input$quarter)
  })
  
  tmp_doc_level_dist_qtr <- reactive({
    doc_tier_dist_qtr %>%
      filter(Year == input$year, Quarter == input$quarter) %>%
      mutate(Quarter = paste(Year, Quarter, sep = "_"))
  })
  
  tmp_doc_department_dist_qtr <- reactive({
    doc_department_dist_qtr %>%
      filter(Year == input$year, Quarter == input$quarter) %>%
      mutate(Quarter = paste(Year, Quarter, sep = "_"))
  })
  
  output$region_pie <- renderPlotly({
    plot_ly(tmp_doc_region_dist_qtr(), labels = ~region, values = ~doc_cnt) %>%
      add_pie(hole = 0.6) %>%
      layout(title = paste("Region Distribution of Physicians in ",
                           input$year, input$quarter, sep = ""),
             showlegend = F,
             xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
             yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
    
  })
  
  output$level_pie <- renderPlotly({
    plot_ly(tmp_doc_level_dist_qtr(), labels = ~doctor.tier, values = ~doc_cnt) %>%
      add_pie(hole = 0.6) %>%
      layout(title = paste("Level Distribution of Physicians in ", 
                           input$year, input$quarter, sep = ""),
             showlegend = F,
             xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
             yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
             autosize = T,
             margin =  list(
               l = 150,
               r = 150,
               b = 100,
               t = 100,
               pad = 4
             ))
    
  })
  
  output$department_pie <- renderPlotly({
    plot_ly(tmp_doc_department_dist_qtr(), labels = ~department, values = ~doc_cnt) %>%
      add_pie(hole = 0.6) %>%
      layout(title = paste("Department Distribution of Physicians in ", 
                           input$year, input$quarter, sep = ""),
             showlegend = F,
             xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
             yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
             autosize = T,
             margin =  list(
               l = 150,
               r = 150,
               b = 100,
               t = 100,
               pad = 4
             ))
    
  })
  
  ##- below charts are for tracking of physicians score and ratio
  tmp_doc_perception_score_tier_qtr <- reactive({
    doc_perception_score_tier_qtr %>%
      filter(doctor.tier == input$level) %>%
      mutate(Quarter = paste(Year, Quarter, sep = "_"))
  })
  
  output$doc_line <- renderPlotly({
    plot_ly(tmp_doc_perception_score_tier_qtr(),
            x = ~Quarter,
            y = ~doc_cnt.x,
            type = "scatter",
            mode = 'linesmarker',
            linetype = ~hcp.major,
            color = ~hcp.major) %>%
      layout(title = paste("Tracking of ", input$level, 
                           " Tier Physicians Scores by Quarter"),
             xaxis = list(title = 'Quarter'),
             yaxis = list(title = 'Physicians Count'),
             autosize = T,
             margin =  list(
               l = 150,
               r = 150,
               b = 100,
               t = 100,
               pad = 4
             ))
  })
  
  output$doc_cnt_stk_bar <- renderPlotly({
    plot_ly(tmp_doc_perception_score_tier_qtr(),
            x = ~Quarter,
            y = ~doc_cnt_pct,
            type = "bar",
            color = ~hcp.major) %>%
      layout(title = paste("Distribution of ", input$level, 
                           " Tier Physicians Scores by Quarter"),
             xaxis = list(title = 'Quarter'),
             yaxis = list(title = 'Percentage'),
             barmode = "stack",
             autosize = T,
             margin =  list(
               l = 150,
               r = 150,
               b = 100,
               t = 100,
               pad = 4
             ))
  })
  
  ##- below charts are for distribution of physicians score and ratio
  tmp_doc_perception_score_region_qtr <- reactive({
    doc_perception_score_region_qtr %>%
      filter(Year == input$year1,
             Quarter == input$quarter1)
  })
  
  tmp1_doc_perception_score_tier_qtr <- reactive({
    doc_perception_score_tier_qtr %>%
      filter(Year == input$year1,
             Quarter == input$quarter1)
  })
  
  tmp_doc_perception_score_department_qtr <- reactive({
    doc_perception_score_department_qtr %>%
      filter(Year == input$year1,
             Quarter == input$quarter1)
  })
  
  output$region_stk <- renderPlotly({
    plot_ly(tmp_doc_perception_score_region_qtr(),
            x = ~region,
            y = ~doc_cnt_pct,
            type = "bar",
            color = ~hcp.major) %>%
      layout(title = paste("Distribution of Physicians Scores by Region in ",
                           input$year1, input$quarter1, sep = ""),
             xaxis = list(title = 'Region'),
             yaxis = list(title = 'Percentage'),
             barmode = "stack",
             autosize = T,
             margin =  list(
               l = 150,
               r = 150,
               b = 100,
               t = 100,
               pad = 4
             ))
  })
  
  output$level_stk <- renderPlotly({
    plot_ly(tmp1_doc_perception_score_tier_qtr(),
            x = ~doctor.tier,
            y = ~doc_cnt_pct,
            type = "bar",
            color = ~hcp.major) %>%
      layout(title = paste("Distribution of Physicians Scores by Doctor Level in ",
                           input$year1, input$quarter1, sep = ""),
             xaxis = list(title = 'Doctor Level'),
             yaxis = list(title = 'Percentage'),
             barmode = "stack",
             autosize = T,
             margin =  list(
               l = 150,
               r = 150,
               b = 100,
               t = 100,
               pad = 4
             ))
  })
  
  output$department_stk <- renderPlotly({
    plot_ly(tmp_doc_perception_score_department_qtr(),
            x = ~department,
            y = ~doc_cnt_pct,
            type = "bar",
            color = ~hcp.major) %>%
      layout(title = paste("Distribution of Physicians Scores by Department in ",
                           input$year1, input$quarter1, sep = ""),
             xaxis = list(title = 'Department'),
             yaxis = list(title = 'Percentage'),
             barmode = "stack",
             autosize = T,
             margin =  list(
               l = 150,
               r = 150,
               b = 100,
               t = 100,
               pad = 4
             ))
  })
  
  ##- below charts are for Attitude to Sporanox and promotional activities for
  ##- all physicians (questionnaire Q15 &Q16)
  
  tmp_eda_dat_15_q <- reactive({
    eda_dat_15_q %>%
      filter(Year == input$year2,
             Quarter == input$quarter2) %>%
      mutate(Quarter = paste(Year, Quarter, sep = "_"))
  })
  
  tmp_eda_dat_16_q <- reactive({
    eda_dat_16_q %>%
      filter(Year == input$year2,
             Quarter == input$quarter2) %>%
      mutate(Quarter = paste(Year, Quarter, sep = "_"))
  })
  
  output$q15_line <- renderPlotly({
    plot_ly(tmp_eda_dat_15_q(),
            x = ~hcp.major,
            y = ~doc_cnt_pct,
            type = "scatter",
            mode = 'linesmarker',
            linetype = ~answers,
            color = ~answers) %>%
      layout(title = paste("Sporanox Advantages Accepted by Physician with Different Scores in ",
                           input$year2, input$quarter2, sep = ""),
             xaxis = list(title = 'HCP Major'),
             yaxis = list(title = 'Percentage'),
             autosize = T,
             margin =  list(
               l = 150,
               r = 150,
               b = 100,
               t = 100,
               pad = 4
             ))
  })
  
  output$q16_line <- renderPlotly({
    plot_ly(tmp_eda_dat_16_q(),
            x = ~hcp.major,
            y = ~doc_cnt_pct,
            type = "scatter",
            mode = 'linesmarker',
            linetype = ~answers,
            color = ~answers) %>%
      layout(title = paste("Sporanox Promotions Accepted by Physician with Different Scores in ",
                           input$year2, input$quarter2, sep = ""),
             xaxis = list(title = 'HCP Major'),
             yaxis = list(title = 'Percentage'),
             autosize = T,
             margin =  list(
               l = 150,
               r = 150,
               b = 100,
               t = 100,
               pad = 4
             ))
  })
  
  
  ##-- Part2 
  ##-- 2.1
  output$track_pb_overall <- renderPlotly({
    plot_ly(smmy_psc_qtr_smmy_nat, 
            x = ~Quarter,
            y = ~adv, 
            type = "bar", 
            name = "Counts of Physicians with Perception Progression") %>%
      add_trace(x = ~Quarter, 
                y = ~adv_ratio, 
                type = "scatter",
                mode = "lines",
                yaxis = "y2",
                name = "Percentage of Physicians with Perception Progression") %>%
      layout(yaxis2 = list(title = "Percentage",
                           overlaying = "y",
                           side = "right",
                           fill = "tozeroy"),
             yaxis = list(title = "Counts of Physicans with Perception Progression"),
             xaxis = list(name = "Quarter"),
             title = "Tracking of Physicans' Counts/Percentage with Perception Progression in Overall Physician",
             legend = list(x = 0,
                           y = 100,
                           orientation = "h"),
             autosize = T,
             margin =  list(
               l = 150,
               r = 150,
               b = 100,
               t = 100,
               pad = 4
             ))
  })
  
  observe({
    tmp <- get(paste("smmy_psc_qtr_smmy_", input$field, sep = ""))
    tmp1 <- unique(unlist(tmp[, 1]))
    updateSelectizeInput(session,
                         'item',
                         choices = tmp1[rank(tmp1)],
                         server = TRUE)
  })
  
  smmy_psc_qtr_smmy_field <- reactive({
    tmp <- get(paste("smmy_psc_qtr_smmy_", input$field, sep = ""))
    tmp1 <- unique(unlist(tmp[, 1]))
    if (is.null(input$item)) {
      tmp
    } else {
      tmp[unlist(tmp[, 1]) == input$item, ]
    }
  })
  
  output$pb_in_field <- renderPlotly({
    plot_ly(smmy_psc_qtr_smmy_field(), 
            x = ~Quarter,
            y = ~adv, 
            type = "bar", 
            name = "Counts of Physicians with Perception Progression") %>%
      add_trace(x = ~Quarter, 
                y = ~adv_ratio, 
                type = "scatter",
                mode = "lines",
                yaxis = "y2",
                name = "Percentage of Physicians with Perception Progression") %>%
      layout(yaxis2 = list(title = "Percentage",
                           overlaying = "y",
                           side = "right",
                           rangemode = "tozero"),
             yaxis = list(title = "Counts of Physicans with Perception Progression"),
             xaxis = list(name = "Quarter"),
             title = paste("Physicans' Counts/Percentage with Perception Progression in", input$field, input$item, sep = " "),
             legend = list(x = 0,
                           y = 100,
                           orientation = "h"),
             autosize = T,
             margin =  list(
               l = 150,
               r = 150,
               b = 100,
               t = 100,
               pad = 4
             ))
  })
  
  ##-- 2.2
  tmp_smmy_psc_qtr_smmy_region <- reactive({
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
      filter(Year == input$year3,
             Quarter == input$quarter3) %>%
      mutate(`No Change` = 1 - `Physican with Progression`) %>%
      mutate(Quarter = paste(Year, Quarter, sep = "")) %>%
      select(-total, -adv, -Year, -adv_ratio) %>%
      gather(item, ratio, -region, -Quarter)
  })
  
  tmp_smmy_psc_qtr_smmy_department <- reactive({
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
      filter(Year == input$year3,
             Quarter == input$quarter3) %>%
      mutate(`No Change` = 1 - `Physican with Progression`) %>%
      mutate(Quarter = paste(Year, Quarter, sep = "")) %>%
      select(-total, -adv, -Year, -adv_ratio) %>%
      gather(item, ratio, -department, -Quarter)
  })
  
  tmp_smmy_psc_qtr_smmy_tier <- reactive({
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
      filter(Year == input$year3,
             Quarter == input$quarter3) %>%
      mutate(`No Change` = 1 - `Physican with Progression`) %>%
      mutate(Quarter = paste(Year, Quarter, sep = "")) %>%
      select(-total, -adv, -Year, -adv_ratio) %>%
      gather(item, ratio, -doctor.tier, -Quarter)
    
    tmp$doctor.tier <- factor(tmp$doctor.tier, 
                              levels = c("Total", "A", "B", "C", "U", NA))
    
    tmp
  })
  
  
  output$region_stk_pb_var <- renderPlotly({
    plot_ly(tmp_smmy_psc_qtr_smmy_region(),
            x = ~region,
            y = ~ratio,
            type = "bar",
            color = ~item) %>%
      layout(title = paste("Distribution of Physicians Scores by Region in ",
                           input$year3, input$quarter3, sep = ""),
             xaxis = list(title = 'Region'),
             yaxis = list(title = 'Percentage'),
             barmode = "stack",
             autosize = T,
             margin =  list(
               l = 150,
               r = 150,
               b = 100,
               t = 100,
               pad = 4
             ))
  })
  
  output$department_stk_pb_var <- renderPlotly({
    plot_ly(tmp_smmy_psc_qtr_smmy_department(),
            x = ~department,
            y = ~ratio,
            type = "bar",
            color = ~item) %>%
      layout(title = paste("Distribution of Physicians Scores by Department in ",
                           input$year3, input$quarter3, sep = ""),
             xaxis = list(title = 'Department'),
             yaxis = list(title = 'Percentage'),
             barmode = "stack",
             autosize = T,
             margin =  list(
               l = 150,
               r = 150,
               b = 100,
               t = 100,
               pad = 4
             ))
  })
  
  output$tier_stk_pb_var <- renderPlotly({
    plot_ly(tmp_smmy_psc_qtr_smmy_tier(),
            x = ~doctor.tier,
            y = ~ratio,
            type = "bar",
            color = ~item) %>%
      layout(title = paste("Distribution of Physicians Scores by Doctor Level in ",
                           input$year3, input$quarter3, sep = ""),
             xaxis = list(title = 'Doctor Level'),
             yaxis = list(title = 'Percentage'),
             barmode = "stack",
             autosize = T,
             margin =  list(
               l = 150,
               r = 150,
               b = 100,
               t = 100,
               pad = 4
             ))
  })
  
  ##-- 2.3
  tmp_eda_dat_15_q_adv <- reactive({
    eda_dat_15_q_adv %>%
      filter(Year == input$year4,
             Quarter == input$quarter4) %>%
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
  })
  
  tmp_eda_dat_16_q_adv <- reactive({
    eda_dat_16_q_adv %>%
      filter(Year == input$year4,
             Quarter == input$quarter4) %>%
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
  })
  
  output$q15_bar_adv <- renderPlotly({
    plot_ly(tmp_eda_dat_15_q_adv(),
            x = ~doc_cnt_pct,
            y = ~answers,
            type = "bar",
            color = ~flag,
            orientation = "h") %>%
      layout(title = paste("Sporanox Advantages Accepted by Physicians with Progression ",
                           input$year4, input$quarter4, sep = ""),
             xaxis = list(title = 'Percentage'),
             yaxis = list(title = 'Attitude'),
             barmode = "group",
             autosize = T,
             margin =  list(
               l = 250,
               r = 150,
               b = 100,
               t = 100,
               pad = 4
             ))
  })
  
  output$q16_bar_adv <- renderPlotly({
    plot_ly(tmp_eda_dat_16_q_adv(),
            x = ~doc_cnt_pct,
            y = ~answers,
            type = "bar",
            color = ~flag,
            orientation = "h") %>%
      layout(title = paste("Sporanox Promotional Activities Accepted by Physicians with Progression ",
                           input$year4, input$quarter4, sep = ""),
             xaxis = list(title = 'Percentage'),
             yaxis = list(title = 'Promotional Activities'),
             barmode = "group",
             autosize = T,
             margin =  list(
               l = 250,
               r = 150,
               b = 100,
               t = 100,
               pad = 4
             ))
  })
  
  ##-- Part3
  ##-- 3.1
  tmp_eda_dat_tgt_with_meeting_all <- reactive({
    eda_dat_tgt_with_meeting_all %>%
      filter(Year == input$year5,
             Quarter == input$quarter5)
  })
  
  tmp_eda_dat_tgt_with_call_all <- reactive({
    eda_dat_tgt_with_call_all %>%
      filter(Year == input$year5,
             Quarter == input$quarter5)
  })
  
  output$meeting_bar_score <- renderPlotly({
    plot_ly(tmp_eda_dat_tgt_with_meeting_all(),
            x = ~imeeting.type,
            y = ~avg_meeting_cnt,
            type = "bar",
            color = ~hcp.major) %>%
      layout(title = paste("Average Meeting Counts for Physicians with Different Scores in ",
                           input$year5, input$quarter5, sep = ""),
             xaxis = list(title = 'Meeting Type'),
             yaxis = list(title = 'Average Meeting Counts'),
             barmode = "group",
             autosize = T,
             margin =  list(
               l = 150,
               r = 150,
               b = 150,
               t = 100,
               pad = 4
             ))
  })
  
  output$call_bar_score <- renderPlotly({
    plot_ly(tmp_eda_dat_tgt_with_call_all(),
            x = ~region,
            y = ~avg_call_cnt,
            type = "bar",
            color = ~hcp.major) %>%
      layout(title = paste("Average Call Counts for Physicians with Different Scores in ",
                           input$year5, input$quarter5, sep = ""),
             xaxis = list(title = 'Region'),
             yaxis = list(title = 'Average Call Counts'),
             barmode = "group",
             autosize = T,
             margin =  list(
               l = 150,
               r = 150,
               b = 150,
               t = 100,
               pad = 4
             ))
  })
  
  ##-- 3.2
  tmp1_eda_dat_tgt_with_meeting_all <- reactive({
    eda_dat_tgt_with_meeting_all %>%
      ungroup() %>%
      mutate(Quarter = paste(Year, Quarter, sep = "")) %>%
      filter(imeeting.type == input$meeting_type,
             !is.na(imeeting.type))
  })
  
  output$average_meeting_line <- renderPlotly({
    plot_ly(tmp1_eda_dat_tgt_with_meeting_all(),
            x = ~Quarter,
            y = ~avg_meeting_cnt,
            type = "scatter",
            mode = 'linesmarker',
            linetype = ~hcp.major,
            color = ~hcp.major) %>%
      layout(title = paste("Tracking of Average Meeting Count for Physicians with Different Scores"),
             xaxis = list(title = 'Quarter'),
             yaxis = list(title = 'Average Meeting Counts'),
             autosize = T,
             margin =  list(
               l = 150,
               r = 150,
               b = 50,
               t = 100,
               pad = 4
             ))
  })
  
  tmp1_eda_dat_tgt_with_call_all <- reactive({
    eda_dat_tgt_with_call_all %>%
      ungroup() %>%
      mutate(Quarter = paste(Year, Quarter, sep = "")) %>%
      filter(region == input$region1,
             Year != "2016")
  })
  
  output$average_call_line <- renderPlotly({
    plot_ly(tmp1_eda_dat_tgt_with_call_all(),
            x = ~Quarter,
            y = ~avg_call_cnt,
            type = "scatter",
            mode = 'linesmarker',
            linetype = ~hcp.major,
            color = ~hcp.major) %>%
      layout(title = paste("Tracking of Average Call Count for Physicians with Different Scores"),
             xaxis = list(title = 'Quarter'),
             yaxis = list(title = 'Average Call Counts'),
             autosize = T,
             margin =  list(
               l = 150,
               r = 150,
               b = 50,
               t = 100,
               pad = 4
             ))
  })
  
  ##-- Part4
  ##-- 4.1
  tmp_eda_dat_tgt_with_meeting_adv_all <- reactive({
    eda_dat_tgt_with_meeting_adv_all %>%
      filter(Year == input$year6,
             Quarter == input$quarter6)
  })
  
  tmp_eda_dat_tgt_with_call_adv_all <- reactive({
    eda_dat_tgt_with_call_adv_all %>%
      filter(Year == input$year6,
             Quarter == input$quarter6)
  })
  
  output$meeting_bar_adv <- renderPlotly({
    plot_ly(tmp_eda_dat_tgt_with_meeting_adv_all(),
            x = ~imeeting.type,
            y = ~avg_meeting_cnt,
            type = "bar",
            color = ~adv_flag) %>%
      layout(title = paste("Average Meeting Counts for Physicians with Perception Progression VS. Overall in ",
                           input$year6, input$quarter6, sep = ""),
             xaxis = list(title = 'Meeting Type'),
             yaxis = list(title = 'Average Meeting Counts'),
             barmode = "group",
             autosize = T,
             margin =  list(
               l = 150,
               r = 150,
               b = 150,
               t = 100,
               pad = 4
             ))
  })
  
  output$call_bar_adv <- renderPlotly({
    plot_ly(tmp_eda_dat_tgt_with_call_adv_all(),
            x = ~region,
            y = ~avg_call_cnt,
            type = "bar",
            color = ~adv_flag) %>%
      layout(title = paste("Average Call Number for Physicians with Perception Progression VS. Overall in ",
                           input$year6, input$quarter6, sep = ""),
             xaxis = list(title = 'Region'),
             yaxis = list(title = 'Average Call Counts'),
             barmode = "group",
             autosize = T,
             margin =  list(
               l = 150,
               r = 150,
               b = 150,
               t = 100,
               pad = 4
             ))
  })
  
  ##-- 4.2
  tmp1_eda_dat_tgt_with_meeting_adv_all <- reactive({
    eda_dat_tgt_with_meeting_adv_all %>%
      ungroup() %>%
      mutate(Quarter = paste(Year, Quarter, sep = "")) %>%
      filter(imeeting.type == input$meeting_type1,
             !is.na(imeeting.type))
  })
  
  output$average_meeting_line_adv <- renderPlotly({
    plot_ly(tmp1_eda_dat_tgt_with_meeting_adv_all(),
            x = ~Quarter,
            y = ~avg_meeting_cnt,
            type = "scatter",
            mode = 'linesmarker',
            linetype = ~adv_flag,
            color = ~adv_flag) %>%
      layout(title = paste("Tracking of Average Meeting Count for Physicians with Perception Progression VS. Overall"),
             xaxis = list(title = 'Quarter'),
             yaxis = list(title = 'Average Meeting Counts'),
             autosize = T,
             margin =  list(
               l = 150,
               r = 150,
               b = 150,
               t = 100,
               pad = 4
             ))
  })
  
  tmp1_eda_dat_tgt_with_call_adv_all <- reactive({
    eda_dat_tgt_with_call_adv_all %>%
      ungroup() %>%
      mutate(Quarter = paste(Year, Quarter, sep = "")) %>%
      filter(region == input$region2,
             Year != "2016")
  })
  
  output$average_call_line_adv <- renderPlotly({
    plot_ly(tmp1_eda_dat_tgt_with_call_adv_all(),
            x = ~Quarter,
            y = ~avg_call_cnt,
            type = "scatter",
            mode = 'linesmarker',
            linetype = ~adv_flag,
            color = ~adv_flag) %>%
      layout(title = paste("Tracking of Average Call Count for Physicians with Perception Progression VS. Overall"),
             xaxis = list(title = 'Quarter'),
             yaxis = list(title = 'Average Call Counts'),
             autosize = T,
             margin =  list(
               l = 150,
               r = 150,
               b = 150,
               t = 100,
               pad = 4
             ))
  })
}
