## library packages
library(shiny)
ui<-navbarPage("Descriptive Data Analysis",
               tabPanel("受访医生概况",
                        sidebarLayout(
                          sidebarPanel({}),
                          mainPanel({})
                        )
                        plotOutput('p1first','')),
               tabPanel("医生观念进阶情况",),
               tabPanel("推广活动和受访医生",),
               tabPanel("推广活动对观念进阶医生的影响",))
server<-function(input,output,session){
  
}