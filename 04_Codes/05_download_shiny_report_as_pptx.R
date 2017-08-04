library(shiny)
library(shiny)
library(DT)
library(ReporteRs)

app <- shinyApp(
  ui = 
    # Use a fluid Bootstrap layout
    fluidPage(    
      selectInput("sel",label = "col",choices = colnames(iris)[2:ncol(iris)]),
      downloadButton('downloadData', 'Download')
      
    ),
  
  server = function(input, output,session) {
    
    output$downloadData <- downloadHandler(
      filename = "file.pptx",
      content = function(file) {
        doc = pptx()
        
        # Slide 1 : Title slide
        #+++++++++++++++++++++++
        doc <- addSlide(doc, "Title Slide")
        doc <- addTitle(doc, "Create a PowerPoint document from R software")
        doc <- addSubtitle(doc, "R and ReporteRs package")
        
        # Slide 2 : Add Word Cloud
        #+++++++++++++++++++++++
        doc <- addSlide(doc, "Title and Content")
        doc <- addTitle(doc, "Bar Plot")
        words_per_tweet=iris
        pptwordcloud<-function(){
          barplot(table(words_per_tweet[,c("Sepal.Length",input$sel)]), 
                  border=NA,
                  main="Distribution of words per tweet", 
                  cex.main=1,
                  col="darkcyan")
        }
        doc <- addPlot(doc, fun= pptwordcloud,vector.graphic =FALSE ) 
        writeDoc(doc,file)
      }
    )
})

runApp(app)