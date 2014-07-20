
# This is the server logic for a Shiny web application.
# You can find out more about building applications with Shiny here:
# 
# http://www.rstudio.com/shiny/
#

library(shiny)
load('school_data.Rda')

school_data_clean <- merge(school_data[school_data$School.Code != 0,], entity_data)
agg_data <- aggregate(
    cbind(
      as.numeric(Percentage.At.Or.Above.Proficient) * Students.with.Scores/100, 
      as.numeric(Percentage.Advanced) * Students.with.Scores/100, 
      as.numeric(Students.with.Scores) 
      )  
        ~ School.Name + District.Name + Grade + Test.Id, school_data_clean, FUN=sum)
names(agg_data)[5] <- 'Proficient'
names(agg_data)[6] <- 'Advanced'
names(agg_data)[7] <- 'Students.with.Scores'
agg_data$Test.Id <- as.numeric(agg_data$Test.Id)

shinyServer(function(input, output) {
  
  output$district <- reactive({paste("District ID:", input$district)})
  selectedSchools <- reactive({
    
    if (input$grade != 0){
      agg_data <- agg_data[agg_data$Grade == input$grade,]
    }
    if (input$test != 0){
      agg_data <- agg_data[agg_data$Test.Id == input$test,]
    }
    agg_data <- aggregate(
      cbind(Proficient, Advanced, Students.with.Scores)  
          ~ School.Name + District.Name, agg_data, FUN=sum)
    
    agg_data <- agg_data[agg_data$District.Name == input$district, ]
    names(agg_data)[3] <- 'Proficient'
    names(agg_data)[4] <- 'Advanced'
    names(agg_data)[5] <- 'Students.with.Scores'
    agg_data$Percent.Proficient <- agg_data$Proficient *100 / agg_data$Students.with.Scores
    agg_data$Percent.Advanced <- agg_data$Advanced *100 / agg_data$Students.with.Scores
    agg_data
    })
  output$numscores <- reactive({sum(selectedSchools()$Students.with.Scores)})
  output$numproficient <- reactive({sum(selectedSchools()$Proficient)})
  
  output$distPlot <- renderPlot({
    
    # generate bins based on input$bins from ui.R

    
    district <- input$district
    
    selected_data <- selectedSchools()
    
    if (length(selected_data$School.Name) == 0){
      return()
    }
    
    bins <- input$bins
    # draw the histogram with the specified number of bins
    hist(selected_data$Percent.Proficient , 
         breaks = bins, col = 'darkgray', border = 'white', 
         xlab="Percent of students proficient", ylab="Number of Schools",
         main="Distribution of Star Test Proficiency")
    
    
  })
  output$schoolTable <- renderTable({
    selected_data <- selectedSchools()
    if (length(selected_data$School.Name) == 0){
      return(data.frame())
    }
    selected_data[
      order(-selected_data$Percent.Proficient),c('School.Name', 'Percent.Proficient','Percent.Advanced', 'Students.with.Scores')]
    })
  
})
