
# This is the user-interface definition of a Shiny web application.
# You can find out more about building applications with Shiny here:
# 
# http://www.rstudio.com/shiny/
#

library(shiny)

load('school_data.Rda')

entity_data_clean <- entity_data[entity_data$School.Code != 0,]
district_df <- data.frame(
    District.Code=entity_data_clean$District.Code, 
    District.Name=as.character(entity_data_clean$District.Name))

district.choices <- sort(unique(as.character(district_df$District.Name)))

#grade_df <- unique(school_data$Grade)
grade.choices <- c(0,seq(2,11), 13)
names(grade.choices) <- c("All", seq(2,11), "End of Course")

test.choices <- c(0, tests$Test.ID.Num)
names(test.choices) <- c("All", lapply(tests$Test.Name ,FUN = function(st) { substr(st, 0, 30)} ))
lausd_index <- district.choices[grep("Los Angeles Unified", names(district.choices))][1]
shinyUI(pageWithSidebar(
  
  # Application title
  headerPanel("California Schools"),
  
  # Sidebar with a slider input for number of bins
  sidebarPanel(
    
    selectInput("district", label="District:", choices = district.choices),
    selectInput("grade", label="Grade:", choices = grade.choices),
    selectInput("test", label="Test:", choices = test.choices),
    sliderInput("bins",
                "Number of bins:",
                min = 1,
                max = 50,
                value = 30),
    h3("Getting Started"),
    p(" "),
    p(paste("Being by selecting the school district of interest, such as the \"Los Angeles Unified\" ",
            "or \"San Franciso Unified\". From there, schools are placed in order by the percentage of",
            "tests with a \"proficient\" score or higher. If data from only a particular grade level or test",
            "is desired, select that as well, and the data displayed will only reflect that grade or test."))
  ),
  
  # Show a plot of the generated distribution
  mainPanel(
    h3("Measured by 2013 Standardized Test Results"),
    textOutput("district"),
    plotOutput("distPlot"),
    tableOutput("schoolTable")
  )
))
