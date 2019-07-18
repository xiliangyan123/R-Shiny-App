library(shiny)
library(shinydashboard)
library(readr)
library(dplyr)
library(DT)
library(ggplot2)

setwd('C:/Users/xiliangyan123/Desktop/School/ST558/Project 3/Project3')

data <- read_csv("Dataset.csv") 
pdata <- data %>% filter(Year==2010:2015 & Cause!="All causes" & State!="United States")

shinyServer(function(input, output, session) {
    
    url <- a("My github page", href="https://github.com/xiliangyan123/R-Shiny-App")
    urldata <- a("Dataset", href="https://catalog.data.gov/dataset/age-adjusted-death-rates-for-the-top-10-leading-causes-of-death-united-states-2013")
    
    #Create URLs for dataset and github page. 
    output$datatab <- renderUI({
        tagList("Here you can find the dataset:", urldata)
    })
    output$tab <- renderUI({
        tagList("Here you can see my code:", url)
    })
    
    getData <- reactive({
        newData <- pdata %>% filter(pdata$Cause == input$causes)
    })
    
    getData1 <- reactive({
        newData <- pdata %>% filter(pdata$Cause == input$cause)
    })
    
    #Dynamic UI that enables user to reset to default settings
    observe({
         if(input$check){
             updateSliderInput(session, "size", min=1, max=10, value=1)
             updateTextInput(session, "info")
         }
         else
             updateSliderInput(session, "size", min=1, max=10, value=5)
     })
    
    #create plot
    output$dataplot <- renderPlot({
        
        #get filtered data
        newData <- getData()
        
        #create plot
        g <- ggplot(newData, aes(x=Deaths, y=AADR)) 
        g + geom_point(size=input$size, aes(col=input$causes))
    })

    #creating the dynamic titles for our exploration page
    output$title <- renderUI({
        text <- HTML("Exploring Data <br> <br> Scatterplot of Cause:",
                       input$causes)
    })
    
    #create ability for user to click on plot
    output$info <- renderText({
        paste0("x=", input$plot_click$x, "\ny=", input$plot_click$y)
    })
    
    #create our numeric summaries
    output$text <- renderText({
         #get filtered data
         newData <- getData1()
         paste("The average deaths for cause", input$causes, 
               "is", round(mean(newData$Deaths, na.rm = TRUE), 2), 
               "and the average age-adjusted death rate is", 
               round(mean(newData$AADR, na.rm = TRUE), 2), sep = " ")
     })
     
    #create output of observations for data table page
    output$Tables <- renderTable({
        newdata <- getData1() %>% head(n=10)
    })
    
    #create output of observations for exploration page
    output$table <- renderTable({
        newdata <- getData() %>% head(n=10)
        print(newdata)
    })
    
    #Make a button that allows us to download a csv file of the data
    output$DownloadData <- downloadHandler(
        filename = function(){
        paste(input$cause, ".csv", sep="")
    },
    content = function(file){
        write.csv(getData(), file, row.names = FALSE)
    }
    )
})
