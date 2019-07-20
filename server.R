library(shiny)
library(shinydashboard)
library(readr)
library(devtools)
library(dplyr)
library(DT)
library(ggplot2)

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
        tagList("Here you can see visit my github page to see code:", url)
    })
    
    #Get our Filtered Data
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
        newData <- getData() %>% filter(State=="Texas" | State=="California" | State=="Florida")
        
        #create plot
        g <- ggplot(newData, aes(x=Deaths, y=AADR)) 
        g + geom_point(size=input$size, aes(col=input$causes))
    })
    
    plotInput <- reactive({
        #get filtered data
        newData <- getData()
        
        #create plot
        g <- ggplot(newData, aes(x=Deaths, y=AADR)) 
        g + geom_point(size=input$size, aes(col=input$causes))
    })

    output$DownloadPlot <- downloadHandler(
        filename = function() { paste(input$causes, '.png', sep='') },
        content = function(file) {
            png(file)
            print(plotInput())
            dev.off()
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
        newdata <- getData1() 
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
    
    #Filter data for only 3 largest states in the U.S.
    filtdata <- pdata %>% dplyr::filter(State=="Texas" | State=="Florida" | State=="California")
    
    #Begin making dynamic user interface for choosing models 
    ModelData <- reactive({
        filtdata[, c("AADR", input$slrmodel)]
    })
    
    #Models to choose from
    model1 <- lm(AADR ~ Deaths, data=filtdata)
    model2 <- lm(AADR ~ Year, data=filtdata)
    model3 <- lm(AADR ~ State, data=filtdata)
    
    model4 <- lm(AADR ~ Deaths + Cause, data=filtdata)
    model5 <- lm(AADR ~ Deaths + State, data=filtdata)
    model6 <- lm(AADR ~ ., data = filtdata)
    
    output$plot1 <- renderPlot({
        par(mar=c(5.1, 4.1, 0, 1))
        plot(ModelData())
        if(input$slrmodel=='Deaths'){abline(model1)}
        if(input$slrmodel=='Year'){abline(model2)}
        if(input$slrmodel=='State'){abline(model3)}
    })

    dapcs <- prcomp(select(pdata, AADR, Deaths))
    yapcs <- prcomp(select(pdata, AADR, Year))
    
    output$pc <- renderPlot({
    if(input$PCs=="Deaths"){
        biplot(dapcs, xlabs=rep(".", nrow(pdata)), cex=1.5)
    }
    if(input$PCs=="Year"){
        biplot(yapcs, xlabs=rep(".", nrow(pdata)), cex=1.2)
    }
    })
    
    output$mtable <- renderTable(({
        head(ModelData(), 10)
    }))
    
    
    
})
