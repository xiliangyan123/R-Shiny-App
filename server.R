#load in packages
library(shiny)
library(shinydashboard)
library(readr)
library(devtools)
library(dplyr)
library(DT)
library(ggplot2)
library(caret)
library(ggfortify)

#Load in our data and filter by year
data <- read_csv("Dataset.csv") 
pdata <- data %>% filter(Year==2010:2015 & Cause!="All causes" & State!="United States")

#Set up URLs 
url <- a("My github page", href="https://github.com/xiliangyan123/R-Shiny-App")
urldata <- a("Dataset", href="https://catalog.data.gov/dataset/age-adjusted-death-rates-for-the-top-10-leading-causes-of-death-united-states-2013")

shinyServer(function(input, output, session){
    
    #Link URLs to the dataset and personal github page. 
    output$datatab <- renderUI({
        tagList("Here you can find the dataset:", urldata)
    })
    output$tab <- renderUI({
        tagList("Here you can see visit my github page to see code:", url)
    })
    
    #Get our Filtered Data dynamically. 
    getData <- reactive({
        newData <- pdata %>% filter(pdata$Cause == input$causes)
    })
    getData1 <- reactive({
        newData <- pdata %>% filter(pdata$Cause == input$cause)
    })
    getData2 <- reactive({
        newData <- pdata %>% filter(pdata$Cause == input$cauze)
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
    
    #Make a reactive plot. 
    plotInput <- reactive({
        #get filtered data
        newData <- getData()
        
        #create plot
        g <- ggplot(newData, aes(x=Deaths, y=AADR)) 
        g + geom_point(size=input$size, aes(col=input$causes))
    })
    
    #Make the plot downloadable to a png file. 
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
    
    output$biplottext <- renderUI({
        if(input$PCs=="Year"){
            paste0("Here we look at the linear combination of the AADR 
                   and the Year variables for the cause: ", input$cauze,". This biplot shows 
                   us how strongly the principal components, Year and AADR, influence each other, and to what extent,
                   shown by the arrows and the points. Here we can see that the Year somewhat influences the AADR
                   as shown by how long the line or vector is for both components. However, it can be difficult to tell as well, 
                   since the points are scattered in a odd-looking pattern.")
        }
        else if(input$PCs=="Deaths"){
            paste0("Here we look at the linear combination of 2 principal components: AADR and the 
                   Deaths. The cause of death that we are looking at is: ", input$cauze,". This biplot 
                   shows us how strongly the principal components influence each other and to what extent, 
                   as shown by the arrows and the points. Here we can see that the Deaths component greatly influences the 
                   AADR, as the number of deaths, intuitively, tells us how the death rate will be. The line or vector that is pointing out, 
                   indicates a strong relationship, which can also be shown in the direction of the points. ")
        }
    })
    
    #Create text for model output
    output$modeltext <- renderUI({
        if(input$slrmodel=="Year"){
            paste0("It looks like that as we have the Year to measure our AADR, we visualize an interesting 
                   regression plot. We can view the frequency or # of occurrences with how the levels of AADR
                   that we have. For example, for 2015, we have AADR relatively spread out, with the higher levels
                   having many occurrences 150 and 250 for the AADR. As for the previous years, we see that they are 
                   more concentrated around a smaller interval such as 150 and 200. This may seem to indicate that as year
                   grows, the death rate may be variable and highly unpredictable given the top 10 causes of death in the U.S.")
        }
        
        else if(input$slrmodel=="Deaths"){
            paste0("It looks like the number of deaths appears to be a great predictor of the death rate, as expected. 
                   Since the Death rate resembles the number of deaths from a population given a certain time frame, the 
                   deaths that we have seem to be correlated with our death rate. ")
        }
    })
    
    #Get our numeric summaries
    output$text <- renderText({
         #get filtered data
         newData <- getData()
         paste("The average deaths for cause", input$causes, 
               "is", round(mean(newData$Deaths, na.rm = TRUE), 2), 
               "and the average age-adjusted death rate is", 
               round(mean(newData$AADR, na.rm = TRUE), 2), sep = " ")
    })
     
    #create table of output observations for data table page
    output$Tables <- renderDataTable({
        newdata <- getData1()
    })
    
    #create table of output observations for exploration page
    output$table <- renderDataTable({
        newdata <- getData() 
        print(newdata)
    })
    
    #Make a button that downloads a csv file of the data
    output$DownloadData <- downloadHandler(
        filename = function(){
        paste(input$cause, ".csv", sep="")
    },
    content = function(file){
        write.csv(getData(), file, row.names = FALSE)
    }
    )
    
    #Begin making dynamic user interface for choosing models 
    ModelData <- reactive({
        pdata[, c(input$slrmodel, "AADR")]
    })
    
    #1st supervised learning model
    #Create simple models to choose from
    model1 <- lm(AADR ~ Deaths, data=pdata)
    model2 <- lm(AADR ~ Year, data=pdata)
    model3 <- lm(AADR ~ State, data=pdata)
    sum_mod1 <- summary(model1)
    
    output$plot1 <- renderPlot({
        par(mar=c(5.1, 4.1, 0, 1))
        plot(ModelData())
        if(input$slrmodel=='Deaths'){
            abline(a=sum_mod1$coefficients[1,1], b=sum_mod1$coefficients[2,1])
            }
        if(input$slrmodel=='Year'){abline(model2)}
        if(input$slrmodel=='State'){abline(model3)}
    })
    #Create Biplots
    output$Biplot <- renderPlot({
     
     newdata <- getData2()
     dapcs <- prcomp(select(newdata, AADR, Deaths))
     yapcs <- prcomp(select(newdata, AADR, Year))
     
    if(input$PCs=="Deaths"){
         biplot(dapcs, xlabs=rep(".", nrow(newdata)), cex=1.8, col="Green")
    }
    
    else if(input$PCs=="Year"){
         biplot(yapcs, xlabs=rep(".", nrow(newdata)), cex=1.8, col="Blue")
    }
    })
    
    #Predict our death rate based on # of deaths. Used a sliderinput. 
    output$mtable <- renderTable({
        modeldata <- data.frame(Deaths = pdata$Deaths, AADR = pdata$AADR)
        model_lm <- lm(AADR ~ Deaths, data=modeldata)
        ddata <- data.frame(Deaths=input$Deaths)
        preddata <- predict(model_lm, ddata)
        
    })
    
    #Make the models for our MLR models. 
    model4 <- lm(AADR ~ Deaths + Cause, data=pdata)
    model5 <- lm(AADR ~ Deaths + Year, data=pdata)
    model6 <- lm(AADR ~ Cause + Year, data=pdata)
    
    #Summaries to get our coefficients of R-Squared. 
    sum_mod4 <- summary(model4)
    sum_mod5 <- summary(model5)
    sum_mod6 <- summary(model6)
    
    #Second supervised learning model - MLR
    output$mlrmodel<- renderTable({
        if(input$mlrpreds=="Effect of Deaths and Cause on AADR"){
            paste("R-squared: ", sum_mod4$r.squared, "As this is a multiple linear reg. model, we interpret the R-squared value as how the cause and year predictors are related.",
                   "As the R-squared is high, we can conclude that the # of Deaths and Cause are highly related in measuring the death rate.")
        }
        else if(input$mlrpreds=="Effect of Deaths and Year on AADR"){
            paste("R-squared: ", sum_mod5$r.squared, "As this is a multiple linear reg. model, we interpret the R-squared value as how the cause and year predictors are related.",
                   "As the R-squared is low, we can conclude that the # of Deaths and Year aren't very related in measuring the death rate.")
        }
        else if(input$mlrpreds=="Effect of Cause and Year on AADR"){
            paste("R-squared: ", sum_mod6$r.squared, "As this is a multiple linear reg. model, we interpret the R-squared value as how the cause and year predictors are related.",
                  "As the R-squared is high, we can conclude that the cause and year are highly related in measuring the death rate.")
            
        }
    })
    
})
