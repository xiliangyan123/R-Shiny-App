#load in packages
library(shiny)
library(shinydashboard)
library(readr)
library(dplyr)
library(tidyverse)
library(devtools)
library(DT)

#read in dataset
data <- as_tibble(read_csv("Dataset.csv"))
pdata <- data %>% filter(Year==2010:2015 & Cause!="All causes" & State!="United States")

#Create the basis for our dashboard
dashboardPage(skin = "blue",
dashboardHeader(title = "Welcome to my App", titleWidth = "250"),
dashboardSidebar(
    sidebarMenu(
        menuItem("Info Page", tabName = "Information", icon = icon("dashboard")),
        menuItem("Data Page", tabName = "Data", icon=icon('th')),
        menuItem("Data Exploration", tabName = "Exploration", icon=icon("th")),
        menuItem("Unsupervised Learning", tabName = "PCAnalysis", icon=icon('th')),
        menuItem("Modeling Page", tabName = "Models", icon=icon('th'))
    )
),

dashboardBody(
    tabItems(
        tabItem(tabName = "Information", em(h2("Describing the App and Data", style="color:blue")),
                box(background = "red", width=12, 
                h4("This dataset presents the age-adjusted death rates 
                    for the 10 leading causes of death in the United States
                    beginning in 1999. Data are based on information from all resident
                    death certificates filed in the 50 states and the District of Columbia 
                    using demographic and medical characteristics. 
                    Age-adjusted death rates (per 100,000 population) are based on the 2000 
                    U.S. standard population. For the app purposes, we will filter out this data to only consider 
                    years between 2010 to 2015. ")),
                box(background = "green", width=12, h3("This App enables the user interact with different widgets that allows them to make graphical plots and summaries, apply unsupervised learning methods, incorporate modeling techniques, and more.")), 
                box(background = "yellow", width=12, h3("Some useful links!"), uiOutput("tab"), uiOutput("datatab"))), 
        
        tabItem(tabName = "PCAnalysis", strong(em(h2("Unsupervised Learning"))),
                fluidRow(
                box(background="green", width=10, collapsible = TRUE, collapsed=TRUE, 
                title="About PCA", h4("Principal Component Analysis, also known as PCA, is a dimension reduction technique. 
                                        Dimension reduction is often applied where our main goal is where we have a large dataset of 
                                        variables and values, and we only want to focus on a subset of those parameters. This allows us 
                                        to focus on fewer relationships between variables and allows us to not overfit (over complicated) the model of data.
                                        In our case here, we have three principal components - the Year, the # of deaths, and the death rate. 
                                        You can select either the year and deaths as the component and it will provide a biplot of scattered points, based on the cause of death. 
                                        This will help visualize the relationship of the variables of year and death rate, and Deaths and death rate.")),
                
                selectizeInput("PCs", "Choose the principal components", choices = names(pdata[c(1,4)])),
                
                selectizeInput("cauze", "Look to see biplot based on causes", choices = levels(as.factor(pdata$Cause))),
                uiOutput("biplottext"),
                br(), 
                box(title = "BiPlot", width = 6, plotOutput("Biplot")))),
        
        tabItem(tabName = "Models", strong(em(h2("Data Modeling"))), 
                
                box(title="About", h4("We will plot 2 types of supervised models: a simple linear regression model and a multiple linear regression model. 
                                      For a simple linear regression model, note that we have one predictor value that helps us estimate a response. 
                                      To help us determine whether the predictor is useful, we can generally look at the R-squared value, which is a measure of correlation. 
                                      For multiple linear regression, we have more than one predictor value, or we can have a combination of multiple values as well. The R-square here 
                                      represents the relationship between our predictors and their impact on the response. Below you can see examples of the models written in math form."), 
                solidHeader = TRUE, collapsible = TRUE, collapsed = TRUE, width=10, background = "green", 
                withMathJax(), helpText(h4('An example of a simple linear reg. model - 
                                           $$y = b_1x_1 + e$$')), 
                helpText(h4('An example of a multiple linear reg. model - 
                                $$y = b_1x_1 + b_2x_2 + b_3x_1x_2 + e$$'))),
                
                selectizeInput("slrmodel", "Simple Linear Reg. Model - choose the explanatory variable to visualize relation to the death rate", names(pdata)[c(1,4)]),
                uiOutput("modeltext"),
                br(),
                plotOutput("plot1"),
                
                sliderInput("Deaths", "Predict Death Rate by Number of Deaths", min=1000, max=50000, value=10000), 
                
                tableOutput("mtable"),
                
                radioButtons("mlrpreds", "Choose the mlr predictors", 
                             choices=c("Effect of Deaths and Cause on AADR", 
                                       "Effect of Deaths and Year on AADR", 
                                       "Effect of Cause and Year on AADR")),
                
                tableOutput("mlrmodel")),
        
        tabItem(tabName = "Data", strong(em(h2("Data Tables"))),
                
                fluidRow(box(title="About this app", solidHeader = TRUE, collapsible = TRUE, collapsed = TRUE, width = 10, background="green",
                    h5("In this app, we see the data grouped by cause of death from 2010 to 2015. The purpose of this is to compare the different causes and their data in deaths, deathrate, and more.
                       You can also view different causes in the drop down menu or click the button the view the full dataset for the cause of death. Feel free to play around!")),
                
                selectizeInput("cause", "Causes", choices = levels(as.factor(pdata$Cause))),
                downloadButton("DownloadData", "Download", "Data"),
                hr(), 
                dataTableOutput("Tables"))),
        
        tabItem(tabName = "Exploration", h2(uiOutput("title")),
                column(width=5, 
                    box(title="About", solidHeader = TRUE, collapsible = TRUE, collapsed = TRUE, width=10, background = "green", 
                    h5("In this app, we are looking at filtered data for all 50 States across from 2010 to 2015. By selecting the 
                       from the causes of death in the dropdown menu, we can view how the number of deaths have impacted the death rate in the five year interval. We can visualize
                       this in a plot, we can visualize the average of the deaths compared to the death rate, and we can also see a subset of our data of interest in a table."))),
                
                fluidRow(
                selectizeInput("causes", "Causes", choices = levels(as.factor(pdata$Cause))),
                
                plotOutput("dataplot", click = "plot_click", width = "500", height = "350px"),
                br(), 
                downloadButton("DownloadPlot", "Download Plot"),
                hr(), 
                sliderInput("size", "Size of Points on Graph",
                                       min = 1, max = 10, value = 3, step = 1),
                
                checkboxInput("check", "Reset All Entries"),
                
                verbatimTextOutput("info"),
                
                box(h2("Numeric Summary of Data"), textOutput("text")),
                
                box(dataTableOutput("table"), title="Click to see table of plot data", solidHeader = TRUE, collapsible = TRUE, collapsed = TRUE))
)
)
)
)
