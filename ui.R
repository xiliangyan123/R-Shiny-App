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

# Define UI for application that draws a histogram
dashboardPage(skin = "blue",
dashboardHeader(title = "Welcome to my App", titleWidth = "250"),
dashboardSidebar(
    sidebarMenu(
        menuItem("Info Page", tabName = "Information", icon = icon("dashboard")),
        menuItem("Data Page", tabName = "Data", icon=icon('th')),
        menuItem("Data Exploration", tabName = "Exploration", icon=icon("th")),
        menuItem("Unsupervised Learning", tabName = "Clustering", icon=icon('th')),
        menuItem("Modeling Page", tabName = "Models", icon=icon('th'))
    )
),

dashboardBody(
    tabItems(
        tabItem(tabName = "Information", em(h2("Describing the App and Data", style="color:blue")),
                box(background = "red", width=12, h2("The data that we have consists of the top 10 leading causes of death in the United States since 1999.")),
                box(background = "green", width=12, h2("This App enables the user interact with different widgets that allows them to make graphical plots and summaries, apply unsupervised learning methods, incorporate modeling techniques, and more.")), 
                box(background = "yellow", width=12, h2("Some useful links!"), uiOutput("tab"), uiOutput("datatab"))), 
        
        tabItem(tabName = "Clustering", strong(em(h2("Unsupervised Learning"))),
                fluidRow(box(selectizeInput("PCs", "Choose the principal components", choices = names(pdata[c(1, 3, 4)])))),
                fluidRow(box(plotOutput("pc")))),
        
        tabItem(tabName = "Models", strong(em(h2("Data Modeling"))), 
                box(title="About", solidHeader = TRUE, collapsible = TRUE, collapsed = TRUE, width=10, background = "green", h2("Here, we want to predict the value of the Age Adjusted Death Rate according to some predictor values - Year and Cause"), 
                    withMathJax(), helpText(h4('An example of a simple linear reg. model- $$y = b_1x_1 + e$$')), helpText(h4('An example of a multiple linear reg. model - $$y = b_1x_1 + b_2x_2 + b_3x_1x_2 + e$$'))),
                selectizeInput("slrmodel", "Simple Linear Reg. Model - choose the explanatory variable to visualize relation to the death rate", names(pdata)[c(1,3,4)]),
                plotOutput("plot1"),
                sliderInput("Deaths", "Predict Death Rate by Number of Deaths", min=1000, max=50000, value=10000), 
                tableOutput("mtable"),
                selectizeInput("rfmodel", "Choose the rf predictors", choices=c("Deaths and Year", "Deaths and Cause", "Year and Cause")),
                tableOutput("modelcomp")),
        
        tabItem(tabName = "Data", strong(em(h2("Data Tables"))),
                fluidRow(box(title="About this app", solidHeader = TRUE, collapsible = TRUE, collapsed = TRUE, width = 10, background="green",
                    h5("In this app, we see the data grouped by cause of death from 2010 to 2015. The purpose of this is to compare the different causes and their data in deaths, deathrate, and more.
                       You can also view different causes in the drop down menu or click the button the view the full dataset for the cause of death. Feel free to play around!")),
                selectizeInput("cause", "Causes", choices = levels(as.factor(pdata$Cause))),
                downloadButton("DownloadData", "Download", "Data"),
                    dataTableOutput("Tables"))),
        
        tabItem(tabName = "Exploration", h2(uiOutput("title")),
                box(title="About", solidHeader = TRUE, collapsible = TRUE, collapsed = TRUE, width=10, background = "green", 
                    h5("In this app, we are looking at filtered data for all 50 States across from 2010 to 2015. By selecting the 
                       from the causes of death in the dropdown menu, we can view how the number of deaths have impacted the death rate in the five year interval. We can visualize
                       this in a plot, we can visualize the average of the deaths compared to the death rate, and we can also see a subset of our data of interest in a table.")),
                selectizeInput("causes", "Causes", choices = levels(as.factor(pdata$Cause))),
                fluidPage(column(width=2,plotOutput("dataplot", click = "plot_click", width = "500", height = "350px"))),
                downloadButton("DownloadPlot", "Download Plot"),
                fluidRow(column(4, sliderInput("size", "Size of Points on Graph",
                                               min = 1, max = 10, value = 3, step = 1))),
                fluidRow(checkboxInput("check", "Reset All Entries")),
                fluidRow(column(width=10, verbatimTextOutput("info")),
                box(h2("Numeric Summary of Data"), textOutput("text")),
                box(dataTableOutput("table"), title="Click to see table of plot data", solidHeader = TRUE, collapsible = TRUE, collapsed = TRUE)))
))
)
