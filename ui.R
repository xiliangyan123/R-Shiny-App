library(shiny)
library(shinydashboard)
library(readr)
library(dplyr)
library(tidyverse)

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
        
        tabItem(tabName = "Clustering", strong(em(h2("Unsupervised Learning")))),
        
        tabItem(tabName = "Models", strong(em(h2("Data Modeling"))), 
                box(title="About", solidHeader = TRUE, collapsible = TRUE, collapsed = TRUE, width=10, background = "green", h2("Here, we want to predict the value of the Age Adjusted Death Rate according to some predictor values - Year, State, and Cause")),
                selectizeInput("pred", "Explanatory Variables", choices = c("Year", "Cause", "State"))),
        
        tabItem(tabName = "Data", strong(em(h2("Data Tables"))),
                box(title="About this app", solidHeader = TRUE, collapsible = TRUE, collapsed = TRUE, width = 10, background="green",
                    h5("In this app, we see the data grouped by cause of death from 2010 to 2015. The purpose of this is to compare the different causes and their data in deaths, deathrate, and more.
                       You can also view different causes in the drop down menu or click the button the view the full dataset for the cause of death. Feel free to play around!")),
                selectizeInput("cause", "Causes", choices = levels(as.factor(pdata$Cause))),
                downloadButton("DownloadData", "Download Full Data"),
                box(title="Click to View", solidHeader = TRUE, collapsible = TRUE, collapsed = TRUE, width=10, background="green", 
                    tableOutput("Tables"))),
        
        tabItem(tabName = "Exploration", h2(uiOutput("title")),
                selectizeInput("causes", "Causes", choices = levels(as.factor(pdata$Cause))),
                plotOutput("dataplot", click = "plot_click", width = "400", height = "400px"),
                sliderInput("size", "Size of Points on Graph",
                            min = 1, max = 10, value = 5, step = 1),
                checkboxInput("check", "Reset All Entries"),
                verbatimTextOutput("info"),
                downloadButton("DownloadPlot", "Download Plot"),
                textOutput("text"),
                tableOutput("table"))
    ))
)
