#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(tidyverse)
library(ggplot2)
library(tidyr)
library(magrittr)
library(dplyr)
library(readr)
library(knitr)
library(ggvis)
library(ggthemes)
library(ggpubr)


beer = read.csv('Beers.csv')
brewery = read.csv('Breweries.csv')
both = read.csv('bs.csv')
both = both %>% filter(!is.na(IBU)) %>% filter(!is.na(ABV))
states = c('All States',levels(both$State))
bcb = read.csv('brew_cnt_bar.csv')

# Define UI for application that draws a histogram
ui <- fluidPage(
    #add tab title
    navbarPage(title="Interactie Beer Study"),
    # Application title
    titlePanel(title = h4("Beer Study", align = 'center')),
    
    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            #add drop down for data set selection
            selectInput(inputId = 'dataset',
                        label = 'Choose a Dataset:',
                        choices = c('Beer','Brewery','Both'),
                        selected = 'Both'),
            
            
            #add drop down for number of rows to show in header
            numericInput(inputId = "obs",
                         label = "Number of observations to view:",
                         value = 5),
            
            selectInput(inputId = 'var',
                        label = 'Choose Variable to Plot in Histogram:',
                        choices = c('ABV' = 7,'IBU' = 8)),
            
            sliderInput(inputId = "bins",
                        label = "Number of bins in Histogram:",
                        min = 1,
                        max = 50,
                        value = 30),
            
            radioButtons(inputId = 'plot',
                         label = 'Choose Plot Type:',
                         choices = c('Histogram','Box','Scatterplot','Bar'),
                         selected = 'Histogram'
                        ),
    
            
            selectInput(inputId = 'statefilter',
                        label = 'State Filter:',
                        choices = sort(both$State),
                        multiple = TRUE
                
                        ),
            
            #input for regression line filter
            selectInput(inputId = 'regression',
                        label = 'Add Regression line to Scatterplot?',
                        choices = c('Yes','No'),
                        selected = 'No')
            
            
           
            
        ),
        
        # Show a plot of the generated distribution
        mainPanel(
            
            tabsetPanel(type = 'tabs',
                    
                        tabPanel('Data View',tableOutput('view')),
                        tabPanel('Summary View',verbatimTextOutput('summary')),
                        tabPanel('Plot',plotOutput(outputId = 'plot'))
                
            )
        
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
    
    #return requested dataset
    datasetInput = reactive({
        switch(input$dataset,
               "Beer" = beer,
               "Brewery" = brewery,
               "Both" = both)
        
        
    })
    
    #return requested column
    columnInput = reactive({
        switch(input$var,
               "ABV" = both[,7],
               "IBU" = both[,8])
        
        
    })
    
    output$summary = renderPrint({
        
        dataset = datasetInput()
        summary(dataset)
    })
    
    output$view <- renderTable({
        
        head(datasetInput(), n = input$obs)
    })
    

    
    output$plot = renderPlot({
        colm = as.numeric(input$var)
       df = if(isTruthy(input$statefilter)) {
           temp = both %>% filter(State %in% input$statefilter)
       } else if(!isTruthy(input$statefilter)) {
            temp = both
       }
        if (input$plot == 'Histogram'){
            hist(
                df[,colm],
                breaks = seq(min(df[,colm]),max(df[,colm]), l = input$bins + 1),
                main = 'Histogram of Beer Data Set',
                xlab = names(df[colm]),
                col = 'blue'
            )
        } else if (input$plot == 'Box') {
            ggplot(data = df, aes(ABV,Region))+
                geom_boxplot(aes(fill = Region))
        } else if (input$plot == 'Scatterplot'){
            if (input$regression == 'Yes'){
                ggplot(data = df, aes(ABV,IBU, color = Region)) +
                    geom_point(position = 'jitter')+
                    geom_smooth(method = 'lm')+
                    stat_cor(method="pearson", label.x = 0,label.y = 130)+
                    ggtitle('Correlation of IBU and IPA in Beer by Region')+
                    facet_wrap(~Region)+
                    theme_minimal()
            } else {
                ggplot(data = df, aes(ABV,IBU, color = Region)) +
                    geom_point(position = 'jitter')+
                    ggtitle('Correlation of IBU and IPA in Beer by Region')+
                    facet_wrap(~Region)+
                    theme_minimal()
            }
        } else {
            brew_cnt_bar %>%
                ggplot(aes(Brewery_Count,reorder(State,Brewery_Count),fill = Region)) +
                geom_col()+
                ggtitle('Number of Breweries in Each State')+
                xlab('Number of Breweries')+
                ylab('State')+
                geom_text(aes(label=Brewery_Count),hjust=1)
        }
            
    
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
