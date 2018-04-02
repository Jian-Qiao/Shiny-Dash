library(shiny)
library(DT)
library(shinythemes)
shinyUI(
  navbarPage("Almod BI Dash Board",
             
     tags$head(
       tags$link(rel = "stylesheet", type = "text/css", href = "table.css")
     ),
    
    theme=shinytheme('united'),
                  
    tabPanel('Sales',
      tabsetPanel(
        tabPanel('Daily Sales Summary',
                 fluidRow(
                   column(5,
                   htmlOutput('Daily_Sales_text_1'),
                   htmlOutput('Daily_Sales_text_2')
                 ),
                   column(7,
                   htmlOutput('Daily_Sales_Graph_1'),
                   htmlOutput('Daily_Sales_Graph_2'),
                   hr(),
                  h3('by Group Style (Collection)'),
                  dataTableOutput('Daily_Sales_by_GroupStyle'),
                  hr(),
                  h3('by Region'),
                  dataTableOutput('Daily_Sales_by_Region')
                  )
                 )
                 ),
        tabPanel('Daily Sales Detail',
              fluidRow(
                column(4, align="center", selectInput('Feature_1','Choose feature',c('COL','KBA','FOM','LUCE'))),
                column(4, align="center", dateRangeInput('Date_1','Choose Start/End Date',start=Sys.Date()-1,end=Sys.Date()-1)),
                column(4, align="center", uiOutput('Diamond_Type_1'))
              ),
            div(
              DT::dataTableOutput('Daily_Sales_1'),
              style='font-size:70%'
            )
          ),
        tabPanel('Performance Summary')
      )
    ),
    
    tabPanel('Inventory',
      tabsetPanel(
        tabPanel('Summary',
          h2('Summary:'),
          p(textOutput('summary')),
          h3('Jewelry in Inventory'),
          div(
            tableOutput('Pivot_Jewelry_1'),
            tableOutput('Pivot_Jewelry_2'),
            tableOutput('Pivot_Jewelry_3'),
            tableOutput('Pivot_Jewelry_4'),
            tableOutput('Pivot_Jewelry_5'),
            style='width:100%'
          ),
          div(style='clear:both;'),
          h3('Loose Stone in Inventory'),
          div(
            tableOutput('Pivot_Loose_1'),
            tableOutput('Pivot_Loose_2'),
            tableOutput('Pivot_Loose_3'),
            tableOutput('Pivot_Loose_4'),
            tableOutput('Pivot_Loose_5'),
            style='width:100%'
          )
        ),
        
        tabPanel('Warehouses in New York',
          h2('Warehouses in New York'),
          div(
            tableOutput('Pivot_Loose_in_20_1'),
            tableOutput('Pivot_Loose_in_20_2'),
            tableOutput('Pivot_Loose_in_20_3'),
            tableOutput('Pivot_Loose_in_20_4'),
            tableOutput('Pivot_Loose_in_20_5'),
            style='width:100%'
          ),
          
          div(style='clear:both;'),
          
          div(style='clear:left',
          h2('Detail'),
          uiOutput('Store_Select_1'),
          shiny::dataTableOutput('Detail_Loose_in_20')
          )
        ),
        
        tabPanel('Jewelry Inventory by Region',
          h2('Jewelry Inventory by Region'),
          uiOutput("Region_Select_1"),
          DT::dataTableOutput('Jewelry_Inventory_by_Region')
        ),
        
        tabPanel('Loose Stone Inventory by Region',
           h2('Loose Stone Inventory by Region'),
           fluidRow(
             column(2,uiOutput('Region_Select_2')),
             column(2,uiOutput('Loose_Criteria_1')),
             column(2,uiOutput('Loose_Criteria_2')),
             column(2,uiOutput('Loose_Criteria_3')),
             column(2,uiOutput('Loose_Criteria_4')),
             column(2,uiOutput('Loose_Criteria_5'))
           ),
           dataTableOutput('Loose_Stone_Inventory_by_Region')
        )
      )
    ),
    
    tabPanel('Open Memos',
        uiOutput('tables')
    ),
    tabPanel('Distribution'
    )
))
                  