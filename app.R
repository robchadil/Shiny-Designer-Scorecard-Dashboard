## app.R ##
library(shiny)
library(shinydashboard)
library(zoo)
library(plotly)
library(readr)
# library(emojifont)
# load.fontawesome()


#Import csv
tandb.data <- read.csv('T&Bdata.csv')

# extract needed columns
df0 <- tandb.data[c('Designer.Preferred.Name','Month.Year','Tasks.Completed..LR.CID.R.A.')]

###Convert Data###
df0$Month.Year <- as.Date(df0$Month.Year, format = '%m/%d/%Y')
designer.names <- unique(df0$Designer.Preferred.Name)
df0$Tasks.Completed..LR.CID.R.A. <- as.numeric(as.character(df0$Tasks.Completed..LR.CID.R.A.))

#test data
sups <- c('Darryl','Fran','Iliana','Mark','Methode','Trent','Tyler','Nik','Chris B.','Mich','Adam','Angel','Brian','Chris O.')
slatest <- round(runif(nrow(df0),50,95)/100,3)
df0$slatest <- slatest

ui <- dashboardPage(
  
  ### Header ###
  dashboardHeader(title = "Designer Scorecard"),
  
  ### Sidebar ###
  dashboardSidebar(
    sidebarMenu(
      menuItem('Dashboard', tabName = 'dashboard', icon = icon('dashboard')),
      # menuItem("Widgets", icon = icon("th"), tabName = "widgets",
      #          badgeLabel = "new", badgeColor = "green")
      
      menuItem("Filters", icon = icon("bar-chart-o"),
               # Input directly under menuItem
               selectInput('designerFilter', "Designer Name",
                           choices = designer.names, multiple=FALSE, selectize=TRUE,
                           width = '99%'),
               selectInput('supFilter', 'Supervisor',
                           choices = sups, selected = '', multiple=FALSE, selectize=TRUE,
                           width = '99%')
      
      ## Designer Name drop down filter ##
      #put stuff here
      
      )
    )),
  dashboardBody(
    
    ## Sidebar Tabs content ##
    tabItems(
      tabItem(tabName = 'dashboard',

              h2(textOutput('name.test')),
              
              ## box to test name filters ##
              # verbatimTextOutput('name.test'),
              
              ## infoboxes ##
              fluidRow(
                valueBox('48%','Core Task SLA',icon = icon('thumbs-o-down'), color = 'red'),
                valueBox(77, 'Tasks Completed', icon = icon('line-chart'), color = 'olive'),
                valueBox(40.5,'Hours Worked', icon = icon('clock-o'))
                # Dynamic valueBoxes
                # valueBoxOutput("approvalBox")
              ),
              
              ### Add Boxes (plots) by fluidRow ###
              fluidRow(
                tabBox(title = 'Tab test', id = 'tabtest1',height = 560,
                       tabPanel('Tab1-test',
                                ## Tasks Per Month Plot ##
                                box(title = 'Core Tasks Completed',background = 'blue',width = '99%',plotlyOutput('taskPerMonth',height = 400,width = '99%'))),
                       tabPanel('Tab2-test','moar stuff')),

                ## SLA plot ##
                box(title = 'Core Tasks SLA',background = 'purple',plotlyOutput('sla',height = 500))
                ),
              
              ## Table to test data ##
              fluidRow(dataTableOutput('test.table'))
              ),
      tabItem(tabName = "widgets",
              h2("Widgets tab content")
              )
      ) ### tabItems end ###
    )### dashboardBody end ###
)### dashboardPage end ### 

server <- function(input, output) {
  
  ### test data table output ###
  output$test.table <- renderDataTable({subset(df0,subset = Designer.Preferred.Name == input$designerFilter)})
  
  ### tasks completed plotly ###
  output$taskPerMonth <- renderPlotly({
    df0 <- subset(df0,subset = Designer.Preferred.Name == input$designerFilter)
    plot_ly(df0, x = df0$Month.Year, y = df0$Tasks.Completed..LR.CID.R.A., type = 'bar') %>%
    layout(xaxis = list(title = 'Month'),
           yaxis = list(title = 'Tasks Completed',dtick = 10)) ##### chart, x and y axis titles
  })
  
  # ### SLA plotly ###
  output$sla <- renderPlotly({
    df0 <- subset(df0,subset = Designer.Preferred.Name == input$designerFilter)
    plot_ly(df0, x = df0$Month.Year, y = df0$slatest , type = 'scatter',mode = 'lines',line = list(color = '#a2988d', width = 3)) %>%
      layout(
      yaxis = list(range = c(0,1), tickformat = '%'))
  })
  # df0 <- reactive({subset(df0,subset = Designer.Preferred.Name == input$designerFilter)})
  output$name.test <- renderText({paste0(input$designerFilter)})
}

shinyApp(ui, server)


###########################

