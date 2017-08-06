library(shiny)
library(shinydashboard)
library(dplyr)
library(tidyr)
library(ggplot2)
library(plotly)
library(highcharter)
library(DT)

ui <- dashboardPage(
  dashboardHeader(title = "2017 Asia Hackathon ", titleWidth = 500),
    dashboardSidebar(
      width = 300,
      sidebarMenu(
        menuItem('Taipei % Sinbei city electricity situation', tabName = 'overview', icon = icon("bar-chart")),
        menuItem('5 clustering and outlier on Taipei', tabName = 'first_slide', icon = icon("plug")),
        
        h3(em(strong('Subject : Power Saving'))),
        br(),
        h4(strong('Team : "The Lift is Strugle"')),
        h5('1. Peng-Wen,Lin(林芃彣)', style = "font-family: 'times'; font-si16pt  ;line-height: 10px ",align="left"),
        h5(' NCCU Department of Statistics ', style = "font-family: 'times'; font-si16pt ;line-height: 0px ", align="center" ),
        br(),
        h5('2. Pei Shuan, Haing(黃培軒)', style = "font-family: 'times'; font-si16pt  ;line-height: 10px ",align="left"),
        h5(' NCCU Department of Statistics ', style = "font-family: 'times'; font-si16pt ;line-height: 0px ", align="center" ),
        br(),
        h5('3. Pei Wen,Yang(楊佩雯)', style = "font-family: 'times'; font-si16pt  ;line-height: 10px ",align="left"),
        h5(' NCCU Department of Statistics ', style = "font-family: 'times'; font-si16pt ;line-height: 0px ", align="center" ),
        br(),
        h5('4. Li-Jer, Lin (林立哲)', style = "font-family: 'times'; font-si16pt  ;line-height: 10px ",align="left"),
        h5('  CITIC Housing ', style = "font-family: 'times'; font-si16pt ;line-height: 0px ", align="center" )
      )
    ),
  dashboardBody(
    tabItems(
      ### 第一個主頁
      tabItem(tabName = 'overview',
              fluidRow(
                tags$iframe(src = 'https://ssweetcoww.carto.com/viz/eeebb44a-885b-11e6-95c9-0e05a8b3e3d7/embed_map',
                            width = '100%', height = '520')
                      )
              
              ),
      
      tabItem(
        ### 第二個主頁
        tabName = 'first_slide',
              fluidRow(
                tags$iframe(src = 'https://ssweetcoww.carto.com/viz/88e9e3fa-bd96-4203-b68e-c12a33b1d1ee/embed_map',
                            width = '100%', height = '520')
                      ),
              fluidRow(
                column(width = 2,
                  box( width = NULL, status = 'warning', title = "Clustering Class:",
                       radioButtons("choose", label = NULL,
                                    choices = list("Class 1" = 1, "Class 2" = 2,
                                                   "Class 3" = 3, "Class 4" = 4,
                                                   "Class 5" = 5), selected = 1))
                      ),
                column(
                  width = 8,
                  tabBox(
                    width = NULL,
                    tabPanel(h5('Class detail'), status = 'info', height = 400,
                             dataTableOutput("DT")
                    ),
                    tabPanel(h5('Visualization'), status = 'info', height = 400,
                             plotlyOutput('hist')
                    )
                  )
                )
                
              )
        
        
      )
      
      
    )
  )
  
)


server <- function(input, output){

  
  output$DT <- renderDataTable({
    tem1 = Data_class_outlier[,c(2,3,4,5,19,31,47,74,79)] %>% 
      filter(分群 == input$choose)
    tem1$區 = substr(tem1$行政區域, 1, 3)
    tem1$里 = substr(tem1$行政區域, 4, 6)
    tem1 = tem1[,c(10,11,2:8)]
    datatable(tem1)
  })
  
  
  output$hist <- renderPlotly({
    tem1 = Data_class_outlier[,c(2,3,4,5,19,31,47,74,79)] %>% 
      filter(分群 == input$choose)
    ggplot(tem1, aes(x = 戶均用電)) +
      geom_histogram() +
      theme_bw(base_family="STHeiti") +
      labs(x = '每戶平均用電(度) / 2個月 (此群下的所有里)', ylab = NULL) +
      theme(axis.text.x=element_text(angle =0 ,size=15),
            axis.text.y=element_text(size=15),axis.title.x=element_text(size=15),
            axis.title.y=element_text(size=20),plot.title = element_text(size = rel(1.5))) -> plot_tmp
    ggplotly(plot_tmp)
  })
  
  
}

shinyApp(ui, server)
