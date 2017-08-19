library(shiny)
library(shinydashboard)
library(dplyr)
library(tidyr)
library(ggplot2)
library(plotly)
library(highcharter)
library(DT)
#setwd('/Users/xiaopingguo/Desktop/final ')

#################################################################

Data = read.csv('taipei456_cluster_target.csv')

##(1) 五群用電情形
Group_5 <- highchart() %>% 
  hc_chart(type = "column") %>% 
  hc_title(text = "The State of Electricity about 5 Groups") %>% 
  hc_xAxis(categories = c('Multi resident in one housing', 'Disadvantage Family', 'General Family', 
                          'Three generations of well-off family', 'Single Group')) %>% 
  hc_yAxis(min = 500, max = 1200) %>% 
  hc_add_series(data = c(693.4,  863.8,  990.7,  1145.2, 1185.6),
                name = "2 Month Electricity Consumption / family ")

##(2) 五群雷達圖

gogocluster <- Data %>%  mutate(用電 = log(戶均用電),
                        扶養比 = (青少年人口+老年人口)/壯年人口,
                        所得中位log = log10(中位數),
                        老年比例 = 每戶平均老年人口數.人./每戶平均人數,
                        單身率  = 1 - 有偶比例...) %>% 
  select(行政區域,分群,  X1人一宅宅數比例... ,X6人以上一宅宅數比例..., 房價中位數,大學以下比例, 扶養比,老年比例 ,單身率, 戶均用電) 
names(gogocluster) = c("Region","Cluster", "1_live_rate", "6_live_rate", "Med_housing_price", "Under_college%", "Dependency_ratio","Eldery_one", "Single_rate", "Electricity")

gogocluster %>%
  mutate_each(funs(scale(.,center=TRUE,scale=TRUE)),-c(Region,Cluster))  %>% 
  gather('index','value',-c(Region,Cluster)) %>% 
  group_by(index) %>%
  mutate(value = (value-min(value))/(max(value)-min(value))) %>% 
  spread(index,value) -> data.h

data.h %>% 
  select(-1) %>% 
  group_by(Cluster) %>% 
  summarise_each(funs(median)) %>% 
  gather('index','value',-Cluster) %>%
  spread(Cluster,value) -> cluster_rader


#cluster_rader$mean <- rowMeans(cluster_rader[,-1])
cluster_rader$med <- apply(cluster_rader[,-1],1,median)
cluster_rader %>% 
  mutate_each(funs(round(.,digits=3)),-index) 

cluster_rader[cluster_rader$index ==  'Electricity',2:7 ] = 
  cluster_rader[cluster_rader$index ==  'Electricity',2:7 ] + 0.3

cluster_rader = cluster_rader[c(5,2,8,1,7,4,3,6),]

# write.csv(cluster_rader, "processed_data/radar_plot.csv", fileEncoding = "utf8", row.names = F)

## color
col.raw <- c("#1d3156","#984ea3","#4daf4a","#ff7f00","#e41a1c","#377eb8")

Group_5_radar <- highchart() %>% 
  hc_chart(polar = TRUE, type = "line") %>% 
  hc_xAxis(categories = cluster_rader$index,
           tickmarkPlacement = 'on',
           lineWidth = 0) %>% 
  hc_yAxis(gridLineInterpolation = 'polygon',
           lineWidth = 0,
           min = 0, max = 1) %>%
  hc_series(
    list(
      name = "Multi resident in one housing",
      data = cluster_rader$`第一群`,
      pointPlacement = 'on',color=col.raw[2]),
    list(
      name = "Disadvantage Family",
      data = cluster_rader$`第二群`,
      pointPlacement = 'on',color=col.raw[3]),
    list(
      name = "General Family",
      data = cluster_rader$`第三群`,
      pointPlacement = 'on',color=col.raw[4]),
    list(
      name = "Three generations of well-off family",
      data = cluster_rader$`第四群`,
      pointPlacement = 'on',color=col.raw[5]),
    list(
      name = "Single Group",
      data = cluster_rader$`第五群`,
      pointPlacement = 'on',color=col.raw[6]),
    list(
      name = "Total median",
      data = cluster_rader$med,
      pointPlacement = 'on',color= col.raw[1])
  )

##(3) 2016非營業用電
Data_Res <- read.csv('2016台北市非營業用電.csv', stringsAsFactors = F)


Mean2 <- Data_Res %>%
  left_join(Data[,c(1,105)], by = '行政區域') %>% 
  mutate(非營業用電 = gen_sum/戶數) %>% 
  mutate(Ym = factor(Ym, levels= c('10501', '10503', '10505', '10507', '10509', '10511' ))) %>% 
  separate(行政區域, c('區','里'), 3) %>% 
  group_by(Ym, 縣市名稱, 區) %>% 
  summarise(Average_electricity_consumption = mean(非營業用電, na.rm = T), Deviance_electricity_consumption = sd(非營業用電, na.rm = T)) %>%   
  hchart('line', hcaes(x = Ym, y = Average_electricity_consumption, group = 區)) %>% 
  hc_title(text = "1 year non-commercial use - Taipei City, the average household electricity consumption - AVERAGE",
           margin = 20, align = "left",
           style = list(color = "orange", useHTML = TRUE))


Var2 <- Data_Res %>%
  left_join(Data[,c(1,105)], by = '行政區域') %>% 
  mutate(非營業用電 = gen_sum/戶數) %>% 
  mutate(Ym = factor(Ym, levels= c('10501', '10503', '10505', '10507', '10509', '10511' ))) %>% 
  separate(行政區域, c('區','里'), 3) %>% 
  group_by(Ym, 縣市名稱, 區) %>% 
  summarise(Average_electricity_consumption = mean(非營業用電, na.rm = T), Deviance_electricity_consumption = sd(非營業用電, na.rm = T)) %>%   
  hchart('line', hcaes(x = Ym, y = Deviance_electricity_consumption, group = 區)) %>% 
  hc_title(text = "1 year non-commercial use - Taipei City, the average household electricity consumption - DEVIANCE",
           margin = 20, align = "left",
           style = list(color = "Green", useHTML = TRUE))


##(5)常用英文分群資料
Data$分群 = factor(Data$分群 , levels= c('第一群','第二群', '第三群', '第四群', '第五群' ))

Data1 = Data
Data1$分群 = gsub(pattern='第一群', 'Multi resident in one housing',Data1$分群)
Data1$分群 = gsub(pattern='第二群', 'Disadvantage Family',Data1$分群)
Data1$分群 = gsub(pattern='第三群', 'General Family',Data1$分群)
Data1$分群 = gsub(pattern='第四群', 'Three generations of well-off family',Data1$分群)
Data1$分群 = gsub(pattern='第五群', 'Single Group',Data1$分群)
names(Data1)[116] = 'clustering'

Data1$clustering = factor(Data1$clustering, levels = c('Multi resident in one housing',
                                                       'Disadvantage Family',
                                                       'General Family',
                                                       'Three generations of well-off family',
                                                       'Single Group'))


#################################################################

ui <- dashboardPage(
  dashboardHeader(title = "2017 Asia Hackathon ", titleWidth = 500),
    dashboardSidebar(
      width = 300,
      sidebarMenu(
        menuItem('Taipei % Sinbei city electricity situation', tabName = 'overview', icon = icon("bar-chart")),
        menuItem('5 clustering and outlier on Taipei', tabName = 'first_slide', icon = icon("plug")),
        menuItem('Target Group', tabName = 'second_slide', icon = icon("user")),
        

        h3(em(strong('Subject : Energy Conservation'))),
        br(),
        h4(strong('Team : "The Life is Struggle"')),
        h5('1. Peng-Wen,Lin(林芃彣)', style = "font-family: 'times'; font-si16pt  ;line-height: 10px ",align="left"),
        h5(' NCCU Department of Statistics ', style = "font-family: 'times'; font-si16pt ;line-height: 0px ", align="center" ),
        br(),
        h5('2. Pei Shuan, Haung(黃培軒)', style = "font-family: 'times'; font-si16pt  ;line-height: 10px ",align="left"),
        h5(' NCCU Department of Statistics ', style = "font-family: 'times'; font-si16pt ;line-height: 0px ", align="center" ),
        br(),
        h5('3. Pei Wen,Yang(楊佩雯)', style = "font-family: 'times'; font-si16pt  ;line-height: 10px ",align="left"),
        h5(' NCCU Department of Statistics ', style = "font-family: 'times'; font-si16pt ;line-height: 0px ", align="center" ),
        br(),
        h5('4. Li-Jer, Lin (林立哲)', style = "font-family: 'times'; font-si16pt  ;line-height: 10px ",align="left"),
        h5('  CITIC Housing ', style = "font-family: 'times'; font-si16pt ;line-height: 0px ", align="center" ),
        br(),
        h5('5. Jia-Hau Liu (劉家豪)', style = "font-family: 'times'; font-si16pt  ;line-height: 10px ",align="left"),
        h5(' NTU Internation Business ', style = "font-family: 'times'; font-si16pt ;line-height: 3px ", align="center" ),
        h5(' NCCU Department of Statistics ', style = "font-family: 'times'; font-si16pt ;line-height: 0px ", align="center" )
      )
    ),
  dashboardBody(
    tabItems(
      ### 第一個主頁
      tabItem(tabName = 'overview',
              titlePanel(p(strong(span("U-Optimizer - We are Life is struggle."), style = "color:blue"), align = 'center')),
              h2('Electricity Shortage is knocking our doors'),
              h4('In Taiwan, insufficient electricity supply is always an important issue. Especially in summer, we are usually under the risk of power outage. Sometimes, it really happened(8/17 Power Outage in Taiwan)In both Trends of Demand and Stability imply electricity shortage will be an even serious problem. '),
              h2('Our goal'),
              p('The Electricity saving policies enforcing process can be broken down into 3 parts, Identify the regions wasting electricity, figure out the reasons they wasting, and Set up corresponding policies. Our product was designed to shorten the time consumed during this process and help governors apply the right policies on the right regions. ',
                 style = "font-family: 'times'; font-si16pt"),
              fluidRow(
                tags$iframe(src = 'https://ssweetcoww.carto.com/viz/0dda36f9-1689-4eb1-9302-526e7c834992/embed_map',
                            width = '45%', height = '520'),
                tags$iframe(src = 'https://ssweetcoww.carto.com/viz/44328ecc-01f6-413a-85c4-79885a50fe64/embed_map',
                            width = '45%', height = '520')
                      ),
              fluidRow(
                tags$iframe(src = 'https://ssweetcoww.carto.com/viz/75b22836-b7fb-40e8-bb95-0fbaf99d2618/embed_map',
                            width = '45%', height = '520'),
                tags$iframe(src = 'https://ssweetcoww.carto.com/viz/963a79f1-2ccf-4539-a4a5-114f21b6c72a/embed_map',
                            width = '45%', height = '520')
              )
              
              ),
      tabItem(tabName = 'first_slide',
        #fluidRow()  等立哲地圖
        fluidRow(
          
          box(width = 5,status = 'info', height = 500, title = "",
              highchartOutput(outputId = "hist")
          )
          ,
          box(width = 6, status = 'info', height = 500, title = "The Composition of 5 group",
              highchartOutput(outputId = "Radar")
          )
        )
      ),
      tabItem(tabName = 'second_slide',
        fluidRow(
          box(width = 6,status = 'info', height = 500, title = "",
              fluidRow(highchartOutput(outputId = "Mean")
             )
          ),
          box(width = 6,status = 'info', height = 500, title = "",
              fluidRow(highchartOutput(outputId = "Var")
              )
          )
        ),
        fluidRow(
          align="center",
          sliderInput("slider1", label = h3("Threshold"),
                      min = 20, max = 360, value = 200)
        ),
        fluidPage(
          column(
            width = 6,
            highchartOutput(outputId = "Rate"),
          
            dataTableOutput('table')
          ), 
          column(
            width = 6,
            highchartOutput(outputId = "ave"),
            
            box(width = 13, status = 'info',solidHeader = TRUE, collapsed = F, collapsible = F,title = 'Benefit',
                p('Different threshold decide different amount of target group, if we focus on', strong('Three generations of well-off family'), 'and',
                  strong('Single Group'), '. The characteristics of these two groups is the elderly group, so if our policy works, the benefit we can obtain is :'),
                
                p('Saving', textOutput("conclusion1"), 'Amount of Electricity')
                
                )
            
          )
        )
        
      )
      ####
    )
  )
)


server <- function(input, output){

  
  output$DT <- renderDataTable({
    
    Data %>% select(行政區域, 戶數, 戶均用電, 扶養比, 平均教育程度, 平均綜合所得, 分群) %>% 
    filter(分群 == '第一群') -> tem1
    datatable(tem1)
  })
  
  
  output$hist <- renderHighchart({
    Group_5
  })
  
  output$Radar <- renderHighchart({
    Group_5_radar
  })
  
  output$Mean <- renderHighchart({
    Mean2
  })
  
  output$Var <- renderHighchart({
    Var2
  })
 output$Rate <- renderHighchart({
   
   tem1 = Data1 %>% group_by(clustering) %>% 
     summarise(RATE = sum(非營業用電_里變異 > input$slider1)/n())
   tem2 = Data1 %>% group_by(clustering) %>% 
     summarise(RATE = sum(非營業用電_里變異 <= input$slider1)/n())
   tem3 = cbind(rbind(tem1, tem2), Target=c(rep('target',5), rep('normal',5)))
   tem3$Target = factor(tem3$Target, levels = c('target', 'normal'))
   
   hchart(tem3, "column", hcaes(x = clustering, y = RATE, group = Target)) %>% 
   hc_title(text="The Percentage of Target Group in each Clustering", align = "center",
            style = list(color = c('#CD9B1D'))) %>% 
            hc_yAxis(title=list(text= 'percentage'), min=0 , max= 1)# 給標題
 })
 
 output$table <- renderDataTable({
   Data1 %>% mutate(分群1 = ifelse(非營業用電_里變異 > input$slider1, 'target', 'normal')) %>% 
     group_by(clustering ) %>% summarise(Anerage_Electricity = round(mean(戶均用電),4),
                                         family_num = sum(戶數),
                                         percentage = round(sum(分群1 == 'target')/n(),4)) -> Data2
 
   
  })

 output$ave <- renderHighchart({
   tem1 = Data1 %>% group_by(clustering) %>% 
     summarise(RATE = sum(非營業用電_里變異 > input$slider1/n()))
   tem2 = Data1 %>% group_by(clustering) %>% 
     summarise(RATE = sum(非營業用電_里變異 <= input$slider1/n()))
   tem3 = cbind(rbind(tem1, tem2), Target=c(rep('target',5), rep('normal',5)))
   tem3$RATE = 0
   tem3$householding = 0
   
   Data1 %>% mutate(Target = ifelse(非營業用電_里變異 > input$slider1, 'target', 'normal')) %>% 
     group_by(clustering, Target) %>% summarise(Electricity = mean(戶均用電), householding = sum(戶數)) %>% as.data.frame() -> Data3
   
   for(i in 1 : dim(Data3)[1]){
     index1 = which(as.character(unlist(Data3$clustering[i])) == as.character(unlist(tem3$clustering)) & as.character(unlist(Data3$Target[i])) == as.character(unlist(tem3$Target)))
     tem3[index1,2] = Data3[i,3]
     
     tem3[index1,4] = Data3[i,4]
   }
   

   tem3$Target = factor(tem3$Target, levels = c('target', 'normal'))
   names(tem3)[2] = 'Electricity_Usage'
   hchart(tem3, "column", hcaes(x = clustering, y = Electricity_Usage, group = Target)) %>% 
     hc_title(text="The Total Amount of Electricity Usage between target and normal in each Clustering", align = "center",
              style = list(color = c("#008B00"))) %>% 
     hc_yAxis(title=list(text= 'Average Amount Electricity Usage'), min=500 , max= 1500)
 })
 
 output$conclusion1 <- renderText({
   
   tem1 = Data1 %>% group_by(clustering) %>% 
     summarise(RATE = sum(非營業用電_里變異 > input$slider1/n()))
   tem2 = Data1 %>% group_by(clustering) %>% 
     summarise(RATE = sum(非營業用電_里變異 <= input$slider1/n()))
   tem3 = cbind(rbind(tem1, tem2), Target=c(rep('target',5), rep('normal',5)))
   tem3$RATE = 0
   tem3$householding = 0
   
   Data1 %>% mutate(Target = ifelse(非營業用電_里變異 > input$slider1, 'target', 'normal')) %>% 
     group_by(clustering, Target) %>% summarise(Electricity = mean(戶均用電), householding = sum(戶數)) %>% as.data.frame() -> Data3
   
   for(i in 1 : dim(Data3)[1]){
     index1 = which(as.character(unlist(Data3$clustering[i])) == as.character(unlist(tem3$clustering)) & as.character(unlist(Data3$Target[i])) == as.character(unlist(tem3$Target)))
     tem3[index1,2] = Data3[i,3]
     
     tem3[index1,4] = Data3[i,4]
   }
   
   
   tem3$Target = factor(tem3$Target, levels = c('target', 'normal'))
   names(tem3)[2] = 'Electricity_Usage'
   
   #########################################################################################################
   
  tem4 = tem3  %>% group_by(clustering) %>% summarise(dif = ifelse(diff(Electricity_Usage) > 0 , 0, abs(diff(Electricity_Usage) )))
  
  tem5 = tem4 %>% left_join( tem3 %>% filter(Target == 'target') %>% select(clustering, householding), by = 'clustering')
  Amount = {{tem5$dif*tem5$householding} %>% rev()}[1:2] %>% sum()  ## 第四、五群
  Rate = Amount/{Data1$gen_sum %>% sum()}
  Amount 
 })


 
 
}

shinyApp(ui, server)
