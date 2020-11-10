### Assignment 3 Dashboard
##################################################################################

#Packages
library(shinydashboard)
library(shiny)
library(readr)
library(readxl)
library(dplyr)
library(tidyr)
library(ggplot2)
library(plotly)
##################################################################################

#Data Set Imports

###Type 
df_type <- read_excel("Aus_Suicide_2019.xlsx",sheet=6,skip=6)
###State
df_state <- read_excel("Aus_Suicide_2019.xlsx",sheet=7,skip=6)
###Age 
df_age <- read_excel("Aus_Suicide_2019.xlsx",sheet=2,skip=6)
##################################################################################

#Universal Code

yearly <- c(2010,2011,2012,2013,2014,2015,2016,2017,2018,2019)
m <- list(b = 20, l = 30, r = 10, t = 10, pad = 0, autoexpand = FALSE)
##################################################################################

#Subsetting and Formatting of Data Frames

#Total Type Subset and Format
total_type <- df_type[-c(1:3,13,14,24,25,35:46),1:11]
colnames(total_type)[1]<- "Type"
total_type[1:9,12] <- "Male"
total_type[10:18,12] <- "Female"
total_type[19:27,12] <- "Persons"
colnames(total_type)[12]<- "Sex"
total_type <- total_type %>% gather(`2010`,`2011`,`2012`,`2013`,
                                    `2014`,`2015`,`2016`,`2017`,
                                    `2018`,`2019`,key='Year',value='Number of Suicides')
total_type$`Number of Suicides` <- as.numeric(total_type$`Number of Suicides`)
total_type$Year <- as.numeric(total_type$Year)

#Total State Subset and Format
total_state <- df_state[-c(1:3,13,14,24,25,35:46),1:11]
colnames(total_state)[1]<- "State"
total_state[1:9,12] <- "Male"
total_state[10:18,12] <- "Female"
total_state[19:27,12] <- "Persons"
colnames(total_state)[12]<- "Sex"
total_state <- total_state %>% gather(`2010`,`2011`,`2012`,`2013`,
                                      `2014`,`2015`,`2016`,`2017`,
                                      `2018`,`2019`,key='Year',value='Number of Suicides')
total_state$`Number of Suicides` <- as.numeric(total_state$`Number of Suicides`)

###Total Age Subset and Format
df_age <- read_excel("Aus_Suicide_2019.xlsx",sheet=2,skip=6)

total_age <- df_age[c(3:19,22:38,41:57),]
colnames(total_age)[1]<- "Age"
total_age[1:17,12] <- "Male"
total_age[18:34,12] <- "Female"
total_age[35:51,12] <- "Persons"
colnames(total_age)[12]<- "Sex"
total_age <- total_age %>% gather(`2010`,`2011`,`2012`,`2013`,
                                  `2014`,`2015`,`2016`,`2017`,
                                  `2018`,`2019`,key='Year',value='Number of Suicides')
total_age$`Number of Suicides` <- as.numeric(total_age$`Number of Suicides`)
total_age$Year <- as.numeric(total_age$Year)

##################################################################################

## app.R ##
ui <- dashboardPage(
  dashboardHeader(title = "Aus Suicide Dashboard"),
  ## Sidebar content
  dashboardSidebar(
    sidebarMenu(
      menuItem("Geographical", tabName = "state", icon = icon("search-location")),
      menuItem("Age & Type", tabName = "age", icon = icon("th")),
      menuItem("References", tabName = "refer", icon = icon("list-ul")),
      menuItem("Data Download", icon = icon("file-excel"), 
               href = "https://www.abs.gov.au/statistics/health/causes-death/causes-death-australia/2019/3303_11%20Intentional%20self-harm%20%28suicide%29%28australia%29.xlsx")
    )
  ),
  
  ## Body content
  dashboardBody(
    tabItems(
      # First tab content
      tabItem(tabName = "state",
              fluidRow(
                column(width=3,
                       box(
                         title="New South Wales:",status = "primary",
                         plotlyOutput("NSWState", height = 160,width = "100%"),width=NULL
                       ),
                       box(
                         title="Victoria:",status = "success",
                         plotlyOutput("VicState", height = 160),width=NULL
                       )
                ),
                
                column(width=6,
                  box(
                    title=strong("Australia:"),status = "danger",
                    plotlyOutput("ausState", height = 400),width=NULL
                    )
                ),
                
                column(width=3,
                       box(
                         title="Western Australia:",status = "primary",
                         plotlyOutput("WAState", height = 160),width=NULL
                       ),
                       box(
                         title="Australian Capital Territory:",status = "success",
                         plotlyOutput("ACTState", height = 160),width=NULL
                       )
                )
              ),
              fluidRow(
                box(
                  title="Queensland:",status = "info",width=3,
                  plotlyOutput("QldState", height = 160)
                ),
                box(
                  title="South Australia:",status = "warning",width=2,
                  plotlyOutput("SAState", height = 160)
                ),
                box(
                  width=2,height = 220,
                  radioButtons("buttons", h4("Gender:"),
                               c("All"="Persons",
                                 "Male Female Split" = "MF",
                                 "Male"="Male",
                                 "Female"="Female")),
                  ("**Hover over plot points to display information**")
                ),
                box(
                  title="Tasmania:",status = "warning",
                  plotlyOutput("TasState", height = 160),width=2
                ),
                box(
                  title="Northern Territory:",status = "info",width=3,
                  plotlyOutput("NTState", height = 160)
                )
              )
      ),
      
      # Second tab content
      tabItem(tabName = "age",
              fluidRow(
                box(width=6,status = "primary",
                    plotOutput("Ages")
                ),
                box(width=6,status = "success",
                    plotOutput("Type")
                       )
              ),
              fluidRow(
                box(width=3,status = "primary",height=177,
                  selectInput("select1", h4("Select Age:"), 
                                c("All ages"="All ages",
                                  "0-14"="0-14",
                                 "15-19"="15-19",
                                 "20-24"="20-24",
                                 "25-29"="25-29",
                                 "30-34"="30-34",
                                 "35-39"="35-39",
                                 "40-44"="40-44",
                                 "45-49"="45-49",
                                 "50-54"="50-54",
                                 "55-59"="55-59",
                                 "60-64"="60-64",
                                 "65-69"="65-69",
                                 "70-74"="70-74",
                                 "75-79"="75-79",
                                 "80-84"="80-84",
                                 "85 and over"="85 and over")),
                  checkboxInput("checkbox1", "Regression Line", value = FALSE)),
                  box(width=3,status = "primary",
                  radioButtons("buttonsAge", h4("Gender:"),
                               c("All"= "Persons",
                                 "Male Female Split"="MF",
                                 "Male"="Male",
                                 "Female"="Female"),
                )
                ),
                box(width=3,status = "success",height=177,
                           selectInput("select2", h4("Select Type:"), 
                                       c("All Types"="All Types",
                                         "Poisoning by drugs"="Poisoning by drugs",
                                         "Poisoning by other"="Poisoning by other",
                                         "Hanging"="Hanging",
                                         "Firearms"="Firearms",
                                         "Contact with sharp object"="Contact with sharp object",
                                         "Drowning and submersion"="Drowning and submersion",
                                         "Falls"="Falls",
                                         "Other"="Other")),
                    checkboxInput("checkbox2", "Regression Line", value = FALSE)),
                box(width=3,status = "success",
                    radioButtons("buttonsType", h4("Gender:"),
                                 c("All"= "Persons",
                                   "Male Female Split"="MF",
                                   "Male"="Male",
                                   "Female"="Female"))
                ),
              )
      ),
      
      # Forth tab content
      tabItem(tabName = "refer",
              h2("References:"),
              fluidRow(
                column(width=12,
              box(h4("Data Set Reference:"),("Causes of Death, Australia, 2019. (2020, October 23). Retrieved November 02, 2020, from https://www.abs.gov.au/statistics/health/causes-death/causes-death-australia/2019"),width=NULL),
              box(("R. (n.d.). Shinydashboard. Retrieved November 02, 2020, from https://rstudio.github.io/shinydashboard/get_started.html"),width=NULL),
              box(("R. (n.d.). Background: Shiny and HTML. Retrieved November 02, 2020, from https://rstudio.github.io/shinydashboard/structure.html"),width=NULL),
              box(("Font Awesome. (n.d.). Retrieved November 02, 2020, from https://fontawesome.com/icons?d=gallery"),width=NULL),
              box(("R. (n.d.). Lesson 3 Add control widgets. Retrieved November 02, 2020, from https://shiny.rstudio.com/tutorial/written-tutorial/lesson3/"),width=NULL),
              box(("R. (n.d.). Skins. Retrieved November 02, 2020, from https://rstudio.github.io/shinydashboard/appearance.html"),width=NULL)
                )
              )
    )
  )
)
)





server <- function(input, output) {
  
  output$ausState <- renderPlotly({
    type <- switch(input$buttons,
                   Persons = total_state %>% filter(Sex=="Persons"),
                   MF = total_state %>% filter(!Sex=="Persons"),
                   Male = total_state %>% filter(Sex=="Male"),
                   Female = total_state %>% filter(Sex=="Female"))
    StateLine <- switch(input$buttons,
                       Persons = "#dd4b39",
                       MF = c("Salmon","#00bfc4"),
                       Male = "#dd4b39",
                       Female = "#dd4b39")
    legend <- switch(input$buttons,
                        Persons = "none",
                        MF = c("right"),
                        Male = "none",
                        Female = "none")
    
    AusPlot <- type %>% 
      filter(State=='Australia') %>%
      ggplot(aes(x=Year,y=`Number of Suicides`,colour=Sex,group=Sex)) +
      theme(axis.title = element_text(size=12,face='bold')) +
      theme(axis.text = element_text(size=9)) +
      theme( legend.title = element_blank())+
      theme(legend.position = legend)+
      geom_line(size=.75) +
      geom_point()+
      scale_color_manual(values=StateLine)
    
    ggplotly(AusPlot,tooltip = list("Year","Number of Suicides")) %>%
      layout(legend = list(orientation="v",x=1,y=.5)) %>%
      config(displayModeBar = F)
  })
  
  output$NSWState <- renderPlotly({
    type <- switch(input$buttons,
                   Persons = total_state %>% filter(Sex=="Persons"),
                   MF = total_state %>% filter(!Sex=="Persons"),
                   Male = total_state %>% filter(Sex=="Male"),
                   Female = total_state %>% filter(Sex=="Female"))
    StateLine <- switch(input$buttons,
                        Persons = "#3c8dbc",
                        MF = c("Salmon","#00bfc4"),
                        Male = "#3c8dbc",
                        Female = "#3c8dbc")
    
    NSWplot <- type %>%
      filter(State=='NSW') %>%
      ggplot(aes(x=Year,y=`Number of Suicides`,colour=Sex,group=Sex)) +
      geom_line(size=.75) +
      geom_point()+
      scale_color_manual(values=StateLine)+
      theme(legend.position = "none")+
      theme(axis.title.x = element_blank(),
            axis.title.y = element_blank()) +
      scale_x_discrete(breaks = c(2010,2012,2014,2016,2018))
    
    ggplotly(NSWplot,tooltip = list("Year","Number of Suicides")) %>%
      config(displayModeBar = F) %>%
      layout(autosize=T,margin = m)
  })
  
  output$VicState <- renderPlotly({
    type <- switch(input$buttons,
                   Persons = total_state %>% filter(Sex=="Persons"),
                   MF = total_state %>% filter(!Sex=="Persons"),
                   Male = total_state %>% filter(Sex=="Male"),
                   Female = total_state %>% filter(Sex=="Female"))
    StateLine <- switch(input$buttons,
                        Persons = "#00a65a",
                        MF = c("Salmon","#00bfc4"),
                        Male = "#00a65a",
                        Female = "#00a65a")
    VicPlot <- type %>%
      filter(State=='Vic.') %>%
      ggplot(aes(x=Year,y=`Number of Suicides`,colour=Sex,group=Sex)) +
      geom_line(size=.75) +
      geom_point()+
      scale_color_manual(values=StateLine)+
      theme(legend.position = "none")+
      theme(axis.title.x = element_blank(),
            axis.title.y = element_blank()) +
      scale_x_discrete(breaks = c(2010,2012,2014,2016,2018))
    
    ggplotly(VicPlot,tooltip = list("Year","Number of Suicides")) %>%
      config(displayModeBar = F) %>%
      layout(autosize=T,margin = m)
  })
  
  output$QldState <- renderPlotly({
    type <- switch(input$buttons,
                   Persons = total_state %>% filter(Sex=="Persons"),
                   MF = total_state %>% filter(!Sex=="Persons"),
                   Male = total_state %>% filter(Sex=="Male"),
                   Female = total_state %>% filter(Sex=="Female"))
    StateLine <- switch(input$buttons,
                        Persons = "#00c0ef",
                        MF = c("Salmon","#00bfc4"),
                        Male = "#00c0ef",
                        Female = "#00c0ef")
    QldPlot <- type %>%
      filter(State=='Qld') %>%
      ggplot(aes(x=Year,y=`Number of Suicides`,colour=Sex,group = Sex)) +
      geom_line(size=.75) +
      geom_point()+
      scale_color_manual(values=StateLine)+
      theme(legend.position = "none")+
      theme(axis.title.x = element_blank(),
            axis.title.y = element_blank()) +
      scale_x_discrete(breaks = c(2010,2012,2014,2016,2018))
    
    ggplotly(QldPlot,tooltip = list("Year","Number of Suicides")) %>%
      config(displayModeBar = F) %>%
      layout(autosize=T,margin = m)
  })
  
  output$SAState <- renderPlotly({
    type <- switch(input$buttons,
                   Persons = total_state %>% filter(Sex=="Persons"),
                   MF = total_state %>% filter(!Sex=="Persons"),
                   Male = total_state %>% filter(Sex=="Male"),
                   Female = total_state %>% filter(Sex=="Female"))
    StateLine <- switch(input$buttons,
                        Persons = "#f39c12",
                        MF = c("Salmon","#00bfc4"),
                        Male = "#f39c12",
                        Female = "#f39c12")
    SAPlot <- type %>%
      filter(State=='SA') %>%
      ggplot(aes(x=Year,y=`Number of Suicides`,colour=Sex,group = Sex)) +
      geom_line(size=.75) +
      geom_point()+
      scale_color_manual(values=StateLine)+
      theme(legend.position = "none")+
      theme(axis.title.x = element_blank(),
            axis.title.y = element_blank()) +
      scale_x_discrete(breaks = c(2010,2014,2018))
    
    ggplotly(SAPlot,tooltip = list("Year","Number of Suicides")) %>%
      config(displayModeBar = F) %>%
      layout(autosize=T,margin = m)
  })
  
  output$WAState <- renderPlotly({
    type <- switch(input$buttons,
                   Persons = total_state %>% filter(Sex=="Persons"),
                   MF = total_state %>% filter(!Sex=="Persons"),
                   Male = total_state %>% filter(Sex=="Male"),
                   Female = total_state %>% filter(Sex=="Female"))
    StateLine <- switch(input$buttons,
                        Persons = "#3c8dbc",
                        MF = c("Salmon","#00bfc4"),
                        Male = "#3c8dbc",
                        Female = "#3c8dbc")
    WAPlot <- type %>% 
      filter(State=='WA') %>%
      ggplot(aes(x=Year,y=`Number of Suicides`,colour=Sex,group = Sex)) +
      geom_line(size=.75) +
      geom_point()+
      scale_color_manual(values=StateLine)+
      theme(legend.position = "none")+
      theme(axis.title.x = element_blank(),
            axis.title.y = element_blank()) +
      scale_x_discrete(breaks = c(2010,2012,2014,2016,2018))
    
    ggplotly(WAPlot,tooltip = list("Year","Number of Suicides")) %>%
      config(displayModeBar = F) %>%
      layout(autosize=T,margin = m)
  })
  
  output$TasState <- renderPlotly({
    type <- switch(input$buttons,
                   Persons = total_state %>% filter(Sex=="Persons"),
                   MF = total_state %>% filter(!Sex=="Persons"),
                   Male = total_state %>% filter(Sex=="Male"),
                   Female = total_state %>% filter(Sex=="Female"))
    StateLine <- switch(input$buttons,
                        Persons = "#f39c12",
                        MF = c("Salmon","#00bfc4"),
                        Male = "#f39c12",
                        Female = "#f39c12")
    TasPlot <- type %>%
      filter(State=='Tas.') %>%
      ggplot(aes(x=Year,y=`Number of Suicides`,colour=Sex,group=Sex)) +
      geom_line(size=.75) +
      geom_point()+
      scale_color_manual(values=StateLine)+
      theme(legend.position = "none")+
      theme(axis.title.x = element_blank(),
            axis.title.y = element_blank()) +
      scale_x_discrete(breaks = c(2010,2014,2018))
    
    ggplotly(TasPlot,tooltip = list("Year","Number of Suicides")) %>%
      config(displayModeBar = F) %>%
      layout(autosize=T,margin = m)
  })
  
  output$NTState <- renderPlotly({
    type <- switch(input$buttons,
                   Persons = total_state %>% filter(Sex=="Persons"),
                   MF = total_state %>% filter(!Sex=="Persons"),
                   Male = total_state %>% filter(Sex=="Male"),
                   Female = total_state %>% filter(Sex=="Female"))
    StateLine <- switch(input$buttons,
                        Persons = "#00c0ef",
                        MF = c("Salmon","#00bfc4"),
                        Male = "#00c0ef",
                        Female = "#00c0ef")
    NTPlot <- type %>%
      filter(State=='NT') %>%
      ggplot(aes(x=Year,y=`Number of Suicides`,colour=Sex,group = Sex)) +
      geom_line(size=.75) +
      geom_point()+
      scale_color_manual(values=StateLine)+
      theme(legend.position = "none")+
      theme(axis.title.x = element_blank(),
            axis.title.y = element_blank()) +
      scale_x_discrete(breaks = c(2010,2012,2014,2016,2018))
    
    ggplotly(NTPlot,tooltip = list("Year","Number of Suicides")) %>%
      config(displayModeBar = F) %>%
      layout(autosize=T,margin = m)
  })
  
  output$ACTState <- renderPlotly({
    type <- switch(input$buttons,
                   Persons = total_state %>% filter(Sex=="Persons"),
                   MF = total_state %>% filter(!Sex=="Persons"),
                   Male = total_state %>% filter(Sex=="Male"),
                   Female = total_state %>% filter(Sex=="Female"))
    StateLine <- switch(input$buttons,
                        Persons = "#00a65a",
                        MF = c("Salmon","#00bfc4"),
                        Male = "#00a65a",
                        Female = "#00a65a")
    ACTPlot <- type %>%
      filter(State=='ACT') %>%
      ggplot(aes(x=Year,y=`Number of Suicides`,colour=Sex,group =Sex)) +
      geom_line(size=.75) +
      geom_point()+
      scale_color_manual(values=StateLine)+
      theme(legend.position = "none")+
      theme(axis.title.x = element_blank(),
            axis.title.y = element_blank()) +
      scale_x_discrete(breaks = c(2010,2012,2014,2016,2018))
    
    ggplotly(ACTPlot,tooltip = list("Year","Number of Suicides")) %>%
      config(displayModeBar = F) %>%
      layout(autosize=T,margin = m)
  })
  
  output$Ages <- renderPlot({
    age <- switch(input$select1,
                  "All ages"="All ages",
                  "0-14"="0-14",
                  "15-19"="15-19",
                  "20-24"="20-24",
                  "25-29"="25-29",
                  "30-34"="30-34",
                  "35-39"="35-39",
                  "40-44"="40-44",
                  "45-49"="45-49",
                  "50-54"="50-54",
                  "55-59"="55-59",
                  "60-64"="60-64",
                  "65-69"="65-69",
                  "70-74"="70-74",
                  "75-79"="75-79",
                  "80-84"="80-84",
                  "85 and over"="85 and over")
    sexAge <- switch(input$buttonsAge,
                   Persons = total_age %>% filter(Sex=="Persons"),
                   MF = total_age %>% filter(!Sex=="Persons"),
                   Male = total_age %>% filter(Sex=="Male"),
                   Female = total_age %>% filter(Sex=="Female"))
    ageCol <- switch(input$buttonsAge,
                     Persons = "#88498F",
                     MF = c("Salmon","#00bfc4"),
                     Male = "#00bfc4",
                     Female = "Salmon")
    ageLine <- switch(input$buttonsAge,
                       Persons = "red",
                       MF = c("Salmon","#00bfc4"),
                       Male = "red",
                       Female = "red")
    ageLegend <- switch(input$buttonsAge,
                      Persons = "none",
                      MF = "right",
                      Male = "none",
                      Female = "none")
    
    appear <- input$checkbox1
    sexAge %>%
      filter(Age==age) %>%
      ggplot(aes(x=Year, y=`Number of Suicides`,fill=Sex,color=Sex)) +
      geom_col(position = position_dodge(), alpha = 0.75,size=0) +
      geom_smooth(method=lm,formula = y~x,se=FALSE,size=appear) +
      scale_x_continuous("Year", labels = as.character(yearly), breaks = yearly)+
      labs(title="Suicides Between the Ages:", subtitle=age)+
      theme_bw() +
      theme(plot.title = element_text(size = 17, face = "bold"))+
      theme(plot.subtitle = element_text(size=13,face= "bold"))+
      theme(legend.position = ageLegend)+
      theme(axis.title = element_text(size=13,face='bold',colour="#5C5C5C")) +
      theme(axis.text = element_text(size=10))+
      scale_fill_manual(values=ageCol)+
      scale_color_manual(values=ageLine)
  })
  
  output$Type <- renderPlot({
    type <- switch(input$select2,
                   "All Types"="Total",
                   "Poisoning by drugs"="Poisoning by drugs",
                   "Poisoning by other"="Poisoning by other",
                   "Hanging"="Hanging",
                   "Firearms"="Firearms",
                   "Contact with sharp object"="Contact with sharp object",
                   "Drowning and submersion"="Drowning and submersion",
                   "Falls"="Falls",
                   "Other"="Other")
    sexType <- switch(input$buttonsType,
                     Persons = total_type %>% filter(Sex=="Persons"),
                     MF = total_type %>% filter(!Sex=="Persons"),
                     Male = total_type %>% filter(Sex=="Male"),
                     Female = total_type %>% filter(Sex=="Female"))
    TypeCol <- switch(input$buttonsType,
                     Persons = "#58A22A",
                     MF = c("Salmon","#00bfc4"),
                     Male = "#00bfc4",
                     Female = "Salmon")
    TypeLine <- switch(input$buttonsType,
                      Persons = "red",
                      MF = c("Salmon","#00bfc4"),
                      Male = "red",
                      Female = "red")
    TypeLegend <- switch(input$buttonsType,
                       Persons = "none",
                       MF = "right",
                       Male = "none",
                       Female = "none")
    appear <- input$checkbox2
    sexType %>% 
      filter(Type==type) %>%
      ggplot(aes(x=Year, y=`Number of Suicides`,fill=Sex,color=Sex)) +
      geom_col(position = position_dodge(), alpha = 0.75,size=0) +
      geom_smooth(method=lm,formula = y~x,se=FALSE,size=appear) +
      scale_x_continuous("Year", labels = as.character(yearly), breaks = yearly)+
      labs(title="Suicides by Type:", subtitle = type)+
      theme_bw() +
      theme(plot.title = element_text(size = 17, face = "bold"))+
      theme(plot.subtitle = element_text(size=13,face= "bold"))+
      theme(legend.position = TypeLegend)+
      theme(axis.title = element_text(size=13,face='bold',colour="#5C5C5C")) +
      theme(axis.text = element_text(size=10))+
      scale_fill_manual(values=TypeCol)+
      scale_color_manual(values=TypeLine)
  })
}

shinyApp(ui, server)

