#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(shiny)
library(shinythemes)
library(DT)
library(shinydashboard)
library(leaflet)
library(leaflet.extras)
library(gsheet)




library(shinydashboard)

ui <- dashboardPage(
  dashboardHeader(title = "2019 n-CoV Dashborad"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Dashboard", tabName = "dashboard", icon = icon("dashboard")),
      menuItem("Live Data", tabName = "widgets", icon = icon("th")),
      menuItem("Information", tabName = "info", icon=icon("th"))
      
    )
  ),
  dashboardBody(
    tabItems(
      # First tab content
      tabItem(tabName = "dashboard",
              fluidPage(
                  theme = shinytheme("flatly"),
                  titlePanel("Enter your symptoms"),
                  sidebarLayout(
                    sidebarPanel(
                      
                      htmlOutput("googleForm")
                    ),
                    mainPanel(
                      box( width = 12,title = "Map of Symptoms",
                           status = "primary",
                           solidHeader = TRUE,

                      leafletOutput('map')),
                      box(width = 12,
                        # A static infoBox
                       fluidRow( 
                        infoBoxOutput("ApprovalBox1",width = 6),
                        
                        
                        infoBoxOutput("ApprovalBox2",width = 6)
                        
                        ),
                       fluidRow( 
                                 infoBoxOutput("ApprovalBox3",width = 6),
                                 
                                 
                                 infoBoxOutput("ApprovalBox4",width = 6)
                                 
                       )
                       ),
                       DT::dataTableOutput("googleFormData"),style = "overflow-y: scroll;overflow-x: scroll;"    ,
                      actionButton("refresh", "Refresh Sheet")
                    )
                  ))),

      # ),
      # 
      tabItem(tabName = "widgets",
              fluidPage(
                theme = shinytheme("flatly"),
                titlePanel("Log a message"),

                  sidebarPanel(
                  
                    htmlOutput("googleForm_2")
                  ),
                  mainPanel(
                    box( width = 12,title = "Worldwide outbreak of n-CoV",
                         status = "primary",
                         solidHeader = TRUE,

                         leafletOutput('map_2')),


                    DT::dataTableOutput("googleData_world"),style = "overflow-y: scroll;overflow-x: scroll;"    ,
                    actionButton("refresh", "Refresh Sheet")
                  )
                )

      ),
      tabItem(tabName = "info",  DT::dataTableOutput("qanda"),style = "overflow-y: scroll;overflow-x: scroll;")
              
              
      
      # Second tab content
      # tabItem(tabName = "widgets", 
      #   
      #   fluidPage(    mainPanel(
      #         
      #        (box(  title = "Settings",
      #               status = "primary",
      #               solidHeader = TRUE,
      #               leafletOutput('map_2'))),
      #         
      #         DT::dataTableOutput("googleData_world"),style = "overflow-y: scroll;overflow-x: scroll;",
      #         actionButton("refresh_world", "Refresh Sheet")
      #       )
      # ))
      )))
  


server <- function(input, output) {
  set.seed(122)
  histdata <- rnorm(500)
  
  output$plot1 <- renderPlot({
    data <- histdata[seq_len(input$slider)]
    hist(data)
  })
  
  
    library(shiny)
    library(DT)
    library(dplyr)
    library(googlesheets)

    ## ======================
    googleform_embed_link <- "https://docs.google.com/forms/d/e/1FAIpQLSch7KPARc6g5jR8L68iFSDEFoursKCeTNRcjAOxt7cx5sh03Q/viewform?embedded=true"
    googleform_data_url <- "https://docs.google.com/spreadsheets/d/1jqEq9Fb907whuk7wUnOS5RhY_q2VPnLjyDw_trBd6QM/edit?usp=sharing"
    googleform_data_url_world <- "https://docs.google.com/spreadsheets/d/1yZv9w9zRKwrGTaR-YzmAqMefw4wMlaXocejdxZaTs6w/"
    googleform_embed_link_2 <- "https://docs.google.com/forms/d/e/1FAIpQLScz0N-EXl62S5pgah-QcKRG4yeN5MUgAdEDumhCaG_-ZAJoGg/viewform?embedded=true"
qa="https://docs.google.com/spreadsheets/d/1Ioj1SWc93IPzTH_4zaxSjNNe95uKTeq1dvjawr-LvGo/edit?usp=sharing"    
    ## ======================
    ss <- gs_url(googleform_data_url, lookup = FALSE, visibility = "public")
    ss_qa <- gs_url(qa, lookup = FALSE, visibility = "public")


    # output$map <- renderLeaflet({ leaflet()%>%addTiles() %>%
    #     addControlGPS(options = gpsOptions(position = "topleft", activate = TRUE,
    #                                        autoCenter = TRUE, maxZoom = 60,
    #                                        setView = TRUE))})
    # observe(
    #   print(input$map_gps_located)
    # )
    
    output$map <- renderLeaflet(  
      
      
      leaflet(data = df) %>% addTiles() %>% addControlGPS(options = gpsOptions(position = "topleft", activate = TRUE,
                                                                                                     autoCenter = TRUE, maxZoom = 60,
                                                                                                                      setView = TRUE)) %>%
        
        addCircleMarkers(
          radius = ~ sqrt(as.integer(df$Confirmed))/5+1,
          stroke = FALSE, fillOpacity = 0.5
        ))

    output$googleForm <- renderUI({
      tags$iframe(id = "googleForm",
                  src = googleform_embed_link,
                  width = 400,
                  height = 625,
                  frameborder = 0,
                  marginheight = 0)
    })
    
    output$googleForm_2 <- renderUI({
      tags$iframe(id = "googleForm",
                  src = googleform_embed_link_2,
                  width = 400,
                  height = 625,
                  frameborder = 0,
                  marginheight = 0)
    })


    output$googleFormData <- DT::renderDataTable({
      input$refresh
      ss_dat <- gs_read(ss)

      DT::datatable(ss_dat)
    })
    
    output$qanda <- DT::renderDataTable({
      input$refresh
      ss_dat_qa <- gs_read(ss_qa)
      
      DT::datatable(ss_dat_qa)
    })
    
    output$googleData_world <- DT::renderDataTable({
      input$refresh_world
      df <- gsheet2tbl(googleform_data_url_world)
      df <- as.data.frame(df)
      paste_noNA <- function(x,sep=", ") {
        gsub(", " ,sep, toString(x[!is.na(x) & x!="" & x!="NA"] ) ) }
      
      sep=" "

      df[] <- lapply(df, gsub, pattern='Mainland', replacement='')
      
      # for(i in 1:length(df$address)){
      #   df$long[i] = as.integer(geocode_OSM(df$address[i])$coords['x'])
      #   df$lat[i] = as.integer(geocode_OSM(df$address[i])$coords['y'])
      #   }
      # 

      
      DT::datatable(df)
    })
    
    
    
    df <- gsheet2tbl(googleform_data_url_world)
    df <- as.data.frame(df)
    paste_noNA <- function(x,sep=", ") {
      gsub(", " ,sep, toString(x[!is.na(x) & x!="" & x!="NA"] ) ) }
    
    sep=" "
    df$address <- apply( df[ , c(0:2) ] , 1 , paste_noNA , sep=sep)
    
    df[] <- lapply(df, gsub, pattern='Mainland', replacement='')
    
    # for(i in 1:length(df$address)){
    #   df$long[i] = as.integer(geocode_OSM(df$address[i])$coords['x'])
    #   df$lat[i] = as.integer(geocode_OSM(df$address[i])$coords['y'])
    #   }
    # 
    
    lati = read.csv("lati.csv")
    longi = read.csv("longi.csv")
    
    df$latitude = lati$x
    df$longitude = longi$x
    
    output$ApprovalBox1 <- renderInfoBox({
      infoBox(
        "Days since the outbreak", 35, icon = icon("list"),
        color = "yellow"
      )
    })
    
    
    output$ApprovalBox2 <- renderInfoBox({
      infoBox(
        "Confirmed Cases", sum(as.integer(df$Confirmed)), icon = icon("list"),
        color = "purple"
      )
    })
    
    
    output$ApprovalBox3 <- renderInfoBox({
      infoBox(
        "Deaths", sum(as.integer(df$Deaths)), icon = icon("list"),
        color = "purple"
      )
    })
    
    
    output$ApprovalBox4 <- renderInfoBox({
      infoBox(
        "Recovered", sum(as.integer(df$Recovered)), icon = icon("list"),
        color = "yellow"
      )
    })
    
    output$map_2 <- renderLeaflet(  
      
      
      leaflet(data = df) %>% addTiles() %>%
                                          addCircleMarkers(
                                            radius = ~ sqrt(as.integer(df$Confirmed))/5+1,
                                            stroke = FALSE, fillOpacity = 0.5
                                          ))
}


# Run the application 
shinyApp(ui = ui, server = server)

