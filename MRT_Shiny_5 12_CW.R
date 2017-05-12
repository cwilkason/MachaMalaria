## app.R ##
library(shiny)
library(shinydashboard)
library(dplyr)
library(leaflet)
library(dygraphs)
library(xts)
library(htmltools)



options(shiny.sanitize.errors = F)


# ui side

ui <- dashboardPage(
      
      
      
      dashboardHeader(title = "Macha Malaria Mapping", titleWidth = 300),
      
      dashboardSidebar(
            fileInput('datafile', 'Upload CSV file', accept=c('text/csv')),
            
            # selects different groups, prototype starts with just adult rdt positive
            
            selectInput("data_type", label = (h3("Variables")), 
                        choices = list(
                              "RDT positive, under 5yo" = "rdtp_u5",
                              "RDT positive, over 5yo" = "rdtp_a5",
                              "RDT negative, under 5yo, not treated" = "rdt_u5ntx",
                              "RDT negative, over 5yo, not treated" = "rdt_a5ntx",
                              "RDT negative, under 5yo, treated" = "rdt_u5tx",
                              "RDT negative, over 5yo, treated" = "rdt_a5tx",
                              "No RDT, under 5yo, treated" = "nordt_u5",
                              "No RDT, above 5yo, treated" = "nordt_a5"
                        ), 
                        selected = 1),
            
            # selects clinic
            selectInput("rhc", label = (h3("Health Centers")), 
                        choices = list(
                              "All" = "All",
                              "Chilala" = "Chilala",
                              "Chilalantambo" = "Chilalantambo",
                              "Chitongo" = "Chitongo",
                              "Habulile" = "Habulile",
                              "Kabulamwanda" = "Kabulamwanda",
                              "Kamwanu" = "Kamwanu",
                              "Macha" = "Macha",
                              "Mangunza" = "Mangunza",
                              "Mapanza" = "Mapanza",
                              "Mbabala" = "Mbabala",
                              "Mobola" = "Mobola",
                              "Nalube" = "Nalube",
                              "Siabunkululu" = "Siabunkululu",
                              "Simaubi" = "Simaubi"
                              
                        ), 
                        selected = 1),
            
            
            
            # select whether you want sum, average, median value
            radioButtons("statistic", label = h3("Subtypes"),
                         choices = list("Sum" = 1, "Average" = 2, "Median" = 3, "Maximum" = 4),
                         selected = 1)
      ),
      
      
      
      dashboardBody(
            # Boxes need to be put in a row (or column)
            
            
            fluidRow(id="isdata",
                     valueBox(textOutput("text1"), ".", color = "green"),        
            
            
            fluidPage(
                  
                  
                  
                  mainPanel(""),
                  
                  
                  leafletOutput("mymap"),
                  dygraphOutput("dygraph")
                  #dateRangeInput("dateRange", label = h3("Date range"),
                                 #start = as.Date("2012-01-01", format ="%Y-%m-%d"), end = Sys.Date()),
                  
                  #verbatimTextOutput("from"),
                  #verbatimTextOutput("to")
                  
            )
            
      )
))






############ server side
server <- function(input, output){
      
      
      # read in the .csv file of interest
      filedata <- reactive({
            infile <- input$datafile
            if (is.null(infile)) {
                  # User has not uploaded a file yet
                  return(NULL)
            }
            read.csv(infile$datapath, stringsAsFactors=FALSE, fileEncoding="latin1")
            
      })
      
      # make any necessary changes to this data frame, so you don't have to do it over again
      
      
      
      
      filedata2<- reactive ({
            if (is.null(filedata())) return(NULL)
            
            
            df<- filedata()
            df$date <- as.Date(df$wk_start_date, format ="%Y-%m-%d")
            
            # this is for the calendar date change, as opposed to the slider. Need to make sure
            # that both work well together. 
            
            #df2<- df %>% filter(date >= input$dateRange[1] & date <= input$dateRange[2])
            
            df2<- df %>% filter(date >= input$dygraph_date_window[1] & date <= input$dygraph_date_window[2])
            
            # select all or specific clinics
            
            if(input$rhc == 'All'){
                  df2 = df2
            }else{
                  df2 = df2 %>% filter(Name == input$rhc)
            }
            
            df2
            
      })
      
      
      
      # write the sum of the total rdt positive malaria
      output$text1 <- renderText({    
            df<- filedata2()
            #return(NULL)
            if (is.null(df)) return(NULL)
            # return nothing if nothing is uploaded
            
            # choose which variable to look at
            var <- df %>% select(one_of(input$data_type))
            
            
            # calculate the sum, average, or median. 
            if(input$statistic == 1){
                  sum_tot <-round(sum(var[,1], na.rm = T), digits = 0)
                  paste('The total is ',sum_tot)
                  
            } else if (input$statistic == 2){
                  sum_tot <-round(mean(var[,1], na.rm = T), digits = 5)
                  paste('The average is ',sum_tot)
                  
            }else if(input$statistic == 3){
                  sum_tot <-round(median(var[,1], na.rm = T), digits = 2)
                  paste('The median is ',sum_tot)
            }
            else if(input$statistic == 4){
                  sum_tot <-round(max(var[,1], na.rm = T), digits = 0)
                  paste('The maximum is ',sum_tot)
            }
            
            
            
            
      })
      
      
      
      
      # clinic coords
      output$mymap <- renderLeaflet({
            df <- filedata2()
            if (is.null(df)) return(NULL) 
            
            # choose which variable to look at
            var<-as.character(input$data_type)
            
            
            
            
            # the map
            leaflet() %>%
                  addProviderTiles(providers$Esri.WorldImagery,
                                   options = providerTileOptions(noWrap = TRUE)) %>%
                  addMarkers(lng = df$long, lat = df$lat, label = df$Name) %>%
                  addCircleMarkers(lng = df$long, lat = df$lat, weight = 1, 
                                   radius = df[,names(df) == var])
      })
      
      # dygraphs
      output$dygraph <- renderDygraph({
            df<- filedata()
            if (is.null(df)) return(NULL) 
            
            # select all or specific clinics
            
            if(input$rhc == 'All'){
                  df = df
            }else{
                  df = df %>% filter(Name == input$rhc)
            }
            
            # choose which variable to look at
            #df$var <- df %>% select(one_of(input$data_type))
            #df$var <- df$rdtp_a5
            var<-as.character(input$data_type)
            
            df$date <- as.Date(df$wk_start_date, format ="%Y-%m-%d")
            wk_xts <- xts(as.numeric(df[,names(df) == var]), order.by=df$date)
            dygraph(wk_xts, main = var, ylab = var, xlab = "Date") %>% 
                  dyRangeSelector() 
      })
      
      # slide bar beginning and end
      output$from <- renderText({
            strftime(req(input$dygraph_date_window[[1]]), "%d %b %Y")      
      })
      
      output$to <- renderText({
            strftime(req(input$dygraph_date_window[[2]]), "%d %b %Y")
      })
      
      
      
}
shinyApp(ui, server)


