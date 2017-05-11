## app.R ##
library(shiny)
library(shinydashboard)
library(dplyr)
library(leaflet)
library(dygraphs)
library(xts)


# ui side

vars <- c(
      "RDT positive, under 5yo" = "rdtp_u5",
      "RDT positive, over 5yo" = "rdtp_a5",
      "RDT negative, under 5yo, not treated" = "rdt_u5ntx",
      "RDT negative, over 5yo, not treated" = "rdt_a5ntx",
      "RDT negative, under 5yo, treated" = "rdt_u5tx",
      "RDT negative, over 5yo, treated" = "rdt_a5tx",
      "No RDT, under 5yo, treated" = "nordt_u5",
      "No RDT, above 5yo, treated" = "nordt_a5"
      )


ui <- dashboardPage(
      
      dashboardHeader(title = "Macha Malaria Mapping", titleWidth = 300),
      
      dashboardSidebar(
            fileInput('datafile', 'Upload CSV file', accept=c('text/csv')),
            radioButtons("time", label = h3("Subtypes"),
                         choices = list("Sum" = 1, "Average" = 2, "Median" = 3),
                         selected = 1)
            ),
      
      dashboardBody(
            # Boxes need to be put in a row (or column)
            
            fluidRow(id="isdata",
                     valueBox(textOutput("text1"), "is the total malaria count", color = "green")),        
            
            
            fluidPage(
                  mainPanel("Malaria maps"),
                  leafletOutput("mymap"),
                  dygraphOutput("dygraph")),
                  
            #want to create a see-through panel similar to https://shiny.rstudio.com/gallery/superzip-example.html

            absolutePanel(title = "Macha Malaria Mapping", titleWidth = 300, id = "controls", class="panel panel-default", fixed = TRUE,
                   draggable = TRUE, top = 60, left = "auto", right = 20, bottom = "auto",
                   width = 330, height = "auto",
                   
                   h2("data_final"),
                   
                   selectInput("Variables", "variables", vars),
                   tableOutput("data")
                   
            )
            )
      )





# server side
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
      
      # write the mean of the total rdt positive malaria
      output$text1 <- renderText({     
            df<- filedata()
            if (is.null(df)) return(NULL) # return nothing if nothing is uploaded
            df$rdt_tot<-df$rdtp_u5 + df$rdtp_a5
            
            sum_tot <-round(sum(df$rdt_tot, na.rm = T), digits = 0)
            sum_tot
            
      })
      
      # clinic coords
      
      output$mymap <- renderLeaflet({
            df <- filedata()
            if (is.null(df)) return(NULL) 
            leaflet() %>%
                  addProviderTiles(providers$Esri,
                                   options = providerTileOptions(noWrap = TRUE)) %>%
                  addMarkers(lng = df$long, lat = df$lat) %>%
                  addCircleMarkers(lng = df$long, lat = df$lat, weight = 1, 
                                   radius = df$rdtp_a5 + df$rdtp_u5)
            
            
      })
      output$dygraph <- renderDygraph({
      df<- filedata()
      if (is.null(df)) return(NULL) 
      data_tbl <- tbl_df(df)
      dataframe <- as.data.frame(cbind(df$wk_start_date, df$rdtp_u5))
      wk_xts <- xts(dataframe[,2], order.by=as.Date(df$wk_start_date))
      dygraph(wk_xts, main = "RDT Positive cases", ylab = "RDT Positive", xlab = "Date") %>% 
            dyRangeSelector() 
      })
      
      }
      


      
      
shinyApp(ui, server)









##can ignore

#all data from all HCs
data_tbl <- tbl_df(data_final)
dataframe <- as.data.frame(cbind(wk_start_date, rdtp_u5))
wk_xts <- xts(dataframe[,2], order.by=as.Date(wk_start_date))
dygraph(wk_xts, main = "RDT Positive cases", ylab = "RDT Positive", xlab = "Date") %>% 
      dyRangeSelector() 

#HC 101
data_tbl <- tbl_df(data_final)
hc_101 <- filter(data_tbl, hcid == 101)
hc_101_wk<- hc_101$wk_start_date
hc_101_rdptu5<- hc_101$rdtp_u5
dataframe_101 <- as.data.frame(cbind(hc_101_wk, hc_101_rdptu5))
wk_xts_101 <- xts(dataframe_101[,-1], order.by=as.Date(hc_101_wk))
dygraph(wk_xts_101, main = "RDT Positive cases", ylab = "RDT Positive", xlab = "Date") %>% 
      dyRangeSelector()

#HC 102
hc_102 <- filter(data_tbl, hcid == 102)
hc_102_wk<- hc_102$wk_start_date
hc_102_rdptu5<- hc_102$rdtp_u5
dataframe_102 <- as.data.frame(cbind(hc_102_wk, hc_102_rdptu5))
wk_xts_102 <- xts(dataframe_102[,-1], order.by=as.Date(hc_102_wk))
dygraph(wk_xts_102, main = "RDT Positive cases: HC 102", ylab = "RDT Positive", xlab = "Date") %>% 
      dyRangeSelector()

#HC 103
hc_103 <- filter(data_tbl, hcid == 103)
hc_103_wk<- hc_103$wk_start_date
hc_103_rdptu5<- hc_103$rdtp_u5
dataframe_103 <- as.data.frame(cbind(hc_103_wk, hc_103_rdptu5))
wk_xts_103 <- xts(dataframe_103[,-1], order.by=as.Date(hc_103_wk))
dygraph(wk_xts_103, main = "RDT Positive cases: HC 103", ylab = "RDT Positive", xlab = "Date") %>% 
      dyRangeSelector()

#HC 104
hc_104 <- filter(data_tbl, hcid == 104)
hc_104_wk<- hc_104$wk_start_date
hc_104_rdptu5<- hc_104$rdtp_u5
dataframe_104 <- as.data.frame(cbind(hc_104_wk, hc_104_rdptu5))
wk_xts_104 <- xts(dataframe_104[,-1], order.by=as.Date(hc_104_wk))
dygraph(wk_xts_104, main = "RDT Positive cases: HC 104", ylab = "RDT Positive", xlab = "Date") %>% 
      dyRangeSelector()

#etc...
