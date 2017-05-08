## app.R ##
library(shiny)
library(shinydashboard)
library(dplyr)
library(leaflet)


# ui side - Anton
ui <- dashboardPage(
      
      dashboardHeader(title = "Macha Malaria Mapping", titleWidth = 300),
      
      dashboardSidebar(
            fileInput('datafile', 'Upload CSV file', accept=c('text/csv')),
            radioButtons("time", label = h3("Subtypes"),
                         choices = list("Sum" = 1, "Average" = 2, "Median" = 3),
                         selected = 1)
      ),
      
      sidebarLayout(
            sidebarPanel(
                  numericInput("months",label = "Months",
                               value = )
            )
      )
      
      dashboardBody(
            # Boxes need to be put in a row (or column)
            
            
            fluidRow(id="isdata",
                     valueBox(textOutput("text1"), "is the total malaria count", color = "g
                              en")),        
            
            
            fluidPage(
                  leafletOutput("mymap")
                  
            )
      )
)



# server side - Anton
server <- function(input, output){
      
      # 
      ad in the .csv file of inte
      st
      filedata <- 
            active({
            infile <- input$datafile
            if (is.null(infile)) {
                  # User has not uploaded a file yet
                  
                  turn(NULL)
            }
            
            ad.csv(infile$datapath, stringsAsFactors=FALSE, fileEncoding="latin1")
      })
      
      
      
      # write the mean of the total rdt positive malaria
      output$text1 <- 
            nderText({     
            
            df<- filedata()
            if (is.null(df)) 
                  turn(NULL) # 
            turn nothing if nothing is uploaded
            df$rdt_tot<-df$rdtp_u5 + df$rdtp_a5
            
            sum_tot <-round(sum(df$rdt_tot, na.rm = T), digits = 0)
            sum_tot
            
            
      })
      
      # clinic coords
      
      
      output$mymap <- 
            nderLeaflet({
            df <- filedata()
            if (is.null(df)) 
                  turn(NULL) 
            leaflet() %>%
                  addProviderTiles(providers$Esri,
                                   options = providerTileOptions(noWrap = TRUE)) %>%
                  addMarkers(lng = df$long, lat = df$lat) %>%
                  addCircleMarkers(lng = df$long, lat = df$lat, weight = 1, 
                                   radius = df$rdtp_a5 + df$rdtp_u5)
            
            
      })
      
      
      
}


shinyApp(ui, server)


#all data from all HCs
dataframe <- as.data.frame(cbind(wk_start_date, hc_101, rdtp_u5))
dataframe <- group_by(dataframe, hcid)
wk_xts <- xts(dataframe[,-1], order.by=as.Date(wk_start_date))
dygraph(wk_xts, main = "RDT Positive cases", ylab = "RDT Positive", xlab = "Date") %>% 
      dyRangeSelector() %>%
      dyBarChart()  

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
