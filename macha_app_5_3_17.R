## app.R ##
library(shiny)
library(shinydashboard)
library(dplyr)
library(leaflet)


# ui side
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
        leafletOutput("mymap")
        
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

  

    }


shinyApp(ui, server)