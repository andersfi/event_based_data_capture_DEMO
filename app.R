
# from https://github.com/r-spatial/mapedit/blob/master/inst/examples/shiny_modules.R 
# from https://github.com/jrowen/rhandsontable/tree/master/inst/examples/rhandsontable_datafile
# Leaflet adwanced: 

library(mapedit)
library(mapview)
library(shiny)
library(rhandsontable)
library(sp)
library(leaflet)
library(leaflet.extras)

# fetch source data 
locations <- read.csv("./data/locations.csv")
coordinates(locations) = ~decimalLongitude + decimalLatitude
latlong = "+init=epsg:4326"
proj4string(locations) = CRS(latlong)

taxon_list <- read.csv("./data/taxon_list.csv", stringsAsFactors = FALSE)

# design leaflet map
m = leaflet(locations) %>%
  addTiles() %>% 
  #addWMSTiles("https://openwms.statkart.no/skwms1/wms.topo4.graatone?service=wms&request=getcapabilities")
  addCircleMarkers(weight = 1, layerId = 1:nrow(locations)) %>%
  addMiniMap() 
  #addSearchFeatures(targetGroups="waterBody")

# UI design ------------------------------------------------------------------------
ui <- fluidPage(
  titlePanel("Select locations"),
  sidebarLayout(
    
    sidebarPanel(

      selectModUI("test-mod"),
      DT::dataTableOutput("selected")
    ),
    
    mainPanel(
      titlePanel("Record occurrences"),
      actionButton("saveBtn", "Save"),
      rHandsontableOutput("hot")
      )
  )
)

# server logic --------------------------------------------------------
server <- function(input, output, session) {
  
  # select locations 
  selections <- callModule(selectMod, id="test-mod", leafmap=m)
  output$selected <- DT::renderDataTable({DT::datatable(locations@data[as.numeric(selections()$id),c(1,3)])})
  observe({
    print(locations@data[as.numeric(selections()$id),])
    })
  
  # Select occurrences 
  fname = tempfile(fileext = ".csv")
  
  observe({
    # remove button and isolate to update file automatically
    # after each table change
    input$saveBtn
    hot = isolate(input$hot)
    if (!is.null(hot)) {
      write.csv(hot_to_r(input$hot), fname)
      print(fname)
      print(locations@data[as.numeric(selections()$id),])
    }
  })
  
  output$hot = renderRHandsontable({
    if (!is.null(input$hot)) {
      DF = hot_to_r(input$hot)
    } else {
      DF = data.frame(scientificName = factor(taxon_list$ScientificName),
                      occurrenceStatus = factor(rep("uncertain",5), levels=c("uncertain","present","absent")),
                      invasive = rep(FALSE, 5), 
                      date = seq(from = Sys.Date(), by = "days", length.out = 5),
                      stringsAsFactors = FALSE)
    }
    
    rhandsontable(DF, width = 600, height = 300) %>%
      hot_col("scientificName", readOnly = TRUE)
  })
  
}
shinyApp(ui, server)

