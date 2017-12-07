source('./helper/setupApp.R')
source('./helper/functions.R')

method <-'intersect'     #Eithe4 intersect or within
#buffer.radius.miles <-500
geocode.source <- 'google' #google or dsk
mpos.census.tracts <-data.frame(ny.tracts.pts.wgs84[,c('geoid','mpo')])
mpos.list <-unique(c('All',mpos.census.tracts[!is.na( mpos.census.tracts$mpo ),'mpo']))
#set_config(use_proxy(url='gateway.zscalertwo.net', port=80))


ui <- fluidPage(
  
  # App title ----
  titlePanel("ACS 2011-2015!"),
  
  # Sidebar layout with input and output definitions ----
  sidebarLayout(
    
    # Sidebar panel for inputs ----
    sidebarPanel(
      
      # Input: Slider for the number of bins ----
      checkboxGroupInput("get.mpo", label = h3("select mpo"), 
                         choices = mpos.list,selected="NYMTC"),
      selectInput("method", "Method:",
                  c("Within","Intersect" )),
      
      textInput(inputId="address",label="Enter Address Here",value="New York"),
      actionButton("get.map", "Get Map"),
      
      textInput(inputId="buffer.radius.miles",label="Enter Buffer Radius Here",value="0.5"),
      actionButton("get.data", "Get Data")
      
      
    ),
    
    # Main panel for displaying outputs ----
    mainPanel(
      tabsetPanel(type='tabs',
                  tabPanel("plot", leafletOutput(outputId = "plot")),
                  tabPanel('test', dataTableOutput("lep")),
                  tabPanel('test', dataTableOutput("age"))
      )

    )
  )
)

# Define server logic required to draw a histogram ----
server <- function(input, output) {
  

#Code to plot LEAFLET map;
  shape.file<-reactive({ 
  a<-tracts_for_analysis_m(state.shp=ny.tracts.wgs84.simple,mpo=input$get.mpo, mpos.list = mpos.list)

 })
    
  

  address <-eventReactive(
    input$get.map,{
      data.frame(geocode(input$address))
    })
  
  
  location      <- reactive({
     shiny::validate(need(!is.na(address()),"Please check geocode address"))
     SpatialPointsDataFrame(coords=address(), data=address(), proj4string = CRS(wgs84))
    })
  address.crs   <- reactive({spTransform(location(), CRS("+init=epsg:3347"))})
  buffer.crs    <- reactive({
    shiny::validate(need(!is.na(input$buffer.radius.miles) & as.numeric(input$buffer.radius.miles) >0, label= "Range must be greater than 0"))
    gBuffer(spgeom = address.crs(), width =as.numeric(input$buffer.radius.miles)*miles.to.meters)
    })
  buffer.wgs84  <- reactive({spTransform(buffer.crs(), wgs84)})
  tracts.sel <- reactive({
    select_tracts_from_buffer(method=input$method,tracts=shape.file(),centroids=ny.tracts.pts.wgs84,buffer.file=buffer.wgs84())
  })
  
  output$plot <- renderLeaflet({
    leaflet(tracts.sel())%>%
      addTiles()%>%
      addPolygons()%>%
      addPolygons(data=buffer.wgs84())
  })
  
  # tracts.counts <- reactive({length(tracts.sel())})
  # 
  # census.data.count <-eventReactive(input$get.data,{
  #     if(tracts.counts()==0){no.data<-datatable(data=data.frame(to.print='Warning: Census Tracts have not been selected'),rownames=FALSE,colnames="")}
  #     else{datatable(data=data.frame(census.data()),rownames=FALSE)}
  #   })
  
    
    output$lep  <-renderDataTable({lep.dt()})
    
      lep.dt<- eventReactive(input$get.data,{
        tracts.sel <- lep.state@geography$geoid %in% tracts.sel()$geoid
        lep <- lep.state[tracts.sel,]
        lep.agg <- apply(lep,1,sum)
        lep.sum <- do.call('rbind', lapply(1:length(lep.agg), process_acs_data, acs.data=lep.agg, acs.shell=language.spoken.at.home,group.var='lep'))
        format.data.table(acs.data=lep.sum,method=input$method, buffer=input$buffer.radius.miles,address=input$address)
      })
      
      output$age  <-renderDataTable({age.dt()})
      
      age.dt<- eventReactive(input$get.data,{
        tracts.sel <- age.state@geography$geoid %in% tracts.sel()$geoid
        age <- age.state[tracts.sel,]
        age.sum <- option2calculation(acs.data=age,acs.shell=age.groups, acs.group.var='age.groups')
        format.data.table(acs.data=age.sum,method=input$method, buffer=input$buffer.radius.miles,address=input$address)
      })
      

  
  
}
shinyApp(ui = ui, server = server)