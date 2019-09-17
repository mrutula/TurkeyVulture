library(plotly)
#install.packages("shinythemes")
install.packages("readxl")
#install.packages("shinythemes")
library("readxl")
library(leaflet)
library(shiny)
library(dplyr)
library(geosphere)
library(plyr)
library(lubridate)
library(tidyverse)
library(tidyr)
library(ggplot2)
library("ggthemes")
library(stringr) #for manipulating string i.e regular expression
library(chron) #package to treat time as time
library(shinythemes) #theme for shiny app

#turkey object contains the dataframe
turkey <- read_excel('TurkeyvulturesinNorthandSouthAmericamigration.xlsx')

colnames(turkey) <- c("event_id","visible","timestamp","location_long","location_lat","manually_marked_outlier","sensor_type","individual_taxon_canonical_name","tag_local_identifier","individual_local_identifier","study_name","utm_easting","utm_northing","utm_zone","study_timezone","study_local_timestamp","tag_id","animal_id","animal_taxon","deploy_on_date","deploy_off_date","animal_comments","animal_life_stage","animal_mass","attachment_type","deployment_comments","deployment_id","duty_cycle","study_site","tag_manufacturer_name","tag_mass","tag_model")

#changed the date time datatype
turkey$timestamp <- as.POSIXct(turkey$timestamp, format= "%Y-%m-%d %H:%M:%S",tz="GMT")
turkey$study_local_timestamp<-as.POSIXct(turkey$study_local_timestamp, format= "%Y-%m-%d %H:%M:%S",tz="GMT")
turkey$deploy_on_date <- as.POSIXct(turkey$deploy_on_date, format= "%Y-%m-%d %H:%M:%S",tz="GMT")
turkey$deploy_off_date <- as.POSIXct(turkey$deploy_off_date, format= "%Y-%m-%d %H:%M:%S",tz="GMT")

#separating into year , month, day
turkey$year1 <- as.numeric(format(as.Date(turkey$timestamp, format="%d/%m/%Y"),"%Y"))
turkey$month1 <-as.numeric(format(as.Date(turkey$timestamp, format="%d/%m/%Y"),"%m"))
turkey$day1 <- as.numeric(format(as.Date(turkey$timestamp, format="%d/%m/%Y"),"%d"))

#replacing the value 1 to 12 with the respective months
turkey$month1 <- mapvalues(turkey$month1,c(1,2,3,4,5,6,7,8,9,10,11,12), c("Jan","Feb","Mar","April","May","June","July","Aug","Sept","Oct","Nov","Dec"))
#summary(turkey)

#since the number of rows of butterball and Schaumboch is repeated we remove the duplicate columns where 
#the value of both the Individual_local_identifier and animal_Id dont match

turkey <- filter(turkey,individual_local_identifier == animal_id)



#distance travelled by each bird
turkey$distance <- NA
#calculating the distance travelled
for(i in 1:215719){
  if(i == 1){
    a <- turkey$location_long[i]
    b <- turkey$location_lat[i]
    c <- turkey$location_long[i]
    d <- turkey$location_lat[i]
    turkey$distance[i] <- distHaversine(c(a,b),c(c,d))/1000
  }
  else{
    e <- turkey$location_long[i-1]
    f <- turkey$location_lat[i-1]
    g <- turkey$location_long[i]
    h <- turkey$location_lat[i]
    turkey$distance[i] <- distHaversine(c(e,f),c(g,h))/1000
  }
  
}
#converting from km to miles
turkey$distance_mile <- NA
turkey$distance_mile <- turkey$distance * 0.62137

#Creating a new population column 
turkey$population <- NA
turkey$population[turkey$deployment_comments == "trapped in Pennsylvania using padded-leg hold traps and monofilament noose traps" ] <- "East Coast"
turkey$population[turkey$deployment_comments == "trapped in La Pampa Argentina using walk-in and noose-string traps" ] <- "South"
turkey$population[turkey$deployment_comments == "trapped in California using walk-in traps" ] <- "West Coast"
turkey$population[turkey$deployment_comments == "trapped on their nests in Saskatchewan"  ] <- "Interior"

#renaming the birds 

turkey$individual_local_identifier[turkey$individual_local_identifier == 'Steamhouse 1'] <- 'Steamhouse1'
turkey$individual_local_identifier[turkey$individual_local_identifier == 'Steamhouse 2'] <- 'Steamhouse2'
turkey$individual_local_identifier[turkey$individual_local_identifier == 'Young Luro'] <- 'YoungLuro'
turkey$individual_local_identifier[turkey$individual_local_identifier == 'La Pampa'] <- 'LaPampa'



turkey_year <- select(turkey,individual_local_identifier,year1,distance)
turkey_loc <- select(turkey,individual_local_identifier,location_long,location_lat,year1,month1,population,animal_mass)

#aggregate weight of the population
aggweight <-aggregate(animal_mass ~ population, turkey_loc,mean)
#for morongo story
turkey_morongo <- filter(turkey_loc,individual_local_identifier == "Morongo")

#user interface
ui <- navbarPage("Navbar!",theme = shinytheme("sandstone"),fluid = TRUE,
                 tabPanel("Plot1",
                          #first tab
                          fluidRow(
                            h2('TURKEY VULTURE MIGRATION'),
                            div('The turkey vulture(Cathartes aura), also known in some North American regions as the turkey buzzard and in some areas of carribean as John crow. It ranges from southern canada to southern tip of South America',style = "color:brown")
                            
                          ),
                          fluidRow(
                               column(2,
                                      #checkbox for year
                                  checkboxGroupInput("year_selected",
                                                     label ="Select the year",
                                                     choices = list("2003","2004","2005","2006","2007","2008","2009","2010","2011","2012","2013"),
                                                     selected = c("2003","2004","2005","2006","2007","2008","2009","2010","2011","2012","2013")
                                  ),
                                  #radio button for choosing miles or km
                                  radioButtons("distMeasure_selected",
                                               label = "In Km or Miles",
                                               choices = list("Km","Miles"),
                                               selected = "Km"
                                  )
                             
                           ),column(4,
                                    #ploting the mass of population
                                    h3("Average Mass of the population"),
                                    #leaflet output
                                    plotlyOutput("wplot")
                           ), column(6,
                                     #plotting the total distance
                              h3('Total distance travelled in each year'),
                               plotOutput(outputId = "distyearPlot")
                           )
                          ),
                          fluidRow(
                            column(2,
                                   h5('The migratory flight pattern of 19 vultures were tracked over a period of 10 years')),
                            column(5,
                                   strong("Vultures living on East Coast,on average , are larger than the other North American vultures")
                      ),
                           
                           column(5,
                                  strong("vultures from interior travel the furthest annually. Most of their travel occurs during the months of April and October"))
                            
                              )

                              
                 ),
                          
                tabPanel("Plot2",
                         #tab 2
                         titlePanel("MIGRATORY PATH OF EACH BIRD"),
                            sidebarLayout(
                              sidebarPanel(
                                #checkbox to select the bird
                                checkboxGroupInput("bird_selected",
                                                   label = "Select the bird to view its path",
                                                   choices = list("Argentina","Butterball","Disney","Domingo","Irma","LaPampa","Leo","Mac","Mark","Mary","Morongo","Prado","Rosalie","Sarkis","Schaumboch","Steamhouse1","Steamhouse2","Whitney","YoungLuro"),
                                                   selected = "Argentina"
                                                   
                                )
                                
                                
                              ),
                              mainPanel(
                                leafletOutput("mymap"),
                                br(),
                                #text below the plit
                                textOutput("Plot21text"),
                                br(),
                                textOutput("Plot22text")
                                
                                
                              )
                            )
                 ),tabPanel("Plot3",
                            #tab 3
                            titlePanel("BIRD POSITION AT A PARTICULAR YEAR AND MONTH"),
                            sidebarLayout(
                              sidebarPanel(
                                radioButtons("birdnew_selected",
                                             label = "Select the bird to view its path",
                                             choices = list("Argentina","Butterball","Disney","Domingo","Irma","LaPampa","Leo","Mac","Mark","Mary","Morongo","Prado","Rosalie","Sarkis","Schaumboch","Steamhouse1","Steamhouse2","Whitney","YoungLuro")
                                ),
                                uiOutput("First"),
                                uiOutput("Second")
                                
                              ) ,
                              mainPanel(
                                
                                leafletOutput("bird")
                              )
                              
                            )
                            
                 ),tabPanel("Plot4",
                            #tab 4
                            fluidRow(
                              column(3,
                                     h2("STORY OF THE MORONGO",align = "center"),
                                     
                                     sliderInput("slider","Select years",min = 2006, max=2009,value = 1),
                                     uiOutput("Third")
                                     
                              ),
                              column(8,
                                     
                                     leafletOutput("mymapmoron")
                                     
                              )
                            ),
                            fluidRow(
                              column(3,
                                     h5("Morongo is a bird belonging to the west coast population"),
                                     strong("Morongo is lost")
                                     ),
                              column(6,
                                     div("Morongo spends his summer breeding in the grounds of city Oregon before the long trip south. He travels over California ,Mexico towards Guatemala . He spends his winter there.During this period he doesnt travel much as he has no kids to feed. when the winter is about to end he starts his journey back home via the same route. This pattern was followed in both 2006 and 1007. But in 2008 while he was starting his journey down south something made him change direction and instead of going in the same old route through mexico, he goes down the California Peninsula and reaches a dead end. After awhile he ran out of land . We can see that he spent over 2 months trying to figure out his way back.After a long period he found his original route and headed back to south.This ofcourse meant he had very less time on the non breeding grounds and starts his journey back home.Did getting lost affect him? did the lack of rest during the winter affect his breeding? We will never know ")
                                     
                                     
                                     )
                            )
                 )
)

server <- function(input, output) {
  #tab4 to choose the color based on the year
  getColor <- function(turkey_morongo){
    
    if(input$slider == 2006){
      "red"
    }else if(input$slider == 2007){
      "violet"
    }else if(input$slider == 2008){
      "green"
    }else if(input$slider == 2009){
      "cadetblue"
    }}
  
  #tab 3 year displayed depends on the bird
  output$First <- renderUI({
    selectInput("monthyear_selected",
                label = "Select the year of travel",
                choice = unique(turkey_loc[turkey_loc$individual_local_identifier == input$birdnew_selected  ,"year1"])
    )
    
    
  })
  #month depends on the bird and year selected
  output$Second <- renderUI({
    selectInput("month_selected",
                label = "Select the month of travel",
                choice = unique(turkey_loc[(turkey_loc$individual_local_identifier == input$birdnew_selected)&(turkey_loc$year1 == input$monthyear_selected) ,"month1"])
                
    )
  })
  
  
  
  
  
  #tab1 function for km or miles
  dat_year <- reactive({
    if(input$distMeasure_selected == "Km"){
      #if km is selected
      turkey_year_fil <- filter(turkey_year,year1 %in% input$year_selected)
      turkey_year_agr <- aggregate(distance~individual_local_identifier+year1, turkey_year_fil, sum)
      return(turkey_year_agr)}else{
        
        #if miles is selcted the distance value mulltiplied with 0.62137
        turkey_year_fil <- filter(turkey_year,year1 %in% input$year_selected)
        turkey_year_fil$distance = turkey_year_fil$distance * 0.62137
        turkey_year_agr <- aggregate(distance~individual_local_identifier+year1, turkey_year_fil, sum)
        return(turkey_year_agr)
      }
  })
  #tab2 filtering the dataframe based on the bird selected
  dat_loc <- reactive({
    turkey_loc_new <- filter(turkey_loc,individual_local_identifier %in% input$bird_selected)
    return(turkey_loc_new)
    
  })
  
  #tab 3 filtering the 
  year_mon <- reactive({
    yearmon <- filter(turkey_loc,individual_local_identifier == input$birdnew_selected)
    
    return(yearmon)
  })
  year_mon1 <- reactive({
    yearfilt <- filter(year_mon(),month1 == input$month_selected & year1 == input$monthyear_selected)
    
    return(yearfilt)
    
    
  })
  #tab1
 output$wplot<-  renderPlotly({
   plot_ly(aggweight,x=~population,y=~animal_mass,type='bar',marker = list(color = c('rgba(100,22,35,0.7)', 'rgba(222,45,38,0.8)','rgba(204,204,204,1)')))
  #plotly for plotting the mass
   
 })
 
 
  
  output$distyearPlot <- renderPlot({
    plot1 <- ggplot(dat_year(), aes(x =  individual_local_identifier , y = distance, fill = factor(year1))) + geom_bar(stat = "identity") +  coord_flip() + labs(title = "Distance travelled by Animal per year",y = "Distance", x = "Bird Name", fill = "year") 
    plot1  #+  theme_solarized_2(light = FALSE) +      scale_colour_solarized("blue")
    
  })
  #Tab2 Map output
  output$mymap  <- renderLeaflet({
    pal <- colorFactor(
      palette = c('coral2','gray','darkorange','deeppink','blue','darkgreen','hotpink','skyblue','yellow','violet','red','maroon','purple','lawngreen','gold','cyan','brown','azure','aquamarine'),
      domain = turkey_loc$individual_local_identifier
    )
    m <- leaflet(dat_loc())
    m <- addProviderTiles(m,provider = providers$Esri.NatGeoWorldMap,options = providerTileOptions(opacity = 0.7))
    m <- addProviderTiles(m,provider = providers$Esri.WorldImagery,options = providerTileOptions(opacity = 0.7))
    m %>% addCircles(~location_long, ~location_lat,  weight = 3, radius=40,color= ~pal(individual_local_identifier), stroke = TRUE, fillOpacity = 0.8)
    
    
  })
  #text for tab 2
  output$Plot21text <- renderText({ 
    " * Turkey Vultures from the interior have the longest migration path flying down from to canada to South America"
  })
  output$Plot22text <- renderText({ 
    "* East coast vultures travelled shorter distances from the tri state  down to Florida. Part of this restriction is due to the fact that their migration corridor ends at tip of Florida Peninsula and they cant fly long distances over the water.Interestingly they tend to be largest in mass"
  })
  #output of tab 3
  output$bird   <- renderLeaflet({
    #icon as marker
    hopkinsIcon <- iconList(
      
      Steamhouse1  = makeIcon("http://icons.iconarchive.com/icons/femfoyou/angry-birds/24/angry-bird-icon.png",iconWidth = 61*215/230, iconHeight = 61),
      Steamhouse2 = makeIcon("http://icons.iconarchive.com/icons/femfoyou/angry-birds/24/angry-bird-green-icon.png",iconWidth = 61*215/230, iconHeight = 61),
      Mac = makeIcon("http://icons.iconarchive.com/icons/femfoyou/angry-birds/24/angry-bird-black-icon.png",iconWidth = 61*215/230, iconHeight =61),
      Leo = makeIcon("http://icons.iconarchive.com/icons/femfoyou/angry-birds/24/angry-bird-red-icon.png",iconWidth = 61*215/230, iconHeight = 61),
      Butterball = makeIcon("http://icons.iconarchive.com/icons/femfoyou/angry-birds/24/angry-bird-white-icon.png",iconWidth = 61*215/230, iconHeight = 61),
      Schaumboch = makeIcon("http://icons.iconarchive.com/icons/sirea/angry-birds/24/Bird-blue-icon.png",iconWidth = 61*215/230, iconHeight = 61),
      Mark = makeIcon("http://icons.iconarchive.com/icons/femfoyou/angry-birds/24/angry-bird-yellow-icon.png",iconWidth = 61*215/230, iconHeight = 61),
      Disney = makeIcon("http://icons.iconarchive.com/icons/sirea/angry-birds/24/Bird-white-icon.png",iconWidth = 61*215/230, iconHeight = 61),
      Mary = makeIcon("http://icons.iconarchive.com/icons/femfoyou/angry-birds/24/angry-bird-blue-icon.png",iconWidth = 61*215/230, iconHeight = 61),
      Morongo = makeIcon("http://icons.iconarchive.com/icons/sirea/angry-birds/24/Bird-black-icon.png",iconWidth =61*215/230, iconHeight = 61),
      Sarkis = makeIcon("http://icons.iconarchive.com/icons/sirea/angry-birds/24/Bird-red-icon.png",iconWidth = 61*215/230, iconHeight = 61),
      Rosalie = makeIcon("http://icons.iconarchive.com/icons/sirea/angry-birds/24/Bird-yellow-icon.png",iconWidth = 61*215/230, iconHeight = 61),
      Irma = makeIcon("http://icons.iconarchive.com/icons/mattahan/ultrabuuf/24/Things-Bird-icon.png",iconWidth =61*215/230, iconHeight = 61),
      LaPampa = makeIcon("http://icons.iconarchive.com/icons/musett/titto-the-dodo/24/The-Extinct-Flightless-Adium-Bird-icon.png",iconWidth = 61*215/230, iconHeight = 61),
      Whitey = makeIcon("http://icons.iconarchive.com/icons/musett/titto-the-dodo/24/The-Extinct-Flightless-Pidgin-Bird-icon.png",iconWidth = 61*215/230, iconHeight = 61),
      YoungLuro = makeIcon("http://icons.iconarchive.com/icons/gianni-polito/colobrush/24/software-songbird-icon.png",iconWidth = 61*215/230, iconHeight = 61),
      Prado = makeIcon("http://icons.iconarchive.com/icons/musett/titto-the-dodo/24/The-Extinct-Flightless-Twitter-Bird-icon.png",iconWidth = 61*215/230, iconHeight = 61),
      Argentina = makeIcon("http://icons.iconarchive.com/icons/artdesigner/tweet-my-web/24/flying-bird-sparkles-icon.png",iconWidth = 61*215/230, iconHeight = 61),
      Domingo = makeIcon("http://icons.iconarchive.com/icons/designbolts/rio-2-movie/24/Rio2-Pedro-icon.png",iconWidth = 61*215/230, iconHeight = 61)
      
    )
    
    
    new_frame <- year_mon1()
    #drawing the leaflet
    n <- leaflet(new_frame[1,],options = leafletOptions(zoomControl = FALSE,minZoom = 1, maxZoom = 2) )
    n <- addProviderTiles(n,provider = providers$Esri.NatGeoWorldMap,options = providerTileOptions(opacity = 0.7))
    n <- addProviderTiles(n,provider = providers$Esri.WorldImagery,options = providerTileOptions(opacity = 0.7))
    #n %>% addAwesomeMarkers(~location_long, ~location_lat, icon=icons)
    n  %>% addMarkers(~location_long,~location_lat,icon =~hopkinsIcon[individual_local_identifier] )
    
    
  })
  #tab 4 month depends on the year selected
  output$Third <- renderUI({
    selectInput("select_month",
                label = "Select the month of travel",
                choice = unique(turkey_morongo[turkey_morongo$year1 == input$slider,"month1"])
    )
    
    
  })
  #tab 4 filtering the dataframe based on the year and month selected
  moron <- reactive({
    yearfilt <- filter(turkey_morongo,month1 == input$select_month & year1 == input$slider)
    
    return(yearfilt)
    
    
  })
  #plotting the tab4 graph 
  output$mymapmoron <- renderLeaflet({
    m <- leaflet(moron(),options = leafletOptions(zoomControl = FALSE,minZoom = 1, maxZoom = 8))
    m <- addProviderTiles(m,provider = providers$Stamen.Toner,options = providerTileOptions(opacity = 0.7))
    # m <- addProviderTiles(m,provider = providers$Esri.WorldImagery,options = providerTileOptions(opacity = 0.7))
    m %>% addPolylines(~location_long, ~location_lat ,color = getColor())
    
  })
}

# Run the app ----
shinyApp(ui = ui, server = server)




