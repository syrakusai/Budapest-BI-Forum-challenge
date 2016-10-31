rm(list=ls())

library(shiny)
library(dplyr)
library(leaflet)
library(ggplot2)

this.dir = dirname(parent.frame(2)$ofile)
setwd(this.dir)

print("Loading data")

data <- read.csv("ingatlan.com_dataviz_challenge_2016_listings.csv", header=TRUE, sep=";", 
                 na.strings = c("NA", "N/A", "", "missing_info"))

print("Doing some cleaning")

#select hot-spots

city_groupedData <- group_by(data, city)
city_groupedData <- summarise(city_groupedData, length(city))
names(city_groupedData) <- c("city", "n")
city_groupedData <- filter(city_groupedData, n>=100)

badCityNames <- as.character(unique(city_groupedData$city))
badCityNames <- badCityNames[!is.na(badCityNames)]

data <- filter(data, data$city %in% badCityNames)

#clear hot-spot city names and their coordinates

cityNamesForMerge<-data.frame(badCityNames)
goodCityNames <- c("Erd","Bekescsaba","Balatonfured","Budaors","Budakeszi",
                   "Budapest I.","Budapest II.","Budapest III.","Budapest IV.","Budapest IX.",
                   "Budapest V.","Budapest VI.","Budapest VII.","Budapest VIII.","Budapest X.",
                   "Budapest XI.","Budapest XII.","Budapest XIII.","Budapest XIV.","Budapest XIX.",
                   "Budapest XV.","Budapest XVI.","Budapest XVII.","Budapest XVIII.","Budapest XX.",
                   "Budapest XXI.","Budapest XXII.","Budapest XXIII.",
                   "Debrecen","Dorog",             
                   "Dunaujvaros","Dunakeszi","Eger","Esztergom","Fot",              
                   "Godollo","Gyor","Gyongyos","Heviz","Hajduszoboszlo",  
                   "Halasztelek","Hatvan","Kaposvar","Kazincbarcika","Kecskemet",        
                   "Keszthely","Kiskunfelegyhaza","Kistarcsa","Komlo","Miskolc",           
                   "Mosonmagyarovar","Nagykanizsa","Nyiregyhaza","Oroszlany","Pecs",             
                   "Pomaz","Salgotarjan","Siofok","Sopron","Szazhalombatta",   
                   "Szekesfehervar","Szeged","Szekszard","Szentendre","Szigetszentmiklos",
                   "Szolnok","Szombathely","Tokol","Tapolca","Tata",              
                   "Tatabanya","Tiszaujvaros","Vac","Varpalota","Vecses",           
                   "Veresegyhaz","Veszprem","Zalaegerszeg")
#because it might give different results from time to time, the coordinates found on console by 
#geocode<-dismo::geocode
#cityLonLat<-geocode(c(goodCityNames))
CityLon<-c(18.90454, 21.08773, 17.88512, 18.95296, 18.92784, 
           19.037458, 18.986934, 19.036852, 19.095212, 19.091623, 19.052018, 19.067763, 19.073376, 19.084543, 19.157503, 
           19.018739, 18.990459, 19.070927, 19.114709, 19.143015, 19.13254, 19.191941, 19.266465, 19.209843, 19.119317, 19.066142, 19.000056, 19.122523, 
           21.62731, 18.72974,
           18.93552, 19.13971, 20.37723, 18.74345, 19.18917, 19.36054, 17.65040, 19.92949, 17.18669, 21.39655, 18.98176, 19.67667, 17.79676, 20.61898, 19.68969,
           17.24796, 19.85152, 19.26103, 20.44166, 20.77844, 17.26892, 16.98968, 21.72441, 18.31550, 18.23227, 19.02180, 19.79998, 18.07462, 16.58448,
           18.90967, 18.42214, 20.14143, 18.70623, 19.06686, 19.03518, 20.18247, 16.62184, 18.96752, 17.43711, 18.33030, 18.40482, 21.04274, 19.13518, 18.16141,
           19.26244, 19.28026, 17.90930, 16.84163)
CityLat<-c(47.39197, 46.67359, 46.95990, 47.46214, 47.51362, 
           47.496822, 47.5393329, 47.567177, 47.577756, 47.464928, 47.500232, 47.508064, 47.502729, 47.488749, 47.482091,
           47.45931, 47.49912, 47.535511, 47.522457, 47.445729, 47.562723, 47.518303, 47.475415, 47.428123, 47.433288, 47.424358, 47.410542, 47.39396, 
           47.53160, 47.72364,
           46.96191, 47.63438, 47.90253, 47.78839, 47.61725, 47.60077, 47.68746, 47.77727, 46.79029, 47.44354, 47.36309, 47.66570, 46.35936, 48.24894, 46.89637,
           46.76547, 46.71128, 47.54464, 47.60140, 48.10348, 47.86815, 46.45902, 47.94953, 47.48588, 46.07273, 47.64564, 48.09352, 46.90906, 47.68166,
           47.30833, 47.18603, 46.25301, 46.34743, 47.67953, 47.33960, 47.16214, 47.23069, 47.31871, 46.88345, 47.64582, 47.56925, 47.91598, 47.78418, 47.20056,
           47.40689, 47.65425, 47.10281, 46.84169)
cityNamesForMerge <- cbind(cityNamesForMerge, goodCityNames, CityLon, CityLat) 
names(cityNamesForMerge) <- c("city", "City", "CityLon", "CityLat")

#merge all above to data

data <- merge(data,cityNamesForMerge, by.y="city")

#make some further cleaning

data <- select(data, -(city:nr), -(postcode:property_type), -is_rent_right)
data <- filter(data, City!="NA")

#change data prices to same unit

data$price_sqm_last_active_at[data$listing_type == "for sale"]<-1000*data$price_sqm_last_active_at[data$listing_type == "for sale"]

CityCoordinateData <- group_by(data, City) %>% summarise(mean(CityLon), mean(CityLat))
names(CityCoordinateData) <- c("Location", "Lon", "Lat")

# define a function for filtering outliers for trend analysis

print("Define necessary functions and vectors")

remove_outliers <- function(x, na.rm = TRUE, ...) {
  qnt <- quantile(x, probs=c(.05, .95), na.rm = na.rm, ...)
  H <- 1.5 * IQR(x, na.rm = na.rm)
  y <- x
  y[x < (qnt[1] - H)] <- NA
  y[x > (qnt[2] + H)] <- NA
  y
}

v_propCount_forSale<-c()
for (i in CityCoordinateData$Location) {
  v_propCount_forSale<-append(v_propCount_forSale, nrow(data[data$City == i & 
                             data$listing_type == "for sale",]))
}

v_trend_forSale<-c()
v_medPrice_forSale<-c()
for (i in CityCoordinateData$Location) {

  subdata=data[data$City == i & data$listing_type == "for sale",]
  
  groupedData <- group_by(subdata, created_at)
  groupedData <- summarise(groupedData, median(price_sqm_last_active_at))
  names(groupedData)<-c("date", "med")
  groupedData$med <- remove_outliers(groupedData$med)
  groupedData$date <- as.numeric(difftime(as.Date(groupedData$date), as.Date("2015-02-09"), units = "days"))
  
  lm = lm(med ~ date, data=groupedData) 
  coef <- coefficients(lm)
  
  trend = as.numeric(round(((coef[2]*30)/coef[1])*100, digits = 1))
  
  v_trend_forSale<-append(v_trend_forSale, trend)
  
  groupedData$med_corr<-groupedData$med+coef[2]*(max(groupedData$date)-groupedData$date)  
  medianPriceSQM = round(median(groupedData$med_corr, na.rm = TRUE), digits = 2)
  v_medPrice_forSale<-append(v_medPrice_forSale, medianPriceSQM)
  }

v_activeDay_forSale<-c()
for (i in CityCoordinateData$Location) {
  subdata=data[data$City == i & data$listing_type == "for sale",]
  v_activeDay_forSale<-append(v_activeDay_forSale, round(median(subdata$active_days), digits=0))
}

# Define UI for application that draws

ui <- shinyUI(fluidPage(
  
    titlePanel("Spatial Property Sales Statistics of Hungarian Hot-spot Settlements"),
  
    sidebarLayout(
      
      sidebarPanel(selectInput("citySelector", "Your city of interest", sort(unique(data$City))),
                   plotOutput("plot1", width = "100%", height = 310),
                   plotOutput("plot2", width = "100%", height = 310)),
   
      mainPanel(leafletOutput("map", width = "100%", height = 735))
                
      )))

# Define server logic required to draw

server <- shinyServer(function(input, output) {
  
  updateUI <- function(){
    
    city = input$citySelector
    
    print(paste("city",city))
    
    cityLon = unique(cityNamesForMerge[cityNamesForMerge$City == input$citySelector, 3])
    cityLat = unique(cityNamesForMerge[cityNamesForMerge$City == input$citySelector, 4])
    
    subdata=data[data$City == input$citySelector & data$listing_type == "for sale",]
    
    groupedData <- group_by(subdata, created_at)
    groupedData <- summarise(groupedData, median(price_sqm_last_active_at))
    names(groupedData)<-c("date", "med")
    groupedData$med <- remove_outliers(groupedData$med)
    groupedData$date <- as.numeric(difftime(as.Date(groupedData$date), as.Date("2015-02-09"), units = "days"))
    lm = lm(med ~ date, data=groupedData) 
    coef <- coefficients(lm)
    
    output$plot2 <- renderPlot({
      g<-ggplot() + 
        geom_histogram(data=subdata, aes(ad_view_cnt/active_days),fill = "red", alpha = 0.3) +
        geom_histogram(data=subdata, aes(active_days),fill = "blue", alpha = 0.3) +
        scale_x_log10(limits=c(1, 1000)) +
        labs(title = "histograms ", x = "red: add views/day \n blue: active days", y = "count")
      print(g)
    })
    
    output$plot1 <- renderPlot({           
      f<-ggplot(data = groupedData, aes(x = date, y = med)) +
          geom_point() +
          geom_abline(slope = coef[2], intercept = coef[1]) +
        annotate("text", x = 275, y = 950, label = paste0("y=", round(coef[2], digits=3), "*x+", round(coef[1], digits=0))) +
        labs(title = "daily median of price/sqm values \n [thousand HUF]", x = "days since 2015-02-09", y = "price/sqm") +
        scale_y_continuous(limits=c(0, 1000))
      print(f)
    })
    
    #this is a function for defning colors
      
    pal <- colorNumeric(
      palette = "YlOrRd",
      domain = v_trend_forSale
    )
    
    #and the map finally
    
    m<-leaflet() %>%
      addTiles() %>% 
      setView(cityLon, cityLat, zoom = 12) %>% 
      addCircleMarkers(CityCoordinateData$Lon, CityCoordinateData$Lat, 
                       radius = v_medPrice_forSale/25,
                       color = pal(v_trend_forSale), 
                       opacity=1, 
                       fillOpacity = 0.5,
                       popup = paste("<h4>",paste0(CityCoordinateData$Location, " (n=", v_propCount_forSale, ")"),
                                     paste("</h4><hr/>Median price/sqm [thousand HUF]: ",v_medPrice_forSale,
                                           "<br/>2015-2016 trend [%/month]: ",v_trend_forSale,"<br/>"))) %>%
      addLegend("topright", pal = pal, values = v_trend_forSale, title = "Price ~ Radius<br><br>Trend [%/month]")
                
    output$map <- renderLeaflet(m) 
  }
  
  observeEvent(input$citySelector, {
    updateUI()
    })
  })

# Run the application 

shinyApp(ui = ui, server = server)

