################################################################################
#                   Author: Joshua Thompson
#   O__  ----       Email:  joshuajamesdavidthompson@gmail.com
#  c/ /'_ ---
# (*) \(*) --
# ======================== Script  Information =================================
# PURPOSE: Rainfall Widget using API
#
# PROJECT INFORMATION:
#   Name: Rainfall Widget using API
#
# HISTORY:----
#   Date		        Remarks
#	-----------	   ---------------------------------------------------------------
#	 03/01/2023   Created script                                   JThompson (JT)
#===============================  Environment Setup  ===========================
#==========================================================================================

if(!require(shiny)){
  install.packages("shiny")
  library(shiny) #'*1.7.1* <---  shiny version
}

if(!require(gfonts)){
  install.packages("gfonts")
  library(gfonts) #'*0.1.3* <---  gfonts version
}

if(!require(stringr)){
  install.packages("stringr")
  library(stringr) #'*1.4.0* <---  stringr version
}

if(!require(tidyverse)){
  install.packages("tidyverse")
  library(tidyverse) #'*1.3.1* <---  tidyverse version
}

if(!require(magrittr)){
  install.packages("magrittr")
  library(magrittr) #'*2.0.2* <---  magrittr version
}

if(!require(tidyselect)){
  install.packages("tidyselect")
  library(tidyselect) #'*1.1.1* <---  tidyselect version
}

if(!require(wesanderson)){
  install.packages("wesanderson")
  library(wesanderson) #'*0.3.6* <---  wesanderson version
}

if(!require(leaflet)){
  install.packages("leaflet")
  library(leaflet) #'*2.0.4.1* <---  leaflet version
}

if(!require(ggtext)){
  install.packages("ggtext")
  library(ggtext) #'*'0.1.1'* <---  ggtext version
}

if(!require(sf)){
  install.packages("sf")
  library(sf) #'*'1.0.5'* <---  sf version
}

if(!require(sp)){
  install.packages("sp")
  library(sp) #'*'1.4.6'* <---  sp version
}

if(!require(DT)){
  install.packages("DT")
  library(DT) #'*'0.20'* <---  DT version
}

if(!require(rmapshaper)){
  install.packages("rmapshaper")
  library(rmapshaper) #'*'0.4.5'* <---  rmapshaper version
}

if(!require(shinycssloaders)){
  install.packages("shinycssloaders")
  library(shinycssloaders) #'*'1.0.05'* <---  shinycssloaders version
}

if(!require(waiter)){
  install.packages("waiter")
  library(waiter) #'*'0.2.5'* <---  waiter version
}

if(!require(Cairo)){
  install.packages("Cairo")
  library(Cairo) #'*'1.5.14'* <---  Cairo version
}

if(!require(leaflet.extras)){
  install.packages("leaflet.extras")
  library(leaflet.extras) #'*'1.0.0'* <---  leaflet.extras version
}

if(!require(tinytex)){
  install.packages("tinytex")
  library(tinytex) #'*'0.38'* <---  tinytex version
}

if(!require(rmarkdown)){
  install.packages("rmarkdown")
  library(rmarkdown) #'*'2.11'* <---  rmarkdown version
}

if(!require(gfonts)){
  install.packages("gfonts")
  library(gfonts) #'*'0.1.3'* <---  gfonts version
}

if(!require(shinyjs)){
  install.packages("shinyjs")
  library(shinyjs) #'*'2.1.0'* <---  shinyjs version
}

if(!require(shinyalert)){
  install.packages("shinyalert")
  library(shinyalert) #'*'3.0.0'* <---  shinyalert version
}

if(!require(shinyBS)){
  install.packages("shinyBS")
  library(shinyBS) #'*'0.61'* <---  shinyBS version
}

if(!require(lubridate)){
  install.packages("lubridate")
  library(lubridate) #'*'1.9.2'* <---  lubridate version
}

if(!require(xml2)){
  install.packages("xml2")
  library(xml2) #'*'1.3.3'* <---  xml2 version
}

if(!require(plotly)){
  install.packages("plotly")
  library(plotly) #'*'4.10.1'* <---  plotly version
}


ui <- fluidPage(
  use_font("palanquin", "www/css/palanquin.css"),
  mainPanel(plotlyOutput('rainplot')%>% withSpinner(color="black")),
  br(),br(),
  fluidRow(
    column(8, htmlOutput("rainfall"),align="center")      
  )
)

server <- function(input, output, session) {
  
  #########################################
  # query api for xml based on systemtime #
  #        extract as data frame          #
  #########################################
  #Sys.setenv(TZ='America/New York')
  d.now <- lubridate::floor_date(lubridate::now(),"5 mins")#-lubridate::hours(5)
  print(paste0("The time in ET should be: ",d.now))
  d.72  <- d.now-lubridate::hours(72)
  xml_address <- paste0("https://monitormywatershed.org/wofpy/rest/1_1/GetValues?location=envirodiy:BWPR-RIVA-01&variable=envirodiy:WEATHERtronics_6011_RainDepth&startDate=",paste0(lubridate::as_date(d.72),"T",sprintf("%02d:%02d:%02d", lubridate::hour(d.72), lubridate::minute(d.72), lubridate::second(d.72))),"&endDate=", paste0(lubridate::as_date(d.now),"T",sprintf("%02d:%02d:%02d", lubridate::hour(d.now), lubridate::minute(d.now), lubridate::second(d.now))))
  print(xml_address)
  doc <- xml2::read_xml(xml_address)
  values <- xml2::xml_find_all(doc, "//d1:values")  
  query_res <- 
    dplyr::tibble(
      dateTimeUTC = values %>% xml2::xml_find_all("./d1:value") %>% xml2::xml_attr("dateTimeUTC"),
      dateTime = values %>% xml2::xml_find_all("./d1:value") %>% xml2::xml_attr("dateTime"),
      precip_mm = values %>% xml2::xml_find_all("./d1:value") %>% xml2::xml_text()
    ) %>%
    dplyr::mutate(dateTimeUTC = lubridate::as_datetime(dateTimeUTC),
                  dateTime = lubridate::as_datetime(dateTime)-lubridate::hours(5)) # this is now eastern time
  print(query_res)

  #########################################
  #   make plot and create plotly plot    #
  #########################################
  
  p <- ggplot2::ggplot(query_res %>% dplyr::mutate(Title = "72-hr Rainfall Record at BWPR",
                                                   `Precipitation (in)` = round(as.numeric(precip_mm)/25.4,2)), 
                       aes(x=dateTime, y=`Precipitation (in)`)) + 
    facet_wrap(~Title,ncol=1) +
    geom_bar(stat="identity",fill="#0434a4") + 
    guides(fill=guide_legend("")) +
    xlab("") +
    theme(legend.position = "None",axis.text.x = element_text(angle = 0, vjust = 0, hjust=0.5,colour="black"),
          strip.background = element_rect(fill="#0072BC"),
          strip.text = element_text(colour = 'white',face="bold",size = 12),
          panel.background = element_blank(),
          panel.grid.major.x = element_line(size = 0.5, linetype = 'solid',
                                            colour = "lightgrey"),
          panel.grid.major.y = element_line(size = 0.5, linetype = 'solid',
                                            colour = "lightgrey"),
          panel.border=element_rect(colour="#0072BC",size=1,fill=NA),
          axis.text.y=element_text(colour="black"),
          legend.background = element_rect(colour = 'black', fill = 'white', linetype='solid'), legend.spacing.x = unit(0, "pt"),legend.margin=margin(t=-0.5,l=0.05,b=0.05,r=0.05, unit='cm'))
  output$rainplot <- plotly::renderPlotly(plotly::ggplotly(p))
  
  #########################################
  #  get rainfall totals for text output  #
  #########################################
   
  total24 <- query_res %>% dplyr::filter(dateTime > lubridate::now()-lubridate::hours(24+5)) %>% dplyr::summarize(rain=sum(as.numeric(precip_mm)/25.4,na.rm = T)) %>% dplyr::pull(rain)
  
  total72 <- query_res %>% dplyr::summarize(rain=sum(as.numeric(precip_mm)/25.4,na.rm = T)) %>% dplyr::pull(rain)
  
  maxhr <- query_res %>% dplyr::filter(dateTime > lubridate::now()-lubridate::hours(24+5)) %>% 
    dplyr::mutate(date=lubridate::as_date(dateTime),
                  hour=lubridate::hour(dateTime)) %>%
    dplyr::group_by(date,hour) %>%
    dplyr::summarise(hr_precip = sum(as.numeric(precip_mm)/25.4,na.rm=T)) %>% dplyr::pull(hr_precip) %>% max()
  
  output$rainfall <- renderText({paste0("<b>The total rainfall in the past 72 hours was ", round(total72,2), 
                                        " inches, with ", round(total24,2), " inches in the past 24 hours. The max hourly rainfall rate in the past 24 hours was ",
                                        round(maxhr,2), " inches per hour. Data are provisional.</b>")})
   
}

shinyApp(ui, server)