library(shiny)
library(shinydashboard)
library(readr)
library(dplyr)
library(rsconnect)
library(DT)
library(plotly)
library(devtools)
library(dashboardthemes)
require(maps)
require(mapdata)
library(ggplot2)
library(ggrepel)

packageVersion('plotly')

terrorbase=read_csv("globalterrorism.csv")


terrorcol= terrorbase %>%
  select (eventid,  iyear  , imonth, iday, country, country_txt, region_txt,city,
          latitude, longitude, attacktype1_txt,multiple, nkill,
          nwound, target1, gname, targtype1_txt,weaptype1_txt,claimmode_txt,
          motive,   summary) 
terrordata=terrorcol %>% 
  rename(Year= iyear,Month=imonth, Day=iday, countries=country,
         Country=country_txt, Region=region_txt,
         AttackType=attacktype1_txt,  Killed=nkill , Wounded=nwound, Target=target1,
         Group=gname,Target_type= targtype1_txt, Weapon_type=weaptype1_txt,Claimmode=claimmode_txt, Motive=motive ,
         Summary=summary
           
  )

# Define UI for application that draws a histogram
ui <- dashboardPage(   
    
dashboardHeader(title= "Terrorist Activities"),
    
dashboardSidebar(
      
      sidebarMenu(
        menuItem("Presentation", tabName = "present", icon=icon("bomb")),
        menuItem("Data",tabName ="datapage", icon = icon("database")  ),
        menuItem("Some shocking numbers", tabName="basicanal", icon = icon("dashboard")),
        menuItem("terrorism categories", tabName = "tercat", icon = icon("chart-pie")),
        menuItem("Over the years", tabName = "overyears", icon = icon("hourglass-half")),
        menuItem("Maps", tabName = "maps", icon = icon("globe"))
      )
      
),
dashboardBody(   shinyDashboardThemes(
  theme = "poor_mans_flatly"
),
      
      
 tabItems(
        tabItem(tabName = "present", 
                fluidPage(
                  h1("Terrorist Activities Around The World "),
                  p("Terrorism is not a 21st century phenomenon and has its roots 
                    in early resistance and political movements. The Sicarii were an early Jewish
                    terrorist organisation founded in the first century AD with the goal of
                    overthrowing the Romans in the Middle East. Judas of Galilee, leader of
                    the Zealots and a key influence on the Sicarii, believed that the Jews should
                    be ruled by God alone and that armed resistance was necessary. "),
                  
  
                
                  br(),
                  br(),
                  h3("Modern terrorism after the second world war"),
                  p("The use of terrorism to further a political cause has accelerated in recent years. 
                    Modern terrorism largely came into being after the Second World War with the rise of
                    nationalist movements in the old empires of the European powers."),
                  
                  h3("Terrorist Activities Dashboard"),
                  p(" The aim of this Dashboard is to explore the terrorist events around the world. In this dataset,
                 we will be exploring the terrorism attacks over the world from 1970-2017, finding the most affected countries and regions, 
                  the most notorious groups, their motives,etc.

                    The database used is -Terrorism around the world- that you can find in Kaggle."),
                  br()
                

                  
                  
                  
                  )
                
                ),
    tabItem(tabName = "datapage",
            
            tabBox(id="tabset1", height = "100%",width = "100%" ,
                   tabPanel("Data", 
                            fluidPage( box(status="success",width=12, DT::dataTableOutput("database")  ) )
                     
                   ),
                   tabPanel(  "Statistics", 
                              
                            fluidPage( box(status="success", title="Summary", width= 12, verbatimTextOutput("strdat") ))
                   )
              
               )
            ),
    tabItem(tabName = "basicanal",
                fluidPage(
        
        fluidRow( infoBoxOutput("attacksnumb"), infoBoxOutput("groupsnumb"), infoBoxOutput("killednumb") ),  
        fluidRow( valueBoxOutput("mostcount"), valueBoxOutput("mostkilled"), valueBoxOutput("mostgroup") ),
                  
         box(width=8,title = "Terrorist attacks by year ", status = "success", solidHeader = TRUE, plotlyOutput("yearattacks")),
         box(width = 4, "   The number of terrorist activities has increased sharply after 2012 and especially in 2014 when ISIS 
             the Salafi jihadist militant group gained global prominence. It drove Iraqi government forces out of key cities in its
             Western Iraq offensive, followed by its capture of Mosul and the Sinjar massacre. ", background = "teal") 
        
                         )
            ),
    
    tabItem(tabName = "tercat",
                
                            tabBox(id="tabset2", height = "100%",width = "100%" ,
                                   
                                   
                                      tabPanel("By region",
                                               fluidPage(
         box(width=12  ,title = "Terrorist attacks by region ", status = "success", solidHeader = TRUE, plotlyOutput("byregion")),
         box(width =12,status = "success", "Middle East and North Africa is the region that has experienced the highest terrorist attacks, followed 
          by South Asia, South America and Western Europe. 
             The Australian Region have experienced very few terrorist events. ")                            
           ) ),   
                                      tabPanel("By country", 
                                               fluidPage(
         box(width=12,title = "20 Countries with Highest Terrorist Attacks ", status = "success", solidHeader = TRUE, plotlyOutput("countattacks")), 
         box(width =12,status = "success" , "These are the 20 counties that have had the highest number of terrorist attacks starting 1972. 
             Iraq has witnessed a very large number of terrorist activities up to 26000 attacks, followed by Pakistan about 1400 attacks." )                                         
         ) ),
                                   
                                       tabPanel("By attack types",
                                               fluidPage(
         box(width = 3,status="success", "The world has witnessed many sort of terrotist attacks, with this data we can say that most of the attacks were done through bombs or explosions. 
             Armed assault attacks come in the second place. The third and fourth places are taken by respectively assassination and hostage taking.
             Facility and infrastructure attacks are also important since there are more than 10,000 terrorist attacks.") ,
         box(width=9  ,title = "Terrorist attacks by Attack type ", status = "success", solidHeader = TRUE, plotlyOutput("attackstype"))
                                           )   ),
                                      tabPanel("By target", 
                                               fluidPage(
         box(width=12  ,title = "Favorite terrorist targets ", status = "success", solidHeader = TRUE, plotlyOutput("targetattacks")),
         
         box(width =12,status = "success" , "The favorite terrorist target are the citizens and private properties
             followed by the military and then the police." )   
                                            )  ),
                                      tabPanel("By weapon",
                                               fluidPage(
         box(width=9  ,title = "Terrorist attacks by weapon ", status = "success", solidHeader = TRUE, plotlyOutput("weapattacks")),
         box(width =3,status = "success", "Half of the terrorist attacks are done using explosives. Up to 32% were done using firearms.  ")
                                          )    ),
                                      tabPanel("By Claiming mode",
                                              fluidPage(
         box(width =3,status = "success" , "The most common way for a group of terrorists to claim an attack is personal claim, followed
             by social media by posting it on a website or a blog. An important number of attacks were claimed by a call after the incident." ),  
         box(width=9  ,title = "Terrorist attacks by claiming mode ", status = "success", solidHeader = TRUE, plotlyOutput("claimmode"))
                    
                  )    )
                                      
                                     
         
                            )
         
                          
            ),
    
    tabItem(tabName = "overyears",
            
            tabBox(id="tabset3", height = "100%",width = "100%" ,
                   
                   
                   tabPanel("By region",
                            fluidPage(
                              box(width=12  ,title = "Terrorist attacks by region ", status = "success", solidHeader = TRUE, plotlyOutput("yearsbyregion"))
                            )   ),
                   tabPanel("By country", 
                            fluidPage(
                            box(width=12,title = "20 Countries with Highest Terrorist Attacks ", status = "success", solidHeader = TRUE, plotlyOutput("yearsbycount")) 
                  )  ),
                   
                   tabPanel("By attack types",
                            fluidPage(
                             
                              box(width=9  ,title = "Terrorist attacks by Attack type ", status = "success", solidHeader = TRUE, plotlyOutput("yearsbyattackstype"))
                              )   ),
                   tabPanel("By target", 
                            fluidPage(
                              box(width=12  ,title = "Favorite terrorist targets ", status = "success", solidHeader = TRUE, plotlyOutput("yearsbytargetattacks"))
                            )  ),
                   tabPanel("By weapon",
                            fluidPage(
                              box(width=9  ,title = "Terrorist attacks by weapon ", status = "success", solidHeader = TRUE, plotlyOutput("yearsbyweapon"))
                              
                            )    )
               
                   
                  
                   
                   )
            
            
                   ),
    
    tabItem("maps",
            
            
            
            
            tabBox(id="tabset2", height = "100%",width = "100%" ,
                   
                   
                   
                   tabPanel("Middle East & North Africa",
                            fluidPage(
                              box(width=12  , status = "success",  plotlyOutput("mapofnorthafrica"))
                            )   ),
                   
                   tabPanel("Western Europe", 
                            fluidPage(
                              box(width=12  ,title = " ", status = "success",  plotlyOutput("mapofeurope"))
                            )  ),
                   
                   
                   tabPanel("North America", 
                            fluidPage(
                            box(width=12  ,title = " ", status = "success",  plotlyOutput("mapofnorthamerica"))
                            
                            
                    ) ),
                   
                   
                   tabPanel("South Asia",
                            fluidPage(
                              box(width=12  ,title = " ", status = "success", plotlyOutput("mapofsouthasia"))
                              
                            )    ),
                   
                   
                   tabPanel("Tunisia",
                            fluidPage(
                              
                              box(width=12  ,title = " ", status = "success",  plotlyOutput("mapoftunisia"))
                            )    ),
                   
                   
                   tabPanel("France",
                            fluidPage(
                              
                              box(width=12  ,title = " ", status = "success",  plotlyOutput("mapoffrance"))
                            )    )
                   
            
                   
                   
                   
             
                 
            
            
            )
            
            
      
      
    )
    
    
    
    
    
    
    
        
   )
      
      
      
)
  
 
  
)

# Define server logic required to draw a histogram
server <- function(input, output) {
   

  
  
  
  output$database <- DT::renderDataTable({terrordata} ,extensions="Responsive",
                                         options = list(
                                           columnDefs = list(list(className = 'dt-center', targets = 5)),
                                           pageLength = 5,
                                           lengthMenu = c(5, 10, 15, 20), scrollX = TRUE)   )
  
 
  
  
  output$strdat= renderPrint ({
    summary(terrordata) })
  
  #############################################################################################
  
  numbattacks= reactive({
    terrordata %>%
      count %>%
      collect
    })
  output$attacksnumb= renderInfoBox({
    infoBox(
      numbattacks(), "Terrorist attacks", icon = icon("bomb"),
      color = "teal", fill=TRUE
    )
  })
  
  

  
  
  numbgroups=reactive({
    terrordata%>% 
      group_by(Group) %>%
      summarise(nn=n())%>%
      count%>%
      collect
  })
  
  
  output$groupsnumb= renderInfoBox({
    infoBox(
      numbgroups(), "Groups", icon = icon("user"),
      color = "olive", fill=TRUE
    )
  })
  

  numbkilled= reactive({
    terrordata %>%
      summarise(nn=sum(Killed, na.rm=TRUE)) %>%
      collect
  })
  
  
  output$killednumb= renderInfoBox({
    infoBox(
      numbkilled(), "Victims", icon = icon("male"),
      color = "teal", fill=TRUE
    )
  })
  
 
  
  output$mostcount=renderValueBox({
  ll= terrordata %>%
    group_by(Country) %>%
    summarise(nn=n()) %>%
     top_n(n=1) %>%
    collect
    
    valueBox(
      ll,"Country with Highest Attacks", icon = icon("place-of-worship", lib = "glyphicon"),
      color = "olive"
    )
   
  
  })
  
  output$mostkilled=renderValueBox({
    ll= terrordata %>%
     select(Country, Killed)%>%
      summarise(nn=max(Killed, na.rm = TRUE)) %>%
    
      collect
    
    valueBox(
      ll,"Maximum people killed in an attack", icon = icon("place-of-worship", lib = "glyphicon"),
      color = "teal"
    )
    
    
  })
  
  
  output$mostgroup=renderValueBox({
    ll= terrordata %>%
      group_by(Region) %>%
      summarise(nn=n()) %>%
      top_n(n=1) %>%
      collect
    
    valueBox(
      ll,"Regions with Highest Attacks", icon = icon("place-of-worship", lib = "glyphicon"),
      color = "olive"
    )
    
    
  })
  
  
  output$yearattacks= renderPlotly({
    ll=  terrordata%>% 
      group_by(Year) %>%
      summarise(nyears=n())%>%
      collect
    
    
      plot_ly(ll, x = ~Year, y = ~nyears, type= 'scatter', mode='lines+markers' ,
              marker = list(color = c('rgba(222,45,38,0.8)'
                                      
              ))) %>%  
      
      layout( title='', xaxis = list(title = 'Years'), yaxis =list(title= 'Number of attacks') )
    
  })
###################################################################################################
    
  
  output$byregion=renderPlotly({
    ss= terrordata %>%
      group_by(Region) %>%
      summarise(numberattacks=n()) %>%
      arrange(desc(numberattacks)) %>%
      collect
    
    plot_ly(ss, x = ~numberattacks, y = ~Region,  type = 'bar'  , orientation = 'h',
            marker = list(color = "cadetblue"  ) ) %>%
     
      layout(title = "", autosize = T,
             xaxis = list(title ="Number of terrorist attacks" ),
             yaxis = list(title = "By region"))
  })
  
  output$countattacks=renderPlotly({
    ss= terrordata %>%
      group_by(Country) %>%
      summarise(numberattacks=n()) %>%
      top_n(n=20) %>%
      arrange(numberattacks) %>%
      collect
    
    plot_ly(ss, x = ~Country, y = ~ss$numberattacks,  type = 'bar' ,
            marker = list(color = c('rgba(222,45,38,0.8)'
                                    
            ))) %>%
      layout(title = "", autosize = T,
             xaxis = list(title = "Country"),
             yaxis = list(title = "Number of terrorist attacks"))
    
    
  })
  
  
  
  output$attackstype=renderPlotly({
    ss= terrordata %>%
      group_by(AttackType) %>%
      summarise(numberattacks=n()) %>%
      arrange(desc(numberattacks)) %>%
      collect
    
    plot_ly(ss, x = ~AttackType, y = ~numberattacks,  type = 'bar' ,
            marker = list(color ="cadetblue") ) %>%
      
      
      layout(title = "", autosize = T,
             xaxis = list(title = "Attack type"),
             yaxis = list(title = "Number of terrorist attacks"))
  })
  
  
  output$weapattacks=renderPlotly({
    
    
    ss=terrordata %>%
      group_by( Weapon_type ) %>%
      summarise(numberattacks=n())%>%
      filter(Weapon_type!= "Vehicle (not to include vehicle-borne explosives, i.e., car or truck bombs)")
    collect
    
    plot_ly(ss, labels = ~Weapon_type, values = ~numberattacks,  type = 'pie' ,
            
            textposition = 'inside',
            textinfo = 'label+percent',
            insidetextfont = list(color = '#FFFFFF'),
            hoverinfo = 'text',
            text = ~paste( Weapon_type ,numberattacks ),
            
            marker = list(colors = colors,
                          line = list(color = '#FFFFFF', width = 1)), showlegend = TRUE ) %>%
      
      
      layout(title = "", autosize = T,
             xaxis = list(title = "Attack type"),
             yaxis = list(title = "Number of terrorist attacks"))
  })
  
  output$targetattacks=renderPlotly({
    ss= terrordata %>%
      group_by(Target_type) %>%
      summarise(numberattacks=n()) %>%
      arrange(desc(numberattacks)) %>%
      collect
    
    plot_ly(ss, x = ~numberattacks, y = ~Target_type,  type = 'bar'  , orientation = 'h',
            marker = list(color = "lightcoral"  ) ) %>%
      
      
      layout(title = "", autosize = T,
             xaxis = list(title ="Number of terrorist attacks"  ),
             yaxis = list(title ="Target type"))
  })
  

  
  output$claimmode=renderPlotly({
    ss= terrordata %>%
      group_by(Claimmode) %>%
      summarise(numberattacks=n()) %>%
      arrange(desc(numberattacks)) %>%
      collect
    
    plot_ly(ss, x = ~numberattacks, y = ~Claimmode,  type = 'bar'  , orientation = 'h',
            marker = list(color = "lightcoral"  ) ) %>%
      
      
      layout(title = "", autosize = T,
             xaxis = list(title ="Number of terrorist attacks"),
             yaxis = list(title ="Claiming mode"))
  })
  
  
  ###############################################################################
  
  output$yearsbyregion= renderPlotly({
    ll=  terrordata%>% 
      group_by( Region, Year) %>%
      summarise(nyears=n())%>%
     
      collect
    
    
    plot_ly(ll, x = ~Year, y = ~nyears , color=~Region) %>%  
      add_trace( mode='lines+markers') %>%
      layout(title = "", autosize = T,
             yaxis = list(title ="Number of terrorist attacks"))
      
   
  })

  output$yearsbycount= renderPlotly({
   gg=terrordata%>%
     group_by(Country) %>%
     summarise(nomb=n()) %>%
     top_n(15) %>%
     collect
    
     ll=  terrordata%>% 
      group_by( Country, Year) %>%
      summarise(nyears=n())%>%
      filter(Country %in% gg$Country) %>%
      collect
    
    
    plot_ly(ll, x = ~Year, y = ~nyears , color=~Country) %>%  
      add_trace( mode='lines+markers') %>%
      layout(title = "", autosize = T,
             yaxis = list(title ="Number of terrorist attacks"))
    
    
    
  }) 
  
  output$yearsbyattackstype = renderPlotly({
    ll=  terrordata%>% 
      group_by( AttackType, Year) %>%
      summarise(nyears=n())%>%
      
      collect
    
    
    plot_ly(ll, x = ~Year, y = ~nyears , color=~AttackType) %>%  
      add_trace( mode='lines+markers') %>%
      layout(title = "", autosize = T,
             yaxis = list(title ="Number of terrorist attacks"))
    
  }) 
  
  
  
  output$yearsbytargetattacks = renderPlotly({
    ll=  terrordata%>% 
      group_by( Target_type, Year) %>%
      summarise(nyears=n())%>%
      
      collect
    
    
    plot_ly(ll, x = ~Year, y = ~nyears , color=~Target_type) %>%  
      add_trace( mode='lines+markers') %>%
      layout(title = "", autosize = T,
             yaxis = list(title ="Number of terrorist attacks"))
    
    
    
  }) 

  
  
  output$yearsbyweapon = renderPlotly({
    ll=terrordata %>%
      group_by( Weapon_type , Year) %>%
      summarise(nyears=n())%>%
      filter(Weapon_type!= "Vehicle (not to include vehicle-borne explosives, i.e., car or truck bombs)")
    collect
    
    
    
    plot_ly(ll, x = ~Year, y = ~nyears , color=~Weapon_type) %>%  
      add_trace( mode='lines+markers') %>%
      layout(title = "", autosize = T,
             yaxis = list(title ="Number of terrorist attacks"))
    
    
    
  }) 
  
#################################################################################
  

  output$mapofnorthafrica = renderPlotly({
    ll=  terrordata%>% 
      select(Region, latitude, longitude, Country,Year, Group,  Killed) %>%
      filter(Region=='Middle East & North Africa') %>%
      collect 
    ll=na.omit(ll)
    
    m <- list(colorbar = list(title = "Years"))
    g <- list(
      scope = 'africa + asia',
      showland = TRUE,
      landcolor = toRGB("grey83"),
      subunitcolor = toRGB("white"),
      countrycolor = toRGB("white"),
      showlakes = TRUE,
      lakecolor = toRGB("white"),
      showsubunits = TRUE,
      showcountries = TRUE,
      resolution = 50,
      
      lonaxis = list(
        showgrid = TRUE,
        gridwidth = 0.5,
        range = c(-20, 60),
        dtick = 5
      ),
      lataxis = list(
        showgrid = TRUE,
        gridwidth = 0.5,
        range = c(19, 50),
        dtick = 5
      )
    )
    
    p <- plot_geo(ll, lat = ~as.numeric(latitude), lon = ~as.numeric(longitude), color=~Year) %>%
      add_markers(
        text = ~paste(ll$Country, ll$Year,"Group is: " ,ll$Group, ll$Killed, "killed" ), hoverinfo = "text"
      ) %>%
      layout(title = 'Terrorism in Middle East & North Africa', geo = g)
    
    
  }) 
  
  
  
  output$mapofeurope = renderPlotly({
    ll=  terrordata%>% 
      select(Region, latitude, longitude, Country,Year, Group,  Killed) %>%
      filter(Region=='Western Europe') %>%
      collect 
    ll=na.omit(ll)
    
    m <- list(colorbar = list(title = "Years"))
    g <- list(
      scope = 'europe',
      showland = TRUE,
      landcolor = toRGB("grey83"),
      subunitcolor = toRGB("white"),
      countrycolor = toRGB("white"),
      showlakes = TRUE,
      lakecolor = toRGB("white"),
      showsubunits = TRUE,
      showcountries = TRUE,
      resolution = 50,
      
      lonaxis = list(
        showgrid = TRUE,
        gridwidth = 0.5,
        range = c(-10, 45),
        dtick = 5
      ),
      lataxis = list(
        showgrid = TRUE,
        gridwidth = 0.5,
        range = c(35, 60),
        dtick = 5
      )
    )
    
    p <- plot_geo(ll, lat = ~as.numeric(latitude), lon = ~as.numeric(longitude), color=~Year) %>%
      add_markers(
        text = ~paste(ll$Country, ll$Year,"Group is: " ,ll$Group, ll$Killed, "killed" ), hoverinfo = "text"
      ) %>%
      layout(title = 'Terrorism in Europe', geo = g)
    
    
  }) 
  
  
 
  
  output$mapofnorthamerica = renderPlotly({
    ll=  terrordata%>% 
     select(Country, latitude, longitude, city,Year, Group, Killed) %>%
      filter(Country=='United States') %>%
      collect 
    ll=na.omit(ll)

    
    m <- list(colorbar = list(title = "Years"))
    
    # geo styling
    g <- list(
      scope = 'north america',
      showland = TRUE,
      landcolor = toRGB("grey83"),
      subunitcolor = toRGB("white"),
      countrycolor = toRGB("white"),
      showlakes = TRUE,
      lakecolor = toRGB("white"),
      showsubunits = TRUE,
      showcountries = TRUE,
      resolution = 50,
      projection = list(
        type = 'conic conformal',
        rotation = list(lon = -100)
      ),
      lonaxis = list(
        showgrid = TRUE,
        gridwidth = 0.5,
        range = c(-140, -55),
        dtick = 5
      ),
      lataxis = list(
        showgrid = TRUE,
        gridwidth = 0.5,
        range = c(20, 60),
        dtick = 5
      )
    )
    
    p <- plot_geo(ll, lat = ~as.numeric(latitude), lon = ~as.numeric(longitude), color= ~Year) %>%
      add_markers(
        text = ~paste(ll$city,ll$Year, ll$Group,  ll$Killed), hoverinfo = "text"
      ) %>%
      layout(title = 'Terrorism in USA', geo = g)
    
    
    
    
  }) 
  
  output$mapoftunisia = renderPlotly({
    ll=  terrordata%>% 
      select(latitude, longitude, Country, city, Year, Group) %>%
      filter(Country=='Tunisia') %>%
      collect 
    
    
    m <- list(colorbar = list(title = "Years"))
    g <- list(
      scope = 'tunisia',
      showland = TRUE,
      landcolor = toRGB("grey83"),
      subunitcolor = toRGB("white"),
      countrycolor = toRGB("white"),
      showlakes = TRUE,
      lakecolor = toRGB("white"),
      showsubunits = TRUE,
      showcountries = TRUE,
      resolution = 50,
      
      lonaxis = list(
        showgrid = TRUE,
        gridwidth = 0.5,
        range = c(6, 12),
        dtick = 5
      ),
      lataxis = list(
        showgrid = TRUE,
        gridwidth = 0.5,
        range = c(30, 38),
        dtick = 5
      )
    )
    
    p <- plot_geo(ll, lat = ~as.numeric(latitude), lon = ~as.numeric(longitude), color=~Group) %>%
      add_markers(
        text = ~paste(ll$city, ll$Year,"Group is: " ,ll$Group ), hoverinfo = "text"
      ) %>%
      layout(title = 'Terrorism in Tunisia', geo = g)
    
    
  }) 
  
  
  
  output$mapoffrance = renderPlotly({
    ll=  terrordata%>% 
      select(Country, latitude, longitude, city, Year, Group, Killed) %>%
      filter(Country=='France') %>%
      collect 
    ll=na.omit(ll)
    
    m <- list(colorbar = list(title = "Years"))
    g <- list(
      scope = 'france',
      showland = TRUE,
      landcolor = toRGB("grey83"),
      subunitcolor = toRGB("white"),
      countrycolor = toRGB("white"),
      showlakes = TRUE,
      lakecolor = toRGB("white"),
      showsubunits = TRUE,
      showcountries = TRUE,
      resolution = 50,
      
      lonaxis = list(
        showgrid = TRUE,
        gridwidth = 0.5,
        range = c(-5, 12),
        dtick = 5
      ),
      lataxis = list(
        showgrid = TRUE,
        gridwidth = 0.5,
        range = c(40, 52),
        dtick = 5
      )
    )
    
    p <- plot_geo(ll, lat = ~as.numeric(latitude), lon = ~as.numeric(longitude), color=~Year) %>%
      add_markers(
        text = ~paste(ll$city, ll$Year, ll$Group, ll$Killed ), hoverinfo = "text"
      ) %>%
      layout(title = 'Terrorism in France', geo = g)
    
    
  }) 
  
  output$mapofsouthasia = renderPlotly({
    ll=  terrordata%>% 
      select(Region, latitude, longitude, Country,Year, Group,  Killed) %>%
      filter(Region=='South Asia') %>%
      collect 
    ll=na.omit(ll)
    
    m <- list(colorbar = list(title = "Years"))
    g <- list(
      scope = 'asia',
      showland = TRUE,
      landcolor = toRGB("grey83"),
      subunitcolor = toRGB("white"),
      countrycolor = toRGB("white"),
      showlakes = TRUE,
      lakecolor = toRGB("white"),
      showsubunits = TRUE,
      showcountries = TRUE,
      resolution = 50,
      
      lonaxis = list(
        showgrid = TRUE,
        gridwidth = 0.5,
        range = c(60, 100),
        dtick = 5
      ),
      lataxis = list(
        showgrid = TRUE,
        gridwidth = 0.5,
        range = c(4, 40),
        dtick = 5
      )
    )
    
    p <- plot_geo(ll, lat = ~as.numeric(latitude), lon = ~as.numeric(longitude), color=~Year) %>%
      add_markers(
        text = ~paste(ll$Country, ll$Year,"Group is: " ,ll$Group, ll$Killed, "killed" ), hoverinfo = "text"
      ) %>%
      layout(title = 'Terrorism in South Asia', geo = g)
    
    
  }) 
  
  
  
  }

# Run the application 
shinyApp(ui = ui, server = server)

