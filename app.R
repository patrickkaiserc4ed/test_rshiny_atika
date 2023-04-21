## app.R ##

#### PREPARATION ####


#### Make sure all required packages are installed ####


# Function that loops over list of packages that should be installed --> checks if installed, install if not; load package in either case
# install_missing <- function(x){
#   for( i in x ){
#     #  require returns TRUE invisibly if it was able to load package
#     if( ! require( i , character.only = TRUE ) ){
#       #  If package was not able to be loaded then re-install
#       install.packages( i , dependencies = TRUE )
#       #  Load package after installing
#       require( i , character.only = TRUE )
#     }
#   }
# }


# Function that loops over list of packages and loads them
# load_pkgs <- function(x){
#   for( i in x ){
#     library(i)
#     }
#   }

# Packages required for script to run
# packages <- c("shinydashboard",
#               "RStata",
#               "shinyWidgets",
#               "haven",
#               "dplyr",
#               "ggplot2",
#               "ggtext",
#               "labelled",
#               "readxl",
#               "plotly",
#               "DT",
#               "stringr",
#               "lemon",
#               "rlist",
#               "reshape2",
#               "runner"
#               )


library("shinydashboard")
library("shiny")
library("RStata")
library("shinyWidgets")
library("haven")
library("dplyr")
library("ggplot2")
library("ggtext")
library("labelled")
library("readxl")
library("plotly")
library("DT")
library("stringr")
library("lemon")
library("rlist")
library("reshape2")
library("runner")


#### **** FOR DEPLOYMENT ****  
# Comment out 'install_missing()' fucntion above and this line below too
# install_missing( packages )

# Comment in 'load_pkgs'
# load_pkgs( packages )




#### C4ED-themed colour palette ####

c4edcolours <- c("#047B77", "#5CE2DD", "#1D5351", "#A0E2DF", "#022827", "#123787", "#6B92E4", "#26375C", 
                 "#A9BCE4", "#031A4A", "#0DA405", "#67EB60", "#2A6F26", "#AAEBA7", "#055A01")


#### Set WD ####

#Set working directory when executed standalone --> COMMENT OUT WHEN DEPLOYING APP

  username<-Sys.getenv("USERNAME")
  user_wd <- file.path("C:/Users",
                       paste(username, "/C4ED/CovidResponse - General/3_Dashboard", sep=""))

   setwd(user_wd)


#setwd("C:/Users/Marc Gillaizeau/Documents/C4ED - OWN/PROJECTS/P2064 - BMZ_Covid/3_Dashboard")

#### Import data ####

# Make sure that there is a column called "district" and one called "wave"
# Must make sure to save data in that folder when preparing in Stata
# Should be the only Stata dataset in forlder with name ending with "_dashboard"
datfile <- list.files(path="./Data", pattern="_dashboard.dta", full.names=TRUE)
dat <- read_dta(normalizePath(file.path(datfile)))

#extract variable labels make data nice for R
labels <- lapply(dat,attr,"label")
value_labels  <- lapply(dat,attr,"labels")
dat <- mutate_if(dat,is.labelled,to_factor)
dat <- zap_formats(as.data.frame(lapply(dat,remove_labels)))

# Question labels from questionnaire --> Stata dataset prepared before called "variable_notes"
qxfile <- list.files(path="./Data", pattern="variable_notes.dta", full.names=T)
questions <- read_dta(normalizePath(file.path(qxfile)))
qx.labs <- as.list(questions$note)
names(qx.labs) <- questions$varname
rm(questions)

#### List variables for each section of dashboard ####

# At a glance
#overall.vars <- c("r1","r2")
#c("r1","r2","r3")

# Health care
hc.vars <- c("r1", "r2", "r7a", "r7a1")


### ** Social distancing (prepare tabs)

# make a dataframe for the different tabs in the social distancing section
# Travel -> forms basis for dataframe
panel <- "Travel"
vars <- c("sd1", "sd2")
sd.panel.assign <- data.frame(vars,
                       Label = unlist(labels[vars],use.names=F),
                       Panel = panel)

# Family and Visits
panel <- "Family and Visits"
vars <- c("sd3", "sd4", "sd0c", "sd0g", "sd0b")
sd.panel.assign <- data.frame(rbind(sd.panel.assign,
                                    cbind(vars,
                       Label = unlist(labels[vars],use.names=F),
                       Panel = panel)))

# Work
panel <- "Work"
vars <- c("sd7")
sd.panel.assign <- data.frame(rbind(sd.panel.assign,
                                    cbind(vars,
                                          Label = unlist(labels[vars],use.names=F),
                                          Panel = panel)))

# Community
panel <- "Community"
vars <- c("sd9", "sd11", "sd12a", "sd12", "sd13")
sd.panel.assign <- data.frame(rbind(sd.panel.assign,
                                  cbind(vars,
                                        Label = unlist(labels[vars],use.names=F),
                                        Panel = panel)))



### ** Knowledge (prepare tabs)

# make a dataframe for the different tabs in the Knowledge section

# Awareness -> forms basis for dataframe
panel <- "Awareness"
vars <- c("a2", "a4")
kn.panel.assign <- data.frame(vars,
                              Label = unlist(labels[vars],use.names=F),
                              Panel = panel)

# Knowledge about Covid
panel <- "Knowledge about Covid"
vars <- c("a5", "a6", "a6a", "a6b", "a7")
kn.panel.assign <- data.frame(rbind(kn.panel.assign,
                                    cbind(vars,
                                          Label = unlist(labels[vars],use.names=F),
                                          Panel = panel)))

# Prevention of infection
panel <- "Prevention of infection"
vars <- c("a9", "a10", "a10_num")
#"a12", "a14", "a15", "a16", "a17", "a19")
kn.panel.assign <- data.frame(rbind(kn.panel.assign,
                                    cbind(vars,
                                          Label = unlist(labels[vars],use.names=F),
                                          Panel = panel)))

### ** Needs assessment (prepare tabs)
na.vars <- c("r15", "r16", "r19", "r17", "r18", "i4", "i6", "i7")
  

#### Date and Staus ####

# To show date of last update/deployment, store date the data file was last modified
# --> Not ideal: what if data did not change but content did (e.g. new graphs, new indicators)?
#current_time <- as.character(format(Sys.time(), '%d %B, %Y at %H:%M %Z'))
updateDate <- format(file.info(datfile)$mtime, "%d %B, %Y at %H:%M %Z")

# Store status of data collection:
# In Preparation, Ongoing, Completed, Halted
status <- "In Preparation"

####.####


#### APP STARTS HERE ####


#******************************************************
#### Setup HEADER ####
#******************************************************
dheader <- dashboardHeader(title ="COVID-19 Response",
                           dropdownMenu(type = "message",
                                        icon = icon("map-marked-alt"),
                                        headerText = "Provinces",
                                        badgeStatus = NULL,
                                        messageItem("Overall",
                                                    message = " ",
                                                    icon = icon(" ")),
                                        messageItem("Khyber Pakhtunkhwa ",
                                                    message = " ",
                                                    icon = icon(" ")),
                                        messageItem("Punjab ",
                                                    message = " ",
                                                    icon = icon(" ")),
                                        messageItem("Sindh",
                                                    message = " ",
                                                    icon = icon(" "))),
                           
                           dropdownMenu(type = "notifications",
                                        icon = icon('info-circle'),
                                        headerText = "Who we are",
                                        badgeStatus = NULL,
                                        notificationItem("C4ED",
                                                         icon = icon(" "),
                                                         href = "http://c4ed.org/"),
                                        notificationItem("ACTED Pakistan",
                                                         icon = icon(" "),
                                                         href = "https://www.acted.org/en/countries/pakistan/"),
                                        notificationItem("NRSP",
                                                         icon = icon(" "),
                                                         href = "https://nrsp.org.pk/")
                           )
                           
)

#******************************************************
#### Setup SIDEBAR ####
#******************************************************

# Set up Menu --> Tabs and sub-tabs
dsidebar <- dashboardSidebar(
  collapsed = T,
  sidebarMenu(
    tags$head(tags$style(HTML('.logo {
                              background-color: #047B77 !important;
                              }
                              .navbar {
                              background-color: #047B77 !important;
                              }
                              '))),
    menuItem("At a glance", tabName= "glance", icon= icon("far fa-chart-bar")),
    menuItem("Data Collection", tabName= "data", icon= icon("dashboard")),
    menuItem("Numbers", tabName = "numbers", icon = icon("sliders-h"),
             menuSubItem("Health Care", tabName = "hospital", icon= icon("hospital")),
             menuSubItem("Social Distancing", tabName = "socdist", icon= icon("handshake")),
             menuSubItem("Knowledge", tabName = "know", icon= icon("question")),
             menuSubItem("Needs Assessment", tabName = "needassess", icon= icon("exclamation-circle"))
             # menuSubItem("Awareness Campaign", tabName = "aware", icon= icon("broadcast-tower")),
             # menuSubItem("Automated COVID Response", tabName = "auto", icon= icon("phone"))
    ),
    # menuItem("Trends", tabName = "trends", icon = icon("chart-line")),
    menuItem("About Rapid Data for Action", tabName = "about", icon = icon("info-circle"))
  )
)

#**************************************************************
#### Setup CONTENT ####
#**************************************************************
# set up content of the dashboard

dbody <-  dashboardBody(
  
  #### * Website design ####
  
  # set font for document. -> this is not working yet...
  tags$head(
    tags$style(HTML("
                   label{font-family: Arial !important;}
                   input{font-family: Courier New !important;}
                   " 
    ))),
  
  # set color scheme to dark and c4ed colors
  
  
  # define C4ED colors and black background
  # this chunk defines the color scheme of statuses.
  # most items can have a status, which give items predefined colors
  # instead of using the predefined colors, we specify the C4ED colors here.
  # we redefine the color of statuses for primary and success.
  # if you would like to change it, simply echange the HEX color code behind the #
  
  # darker color -> primary 
  tags$style(HTML("
                .box.box-solid.box-primary>.box-header {
                color:#22222;
                background:#047B77;
                }
                .box.box-solid.box-primary{
                  border-bottom-color:#047B77;
                  border-left-color:#047B77;
                  border-right-color:#047B77;
                  border-top-color:#047B77;
                }
                .box{
                  color: #22222;
                  background-color: #fff;
                  border-top-color:#047B77;
                }
                ")),
  
  #lighter color -> success
  tags$style(HTML("
                  .box.box-solid.box-success>.box-header{
                  color:#22222;
                  background:#86CEE0;
                }
                .box.box-solid.box-success{
                  border-bottom-color:#86CEE0;
                  border-left-color:#86CEE0;
                  border-right-color:#86CEE0;
                  border-top-color:#86CEE0;
                }
                  ")),
  # this set color of box headers to white
  tags$style(HTML("
                  .box>.box-header{
                    color:#22222;
                  }
                  ")),
  
  # Some objects do not have a status argument, but rather a color argument
  # for these we redefine "teal" as C4ED blue and "light-blue" as C4ED light blue.
  
  # C4ED-blue -> teal
  tags$style(
    type = 'text/css', 
    '.bg-teal {background-color: #047B77!important; }'
  ),
  
  #C4ED light blue -> light-blue
  tags$style(
    type = 'text/css',
    '.bg-light-blue {background-color: #86CEE0!important;'
  ),
  
  # Lastly, we set the background color of the page
  # to a darker color. (there is no need of CSS and HTML here
  # because the function is built in to shiny)
  setBackgroundColor(
    color = "#ECF0F5",
    gradient = "radial",
    direction = "bottom",
    shinydashboard = TRUE
  ),
  
  
  #### * Website Content ####
  
  fluidPage(
    
    # this put logos on top and the red box
    fluidRow(
      column(width = 4,align = "center",
             div(id="c4ed", tags$a(href="http://c4ed.org", tags$img(src="C4ED_logo_long_nb.png", height="70", width="95")), style="display: inline-block; vertical-align:center;")
      ),
      column(width = 4,align = "center",
             div(id="acted", tags$a(href="http://www.acted.org/en/countries/pakistan/", tags$img(src="ACTED_logo_nb.png", height="60", width="120")), style="display: inline-block; vertical-align:top;")
      ),
      column(width = 4,align = "center",
             div(id="nrsp", tags$a(href="http://nrsp.org.pk", tags$img(src="NRSP_logo_nb.png", height="60", width="120")), style="display: inline-block; vertical-align:top;")
      ),
      div(id="space", style="display:inline-block;vertical-align:top; width: 20px;", HTML("<br>"))
      # box(width = 12,
      #     "DASHBOARD CURRENTLY UNDER CONSTRUCTION",
      #     background ="red")
          # "The figures and data currently displayed are subject to potential changes , which will be replaced with real data soon.")
    ),
    
    
    # Adapt the content of tabItems on the website
    tabItems(
      
      #### * At a glance ####
      
      tabItem(tabName = "glance",
              fluidPage(
                column(width = 12,style = "padding: 0px;",
                       box(width = 9,
                           tags$h2("Dashboard on COVID19-related needs in Pakistan")),
                       box(
                         width = 3,
                         collapsible = T,
                         collapsed = T,
                         status = "primary",
                         solidHeader = T,
                         title = "About",
                         includeHTML(normalizePath(file.path("./Text_files/overall_about.htm")))
                       )
                ),
                fluidRow(
                         column(width = 9, offset = 0, style = "padding: 0px;",
                                box(
                                  width = 12,
                                  column(
                                    width = 12,align = "center",
                                    conditionalPanel(
                                      condition = "input.typ == 'Table'",
                                      DT::dataTableOutput("overall_table")
                                    ),
                                    conditionalPanel(
                                      condition = "input.typ == 'Graph'",
                                      plotOutput("overall_graph")
                                      #tagAppendAttributes(textOutput("overall_caption"), style="white-space:pre-wrap;text-align:left")
                                    )
                                  ),
                                  div(id="space", style="display:inline-block;vertical-align:top; width: 20px;", HTML("<br>")),
                                  fluidRow(
                                    column(
                                      width = 2,
                                      selectInput("overall_wave",
                                                  "Wave:",
                                                  c("All",unique(as.character(dat$wave))),
                                                  selected = "All")
                                    ),
                                    column(width = 4,
                                        selectInput("overall_dist",
                                                    "District:",
                                                    choices = c("All",unique(as.character(dat$district_name))),
                                                    selected = "All")
                                    ),
                                    column(
                                      width = 4,
                                      selectInput("sub_group",
                                                  "By Sub-Group:",
                                                  c("All","By Gender", "By Age Group"),
                                                  selected = "All")
                                    ),
                                    column(
                                      width = 2,
                                      selectInput("typ",
                                                  "Type:",
                                                  c("Graph","Table"),
                                                  selected = "Graph")
                                    )
                                  )
                                )
                         ),
                         column(width = 3, offset = 0,style = "padding: 0px;",
                                box(width = 12, 
                                    title = "Last updated on",
                                    paste(updateDate)
                                    ),
                                valueBox(textOutput("overall_villages"),"Number of villages covered",color = "teal",width = 12),
                                valueBox(textOutput("overall_households"),"Number of households",color = "teal",width = 12),
                                valueBox(textOutput("overall_covered"),"Population covered",color = "teal",width = 12)
                                )
                                # box(
                                #   width = 12,
                                #   "again, this could be anything - Perhaps a trendline?"
                                # 
                                # )
                         ),
                column(width = 12,style = "padding: 0px;",
                       box(width = 6,
                           collapsible = T,
                           collapsed = T,
                           status = "primary",
                           solidHeader = TRUE,
                           title = "COVID-19 in low-income countries",
                           includeHTML(normalizePath(file.path("./Text_files/overall_covid_LICs.htm")))),
                       box(width = 6,
                           collapsible = T,
                           collapsed = T,
                           title = "COVID-19 in Pakistan",
                           status = "primary",
                           solidHeader = TRUE,
                           includeHTML(normalizePath(file.path("./Text_files/overall_covid_pakistan.htm")))
                       )),
                div(id="space", style="display:inline-block;vertical-align:top; width: 40px;", HTML("<br>"))
                
              )),
      
      
      #### * Data Collection Monitoring ####
      
      tabItem(tabName = "data",
              fluidPage(
                column(width = 12,style = "padding: 0px;",
                       box(width = 9,
                           tags$h2("Data Collection")),
                       box(width = 3, 
                           title = "Last updated on",
                           paste(updateDate)
                       )
                ),
                fluidRow(
                  column(width = 12,
                         tabBox(id = "data_tabs",
                                width = 12,
                                tabPanel("Daily progress",
                                         plotlyOutput("monitor_graph"),
                                         div(id="space", style="display:inline-block;vertical-align:top; width: 20px;", HTML("<br>")),
                                         htmlOutput("monitor_graph_note")
                                ),
                                tabPanel("Overall progress",
                                         DT::dataTableOutput("monitor_table"),
                                         div(id="space", style="display:inline-block;vertical-align:top; width: 20px;", HTML("<br>")),
                                         htmlOutput("monitor_table_note")
                                ),
                                div(id="space", style="display:inline-block;vertical-align:top; width: 20px;", HTML("<br>")),
                                selectInput("data_dist",
                                            "District:",
                                            choices = c("All",unique(as.character(dat$district_name))),
                                            selected = "All")
                         )
                )
                ),
                fluidRow(
                  box(width = 4,
                      collapsed = T,
                      collapsible = T,
                      status = "primary",
                      solidHeader = T,
                      title = "Covered Areas",
                      includeHTML(normalizePath(file.path("./Text_files/mo_areas.htm")))
                  ),
                  box(width = 4,
                      collapsed = T,
                      collapsible = T,
                      status = "primary",
                      solidHeader = T,
                      title = "Surveys in times of Corona",
                      includeHTML(normalizePath(file.path("./Text_files/mo_surveys.htm")))
                  ),
                  box(width = 4,
                      collapsed = T,
                      collapsible = T,
                      status = "primary",
                      solidHeader = T,
                      title = "Representativeness",
                      includeHTML(normalizePath(file.path("./Text_files/mo_quality.htm")))
                  )
                ),
                div(id="space", style="display:inline-block;vertical-align:top; width: 40px;", HTML("<br>"))
                
              )),
      
      
      #### * Numbers ####
      # Second tab content --> "Numbers"
      tabItem(tabName = "numbers"),
      
      #### ** Health Care ####
      tabItem(tabName = "hospital",
              fluidPage(
                column(width = 12,style = "padding: 0px;",
                       box(width = 9,
                           tags$h2("How do people care about their health?")),
                       box(width = 3, 
                           title = "Last updated on",
                           paste(updateDate)
                       )
                ),
                fluidRow(
                  column(width = 12,
                         column(width = 10, offset = 0, style = "padding: 0px;",
                                box(
                                  width = 12,
                                  column(
                                    width = 12,align = "center",
                                    conditionalPanel(
                                      condition = "input.hc_typ == 'Table'",
                                      DT::dataTableOutput("hc_table")
                                    ),
                                    conditionalPanel(
                                      condition = "input.hc_typ == 'Graph'",
                                      plotOutput("hc_graph")
                                    )
                                  ),
                                  div(id="space", style="display:inline-block;vertical-align:top; width: 20px;", HTML("<br>")),
                                  fluidRow(
                                    column(
                                      width = 2,
                                      selectInput("hc_wave",
                                                  "Wave:",
                                                  c("All",unique(as.character(dat$wave))),
                                                  selected = "All")
                                    ),
                                    column(
                                      width = 4,
                                      selectInput("hc_dist",
                                                  "District:",
                                                  choices = c("All",unique(as.character(dat$district_name))),
                                                  selected = "All")
                                    ),
                                    column(
                                      width = 4,
                                      selectInput("hc_var",
                                                  "Indicator:",
                                                  choices = unlist(labels[hc.vars],use.names = F),
                                                  selected = unlist(labels[hc.vars],use.names = F)[1])
                                    ),
                                    column(
                                      width = 2,
                                      selectInput("hc_typ",
                                                  "Type:",
                                                  c("Graph","Table"))
                                    )
                                  )
                                )
                         ),
                         column(width = 2, offset = 0,style = "padding: 0px;",
                                valueBox(textOutput("hc_villages"),"Key Figure 1",icon = icon("building"),color = "teal",width = 12),
                                valueBox(textOutput("hc_households"),"Key Figure 2",icon = icon("home"),color = "teal",width = 12),
                                valueBox(textOutput("hc_progress"),"Key Figure 3",icon = icon("chart-line"),color = "teal",width = 12),
                                valueBox(textOutput("hc_whatevaaa"),"Key Figure 4",icon = icon("cross"),color = "teal",width = 12)
                         )
                         # column(width = 3,style = "padding: 0px;",
                         #        box(
                         #          width = 12,
                         #          "This is a test"
                         #        ),
                         #        box(
                         #          width = 12,
                         #          p("again, this could be anything - Perhaps a trendline?")
                         #        ),

                  )
                ),
                
                column(width = 12,style = "padding: 0px;",
                       box(width = 6,
                           collapsible = T,
                           collapsed = T,
                           status = "primary",
                           solidHeader = TRUE,
                           title = "General health care conditions in Pakistan",
                           includeHTML(normalizePath(file.path("./Text_files/hc_general_health_conditions.htm")))),
                       box(width = 6,
                           collapsible = T,
                           collapsed = T,
                           title = "What health data do we collect and how?",
                           status = "primary",
                           solidHeader = TRUE,
                           includeHTML(normalizePath(file.path("./Text_files/hc_data_description.htm")))
                       )),
                div(id="space", style="display:inline-block;vertical-align:top; width: 40px;", HTML("<br>"))
                
              )),
      
      #### ** Social Distancing ####
      tabItem(tabName = "socdist",
              fluidPage(
                column(width = 12,style = "padding: 0px;",
                       box(width = 9,
                           tags$h2("Do people practice social distancing?")),
                       box(width = 3, 
                           title = "Last updated on",
                           paste(updateDate)
                           )
                ),
                fluidRow(
                  column(width = 12,
                         column(width = 10, offset = 0, style = "padding: 0px;",
                                tabBox(
                                  id = "sd_tabs",
                                  width = 12,
                                  tabPanel("Travel"),
                                  tabPanel("Family and Visits"),
                                  tabPanel("Work"),
                                  tabPanel("Community"),
                                  
                                  conditionalPanel(
                                    condition = "input.sd_typ == 'Table'",
                                    DT::dataTableOutput("sd_table")
                                  ),
                                  conditionalPanel(
                                    condition = "input.sd_typ == 'Graph'",
                                    plotOutput("sd_graph")
                                  ),
                                  
                                  div(id="space", style="display:inline-block;vertical-align:top; width: 20px;", HTML("<br>")),
                                  
                                  fluidRow(
                                    column(
                                      width = 2,
                                      selectInput("sd_wave",
                                                  "Wave:",
                                                  c("All",unique(as.character(dat$wave))),
                                                  selected = "All")
                                    ),
                                    column(
                                      width = 4,
                                        selectInput("sd_dist",
                                                    "District:",
                                                    choices = c("All",unique(as.character(dat$district_name))),
                                                    selected = "All")
                                    ),
                                    column(
                                      width = 4,
                                      selectInput("sd_var",
                                                  "Indicator:",
                                                  choices = unlist(labels[sd.panel.assign$vars],use.names = F),
                                                  selected = unlist(labels[sd.panel.assign$vars],use.names = F)[1])
                                    ),
                                    column(
                                      width = 2,
                                      selectInput("sd_typ",
                                                  "Type:",
                                                  c("Graph","Table"))
                                    )
                                  )
                                  
                                )),
                         column(width = 2, offset = 0,style = "padding: 0px;",
                                valueBox(textOutput("sd_villages"),"Key Figure 1",icon = icon("building"),color = "teal",width = 12),
                                valueBox(textOutput("sd_households"),"Key Figure 2",icon = icon("home"),color = "teal",width = 12),
                                valueBox(textOutput("sd_progress"),"Key Figure 3",icon = icon("chart-line"),color = "teal",width = 12),
                                valueBox(textOutput("sd_whatevaaa"),"Key Figure 4",icon = icon("cross"),color = "teal",width = 12)
                         )
                         
                         # column(width = 3,style = "padding: 0px;",
                         #        box(
                         #          width = 12,
                         #          "This could be anything - perhaps data table?"
                         #        ),
                         #        box(
                         #          width = 12,
                         #          "again, this could be anything - Perhaps a trendline?"
                         #          
                         #        ),

                         )
                  ),
                column(width = 12,style = "padding: 0px;",
                       box(width = 12,
                           collapsible = T,
                           collapsed = T,
                           title = "What data do we collect on social distancing",
                           status = "primary",
                           solidHeader = TRUE,
                           includeHTML(normalizePath(file.path("./Text_files/sd_data_description.htm")))
                       )),
                div(id="space", style="display:inline-block;vertical-align:top; width: 40px;", HTML("<br>"))
                
              )),
      
      #### ** Knowledge ####
      tabItem(tabName = "know",
              fluidPage(
                column(width = 12,style = "padding: 0px;",
                       box(width = 9,
                           tags$h2("What do people know about COVID-19?")),
                       box(width = 3,
                           title = "Last updated on",
                           paste(updateDate)
                          )
                ),
                fluidRow(
                  column(width = 12,
                         column(width = 10, offset = 0, style = "padding: 0px;",
                                tabBox(
                                  id = "know_tabs",
                                  width = 12,
                                  tabPanel("Awareness"),
                                  tabPanel("Knowledge about Covid"),
                                  tabPanel("Prevention of infection"),
                                  
                                  conditionalPanel(
                                    condition = "input.kn_typ == 'Table'",
                                    DT::dataTableOutput("kn_table")
                                  ),
                                  conditionalPanel(
                                    condition = "input.kn_typ == 'Graph'",
                                    plotOutput("kn_graph")
                                  ),
                                    
                                  
                                  div(id="space", style="display:inline-block;vertical-align:top; width: 20px;", HTML("<br>")),
                                  fluidRow(
                                    column(
                                      width = 2,
                                      selectInput("kn_wave",
                                                  "Wave:",
                                                  c("All",unique(as.character(dat$wave))),
                                                  selected = "All")
                                    ),
                                    column(width = 4,
                                        selectInput("kn_dist",
                                                    "District:",
                                                    choices = c("All",unique(as.character(dat$district_name))),
                                                    selected = "All")
                                    ),
                                    column(
                                      width = 4,
                                      selectInput("kn_var",
                                                  "Indicator:",
                                                  choices = unlist(labels[kn.panel.assign$vars],use.names = F),
                                                  selected = unlist(labels[kn.panel.assign$vars],use.names = F)[1])
                                    ),
                                    column(
                                      width = 2,
                                      selectInput("kn_typ",
                                                  "Type:",
                                                  c("Graph","Table"))
                                    )
                                  )
                                  
                         )),
                         column(width = 2, offset = 0,style = "padding: 0px;",
                                valueBox(textOutput("kn_villages"),"Key Figure 1",icon = icon("building"),color = "teal",width = 12),
                                valueBox(textOutput("kn_households"),"Key Figure 2",icon = icon("home"),color = "teal",width = 12),
                                valueBox(textOutput("kn_progress"),"Key Figure 3",icon = icon("chart-line"),color = "teal",width = 12),
                                valueBox(textOutput("kn_whatevaaa"),"Key Figure 4",icon = icon("cross"),color = "teal",width = 12)
                         )
                         # column(width = 3,style = "padding: 0px;",
                         #        box(
                         #          width = 12,
                         #          "This could be anything - perhaps data table?"
                         #        ),
                         #        box(
                         #          width = 12,
                         #          "again, this could be anything - Perhaps a trendline?"
                         #          
                         #        ),

                         # )
                  )
                ),
                column(width = 12,style = "padding: 0px;",
                       box(width = 12,
                           collapsible = T,
                           collapsed = T,
                           title = "What data do we collect on knowledge",
                           status = "primary",
                           solidHeader = TRUE,
                           includeHTML(normalizePath(file.path("./Text_files/kn_data_description.htm")))
                           
                       )),
                div(id="space", style="display:inline-block;vertical-align:top; width: 40px;", HTML("<br>"))
                
              )),
      
      #### ** Needs Assessment ####
      
      tabItem(tabName = "needassess",
              fluidPage(
                column(width = 12,style = "padding: 0px;",
                       box(width = 9,
                           tags$h2("What do people need?")),
                       box(width = 3,
                           title = "Last updated on",
                           paste(updateDate)
                       )
                ),
                fluidRow(
                  column(width = 12,
                         column(width = 10, offset = 0, style = "padding: 0px;",
                                box(
                                  width = 12,
                                  column(
                                    width = 12,align = "center",
                                    conditionalPanel(
                                      condition = "input.na_typ == 'Table'",
                                      DT::dataTableOutput("na_table")
                                    ),
                                    conditionalPanel(
                                      condition = "input.na_typ == 'Graph'",
                                      plotOutput("na_graph")
                                    )
                                  ),
                                  div(id="space", style="display:inline-block;vertical-align:top; width: 20px;", HTML("<br>")),
                                  fluidRow(
                                    column(
                                      width = 2,
                                      selectInput("na_wave",
                                                  "Wave:",
                                                  c("All",unique(as.character(dat$wave))))
                                    ),
                                    column(width = 4,
                                        selectInput("na_dist",
                                                    "District:",
                                                    choices = c("All",unique(as.character(dat$district_name))),
                                                    selected = "All")
                                    ),
                                    column(
                                      width = 4,
                                      selectInput("na_var",
                                                  "Indicator:",
                                                  choices = unlist(labels[na.vars],use.names = F),
                                                  selected = unlist(labels[na.vars],use.names = F)[1])
                                    ),
                                    column(
                                      width = 2,
                                      selectInput("na_typ",
                                                  "Type:",
                                                  c("Graph","Table"))
                                    )
                                  )
                                )
                         ),
                         column(width = 2, offset = 0,style = "padding: 0px;",
                                valueBox(textOutput("na_villages"),"Key Figure 1",icon = icon("building"),color = "teal",width = 12),
                                valueBox(textOutput("na_households"),"Key Figure 2",icon = icon("home"),color = "teal",width = 12),
                                valueBox(textOutput("na_progress"),"Key Figure 3",icon = icon("chart-line"),color = "teal",width = 12),
                                valueBox(textOutput("na_whatevaaa"),"Key Figure 4",icon = icon("cross"),color = "teal",width = 12)
                         )
                         
                         # column(width = 3,style = "padding: 0px;",
                         #        box(
                         #          width = 12,
                         #          "This could be anything - perhaps data table?"
                         #        ),
                         #        box(
                         #          width = 12,
                         #          "again, this could be anything - Perhaps a trendline?"
                         #          
                         #        ),
                         # )
                  )
                ),
                
                column(width = 12,style = "padding: 0px;",
                       box(width = 12,
                           collapsible = T,
                           collapsed = T,
                           title = "How to assess needs",
                           status = "primary",
                           solidHeader = TRUE,
                           includeHTML(normalizePath(file.path("./Text_files/na_data_description.htm")))
                       )),
                div(id="space", style="display:inline-block;vertical-align:top; width: 40px;", HTML("<br>"))
                
              )),
      
      #### ** Awareness ####
      
      # tabItem(tabName = "aware",
      #         fluidPage(
      #           fluidRow(
      #             box(width = 12,
      #                 tags$h2("Awareness Campaign")),
      #             box(
      #               width = 12,
      #               collapsible = T,
      #               collapsed = F,
      #               status = "primary",
      #               solidHeader = T,
      #               title = "The Idea",
      #               includeHTML(normalizePath(file.path("./Text_files/aw_idea.htm")))
      #             )
      #           )
      #         )
      #         
      #         
      #         ),
      # tabItem(tabName = "auto"),
      
      #### * Trends ####
      # Third tab content --> "Trends"
      # tabItem(tabName = "trends",
      #         box(width = 12,
      #             solidHeader = T,
      #             status = "primary",
      #             collapsible = T,
      #             collapsed = F,
      #             "Soon, this section will display data that was collected in the different waves of the project. Consult
      #             the tab Awareness for more information on the different waves.")
      #         ),
      
      #### * About COVID Initiative ####
      # this is the last tab, including information on the organisations and explaining the programme
      tabItem(tabName = "about",
              column(width = 12,
                     box(
                       title = "What we do",
                       width = 12,
                       collapsible = T,
                       collapsed = T,
                       status = "primary",
                       solidHeader = TRUE,
                       includeHTML(path = normalizePath(file.path('./Text_files',"about_what_we_do.htm")))
                     ),
                     box(
                       title = "Real-time Data",
                       width = 12,
                       collapsible = T,
                       collapsed = T,
                       status = "primary",
                       solidHeader = TRUE,
                       includeHTML(path = normalizePath(file.path('./Text_files',"about_real_time.htm")))
                     ),
                     box(
                       title = "Data Collection Tools",
                       width = 12,
                       collapsible = T,
                       collapsed = T,
                       status = "primary",
                       solidHeader = TRUE,
                       includeHTML(path = normalizePath(file.path('./Text_files',"about_tools.htm")))
                     ),
                     box(
                       title = "Who We Are",
                       width = 12,
                       collapsible = T, collapsed = T,
                       status = "primary",
                       solidHeader = TRUE,
                       box(title = "C4ED",
                           width = 12, 
                           collapsible = T,
                           collapsed = T,
                           status = "primary",
                           solidHeader = TRUE,
                           column(width = 2,
                                  div(id="nrsp", tags$a(href="http://c4ed.org", tags$img(src="C4ED_logo_long_nb.png", height="70", width="95")), style="display: inline-block; vertical-align:center;")
                           ),
                           column(width = 10,
                                  div(id="number", style="display:inline-block;vertical-align:top; font-size:140%;"),
                                  includeHTML(normalizePath(file.path("./Text_files/description_c4ed.htm")))
                           )),
                       box(title = "ACTED",
                           width = 12, 
                           collapsible = T,
                           collapsed = T,
                           status = "primary",
                           solidHeader = TRUE,
                           column(width = 2,
                                  div(id="acted", tags$a(href="http://nrsp.org.pk", tags$img(src="ACTED_logo_nb.png", height="60", width="120")), style="display: inline-block; vertical-align:center;")
                           ),
                           column(width = 10,
                                  div(id="number", style="display:inline-block;vertical-align:top; font-size:140%;"),
                                  includeHTML(normalizePath(file.path("./Text_files/description_ACTED.htm")))
                           )),
                       box(title = "NRSP",
                           width = 12, 
                           collapsible = T,
                           collapsed = T,
                           status = "primary",
                           solidHeader = TRUE,
                           column(width = 2,
                                  div(id="nrsp", tags$a(href="http://nrsp.org.pk", tags$img(src="NRSP_logo_nb.png", height="60", width="120")), style="display: inline-block; vertical-align:center;")
                           ),
                           column(width = 10,
                                  div(id="number", style="display:inline-block;vertical-align:top; font-size:140%;"),
                                  includeHTML(normalizePath(file.path("./Text_files/description_NRSP.htm")))
                           )
                       )
                     )
              )

              ### End of tabItems
      )
    )))


ui <- dashboardPage(dheader,dsidebar, dbody)


#### . ####

#### SERVER STARTS HERE ####

#******************************************************
#### SETUP SERVER ####
#******************************************************

server <- function(input, output, session) {
  
  #### "Observe" to update list of indicators based on selected tab ####
  # update indicators based on tabs
  
  observe({
    tab_kn <- input$know_tabs
    tab_sd <- input$sd_tabs
    
    labs_kn <- as.character(kn.panel.assign$Label[as.character(kn.panel.assign$Panel) == tab_kn])
    labs_sd <- as.character(sd.panel.assign$Label[as.character(sd.panel.assign$Panel) == tab_sd])
    
    updateSelectInput(session = session,
                      "kn_var",
                      choices = labs_kn,
                      selected = labs_kn[1])
    
    updateSelectInput(session = session,
                      "sd_var",
                      choices = labs_sd,
                      selected = labs_sd[1])
  })
  
  
  
  ########### DYNAMIC VALUE BOXES #################
  
  ###  Allow district breakdown
  
  #### * At a glance ####
  
  # Number of villages covered --> "rv"
  # (villages with at least 1 completed interview)
  output$overall_villages <- renderText({
    rv <- length(as.character(unique(dat$rv[dat$consent == "yes"])))
    
    # By district and by wave
    if(input$overall_dist != "All"){
      if(input$overall_wave!= "All"){
        rv <- length(as.character(unique(dat$rv[dat$district_name == input$overall_dist & dat$wave==input$overall_wave & dat$consent == "yes"])))
        }
      else{
        rv <- length(as.character(unique(dat$rv[dat$district_name == input$overall_dist & dat$consent == "yes"])))
        }
    }
    else{
      if(input$overall_wave!= "All"){
        rv <- length(as.character(unique(dat$rv[dat$wave == input$overall_wave & dat$consent == "yes"])))
      }
    }
    rv <-  prettyNum(rv, big.mark = ",")
  })
  
  # Number of HH covered --> "contact_id"
  # (only with completed interviews)
  output$overall_households <- renderText({
    hh <- length(as.character(unique(dat$contact_id[dat$consent == "yes"])))
    # By district and by wave
    if(input$overall_dist != "All"){
      if(input$overall_wave!= "All"){
        hh <- length(as.character(unique(dat$contact_id[dat$district_name == input$overall_dist & dat$wave==input$overall_wave & dat$consent == "yes"])))
      }
      else{
        hh <- length(as.character(unique(dat$contact_id[dat$district_name == input$overall_dist & dat$consent == "yes"])))
      }
    }
    else{
      if(input$overall_wave!= "All"){
        hh <- length(as.character(unique(dat$contact_id[dat$wave == input$overall_wave & dat$consent == "yes"])))
      }
    }
    hh <-  prettyNum(hh, big.mark = ",")
  })
  
  # Population covered (avg HH size * nb HHs)
  # (only with completed interviews)
  # CAREFUL: special codes 666, 777, 888, 999
  output$overall_covered <- renderText({
    # Subset data to HH with completed interviews + no NA + no special code
    pop <- sum(dat[!is.na(dat$hh_size) & dat$consent=="yes" & dat$hh_size<666, "hh_size"])
    # In case specific district selected
    if(input$overall_dist != "All"){
      pop <- sum(dat[!is.na(dat$hh_size) & dat$consent=="yes" & dat$hh_size<666 & dat$district_name == input$overall_dist, "hh_size"])
      }
    # By district and by wave
    if(input$overall_dist != "All"){
      if(input$overall_wave!= "All"){
        pop <- sum(dat[!is.na(dat$hh_size) & dat$consent=="yes" & dat$hh_size<666 & dat$district_name == input$overall_dist & dat$wave==input$overall_wave, "hh_size"])
      }
      else{
        pop <- sum(dat[!is.na(dat$hh_size) & dat$consent=="yes" & dat$hh_size<666 & dat$district_name == input$overall_dist, "hh_size"])
      }
    }
    else{
      if(input$overall_wave!= "All"){
        pop <- sum(dat[!is.na(dat$hh_size) & dat$consent=="yes" & dat$hh_size<666 & dat$wave == input$overall_wave, "hh_size"])
      }
    }
    pop <-  prettyNum(pop, big.mark = ",")
  })
  
  # Not in there for now
   # output$overall_target <- renderText({
   #   paste0(200)
   # })
  
  
  output$hc_villages <-  renderText({paste(0)})
  output$hc_households <- renderText({paste(0)})
  output$hc_progress <- renderText({paste(0)})
  output$hc_whatevaaa <- renderText({paste(0)})
  
  output$sd_villages <- renderText({paste(0)})
  output$sd_households <- renderText({paste(0)})
  output$sd_progress <- renderText({paste(0)})
  output$sd_whatevaaa <- renderText({paste(0)})
  
  output$kn_villages <- renderText({paste(0)})
  output$kn_households <- renderText({paste(0)})
  output$kn_progress <- renderText({paste(0)})
  output$kn_whatevaaa <- renderText({paste(0)})
  
  output$na_villages <- renderText({paste(0)})
  output$na_households <- renderText({paste(0)})
  # renderText({
  #   hh <- nrow(dat)
  #   if(input$na_dist != "All"){
  #     #assuming one household per row!
  #     hh <- nrow(dat[dat$district_name == input$na_dist,])
  #   }
  #   hh
  # })
  output$na_progress <- renderText({paste(0)})
  output$na_whatevaaa <- renderText({paste(0)})
  
  # output$st_districts <- renderText({paste(16)})
  # output$st_provinces <- renderText({paste(2)})
  # output$st_villages <- renderText({paste("1,500")})
  # output$st_households <- renderText({paste("22,500")})
  # output$st_benef <- renderText({paste("150,000")})
  # output$st_progress <-  renderText({paste(0, "%")})
  
  
  #### FUNCTIONS FOR GRAPHS AND TABLES ####
  
  #### * Function to prepare data for TABLES and GRAPHS ####
  
  # Output: table/data, captions, type of var
  
  prepare_data <- function(varname, district, wave){
    
    # MC Cat var --> variable = only NA
    if (length(dat[!is.na(dat[,varname]),varname])==0) {

      vartype <- "Multiple"
      
      # Keep only variabels of interest + district + wave
      vals<- dat[, c(grep(varname, names(dat)), grep("district_name", names(dat)), grep("wave", names(dat)), grep("contact_id", names(dat)))]
      
      # Subset data --> selected district
      if(district!= "All"){
        vals <- vals[vals$district_name == district,]
      }
      # Subset data --> selected wave of data collection
      if(wave!= "All"){
        vals <- vals[vals$wave == wave,]
      }
      
      # Get value labels without special codes
      values <- value_labels[[varname]]
      spec_values <- names(values[values==666 | values==777 | values==888 | values==999])
      values <- list.remove(values, spec_values)
      
      # Loop over all values to produce table to plot/display
      j <- 0
      for (i in (1:length(values))){
        j <- j +1
        subvar <- paste0(varname, "_", values[i][[1]])
        pct <- sum(vals[!is.na(vals[, subvar]), subvar])/length(vals[!is.na(vals[, subvar]), subvar])
        if (j==1){
          # Count number of observations for caption
          n <- prettyNum(length(unique(vals[!is.na(vals[, subvar]), "contact_id"])), big.mark = ",")
          
          out_data <- data.frame(cbind(names(values[i]), pct))
        }
        else{
          out_data <- data.frame(rbind(out_data, cbind(names(values[i]), pct)))
        }
      }
      
      names(out_data) <- c("answer", "share")
      # Make sure "share" is numeric
      out_data$share <- as.numeric(out_data$share)
      
      # Answer options with 0 answers
      not_selected_options <- paste(out_data[out_data$share==0, "answer"],collapse = "; ")
      if(not_selected_options==""){
        note_graph <- paste0("Based on data from ", n, " respondents.<br/>",
                              "NOTE: Multiple choice question. Each category can sum up to 100%."
                              )
      }
      else {
        note_graph <- paste0("Based on data from ", n, " respondents.<br/>",
                              "Other possible answers (0 response): ", not_selected_options,"<br/>",
                              "NOTE: Multiple choice question. Each category can sum up to 100%."
                              )
      }

      note_table <- paste0("Based on data from ", n, " respondents.<br/>",
                           "NOTE: Multiple choice question. Each category can sum up to 100%."
                          )
      
    }
    
    else {

      vartype <- "Single"
      
      # Numeric var
      if (is.numeric(dat[!is.na(dat[,varname]),varname])){
        out_data <- dat[!is.na(dat[,varname]) & dat[,varname]!=666 & dat[,varname]!=777 & dat[,varname]!=888 & dat[,varname]!=999 , ]
      }
      
      # Cat var --> not numeric + not MC cat var = should be "simple" Cat var 
      else{
        # Extract value labels of special codes
        values <- value_labels[[varname]]
        spec_values <- names(values[values==666 | values==777 | values==888 | values==999])
        # Subset data for selected variable --> exclude special codes
        out_data <- subset(dat, !is.na(dat[,varname]) & !(dat[,varname] %in% spec_values))
      }
      
      # Subset data --> selected district
      if(district!= "All"){
        out_data <- out_data[out_data$district_name == district,]
      }
      
      # Subset data --> selected wave of data collection
      if(wave!= "All"){
        out_data <- out_data[out_data$wave == wave,]
      }
      
      # create caption of plot including options that were not selected and the number of observations.
      tab <- table(factor(out_data[,varname]))
      not_selected_options <- paste(names(tab[tab == 0]),collapse = "; ")
      n <- prettyNum(nrow(out_data), big.mark = ",")
      if(not_selected_options==""){
        note_graph <- paste0("Based on data from ",n," respondents.")
      }
      else {
        note_graph <- paste0("Based on data from ",n," respondents.<br>",
                              "Other possible answers (0 response): ", not_selected_options)
      }
    
    note_table <- paste0("Based on data from ",n," respondents.")
      
    }
    
    # OUTPUT = data/table, graph footnote, table footnote, variable type
    output <- list("data" = out_data, "caption_graph" = note_graph, "caption_table" = note_table,  "vartype" = vartype)
    return(output)
      
  }

    
  #### * Function to make GRAPHS ####
  
  plot_bar <- function(label, district, wave){
    
    # translate the variable label to the variable name
    # in the plot menu, people select the variable label, not the variable name
    varname <- names(labels[unlist(labels) == label])
    
    out <- prepare_data(varname, district, wave)
    
    # Extract from function output
    # Data
    plotdata <- out[["data"]]
    # Graph caption
    caption <- out[["caption_graph"]]
    # Variable type
    vartype <- out[["vartype"]]
    
    
    if (vartype=="Multiple"){
      
      # Ensure categories stay in order
      #plotdata$answer <- factor(plotdata$answer, levels=unique(plotdata$answer[order(seq(1, length(plotdata$answer)))]))
      plotdata$answer <- factor(plotdata$answer, levels=unique(plotdata$answer[order(plotdata$share, decreasing = T)]))
      
      # Make plot
      bar_plot <- ggplot(plotdata, aes(x=answer, y=share, fill=answer)) +
        geom_bar(stat="identity", position=position_dodge(1)) +
        xlab("") +
        ylab("Percent") +
        scale_y_continuous(labels=scales::percent) +
        scale_fill_manual(values = c4edcolours) +
        guides(fill = guide_legend(title=NULL)) +
        labs(title = paste0("<span style = 'font-size:16pt'>**", 
                            str_replace_all(qx.labs[[varname]],"\"",""),
                            "**</span><br>"),
             caption = paste0("<span style = 'font-size:12pt'>", 
                              caption,
                              "</span>")) +
        theme_bw(base_family = "sans",
                 base_size = 14) +
        theme(plot.title.position = "plot",
              plot.title = element_textbox_simple(size = 16,
                                                  halign=0.5),
              plot.caption = element_textbox_simple(size = 12,
                                                    halign=0),
              legend.background = element_rect(fill="gray96", 
                                               size=0.1, linetype="solid"),
              axis.title.x=element_blank(),
              axis.text.x=element_blank(),
              axis.ticks.x=element_blank()
        )
      
      reposition_legend(bar_plot, 'top right', offset=0.002)  
      #bar_plot
      
    }
    
    else{
      # create the plot   
      
      # Count number of categories --> if more than 4, legend instead of labels on x-axis
      num_cats <- length(unique(factor(plotdata[,varname])))
      
      if (num_cats<=4 | is.numeric(plotdata[,varname])){
        bar_plot <- ggplot(plotdata,aes(x = factor(plotdata[,varname]))) +
          geom_bar(aes(y = (..count..)/sum(..count..)), fill = "#047B77")+
          scale_y_continuous(labels=scales::percent) +
          ylab("Percent") +
          xlab("") +
          labs(title = paste0("<span style = 'font-size:16pt'>**", 
                              str_replace_all(qx.labs[[varname]],"\"",""),
                              "**</span><br>"),
               caption = paste0("<span style = 'font-size:12pt'>", 
                                caption,
                                "</span>")) +
          theme_bw(base_family = "sans",
                   base_size = 14) +
          theme(plot.title.position = "plot",
                plot.title = element_textbox_simple(size = 16,
                                                    halign=0.5),
                plot.caption = element_textbox_simple(size = 12,
                                                      halign=0))
        
        bar_plot    
      }
      else{
        bar_plot <- ggplot(plotdata,aes(x = factor(plotdata[,varname]), fill = factor(plotdata[,varname]))) +
          geom_bar(aes(y = (..count..)/sum(..count..)), position=position_dodge(1))+
          scale_y_continuous(labels=scales::percent) +
          scale_fill_manual(values = c4edcolours) +
          guides(fill = guide_legend(title=NULL)) +
          ylab("Percent") +
          xlab("") +
          labs(title = paste0("<span style = 'font-size:16pt'>**", 
                              str_replace_all(qx.labs[[varname]],"\"",""),
                              "**</span><br>"),
               caption = paste0("<span style = 'font-size:12pt'>", 
                                caption,
                                "</span>")) +
          theme_bw(base_family = "sans",
                   base_size = 14) +
          theme(plot.title.position = "plot",
                plot.title = element_textbox_simple(size = 16,
                                                    halign=0.5),
                plot.caption = element_textbox_simple(size = 12,
                                                      halign=0),
                legend.background = element_rect(fill="gray96", 
                                                 size=0.1, linetype="solid"),
                axis.title.x=element_blank(),
                axis.text.x=element_blank(),
                axis.ticks.x=element_blank()
          )
        
        reposition_legend(bar_plot, 'top right', offset=0.002)  
        
      }
    }
  }
  
  # If ever you would like to add hovering to the plots, 
  # use the following snippet of code and exchange 
  # renderPlot({}) with renderPlotly({}) in the server
  # plotOutput({}) wtth plotlyOutput({}) in the UI
  # ggplotly(bar_plot, tooltip = c("y", Options))
  # however you will have to find a way how to adjust the caption...
  # and the displaying of the value...

    
  #### * Function to make TABLES ####  
  
  make_table <- function(label, district, wave){
    # translate the variable label to the variable name
    # in the plot menu, people select the variable label, not the variable name
    varname <- names(labels[unlist(labels) == label])
    
    out <- prepare_data(varname, district, wave)
    
    # Extract from function output
    # Data
    tabdata <- out[["data"]]
    # Graph caption
    caption <- out[["caption_table"]]
    # Variable type
    vartype <- out[["vartype"]]
    
    # Shape data for tabulation
    if (vartype=="Multiple"){
      tab <- data.frame("answer" = tabdata$answer, "share" = round(tabdata[,"share"]* 100,1))
    }
    else{
      tab <- data.frame(round(prop.table(table(factor(tabdata[,varname]))),3) * 100)
    }
    
    # JavaScript function needed to have two table captions, top and bottom
    js <- c(
      "function(settings){",
      "  var datatable = settings.oInstance.api();",
      "  var table = datatable.table().node();",
      paste0("  var caption = ", "'", caption, "'"),
      "  $(table).append('<caption style=\"caption-side: bottom; color: black\">' + caption + '</caption>');",
      "}"
    )
    
    names(tab) <- c("Answer Option","Share (in %)")
    DT::datatable(tab, rownames = F, options = list(paging=F,
                                                    searching=F,
                                                    bInfo=F,
                                                    order=list(1, 'desc'),
                                                    drawCallback = JS(js)
                                                    ),
                  caption = htmltools::tags$caption(
                            style = 'caption-side: top; text-align: center; color: black; font-size: 140%',
                            tags$b(str_replace_all(qx.labs[[varname]],"\"",""))
                            )
                  )
  }
  
  
  
  #### * Function to prepare AT A GLANCE DATA ####  
  
  make_glance_data <- function(district, wave, population){
    
    # Get data for HH where some people sick 
    vals <- dat[!is.na(dat$r5) & dat$r5>0 & dat$r5<666 & dat$consent=="yes",]
    
    # subset the data, based on the selected region
    if(district!= "All"){
      vals <- vals[vals$district_name == district,]
    }
    
    # subset data based on wave of data collection
    if(wave!= "All"){
      vals <- vals[vals$wave == wave,]
    }
    
    # DF with required data for table + ensure order of categories
    #labs <- c("Total", labels[["hh_one_symptom"]], labels[["hh_more_symptoms"]])
    labs <- c("Total", "At least two Covid 19-like symptoms", "More than two Covid 19-like symptoms")
    
    # Depends on population selected: "All", "Gender", "By Age Group"
    if (population=="All"){
      values <- c(sum(vals$r5), sum(vals$hh_more_symptoms), sum(vals$hh_2more_symptoms))
      sick <- data.frame("order" = c(1,2,3), "sicks" = labs, "variable" = c("", "", ""), "value" = values)
    
      # Title
      title <- "Number of people in the household falling sick in the last 14 days"
    }

    # By gender
    if (population=="By Gender"){
      values <- c(sum(vals$hh_sick_m), sum(vals$hh_more_symptoms_m), sum(vals$hh_2more_symptoms_m))
      sick.men <- data.frame("order" = c(1,2,3), "sicks" = labs, "variable" = rep("Male", 3), "value" = values)
      
      values <- c(sum(vals$hh_sick_f), sum(vals$hh_more_symptoms_f), sum(vals$hh_2more_symptoms_f))
      sick.women <- data.frame("order" = c(4,5,6), "sicks" = labs, "variable" = rep("Female", 3), "value" = values)
      
      sick <- data.frame(rbind(sick.men, sick.women))
    
      # Title
      title <- "Number of people in the household falling sick in the last 14 days, by gender"
    }
    
    # By age group
    if (population=="By Age Group"){
      j<-0
      age_groups <- c("0-14", "15-34", "35-59", "60+")
      for (i in c("014", "1534", "3559", "60")){
        j <- j+1
        values <- c(sum(vals[,paste0("hh_sick_",i)]), sum(vals[,paste0("hh_more_symptoms_",i)]), sum(vals[,paste0("hh_2more_symptoms_",i)]))
        sick_tmp <- data.frame("order" = seq(1+(j-1)*3,j*3), "sicks" = labs, "variable" = rep(age_groups[j], 3), "value" = values)
        if (j==1) {
          sick <- data.frame(sick_tmp)
        }
        else{
          sick <- data.frame(rbind(sick, sick_tmp))
        }
      }
    
      # Title
      title <- "Number of people in the household falling sick in the last 14 days, by age group"
    }
    
    # Note for graph and table
    # Extract List of possible Covid-like symptoms
    vallabs <- value_labels[["r6"]]
    rmv_vals <- names(vallabs[vallabs==14 | vallabs==666 | vallabs==777 | vallabs==888 | vallabs==999])
    vallabs <- list.remove(vallabs, rmv_vals)
    # No. of respondents
    n <- prettyNum(length(unique(vals$contact_id)), big.mark = ",")
    
    description <- paste0("Based on data from ", n, " respondents.<br/>",
                          "Possible Covid 19-like symptoms:<br/>",
                          paste0(paste(names(vallabs), collapse="; "), "."))
    
    # Store in list to access outside function
    output <- list("data" = sick, "caption" = description, "title" = title)
    return(output)

    }
  
  
  
  #### * Function to prepare DATA COLLECTION MONITORING DATA ####
  
  make_monitor_data <- function(district){
    
    # Keep only variables of interest + district + wave
    vals<- dat[, c("attempts_day", "district_name", "wave", "date", "consent")]
  
    # Subset data --> selected district
    if(district!= "All"){
      vals <- vals[vals$district_name == district,]
    }
    
    # Encode consent variable
    vals$success_day <- ifelse(vals$consent == "yes", 1, 0)
    
    ### Daily stats (graph) --> Attempts per day + success per day
    # Generate variables
    prog_day <- as.data.frame(aggregate(vals[,c("attempts_day", "success_day")], by = list(date=vals$date), sum))
    prog_day$success_rate <- prog_day$success_day / prog_day$attempts_day * 100
    # Reshape for graph
    prog1 <- reshape2::melt(prog_day[, c("date","success_day","attempts_day")], id.var = "date")
    levels(prog1$variable)[levels(prog1$variable)=="attempts_day"] <- "Attempts"
    levels(prog1$variable)[levels(prog1$variable)=="success_day"] <- "Successful interviews"
    
    prog2 <- prog_day[, c("date","success_rate")]
  
    ### Stats by wave --> table
    prog_wave <- as.data.frame(aggregate(vals[,c("attempts_day", "success_day")], by = list(date=vals$wave), sum))
    prog_wave$success_rate <- prog_wave$success_day / prog_wave$attempts_day * 100
    
    # Caption
    description <- paste0("Number of attempts = Total number of phone calls made/attempted.<br/>",
                      "F0 = Baseline data collection.<br/>",
                      "F1, F2,... = Follow-up data collections.<br/>")
    
    # Store in list to access outside function
    output <- list("prog1" = prog1, "prog2" = prog2, "prog_wave" = prog_wave, "caption" = description)
    return(output)    
    
  }
  
  
  ##### GRAPHS AND TABLES #####
  
  #### ** At a glance ####
  # Cannot use the function defined above, need some tailoring
  #plot_bar(input$overall_var, input$overall_dist, input$overall_wave)
  
  #### Make plot for At a glance page ####
  output$overall_graph <- renderPlot({
    # Prepare data
    out <- make_glance_data(input$overall_dist, input$overall_wave, input$sub_group)
    plotdata <- out[["data"]]
    title <- out[["title"]]
    caption <- out[["caption"]]
    
    # Ensure categories stay in order
    plotdata$sicks <- factor(plotdata$sicks, levels=unique(plotdata$sicks[order(plotdata$order)]))
    
    # Plot
    bar_plot <- 
    ggplot(plotdata, aes(x=variable, y=value, fill=sicks)) +
      geom_bar(stat="identity", position=position_dodge()) +
      xlab("") + ylab("") +
      scale_y_continuous(labels=function(x) format(x, big.mark = ",", scientific = FALSE)) + 
      scale_fill_manual(values = c4edcolours) +
      guides(fill = guide_legend(title=NULL)) +
      labs(title = paste0("<span style = 'font-size:16pt'>**", 
                          title,
                          "**</span><br>"),
           caption = paste0("<span style = 'font-size:12pt'>", 
                            caption,
                            "</span>")) +
      theme_bw(base_family = "sans",
               base_size = 14) +
      theme(plot.title.position = "plot",
            plot.title = element_textbox_simple(size = 16,
                                                halign=0.5),
            plot.caption = element_textbox_simple(size = 12,
                                                  halign=0),
            legend.background = element_rect(fill="gray96", 
                                             size=0.1, linetype="solid")
            )

    reposition_legend(bar_plot, 'top right', offset=0.002)  

    }) 
  
  
  #### Make table for At a glance page ####
  output$overall_table <- DT::renderDataTable({
    # Prepare data
    out <- make_glance_data(input$overall_dist, input$overall_wave, input$sub_group)
    tabdata <- out[["data"]]
    title <- out[["title"]]
    caption <- out[["caption"]]
    sub_pop <- input$sub_group
    
    # Format numbers --> thousand separator
    tabdata$value <- format(tabdata$value, big.mark=',', scientific=FALSE)
    
    # Reshape data if by sub-group
    # By gender
    if (sub_pop=="By Gender"){
      tabdata <- data.frame("sicks" = unique(tabdata$sicks), "value_m" = tabdata[tabdata$variable=="Male","value"], "value_f" = tabdata[tabdata$variable=="Female","value"])
      tabvars <- c("sicks", "value_m", "value_f")
      col_names <- c("Male", "Female")
    }
    
    # By age group
    if (sub_pop=="By Age Group"){
      tmpdata <- data.frame("sicks" = unique(tabdata$sicks))
      tabvars <- "sicks"
      j<-0
      age_groups <- c("0-14", "15-34", "35-59", "60+")
      for (i in c("014", "1534", "3559", "60")){
        j <- j+1
        tmpdata <- data.frame(tmpdata, tabdata[tabdata$variable==age_groups[j],"value"])
        tabvars <- c(tabvars, paste0("value_",i))
      }
      tabdata <- tmpdata
      names(tabdata) <- tabvars
      col_names <- c("0-14", "15-34", "35-59", "60+")
    }
    
    # All
    if (sub_pop=="All"){
      tabvars <- c("sicks", "value")
      col_names <- ""
    }
    
    # JavaScript function needed to have two table captions, top and bottom
    js <- c(
      "function(settings){",
      "  var datatable = settings.oInstance.api();",
      "  var table = datatable.table().node();",
      paste0("  var caption = ", "'", caption, "'"),
      "  $(table).append('<caption style=\"caption-side: bottom; text-align: left; color: black\">' + caption + '</caption>');",
      "}"
    )
    
    # Table
    DT::datatable(tabdata[,tabvars],
                  rownames = F,
                  colnames = col_names,
                  options = list(paging=F,
                                 searching=F,                                            
                                 bInfo=F,
                                 order=list(1, 'desc'),
                                 dom = 't',
                                 bSort=FALSE,
                                 drawCallback = JS(js)
                                 ),
                  caption = htmltools::tags$caption(
                    style = 'caption-side: top; text-align: center; color: black; font-size: 120%',
                    tags$b(title)
                    )
                  )  
    })
  
  
  #### ** Data Collection Monitoring ####
  
  # Graph
  output$monitor_graph <- renderPlotly({
    
    # Prepare data
    out <- make_monitor_data(input$data_dist)
    prog1 <- out[["prog1"]]
    prog2 <- out[["prog2"]]
    
    # Determine breaks for y-axis
    max <- max(prog1$value)
    if(max<100){
      step <- 10
      top <- 100
    }
    if(max<500 & max>=100){
      step <- 50
      top <- (floor(max/step)+1)*step
    }
    if(max<1000 & max>=500){
      step <- 100
      top <- (floor(max/step)+1)*step
    }
    if(max>=1000){
      step <- 500
      top <- (floor(max/step)+1)*step      
    }
        
    p <- ggplot(prog1, aes(x=date, y = value, colour = variable)) +
      geom_line() +
      geom_point() +
      geom_line(data = prog2, aes(x = date, y = success_rate), colour = "transparent") +
      scale_color_manual(values = c4edcolours) + 
      scale_x_date(date_labels = "%d %b %y", date_breaks = "1 week") +
      scale_y_continuous(labels=function(x) format(x, big.mark = ",", scientific = FALSE),
                         breaks = seq(0, top, step)) +
      xlab("") +
      ylab("") +
      theme(legend.title = element_blank(),
            axis.text.x = element_text(angle = 45)) +
      geom_vline(xintercept = as.numeric(as.Date("2020-08-06")), linetype="dashed", 
                 color = "blue", size=0.5) +
      geom_vline(xintercept = as.numeric(as.Date("2020-09-21")), linetype="dashed", 
                 color = "blue", size=0.5) +
      geom_vline(xintercept = as.numeric(as.Date("2020-10-21")), linetype="dashed", 
                 color = "blue", size=0.5) +
      geom_vline(xintercept = as.numeric(as.Date("2020-11-06")), linetype="dashed", 
                 color = "blue", size=0.5) +
      geom_vline(xintercept = as.numeric(as.Date("2020-11-21")), linetype="dashed", 
                 color = "blue", size=0.5) +
      geom_vline(xintercept = as.numeric(as.Date("2020-12-09")), linetype="dashed", 
                 color = "blue", size=0.5) +
      geom_vline(xintercept = as.numeric(as.Date("2020-10-28")), linetype="dotted", 
                 color = "green", size=0.5) +
      geom_vline(xintercept = as.numeric(as.Date("2020-10-20")), linetype="dotted", 
                 color = "green", size=0.5) +
      geom_vline(xintercept = as.numeric(as.Date("2020-11-05")), linetype="dotted", 
                 color = "green", size=0.5) +
      geom_vline(xintercept = as.numeric(as.Date("2020-11-20")), linetype="dotted", 
                 color = "green", size=0.5) +
      geom_vline(xintercept = as.numeric(as.Date("2020-12-05")), linetype="dotted", 
                 color = "green", size=0.5)
    
    startf0 <- list(yref = 'paper', xref = "x",
                    y = 1,
                    x = as.numeric(as.Date("2020-08-06")),
                    text = paste0("Start F0<br>",format(as.Date("2020-08-06"), "%d %b")),
                    showarrow = FALSE,
                    xanchor = "left",
                    font = list(size=12, color = "blue"))
    startf1 <- list(yref = 'paper', xref = "x",
                    y = 1,
                    x = as.numeric(as.Date("2020-09-21")),
                    text = paste0("Start F1<br>",format(as.Date("2020-09-21"), "%d %b")),
                    showarrow = FALSE,
                    xanchor = "left",
                    font = list(size=12, color = "blue"))
    startf2 <- list(yref = 'paper', xref = "x",
                    y = 1,
                    x = as.numeric(as.Date("2020-10-21")),
                    text = paste0("Start F2<br>",format(as.Date("2020-10-21"), "%d %b")),
                    showarrow = FALSE,
                    xanchor = "left",
                    font = list(size=12, color = "blue"))
    startf3 <- list(yref = 'paper', xref = "x",
                    y = 1,
                    x = as.numeric(as.Date("2020-11-06")),
                    text = paste0("Start F3<br>",format(as.Date("2020-11-06"), "%d %b")),
                    showarrow = FALSE,
                    xanchor = "left",
                    font = list(size=12, color = "blue"))
    startf4 <- list(yref = 'paper', xref = "x",
                    y = 1,
                    x = as.numeric(as.Date("2020-11-21")),
                    text = paste0("Start F4<br>",format(as.Date("2020-11-21"), "%d %b")),
                    showarrow = FALSE,
                    xanchor = "left",
                    font = list(size=12, color = "blue"))
    startf5 <- list(yref = 'paper', xref = "x",
                    y = 1,
                    x = as.numeric(as.Date("2020-12-09")),
                    text = paste0("Start F5<br>",format(as.Date("2020-12-09"), "%d %b")),
                    showarrow = FALSE,
                    xanchor = "left",
                    font = list(size=12, color = "blue"))
    endf1 <- list(yref = 'paper', xref = "x",
                  y = 0.8,
                  x = as.numeric(as.Date("2020-10-20")),
                  text = paste0("End F1<br>",format(as.Date("2020-10-20"), "%d %b")),
                  showarrow = FALSE,
                  xanchor = "right",
                  font = list(size=12, color = "green"))
    endf0 <- list(yref = 'paper', xref = "x",
                  y = 0.8,
                  x = as.numeric(as.Date("2020-10-28")),
                  text = paste0("End F0<br>",format(as.Date("2020-10-28"), "%d %b")),
                  showarrow = FALSE,
                  xanchor = "right",
                  font = list(size=12, color = "green"))
    endf2 <- list(yref = 'paper', xref = "x",
                  y = 0.8,
                  x = as.numeric(as.Date("2020-11-05")),
                  text = paste0("End F2<br>",format(as.Date("2020-11-05"), "%d %b")),
                  showarrow = FALSE,
                  xanchor = "right",
                  font = list(size=12, color = "green"))
    endf3 <- list(yref = 'paper', xref = "x",
                  y = 0.8,
                  x = as.numeric(as.Date("2020-11-20")),
                  text = paste0("End F3<br>",format(as.Date("2020-11-20"), "%d %b")),
                  showarrow = FALSE,
                  xanchor = "right",
                  font = list(size=12, color = "green"))
    endf4 <- list(yref = 'paper', xref = "x",
                  y = 0.8,
                  x = as.numeric(as.Date("2020-12-05")),
                  text = paste0("End F4<br>",format(as.Date("2020-12-05"), "%d %b")),
                  showarrow = FALSE,
                  xanchor = "right",
                  font = list(size=12, color = "green"))
    
    
    w <- ggplotly(p)
    
    text_date <- paste0("Date: ", format(as.Date.numeric(w$x$data[[1]]$x, origin = "1970-01-01"), "%d %b %Y"))
    # Successful interviews = Trace 1 in plotly object
    text_success <- paste0("Successful interviews: ", format(w$x$data[[1]]$y, big.mark=",", scientific=F))
    # Attemtps = Trace 2 in plotly object
    text_attempts <- paste0("Number of attempts: ", format(w$x$data[[2]]$y, big.mark=",", scientific=F))
    # Success Rate = Trace 3 in plotly object
    text_success_rate <- paste0("Success rate: ", format(round(w$x$data[[3]]$y, 1), nsmall =1), "%")
    
    w %>%
      style(hoverinfo = "none", traces = 3:13) %>%
      style(text = paste0(text_success), traces = 1) %>%
      style(text = paste0(text_date, "<br>", text_attempts, "<br>", text_success_rate), traces = 2) %>%
      style(hoverlabel = list(bgcolor = "white")) %>%
      layout(showlegend = TRUE,
             hovermode = "x",
             legend = list(orientation = "h",
                           xanchor = "center",
                           x = 0.5,
                           y=-0.25,
                           itemclick = "toggleothers",
                           itemdoubleclick = F)) %>%
      layout(annotations=list(startf0)) %>%
      layout(annotations=list(startf1)) %>%
      layout(annotations=list(startf2)) %>%
      layout(annotations=list(startf3)) %>%
      layout(annotations=list(startf4)) %>%
      layout(annotations=list(startf5)) %>%
      layout(annotations=list(endf1)) %>%
      layout(annotations=list(endf0)) %>%
      layout(annotations=list(endf2)) %>%
      layout(annotations=list(endf3)) %>%
      layout(annotations=list(endf4))
    
    
  })
  
  output$monitor_graph_note <- renderUI({
    HTML(paste0("Number of attempts = Total number of phone calls made/attempted.<br/>",
           "F0 = Baseline data collection.<br/>",
           "F1, F2,... = Follow-up data collections.<br/>"))
  })
  
  # Table
  output$monitor_table <- DT::renderDataTable({
    # Prepare data
    out <- make_monitor_data(input$data_dist)
    prog_wave <- out[["prog_wave"]]
    caption <- out[["caption"]]
    
    # Format numbers --> thousand separator
    prog_wave$attempts_day <- format(prog_wave$attempts_day, big.mark=',', scientific=FALSE)
    prog_wave$success_day <- format(prog_wave$success_day, big.mark=',', scientific=FALSE)
    prog_wave$success_rate <- format(round(prog_wave$success_rate, 1), nsmall = 1)
    #prog_wave$success_rate <- paste(prog_wave$success_rate,"%")
    
    # Variable names
    col_names <- c("Wave", "Number of attempts", "Number of successful interviews", "Success rate (%)")
      
    # Table
    DT::datatable(prog_wave,
                  rownames = F,
                  colnames = col_names,
                  options = list(paging=F,
                                 searching=F,                                            
                                 bInfo=F,
                                 order=list(1, 'desc'),
                                 dom = 't',
                                 bSort=FALSE
                  )
                  )
  })
  
  output$monitor_table_note <- renderUI({
    HTML(paste0("Number of attempts = Total number of phone calls made/attempted.<br/>",
                "F0 = Baseline data collection.<br/>",
                "F1, F2,... = Follow-up data collections.<br/>"))
  })
  
  
  
  #### ** Health facilities ####
  output$hc_graph <- renderPlot({
    plot_bar(input$hc_var, input$hc_dist, input$hc_wave)
  }) 
  output$hc_table <- DT::renderDataTable({
    make_table(input$hc_var, input$hc_dist, input$hc_wave)
  })
  
  
  #### ** Social behaviour ####
  output$sd_graph <-renderPlot({
    plot_bar(input$sd_var, input$sd_dist, input$sd_wave)
  }) 
  output$sd_table <- DT::renderDataTable({
    make_table(input$sd_var, input$sd_dist, input$sd_wave)
  })
  
  
  #### ** Knowledge ####
  output$kn_graph <- renderPlot({
    plot_bar(input$kn_var, input$kn_dist, input$kn_wave)
  }) 
  output$kn_table <- DT::renderDataTable({
    make_table(input$kn_var, input$kn_dist, input$kn_wave)
  })
  
  
  #### ** Needs Assessment ####
  output$na_graph <- renderPlot({
    plot_bar(input$na_var, input$na_dist, input$na_wave)
  }) 
  output$na_table <- DT::renderDataTable({
    make_table(input$na_var, input$na_dist, input$na_wave)
  })
  

  
  
  
  ############ NGOs logos #################
  
  ### 'NRSP_logo.png', C4ED_logo.png and 'ACTED_logo.png' must be in the 'www' folder.  
  
  # function to call png files from the graphs folder
  
 
  
  
  #***********************************************************
  
} 

shinyApp(ui, server)






