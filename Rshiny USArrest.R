library(shiny)
library(shinydashboard)
source("Server.R")
source("global.R")
# install.packages("shinycssloaders")
#install.packages("shinymanager") # for credential
library(shinycssloaders)#for spinner
library(shinymanager)

ui<-secure_app( dashboardPage(dashboardHeader(title="Exploring the 1973 US Arrests data",
                              titleWidth = 650,
                              tags$li(class="dropdown",tags$a(href="https://rishabjn.netlify.app/", icon("youtube"),"My Portfolio", target="_blank")),
                              tags$li(class="dropdown",tags$a(href="https://rishabjn.netlify.app/", icon("linkedin"),"My Portfolio", target="_blank")),
                              tags$li(class="dropdown",tags$a(href="https://rishabjn.netlify.app/", icon("github"),"My Portfolio", target="_blank"))),
              dashboardSidebar(
                sidebarMenu(
                id="sidebar",
                menuItem("Dataset",tabName = "data",icon=icon("database")),
                menuItem(text="visuaization",tabName = "viz",icon=icon("chart-line")),
                conditionalPanel("input.sidebar == 'viz' && input.t2 == 'distro'", selectInput(inputId = "var1" , label ="Select the Variable" , choices = c1)),
                conditionalPanel("input.sidebar == 'viz' && input.t2 == 'trends' ", selectInput(inputId = "var2" , label ="Select the Arrest type" , choices = c2)),
                conditionalPanel("input.sidebar == 'viz' && input.t2 == 'relation' ", selectInput(inputId = "var3" , label ="Select the X variable" , choices = c1, selected = "Rape")),
                conditionalPanel("input.sidebar == 'viz' && input.t2 == 'relation' ", selectInput(inputId = "var4" , label ="Select the Y variable" , choices = c1, selected = "Assault")),
                menuItem("Choropleth Map", tabName = "map", icon=icon("map"))
                )
              ),
              dashboardBody(
                tags$head(
                tags$link(rel = "stylesheet", type = "text/css", href = "style.css")
                ),
                tabItems(
                tabItem(tabName = "data",
                        tabBox(id="t1",width = 12,
                               tabPanel("About", icon=icon("address-card"),
                                        fluidRow(
                                 column(width=8, tags$img(src="crime.jpg",width=600,height=300),
                                        tags$br(),
                                        tags$a("Photo by Jensen "),align="center"),
                                 column(width=4, tags$br(),
                                        tags$p("This data set comes along with base R and contains statistics, in arrests per 100,000 residents for assault, murder, and rape in each of the 50 US states in 1973. Also, given is the percent of the population living in urban areas."))
                               )),
                               tabPanel(title="Data", icon=icon("table"),dataTableOutput("dataT")),
                               tabPanel(title="Structure", icon=icon("uncharted"),verbatimTextOutput("structure")),
                               tabPanel(title="Summary Stats", icon=icon("chart-pie"),verbatimTextOutput("summary"))
                               )
                        ),
                tabItem(tabName = "viz", 
                        tabBox(id="t2",  width=12, 
                               tabPanel("Crime Trends by State", value="trends",
                                        fluidRow(tags$div(align="center", box(tableOutput("top5"), title = textOutput("head1") , collapsible = TRUE, status = "primary",  collapsed = TRUE, solidHeader = TRUE)),
                                                 tags$div(align="center", box(tableOutput("low5"), title = textOutput("head2") , collapsible = TRUE, status = "primary",  collapsed = TRUE, solidHeader = TRUE))
                                                 
                                        ),
                                        withSpinner(plotlyOutput("bar"))
                               ),
                               tabPanel("Distribution", value="distro",
                                        # selectInput("var", "Select the variable", choices=c("Rape", "Assault")),
                                        withSpinner(plotlyOutput("histplot", height = "350px"))),
                               tabPanel("Correlation Matrix", id="corr" , withSpinner(plotlyOutput("cor"))),
                               tabPanel("Relationship among Arrest types & Urban Population", 
                                        radioButtons(inputId ="fit" , label = "Select smooth method" , choices = c("loess", "lm"), selected = "lm" , inline = TRUE), 
                                        withSpinner(plotlyOutput("scatter")), value="relation"),
                               side = "left"
                        ),
                        
                ),
                
                #third tabItem
                tabItem(tabName="map" ,
                        box(selectInput("crimetype","Select Arrest Type", choices = c2, selected="Rape", width = 250),
                            withSpinner(plotOutput("map_plot")), width = 12)
              )
              )
            )
  ))

shinyApp(ui, server)
