library(shinydashboard)
library(shinyIncubator)
library(zipcode)

data("zipcode")

header<-dashboardHeader(title = tags$a(href='http://walmart.com',
                                       tags$img(src="https://upload.wikimedia.org/wikipedia/commons/thumb/7/76/New_Walmart_Logo.svg/200px-New_Walmart_Logo.svg.png")))

body<-dashboardBody(
  
  fluidRow(
    column(width = 9,
           box(width = NULL, solidHeader = TRUE,
               leafletOutput("map",height = "400px")
           ),
           box(width=NULL,#background = "light-blue",
               dataTableOutput("ProductTable")
               
           )
    ),
    
    column(width=3,
           box(width=NULL, background = "yellow",
               selectInput("state", "State:","WA",choices=unique(zipcode$state)),
               uiOutput("city_selecter"),
               #selectizeInput("zip", "Zip Code:","99354",choices=zipcode$zip),
               uiOutput("store_selecter"),
               textInput("text","Search Item:"),
               actionButton("submit","Search")
               
           )
    )
  )
)

dashboardPage(skin="black",
  header,
  dashboardSidebar(disable = TRUE),
  body
)
