#**************************************************
#*PACKAGES
#**************************************************
library(shiny)             # Load the Shiny package for building web applications
library(shinyjs)           # improve user experience with JavaScript
library(shinythemes)       # themes for shiny
library(bslib)             # Load the bslib package for customizing Bootstrap-based CSS stylesheets
library(dbplyr)            # Load the dbplyr package for using dplyr syntax to query databases directly
library(lubridate)         # Load the lubridate package for working with dates and times
library(plotly)            # Load the plotly package for creating interactive plots
library(bsicons)           # Load the bsicons package for adding Bootstrap icons to Shiny applications
library(reactable)         # Load the reactable package for creating interactive tables
library(echarts4r)         # Load the echarts4r package for creating interactive charts and maps
library(DBI)               # Load the DBI package for database interface
library(stringr)           # Load the stringr package for working with strings
library(scales)            # Load the scales package for formatting numbers and dates
library(readr)             # Load the readr package for reading and parsing data files
library(shinyWidgets)      # Load the shinyWidgets package for adding custom widgets to Shiny applications
library(shinymanager)      # Load the shinymanager package for adding user authentication to Shiny applications
library(htmltools)         # Load the htmltools package for working with HTML code in R
library(htmlwidgets)       # Load the htmlwidgets package for creating interactive widgets
library(shiny.fluent)      # Load the shiny.fluent package for creating Shiny applications with a Fluent Design style
# library(RMySQL)            # Load the RMySQL package for interfacing with MySQL databases from R
library(janitor)           # Load the janitor package for cleaning and formatting messy data
library(heatmaply)
library(mapboxer)
library(bcrypt)
library(dplyr)             # Load the dplyr package for data manipulation
library(shinyauthr)

#************************************************** 
#*GLOBAL PARAMETERS
#**************************************************
#*
# *******  parameters 
# Define parameters for the dashboard, such as color scheme, logo, and title

primary_colour <- "#008080" #"#0675DD"
name_dashboard <- "ANNUAL HOUSEHOLD INCOME & EXPENDITURE SURVEY, 2023"
logo_location  <- "images/logo.jpg"
colour_1 <- "#F66068"
colour_2 <- "#206095"

# Set color palette for male and female
sex <- c(colour_1, colour_2)
pal <- setNames(sex, c("Male", "Female"))



# Define UI for application that draws a histogram
shinyUI(fluidPage(
  fluidRow(
    column(12,
           page_navbar(
             tags$head(
               tags$style(
                 HTML(
                   '
          /* Align header text to the bottom */
          .header,
          .group-header {
            display: flex;
            flex-direction: column;
            justify-content: flex-end;
          }
          
          .header {
            border-bottom-color: #555;
            font-size: 13px;
            font-weight: 400;
            text-transform: uppercase;
          }
          
          /* Highlight headers when sorting */
          .header:hover,
          .header[aria-sort="ascending"],
          .header[aria-sort="descending"] {
            background-color: #eee;
          }
          
          .border-left {
            border-left: 2px solid #555;
          }
          
          .selectize-input {height: 100px;
          width: 500px;}
          ')
               )
             ),
             fluid = TRUE,
             # This styles the look and feel of the plot
             theme = bs_theme(
               version = 5,
               base_font = font_google("Lato"),
               "primary" = primary_colour,
               "navbar-bg" = primary_colour
             ),
             
             # This includes a logo to the app
             title = tags$span(
               tags$img(src = logo_location,
                        style = "width:40px;height:auto;margin-right:24px;"),
               name_dashboard,
               
               # add logout button UI
               span(class = "pull-right", shinyauthr::logoutUI(id = "logout")),
               
             ),
             
             # nav("overview", title = "Overview"),
             # nav("team_enumerator", title = "Team and Enumerator"),
             # nav("survey_data", title = "Survey Data"),
             # nav("individual_data",title = "Individual Data"),
           ),
           
           # tabsetPanel(type = "tabs",
           # tabPanel("Plot", plotOutput("plot")),
           # tabPanel("Summary", verbatimTextOutput("summary")),
           # tabPanel("Table", tableOutput("table")),
           
           mainPanel(
             # Output: Tabset w/ plot, summary, and table ----
             tabsetPanel(type = "tabs",
                         tabPanel("User Guide", uiOutput("user_guide")),
                         tabPanel("Overview", uiOutput("overview")),
                         tabPanel("Team & Enumerator", uiOutput("team_enumerator")),
                         tabPanel("Survey Data", tableOutput("survey_date"))
             ),

             width = 12),
           
           # add login panel UI function
           shinyauthr::loginUI(id = "login"),
    )
  )
  
  # # this adds the first tab
  #uiOutput("Overview")
  
))

# uiOutput("mainTab"),
# uiOutput("user_guide"),
# uiOutput("overview"),
# uiOutput("team_enumerator"),

# The HTML tag functions in Shiny, 
#like div() and p() return objects that can be rendered as HTML


# this adds the second tab
# nav("Summary by team and enumerator", page_2_ui),
# # this adds the third tab (NOT THERE YET AS THERE IS ONLY LISTING DATA SO FAR)
# # nav("Summary of survey data", page_3_ui)
# nav("output per EA", page_4_ui),
# nav("summary boxes per team", page_5_ui),
# nav("Listing Itenary", page_6_ui),
# nav("Listing errors", page_7_ui)

