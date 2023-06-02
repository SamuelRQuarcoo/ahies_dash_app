#**************************************************
#*PACKAGES
#**************************************************
#*

#remove.packages("ggplot2")
#install.packages("ggplot2")
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
library(dplyr)
library(shinyauthr)
# dataframe that holds usernames, passwords and other user data
user_base <- tibble::tibble(
  user = c("user1", "user2"),
  password = c("pass1", "pass2"),
  permissions = c("admin", "standard"),
  name = c("User One", "User Two")
)

#* *************************************************
#* LOAD SURVEY DATA
#* *************************************************

#* SET DATA FILE PATH
data_path <- "input_data/"

# pff file 
# pff_file <- "ahies_download_app.pff"

#run pff file to download data
#openFile(pff_file)


#data_file <- paste0(data_path, "ahies_20230529.csdb")

#* LOAD CSDB FILE
#* **************
# dbConn <- dbConnect(RSQLite::SQLite(), data_file) 
# dbListTables(dbConn)

#******************************************************************************************************
#*                                CONNECT TO CSPRO DATABASE AND CONVERT TO RDS
#******************************************************************************************************

#* IDENTIFICATION
#* **************
# df_identification <- tbl(dbConn, 'level-1') %>%
#   #select(`id00`, `idq0`, `id01`, `id02`) %>%
#   collect() %>%
#   janitor::clean_names() %>% 
#   filter(idq0 == 2) #QUARTER 2
# 
# #* save and read dataframe as RDS
# saveRDS(df_identification, file = "input_data/identification.RDS")

#* CASES
#* *************
# df_cases <- tbl(dbConn, 'cases') %>%
#   #select(`id`, `key`, `deleted`, `partial_save_mode`) %>% 
#   collect() %>%
#   janitor::clean_names() 
# 
# #* save and read dataframe as RDS
# saveRDS(df_cases, file = "input_data/cases.RDS")

#* HOUSEHOLD
#* **************
# df_household <- tbl(dbConn, 'hhrecord') %>%
#   collect() %>%
#   janitor::clean_names() %>% 
#   inner_join(df_identification, by = ("level_1_id"))
# 
# #* save and read dataframe as RDS
# saveRDS(df_household, file = "input_data/household.RDS")

#* METADATA
#* **************
# df_metadata <- tbl(dbConn, 'metadata') %>%
#   collect() %>%
#   janitor::clean_names() %>% 
#   inner_join(df_household, by = ("level_1_id"))
# 
# #* save and read dataframe as RDS
# saveRDS(df_metadata, file = "input_data/metadata.RDS")

#* INDIVIDUAL
#* **************
# df_indiv <- tbl(dbConn, 'indrecord') %>%
#   collect() %>%
#   janitor::clean_names() %>% 
#   inner_join(df_identification, by = ("level_1_id"))
# 
# #* save and read dataframe as RDS
# saveRDS(df_indiv, file = "input_data/indiv.RDS")

#******************************************************************************************************
#*                                DATAFRAMES/SECTIONS OF QUESTIONNAIRE
#******************************************************************************************************

#* *************************************************
#* ID 
#* *************************************************
rds_identification <- readRDS("input_data/identification.RDS")
rds_identification <- rds_identification %>% filter(idq0 == 2)

#* *************************************************
#* CASES 
#* *************************************************
rds_cases <- readRDS("input_data/cases.RDS")


#* partial saves
partial_saves <- rds_cases %>% 
  count(partial_save_mode)

#* 1
partial_save_1 <- partial_saves %>% filter(partial_save_mode == 1)
partial_save_1 <- partial_save_1$n

#* 2
partial_save_2 <- partial_saves %>% filter(partial_save_mode == 2)
partial_save_2 <- partial_save_2$n

?count

partial_save_total <- partial_save_1 + partial_save_2
partial_save_percent <- as.character(round((partial_save_total)/nrow(rds_cases) * 100, digits = 1))
partial_save_total <- as.character(partial_save_total)

#* deleted cases
deleted_cases <- rds_cases %>% 
  filter(deleted > 0)

deleted_total <- nrow(deleted_cases)
deleted_percent <- round(deleted_total/nrow(rds_cases) * 100, digits = 1)

#* *************************************************
#* Household 
#* *************************************************
rds_household <- readRDS("input_data/household.RDS") %>% 
  inner_join(rds_identification, by = "level_1_id")
  
#***************************************************
#* Meta Data 
#* *************************************************
rds_metadata <- readRDS("input_data/metadata.RDS")
rds_metadata <- rds_metadata %>% 
  inner_join(rds_identification, by = "level_1_id")

#***************************************************
#* Individual Data 
#* *************************************************
rds_indiv <- readRDS("input_data/indiv.RDS") %>% 
  inner_join(rds_identification, by = "level_1_id")

  
#* total households enumerated
hhold_enumerated <- formatC(nrow(rds_household), big.mark = ",")

#* number of households enumerated in the last 3 days
cur_date <- Sys.Date()
df_hhold_with_data_3 <- rds_metadata %>% 
  select(hhrecord_id, metadata_id, v04, v05, v09, id00.y, idq0.y, id01.y, id02.y) %>% 
  filter(v05 > 0) %>% 
  mutate(no_days = difftime(cur_date, strptime(v05, format = "%Y%m%d"), units = 'days')) %>% 
  filter(no_days <= 7) 
  
hhold_with_data_3 <- formatC(nrow(df_hhold_with_data_3), big.mark = ",")

#* no of individuals in data
no_of_indiv <- formatC(nrow(rds_indiv), big.mark = ",")

#* houshold size
hhold_size <- round(nrow(rds_indiv)/nrow(rds_household), digits = 1)

#* EAs with data
df_eas_with_data <- rds_household %>% 
  select(id01.y) %>% 
  distinct(id01.y) %>% 
  group_by(id01.y) %>%
  summarize(count_nos = n()) %>% 
  filter(!is.na(id01.y))
  
eas_with_data <- formatC(nrow(df_eas_with_data), big.mark = ",")

#* EAs with data - past 24 hrs
df_eas_with_data_3 <- rds_metadata %>% 
  select(hhrecord_id, metadata_id, v04, v05, v09, id00.y, idq0.y, id01.y, id02.y) %>% 
  mutate(no_days = as.character(round(difftime(cur_date, strptime(v05, format = "%Y%m%d"), units = 'days')), digit = 1)) %>% 
  filter(no_days %in% c("3")) %>% 
  distinct(id01.y) %>% 
  filter(id01.y > 0)
 
eas_with_data_3 <- as.character(nrow(df_eas_with_data_3))

#* Enumerators with synced data - past 24 hrs
df_enumerators_with_data_3 <- rds_metadata %>% 
  select(hhrecord_id, metadata_id, v04, v05, v09, id00.y, idq0.y, id01.y, id02.y, v01) %>% 
  filter(v01 > 0) %>%  
  mutate(no_days = as.character(round(difftime(cur_date, strptime(v05, format = "%Y%m%d"), units = 'days')), digit = 1)) %>%  
  filter(no_days %in% c("3"))

enumerators_with_data_3 <- as.character(nrow(df_enumerators_with_data_3))

#* Duplicate cases
df_duplicate_cases <- rds_cases %>% 
  inner_join(rds_household, by=c("id" = "case_id.x")) %>% 
  select(id) %>% 
  group_by(id) %>% 
  summarize(count_id = n()) %>% 
  filter(count_id > 1)

#* save and read dataframe as RDS
# #saveRDS(df_duplicate_cases, file = "input_data/duplicate_cases.RDS")
# rds_duplicate_cases <- readRDS("input_data/duplicate_cases.RDS")

duplicate_cases <- as.character(nrow(df_duplicate_cases)) 

#* percentage of male only households
df_sex <- rds_household %>% 
  inner_join(rds_indiv, by = "level_1_id") %>%
  group_by(id02.y.y, s1aq1) %>% 
  summarize(count_gender = n())

df_males_only <- df_sex %>% 
  filter(s1aq1 == 1)

male_count <- df_sex %>% filter(s1aq1 == 1) %>% pull
female_count <- df_sex %>% filter(s1aq1 == 2) %>% pull

#************************************************** 
#*USER INTERFACE
#**************************************************
#*

#* User Guide
user_guide <- layout_column_wrap(
  tags$head(tags$style(HTML(".small-box {height: 50px}"))),
  width = 1,
  heights_equal = "row",
  card(
    card_header("Guide to Using the AHIES Dashboard"),
    layout_column_wrap(
      width = "100px",
      card(
        full_screen = TRUE,
        layout_column_wrap(
          width = "50px",
          value_box(
            title = tags$p("Households Enumerated",  style = "font-size: 100%;"),
            value = tags$p(as.character(hhold_enumerated), style = "font-size: 150%; color: yellow;"),
            showcase = bs_icon("house"),
            height = 20,
            style = "font-size: 80%;"
          ),
          
        ),
        
        tags$p(),
        
        layout_column_wrap(
          width = "50px",
          value_box(
            title = tags$p("Average Household Size", style = "font-size: 100%;"),
            value = tags$p(as.character(hhold_size), style = "font-size: 150%; color: yellow;"),
            showcase = bs_icon("file-break"),
          ),
        ),
        
        tags$p(),
        
        layout_column_wrap(
          width = "50px",
          value_box(
            title = tags$p("Males", style = "font-size: 100%;"),
            value = tags$p(as.character(99999), style = "font-size: 150%; color: yellow;"),
            showcase = bs_icon("gender-male"),
          ),
        )
        
      )
    )
  ))



#*Summary
page_1 <- layout_column_wrap(
  tags$head(tags$style(HTML(".small-box {height: 50px}"))),
  width = 1,
  heights_equal = "row",
  card(
    card_header("Summary of Enumeration Data"),
    layout_column_wrap(
    width = "100px",
    card(
      full_screen = TRUE,
      layout_column_wrap(
        width = "50px",
        value_box(
          title = tags$p("Households Enumerated",  style = "font-size: 100%;"),
          value = tags$p(as.character(hhold_enumerated), style = "font-size: 150%; color: yellow;"),
          showcase = bs_icon("house"),
          height = 20,
          style = "font-size: 80%;"
        ),
        value_box(
          title = tags$p("Households Enumerated (3 days)", style = "font-size: 100%;"),
          value = tags$p(as.character(hhold_with_data_3), style = "font-size: 150%; color: yellow;"),
          showcase = bs_icon("house-add"),
        ),
        value_box(
          title = tags$p("Individuals in Data", style = "font-size: 100%;"),
          value = tags$p(as.character(no_of_indiv), style = "font-size: 150%; color: yellow;"),
          showcase = bs_icon("people"),
        ),
        value_box(
          title = tags$p("Partial Saves", style = "font-size: 100%;"),
          value = tags$p( paste0(partial_save_total, " (", partial_save_percent, "%)"), 
                          style = "font-size: 150%; color: yellow;"),
          showcase = bs_icon("mask"),
        ),
        value_box(
          title = tags$p("Deleted Cases", style = "font-size: 100%;"),
          value = tags$p( paste0(deleted_total, " (", deleted_percent, "%)"), 
                          style = "font-size: 150%; color: yellow;"),
          showcase = bs_icon("x-circle"),
        )
      ),
      
      tags$p(),
      
      layout_column_wrap(
        width = "50px",
        value_box(
          title = tags$p("Average Household Size", style = "font-size: 100%;"),
          value = tags$p(as.character(hhold_size), style = "font-size: 150%; color: yellow;"),
          showcase = bs_icon("file-break"),
        ),
        value_box(
          title = tags$p("EAs with Data", style = "font-size: 100%;"),
          value = tags$p(as.character(eas_with_data), style = "font-size: 150%; color: yellow;"),
          showcase = bs_icon("pin-map")
        ),
        value_box(
          title = tags$p("EAs with Data (3days)", style = "font-size: 100%;"),
          value = tags$p(eas_with_data_3, style = "font-size: 150%; color: yellow;"),
          showcase = bs_icon("pin-map-fill"),
        ),
        value_box(
          title = tags$p("Enumerators with synced data (3days)", style = "font-size: 100%;"),
          value = tags$p(enumerators_with_data_3, style = "font-size: 150%; color: yellow;"),
          showcase = bs_icon("clock-history"),
        ),
        value_box(
          title = tags$p("Duplicate Cases", style = "font-size: 100%;"),
          value = tags$p(duplicate_cases, style = "font-size: 150%; color: yellow;"),
          showcase = bs_icon("clipboard"),
        ),
      ),
      
      tags$p(),
      
      layout_column_wrap(
        width = "50px",
        value_box(
          title = tags$p("Males", style = "font-size: 100%;"),
          value = tags$p(as.character(9999), style = "font-size: 150%; color: yellow;"),
          showcase = bs_icon("gender-male"),
        ),
        value_box(
          title = tags$p("Females", style = "font-size: 100%;"),
          value = tags$p(as.character(9999), style = "font-size: 150%; color: yellow;"),
          showcase = bs_icon("gender-female")
        ),
        value_box(
          title = tags$p("EAs with Data (3days)", style = "font-size: 100%;"),
          value = tags$p(eas_with_data_3, style = "font-size: 150%; color: yellow;"),
          showcase = bs_icon("pin-map-fill"),
        ),
        value_box(
          title = tags$p("Enumerators with synced data (3days)", style = "font-size: 100%;"),
          value = tags$p(enumerators_with_data_3, style = "font-size: 150%; color: yellow;"),
          showcase = bs_icon("clock-history"),
        ),
        value_box(
          title = tags$p("Duplicate Cases", style = "font-size: 100%;"),
          value = tags$p(duplicate_cases, style = "font-size: 150%; color: yellow;"),
          showcase = bs_icon("clipboard"),
        ),
      )
      
    )
  )
))
  

page_2 <- layout_column_wrap(
  width = 1,
  heights_equal = "row",
  card(
    card_header("Summary of Enumeration Data"),
    layout_column_wrap(
      width = 1,
      card(
        full_screen = TRUE,
        layout_column_wrap(
          width = "250px",
          value_box(
            title = "Households Enumerated", 
            value = hhold_enumerated,
            showcase = bs_icon("house"),
            p("total households covered in survey")
          ),
          value_box(
            title = "Household Enumerated (past 3 days)", 
            value = as.character(hhold_with_data_3),
            showcase = bs_icon("house-add"),
            p("The 2nd detail"),
          ),
          value_box(
            title = "Individuals in Data", 
            value = "789",
            showcase = bs_icon("pencil-square"),
            p("number of individuals in enumeration data"),
          )
        )
      ),
    )
  ),
)


#************************************************** 
#*SERVER FUNCTION
#**************************************************
shinyServer(function(input, output, session) {

  # call login module supplying data frame, 
  # user and password cols and reactive trigger
  credentials <- shinyauthr::loginServer(
    id = "login",
    data = user_base,
    user_col = user,
    pwd_col = password,
    log_out = reactive(logout_init())
  )

  # call the logout module with reactive trigger to hide/show
  logout_init <- shinyauthr::logoutServer(
    id = "logout",
    active = reactive(credentials()$user_auth))
  
  output$overview <- renderUI({
    # use req to only render results when credentials()$user_auth is TRUE
    req(credentials()$user_auth) # only run after a successful login
    page_1
  })
  
  # output$team_enumerator <- renderUI({
  #   # use req to only render results when credentials()$user_auth is TRUE
  #   #req(credentials()$user_auth) # only run after a successful login
  #   page_2
  # })

})

# output$mainTab <- renderUI(
#   mainPanel(
#     tabsetPanel(type = "tabs",
#                 tabPanel("User Guide", uiOutput("user_guide")),
#                 tabPanel("Overview", uiOutput("overview")),
#                 tabPanel("Overview", uiOutput("overview")),
#     ), width = 12
#   )
# )

# card_header("Summary of Enumeration Data"),
# layout_column_wrap(
#   width = "200px",
#   
#   value_box(
#     title = "total households enumerated", 
#     value = no_hh_enumerated,
#     showcase = bs_icon("house"),
#     #p("The 1st detail")