
library(shiny)
library(shinydashboard)
library(data.table)
library(dplyr)
library(ggvis)
library(tidyr)

#------------------------------------------------------------

tmp_data <- fread("new.csv", header=TRUE, verbose=FALSE, 
                  stringsAsFactors=FALSE, encoding = 'UTF-8')
d <- tbl_df(tmp_data) %>%
    mutate(date_form = as.Date(date_time, format="%d.%m.%y %H:%M"))

user_rank <- d %>%
    group_by(user) %>%
    arrange(desc(user))


body <- dashboardBody(
    fluidRow(
        column(width = 6,
            box(
                title = "Users Ranking",
                width = NULL,
                height = 720, #870
                ggvisOutput("barplot")
                #hr(),
                #dateRangeInput('dateRange',
                #               label = 'Date range input: yyyy-mm-dd',
                #               start = as.Date("2017-02-14", format="%Y-%m-%d"), end = Sys.Date()
                #)
            )
        ),
        column(width = 6,
               box(
                   title = "Overall Dynamics",
                   width = NULL,
                   height = 300,

                   ggvisOutput("dinamics")
               ),
               box(
                   title = "Personal Dynamics",
                   #collapsible = TRUE,
                   width = NULL,
                   height = 400,
                   status = "primary",
                   solidHeader = TRUE,
                   selectInput(
                       'e0', 'Select a user', 
                       choices = user_rank$user
                    ),
                   hr(),
                   ggvisOutput("dinamics_personal")
                   #textOutput("user")
                   
               )
        )
    )
)

dashboardPage(
  dashboardHeader(title="ЖГ: Speaking Stats"),
  dashboardSidebar(),
  body
)
