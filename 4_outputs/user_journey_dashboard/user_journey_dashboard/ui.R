
library(shiny)
library(shinydashboard)
library(DT)

ui <- dashboardPage(
  skin = "black",

  dashboardHeader(
    title = div("CodeClan website navigation analytics",
      style = "color: #1b3445; 
                                    font-size: 20px; 
                                    text-align: left"
    ),
    titleWidth = 350
  ),

  dashboardSidebar(
    sidebarMenu(
      menuItem("Goal completions by Channel", tabName = "goal_channel", icon = icon("dashboard")),
      menuItem("Goal completions by Page", tabName = "goal_page", icon = icon("dashboard")),
      menuItem("User Journey", tabName = "user_journey", icon = icon("dashboard")),
      menuItem("User Aquisitions", tabName = "user_aquisitions", icon = icon("dashboard")),
      # Adding blank menuItem to give space for dateRangeInput
      menuItem(""),
      # Adding date picker that allows users to select specific date range and the plot automatically updates itself

      dateRangeInput(

        # InputId to link up date picker code within the server function
        inputId = "date_range",

        # Setting "Select date range" subheader to a h5 header and bold
        label = tags$b(tags$h5("Select date range")),

        # Start date in the date picker is by default is set to the first date that is found within the date column of clean_dashboard_data
        start = first(clean_dashboard_data$date),

        # End date in the date picker by default is set to the last date found within the date column of clean_dashboard_data
        end = last(clean_dashboard_data$date),

        #  min and max values are set to prevent users selecting values  in date picker that are outwith the dates available in clean_dashboard_data
        min = first(clean_dashboard_data$date),

        max = last(clean_dashboard_data$date)
      )
    )
  ),

  dashboardBody(
    tabItems(
      tabItem(
        tabName = "goal_channel",
        # Keegan insert code here
        fluidRow(
          shinydashboard::box(
            title = "Channel Conversions Graph",
            width = 9,
            "Channel Graph Content"
          ),
          shinydashboard::box(
            title = "Channel Conversions Table",
            width = 3,
            "Channel Conversions Table Content"
          )
        ),
        fluidRow(
          shinydashboard::box(
            title = "Social Media Conversions Graph",
            width = 9,
            "Social Media Graph Content"
          ),
          shinydashboard::box(
            title = "Social Media Conversions Table",
            width = 3,
            "Social Media Conversions Table Content"
          )
        )
      ),

      tabItem(
        tabName = "goal_page",
        # Amber insert code here
      ),

      # User Journey Dashboard UI Script ----------------------------------------


      tabItem(
        tabName = "user_journey",
       
       tags$h1(tags$b(titlePanel("First and Second Page Interaction for the last 12 Months"))),
        tags$br(),
       
        fluidRow(
          column(
            3,

            selectInput(
              inputId = "channel",
              label = "Select Channel",
              choices = unique(entry_page_user_flow$channel),
              selected = "organic search",
              multiple = FALSE
            )
          ),

          column(
            3,
            selectInput(
              inputId = "device",
              label = "Select Device",
              choices = unique(next_page_user_flow$device),
              selected = "desktop",
              multiple = FALSE
            )
          ),
          
          column(
              3,
              checkboxInput(
                  inputId = "not_set",
                  label = "Exclude (not set)",
                  value = FALSE
                  )
              )
          
        ),

       
       fluidRow(
           shinydashboard::box(DT::dataTableOutput("user_flow"),
                               title = "Behaviour Flow",
                               width = 12
           )
       ),

        fluidRow(
          shinydashboard::box(DT::dataTableOutput("entry_page"),
            title = "Entry Page Engagement",
            width = 12
          )
        ),
        
        fluidRow(
            shinydashboard::box(DT::dataTableOutput("next_page"),
                                title = "Next Page Engagement",
                                width = 12
            )
        )
      ),

      tabItem(
        tabName = "user_aquisitions",
        # Greg insert code here
      )
    )
  )
)
