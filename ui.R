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
      menuItem("Goal completions by Channel", tabName = "goal_channel", icon = icon("futbol")),
      menuItem("Goal completions by Page", tabName = "goal_page", icon = icon("futbol")),
      menuItem("User Journey", tabName = "user_journey", icon = icon("walking")),
      menuItem("User Aquisitions", tabName = "user_aquisitions", icon = icon("users")),
      # Adding blank menuItem to give space for dateRangeInput
      HTML("<br/><br/><br/>"),
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
          tabBox(
            title = "Goal Completions by Channel and Social Network",
            width = 12,
            height = "500px",
            tabPanel(
              "Channel",
              plotlyOutput("channel_conversions_plot", height = "400px"),
              radioButtons("downsampling_channel", 
                           "Summarise by Month or Day", 
                           c("Month", "Day"), 
                           selected = NULL)
            ),
            tabPanel(
              "Social Network",
              plotlyOutput("social_conversions_plot", height = "400px"),
              radioButtons("downsampling_social", 
                           "Summarise by Month or Day", 
                           c("Month", "Day"), 
                           selected = NULL)
            )
          )
        )
      ),

      tabItem(
        tabName = "goal_page",
        # Amber insert code here
        fluidRow(
          tabBox(
            title = tagList(shiny::icon("clipboard-check"), "Where Users Click on Events on the CodeClan Website"), 
            width = 12, height = "400px",
            tabPanel("Glasgow",
                     plotOutput("goal3cc_test1_plot", height = "300px"),
                     plotOutput("goal3cc_comparison_plot", height = "300px")),
            tabPanel("Edinburgh",
                     plotOutput("goal5cc_test1_plot", height = "300px"),
                     plotOutput("goal5cc_comparison_plot", height = "300px")))
            )
          ),
        
      

# User Journey Dashboard UI Script ----------------------------------------
      
      
      tabItem(
        tabName = "user_journey",
        
        
        tags$h1(tags$b(titlePanel("First and Second Page Interaction for the last 12 Months"))),
        tags$br(),
        
        fluidRow(
          column(
            3,
            # Selector button that allows user to select which channel to view the data by
            selectInput(
              inputId = "channel",
              label = "Select Channel",
              choices = unique(entry_page_user_flow$channel),
              selected = "direct",
              multiple = FALSE
            )
          ),
          
          column(
            3,
            
            # Selector button that allows user to select what device to view the data by
            selectInput(
              inputId = "device",
              label = "Select Device",
              choices = unique(next_page_user_flow$device),
              selected = "desktop",
              multiple = FALSE
            )
          ),
          
          # Checkbox that allows user to include or exclude (not set) values under second_page_path
          column(
            3,
            checkboxInput(
              inputId = "not_set",
              label = "Exclude (not set)",
              value = FALSE
            )
          )
          
        ),
        
        # Behaviour flow datatable visualisation
        fluidRow(
          shinydashboard::box(DT::dataTableOutput("user_flow"),
                              title = "Behaviour Flow",
                              width = 12
          )
        ),
        
        # Entry page engagement datatable visualisation
        fluidRow(
          shinydashboard::box(DT::dataTableOutput("entry_page"),
                              title = "Entry Page Engagement",
                              width = 12
          )
        ),
        
        # Next page datatable visualisation
        fluidRow(
          shinydashboard::box(DT::dataTableOutput("next_page"),
                              title = "Next Page Engagement",
                              width = 12
          )
        )
      ),

      tabItem(
        tabName = "user_aquisitions",
        # first row
        fluidRow(
          infoBoxOutput("total_number_users_Box"),
          infoBoxOutput("total_number_sessions_Box"),
          infoBoxOutput("mean_bounce_rate_Box"),
          tabBox(
            title = tagList(shiny::icon("walking"), "How users arrive on the CodeClan website"), width = 12, height = "850px",
            tabPanel("by channel",
                     plotlyOutput("users_by_channel_by_day_plot", height = "370px"),
                     tags$br(),
                     plotlyOutput("users_by_channel_by_month_plot", height = "370px"),
                     # radioButtons("which_time_channel_plot",
                     #              inline = TRUE,
                     #              tags$i("Select daily or monthly views"),
                     #              choices = c("daily", "monthly")
                     # )
                     ),
            tabPanel("by device",
                    plotlyOutput("users_by_device_by_day_plot", height = "370px"),
                    tags$br(),
                    plotlyOutput("users_by_device_by_month_plot", height = "370px"),
                    # radioButtons("which_time_device_plot",
                    #              inline = TRUE,
                    #              tags$i("Select daily or monthly views"),
                    #              choices = c("daily", "monthly")
                    # )
          )
        )
      )
    )
  )
)
)
