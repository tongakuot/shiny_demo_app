# DEMO SHINY APP BY JONGLEI INSTITUTE OF TECHNOLOGY ----

# 1.0 Import the packages ----
library(shiny)
library(tidyverse)
library(gt)
library(DT)
library(shinyWidgets)
library(plotly)


# 1.1 FUNCTIONS ----
source("00_scripts/functions.R")

# 1.2  A Custom title ----
title  = tags$a(
  href = "https://jongleiinstitute.com",
  tags$img(
    src    = "jonglei_logo_main.png",
    height = "20",
    width  = "20"
  ),
  " Jonglei Institute of Technology" %>% strong()
)

# 1.3 Data ----
ss_data_tbl <- read_rds("00_data/ss_2008_census_data_tbl.rds")

# 2.0 UI SECTION ----
ui <- navbarPage(
  
  # 2.1  HEAD ----
  title       = title,
  collapsible = TRUE,
  selected    = "Analysis of the South Sudan 2008 Census Data" %>% strong(),
  theme       = shinythemes::shinytheme("flatly"),

  # 2.2 TabPanels ----
  tabPanel(
    title = "Analysis of the South Sudan 2008 Census Data" %>% strong(),
    div(
      class = "container",
      style = "padding-top: 75px;",
      
      # 2.3 Tables ----
        column(
          width = 4,
          tableOutput("state_totals")
        ),
         
         column(
           width = 1
         ),
      
        column(
          width = 6,
          DTOutput("pop_by_gender"),
        )
      ),
    
    br(), br(),
    
    # 2.4 Plots ----
    div(
      class = "container",
      column(
        width = 6,
        plotlyOutput("plot_1")
      ),
      
      column(
        width = 6,
        plotlyOutput("plot_2")
      )
    )
  ),
  
  br(), br(), br(),
  
  # 2.5 About ----
  tabPanel(
    title = "About Us" %>% strong(),
    div(
      class = "container",
      column(
        width = 12,
        div(
          class = "panel-header",
          h2("About Jonglei Institute") %>% strong(),
          div(
            p("Jonglei Institute of Technology (JIT) is a faith-based nonprofit organization established by a group of Lost Boys and Lost Girls of Sudan to empower South Sudanese diaspora families by providing them free online education, tutoring, mentoring services; Bible studies and spiritual support, counseling, and guidance; as well as free financial literacy training. JITeam strongly believes that these free services will help South Sudanese families to become successful. And when these families succeed, they will be able to raise successful kids, for successful families raise successful kids.")
          ),
          
          br(),
          
          div(
            p(
              class = "lead",
              "Alier Reng,",  tags$small("President")),
            p("Alier is a data science professional, statistician, and educator with over seven years of university teaching experience and about 5-year experience in healthcare analytics. He is passionate about delivering quality data-driven results with efficiency and high accuracy. As a former refugee, his life experiences have taught him the importance of patience, perseverance, pressure, and stress management.")
          ),
          
          br(),
          
          div(
            p(class = "lead",
              "Acknowledgement"
              ),
            p("I am grateful to Matt Dancho,", tags$a(href = "https://university.business-science.io/courses", "the Founder & CEO of Busines Science University"), "for teaching me R Shiny and data science with R in general! Please check his courses out to learn data science or R Shiny quickly and thoroughly." ),
          )
        )
      )
    )
  )
  
)

# 3.0 SERVER SECTION ----
server <- function(input, output, session) {
  
  # 3.1 Settings ----
  # 3.1.0 State Totals ----
  state_totals <- reactive(
    ss_data_tbl %>% compute_population_sums(totals = TRUE, State)
    
  )
  
  # 3.1.1 Population by State, and Gender ----
  full_data <- reactive(
    ss_data_tbl %>%
      
      # group and summarize
      compute_population_sums(
        totals = FALSE, 
        State, Gender)
  )

  # 3.2 Population tables ----
  # 3.2.0 Table 1 ----
  output$state_totals <- render_gt({
    
    state_totals() %>% 
      
      # Generate the population totals table
      tabulate_state_totals(Population, 
                                 head_bg_color = "#4caf50",
                                 title         = "Population by State",
                                 ft_note       = "Alier Reng, https://jongleiinstitute.com")
 
  })
  
  # 3.2.1 Table 2 ----
  output$pop_by_gender <- renderDT({
    
    full_data() %>% 
      
      # Initialize DT table
      datatable(
        caption = htmltools::tags$caption(
          'Table 1:', htmltools::em('South Sudan 2008 Census Data by State, and Gender.')
        ),
        
        filter  = "top",
        class   = 'cell-border stripe',
        options = list(
          autoWidth  = TRUE,
          rownames   = FALSE,
          columnDefs = list(list(className = 'dt-center', targets = 2:3))
        )
      )
  })
  
  # 3.3 Plots ----
  # 3.3.0 Plot 1 ----
  output$plot_1 <- renderPlotly({
    g <- state_totals () %>% 
      
      plot_population_data(totals = TRUE,
                           title   = 'Jonglei State has the largest population')
     
    # Plot an interactive graph with ggplotly
    ggplotly(g)
  })
 
  # 3.3.1 Plot 2
  output$plot_2 <- renderPlotly({
    g <- full_data() %>% 
      plot_population_data(totals = FALSE,
                             title   = 'Jonglei State has the largest population')

    # Plot an interactive graph with ggplotly
    ggplotly(g)
  }) 
}

# 4.0 RUN THE APP ----
shinyApp(ui, server)