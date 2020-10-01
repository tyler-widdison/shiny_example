
# Libraries ---------------------------------------------------------------
library(shiny)
library(datavolley)
library(tidyverse)
library(shinyWidgets)
library(shinydashboard)
library(DT)
library(shinydashboardPlus)


#WD needed for shiny to work
#setwd("~/Documents/R Projects/volley_kpis")

# Create df from dvws ---------------------------------------------------------------
dvws <- dir(pattern = "dvw$", full.names = TRUE)
dvw_parse <- lapply(dvws, dv_read, skill_evaluation_decode = "volleymetrics")
df_not_parsed <- do.call(rbind, lapply(dvw_parse, plays))

# Wrangle dvw df ---------------------------------------------------------------
df_not_parsed <- df_not_parsed %>% 
    mutate(skill = ifelse(point == T, 'Point', skill)) %>% 
    filter(!is.na(skill))
    
    
# shiny dmeo ---------------------------------------------------------------
df <- df_not_parsed %>% 
    mutate(atk_set_call = ifelse(skill == 'Attack', lag(set_code,1), NA),
           team_rotation = ifelse(home_team_id == team_id, home_setter_position, visiting_setter_position),
           opponent = ifelse(home_team_id == team_id, visiting_team, home_team)) %>% 
    filter(team == 'Sir Safety Conad Perugia', !is.na(atk_set_call), skill == 'Attack')

# shiny dmeo ---------------------------------------------------------------


body <- 
    dashboardBody(
        fluidRow(
            tabBox(width = '150%',
                   tabPanel('Summary',
                            fluidRow(
                                column(12, box(title = 'Attacks',DT::dataTableOutput('table'),width = NULL)))), 
                   tabPanel('Team comparison'),
                   tabPanel('Serving'),
                   tabPanel('Reception'),
                   tabPanel('Attack'),
                   tabPanel('Set distribuition'))))


shinyApp(
    ui <- 
        dashboardPagePlus(
            dashboardHeaderPlus(),
            dashboardSidebar(
                pickerInput("SelectOpponent","Opponent", sort(unique(df$opponent)), selected = 'Doppler/Horst', options = list('actions-box' = T, size = 12), multiple = T),
                pickerInput("SelectRotation","Rotation", choices= NULL, options = list('actions-box' = T, size = 12), multiple = T), 
                pickerInput("SelectSetCall","Set Call", choices= NULL, options = list('actions-box' = T, size = 12), multiple = T), 
                checkboxGroupInput('SelectMW','Match Won', choices = c('Yes', 'No')),
                checkboxGroupInput('SelectCZ','Crunch Zone', choices = c('Yes', 'No'))),
            body),
    
    server <- function(input, output, session) {
        
        #this allows filtering and buttons not resetting
        current_inRotation <- reactiveVal()
        observe({
            current_inRotation(input$SelectRotation)  
        })
        current_inSetCall <- reactiveVal()
        observe({
            current_inSetCall(input$SelectSetCall)  
        })
        
        #this talks to the filters saying, dont you reset!
        observeEvent(input$SelectOpponent,{
            updatePickerInput(session,'SelectRotation',selected = current_inRotation(),
                              choices=unique(sort(df$team_rotation[df$opponent==input$SelectOpponent])))
        }) 
        observeEvent(input$SelectRotation,{
            updatePickerInput(session,'SelectSetCall',selected = current_inSetCall(),
                              choices=unique(sort(df$atk_set_call[df$opponent==input$SelectOpponent & df$team_rotation %in% input$SelectRotation])))
        })
        
        
        
        
        output$table  <- DT::renderDataTable({
            DT::datatable(df %>% 
                                  filter(opponent %in% input$SelectOpponent, team_rotation %in% input$SelectRotation, atk_set_call %in% input$SelectSetCall, skill == 'Attack', !is.na(atk_set_call)) %>% 
                              group_by(team_rotation) %>% 
                              summarise(k1 = sum(atk_set_call == 'K1'), 
                                        k2 = sum(atk_set_call == 'K2'), 
                                        k7 = sum(atk_set_call == 'K7'), 
                                        k1 = sum(atk_set_call == 'K1'), 
                                        km = sum(atk_set_call == 'KM'), 
                                        kb = sum(atk_set_call == 'KB'),
                                        .groups = 'drop') %>% 
                              ungroup(),
                          class = 'disply nowrap compact',
                          rownames = FALSE,
                          options = list(
                              scrollX = 500,
                              lengthChange = FALSE,
                              scroller = T,
                              columnDefs = list(list(className = 'dt-center', targets ="_all")),
                              dom  = 't',
                              stateSave = FALSE)) %>% 
                formatStyle(columns = c(1), fontSize = '75%')
        })
        }) 
