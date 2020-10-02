
# Libraries ---------------------------------------------------------------
library(shiny) #needed to run
library(datavolley) #for all the other things
library(tidyverse) #for all the things
library(shinyWidgets) #picker input option rocks
library(shinydashboardPlus) #shinydashboardplus
library(shinydashboard) #needed to run dashboard version
library(DT) #needed to run data tables
library(janitor) #needed for adorn total


#WD needed for shiny to work but shouldn't be in the production pushed code or else it will break
#setwd("~/Documents/R Projects/volley_kpis")

# Create df from dvws ---------------------------------------------------------------
dvws <- dir(pattern = "dvw$", full.names = TRUE)
dvw_parse <- lapply(dvws, dv_read, skill_evaluation_decode = "volleymetrics")
df_not_wrangled <- do.call(rbind, lapply(dvw_parse, plays))

# Wrangle dvw df ---------------------------------------------------------------
    #Nothing against Ben Raymonds wording for some of the columns or what is missing... Just makes more sense to me to arrange things that make sense in my mind
df <- df_not_wrangled %>% 
    mutate(skill = ifelse(point == T, 'Point', skill),
           player_name = str_to_title(player_name), 
           player_name = ifelse(player_name == 'Sebastia Sole', 'Sebastian Sole', player_name), 
           atk_set_call = ifelse(skill == 'Attack', lag(set_code,1), NA), 
           team_rotation = ifelse(home_team_id == team_id, home_setter_position, visiting_setter_position), 
           opp_rotation = ifelse(home_team_id == team_id, visiting_setter_position, home_setter_position), 
           opponent = ifelse(home_team_id == team_id, visiting_team, home_team), 
           reception_team = ifelse(home_team == serving_team, visiting_team, home_team), 
           fbso = ifelse(skill == 'Attack' & lag(skill,2) == 'Reception', 'attack', NA), 
           bp = ifelse(serving_team == point_won_by, T, F), 
           so = ifelse(reception_team == point_won_by, T, F),
           score_diff = ifelse(home_team_id == team_id, home_team_score - visiting_team_score, visiting_team_score - home_team_score),
           crunch_zone = ifelse(score_diff <= 3 & home_team_score > 21 | visiting_team_score > 21, T, F)) %>% 
    filter(!is.na(team),
           !is.na(skill)) %>% 
    select(match_id, team, player_name, skill, evaluation_code, attack_code, set_code, skill_subtype, 
           num_players_numeric, home_team_score:visiting_setter_position, set_number, home_team:crunch_zone) %>% 
    rename(grade = evaluation_code,
           attack_combo = attack_code,
           set_call = set_code,
           details = skill_subtype,
           block = num_players_numeric)


# shiny ideas  ---------------------------------------------------------------
# I use this area when I am trying to get ideas together about how I need to wrangle the when creating speicifc outputs


# shiny dmeo ---------------------------------------------------------------
body <- 
    dashboardBody(
        fluidRow(
            tabBox(width = '150%',
                   tabPanel('Summary',
                   #Add stuff here!
                            fluidRow(
                                box(solidHeader = T, status = 'primary', title = 'Attacks',
                                    column(12,
                                           DT::dataTableOutput('AttackTable'))),
                                box(solidHeader = T, status = 'primary', title = 'Reception',
                                    column(12,
                                           DT::dataTableOutput('ReceptionTable')))),
                            fluidRow(
                                box(solidHeader = T, status = 'primary', title = 'Serve',
                                    column(12,
                                           DT::dataTableOutput('ServeTable'))),
                                box(solidHeader = T, status = 'primary', title = 'Serve',
                                    column(12,
                                           plotOutput('ServePlot'))))),
                   tabPanel('Team comparison'),
                   #Add stuff in this area as listed above
                   tabPanel('Serving'),
                   #Add stuff in this area as listed above
                   tabPanel('Reception'),
                   #Add stuff in this area as listed above
                   tabPanel('Attack'),
                   #Add stuff in this area as listed above
                   tabPanel('Set distribuition')
                   #Add stuff in this area as listed above
                   )))


shinyApp(
    ui <- 
        dashboardPagePlus(
            dashboardHeaderPlus(),
            dashboardSidebar( #you can creatre multiple inputs. Setter calls, oppontn rotations, which players are on the court, attack combos etc... Basically anything that is in 'df' you can use as a 'input' which is a filter you can interact with. Even types of plots, types of colors on plots etc etc.
                selectInput("SelectTeam", "Team", sort(unique(df$team)), selected = 'Sir Safety Conad Perugia'),
                pickerInput("SelectOpponent","Opponent", choices = NULL, options = list('actions-box' = T, size = 12), multiple = T),
                pickerInput("SelectRotation","Rotation", choices= NULL, options = list('actions-box' = T, size = 12), multiple = T), 
                checkboxGroupInput('SelectCZ','Crunch Zone', choices = c(T, F), selected = c(T, F))),
            body),
    
    server <- function(input, output, session) {
        
        
    #Start of reactive values
        #this allows filtering and buttons not resetting
        current_inOpponent <- reactiveVal()
        observe({
            current_inOpponent(input$SelectOpponent)  
        })
        current_inRotation <- reactiveVal()
        observe({
            current_inRotation(input$SelectRotation)  
        })
        current_inSetCall <- reactiveVal()
        observe({
            current_inSetCall(input$SelectSetCall)  
        })
    
    #Updating inputs
        #this talks to the filters saying, dont you reset!
        observeEvent(input$SelectTeam,{
            updatePickerInput(session,'SelectOpponent',selected = current_inOpponent(),
                              choices=unique(sort(df$opponent[df$team==input$SelectTeam])))
        })
        observeEvent(input$SelectOpponent,{
            updatePickerInput(session,'SelectRotation',selected = current_inRotation(),
                              choices=unique(sort(df$team_rotation[df$team == input$SelectTeam & df$opponent %in% input$SelectOpponent])))
        }) 
    
    #Create tables
        output$AttackTable  <- DT::renderDataTable({
            DT::datatable(df %>% 
                              filter(team %in% input$SelectTeam, 
                                     crunch_zone %in% input$SelectCZ,
                                     opponent %in% input$SelectOpponent, 
                                     team_rotation %in% input$SelectRotation) %>% 
                              group_by(player_name) %>% 
                              summarise(`Attack Attempt` = sum(skill == 'Attack'),
                                        kills = sum(skill == 'Attack' & grade == '#'),
                                        errors = sum(skill == 'Attack' & grade %in% c('=', '/')),
                                        `FBSO Attempt` = sum(fbso == 'attack', na.rm = T),
                                        fbso_kill = sum(fbso == 'attack' & grade == '#', na.rm = T),
                                        .groups = 'drop') %>% 
                              ungroup() %>% 
                              filter(!is.na(player_name)) %>% 
                              adorn_totals('row') %>% 
                              mutate(`Kill %` = kills / `Attack Attempt`,
                                     `FBSO Kill %` = fbso_kill / `FBSO Attempt`,
                                     `Attack Eff` = (kills - errors) / `Attack Attempt`) %>% 
                              rename(Player = player_name) %>% 
                              filter(`Attack Attempt` != 0) %>% 
                              select(-c('kills', 'errors', 'fbso_kill')) %>% 
                              mutate_if(is.numeric, round, 3),
                          extensions = 'FixedColumns',
                          filter = "top",
                          options = list(
                              fixedColumns = list(leftColumns =1),
                              scrollX = T,
                              scrollY = 200,
                              autoWidth = F,
                              lengthChange = F,
                              scroller = T,
                              paging = FALSE,
                              columnDefs = list(list(className = 'dt-center', targets ="_all")),
                              dom  = 't',
                              stateSave = F),
                          rownames = F) %>%
                formatStyle(c(1:13), `border-right` = "solid 1px") %>% 
                formatPercentage(c('Kill %', 'FBSO Kill %'), 2)
        }, height = 200, width = 200)
        
        output$ReceptionTable  <- DT::renderDataTable({
            DT::datatable(df %>% 
                              filter(team %in% input$SelectTeam, 
                                     crunch_zone %in% input$SelectCZ,
                                     opponent %in% input$SelectOpponent, 
                                     team_rotation %in% input$SelectRotation) %>% 
                              group_by(player_name) %>% 
                              summarise(`Reception Att` = sum(skill == 'Reception'),
                                        `Reception Err` = sum(skill == 'Reception' & grade == '='),
                                        `Reception #+` = sum(skill == 'Reception' & grade %in% c('#', '+')),
                                        `Reception !` = sum(skill == 'Reception' & grade == '!'),
                                        `Reception -` = sum(skill == 'Reception' & grade == '-'),
                                        `Reception =` = sum(skill == 'Reception' & grade == '='),
                                        `Reception /` = sum(skill == 'Reception' & grade == '/'),
                                        so = sum(skill == 'Reception' & so == T),
                                        .groups = 'drop') %>% 
                              ungroup() %>% 
                              adorn_totals('row') %>% 
                              mutate(`mod SO%` = so / `Reception Att`) %>% 
                              rename(Player = player_name) %>% 
                              filter(`Reception Att` != 0, !is.na(Player)) %>% 
                              select(-so),
                          extensions = 'FixedColumns',
                          filter = "top",
                          options = list(
                              fixedColumns = list(leftColumns =1),
                              scrollX = T,
                              scrollY = 200,
                              autoWidth = F,
                              lengthChange = F,
                              scroller = T,
                              paging = FALSE,
                              columnDefs = list(list(className = 'dt-center', targets ="_all")),
                              dom  = 't',
                              stateSave = F),
                          rownames = F) %>%
                formatStyle(c(1:13), `border-right` = "solid 1px") %>% 
                formatPercentage(c('mod SO%'), 2)
        }, height = 200, width = 500)
        
        output$ServeTable  <- DT::renderDataTable({
            DT::datatable(df %>% 
                              filter(team %in% input$SelectTeam, 
                                     crunch_zone %in% input$SelectCZ,
                                     opponent %in% input$SelectOpponent, 
                                     team_rotation %in% input$SelectRotation) %>% 
                              group_by(player_name) %>% 
                              summarise(`Serve Attempt` = sum(skill == 'Serve'),
                                        `Serve Error` = sum(skill == 'Serve' & grade == '='),
                                        `Serve Ace` = sum(skill == 'Serve' & grade == '#'),
                                        ps = sum(skill == 'Serve' & bp == T),
                                        .groups = 'drop') %>% 
                              ungroup() %>% 
                              adorn_totals('row') %>% 
                              mutate(`PS%` = ps / `Serve Attempt`) %>% 
                              rename(Player = player_name) %>% 
                              filter(!is.na(Player), `Serve Attempt` != 0),
                          extensions = 'FixedColumns',
                          filter = "top",
                          options = list(
                              fixedColumns = list(leftColumns =1),
                              scrollX = T,
                              scrollY = 200,
                              autoWidth = F,
                              lengthChange = F,
                              scroller = T,
                              paging = FALSE,
                              columnDefs = list(list(className = 'dt-center', targets ="_all")),
                              dom  = 't',
                              stateSave = F),
                          rownames = F) %>%
                formatStyle(c(1:13), `border-right` = "solid 1px") %>% 
                formatPercentage(c('PS%'), 2)
        }, height = 200)
    
    #Create plot
        output$ServePlot <- renderPlot({
            df %>% 
            filter(team %in% input$SelectTeam, 
                   crunch_zone %in% input$SelectCZ,
                   opponent %in% input$SelectOpponent, 
                   team_rotation %in% input$SelectRotation) %>% 
            group_by(match_id, player_name) %>% 
            summarise(srv_bp = sum(skill == 'Serve' & bp == T), 
                      srv_at = sum(skill == 'Serve')) %>% 
            ungroup() %>% 
            rename(`Serve Break Point` = srv_bp,
                   `Serve Attempt` = srv_at) %>% 
            ggplot(aes(`Serve Attempt`, `Serve Break Point`)) + 
            geom_point() + 
            theme_bw() +
            geom_smooth(se = F)
        })
    }) 
