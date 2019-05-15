library(shiny)
library(tidyverse)
library(lubridate)
library(shinysky)

candidate <- read_csv(here::here("data/candidate.csv"))
voteshare <- read_csv(here::here("data/voteshare.csv"))


## creates duplciates - check that this is ok
voteshare_merged <- voteshare %>% 
    left_join(candidate, by = c("district", "state", "party")) %>% 
    mutate_at(vars(contains("voteshare")), function(x) x / 100) %>%
    replace_na(list(party = "Independent")) %>% 
    mutate(party = as.character(fct_lump(party, 2, other_level = "Independent")))

candidate_choices <- c(unique(voteshare_merged$candidate), "all")

state_choices <- setNames(
    c(unique(voteshare_merged$state), "all"),
    c(unique(voteshare_merged$state), "all")
)

party_choices <- setNames(
    c(unique(voteshare_merged$party), "all"),
    c(unique(voteshare_merged$party), "all")
)

ui <- fluidPage(

    titlePanel("Election forecast"),

    sidebarLayout(
        sidebarPanel(
            selectInput("state",
                        "State",
                        state_choices,
                        selected = "all"
            ),
            selectInput("party",
                        "Party",
                        party_choices,
                        selected = "all"
            ),
            select2Input("candidate",
                        "Candidate (use 'all' for all candidates)",
                        choices = list(candidate_choices = candidate_choices),
                        selected = "all",
                        type = "select"

            )
        ),
        
        mainPanel(
            plotOutput("voteshare_plot"),
            textOutput("current_win_prob"),
            tableOutput("win_prob_change")
        )
    )
)

server <- function(input, output) {
    
    filtered_data <- reactive({
        
        data <- voteshare_merged
        
        if(input$state != "all") {
            
            data <- data %>% 
                filter(state == input$state)
        }
        
         if(input$party != "all") {
            
            data <- data %>% 
                filter(party == input$party)
        }
        
         if(input$candidate != "all") {
            
            data <- data %>% 
                filter(candidate %in% input$candidate)
         } 
        
       data %>% 
           group_by(party, forecastdate) %>% 
           summarise_at(vars(contains("voteshare")), mean)
    })

    output$voteshare_plot <- renderPlot({
        
        filtered_data() %>% 
            ggplot(aes(x = forecastdate, 
                       y = voteshare,
                       ymin = p10_voteshare,
                       ymax = p90_voteshare,
                       group = party,
                       color = party)) +
                geom_point() +
                geom_line() +
                geom_ribbon(alpha = 0.1, size = 0) +
                scale_y_continuous(limits = c(0, 1), labels = scales::percent_format()) +
                scale_color_manual(values = c("blue", "green", "red")) +
                theme_minimal() +
                labs(
                    x = "date",
                    y = "predicted percent of votes"
                )
        
    })
    
    output$current_win_prob <- renderText({
        
        voteshare_merged %>% 
            filter(candidate == input$candidate) %>%
            arrange(desc(forecastdate)) %>% 
            slice(1) %>% 
            pull(win_probability)
        
    })
    
    output$win_prob_change <- renderTable({
        
        
        
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
