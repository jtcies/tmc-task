library(shiny)
library(tidyverse)
library(lubridate)

candidate <- read_csv(here::here("data/candidate.csv"))
voteshare <- read_csv(here::here("data/voteshare.csv"))


## creates duplciates - check that this is ok
voteshare_merged <- voteshare %>% 
    left_join(candidate, by = c("district", "state", "party")) %>% 
    mutate_at(vars(contains("voteshare")), function(x) x / 100) %>%
    mutate(party = case_when(
               party == "D" ~ "Democrat",
               party == "R" ~ "Republican",
               TRUE ~ "Independent"
           ))

# plot colors

parties <- unique(voteshare_merged$party)[order(unique(voteshare_merged$party))]

colors = c("blue", "green", "red")

names(colors) <- parties

candidate_choices <- c("all", unique(voteshare_merged$candidate))

col_scale = scale_color_manual(values = colors)

state_choices <- setNames(
    c("all", unique(voteshare_merged$state)),
    c("all", unique(voteshare_merged$state))
)

party_choices <- setNames(
    c("all", unique(voteshare_merged$party)),
    c("all", unique(voteshare_merged$party))
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
            selectizeInput("candidate",
                        "Candidate (use 'all' for all candidates)",
                        choices = candidate_choices,
                        selected = "all"
            ),
            dateRangeInput("daterange",
                           "Date Range",
                           start = "2018-08-01",
                           end = "2018-11-06")
        ),
        
        mainPanel(
            plotOutput("voteshare_plot"),
            tableOutput("current_win_prob")
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
           filter(
               forecastdate >= input$daterange[[1]],
               forecastdate <= input$daterange[[2]]
           ) %>% 
           group_by(party, forecastdate) %>% 
           summarise_at(vars(contains("voteshare"), win_probability), mean)
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
                geom_ribbon(alpha = 0.1, size = 0, show.legend =  FALSE) +
                scale_y_continuous(limits = c(0, 1), labels = scales::percent_format()) +
                col_scale + 
                theme_minimal() +
                labs(
                    x = "date of forecast",
                    y = "predicted percent of votes"
                )
        
    })
    
    output$current_win_prob <- renderTable({
        
        filtered_data() %>% 
            filter(forecastdate == max(forecastdate)) %>% 
            select(party, win_probability) %>% 
            mutate(win_probability = scales::percent(win_probability )) %>% 
            spread(party, win_probability)
        
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
