library(shiny)
library(tidyverse)
library(lubridate)
library(scales)
library(shinyBS)

voteshare <- read_csv(here::here("data/voteshare.csv"))


## creates duplciates - check that this is ok
voteshare_tidy <- voteshare %>% 
    mutate_at(vars(contains("voteshare")), function(x) x / 100) %>%
    mutate(party = case_when(
               party == "D" ~ "Democrat",
               party == "R" ~ "Republican",
               TRUE ~ "Independent"
           )) %>% 
    mutate(district = paste0(state, district)) %>% 
    arrange(desc(win_probability)) %>% 
    distinct(forecastdate, party, district, .keep_all = TRUE)

# plot colors

parties <- c("Democrat", "Independent", "Republican")

color_options <- c(parties, "toss-up")

colors = c("blue", "green", "red", "purple")

names(colors) <- color_options

col_scale = scale_color_manual(values = colors)

district_choices <- c("all", unique(voteshare_tidy$district))


state_choices <- setNames(
    c("all", unique(voteshare_tidy$state)),
    c("all", unique(voteshare_tidy$state))
)

ui <- fluidPage(

    titlePanel("Election forecast"),

    sidebarLayout(
        sidebarPanel(
            sliderInput(
                "turnout",
                "Party Turnout",
                min = 0.01,
                max = 0.99,
                value = 0.5
            ),
            bsTooltip(
                "turnout", 
                paste0("Slide to right for higher Democratic turnout,",
                " left for higher Republican turnout. Options on the extremes",
                " are much less likely to occur.")
            ),
            selectInput(
                "state",
                "State (use 'all' to view all states)",
                state_choices,
                selected = "all"
            ),
            selectizeInput(
                "district",
                "District (use 'all' to view all districts)",
                district_choices,
                selected = "all"
            ),
            dateInput(
                "daterange",
                "Prediction Date",
                value = "2018-11-06",
                min = "2018-08-01",
                max = "2018-11-06"
            ),
            sliderInput(
                "closecount",
                "Number of close races to show",
                min = 1,
                max = 435, 
                value = 5
            )
        ),
        
        mainPanel(
            h3(textOutput("date_max")),
            br(),
            h4("Predicted wins by party"),
            tableOutput("party_win"),
            br(),
            h4("Predicted wins over time"),
            plotOutput("seats_won_plot"),
            br(),
            h4("Closest Races"),
            tableOutput("closest_races")
        )
    )
)

server <- function(input, output, session) {
    
    filtered_candidates <- reactive({
        
        data <- voteshare_tidy
        
        
        if(input$state != "all") {
            
            data <- data %>% 
                filter(state == input$state)
        }
        
        
        if(input$district != "all") {
            
            data <- data %>% 
                filter(district %in% input$district)
        } 
        
        data %>% 
            arrange(district) %>% 
            mutate(
                sd = abs((p90_voteshare - voteshare) / 1.28),
                voteshare_update = case_when(
                    party == "Democrat" ~ qnorm(input$turnout, voteshare, sd),
                    party == "Republican" ~ qnorm(1 - input$turnout, voteshare, sd),
                    TRUE ~ voteshare
                )
            )
        
    })
    
    output$party_win <- renderTable({
        
        dat <- filtered_candidates() %>% 
            filter(forecastdate == input$daterange) %>% 
            group_by(district) %>% 
            mutate(
                win = if_else(voteshare_update > 0.5, 1, 0),
                tossup = if_else(max(voteshare_update) <= 0.5, 1L, 0L)
            ) %>% 
            ungroup() 
             
         party <- dat %>% 
            group_by(party) %>% 
            summarise(win = as.integer(sum(win))) %>% 
            spread(party, win)
         
         tossup <- dat %>% 
             distinct(district, tossup) %>% 
             summarise(`toss-up` = sum(tossup))
         
         bind_cols(party, tossup)
    })
    
    output$seats_won_plot <- renderPlot({
        
        filtered_candidates() %>%
            filter(forecastdate <= input$daterange) %>%
            select(forecastdate, district, voteshare_update, party) %>% 
            complete(forecastdate, district, party = parties, fill = list(voteshare_update = 0)) %>% 
            spread(party, voteshare_update) %>% 
            mutate(
                outcome = case_when(
                    Democrat > 0.5 ~ "Democrat",
                    Republican > 0.5 ~ "Republican",
                    Independent > 0.5 ~ "Independent",
                    TRUE ~ "toss-up"
                )
            ) %>% 
            count(forecastdate, outcome) %>% 
            ggplot(aes(x = forecastdate, 
                       y = n,
                       group = outcome,
                       color = outcome)) +
                geom_point() +
                geom_line() +
                col_scale + 
                theme_minimal() +
                labs(
                    x = "date of forecast",
                    y = "predicted wins"
                )
        
    })
    
    output$date_max <- renderText({
        
        d <- filtered_candidates() %>% 
            filter(forecastdate == max(forecastdate)) %>% 
            slice(1) %>% 
            pull(forecastdate)
        
        d <- format(d, "%m/%d/%Y")
        
        paste0("Predictions as of ", d[[1]])
    })
    
    output$closest_races <- renderTable({
        
        # need to correct this
        filtered_candidates() %>%
            filter(forecastdate == input$daterange) %>% 
            select(district, party, voteshare_update) %>%
            spread(party, voteshare_update) %>%
            mutate(dif = abs(Democrat - Republican)) %>%
            top_n(input$closecount, wt = -dif) %>%
            select(-dif) %>% 
            mutate_if(is.numeric, function(x)
                if_else(is.na(x), NA_character_, percent(x, accuracy = .01))
                )
        
    })

}

# Run the application 
shinyApp(ui = ui, server = server)
