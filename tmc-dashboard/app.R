library(shiny)
library(tidyverse)
library(lubridate)
library(scales)

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

parties <- unique(voteshare_tidy$party)[order(unique(voteshare_tidy$party))]

colors = c("blue", "green", "red")

names(colors) <- parties

district_choices <- c("all", unique(voteshare_tidy$district))

col_scale = scale_color_manual(values = colors)

state_choices <- setNames(
    c("all", unique(voteshare_tidy$state)),
    c("all", unique(voteshare_tidy$state))
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
            selectizeInput("district",
                           "District",
                           district_choices,
                           selected = "all"
            ),
            dateInput("daterange",
                      "Prediction Date",
                      value = "2018-11-06",
                      min = "2018-08-01",
                      max = "2018-11-06"
            ),
            sliderInput("closecount",
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
            h4("Change in predicted voteshare over time"),
            plotOutput("voteshare_plot"),
            br(),
            h4("Closest Races (by probability of winning)"),
            tableOutput("closest_races")
        )
    )
)

server <- function(input, output, session) {
    
   
    filtered_data <- reactive({
        
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
           filter(forecastdate <= input$daterange) %>% 
           group_by(party, forecastdate) %>% 
           summarise_at(vars(contains("voteshare"), win_probability), mean)
    })
    
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
            filter(forecastdate <= input$daterange) %>% 
            filter(forecastdate == max(forecastdate)) %>% 
            select(district, party, win_probability,
                   starts_with("voteshare")) %>% 
            arrange(district)
        
    })
    
    output$party_win <- renderTable({
        
        dat <- filtered_candidates() %>% 
            group_by(district) %>% 
            mutate(
                win = if_else(voteshare > 0.5, 1, 0),
                tossup = if_else(max(voteshare) <= 0.5, 1L, 0L)
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
                    y = "average predicted voteshare"
                )
        
    })
    
    output$date_max <- renderText({
        
        d <- filtered_data() %>% 
            filter(forecastdate == max(forecastdate)) %>% 
            slice(1) %>% 
            pull(forecastdate)
        
        d <- format(d, "%m/%d/%Y")
        
        paste0("Predictions as of ", d[[1]])
    })
    
    output$closest_races <- renderTable({
        
        # need to correct this
        filtered_candidates() %>%
            select(district, party, win_probability) %>%
            spread(party, win_probability) %>%
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
