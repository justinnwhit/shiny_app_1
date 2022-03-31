
library(shiny)
library(tidyverse)
library(rsconnect)

covid19 <- read_csv("https://raw.githubusercontent.com/nytimes/covid-19-data/master/us-states.csv")
census_pop_est_2018 <- read_csv("https://www.dropbox.com/s/6txwv3b4ng7pepe/us_census_2018_state_pop_est.csv?dl=1")

census_pop_est_2018 <- read_csv("https://www.dropbox.com/s/6txwv3b4ng7pepe/us_census_2018_state_pop_est.csv?dl=1") %>% 
  separate(state, into = c("dot","state"), extra = "merge") %>% 
  select(-dot) %>% 
  mutate(state = str_to_lower(state))

cases_with_2018_pop_est <-
  covid19 %>% 
  mutate(state = str_to_lower(state)) %>%
  left_join(census_pop_est_2018,
            by = c("state" = "state")) %>% 
  mutate(cases_per = (cases/`est_pop_2018`)*100000)




ui <- fluidPage(
  sliderInput(inputId = "date", 
              label = "Date Range",
              min = as.Date("2020-01-21"), 
              max = as.Date("2022-03-30"), 
              value = as.Date(c("2020-01-21","2022-03-30"))),
  selectInput("state",
              "Selesct state(s) of interest",
              multiple = TRUE,
              choices = unique(census_pop_est_2018$state)),
  submitButton("Submit"),
  plotOutput(outputId = "state_plot")
)

server <- function(input, output){
  output$state_plot <- renderPlot(
    cases_with_2018_pop_est %>% 
filter(state == input$state) %>% 
      ggplot(aes(x=date, y=cases_per, color=state))+
      geom_line()+
      scale_x_date(limits = input$date)
+
  labs(title = "Cases per 100,000",x="Date",y="Cases", color="States of Interest"))
}

shinyApp(ui = ui, server = server)
