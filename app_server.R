#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(lintr)
library("tidyverse", warn.conflicts = FALSE)
library("plotly", warn.conflicts = FALSE)
library("leaflet", warn.conflicts = FALSE)
library("ggplot2", warn.conflicts = FALSE)
dataset <- read_csv("https://raw.githubusercontent.com/owid/co2-data/master/owid-co2-data.csv", show_col_types = FALSE)
lint("app_server.R")

## show the highest Co2 amount in the world
highest_co2_production_by_country <- dataset %>%
  filter(year > 1950) %>%
  filter(!is.na(iso_code)) %>%
  filter(country != "World") %>%
  select(country, year, co2) %>%
  group_by(country) %>%
  summarise(
    Total_pollution = sum(co2, na.rm = TRUE)) %>%
  filter(Total_pollution == max(Total_pollution, na.rm = TRUE)) %>%
  pull(Total_pollution)

## show the highest Co2 amount in the world
highest_co2_country <- dataset %>%
  filter(year > 1950) %>%
  filter(!is.na(iso_code)) %>%
  filter(country != "World") %>%
  select(country, year, co2) %>%
  group_by(country) %>%
  summarise(
    Total_pollution = sum(co2, na.rm = TRUE)) %>%
  filter(Total_pollution == max(Total_pollution, na.rm = TRUE)) %>%
  pull(country)

## the amount of Co2 produced in the world from 1750 to 2021
total_co2_production <- dataset %>%
  filter(year > 1950) %>%
  filter(!is.na(iso_code)) %>%
  filter(country != "World") %>%
  select(co2) %>%
  summarise(
    Total_pollution = sum(co2, na.rm = TRUE)) %>%
  pull(Total_pollution)

##the average co2 from 1750 to 2021
the_average_co2 <- dataset %>%
  filter(!is.na(iso_code)) %>%
  filter(country != "World") %>%
  select(co2) %>%
  summarize(
    pollution = mean(co2, na.rm = TRUE)) %>%
  pull(pollution)

##the average co2 from in 2021
the_average_co2_in2021 <- dataset %>%
  filter(year == 2021) %>%
  filter(!is.na(iso_code)) %>%
  filter(country != "World") %>%
  select(co2) %>%
  summarize(
    pollution = mean(co2, na.rm = TRUE)) %>%
  pull(pollution)

## The range of co2 from 1750 to 2021
the_range_co2 <- dataset %>%
  filter(!is.na(iso_code)) %>%
  filter(country != "World") %>%
  select(co2) %>%
  summarize(
    range = max(co2, na.rm = TRUE) - min(co2, na.rm = TRUE)) %>%
  pull(range)

type <- c("the average amount of carbon dioxide production from 1750 to 2021",
          "The average amount of carbon dioxide produced in 2021",
          "The amount of carbon dioxide production produced by the highest country ",
          "The country that produced the highest amount of carbon dioxide starting from 1950",
          "The total amount of carbon dioxide production from 1750 to 2021",
          "The range of the amount of carbon dioxide production from 1750 to 2021.")

value <- c(the_average_co2,
           the_average_co2_in2021,
           highest_co2_production_by_country,
           highest_co2_country,
           total_co2_production,
           the_range_co2
           )

data <- data.frame(type, value, stringsAsFactors = FALSE)
server <- function(input, output) {
  output$scatter <- renderPlotly({
    title <- paste0("Climate Change Dataset: ", input$x_var, " v.s. ", input$y_var, " since 1900")
    chart <- ggplot(char_data) +
      geom_point(mapping = aes_string(x = input$x_var, y = input$y_var),
                 size = input$size,
                 color = input$color) +
      labs(x = input$x_var, y = input$y_var, title = title)
    chart
    intergraph <-  ggplotly(chart)
    intergraph
  })
  output$table <- renderTable({
    data})
}
