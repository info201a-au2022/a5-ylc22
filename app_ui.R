#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(bslib)
library(lintr)
library("tidyverse", warn.conflicts = FALSE)
library("plotly", warn.conflicts = FALSE)
library("leaflet", warn.conflicts = FALSE)
library("ggplot2", warn.conflicts = FALSE)

dataset <- read_csv("https://raw.githubusercontent.com/owid/co2-data/master/owid-co2-data.csv"
                    , show_col_types = FALSE)
lint("app_ui.R")
intro_panel <- tabPanel(
  "Introduction",
  titlePanel("Climate Change due to CO2"),
  p("Author: Luis Chan (ylc22@uw.edu)"),
  p("Climate change has continued to be an unprecented ", strong("catastrophic phenomenon"), "around the world.
    While the emission of CO2 has been one of the culprits of the issue, my project aims to raise awareness of the seriousness of climate change by showing carbon dioxide production.
    I have selected 6 variables to demonstrate the situation, which are
    the average amount of carbon dioxide production from 1750 to 2021,
    the average amount of carbon dioxide produced in 2021,
    the highest amount of carbon dioxide production from a country,
    the country that produced the highest amount of carbon dioxide starting from 1950,
    the total amount of carbon dioxide production from 1750 to 2021,
    and last but not least, the range of the amount of carbon dioxide production from 1750 to 2021. "),
  tableOutput("table"),
  p(
    "The average of carbon dioxide production from 1750 to 2021 and the average in 2021 can show us the contrasts of the increasing carbon dioxide production.
    The variable about the country and the amount of the highest carbon dioxide production enables us to conduct comparisons between countries.
    The variable about the total amount of carbon dioxide production provides us with an overview of the seriousness of the situation.
    Finally, the range from 1750 to 2021 can show us the spread of the dataset."),
  p("Please visit ", strong("Scatter Plot"), "to see an interactive plot of the emission of CO2")
)
char_data <- dataset %>%
  filter(year > 1900) %>%
  filter(!is.na(iso_code)) %>%
  filter(country != "World") %>%
  select(year, co2, co2_per_capita, cumulative_co2) %>%
  group_by(year) %>%
  summarize(
    Emission = sum(co2, na.rm = TRUE),
    PerCapita = sum(co2_per_capita, na.rm = TRUE),
    Cumulative = sum(cumulative_co2, na.rm = TRUE)
  ) 


scatter_main_content <- mainPanel(
  plotlyOutput("scatter")
)

select_values <- colnames(char_data)

x_input <- selectInput(
  "x_var",
  label = "X Variable",
  choices = select_values,
  selected = "year"
)
y_input <- selectInput(
  "y_var",
  label = "Y Variable",
  choices = select_values,
  selected = "Emission"
)
color_input <- selectInput(
  "color",
  label = "Color",
  choices = list("Red" = "red", "Orange" = "orange", "Yellow" = "yellow", "Green" = "green", "Cyan" = "cyan", "Blue" = "blue", "Violet" = "purple3")
)
size_input <- sliderInput(
  "size",
  label = "Size of point", min = 1, max = 10, value = 2
)
graph_panel <- tabPanel(
  "Scatter Plot",
  titlePanel("Climate Change due to CO2 Graph"),
  sidebarLayout(
    sidebarPanel(x_input,
                 y_input,
                 color_input,
                 size_input),
    scatter_main_content
  ),
  p(
    "The  graph shows that carbon dioxide production increased exponentially throughout the decades.
    From ", strong("1945 to 1980"), " and ", strong("2000 to 2021"), ", the fact that the emission of CO2 increased heavily during the decades might be indispensably related to the burning of fossil fuels due to an increased number of motor cars, manufacturing, and globalization.
    From 1950 to 1975, the carbon dioxide emission was suprisingly ", strong("three times of"), " the emissions from 5243 to 16597 in just ", strong(" around 25 years"), " when it used to take ", strong("only 50 years"), " years before.
    Besides, the scatterplot also had a dramatic increasing rate of carbon dioxide emission since 1950,  and it did not show a signs of recession.
    Therefore, the graph predicts that the emission rate may not be slowing down in the near future as carbon dioxide, a major contributor to climate change, is still contiuing rising.
    While the graph proves the enourmous issue of CO2 which lead to irreversible impacts of climate change that we might face, we have to take actions right now, in order to save our planet and improve our quality of lives.")
  )
ui <- navbarPage(
  "Climate Change",
  intro_panel,
  graph_panel,
  theme = bs_theme(version = 4, bootswatch = "superhero"),
)

