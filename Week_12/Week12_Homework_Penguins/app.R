library(shiny)
library(tidyverse)
library(palmerpenguins)

# Load in the data
Penguins <- penguins |> 
  drop_na()

# I want to make a interactive scatterplot that allows
# the user to select the x and y variables, the species 
# displayed, and then have a summary table.

# Choices for the x and y dropdown menus.
variable_options <- c("Bill length (mm)" = "bill_length_mm",
                      "Bill depth (mm)" = "bill_depth_mm",
                      "Flipper length (mm)" = "flipper_length_mm",
                      "Body mass (g)" = "body_mass_g")


# Checkbox choices for species
species_choices <- c("Adelie" = "Adelie",
                     "Gentoo" = "Gentoo",
                     "Chinstrap" = "Chinstrap")

# UI

ui <- fluidPage(
  titlePanel("Penguin morphology explorer"), # Add title
  
  # Inputs
  # X axis variable selection
  selectInput(inputId = "xvar",  # we can read this later as input$xvar
              label = "X-axis variable",
              choices = variable_options,
              selected = "body_mass_g"), # starting x value
  
  # Y axis variable selection
  selectInput(inputId = "yvar",  # we can read this later as input$xvar
              label = "Y-axis variable",
              choices = variable_options,
              selected = "flipper_length_mm"), # starting y value
  
  # Checkbox for species
  checkboxGroupInput(inputId = "species",  # we can read this later as input$species
                     label = "Species to display",
                     choices = species_choices,
                     selected = c("Adelie", "Chinstrap", "Gentoo")), # starting with all shown
  
  
  
  # Outputs (we will build these in the server)
  plotOutput("scatter"),
  tableOutput("summary_table")
  
)
  


# Server

server <- function(input, output) {
  
  # Shared reactive. Filtered once and used by both outputs.
  filtered <- reactive({
    req(input$species)                 # stop if no boxes are checked
    Penguins |>
      filter(species %in% input$species)   # keep only the checked species
  })
  
  # Scatterplot
  output$scatter <- renderPlot({
    ggplot(filtered(),                       # added the () for reactive
           aes(x = .data[[input$xvar]],      # turn text string into a data column
               y = .data[[input$yvar]],
               color = species)) +
      geom_point(size = 3, alpha = 0.7) +
      labs(x = names(variable_options)[variable_options == input$xvar], # correctly format labels
           y = names(variable_options)[variable_options == input$yvar], # correctly format labels
           color = "Species") +
      theme_bw()
  })
  
  # Summary table (Could not seem to get label formatting to look proper here, may come back to try again)
  output$summary_table <- renderTable({
    filtered() |>                                       # same data as the plot
      select(species, all_of(c(input$xvar, input$yvar))) |> # keep species and chosen variables
      pivot_longer(cols      = -species,                    # stack x and y into one column.
                   names_to  = "variable",
                   values_to = "value") |>
      group_by(species, variable) |>
      summarize(n    = n(),
                mean = mean(value),
                sd   = sd(value),
                min  = min(value),
                max  = max(value),
                .groups = "drop")
  })
}


shinyApp(ui = ui, server = server)                    