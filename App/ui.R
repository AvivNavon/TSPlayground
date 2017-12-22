library(ggplot2)
library(markdown)
library(shinythemes)

# Define UI for application
ui <- fluidPage(
  titlePanel("Time Series Playground"),
  fluidRow(
    column(3,
           wellPanel(
             theme = shinytheme("flatly"),
             h4("Parameters"),
             sliderInput("p", 
                         label = "Choose p",
                         min = 0, 
                         max = 10,
                         value = 1,
                         step = 1,
                         animate = TRUE),
             sliderInput("d", 
                         label = "Choose d",
                         min = 0, 
                         max = 10,
                         value = 1,
                         step = 1,
                         animate = TRUE),
             sliderInput("q", 
                         label = "Choose q",
                         min = 0, 
                         max = 10,
                         value = 1,
                         step = 1,
                         animate = TRUE),
           selectInput("H", 
                       label = "Horizon (Months)",
                       choices = list(12,
                                      24,
                                      36),
                       selected = "12"),
           sliderInput("min_learn", 
                       label = "Min. Learning (Years)",
                       min = 10, 
                       max = 30,
                       value = 20,
                       step = 1,
                       animate = FALSE))),
    fluidRow(column(8, plotOutput("plot"))),
    column(11
          ,headerPanel(HTML(markdownToHTML(fragment.only = TRUE, 
                                          text = "## Metrics Table")))
          ,dataTableOutput('table'))
    ,fluidRow(column(11,
             wellPanel(
             helpText(HTML(markdownToHTML(fragment.only = TRUE, 
                                          text = c(
"**Variables Description:**

  - `q` - The order of the moving-average model.
  - `p` - The order (number of time lags) of the autoregressive model.
  - `d` - The degree of differencing (the number of times the data have had past values subtracted).
  - `Horizon` - Forecast horizon, i.e number of month to forecast.
  - `Min. Learning` - First cutoff point.

**Note:** Then the seasonal model set fixed with an autoregressive term of first lag (D) at model period 12 units, in this case months.")))))
      )
    )
  )
)