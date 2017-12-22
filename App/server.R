library(shiny)
library(dplyr)
library(ggplot2)
library(reshape2)
# library(DT)

########
# data #
########
path <- "candy_production.csv"
data <- read.csv(path,
                 col.names = c("period", "prod"))

# modify to a ts object
data.ts <- ts(data$prod, 
              c(1972, 1), 
              c(2017, 8), 
              12)

# Define server logic required to draw the plot
server <- function(input, output, session) {
  # We will add rows to a matrics DF for each (p, d, q) combination
  values <- reactiveValues()
  values$metrics.df <- data.frame()
    
  get.data <- reactive({
    # Parameters
    H <- input$H %>% as.integer()
    start <- input$min_learn
    end <- length(data.ts) - 2 * H
    p <- input$p
    q <- input$q
    d <- input$d
    # Cutoffs
    cutoffs <- seq(start, end, H)
    values$cutoffs <- cutoffs
    # Generate predictions at each cutoff
    pred.df <- data.frame()
    
    for (cutoff in cutoffs) {
        train <- data.ts[1:cutoff]
        test <- data.ts[(cutoff + 1):(cutoff + H)]
        mod <- arima(train, 
                     order = c(p, q, d),
                     seasonal = list(order = c(1, 0, 0), period = 12))   # ARIMA(p, d, q)
        y.hat <- predict(mod, n.ahead = H)                               # forecast H months into the "future"
        # current DF
        curr <- data.frame(H = H,
                           first_cutoff = input$min_learn,
                           p = rep(p, H),
                           q = rep(q, H),
                           d = rep(d, H),
                           y.hat = y.hat$pred %>% as.vector(),
                           y = test, 
                           t = (cutoff + 1):(cutoff + H),
                           cut = cutoff, 
                           bic = BIC(mod), 
                           aic = AIC(mod))
        # update DF
        pred.df <- rbind(pred.df, curr)
    }
    # update matrics DF
    isolate(values$metrics.df <- rbind(values$metrics.df,
                                       pred.df %>%
                                         group_by(H, first_cutoff, p, d, q) %>%
                                         summarise(avg_AIC = aic %>% mean %>% round(2),
                                                   avg_BIC = bic %>% mean %>% round(2)
                                         ) %>%
                                         ungroup() %>% 
                                         arrange(avg_AIC)))
    # remove duplicate rows
    isolate(values$metrics.df <- values$metrics.df[!duplicated(values$metrics.df), ]) 
    pred.df
    })
  
  # Generate plot
  output$plot <- renderPlot({ 
    get.data() %>% 
    melt(id.vars = c("t", "p", "q", "d", 
                     "first_cutoff", "H",
                     "cut", "aic", "bic")) %>% 
    ggplot(aes(t, 
               value, 
               color = variable)) +
      geom_line() +
      geom_vline(xintercept = values$cutoffs,
                 size = .1) +
      theme_dark() +
      theme(panel.grid.major = element_blank(), 
            panel.grid.minor = element_blank(),
            axis.text.y = element_blank(),
            axis.ticks = element_blank()) +
      scale_color_manual(values = c("#93c9f9", "#f96868")) +
      labs(x = "Time", 
           y = "", 
           title = paste0("ARIMA(", input$p, ", ", 
                          input$q, ", ", input$d, ")"), 
           subtitle = "Prediction at each cutoff") +
      scale_fill_manual(name = "Variable")
  })
  
  # Generate table
  # output$table <- DT::renderDataTable(DT::datatable(values$metrics.df,
  #                                                   options = list(lengthMenu = c(5, 10, 20),
  #                                                                  pageLength = 5),
  #                                                   colnames = c("p", "d", "q", "AIC", "BIC"),
  #                                                   class = 'cell-border stripe',
  #                                                   filter = 'top'))
  output$table <- renderDataTable(values$metrics.df)
}

