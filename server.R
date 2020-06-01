#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(ggplot2)
# Define server logic required to draw a histogram
shinyServer(function(input, output) {
  output$PosteriorPred <- renderPlot({
    alpha <- ((1 - input$mean_prior) * input$mean_prior^2 - input$mean_prior * input$var_prior) / input$var_prior
    beta <- (1 - input$mean_prior) * ((1 - input$mean_prior) * input$mean_prior - input$var_prior) / input$var_prior
    alpha.post <- input$y + alpha
    beta.post <- input$n - input$y + beta
    pred <- rbinom(n = 10000, size = input$n_tilde, prob = rbeta(n = 10000, alpha.post, beta.post))
    tN <- table(pred) / 10000
    r <- data.frame(tN)
    p <- ggplot(data = r, aes(x = pred, y = Freq)) +
      geom_bar(stat = "identity") +
      theme_classic()
    p
  })

  output$text1 <- renderText({
    alpha <- ((1 - input$mean_prior) * input$mean_prior^2 - input$mean_prior * input$var_prior) / input$var_prior
    beta <- (1 - input$mean_prior) * ((1 - input$mean_prior) * input$mean_prior - input$var_prior) / input$var_prior
    alpha.post <- input$y + alpha
    beta.post <- input$n - input$y + beta
    pred <- rbinom(n = 10000, size = input$n_tilde, prob = rbeta(n = 10000, alpha.post, beta.post))
    paste("probability of needing more than ", input$n_stock, "is:", sum(pred > input$n_stock) / 10000)
  })
})
