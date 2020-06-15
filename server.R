library(shiny)
library(shinydashboard)
library(shinyFeedback)
library(dplyr)
library(purrr)
library(ggplot2)

server <- function(input, output, session) {
  output$sidebarmenu <- shinydashboard::renderMenu({
    sidebarMenu(
      id = "sidebarmenu",
      menuItem("Landing page", tabName = "landing", icon = icon("cog")),
      menuItem("Dashboard", tabName = "dashboard", icon = icon("dashboard")),
      menuItem("Historical data", tabName = "historical", icon = icon("calendar")),
      menuItem("Prior data", tabName = "data", icon = icon("table"))
    )
  })

  observeEvent(input$confirm, {
    output$sidebarmenu <- shinydashboard::renderMenu({
      sidebarMenu(
        id = "sidebarmenu",
        menuItem("Dashboard", tabName = "dashboard", icon = icon("dashboard")),
        menuItem("Historical data", tabName = "historical", icon = icon("calendar")),
        menuItem("Prior data", tabName = "data", icon = icon("table"))
      )
    })
    updateTabsetPanel(session, "sidebarmenu", "data")
    updateTabsetPanel(session, "sidebarmenu", "historical")
    updateTabsetPanel(session, "sidebarmenu", "dashboard")
  })

  observeEvent(
    input$n_tilde,
    feedbackDanger(
      "n_tilde",
      input$n_tilde %% 1 != 0 | input$n_tilde < 0 | !is.numeric(input$n_tilde),
      "Please provide a non-negative integer"
    )
  )

  observeEvent(
    input$n_stock,
    feedbackDanger(
      "n_stock",
      input$n_stock %% 1 != 0 | input$n_stock < 0 | !is.numeric(input$n_stock),
      "Please provide a non-negative integer"
    )
  )

  observeEvent(
    input$n,
    feedbackDanger(
      "n",
      input$n %% 1 != 0 | input$n < 0 | !is.numeric(input$n),
      "Please provide a non-negative integer"
    )
  )

  observeEvent(
    input$y,
    feedbackDanger(
      "y",
      input$y %% 1 != 0 | input$y < 0 | !is.numeric(input$y),
      "Please provide a non-negative integer"
    )
  )

  observeEvent(
    input$mean,
    feedbackDanger(
      "mean",
      input$mean %% 1 != 0 | input$mean < 0 | !is.numeric(input$mean),
      "Please provide a non-negative integer"
    )
  )

  observeEvent(
    input$var,
    feedbackDanger(
      "var",
      input$var %% 1 != 0 | input$var < 0 | !is.numeric(input$var),
      "Please provide a non-negative integer"
    )
  )

  output$notificationsMenu <- renderMenu({
    dropdownMenu(
      type = "notifications",
      notificationItem(
        text = "10 items delivered",
        icon("truck"),
        status = "success"
      )
    )
  })
  output$PosteriorPred <- renderPlot({
    if (is.null(filedata())) {
      df <- tibble(mean_prior = input$mean / 20, var_prior = (input$var / 20)^2, n = input$n, y = input$y)
    } else {
      df <- filedata()
    }
    mean_prior <- df %>%
      select(mean_prior) %>%
      flatten_dbl()
    var_prior <- df %>%
      select(var_prior) %>%
      flatten_dbl()
    nb <- df %>%
      select(n) %>%
      flatten_dbl()
    y <- df %>%
      select(y) %>%
      flatten_dbl()
    alpha <- ((1 - mean_prior) * mean_prior^2 - mean_prior * var_prior) / var_prior
    beta <- (1 - mean_prior) * ((1 - mean_prior) * mean_prior - var_prior) / var_prior
    alpha.post <- y + alpha
    beta.post <- nb - y + beta
    pred <- rbinom(n = 10000, size = input$n_tilde, prob = rbeta(n = 10000, alpha.post, beta.post))
    tN <- table(pred) / 10000
    r <- data.frame(tN)
    p <- ggplot(data = r, aes(x = pred, y = Freq)) +
      geom_bar(stat = "identity", fill = "darkblue") +
      labs(title = "Posterior distribution", x = "No of componenets", y = "Probability") +
      theme_linedraw()
    p
  })

  output$text1 <- renderText({
    if (is.null(filedata())) {
      df <- tibble(mean_prior = input$mean / 20, var_prior = (input$var / 20)^2, n = input$n, y = input$y)
    } else {
      df <- filedata()
    }
    mean_prior <- df %>%
      select(mean_prior) %>%
      flatten_dbl()
    var_prior <- df %>%
      select(var_prior) %>%
      flatten_dbl()
    nb <- df %>%
      select(n) %>%
      flatten_dbl()
    y <- df %>%
      select(y) %>%
      flatten_dbl()
    alpha <- ((1 - mean_prior) * mean_prior^2 - mean_prior * var_prior) / var_prior
    beta <- (1 - mean_prior) * ((1 - mean_prior) * mean_prior - var_prior) / var_prior
    alpha.post <- y + alpha
    beta.post <- nb - y + beta
    pred <- rbinom(n = 10000, size = input$n_tilde, prob = rbeta(n = 10000, alpha.post, beta.post))
    paste("Probability of needing more than ", input$n_stock, "is:", round(sum(pred > input$n_stock) / 100, 2), "%")
  })

  output$required_componenets <- renderText({
    if (is.null(filedata())) {
      df <- tibble(mean_prior = input$mean / 20, var_prior = (input$var / 20)^2, n = input$n, y = input$y)
    } else {
      df <- filedata()
    }
    mean_prior <- df %>%
      select(mean_prior) %>%
      flatten_dbl()
    var_prior <- df %>%
      select(var_prior) %>%
      flatten_dbl()
    nb <- df %>%
      select(n) %>%
      flatten_dbl()
    y <- df %>%
      select(y) %>%
      flatten_dbl()
    alpha <- ((1 - mean_prior) * mean_prior^2 - mean_prior * var_prior) / var_prior
    beta <- (1 - mean_prior) * ((1 - mean_prior) * mean_prior - var_prior) / var_prior
    alpha.post <- y + alpha
    beta.post <- nb - y + beta
    pred <- rbinom(n = 10000, size = input$n_tilde, prob = rbeta(n = 10000, alpha.post, beta.post))
    prob <- rep(0, input$n_tilde)
    for (i in 1:input$n_tilde) {
      prob[i] <- 1 - sum(pred > i) / 10000
    }
    components <- min(which(prob >= input$conf / 100))
    paste("Minimal required number of components is:", components)
  })

  filedata <- reactive({
    infile <- input$file1
    if (is.null(infile)) {
      return(NULL)
    }
    df <- read.csv(infile$datapath)
    df %>%
      filter(Group == input$group) %>%
      filter(AgeGroup == input$agegroup) %>%
      group_by(Group) %>%
      mutate(Mean = Mean * Conf, Std = Std * Conf) %>%
      summarise(mean_prior = sum(Mean) / (20 * sum(Conf)), var_prior = (sum(Std) / sum(Conf))^2, n = sum(nb_total), y = sum(nb_broken))
  })
}
