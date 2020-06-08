library(shiny)
library(shinydashboard)
library(shinyWidgets)

groups <- c(
  "Group 1" = "group1",
  "Group 2" = "group2",
  "Group 3" = "group3",
  "Group 4" = "group4",
  "Group 5" = "group5"
)

agegroups <- c(
  "< 1 year old" = "g1",
  "1 - 5 years old" = "g2",
  "5 - 10 years old" = "g3",
  "10 - 15 years old" = "g4",
  "15 - 20 years old" = "g5",
  "> 20 years old" = "g6"
)

# Define UI for application that draws a histogram
header <- dashboardHeader(
  title = "Bayesian Stock Management", titleWidth = 350, dropdownMenuOutput("notificationsMenu")
)

sidebar <- dashboardSidebar(
  sidebarMenuOutput("sidebarmenu"),
  sliderInput("conf", tags$p("Confidence level", style = "font-size: 24; font-family: Times New Roman"), value = 80, min = 0, max = 100)
)

body <- dashboardBody(
  setBackgroundColor(
    color = "LightSteelBlue",
    gradient = c("linear", "radial"),
    direction = c("bottom", "top", "right", "left"),
    shinydashboard = TRUE
  ),
  tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "fonts.css"),
    tags$style("#required_componenets{font-size: 42px; font-family: Times New Roman;} #text1{font-size: 24px; font-family: Times New Roman;}")
  ),
  tabItems(
    tabItem(
      tabName = "landing",
      fluidRow(
        box(
          title = tags$p("Group of components", style = "font-size: 42px; font-family: Times New Roman"), solidHeader = TRUE, collapsible = TRUE,
          width = 8, collapsed = FALSE, background = "light-blue",
          selectInput("group", label = tags$p("Select a group of componenets", style = "font-size: 32; font-family: Times New Roman"), choices = groups),
          # actionButton(
          #   inputId = "confirm",
          #   label = "Next",
          #   icon = icon("ok-circle")
          # )
        ),
        box(
          title = tags$p("Age category", style = "font-size: 42px; font-family: Times New Roman"), solidHeader = TRUE, collapsible = TRUE,
          width = 8, collapsed = FALSE, background = "light-blue",
          selectInput("agegroup", label = tags$p("Select an age category", style = "font-size: 32; font-family: Times New Roman"), choices = agegroups),
          actionButton(
            inputId = "confirm",
            label = "Next",
            icon = icon("ok-circle")
          )
        )
      )
    ),
    tabItem(
      tabName = "dashboard",
      fluidRow(
        column(
          width = 4,
          box(
            title = tags$p("Current values", style = "font-size: 42px; font-family: Times New Roman"), solidHeader = TRUE, collapsible = TRUE,
            width = NULL, collapsed = FALSE, background = "light-blue",
            numericInput("n_tilde", tags$p("Number of components", style = "font-size: 32; font-family: Times New Roman"), 10, min = 1, max = 100),
            numericInput("n_stock", tags$p("Number of components in stock", style = "font-size: 32; font-family: Times New Roman"), 0, min = 1, max = 100)
          )
        ),
        column(
          width = 8,
          box(
            title = tags$p("Requirements:", style = "font-size: 42px; font-family: Times New Roman; font-style: italic;"), solidHeader = TRUE, collapsible = FALSE,
            width = NULL, collapsed = FALSE, background = "maroon",
            textOutput("required_componenets")
          ),
          box(
            title = tags$p("Summary:", style = "font-size: 42px; font-family: Times New Roman;"), solidHeader = TRUE, collapsible = TRUE,
            width = NULL, collapsed = TRUE, background = "light-blue",
            plotOutput("PosteriorPred"),
            textOutput("text1")
          )
        )
      )
    ),
    tabItem(
      tabName = "data",
      fluidRow(
        column(
          width = 7,
          box(
            title = tags$p("Prior information", style = "font-size: 42px; font-family: Times New Roman"), solidHeader = TRUE, collapsible = TRUE,
            width = NULL, collapsed = FALSE, background = "light-blue",
            numericInput("mean", tags$p("Out of 20 components how many of them will be broken?", style = "font-size: 32; font-family: Times New Roman"), 3, min = 0, max = 20),
            numericInput("var", tags$p("The above number can vary by +/- :", style = "font-size: 32; font-family: Times New Roman"), 1, min = 0, max = 20)
          )
        ),
        column(
          width = 5,
          box(
            title = tags$p("Prior information from survey", style = "font-size: 42px; font-family: Times New Roman"), solidHeader = TRUE, collapsible = TRUE,
            width = NULL, collapsed = FALSE, background = "light-blue",
            fileInput("file1", tags$p("Choose CSV file", style = "font-size: 32; font-family: Times New Roman"), accept = c("text/csv", "text/comma-separated-values,text/plain", ".csv"))
          )
        )
      )
    ),
    tabItem(
      tabName = "historical",
      fluidRow(column(
        width = 8,
        box(
          title = tags$p("Historical values", style = "font-size: 42px; font-family: Times New Roman"), solidHeader = TRUE, collapsible = TRUE,
          width = NULL, collapsed = FALSE, background = "light-blue",
          numericInput("n", tags$p("Total number of components", style = "font-size: 32; font-family: Times New Roman"), 30, min = 1, max = 100),
          numericInput("y", tags$p("Number of broken components", style = "font-size: 32; font-family: Times New Roman"), 3, min = 1, max = 100)
        )
      ))
    )
  )
)

ui <- dashboardPage(header = header, sidebar = sidebar, body = body)
