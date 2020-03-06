## app.R ##
library(shiny)
library(shinyjs)
library(shinydashboard)
library(dplyr)
library(ggplot2)

## Functions ----

source("R/Bf.R")
source("R/bfrr.R")
source("R/plot.bfrr.R")
source("R/summary.bfrr.R")
source("R/default.R")
source("R/likelihood.R")
source("R/utils-pipe.R")

ggplot2::theme_set(theme_bw(base_size = 20))

## UI ----
ui <- dashboardPage(
    dashboardHeader(title = "bfrr"),
    dashboardSidebar(
        sidebarMenu(
            #actionButton("reset", "Reset Parameters"),
            # input ----
            numericInput("sample_mean", "sample mean", value = 0, step = 0.05),
            numericInput("sample_se", "sample standard error", value = 0.1, min = 0.001, step = 0.01),
            numericInput("sample_df", "sample df", value = 99, min = 1, step = 1),
            selectInput("model", "model", choices = c("normal", "uniform"), selected = "normal"),
            numericInput("theory_mean", "H1 mean", value = 0, step = 0.05),
            numericInput("theory_sd", "H1 SD", value = 1, min = 0.01, step = 0.01),
            selectInput("tail", "tails", choices = c(1, 2), selected = 2),
            numericInput("criterion", "criterion", value = 3, min = 1.01, step = 1),
            selectInput("precision", "precision", choices = c(0.01, .025, .05, .1, .25, .5), selected = .05)
        )
    ),
    dashboardBody(
        useShinyjs(),
        tags$head(
            tags$link(rel = "stylesheet", type = "text/css", href = "custom.css")
        ),
        h3("Robustness Regions for Bayes Factors"),
        textOutput("summary"),
        plotOutput("plot"),
        p("Try setting the sample mean to 0.25, the H1 mean to 0.5, and tails to 1, then see what happens when you increase the criterion."),
        p("This app is under development. Don't trust anything yet!")
    )
)

## server ----
server <- function(input, output, session) {
    output$summary <- renderText({
        rr <- bfrr(sample_mean = input$sample_mean,
                   sample_se = input$sample_se,
                   sample_df = input$sample_df,
                   model = input$model,
                   mean = input$theory_mean,
                   sd = input$theory_sd,
                   tail = as.numeric(input$tail),
                   criterion = input$criterion,
                   rr_interval = NA,
                   precision = as.numeric(input$precision))

        output$plot <- renderPlot(plot(rr))

        capture.output(summary(rr))
    })
}

shinyApp(ui, server)
