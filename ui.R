library(shiny); library(ggplot2); library(datasets)
library(shinythemes)

# Define UI for miles per gallon application
shinyUI(fluidPage(theme = shinytheme("spacelab"),
    
    # Application title
    h3("Power/Sample size calculator"),
    
    sidebarLayout(
        sidebarPanel(

            #only show if calc = Sample size
            conditionalPanel(
                condition = "input.calc == 'sample'",
                h4("Sample size calculator"),
                sliderInput("ssPower", "Desired power", value = 0.80, 
                            min = 0.01, max = 1.0, step = 0.01),
                numericInput("ssM1", "Group 1 mean:", 50),
                numericInput("ssM2", "Group 2 mean:", 53),
                numericInput("ssSigma", "Standard Deviation", 4),
                br(),
                sliderInput("ssAlpha", "Alpha", min = 0.01, max = 0.1, 
                            value = 0.05, step = 0.01)
            ),
            
            conditionalPanel(
                condition = "input.calc == 'power'",
                h4("Power calculator"),
                sliderInput("pN", "Sample size:", min = 1, max = 200, value = 15),
                numericInput("pM1", "Mean 1 (pop. or group 1):", 50, step = 0.25),
                numericInput("pM2", "Mean 2 (sample or group 2):", 53, step = 0.25),
                numericInput("pSigma", "Standard Deviation", 4.5, step = 0.1),
                br(),
                sliderInput("pAlpha", "Alpha", min = 0.01, max = 0.1, value = 0.05, 
                    step = 0.01)
            ),
            
            br(),
            radioButtons("sample", "Samples", list("one.sample", "two.sample")),
            radioButtons("side", "Sides", list("one.sided", "two.sided")),
            p(strong("Created by:"), 
              tags$a("Joseph V. Casillas", href="http://www.jvcasillas.com"),
              br(), strong("Source code:"), 
              tags$a("Github", href="https://github.com/jvcasill/shiny_power/"))
        ),

    mainPanel(
        tabsetPanel(type = "pills", position = "left", id = "calc",
            tabPanel("Power", value = "power",
                fluidRow(
                    column(4,
                        h4("Summary"),
                        br(),br(),
                        tableOutput("powerValues")
                    ),
                    column(8,
                        h4("Relationship between power, beta, and alpha", align = 'center'),
                        plotOutput("densityPlots")
                    )
                )
            ),
            tabPanel("Sample size", value = "sample",
                h4("Summary"),
                br(),br(),
                tableOutput("sampleValues"))
            )
        )
    )
))