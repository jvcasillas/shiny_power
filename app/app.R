library("shiny") 
library("ggplot2") 
library("datasets")
library("shinythemes")

myplot <- function(sigma, m1, m2, n, alpha) {
  g <- ggplot(data.frame(mu = c(m1-sigma*2, m2+sigma*2)), aes(x = mu))
  
  g <- g + stat_function(
    fun = dnorm, 
    geom = "line", 
    args = list(mean = m1, sd = sigma/sqrt(n)), 
    linewidth = .8, 
    col = rgb(0, 0, 204, 102, maxColorValue = 255)
  )
  
  g <- g + stat_function(
    fun = dnorm, 
    geom = "line", 
    args = list(mean = m2, sd = sigma/sqrt(n)), 
    linewidth = .8, 
    col = rgb(150, 0, 204, 102, maxColorValue = 255)
  )
  
  xitc <- m1 + qnorm(1 - alpha) * sigma/sqrt(n)
  
  g <- g + geom_vline(xintercept = xitc, linewidth = .8) +
  labs(title = NULL, x = NULL, y = NULL) + 
  theme_bw(base_family = "Palatino", base_size = 16)
  
  g
}

# Define UI for miles per gallon application
ui <- fluidPage(theme = shinytheme("spacelab"),
    
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
        tabsetPanel(type = "pills", id = "calc",
            tabPanel(title = "Power", value = "power",
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
            tabPanel(title = "Sample size", value = "sample",
                h4("Summary"),
                br(),br(),
                tableOutput(outputId = "sampleValues"))
            )
        )
    )
)

server <- function(input, output) {
  
  output$densityPlots <- renderPlot({
    myplot(input$pSigma, input$pM1, input$pM2, input$pN, input$pAlpha)
  })
  
  powerdf <- reactive({
    powerSum <- power.t.test(
      n = input$pN, 
      delta = (input$pM2 - input$pM1), 
      sd = input$pSigma, 
      type = input$sample, 
      alternative = input$side, 
      sig.level = input$pAlpha
    )
    
    data.frame(
      Statistic = c(
        "Sample size",
        "Standard Deviation",
        "Delta",
        "Alpha",
        "Power"),
      Value = c(
        input$pN,
        input$pSigma,
        (input$pM2-input$pM1),
        input$pAlpha,
        powerSum$power),
      stringsAsFactors = FALSE
    )
  })
  
  output$powerValues <- renderTable({
    powerdf()
  })
  
  sampledf <- reactive({
    sampleSum <- power.t.test(
      n = NULL,
      power = input$ssPower,
      delta = (input$ssM2 - input$ssM1), 
      sd = input$ssSigma, 
      type = input$sample, 
      alternative = input$side, 
      sig.level = input$ssAlpha
    )
    
    data.frame(
      Statistic = c(
        "Standard Deviation",
        "Delta",
        "Alpha",
        "Desired power",
        "Required sample size"),
      Value = c(
        input$ssSigma,
        (input$ssM2-input$ssM1),
        input$ssAlpha,
        input$ssPower,
        sampleSum$n),
      stringsAsFactors=FALSE)
  })
  
  output$sampleValues <- renderTable({
    sampledf()
  })
}

shinyApp(ui = ui, server = server)
