library(shiny); library(ggplot2); library(datasets)
library(xtable)

myplot <- function(sigma, m1, m2, n, alpha) {
    g = ggplot(data.frame(mu = c(m1-sigma*2, m2+sigma*2)), aes(x = mu))
    g = g + stat_function(fun = dnorm, geom = "line", 
                          args = list(mean = m1, sd = sigma/sqrt(n)), 
                          linewidth = .8, col = rgb(0, 0, 204, 102, maxColorValue = 255))
    g = g + stat_function(fun = dnorm, geom = "line", 
                          args = list(mean = m2, sd = sigma/sqrt(n)), 
                          linewidth = .8, col = rgb(150, 0, 204, 102, maxColorValue = 255))
    xitc = m1 + qnorm(1 - alpha) * sigma/sqrt(n)
    g = g + geom_vline(xintercept = xitc, linewidth = .8) +
        labs(title = "", x = "", y = "")
    g
}

shinyServer(function(input, output) {

    output$densityPlots <- renderPlot({
        myplot(input$pSigma, input$pM1, input$pM2, input$pN, input$pAlpha)
    })

    powerdf <- reactive({
        powerSum <- power.t.test(n = input$pN, 
                                 delta = (input$pM2 - input$pM1), 
                                 sd = input$pSigma, 
                                 type = input$sample, 
                                 alternative = input$side, 
                                 sig.level = input$pAlpha)

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
            stringsAsFactors=FALSE)
    })
    
    output$powerValues <- renderTable({
        powerdf()
    })
    
    sampledf <- reactive({
        sampleSum <- power.t.test(n = NULL,
                                 power = input$ssPower,
                                 delta = (input$ssM2 - input$ssM1), 
                                 sd = input$ssSigma, 
                                 type = input$sample, 
                                 alternative = input$side, 
                                 sig.level = input$ssAlpha)
        
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
})
