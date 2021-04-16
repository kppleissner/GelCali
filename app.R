###########################################################
# Calibration of 2-D electrophoresis gels
# Linear regression of pI-values
# Logarithmic regression of Mr-values
#
# Author: Dr. Klaus-Peter Pleissner
# Date: 13.04.2021
#
# This is a Shiny application.
#
###########################################################

library(shiny)
library(ggplot2)
library(plotly)
library(rmarkdown)
library(knitr)
library(pander)
library(readxl)

# Define UI for application that draws a histogram
ui <- fluidPage(
    
    # Application title
    h1(id="big-heading", "Calibration of 2-dimensional electrophoresis gels"),
    tags$style(HTML("#big-heading{color: blue;}")),
    h3(id="small-heading", "Linear regression of pI-axis and logarithmic regression of Mr-axis"),
    tags$style(HTML("#small-heading{color: black;}")),
    
    
    
    sidebarLayout(
        sidebarPanel(
            
            HTML('<p>For the calibration of pI | Mr-axis you can use manually given pI | Mr-values and distances (see below) or data stored in an external EXCEL-file. 
            A template of such EXCEL-file (.xlsx)  can be found on  <a href="https://github.com/kppleissner/GelCali" target="_blank"> GitHub</a>.'), 
            br(),
            br(),
            tags$b("Please enter distance and pI-values for selected spots:"),
            br(),
            textInput("x", "distance [mm or pixels]", value = "15, 30, 77, 118, 150,200", placeholder = "Enter values separated by a comma with decimals as points, e.g. 4.2, 4.4, 5, 5.03, etc."),
            textInput("y", "pI-value", value = "3.5, 4.2, 5.6, 7.3, 8.3,11", placeholder = "Enter values separated by a comma with decimals as points, e.g. 4.2, 4.4, 5, 5.03, etc."),
            hr(),
            tags$b("Please enter distance and Mr-values for selected spots:"),
            textInput("xx", "distance [mm or pixels]", value = "2, 167, 95, 65, 55, 250", placeholder = "Enter values separated by a comma with decimals as points, e.g. 4.2, 4.4, 5, 5.03, etc."),
            textInput("yy", "Mr-value", value = "80000, 15200, 25000, 33000, 36000, 14200", placeholder = "Enter values separated by a comma with decimals as points, e.g. 4.2, 4.4, 5, 5.03, etc."),
            hr(),
            checkboxInput("check", "Check if you want to use  EXCEL file for calibration", FALSE),
            fileInput('uploadfile',"Upload Calibration data as EXCEL-file: .xlsx",  accept = c(".xlsx")), 
            tags$b("Plot:"),
            checkboxInput("se", "Add confidence interval around the regression line", TRUE),
            
            HTML('<p>The calibration of 2-dimensional electrophoresis gels was developed by <a href="https://kppleissner.github.io"> Dr. Klaus-Peter Pleissner</a>
          and is based on code template of <a href="https://www.antoinesoetewey.com/">Antoine Soetewey</a>.
          The original code is available  on <a href="https://github.com/AntoineSoetewey/statistics-202" target="_blank">GitHub</a>.')
            
            
        ),
        
        mainPanel(
            # tags$b("Your data for pI-values:"),
            tags$b("Your data used for calibration of pI-axis:"),
            br(),
            DT::dataTableOutput("tbl"),
            uiOutput("data"),
            hr(style = "border-top: 2px solid #0000FF;"),
            br(),
            tags$b("Formula for linear regression of pI-values:"),
            uiOutput("formula"),
            br(),
            tags$b("Computed parameters:"),
            br(),
            br(),
            #tags$b("Computed parameters:"),
            uiOutput("results"),
            br(),
            tags$b("Linear regression plot for pI-values"),
            plotlyOutput("plot"),
            
            hr(style = "border-top: 2px solid #0000FF;"),
            
            ## logarithmic regression of  Mr-values
            
            tags$b("Your data used for calibration of Mr-axis:"),
            DT::dataTableOutput("tbllog"),
            uiOutput("datalog"),
            hr(style = "border-top: 2px solid #0000FF;"),
            tags$b("Formula for logarithmic regression of Mr-values:"),
            br(),
            uiOutput("loga_formula"),
            br(),
            tags$b("Computed parameters:"),
            #verbatimTextOutput("summary"),
            br(),
            br(),
            uiOutput("loga_result"),
            br(),
            tags$b("Logarithmic regression plot for Mr-values"),
            plotlyOutput("loga_plot"),
            br(),
            br()
            
        )
    )
)




server <- function(input, output) {
    
    data_excel <- reactive({
        if(input$check){
            req(input$uploadfile)
            inFile <- input$uploadfile
            return (read_xlsx(inFile$datapath, sheet =  1))
        }
        
    })
    
    
    extract <- function(text) {
        text <- gsub(" ", "", text)
        split <- strsplit(text, ",", fixed = FALSE)[[1]]
        as.numeric(split)
    }
    
    # Data output for pI-values
    output$tbl <- DT::renderDataTable({
        
        if(input$check){
            #if( !exists("data_excel()") ){
            y <-  data_excel()$pI
            x <-  data_excel()$distance_pI
        }else{
            y <- extract(input$y)
            x <- extract(input$x)
        }
        
        
        df <-  data.frame(distance = x , pI_value = y)
        DT::datatable(data.frame(distance = x , pI_value = y),
                      extensions = "Buttons",
                      options = list(
                          lengthChange = FALSE,
                          dom = "Blfrtip",
                          buttons = c("copy", "csv", "excel", "pdf", "print")
                      )
        )
    })
    
    
    
    
    # Data output for Mr-values
    output$tbllog <- DT::renderDataTable({
        
        if(input$check){
            y <-  data_excel()$Mr
            x <-  data_excel()$distance_Mr
        }else{
            y <- extract(input$yy)
            x <- extract(input$xx)
        }
        
        DT::datatable(data.frame(distance = x , Mr_value = y),
                      extensions = "Buttons",
                      options = list(
                          lengthChange = FALSE,
                          dom = "Blfrtip",
                          buttons = c("copy", "csv", "excel", "pdf", "print")
                      )
        )
    })
    
    
    output$data <- renderUI({
        if(input$check){
            y <-  data_excel()$pI
            x <-  data_excel()$distance_pI
        }else{
            y <- extract(input$y)
            x <- extract(input$x)
        }
        if (anyNA(x) | length(x) < 2 | anyNA(y) | length(y) < 2  ) {
            "Invalid input (must not be zero!) or not enough observations !"
        } else if (length(x) != length(y)) {
            "Number of observations must be equal for x and y"
        } else {
            withMathJax(
                # paste0("\\(\\bar{x} =\\) ", round(mean(x), 3)),
                #  br(),
                # paste0("\\(\\bar{y} =\\) ", round(mean(y), 3)),
                # br(),
                #paste0("Number of spots used for linear regression = ", length(x))
                
            )
        }
    })
    
    
    output$datalog <- renderUI({
        if(input$check){
            y <-  data_excel()$Mr
            x <-  data_excel()$distance_Mr
        }else{
            y <- extract(input$yy)
            x <- extract(input$xx)
        }
        
        if (anyNA(x) | length(x) < 2 | anyNA(y) | length(y) < 2  | x[1] == 0 | y[1] == 0) {
            "Invalid input for logarithmic expression. Not enough observations. Zero is not allowed !"
        } else if (length(x) != length(y)) {
            "Number of observations must be equal for x and y"
        } else {
            withMathJax(
                # paste0("\\(\\bar{x} =\\) ", round(mean(x), 3)),
                #  br(),
                # paste0("\\(\\bar{y} =\\) ", round(mean(y), 3)),
                # br(),
                #paste0("Number of spots used for logarithmic regression = ", length(x))
                
            )
        }
    })
    # Output of formula of linear regression  
    output$formula <- renderUI({
        if( input$check){
            y <-  data_excel()$pI
            x <-  data_excel()$distance_pI
        }else{
            y <- extract(input$y)
            x <- extract(input$x)
        }
        
        fit <- lm(y ~ x)
        withMathJax(
            br(),
            paste0("\\(  y = \\beta_0 + \\beta_1 * x = \\) ", round(fit$coef[[1]], 3), " + ", round(fit$coef[[2]], 3), "  \\(  *  x \\)")
        )
    })
    
    
    
    #output of parameters of linear regression
    output$results <- renderUI({
        if( input$check){
            y <-  data_excel()$pI
            x <-  data_excel()$distance_pI
        }else{
            y <- extract(input$y)
            x <- extract(input$x)
        }
        fit <- lm(y ~ x)
        
        withMathJax(
            paste0("\\( R^2 = \\) ", round(summary(fit)$adj.r.squared, 3),";"),
            paste0( "\\(\\beta_0 = \\) ", round(fit$coef[[1]], 3),";"),
            paste0(  " \\(\\beta_1 = \\) ", round(fit$coef[[2]], 3),";"),
            paste0(  " P-value ", "\\( = \\) ", signif(summary(fit)$coef[2, 4], 3))
            
        )
    })
    
    # plot of linear regression
    
    output$plot <- renderPlotly({
        if( input$check){
            y <-  data_excel()$pI
            x <-  data_excel()$distance_pI
        }else{
            y <- extract(input$y)
            x <- extract(input$x)
        }
        
        fit <- lm(y ~ x)
        dat <- data.frame(x, y)
        p <- ggplot(dat, aes(x = x, y = y)) +
            geom_point() +
            stat_smooth(method = "lm", se = input$se,fullrange = T) +
            ylab("pI") +
            xlab("distance [mm or pixels]") +
            theme_minimal()
        
        p <- p + expand_limits(x = c(0, 250), y = c(0, 13))
        ggplotly(p)
    })
    
    
    
    ### logarithmic regression of Mr-values #################
    ## output of formula with coefficients
    
    output$loga_formula <- renderUI({
        if(input$check){
            y <-  data_excel()$Mr
            x <-  data_excel()$distance_Mr
        }else{
            y <- extract(input$yy)
            x <- extract(input$xx)
        }
        dat <- data.frame(x, y)
        
        if (is.null(x) | is.null(y)) {
            "x or y-values are NULL"
        }
        
        # log. fitting
        
        fit <- lm(y ~ log(x))
        
        withMathJax(
            br(),
            paste0("\\(  y = \\beta_0 + \\beta_1 * log(x) = \\) ", round(fit$coef[[1]], 3), " + ", round(fit$coef[[2]], 3), "  * log(x) ")
        )
    })
    
    
    # output of separate coefficients
    
    output$loga_result <- renderUI({
        if(input$check){
            y <-  data_excel()$Mr
            x <-  data_excel()$distance_Mr
        }else{
            y <- extract(input$yy)
            x <- extract(input$xx)
        }
        dat <- data.frame(x, y)
        fit <- lm(y ~ log(x))
        
        withMathJax(
            paste0("Adjusted \\( R^2 = \\) ", round(summary(fit)$adj.r.squared, 3),";"),
            paste0( "\\(\\beta_0 = \\) ", round(fit$coef[[1]], 1),";"),
            paste0(  " \\(\\beta_1 = \\) ", round(fit$coef[[2]], 1),";"),
            paste0(  "      P-value ", "\\( = \\) ", signif(summary(fit)$coef[2, 4], 3))
            
        )
    })
    
    
    
    ## plot of logarithmic regression 2.degree
    
    output$loga_plot <- renderPlotly({
        if(input$check){
            y <-  data_excel()$Mr
            x <-  data_excel()$distance_Mr
        }else{
            y <- extract(input$yy)
            x <- extract(input$xx)
        }
        dat <- data.frame(x, y)
        
        p <- ggplot(dat, aes(x, y) ) +
            geom_point() +
            stat_smooth(method = lm, formula = y ~ log(x), se = input$se,fullrange = T) +
            ylab("Mr [kDa]") +
            xlab ("distance [mm or pixels]") +
            theme_minimal()
        
        p <- p + expand_limits(x = c(0, 300), y = c(0, 100000))
        
        ggplotly(p)
    })
    
    
    
} #end server

# Run the application
shinyApp(ui = ui, server = server)
