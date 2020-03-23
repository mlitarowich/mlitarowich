library(shiny)
library(tidyverse)
library(lubridate)
library(DT)
library(DescTools)
library(lmtest)
library(rsconnect)
library(scales)

syms <- read.csv("https://raw.githubusercontent.com/mlitarowich/mlitarowich/master/stock_symbols2020_returns.csv")  
syms$Date <- mdy(syms$Date)

syms_prices <- read.csv("https://raw.githubusercontent.com/mlitarowich/mlitarowich/master/stock_symbols2020.csv")
syms_prices$Date <- mdy(syms_prices$Date)

ui <- fluidPage(
  
  tabsetPanel(
    
    #--------------------------------------------------------------- Tab 1 - Stock Price Chart and Data Table
    
    tabPanel("Stock Price History",
             
      fluidRow(
        column(1),
        column(9, h2(p(strong("Stock Prices for the Past 5 Years")), align = "center"))
      ),
      br(),
      hr(),
      hr(),
      
      fluidRow(
        column(2, checkboxGroupInput("checkbox", label = "Choose any stocks (prices are on a log 10 scale)",
                                     choices = c("All", colnames(syms[-1])), selected = colnames(syms[2]))),
        column(8, plotOutput("pricing"))
      ),
      
      br(),
      br(),
      hr(),
      hr(),
      br(),
      br(),
      
      fluidRow(
        column(1),
        column(9, h3(p(strong("Data Table of Stock Price History")), align = "center"))
      ),
      
      fluidRow(
        column(3, dateRangeInput("daterangeprices", label = "Select your Date Range",
                                 start = "2015-01-01", end = "2020-03-20",
                                 min = "2015-01-01", max = "2020-03-20"))
      ),

      fluidRow(
        column(12, dataTableOutput("tableprices"))
      )
    ),

    #-------------------------------------------------------------- Tab 2 - Data Table
    tabPanel("Data Table of Returns",
      fluidRow(
        column(3), 
        column(9, h2(p(strong("Data Table of Daily Returns for the Past 5 Years"))))
      ),
      br(),
      br(),
      fluidRow(
        column(3),
        column(4, h4(p(strong("Entries are in percent format")), align = "center"))
      ),
      br(),
      
      fluidRow(column(3, dateRangeInput(inputId = "daterange", label = "Select your Date Range",
                                        start = "2015-01-01", end = "2020-03-20",
                                        min = "2015-01-01", max = "2020-03-20") 
                      ),
               column(3, selectInput(inputId = "symhist1", label = "Choose Symbol to see Distribution of Returns",
                                     choices = colnames(syms)[-1], selected = colnames(syms)[1])
                      ), 
               column(3, selectInput(inputId = "symhist2", label = "Choose Symbol to see Distribution of Returns",
                                     choices = colnames(syms)[-1], selected = colnames(syms)[1])
                      ),
               column(3, selectInput(inputId = "symhist3", label = "Choose Symbol to see Distribution of Returns",
                                     choices = colnames(syms)[-1], selected = colnames(syms)[1])
                      ) 
      ),
      
      fluidRow(
        column(12, dataTableOutput("table"))
      ),
      fluidRow(
        column(4, plotOutput("returnhist1")),
        column(4, plotOutput("returnhist2")),
        column(4, plotOutput("returnhist3"))
      )
      
    ),
    
    #----------------------------------------------------------------------- Tab 3 - Regression Plot
    
    tabPanel("Regression Plot and Statistics",
    
      h2(p(strong("Regression on Daily Stock Return Data")), align = "center"),
      h4(p(strong("Dataset contains daily returns over the period of 2015-01-01 to 2020-03-20")), align = "center"),
      hr(),
      hr(),
  
      sidebarLayout(
        sidebarPanel(
          
          dateRangeInput(inputId = "symdate", label = "Choose a Date Range for the Regression",
                         start = "2015-01-01", end = "2020-03-20",
                         min = "2015-01-01", max = "2020-03-20"),  
          
          selectInput(inputId = "sym1", label = "Choose your Dependent Variable", 
                      choices = colnames(syms)[-1]),
          selectInput(inputId = "sym2", label = "Choose your Independent Variable", 
                      choices = colnames(syms)[-1], selected = colnames(syms)[3]),
          selectInput(inputId = "sym3", label = "Choose 2nd Independent Variable (Optional)", 
                      choices = c("None", colnames(syms)[-1])),
          selectInput(inputId = "sym4", label = "Choose 3rd Independent Variable (Optional)", 
                      choices = c("None", colnames(syms)[-1])),
          selectInput(inputId = "sym5", label = "Choose 4th Independent Variable (Optional)", 
                      choices = c("None", colnames(syms)[-1])),
          fluidRow(
            column(12, verbatimTextOutput(outputId = 'summary'))
          )
          
        ),
    
        mainPanel(
          fluidRow(
            column(12, plotOutput("point"))
          )
        )
      )
    ), 
    
    #------------------------------------------------------------------- Tab 4 - Residual Plots
    tabPanel("Residuals Analysis",
      fluidRow(
        column(1),
        column(9, h2(p(strong("Residual Analysis for 2 variables")), align = "center"),
        )
      ),
      br(),
      hr(),
      hr(),
      
      fluidRow(
        column(2, dateRangeInput(inputId = "residdate", label = "Choose a time period for the regression",
                                 start = "2015-01-01", end = "2020-03-20",
                                 min = "2015-01-01", max = "2020-03-20"),
                  selectInput(inputId = "residsym1", label = "Choose Dependent Variable", 
                              choices = colnames(syms)[-1]),
                  selectInput(inputId = "residsym2", label = "Choose Independent Variable",
                              choices = colnames(syms)[-1], selected = colnames(syms)[3]),
               ),
        column(5, plotOutput(outputId = "resid")
               ),
        column(5, plotOutput("residhist"))
      ),
      hr(),
      hr(),
      fluidRow(
        column(width = 4, h5(p(strong("Jarque-Bera Test for Normality of Residuals")), align = "center")),
        column(width = 4, h5(p(strong("Breush-Pagan Test for Heteroskedasticity of Residual Variance")), align = "center")),
        column(width = 4, h5(p(strong("Durbin-Watson Test for Autocorrelation of Observations")), align = "center"))
      ),
      br(),
      br(),
      fluidRow(
        column(width = 4, verbatimTextOutput(outputId = "jb")),
        column(width = 4, verbatimTextOutput(outputId = "bp")),
        column(width = 4, verbatimTextOutput(outputId = "dw"))
      )
    )
  )
)

#==================================================================================================================================
#==================================================================================================================================

server <- function(input, output) {

  syms <- read.csv("https://raw.githubusercontent.com/mlitarowich/mlitarowich/master/stock_symbols2020_returns.csv")  
  syms$Date <- mdy(syms$Date)
  
  syms_prices <- read.csv("https://raw.githubusercontent.com/mlitarowich/mlitarowich/master/stock_symbols2020.csv")
  syms_prices$Date <- mdy(syms_prices$Date)
  
  data <- syms
  data_prices <- syms_prices
  prices <- syms_prices %>%
    gather(key = "Stock", value = "Prices", -Date)
  
  # --------------------------------------------------------- Tab 1 - Stock Price History

  output$pricing <- renderPlot({
    stock_list <- colnames(syms[-1])
    check <- c(input$checkbox)
    filt <- stock_list[stock_list %in% check]
    prices_filt <- prices %>% filter(prices$Stock %in% filt)
    
    if (input$checkbox == "All"){
      ggplot(prices, aes(x = prices$Date, y = log10(prices$Prices), color = Stock)) + 
        geom_line() + 
        ggtitle(paste0("5-Year Stock Price History")) +
        xlab("Date") + 
        ylab("Price (log 10)") + 
        theme(
          plot.title = element_text(size = 35, hjust = 0.5),
          axis.title.x = element_text(size = 30),
          axis.title.y = element_text(size = 30),
          axis.text.x = element_text(size = 20),
          axis.text.y = element_text(size = 20),
          legend.title = element_text(size = 30),
          legend.text = element_text(size = 25)
        )
    }else {
      ggplot(prices_filt, aes(x = prices_filt$Date, y = log10(prices_filt$Prices), color = Stock)) + 
        geom_line() + 
        ggtitle(paste0("5-Year Stock Price History")) +
        xlab("Date") + 
        ylab("Price (log 10)") + 
        theme(
          plot.title = element_text(size = 35, hjust = 0.5),
          axis.title.x = element_text(size = 30),
          axis.title.y = element_text(size = 30),
          axis.text.x = element_text(size = 20),
          axis.text.y = element_text(size = 20),
          legend.title = element_text(size = 30),
          legend.text = element_text(size = 25)
        ) 
    }
    
  }, height = 900)
  
  output$tableprices <- DT::renderDataTable({
    data_filtered_prices <- data_prices %>% filter(data_prices[,1] >= input$daterangeprices[1] & 
                                              data_prices[,1] <= input$daterangeprices[2])
  })
  
  
  #----------------------------------------------------------------- Tab 2 - Data Table
  
  output$table <- DT::renderDataTable({
    data_filtered <- (data %>% filter(data[,1] >= input$daterange[1] & data[,1] <= input$daterange[2]))
    data_filtered_percent <- data_filtered[-1] * 100
    data_filtered_percent$Date <- data_filtered$Date
    data_filtered_percent %>%
      select(36, 1:35)
  })
  
  output$returnhist1 <- renderPlot({
    
    data_filtered <- data %>% filter(data[,1] >= input$daterange[1] & data[,1] <= input$daterange[2])
    
    ggplot(data_filtered, aes(x = data_filtered[,input$symhist1])) + 
      geom_histogram(bins = (nrow(data_filtered) / sqrt(nrow(data_filtered) / 2)), fill = "darkgreen", 
                     color = "black", aes(y = ..density..)) + 
      ggtitle(paste0("Histogram of ", input$symhist1, " daily returns")) +
      xlab(input$symhist1) + 
      ylab("Proportion") + 
      theme(
        plot.title = element_text(size = 25, hjust = 0.5),
        axis.title.x = element_text(size = 20),
        axis.title.y = element_text(size = 20),
        axis.text.x = element_text(size = 15),
        axis.text.y = element_text(size = 15)
        )
  })
  
  output$returnhist2 <- renderPlot({
    data_filtered <- data %>% filter(data[,1] >= input$daterange[1] & data[,1] <= input$daterange[2])
    ggplot(data_filtered, aes(x = data_filtered[,input$symhist2])) + 
      geom_histogram(bins = (nrow(data_filtered) / sqrt(nrow(data_filtered) / 2)), fill = "goldenrod", 
                     color = "black", aes(y = ..density..)) + 
      ggtitle(paste0("Histogram of ", input$symhist2, " daily returns")) +
      xlab(input$symhist2) + 
      ylab("Proportion") + 
      theme(
        plot.title = element_text(size = 25, hjust = 0.5),
        axis.title.x = element_text(size = 20),
        axis.title.y = element_text(size = 20),
        axis.text.x = element_text(size = 15),
        axis.text.y = element_text(size = 15)
      )
  })
  
  output$returnhist3 <- renderPlot({
    data_filtered <- data %>% filter(data[,1] >= input$daterange[1] & data[,1] <= input$daterange[2])
    ggplot(data_filtered, aes(x = data_filtered[,input$symhist3])) + 
      geom_histogram(bins = (nrow(data_filtered) / sqrt(nrow(data_filtered) / 2)), fill = "lightblue", 
                     color = 'black', aes(y = ..density..)) + 
      ggtitle(paste0("Histogram of ", input$symhist3, " daily returns")) + 
      xlab(input$symhist3) + 
      ylab("Proportion") + 
      theme(
        plot.title = element_text(size = 25, hjust = 0.5),
        axis.title.x = element_text(size = 20),
        axis.title.y = element_text(size = 20),
        axis.text.x = element_text(size = 15),
        axis.text.y = element_text(size = 15)
      )
  })
  
  
  #------------------------------------------------------------------ Tab 3 - Scatter Plot with Regression Line
  output$point <- renderPlot({
    
    data_range <- data %>% filter(data[,1] >= input$symdate[1] & data[,1] <= input$symdate[2])
    
    ggplot(data_range, aes(x = data_range[,input$sym2], y = data_range[,input$sym1])) + 
      geom_point(color = "darkblue", size = 3, alpha = 0.5, pch = 16) + 
      geom_smooth(aes(x = data_range[,input$sym2], y = data_range[,input$sym1]), 
                  se = FALSE, method = "lm", color = "red", size = 1) + 
      ggtitle("Regression Scatter Chart and Line") + 
      xlab(input$sym2) + 
      ylab(input$sym1) + 
      theme(
        plot.title = element_text(size = 30, hjust = 0.5),
        axis.title.x = element_text(size = 20),
        axis.title.y = element_text(size = 20),
        axis.text.x = element_text(size = 15),
        axis.text.y = element_text(size = 15)
      )
  }, height = 900)
  
  #--------------------- Regression Summary
  output$summary <- renderPrint({
    
    data_range <- data %>% filter(data[,1] >= input$symdate[1] & data[,1] <= input$symdate[2])
    
    if ((input$sym3 != "None") & (input$sym4 != "None") & (input$sym5 != "None")){
      fit1 <- lm(data_range[, input$sym1] ~ data_range[, input$sym2] + 
                   data_range[, input$sym3] + data_range[, input$sym4] + data_range[, input$sym5])
      names(fit1$coefficients) <- c("Intercept", input$sym2, input$sym3, input$sym4, input$sym5)
      varnames <- paste(input$sym1, "~", paste0(input$sym2, " + ", input$sym3, " + ", input$sym4, " + ", input$sym5))
      fit1[["call"]] <- varnames
      summary(fit1)
      
    }else if ((input$sym3 != "None") & (input$sym4 != "None") & (input$sym5 == "None")){
      fit1 <- lm(data_range[, input$sym1] ~ data_range[, input$sym2] + data_range[, input$sym3] + data_range[, input$sym4])
      names(fit1$coefficients) <- c("Intercept", input$sym2, input$sym3, input$sym4)
      varnames <- paste(input$sym1, "~", paste0(input$sym2, " + ", input$sym3, " + ", input$sym4))
      fit1[["call"]] <- varnames
      summary(fit1)
      
    }else if ((input$sym3 != "None") & (input$sym4 == "None") & (input$sym5 == "None")){
      fit1 <- lm(data_range[, input$sym1] ~ data_range[, input$sym2] + data_range[, input$sym3])
      names(fit1$coefficients) <- c("Intercept", input$sym2, input$sym3)
      varnames <- paste(input$sym1, "~", paste0(input$sym2, " + ", input$sym3))
      fit1[["call"]] <- varnames
      summary(fit1)
      
    }else if ((input$sym3 == "None") & (input$sym4 == "None") & (input$sym5 == "None")){
      fit1 <- lm(data_range[, input$sym1] ~ data_range[, input$sym2])
      names(fit1$coefficients) <- c("Intercept", input$sym2)
      varnames <- paste(input$sym1, "~", paste(input$sym2))
      fit1[["call"]] <- varnames
      summary(fit1)
      
    }
  })

  
  #------------------------------------------------------------------------ Tab 4 - Residual Plots
  output$resid <- renderPlot({
    
    data_date <- data %>% filter(data[,1] >= input$residdate[1] & data[,1] <= input$residdate[2])
    
    fit2 <- lm(data_date[,input$residsym1] ~ data_date[,input$residsym2], data)
    ggplot(fit2, aes(x = seq(min(fit2$model[,2]), max(fit2$model[,2]), length.out = nrow(data_date)), y = fit2$residuals)) + 
      geom_point(color = 'darkblue') + 
      ggtitle(paste0("Residual Analysis of ", input$residsym1, " ~ ", input$residsym2)) + 
      xlab("X") + 
      ylab("Residuals") + 
      theme(
        plot.title = element_text(size = 25, hjust = 0.5),
        axis.title.x = element_text(size = 20),
        axis.title.y = element_text(size = 20),
        axis.text.x = element_text(size = 15),
        axis.text.y = element_text(size = 15)
      )
  })
  
  output$residhist <- renderPlot({
    
    data_date <- data %>% filter(data[,1] >= input$residdate[1] & data[,1] <= input$residdate[2])
    
    residfit <- lm(data_date[,input$residsym1] ~ data_date[,input$residsym2])
    names(residfit$coefficients) <- c("Intercept", input$residsym2)
    ggplot(data = residfit, aes(x = residfit$residuals)) + 
      geom_histogram(bins = (nrow(data_date) / sqrt(nrow(data_date) / 2)), fill = "green", color = "black") + 
      ggtitle("Histogram of Residuals") + 
      xlab("Residuals") + 
      theme(
        plot.title = element_text(size = 25, hjust = 0.5),
        axis.title.x = element_text(size = 20),
        axis.title.y = element_text(size = 20),
        axis.text.x = element_text(size = 15),
        axis.text.y = element_text(size = 15)
      )
  })
  
  
  output$jb <- renderPrint({
    
    data_date <- data %>% filter(data[,1] >= input$residdate[1] & data[,1] <= input$residdate[2])
    
    LinearRegression <- lm(data_date[,input$residsym1] ~ data_date[,input$residsym2])
    names(LinearRegression$coefficients) <- c("Intercept", input$residsym2)
    JB <- JarqueBeraTest(LinearRegression$residuals)
    print(JB)
    print(paste0("Skewness = ", Skew(LinearRegression$residuals)))
    print(paste0("Excess Kurtosis = ", Kurt(LinearRegression$residuals)))
    
    if (JB[[3]] <= 0.05){
      print("The distribution of residuals is likely non-normal")
    }else {
      print("The distribution of residuals is likely normal")
    }
    
  })

  output$bp <- renderPrint({
    
    data_date <- data %>% filter(data[,1] >= input$residdate[1] & data[,1] <= input$residdate[2])
    
    LinearRegression <- lm(data_date[,input$residsym1] ~ data_date[,input$residsym2])
    names(LinearRegression$coefficients) <- c("Intercept", input$residsym2)
    BP <- bptest(LinearRegression)
    print(BP)
    
    if (BP[[4]] <= 0.05){
      print("The data is likely heteroskedastic")
    }else{
      print("The data is likely not heteroskedastic")
    }
    
  })
  
  output$dw <- renderPrint({
    
    data_date <- data %>% filter(data[,1] >= input$residdate[1] & data[,1] <= input$residdate[2])
    
    LinearRegression <- lm(data_date[,input$residsym1] ~ data_date[,input$residsym2])
    names(LinearRegression$coefficients) <- c("Intercept", input$residsym2)
    DW <- dwtest(LinearRegression)
    print(DW)
    
    if ((DW[[1]] >= 0) & (DW[[1]] <= 1.9)){
      print("The data likely has positive autocorrelation")
    }else if ((DW[[1]] >=2.1) & (DW[[1]] <= 4)){
      print("The data likely has negative autocorrelation")
    }else{
      print("The data likely does not have autocorrelation")
    }
    
  })
  
  
} 


shinyApp(ui, server)













