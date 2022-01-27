ui <- function(request){fluidPage(  
  titlePanel("COVID-19: last confirmed cases"),
  fluidRow(
    column(3,checkboxGroupInput("Country", "Countries to show:",
                                c("Russia", "USA", "Italy", "France"),
                                selected = "Russia")
    ),
    column(3,radioButtons("abs_rel", "Type of values:",
                          c("Relative", "Absolute"))
    ),
    column(3,radioButtons("real_for", "What to show:",
                                c("Real data only", "Real data with forecast"))
    ),
    column(3,conditionalPanel("input.real_for === 'Real data with forecast'",
                                radioButtons("model", "Model type:", 
                                c("Additive", "Mulitplicative"))))
    ),
    column(3,sliderInput("past", "How many days to show:",
              min = 30, max = 180,
              value = 30, step = 15)
    ),
    column(3,conditionalPanel("input.real_for === 'Real data with forecast'",
                                sliderInput("forecast", "Forecast window size in days:",
                                min = 10, max = 30,
                                value = 10, step = 5))
    ),
    plotOutput("cases_plot")
  )
}


