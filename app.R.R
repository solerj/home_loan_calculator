library(shinydashboard)
library(shiny)
library(DT)
library(leaflet)
library(htmltools)
library(lubridate)

sidebar <- dashboardSidebar(
  sidebarMenu(
    menuItem("Loan Calculator"
             , tabName = "calculator"
             , icon = icon("calculator"))
  )
)

body <- dashboardBody(
  tabItems(
    tabItem(tabName = "calculator"
            , h2(
              fluidRow(
                column(
                  width = 6
                  , box(
                    width = 12
                    , title = tags$p('Maximum Loan Calculator'
                                     , style = "font-size: 25px;")
                    , background = "teal"
                    
                    , column(
                      width = 6
                      
                      , selectizeInput(inputId = "max_loanType"
                                       , label = tags$p("Home Loan Type:"
                                                        , style = "font-size: 20px;")
                                       , choices = c('First Property'
                                                     , 'Second Property'
                                                     , 'Buy-to-Let')
                                       , selected = 'First Property')
                      
                      , textInput(inputId = "max_grossSalary"
                                  , label = tags$p("Yearly Gross Salary:"
                                                   , style = "font-size: 20px;")
                                  , placeholder = "e.g. €25,000")
                      
                      , textInput(inputId = "max_birthYear"
                                  , label = tags$p("Birth Year:"
                                                   , style = "font-size: 20px;")
                                  , placeholder = "e.g. 1990")
                      
                      , textInput(inputId = 'max_availableDeposit'
                                  , label = tags$p("Cash Available for Deposit:"
                                                   , style = "font-size: 20px;")
                                  , placeholder = "e.g. €30,000")
                      
                      , textInput(inputId = 'max_otherLoans'
                                  , label = tags$p("Existing Loan Commitments (mth):"
                                                   , style = "font-size: 20px;")
                                  , placeholder = "e.g. €750")
                    )
                    
                    , column(
                      width = 6
                      
                      , sliderInput(inputId = "max_interestRate"
                                    , label = tags$p("Yearly Interest Rate:"
                                                     , style = "font-size: 20px;")
                                    , min = 2.5
                                    , max = 5.0
                                    , step = 0.05
                                    , value = 3.0)
                      
                      , sliderInput(inputId = "max_minDeposit"
                                    , label = tags$p("Minimum % Deposit:"
                                                     , style = "font-size: 20px;")
                                    , min = 5
                                    , max = 40
                                    , step = 5
                                    , value = 10)
                      
                      , sliderInput(inputId = "max_years"
                                    , label = tags$p("Number of Years to repay Loan:"
                                                     , style = "font-size: 20px;")
                                    , min = 5
                                    , max = 45
                                    , value = 30)
                    )
                  )
                )
                , column(
                  width = 3
                  , infoBoxOutput("max_loanAmount", width = 12)
                  , infoBoxOutput("max_propertyValue", width = 12)
                  , infoBoxOutput("max_loanPerc", width = 12)
                  , infoBoxOutput("max_monthlyPayment", width = 12)
                  , infoBoxOutput("max_remainingNet", width = 12)
                )
                , column(
                  width = 3
                  , infoBoxOutput("max_totalPayments", width = 12)
                  , infoBoxOutput("max_totalInterest", width = 12)
                )
              )
              
              , fluidRow(
                column(
                  width = 6
                  , box(
                    width = 12
                    , title = tags$p('Monthly Payment Calculator'
                                     , style = "font-size: 25px;")
                                     , background = "green"
                    
                    , column(
                      width = 6
                      
                      , selectizeInput(inputId = "check_loanType"
                                       , label = tags$p("Home Loan Type:"
                                                        , style = "font-size: 20px;")
                                       , choices = c('First Property'
                                                     , 'Second Property'
                                                     , 'Buy-to-Let')
                                       , selected = 'First Property')
                      
                      , textInput(inputId = 'check_propertyValue'
                                  , label = tags$p("Property Value:"
                                                   , style = "font-size: 20px;")
                                  , placeholder = "e.g. €200,000")
                      
                      , textInput(inputId = "check_birthYear"
                                  , label = tags$p("Birth Year:"
                                                   , style = "font-size: 20px;")
                                  , placeholder = "e.g. 1990")
                      
                      # , textInput(inputId = "check_grossSalary"
                      #             , label = tags$p("Yearly Gross Salary:"
                      #                              , style = "font-size: 20px;")
                      #             , placeholder = "e.g. €25,000")
                    )
                    
                    , column(
                      width = 6
                      
                      , sliderInput(inputId = "check_interestRate"
                                    , label = tags$p("Yearly Interest Rate:"
                                                     , style = "font-size: 20px;")
                                    , min = 2.5
                                    , max = 5.0
                                    , step = 0.05
                                    , value = 3.0)
                      
                      , sliderInput(inputId = "check_percDeposit"
                                    , label = tags$p("% Deposit:"
                                                     , style = "font-size: 20px;")
                                    , min = 5
                                    , max = 40
                                    , step = 1
                                    , value = 10)
                      
                      , sliderInput(inputId = "check_years"
                                    , label = tags$p("Number of Years to repay Loan:"
                                                     , style = "font-size: 20px;")
                                    , min = 5
                                    , max = 45
                                    , value = 30)
                    )
                  )
                )
                , column(
                  width = 3
                  , infoBoxOutput("check_propertyValue", width = 12)
                  , infoBoxOutput("check_loanAmount", width = 12)
                  , infoBoxOutput("check_depositAmount", width = 12)
                  , infoBoxOutput("check_monthlyPayment", width = 12)
                )
                , column(
                  width = 3
                  , infoBoxOutput("check_totalPayments", width = 12)
                  , infoBoxOutput("check_totalInterest", width = 12)
                )
              )
            )
    )
  )
)

# Put them together into a dashboardPage
ui <- dashboardPage(
  dashboardHeader(title = "Smart Agent"),
  sidebar,
  body
)

# Define server logic required to draw a histogram ----
server <- function(session, input, output) {
  
  max_type_maxYears <- reactive({
    if (input$max_loanType == 'Buy-to-Let'){
      25
    } else{
      45
    }
  })
  
  max_type_interestRate <- reactive({
    if (input$max_loanType == 'Buy-to-Let'){
      4.0
    } else if (input$max_loanType == 'Second Property'){
      3.25
    } else if (input$max_loanType == 'First Property'){
      3.0
    }
  })
  
  max_type_percDeposit <- reactive({
    if (input$max_loanType == 'Buy-to-Let'){
      30
    } else if (input$max_loanType == 'Second Property'){
      25
    } else if (input$max_loanType == 'First Property'){
      10
    }
  })
  
  check_type_maxYears <- reactive({
    if (input$check_loanType == 'Buy-to-Let'){
      25
    } else{
      45
    }
  })
  
  check_type_interestRate <- reactive({
    if (input$check_loanType == 'Buy-to-Let'){
      4.0
    } else if (input$check_loanType == 'Second Property'){
      3.25
    } else if (input$check_loanType == 'First Property'){
      3.0
    }
  })
  
  check_type_percDeposit <- reactive({
    if (input$check_loanType == 'Buy-to-Let'){
      30
    } else if (input$check_loanType == 'Second Property'){
      25
    } else if (input$check_loanType == 'First Property'){
      10
    }
  })
  
 # --------------------------------------------------------------
 
 observe({
   
   updateSliderInput(session
                   , inputId = "max_years"
                   , value = min(max(5
                                     , (as.numeric(65) - (as.numeric(year(Sys.Date())) - (as.numeric(input$max_birthYear))) - as.numeric(1)))
                                 , max_type_maxYears()))
   
   updateSliderInput(session
                     , inputId = "max_interestRate"
                     , value = format(max_type_interestRate(), nsmall = 2))
   
   updateSliderInput(session
                     , inputId = "max_minDeposit"
                     , value = max_type_percDeposit())
   
   updateSliderInput(session
                     , inputId = "check_years"
                     , value = min(max(5
                                       , (as.numeric(65) - (as.numeric(year(Sys.Date())) - (as.numeric(input$check_birthYear))) - as.numeric(1)))
                                   , check_type_maxYears()))
 })
  
  observe({
    
   updateSliderInput(session
                     , inputId = "check_interestRate"
                     , value = format(check_type_interestRate(), nsmall = 2))
   
   updateSliderInput(session
                     , inputId = "check_percDeposit"
                     , value = check_type_percDeposit())
   
   #    # Can also set the label, this time for input$inText2
   #    # updateTextInput(session, "inText2",
   #    #                 label = paste("New label", x),
   #    #                 value = paste("New text", x))
   
   })
  
  # --------------------------------------------------------------
  
  # ADD NI CONTRIBUTION
  
  max_net <- reactive({
    if (is.na(as.numeric(input$max_grossSalary))){
      NA
    } else if (as.numeric(input$max_grossSalary)<=9100){
      (as.numeric(input$max_grossSalary)-min(48.05*52, as.numeric(input$max_grossSalary)*0.1))/12
    } else if (as.numeric(input$max_grossSalary)<=14500){
      (as.numeric(input$max_grossSalary) - (as.numeric(input$max_grossSalary)*0.15 - 1365) - min(48.05*52, as.numeric(input$max_grossSalary)*0.1))/12
    } else if (as.numeric(input$max_grossSalary)<=19500){
      (as.numeric(input$max_grossSalary) - (as.numeric(input$max_grossSalary)*0.25 - 2815) - min(48.05*52, as.numeric(input$max_grossSalary)*0.1))/12
    } else if (as.numeric(input$max_grossSalary)<=60000){
      (as.numeric(input$max_grossSalary) - (as.numeric(input$max_grossSalary)*0.25 - 2725) - min(48.05*52, as.numeric(input$max_grossSalary)*0.1))/12
    } else {
      (as.numeric(input$max_grossSalary) - (as.numeric(input$max_grossSalary)*0.35 - 8725) - min(48.05*52, as.numeric(input$max_grossSalary)*0.1))/12
    }
  })
  
  max_otherLoans <- reactive({
    if (is.na(as.numeric(input$max_otherLoans))){
      as.numeric(0)
    } else {
      as.numeric(input$max_otherLoans)
    }
  })
  
  a_s <- reactive({
    (as.numeric(input$max_grossSalary)/12)*0.3-max_otherLoans()
  })
  
  a_d <- reactive({
    if (is.na(input$max_availableDeposit)){
      0
    } else {
      as.numeric(input$max_availableDeposit)
    }
  })
  
  a_m <- reactive({
    as.numeric(input$max_years)*12
  })
  
  a_i <- reactive({
    as.numeric(input$max_interestRate)/1200
  })
  
  a_p <- reactive({
    as.numeric(input$max_minDeposit)/100
  })
  
  max_discountFactor <- reactive({
    as.numeric(((1+a_i())^a_m()-1) / (a_i()*(1+a_i())^a_m()))
  })
  
  max_loanAmount <- reactive({
    as.numeric(a_s()*max_discountFactor())
  })
  
  max_propertyValue <- reactive({
    if (is.na(a_d()) | is.na(max_loanAmount())){
      NA
    } else if (as.numeric(a_d()) >= as.numeric(a_p())/(as.numeric(1)-as.numeric(a_p()))*as.numeric(max_loanAmount())){
      as.numeric(a_d() + max_loanAmount())
    } else {
      as.numeric(a_d()/a_p())
    }
  })
  
  output$max_loanAmount <- renderInfoBox({
    infoBox(
      title = tags$p("Maximum Loan Amount", style = "font-size: 20px;")
      , value = tags$p(paste0('€', format(round(max_loanAmount(), 0)
                                          , big.mark=","
                                          , scientific=FALSE))
                       , style = "font-size: 25px;")
      , color = "teal"
      , icon = icon("hand-holding-usd")
      , fill = T
    )
  })
  
  output$max_propertyValue <- renderInfoBox({
    infoBox(
      title = tags$p("Maximum Property Value", style = "font-size: 20px;")
      , value = tags$p(paste0('€', format(round(max_propertyValue(), 0)
                                          , big.mark=","
                                          , scientific=FALSE))
                       , style = "font-size: 25px;")
      , color = "teal"
      , icon = icon("home")
      , fill = T
    )
  })
  
  output$max_loanPerc <- renderInfoBox({
    infoBox(
      title = tags$p("% Deposit", style = "font-size: 20px;")
      , value = tags$p(paste0(round(a_d()/max_propertyValue()*100,0), '%')
                       , style = "font-size: 25px;")
      , color = "teal"
      , icon = icon("piggy-bank")
      , fill = T
    )
  })
  
  output$max_monthlyPayment <- renderInfoBox({
    infoBox(
      title = tags$p("Monthly Payment", style = "font-size: 20px;")
      , value = tags$p(paste0('€', format(round((max_propertyValue()-a_d())/max_discountFactor(), 0)
                                          , big.mark=","
                                          , scientific=FALSE))
                       , style = "font-size: 25px;")
      , color = "teal"
      , icon = icon("calendar-check")
      , fill = T
    )
  })
  
  output$max_remainingNet <- renderInfoBox({
    infoBox(
      title = tags$p("Remaining Net Salary/mth", style = "font-size: 20px;")
      , value = tags$p(paste0('€', format(round(max_net()-((max_propertyValue()-a_d())/max_discountFactor())-max_otherLoans(), 0)
                                          , big.mark=","
                                          , scientific=FALSE))
                       , style = "font-size: 25px;")
      , color = "teal"
      , icon = icon("wallet")
      , fill = T
    )
  })
  
  output$max_totalPayments <- renderInfoBox({
    infoBox(
      title = tags$p("Total Payments Incl. Dep", style = "font-size: 20px;")
      , value = tags$p(paste0('€', format(round((max_propertyValue()-a_d())/max_discountFactor()*a_m()+a_d(), 0)
                                          , big.mark=","
                                          , scientific=FALSE))
                       , style = "font-size: 25px;")
      , color = "teal"
      , icon = icon("university")
      , fill = T
    )
  })
  
  output$max_totalInterest <- renderInfoBox({
    infoBox(
      title = tags$p("Total Interest", style = "font-size: 20px;")
      , value = tags$p(paste0('€', format(round((max_propertyValue()-a_d())/max_discountFactor()*a_m()-(max_propertyValue()-a_d()), 0)
                                          , big.mark=","
                                          , scientific=FALSE))
                       , style = "font-size: 25px;")
      , color = "teal"
      , icon = icon("university")
      , fill = T
    )
  })
  
  # --------------------------------------------------------------
  
  # WORK OUT NET SALARY
  # c_s <- reactive({
  #   (as.numeric(input$max_grossSalary)/12)/3
  # })
  
  c_m <- reactive({
    as.numeric(input$check_years)*12
  })
  
  c_i <- reactive({
    as.numeric(input$check_interestRate)/1200
  })
  
  c_p <- reactive({
    as.numeric(input$check_percDeposit)/100
  })
  
  check_discountFactor <- reactive({
    as.numeric(((1+c_i())^c_m()-1) / (c_i()*(1+c_i())^c_m()))
  })
  
  check_propertyValue <- reactive({
    as.numeric(input$check_propertyValue)
  })
  
  check_loanAmount <- reactive({
    as.numeric((1-c_p())*check_propertyValue())
  })
  
  output$check_propertyValue <- renderInfoBox({
    infoBox(
      title = tags$p("Property Value", style = "font-size: 20px;")
      , value = tags$p(paste0('€', format(round(check_propertyValue(), 0)
                                          , big.mark=","
                                          , scientific=FALSE))
                       , style = "font-size: 25px;")
      , color = "green"
      , icon = icon("home")
      , fill = T
    )
  })
  
  output$check_loanAmount <- renderInfoBox({
    infoBox(
      title = tags$p("Loan Amount", style = "font-size: 20px;")
      , value = tags$p(paste0('€', format(round(check_loanAmount(), 0)
                                          , big.mark=","
                                          , scientific=FALSE))
                       , style = "font-size: 25px;")
      , color = 'green'
      , icon = icon("hand-holding-usd")
      , fill = T
    )
  })
  
  output$check_depositAmount <- renderInfoBox({
    infoBox(
      title = tags$p("Deposit Amount", style = "font-size: 20px;")
      , value = tags$p(paste0('€', format(round(check_propertyValue()-check_loanAmount(),0)
                                          , big.mark=","
                                          , scientific=FALSE))
                              , style = "font-size: 25px;")
      , color = "green"
      , icon = icon("piggy-bank")
      , fill = T
    )
  })
  
  output$check_monthlyPayment <- renderInfoBox({
    infoBox(
      title = tags$p("Monthly Payment", style = "font-size: 20px;")
      , value = tags$p(paste0('€', format(round(check_loanAmount()/check_discountFactor(), 0)
                                          , big.mark=","
                                          , scientific=FALSE))
                       , style = "font-size: 25px;")
      , color = "green"
      , icon = icon("calendar-check")
      , fill = T
    )
  })
  
  output$check_totalPayments <- renderInfoBox({
    infoBox(
      title = tags$p("Total Payments Incl. Dep", style = "font-size: 20px;")
      , value = tags$p(paste0('€', format(round(check_loanAmount()/check_discountFactor()*c_m()+(check_propertyValue()-check_loanAmount()), 0)
                                          , big.mark=","
                                          , scientific=FALSE))
                       , style = "font-size: 25px;")
      , color = "green"
      , icon = icon("university")
      , fill = T
    )
  })
  
  output$check_totalInterest <- renderInfoBox({
    infoBox(
      title = tags$p("Total Interest", style = "font-size: 20px;")
      , value = tags$p(paste0('€', format(round(check_loanAmount()/check_discountFactor()*c_m()-check_loanAmount(), 0)
                                          , big.mark=","
                                          , scientific=FALSE))
                       , style = "font-size: 25px;")
      , color = "green"
      , icon = icon("university")
      , fill = T
    )
  })
  
}

shinyApp(ui, server)
