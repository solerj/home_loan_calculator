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
                  width = 3
                , box(
                  width = 12
                  , title = tags$p('*OPTIONAL* Completing this form will pre-fill the calculator parameters for you. But you can still manually overwite the parameters on the right'
                                   , style = "font-size: 25px;")
                  , background = "light-blue"
                  
                  , selectizeInput(inputId = "loanType"
                                   , label = tags$p("Home Loan Type:"
                                                    , style = "font-size: 20px;")
                                   , choices = c('First Property'
                                                 , 'Second Property'
                                                 , 'Buy-to-Let')
                                   , selected = 'First Property')
                  
                  , textInput(inputId = "birthYear"
                              , label = tags$p("Birth Year:"
                                               , style = "font-size: 20px;")
                              , placeholder = "e.g. 1990")
                  
                  , textInput(inputId = "grossSalary"
                              , label = tags$p("Yearly Gross Salary:"
                                               , style = "font-size: 20px;")
                              , placeholder = "e.g. 25000")
                  
                  , textInput(inputId = "propertyValue"
                              , label = tags$p("Desired Property Value:"
                                               , style = "font-size: 20px;")
                              , placeholder = "e.g. 200000")
                  
                  , textInput(inputId = 'availableDeposit'
                              , label = tags$p("Cash Available for Deposit:"
                                               , style = "font-size: 20px;")
                              , placeholder = "e.g. 30000")
                  
                )
                )
                , column(
                  width = 3
                  , box(
                    width = 12
                    , title = tags$p('Maximum Loan Calculator'
                                     , style = "font-size: 25px;")
                  , background = "teal"
                  
                  , textInput(inputId = "afford_grossSalary"
                              , label = tags$p("Yearly Gross Salary:"
                                               , style = "font-size: 20px;")
                              , placeholder = "e.g. 25000")
                  
                  , textInput(inputId = "afford_availableDeposit"
                              , label = tags$p("Cash Available for Deposit:"
                                               , style = "font-size: 20px;")
                              , placeholder = "e.g. 30000")
                  
                  , textInput(inputId = "afford_years"
                              , label = tags$p("Number of Years to repay Loan:"
                                               , style = "font-size: 20px;")
                              , placeholder = "e.g. 25")
                  
                  , textInput(inputId = "afford_interestRate"
                              , label = tags$p("Yearly Interest Rate:"
                                               , style = "font-size: 20px;")
                              , value = 3.1
                              , placeholder = "e.g. 3.1")
                )
                , infoBoxOutput("afford_loanAmount", width = 12)
                , infoBoxOutput("afford_propertyValue", width = 12)
                
                )
                
                , column(
                  width = 3
                  , box(
                    width = 12
                  , title = tags$p('Monthly Payment Calculator'
                                   , style = "font-size: 25px;")
                  , background = "green"
                  
                  , textInput(inputId = "check_propertyValue"
                              , label = tags$p("Property Value:"
                                               , style = "font-size: 20px;")
                              , placeholder = "e.g. 200000")
                  
                  , textInput(inputId = "check_availableDeposit"
                              , label = tags$p("Cash Available for Deposit:"
                                               , style = "font-size: 20px;")
                              , placeholder = "e.g. 30000")
                  
                  , textInput(inputId = "check_years"
                              , label = tags$p("Number of Years to repay Loan:"
                                               , style = "font-size: 20px;")
                              , placeholder = "e.g. 25")
                  
                  , textInput(inputId = "check_interestRate"
                              , label = tags$p("Yearly Interest Rate:"
                                               , style = "font-size: 20px;")
                              , placeholder = "e.g. 3.1")
                )
                , infoBoxOutput("check_monthlyPayment", width = 12)
                , infoBoxOutput("check_remainingNet", width = 12)
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
  
  maxYears <- reactive({
    if (input$loanType == 'Buy-to-Let'){
      25
    } else{
      45
    }
  })
  
  interestRate <- reactive({
    if (input$loanType == 'Buy-to-Let'){
      4.0
    } else if (input$loanType == 'Second Property'){
      3.1
    } else if (input$loanType == 'First Property'){
      2.9
    }
  })
  
  observe({
    
    #updateTextInput(session, inputId = "afford_years", value = (65-(year(Sys.Date())-input$birthYear+1)))
    
    updateTextInput(session
                    , inputId = "afford_grossSalary"
                    , value = input$grossSalary)
    
    updateTextInput(session
                    , inputId = "afford_availableDeposit"
                    , value = input$availableDeposit)
    
    updateTextInput(session
                    , inputId = "afford_years"
                    , value = min(max(5
                                      , (as.numeric(65) - (as.numeric(year(Sys.Date())) - (as.numeric(input$birthYear))) - as.numeric(1)))
                                  , maxYears()))
    
    updateTextInput(session
                    , inputId = "afford_interestRate"
                    , value = format(interestRate(), nsmall = 1))
    
    # --------------------------------------------------------------
    
    updateTextInput(session
                    , inputId = "check_propertyValue"
                    , value = input$propertyValue)
    
    updateTextInput(session
                    , inputId = "check_availableDeposit"
                    , value = input$availableDeposit)
    
    updateTextInput(session
                    , inputId = "check_years"
                    , value = min(max(5
                                      , (as.numeric(65) - (as.numeric(year(Sys.Date())) - (as.numeric(input$birthYear))) - as.numeric(1)))
                                  , maxYears()))
    
    updateTextInput(session
                    , inputId = "check_interestRate"
                    , value = format(interestRate(), nsmall = 1))
                    
    #updateTextInput(session, inputId = "afford_years", value = input$birthYear)
    
    # Can also set the label, this time for input$inText2
    # updateTextInput(session, "inText2",
    #                 label = paste("New label", x),
    #                 value = paste("New text", x))
    
    # --------------------------------------------------------------
    
    a_s <- reactive({
      as.numeric(input$afford_grossSalary)/12/3
    })
    
    a_d <- reactive({
      as.numeric(input$afford_availableDeposit)
    })
    
    a_m <- reactive({
      as.numeric(input$afford_years)*12
    })
    
    a_i <- reactive({
      as.numeric(input$afford_interestRate)/1200
    })
    
    afford_discountFactor <- reactive({
      as.numeric(((1+a_i())^a_m()-1) / (a_i()*(1+a_i())^a_m()))
    })
    
    afford_loanAmount <- reactive({
      as.numeric(a_s()*afford_discountFactor())
    })
    
    afford_propertyValue <- reactive({
      #as.numeric(a_s()*afford_discountFactor())
    })
    
    output$afford_loanAmount <- renderInfoBox({
      infoBox(
        title = tags$p("Maximum Loan Amount", style = "font-size: 20px;")
        , value = tags$p(paste0('€', format(afford_loanAmount()
                                            , big.mark=","
                                            , scientific=FALSE))
                         , style = "font-size: 25px;")
        , color = "teal"
        , icon = icon("hand-holding-usd")
        , fill = T
      )
    })
    
    output$afford_propertyValue <- renderInfoBox({
      infoBox(
        title = tags$p("Maximum Property Value", style = "font-size: 20px;")
        , value = tags$p('€285,000 test'
                         , style = "font-size: 25px;")
        , color = "teal"
        , icon = icon("home")
        , fill = T
      )
    })
    
    
    
  })
}

shinyApp(ui, server)
