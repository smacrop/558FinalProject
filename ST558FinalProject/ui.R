library(caret)
library(shiny)
library(DT)
shinyUI(fluidPage(
  titlePanel(h2("House Price Predictor App")),
  sidebarLayout(
    sidebarPanel(
      h4("User Inputs"),
      
      radioButtons("variables", "Variable to Explore in DATA Tab", c(
                                                      "Area" = 2,
                                                      "Bedrooms" = 3,
                                                      "Bathrooms" = 4,
                                                      "Stories" = 5,
                                                      "Mainroad" = 6,
                                                      "Guestroom" = 7,
                                                      "Basement" = 8,
                                                      "Hot water heating" = 9,
                                                      "Air conditioning" = 10,
                                                      "Parking Spots" = 11,
                                                      "Furnishing" = 13)),
      
      numericInput("rows", "Dataset Rows in DATA Tab (by 10)", value = 0),

      selectInput("plots", "Variable Type for DATA EXPLORATION Tab", c("Continuous",
                                              "Discrete",
                                              "Both")),
      
      selectInput("models", "Variable Type for MODEL FITTING Tab", c("Numeric",
                                                              "Categorical",
                                                              "Both")),

      numericInput("area", "Area (1000-17000 Numeric)", value = 1000),
      numericInput("bedrooms", "Bedrooms (1-6 Numeric)", value = 1),
      numericInput("bathrooms", "Bathrooms (1-4 Numeric)", value = 1),
      numericInput("stories", "Stories (1-4 Numeric)", value = 1),
      numericInput("parking", "Parking Spots (0-3 Numeric)", value = 0),
      textInput("mainroad", "Mainroad? (yes or no Categorical)", value = "yes"),
      textInput("guestroom", "Guestroom? (yes or no Categorical)", value = "yes"),
      textInput("basement", "Basement? (yes or no Categorical)", value = "yes"),
      textInput("hotwaterheating", "Hot Water Heating? (yes or no Categorical)", value = "yes"),
      textInput("airconditioning", "Air Conditioning? (yes or no Categorical)", value = "yes"),
      textInput("furnishingstatus", "Furnishing Status? (unfurnished, semi-furnished or furnished Categorical)", value = "furnished"),
    
      
      numericInput("cvprop", "Training Set Proportion", value = 0.7),
      
      selectInput("predictormodel", "Model for PREDICTION Tab", c("MLR",
                                                              "Regression Tree",
                                                              "Random Forest"))
      
    ),
    
    mainPanel(
      tabsetPanel(type = "tabs",
                  tabPanel("ABOUT", textOutput("about")),
                  tabPanel("DATA EXPLORATION", plotOutput("dataExploration")),
                  tabPanel("MODELING INFO", textOutput("info")),
                  tabPanel("MODEL FITTING", dataTableOutput("fits")),
                  tabPanel("PREDICTION", dataTableOutput("pred")),
                  tabPanel("DATA", dataTableOutput("data"))
    )
  )
)))

