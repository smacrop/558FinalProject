library(shiny)
library(caret)
library(tidyverse)
library(DT)

shinyServer(function(input, output) {
  
  housedata <- reactive({house <- read.csv("C:/Users/Owner/OneDrive/Documents/Housing.csv")})
  
  output$about <- renderText("The purpose of this app is to investigate housing data and to predict house prices.
                             The dataset can be found at the following URL: 
                             https://www.kaggle.com/datasets/yasserh/housing-prices-dataset.
                             The variables in the dataset include house price ($), area (sqft), number of bedrooms,
                             number of bathrooms, number of stories, whether or not connected to main road (yes or no),
                             whether or not there is a guest room (yes or no), whether or not there is a basement (yes or no), 
                             whether or not there is a hot water heater (yes or no), whether or not there is Air conditioning (yes or no), 
                             the number of parking spots, and the furnishing status (unfurnished, semi-furnished, or furnished).
                             The Data Exploration tab allows the user to create histograms and scatterplots for the 
                             numeric variables and barplots for the categorical variables. The Modeling Info tab
                             tells the user about the benefits and drawbacks of the three modeling techniques used 
                             to predict house prices. The Model Fitting tab allows the user to select variables for 
                             modeling building to get summary statistics on the chosen model's fit. The Prediction tab
                             allows the user to use their chosen model to predict house prices. The Data tab allows 
                             the user to see the dataset and subset it based on variables of interest.")
  
  output$dataExploration <- renderPlot({
    house <- housedata()
    
    
    if (input$plots=="Continuous") {
      par( mfrow= c(2,2) )
      hist(house$price, main="House Prices", xlab="Price $", col="red")
      hist(house$area, main="House Areas", xlab="Area Sqft", col="blue")
      plot(house$area,house$price, main="Price vs Area", xlab="Area", ylab="Price", col="purple")
    } else if (input$plots=="Discrete") {
      par( mfrow= c(5,2) )
      par(mar = c(2, 2, 2, 2))
      hist(house$bedrooms, main="Bedrooms", xlab="", col="red")
      hist(house$bathrooms, main="Bathrooms", xlab="", col="blue")
      hist(house$stories, main="Stories", xlab="", col="green")
      barplot(table(house$mainroad), main = "Mainroad", col="orange")
      barplot(table(house$guestroom), main = "Guestroom", col="red")
      barplot(table(house$basement), main = "Basement", col="blue")
      barplot(table(house$hotwaterheating), main = "Hot Water Heating", col="green")
      barplot(table(house$airconditioning), main = "Air Conditioning", col="orange")
      hist(house$parking, main = "Parking Spots", col="red")
      barplot(table(house$furnishingstatus), main = "Furnishing", col="blue")
    } else {
      par( mfrow= c(7,2) )
      par(mar = c(2, 2, 2, 2))
      hist(house$price, main="House Prices", xlab="Price $", col="red")
      hist(house$area, main="House Areas", xlab="Area Sqft", col="blue")
      plot(house$area,house$price, main="Price vs Area", xlab="Area", ylab="Price", col="purple")
      hist(house$bedrooms, main="Bedrooms", xlab="", col="red")
      hist(house$bathrooms, main="Bathrooms", xlab="", col="blue")
      hist(house$stories, main="Stories", xlab="", col="green")
      barplot(table(house$mainroad), main = "Mainroad", col="orange")
      barplot(table(house$guestroom), main = "Guestroom", col="red")
      barplot(table(house$basement), main = "Basement", col="blue")
      barplot(table(house$hotwaterheating), main = "Hot Water Heating", col="green")
      barplot(table(house$airconditioning), main = "Air Conditioning", col="orange")
      hist(house$parking, main = "Parking Spots", col="red")
      barplot(table(house$furnishingstatus), main = "Furnishing", col="blue")
    }
  })
  
  
  
  
  # This is renderText()
  output$info <- renderText({
    "Multiple Linear Regression models have the advantage of simple implementation and
    interpretability since the model simply sets the mean response equal to a linear 
    combination of the predictor variables and the slope parameters. The disadvantages 
    include inflated parameter estimate variances (when the predictors are correlated),
    the user must add polynomial and interaction effects if desired, the errors are assumed
    to follow a Gaussian distribution, and others. The regression tree is mostly advantageous
    in its interpretability and the fact that interaction and higher order effects are 
    automatically accounted for. A big drawback of regression trees is the need for pruning
    because of potential overfitting. The random forest does not prune its trees (which 
    decreases bias for each tree) and also averages the tree predictions using a random subset
    of predictors for each iteration (which also decreases variance and correlation in predictions).
    The main drawback of random forest models is the loss of interpretability."

  })
  
  
  
  
  output$fits <- renderDataTable({
    house <- housedata()
    houseIndex <- createDataPartition(house$price, p = input$cvprop, list = FALSE)
    houseTrain <- house[houseIndex, ]
    houseTest <- house[-houseIndex, ]
    
    if (input$models=="Numeric") {
      
      # MLR
      lmFit <- train(price ~ area + bedrooms + bathrooms + stories + parking, data = houseTrain,
                     method = "lm",
                     trControl = trainControl(method = "cv",
                                              number = 5))
      # reg tree
      rtFit <- train(price ~ area + bedrooms + bathrooms + stories + parking, data = houseTrain,
                     method = "rpart",
                     trControl = trainControl(method = "cv", number = 5),
                     tuneGrid = data.frame(cp = seq(0,0.1,by=0.01)))
      
      # random forest
      rfFit <- train(price ~ area + bedrooms + bathrooms + stories + parking, data = houseTrain,
                     method = "rf",
                     trControl = trainControl(method = "cv",
                                              number = 5),
                     tuneGrid = data.frame(mtry = 1:5))
      
    } else if (input$models=="Categorical") {
      
      # MLR
      lmFit <- train(price ~ mainroad + guestroom + basement + hotwaterheating + 
                       airconditioning + furnishingstatus, data = houseTrain,
                     method = "lm",
                     trControl = trainControl(method = "cv",
                                              number = 5))
      
      # reg tree
      rtFit <- train(price ~ mainroad + guestroom + basement + hotwaterheating + 
                       airconditioning + furnishingstatus, data = houseTrain,
                     method = "rpart",
                     trControl = trainControl(method = "cv", number = 5),
                     tuneGrid = data.frame(cp = seq(0,0.1,by=0.01)))
      
      # random forest
      rfFit <- train(price ~ mainroad + guestroom + basement + hotwaterheating + 
                       airconditioning + furnishingstatus, data = houseTrain,
                     method = "rf",
                     trControl = trainControl(method = "cv",
                                              number = 5),
                     tuneGrid = data.frame(mtry = 1:6))
    } else {
      
      # MLR
      lmFit <- train(price ~ ., data = houseTrain,
                     method = "lm",
                     trControl = trainControl(method = "cv",
                                              number = 5))
      
      # reg tree
      rtFit <- train(price ~ ., data = houseTrain,
                     method = "rpart",
                     trControl = trainControl(method = "cv", number = 5),
                     tuneGrid = data.frame(cp = seq(0,0.1,by=0.01)))
      
      # random forest
      rfFit <- train(price ~ ., data = houseTrain,
                     method = "rf",
                     trControl = trainControl(method = "cv",
                                              number = 5),
                     tuneGrid = data.frame(mtry = 1:11))
    }
    
    
    predmlr <- predict(lmFit, newdata = houseTest)
    testmlr <- postResample(predmlr, houseTest$price)
    testmlr <- cbind(t(testmlr), 0)
    testmlr <- rbind(t(testmlr), 0)
    testmlr <- rbind(testmlr, 0)
    
    predrt <- predict(rtFit, newdata = houseTest)
    testrt <- postResample(predrt, houseTest$price)
    testrt <- cbind(t(testrt), 0)
    testrt <- rbind(t(testrt), 0)
    testrt <- rbind(testrt, 0)
    
    predrf <- predict(rfFit, newdata = houseTest)
    testrf <- postResample(predrf, houseTest$price)
    testrf <- cbind(t(testrf), 0)
    testrf <- rbind(t(testrf), 0)
    testrf <- rbind(testrf, 0)
    
    stats <- data.frame(t(lmFit$results[,2:7]), testmlr,
                        t(rtFit$results[rtFit$bestTune[1,1]+1,2:7]), testrt,
                        t(rfFit$results[rfFit$bestTune[1,1],2:7]), testrf)
    
    colnames(stats) <- c("MLR Train", "MLR Test", 
                              "Regression Tree Train", "Regression Tree Test", 
                              "Random Forest Train", "Random Forest Test")
    
    stats

  })
  
 
  
  
  
  
  
   output$pred <- renderDataTable({
    
    house <- housedata()
    houseIndex <- createDataPartition(house$price, p = 1-input$cvprop, list = FALSE)
    houseTrain <- house[houseIndex, ]
    houseTest <- house[-houseIndex, ]
    
    if (input$models=="Numeric") {
      
      # MLR
      lmFit <- train(price ~ area + bedrooms + bathrooms + stories + parking, data = houseTrain,
                     method = "lm",
                     trControl = trainControl(method = "cv",
                                              number = 5))
      # reg tree
      rtFit <- train(price ~ area + bedrooms + bathrooms + stories + parking, data = houseTrain,
                     method = "rpart",
                     trControl = trainControl(method = "cv", number = 5),
                     tuneGrid = data.frame(cp = seq(0,0.1,by=0.01)))
      
      # random forest
      rfFit <- train(price ~ area + bedrooms + bathrooms + stories + parking, data = houseTrain,
                     method = "rf",
                     trControl = trainControl(method = "cv",
                                              number = 5),
                     tuneGrid = data.frame(mtry = 1:5))
      
      if (input$predictormodel=="MLR"){
        Predicted_House_Price <- predict(lmFit, newdata = data.frame(area = input$area,
                                            bedrooms = input$bedrooms,
                                            bathrooms = input$bathrooms,
                                            stories = input$stories,
                                            parking = input$parking))
        as.data.frame(round(Predicted_House_Price,0))
        
      } else if (input$predictormodel=="Regression Tree"){
        Predicted_House_Price <- predict(rtFit, newdata = data.frame(area = input$area,
                                            bedrooms = input$bedrooms,
                                            bathrooms = input$bathrooms,
                                            stories = input$stories,
                                            parking = input$parking))
        as.data.frame(round(Predicted_House_Price,0))
        
      } else {
        Predicted_House_Price <- predict(rfFit, newdata = data.frame(area = input$area,
                                            bedrooms = input$bedrooms,
                                            bathrooms = input$bathrooms,
                                            stories = input$stories,
                                            parking = input$parking))
        as.data.frame(round(Predicted_House_Price,0))
      }
      
    } else if (input$models=="Categorical") {
      
      # MLR
      lmFit <- train(price ~ mainroad + guestroom + basement + hotwaterheating + 
                       airconditioning + furnishingstatus, data = houseTrain,
                     method = "lm",
                     trControl = trainControl(method = "cv",
                                              number = 5))
      
      # reg tree
      rtFit <- train(price ~ mainroad + guestroom + basement + hotwaterheating + 
                       airconditioning + furnishingstatus, data = houseTrain,
                     method = "rpart",
                     trControl = trainControl(method = "cv", number = 5),
                     tuneGrid = data.frame(cp = seq(0,0.1,by=0.01)))
      
      # random forest
      rfFit <- train(price ~ mainroad + guestroom + basement + hotwaterheating + 
                       airconditioning + furnishingstatus, data = houseTrain,
                     method = "rf",
                     trControl = trainControl(method = "cv",
                                              number = 5),
                     tuneGrid = data.frame(mtry = 1:6))
      
      if (input$predictormodel=="MLR"){
        Predicted_House_Price <- predict(lmFit, newdata = data.frame(mainroad = input$mainroad,
                                            guestroom = input$guestroom,
                                            basement = input$basement,
                                            hotwaterheating = input$hotwaterheating,
                                            airconditioning = input$airconditioning,
                                            furnishingstatus = input$furnishingstatus))
        as.data.frame(round(Predicted_House_Price,0))
        
      } else if (input$predictormodel=="Regression Tree"){
        Predicted_House_Price <- predict(rtFit, newdata = data.frame(mainroad = input$mainroad,
                                            guestroom = input$guestroom,
                                            basement = input$basement,
                                            hotwaterheating = input$hotwaterheating,
                                            airconditioning = input$airconditioning,
                                            furnishingstatus = input$furnishingstatus))
        as.data.frame(round(Predicted_House_Price,0))
        
      } else {
        Predicted_House_Price <- predict(rfFit, newdata = data.frame(mainroad = input$mainroad,
                                            guestroom = input$guestroom,
                                            basement = input$basement,
                                            hotwaterheating = input$hotwaterheating,
                                            airconditioning = input$airconditioning,
                                            furnishingstatus = input$furnishingstatus))
        as.data.frame(round(Predicted_House_Price,0))
      }
      
    } else {
      
      # MLR
      lmFit <- train(price ~ ., data = houseTrain,
                     method = "lm",
                     trControl = trainControl(method = "cv",
                                              number = 5))
      
      # reg tree
      rtFit <- train(price ~ ., data = houseTrain,
                     method = "rpart",
                     trControl = trainControl(method = "cv", number = 5),
                     tuneGrid = data.frame(cp = seq(0,0.1,by=0.01)))
      
      # random forest
      rfFit <- train(price ~ ., data = houseTrain,
                     method = "rf",
                     trControl = trainControl(method = "cv",
                                              number = 5),
                     tuneGrid = data.frame(mtry = 1:11))
      
      if (input$predictormodel=="MLR"){
        Predicted_House_Price <- predict(lmFit, newdata = data.frame(area = input$area,
                                            bedrooms = input$bedrooms,
                                            bathrooms = input$bathrooms,
                                            stories = input$stories,
                                            parking = input$parking, 
                                            mainroad = input$mainroad,
                                            guestroom = input$guestroom,
                                            basement = input$basement,
                                            hotwaterheating = input$hotwaterheating,
                                            airconditioning = input$airconditioning,
                                            furnishingstatus = input$furnishingstatus))
        as.data.frame(round(Predicted_House_Price,0))
        
      } else if (input$predictormodel=="Regression Tree"){
        Predicted_House_Price <- predict(rtFit, newdata = data.frame(area = input$area,
                                            bedrooms = input$bedrooms,
                                            bathrooms = input$bathrooms,
                                            stories = input$stories,
                                            parking = input$parking,
                                            mainroad = input$mainroad,
                                            guestroom = input$guestroom,
                                            basement = input$basement,
                                            hotwaterheating = input$hotwaterheating,
                                            airconditioning = input$airconditioning,
                                            furnishingstatus = input$furnishingstatus))
        as.data.frame(round(Predicted_House_Price,0))
        
      } else {
        Predicted_House_Price <- predict(rfFit, newdata = data.frame(area = input$area,
                                            bedrooms = input$bedrooms,
                                            bathrooms = input$bathrooms,
                                            stories = input$stories,
                                            parking = input$parking,
                                            mainroad = input$mainroad,
                                            guestroom = input$guestroom,
                                            basement = input$basement,
                                            hotwaterheating = input$hotwaterheating,
                                            airconditioning = input$airconditioning,
                                            furnishingstatus = input$furnishingstatus))
        as.data.frame(round(Predicted_House_Price,0))
      }
      
    }
    
    
  })
  
 
   
   
    output$data <- renderDataTable({
    house <- housedata()
    house[(10*(input$rows)+1):(10*(input$rows+1)),c(1,as.numeric(input$variables))]
  })
  
  
})





