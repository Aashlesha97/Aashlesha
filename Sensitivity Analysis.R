sensitivityAnalysis <- function(model,
                                target = NULL,
                                predictors = NULL, #Data Frame
                                predictorsMeans = NULL, #Data Frame
                                samplesN = 101,
                                from = 0.6,
                                to = 1.4,
                                targetPrediction = "ratio", # c("ratio", "absolute")
                                predictionType = "prediction",
                                level = 0.9) {
  
  if (missing(target)) {
    predictors <- model$model[-1]
    target <- model$model[1]
  }
  targetName <- names(target)
  
  target <- target[[1]]
  
  numberPredictors <- ncol(predictors)     
  
  if (missing(predictorsMeans)) {
    initialPredictorsValues <- predictors %>%
      summarise_all(mean)
    
  } else {
    initialPredictorsValues <- predictorsMeans    
  }
  
  initialTargetValue <- mean(predict(model, newdata = initialPredictorsValues))
  
  sensitivityData <- sapply(initialPredictorsValues, function(x) rep(x, samplesN * numberPredictors))
  changeDF <- seq(from, to, length.out = samplesN)
  
  for (i in seq(1, numberPredictors)) {
    sensitivityData[seq((i-1)*samplesN + 1, i * samplesN), i] <- changeDF * initialPredictorsValues[[i]]    
  }
  sensitivityData <- data.frame(sensitivityData)
  
  predictedTarget <- predict(model,
                             newdata = sensitivityData,
                             interval = predictionType,
                             level = level)
  
  predictedTarget <- as.data.frame(predictedTarget)
  
  if (targetPrediction == "ratio")
    predictedTarget <- mutate_all(predictedTarget, function(x) {x / initialTargetValue})
  
  df <- data.frame(normalized.predictor.change = numeric(0),
                   predictor = character(0),
                   normalized.target.change.fit = numeric(0),
                   normalized.target.change.lower= numeric(0),
                   normalized.target.change.upper = numeric(0)) 
  
  for (i in seq(1, numberPredictors)) {
    df <- rbind(df, 
                data.frame(
                  normalized.predictor.change = changeDF,
                  predictor = names(predictors)[i],
                  normalized.target.change.fit = predictedTarget$fit[seq((i-1)*samplesN + 1, i * samplesN)],
                  normalized.target.change.lower = predictedTarget$lwr[seq((i-1)*samplesN + 1, i * samplesN)],
                  normalized.target.change.upper = predictedTarget$upr[seq((i-1)*samplesN + 1, i * samplesN)]))
  }
  if (targetPrediction == "ratio") {
    gg <- ggplot(df, aes(x = normalized.predictor.change, 
                         group = predictor)) +
      geom_ribbon(aes(ymin = normalized.target.change.lower,
                      ymax = normalized.target.change.upper,
                      fill = predictor), alpha = 0.1) + 
      geom_vline(xintercept = 1,  color = "grey80") + 
      geom_hline(yintercept = 1, color = "grey80") +
      geom_abline(slope = 1, linetype = "dashed", color = "grey80") + 
      geom_abline(slope = -1, intercept = 2, linetype = "dashed", color = "grey80") +
      geom_line(aes(x = normalized.predictor.change,
                    y = normalized.target.change.fit,
                    color = predictor)) +
      ylab(paste("Normalized", targetName, "change")) +
      xlab("Normalized Predictor Change") + 
      theme_few()
  } else {
    gg <- ggplot(df, aes(x = normalized.predictor.change, 
                         group = predictor)) +
      geom_ribbon(aes(ymin = normalized.target.change.lower,
                      ymax = normalized.target.change.upper,
                      fill = predictor), alpha = 0.1) + 
      geom_vline(xintercept = 1,  color = "grey80") + 
      geom_hline(yintercept = initialTargetValue, color = "grey80") +
      geom_line(aes(x = normalized.predictor.change,
                    y = normalized.target.change.fit,
                    color = predictor)) +
      ylab(targetName) + 
      xlab("Normalized Predictor Change") + 
      theme_few()        
  }
  
  variableMeans <- cbind(data.frame(target = initialTargetValue),
                         data.frame(initialPredictorsValues))
  names(variableMeans) <- c(targetName, names(initialPredictorsValues))
  return(list(ggplot = gg,
              predictionData = sensitivityData,
              resultsData = df,
              variableMeans = variableMeans))
}
install.packages("dplyr")
install.packages("ggplot2")
install.packages("ggthemes")

#load the libraries

library(dplyr)
library(ggplot2)
library(ggthemes)

#import the data file from excel

library(readxl)
Book1 <- read_excel("D:/KTH/thesis/THESIS WORK/Book1.xlsx")
View(Book1)

#create a linear model for the data

model <- lm( covid ~ flowrate , data = Book1)
summary(model)
results <- sensitivityAnalysis(model, level = .90, predictionType = "prediction", targetPrediction = "raw")
plot(results$ggplot)

#create a polynomial model for the data
#in this example, a cubic model is created

polymodel <- Book1$output_par
predictors <- select(Book1 , Dep_var1 , Dep_var2)
model_1 <- lm (output ~ poly(Dep_var1, 3, raw = TRUE) *poly(Dep_var2, 3, raw = TRUE) , data = Book1)
summary(model_1)
results_1 <- sensitivityAnalysis(model_1 , polymodel , predictors , level = 0.90 , predictionType = "prediction", targetPrediction = "raw")
plot(results_1$ggplot)
results$variableMeans
