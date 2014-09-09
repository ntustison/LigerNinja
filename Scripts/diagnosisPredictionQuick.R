library( kernlab )
library( caret )
library( randomForest )
library( ggplot2 )
library( grid )

results <- read.csv( 'compiledQuickResults.csv' )
results$SITE <- as.factor( results$SITE )
results$GENDER <- as.factor( results$GENDER )
results$DIAGNOSIS <- as.factor( results$DIAGNOSIS )

corticalLabels <- tail( colnames( results ), n = 62 )

nPermutations <- 500

trainingPortions <- c( 0.5 )

count <- 1
for( p in trainingPortions )
  {
  quickAccuracy <- rep( 0, nPermutations )
  quickKappa <- rep( 0, nPermutations )

  quickTestingData <- c()
  quickPredictions <- c()

  noQuickAccuracy <- rep( 0, nPermutations )
  noQuickKappa <- rep( 0, nPermutations )

  noQuickTestingData <- c()
  noQuickPredictions <- c()

  trainingPortion <- p
  cat( "trainingPortion = ", trainingPortion, "\n", sep = '' )

  forestImp <- list()
  for( n in seq( 1, nPermutations, by = 1 ) )
    {
    quickTypes = c( 0, 1 )
    for( whichQuick in quickTypes )
      {
      resultsWhichQuick <- results[which( results$QUICK == whichQuick ),]

      drops <- c( "ID", "SITE", "VISIT", "QUICK", "X" )

      resultsWhichQuick <- resultsWhichQuick[, !( names( resultsWhichQuick ) %in% drops )]

      trainingIndices <- createDataPartition( resultsWhichQuick$GENDER, p = trainingPortion, list = FALSE, times = 1 )
      trainingData <- resultsWhichQuick[trainingIndices,]
      testingData <- resultsWhichQuick[-trainingIndices,]

      brainDxRF <- randomForest( DIAGNOSIS ~ ., data = trainingData, importance = TRUE,
                    na.action = na.omit, replace = FALSE, ntree = 200 )
      if( n == 1 )
        {
        if( whichQuick == 1 )
          {
          forestImp[[1]] <- importance( brainDxRF, type = 1 )
          } else {
          forestImp[[2]] <- importance( brainDxRF, type = 1 )
          }
        } else {
        if( whichQuick == 1 )
          {
          forestImp[[1]] <- forestImp[[1]] + importance( brainDxRF, type = 1 )
          } else {
          forestImp[[2]] <- forestImp[[2]] + importance( brainDxRF, type = 1 )
          }
        }
      predictedDx <- predict( brainDxRF, testingData )

      assessment <- postResample( testingData$DIAGNOSIS, predictedDx )

      if( whichQuick == 1 )
        {
        quickAccuracy[n] <- assessment[1];
        quickKappa[n] <- assessment[2];
        quickTestingData <- append( quickTestingData, testingData$DIAGNOSIS )
        quickPredictions <- append( quickPredictions, predictedDx )
        } else {
        noQuickAccuracy[n] <- assessment[1];
        noQuickKappa[n] <- assessment[2];
        noQuickTestingData <- append( noQuickTestingData, testingData$DIAGNOSIS )
        noQuickPredictions <- append( noQuickPredictions, predictedDx )
        }
      }
    cat( "  Permutation ", n, ":  accuracy = ", quickAccuracy[n], ", ", noQuickAccuracy[n], "\n", sep = '' )
    }

  for( n in 1:2 )
    {
    forestImp[[n]] <- forestImp[[n]] / nPermutations

    forestImp.df <- data.frame( Statistic = names( forestImp[[n]][,1] ), Importance = as.numeric( forestImp[[n]][,1] )  )
    forestImp.df <- forestImp.df[order( forestImp.df$Importance ),]

    forestImp.df$Statistic <- factor( x = forestImp.df$Statistic, levels = forestImp.df$Statistic )

    vPlot <- ggplot( data = forestImp.df, aes( x = Importance, y = Statistic ) ) +
             geom_point( aes( color = Importance ) ) +
             ylab( "" ) +
             scale_x_continuous( "MeanDecreaseAccuracy", limits = c( 0, 20 ) ) +
             scale_color_continuous( low = "navyblue", high = "darkred" ) +
             theme( axis.text.y = element_text( size = 5 ) ) +
             theme( plot.margin = unit( c( 0.1, 0.1, 0.1, -0.5 ), "cm" ) ) +
             theme( axis.title = element_text( size = 10 ) ) +
             theme( legend.position = "none" )

    ggsave( file = paste( "~/Desktop/importanceSubset", quickTypes[n], p, ".pdf", sep = "" ), plot = vPlot, width = 4, height = 8 )
    }

  confusionMatrix( noQuickTestingData, noQuickPredictions )
  confusionMatrix( quickTestingData, quickPredictions )

  cat( "Average accuracy = ", mean( quickAccuracy ), ", ", mean( noQuickAccuracy ), "\n", sep = '' )

  count <- count + 1
  }
