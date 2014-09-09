library( kernlab )
library( caret )
library( randomForest )
library( ggplot2 )
library( grid )

nPermutations <- 500

trainingPortions <- c( 0.9 )


count <- 1
for( p in trainingPortions )
  {
  antsAccuracy <- rep( 0, nPermutations )
  antsKappa <- rep( 0, nPermutations )

  antsTestingData <- c()
  antsPredictions <- c()

  fsAccuracy <- rep( 0, nPermutations )
  fsKappa <- rep( 0, nPermutations )

  fsTestingData <- c()
  fsPredictions <- c()

  trainingPortion <- p
  cat( "trainingPortion = ", trainingPortion, "\n", sep = '' )

  forestImp <- list()
  for( n in seq( 1, nPermutations, by = 1 ) )
    {
    cat( "  Permutation ", n, "\n", sep = '' )

    thicknessTypes = c( 'Ants', 'FreeSurfer' )
    for( whichPipeline in thicknessTypes )
      {
      resultsCombined <- read.csv( paste0( 'labelresults', whichPipeline, '.csv' ) )
      resultsCombined$SITE <- as.factor( resultsCombined$SITE )
      resultsCombined$SEX <- as.factor( resultsCombined$SEX )
      resultsCombined$DIAGNOSIS <- as.factor( resultsCombined$DIAGNOSIS )

      corticalLabels <- tail( colnames( resultsCombined ), n = 62 )

      drops <- c( "ID", "SITE", "VISIT" )

      resultsCombined <- resultsCombined[, !( names( resultsCombined ) %in% drops )]

      trainingIndices <- createDataPartition( resultsCombined$SEX, p = trainingPortion, list = FALSE, times = 1 )
      trainingData <- resultsCombined[trainingIndices,]
      testingData <- resultsCombined[-trainingIndices,]

      brainDxRF <- randomForest( DIAGNOSIS ~ ., data = trainingData, importance = TRUE,
                    na.action = na.omit, replace = FALSE, ntree = 200 )
      if( n == 1 )
        {
        if( whichPipeline == 'Ants' )
          {
          forestImp[[1]] <- importance( brainDxRF, type = 1 )
          } else {
          forestImp[[2]] <- importance( brainDxRF, type = 1 )
          }
        } else {
        if( whichPipeline == 'Ants' )
          {
          forestImp[[1]] <- forestImp[[1]] + importance( brainDxRF, type = 1 )
          } else {
          forestImp[[2]] <- forestImp[[2]] + importance( brainDxRF, type = 1 )
          }
        }
      predictedDx <- predict( brainDxRF, testingData )

      assessment <- postResample( testingData$DIAGNOSIS, predictedDx )

      if( whichPipeline == 'Ants' )
        {
        antsAccuracy[n] <- assessment[1];
        antsKappa[n] <- assessment[2];
        antsTestingData <- append( antsTestingData, testingData$DIAGNOSIS )
        antsPredictions <- append( antsPredictions, predictedDx )
        } else {
        fsAccuracy[n] <- assessment[1];
        fsKappa[n] <- assessment[2];
        fsTestingData <- append( fsTestingData, testingData$DIAGNOSIS )
        fsPredictions <- append( fsPredictions, predictedDx )
        }
      }
    cat( "  Permutation ", n, ":  accuracy = ", antsAccuracy[n], ", ", fsAccuracy[n], "\n", sep = '' )
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

    ggsave( file = paste( "~/Desktop/importanceSubset", thicknessTypes[n], p, ".pdf", sep = "" ), plot = vPlot, width = 4, height = 8 )
    }

  confusionMatrix( fsTestingData, fsPredictions )
  confusionMatrix( antsTestingData, antsPredictions )

  cat( "Average accuracy = ", mean( antsAccuracy ), ", ", mean( fsAccuracy ), "\n", sep = '' )

  count <- count + 1
  }
