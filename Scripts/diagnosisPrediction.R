library( kernlab )
library( caret )
library( randomForest )
library( ggplot2 )
library( grid )

nPermutations <- 100

trainingPortions <- c( 0.9 )

timePoints <- c( 'ADNI1 Screening', 'ADNI1 Baseline', 'ADNI1/GO Month 6',
  'ADNI1/GO Month 12', 'ADNI1/GO Month 24', 'ADNI1/GO Month 36' )

resultsCombined <- read.csv( 'adniThicknessResultsSubjectSpace.csv' )
resultsCombined <- resultsCombined[which( resultsCombined$RECALLED == 0 ),]

resultsCombined$GENDER <- as.factor( resultsCombined$GENDER )
resultsCombined$VISIT <- as.character( resultsCombined$VISIT )
resultsCombined$DIAGNOSIS <- as.factor( resultsCombined$DIAGNOSIS )
resultsCombined$FIELD_STRENGTH <- as.numeric( resultsCombined$FIELD_STRENGTH )

corticalLabels <- tail( colnames( resultsCombined ), n = 62 )

#       resultsCombined <- resultsCombined[which( resultsCombined$VISIT == q ),]
drops <- c( 'ID', 'SITE', 'IMAGE_ID', 'STUDY_ID', 'VISIT', 'RECALLED' )
#       drops <- c( 'ID', 'SITE', 'IMAGE_ID', 'STUDY_ID', 'VISIT', 'RECALLED', 'FIELD_STRENGTH' )
resultsCombined <- resultsCombined[, !( names( resultsCombined ) %in% drops )]



for( q in timePoints )
  {
  cat( q, '\n' )
  count <- 1
  for( p in trainingPortions )
    {
    antsAccuracy <- rep( 0, nPermutations )
    antsKappa <- rep( 0, nPermutations )

    antsTestingData <- c()
    antsPredictions <- c()

    trainingPortion <- p
    cat( "trainingPortion = ", trainingPortion, "\n", sep = '' )

    forestImp <- list()
    for( n in seq( 1, nPermutations, by = 1 ) )
      {
      trainingIndices <- createDataPartition( resultsCombined$GENDER, p = trainingPortion, list = FALSE, times = 1 )
      trainingData <- resultsCombined[trainingIndices,]
      testingData <- resultsCombined[-trainingIndices,]

      brainDxRF <- randomForest( DIAGNOSIS ~ ., data = trainingData, importance = TRUE,
                    na.action = na.omit, replace = FALSE, ntree = 200 )
      if( n == 1 )
        {
        forestImp[[1]] <- importance( brainDxRF, type = 1 )
        } else {
        forestImp[[1]] <- forestImp[[1]] + importance( brainDxRF, type = 1 )
        }
      predictedDx <- predict( brainDxRF, testingData )

      assessment <- postResample( testingData$DIAGNOSIS, predictedDx )

      antsAccuracy[n] <- assessment[1];
      antsKappa[n] <- assessment[2];
      antsTestingData <- append( antsTestingData, testingData$DIAGNOSIS )
      antsPredictions <- append( antsPredictions, predictedDx )

      cat( "  Permutation ", n, ":  accuracy = ", antsAccuracy[n], "\n", sep = '' )
      }

    forestImp[[1]] <- forestImp[[1]] / nPermutations

    forestImp.df <- data.frame( Statistic = names( forestImp[[1]][,1] ), Importance = as.numeric( forestImp[[1]][,1] )  )
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

    qname <- gsub( " ", "_", q )
    qname <- gsub( "/", "_", qname )

    ggsave( file = paste( "~/Desktop/importanceANTs", qname, '_', p, ".pdf", sep = "" ), plot = vPlot, width = 4, height = 8 )
    cat( "Average accuracy = ", mean( antsAccuracy ), "\n", sep = '' )

    confusionMatrix( antsTestingData, antsPredictions )

    count <- count + 1
    }
  }
