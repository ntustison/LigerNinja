library( ggplot2 )

resultsAll <- read.csv( 'adniThicknessResultsSubjectSpace.csv' )
results3T <- read.csv( 'Subset3T/labelresultsAnts.csv' )

results3T$ID <- as.character( results3T$ID )
results3T$VISIT <- as.character( results3T$VISIT )
resultsAll$ID <- as.character( resultsAll$ID )
resultsAll$VISIT <- as.character( resultsAll$VISIT )

# newColnames <- append( colnames( results3T ), c( "QUICK" ), after = 1 )
newColnames <- colnames( resultsAll )
compiledResults <- data.frame( matrix( vector(), 0, length( newColnames ), dimnames = list( c(), newColnames ) ), stringsAsFactors = FALSE )

count <- 0
for( i in 1:nrow( results3T ) )
  {
  individualSubject <- results3T[i,]

  subjectId <- individualSubject$ID
  visit <- individualSubject$VISIT
  age <- as.numeric( individualSubject$AGE )

  matched <- which( resultsAll$ID == subjectId & resultsAll$VISIT == visit
    & resultsAll$RECALLED == 0 & resultsAll$FIELD_STRENGTH > 2.0 )

  if( length( matched ) == 1 )
    {
    cat( i, "->", as.character( subjectId ), "\n" )

    count <- count + 1

    compiledResults[nrow( compiledResults ) + 1,] <- resultsAll[matched,]

#     compiledResults[nrow( compiledResults ) + 1,] <-
#       c( as.character( resultsAll$ID[matched] ), 1, as.character( resultsAll$SITE[matched] ),
#       as.character( individualSubject$SEX ), resultsAll$AGE[matched], as.character( resultsAll$DIAGNOSIS[matched] ),
#       as.character( resultsAll$VISIT[matched] ), resultsAll$BRAIN_VOLUME[matched],
#       tail( as.numeric( resultsAll[matched,] ), n = 62 ) )

#     compiledResults[nrow( compiledResults ) + 1,] <-
#       c( as.character( individualSubject$ID ), 0, as.character( individualSubject$SITE ),
#       as.character( individualSubject$SEX ), individualSubject$AGE, as.character( individualSubject$DIAGNOSIS ),
#       as.character( individualSubject$VISIT ), individualSubject$VOLUME,
#       tail( as.numeric( individualSubject ), n = 62 ) )

    }
  }

write.csv( compiledResults, file = "compiledQuickResults.csv" )
