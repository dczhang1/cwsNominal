### Function for calculating CWS Index for Nominal Data 
### Don C. Zhang, dc.zhang1@gmail.com


### Example dataframe with 5 items, 2 test administrations
### This code can be generalized to more than 2 test administrations
inputData <- data.frame(time1 = c("A","A","B","D","C")
                      ,time2= c("A","B","D","D","C"))


### Function for calculating single subject CWS Index
funCWS <- function(input) {
            ### Main data parameters
                        # Count number of rows (number of items)
                        nRows <- dim(inputData)[1]
                        # Count numbner of columns (number of time points)
                        nCol <- dim(inputData)[2]
                        # Count total number of responses
                        nTotal <- nRows * nCol
                        # Total possible matches between and across items
                        matchTotal <- choose(nTotal, 2)
            
            ### Calculating inconsistency
                        # Possible matches within item
                        possWithin <- choose(nCol, 2) * nRows
                        # Matches within item
                        matchWithin <- 0
                        # SUm the matches for each item across all administrations
                        for (i in 1:nRows) {
                                    x <- as.character(inputData[i, ])
                                    z <- sum(choose(table(x), 2))
                                    matchWithin <- matchWithin + z
                        }
                        # Inconsistency Index (Denominator of CWS)
                        inconsistency <- 1 - (as.numeric(matchWithin / possWithin))
                        
            ### Calculating discrimination
                        # Possible match across items
                        possBetween <- matchTotal - possWithin
                        
                        # Non-Match across items
                        #Gather all responses into the same array
                        gatherData <- gather(inputData)$value 
                        #count total number of matches and subtract matches within a column
                        matchBetween <- sum(choose(table(gatherData), 2)) - matchWithin 
                        # Discrimination Index
                        discrim <- (possBetween - matchBetween) / possBetween
                        
                        # CWS Index
                        cwsIndex <- discrim / inconsistency         
                        cwsIndex #function output
}


### Using the function
funCWS(inputData)


