# install the text mining package
#install.packages("tm")
library(tm)

#install.packages("wordcloud")
library(wordcloud)

#install.packages("SnowballC")
library(SnowballC)

#install.packages("ggplot2")
library(ggplot2)

install.packages("tidytext")
library(tidytext)

#Step 1


afinn1 <- scan("C:/Users/kdoyl/OneDrive/Documents/IST687/AFINN (1).txt", character(0),sep="\n")
afinn1 <- strsplit(afinn1, ",")
afinn1 <- unlist(afinn1)
afinn1 <- split(afinn1, c(1,2))

afinnwords <- unlist(afinn1[1],use.names = FALSE)
afinnscores <- unlist(afinn[2],use.names = FALSE)

#Step 2 MLK afinn score

MLK <- "C:/Users/kdoyl/OneDrive/Documents/IST687/MLKspeech (1).txt"
MLK <- scan(MLK, character(0),sep = "\n")


getTermDocMatrix <- function(input_text){
  
  # first, read in the data and create a corpus
  words.vec <- VectorSource(input_text)
  words.corpus <- Corpus(words.vec)
  words.corpus
  
  # then process the corpus into a nice "bag of words"
  words.corpus <- tm_map(words.corpus, content_transformer(tolower))
  words.corpus <- tm_map(words.corpus, removePunctuation)
  words.corpus <- tm_map(words.corpus, removeNumbers)
  words.corpus <- tm_map(words.corpus, removeWords, stopwords(kind = "en"))
  words.corpus <- tm_map(words.corpus, stemDocument)
  words.corpus <- tm_map(words.corpus, stripWhitespace) # remove whitespace
  
  return(TermDocumentMatrix(words.corpus)) # return the term matrix
}
tdm <- getTermDocMatrix(MLK)

m <- as.matrix(tdm)

mlkwordcount <- rowSums(m)

mlkwordcount <- sort(mlkwordcount,decreasing = TRUE)

mlkwords <- names(mlkwordcount)

mlkMatchedwords <- match(mlkwords, afinnwords, nomatch = 0)
mlkmatchedscores <-as.numeric(afinnscores[mlkMatchedwords])
mlkmatchedCount <- mlkwordcount[which(mlkMatchedwords != 0)]
sum(mlkmatchedCount*mlkmatchedscores)

#Step 3
mlk <- "C:/Users/kdoyl/OneDrive/Documents/IST687/MLKspeech (1).txt"
line.count <- length(scan(mlk, character(0), sep = "\n")) # count the number of lines in the speech
i <- ceiling(line.count/4) # the number of lines to read in

first_quadrant <- scan(mlk, character(0), sep = "\n", nlines = i) # read in the first 25%
second_quadrant <- scan(mlk, character(0), sep = "\n", skip = i, nlines = i) # skip the first 25%, read the 2nd 25%
third_quadrant <- scan(mlk, character(0), sep = "\n", skip = 2*i, nlines = i) # skip the first 50%, read the 3rd 25%
fourth_quadrant <- scan(mlk, character(0), sep = "\n", skip = 3*i) # skip the first 75%, read the rest

ScoreCalc <- function(inputVector) {
  section.vector <- VectorSource(inputVector)
  section.corpus <- Corpus(section.vector)
  sectionTDM <- TermDocumentMatrix(section.corpus)
  sectionMatrix <- as.matrix(sectionTDM)
  sectionWordCount <- rowSums(sectionMatrix)
  sectionWords <- names(sectionWordCount)
  Matched <- match(sectionWords,afinnwords,nomatch = 0)
  matchedScores <- as.numeric(afinnscores[Matched])
  matchedCount <- sectionWordCount[which(Matched !=0)]
  finalScore <- sum(matchedCount*matchedScores)
  return(finalScore)
  }

Score1 <- ScoreCalc(first_quadrant)
Score2 <- ScoreCalc(second_quadrant)
Score3 <- ScoreCalc(third_quadrant)
Score4 <- ScoreCalc(fourth_quadrant)


#Step 4
result.df <- data.frame(matrix(ncol = 2))
colnames(result.df) <- c("Score","mlkscore")
result.df <- rbind(result.df, c("Score1",Score1))
result.df <- rbind(result.df, c("Score2",Score2))
result.df <- rbind(result.df, c("Score3",Score3))
result.df <- rbind(result.df, c("Score4",Score4))
result.df <- result.df[-c(1),]

ggplot(result.df, aes(x = Score, y = mlkscore)) +
  geom_bar(stat = "identity", position = "dodge")
