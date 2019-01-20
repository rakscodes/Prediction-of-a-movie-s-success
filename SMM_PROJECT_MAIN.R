#Packages Required
#install.packages('twitteR')
#install.packages('RCurl')
#install.packages('tidyverse')
#install.packages("ggplot2")
#install.packages("wordcloud")
#install.packages("snowballc")
#install.packages("syuzhet")
#install.packages("tm")
#install.packages("rtweet")
#install.packages("openssl")
#install.packages("httpuv")
#install.packages("caret")
#install.packages("rpart.plot")
#install.packages("ggplot2")
#install.packages("e1071")
#install.packages("rminer")
#install.packages("psych")
#install.packages("naivebayes")
#install.packages("qdap")
#Packages Required

#LIBRARIES REQUIRED
library("rtweet")
library("twitteR")
library("RCurl")
library("tidyverse")
library("ggplot2")
library("stringr")
library("dplyr")
library("wordcloud")
library("SnowballC")
library("tm")
library("syuzhet")
library("caret")
library("rpart.plot")
library("e1071")
library("rminer")
library("psych")
library("naivebayes")
library("qdap")
#LIBRARIES REQUIRED

#CUSTOM METHODS
train_sentiment <- function(movienames){
  
  #VARIABLE DECLARATIONS
  positive_tweets_column = c()
  negative_tweets_column = c()
  result = c()
  nameOfMovie= c()
  current_row = c()
  output_matrix = matrix(ncol = 5)
  #VARIABLE DECLARATIONS
  
  for (mn in movienames) {
    
    current_row = c()
    raw_data <- search_tweets(
      mn, n = 1000, retryonratelimit = FALSE,lang = "en"
    )
    
    tw_data_frame <- as.data.frame(raw_data)
    
    #CLEANING DATA STARTS HERE
    tempDF <- tw_data_frame
    
    #STOPWORDS REMOVAL
    tempDF$text <- tolower(tempDF$text)
    stopwords_regex = paste(stopwords('en'), collapse = '\\b|\\b')
    stopwords_regex = paste0('\\b', stopwords_regex, '\\b')
    tempDF$text = stringr::str_replace_all(tempDF$text, stopwords_regex, '')
    #STOPWORDS REMOVAL
    
    tempDF$text <- enc2native(tempDF$text)
    url_pattern <- "http[s]?://(?:[a-zA-Z]|[0-9]|[$-_@.&+]|[!*\\(\\),]|(?:%[0-9a-fA
    -F][0-9a-fA-F]))+"
    tempDF$contentURL <- str_extract(tempDF$text, url_pattern)
    tempDF$text <- gsub("^[[:space:]]*","",tempDF$text)
    tempDF$text <- gsub("[[:space:]]*$","",tempDF$text)
    tempDF$text <- gsub(" +"," ",tempDF$text)
    tempDF$text <- gsub("'", "%%", tempDF$text)
    tempDF$text <- iconv(tempDF$text, "latin1", "ASCII", sub="")
    tempDF$text <- gsub("<(.*)>", "", tempDF$text)
    tempDF$text <- gsub("\\ \\. ", " ", tempDF$text)
    tempDF$text <- gsub("  ", " ", tempDF$text)
    tempDF$text <- gsub("%%", "\'", tempDF$text)
    tempDF$text <- gsub("https(.*)*$", "", tempDF$text)
    tempDF$text <- gsub("\\n", "-", tempDF$text)
    tempDF$text <- gsub("--", "-", tempDF$text)
    tempDF$text <- gsub("&amp;", "&", tempDF$text)
    tempDF$text[tempDF$text == " "] <- "<no text>"
    tempDF$text <- gsub("[[:space:]]*$","...",tempDF$text)
    
    cleanTweets <- tempDF %>% select("text")
    #CLEANING DATA ENDS HERE
    
    #PLOT WORD CLOUD IF REQUIRED
    word.corpus<-Corpus(VectorSource(tempDF$text))
    word.counts<-as.matrix(TermDocumentMatrix(word.corpus))
    word.freq<-sort(rowSums(word.counts), decreasing=TRUE)
    head(word.freq)
    set.seed(32)
    wordcloud(words=names(word.freq), freq=word.freq, scale=c(3,.5),max.words = 100, random.order = TRUE)
    #PLOT WORD CLOUD IF REQUIRED
    
    #SENTIMENT ANALYSIS BEGINS HERE
    word.df <- as.vector(tempDF$text)
    emotion.df <- get_nrc_sentiment(word.df)
    emotion.df2 <- cbind(tempDF$text, emotion.df) 
    write.csv(emotion.df2,"E:/MS COMPUTER SCIENCE/MS PROJECTS/Semester 1/Social Media Mining/emotions2.csv")
    sent.value <- get_sentiment(word.df)
    sent.value
    category_senti <- ifelse(sent.value < 0, "Negative", ifelse(sent.value > 0, "Positive", "Neutral"))
    table(category_senti)
    #SENTIMENT ANALYSIS ENDS HERE
    
    #FIND POSITIVE AND NEGATIVE PERCENTAGE
    positive.tweets <- word.df[sent.value > 0]
    negative.tweets <- word.df[sent.value < 0]
    pos <- as.data.frame(positive.tweets)
    neg <- as.data.frame(negative.tweets)
    total_positive_tweets <- count(pos)
    total_negative_tweets <- count(neg)
    total_rows <- nrow(cleanTweets)
    percent_positive_tweets <- (total_positive_tweets / total_rows) * 100
    percent_negative_tweets <- (total_negative_tweets / total_rows) * 100
    percent_neutral_tweets <- 100 - (percent_positive_tweets + percent_negative_tweets)
    #FIND POSITIVE AND NEGATIVE PERCENTAGE
    
    #CREATING THE TRAINING DATA FRAME
    if (percent_positive_tweets > percent_negative_tweets + percent_neutral_tweets){
      result <- "Hit" #c(result, "Hit")
    } else{
      if(percent_negative_tweets > percent_neutral_tweets){
        result <- "Flop"
      }else{
        if(percent_neutral_tweets > percent_negative_tweets){
          result <- "Average"
        }
      }
    }
    
    current_row <- c(current_row, mn, percent_positive_tweets, percent_negative_tweets, percent_neutral_tweets, result)
    output_matrix <- rbind(output_matrix, current_row)
    print(output_matrix)
    
    #WRITING COLLECTED DATA
    fileName <- paste("E:/MS COMPUTER SCIENCE/MS PROJECTS/Semester 1/Social Media Mining/Data Collected/SW removed 2/", mn, "_Data.csv", sep = "")
    write.csv(cleanTweets, fileName)
    #WRITING COLLECTED DATA
    
    #CREATING TRAINING DATA FRAME
  }
}

call_DecisionTree <- function(DataSetName){
  
  fileLoc <- paste("E:/MS COMPUTER SCIENCE/MS PROJECTS/Semester 1/Social Media Mining/", DataSetName, ".csv", sep = "")
  movie_df <- read.csv(fileLoc, sep = ',', header = TRUE)
  with(movie_df, table( Result))
  set.seed(3033)
  intrain <- createDataPartition(y = movie_df$Result, p= 0.4, list = FALSE)
  training <- movie_df[intrain,]
  testing <- movie_df[-intrain,]
  trctrl <- trainControl(method = "repeatedcv", number = 3, repeats = 2)
  set.seed(3333)
  dtree_fit <- train(Result ~., data = training, method = "rpart",
                     parms = list(split = "information"),
                     trControl=trctrl,
                     tuneLength = 10)
  dtree_fit
  prp(dtree_fit$finalModel, box.palette = "Reds", tweak = 1.2)
  testing[1,]
  predict(dtree_fit, newdata = testing[1,])
  test_pred <- predict(dtree_fit, newdata = testing)
  confusionMatrix(test_pred, testing$Result )
  
}

call_NaiveBayesian <- function(DataSetName){
  
  fileLoc <- paste("E:/MS COMPUTER SCIENCE/MS PROJECTS/Semester 1/Social Media Mining/", DataSetName, ".csv", sep = "")
  movie_df <- read.csv(fileLoc, sep = ',', header = TRUE)
  movie_df <- read.csv("E:/MS COMPUTER SCIENCE/MS PROJECTS/Semester 1/Social Media Mining/TrainingDataSet.csv", sep = ',', header = TRUE, stringsAsFactors = TRUE)
  set.seed(3033)
  intrain <- createDataPartition(y = movie_df$Result, p= 0.3, list = FALSE)
  training <- movie_df[intrain,]
  testing <- movie_df[-intrain,]
  mod <- naiveBayes(Result~., data = training)
  mod
  pr <- predict(mod,testing)
  print(confusionMatrix(pr,testing$Result,positive = "Yes",dnn = c("Prediction","True")))
  
}

plot_Graphs <- function(DataSetName){
  
  fileLoc <- paste("E:/MS COMPUTER SCIENCE/MS PROJECTS/Semester 1/Social Media Mining/", DataSetName, ".csv", sep = "")
  movie_df <- read.csv(fileLoc, sep = ',', header = TRUE)
  pairs.panels(movie_df[-1])
  
  movie_df %>%
    ggplot(aes(x=Result, y=PositivePercent, fill=Result))+
    geom_boxplot() + ggtitle("Box Plot")
  
  #movie_df %>%
  #  ggplot(aes(x=Result, y=NegativePercent, fill=Result))+
  #  geom_boxplot() + ggtitle("Box Plot")
  
  #movie_df %>%
  #  ggplot(aes(x=Result, y=NeutralPercent, fill=Result))+
  #  geom_boxplot() + ggtitle("Box Plot")
  
}
#CUSTOM METHODS

#CONNECTION TO TWITTER DEVELOPER ACCOUNT AND AUTHENTICATION
create_token(
  app = "R_text_miningraks",
  consumer_key = "Z7soBlhV2gyg5zRVnDyIG858U",
  consumer_secret = "qDv9sp2fYZ8ugmL1q2Aqx13E5fL6jlxQABH9ZBjH4UODkX0VOk")
#CONNECTION TO TWITTER DEVELOPER ACCOUNT AND AUTHENTICATION

#PERFORM SENTIMENT ANALYSIS AND GET TRAINING DATA SET
train_sentiment(c("#AStarIsBorn","#Bodied","#Creed2","#CrimesOfGrindelwald",
                  "#Oceans8","#Ratchasan","#RedSparrow","#Sarkar",
                  "#TheGirlInTheSpidersWeb","#TheNun","#ThePredator",
                  "#TimeFreak","#TombRaider","#UnderTheWire","#APrivateWar",
                  "#ASimpleFavor","#GreenBook","#GreenBookMovie","#HereAndNow",
                  "#InaRelationship","#Jonathan","#Kaala","#Sanju",
                  "#MaryQueenofScots","#Taxiwala","#GeethaGovindam",
                  "#KaatrinMozhi","#Viswasam","#firstman","#GameNight",
                  "#grinchmovie","#Incredibles2","#indivisiblemovie",
                  "#MonsterParty","#Overlord","#RalphBreaksTheInternet",
                  "#Rampage","#Rampant","#RobinHoodMovie","#Seemaraja",
                  "#Skyscraper","#Suspiria","#TheLastKey","#ThugsOfHindostan",
                  "#Venom","#Vishwaroopam2","#WidowsMovie","#12RoundGun",
                  "#55Steps","#8Remains","#Dhadak","#ThePossessionOfHannahGrace",
                  "#TheBountyKiller","#TheQuake","#ITChapter2","#96Movie",
                  "#alphamovie","#AntManAndTheWasp","#BlackPanther",
                  "#BohemianRhapsody","#CCV","#Deadpool2","#InstantFamily",
                  "#JurassicWorld","#KolamaavuKokila","#PadMan","#Padmavat",
                  "#PariyerumPerumal","#Raazi","#Race3","#ReadyPlayerOne",
                  "#RiverRunsRed","#SearchingMovie","#TheAngel","#TheFrontRunner",
                  "#TheHappyTimeMurders","#VadaChennai","#Weightless",
                  "#WrinkleInTime","#Shazam","#MortalEngines","#CaptainMarvel",
                  "#MowgliLegendOfTheJungle","#TheNewMutants","#Aquaman",
                  "#BloodBrother","#HeadFullOfHoney","#TheMercy","#BenIsBack",
                  "#OnceUponADeadpool","#SpiderManIntoTheSpiderVerse",
                  "#BumblebeeMovie","#SecondAct","#Zero","#2Point0","#Kedarnath",
                  "#Simmba","#SaamySquare","#Petta","#Sandakozhi2"))
#PERFORM SENTIMENT ANALYSIS AND GET TRAINING DATA SET

#GENERATE DECISION TREE
call_DecisionTree("TrainingDataSet")
#GENERATE DECITION TREE

#NAVIE BAYESIAN CLASSIFIER
call_NaiveBayesian("TrainingDataSet")
#NAIVE BAYESIAN CLASSIFIER

#PLOT RESULT GRAPH
plot_Graphs("TrainingDataSet")
#PLOT RESULT GRAPH