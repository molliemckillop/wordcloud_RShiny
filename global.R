########## Set WD ##########
#setwd("~/Documents/DataVisualization/HW1_Submission/shinyT")

########## Install Packages ##########
# install.packages("SnowballC")
# install.packages("streamR")
# install.packages("shiny")
# install.packages("wordcloud")
# install.packages("tm")
# install.packages("rjson")
# install.packages("ROAuth")
# install.packages("memoise")

########## Open Packages ##########
library(rjson)
library(SnowballC)
library(shiny)
library(streamR)
library(bitops)
library(RCurl)
library(rjson)
library(ROAuth)
library(tm)
library(wordcloud)
library(memoise)

########## Download Tweets - Using Twitter API ##########

# requestURL <- "https://api.twitter.com/oauth/request_token"
# accessURL <- "https://api.twitter.com/oauth/access_token"
# authURL <- "https://api.twitter.com/oauth/authorize"
# consumerKey <- "PaWhYmU2CeF9YVEDbJnGnfBpG"
# consumerSecret <- "0031VgEJhYdSjXLki3vj3wZHr1s7KcP4JIZF43XENJulUHRHTQ"
# my_oauth <- OAuthFactory$new(consumerKey = consumerKey, consumerSecret = consumerSecret, 
#                              requestURL = requestURL, accessURL = accessURL, authURL = authURL)
# my_oauth$handshake(cainfo = system.file("CurlSSL", "cacert.pem", package = "RCurl"))
# 
# save(my_oauth, file = "my_oauth.Rdata")
# load("my_oauth")

# determining keywords 
# filterStream("tweets_full.json", track = c("Hillary Clinton", "Bernie Sanders", "Jeb Bush", "Marco Rubio", "Ted Cruz", "Donald Trump",
#                                       "John kasich", "Ben Carson", "Chris Christie", "Carly Fiorina", "Hillaryclinton", "Jebbush",
#                                       "Berniesanders", "Marcorubio", "Chrischristie", "Bencarson", "Johnkasich", "Donaldtrump",
#                                       "Tedcruz", "CarlyFiorina"), timeout = 3600, 
#              oauth = my_oauth)


########## Processing collected tweets ##########

## using the collected json file, creates different data frames for each candidate 


# loading the data from Json file
tweets_full.df <- read.csv("merge_new.csv", header=T)
tweets_full.df$text <- sapply(tweets_full.df$text,function(row) iconv(row, "latin1", "ASCII", sub=""))


#tweets_full.df$created_at <- as.Date(tweets_full.df$created_at, format="%a %b %d %H:%M:%S %z %Y")

#tweets_full.df$created_at <- as.POSIXct(tweets_full.df$created_at, format="%a %b %d %H:%M:%S %z %Y")

candidate.terms <- c("Hillary Clinton", "Hillary", "Clinton", "Bernie Sanders", "Bernie", "Sanders", 
                     "Ted Cruz", "Ted", "Cruz","Donald Trump", "Donald", "Trump",
                      "Hillaryclinton","Berniesanders", "Donaldtrump","Tedcruz" )

# creating dataframe for each candidate
for (i in candidate.terms) {
  dataset <- subset(tweets_full.df, grepl(i,tweets_full.df$text, ignore.case = TRUE))
  i_new <- gsub(" ", "_", i, fixed = TRUE)
  assign(paste(i_new,".df",sep=""), dataset)
}

# appending the candidate dataframes
bs <- unique(rbind(Bernie_Sanders.df, Berniesanders.df, Bernie.df, Sanders.df))
dt <- unique(rbind(Donald_Trump.df, Donaldtrump.df, Donald.df, Trump.df))
hc <- unique(rbind(Hillary_Clinton.df,Hillaryclinton.df, Hillary.df, Clinton.df))
tc <- unique(rbind(Ted_Cruz.df, Tedcruz.df, Ted.df, Cruz.df))


# list of dataframes
cand <- list(bs,hc,dt,tc)
cand_names <- c("bs","hc","dt","tc")

# creating TweetCorpus by candidate 
# outputs a corpus with candidates' initials (corpus_dt is donald trumps')
for (i in 1:length(cand_names)) {
  dataframe <- data.frame(cand[i])
  tweets_full.text<-paste(unlist(dataframe$text), collapse =" ")
  #print("1")
  
  
  # Convert all text to lower case
  tweets_full.text <- tolower(tweets_full.text)
  
  # Replace blank space (“rt”)
  tweets_full.text <- gsub("rt", "", tweets_full.text)
  
  # Replace @UserName
  tweets_full.text <- gsub("@\\w+", "", tweets_full.text)
  
  # Remove punctuation
  tweets_full.text <- gsub("[[:punct:]]", "", tweets_full.text)
  
  # Remove links
  tweets_full.text <- gsub("http\\w+", "", tweets_full.text)
  
  # Remove tabs
  tweets_full.text <- gsub("[ |\t]{2,}", "", tweets_full.text)
  
  # Remove blank spaces at the beginning
  tweets_full.text <- gsub("^ ", "", tweets_full.text)
  
  # Remove blank spaces at the end
  tweets_full.text <- gsub(" $", "", tweets_full.text)
  
  tweets_full.text <- Corpus(VectorSource(tweets_full.text))
  
  # Clean up by removing stop words
  tweets_full.text <- tm_map(tweets_full.text, function(x)removeWords(x,stopwords()))
  tweets_full.text <- tm_map(tweets_full.text, removeWords, c(stopwords('english'), '&amp'))
  
  # Remove numbers 
  tweets_full.text <- tm_map(tweets_full.text, removeNumbers)
  
  # Rename the corpora for each candidate (corpora_bs = bernie sanders' corpora)
  i_new <- gsub(" ", "_", cand_names[i], fixed = TRUE)
  assign(paste("corpus_",i_new,sep=""), tweets_full.text)
}
# wordcloud(corpus_tc, max.words = 100, random.order = FALSE)
# wordcloud(corpus_tc,min.freq = 2, scale=c(7,0.5),colors=brewer.pal(8, "Dark2"),  random.color= TRUE, random.order = FALSE, max.words = 150)



########## Create corpus for each candidate ##########

# List of candidates
candidates <<- list("Bernie Sanders"="bs", "Donald Trump" = "dt", "Hillary Clinton" = "hc","Ted Cruz"="tc")


# Using "memoise" to automatically cache the results
getTermMatrix <- (function(candidate) {
  # Careful not to let just any name slip in here; a
  # malicious user could manipulate this value.
  if (!(candidate %in% candidates))
    stop("Unknown candidate")
    print(candidate)


  if (candidate=="bc") { myDTM = TermDocumentMatrix(corpus_bc,
                                                    control = list(minWordLength = 1))}
  else if (candidate=="bs") { myDTM = TermDocumentMatrix(corpus_bs,
                                                  control = list(minWordLength = 1))}
  else if (candidate=="dt") { myDTM = TermDocumentMatrix(corpus_dt,
                                                       control = list(minWordLength = 1))}
  else if (candidate=="hc") { myDTM = TermDocumentMatrix(corpus_hc,
                                                       control = list(minWordLength = 1))}
  else if (candidate=="tc") { myDTM = TermDocumentMatrix(corpus_tc,
                                                       control = list(minWordLength = 1))}
  

  
  m = as.matrix(myDTM)
  
  sort(rowSums(m), decreasing = TRUE)
})

getCandDf <- (function(candidate) {
  if (!(candidate %in% candidates))
    stop("Unknown candidate")
      if (candidate=="bc") { candDF<-bc }
      else if (candidate=="bs")  { candDF<-bs }
      else if (candidate=="dt")  { candDF<-dt }
      else if (candidate=="hc")  { candDF<-hc }
      else if (candidate=="tc")  { candDF<- tc }
  
      return(candDF)
})
