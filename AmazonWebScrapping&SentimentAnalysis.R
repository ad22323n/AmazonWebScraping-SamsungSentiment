#Parse Amazon html pages for data
amazon_scraper <- function(doc, reviewer = T, delay = 0){
  
  
  pacman::p_load_gh("trinker/SentimentAnalysis")
  pacman::p_load(RCurl, XML, dplyr, stringr, rvest, audio)
  
  sec = 0
  if(delay < 0) warning("delay was less than 0: set to 0")
  if(delay > 0) sec = max(0, delay + runif(1, -1, 1))
  
  #Remove all white space
  trim <- function (x) gsub("^\\s+|\\s+$", "", x)
  
  title <- doc %>%
    html_nodes("#cm_cr-review_list .a-color-base") %>%
    html_text()
  
  author <- doc %>%
    html_nodes(".review-byline .author") %>%
    html_text()
  
  date <- doc %>%
    html_nodes("#cm_cr-review_list .review-date") %>%
    html_text() %>% 
    gsub(".*on ", "", .)
  
  ver.purchase <- doc%>%
    html_nodes(".review-data.a-spacing-mini") %>%
    html_text() %>%
    grepl("Verified Purchase", .) %>%
    as.numeric()
  
  format <- doc %>% 
    html_nodes(".review-data.a-spacing-mini") %>% 
    html_text() %>%
    gsub("Color: |\\|.*|Verified.*", "", .)
  #if(length(format) == 0) format <- NA
  
  stars <- doc %>%
    html_nodes("#cm_cr-review_list  .review-rating") %>%
    html_text() %>%
    str_extract("\\d") %>%
    as.numeric()
  
  comments <- doc %>%
    html_nodes("#cm_cr-review_list .review-text") %>%
    html_text() 
  
  helpful <- doc %>%
    html_nodes(".cr-vote-buttons .a-color-secondary") %>%
    html_text() %>%
    str_extract("[:digit:]+|One") %>%
    gsub("One", "1", .) %>%
    as.numeric()
  
  if(reviewer == T){
    
    rver_url <- doc %>%
      html_nodes(".review-byline .author") %>%
      html_attr("href") %>%
      gsub("/ref=cm_cr_othr_d_pdp\\?ie=UTF8", "", .) %>%
      gsub("/gp/pdp/profile/", "", .) %>%
      paste0("https://www.amazon.com/gp/cdp/member-reviews/",.) 
    
    #average rating of past 10 reviews
    rver_avgrating_10 <- rver_url %>%
      sapply(., function(x) {
        read_html(x) %>%
          html_nodes(".small span img") %>%
          html_attr("title") %>%
          gsub("out of.*|stars", "", .) %>%
          as.numeric() %>%
          mean(na.rm = T)
      }) %>% as.numeric()
    
    rver_prof <- rver_url %>%
      sapply(., function(x) 
        read_html(x) %>%
          html_nodes("div.small, td td td .tiny") %>%
          html_text()
      )
    
    rver_numrev <- rver_prof %>%
      lapply(., function(x)
        gsub("\n  Customer Reviews: |\n", "", x[1])
      ) %>% as.numeric()
    
    rver_numhelpful <- rver_prof %>%
      lapply(., function(x)
        gsub(".*Helpful Votes:|\n", "", x[2]) %>%
          trim()
      ) %>% as.numeric()
    
    rver_rank <- rver_prof %>%
      lapply(., function(x)
        gsub(".*Top Reviewer Ranking:|Helpful Votes:.*|\n", "", x[2]) %>%
          removePunctuation() %>%
          trim()
      ) %>% as.numeric()
    
    df <- data.frame(title, date, ver.purchase, format, stars, comments, helpful,
                     rver_url, rver_avgrating_10, rver_numrev, rver_numhelpful, rver_rank, stringsAsFactors = F)
    
  } else df <- data.frame(title, author, date, ver.purchase, format, stars, comments, helpful, stringsAsFactors = F)
  
  return(df)
}


#Remove all white space
trim <- function (x) gsub("^\\s+|\\s+$", "", x)
prod_code = "B01M7O431L"
url <- paste0("https://www.amazon.com/dp/", prod_code)
doc <- read_html(url)
#obtain the text in the node, remove "\n" from the text, and remove white space
prod <- html_nodes(doc, "#productTitle") %>% html_text() %>% gsub("\n", "", .) %>% trim()
prod
#Source funtion to Parse Amazon html pages for data
#source("https://raw.githubusercontent.com/rjsaito/Just-R-Things/master/Text%20Mining/amazonscraper.R")
pages <- 50
reviews_all <- NULL
for(page_num in 1:pages){
  url <- paste0("http://www.amazon.com/product-reviews/",prod_code,"/?pageNumber=", page_num)
  doc <- read_html(url)
  
  reviews <- amazon_scraper(doc, reviewer = F, delay = 2)
  reviews_all <- rbind(reviews_all, cbind(prod, reviews))
}

View(reviews_all)

write.csv(reviews_all, "myreviews.csv", row.names = FALSE)

qplot(data=reviews_all,stars, fill=format)


#+++++++++++++++++++++++++++++++++++++++++++++++++++++++ANALYSIS++++++++++++++++++++++++++++++++++++++++++++


library(SnowballC) #stop world
library(syuzhet) #sentiment analysis
library(wordcloud) #visual analysis graph
library(sentimentr)
require(tm)# for transformation Corpus etc..
require(dplyr) #data manipulation "wrangling
require(SentimentAnalysis)
require(stringr) #deals with NA and 0 length vectors



source("https://bioconductor.org/biocLite.R")
biocLite()
biocLite("Rgraphviz")

qplot(mean(SamSungData$stars),xlab="Mean (3.823529)")
#create copus and cleaninng data.

corp <- Corpus(VectorSource(SamSungData$comments))
corp<-tm_map(corp, removeWords, stopwords('english'))
corp<-tm_map(corp, stemDocument)
class(corp)
View(corp)

#calculate how many times each term(col) appears in the document(row) each row will represent a doc and every col
#is a term and each cell tell as how many time show in the doc
mIdata<-DocumentTermMatrix(corp)
inspect(mIdata)
freq<-colSums(as.matrix(mIdata))
# let's review the most frequent and dominant words used in the comments 
freq[1:40]
freqSort<-freq[order(-freq[1:40])]
#View(as.matrix(mIdata))
head(freqSort)
tail(freqSort, 6) #least frequent words in the doc
length(freq)

findFreqTerms(mIdata, lowfreq = 25)# find the terms that appear 25 or more times in all doc
findFreqTerms(mIdata, 10) #most freq term in each doc

#find the terms associated with some words
findAssocs(mIdata, c( "amazing", "money", "bad", "battery","good","hate","sucks",
                      "never","useful" ), corlimit = 0.70)
View(mIdata)
#TErm that appears at least No times with correlation >= 0. How fick the line is proportionate to the correaltion
# if the term appears at least 20 time it will be leasted in a triangle
plot(mIdata, terms=findFreqTerms(mIdata, lowfreq =23), corThreshold=0.5, weighting=T)

mean(SamSungData$stars)
#plot(mIdata, terms=findFreqTerms(mIdata, lowfreq = 20), corThreshold=0.3, wwighting=T)

myPlot<-data.frame(word=names(freq), freq=freq)
p<-ggplot(subset(myPlot, freq>20), aes(word,freq))
p<-p+geom_bar(stat = "identity")
p<- p+ ggtitle("Word Freq>20")
p

set.seed(123)
wordcloud(names(freq), freq, min.freq = 8)
No0<-removeSparseTerms(mIdata, 0.2)
freq1<-colSums(as.matrix(mIdata))
dark2<-brewer.pal(8,"Spectral")
wordcloud(names(freq1), freq1, min.freq = 3, rot.per = 0.2, colors =dark2,
          random.order = FALSE, width=100, height=100)




#Remaking corpus for sComment as a single dataset(document)
sComments<-SamSungData$comments

sComments<-Corpus(VectorSource(sComments))
sComments<-tm_map(sComments, removePunctuation)
myIcomments<-tm_map(sComments, stripWhitespace)
sComments<-tm_map(sComments, removeNumbers)

removeEmail<-function(x) gsub("http[^[:space:]]*", "", x)
sComments<-tm_map(sComments, content_transformer(removeEmail))

View(sComments)



sComments<-as.character(sComments) #convert it in order to get sentiment
#you get all text in a big file instead of 1 .2.3....the last text

get_nrc_sentiment(sComments)
mySentiment<-get_nrc_sentiment(sComments)
mySentimentScore<-data.frame(colSums(mySentiment[,])) #sentiment of all rows
names(mySentimentScore)<-"score"

mySentimentScore<-cbind("sentiment" = rownames(mySentimentScore), mySentimentScore)
View(mySentimentScore)

hist(mySentimentScore$score)

View(sComments)

rownames(mySentimentScore)<-NULL
ggplot(data=mySentimentScore, aes(x=sentiment, y=score))+
  geom_bar(aes(fill=sentiment), stat="identity")
+ggtitle("iSentiment Score according Amazon")

prop.table(mySentimentScore$score)
#write.xlsx(analysis, "sentiment.xlsx")
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#sentiment of some word
#Lets perform sentiments anlysis o samsungData
#build models with matrix Midata
myMatrix<-as.matrix

library(data.table) #Wrangling
set.seed(123)
x<-get_sentences(paste0(sComments[[1]]),1000, TRUE)
sentiment(x)

View(sComments)
pol_wor<-extract_sentiment_terms(x)
pol_wor
pol_wor$sentence
pol_wor$neutral
#pol_wor$positive

data.table::as.data.table(pol_wor)

attributes(extract_sentiment_terms(x))$counts
attributes(extract_sentiment_terms(x))$elements

#rm(x)
get_sentences(x)
get_sentences(x, as_vector = FALSE)

#___________________+++++++++++++++++++++++++++____________+++++___________
#create 70%/30% stratified split
myDataMod<-as.data.frame(SamSungData)
myDataMod$comments<-as.factor(myDataMod$comments)

sum(is.na(myDataMod))
myDataMod<-na.omit(myDataMod)

set.seed(2018)
inTrain<-createDataPartition(myDataMod$comments, times = 1, p=0.7, list = FALSE)
Train<-myDataMod[inTrain,]
Test<-myDataMod[-inTrain,]

#preprocess and tokenize data ,stemming on the tokens and cleaning 
library(quanteda)


Train.token<-tokens(as.character(Train$comments), what= "word", remove_numbers=TRUE, remove_punc=TRUE,
                    remove_symbols=TRUE, remove_hyphens=TRUE)

Train.token<-tokens_tolower(Train.token)
Train.token<-tokens_select(Train.token, stopwords(),
                           selection = "remove")

#create firt bag data & transform into matrix
Train.token.df<-dfm(Train.token, tolower = FALSE, remove = stopwords())
Train.token.matrix<-as.matrix(Train.token.df)

#firs 20 rown an dfirst 100 col
View(Train.token.matrix[1:20, 1:100])
dim(Train.token.matrix) #68 937
colnames(Train.token.matrix)[1:50]

#setup data frame with label
Train.token.dff<-cbind(Label=Train$comments, as.data.frame(Train.token.df))

#10 fold cross validation
#Data partition with a lot of stratified data folder
set.seed(123)
myfolder<-createMultiFolds(Train$comments, k=10, times=3)
myfolder_ctrl<-trainControl(method = "repeatedcv", number=10,
                            repeats = 3, index = myfolder)

library(doSNOW) #allow speed up cross validatiion and traning

#create a cluster to work on 10 logical cores
myCluster<-makeCluster(3, type = "SOCK") #create doSnow cluster with "socker" cluster in order to create * instance and mke it available to caret
registerDoSNOW(myCluster) #allow caret to use myCluster 

#x<-as.vector(Train.token.dff$Label)

myrPart<-train(as.integer(Label)~., data= Train.token.dff, method="rpart",
               trControl=myfolder_ctrl, tuneLength=7, silent=TRUE, na.rm=TRUE) #Not working yet



























