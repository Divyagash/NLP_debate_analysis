library(rvest)
library(stringr)
library(plyr)
library(ggmap)
library(dplyr)
library(plyr)
library(tm)
library(wordcloud)
library(datasets)
library(maps)
library(countrycode)
library(psData)

# Read the link
transcript_link <- read_html("https://www.debates.org/voter-education/debate-transcripts/september-26-2016-debate-transcript/")

# I use a browser add in (Selector Gadget) to extract out the relevant html tag for the transcript 
# the tag is titled "#main-content" - I then extract out the content using the following functions from the rvest package
transcript <- transcript_link %>% rvest::html_nodes("#content-sm") %>% rvest::html_text()

# We have 3 different patterns to search for - which will include in our expression using the or operator '|'
# str_locate_all will give the index (start and end position) of all the matches
markers <- str_locate_all(transcript, pattern = "CLINTON|TRUMP|HOLT")

# This returns a list with one component - we extract out that component
markers <- markers[[1]]

# Now markers is a matrix indicating the start and end positions
# We are only interested in the start positions, so we extract that out
markers <- markers[,1]

# Initialize a vector to store the results
res <- vector(mode = "character", length = length(markers) - 1)
for (i in 1:(length(markers)-1)) {
  res[i] <- substr(transcript,markers[i],markers[i+1]-1)
  
}

# Get the parts for Clinton and Trump
clinton <- res[sapply(res,function(x) grepl("CLINTON",x))]
trump <- res[sapply(res,function(x) grepl("TRUMP",x))]

# Loop thru each element of our vector
# Use regular expr. to match the ellipsis pattern
ClintonInterruption <- sum(sapply(trump,function(x) grepl("[.]{3}",x)))
TrumpInterruption <- sum(sapply(clinton,function(x) grepl("[.]{3}",x)))


tot_words_clinton <- unlist(sapply(clinton, function(x) str_split(x, " ")))
tot_words_trump <- unlist(sapply(trump, function(x) str_split(x, " ")))

tot_words_clinton <- removePunctuation(tot_words_clinton, preserve_intra_word_dashes = FALSE)
tot_words_trump <- removePunctuation(tot_words_trump, preserve_intra_word_dashes = FALSE)

# This also returns some blank values which we exclude
tot_words_clinton <- tot_words_clinton[tot_words_clinton != ""]
length(tot_words_clinton)
tot_words_trump <- tot_words_trump[tot_words_trump != ""]
length(tot_words_trump)
#removePunctuation(tot_words_clinton[4], preserve_intra_word_dashes = FALSE)



# Extract the list of common key words
stop_words <- stopwords("SMART")

# Remove these words from our "bag-of-words" list
tot_words_clinton <- tot_words_clinton[!(tot_words_clinton %in% stop_words)]
tot_words_trump <- tot_words_trump[!(tot_words_trump %in% stop_words)]

# We use the count function from the plyr package on the "tot_words_clinton" variable which has the "bag-of-words" 
# We then use the dplyr function arrange to sort the words as per descending frequency
word_freq <- plyr::count(tot_words_clinton) %>% arrange(desc(freq))
word_freq_trump <- plyr::count(tot_words_trump) %>% arrange(desc(freq))

# state names by the candidates
clintonStateName <- state.name[state.name %in% word_freq$x]
trumpStateName <- state.name[state.name %in% word_freq_trump$x]
matchedRowClinton <- match(clintonStateName,word_freq$x)
matchedRowTrump <- match(trumpStateName,word_freq_trump$x)
ClintonStatefreq <- word_freq$freq[matchedRowClinton]
TrumpStatefreq <- word_freq_trump$freq[matchedRowTrump]
StateFreqClinton <- as.data.frame(cbind(state.name))
StateFreqClinton$freq <- NA
StateFreqClinton$freq[StateFreqClinton$state.name == clintonStateName] <- ClintonStatefreq
colnames(StateFreqClinton) <- c("region","freq")

StateFreqTrump <- as.data.frame(cbind(state.name))
StateFreqTrump$freq <- NA
StateFreqTrump$freq[match(trumpStateName,StateFreqTrump$state.name)] <- TrumpStatefreq
colnames(StateFreqTrump) <- c("region","freq")


all_states <- map_data("state")
all_states
head(all_states)


StateFreqClinton$region <- tolower(StateFreqClinton$region)
Total <- merge(all_states, StateFreqClinton, by = "region")
head(Total)
Total <- Total[Total$region != "district of columbia", ]






p <- ggplot()
p <- p + geom_polygon(data=Total, aes(x = long, y = lat,group = group, fill = Total$freq), colour="blue") + 
      scale_fill_continuous(low = "thistle2", high = "darkred", guide = "colorbar")
P1 <- p + theme_bw() + labs(fill = "Frequency", title = "States mentioned by Clinton during the debate", x="", y="")
P1
pdf("/Users/Divy/Documents/ClintonStates.pdf", width=9, height=6)
P1

dev.off()

StateFreqTrump$region <- tolower(StateFreqTrump$region)
TotalTrump <- merge(all_states, StateFreqTrump, by = "region")
head(TotalTrump)
TotalTrump <- TotalTrump[TotalTrump$region != "district of columbia", ]


p2 <- ggplot()
p2 <- p2 + geom_polygon(data = TotalTrump, aes(x = long, y = lat, group = group, fill = TotalTrump$freq), colour = "blue") + 
      scale_fill_continuous(low = "thistle2", high = "darkred", guide = "colorbar")
P2 <- p2 + theme_bw()  + labs(fill = "Frequency" , 
                              title = "States mentioned by Trump during the first presidential debate", x="", y="")
pdf("/Users/Divy/Documents/TrumpStates.pdf", width=9, height=6)
P2

dev.off()



# Country names uttered by them
CountryList <- countrycode_data$country.name
CountryList <- c(CountryList, "Russia")
clintonCountryName <- CountryList[CountryList %in% word_freq$x]
trumpCountryName <- CountryList[CountryList %in% word_freq_trump$x]
matchedRowClintonCountry <- match(clintonCountryName, word_freq$x)
matchedRowTrumpCountry <- match(trumpCountryName, word_freq_trump$x)
ClintonCountryfreq <- word_freq$freq[matchedRowClintonCountry]
TrumpCountryfreq <- word_freq_trump$freq[matchedRowTrumpCountry]
CountryFreqClinton <- as.data.frame(CountryList)
CountryFreqClinton$freq <- NA
CountryFreqClinton$freq[match(clintonCountryName, CountryList)] <- ClintonCountryfreq
colnames(CountryFreqClinton) <- c("region","freq")
CountryFreqClinton$freq[CountryFreqClinton$region == "Jersey"] <- NA
CountryFreqClinton$freq[CountryFreqClinton$region == "Russian Federation"] <- CountryFreqClinton$freq[CountryFreqClinton$region == "Russia"]


CountryFreqTrump <- as.data.frame(CountryList)
CountryFreqTrump$freq <- NA
CountryFreqTrump$freq[match(trumpCountryName,CountryList)] <- TrumpCountryfreq
colnames(CountryFreqTrump) <- c("region", "freq")
CountryFreqTrump$freq[CountryFreqTrump$region == "Russian Federation"] <- CountryFreqTrump$freq[CountryFreqTrump$region == "Russia"]
CountryFreqTrump$freq[CountryFreqTrump$region=="Federal Republic of Germany"] <- CountryFreqTrump$freq[CountryFreqTrump$region == "Germany"]


##########################
##########################


library(rworldmap)

#create a map-shaped window
mapDevice('x11')
#join to a coarse resolution map
colnames(CountryFreqClinton)<-c("region","Frequency of countries mentioned by Clinton")
spdf <- joinCountryData2Map(CountryFreqClinton, joinCode="NAME", nameJoinColumn="region")

clintonCountry <- mapCountryData(spdf, nameColumnToPlot="Frequency of countries mentioned by Clinton", catMethod="fixedWidth",colourPalette ="negpos8")

#create a map-shaped window
mapDevice('x11')
#join to a coarse resolution map
colnames(CountryFreqTrump)<-c("region","Frequency of countries mentioned by Trump")
spdf_Trump <- joinCountryData2Map(CountryFreqTrump, joinCode="NAME", nameJoinColumn="region")

trumpCountry <- mapCountryData(spdf_Trump, nameColumnToPlot="Frequency of countries mentioned by Trump", catMethod="fixedWidth",colourPalette ="negpos8")



###########################
###########################





# Parts of Speach tagging
# Sentiment Analysis
# classify emotion
library(syuzhet)
library(sentiment)
clinton_nrc<-colSums(get_nrc_sentiment(clinton))
trump_nrc<-colSums(get_nrc_sentiment(trump))

# Plot the sentiment from each candidate
clinton_nrc_Df <- as.data.frame(cbind(names(clinton_nrc),clinton_nrc))

trump_nrc_Df <- as.data.frame(cbind(names(trump_nrc),trump_nrc))
colnames(clinton_nrc_Df) <- c("emotion","score")
colnames(trump_nrc_Df) <- c("emotion","score")
clinton_nrc_Df$score <- as.numeric(levels(clinton_nrc_Df$score)[clinton_nrc_Df$score])
trump_nrc_Df$score <- as.numeric(levels(trump_nrc_Df$score)[trump_nrc_Df$score])


counts <- table(mtcars$gear)
#barplot(unlist(clinton_nrc), main="Car Distribution", horiz=TRUE, names.arg=clinton_nrc_Df$emotion, cex.names=0.5)

#ggplot(clinton_nrc_Df)+stat_count(data=clinton_nrc_Df)


ggplot(clinton_nrc_Df, aes(factor(clinton_nrc_Df$emotion),y = clinton_nrc_Df$score))+geom_bar(stat="identity",position="dodge",fill="blue")+ylab("score")+xlab("emotions")+
  theme(axis.text.x = element_text(angle = 90, hjust = 1))+ggtitle("Emotions expressed by Clinton based on \n her transcript")


ggplot(trump_nrc_Df, aes(factor(trump_nrc_Df$emotion),y = trump_nrc_Df$score))+geom_bar(stat="identity",position="dodge",fill="red")+ylab("score")+xlab("emotions")+
  theme(axis.text.x = element_text(angle = 90, hjust = 1))+ggtitle("Emotions expressed by Trump based on \n his transcript")


class_emo = classify_emotion(clinton, algorithm="bayes", prior=1.0)
# get emotion best fit
emotion = class_emo[,7]
# substitute NA's by "unknown"
emotion[is.na(emotion)] = "unknown"

# classify polarity
class_pol = classify_polarity(some_txt, algorithm="bayes")
# get polarity best fit
polarity = class_pol[,4]


# Word Cloud
word_freq_WC = plyr::count(tot_words_clinton) %>% arrange(desc(freq))
word_freq_trump_WC = plyr::count(tot_words_trump) %>% arrange(desc(freq))


# Clinton wordcloud
word_freq_WC$x<-tolower(word_freq_WC$x)
Words <- word_freq_WC$x[!(word_freq_WC$x %in% stop_words)]
word_freq_WC<-word_freq_WC[match(Words,word_freq_WC$x),]


word_freq_WC$x<-removePunctuation(word_freq_WC$x)
Words <- word_freq_WC$x[!(word_freq_WC$x %in% stop_words)]
word_freq_WC<-word_freq_WC[match(Words,word_freq_WC$x),]


distinct_df = word_freq_WC %>% distinct(x)
word_freq_WC<-distinct_df[-c(1,2),]

pal2 <- brewer.pal(8,"Dark2")
png("wordcloud_packages_clinton.png", width=1280,height=800)
wordcloud(word_freq_WC$x,word_freq_WC$freq, scale=c(8,.2),min.freq=5,
          max.words=Inf, random.order=FALSE, rot.per=.15, colors=pal2)

dev.off()

# Trump wordcloud
word_freq_trump_WC$x<-tolower(word_freq_trump_WC$x)
Words <- word_freq_trump_WC$x[!(word_freq_trump_WC$x %in% stop_words)]
word_freq_trump_WC<-word_freq_trump_WC[match(Words,word_freq_trump_WC$x),]


word_freq_trump_WC$x<-removePunctuation(word_freq_trump_WC$x)
Words <- word_freq_trump_WC$x[!(word_freq_trump_WC$x %in% stop_words)]
word_freq_trump_WC<-word_freq_trump_WC[match(Words,word_freq_trump_WC$x),]


distinct_df = word_freq_trump_WC %>% distinct(x)
word_freq_trump_WC<-distinct_df[-c(1,2),]

pal2 <- brewer.pal(8,"Dark2")
png("wordcloud_packages_trump.png", width=1280,height=800)
wordcloud(word_freq_trump_WC$x,word_freq_trump_WC$freq, scale=c(8,.2),min.freq=5,
          max.words=Inf, random.order=FALSE, rot.per=.15, colors=pal2)

dev.off()



##################################
##########################
##################

Words <- word_freq_WC$x[!(word_freq_WC$x %in% stop_words)]
corpus=Corpus(VectorSource(Words))
corpus=tm_map(corpus,PlainTextDocument)

col=brewer.pal(6,"Dark2")
wordcloud(corpus, min.freq=25, scale=c(5,2),rot.per = 0.25,
          random.color=T, max.word=45, random.order=F,colors=col)

#Words <- word_freq_trump_WC$x[!(word_freq_trump_WC$x %in% stop_words)]

#

pal2 <- brewer.pal(8,"Dark2")
wordcloud(word_freq_trump_WC$x,word_freq_trump_WC$freq, scale=c(8,.2),min.freq=10,
          max.words=Inf, random.order=FALSE, rot.per=.15, colors=pal2)


corpus=Corpus(VectorSource(word_freq_trump_WC))
corpus=tm_map(corpus,PlainTextDocument)
corpus=tm_map(corpus,removePunctuation)
corpus <- tm_map(corpus, tolower)
corpus <- tm_map(corpus, function(x) removeWords(x, stopwords("english")))
corpus <- tm_map(corpus, PlainTextDocument)

tdm <- TermDocumentMatrix(corpus)
ap.m <- as.matrix(tdm)
ap.v <- sort(rowSums(ap.m),decreasing=TRUE)
ap.d <- data.frame(word = names(ap.v),freq=ap.v)
table(ap.d$freq)
pal2 <- brewer.pal(8,"Dark2")
wordcloud(word_freq_WC$x,word_freq_WC$freq, scale=c(8,.2),min.freq=10,
          max.words=Inf, random.order=FALSE, rot.per=.15, colors=pal2)

png("wordcloud_packages.png", width=1280,height=800)

dev.off()


col=brewer.pal(6,"Dark2")
wordcloud(corpus, min.freq=55, scale=c(5,2),rot.per = 0.15,
          random.color=T, max.word=45, random.order=F,colors=col)

word_freq_trump <- tm_map(word_freq_trump, tolower)
Words <- word_freq_trump$x[!(word_freq_trump$x %in% stop_words)]

pal2 <- brewer.pal(8,"Dark2")

wordcloud(corpus, scale=c(8,.2),min.freq=3,
          max.words=Inf, random.order=FALSE, rot.per=.15, colors=pal2)

dtm <- DocumentTermMatrix(corpus)
inspect(dtm)


# Part of speech tagging


# Sentiment Analysis