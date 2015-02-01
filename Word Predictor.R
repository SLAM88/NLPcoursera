library(tm)
library(RWeka)
library(fastmatch)

news <- readLines("en_US.news.txt")
blogs <- readLines("en_US.blogs.txt")
twitter <- readLines("en_US.twitter.txt")

raw <- c(news,blogs,twitter)
rm(news,blogs,twitter)
raw <- iconv(raw,"UTF-8","ASCII")
raw <- removePunctuation(raw)
raw <- removeNumbers(raw)
raw <- stripWhitespace(raw)
raw <- tolower(raw)


UnigramTokenizer <- function(x) NGramTokenizer(x, Weka_control(min = 1, max = 1))

table <- as.data.frame(table(matrix(UnigramTokenizer(raw))), stringsAsFactors = FALSE)
WORDS <- array(table[,1])

UNIGRAM <- WORDS
UNIGRAM <- cbind(UNIGRAM,table$Freq)
colnames(UNIGRAM) <- c("ID", "Count")
head(UNIGRAM)
UNIGRAM <- UNIGRAM[complete.cases(UNIGRAM),]

rm(table)


BigramTokenizer <- function(x) NGramTokenizer(x, Weka_control(min = 2, max = 2))
test2 <- as.data.frame(table(BigramTokenizer(raw)), stringsAsFactors = FALSE)
tester <- apply(as.matrix(unlist(strsplit(test2[,1], " "))),1,function(x) fmatch(x, WORDS))

BIGRAM <- matrix(tester, nrow = nrow(test2), ncol = 2, byrow = TRUE)
BIGRAM <- cbind(BIGRAM,test2$Freq)
colnames(BIGRAM) <- c("ID1", "ID2", "Count")
BIGRAM <- BIGRAM[complete.cases(BIGRAM),]

rm(test2, tester)


TrigramTokenizer <- function(x) NGramTokenizer(x, Weka_control(min = 3, max = 3))
test3 <- as.data.frame(table(TrigramTokenizer(raw)), stringsAsFactors = FALSE)
tester <- apply(as.matrix(unlist(strsplit(test3[,1], " "))),1,function(x) fmatch(x, WORDS))

TRIGRAM <- matrix(tester, nrow = nrow(test3), ncol = 3, byrow = TRUE)
TRIGRAM <- cbind(TRIGRAM,test3$Freq)
colnames(TRIGRAM) <- c("ID1", "ID2", "ID3", "Count")

TRIGRAM <- TRIGRAM[complete.cases(TRIGRAM),]

rm(test3, tester)


QuadgramTokenizer <- function(x) NGramTokenizer(x, Weka_control(min = 4, max = 4))
test4 <- as.data.frame(table(QuadgramTokenizer(raw)), stringsAsFactors = FALSE)
tester <- apply(as.matrix(unlist(strsplit(test4[,1], " "))),1,function(x) fmatch(x, WORDS))

QUADGRAM <- matrix(tester, nrow = nrow(test4), ncol = 4, byrow = TRUE)
QUADGRAM <- cbind(QUADGRAM,test4$Freq)
colnames(QUADGRAM) <- c("ID1", "ID2", "ID3", "ID4", "Count")

QUADGRAM <- QUADGRAM[complete.cases(QUADGRAM),]

rm(test4, tester)


PentagramTokenizer <- function(x) NGramTokenizer(x, Weka_control(min = 5, max = 5))
test5 <- as.data.frame(table(PentagramTokenizer(raw)), stringsAsFactors = FALSE)
tester <- apply(as.matrix(unlist(strsplit(test5[,1], " "))),1,function(x) fmatch(x, WORDS))

PENTAGRAM <- matrix(tester, nrow = nrow(test5), ncol = 5, byrow = TRUE)
PENTAGRAM <- cbind(PENTAGRAM,test5$Freq)
colnames(PENTAGRAM) <- c("ID1", "ID2", "ID3", "ID4", "ID5", "Count")

PENTAGRAM <- PENTAGRAM[complete.cases(PENTAGRAM),]


w <- fmatch("he", WORDS)
x <- fmatch("wasnt", WORDS)
y <- fmatch("home", WORDS)
z <- fmatch("alone", WORDS)

PentaPredictor <- function(a,b,c,d) {

w <- fmatch(a, WORDS)
x <- fmatch(b, WORDS)
y <- fmatch(c, WORDS)
z <- fmatch(d, WORDS)

#PENTAGRAM
herewego <- subset(PENTAGRAM, PENTAGRAM[,"ID1"] == w & PENTAGRAM[,"ID2"] == x & PENTAGRAM[,"ID3"] == y & PENTAGRAM[,"ID4"] == z)
digit <- which.max(herewego[,"Count"])
reverse <- herewego[digit,5]
if (length(WORDS[reverse]) > 0) {
	WORDS[reverse]
}

else {

#QUADGRAM
herewego <- subset(QUADGRAM, QUADGRAM[,"ID1"] == x & QUADGRAM[,"ID2"] == y & QUADGRAM[,"ID3"] == z)
digit <- which.max(herewego[,"Count"])
reverse <- herewego[digit,4]
if (length(WORDS[reverse]) > 0) {
	WORDS[reverse]
}
else {
#TRIGRAM
herewego <- subset(TRIGRAM, TRIGRAM[,"ID1"] == y & TRIGRAM[,"ID2"] == z)
digit <- which.max(herewego[,"Count"])
reverse <- herewego[digit,3]
if (length(WORDS[reverse]) > 0) {
	WORDS[reverse]
}
else {
herewego <- subset(BIGRAM, BIGRAM[,"ID1"] == z)
digit <- which.max(herewego[,"Count"])
reverse <- herewego[digit,2]
if (length(WORDS[reverse]) > 0) {
	WORDS[reverse]
}
else {
digit <- which.max(UNIGRAM[,"Count"])
UNIGRAM[digit,1]
}
}
}
}
}


