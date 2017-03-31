#=========================================
# TEXT MINING                            #
#=========================================
# libraries 
library(tm)                 # Framework for text mining.
library(qdap)               # Quantitative discourse analysis of transcripts.
library(qdapDictionaries)
library(dplyr)              # Data wrangling, pipe operator %>%().
library(RColorBrewer)       # Generate palette of colours for plots.
library(ggplot2)            # Plot word frequencies.
library(scales)             # Include commas in numbers.
library(Rgraphviz)          # Correlation plots.
# library(magrittr)
library(SnowballC)

# library(help=tm)  # libray documentation
# search()          # prioridade de funcoes repetidas

# getSources()
# getReaders()
cname <- file.path("E:/Dev/share_win/txt_files/")
cname
length(dir(cname))
# junta os arquivos do diretorio
docs <- Corpus(DirSource(cname))
summary(docs)
inspect(docs[])
# getTransformations()
# custom transformer: change a char to " "
toSpace <- content_transformer(function(x, pattern) gsub(pattern, " ", x))
docs <- tm_map(docs, toSpace, "@")  # tira o "@" de todo o corpus e substitui por " "
docs <- tm_map(docs, toSpace, "/")
docs <- tm_map(docs, toSpace, "\\|")
# visualizacao do conteudo do primeiro arquivo do corpus
strwrap(docs[[1]])
# custom transformer: lower case
docs <- tm_map(docs, content_transformer(tolower))
strwrap(docs[[1]])
# remove numbers
docs <- tm_map(docs, removeNumbers)
strwrap(docs[[1]])
# remove Punctuation
docs <- tm_map(docs, removePunctuation)
strwrap(docs[[1]])
# Stop words are common words found in a language
length(stopwords("portuguese"))
stopwords("portuguese")
# remove stopwords in portuguese language
docs <- tm_map(docs, removeWords, stopwords("portuguese"))
strwrap(docs[[1]])
# remove custom stopwords
docs <- tm_map(docs, removeWords, c("estatísticas", "próximos", "correlação"))
strwrap(docs[[1]])
# Multiple whitespace characters are collapsed to a single blank.
docs <- tm_map(docs, stripWhitespace)
strwrap(docs[[1]])
# substitui strings por outras
toString <- content_transformer(function(x, from, to) gsub(from, to, x))
docs <- tm_map(docs, toString, "instituto ciências matemáticas computação", "ICMC")
strwrap(docs[[1]])
# removes common word endings 
docs <- tm_map(docs, stemDocument, language = "portuguese")  
strwrap(docs[[1]])

# document terms matrix
dtm <- DocumentTermMatrix(docs)
# some information
dtm
class(dtm)
dim(dtm)       # dimension
# just some part of the matix
inspect(dtm[1:2, 1:5]) 
# transpose the matrix
tdm <- TermDocumentMatrix(docs)
dim(tdm)
# matrix of the sums of columns 
freq <- colSums(as.matrix(dtm))
length(freq)
# ordering
ord <- order(freq)
# Least frequent terms
freq[head(ord,15)]
# Most frequent terms
freq[tail(ord,15)]
# distribution of frequencies.
head(table(freq), 15)
tail(table(freq), 15)
# conversion to matrix to write as .csv
m <- as.matrix(dtm)
dim(m)
write.csv(m, file="E:/Dev/share_win/write_files/dtm.csv")
read.csv("E:/Dev/share_win/write_files/dtm.csv")
# create a dataframe
library(readr)
df.dtm <- read_csv("E:/Dev/share_win/write_files/dtm.csv")

# We are often not interested in infrequent terms in our documents.
dtms <- removeSparseTerms(dtm, 0.1)
dim(dtms)
dim(dtm)
inspect(dtms)
# distribution of frequencies
freq <- colSums(as.matrix(dtms))
freq
table(freq)
# terms that occur at least 100 times
findFreqTerms(dtm, lowfreq = 100)
# We can also find associations with a word, specifying a correlation limit.
# If two words always appear together then the correlation would be 1.0 
# and if they never appear together the correlation would be 0.0. Thus 
# the correlation is a measure of how closely associated the words are in the corpus.
findAssocs(dtm, "miner", corlimit=0.9) # parece incorreto
# Rgraphviz plot
plot(dtm, terms = findFreqTerms(dtm, lowfreq=100)[1:20], corThreshold=0.5)

# plotting words frequencies
freq <- sort(colSums(as.matrix(dtm)), decreasing = TRUE)
head(freq, 14)
names(freq)  
wf <- data.frame(word = names(freq), Frequency = freq)
head(wf)

library(ggplot2)
subset(wf, freq>200)             %>%
  ggplot(aes(word, Frequency))   +
  geom_bar(stat="identity")      +
  theme(axis.text.x=element_text(angle=45, hjust=1))

library(wordcloud)
brewer.pal.info  # colours set
set.seed(123)
wordcloud(names(freq), 
          scale = c(2,0.5),                # range of font sizes
          freq, min.freq = 30,
          max.words = 130,                 # limiting number of words
        #  colors = "blue"                 # single color option
          colors = brewer.pal(8,"Dark2")   # multi color
          )

# retain words shorter than 20 characters.
words <- dtm   %>%
  as.matrix    %>%
  colnames     %>%
  (function(x) x[nchar(x) < 20])

# library(qdap) 
dist_tab(nchar(words))
# words lenght histogram
data.frame(nletters = nchar(words))                      %>%
  ggplot(aes(x = nletters))                              +
  geom_histogram(binwidth = 1)                           +
  geom_vline(xintercept = mean(nchar(words)),
             colour = "green", size = 1, alpha = .5)     +
  labs(x = "Number of Letters", y = "Number of Words")

# letter frequency
library(stringr)
words                                   %>%
str_split("")                           %>%  # split the words into characters
sapply(function(x) x[-1])               %>%  # retira e lista a primeira letra
unlist                                  %>%  # Reducing the result into a simple vector
dist_tab                                %>%  # generate a dataframe
mutate(Letter=factor(toupper(interval),
                     levels=toupper(interval[order(freq)]))) %>%
ggplot(aes(Letter, weight=percent))     +
geom_bar()                              +
coord_flip()                            +
labs(y="Proportion")                    +
scale_y_continuous(breaks=seq(0, 12, 2),
                   label=function(x) paste0(x, "%"),
                   expand=c(0,0), limits=c(0,12))


# Pareto charts
library(qcc)
pareto.chart(table(state.division), ylab = "Frequency") 


