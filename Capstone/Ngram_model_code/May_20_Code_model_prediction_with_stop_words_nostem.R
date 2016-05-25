## We first set the working directory
setwd("~/Documents/Coursera/Capstone/Prediction Model/")

## We first import the packages we will use
library(tm)
library(SnowballC)
library(wordcloud)
library(RWeka)
library(ggplot2)
library(slam)
library(dplyr)
library(reshape2)

## The data can be downloaded from https://d396qusza40orc.cloudfront.net/dsscapstone/dataset/Coursera-SwiftKey.zip
## After unzip it, there should be a folder called ``final" which 
## contains three folders. The one named en_US directory contains 
## three text files, which are the texts we use for building models

## There are three text files in the ./final/en_US directory
## Let's first import the files
data_blogs <- readLines("./final/en_US/en_US.blogs.txt", encoding = "UTF-8", skipNul = T)
data_news <- readLines("./final/en_US/en_US.news.txt", encoding = "UTF-8", skipNul = T)
data_twit<- readLines("./final/en_US/en_US.twitter.txt", encoding = "UTF-8",skipNul = T)


########################################################
########################################################
clean.corpus.dtm <- function(trainCorpus, ng){
        trainCorpus <- Corpus(VectorSource(trainCorpus))

        options(mc.cores=1)
        tokenizer <- function(x){NGramTokenizer(x,Weka_control(min = ng, max = ng))}
        #dtm <- DocumentTermMatrix(trainCorpus, control = list(tokenize = tokenizer, wordLengths = c(1, Inf))) # create tdm from n-grams
        
        dtm <- DocumentTermMatrix(trainCorpus, control = list(tokenize = tokenizer, wordLengths = c(1,Inf))) # create dtm from n-gram. 
            # wordlengths give the minimum length = 1 to keep the words as
            # i, a,....
        return(dtm)
        }

#######################################################
########################################################
#clean corpus without using tm package
prepross.corpus <- function(x){
        
        prep.data <- tolower(x)
        
        ## If we want to keep stopwords, we will meet lots of abbreviation
        ## Let's first replace them with their usual expressions
        
        tiewords <- c("i'm", "you're" , "he's", "she's", "it's", "we're", "they're", "i've","you've", "we've", "they've", "i'd","you'd", "he'd", "she'd", "we'd", "they'd", "i'll", "you'll",  "he'll", "she'll", "we'll", "they'll", "isn't", "aren't", "wasn't", "weren't", "hasn't", "haven't", "hadn't", "doesn't", "don't", "didn't", "won't", "wouldn't", "shan't", "shouldn't", "can't", "cannot", "couldn't",   "mustn't", "let's",  "that's", "who's", "what's",     "here's",     "there's",    "when's", "where's", "why's", "how's")
        
        untiewords <- c("i am", "you are" , "he is", "she is", "it is", "we are", "they are", "i have","you have", "we have", "they have", "i would","you would", "he would", "she would", "we would", "they would", "i will", "you will",  "he will", "she will", "we will", "they will", "is not", "are not", "was not", "were not", "has not", "have not", "had not", "does not", "do not", "did not", "will not", "would not", "shall not", "should not", "can not", "can not", "could not",   "must not", "let us",  "that is", "who is", "what is", "here is", "there is", "when is", "where is", "why is", "how is")
                
        for (i in 1:length(tiewords)){
                prep.data <- gsub(tiewords[i], untiewords[i], prep.data)
                }
        
        ## Remove the equals
        prep.data<- gsub("=", "", prep.data)
        
        ## Remove the -
        prep.data<- gsub("-", " ", prep.data) 
        
        ## Remove the dollar sign
        prep.data <- gsub("$", "", prep.data)
        
        ## Remove the question sign
        prep.data <- gsub("?", "", prep.data)
        
        ## Remove the : sign
        prep.data <- gsub(":", "", prep.data) 
        
        ## Remove the exclamation sign
        prep.data <- gsub("!", "", prep.data) 
        
        ## Remove the @ sign
        prep.data <- gsub("@", "", prep.data) 
        
        ## Remove the ) sign
        prep.data <- gsub(")", "", prep.data) 
        
        ## Remove the ( sign
        prep.data <- gsub("\\(", "", prep.data) 
        
        ## Remove the , sign
        prep.data <- gsub(",|/.*$", "", prep.data) 
        
        
        ## Import the dirty words that need removing
        dirtywords <- readLines("swearWords.txt", warn = FALSE)[1:551]

                prep.data <- prep.data[!(prep.data %in% dirtywords)]
        

        # remove numbers
        prep.data <- removeNumbers(prep.data)
        #prep.data <- gsub("[0-9]","",prep.data)
        
        # remove punctuations
        prep.data <- removePunctuation(prep.data)

        
        ## Remove white spaces left after we perform all the steps above
#        prep.data<- stripWhitespace(prep.data)
        ## Let's split the additional two words created above again
        prep.data <- unlist(strsplit(prep.data, split = " "))
        
        ############### Stem or not is a good question###############               
        # stem
#        data.copy <- prep.data
#        prep.data <- stemDocument(prep.data)
#        prep.data <- stemCompletion(prep.data, dictionary = data.copy )
        ###############################################################
        
        prep.data <- prep.data[grep("^[a-z]+$", prep.data)]
        return(prep.data)
}

########################################
#######################################
ngram_predict <- function(D) {
        # Need u.count,b.count,u.logpr,b.logpr,dictionary
        #load("TextModel.RData")
        # Assume data has already been tokenized using ngram_tokenize and filtered through dictionary. 
        D <- unlist(strsplit(D, split= " "))
        D <- prepross.corpus(D)
        D[!(D %in% word.ref)] <- "oov" # if words not in word.ref set it to oov
       
        L <- length(D)
        N = 2 # use last 2 words to predict next word.
        S <- ifelse(L >= N, L-N+1, 1) 
        # i.e., if L = 10, N = 1, take S = 9. Which means we take word 9 and 10
        # for the first two words in trigram
        # if L = N =2, take S =1, whcih means take 1st and 2nd words 
        # if L =1 <N, simply take S = 1
        
        # If trigram exists in the trigram table then use trigram log q_uvw
        # Otherwise go back to search bigram
        # If bigram exists in bigram table then use bigram log probabiilty
        # else if first word is present in dictionary then use smoothed 
        # conditional probability else back off to unigram probabilities
        # on all words.
        
        ## If L > N, S will be L - N +1, we can map out previous two words
        ## called YZ by pasting D[S:(L-1)] and D[(S+1):L]
        if (L >= N){
                
        #XY <- mapply(function(y,z) paste(y,z,collapse = " "), D[S:(L-1)], D[(S+1):L], USE.NAMES = F)
        XY <- paste(D[L-1],D[L],collapse = " ")
        
        Z <- word.ref
        
        # Let's make a trigram by combining XY with the words from word.ref
        XYZ <- mapply(function(p,q) paste(p,q,collapse = " "), XY, Z, USE.NAMES = F)

        # Now let's search for the trigram table
        a <- t.logpr[XYZ] # trigram present in trigram table
        
        # If there is no corresponding trigram, it will give NA
        # Which also means count(q_XYZ) = 0
        # Let's search for the locations of NA
#        na_id <- which(is.na(a))
        
        ## Now since Z is in the word.ref, it is definitely not a "oov"
        ## Then we need to ask if X or Y is an "oov"
        
        X <- D[S] # This will give the first words of a trigram
        Y <- D[S+1] # This gives the second word of a trigram
        
        # We also need YZ for prediction
        YZ <- mapply(function(y,z) paste(y,z,collapse = " "), Y, Z, USE.NAMES = F)
        id0 <- (XYZ %in% names(trigram.freq))
        ## Now we need to ask if X or Y or both are "oov"
        ## We first check if XY and YZ are in the bigram table
        id1 <- ((!id0) & ( XY %in% names(bigram.freq)) & (YZ %in% names(bigram.freq)))
        
        id2 <-  ((!id0) & ( XY %in% names(bigram.freq)) & !(YZ %in% names(bigram.freq))) 
        
        id3 <-  ((!id0) & !(XY %in% names(bigram.freq)) & (YZ %in% names(bigram.freq)) & ( X != "oov")) 
        
        id4 <-  ((!id0) & !(XY %in% names(bigram.freq)) & (YZ %in% names(bigram.freq)) & ( X == "oov")) 
        
        id5 <-  ((!id0) & !(XY %in% names(bigram.freq)) & !(YZ %in% names(bigram.freq)) & ( X != "oov") & ( Y!= "oov") ) 
        
        id6 <-  ((!id0) & !(XY %in% names(bigram.freq)) & !(YZ %in% names(bigram.freq)) & ( X != "oov") & ( Y == "oov") ) 
        
        id7 <-  ((!id0) & !(XY %in% names(bigram.freq)) & !(YZ %in% names(bigram.freq)) & ( X == "oov") & ( Y != "oov") ) 
        
        id8 <-  ((!id0) & !(XY %in% names(bigram.freq)) & !(YZ %in% names(bigram.freq)) & ( X == "oov") & ( Y == "oov") ) 
        
        
        if(sum(id1) >0){ a[which(id1)] <- (
                b.logpr[XY] + b.logpr[YZ[id1]]
        ) }
        

        if(sum(id2)>0){ a[which(id2)] <- (
                b.logpr[XY] + u.logpr[Y] + u.logpr[Z[id2]]
        )}
        
        
        if(sum(id3)>0){ a[which(id3)] <- (
                u.logpr[X] + u.logpr[Y] + b.logpr[YZ[id3]]
        )}
        

        if(sum(id4)>0){ a[which(id4)] <- (
                u.logpr["oov"] + u.logpr[Y] + b.logpr[YZ[id4]]
        )}
        

        if(sum(id5)>0){ a[which(id5)] <-(
                u.logpr[X] + u.logpr[Y] + u.logpr[Z[id5]]
        )}
        
        #id6 <-  ((!id0) & !(XY %in% names(bigram.freq)) & !(YZ %in% names(bigram.freq)) & ( X != "oov") & ( Y == "oov") ) 
        
        if(sum(id6)>0){ a[which(id6)] <-(
                u.logpr[X] + u.logpr["oov"] + u.logpr[Z[id6]]
        )}
        
        
        #id7 <-  ((!id0) & !(XY %in% names(bigram.freq)) & !(YZ %in% names(bigram.freq)) & ( X == "oov") & ( Y != "oov") ) 
        
        if(sum(id7)>0){ a[which(id7)] <-(
                u.logpr["oov"] + u.logpr[Y] + u.logpr[Z[id7]]
        )}
        
        #id8 <-  ((!id0) & !(XY %in% names(bigram.freq)) & !(YZ %in% names(bigram.freq)) & ( X == "oov") & ( Y == "oov") ) 
        
        if(sum(id8)>0){ a[which(id8)] <-(
                u.logpr["oov"] + u.logpr["oov"] + u.logpr[Z[id8]]
        )}
        
        
        # pick the word with largest log q_XYZ
        names(a) <- word.ref
        a <- a[!is.na(a)]
        print(word.ref[which.max(a)])
        
        a <- sort(a, decreasing = T)      
        print(names(a)[1:5])
        } 
        else{    # the length of the corpus will be simply 1
                   # The best model we can use is 'bigram'
                P <- D[L]
                Q <- word.ref
                # Let's paste the input word P and the word.ref
                # to create a list of bigram PQ
                PQ <- mapply(function(p,q) paste(p,q,collapse = " "), P, Q, USE.NAMES = F)
                b <- b.logpr[PQ] # search for bigram table
                b.na_id <- which(is.na(b)) # locate the na
                #b <- b[-b.na_id] # let's remove nas
                
                p.id0 <- (PQ %in% names(bigram.freq))
                # For location of na, there are two situations
                # 1st: P is not "oov" but just the combined PQ can not
                # be found in bigram table
                # 2nd: P is indeed a "oov"
                
                #check if the input word is "oov"
                p.id1 <- ((!p.id0) & (P != "oov")) 
                p.id2 <- ((!(p.id0)) & ( P == "oov"))  # means it is a "oov"
                
                if(sum(p.id1)>0){b[p.id1] <- (
                        u.logpr[P] + u.logpr[Q[p.id1]]
                )}
                
                if(sum(p.id2>0)){b[p.id2] <- (
                        u.logpr["oov"] + u.logpr[Q[p.id2]]
                )}
                
                names(b) <- word.ref
                print(word.ref[which.max(b)])
                
                b <- sort(b, decreasing = T)
                print(names(b)[1:5])
                
        }
}


##############################################################

##Let's pull out parts of the text files to be the training set
sp.n = 100 # sample size for each text file
nloop = 3000  ## total number of lines read is sp.n*nloop
M <- matrix(1:(sp.n*nloop),nrow=sp.n, ncol = nloop)

##open null vectors for appending
unigram.total.freq <- c()
bigram.total.freq <- c()
trigram.total.freq <- c()

val_blogs <- c()
test_blogs <- c()
val_news <- c()
test_news <- c()
val_twit <- c()
test_twit <- c()


### Let's first choose some random samples
set.seed(1) # for reproducible analysis
sp.nblog <- sample(1:length(data_blogs), sp.n*nloop, replace = FALSE)
sp.nnews <- sample(1:length(data_news), sp.n*nloop, replace = FALSE)
sp.ntwit <- sample(1:length(data_twit), sp.n*nloop, replace = FALSE)

s_blogs <- data_blogs[sp.nblog]
s_news <- data_news[sp.nnews]
s_twit <- data_twit[sp.ntwit]
############ Loop starts ####################
for (i in 1:nloop){
sample_blogs <- s_blogs[M[,i]]
sample_news <- s_news[M[,i]]
sample_twit <- s_twit[M[,i]]

## Take  80 % of samples to be train sets
## 10 % to be validation sets
## 10 % to be test sets
train.n = 0.8*sp.n
valid.n = 0.1*sp.n
test.n = 0.1*sp.n

## Let's randomly select lines for training, validation, and testing sets 
set.seed(11) 
tr.n <- sample(1:sp.n, train.n, replace = FALSE)
va.n <- sample(which(!(seq(sp.n) %in% tr.n)), 0.1*sp.n, replace = FALSE)
## separate out training sets, validation sets, and test sets
## For train sets, we can only focus on the 100 lines read in one loop only
## We will extract n-grams out of this 100 lines each loop (time) and 
## After loop we will combine the results
train_blogs <- sample_blogs[tr.n]
train_news <- sample_news[tr.n]
train_twit <- sample_twit[tr.n]

## For Val and Test sets, we need to store the complete data for use later
val_blogs <- c(val_blogs,sample_blogs[va.n])
val_news <- c(val_news,sample_news[va.n])
val_twit <- c(val_twit,sample_twit[va.n])

test_blogs <- c(test_blogs,sample_blogs[-c(tr.n, va.n)])
test_news <- c(test_news,sample_news[-c(tr.n, va.n)])
test_twit <- c(test_twit,sample_twit[-c(tr.n,va.n)])

## Let's combine the three subdata to be one
train_data <- unlist(c(train_blogs, train_news, train_twit))
val_data <- unlist(c(val_blogs, val_news, val_twit))
test_data <- unlist(c(test_blogs, test_news, test_twit))

##########################################
## Building N-gram models:
train_data <- unlist(strsplit(train_data, split = " "))
train_data <- prepross.corpus(train_data)
train_data <- paste(train_data, collapse = " ")
cleancorpus <- train_data
## We first generate a clean corpus for N-grams 
#cleancorpus <- clean.corpus(train_data)

## 1-gram model
unigram.dtm <- clean.corpus.dtm(cleancorpus,1) ##generate 1-gram dtm

unigram.dtm <- rollup(unigram.dtm, 1, na.rm=TRUE, FUN = sum)
unigram.freq <- colSums(as.matrix(unigram.dtm))

## Now let's give the top 10 two-words occuring most frequently
#unigram.freq <- sort(unigram.freq, decreasing = T)
#total.unigram.freq <- sum(unigram.freq)
## create the unigrams data frame
#unigram.wf <- data.frame(unigram.word=names(unigram.freq), unigram.freq=unigram.freq, unigram.probability = unigram.freq/total.unigram.freq, row.names = NULL) 

#unigram <- unigram.wf

#unigram[,1] <- as.character(unigram[,1]) # make col one characters

#head(unigram.wf, 100)

## 2-grams
#######################################
bigram.dtm <- clean.corpus.dtm(cleancorpus,2) ##generate sample dtm

bigram.dtm <- rollup(bigram.dtm, 1, na.rm=TRUE, FUN = sum)
bigram.freq <- colSums(as.matrix(bigram.dtm))

## Now let's give the top two-words occuring most frequently
bigram.freq <- sort(bigram.freq, decreasing = T)
total.bigram.freq <- sum(bigram.freq)



#head(bigram.wf, 100)
################################################

## 3-grams
trigram.dtm <- clean.corpus.dtm(cleancorpus,3) ##generate sample 1 dtm

trigram.dtm <- rollup(trigram.dtm, 1, na.rm=TRUE, FUN = sum)
trigram.freq <- colSums(as.matrix(trigram.dtm))


## Now let's give the top two-words occuring most frequently
trigram.freq <- sort(trigram.freq, decreasing = T)
total.trigram.freq <- sum(trigram.freq)


unigram.total.freq <- c(unigram.total.freq, unigram.freq)
bigram.total.freq <- c(bigram.total.freq, bigram.freq)
trigram.total.freq <- c(trigram.total.freq, trigram.freq)
}
############### End loop #########################
# Lot's of data need combining

## make "complete" unigram.wf
unigram <- data.frame(unigram.word=names(unigram.total.freq), unigram.freq=unigram.total.freq, row.names = NULL) 

unigram[,1] <- as.character(unigram[,1]) # make col one characters

## Let's use reshape2 package to combine common terms
u.melt <- melt(unigram, id = "unigram.word", measure.vars = "unigram.freq")
unigram <- dcast(u.melt, unigram.word ~ variable, sum)
#head(unigram, 100)
unigram <- arrange(unigram, desc(unigram.freq))
unigram.freq <- unigram[,2]
names(unigram.freq) <- unigram[,1]

############# Select out vocabularies #############
max.line = 8000 
sub.unigram <- unigram[1:max.line,]
cover.rate <- sum(sub.unigram[,2])/sum(unigram[,2])
cover.rate
unigram_90 <- unigram[1:max.line,]
word.ref <- unigram_90[,1]
unigram_90.freq <- unigram.freq[1:max.line]
############################################

#####################################################
## make "complete" bigram.wf
## create the bigrams data frame
bigram <- data.frame(bigram.word=names(bigram.total.freq), bigram.freq=bigram.total.freq,row.names = NULL) 


## use reshape2 package
b.melt <- melt(bigram, id = "bigram.word", measure.vars = "bigram.freq")
bigram <- dcast(b.melt, bigram.word ~ variable, sum)

bigram[,1] <- as.character(bigram[,1])
bigram.freq <- bigram[,2]
names(bigram.freq) <- bigram[,1]
## We now want to filter out the bigrams containing words out of 
## word.ref
b.first.wd <- sapply(strsplit(names(bigram.freq), split = " "), function(x){x[1]} ) ## split the bigram words and extract the first word
b.second.wd <- sapply(strsplit(names(bigram.freq), split = " "), function(x){x[2]})

b.in.word.ref <- (b.first.wd %in% word.ref) & (b.second.wd %in% word.ref)
bigram.freq <- bigram.freq[b.in.word.ref]

## Re-create the bigrams data frame
bigram <- data.frame(bigram.word=names(bigram.freq), bigram.freq=bigram.freq, row.names = NULL) 
bigram[,1] <- as.character(bigram[,1])

bigram <- arrange(bigram, desc(bigram.freq))
bigram.freq <- bigram[,2]
names(bigram.freq) <- bigram[,1]
head(bigram,100)
################################################
## make "complete" trigram.wf
## create the trigrams data frame
trigram <- data.frame(trigram.word=names(trigram.total.freq), trigram.freq= trigram.total.freq, row.names = NULL) 


## use reshape2 package
t.melt <- melt(trigram, id = "trigram.word", measure.vars = "trigram.freq")
trigram <- dcast(t.melt, trigram.word ~ variable, sum)

trigram.freq <- trigram[,2]
names(trigram.freq) <- trigram[,1]
## We now want to filter out the bigrams containing words out of 
## word.ref
t.first.wd <- sapply(strsplit(names(trigram.freq), split = " "), function(x){x[1]} ) ## split the bigram words and extract the first word

t.second.wd <- sapply(strsplit(names(trigram.freq), split = " "), function(x){x[2]})

t.third.wd <- sapply(strsplit(names(trigram.freq), split = " "), function(x){x[3]})

t.in.word.ref <- (t.first.wd %in% word.ref) & (t.second.wd %in% word.ref) & (t.third.wd %in% word.ref)

trigram.freq <- trigram.freq[t.in.word.ref]

## Re-create the trigrams data frame
trigram <- data.frame(trigram.word=names(trigram.freq), trigram.freq=trigram.freq, row.names = NULL) 
trigram[,1] <- as.character(trigram[,1])

trigram <- arrange(trigram, desc(trigram.freq))
trigram.freq <- trigram[,2]
names(trigram.freq) <- trigram[,1]
head(trigram,100)
##################################################

# Calculate unigram log q(w) = log(add lambda smoothing), where lambda =1
# corresponds to the usual add one smoothing
N <- sum(unigram_90[,2]) # Total number of tokens (words)
# N <- sum(unigram_90.freq) # unigram_90.freq = unigram_90[,2]
V <- max.line # Number of unique words in the dictionary    

## ?? What's the best value for lambda??????
lambda <- 0.04  # add-lambda smoothing = generalized add-one appraoch
               # We need to check what value of lambda optimize the log(q)

## Inlude a new row of out of vocabulary word named "oov"
oov <- data.frame(as.character("oov"), as.numeric(0))
names(oov) <- names(unigram_90)

## Give a new unigram_90 including oov
unigram_90 <- rbind(unigram_90, oov)

## Laplacian smoothing....add 1 to every count freq
unigram_90[,3] <- unigram_90[,2] + lambda

## Count the log probability
u.logpr <- log(unigram_90[,3]/(N + lambda*V))

unigram_90[,4] <- u.logpr

names(unigram_90) <- c("unigram", "unigram.freq", "add.lambda.smooth","u.logpr")

names(u.logpr) <- unigram_90[,1] ## assign names for u.logpr

## separate out lambda smooth count for later use for 
## fast calculation
u.lambda.smth <- unigram_90[,3]
u.count <- unigram_90[,3] ## the same to u.lap.smth

names(u.lambda.smth) <- unigram_90[,1] ## assign names for u.lambda.smth
names(u.count) <- unigram_90[,1] ## assign names for u.lap.smth

print(head(unigram_90))
print(tail(unigram_90))

#############################################
### Backoff approach ####

# Calculate bigram log likelihood
bg.names <- names(bigram.freq)

bg.1st.wd <- sapply(strsplit(bg.names, split = " "), function(x){x[1]} )
## split the bigram words and extract the first word

bg.2nd.wd <- sapply(strsplit(bg.names, split = " "), function(x){x[2]} )
## split the bigram words and extract the seond word


# Let's again use the general add lambda approach

b_w <- (unigram_90.freq[bg.2nd.wd] + lambda) / ( N + lambda * V)
b_vw <- (bigram.freq + lambda * V * b_w)/(unigram_90.freq[bg.1st.wd] + lambda * V)
b.logpr <- log(b_vw)

## calculate the log likelihood
b.count <- bigram[,2] + lambda
bigram[,3]<- b.count
bigram[,4] <- b.logpr
names(bigram) <- c("bigram", "bigram.freq","lambda.smooth","bi.logpr")

names(b.logpr) <- bigram[,1]
names(b.count) <- bigram[,1]
print(head(bigram,10))
print(tail(bigram,10))

###############################################

# Calculate trigram log likelihood
tg.names <- names(trigram.freq)
tg.1st2.wd <- sapply(strsplit(tg.names, split = " "), function(x){
        paste(x[1:2], collapse = " ")
} ) ## split the trigram and extract the 1st and 2nd word
tg.2nd3.wd <- sapply(strsplit(tg.names, split = " "), function(x){
        paste(x[2:3], collapse = " ")
} ) ## split the trigram and extract the 2nd and 3rd word

tg.2nd.wd <- sapply(strsplit(tg.names, split = " "), function(x){x[2]})
## split the trigram and extract the 2nd word

tg.3rd.wd <- sapply(strsplit(tg.names, split = " "), function(x){x[3]})
## split the trigram and extract the 3rd word

# Remind N = total number of tokens (words)

# Remind V = Number of unique words in the dictionary    

# get the last word unigram add-lambda q_w
q_w <- (unigram_90.freq[tg.3rd.wd] + lambda)/(N + lambda * V)

# get the last two words bigram add-lambda q_w
q_vw <- (bigram.freq[tg.2nd3.wd] + lambda * V *q_w)/(unigram_90.freq[tg.2nd.wd] + lambda * V)

# finally the trigram add-lambda formula
q_uvw <- (trigram.freq + lambda * V * q_vw)/(bigram.freq[tg.1st2.wd] + lambda * V)
t.logpr <- log(q_uvw)

t.count <- trigram[,2] + lambda
trigram[,3]<- t.count
trigram[,4] <- t.logpr
names(trigram) <- c("trigram", "trigram.freq","lambda.smooth","t.logpr")

names(t.logpr) <- trigram[,1]
names(t.count) <- trigram[,1]
print(head(trigram,10))
print(tail(trigram,10))

##############################################

summary(u.logpr)
summary(b.logpr)
summary(t.logpr)

###############################################

par(mfrow=c(1,3))
hist(u.logpr,xlab="Unigram log prob", main= "Histogram of Unigram log prob",col = "blue")
hist(b.logpr,xlab="Bigram log prob", main = "Histogram of Bigram log prob",col = "green")
hist(t.logpr,xlab="Trigram log prob", main = "Histogram of Trigram log prob", col = "salmon")

###################################################################

## validation sample

## take some parts of the val_data to be val_set for comparing models
set.seed(111) # for rerproducible analysis
n.sp <- sample(1:length(val_blogs), length(val_blogs)/10, replace = FALSE)

val_set <- unlist(c(val_blogs[n.sp], val_news[n.sp], val_twit[n.sp]))
val_set <- unlist(strsplit(val_set, split = " ")) ## make single word

val_set <- prepross.corpus(val_set) 

val_set[!(val_set %in% word.ref)] <- "oov" ## word out of the ref is marked as oov (out of vocabulary)

## Calculate unigram perplexity
#row.names(unigram_90) <- unigram_90[,1] 
# assign the unigram to be the row name 

row.names(bigram) <- bigram[,1]
## assign the bigram to be the row name 

row.names(trigram) <- trigram[,1]
# assign the trigram to be the row name 

A <- u.logpr[val_set] ## This gives the log prob of all words
u.perp <- exp(-sum(A)/length(A)) ## This is the unigram perplexity
print(u.perp) # print it out

##################################################################
##bigram perplexity
# Bigram model with backoff
# If bigram exists in bigram table then use bigram log probabiilty
# else if first word is present in dictionary then use smoothed 
# conditional probability
# else back off to unigram probabilities on both words.

YZ <- mapply(function(x,y){paste(x,y, sep = " ")}, val_set[1:(length(val_set)-1)], val_set[2:length(val_set)])
## Manually create bigrams

Y <-  sapply(strsplit(YZ, split = " "), function(x) {x[[1]]})## give first words
Z <- sapply(strsplit(YZ, split = " "), function(x) {x[[2]]}) ## give second words

b.id1 <- (YZ %in% names(b.logpr))
b.id2 <- ((!b.id1) & (Y != "oov") & (Z !="oov"))
b.id3 <- ((!b.id1) & (Y != "oov") & (Z =="oov"))
b.id4 <- ((!b.id1) & (Y == "oov") & (Z != "oov"))
b.id5 <-  ((!b.id1) & (Y == "oov") & (Z == "oov"))

## create a Null vector
a <- rep(0,length(YZ))
names(a) <- YZ

if(sum(b.id1)>0) {a[b.id1] <- b.logpr[YZ[b.id1]]}

if(sum(b.id2)>0) {a[b.id2] <- log((lambda * V * (unigram_90.freq[Z[b.id2]] + lambda)/(N + lambda * V)) / ( unigram_90.freq[Y[b.id2]] + lambda * V))}

if(sum(b.id3)>0) {a[b.id3] <- log(lambda * V) + u.logpr["oov"] -log(unigram_90.freq[Y[b.id3]] + lambda * V)}

if(sum(b.id4)>0) {a[b.id4] <- log(lambda * V) + u.logpr[Z[b.id4]] - log(lambda + lambda * V)}

if(sum(b.id5)>0) {a[b.id5] <- log(lambda * V) + u.logpr["oov"] - log(lambda + lambda * V)}

b.perp <- exp(-sum(a)/length(a))
print(b.perp)


###########################################################
## Trigram model with backoff
# If trigram exist in the trigram table then use trigram log 
# probability else for each bigram if bigrams exists in
# bigram table then use bigram log probabilit
# else if first word is present in dictionary then use 
# smoothed conditional probability
# else back off to unigram probabilities across all three words

PQR <- mapply(function(p,q,r){paste(p,q,r, sep = " ")}, val_set[1:(length(val_set)-2)], val_set[2:(length(val_set)-1)], val_set[3:length(val_set)]) ## create three grams

PQ <- sapply(strsplit(PQR,split = " "), function(x){ paste(x[[1]], x[[2]], sep =" ")}) # 1st and 2nd words of trigrams

QR <- sapply(strsplit(PQR,split = " "), function(x){ paste(x[[2]], x[[3]], sep =" ")}) # 2nd and third words of trigram

P <-  sapply(strsplit(PQR, split = " "), function(x) {x[[1]]}) # give first words

Q <-  sapply(strsplit(PQR, split = " "), function(x) {x[[2]]}) # give 2nd words

R <-  sapply(strsplit(PQR, split = " "), function(x) {x[[3]]}) # give last words


### Many possible situations below
t.id1 <- (PQR %in% names(trigram.freq)) # counted trigram

t.id2 <- ( (!t.id1) & (PQ %in% names(bigram.freq)) & (QR %in% names(bigram.freq)))

t.id3.1 <- ( (!t.id1) & (PQ %in% names(bigram.freq)) & !(QR %in% names(bigram.freq)) & (R !="oov"))

t.id3.2 <- ( (!t.id1) & (PQ %in% names(bigram.freq)) & !(QR %in% names(bigram.freq)) & (R =="oov"))

t.id4.1 <- ( (!t.id1) & !(PQ %in% names(bigram.freq)) & (QR %in% names(bigram.freq)) & (P !="oov"))

t.id4.2 <- ( (!t.id1) & !(PQ %in% names(bigram.freq)) & (QR %in% names(bigram.freq)) & (P =="oov"))

t.id5.1 <- ( (!t.id1) & !(PQ %in% names(bigram.freq)) & !(QR %in% names(bigram.freq)) & (P != "oov") & (Q != "oov") & (R == "oov"))

t.id5.2 <- ( (!t.id1) & !(PQ %in% names(bigram.freq)) & !(QR %in% names(bigram.freq)) & (P != "oov") & (Q != "oov") & (R != "oov"))

t.id5.3 <- ( (!t.id1) & !(PQ %in% names(bigram.freq)) & !(QR %in% names(bigram.freq)) & (P != "oov") & (Q == "oov") & (R != "oov"))

t.id5.4 <- ( (!t.id1) & !(PQ %in% names(bigram.freq)) & !(QR %in% names(bigram.freq)) & (P == "oov") & (Q != "oov") & (R != "oov"))

t.id5.5 <- ( (!t.id1) & !(PQ %in% names(bigram.freq)) & !(QR %in% names(bigram.freq)) & (P != "oov") & (Q == "oov") & (R == "oov"))

t.id5.6 <- ( (!t.id1) & !(PQ %in% names(bigram.freq)) & !(QR %in% names(bigram.freq)) & (P == "oov") & (Q != "oov") & (R == "oov"))

t.id5.7 <- ( (!t.id1) & !(PQ %in% names(bigram.freq)) & !(QR %in% names(bigram.freq)) & (P == "oov") & (Q == "oov") & (R != "oov"))

t.id5.8 <- ( (!t.id1) & !(PQ %in% names(bigram.freq)) & !(QR %in% names(bigram.freq)) & (P == "oov") & (Q == "oov") & (R == "oov"))


## We now calculate the log q(uvw)
b <- rep(0,length(PQR))

if(sum(t.id1)>0) { b[t.id1] <- t.logpr[PQR[t.id1]] } # existing trigram

if(sum(t.id2)>0) { b[t.id2] <- log(lambda*V*((bigram.freq[QR[t.id2]]+lambda * V*((unigram_90.freq[R[t.id2]] + lambda)/(N + lambda * V)))/(unigram_90.freq[Q[t.id2]]+ lambda * V))/(bigram.freq[PQ[t.id2]] + lambda * V)) }


if(sum(t.id3.1)>0) { b[t.id3.1] <- log(lambda * V *((lambda * V *((unigram_90.freq[R[t.id3.1]] + lambda)/(N + lambda * V)))/(u.count["oov"]+ lambda * V)))/(bigram.freq[PQ[t.id3.1]] + lambda * V)}


if(sum(t.id3.2)>0){ b[t.id3.2] <- log((lambda * V *((lambda * V *((u.count["oov"] + lambda)/(N + lambda * V)))/(unigram_90.freq[Q[t.id3.2]] + lambda * V)))/(bigram.freq[PQ[t.id3.2]] + lambda * V))}



if(sum(t.id4.1)>0){ b[t.id4.1] <- log((bigram.freq[QR[t.id4.1]]+ lambda * V*((unigram_90.freq[R[t.id4.1]]+lambda)/(N + lambda*V)))/(unigram_90.freq[Q[t.id4.1]] + lambda * V))}


if(sum(t.id4.2)>0){ b[t.id4.2] <- log((bigram.freq[QR[t.id4.2]]+ lambda * V*((unigram_90.freq[R[t.id4.2]]+lambda)/(N + lambda*V)))/(unigram_90.freq[Q[t.id4.2]] + lambda * V))}


if(sum(t.id5.1) >0){b[t.id5.1] <- log((lambda * V)*((u.count["oov"] + lambda)/(N + lambda * V))/(unigram_90.freq[Q[t.id5.1]] + lambda * V))}


if(sum(t.id5.2) >0){b[t.id5.2] <- log((lambda * V)*((unigram_90.freq[R[t.id5.2]] + lambda)/(N + lambda * V))/(unigram_90.freq[Q[t.id5.2]] + lambda * V))}



if(sum(t.id5.3)>0){b[t.id5.3] <- log((lambda * V *((unigram_90.freq[R[t.id5.3]] + lambda)/(N + lambda * V)))/(u.count["oov"] + lambda * V))}


if(sum(t.id5.4)>0){b[t.id5.4] <- log(lambda * V*((unigram_90.freq[R[t.id5.4]] + lambda)/(N + lambda * V))/(unigram_90.freq[Q[t.id5.4]] + lambda * V))}


if(sum(t.id5.5) >0){ b[t.id5.5] <- log((lambda*V*((u.count["oov"] + lambda)/(N + lambda * V)))/(u.count["oov"] + lambda * V))}


if(sum(t.id5.6)>0){ b[t.id5.6] <- log((lambda * V *((u.count["oov"] + lambda)/(N + lambda * V)))/(unigram_90.freq[Q[t.id5.6]] + lambda * V))}


if(sum(t.id5.7)>0){ b[t.id5.7] <- log((lambda * V * ((unigram_90.freq[R[t.id5.7]] + lambda)/(N + lambda * V)))/(u.count["oov"] + lambda * V))}


if(sum(t.id5.8)>0){ b[t.id5.8] <- log((lambda * V *((u.count["oov"] + lambda)/(N + lambda*V)))/(u.count["oov"] + lambda * V))}

t.perp <- exp(-sum(b)/length(b))

print(t.perp)



# Save files
###########################################################

save(unigram,bigram,trigram, unigram_90, word.ref, unigram.freq, unigram_90.freq, bigram.freq, trigram.freq, u.count, b.count, u.logpr,b.logpr,t.logpr, cover.rate, N, V, lambda, file="300000_w_stopword-complete_data.RData")

save(word.ref, unigram_90.freq,  u.count, u.logpr, N, V, lambda,cover.rate, file="300000_w_stopword_unigram90.RData")

save(bigram.freq, b.count, b.logpr, file="300000_w_stopword_bigram.RData")

save(trigram.freq, t.logpr, file="300000_w_stopword_REDUCED_trigram.RData")

###########################################################
#Prediction--Test
ngram_predict("We need longer sentences for a good")
ngram_predict("What did you get as a Valentine's")
ngram_predict("What did you get as a Valentine's day")
ngram_predict("05th of May is Cinco de")
ngram_predict("I live in the united")
ngram_predict("I want to love")
ngram_predict("September 2nd is my")
ngram_predict("I am so hungry. I want to get")
ngram_predict("cinco de")
ngram_predict("05th of May is Cinco")
ngram_predict("This is so strange. I can not predict")
ngram_predict("I am going to finish this prediction word. I am sad and I")
ngram_predict("Second Sunday in May is the Happy Mother's")
ngram_predict("July 4th is the United states'")
ngram_predict("In Texas, 05th of May is the famous Cinco")
ngram_predict("Today is my happy")
ngram_predict("I am hungry. Give me a")
ngram_predict("I am hungry. Give me a lot of")
ngram_predict("I am hungry. I want to eat")
ngram_predict("I love")
ngram_predict("I want to sleep with")
ngram_predict("Second Sunday in May is the Mother's")
