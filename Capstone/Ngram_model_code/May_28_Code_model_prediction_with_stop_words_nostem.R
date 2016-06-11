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
        
        ############### Stem or not is a good question###############                 ## Let's not stem, since stemming process takes forever
        ## and it lowers the prediction accuracy
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
        
        # I want to include stopwords, so that
        # I can choose either to show the predicted word without stop words
        # or show the predicted word including the stop words
        stopwords <-readLines("stopwords.txt", warn = FALSE)
        
        # Assume data has already been tokenized using ngram_tokenize and filtered through dictionary. 
        D <- unlist(strsplit(D, split= " "))
        D <- prepross.corpus(D)
        D[!(D %in% word.ref)] <- "oov" # if words not in word.ref set it to oov
       
        L <- length(D)
        N = 4 # use last 2 words to predict next word.
        S <- ifelse(L >= N, L-N+1, ifelse(L >= 1, 1, 0)) 
        # i.e., if L = 10, N = 4, take S = 7. Which means we take 
        # 7th-10th words
        # if L < N = 4, take S =1, whcih means the whole sentence will be
        # used for prediction
        # if L = 1 < N, simply take S = 1
        # We probably also need to rule out the possibility 
        # that L = 0, which means no text input
        
        ## If L > N, S will be L - N +1, we can map out previous two words
        ## called YZ by pasting D[S:(L-1)] and D[(S+1):L]
        
        if (L >= N){ 
                
        # We can then start from pentagram for the initial prediction
        VWXY <- paste(D[L-3], D[L-2], D[L-1], D[L], collapse = " ")
        Z <- word.ref
        
        VWXYZ <- mapply(function(p,q) paste(p,q,collapse = " "), VWXY, Z, USE.NAMES = F)
        
        # Now let's search for the pentagram table
        a <- p.logpr[VWXYZ] # pentagram present in pentagram table
        
        # If there is no corresponding pentagram, it will give NA
        # Which also means count(q_VWXYZ) = 0
        
        # We need each word for prediction later
        V <- D[S]  # This will give the first word of the pentagram
        W <- D[S+1] # This will give the second word 
        X <- D[S+2] # This will give the third word 
        Y <- D[S+3] # This gives the the fourth word 
        
        #We also need WXYZ for prediction when go back to quadgram
        WXYZ <- mapply(function(w,x,y,z) paste(w,x,y,z,collapse = " "), W, X, Y, Z, USE.NAMES = F)
        
        XYZ <- mapply(function(x,y,z) paste(x,y,z,collapse = " "), X, Y, Z, USE.NAMES = F)
        
        WXY <- paste(W,X,Y, collapse = " ")
        
        WX <- paste(W,X, collapse = " ")
        XY <- paste(X,Y, collapse = " ")
        
        # We also need YZ for prediction
        YZ <- mapply(function(y,z) paste(y,z,collapse = " "), Y, Z, USE.NAMES = F)
        
        # Now we have VWXYZ, VWXY, WXYZ, WXY, XYZ, XY, YZ, V, W, X, Y, Z
        # which we will need for prediction
        
        id0 <- (VWXYZ %in% names(pentagram.freq))
        ## Now we want to know if V, W, X, Y are "oov"
        ## We first check if VWXY, WXYZ are in the quadgram table
        
        id1 <- ((!id0) & ( VWXY %in% names(quadgram.freq)) & (WXYZ %in% names(quadgram.freq)))
        
        id2 <- ((!id0) & ( VWXY %in% names(quadgram.freq)) & !(WXYZ %in% names(quadgram.freq)) & (WXY %in% names(trigram.freq)) & ( XYZ %in% names(trigram.freq)))
        
        id3 <- ((!id0) & ( VWXY %in% names(quadgram.freq)) & !(WXYZ %in% names(quadgram.freq)) & (WXY %in% names(trigram.freq)) & !( XYZ %in% names(trigram.freq)) & (XY %in% names(bigram.freq)) & ( YZ %in% names(bigram.freq)))
        
        id4 <- ((!id0) & ( VWXY %in% names(quadgram.freq)) & !(WXYZ %in% names(quadgram.freq)) & (WXY %in% names(trigram.freq)) & !( XYZ %in% names(trigram.freq)) & (XY %in% names(bigram.freq)) & !( YZ %in% names(bigram.freq)))
        
        id5 <-  ((!id0) & ( VWXY %in% names(quadgram.freq)) & !(WXYZ %in% names(quadgram.freq)) & (WXY %in% names(trigram.freq)) & !( XYZ %in% names(trigram.freq)) & !(XY %in% names(bigram.freq)) & ( YZ %in% names(bigram.freq))) # Since WXY in trigram, W, X, Y are all not "oov"
        

        id6 <- ((!id0) & ( VWXY %in% names(quadgram.freq)) & !(WXYZ %in% names(quadgram.freq)) & (WXY %in% names(trigram.freq)) & !( XYZ %in% names(trigram.freq)) & !(XY %in% names(bigram.freq)) & !(YZ %in% names(bigram.freq))) # X and Y can not be "oov" since WXY are in trigram
        
        id7 <- ((!id0) & ( VWXY %in% names(quadgram.freq)) & !(WXYZ %in% names(quadgram.freq)) & !(WXY %in% names(trigram.freq)) & ( XYZ %in% names(trigram.freq)) & (WX %in% names(bigram.freq)) & (XY %in% names(bigram.freq)))
        
        id8 <- ((!id0) & ( VWXY %in% names(quadgram.freq)) & !(WXYZ %in% names(quadgram.freq)) & !(WXY %in% names(trigram.freq)) & ( XYZ %in% names(trigram.freq)) & !(WX %in% names(bigram.freq)) & (XY %in% names(bigram.freq)))
        
        id9 <- ((!id0) & ( VWXY %in% names(quadgram.freq)) & !(WXYZ %in% names(quadgram.freq)) & !(WXY %in% names(trigram.freq)) & ( XYZ %in% names(trigram.freq)) & (WX %in% names(bigram.freq)) & !(XY %in% names(bigram.freq)))
        
        id10 <- ((!id0) & ( VWXY %in% names(quadgram.freq)) & !(WXYZ %in% names(quadgram.freq)) & !(WXY %in% names(trigram.freq)) & ( XYZ %in% names(trigram.freq)) & !(WX %in% names(bigram.freq)) & !(XY %in% names(bigram.freq)))
        id11 <-  ((!id0) & ( VWXY %in% names(quadgram.freq)) & !(WXYZ %in% names(quadgram.freq)) & !(WXY %in% names(trigram.freq)) & !(XYZ %in% names(trigram.freq)) & (WX %in% names(bigram.freq)) & (XY %in% names(bigram.freq)) & (YZ %in% names(bigram.freq)))
        
        id12 <-  ((!id0) & ( VWXY %in% names(quadgram.freq)) & !(WXYZ %in% names(quadgram.freq)) & !(WXY %in% names(trigram.freq)) & !(XYZ %in% names(trigram.freq)) & (WX %in% names(bigram.freq)) & (XY %in% names(bigram.freq)) & !(YZ %in% names(bigram.freq)))
        
        id13 <- ((!id0) & ( VWXY %in% names(quadgram.freq)) & !(WXYZ %in% names(quadgram.freq)) & !(WXY %in% names(trigram.freq)) & !(XYZ %in% names(trigram.freq)) & (WX %in% names(bigram.freq)) & !(XY %in% names(bigram.freq)) & (YZ %in% names(bigram.freq)))
        
        id13.1 <- ((!id0) & ( VWXY %in% names(quadgram.freq)) & !(WXYZ %in% names(quadgram.freq)) & !(WXY %in% names(trigram.freq)) & !(XYZ %in% names(trigram.freq)) & !(WX %in% names(bigram.freq)) & (XY %in% names(bigram.freq)) & (YZ %in% names(bigram.freq)))
        
        id13.2 <- ((!id0) & ( VWXY %in% names(quadgram.freq)) & !(WXYZ %in% names(quadgram.freq)) & !(WXY %in% names(trigram.freq)) & !(XYZ %in% names(trigram.freq)) & !(WX %in% names(bigram.freq)) & !(XY %in% names(bigram.freq)) & (YZ %in% names(bigram.freq)))
        
        
        id13.3 <- ((!id0) & ( VWXY %in% names(quadgram.freq)) & !(WXYZ %in% names(quadgram.freq)) & !(WXY %in% names(trigram.freq)) & !(XYZ %in% names(trigram.freq)) & !(WX %in% names(bigram.freq)) & (XY %in% names(bigram.freq)) & !(YZ %in% names(bigram.freq)))
        
        id13.4 <- ((!id0) & ( VWXY %in% names(quadgram.freq)) & !(WXYZ %in% names(quadgram.freq)) & !(WXY %in% names(trigram.freq)) & !(XYZ %in% names(trigram.freq)) & (WX %in% names(bigram.freq)) & !(XY %in% names(bigram.freq)) & !(YZ %in% names(bigram.freq)))
        
##### Fall back to quadgram
        
        id14 <- ((!id0) & !(VWXY %in% names(quadgram.freq)) & (WXYZ %in% names(quadgram.freq))) ## Fall back to a pure quadgram
        
        id15 <- ((!id0) & !(VWXY %in% names(quadgram.freq)) & !(WXYZ %in% names(quadgram.freq)) & (WXY %in% names(trigram.freq)) & (XYZ %in% names(trigram.freq)))
        
        id16 <- ((!id0) & !(VWXY %in% names(quadgram.freq)) & !(WXYZ %in% names(quadgram.freq)) & (WXY %in% names(trigram.freq)) & !(XYZ %in% names(trigram.freq)) & (XY %in% names(bigram.freq)) & (YZ %in% names(bigram.freq)))
        
        id17 <- ((!id0) & !(VWXY %in% names(quadgram.freq)) & !(WXYZ %in% names(quadgram.freq)) & (WXY %in% names(trigram.freq)) & !(XYZ %in% names(trigram.freq)) & (XY %in% names(bigram.freq)) & !(YZ %in% names(bigram.freq)))
        id18 <- ((!id0) & !(VWXY %in% names(quadgram.freq)) & !(WXYZ %in% names(quadgram.freq)) & (WXY %in% names(trigram.freq)) & !(XYZ %in% names(trigram.freq)) & !(XY %in% names(bigram.freq)) & (YZ %in% names(bigram.freq)))
        id19 <- ((!id0) & !(VWXY %in% names(quadgram.freq)) & !(WXYZ %in% names(quadgram.freq)) & (WXY %in% names(trigram.freq)) & !(XYZ %in% names(trigram.freq)) & !(XY %in% names(bigram.freq)) & !(YZ %in% names(bigram.freq)))
        
## Now start from trigrams
        id20 <- ((!id0) & !(VWXY %in% names(quadgram.freq)) & !(WXYZ %in% names(quadgram.freq)) & !(WXY %in% names(trigram.freq)) & (XYZ %in% names(trigram.freq)) & (WX %in% names(bigram.freq)) & (XY %in% names(bigram.freq)))
        

        id21 <- ((!id0) & !(VWXY %in% names(quadgram.freq)) & !(WXYZ %in% names(quadgram.freq)) & !(WXY %in% names(trigram.freq)) & (XYZ %in% names(trigram.freq)) & !(WX %in% names(bigram.freq)) & (XY %in% names(bigram.freq)))
        
        id22 <- ((!id0) & !(VWXY %in% names(quadgram.freq)) & !(WXYZ %in% names(quadgram.freq)) & !(WXY %in% names(trigram.freq)) & (XYZ %in% names(trigram.freq)) & (WX %in% names(bigram.freq)) & !(XY %in% names(bigram.freq)) )
        
        
        id23 <- ((!id0) & !(VWXY %in% names(quadgram.freq)) & !(WXYZ %in% names(quadgram.freq)) & !(WXY %in% names(trigram.freq)) & (XYZ %in% names(trigram.freq)) & !(WX %in% names(bigram.freq)) & !(XY %in% names(bigram.freq)))
        
        
        ## Start from bigrams
        id24 <- ((!id0) & !(VWXY %in% names(quadgram.freq)) & !(WXYZ %in% names(quadgram.freq)) & !(WXY %in% names(trigram.freq)) & !(XYZ %in% names(trigram.freq)) & (XY %in% names(bigram.freq)) & (YZ %in% names(bigram.freq)))
        
        id25 <- ((!id0) & !(VWXY %in% names(quadgram.freq)) & !(WXYZ %in% names(quadgram.freq)) & !(WXY %in% names(trigram.freq)) & !(XYZ %in% names(trigram.freq)) & (XY %in% names(bigram.freq)) & !(YZ %in% names(bigram.freq)))
        
        id26 <- ((!id0) & !(VWXY %in% names(quadgram.freq)) & !(WXYZ %in% names(quadgram.freq)) & !(WXY %in% names(trigram.freq)) & !(XYZ %in% names(trigram.freq)) & !(XY %in% names(bigram.freq)) & (YZ %in% names(bigram.freq)))
        
   ## Purely unigrams since no n-grams fit        
        id27 <- ((!id0) & !(VWXY %in% names(quadgram.freq)) & !(WXYZ %in% names(quadgram.freq)) & !(WXY %in% names(trigram.freq)) & !(XYZ %in% names(trigram.freq)) & !(XY %in% names(bigram.freq)) & !(YZ %in% names(bigram.freq)))
        
###############################        
# Now let's calculated the log probability for each conidtion        
        

        if(sum(id1) > 0){ a[which(id1)] <- (
                q.logpr[VWXY] + q.logpr[WXYZ[id1]]
        )}
        

        if(sum(id2) > 0){ a[which(id2)] <- (
                q.logpr[VWXY] + t.logpr[WXY] + t.logpr[XYZ[id2]]
        )}
        

        if(sum(id3) > 0){ a[which(id3)] <- (
                q.logpr[VWXY] + t.logpr[WXY] + b.logpr[XY] + b.logpr[YZ[id3]]
        )}
        
        
        if(sum(id4) > 0){ a[which(id4)] <- (
                q.logpr[VWXY] + t.logpr[WXY] + b.logpr[XY] + u.logpr[Y] + u.logpr[Z[id4]]
        )}
        
        
        if(sum(id5) > 0){ a[which(id5)] <-(
                q.logpr[VWXY] + t.logpr[WXY] + u.logpr[X] + u.logpr[Y] + b.logpr[YZ[id5]]
        )}
        

        if(sum(id6) > 0){ a[which(id6)] <-(
                q.logpr[VWXY] + t.logpr[WXY] + u.logpr[X] + u.logpr[Y] + u.logpr[Y] + u.logpr[Z[id6]]
        )}
        

        if(sum(id7) > 0){ a[which(id7)] <-(
                q.logpr[VWXY] + b.logpr[WX] + b.logpr[XY] + t.logpr[XYZ[id7]]
        )}
        

        if(sum(id8) > 0){ a[which(id8)] <-(
                q.logpr[VWXY] + u.logpr[W] + u.logpr[X] + b.logpr[XY] + t.logpr[XYZ[id8]]
        )}
        

        if(sum(id9) > 0){ a[which(id9)] <-(
                q.logpr[VWXY] + u.logpr[X] + u.logpr[Y] + b.logpr[WX] + t.logpr[XYZ[id9]]
        )}
        
        
        if(sum(id10) > 0){ a[which(id10)] <-(
                q.logpr[VWXY] + u.logpr[X] + u.logpr[Y] + u.logpr[W] + u.logpr[X] + t.logpr[XYZ[id10]]
        )}
        

        if(sum(id11) > 0){ a[which(id11)] <-(
                q.logpr[VWXY] + b.logpr[WX] + b.logpr[XY] + b.logpr[YZ[id11]]         )}
        

        if(sum(id12) > 0){ a[which(id12)] <-(
                q.logpr[VWXY] + b.logpr[WX] + b.logpr[XY] + u.logpr[Y] + u.logpr[Z[id12]]
        )}
        

        if(sum(id13) > 0){ a[which(id13)] <-(
                q.logpr[VWXY] + b.logpr[WX] + u.logpr[X] + u.logpr[Y]+ b.logpr[YZ[id13]]
        )}
        
        if(sum(id13.1) > 0){ a[which(id13.1)] <-(
                q.logpr[VWXY] + u.logpr[W] + u.logpr[X] + b.logpr[XY] + b.logpr[YZ[id13.1]]
        )}
        
        if(sum(id13.2) > 0){ a[which(id13.2)] <-(
                q.logpr[VWXY] + u.logpr[W] + u.logpr[X] + u.logpr[X] + u.logpr[Y] + b.logpr[YZ[id13.2]]
        )}
        
        
        if(sum(id13.3) > 0){ a[which(id13.3)] <-(
                q.logpr[VWXY] + u.logpr[W] + u.logpr[X] + b.logpr[XY] + u.logpr[Y] + u.logpr[Z[id13.3]]
        )}
        
        
        if(sum(id13.4) > 0){ a[which(id13.4)] <-(
                q.logpr[VWXY] + b.logpr[WX] + u.logpr[X] + u.logpr[Y] + u.logpr[Y] + u.logpr[Z[id13.4]]
        )}
        

        if(sum(id14) > 0){ a[which(id14)] <-(
                q.logpr[WXYZ[id14]]
        )}
        

        if(sum(id15) > 0){ a[which(id15)] <-(
                t.logpr[WXY] + t.logpr[XYZ[id15]]
        )}
        

        if(sum(id16) > 0){ a[which(id16)] <-(
                t.logpr[WXY] + b.logpr[XY] + b.logpr[YZ[id16]]
        )}
        
        if(sum(id17) > 0){ a[which(id17)] <-(
                t.logpr[WXY] + b.logpr[XY] + u.logpr[Y] + u.logpr[Z[id17]]
        )}
        
        if(sum(id18) > 0){ a[which(id18)] <-(
                t.logpr[WXY] + u.logpr[X] + u.logpr[Y] + b.logpr[YZ[id18]]
        )}
        
        if(sum(id19) > 0){ a[which(id19)] <-(
                t.logpr[WXY] + u.logpr[X] + 2*u.logpr[Y] + u.logpr[Z[id19]]
        )}
        
        
        if(sum(id20) > 0){ a[which(id20)] <-(
                t.logpr[XYZ[id20]] + b.logpr[WX] + b.logpr[XY]
        )}
        
        
        if(sum(id21) > 0){ a[which(id21)] <-(
                t.logpr[XYZ[id21]] + u.logpr[W] + u.logpr[X] + b.logpr[XY]
        )}
        
        
        if(sum(id22) > 0){ a[which(id22)] <-(
                t.logpr[XYZ[id22]] + b.logpr[WX] + u.logpr[X] + u.logpr[Y]
        )}
        
        
        if(sum(id23) > 0){ a[which(id23)] <-(
                t.logpr[XYZ[id23]] + u.logpr[W] + 2*u.logpr[X] + u.logpr[Y]
        )}
        
        
        if(sum(id24) > 0){ a[which(id24)] <-(
                b.logpr[XY] + b.logpr[YZ[id24]]
        )}
        
        if(sum(id25) > 0){ a[which(id25)] <-(
                b.logpr[XY] + u.logpr[Y] + u.logpr[Z[id25]]
        )}
        

        if(sum(id26) > 0){ a[which(id26)] <-(
                u.logpr[X] + u.logpr[Y] + b.logpr[YZ[id26]]
        )}
        
        if(sum(id27) > 0){ a[which(id27)] <-(
                u.logpr[X] + 2 * u.logpr[Y] + u.logpr[Z[id27]]
        )}
        
################        
        # pick the word with largest log q_VWXYZ
        names(a) <- word.ref
        a <- a[!is.na(a)]
        a <- sort(a, decreasing = T)
        
        print("If we include stop words, the first five words are")
        print(names(a)[1:5])
        
        a <- a[!(names(a) %in% stopwords)]
        a <- sort(a, decreasing = T)      
        
        print("If we ignore stop words, the first five words are")
        print(names(a)[1:5])
        } 
### ------- ###        
        else if (L == 3){
        WXY <- paste( D[L-2], D[L-1],D[L], collapse = " ")
        Z <- word.ref
        
        WXYZ <- mapply(function(p,q) paste(p,q,collapse = " "), WXY, Z, USE.NAMES = F)
        
        # Now let's search for the pentagram table
        a <- q.logpr[WXYZ] # pentagram present in pentagram table
        
        W <- D[S]  # This will give the first word of the pentagram
        X <- D[S+1] # This will give the second word 
        Y <- D[S+2] # This will give the third word 

        #We also need XYZ for prediction when go back to quadgram
        XYZ <- mapply(function(x,y,z) paste(x,y,z,collapse = " "), X, Y, Z, USE.NAMES = F)
        

        XY <- paste(X,Y, collapse = " ")
        WX <- paste(W, X, collapse = " ")
        # We also need YZ for prediction
        YZ <- mapply(function(y,z) paste(y,z,collapse = " "), Y, Z, USE.NAMES = F)
        id0 <- (WXYZ %in% names(quadgram.freq))

        id1 <- ((!id0) & ( WXY %in% names(trigram.freq)) & (XYZ %in% names(trigram.freq)))
        
        id2 <- ((!id0) & ( WXY %in% names(trigram.freq)) & !(XYZ %in% names(trigram.freq)) & (XY %in% names(bigram.freq)) & ( YZ %in% names(bigram.freq)))
        
        id3 <- ((!id0) & ( WXY %in% names(trigram.freq)) & !(XYZ %in% names(trigram.freq)) & (XY %in% names(bigram.freq)) & !(YZ %in% names(bigram.freq)))
        
        id4 <- ((!id0) & ( WXY %in% names(trigram.freq)) & !(XYZ %in% names(trigram.freq)) & !(XY %in% names(bigram.freq)) & ( YZ %in% names(bigram.freq)))
        
        id5 <-  ((!id0) & ( WXY %in% names(trigram.freq)) & !(XYZ %in% names(trigram.freq)) & !(XY %in% names(bigram.freq)) & !(YZ %in% names(bigram.freq))) 
        id6 <-  ((!id0) & !(WXY %in% names(trigram.freq)) & (XYZ %in% names(trigram.freq)) & (WX %in% names(bigram.freq)) & (XY %in% names(bigram.freq))) 
        id7 <-  ((!id0) & !(WXY %in% names(trigram.freq)) & (XYZ %in% names(trigram.freq)) & (WX %in% names(bigram.freq)) & !(XY %in% names(bigram.freq))) 
        id8 <-  ((!id0) & !(WXY %in% names(trigram.freq)) & (XYZ %in% names(trigram.freq)) & !(WX %in% names(bigram.freq)) & (XY %in% names(bigram.freq))) 
        id9 <-  ((!id0) & !(WXY %in% names(trigram.freq)) & (XYZ %in% names(trigram.freq)) & !(WX %in% names(bigram.freq)) & !(XY %in% names(bigram.freq))) 
        id10 <-  ((!id0) & !(WXY %in% names(trigram.freq)) & !(XYZ %in% names(trigram.freq)) & (XY %in% names(bigram.freq)) & (YZ %in% names(bigram.freq))) 
        id11 <-  ((!id0) & !(WXY %in% names(trigram.freq)) & !(XYZ %in% names(trigram.freq)) & (XY %in% names(bigram.freq)) & !(YZ %in% names(bigram.freq))) 
        id12 <-  ((!id0) & !(WXY %in% names(trigram.freq)) & !(XYZ %in% names(trigram.freq)) & !(XY %in% names(bigram.freq)) & (YZ %in% names(bigram.freq))) 
        id13 <-  ((!id0) & !(WXY %in% names(trigram.freq)) & !(XYZ %in% names(trigram.freq)) & !(XY %in% names(bigram.freq)) & !(YZ %in% names(bigram.freq))) 


##### Calculate the log probability for each condition        
        if(sum(id1) > 0){ a[which(id1)] <-(
                t.logpr[WXY] + t.logpr[XYZ[id1]]
        )}
        
        if(sum(id2) > 0){ a[which(id2)] <-(
                t.logpr[WXY] + b.logpr[XY] + b.logpr[YZ[id2]]
        )}
        
        if(sum(id3) > 0){ a[which(id3)] <-(
                t.logpr[WXY] + b.logpr[XY] + u.logpr[Y] + u.logpr[Z[id3]]
        )}
        
        if(sum(id4) > 0){ a[which(id4)] <-(
                t.logpr[WXY] + u.logpr[X] + u.logpr[Y] + b.logpr[YZ[id4]]
        )}
        
        if(sum(id5) > 0){ a[which(id5)] <-(
                t.logpr[WXY] + u.logpr[X] +2* u.logpr[Y] + u.logpr[Z[id5]]
        )}
        
        if(sum(id6) > 0){ a[which(id6)] <-(
                t.logpr[XYZ[id6]] + b.logpr[WX] + b.logpr[XY]
        )}
        
        if(sum(id7) > 0){ a[which(id7)] <-(
                t.logpr[XYZ[id7]] + b.logpr[WX] + u.logpr[X] + u.logpr[Y]
        )}
        
        if(sum(id8) > 0){ a[which(id8)] <-(
                t.logpr[XYZ[id8]] + b.logpr[XY] + u.logpr[W] + u.logpr[X]
        )}
        
        if(sum(id9) > 0){ a[which(id9)] <-(
                t.logpr[XYZ[id9]] + u.logpr[W] + 2*u.logpr[X] + u.logpr[Y]
        )}
        
        if(sum(id10) > 0){ a[which(id10)] <-(
                b.logpr[XY] + b.logpr[YZ[id10]]
        )}
        
        if(sum(id11) > 0){ a[which(id11)] <-(
                b.logpr[XY] + u.logpr[Y] + u.logpr[Z[id11]]
        )}
        
        if(sum(id12) > 0){ a[which(id12)] <-(
                b.logpr[YZ[id12]] + u.logpr[X] + u.logpr[Y]
        )}
        
        if(sum(id13) > 0){ a[which(id13)] <-(
                u.logpr[X] + 2 * u.logpr[Y] + u.logpr[Z[id13]]
        )}
        
#############        
        # pick the word with largest log q_WXYZ
        names(a) <- word.ref
        a <- a[!is.na(a)]
        a <- sort(a, decreasing = T)
        
        print("If we include stop words, the first five words are")
        print(names(a)[1:5])
        
        a <- a[!(names(a) %in% stopwords)]
        a <- sort(a, decreasing = T)      
        
        print("If we ignore stop words, the first five words are")
        print(names(a)[1:5])
        }  
######---------------        
        else if (L==2){
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
                
                ## Get each word
                X <- D[S] # This will give the first words of a trigram
                Y <- D[S+1] # This gives the second word of a trigram
                
                # We also need YZ for prediction
                YZ <- mapply(function(y,z) paste(y,z,collapse = " "), Y, Z, USE.NAMES = F)
                id0 <- (XYZ %in% names(trigram.freq))

                ## We first check if XY and YZ are in the bigram table
                id1 <- ((!id0) & ( XY %in% names(bigram.freq)) & (YZ %in% names(bigram.freq)))
                
                id2 <-  ((!id0) & ( XY %in% names(bigram.freq)) & !(YZ %in% names(bigram.freq))) 
                
                id3 <-  ((!id0) & !(XY %in% names(bigram.freq)) & (YZ %in% names(bigram.freq))) 
                
                id4 <-  ((!id0) & !(XY %in% names(bigram.freq)) & !(YZ %in% names(bigram.freq))) 
                
####### Calculate log probability for each condition                
                if(sum(id1) > 0){ a[which(id1)] <- (
                        b.logpr[XY] + b.logpr[YZ[id1]]
                ) }
                
                
                if(sum(id2) > 0){ a[which(id2)] <- (
                        b.logpr[XY] + u.logpr[Y] + u.logpr[Z[id2]]
                )}
                
                
                if(sum(id3) > 0){ a[which(id3)] <- (
                        u.logpr[X] + u.logpr[Y] + b.logpr[YZ[id3]]
                )}
                
                
                if(sum(id4) > 0){ a[which(id4)] <- (
                        u.logpr[X] + u.logpr[Y] + u.logpr[Y] + u.logpr[Z[id4]]
                )}
                
###########                
                # pick the word with largest log q_XYZ
                names(a) <- word.ref
                a <- a[!is.na(a)]
                a <- sort(a, decreasing = T)
                
                print("If we include stop words, the first five words are")
                print(names(a)[1:5])
                
                a <- a[!(names(a) %in% stopwords)]
                a <- sort(a, decreasing = T)      
                
                print("If we ignore stop words, the first five words are")
                print(names(a)[1:5])
        } 
#######------------------------        
        else if(L==1){    
                # the length of the corpus will be simply 1
                # The best model we can use is 'bigram'
                P <- D[L]
                Q <- word.ref
                # Let's paste the input word P and the word.ref
                # to create a list of bigram PQ
                PQ <- mapply(function(p,q) paste(p,q,collapse = " "), P, Q, USE.NAMES = F)
                b <- b.logpr[PQ] # search for bigram table
                b.na_id <- which(is.na(b)) # locate the na

                p.id0 <- (PQ %in% names(bigram.freq))
                # For location of na, there are two situations

                #check if the input word is "oov"
                p.id1 <- (!p.id0)
                
                if(sum(p.id1)>0){b[p.id1] <- (
                        u.logpr[P] + u.logpr[Q[p.id1]]
                )}
                
                
                names(b) <- word.ref
                b <- sort(b, decreasing = T)
                
                print("If we include stop words, the first five words are")
                print(names(b)[1:5])
                
                b <- b[!(names(b) %in% stopwords)]
                b <- sort(b, decreasing = T)
                print("If we ignore stop words, the first five words are")
                print(names(b)[1:5])
                
        } else {print("Please enter at least one word!")}
}


##############################################################

##Let's pull out parts of the text files to be the training set
sp.n = 100 # sample size for each text file
nloop = 2000  ## total number of lines read is sp.n*nloop
M <- matrix(1:(sp.n*nloop),nrow=sp.n, ncol = nloop)

##open null vectors for appending
unigram.total.freq <- c()
bigram.total.freq <- c()
trigram.total.freq <- c()
quadgram.total.freq <- c()
pentagram.total.freq <- c()

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

##################################################
## 4-grams
quadgram.dtm <- clean.corpus.dtm(cleancorpus,4) ##generate sample 1 dtm

quadgram.dtm <- rollup(quadgram.dtm, 1, na.rm=TRUE, FUN = sum)
quadgram.freq <- colSums(as.matrix(quadgram.dtm))


## Now let's give the top two-words occuring most frequently
quadgram.freq <- sort(quadgram.freq, decreasing = T)
total.quadgram.freq <- sum(quadgram.freq)

##################################################
## 5-grams
pentagram.dtm <- clean.corpus.dtm(cleancorpus,5) ##generate sample 1 dtm

pentagram.dtm <- rollup(pentagram.dtm, 1, na.rm=TRUE, FUN = sum)
pentagram.freq <- colSums(as.matrix(pentagram.dtm))


## Now let's give the top two-words occuring most frequently
pentagram.freq <- sort(pentagram.freq, decreasing = T)
total.pentagram.freq <- sum(pentagram.freq)


unigram.total.freq <- c(unigram.total.freq, unigram.freq)
bigram.total.freq <- c(bigram.total.freq, bigram.freq)
trigram.total.freq <- c(trigram.total.freq, trigram.freq)
quadgram.total.freq <- c(quadgram.total.freq, quadgram.freq)
pentagram.total.freq <- c(pentagram.total.freq, pentagram.freq)
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

b.in.word.ref <- ((b.first.wd %in% word.ref) & (b.second.wd %in% word.ref))
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
## We now want to filter out the trigrams containing words out of 
## word.ref
t.first.wd <- sapply(strsplit(names(trigram.freq), split = " "), function(x){x[1]} ) ## split the trigram words and extract the first word

t.second.wd <- sapply(strsplit(names(trigram.freq), split = " "), function(x){x[2]})

t.third.wd <- sapply(strsplit(names(trigram.freq), split = " "), function(x){x[3]})

t.in.word.ref <- ((t.first.wd %in% word.ref) & (t.second.wd %in% word.ref) & (t.third.wd %in% word.ref))

trigram.freq <- trigram.freq[t.in.word.ref]

## Re-create the trigrams data frame
trigram <- data.frame(trigram.word=names(trigram.freq), trigram.freq=trigram.freq, row.names = NULL) 
trigram[,1] <- as.character(trigram[,1])

trigram <- arrange(trigram, desc(trigram.freq))
trigram.freq <- trigram[,2]
names(trigram.freq) <- trigram[,1]
head(trigram,100)

################################################
## make "complete" quadgram.wf
## create the quadgrams data frame
quadgram <- data.frame(quadgram.word=names(quadgram.total.freq), quadgram.freq= quadgram.total.freq, row.names = NULL) 


## use reshape2 package
q.melt <- melt(quadgram, id = "quadgram.word", measure.vars = "quadgram.freq")
quadgram <- dcast(q.melt, quadgram.word ~ variable, sum)

quadgram.freq <- quadgram[,2]
names(quadgram.freq) <- quadgram[,1]

## We now want to filter out the quadgrams containing words out of 
## word.ref
q.first.wd <- sapply(strsplit(names(quadgram.freq), split = " "), function(x){x[1]} ) ## split the quadgram words and extract the first word

q.second.wd <- sapply(strsplit(names(quadgram.freq), split = " "), function(x){x[2]})

q.third.wd <- sapply(strsplit(names(quadgram.freq), split = " "), function(x){x[3]})

q.fourth.wd <- sapply(strsplit(names(quadgram.freq), split = " "), function(x){x[4]})

q.in.word.ref <- ((q.first.wd %in% word.ref) & (q.second.wd %in% word.ref) & (q.third.wd %in% word.ref) &(q.fourth.wd %in% word.ref))

quadgram.freq <- quadgram.freq[q.in.word.ref]

## Re-create the trigrams data frame
quadgram <- data.frame(quadgram.word=names(quadgram.freq), quadgram.freq=quadgram.freq, row.names = NULL) 
quadgram[,1] <- as.character(quadgram[,1])

quadgram <- arrange(quadgram, desc(quadgram.freq))
quadgram.freq <- quadgram[,2]
names(quadgram.freq) <- quadgram[,1]
head(quadgram,100)

################################################
## make "complete" pentagram.wf
## create the pentagrams data frame
pentagram <- data.frame(pentagram.word=names(pentagram.total.freq), pentagram.freq= pentagram.total.freq, row.names = NULL) 


## use reshape2 package
p.melt <- melt(pentagram, id = "pentagram.word", measure.vars = "pentagram.freq")
pentagram <- dcast(p.melt, pentagram.word ~ variable, sum)

pentagram.freq <- pentagram[,2]
names(pentagram.freq) <- pentagram[,1]

## We now want to filter out the pentagrams containing words out of 
## word.ref
p.first.wd <- sapply(strsplit(names(pentagram.freq), split = " "), function(x){x[1]} ) ## split the pentagram words and extract the first word

p.second.wd <- sapply(strsplit(names(pentagram.freq), split = " "), function(x){x[2]})

p.third.wd <- sapply(strsplit(names(pentagram.freq), split = " "), function(x){x[3]})

p.fourth.wd <- sapply(strsplit(names(pentagram.freq), split = " "), function(x){x[4]})

p.fifth.wd <- sapply(strsplit(names(pentagram.freq), split = " "), function(x){x[5]})

p.in.word.ref <- ((p.first.wd %in% word.ref) & (p.second.wd %in% word.ref) & (p.third.wd %in% word.ref) & (p.fourth.wd %in% word.ref) & (p.fifth.wd %in% word.ref))

pentagram.freq <- pentagram.freq[p.in.word.ref]

## Re-create the trigrams data frame
pentagram <- data.frame(pentagram.word=names(pentagram.freq), pentagram.freq=pentagram.freq, row.names = NULL) 
pentagram[,1] <- as.character(pentagram[,1])

pentagram <- arrange(pentagram, desc(pentagram.freq))
pentagram.freq <- pentagram[,2]
names(pentagram.freq) <- pentagram[,1]
head(pentagram,100)

##################################################

# Calculate unigram log q(w) = log(add lambda smoothing), where lambda =1
# corresponds to the usual add one smoothing
N <- sum(unigram_90[,2]) # Total number of tokens (words)
# N <- sum(unigram_90.freq) # unigram_90.freq = unigram_90[,2]
V <- max.line # Number of unique words in the dictionary    

## ?? What's the best value for lambda??????
lambda <- 0.04  # add-lambda smoothing = generalized add-one appraoch
                # We need to check what value of lambda optimize the 
                # the accuracy

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

###############################################

# Calculate quadgram log likelihood
qg.names <- names(quadgram.freq)

qg.1to3.wd <- sapply(strsplit(qg.names, split = " "), function(x){
        paste(x[1:3], collapse = " ")
}) ## split the quadgram and extract the first, 2nd, and third words

qg.2to4.wd <- sapply(strsplit(qg.names, split = " "), function(x){
        paste(x[2:4], collapse = " ")
}) ## split the quadgram and extract the 2nd, 3rd, and 4th words

#qg.1st2.wd <- sapply(strsplit(qg.names, split = " "), function(x){
#        paste(x[1:2], collapse = " ")
#} ) ## split the quadgram and extract the 1st and 2nd word
qg.2nd3.wd <- sapply(strsplit(qg.names, split = " "), function(x){
        paste(x[2:3], collapse = " ")
} ) ## split the quadgram and extract the 2nd and 3rd word

qg.3rd4.wd <- sapply(strsplit(qg.names, split = " "), function(x){
        paste(x[3:4], collapse = " ")
} ) ## split the quadgram and extract the 2nd and 3rd word

#qg.2nd.wd <- sapply(strsplit(qg.names, split = " "), function(x){x[2]})
## split the quadgram and extract the 2nd word

qg.3rd.wd <- sapply(strsplit(qg.names, split = " "), function(x){x[3]})
## split the qudgram and extract the 3rd word

qg.4th.wd <- sapply(strsplit(qg.names, split = " "), function(x){x[4]})
## split the qudgram and extract the 4th word

# Remind N = total number of tokens (words)

# Remind V = Number of unique words in the dictionary    

# get the last word unigram add-lambda q_w
q_w <- (unigram_90.freq[qg.4th.wd] + lambda)/(N + lambda * V)

# get the last two words bigram add-lambda q_w
q_vw <- (bigram.freq[qg.3rd4.wd] + lambda * V *q_w)/(unigram_90.freq[qg.3rd.wd] + lambda * V)

# the trigram add-lambda formula
q_uvw <- (trigram.freq[qg.2to4.wd] + lambda * V * q_vw)/(bigram.freq[qg.2nd3.wd] + lambda * V)

# Finally the quadgram add-lambda formula
q_buvw <- (quadgram.freq + lambda * V * q_uvw)/(trigram.freq[qg.1to3.wd] + lambda * V)

q.logpr <- log(q_buvw)

q.count <- quadgram[,2] + lambda
quadgram[,3]<- q.count
quadgram[,4] <- q.logpr
names(quadgram) <- c("quadgram", "quadgram.freq","lambda.smooth","q.logpr")

names(q.logpr) <- quadgram[,1]
names(q.count) <- quadgram[,1]
print(head(quadgram,10))
print(tail(quadgram,10))

###############################################

# Calculate pentagram log likelihood
pg.names <- names(pentagram.freq)

pg.1to4.wd <- sapply(strsplit(pg.names, split = " "), function(x){
        paste(x[1:4], collapse = " ")
}) ## split the pentagram and extract the first, 2nd, third, fourth words

pg.2to5.wd <- sapply(strsplit(pg.names, split = " "), function(x){
        paste(x[2:5], collapse = " ")
}) ## split the pentagram and extract the 2nd, third, fourth and fifth words

#pg.1to3.wd <- sapply(strsplit(pg.names, split = " "), function(x){
#        paste(x[1:3], collapse = " ")
#}) ## split the pentagram and extract the first, 2nd, and third words

pg.2to4.wd <- sapply(strsplit(pg.names, split = " "), function(x){
        paste(x[2:4], collapse = " ")
}) ## split the pentagram and extract the 2nd, 3rd, and 4th words

pg.3to5.wd <- sapply(strsplit(pg.names, split = " "), function(x){
        paste(x[3:5], collapse = " ")
}) ## split the pentagram and extract the 3rd, 4th, and 5th words

#pg.1st2.wd <- sapply(strsplit(pg.names, split = " "), function(x){
#        paste(x[1:2], collapse = " ")
#} ) ## split the quadgram and extract the 1st and 2nd word

#pg.2nd3.wd <- sapply(strsplit(pg.names, split = " "), function(x){
#        paste(x[2:3], collapse = " ")
#} ) ## split the pentagram and extract the 2nd and 3rd word

pg.3rd4.wd <- sapply(strsplit(pg.names, split = " "), function(x){
        paste(x[3:4], collapse = " ")
} ) ## split the pentagram and extract the 2nd and 3rd word

pg.4th5.wd <- sapply(strsplit(pg.names, split = " "), function(x){
        paste(x[4:5], collapse = " ")
} ) ## split the pentagram and extract the 4th and 5th word

#pg.2nd.wd <- sapply(strsplit(pg.names, split = " "), function(x){x[2]})
## split the pentagram and extract the 2nd word

#pg.3rd.wd <- sapply(strsplit(pg.names, split = " "), function(x){x[3]})
## split the pentagram and extract the 3rd word

pg.4th.wd <- sapply(strsplit(pg.names, split = " "), function(x){x[4]})
## split the pentagram and extract the 4th word

pg.5th.wd <- sapply(strsplit(pg.names, split = " "), function(x){x[5]})
## split the pentagram and extract the 5th word

# Remind N = total number of tokens (words)

# Remind V = Number of unique words in the dictionary    

# get the last word unigram add-lambda q_w
p_w <- (unigram_90.freq[pg.5th.wd] + lambda)/(N + lambda * V)

# get the last two words bigram add-lambda q_w
p_vw <- (bigram.freq[pg.4th5.wd] + lambda * V *p_w)/(unigram_90.freq[pg.4th.wd] + lambda * V)

# the trigram add-lambda formula
p_uvw <- (trigram.freq[pg.3to5.wd] + lambda * V * p_vw)/(bigram.freq[pg.3rd4.wd] + lambda * V)

# The quadgram add-lambda formula
p_buvw <- (quadgram.freq[pg.2to5.wd] + lambda * V * p_uvw)/(trigram.freq[pg.2to4.wd] + lambda * V)


# The pendtagram add-lambda formula
p_abuvw <- (pentagram.freq + lambda * V * p_buvw)/(quadgram.freq[pg.1to4.wd] + lambda * V)

p.logpr <- log(p_abuvw)

p.count <- pentagram[,2] + lambda
pentagram[,3]<- p.count
pentagram[,4] <- p.logpr
names(pentagram) <- c("pentagram", "pentagram.freq","lambda.smooth","p.logpr")

names(p.logpr) <- pentagram[,1]
names(p.count) <- pentagram[,1]
print(head(pentagram,10))
print(tail(pentagram,10))

##############################################
##############################################

summary(u.logpr)
summary(b.logpr)
summary(t.logpr)
summary(q.logpr)
summary(p.logpr)


###############################################

par(mfrow=c(1,5))
hist(u.logpr,xlab="Unigram log prob", main= "Histogram of Unigram log prob",col = "blue")
hist(b.logpr,xlab="Bigram log prob", main = "Histogram of Bigram log prob",col = "green")
hist(t.logpr,xlab="Trigram log prob", main = "Histogram of Trigram log prob", col = "salmon")
hist(q.logpr,xlab="Quadgram log prob", main = "Histogram of Quadgram log prob", col = "salmon")
hist(p.logpr,xlab="Pentagram log prob", main = "Histogram of Pentagram log prob", col = "salmon")

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

ulp <- u.logpr[val_set] ## This gives the log prob of all words
u.perp <- exp(-sum(ulp)/length(ulp)) ## This is the unigram perplexity
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

## Fall back to bigram
t.id2 <- ( (!t.id1) & (PQ %in% names(bigram.freq)) & (QR %in% names(bigram.freq)))

t.id3.1 <- ( (!t.id1) & (PQ %in% names(bigram.freq)) & !(QR %in% names(bigram.freq)) & (R !="oov"))

t.id3.2 <- ( (!t.id1) & (PQ %in% names(bigram.freq)) & !(QR %in% names(bigram.freq)) & (R =="oov"))

## only need to consider the last two words!
t.id4 <- ( (!t.id1) & !(PQ %in% names(bigram.freq)) & (QR %in% names(bigram.freq)) )


## Fall back to unigram, only last two words need considering
t.id5.1 <- ( (!t.id1) & !(PQ %in% names(bigram.freq)) & !(QR %in% names(bigram.freq)) & (Q != "oov") & (R == "oov"))

t.id5.2 <- ( (!t.id1) & !(PQ %in% names(bigram.freq)) & !(QR %in% names(bigram.freq)) & (Q != "oov") & (R != "oov"))

t.id5.3 <- ( (!t.id1) & !(PQ %in% names(bigram.freq)) & !(QR %in% names(bigram.freq)) & (Q == "oov") & (R != "oov"))

t.id5.4 <- ( (!t.id1) & !(PQ %in% names(bigram.freq)) & !(QR %in% names(bigram.freq)) & (Q == "oov") & (R == "oov")) 


## We now calculate the log q(uvw)
b <- rep(0,length(PQR))

if(sum(t.id1)>0) { b[t.id1] <- t.logpr[PQR[t.id1]] } # existing trigram

if(sum(t.id2)>0) { b[t.id2] <- log(lambda*V*((bigram.freq[QR[t.id2]]+lambda * V*((unigram_90.freq[R[t.id2]] + lambda)/(N + lambda * V)))/(unigram_90.freq[Q[t.id2]]+ lambda * V))/(bigram.freq[PQ[t.id2]] + lambda * V)) }


if(sum(t.id3.1)>0) { b[t.id3.1] <- log(lambda * V *((lambda * V *((unigram_90.freq[R[t.id3.1]] + lambda)/(N + lambda * V)))/(u.count["oov"]+ lambda * V)))/(bigram.freq[PQ[t.id3.1]] + lambda * V)}


if(sum(t.id3.2)>0){ b[t.id3.2] <- log((lambda * V *((lambda * V *((u.count["oov"] + lambda)/(N + lambda * V)))/(unigram_90.freq[Q[t.id3.2]] + lambda * V)))/(bigram.freq[PQ[t.id3.2]] + lambda * V))}



if(sum(t.id4)>0){ b[t.id4] <- log((bigram.freq[QR[t.id4]]+ lambda * V*((unigram_90.freq[R[t.id4]]+lambda)/(N + lambda*V)))/(unigram_90.freq[Q[t.id4]] + lambda * V))}



if(sum(t.id5.1) >0){b[t.id5.1] <- log((lambda * V)*((u.count["oov"] + lambda)/(N + lambda * V))/(unigram_90.freq[Q[t.id5.1]] + lambda * V))}


if(sum(t.id5.2) >0){b[t.id5.2] <- log((lambda * V)*((unigram_90.freq[R[t.id5.2]] + lambda)/(N + lambda * V))/(unigram_90.freq[Q[t.id5.2]] + lambda * V))}



if(sum(t.id5.3)>0){b[t.id5.3] <- log((lambda * V *((unigram_90.freq[R[t.id5.3]] + lambda)/(N + lambda * V)))/(u.count["oov"] + lambda * V))}


if(sum(t.id5.4) >0){ b[t.id5.4] <- log((lambda*V*((u.count["oov"] + lambda)/(N + lambda * V)))/(u.count["oov"] + lambda * V))}


t.perp <- exp(-sum(b)/length(b))

print(t.perp)


#######################################################
## Quadgram model with backoff
# If quadgram exist in the quadgram table then use quadgram log 
# probability else for each trigram if trigrams exists in
# trigram table then use trigram log probability.
# Otherwise, use bigram log probabligy.
# Else if first word is present in dictionary then use 
# smoothed conditional probability
# Else back off to unigram probabilities across all words

BPQR <- mapply(function(b,p,q,r){paste(b, p, q, r, sep = " ")}, val_set[1:(length(val_set)-3)], val_set[2:(length(val_set)-2)], val_set[3:(length(val_set)-1)], val_set[4:length(val_set)]) ## create quadgrams

BPQ <- sapply(strsplit(BPQR,split = " "), function(x){ paste(x[[1]],x[[2]],x[[3]], sep =" ")}) # 1st-3rd words of quadgrams

PQR <- sapply(strsplit(BPQR,split = " "), function(x){ paste(x[[2]],x[[3]],x[[4]], sep =" ")}) # 2nd-4th words of quadgrams

BP <- sapply(strsplit(BPQR,split = " "), function(x){ paste(x[[1]], x[[2]], sep =" ")}) # 1st and 2nd words of quadgrams

PQ <- sapply(strsplit(BPQR,split = " "), function(x){ paste(x[[2]], x[[3]], sep =" ")}) # 2nd and 3rd words of quadgrams

QR <- sapply(strsplit(BPQR,split = " "), function(x){ paste(x[[3]], x[[4]], sep =" ")}) # 3rd and 4th words of quadgram

B <-  sapply(strsplit(BPQR, split = " "), function(x) {x[[1]]}) # give 1st words

P <-  sapply(strsplit(BPQR, split = " "), function(x) {x[[2]]}) # give 2nd words

Q <-  sapply(strsplit(BPQR, split = " "), function(x) {x[[3]]}) # give 3rd words

R <-  sapply(strsplit(BPQR, split = " "), function(x) {x[[4]]}) # give last words


### Many possible situations below
q.id1 <- (BPQR %in% names(quadgram.freq)) # counted quadgram

q.id2 <- ((!q.id1) & (BPQ %in% names(trigram.freq)) & (PQR %in% names(trigram.freq)))

q.id3 <- ((!q.id1) & (BPQ %in% names(trigram.freq)) & !(PQR %in% names(trigram.freq)) & (PQ %in% names(bigram.freq)) & (QR %in% names(bigram.freq)))

q.id3.1 <- ((!q.id1) & (BPQ %in% names(trigram.freq)) & !(PQR %in% names(trigram.freq)) & (PQ %in% names(bigram.freq)) & !(QR %in% names(bigram.freq)) & (R != "oov"))

q.id3.2 <- ((!q.id1) & (BPQ %in% names(trigram.freq)) & !(PQR %in% names(trigram.freq)) & (PQ %in% names(bigram.freq)) & !(QR %in% names(bigram.freq)) & (R == "oov"))

## Fall back to trigram below
q.id4 <- ((!q.id1) & !(BPQ %in% names(trigram.freq)) & (PQR %in% names(trigram.freq))) # go back to trigram ! Only need to consider PQR !


## Below fall back to bigram
q.id5 <- ((!q.id1) & !(BPQ %in% names(trigram.freq)) & !(PQR %in% names(trigram.freq)) & (PQ %in% names(bigram.freq)) & (QR %in% names(bigram.freq)))
# Fall back to bigram only!! Still need to know if PQ %in% bigram.freq

q.id6.1 <- ((!q.id1) & !(BPQ %in% names(trigram.freq)) & !(PQR %in% names(trigram.freq)) & (PQ %in% names(bigram.freq)) & !(QR %in% names(bigram.freq)) & (Q != "oov") & (R != "oov"))

q.id6.2 <- ((!q.id1) & !(BPQ %in% names(trigram.freq)) & !(PQR %in% names(trigram.freq)) & (PQ %in% names(bigram.freq)) & !(QR %in% names(bigram.freq)) & (Q != "oov") & (R == "oov"))

q.id6.3 <- ((!q.id1) & !(BPQ %in% names(trigram.freq)) & !(PQR %in% names(trigram.freq)) & !(PQ %in% names(bigram.freq)) & (QR %in% names(bigram.freq))) # Only need to consider last two words!

## Below fall back to unigram only
q.id7 <- ((!q.id1) & !(BPQ %in% names(trigram.freq)) & !(PQR %in% names(trigram.freq)) & !(PQ %in% names(bigram.freq)) & !(QR %in% names(bigram.freq)) & (Q != "oov") & (R != "oov"))


q.id7.1 <- ((!q.id1) & !(BPQ %in% names(trigram.freq)) & !(PQR %in% names(trigram.freq)) & !(PQ %in% names(bigram.freq)) & !(QR %in% names(bigram.freq)) & (Q == "oov") & (R != "oov"))

q.id7.2 <- ((!q.id1) & !(BPQ %in% names(trigram.freq)) & !(PQR %in% names(trigram.freq)) & !(PQ %in% names(bigram.freq)) & !(QR %in% names(bigram.freq)) & (Q != "oov") & (R == "oov"))

q.id7.3 <- ((!q.id1) & !(BPQ %in% names(trigram.freq)) & !(PQR %in% names(trigram.freq)) & !(PQ %in% names(bigram.freq)) & !(QR %in% names(bigram.freq)) & (Q == "oov") & (R == "oov"))


## We now calculate the log q(buvw)

d <- rep(0,length(BPQR))

if(sum(q.id1)>0) { d[q.id1] <- q.logpr[BPQR[q.id1]] } # existing trigram


if(sum(q.id2)>0) { d[q.id2] <- log((lambda*V*(trigram.freq[PQR[q.id2]] + lambda * V *((bigram.freq[QR[q.id2]] + lambda * V*(unigram_90.freq[R[q.id2]]+lambda)/(N + lambda * V))/(unigram_90.freq[Q[q.id2]] + lambda * V)))/(bigram.freq[PQ[q.id2]]+lambda*V))/(trigram.freq[BPQ[q.id2]] + lambda * V))}



if(sum(q.id3)>0) { d[q.id3] <- log((lambda*V*( lambda * V *((bigram.freq[QR[q.id3]] + lambda * V*(unigram_90.freq[R[q.id3]]+lambda)/(N + lambda * V))/(unigram_90.freq[Q[q.id3]] + lambda * V)))/(bigram.freq[PQ[q.id3]]+lambda*V))/(trigram.freq[BPQ[q.id3]] + lambda * V))}


if(sum(q.id3.1)>0) { d[q.id3.1] <- log((lambda*V*(lambda * V *((lambda * V*(unigram_90.freq[R[q.id3.1]]+lambda)/(N + lambda * V))/(unigram_90.freq[Q[q.id3.1]] + lambda * V)))/(bigram.freq[PQ[q.id3.1]]+lambda*V))/(trigram.freq[BPQ[q.id3.1]] + lambda * V))}


if(sum(q.id3.2)>0) { d[q.id3.2] <- log((lambda*V*(lambda * V *((lambda * V*(u.count["oov"] + lambda)/(N + lambda * V))/(unigram_90.freq[Q[q.id3.2]] + lambda * V)))/(bigram.freq[PQ[q.id3.2]]+lambda*V))/(trigram.freq[BPQ[q.id3.2]] + lambda * V))}


if(sum(q.id4)>0) { d[q.id4] <- t.logpr[PQR[q.id4]]}

# Fall back to bigram only!!

if(sum(q.id5) > 0){ d[q.id5] <- log(lambda * V *((bigram.freq[QR[q.id5]]+lambda*V*(unigram_90.freq[R[q.id5]]+lambda)/(N + lambda * V))/(unigram_90.freq[Q[q.id5]]+lambda*V))/(bigram.freq[PQ[q.id5]]+lambda*V))}


if(sum(q.id6.1) > 0){ d[q.id6.1] <- log(lambda * V *((lambda*V*(unigram_90.freq[R[q.id6.1]] + lambda)/(N + lambda * V))/(unigram_90.freq[Q[q.id6.1]]+lambda*V))/(bigram.freq[PQ[q.id6.1]]+lambda*V))}


if(sum(q.id6.2) > 0){ d[q.id6.2] <- log(lambda * V *((lambda*V*(u.count["oov"] + lambda)/(N + lambda * V))/(unigram_90.freq[Q[q.id6.2]]+lambda*V))/(bigram.freq[PQ[q.id6.2]]+lambda*V))}


if(sum(q.id6.3) > 0){ d[q.id6.3] <- b.logpr[QR[q.id6.3]]}


if(sum(q.id7) > 0){ d[q.id7] <- lambda*V*((unigram_90.freq[R[q.id7]]+lambda)/(N+lambda*V))/(unigram_90.freq[Q[q.id7]]+lambda*V)}


if(sum(q.id7.1) > 0){ d[q.id7.1] <- lambda*V*((unigram_90.freq[R[q.id7.1]]+lambda)/(N+lambda*V))/(u.count["oov"] + lambda*V)}


if(sum(q.id7.2) > 0){ d[q.id7.2] <- lambda*V*((u.count["oov"]+lambda)/(N+lambda*V))/(unigram_90.freq[Q[q.id7.2]]+lambda*V)}


if(sum(q.id7.3) > 0){ d[q.id7] <- lambda*V*((u.count["oov"] + lambda)/(N+lambda*V))/(u.count["oov"] + lambda*V)}


q.perp <- exp(-sum(d)/length(d))

print(q.perp)


#######################################################
## Pentagram model with backoff

ABPQR <- mapply(function(a,b,p,q,r){paste(a, b, p, q, r, sep = " ")}, val_set[1:(length(val_set)-4)], val_set[2:(length(val_set)-3)], val_set[3:(length(val_set)-2)], val_set[4:(length(val_set)-1)], val_set[5:length(val_set)]) ## create pentagrams

ABPQ <-sapply(strsplit(ABPQR,split = " "), function(x){ paste(x[[1]],x[[2]],x[[3]],x[[4]], sep =" ")}) # 1st-4th words of pentagrams

BPQR <- sapply(strsplit(ABPQR,split = " "), function(x){ paste(x[[2]],x[[3]],x[[4]], x[[5]], sep =" ")}) # 2nd-5th words of quadgrams

ABP <- sapply(strsplit(ABPQR,split = " "), function(x){ paste(x[[1]],x[[2]],x[[3]], sep =" ")}) # 1st-3rd words of quadgrams

BPQ <- sapply(strsplit(ABPQR,split = " "), function(x){ paste(x[[2]],x[[3]],x[[4]], sep =" ")}) # 1st-3rd words of quadgrams

PQR <- sapply(strsplit(ABPQR,split = " "), function(x){ paste(x[[3]],x[[4]],x[[5]], sep =" ")}) # 2nd-4th words of quadgrams

AB <- sapply(strsplit(ABPQR,split = " "), function(x){ paste(x[[1]], x[[2]], sep =" ")}) # 1st and 2nd words of quadgrams

BP <- sapply(strsplit(ABPQR,split = " "), function(x){ paste(x[[2]], x[[3]], sep =" ")}) # 2nd and 3rd words of quadgrams

PQ <- sapply(strsplit(ABPQR,split = " "), function(x){ paste(x[[3]], x[[4]], sep =" ")}) # 3rd and 4th words of quadgrams

QR <- sapply(strsplit(ABPQR,split = " "), function(x){ paste(x[[4]], x[[5]], sep =" ")}) # 4th and 5th words of quadgram

A <-  sapply(strsplit(ABPQR, split = " "), function(x) {x[[1]]}) # give 1st words

B <-  sapply(strsplit(ABPQR, split = " "), function(x) {x[[2]]}) # give 2nd words

P <-  sapply(strsplit(ABPQR, split = " "), function(x) {x[[3]]}) # give 3nd words

Q <-  sapply(strsplit(ABPQR, split = " "), function(x) {x[[4]]}) # give 4th words

R <-  sapply(strsplit(ABPQR, split = " "), function(x) {x[[5]]}) # give last words


### Many possible situations below
p.id1 <- (ABPQR %in% names(pentagram.freq)) # counted pentagram

p.id2 <- (!(p.id1) & (ABPQ %in% names(quadgram.freq)) & (BPQR %in% names(quadgram.freq))) # Fall back to quadgram

#### Nontrivial logics below between #### ?? Can these terms exist???
p.id3 <- (!(p.id1) & (ABPQ %in% names(quadgram.freq)) & !(BPQR %in% names(quadgram.freq)) & (BPQ %in% names(trigram.freq)) & (PQR %in% names(trigram.freq)))

p.id4 <-  (!(p.id1) & (ABPQ %in% names(quadgram.freq)) & !(BPQR %in% names(quadgram.freq)) & !(BPQ %in% names(trigram.freq)) & (PQ %in% names(trigram.freq)) & (PQR %in% names(trigram.freq))) # quadgram + trigram ?

p.id4.1 <-  (!(p.id1) & (ABPQ %in% names(quadgram.freq)) & !(BPQR %in% names(quadgram.freq)) & !(BPQ %in% names(trigram.freq)) & !(PQ %in% names(trigram.freq)) & (PQR %in% names(trigram.freq))) # quadgram + trigram ?

p.id5.1 <-  (!(p.id1) & (ABPQ %in% names(quadgram.freq)) & !(BPQR %in% names(quadgram.freq)) & (BPQ %in% names(trigram.freq)) & !(PQR %in% names(trigram.freq)) & (PQ %in% names(bigram.freq)) & (QR %in% names(bigram.freq))) 

p.id5.2 <-  (!(p.id1) & (ABPQ %in% names(quadgram.freq)) & !(BPQR %in% names(quadgram.freq)) & (BPQ %in% names(trigram.freq)) & !(PQR %in% names(trigram.freq)) & !(PQ %in% names(bigram.freq)) & (QR %in% names(bigram.freq))) 
# quadgram + bigram ? P must be non oov since BPQ is in the vocabolary

p.id5.3 <-  (!(p.id1) & (ABPQ %in% names(quadgram.freq)) & !(BPQR %in% names(quadgram.freq)) & (BPQ %in% names(trigram.freq)) & !(PQR %in% names(trigram.freq)) & !(PQ %in% names(bigram.freq)) & !(QR %in% names(bigram.freq)) & (R != "oov"))

p.id5.4 <-  (!(p.id1) & (ABPQ %in% names(quadgram.freq)) & !(BPQR %in% names(quadgram.freq)) & (BPQ %in% names(trigram.freq)) & !(PQR %in% names(trigram.freq)) & !(PQ %in% names(bigram.freq)) & !(QR %in% names(bigram.freq)) & (R == "oov")) # quadgram + trigram + unigram ?

p.id5.5 <-  (!(p.id1) & (ABPQ %in% names(quadgram.freq)) & !(BPQR %in% names(quadgram.freq)) & (BPQ %in% names(trigram.freq)) & !(PQR %in% names(trigram.freq)) & (PQ %in% names(bigram.freq)) & !(QR %in% names(bigram.freq)) &(R != "oov")) 

p.id5.6 <-  (!(p.id1) & (ABPQ %in% names(quadgram.freq)) & !(BPQR %in% names(quadgram.freq)) & (BPQ %in% names(trigram.freq)) & !(PQR %in% names(trigram.freq)) & (PQ %in% names(bigram.freq)) & !(QR %in% names(bigram.freq)) &(R == "oov")) 

p.id6 <- (!(p.id1) & (ABPQ %in% names(quadgram.freq)) & !(BPQR %in% names(quadgram.freq)) & !(BPQ %in% names(trigram.freq)) & !(PQR %in% names(trigram.freq)) & (PQ %in% names(bigram.freq)) & (QR %in% names(bigram.freq))) # quadgram + bigrams ?

 p.id6.1  <- (!(p.id1) & (ABPQ %in% names(quadgram.freq)) & !(BPQR %in% names(quadgram.freq)) & !(BPQ %in% names(trigram.freq)) & !(PQR %in% names(trigram.freq)) & !(PQ %in% names(bigram.freq)) & (QR %in% names(bigram.freq))) 

p.id6.2  <- (!(p.id1) & (ABPQ %in% names(quadgram.freq)) & !(BPQR %in% names(quadgram.freq)) & !(BPQ %in% names(trigram.freq)) & !(PQR %in% names(trigram.freq)) & (PQ %in% names(bigram.freq)) & !(QR %in% names(bigram.freq)) & (R != "oov")) 
 
p.id6.3  <- (!(p.id1) & (ABPQ %in% names(quadgram.freq)) & !(BPQR %in% names(quadgram.freq)) & !(BPQ %in% names(trigram.freq)) & !(PQR %in% names(trigram.freq)) & (PQ %in% names(bigram.freq)) & !(QR %in% names(bigram.freq)) & (R == "oov")) 

p.id6.4  <- (!(p.id1) & (ABPQ %in% names(quadgram.freq)) & !(BPQR %in% names(quadgram.freq)) & !(BPQ %in% names(trigram.freq)) & !(PQR %in% names(trigram.freq)) & !(PQ %in% names(bigram.freq)) & !(QR %in% names(bigram.freq)) & (Q !="oov") & (R != "oov")) # Q must be nonoov since ABPQ are in quadgrams
 
p.id6.5  <- (!(p.id1) & (ABPQ %in% names(quadgram.freq)) & !(BPQR %in% names(quadgram.freq)) & !(BPQ %in% names(trigram.freq)) & !(PQR %in% names(trigram.freq)) & !(PQ %in% names(bigram.freq)) & !(QR %in% names(bigram.freq)) & (Q != "oov") & (R == "oov")) # Q must be nonoov since ABPQ are in quadgrams

####################################

p.id7 <- (!(p.id1) & !(ABPQ %in% names(quadgram.freq)) & (BPQR %in% names(quadgram.freq))) # fall to purely quadgrams

p.id8 <- (!(p.id1) & !(ABPQ %in% names(quadgram.freq)) & !(BPQR %in% names(quadgram.freq)) & (BPQ %in% names(trigram.freq)) & (PQR %in% names(trigram.freq))) # fall down to two trigrams
 
p.id9 <- (!(p.id1) & !(ABPQ %in% names(quadgram.freq)) & !(BPQR %in% names(quadgram.freq)) & !(BPQ %in% names(trigram.freq)) & (PQR %in% names(trigram.freq))) # fall down to pure trigrams 

p.id9.1 <- (!(p.id1) & !(ABPQ %in% names(quadgram.freq)) & !(BPQR %in% names(quadgram.freq)) & !(BPQ %in% names(trigram.freq)) & !(PQR %in% names(trigram.freq)) & (PQ %in% names(bigram.freq)) & (QR %in% names(bigram.freq)))
# down to bigrams

p.id9.2 <- (!(p.id1) & !(ABPQ %in% names(quadgram.freq)) & !(BPQR %in% names(quadgram.freq)) & !(BPQ %in% names(trigram.freq)) & !(PQR %in% names(trigram.freq)) & (PQ %in% names(bigram.freq)) & !(QR %in% names(bigram.freq)) & ( R != "oov"))

p.id9.3 <- (!(p.id1) & !(ABPQ %in% names(quadgram.freq)) & !(BPQR %in% names(quadgram.freq)) & !(BPQ %in% names(trigram.freq)) & !(PQR %in% names(trigram.freq)) & (PQ %in% names(bigram.freq)) & !(QR %in% names(bigram.freq)) & ( R == "oov"))

p.id9.4 <- (!(p.id1) & !(ABPQ %in% names(quadgram.freq)) & !(BPQR %in% names(quadgram.freq)) & !(BPQ %in% names(trigram.freq)) & !(PQR %in% names(trigram.freq)) & !(PQ %in% names(bigram.freq)) & (QR %in% names(bigram.freq)))
# purely bigrams

########## 
p.id9.5 <- (!(p.id1) & !(ABPQ %in% names(quadgram.freq)) & !(BPQR %in% names(quadgram.freq)) & (BPQ %in% names(trigram.freq)) & !(PQR %in% names(trigram.freq)) & (PQ %in% names(bigram.freq)) & (QR %in% names(bigram.freq)))

p.id9.6 <- (!(p.id1) & !(ABPQ %in% names(quadgram.freq)) & !(BPQR %in% names(quadgram.freq)) & (BPQ %in% names(trigram.freq)) & !(PQR %in% names(trigram.freq)) & !(PQ %in% names(bigram.freq)) & (QR %in% names(bigram.freq)))

p.id9.7 <- (!(p.id1) & !(ABPQ %in% names(quadgram.freq)) & !(BPQR %in% names(quadgram.freq)) & (BPQ %in% names(trigram.freq)) & !(PQR %in% names(trigram.freq)) & (PQ %in% names(bigram.freq)) & !(QR %in% names(bigram.freq) & (R != "oov")))

p.id9.8 <- (!(p.id1) & !(ABPQ %in% names(quadgram.freq)) & !(BPQR %in% names(quadgram.freq)) & (BPQ %in% names(trigram.freq)) & !(PQR %in% names(trigram.freq)) & (PQ %in% names(bigram.freq)) & !(QR %in% names(bigram.freq)) & (R == "oov"))

#####################

p.id10 <- (!(p.id1) & !(ABPQ %in% names(quadgram.freq)) & !(BPQR %in% names(quadgram.freq)) & !(BPQ %in% names(trigram.freq)) & !(PQR %in% names(trigram.freq)) & !(PQ %in% names(bigram.freq)) & !(QR %in% names(bigram.freq)) & (Q != "oov") & ( R!= "oov")) # purely unigrams

p.id10.1 <- (!(p.id1) & !(ABPQ %in% names(quadgram.freq)) & !(BPQR %in% names(quadgram.freq)) & !(BPQ %in% names(trigram.freq)) & !(PQR %in% names(trigram.freq)) & !(PQ %in% names(bigram.freq)) & !(QR %in% names(bigram.freq)) & (Q != "oov") & ( R == "oov")) # purely unigrams

p.id10.2 <- (!(p.id1) & !(ABPQ %in% names(quadgram.freq)) & !(BPQR %in% names(quadgram.freq)) & !(BPQ %in% names(trigram.freq)) & !(PQR %in% names(trigram.freq)) & !(PQ %in% names(bigram.freq)) & !(QR %in% names(bigram.freq)) & (Q == "oov") & ( R != "oov")) # purely unigrams

p.id10.3 <- (!(p.id1) & !(ABPQ %in% names(quadgram.freq)) & !(BPQR %in% names(quadgram.freq)) & !(BPQ %in% names(trigram.freq)) & !(PQR %in% names(trigram.freq)) & !(PQ %in% names(bigram.freq)) & !(QR %in% names(bigram.freq)) & (Q == "oov") & ( R == "oov")) # purely unigrams



## We now calculate the log q(abuvw)

f <- rep(0,length(ABPQR))

if(sum(p.id1)>0) { f[p.id1] <- p.logpr[ABPQR[p.id1]] } # existing pentagram


if(sum(p.id2)>0) { f[p.id2] <- log(lambda*V*(quadgram.freq[BPQR[p.id2]] + lambda*V*(trigram.freq[PQR[p.id2]] + lambda * V * (bigram.freq[QR[p.id2]]+ lambda * V * (unigram_90.freq[R[p.id2]] + lambda)/(N + lambda*V))/(unigram_90.freq[Q[p.id2]]+lambda*V))/(bigram.freq[PQ[p.id2]]+lambda*V))/(trigram.freq[BPQ[p.id2]]+lambda*V)/(quadgram.freq[ABPQ[p.id2]]+lambda*V))}


if(sum(p.id3)>0) { f[p.id3] <- log(lambda * V * (lambda*V*(trigram.freq[PQR[p.id3]] + lambda*V*(bigram.freq[QR[p.id3]] + lambda*V*(unigram_90.freq[R[p.id3]] + lambda)/(N + lambda*V))/(unigram_90.freq[Q[p.id3]] + lambda*V))/(bigram.freq[PQ[p.id3]]+lambda*V))/(trigram.freq[BPQ[p.id3]]+lambda*V)/(quadgram.freq[ABPQ[p.id3]]+lambda*V))}


if(sum(p.id4)>0) { f[p.id4] <- log(lambda*V*((trigram.freq[PQR[p.id4]]+lambda*V*(bigram.freq[QR[p.id4]] + lambda*V*(unigram_90.freq[R[p.id4]] + lambda)/(N + lambda*V))/(unigram_90.freq[Q[p.id4]]+lambda*V))/(bigram.freq[PQ[p.id4]]+lambda*V))/(quadgram.freq[ABPQ[p.id4]]+lambda*V))}


if(sum(p.id4.1)>0) { f[p.id4.1] <- log(((trigram.freq[PQR[p.id4]]+lambda*V*(bigram.freq[QR[p.id4]] + lambda*V*(unigram_90.freq[R[p.id4]] + lambda)/(N + lambda*V))/(unigram_90.freq[Q[p.id4]]+lambda*V)))/(quadgram.freq[ABPQ[p.id4]]+lambda*V))}


if(sum(p.id5.1)>0) { f[p.id5.1] <- log(lambda*V*(lambda*V*(lambda*V*(bigram.freq[QR[p.id5.1]]+lambda*V*(unigram_90.freq[R[p.id5.1]] + lambda)/(N + lambda*V))/(unigram_90.freq[Q[p.id5.1]]+lambda*V))/(bigram.freq[PQ[p.id5.1]]+lambda*V))/(trigram.freq[BPQ[p.id5.1]]+lambda*V)/(quadgram.freq[ABPQ[p.id5.1]]+lambda*V))}



if(sum(p.id5.2)>0) { f[p.id5.2] <- log(lambda*V*(lambda*V*(bigram.freq[QR[p.id5.2]]+lambda*V*(unigram_90.freq[R[p.id5.2]] + lambda)/(N + lambda*V))/(unigram_90.freq[Q[p.id5.2]]+lambda*V))/(trigram.freq[BPQ[p.id5.2]]+lambda*V)/(quadgram.freq[ABPQ[p.id5.2]]+lambda*V))}


if(sum(p.id5.3)>0) { f[p.id5.3] <- log(lambda*V*(lambda*V*(lambda*V*(unigram_90.freq[R[p.id5.3]] + lambda)/(N + lambda*V))/(unigram_90.freq[Q[p.id5.3]]+lambda*V))/(trigram.freq[BPQ[p.id5.3]]+lambda*V)/(quadgram.freq[ABPQ[p.id5.3]]+lambda*V))}



if(sum(p.id5.4)>0) { f[p.id5.4] <- log(lambda*V*(lambda*V*(lambda*V*(u.count["oov"] + lambda)/(N + lambda*V))/(unigram_90.freq[Q[p.id5.4]]+lambda*V))/(trigram.freq[BPQ[p.id5.4]]+lambda*V)/(quadgram.freq[ABPQ[p.id5.4]]+lambda*V))}


if(sum(p.id5.5)>0) { f[p.id5.5] <- log(lambda*V*(lambda*V*(lambda*V*(lambda*V*(unigram_90.freq[R[p.id5.5]]+lambda)/(N+lambda*V)/(unigram_90.freq[Q[p.id5.5]]+lambda*V))/(bigram.freq[PQ[p.id5.5]]+lambda*V))/(trigram.freq[BPQ[p.id5.5]]+lambda*V))/(quadgram.freq[ABPQ[p.id5.5]]+lambda*V))}



if(sum(p.id5.6)>0) { f[p.id5.6] <- log(lambda*V*(lambda*V*(lambda*V*(lambda*V*(u.count["oov"] + lambda)/(N + lambda*V))/(unigram_90.freq[Q[p.id5.6]]+lambda*V))/(bigram.freq[PQ[p.id5.6]]+lambda*V))/(trigram.freq[BPQ[p.id5.6]]+lambda*V)/(quadgram.freq[ABPQ[p.id5.6]]+lambda*V))}


if(sum(p.id6)>0) { f[p.id6] <- log((lambda*V*(lambda*V*(bigram.freq[QR[p.id6]]+lambda*V*(unigram_90.freq[R[p.id6]] + lambda)/(N + lambda*V))/(unigram_90.freq[Q[p.id6]]+lambda*V))/(bigram.freq[PQ[p.id6]]+lambda*V))/(quadgram.freq[ABPQ[p.id6]]+lambda*V))}



if(sum(p.id6.1)>0) { f[p.id6.1] <- log((lambda*V*(bigram.freq[QR[p.id6.1]]+lambda*V*(unigram_90.freq[R[p.id6.1]] + lambda)/(N + lambda*V))/(unigram_90.freq[Q[p.id6.1]]+lambda*V))/(quadgram.freq[ABPQ[p.id6.1]]+lambda*V))}



if(sum(p.id6.2)>0) { f[p.id6.2] <- log((lambda*V*(lambda*V*(lambda*V*(unigram_90.freq[R[p.id6.2]] + lambda)/(N + lambda*V))/(unigram_90.freq[Q[p.id6.2]]+lambda*V))/(bigram.freq[PQ[p.id6.2]]+lambda*V))/(quadgram.freq[ABPQ[p.id6.2]]+lambda*V))}



if(sum(p.id6.3)>0) { f[p.id6.3] <- log((lambda*V*(lambda*V*(lambda*V*(u.count["oov"] + lambda)/(N + lambda*V))/(unigram_90.freq[Q[p.id6.3]]+lambda*V))/(bigram.freq[PQ[p.id6.3]]+lambda*V))/(quadgram.freq[ABPQ[p.id6.3]]+lambda*V))}


if(sum(p.id6.4)>0) { f[p.id6.4] <- log((lambda*V*(lambda*V*(unigram_90.freq[R[p.id6.4]] + lambda)/(N + lambda*V))/(unigram_90.freq[Q[p.id6.4]]+lambda*V))/(quadgram.freq[ABPQ[p.id6.4]]+lambda*V))}



if(sum(p.id6.5)>0) { f[p.id6.5] <- log((lambda*V*(lambda*V*(u.count["oov"] + lambda)/(N + lambda*V))/(unigram_90.freq[Q[p.id6.5]]+lambda*V))/(quadgram.freq[ABPQ[p.id6.5]]+lambda*V))}



if(sum(p.id7)>0) { f[p.id7] <- q.logpr[BPQR[p.id7]]}



if(sum(p.id8)>0){ f[p.id8] <- log(lambda*V*(trigram.freq[PQR[p.id8]]+lambda*V*(bigram.freq[QR[p.id8]]+lambda*V*(unigram_90.freq[R[p.id8]] + lambda)/(N + lambda*V))/(unigram_90.freq[Q[p.id8]]+lambda*V))/(bigram.freq[PQ[p.id8]] + lambda*V)/(trigram.freq[BPQ[p.id8]] + lambda*V))}



if(sum(p.id9)>0){ f[p.id9] <- t.logpr[PQR[p.id9]]}


# down to two bigram

if(sum(p.id9.1)>0){ f[p.id9.1] <- log((lambda*V*(bigram.freq[QR[p.id9.1]]+lambda*V*(unigram_90.freq[R[p.id9.1]] + lambda)/(N + lambda*V))/(unigram_90.freq[Q[p.id9.1]]+lambda*V))/(bigram.freq[PQ[p.id9.1]] + lambda*V))}



if(sum(p.id9.2)>0){ f[p.id9.2] <- log(lambda*V*(lambda*V*(unigram_90.freq[R[p.id9.2]] + lambda)/(N + lambda*V)/(unigram_90.freq[Q[p.id9.2]]+lambda*V))/(bigram.freq[PQ[p.id9.2]]+lambda*V))}



if(sum(p.id9.3)>0){ f[p.id9.3] <- log(lambda*V*(lambda*V*(u.count["oov"] + lambda)/(N + lambda*V)/(unigram_90.freq[Q[p.id9.3]]+lambda*V))/(bigram.freq[PQ[p.id9.3]]+lambda*V))}


## fall back to purelly bigram ######


if(sum(p.id9.4)>0){ f[p.id9.4] <- b.logpr[QR[p.id9.4]]}



if(sum(p.id9.5)>0){ f[p.id9.5] <- log((lambda*V*(lambda*V*(bigram.freq[QR[p.id9.5]] + lambda*V*(unigram_90.freq[R[p.id9.5]] + lambda)/(N+lambda*V))/(unigram_90.freq[Q[p.id9.5]] + lambda*V))/(bigram.freq[PQ[p.id9.5]] + lambda*V))/(trigram.freq[BPQ[p.id9.5]]+lambda*V))}


if(sum(p.id9.6)>0){ f[p.id9.6] <- log((lambda*V*((bigram.freq[QR[p.id9.6]]+lambda*V*(unigram_90.freq[R[p.id9.6]] + lambda)/(N+lambda*V))/(unigram_90.freq[Q[p.id9.6]] + lambda*V))/(trigram.freq[BPQ[p.id9.6]]+lambda*V)))}


if(sum(p.id9.7)>0){ f[p.id9.7] <- log((lambda*V*(lambda*V*(lambda*V*(unigram_90.freq[R[p.id9.7]] + lambda)/(N+lambda*V))/(unigram_90.freq[Q[p.id9.7]] + lambda*V))/(bigram.freq[PQ[p.id9.7]] + lambda*V))/(trigram.freq[BPQ[p.id9.7]]+lambda*V))}


if(sum(p.id9.8)>0){ f[p.id9.8] <- log((lambda*V*(lambda*V*(lambda*V*(u.count["oov"] + lambda)/(N+lambda*V))/(unigram_90.freq[Q[p.id9.8]] + lambda*V))/(bigram.freq[PQ[p.id9.8]] + lambda*V))/(trigram.freq[BPQ[p.id9.8]]+lambda*V))}


## fall back to purely unigrams#####


if(sum(p.id10) >0){ f[p.id10] <- log((lambda*V*(unigram_90.freq[R[p.id10]] + lambda)/(N+lambda*V))/(unigram_90.freq[Q[p.id10]]+lambda*V))}



if(sum(p.id10.1) >0){ f[p.id10.1] <- log((lambda*V*(u.count["oov"] + lambda)/(N+lambda*V))/(unigram_90.freq[Q[p.id10.1]]+lambda*V))}



if(sum(p.id10.2) >0){ f[p.id10.2] <- log((lambda*V*(unigram_90.freq[R[p.id10.2]] + lambda)/(N+lambda*V))/(u.count["oov"]+lambda*V))}


if(sum(p.id10.3) >0){ f[p.id10.3] <- log((lambda*V*(u.count["oov"] + lambda)/(N+lambda*V))/(u.count["oov"] + lambda*V))}

p.perp <- exp(-sum(f)/length(f))

print(p.perp)


# Save files
###########################################################

save(unigram,bigram,trigram, quadgram, pentagram, unigram_90, word.ref, unigram.freq, unigram_90.freq, bigram.freq, trigram.freq, quadgram.freq, pentagram.freq, u.count, b.count, t.count, q.count, p.count, u.logpr,b.logpr,t.logpr, q.logpr, p.logpr, cover.rate, N, V, lambda, file="200000_w_stopword-complete_data.RData")

save(word.ref, unigram_90.freq,  u.count, u.logpr, N, V, lambda,cover.rate, file="200000_w_stopword_unigram90.RData")

save(bigram.freq, b.count, b.logpr, file="200000_w_stopword_bigram.RData")

save(trigram.freq, t.logpr, file="200000_w_stopword_trigram.RData")

save(quadgram.freq, q.logpr, file="200000_w_stopword_quadgram.RData")

save(pentagram.freq, p.logpr, file="200000_w_stopword_pentagram.RData")

###########################################################
#Prediction--Test
ngram_predict("")
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
