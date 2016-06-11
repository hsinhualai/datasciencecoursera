
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
                
                a
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
                a
                
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
                a
                
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
                b
                
                
        } else {b <- rep(print("Please enter at least one word!"),5)}
}
