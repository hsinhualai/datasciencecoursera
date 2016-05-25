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
                #word.ref[which.max(a)]
                
                a <- sort(a, decreasing = T)      
                names(a)
                
                
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
                #word.ref[which.max(b)]
                
                b <- sort(b, decreasing = T)
                names(b)
               
        }
}
