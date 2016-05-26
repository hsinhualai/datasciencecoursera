Text Prediction Shiny Application
========================================================
author: Hsin-Hua Lai
date: May 26, 2016
width: 1440
height: 900

<small> 
Slide presentation for Capstone project  
Johns Hopkins University   
Coursera Data Science Specialization
</small>

Overview
========================================================

- This presentation illustrateds the word prediction Shiny app I develop for the Capston project

- The Shiny app predicts the most probable word following a partial sentence entered by Users

- The following slides illustrate
   - Data source and preprocessing
   - Algorithm for word prediction
   - Future Plan
   
The Shiny App is on the shinyapp.io website: https://hsinhualai.shinyapps.io/Capstone_Project-Text_Prediction/

All the codes and relevant materials are on my github: https://github.com/hsinhualai/datasciencecoursera/tree/master/Capstone


Data
========================================================

- The complete text data for buidling text model is downloaded from https://d396qusza40orc.cloudfront.net/dsscapstone/dataset/Coursera-SwiftKey.zip

- The data file contains four folders, in which the one names en_US is used

- The en_US file folder contains three text files from blogs, news, and twitter. The more complete exploratory analysis is at https://rpubs.com/hsinhua/177290.

- Preprocessing steps 
  - Sample about 30%, 25%, and 20% from the blogs, news, and twitter text data
  - Convert characters to lower case and remove all bad words and non-alphabetic characters including numbers, punctuations
  - Extract unigrams, bigrams, and trigrams along with their frequencies
  - Use only 90% of the unigrams for efficiency and the corresponding bigrams and trigrams
  - Eliminate all trigrams whose frequencies are one, which significantly reduces the trigram file size


Prediction Algorithm
========================================================

- The approach is a Back-off model illustrated in the Natural Language Processing lectures, http://web.mit.edu/6.863/www/fall2012/lectures/lecture2&3-notes12.pdf 

- The backoff smoother used do a weighted average between all trigrams, bigrams, and unigrams.

- For a partial sentence whose length is larger than three, the last two words will be used for next word prediction
   - The last two words will be combined with any unigram to give a trigram, two bigrams, and three unigrams
   - The built algorithm will search for built-in trigram, bigram, and unigram tables. 
   - If there is a match in higher n-grams, the weight contributed from higher n-grams is significantly larger
   - If there is no match in n-grams, it will back off to search (n-1)-grams tables and do weight average between (n-1)-grams down to unigrams

Shiny App and Future Plan
==========================================================

- The Shiny App is on the shinyapp.io website: https://hsinhualai.shinyapps.io/Capstone_Project-Text_Prediction/
   - On the left panel, the users enter a partial sentence
   - On the main panel, there will appear five words from left to right, ranked by probability
   - The word on the left most is the most probable word
   
- Future Plan
   - Try to build in higher n-grams with $~n>3$ for a better prediction app
   - Try to build in an algorithm which can learn from the Users' input to generalize the dictionary
