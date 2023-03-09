# STEP 0: Preliminary_Install & Load Packages ------------------------------------
install.packages(c("tm","dplyr")) # run once to import packages

library(tm)
library(dplyr)

# STEP 1: Data_Create 6-Letter Shakespearean-Based Dictionary  -------------------
# Read data from website
url <- "https://www.gutenberg.org/files/100/100-0.txt"
shakespeare.text <- readLines(url, encoding = "UTF-8")

# Eliminate preamble and afterword so that only 'The Complete Works of William Shakespeare' is analyzed 
start <- which(shakespeare.text == "The Complete Works of William Shakespeare")
stop <- which(shakespeare.text == "*** END OF THE PROJECT GUTENBERG EBOOK THE COMPLETE WORKS OF WILLIAM SHAKESPEARE ***") -1

# Replace non-alphanumeric characters with space, 
# 	i.e. serparate words with special characters, and treat as separate words
shakespeare.text.subset<-gsub("[^[:alnum:]]"," ",shakespeare.text[start:stop])

#View first 50 lines to double check that data is uploaded and subseted correctly
shakespeare.text.subset[1:50] 

# Transform character list to corpus for easier text analysis of unstructured text 
# Remove punctuation and numbers, keep stopwords, and no stemming 
shakespeare.corpus <- tm::Corpus(VectorSource(shakespeare.text.subset))
shakespeare.tdm <- tm::TermDocumentMatrix(shakespeare.corpus, 
                                          control = list(removePunctuation = TRUE,
                                                         removeNumbers = TRUE,
                                                         stopwords = FALSE,
                                                         stemming = FALSE))

# Pull character list of terms
shakespeare.tdm.terms<-shakespeare.tdm[["dimnames"]][["Terms"]]

# Subset character list to only include 6 letter terms
shakespeare.tdm.terms.sixLetters<-unique(shakespeare.tdm.terms[which(nchar(shakespeare.tdm.terms)== 6)])



# STEP 2: Exploratory Analysis  --------------------------------------------------
# List the unique letter characters in term list
shakespeare.dict.char.list<-strsplit(paste(shakespeare.tdm.terms.sixLetters, collapse = ""),'')[[1]]
shakespeare.dict.char.list<-sort(unique(shakespeare.dict.char.list))
shakespeare.dict.char.list


# Summary analysis of vowel characters 
shakespeare.dict.df<-data.frame(term = shakespeare.tdm.terms.sixLetters)%>%
  dplyr::mutate(
    id = row_number(),
    vowel_a = ifelse(grepl("a", term, fixed=TRUE), 1, 0),
    vowel_æ = ifelse(grepl("æ", term, fixed=TRUE), 1, 0),
    vowel_e = ifelse(grepl("e", term, fixed=TRUE), 1, 0),
    vowel_é = ifelse(grepl("é", term, fixed=TRUE), 1, 0),
    vowel_è = ifelse(grepl("è", term, fixed=TRUE), 1, 0),
    vowel_i = ifelse(grepl("i", term, fixed=TRUE), 1, 0),
    vowel_o = ifelse(grepl("o", term, fixed=TRUE), 1, 0),
    vowel_u = ifelse(grepl("u", term, fixed=TRUE), 1, 0)
  )

shakespeare.dict.df.factors<-shakespeare.dict.df
shakespeare.dict.df.factors[,-c(1,2)] <- lapply(shakespeare.dict.df.factors[,-c(1,2)], factor) 
summary(shakespeare.dict.df.factors[,-c(1:2)])

# Because "special-letter" vowels (i.e. æ, é, è) are seen in at most .2% of words, 
#   I replaced them with the US 26-letter vowel equivalence.
# Replacing æ with a, e will make word 7 characters long.  
# As a result, the word æ was removed.
# If I had time, I would go back and replace æ with a e during data-cleansing step after line 18.
# I would also look for and replace other "double-letter" characters

shakespeare.dict.df.update<-shakespeare.dict.df%>%
  dplyr::filter(!grepl("æ", term, fixed=TRUE))%>%
  dplyr::mutate(
    term = gsub("é", "e", term),
    term = gsub("è", "e", term),
    vowel_e = ifelse(grepl("e", term, fixed=TRUE), 1, 0)
  )%>%
  dplyr::select(id, term, vowel_a, vowel_e, vowel_i, vowel_o, vowel_u)

shakespeare.dict.df.update.factors<-shakespeare.dict.df.update
shakespeare.dict.df.update.factors[,-c(1,2)] <- lapply(shakespeare.dict.df.update.factors[,-c(1,2)], factor) 
summary(shakespeare.dict.df.update.factors[,-c(1:2)])

# STEP 3: Examine vowel patterns  ------------------------------------------------

# Create variable listing vowel patterns
shakespeare.dict.df.unique.patterns<-shakespeare.dict.df.update%>%
  dplyr::mutate(
    Pattern = ifelse(vowel_a == 1, "Pattern: A", "Pattern:"),
    Pattern = ifelse(vowel_e == 1, paste(Pattern, " E", sep = "") , Pattern), 
    Pattern = ifelse(vowel_i == 1, paste(Pattern, " I", sep = "") , Pattern),
    Pattern = ifelse(vowel_o == 1, paste(Pattern, " O", sep = "") , Pattern),
    Pattern = ifelse(vowel_u == 1, paste(Pattern, " U", sep = "") , Pattern)
  )

# How many unique cluster are formed by looking at vowel patterns? 30
vowel_pattern_list <- unique(shakespeare.dict.df.unique.patterns$Pattern)
length(vowel_pattern_list)

# What patterns contained the most vowels? Max length of vowel patterns = 4
max_vowel_pattern_nchar = max(nchar(vowel_pattern_list))
vowel_pattern_list.max_nchar<-vowel_pattern_list[which(nchar(vowel_pattern_list)==max_vowel_pattern_nchar)]
vowel_pattern_list.max_nchar

# Because vowel patterns only clusters data into 30 groups
# and the max character length of vowel = 4, consonants need to be introduced to 
# to further cluster data.
# To identify which consonants to include, only words with 
# four vowels will be considered. That way, the consonants considered would guarantee
# to form a word. 

#Candidate words
shakespeare.dict.df.unique.patterns.4vowels<-shakespeare.dict.df.unique.patterns%>%
  dplyr::filter(Pattern %in% vowel_pattern_list.max_nchar)%>%
  dplyr::distinct()

word.list.4vowels<-unique(shakespeare.dict.df.unique.patterns.4vowels$term)
word.list.4vowels

# STEP 4: Examine candidate word patterns  ---------------------------------------

# Create list of data frames where each data frame include 
# reference term, binary indicator variable for each letter
# and letter pattern text

word.list.4vowels.df <- list() 

for(i in 1:length(word.list.4vowels)){
  word.list.4vowels.df[[i]]<-data.frame(term = shakespeare.tdm.terms.sixLetters)%>%
    dplyr::filter(!grepl("æ", term, fixed=TRUE))%>%
    dplyr::mutate(
      term = gsub("é", "e", term),
      term = gsub("è", "e", term)
    )%>%
    dplyr::mutate(id = row_number()) %>%
    dplyr::select(id, term)%>%
    dplyr::mutate(
      reference_term = word.list.4vowels[i],
      letter_1 = ifelse(grepl(substr(word.list.4vowels[i], 1, 1), term, fixed=TRUE), 1, 0),
      letter_2 = ifelse(grepl(substr(word.list.4vowels[i], 2, 2), term, fixed=TRUE), 1, 0),
      letter_3 = ifelse(grepl(substr(word.list.4vowels[i], 3, 3), term, fixed=TRUE), 1, 0),
      letter_4 = ifelse(grepl(substr(word.list.4vowels[i], 4, 4), term, fixed=TRUE), 1, 0),
      letter_5 = ifelse(grepl(substr(word.list.4vowels[i], 5, 5), term, fixed=TRUE), 1, 0),
      letter_6 = ifelse(grepl(substr(word.list.4vowels[i], 6, 6), term, fixed=TRUE), 1, 0),
      Pattern = ifelse(letter_1 == 1, paste("Pattern: ", toupper(substr(word.list.4vowels[i], 1, 1)), sep = ""), "Pattern:"),
      Pattern = ifelse(letter_2 == 1, paste(Pattern, " ", toupper(substr(word.list.4vowels[i], 2, 2)), sep = "") , Pattern), 
      Pattern = ifelse(letter_3 == 1, paste(Pattern, " ", toupper(substr(word.list.4vowels[i], 3, 3)), sep = "") , Pattern),
      Pattern = ifelse(letter_4 == 1, paste(Pattern, " ", toupper(substr(word.list.4vowels[i], 4, 4)), sep = "") , Pattern),
      Pattern = ifelse(letter_5 == 1, paste(Pattern, " ", toupper(substr(word.list.4vowels[i], 5, 5)),sep = "") , Pattern),
      Pattern = ifelse(letter_6 == 1, paste(Pattern, " ", toupper(substr(word.list.4vowels[i], 6, 6)), sep = "") , Pattern)
    )%>%
    dplyr::group_by(reference_term , Pattern)%>%
    dplyr::summarize(term_count = n())
}


# Create summary data frame holding summary for each word

partitions <- NULL
min.terms <- NULL
max.terms <- NULL
mean.terms <- NULL
median.terms <- NULL
sd.terms <- NULL


for(i in 1:length(word.list.4vowels.df)){
  
  partitions[i] = nrow(word.list.4vowels.df[[i]])
  min.terms[i] = min(word.list.4vowels.df[[i]][3], na.rm = TRUE)
  max.terms[i] = max(word.list.4vowels.df[[i]][3], na.rm = TRUE)
  mean.terms[i] = mean(unlist(word.list.4vowels.df[[i]][3]), na.rm = TRUE)
  median.terms[i] = median(unlist(word.list.4vowels.df[[i]][3]), na.rm = TRUE)
  sd.terms[i] = sd(unlist(word.list.4vowels.df[[i]][3]), na.rm = TRUE)
  
}

word.list.4vowels.df.summary<-data.frame(
  reference_term = word.list.4vowels,
  clusters_n = partitions, 
  min_words_in_cluster = min.terms, 
  max_words_in_cluster = max.terms, 
  mean_words_in_cluster = mean.terms, 
  median_words_in_cluster = median.terms, 
  sd_words_in_cluster = sd.terms 
)


# Final summary table
word.list.4vowels.df.summary

# Because 'AROUSE' clusters the data into most partitions, "AROUSE" would be my starting word.



