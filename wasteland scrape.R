

library(stringr)
library(rvest)
library(XML)
library(tidyr)
start<-Sys.time()
url <- 'https://www.bartleby.com/201/1.html'


#Reading the HTML code from the website
webpage <- read_html(url)

#taking the text from the url 

wasteland <- webpage %>% html_nodes("td") %>% html_text()  

wasteland<- iconv(wasteland, "latin1", "ASCII", sub="")

#text has a bunch of numbers representing which line; take these out
#remove blanks

wasteland<- gsub('[[:digit:]]+', '', wasteland)

wasteland <- wasteland[!wasteland %in% ""]

#there is header information, remove it
# convert to data frame

wasteland <- wasteland[9:441]

wasteland <- as.data.frame(wasteland, stringsAsFactors = FALSE)

# add sections as the second column in dataframe

sections <- c("I. THE BURIAL OF THE DEAD","II. A GAME OF CHESS","III. THE FIRE SERMON","IV. DEATH BY WATER","V. WHAT THE THUNDER SAID")



j=0

for (i in 1:nrow(wasteland)){
  if (j<length(sections)) {
    if (grepl(sections[j+1],wasteland[i,1])) {
      
      j <- j+1
      pattern <- sections[j]
      wasteland[i,1] <- sub(pattern,"",wasteland[i,1])  
      
    } 
  }
  wasteland[i,1] <- tolower(wasteland[i,1])
  wasteland$section[i] <- j
  wasteland$sectionname[i] <- sections[j]
  
  
}


end <-Sys.time()
end - start

#making a vector of all the words, faster to make a blank vector then fill it and trim it


words <-c(1:10000)
k <- 0

for(i in 1:nrow(wasteland)){
  
  split <- str_split(wasteland[i,1]," ")
  wasteland$dirtywords[i] <- split
  split <- unlist(split)
  length <- length(split)
  
  for(j in 1:length){
    split[j] <- gsub('[[:punct:] ]+','',split[j])
    words[j+k] <- split[j]
  }
  k <- k+length
  
}
words <-words[1:k]
words <- as.data.frame(words,'word'=as.character(words),stringsAsFactors = FALSE)

for (i in 1:nrow(words)){
  words$stem[i] <- text_tokens(words[i,1], stemmer = "en")
  
}

head(words)

doc_term_matrix <- dfm(words$words, stem = FALSE)
missing <- doc_term_matrix@Dimnames$features %in% colnames(doc_term_matrix)
doc_term_matrix@Dimnames$features[which(missing == 0)]


77
