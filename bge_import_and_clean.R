
# IMPORT AND CLEANING


start<-Sys.time()
print("starting import and cleaning")

#open libraries

library(stringr)
library(rvest)
library(XML)
library(tidyr)

# importing nietzsche's beyond good and evil from gutenberg library

url <- "https://www.gutenberg.org/files/4363/4363-h/4363-h.htm"

webpage <- read_html(url)

#taking the text from the url 

bge <- webpage %>% html_nodes("p") %>% html_text()  

bge <- iconv(bge, "latin1", "ASCII", sub="")

print("imported")

#bge is full text, cutting down to just apophthegms
#known badness here is that i hardcoded the start and end
# this should be abstracted to a function that takes url, start and end strings as inputs and cuts down to that
apophthegms <- bge[83:207]

#each sentence begins w/ "\n    " so gonna take that out
apophthegms <- gsub("\n","",apophthegms)

apophthegms <- gsub('[[:punct:] ]+',' ',apophthegms)

apophthegms <- gsub('[[:digit:]]+', '', apophthegms)

apophthegms <- tolower(apophthegms)

apophthegms <- trimws(apophthegms)

cleanedtext <- apophthegms


print("cleaned")

end <- Sys.time()

end - start
