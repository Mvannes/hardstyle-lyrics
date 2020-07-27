require(dplyr)
require(ggplot2)
require(tm)
require(wordcloud2)
require(stringr)

source("utility.R")

corpus <- createCleanCorpus(artist.songs$Lyrics)
tdm <- TermDocumentMatrix(corpus)

createWordCloud(tdm, "ellende")
