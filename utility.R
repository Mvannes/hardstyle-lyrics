
toSpace <- content_transformer(function(x, pattern) {return (gsub(pattern, " ", x))})
alterApostrophe <- content_transformer(
    function(x) {
        x <- gsub("’", "'", x)
        return (x)
    }
)

createCleanCorpus <- function(content) {
    content <- tolower(content)
    corpus <- Corpus(VectorSource(content))
    corpus <- tm_map(corpus, toSpace, "-")
    corpus <- tm_map(corpus, toSpace, ":")
    corpus <- tm_map(corpus, alterApostrophe)
    corpus <- tm_map(corpus, removePunctuation)
    dutch_stop_words <- readLines("dutch_stop_words.txt")
    corpus <- tm_map(corpus, removeWords, c(dutch_stop_words, "nee"))
    stopwords_eng <- stopwords() %>% gsub("’", "", .) %>% gsub("'", "", .)
    corpus <- tm_map(corpus, removeWords, c(stopwords_eng, "maybe", "just", "also", "really", "thats", "get"))
    corpus <- tm_map(corpus, stripWhitespace)

    return(corpus)
}


createWordCloud <- function(cloud_tdm, title) {
    matrix <- as.matrix(cloud_tdm)
    sorted_matrix <- sort(rowSums(matrix), decreasing = TRUE)
    sorted_df <- data.frame(word = names(sorted_matrix), freq = sorted_matrix)
    wordcloud2(
        sorted_df,
        color = "#F85D2F",
        backgroundColor = "white",
        rotateRatio = 0.5
    )
}
