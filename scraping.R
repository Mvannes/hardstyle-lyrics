require("httr")
require("jsonlite")
require("rvest")

token <- "LDsUlIW3ABdbmF461J2HzyDxirsyNF0wvAuGdxjh4SYgx0ceevbj7Wx.rEFB1bF3"
base.url <- "https://www.genius.com/api"
base.artist.url <- paste(sep="", base.url, "/artists/")
base.songs.url <- paste(sep="", base.url, "/songs/")
artists <- c(
    344100,
    452181,
    236502,
    452297,
    1185555,
    1227120,
    646866,
    452440,
    454552,
    452365,
    377558,
    389116
)
artist.songs <- data.frame(Id=c(), Title=c(), Lyrics=c(), Artist=c())
# This does x website / api calls. Where x = (artists*2)+(artists*songs_by_artist*2)
for (artist in artists) {
    artist.url <- paste(sep="", base.artist.url, artist, "/")
    artist.response <- GET(artist.url)
    artist.representation <- fromJSON(content(artist.response, "text"))
    artist.name <- artist.representation$response$artist$name
    print(artist.name)

    base.artist.songs.url <- paste(sep="", artist.url, "songs/?per_page=50")
    current_page <- "1"
    songs <- data.frame(Id=c(), Title=c(), Lyrics=c())
    while (!is.null(current_page)) {
        current.songs.url <- paste(sep="", base.artist.songs.url, "&page=", current_page)

        current.songs.response <- GET(current.songs.url)
        current.songs.representation <- fromJSON(content(current.songs.response, "text"))
        current_page <- current.songs.representation$response$next_page

        current.songs.ids <- current.songs.representation$response$songs$id
        current.songs.titles <- current.songs.representation$response$songs$title
        print(length(current.songs.ids))
        print(length(current.songs.titles))
        song.lyrics <- lapply(current.songs.ids, function(id) {
            specific.song.url <- paste(sep="", base.songs.url, id)
            specific.song.response <- GET(specific.song.url)
            specific.song.representation <- fromJSON(content(specific.song.response, "text"))


            paths <- specific.song.representation$response$song$path
            lyrics <- c()
            for (path in paths) {
                page <- read_html(paste("https://www.genius.com", path, sep=""))

                lyrics <- c(lyrics, html_nodes(page, ".lyrics> p") %>%
                    html_text()) %>%
                    gsub("\\n", ". ", .)

                # Close the connection.
                rm(page)
            }
            return(lyrics)
        }) %>%  unlist
        print(length(song.lyrics))
        print(length(current.songs.ids))
        print(length(current.songs.titles))
        songs <- rbind(songs, data.frame(Id=current.songs.ids, Title=current.songs.titles, Lyrics=song.lyrics))
    }
    songs$Artist <- rep(artist.name, nrow(songs))
    artist.songs <- rbind(artist.songs, songs)
}

