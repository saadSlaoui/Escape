setwd("D:/iXperience/Projects/escape/wordclouds")
library(rvest)
library(wordcloud)
library(tm)
library(RColorBrewer)
library(stringr)
library(RCurl)

#--------------------------------------------

# In order to imitate every url, we need to add the region in which the restaurant is located and to clean the 
# scraped data quite a bit

dat <- read_html("https://www.zomato.com/capetown/breakfast?page=1") 
breakfast.regions <- c()
for (i in 1:30) { 
  breakfast.regions <- c(breakfast.regions, dat %>% html_nodes(css = paste("#orig-search-list > div:nth-child(", i, ") > div.content > div > article > div.pos-relative.clearfix > div > div.col-s-16.col-m-12.pl0 > div:nth-child(1) > div.col-s-12 > a.ln24.search-page-text.mr10.zblack.search_result_subzone.left > b", sep = "")) %>% html_text())
}
dat <- read_html("https://www.zomato.com/capetown/breakfast?page=2") 
for (i in 1:30) { 
  breakfast.regions <- c(breakfast.regions, dat %>% html_nodes(css = paste("#orig-search-list > div:nth-child(", i, ") > div.content > div > article > div.pos-relative.clearfix > div > div.col-s-16.col-m-12.pl0 > div:nth-child(1) > div.col-s-12 > a.ln24.search-page-text.mr10.zblack.search_result_subzone.left > b", sep = "")) %>% html_text())
}
breakfast.regions <- breakfast.regions[1:50]

dat <- read_html("https://www.zomato.com/capetown/lunch?page=1") 
lunch.regions <- c()
for (i in 1:30) { 
  dinner.regions <- c(lunch.regions, dat %>% html_nodes(css = paste("#orig-search-list > div:nth-child(", i, ") > div.content > div > article > div.pos-relative.clearfix > div > div.col-s-16.col-m-12.pl0 > div:nth-child(1) > div.col-s-12 > a.ln24.search-page-text.mr10.zblack.search_result_subzone.left > b", sep = "")) %>% html_text())
}
dat <- read_html("https://www.zomato.com/capetown/lunch?page=2") 
for (i in 1:30) { 
  lunch.regions <- c(lunch.regions, dat %>% html_nodes(css = paste("#orig-search-list > div:nth-child(", i, ") > div.content > div > article > div.pos-relative.clearfix > div > div.col-s-16.col-m-12.pl0 > div:nth-child(1) > div.col-s-12 > a.ln24.search-page-text.mr10.zblack.search_result_subzone.left > b", sep = "")) %>% html_text())
}
lunch.regions <- lunch.regions[1:50]


dat <- read_html("https://www.zomato.com/capetown/dinner?page=1") 
dinner.regions <- c()
for (i in 1:30) { 
  dinner.regions <- c(dinner.regions, dat %>% html_nodes(css = paste("#orig-search-list > div:nth-child(", i, ") > div.content > div > article > div.pos-relative.clearfix > div > div.col-s-16.col-m-12.pl0 > div:nth-child(1) > div.col-s-12 > a.ln24.search-page-text.mr10.zblack.search_result_subzone.left > b", sep = "")) %>% html_text())
}
dat <- read_html("https://www.zomato.com/capetown/dinner?page=2") 
for (i in 1:30) { 
  dinner.regions <- c(dinner.regions, dat %>% html_nodes(css = paste("#orig-search-list > div:nth-child(", i, ") > div.content > div > article > div.pos-relative.clearfix > div > div.col-s-16.col-m-12.pl0 > div:nth-child(1) > div.col-s-12 > a.ln24.search-page-text.mr10.zblack.search_result_subzone.left > b", sep = "")) %>% html_text())
}
dinner.regions <- dinner.regions[1:50]

regions <- c(breakfast.regions, lunch.regions, dinner.regions)

regions <- gsub(regions, pattern = ",", replacement = "")
regions <- gsub(regions, pattern = "& ", replacement = "")
regions <- tolower(regions)
regions <- gsub(regions, pattern = " ", replacement = "-")
#--------------------------------------------

# second step: adapt the names
food <- read.csv("D:/iXperience/Projects/escape/food.csv")
names <- food$eventname
names <- tolower(names)
names <- gsub(names, pattern = " ", replacement = "-")
names <- gsub(names, pattern = "&", replacement = "")
names <- gsub(names, pattern = "---", replacement = "-")
names[which(names == "iyo-burgers-(inside-&-you're-out)")] <- "iyo-burgers-inside-youre-out"

for(i in 1:10) {
  
  url <- paste("https://www.zomato.com/capetown/", names[i], "-", regions[i], "/reviews", sep = "")
  if (!url.exists(url)) {
    next
  }
  reviews <- read_html(url) 

  
  # Extract text
  #

  reviews.text

  reviews.text = c()             
  for (j in 1:20) {
    reviews.text <- c(reviews.text,
                      reviews %>% html_nodes(css = paste("#reviews-container > div.notifications-content > div.res-reviews-container.res-reviews-area > div.zs-following-list > div:nth-child(", j, ") > div > div.ui.segment.clearfix.brtop > div.rev-text.mbot0", sep = "")) %>% html_text())
  }


  # Remove unprintable characters
  #
  reviews.text= gsub("[^[:print:]]", "", reviews.text)

  # Remove URLs
  #
  reviews.text = gsub("http[s]?://(?:[a-zA-Z]|[0-9]|[$-_@.&+]|[!*\\(\\),]|(?:%[0-9a-fA-F][0-9a-fA-F]))+", "", reviews.text)
  reviews.text = gsub(reviews.text, pattern = "\\\n", replacement = "")
  reviews.text = gsub(reviews.text, pattern = "Rated", replacement = "")

  # Remove truncated text
  #
  reviews.text = gsub("[^[:space:]]*.$", "", reviews.text)
  reviews.text = str_trim(reviews.text)

  reviews.corpus = Corpus(VectorSource(reviews.text))

  reviews.corpus <- tm_map(reviews.corpus, stripWhitespace)
  reviews.corpus <- tm_map(reviews.corpus, content_transformer(tolower))
  reviews.corpus <- tm_map(reviews.corpus, removePunctuation, lazy = TRUE)
  reviews.corpus <- tm_map(reviews.corpus, removeNumbers, lazy = TRUE)
  #
  # Remove frequent terms with low information.
  #
  reviews.corpus <- tm_map(reviews.corpus, removeWords, stopwords("english"))

  # Create a TDM.
  #
  reviews.tdm = TermDocumentMatrix(reviews.corpus)

  reviews.tdm = as.matrix(reviews.tdm)
  reviews.word_freq = sort(rowSums(reviews.tdm), decreasing = TRUE)
  reviews.word_freq = data.frame(word = names(reviews.word_freq), freq = reviews.word_freq)

  # Change the resolution to get a good fit.

  # Store the image

  png(paste("reviews-wordcloud", i, ".png", sep = ""), width = 1200, height = 1200, res = 175)

  # Direct visualisation
  wordcloud(reviews.word_freq$word, reviews.word_freq$freq, random.order = TRUE, colors = rev(brewer.pal(8, "Dark2")),
            min.freq = 2)

}
