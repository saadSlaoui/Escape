setwd("C:/Users/maggie.lou/Dropbox/MyDocuments/NU 2019/Data Mining/Projects/Escape Travel Schedule")

library(rvest)
library(stringr)
library(jsonlite)
library(ggmap)

# Note: table generation is at the bottom 

#' @param subtype A string representing the type of activity. 
#' @return A dataframe including all the data for that subtype.
get_data <- function(subtype) {
  #generate url
  url <- paste("https://www.tripadvisor.co.za/Search?geo=1722390&pid=3826&typeaheadRedirect=true&redirect=&startTime=1468843333054&uiOrigin=MASTHEAD&q=", subtype,"&returnTo=http%253A__2F____2F__www__2E__tripadvisor__2E__co__2E__za__2F__Tourism__2D__g1722390__2D__Cape__5F__Town__5F__Western__5F__Cape__2D__Vacations__2E__html&searchSessionId=54C82AF511E0EECD3FCDFE349212CF8B1468850526354ssid","&o=0", sep = "")
  input <- read_html(url)
  
  #scrape number of pages
  # num.pages <- input %>% html_nodes("div.pageNumbers > a") %>% html_text()
  # num.pages <- as.numeric(num.pages[length(num.pages)])
  
  #scrape activity names
  activity.names <- input %>% html_nodes("div.title > span") %>% html_text()
  #get second page of activities
  url.pieces <- str_split(url, pattern = "&o=")
  url.secondpage <- paste(url.pieces[[1]][[1]],"&o=",as.character(30 + as.numeric(url.pieces[[1]][[2]])), sep = "")
  activity.names <- c(activity.names,read_html(url.secondpage) %>% html_nodes("div.title > span") %>% html_text())
  
  
  #scrape number of reviews 
  num.reviews <- input %>% html_nodes("div.reviews > a") %>% html_text()
  url.pieces <- str_split(url, pattern = "&o=")
  url.secondpage <- paste(url.pieces[[1]][[1]],"&o=",as.character(30 + as.numeric(url.pieces[[1]][[2]])), sep = "")
  num.reviews <- c(num.reviews, read_html(url) %>% html_nodes("div.reviews > a") %>% html_text())
  
  #transform reviews into integers
  num.reviews <- gsub("Be the first to review this attraction", "0", num.reviews)
  num.reviews <- gsub("reviews ","", num.reviews)
  num.reviews <- gsub("review ","", num.reviews)
  num.reviews <- gsub("[[:space:]]","", num.reviews)
  num.reviews <- str_trim(num.reviews)
  num.reviews <- as.integer(num.reviews)
  
  #translate number of reviews to popularity rating (min = 1, max = 3)
  pop.quant <- quantile(num.reviews, probs = seq(0,1,1/3))
  popularity <- c()
  for (i in 1:length(num.reviews)) {
    if (num.reviews[i]<pop.quant[2]) 
      popularity[i] <- 1
    else if (num.reviews[i] <pop.quant[3])
      popularity[i] <- 2
    else 
      popularity[i] <- 3
  }
  
  
  #scrape addresses
  address <- input %>% html_nodes("div.address") %>% html_text()
  url.pieces <- str_split(url, pattern = "&o=")
  url.secondpage <- paste(url.pieces[[1]][[1]],"&o=",as.character(30 + as.numeric(url.pieces[[1]][[2]])), sep = "")
  address <- c(address, read_html(url.secondpage) %>% html_nodes("div.address") %>% html_text())
  
  #limit to 50 entries 
  activity.names <- activity.names[1:50]
  num.reviews <- num.reviews[1:50]
  address <- address[1:50]
  
  #get descriptions
  description <- sapply(activity.names, get_description)
  
  #get latitude and longitude 
  # latitude <- c()
  # longitude <- c()
  # for (i in 1:50) {
  #   latitude <- c(latitude, geocode(address[i])[1,1])
  #   longitude <- c(longitude, geocode(address[i])[1,2])
  #   
  # }
  # 
  geo_location <- sapply(address, geocode)
  odds <- seq(from =1, to = 100, by = 2)
  evens <- seq(from =2, to = 100, by = 2)
  
  latitude <- c()
  longitude <- c()
  
  for (i in 1:50) {
    longitude <- c(longitude, geo_location[[odds[i]]][1])
    latitude <- c(latitude, geo_location[[evens[i]]][1])
  }
  
  
  #create dataframe
  eventid <- rep(NA, 50)
  eventname <- activity.names
  subtype <- rep(subtype, 50)
  price <- rep(NA, 50)
  openWeek <- rep(NA, 50)
  closeWeek <- rep(NA, 50)
  openSat <- rep(NA, 50)
  closeSat <- rep(NA, 50)
  openSun <- rep(NA, 50)
  closeSun <- rep(NA, 50)
  eventlength <- rep(NA, 50)
  
  activity.table <- data.frame(eventid, eventname, subtype, description, price, openWeek, closeWeek, openSat, closeSat, openSun, closeSun, eventlength, popularity, latitude, longitude, address)
  
}



#' @param event A string of the name of the event.
#' @return A string description of the event.  
get_description <- function(event) {
  
  #transform search query to text+text format (replace spaces with +)
  event <- gsub(" ","+", event )
  
  #generate url 
  #search_url <- sprintf("https://www.google.co.za/search?q=%s&oq=%s&aqs=chrome", event, event)
  #google_search <- read_html(search_url)get_
  
  #scrape description using knowledge graph API 
  #apikey <- "AIzaSyDuw9cd8m8NpfszBKu2vCpATyLPKOyXeiY"
  
  #use Arthur's key for now 
  apikey <- "AIzaSyBbXp8gpgioisz6foCYy1EdxhbM-ItRcG4"
  
  url <- paste0("https://kgsearch.googleapis.com/v1/entities:search?query=",event,"&key=", apikey, "&limit=1&indent=True")
  
  #error check URL 
  if (verify_URL(url)) {
    json <-  fromJSON(url)
    description <- json$itemListElement$result$detailedDescription$articleBody
    
    if(is.null(description)) {
      description <- "There is no description available."
      
    }
  } 
  else {
    description <- "There is no description available."
  }
  
  #trim whitespace 
  description <- str_trim(description)
  
  return(description)
}

#' @param url URL for a search query of Google's Knowledge graph API.
#' @return Returns TRUE if the url was valid, FALSE if there doesn't exist a knowledge graph for that query. 

verify_URL <- function(url) {
  tryCatch({
    fromJSON(url)
    return(TRUE)
  }, error = function(e) {
    #message(paste("Warning: There is no description available."))
    return(FALSE)
  }, finally = {}
  )
}




bars <- get_data("bars")
museums <- get_data("museums")
outdoor <- get_data("outdoor")
shopping <- get_data("shopping")

activity <- rbind(bars, museums, outdoor, shopping)

activity$eventid <- seq(from =2, to = 400, by = 2)

#write.csv(activity, file = "Activity_frame.csv")


