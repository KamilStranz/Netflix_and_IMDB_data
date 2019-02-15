# The following script aims to build interactive table browser of Netflix films,
# combined with data about film rating from IMDB, thus allowing filtering by rating
# Secondly, I collect data about films periodically which will allow me to spot the difference 
# in Netflix offer: additions as well as deletions. This excercise let's me experiment with
# storing and manipulating data on my local PostgreSQL database as well as combine it with 
# Rmarkdown automatic reporting. Ultimately this project will allow me a smooth process of 
# choosing films on Saturday evening.

# loading required libraries
library(rvest)
library(tidyverse)
library(readr)
library(DT)
library(data.table)
library(flexdashboard)

# In order to combine the data we need to get data first: both, from Netflix and IMDB.
# finder.com website has an agreement with Netflix (as of Feb 2019) following deprecation of its API
# let's acquire list of films from website's html
netflix_movies <- read_html("https://www.finder.com/pl/netflix-movies")
table_element <- html_nodes(netflix_movies, css = "table")
netflix_table <- html_table(table_element)

#checking results
netflix_table[[1]][[4]]

# collected data is in a list, lets change it to data.frame
net_tab3 <- do.call(rbind.data.frame, netflix_table)

# checking structure of the data and basic statistics
head(net_tab3)
glimpse(net_tab3)
summary(net_tab3)

# adjusting column names
colnames(net_tab3) <- c("Title", "Year", "Runtime", "Genres")

# At some point I spotted that Netflix adds "Original" to the productions names it has rights to
net_tab3 %>% filter(Title %like% "Original")

# therefore, if we want to match by title using some other source of film titles we need to clean that part
net_tab3$Title <- gsub("Original$","", net_tab3$Title)

# create trim function and get rid off white spaces
trim <- function (x) gsub("^\\s+|\\s+$", "", x)
net_tab3$Title <- trim(net_tab3$Title)

# first part is ready, let's download IMDB film dataset from https://datasets.imdbws.com/ - pretty heavy, some 500mb
# I download couple different tables, we will need to do some joins but documentation describes well all the fields
# check current wd, switch working directory and read the file
getwd()
setwd("C:/Users/Varia/Downloads")
imdb_titles <- read_tsv("name.basics.tsv")

# as dataset contains not only films let's filter movies only
imdb_titles <- imdb_titles %>% filter(titleType == "movie")
head(imdb_titles)

# next, reading the ratings table and making join on primary key, i.e. "tconst" field
imdb_ratings <- read_tsv("title.ratings.tsv")
imdb_titles_ratings <- inner_join(imdb_ratings, imdb_titles, by = "tconst")

# filtering out unusable stuff
imdb_titles_ratings_fil1 <- imdb_titles_ratings %>% filter(!runtimeMinutes == "\\N")
tail(imdb_titles_ratings_fil1, 30)

# trim whitespaces
imdb_titles_ratings_fil1$primaryTitle <- trim(imdb_titles_ratings_fil1$primaryTitle)

# good! IMDB data frame ready. I can merge it with Netflix df
films_mergedDbs <- left_join(x = net_tab3, y = imdb_titles_ratings_fil1, by = c("Title" = "primaryTitle", "Year" = "startYear"))
head(films_mergedDbs, 20)

# I see that some films have data missing. I will need to investigate further whether this is spelling mistakes, foreign titles or else
# at the moment let's check how many films have ratings
films_mergedDbs[complete.cases(films_mergedDbs),] %>% nrow()

# half of the movies available on Netflix in Poland got matched with IMDB ratings but this can be due to small number of votes, foreign spelling and so on
# at the moment I keep the films with no ratings to investigate the case futher and if possible to fix after initial round of tests

# I pick only the columns I want to visualise in the Excel-like table (sorting, filtering)
filmsMerged <- films_mergedDbs %>% select(c(1:4, 6:7))

# checking how it looks like
DT::datatable(filmsMerged)

# saving the file to run later with Rmarkdown to produce easy to use html file
saveRDS(filmsMerged, file = "filmsMerged.RData")

filmsMerged20181201 <- readRDS(file = "filmsMerged.RData")