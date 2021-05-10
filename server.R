## server.R

library(dplyr)
library(ggplot2)
library(recommenderlab)
library(DT)
library(data.table)
library(reshape2)
library(Matrix)

source('functions/helpers.R')

# Code for System 1
# define functions
get_user_ratings = function(value_list) {
  dat <- data.table(MovieID = sapply(strsplit(names(value_list), "_"),
                                    function(x) ifelse(length(x) > 1, x[[2]], NA)),
                   Rating = unlist(as.character(value_list)))
  # dat = data.table(MovieID = names(value_list),Rating = unlist(as.character(value_list)))
  dat <- dat[!is.null(Rating) & !is.na(MovieID)]
  dat[Rating == " ", Rating := 0]
  dat[, ':=' (MovieID = as.numeric(MovieID), Rating = as.numeric(Rating))]
  dat <- dat[Rating > 0]
  dat[,MovieID := paste0('m', MovieID)]

}

# read in data
myurl = "https://liangfgithub.github.io/MovieData/"
movies = readLines(paste0(myurl, 'movies.dat?raw=true'))
movies = strsplit(movies, split = "::", fixed = TRUE, useBytes = TRUE)
movies = matrix(unlist(movies), ncol = 3, byrow = TRUE)
movies = data.frame(movies, stringsAsFactors = FALSE)
colnames(movies) = c('MovieID', 'Title', 'Genres')
movies$MovieID = as.integer(movies$MovieID)
movies$Title = iconv(movies$Title, "latin1", "UTF-8")

small_image_url = "https://liangfgithub.github.io/MovieImages/"
movies$image_url = sapply(movies$MovieID,
                          function(x) paste0(small_image_url, x, '.jpg?raw=true'))

ratings = read.csv(paste0(myurl, 'ratings.dat?raw=true'),
                   sep = ':',
                   colClasses = c('integer', 'NULL'),
                   header = FALSE)
colnames(ratings) = c('UserID', 'MovieID', 'Rating', 'Timestamp')

## Ratings per Movie
ratings_m = ratings %>% 
  group_by(MovieID) %>% 
  summarize(ratings_per_movie = n(), ave_ratings = mean(Rating)) %>%
  inner_join(movies, by = 'MovieID')

## Create genre matrix
genres = as.data.frame(movies$Genres, stringsAsFactors=FALSE)
tmp = as.data.frame(tstrsplit(genres[,1], '[|]',
                              type.convert=TRUE),
                    stringsAsFactors=FALSE)
genre_list = c("Action", "Adventure", "Animation", 
               "Children's", "Comedy", "Crime",
               "Documentary", "Drama", "Fantasy",
               "Film-Noir", "Horror", "Musical", 
               "Mystery", "Romance", "Sci-Fi", 
               "Thriller", "War", "Western")
m = length(genre_list)
genre_matrix = matrix(0, nrow(movies), length(genre_list))
for(i in 1:nrow(tmp)){
  genre_matrix[i,genre_list %in% tmp[i,]]=1
}
colnames(genre_matrix) = genre_list
rownames(genre_matrix) = movies$MovieID
remove("tmp", "genres")

#Code for System 2
## Recommender System

# Create rating matrix
i = paste0('u', ratings$UserID)
j = paste0('m', ratings$MovieID)
x = ratings$Rating
tmp = data.frame(i, j, x, stringsAsFactors = T)
Rmat = sparseMatrix(as.integer(tmp$i), as.integer(tmp$j), x = tmp$x)
rownames(Rmat) = levels(tmp$i)
colnames(Rmat) = levels(tmp$j)
Rmat = new('realRatingMatrix', data = Rmat)

# Create the recommenders
e <- evaluationScheme(Rmat, method="split", train=0.9, given=3, goodRating=5)
r1 <- Recommender(getData(e, "train"), "UBCF", parameter = list(normalize = 'center',
                                                                method = 'Cosine', nn = 50))

## Shiny Server
 
shinyServer(function(input, output, session) {
  
  ## System 1
  
  #exact data from dataset
  datasetInput <- reactive({input$dataset})  
  
  # Calculate recommendations when the sbumbutton is clicked
  df1 <- eventReactive(input$btn1, {
    dataset = datasetInput()

    # get the movieID (Row in Movie)
    movielist = rownames(genre_matrix[genre_matrix[,dataset] == 1,])
    # Rank the movies based on their popularities
    genre_movies <- ratings_m %>% filter(MovieID %in% movielist) 
    genre_movies <- genre_movies %>% arrange(desc(ratings_per_movie))
    data.table(Number = 1:20, MovieID = genre_movies$MovieID[1:20], Title = genre_movies$Title[1:20])
    
  }) # clicked on button

  # display the recommendations
  output$results1 <- renderUI({
    num_rows <- 4
    num_movies <- 5
    recom_result <- df1()
    
    lapply(1:num_rows, function(i) {
      list(fluidRow(lapply(1:num_movies, function(j) {
        box(width = 2, status = "success", solidHeader = TRUE, title = paste0("Number ", (i - 1) * num_movies + j),
            
            div(style = "text-align:center", 
                a(img(src = movies$image_url[which(movies$MovieID==recom_result$MovieID[(i - 1) * num_movies + j])], height = 150))
            ),
            div(style="text-align:center; font-size: 100%", 
                strong(movies$Title[which(movies$MovieID==recom_result$MovieID[(i - 1) * num_movies + j])])
            )
            
        )        
      }))) # columns
    }) # rows
    
  }) # renderUI function
  
  ## System 2

  # show the movies to be rated
  output$ratings <- renderUI({
    num_rows <- 40
    num_movies <- 6 # movies per row

    lapply(1:num_rows, function(i) {
      list(fluidRow(lapply(1:num_movies, function(j) {
        list(box(width = 2,
                 div(style = "text-align:center", img(src = movies$image_url[(i - 1) * num_movies + j], height = 150)),
                 div(style = "text-align:center", strong(movies$Title[(i - 1) * num_movies + j])),
                 div(style = "text-align:center; font-size: 150%; color: #f0ad4e;", ratingInput(paste0("select_", movies$MovieID[(i - 1) * num_movies + j]), label = "", dataStop = 5)))) #00c0ef
      })))
    })
  })

  # Calculate recommendations when the sbumbutton is clicked
  df2 <- eventReactive(input$btn2, {
    withBusyIndicatorServer("btn2", { # showing the busy indicator
      # hide the rating container
      useShinyjs()
      jsCode <- "document.querySelector('[data-widget=collapse]').click();"
      runjs(jsCode)

      # get the user's rating data
      value_list <- reactiveValuesToList(input)
      user_ratings <- get_user_ratings(value_list)
      m_new <- sort(unique(paste0('m', ratings$MovieID)))
      u_new <- rep("u000", length(m_new))
      r_new <- rep(0, length(m_new))
      Rnew = data.table(UserID=u_new, MovieID=m_new, Rating=r_new, stringsAsFactors = T)
      # write.table(Rnew, file = 'rec.txt', sep = ',')
      Rnew[MovieID %in% user_ratings$MovieID, Rating:= as.numeric(user_ratings$Rating)]
      Rnew[Rating == 0, Rating := NA]
      # write.table(Rnew, file = 'rec2.txt', sep = ',')
      RSnew = matrix(Rnew$Rating, ncol=length(m_new), dimnames=list(UserID="u000", MovieID=Rnew$MovieID))
      RSnew <- as(RSnew,"realRatingMatrix")

      movielist <- predict(r1, RSnew, type="topNList", n=20)
      movielist <- as(movielist,"list")[[1]]
      movielist <- as.numeric(lapply(movielist, function(x) substr(x, 2, nchar(x))))

      # Rank the movies based on their popularities
      rec_movies <- ratings_m %>% filter(MovieID %in% movielist) 
      rec_movies <- rec_movies %>% arrange(desc(ratings_per_movie))
      data.table(Number = 1:20, MovieID = rec_movies$MovieID[1:20], Title = rec_movies$Title[1:20])
      
    }) # still busy

  }) # clicked on button


  # display the recommendations
  output$results2 <- renderUI({
    num_rows <- 4
    num_movies <- 5
    recom_result <- df2()

    lapply(1:num_rows, function(i) {
      list(fluidRow(lapply(1:num_movies, function(j) {
        box(width = 2, status = "success", solidHeader = TRUE, title = paste0("Rank ", (i - 1) * num_movies + j),

            div(style = "text-align:center",
                a(img(src = movies$image_url[which(movies$MovieID==recom_result$MovieID[(i - 1) * num_movies + j])], height = 150))
            ),
            div(style="text-align:center; font-size: 100%",
                strong(movies$Title[which(movies$MovieID==recom_result$MovieID[(i - 1) * num_movies + j])])
            )

        )
      }))) # columns
    }) # rows

  }) # renderUI function
  
}) # server function