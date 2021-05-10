##############################################################################
# server.R
##############################################################################

source('functions/helpers.R')


library(dplyr)
library(recommenderlab)
library(DT)
library(data.table)
library(reshape2)


# DEFINE FUNCTION
get_user_ratings = function(value_list) {
    dat = data.table(MovieID = sapply(strsplit(names(value_list), "_"),
                                       function(x) ifelse(length(x) > 1, x[[2]], NA)),
                      Rating = unlist(as.character(value_list)))
    dat = dat[!is.null(Rating) & !is.na(MovieID)]
    dat[Rating == " ", Rating := 0]
    dat[, ':=' (MovieID = as.numeric(MovieID), Rating = as.numeric(Rating))]
    dat = dat[Rating > 0]
    dat[,MovieID := paste0('m', MovieID)]
    
}

# READ DATA
movies = readLines('./data/movies.dat')
movies = strsplit(movies, split = "::", fixed = TRUE, useBytes = TRUE)
movies = matrix(unlist(movies), ncol = 3, byrow = TRUE)
movies = data.frame(movies, stringsAsFactors = FALSE)
colnames(movies) = c('MovieID', 'Title', 'Genres')
movies$MovieID = as.integer(movies$MovieID)
movies$Title = iconv(movies$Title, "latin1", "UTF-8")

small_image_url = "https://liangfgithub.github.io/MovieImages/"
movies$image_url = sapply(movies$MovieID,
                          function(x) paste0(small_image_url, x, '.jpg?raw=true'))

ratings = read.csv('./data/ratings.dat',
                   sep = ':',
                   colClasses = c('integer', 'NULL'),
                   header = FALSE)
colnames(ratings) = c('UserID', 'MovieID', 'Rating', 'Timestamp')

rate_per_movie = ratings %>% 
    group_by(MovieID) %>% 
    summarize(ratings_per_movie = n(), ave_ratings = mean(Rating)) %>%
    inner_join(movies, by = 'MovieID')


## GENERATE GENRE MATRIX
genres = as.data.frame(movies$Genres, stringsAsFactors=FALSE)
tmp = as.data.frame(tstrsplit(genres[,1], '[|]',
                              type.convert=TRUE),
                    stringsAsFactors=FALSE)

# AVAILABLE GENRES
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


# CREATE REAL RATING MATRIX
i = paste0('u', ratings$UserID)
j = paste0('m', ratings$MovieID)
x = ratings$Rating
tmp = data.frame(i, j, x, stringsAsFactors = TRUE)
Rmat = sparseMatrix(as.integer(tmp$i), as.integer(tmp$j), x = tmp$x)
rownames(Rmat) = levels(tmp$i)
colnames(Rmat) = levels(tmp$j)
Rmat = new('realRatingMatrix', data = Rmat)


# GENERATE RECOMMENDER (via recommenderlab package)
recommender <<- Recommender(Rmat, 
                            method = "UBCF", 
                            param=list(method="Cosine",nn=25))

sys_2_helper = function(value_list) {
    user_ratings = get_user_ratings(value_list)
    m_new = sort(unique(paste0('m', ratings$MovieID)))
    u_new = rep("cdubbs", length(m_new))
    r_new = rep(0, length(m_new))
    RN = data.table(UserID=u_new, MovieID=m_new, Rating=r_new, stringsAsFactors = TRUE)
    RN[MovieID %in% user_ratings$MovieID, Rating:= as.numeric(user_ratings$Rating)]
    RN[Rating == 0, Rating := NA]
    RSN = matrix(RN$Rating, ncol=length(m_new), dimnames=list(UserID="cdubbs", MovieID=RN$MovieID))
    RSN <- as(RSN,"realRatingMatrix")
    
    ml = predict(recommender, RSN, type="topNList", n=10)
    ml = as(ml,"list")[[1]]
    ml = as.numeric(lapply(ml, function(x) substr(x, 2, nchar(x))))
    print(ml)
    # RANK MOVIES BY POPULARITY
    rec_movies = rate_per_movie %>% filter(MovieID %in% ml) 
    rec_movies = rec_movies %>% arrange(desc(ratings_per_movie))
    data.table(Number = 1:10, MovieID = rec_movies$MovieID[1:10], Title = rec_movies$Title[1:10])
    
}
##############################################################################
# SHINY SERVER
##############################################################################
shinyServer(function(input, output, session) {
    
    ############
    # SYSTEM 1
    ############
    
    # ASSIGN DATA
    datasetInput = reactive({input$dataset})  
    
    # CALCULATE RECOMMENDATIONS
    recom1 = eventReactive(input$btn1, {
        dataset = datasetInput()
        
        # SELECT MOVIE ID
        ml = rownames(genre_matrix[genre_matrix[,dataset] == 1,])
        
        # RANK MOVIES BY POPULARITY
        genre_movies = rate_per_movie %>% filter(MovieID %in% ml) 
        genre_movies = genre_movies %>% arrange(desc(ratings_per_movie))
        data.table(Number = 1:15, MovieID = genre_movies$MovieID[1:15], 
                   Title = genre_movies$Title[1:15])
        
    }) # BUTTON CLICKED
    
    # DISPLAY RESULTS
    output$results1 = renderUI({
        num_rows = 3
        num_movies = 5
        recom_result = recom1()
        
        lapply(1:num_rows, function(i) {
            list(fluidRow(lapply(1:num_movies, function(j) {
                box(width = 2, status = "primary", solidHeader = TRUE, 
                    title = paste0("#", (i - 1) * num_movies + j),
                    
                    div(style = "text-align:center", 
                        a(img(src = movies$image_url[which(movies$MovieID==recom_result$MovieID[(i - 1) * num_movies + j])], 
                              height = 150))
                    ),
                    div(style="text-align:center; font-size: 100%", 
                        strong(movies$Title[which(movies$MovieID==recom_result$MovieID[(i - 1) * num_movies + j])])
                    )
                    
                )        
            }))) # COLUMN
        }) # ROW
        
    }) # renderUI FUNCTION
    
    ############
    # SYSTEM 2
    ############
    
    # DISPLAY MOVIES TO RATE
    output$ratings = renderUI({
        num_rows = 40
        num_movies = 6 # movies per row
        
        lapply(1:num_rows, function(i) {
            list(fluidRow(lapply(1:num_movies, function(j) {
                list(box(width = 2,
                         div(style = "text-align:center", img(src = movies$image_url[(i - 1) * num_movies + j], height = 150)),
                         div(style = "text-align:center", strong(movies$Title[(i - 1) * num_movies + j])),
                         div(style = "text-align:center; font-size: 150%; color: #f0ad4e;", ratingInput(paste0("select_", movies$MovieID[(i - 1) * num_movies + j]), label = "", dataStop = 5)))) #00c0ef
            })))
        })
        
    })
    
    # CALCULATE RECOMMENDATIONS
    recom2 = eventReactive(input$btn2, {
        withBusyIndicatorServer("btn2", { # showing the busy indicator
            # COLLAPSE RATINGS
            useShinyjs() # EXTEND SHINY TO USE CUSTOM JS
            runjs("document.querySelector('[data-widget=collapse]').click();")
            
            # GET USER RATINGS
            movie_list = sys_2_helper(reactiveValuesToList(input))
            print(movie_list)
        }) # BUSY
        
    }) # BUTTON CLICKED
    
    
    # DISPLAY RECOMMENDATIONS
    output$results2 = renderUI({
        num_rows = 2
        num_movies = 5
        recom_result = recom2()
        
        lapply(1:num_rows, function(i) {
            list(fluidRow(lapply(1:num_movies, function(j) {
                box(width = 2, status = "primary", solidHeader = TRUE, title = paste0("Rank ", (i - 1) * num_movies + j),
                    
                    div(style = "text-align:center",
                        a(img(src = movies$image_url[which(movies$MovieID==recom_result$MovieID[(i - 1) * num_movies + j])], 
                              height = 150))
                    ),
                    div(style="text-align:center; font-size: 100%",
                        strong(movies$Title[which(movies$MovieID==recom_result$MovieID[(i - 1) * num_movies + j])])
                    )
                    
                )
            }))) # COLUMN
        }) # ROW
        
    }) # renderUI FUNCTION
    
}) # SERVER FUNCTION