#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)

get_user_ratings = function(value_list) {
    dat = data.table(MovieID = sapply(strsplit(names(value_list), "_"), 
                                      function(x) ifelse(length(x) > 1, x[[2]], NA)),
                     Rating = unlist(as.character(value_list)))
    dat = dat[!is.null(Rating) & !is.na(MovieID)]
    dat[Rating == " ", Rating := 0]
    dat[, ':=' (MovieID = as.numeric(MovieID), Rating = as.numeric(Rating))]
    dat = dat[Rating > 0]
    
    user_ratings = sparseMatrix(i = rep(1, nrow(dat)),
                                j = dat$MovieID,
                                x = dat$Rating,
                                dims = c(1, ncol(ratings)))
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

# Convert to user x movie ratings matrix
# We know that for this data, each user has at least 20 ratings
ratings = sparseMatrix(
    i = ratings$UserID,
    j = ratings$MovieID,
    x = ratings$Rating
)

r = Recommender(normalize(as(ratings, "realRatingMatrix"), "center"),
                "UBCF",
                param=list(method="cosine", nn=10))

#################
# System 1 code
popular = read.table("data/popular_by_genre.dat", sep="|", header=TRUE)
genres = unique(popular$genre)

#########################################

shinyServer(function(input, output, session) {
    
    output$box1Title = renderText({
        ifelse(input$recommendationType == "Genre",
               "Step 1: Select a genre",
               "Step 1: Rate as many movies as possible")
    })
    
    output$box1Class = renderText({
        ifelse(input$recommendationType == "Genre",
               "genres",
               "rateitems")
    })

    # Give the user a choice
    output$step1 <- renderUI({
        if (input$recommendationType == "Genre") {
            selectInput("genre", "Genre", 
                        choices = sort(genres), 
                        )
        } else {
            num_rows <- 20
            num_movies <- 6 # movies per row
            
            lapply(1:num_rows, function(i) {
                list(fluidRow(lapply(1:num_movies, function(j) {
                    list(box(width = 2,
                             div(style = "text-align:center", img(src = movies$image_url[(i - 1) * num_movies + j], height = 150)),
                             div(style = "text-align:center", strong(movies$Title[(i - 1) * num_movies + j])),
                             div(style = "text-align:center; font-size: 150%; color: #f0ad4e;", ratingInput(paste0("select_", movies$MovieID[(i - 1) * num_movies + j]), label = "", dataStop = 5)))) #00c0ef
                })))
            })
        }

    })
    

    
    # Calculate recommendations when the sbumbutton is clicked
    df <- eventReactive(input$btn, {
        withBusyIndicatorServer("btn", { # showing the busy indicator
            # hide the rating container
            useShinyjs()
            jsCode <- "document.querySelector('[data-widget=collapse]').click();"
            runjs(jsCode)
            
            if (input$recommendationType == "Genre") {
                recom_results <- subset(popular, genre==input$genre)
            } else {

                
                # get the user's rating data
                value_list <- reactiveValuesToList(input)
                #browser()
                user_ratings <- get_user_ratings(value_list)
            
                # If too few rated, then don't normalize
                if (user_ratings@x == 0) {
                    recom_results <- popular[order(popular$avg_rating, decreasing=TRUE), ]
                    return(recom_results)
                } else if (user_ratings@x < 4) {
                    # If too few rated, then don't normalize
                    user_matrix = as(user_ratings, "realRatingMatrix")
                } else {
                    user_matrix = normalize(as(user_ratings, "realRatingMatrix"))
                }
                p = predict(r, user_matrix, type="ratings")

                ####################################
                num_movies = 5
                user_predicted_ids = order(p@data[1, ], decreasing=TRUE)[1:num_movies]
                user_results = p@data[1, user_predicted_ids]
                recom_results <- data.table(Rank = 1:5, 
                                            MovieID = movies$MovieID[user_predicted_ids], 
                                            Title = movies$Title[user_predicted_ids], 
                                            Predicted_rating =  user_results)
            }
        }) # still busy
        
    }) # clicked on button
    
    
    # display the recommendations
    output$results <- renderUI({
        num_rows <- 1
        num_movies <- 5
        recom_result <- df()
        
        lapply(1:num_rows, function(i) {
            list(fluidRow(lapply(1:num_movies, function(j) {
                box(width = 2, status = "success", solidHeader = TRUE, title = paste0("Rank ", (i - 1) * num_movies + j),
                    
                    div(style = "text-align:center", 
                        a(img(src = movies$image_url[recom_result$MovieID[(i - 1) * num_movies + j]], height = 150))
                    ),
                    div(style="text-align:center; font-size: 100%", 
                        strong(movies$Title[recom_result$MovieID[(i - 1) * num_movies + j]])
                    )
                    
                )        
            }))) # columns
        }) # rows
        
    }) # renderUI function
    
}) # server function
