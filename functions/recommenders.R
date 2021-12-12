
# ============================================================
# Functions used in implementation of movie recommendation.
# ============================================================

# load libraries
library(data.table)
library(dplyr)
library(recommenderlab)
library(reshape2)
library(Matrix)
library(tidyverse)
library(data.table)

SYSTEM2.MODEL = "data/system2.best.model.rds"
set.seed(4675)
top.20.movies.global = rep(NA, 20)

if(file.exists('data/top_20_movies_global.rds')) {
  top.20.movies.global = readRDS('data/top_20_movies_global.rds')
}

# genre-based recommender
getRecommendationsByGenre <- function(movies, ratings, user_genre, topN = 10){
  print(paste("getRecommendationsByGenre()::", user_genre))

  movie_id_genres <- movies %>% filter(Genres == user_genre)
  print(length(movie_id_genres$MovieID))
  print(unique(movie_id_genres$MovieID))
  if(length(movie_id_genres$MovieID) == 0 || length(movie_id_genres$MovieID) < (topN + 5)) {
    print("More movies...")
    movie_id_genres <- movies %>% filter(grepl(user_genre, Genres))
  }
  ratings_mov_genres <- ratings %>% filter(MovieID %in% movie_id_genres$MovieID)
  dimension_names <- list(user_id = sort(unique(ratings_mov_genres$UserID)), movie_id =   sort(unique(ratings_mov_genres$MovieID)))
  
  # ratingmat <- spread(select(ratings_mov_genres, MovieID, UserID, Rating), MovieID, Rating) %>% select(-UserID)
  ratingmat = spread(ratings_mov_genres[, c("MovieID", "UserID", "Rating")], MovieID, Rating)
  ratingmat = ratingmat[ , -which(names(ratingmat) %in% c("UserID"))]
  
  ratingmat <- as.matrix(ratingmat)
  dimnames(ratingmat) <- dimension_names
  
  rating_rrm <- as(ratingmat , "realRatingMatrix")
  rating_rrm <- normalize(rating_rrm)
  
  MovRec_popular = Recommender(rating_rrm, method = 'POPULAR')
  rec.popular.predict = predict(MovRec_popular, rating_rrm, type = 'ratingMatrix')
  
  rec.popular.predict.matrix = as(rec.popular.predict, "matrix")
  
  rec.popular.predict.matrix[rec.popular.predict.matrix > 5] = 5
  rec.popular.predict.matrix[rec.popular.predict.matrix < 0] = 0
  
  movie_ratings_aggregration = colSums(rec.popular.predict.matrix, na.rm = TRUE)
  #print(sort(movie_ratings_aggregration, decreasing = TRUE)[1:20])
  recom_pop = as.numeric(names(sort(movie_ratings_aggregration, decreasing = TRUE)[1:topN]))
  
  print("***** Recommended.movies (Genre) ****")
  print(recom_pop)
  
  gc(verbose = FALSE)
  
  return (recom_pop)
}


# UBCF recommender
# user_ratings is a data table with two columns
#       MovieID Rating
# 1:       2      3
# 2:       1      5
getRecommendationsByUserRatings = function(system2.best.model, 
                                           Rmat,
                                           ratings, 
                                           user_ratings, 
                                           topN = 10){
  
  # When there is no user ratings input at all, we shall return top rated (globally) movies from our data base
  if (nrow(user_ratings) == 0) {
    return (top.20.movies.global[1:topN])
  }
  
  print("***** user_ratings ****")
  print(user_ratings)

  # Simulate a test user
  new.user = matrix(NA, 
                    nrow=1,
                    ncol=ncol(Rmat),
                    dimnames = list(user="TestUser1",
                                    item = colnames(Rmat)))  
  #Sample: new.user[1, c("m1", "m364", "m76", "m2081", "m588")] = c(4, 4, 5, 3, 4)
  new.user[1, paste0('m', user_ratings$MovieID)] = user_ratings$Rating
  new.user.rtmat = as(new.user, "realRatingMatrix")

  if (is.null(system2.best.model)) {
    if (file.exists(SYSTEM2.MODEL)) {
      system2.best.model = readRDS(SYSTEM2.MODEL)
      print("system2.best.model loading from file")
    } else {
      system2.best.model = Recommender(Rmat, method="UBCF", parameter = list(normalize="z-score", 
                                                                             method="cosine",
                                                                             weighted=FALSE,
                                                                             nn=10))
      saveRDS(system2.best.model, file="data/system2.best.model.rds")
      print("system2.best.model reconstructed")
    }
  } else {
    print("system2.best.model received")
  }

  # saveRDS(system2.best.model, file="system2.best.model.rds")
  pred.results = predict(system2.best.model, new.user.rtmat, type="topN", n=topN)
  # There is only one user at a time for each request
  pred.items = pred.results@items[[1]]
  
  recommended.movies = rep(NA, topN)
  
  # No predictions returned
  if(length(pred.items) == 0) {
    recommended.movies = (top.20.movies.global[1:topN])
  } else if(length(pred.items) < topN) { # when there are fewer movies got predicted than what we want 
    recommended.movies = replace(recommended.movies, 1:length(pred.items), pred.items)
    
    globalTop20MovieCounter = 0
    for(idx in seq_along(recommended.movies)) {
      if(is.na(recommended.movies[idx])) {
        
        # Find one of the global top 20 recommendation list movie which is not there in user input nor recommender predicted.
        # Essentially no duplicates in the topN final list presented to the user
        repeat {
          globalTop20MovieCounter = globalTop20MovieCounter + 1;
          
          if( (sum(which(recommended.movies == top.20.movies.global[globalTop20MovieCounter])) == 0)
              && (sum(which(user_ratings$MovieID == top.20.movies.global[globalTop20MovieCounter])) == 0))  {
            break
          }
        }
        
        recommended.movies[idx] = top.20.movies.global[globalTop20MovieCounter]
      }
    }
  } else {
    recommended.movies = pred.items[1:topN]
  }

  print("***** Recommended.movies (Ratings) ****")
  print(recommended.movies)
  
  gc(verbose = FALSE)
  
  return (recommended.movies)
}


# genre-based recommender 
# Unused
getRecommendationsByGenre2 <- function(movies, ratings, user_genre, topN = 10){
  print(user_genre)
  # combine rating information into movies
  mr = ratings %>% 
    group_by(MovieID) %>% 
    summarize(ratings_per_movie = n(), ave_ratings = mean(Rating)) %>%
    inner_join(movies, by = 'MovieID')
  
  # movies from selected genre
  mr_sel = mr[grep(user_genre, mr$Genres),]
  
  # recommender based on popularity
  recom_pop = arrange(mr_sel, desc(ratings_per_movie))
  
  recom_pop$MovieID[1:topN]
}
