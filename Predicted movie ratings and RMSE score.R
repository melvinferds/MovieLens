#############################################################
# ********************************************************* #
# *                                                       * #
# *   HarvardX Professional Cetificate in Data Science    * #
# *                                                       * #
# *              PH125.9X: Capstone Project               * #
# *                                                       * #
# ********************************************************* #
#############################################################


#############################################################
# SECTION 1 - PROVIDED BY COURSE STAFF TO LOAD DATA         #
#############################################################

# Create edx set, validation set, and submission file

# Note: this process could take a couple of minutes

if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")
if(!require(caret)) install.packages("caret", repos = "http://cran.us.r-project.org")

# MovieLens 10M dataset:
# https://grouplens.org/datasets/movielens/10m/
# http://files.grouplens.org/datasets/movielens/ml-10m.zip

dl <- tempfile()
download.file("http://files.grouplens.org/datasets/movielens/ml-10m.zip", dl)

ratings <- read.table(text = gsub("::", "\t", readLines(unzip(dl, "ml-10M100K/ratings.dat"))),
                      col.names = c("userId", "movieId", "rating", "timestamp"))

movies <- str_split_fixed(readLines(unzip(dl, "ml-10M100K/movies.dat")), "\\::", 3)
colnames(movies) <- c("movieId", "title", "genres")
movies <- as.data.frame(movies) %>% mutate(movieId = as.numeric(levels(movieId))[movieId],
                                           title = as.character(title),
                                           genres = as.character(genres))

movielens <- left_join(ratings, movies, by = "movieId")

# Validation set will be 10% of MovieLens data

set.seed(1)
test_index <- createDataPartition(y = movielens$rating, times = 1, p = 0.1, list = FALSE)
edx <- movielens[-test_index,]
temp <- movielens[test_index,]

# Make sure userId and movieId in validation set are also in edx set

validation <- temp %>% 
  semi_join(edx, by = "movieId") %>%
  semi_join(edx, by = "userId")

# Add rows removed from validation set back into edx set

removed <- anti_join(temp, validation)
edx <- rbind(edx, removed)

rm(dl, ratings, movies, test_index, temp, movielens, removed)


#############################################################
#                    END OF SECTION 1                       #
#############################################################

#***********************************************************#



#############################################################
# SECTION 2 - Loss Funtion                                  #
#############################################################

RMSE <- function(true_ratings, predicted_ratings){
  sqrt(mean((true_ratings - predicted_ratings)^2))
}

#############################################################
#                    END OF SECTION 2                       #
#############################################################

#***********************************************************#


#############################################################
# SECTION 3 - Models and Corresponding RMSE's               #
#############################################################

# SECTION 3.1 - Model with same rating for all movies and users
mu <- mean(edx$rating)
mu

# RMSE
base_rmse <- RMSE(validation$rating, mu)
base_rmse


# SECTION 3.2 - Add movie effects

# Add term to represent average ranking by movie
movie_effect <- edx %>% 
  group_by(movieId) %>% 
  summarize(b_i = mean(rating - mu))

# Predictions including movie effect
predicted_ratings <- mu + validation %>%
  left_join(movie_effect, by='movieId') %>% .$b_i

# RMSE
model_1_rmse <- RMSE(predicted_ratings, validation$rating)
model_1_rmse 


# SECTION 3.3 - Add user effects

# Add term to represent average ranking by user
user_effect <- edx %>% 
  left_join(movie_effect, by='movieId') %>%
  group_by(userId) %>%
  summarize(b_u = mean(rating - mu - b_i))

# Predictions including movie effect and user effect
predicted_ratings <- validation %>% 
  left_join(movie_effect, by='movieId') %>%
  left_join(user_effect, by='userId') %>%
  mutate(pred = mu + b_i + b_u) %>% .$pred

# RMSE
model_2_rmse <- RMSE(predicted_ratings, validation$rating)
model_2_rmse 

# SECTION 3.4 - Add Regularisation













#############################################################
#                    END OF SECTION 3                       #
#############################################################

#***********************************************************#
