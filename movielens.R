###########################################################
#
#      COMPLETE SET OF MOVIELENS PROJECT R SCRIPS.
#
# AUTHOR:    GUIDO D'ALESSANDRO
# DATE:      June 2nd, 2019
###########################################################
#
#                    README
#
# NOTES:    * MODIFY RAW_WORK_DIR VARIABLE BELOW TO YOUR 
#             MOVIELENS INSTALLATION DIRECTORY SETTINGS IN
#		      YOUR COMPUTER BEFORE RUNNING EG, "~MOVIELENS"
#
###########################################################
# Libraries.R

# Check if needed libraries are installed
installed_packages <- rownames( installed.packages() )
checkInstallPackage <- function(pckg) {
  if ( !(pckg %in% installed_packages) ) {
    install.packages("pckg")
  }
}

# caret
checkInstallPackage("caret")

# dslabs
checkInstallPackage("dslabs")

# dslabs
checkInstallPackage("dslabs")

# lubridate
checkInstallPackage("lubridate")

# ggrepel
checkInstallPackage("ggrepel")

# gridExtra
checkInstallPackage("gridExtra")

# knitr
checkInstallPackage("knitr")

# kableExtra
checkInstallPackage("kableExtra")

# tidyverse
checkInstallPackage("tidyverse")

# tinytex
if ( !("tinytex" %in% installed_packages) ) {
#
# NOTE: Requires a set path or reboot
# after installation for Markup to be
# able to detect it
#
  install.packages("tinytex")
  tinytex::install_tinytex()
}

##########################
# GLobal variables

# Working directory
raw_work_dir <- "~/datascience/capstone/movielens"
work_dir <-normalizePath(raw_work_dir, winslash="/")

# used data files -- REQUIRES setwd(work_dir)
dataset_files <- c("./data/downloaded/movielens.rds","./data/rds/edx.rds","./data/rds/validation.rds")

# RMSE challenge levels
RMSE_goal  <- 0.8567
RMSE_top10 <- 0.8800
RMSE_ldrbd <- 0.8890

# floating precisions
rnd_rmse <- 4
rnd_pct  <- 2

###################################
# Download and save Movilens dataset
###################################

# Note: this process could take a couple of minutes

# Set working directory
print("Setting working directory...")
work_dir <-normalizePath("~/datascience/capstone/movielens", winslash="/")
setwd(work_dir)

# Verify working directory
if ( getwd() != work_dir ) {
  stop("Incorrect working directory")
}
print( paste("======= Working directory set to ", getwd(), sep="") )


if ( file.exists(dataset_files) ) {

  # Set working directory
  print("======= Setting working directory...")
  setwd(work_dir)
  
  # Verify working directory
  if ( getwd() != work_dir ) {
    stop("Incorrect working directory")
  }
  print( paste("======= Working directory set to ", getwd(), sep="") )
  
  
  movielens <- readRDS(file="data/downloaded/movielens.rds")
  edx       <- readRDS(file="data/rds/edx.rds")
  val       <- readRDS(file="data/rds/validation.rds")
	
} else {

	# MovieLens 10M dataset:
	# Weong Link: https://grouplens.org/datasets/movielens/10m/
	# Correct Link: http://files.grouplens.org/datasets/movielens/ml-10m.zip

	print("======= Downloading main file...")

	dl <- tempfile()
	download.file("http://files.grouplens.org/datasets/movielens/ml-10m.zip", dl)

	ratings <- read.table(text = gsub("::", "\t", 
		readLines( unzip(dl, exdir = ".\\data", "ml-10M100K/ratings.dat"))),
			col.names = c("userId", "movieId", "rating", "timestamp"))

	movies <- str_split_fixed(readLines(unzip(dl, exdir = ".\\data", "ml-10M100K/movies.dat")), "\\::", 3)
	colnames(movies) <- c("movieId", "title", "genres")

	movies <- as.data.frame(movies) %>% mutate(movieId = as.numeric(levels(movieId))[movieId],
		title = as.character(title), genres = as.character(genres))

	movielens <- left_join(ratings, movies, by = "movieId")
	saveRDS(movielens, file="data/downloaded/movielens.rds")

	str(movielens, vec.len=1)

	###################################
	# CREATE TRAINING AND TESTING SETS
	###################################

	## Validation set will be 10% of MovieLens data

	## set.seed(1) # if using R 3.6.0: set.seed(1, sample.kind = "Rounding")
	print("======= Partitioning data... 90% train (edx), 10% test (validation)")
	set.seed(1, sample.kind = "Rounding")
	test_index <- createDataPartition(y = movielens$rating, times = 1, p = 0.1, list = FALSE)
	edx <- movielens[-test_index,]
	temp <- movielens[test_index,]

	# Make sure userId and movieId in validation set are also in edx set

	print("======= Creating test (val) section...")
	val <- temp %>% 
	  semi_join(edx, by = "movieId") %>%
	  semi_join(edx, by = "userId")

	# Add rows removed from validation set back into edx set

	removed <- anti_join(temp, val)

	print("Creating train (edx) section")
	edx <- rbind(edx, removed)

	print("Removing temporaries")
	rm(dl, ratings, movies, test_index, temp, removed)

	# Verify working directory
	if ( getwd() != work_dir ) {
	  stop("Incorrect working directory")
	}
	print("Working directory verifies OK")

	# Kepp a copy of these files for reference
	print("Saving a copy of the generated files")
	saveRDS(edx, file="data/generated/edx.rds")
	saveRDS(val, file="data/generated/validation.rds")

	###################################
	# DOWNLOADED TRAINING AND TEST
	# VERSION -- IN CASE OF DIFERENCES
	# NOTE THE DOWLOADED VERSIONS WERE
	# DOWNLOADED FROM:
	# https://drive.google.com/drive/folders/1IZcBBX0OmL9wu9AdzMBFUG8GoPbGQ38D 
	#
	# NOTE 5/6/19: Do not run the code below if you are using R version 3.6.0.
	# Instead, download the edx and validation data sets from here
	# https://drive.google.com/drive/folders/1IZcBBX0OmL9wu9AdzMBFUG8GoPbGQ38D 
	# Update 5/12/19: If you are using R version 3.6.0, please use
	# set.seed(1, sample.kind = "Rounding") instead of set.seed(1) in the code below.
	#
	# Later I was told that in case of discrepancies to use the downloaded versions
	# which is what much of the code below establishes

	print("======= Loading Manually downloaded versions...")

	edx_download <- readRDS("data/downloaded/edx.rds")
	val_download <- readRDS("data/downloaded/validation.rds")


	print("======= Comparing generated and downloaded versions...")

	e_same <- TRUE
	ne1 <- nrow(edx)
	ne2 <- nrow(edx_download)
	if ( ne1 != ne2 ) {
	  print( paste("Downloaded and generated edx.rds files differ by", abs(ne1-ne2), "rows"))
	  e_same <- FALSE
	} else {
	  ej <- inner_join(edx, edx_download)
	  nej <- nrow(ej)
	  if ( ne1 != nej ) {
		print( paste("Downloaded and generated edx.rds files differ at inner_join by", abs(ne1-nej), "rows"))
		e_same <- FALSE
	  }
	}
	if ( e_same ) {
	  print("edx.rds downloaded and generated are equal, using generated version")
	  saveRDS(edx, file="data/rds/edx.rds")
	} else {
	  print( "Because of edx.rds file differences, using downloaded edx version." )
	  edx <- edx_download
	  saveRDS(edx, file="data/rds/edx.rds")
	}

	v_same <- TRUE
	nv1 <- nrow(val)
	nv2 <- nrow(val_download)
	if ( nv1 != nv2 ) {
	  print( paste("Downloaded and generated validation.rds files differ by", abs(nv1-nv2), "rows"))
	  v_same <- FALSE
	} else {
	  vj <- inner_join(val, val_download)
	  nvj <- nrow(vj)
	  if ( nv1 != nvj ) {
		print( paste("Downloaded and generated validation.rds files differ at inner_join by", abs(nv1-nvj), "rows"))
		v_same <- FALSE
	  }
	}
	if ( v_same ) {
	  print("validation.rds downloaded and generated are equal, using generated version")
	  saveRDS(val, file="data/rds/validation.rds")
	} else {
		print( "Because of validation.rds file differences, using downloaded version." )
		val <- val_download
		saveRDS(val, file="data/rds/validation.rds")
	}
}
  
# edx.rds and validation.rds correct files saved to ./data/used
# objects edx and val contain correct rows

###########################
# Distinct Users and Movies
#
# NOTES:    * Uses the movielens table from datasets-load.R
#             or dataset-download.R
#           * Defines the distinct table used elsewhere


library(dplyr)

distinct <- movielens %>%  
  summarize(n_users = n_distinct(userId), n_movies = n_distinct(movieId))

distinct %>% knitr::kable()

##########################################################
# Calculates ratings per user arranged in descending order
#
# NOTES:    * USes edx from load-datasets-load.R or
#             datasets-download.R
#           * Defines the movie_ratings table used elsewhere


movie_ratings <- edx %>% group_by(movieId) %>%
  summarize(n_ratings = n(), avg_rating = mean(rating))

movie_ratings <- movie_ratings %>% arrange(desc(n_ratings), desc(avg_rating))

######################
# Missing Values Check
#
# NOTES:    * Uses the movielens table from datasets-load.R
#             or dataset-download.R

print( colnames(movielens)[colSums(is.na(movielens)) > 0] )

#######################
# Duplicated Rows Check
#
# NOTES:    * Uses the movielens table from datasets-load.R
#             or dataset-download.R

print( head( movielens[duplicated(movielens[c("movieId","userId")]),] ) )

###################################################
# Ratings Distribution
#
# Note:   * Uses movie_ratings table from Chunk-02.R

# Movies that are rated the most
movie_ratings %>% head(n=5)%>% knitr::kable()

# Movies that are rated the least
movie_ratings %>% tail(n=5) %>% knitr::kable()

#############################################
# Histogram of movie ratings
#
# NOTE:    * Uses edx table from datasets-load.R
#            or datasets-download.R
#          * Defines mu, used elsewhere


library(tidyverse)

# Overall rating average
mu <- mean(edx$rating)

# Plot the histogram
print(
  edx %>% ggplot( aes(rating) ) +
  geom_histogram(binwidth=0.5, aes(fill=..count..)) +
  labs(x = "Rating", y = "Times Applied") +
  geom_vline(xintercept = mu, color = "firebrick2", linetype = "dashed") +
  ggtitle("Ratings Distribution")
)

#################################################
# Plot of Avg Rating vs Times Rated
#
# NOTE: * Uses movie_ratings table from chunk-02.R


# Do the plot
print(
  movie_ratings %>%
  ggplot(aes(n_ratings, avg_rating)) +
  geom_point(shape=1, size=0.5, color="gray") + 
  geom_smooth(linetype="dashed", size=0.5, se=FALSE) +
  labs(x = "Times Rated", y = "Average Rating") +
  geom_vline(xintercept = 500, color = "firebrick2", linetype = "dashed") +
  ggtitle("Average Rating vs. Times Rated")
)

##################################################
# Ratings per Movie
#
# NOTES: * Uses movie_ratings table from chunk-02.R
#        * Defines user_ratings used elsewhere
#        * Defines t_ratings, used elsewhere

# targets
t_ratings <- nrow(edx)
q90 <- 0.9 * t_ratings
  
# Find index reaching 90%
n_ratings <- as.vector(movie_ratings$n_ratings)
s_ratings <- Reduce("+", n_ratings, accumulate = TRUE)
  
ndx <- min( which(s_ratings >= q90) )
qsum <- s_ratings[ndx]
  
# Save percentages
pct_q <- round(100*qsum/t_ratings, rnd_pct)
pct_movies <- round(100*ndx / nrow(movie_ratings), rnd_pct)
  
# Plot
print( ggplot(movie_ratings, aes( 1:nrow(movie_ratings), n_ratings)) +
    geom_point(shape = 20, aes(color=n_ratings)) +
    labs(x = "Movie Rating Rank", y = "Total Ratings", color = "Ratings") +
    geom_vline(xintercept = ndx, color = "firebrick2", linetype = "dashed") +
    ggtitle("Ratings per Movie") )

# Create user_ratings table to use them in
# inline with text
user_ratings <- edx %>% 
  group_by(userId) %>%
  summarize(n_ratings = n(), avg_rating = mean(rating))

user_ratings <- user_ratings %>% arrange(desc(n_ratings), desc(avg_rating))

######################################################
# Ratings per User
#
# NOTES:  * Uses edx table from datasets-load.R
#           or datasets-download.R
#         * Uses user_ratings table from chunk-08.R
#


# Most active users
user_ratings %>% head(n=5) %>% knitr::kable()

# Least active users
user_ratings %>% tail(n=5) %>% knitr::kable()

# #########################################################
# Users Rating Rank
# Ranks users according to how many movies they have ranked
#
# NOTES:    * Uses user_ratings table from chunk-09.R
#           * Uses t_ratings from chunk-08.R
#           

# Find index reaching 90%
n_ratings <- as.vector(user_ratings$n_ratings)
s_ratings <- Reduce("+", n_ratings, accumulate = TRUE)

# Calculate percentages
ndx <- min( which(s_ratings >= q90) )
qsum <- s_ratings[ndx]

# Save percentages
pct_q <- round(100*qsum/t_ratings, rnd_pct)
pct_users <- round(100*ndx / nrow(user_ratings), rnd_pct)

# Plot
range <- seq(1, nrow(user_ratings), 1)
print( 
  ggplot(user_ratings, aes( range, n_ratings)) +
  geom_point( aes(color=avg_rating) ) +
  labs(x = "User Rating Rank", y = "Ratings", color = "Avg. Rating") +
  geom_vline(xintercept = ndx, color = "firebrick2", linetype = "dashed") +
  ggtitle("Ratings per User")
)

###########################################################
# Average movie rating vs times rated by individual users
#
# NOTES:    * Uses user_ratings table from chunk-09.R

# Plot
print(
  user_ratings %>%
  ggplot(aes(n_ratings, avg_rating)) +
  geom_point(shape=1, size=0.5, color="gray") + 
  geom_smooth(linetype="dashed", size=0.5, se=FALSE) +
  labs(x = "Movies Rated", y = "Average Rating") +
  geom_vline(xintercept = 500, color = "firebrick2", linetype = "dashed") +
  ggtitle("Average Rating vs. Movies Rated")
)

##################################################################
# Average User Rating  
# Plots the histogram of the average movie ratings by single users
#
# NOTES:    * Uses user_ratings table from chunk-09.R


print(
  user_ratings %>%
  ggplot( aes(avg_rating) ) +
  geom_histogram(binwidth=0.5, aes(fill=..count..)) +
  labs(x = "Rating", color = "black") +
  geom_vline(xintercept = mu, color = "firebrick2", linetype = "dashed") +
  ggtitle("Average User Rating")
)

###########################################################
# Naive Method
#
# NOTES:    * Uses edx table from load-datasets.R
#           * Uses rnd_rmse variable from in global-vars.R
#           * Uses val table from load-datasets.R
#           * Defines f_RMSE(), , used elsewhere
#           * Defines m1-rmse, used elsewhere
#           * Defines result_tabs table, used elsewhere
#           * Re-defines mu, used elsewhere



# Define f_RMSE() -- Calculates RMSE rounded to 4 decimal places (rnd_rmse)
f_RMSE <- function(actual_rating, predicted_rating) {
  round( sqrt(mean((actual_rating - predicted_rating)^2, na.rm=TRUE)), rnd_rmse)
}

# Re-define mu (from chunk-06.R)
mu <- mean( edx$rating )
mu

# To compare with m1_rmse
sd(val$ratings)

# Next small chunk
m1_rmse <- f_RMSE(val$rating, mu)
m1_rmse

# Create RMSE tabs table
result_tabs <- data.frame(Method = "Naive", RMSE = m1_rmse, Improvement="NA")
result_tabs %>% knitr::kable()

##################################################
# Movie Effects Method 
#
# NOTES:    * Uses edx from load-datasets.R
#           * Uses val fromload-datasets.R
#           * uses f_RMSE() from method-naive.R
#           * uses result_tabs table from method-naive.R
#           * Defines avg_movie_ratings table used elsewhere
#           * Defines m2_rmse used elsewhere

# Compute the b_i's
avg_movie_ratings <- edx %>% group_by(movieId) %>%
  summarize(b_i = mean(rating - mu) ) 

# Predict with b_i's
y_hat <- val %>% left_join(avg_movie_ratings, by='movieId') %>% 
  mutate(pred = mu + b_i) %>% .$pred

# Calculate RMSE and save result
m2_rmse <- f_RMSE(val$rating, y_hat)

# Calculate improvement
improv <- round(100*(m1_rmse-m2_rmse)/m1_rmse, rnd_pct)
m2_rmse

# Save results in RMSE tabs table
result_tabs <- bind_rows(result_tabs, data.frame(Method="Naive + ME", 
  RMSE=m2_rmse, Improvement=paste(improv, "%", sep="")))

# Show results
result_tabs %>% knitr::kable()

##################################################
# User Effects Method 
#
# NOTES:    * Uses edx from load-datasets.R
#           * Uses val fromload-datasets.R
#           * uses f_RMSE() from method-naive.R
#           * uses result_tabs table from method-naive.R
#           * Uses avg_movie_ratings table from method-me.R
#           * Defines avg_user_rating table
#           * Defines m3_rmse used elsewhere

# Compute the b_u's
avg_user_rating <- edx %>%
  left_join(avg_movie_ratings, by='movieId') %>%
  group_by(userId) %>%
  summarize(b_u = mean(rating - mu - b_i))

# Predict with b_u's
y_hat <- val %>%
  left_join(avg_movie_ratings, by='movieId') %>%
  left_join(avg_user_rating, by='userId') %>%
  mutate(pred = mu + b_i + b_u) %>% .$pred

# Compute minimum RMSE
m3_rmse <- f_RMSE(val$rating, y_hat)
m3_rmse

# Calculate improvements
improv <- round(100*(m2_rmse-m3_rmse)/m2_rmse, rnd_pct)
overall <- round(100*(m1_rmse-m3_rmse)/m1_rmse, rnd_pct)

# Add results in RMSE tabs table
result_tabs <- bind_rows(result_tabs, data.frame(Method="Naive + ME + UE", 
  RMSE=m3_rmse, Improvement=paste(overall, "%", sep="")))

# Show results
result_tabs %>% knitr::kable()

##################################################
# Regularized Movie Effects Method 
#
# NOTES:    * Uses edx from load-datasets.R
#           * Uses val fromload-datasets.R
#           * uses f_RMSE() from method-naive.R
#           * uses result_tabs table from method-naive.R
#           * Defines m4_rmse used elsewhere


# Set lambda range values
lambdas <- seq(0, 10, 0.1)

# Iterate through lambdas for cross validation
rmses <- sapply(lambdas, function(lambda) {
  
  # Calculate the b_i's by movie
  b_i <- edx %>%
    group_by(movieId) %>%
    summarize(b_i = sum(rating - mu) / (lambda + n()))
  
  # Calculate ratings predictions on test dataset
  pred <- val %>%
    left_join(b_i, by="movieId") %>%
    mutate(pred = mu + b_i) %>% .$pred
  
  # Retrieve the RMSE from the test set
  return( f_RMSE(val$rating, pred) )
})

# Best Lambda
lambdas[ which.min(rmses) ]

# Min RMSE
m4_rmse <- min(rmses)
m4_rmse

# Save improvements
improv  <- round(100*(m2_rmse-m4_rmse)/m2_rmse, rnd_pct)
overall <- round(100*(m1_rmse-m4_rmse)/m1_rmse, rnd_pct)

# Add results in RMSE tabs table
result_tabs <- bind_rows(result_tabs, data.frame(Method="Naive + RME", 
                                                 RMSE=m4_rmse, Improvement=paste(overall, "%", sep="")))

# Print RMSE tabs 
result_tabs %>% knitr::kable()

##################################################
# Regularized User Effects Method 
#
# NOTES:    * Uses edx from load-datasets.R
#           * Uses val fromload-datasets.R
#           * uses f_RMSE() from method-naive.R
#           * uses result_tabs table from method-naive.R
#           * Defines m5_rmse used elsewhere

# Set lambda range values
lambdas <- seq(0, 10, 0.1)

# Iterate through lambdas for cross validation
rmses <- sapply(lambdas, function(lambda) {
  
  # b_i by movie
  b_i <- edx %>%
    group_by(movieId) %>%
    summarize(b_i = sum(rating - mu) / (lambda + n()))
  
  # b_u by users
  b_u <- edx %>%
    left_join(b_i, by="movieId") %>%
    group_by(userId) %>%
    summarize(b_u = sum(rating - mu - b_i) / (lambda + n()))
  
  # Calculate prediction ratings on test dataset
  pred <- val %>%
    left_join(b_i, by='movieId') %>%
    left_join(b_u, by='userId') %>%
    mutate(pred = mu + b_i + b_u) %>% .$pred
  
  # Retrieve the RMSE from the test set
  return( f_RMSE(val$rating, pred) )
})

# Best Lambda
lambdas[ which.min(rmses) ]

# Min RMSE
m5_rmse <- min(rmses)
m5_rmse

# Save improvements
improv  <- round(100*(m4_rmse-m5_rmse)/m4_rmse, rnd_pct)
overall <- round(100*(m1_rmse-m5_rmse)/m1_rmse, rnd_pct)

# Add results in RMSE tabs table
result_tabs <- bind_rows(result_tabs, data.frame(Method="Naive + RME + RUE", 
  RMSE=m5_rmse,  Improvement=paste(overall, "%", sep="")))

# Print RMSE tabs 
result_tabs %>% knitr::kable()

##################################################
# All Methods Results
#
# NOTES:    * Uses result_tabs table from method-naive.R

# Descriptive note
print("Here are the orginized saved results of our predictive models:")

# Organized in RMSE descending order 
result_tabs %>% arrange(desc(RMSE)) %>% knitr::kable()

