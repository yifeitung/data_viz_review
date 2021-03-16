# Import the tidyverse library
library(tidyverse)
library(rebus)
library(stringi)

# Loading in datasets/users.csv
users <- read_csv("datasets/users.csv")

# Count how many users we've got
nrow(users)

# Taking a look at the 12 first users
head(users, 12)

# Passwords should not be too short
# Verifying SHALL require subscriber-chosen memorized secrets to be at least 8 characters in length

# Calculate the lengths of users' passwords
users$length <- str_count(users$password)

# Flagging the users with too short passwords
users$too_short <- ifelse(users$length <8, 
                          TRUE, 
                          FALSE)

# Count the number of users with too short passwords
length(which(users$too_short == TRUE))

# Taking a look at the 12 first rows
head(users, 12)

# Common passwords people use
# Passwords obtained from previous breach corpuses
# Reading in the top 1000 passwords
common_passwords <- stringi::stri_read_lines("datasets/10_million_password_list_top_10000.txt")

# Taking a look at the top 100
head(common_passwords, 100)

# Password should not be common passwords
# Flagging the users with passwords that are common passwords
users$common_password <- ifelse(users$password %in% common_passwords, 
                                TRUE, 
                                FALSE)

# Count the number of users using common passwords
length(which(users$common_password == TRUE))

# Reading in a list of the 10000 most common words
words <- stringi::stri_read_lines("datasets/google-10000-english.txt")

# Flagging the users with passwords that are common words, the comparison should be case-insensitive
users$common_word <- ifelse(str_to_lower(users$password) %in% words, 
                            TRUE,
                            FALSE)

# Counting the number of users using common words as passwords
length(which(users$common_word == TRUE))

# Passwords should not be your name
# Extracting first and last names into their own columns
users$first_name <- str_extract(string = users$user_name, 
                                pattern = START %R% one_or_more(WRD))

users$last_name <- str_extract(string = users$user_name, 
                               pattern = one_or_more(WRD) %R% END)

# Flagging the users with passwords that matches their names
users$uses_name <- ifelse(str_to_lower(users$password) == users$first_name | str_to_lower(users$password) == users$last_name, TRUE, FALSE)

# Counting the number of users using names as passwords
length(which(users$uses_name == TRUE))

# Password should not be repetitive
# Flagging all passwords that contain 4 or more repeated characters

# Splitting the passwords into vectors of single characters
split_passwords <- str_split(str_to_lower(users$password), "")

# Picking out the max number of repeat characters for each password
users$max_repeats <- sapply(split_passwords, function(split_passwords){
  rle_password <- rle(split_passwords)
  max(rle_password$lengths)
})

# Flagging the passwords with >= 4 repeats
users$too_many_repeats <- ifelse(users$max_repeats >= 4, 
                                 TRUE,
                                 FALSE)

# Take a look at the users with too many repeats
users[users$too_many_repeats == TRUE, ]

# All together
# Flagging all passwords that are bad
# Add column which should be TRUE when a password is bad according to too_short, common_password, common_word, uses_name, or too_many_repeats
users$bad_password <- ifelse(users$too_short == TRUE | users$common_password == TRUE | users$common_word == TRUE | users$uses_name == TRUE | users$too_many_repeats == TRUE, TRUE, FALSE)

# Counting the number of bad passwords
length(which(users$bad_password == TRUE))

# Looking at the first 100 bad passwords
head(users[users$bad_password == TRUE, ], 100)









