# Twitter Analysis for wtmistanbul and wtmankara hastags

# load libraries
library(rtweet)
library(dplyr)
library(tidytext)
library(stringr)
library(janitor)
library(ggplot2)


# Getting tweets
tweetsIstanbul <- search_tweets(q = '#wtmistanbul', n = 10000, include_rts = FALSE, parse = TRUE)
tweetsAnkara <- search_tweets(q = '#wtmankara', n = 1000, include_rts = FALSE, parse = TRUE)

cleanTweetsIstanbul <- janitor::remove_empty_cols(tweetsIstanbul)
readr::write_csv(cleanTweetsIstanbul, path = "istanbulTweets.csv")

cleanTweetsAnkara <- janitor::remove_empty_cols(tweetsAnkara)
readr::write_csv(cleanTweetsAnkara, path = "ankaraTweets.csv")



# Getting users
usersIstanbul <- search_users(q = '#wtmistanbul', n = 100, parse = TRUE)
usersIstanbul <- unique(usersIstanbul)

usersAnkara <- search_users(q = '#wtmankara', n = 100, parse = TRUE)
usersAnkara <- unique(usersAnkara)


# Tidy data
tidyTweetIstanbul <- cleanTweetsIstanbul %>% 
  select(screen_name, user_id, created_at, status_id, text,
                               retweet_count, favorite_count, hashtags, source)


# Basic descriptions
# What is the most retweeted tweet?
mostRetweeted <- tidyTweetIstanbul %>% arrange(desc(retweet_count)) %>% select(text) %>% head(1)
getURLinsideTweet <- gsub(".*(https://)", "https://",mostRetweeted$text)
browseURL(getURLinsideTweet)

mostRetweeted$text
  
# What is the most favorited tweet?
tidyTweetIstanbul %>% arrange(desc(favorite_count)) %>% select(text) %>% head(1) 
getURLinsideReTweet <- gsub(".*(https://)", "https://",mostRetweeted$text)
browseURL(getURLinsideReTweet)

# What are the most used hashtags?
tidy_hashtags <- tidyTweetIstanbul %>% unnest_tokens(newhashtags, hashtags) 

tidy_hashtags <- tidy_hashtags %>%
  count(newhashtags, sort = TRUE) %>% top_n(n = 10, wt = n)

q <- ggplot(data = tidy_hashtags) + 
  geom_bar(aes(x = reorder(newhashtags, -n), y = n), stat = "identity", color = "purple") + 
  theme_bw() + 
  ylab("Hashtaglerin sayisi") + xlab("Hashtagler")

q + theme(axis.text.x = element_text(angle = 60, hjust = 1))

# ankara
tidy_hashtags_2 <- cleanTweetsAnkara %>% unnest_tokens(newhashtags2, hashtags) 

tidy_hashtags_2 %>%
  count(newhashtags2, sort = TRUE) %>% top_n(n = 10, wt = n)

# What are sources of tweets?
sources <- tidyTweetIstanbul %>%  select(source) %>% 
  count(source, sort = TRUE) %>% top_n(n = 10, wt = n)

#  Are the number of retweets and favorites correlated?
ggplot(tidyTweetIstanbul) +
  geom_point(aes(x = retweet_count, y = favorite_count)) + 
  scale_x_continuous(breaks = c(0, 2, 4, 6,8, 10)) +
  scale_y_continuous(breaks = c(0, 5, 10, 15, 20, 25))






