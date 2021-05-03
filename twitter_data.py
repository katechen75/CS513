import tweepy
import datetime
import pandas as pd
### PUT AUTHENTICATOIN KEYS HERE ###
API_KEY = "XGNkQ0rIzYGjQo3pI5DN7ZiI0"
API_SECRET_KEY = "rfaLXwGsLQL6dExb6k4YQrAxtvkoy98Y4f4u8XV6bXrhdOOovp"
ACCESS_TOKEN = "1326411459333664768-gq4363qekPzZT4wU0Uyh0DcV6UI6EB"
ACCESS_TOKEN_SECRET = "Yp6BeUE1Qaik3N60rBAJX96PlXfFvWUjATWVb8a1MwI51"

# Authentication
authenticate = tweepy.OAuthHandler(API_KEY, API_SECRET_KEY)
authenticate.set_access_token(ACCESS_TOKEN, ACCESS_TOKEN_SECRET)

# Use wait_on_rate_limit to avoid going over Twitter's rate limits
api = tweepy.API(authenticate, wait_on_rate_limit = True, wait_on_rate_limit_notify = True)
             
def retreiveTweetInfo(tweet):
  result = []
  userId = tweet.user.id_str
  screen_name = tweet.user.screen_name
  followers = tweet.user.followers_count
  friends = tweet.user.friends_count
  totalTweets = tweet.user.statuses_count
  tweetId = tweet.id_str
  tweetText = tweet.full_text
  created_at = tweet.created_at
  dayofWeek = created_at.weekday()
  retweet_count = tweet.retweet_count
  hashtags = list(map(lambda tag: tag['text'] ,tweet.entities['hashtags']))
  try:
    liked_count =  tweet.retweeted_status.favorite_count
    isReply = "retweeted_status" in tweet._json
  except AttributeError:
    liked_count = tweet.favorite_count
    isReply = False
  result = [userId, screen_name, followers, friends, totalTweets, tweetId, tweetText, created_at, dayofWeek, retweet_count, liked_count, isReply, hashtags]
  return result


def searchKeyword():
  # Keyword
  # removed: -filter:links
  searchQuery = 'Covid OR COVID OR covid or coronavirus or COVID-19' 
  data = []
  new_tweets = tweepy.Cursor(api.search, tweet_mode='extended', q = searchQuery, until='2021-04-28', lang='en', count=25).items(2000)
  for tweet in new_tweets:
    tweet_list = retreiveTweetInfo(tweet)
    data.append(tweet_list)
  return data
  
tweetData = searchKeyword()
tweet_df = pd.DataFrame(data=tweetData, columns=['User ID', 'Username', '# of Followers', '# of Friends', '# of Tweets', 'Tweet ID', 'Tweet Text', 'Tweet Created At', 'Day Of Week', '# of Retweets', '# of Likes', 'Is Tweet Reply', 'Tweet HashTags'])

tweet_df.to_csv('tweets_data.csv', index=False)