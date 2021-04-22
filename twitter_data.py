# I pledge my honor that I have abided by the Stevens Honor System. 
# Author: Katelyn Chen
# Assignment 07
# Program purpose: to retrieve and analyze data using the Twitter API
# Program Inputs: twitter username to be analyzed with the twitter user site
# Program Returns: twitter user's account information, recent followers, and recent tweets
# To run in a terminal window type: python twitter_data.py 
# When prompted, type in the username you wish to search or "STOP" to terminate the program

import tweepy
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
             
# Get Information About a Twitter User Account
def retrieveAccountInfo():    
  # Get Basic Account Information
  print("User Screen Name: ", userName.screen_name)
  print("User Name: ", userName.name)
  print("User ID: ", userName.id)

  # prints out N/A for no description
  if userName.description == "":
    print("User Description: N/A")
  else:
    print("User Description: ", userName.description)

  # prints out N/A for no location
  if userName.location == "":
    print("Location: N/A")
  else:
    print("Location: ", userName.location)

  # prints the users friends and follower count
  print("Number of Friends: ", userName.friends_count)
  print("Number of Followers: ", userName.followers_count)

# retrieves the 5 most recent followers 
def retrieveUserFollowers():
  followerCount = 0
  for followers in tweepy.Cursor(api.followers, screen_name=userName.screen_name).items(5):
    followerCount += 1
    print("FOLLOWER " + str(followerCount) + ": " + followers.screen_name)

# retrieves the 5 most recent tweets 
def retrieveUserTweets():
  tweetCount = 0
  for tweets in tweepy.Cursor(api.user_timeline, screen_name=userName.screen_name).items(10):
    tweetCount += 1
    print("TWEET " + str(tweetCount) + ": " + tweets.text + "\n")

def retreiveTweetInfo(tweet):
  result = []
  screen_name = tweet.user.screen_name
  followers = tweet.user.followers_count
  location = tweet.user.location
  created_at = tweet.created_at
  retweet_count = tweet.retweet_count
  ## liked_count is returning 0 for all, idk why
  liked_count = tweet.favorite_count
  result = [screen_name, followers, location, created_at, retweet_count, liked_count]
  return result


def searchKeyword():
  # Keyword
  # removed: -filter:links
  searchQuery = 'Covid OR COVID OR covid ' 
  data = []
  new_tweets = tweepy.Cursor(api.search, q = searchQuery,result_type = "recent", lang = "en").items(10)
  for tweet in new_tweets:
    tweet_list = retreiveTweetInfo(tweet)
    data.append(tweet_list)
  return data
  

# continuous loop until the user types "STOP"
# while True:
  # prompts user for username to search and analyze
  # inputName = input("Please enter your twitter user name or 'STOP' to end: ")
  # userName = api.get_user(inputName)

  # # checks if the input is "STOP"
  # if inputName == "STOP":
  #   # stops the program (breaks) and prints message
  #   print("\nProgram ended, thank you for using twitter_data.py! ")
  #   break
  # else:
  #   # runs function then prompts user for name again
  #   print("\n------------- Account Information ---------------")
  #   retrieveAccountInfo()
  #   print("\n------------- Recent 5 Followers ----------------")
  #   retrieveUserFollowers()
  #   print("\n-------------- Recent 5 Tweets ------------------")
  #   retrieveUserTweets()  
print("\n---------- Recent 5 COVID searches --------------")
tweetData = searchKeyword()

tweet_df = pd.DataFrame(data=tweetData, columns=['Username', '# of Followers', 'Location', 'Tweet Created At', '# of Retweets', '# of Likes'])

tweet_df.to_csv('tweets_data.csv', header=True )