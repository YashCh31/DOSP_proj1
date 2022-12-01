-module(api).

% Exported Functions

-export([get_timeline/3,
         get_tweets/3,
         tweet/3]).

% API Functions

% Request a page of the timeline of a particular user.
-spec get_timeline(pid(), integer(), integer()) -> [{tweet, integer(), erlang:timestamp(), string()}].
get_timeline(ServerPid, UserId, Page) ->
    ServerPid ! {self(), get_timeline, UserId, Page},
    receive
        {ServerPid, timeline, UserId, Page, Timeline} ->
            Timeline
    end.

% Request a page of tweets of a particular user.
-spec get_tweets(pid(), integer(), integer()) -> [{tweet, integer(), erlang:timestamp(), string()}].
get_tweets(ServerPid, UserId, Page) ->
    ServerPid ! {self(), get_tweets, UserId, Page},
    receive
        {ServerPid, tweets, UserId, Page, Tweets} ->
            Tweets
    end.

% Submit a tweet for a user.
-spec tweet(pid(), integer(), string()) -> [{tweet, integer(), erlang:timestamp(), string()}]. 
tweet(ServerPid, UserId, Tweet) ->
    ServerPid ! {self(), tweet, UserId, Tweet},
    receive
        {ServerPid, tweet_accepted, UserId, Timestamp, TweetId} ->
            [{"tweeted successfully", UserId, Timestamp, TweetId, Tweet}]
    end.
