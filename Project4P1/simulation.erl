%% This module generates some workload for the twitter application
%% in a certain way - of course, this covers only one particular scenario
%% 
%% Its interface allows to specify some hooks in your code where the 
%% methods can be found for initialization, user registration and 
%% user subscription. Of course, it expects them to return the same results
%% as in the example "actor.erl".
%%
%% It defines a user profile based on the following parameters
%% NumberOfUsers : number of users to be created
%% Subscriptions : number of followers for each user
%% ActionCount   : how many times each action (tweet, get_tweets, get_timeline)
%%                 is repeated for each user
%%
%% Actions can be run in order (first all tweets, then all get_tweets, etc...) or
%% can be interleaved.
%%
%% Tweets are generated randomly. If you feel this is a heavy burden, just change
%% the text string to "Hello World" which will save you a lot of RAM and CPU time.
%%
%% Now and then, some statistics are printed to the screen.

-module(simulation).

%%
%% Exported Functions
%%
-export([start/0, generate_users/2, generate_subscriptions/3, exec_tweet/1,
	exec_get_tweets/2, exec_get_timeline/1]).


%% the start() method can be extended to your other configuration
%% for the moment, only the actor is sopported
start() -> start_server_single_actor().
%% start() -> start_server_my_new_actor().

%% different configurations can be tested with this scenario
%% here is where you enter your application's hooks for initialization,
%% user registration and user subscription.
start_server_single_actor() ->
	erlang:display("Load test for: actor"),
	start(fun actor:initialize/0,
	fun actor:register_user/0,
	fun actor:subscribe/3).

%% generic start method
start(Initializer, Registrar, Subscriber) ->

	% call the initialize() method for the configuration we want to test 
	Initializer(),
	
	% initialize statistics: put this anywhere you want to start measuring
	statistics(runtime),
	statistics(wall_clock),

	% user configuration
	% UserType = {NumberOfUsers, Subscriptions, ActionCount}
	% NumberOfUsers : number of users to be created
	% Subscriptions : number of followers for each user
	% ActionCount   : how many times each action (tweet, get_tweets, get_timeline)
	%                 is repeated for each user
	UserType = {1000, 10, 10},
	
	% generate users, result is a list of {UserId, Pid}
	Users = generate_users(UserType, Registrar),

	% generate subscriptions
	generate_subscriptions(Users, UserType, Subscriber),
	print_statistics("Statistics for: Generating users and their subscriptions"),
	
	% execute the other actions: by action type, or interleaved
	execute_actions_by_type(Users, UserType),
	% execute_actions_interleaved(Users, UserType),

	erlang:display("Finished").
	
	
% generates the clients as specified in your "register_user" function
generate_users(UserType, Registrar) ->
	{NumberOfUsers, _Subscriptions, _ActionCount} = UserType,
	UsersToCreate = lists:duplicate(NumberOfUsers, []),
	lists:map(fun(_User) -> Registrar() end, UsersToCreate).
	
% generates the requested number of subscriptions per user, on a random basis
generate_subscriptions(Users, UserType, Subscriber) ->
	{NumberOfUsers, Subscriptions, _ActionCount} = UserType,
	{A1,A2,A3} = now(),
    random:seed(A1, A2, A3),
	lists:foreach(fun(User) -> 
					{UserId, UserPid} = User,
					UsersToConnectTo = lists:sublist(Users, random:uniform(NumberOfUsers - Subscriptions), Subscriptions),
					UserIdsToConnectTo = lists:map(fun({UserId2, _UserPid2}) -> UserId2 end, UsersToConnectTo),
					% now connect to each of the requested subscriptions
					lists:foreach(fun(UserId2) ->
									Subscriber(UserPid, UserId, UserId2)
									end,
									UserIdsToConnectTo)
					end,
					Users).
	
% execute the actions by type: first all tweets, then get_timeline, then get_tweets
% statistics are printed after each action
execute_actions_by_type(Users, UserType) ->
	{NumberOfUsers, Subscriptions, ActionCount} = UserType,
	ActionList = lists:duplicate(ActionCount, []),

	% statistics header
	erlang:display("Load Generation Setup: NumberOfUsers, Subscriptions, ActionCount"),
	erlang:display("Actions executed by type"),
	erlang:display(NumberOfUsers),
	erlang:display(Subscriptions),
	erlang:display(ActionCount),
	
	% send all tweets
	lists:foreach(fun(_Counter) -> exec_tweet(Users) end, ActionList),
	print_statistics("Statistics for: Sending Tweets (tweet)"),

	% read timeline
	lists:foreach(fun(_Counter) -> exec_get_timeline(Users) end, ActionList),
	print_statistics("Statistics for: Reading own Timeline (get_timeline)"),
	
	% read tweets from other users
	lists:foreach(fun(_Counter) -> exec_get_tweets(Users, NumberOfUsers) end, ActionList),
	print_statistics("Statistics for: Reading Tweets from Other Users (get_tweets)").
	
% execute the actions in an interleaved fashion
% statistics are only printed at the end
execute_actions_interleaved(Users, UserType) ->
	{NumberOfUsers, Subscriptions, ActionCount} = UserType,
	ActionList = lists:duplicate(ActionCount, []),

	% statistics header
	erlang:display("Load Generation Setup: NumberOfUsers, Subscriptions, ActionCount"),
	erlang:display("Actions executed in an interleaved way"),
	erlang:display(NumberOfUsers),
	erlang:display(Subscriptions),
	erlang:display(ActionCount),
	
	% send all tweets
	lists:foreach(fun(_Counter) ->
						exec_tweet(Users),
						exec_get_tweets(Users, NumberOfUsers),
						exec_get_timeline(Users)
						end,
						ActionList),
	print_statistics("Statistics for: interleaved actions").
	
% executes a single tweet action for all users
exec_tweet(Users) ->
	lists:foreach(fun({UserId, UserPid}) -> 
					 api:tweet(UserPid, UserId, get_random_tweet_text())
%					 api:tweet(UserPid, UserId, "hello world")
					 end,
					 Users).

% executes a single get_tweets action for all users
exec_get_tweets(Users, NumberOfUsers) ->
	{A1,A2,A3} = now(),
    random:seed(A1, A2, A3),
	lists:foreach(fun({_UserId, UserPid}) -> 
					 api:get_tweets(UserPid, random:uniform(NumberOfUsers-1), 0)
					 end,
					 Users).
					 
% executes a single get_timeline action for all users
exec_get_timeline(Users) ->
	lists:foreach(fun({UserId, UserPid}) -> 
					 api:get_timeline(UserPid, UserId, 0)
					 end,
					 Users).
					 
% prints statistics in a very rudimentary way
print_statistics(Title) ->
	erlang:display(Title),
	{_, RunTime} = statistics(runtime),
	{_, ClockTime} = statistics(wall_clock),
	Time1 = io_lib:format("~.3f",[RunTime / 1000.0]),
	Time2 = io_lib:format("~.3f",[ClockTime / 1000.0]),
	erlang:display("Runtime / Clock"),
	erlang:display(Time1 ++ Time2).
	
% creates and returns a tweet by randomly selecting some words from predefined lists
get_random_tweet_text() ->
	{A1,A2,A3} = now(),
    random:seed(A1, A2, A3),
	ExclamationWords = ["Omgg!", "Check this out:", "Really?", "wth?!?", "Heard before?"],
	Exclamation = get_random_word(ExclamationWords),
	SubjectWords = ["John", "Alexi", "Jacob", "Chrisa", "Matt"],
	Subject = get_random_word(SubjectWords),
	Verbs = ["stole", "has lost", "forgot", "won", "has bought"],
	Verb = get_random_word(Verbs),
	ObjectWords = ["a Bugatti", "a new watch", "tickets for Taylor Swift's concert", "a house", "2 dogs"],
	Object = get_random_word(ObjectWords),
	WhereWords = ["in the city corner", "at UF", "while playing in the garden", "at office", "on the train"],
	Where = get_random_word(WhereWords),
	WhenWords = ["after office.", "this evening.", "a month ago.", "keeping a secret.", "during dinner."],
	When = get_random_word(WhenWords),
	string:join([Exclamation, Subject, Verb, Object, Where, When], " ").

% Randomly selects one word out of a list of 5 words
get_random_word(List) ->
	lists:nth(random:uniform(length(List)), List).