% It will create an actor per client connection, and one actor to represent
% all internal state (all users, their subscriptions, and their tweets).
%
-module(actor).

-include_lib("eunit/include/eunit.hrl").

%
% Macro setting
%
-define(NUM_OF_DATA_ACTORS, 10).

-export([initialize/0,
         register_user/0,
         subscribe/3,

         % internal actors
         data_actor/1,
         entry_actor/0]).


% Start api.
initialize() ->
    % register a list of atoms named from data_actor_1 to data_actor_X,
    % where X is defined by DataActorCount
    [ register(list_to_atom("data_actor_" ++ integer_to_list(X)), 
              spawn_link(?MODULE, data_actor, [[]]))
      || X <- lists:seq(1, ?NUM_OF_DATA_ACTORS) ],
    ok.

% Register a new user and return its id and the entry actor that is
% responsible for this user.
-spec register_user() -> {integer(), pid()}.
register_user() ->
    {_, DataActor_list} = get_data_actors(),
    ChosenDataActor = random_choose_in_list(DataActor_list),
    whereis(ChosenDataActor) ! {self(), register_user},

    receive
        {registered_user, UserId, EntryPid} -> {UserId, EntryPid}
    end.

% Subscribe/follow another user.
-spec subscribe(pid(), integer(), integer()) -> ok.
subscribe(EntryPid, UserId, UserIdToSubscribeTo) ->
    EntryPid ! {self(), subscribe, UserId, UserIdToSubscribeTo},
    receive
        {EntryPid, subscribed, UserId, UserIdToSubscribeTo} -> ok
    end.



% The data actor works like a small database and encapsulates all state of this
% simple implementation.
data_actor(Data) ->
    receive        
        {update, _Sender, register_user} ->
            {NewData, _NewUserId} = add_new_user({update, Data}),
            data_actor(NewData);

        {update, _Sender, tweet,        UserId, Tweet} ->
            {NewData, _Timestamp} = tweet(Data, UserId, Tweet),
            data_actor(NewData);

        {update, _Sender, subscribe,    UserId, UserIdToSubscribeTo} ->
            NewData = subscribe_to_user(Data, UserId, UserIdToSubscribeTo),
            data_actor(NewData);

        {Sender, register_user} ->
            {_ThisName, DataActor_list_others} = get_data_actors(),
            % Forward this msg to other data_actors
            [whereis(OtherActor) ! {update, Sender, register_user} 
             || OtherActor<-DataActor_list_others],
            {NewData, NewUserId, NewUserActor} = add_new_user({Data}),
            Sender ! {registered_user, NewUserId, NewUserActor},
            data_actor(NewData);
            
        {Sender, get_timeline, UserId, Page} ->
            % Since this is not updating the data, do not forward msg
            Sender ! {self(), timeline, UserId, Page, timeline(Data, UserId, Page)},
            data_actor(Data);
            
        {Sender, get_tweets,   UserId, Page} ->
            % Since this is not updating the data, do not forward msg
            Sender ! {self(), tweets,   UserId, Page, tweets(Data, UserId, Page)},
            data_actor(Data);
        
        {Sender, tweet,        UserId, Tweet} ->
            {_ThisName, DataActor_list_others} = get_data_actors(),
            % Forward this msg to other data_actors
            [whereis(OtherActor) ! {update, Sender, tweet, UserId, Tweet} 
             || OtherActor<-DataActor_list_others],
            {NewData, Timestamp} = tweet(Data, UserId, Tweet),
            Sender ! {self(), tweet_accepted, UserId, Timestamp},
            data_actor(NewData);

        {Sender, subscribe,    UserId, UserIdToSubscribeTo} ->
            {_ThisName, DataActor_list_others} = get_data_actors(),
            % Forward this msg to other data_actors
            [whereis(OtherActor) ! {update, Sender, subscribe, UserId, UserIdToSubscribeTo} 
             || OtherActor<-DataActor_list_others],
            NewData = subscribe_to_user(Data, UserId, UserIdToSubscribeTo),
            Sender ! {self(), subscribed, UserId, UserIdToSubscribeTo},
            data_actor(NewData)
    end.

% This simple implementation of the entry actor just delegates to the data
% actor.
entry_actor() ->
    % Since the current process won't be one of the data_actor processes,
    % So the returned list is a full list of data_actors.
    {_, DataActor_list} = get_data_actors(),
    % Set DataActor as a pid value of a randomly selected data_actor.
    DataActor = whereis(random_choose_in_list(DataActor_list)),
    receive
        % RequestType ::= tweets | timeline
        {Sender, RequestType, UserId, PageOrTweetOrUserId} -> 
            DataActor ! {self(), RequestType, UserId, PageOrTweetOrUserId},
            
            receive
                {DataActor, ResponseType, UserId, Page, Result} ->
                    Sender ! {self(), ResponseType, UserId, Page, Result};
                {DataActor, tweet_accepted, UserId, Timestamp} ->
                    Sender ! {self(), tweet_accepted, UserId, Timestamp};
                {DataActor, subscribed, UserId, PageOrTweetOrUserId} ->
                    Sender ! {self(), subscribed, UserId, PageOrTweetOrUserId}
            end
    end,
    entry_actor().

%
% Internal Functions
%

% Update new user, do not need to spawn an entry_actor
add_new_user({update, Data}) ->
    NewUserId = length(Data),
    NewData = Data ++ [{user, NewUserId, [], sets:new()}],
    {NewData, NewUserId};
% Create new user, and spawn a new entry_actor
add_new_user({Data}) ->
    NewUserId = length(Data),
    NewData = Data ++ [{user, NewUserId, [], sets:new()}],
    NewUserActor = spawn_link(?MODULE, entry_actor, []),
    {NewData, NewUserId, NewUserActor}.

timeline(Data, UserId, _Page) ->
    {user, UserId, Tweets, Subscriptions} = lists:nth(UserId + 1, Data),
    
    UnsortedTweetsForTimeLine =
        lists:foldl(fun(UserId2, AllTweets) ->
                        {_, _, SomeTweets, _} = lists:nth(UserId2 + 1, Data),
                        AllTweets ++ SomeTweets
                    end,
                    Tweets,
                    sets:to_list(Subscriptions)),
    SortedTweets = lists:reverse(lists:keysort(3, UnsortedTweetsForTimeLine)),
    lists:sublist(SortedTweets, 10).

tweets(Data, UserId, _Page) ->
    {user, UserId, Tweets, _Subscriptions} = lists:nth(UserId + 1, Data),
    Tweets.

tweet(Data, UserId, Tweet) ->
    {user, UserId, Tweets, Subscriptions} = lists:nth(UserId + 1, Data),
    Timestamp = erlang:now(),
    NewUser = {user, UserId, Tweets ++ [{tweet, UserId, Timestamp, Tweet}], Subscriptions},

    {UsersBefore, [_|UsersAfter]} = lists:split(UserId, Data),
    {lists:append([UsersBefore, [NewUser | UsersAfter]]), Timestamp}.

subscribe_to_user(Data, UserId, UserIdToSubscribeTo) ->
    {user, UserId, Tweets, Subscriptions} = lists:nth(UserId + 1, Data),
    NewUser = {user, UserId, Tweets, sets:add_element(UserIdToSubscribeTo, Subscriptions)},

    {UsersBefore, [_|UsersAfter]} = lists:split(UserId, Data),
    lists:append([UsersBefore, [NewUser | UsersAfter]]).

% Get a list of atoms naming with "data_actor_" registered as data_actors
get_data_actors() ->
    DataActor_list = lists:filter(fun(Aatom)->string:str(atom_to_list(Aatom),
                                                         "data_actor_")
                                              >=1 end, 
                                  registered()),
    % Get the atom the current process is registered to, 
    % remove from the above list
    case erlang:process_info(self(), registered_name) of
        {registered_name, ThisName} -> {ThisName, DataActor_list--[ThisName]};
        [] -> {none, DataActor_list}
    end.


% Choose randomly an element in a list.
% This is used in entry_actor to randomly choose a data_actor to communicate.
random_choose_in_list(AList) ->
    Length = length(AList),
    IndexChoice = random:uniform(Length),
    lists:nth(IndexChoice, AList).

% Unregister all data_actor atoms
unregister_data_actors() ->
    case get_data_actors() of
        {none, DataActors} -> [catch unregister(A_actor)
                               || A_actor <- DataActors];
        {ThisName, Other_DataActors} -> [catch unregister(A_actor) 
                                         || A_actor <- Other_DataActors],
                                        catch unregister(ThisName)
    end.
