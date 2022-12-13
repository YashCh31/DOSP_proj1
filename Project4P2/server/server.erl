-module(server). 
-export([start/0]). 

start() ->
   inets:start(), 
   Pid = inets:start(httpd, [{port, 8001}, {server_name,"twitter_ws"}, 
   {server_root,"."},{document_root,"."},
   {bind_address, "localhost"}]), io:fwrite("~p",[Pid]),
   middleware:start().