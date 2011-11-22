-module(push_broker_web).

%% your web app can push data to clients using a technique called comet long
%% polling.  browsers make a request and your server waits to send a
%% response until data is available.  see wikipedia for a better explanation:
%% http://en.wikipedia.org/wiki/Comet_(programming)#Ajax_with_long_polling
%%
%% since the majority of your http handlers will be idle at any given moment,
%% you might consider making them hibernate while they wait for more data from
%% another process.  however, since the execution stack is discarded when a
%% process hibernates, the handler would usually terminate after your response
%% code runs.  this means http keep alives wouldn't work; the handler process
%% would terminate after each response and close its socket rather than
%% returning to the big @mochiweb_http@ loop and processing another request.
%%
%% however, if mochiweb exposes a continuation that encapsulates the return to
%% the top of the big loop in @mochiweb_http@, we can call that after the
%% response.  if you do that then control flow returns to the proper place,
%% and keep alives work like they would if you hadn't hibernated.

-export([start/1, stop/0, loop/1]).

%% internal export (so hibernate can reach it)
-export([ resume/4
        ]).

-define(LOOP, {?MODULE, loop}).

start(Options) ->
    Batcher = batcher:start(),
    io:format("batch: ~p~n", [Batcher]),
    register(batcher, Batcher),
    mochiweb_http:start([{max, 10}, {name, ?MODULE}, {loop, ?LOOP} | Options]).

stop() ->
    mochiweb_http:stop(?MODULE).

loop(Req) ->
    Path = Req:get(path),
    Qs = Req:parse_qs(),
    try
        case string:tokens(Path, "/") of
            ["parse" | RestOfPath] ->
                io:format("Got some stuff: ~p~n", [Qs]),
                io:format("Specifically: ~p~n", [proplists:get_value("foo", Qs)]);
            ["longpoll" | RestOfPath] ->
                Token = proplists:get_value("token", Qs),
                %% the "reentry" is a continuation -- what @mochiweb_http@
                %% needs to do to start its loop back at the top
                Reentry = mochiweb_http:reentry(?LOOP),
            
                %% here we could send a message to some other process and hope
                %% to get an interesting message back after a while.  for
                %% simplicity let's just send ourselves a message after a few
                %% seconds
                batcher ! {send_notification, self(), [{token, Token}]},
                %%batcher ! flush,
            
                io:format("queued~n", []),
            
                Response = Req:respond({200, [{"Content-Type", "text/plain"}], chunked}),
            
                Text = io_lib:format("finished~n~n", []),
                Response:write_chunk(Text),
            
                %% since we expect to wait for a long time before getting a
                %% reply, let's hibernate.  memory usage will be minimized, so
                %% we won't be wasting memory just sitting in a @receive@
                proc_lib:hibernate(?MODULE, resume, [Req, Response, RestOfPath, Reentry]),

                %% we'll never reach this point, and this function @loop/1@
                %% won't ever return control to @mochiweb_http@.  luckily
                %% @resume/3@ will take care of that.
                io:format("not gonna happen~n", []);

            _ ->
                ok(Req, io_lib:format("some other page: ~p", [Path]))
        end
        
    catch
        Type:What ->
            Report = ["web request failed",
                      {path, Path},
                      {type, Type}, {what, What},
                      {trace, erlang:get_stacktrace()}],
            error_logger:error_report(Report),
            %% NOTE: mustache templates need \ because they are not awesome.
            Req:respond({500, [{"Content-Type", "text/plain"}],
                         "request failed, sorry\n"})
    end,

    io:format("restarting loop normally in ~p~n", [Path]),
    ok.

%% this is the function that's called when a message arrives.
resume(Req, Response, RestOfPath, Reentry) ->
    receive
        notification_sent ->
            io:format("i guess it sent", []),
            Text = io_lib:format("finished~n~n", []),
            Response:write_chunk(Text),
            Response:write_chunk(<<>>),
            %% if we didn't call @Reentry@ here then the function would finish and the
            %% process would exit.  calling @Reentry@ takes care of returning control
            %% to @mochiweb_http@
            io:format("reentering loop via continuation in ~p~n", [Req:get(path)]),
            Reentry(Req)
    end.

ok(Req, Response) ->
    Req:ok({_ContentType = "text/plain",
            _Headers = [],
            Response}).
