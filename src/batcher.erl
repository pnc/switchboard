-module(batcher).

-export([ start/0, receiver/0, batch_timer/1]).

start() ->
    io:format("Batcher started", []),
    Receiver_PID = spawn(?MODULE, receiver, []),
    spawn(?MODULE, batch_timer, [Receiver_PID]),
    Receiver_PID.

receiver() ->
    receive
        flush ->
            flush_it_bro([])
    end,
    receiver().
    
flush_it_bro(Batch) ->
    receive
        {send_notification, Sender_PID, Notification} ->
            io:format("Send notification: ~p~n", [Notification]),
            erlang:send_after(1000, Sender_PID, notification_sent),
            flush_it_bro([Notification | Batch])
    after 0 ->
        io:format("Ready to flush batch: ~p~n", [Batch])
    end.
    
batch_timer(Pid) ->
    receive
    after 5000 ->
        Pid ! flush
    end,
    batch_timer(Pid).
