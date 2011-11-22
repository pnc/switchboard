% Module responsible for maintaining a connection to Apple's Push Notification
% Service and forwarding push notifications to it.

% In addition, this module polls for feedback and maintains a device blacklist
% so as to reject future notifications synchronously.

-module(batcher).

-export([ start/0, receiver/1, batch_timer/1 ]).

start() ->
    % Check for errors
    {ok, PushSocket} = apns_notifications:connect(),
    
    io:format("Batcher started", []),
    Receiver_PID = spawn(?MODULE, receiver, [PushSocket]),
    spawn(?MODULE, batch_timer, [Receiver_PID]),
    Receiver_PID.

receiver(PushSocket) ->
    io:format("Using socket: ~p~n", [PushSocket]),
    receive
        flush ->
            RefreshedPushSocket = flush_it_bro(PushSocket, [])
    end,
    receiver(RefreshedPushSocket).
    
flush_it_bro(PushSocket, Batch) ->
    receive
        {send_notification, Sender_PID, Notification} ->
            %Status = apns_notifications:send(PushSocket),
            Status = socket_closed,
            case Status of
                socket_closed ->
                    RefreshedPushSocket = PushSocket * 2;
                _Else ->
                    RefreshedPushSocket = PushSocket
            end,
            
            io:format("Send notification: ~p~n", [Notification]),
            erlang:send_after(1000, Sender_PID, notification_sent),
            flush_it_bro(RefreshedPushSocket, [Notification | Batch])
    after 0 ->
        io:format("Ready to flush batch: ~p~n", [Batch]),
        PushSocket
    end.
        
batch_timer(Pid) ->
    receive
    after 5000 ->
        Pid ! flush
    end,
    batch_timer(Pid).
