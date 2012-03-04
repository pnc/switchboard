-module(apns_notifications).
-export([ send/2, recv/1, connect/0 ]).
-import(hex).
-import(mochihex).

connect() ->
    {ok, Address} = application:get_env(apns_host), %"gateway.sandbox.push.apple.com",
    Port = 2195,
    {ok, Cert} = application:get_env(apns_certfile), %"keys/apns-dev-cert.pem",
    {ok, Key} = application:get_env(apns_keyfile), %"keys/apns-dev-key-noenc.pem",  

    %Options = [{cacertfile, CaCert}, {certfile, Cert}, {keyfile, Key}, {mode, binary}],
    Options = [{certfile, Cert}, {keyfile, Key}, {mode, binary}],
    Timeout = 10000,
    % What if we can't connect? {ok, Socket}
    {Status, Socket} = ssl:connect(Address, Port, Options, Timeout),
    
    % We should take a PID for error feedback.
    Pid = self(),
    ssl:controlling_process(Socket, spawn(fun() -> ?MODULE:recv(Pid) end)),
    
    {Status, Socket}.

prepare_packet(Notification) ->
    Token = proplists:get_value(token, Notification),
    AlertText = proplists:get_value(alert, Notification),
    UserInfoValue = proplists:get_value(user_info, Notification),
    UserInfo = case UserInfoValue of
        undefined ->
            {};
        _Else ->
            UserInfoValue
    end,
    Payload = mochijson:encode({struct, [{"aps", {struct, [{"alert", AlertText}, {"sound", "default"}]}}, UserInfo]}),
    io:format("Message: ~s", [Payload]),
    BPayload = erlang:list_to_binary(Payload),
    PayloadLen = erlang:byte_size(BPayload),
    %Token = "5f2c2e77356ff2116e32ef5bb7c9fd7a50f27f19988b180401af7d72181ba9a8",
    %Token = proplists:get_value(token, Notification),
    BToken = mochihex:to_bin(Token),
    BTokenLength = erlang:byte_size(BToken),
    
    SomeID= 1,
    {MSeconds,Seconds,_} = erlang:now(),
    Expiry = MSeconds * 1000000 + Seconds + 3600*1,
    
    <<1:8, SomeID:32/big, Expiry:32/big, BTokenLength:16/big, BToken/binary, PayloadLen:16/big, BPayload/binary>>.

send(Socket, Notification) ->
    Packet = prepare_packet(Notification),
    ssl:send(Socket, Packet).
    %ssl:close(Socket).
  
  %5f2c2e77 356ff211 6e32ef5b b7c9fd7a 50f27f19 988b1804 01af7d72 181ba9a8
  
recv(Parent) ->
   receive
       {ssl, Sock, <<Command, Status, SomeID:32/big>>} ->
           error_logger:error_msg("Received",
                                  [Command, Status, SomeID]),
           ssl:close(Sock);
           %Parent ! {error, SomeID}; % notify parent
      {ssl_closed, _Sock} -> ok  %
   end.