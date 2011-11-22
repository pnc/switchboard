-module(apns_notifications).
-export([send_notification/0, recv/1]).
-import(hex).

send_notification() ->
    Address = "gateway.sandbox.push.apple.com",
    Port = 2195,
    Cert = "../keys/apns-dev-cert.pem",
    Key = "../keys/apns-dev-key-noenc.pem",  

    %Options = [{cacertfile, CaCert}, {certfile, Cert}, {keyfile, Key}, {mode, binary}],
    Options = [{certfile, Cert}, {keyfile, Key}, {mode, binary}],
    Timeout = 1000,
    % What if we can't connect?
    {ok, Socket} = ssl:connect(Address, Port, Options, Timeout),
    
    Pid = self(),
    ssl:controlling_process(Socket, spawn(fun() -> ?MODULE:recv(Pid) end)),
    
    Payload = mochijson:encode({struct, [{"aps", {struct, [{"alert", "Call Brandon."}, {"sound", "default"}]}}, {"number", "8643897005"}]}),
    io:format("Message: ~s", [Payload]),
    BPayload = erlang:list_to_binary(Payload),
    PayloadLen = erlang:byte_size(BPayload),
    %last char 8
    Token = "5f2c2e77356ff2116e32ef5bb7c9fd7a50f27f19988b180401af7d72181ba9a9",
    BToken = hex:hexstr_to_bin(Token),
    BTokenLength = erlang:byte_size(BToken),
    
    SomeID= 1,
    {MSeconds,Seconds,_} = erlang:now(),
    Expiry = MSeconds * 1000000 + Seconds + 3600*1,
    
    Packet = <<1:8, SomeID:32/big, Expiry:32/big, BTokenLength:16/big, BToken/binary, PayloadLen:16/big, BPayload/binary>>,
    
    ssl:send(Socket, Packet).
    %ssl:close(Socket).
  
  %5f2c2e77 356ff211 6e32ef5b b7c9fd7a 50f27f19 988b1804 01af7d72 181ba9a8
  
recv(Parent) ->
   receive
       {ssl, Sock, <<Command, Status, SomeID:32/big>>} ->
           error_logger:error_msg("Received",
                                  [Command, Status, SomeID]),
           ssl:close(Sock),
           Parent ! {error, SomeID}; % notify parent
      {ssl_closed, _Sock} -> ok  %
   end.