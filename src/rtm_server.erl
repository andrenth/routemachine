-module(rtm_server).
-behavior(gen_server).

-export([start_link/1]).

% Exports for gen_server.
-export([init/1, handle_info/2, handle_call/3, handle_cast/2, terminate/2,
         code_change/3]).

-include_lib("bgp.hrl").

-record(state, {
  socket,
  data,
  data_proc,
  msg_type,
  msg_len,
  fsm
}).

start_link(FSM) ->
  gen_server:start_link(?MODULE, FSM, []).


%
% Callbacks for gen_server.
%

init(FSM) ->
  io:format("Starting server ~w~n", [self()]),
  process_flag(trap_exit, true),
  State = #state{data = <<>>, data_proc = fun process_header/1, fsm = FSM},
  {ok, State}.

handle_info({tcp, Socket, Bin},
            #state{data = Data, data_proc = Assemble} = State) ->
  io:format("Got data on socket~n", []),
  inet:setopts(Socket, [{active, once}]),
  NewState = State#state{socket = Socket, data = list_to_binary([Data, Bin])},
  {noreply, Assemble(NewState)};

handle_info({tcp_closed, _Socket}, #state{fsm = FSM} = State) ->
  gen_fsm:send_event(FSM, tcp_closed),
  {stop, normal, State#state{socket = undefined}};

handle_info({tcp_error, _Socket}, #state{fsm = FSM} = State) ->
  gen_fsm:send_event(FSM, tcp_fatal),
  {stop, normal, State#state{socket = undefined}}.

handle_call(peername, _From, #state{socket = Socket} = State) ->
  Reply = case inet:peername(Socket) of
    {ok, {Addr, _Port}} -> {ok, Addr};
    {error, Error} -> {error, Error}
  end,
  {reply, Reply, State}.

handle_cast({send_msg, Bin}, #state{socket = Socket} = State) ->
  gen_tcp:send(Socket, Bin),
  {noreply, State};

handle_cast(close_connection, #state{socket = Socket} = State) ->
  io:format("Closing connection with peer, stopping server ~w~n", [self()]),
  gen_tcp:close(Socket),
  {stop, normal, State#state{socket = undefined}}.

terminate(_Reason, _State) ->
  ok.

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

%
% Internal functions.
%

process_header(#state{data = Data} = State)
               when bit_size(Data) >= ?BGP_HEADER_LENGTH * 8 ->
  {Bin, Rest} = split_binary(Data, ?BGP_HEADER_LENGTH),
  NewState = State#state{data = Rest,
                         data_proc = fun process_message/1},
  io:format("Header received: ~w~n", [Bin]),
  case rtm_parser:parse_header(Bin) of
    {ok, Hdr} ->
      #bgp_header{msg_type = Type, msg_len = Len} = Hdr,
      MsgLen = Len - ?BGP_HEADER_LENGTH,
      process_message(NewState#state{msg_type = Type, msg_len = MsgLen});
    {error, Error} ->
      send_notification(Error),
      NewState
  end;

process_header(State) ->
  io:format("Incomplete header~n", []),
  State.


process_message(#state{data     = Data,
                       msg_type = Type,
                       msg_len  = Length,
                       fsm      = FSM} = State) when size(Data) >= Length ->
  {Bin, Rest} = split_binary(Data, Length),
  NewState = State#state{data = Rest,
                         data_proc = fun process_header/1},
  io:format("Message received: ~w~n", [Bin]),
  io:format("Sending event to FSM ~p~n", [FSM]),
  Event = receipt_event(Type, Bin, Length),
  gen_fsm:send_event(FSM, Event),
  process_header(NewState);

process_message(State) ->
  io:format("Incomplete message~n", []),
  State.

receipt_event(Type, Bin, Len) ->
  case Type of
    ?BGP_TYPE_OPEN         -> {open_received, Bin};
    ?BGP_TYPE_UPDATE       -> {update_received, Bin, Len};
    ?BGP_TYPE_NOTIFICATION -> {notification_received, Bin};
    ?BGP_TYPE_KEEPALIVE    -> keepalive_received
  end.

send_notification(Error) ->
  gen_server:cast(self(), {send_msg, rtm_msg:build_notification(Error)}).
