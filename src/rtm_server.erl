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
  {Hdr, Rest} = split_binary(Data, ?BGP_HEADER_LENGTH),
  NewState = State#state{data = Rest,
                         data_proc = fun process_message/1},
  io:format("Header received: ~w~n", [Hdr]),
  Type = 0, Length = 39 - ?BGP_HEADER_LENGTH,
  process_message(NewState#state{msg_type = Type, msg_len = Length});
  %case rtm_parser:parse_header(Header) of
  %  {ok, Length, Type} ->
  %    process_message(NewState#state{msg_type = Type, msg_len = Length});
  %  {error, _Err} -> error % XXX
  %end;

process_header(State) ->
  io:format("Incomplete header~n", []),
  State.


process_message(#state{data = Data, msg_len = Length, fsm = FSM} = State)
                when size(Data) >= Length ->
  {Msg, Rest} = split_binary(Data, Length),
  NewState = State#state{data = Rest,
                         data_proc = fun process_header/1},
  io:format("Message received: ~w~n", [Msg]),
  % TODO call into the FSM.
  io:format("Sending event to FSM ~p~n", [FSM]),
  process_header(NewState);
  %case rtm_parser:parse_message(Msg, Type) of
  %  % TODO
  %end;

process_message(State) ->
  io:format("Incomplete message~n", []),
  State.
