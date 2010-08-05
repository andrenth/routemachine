-module(rtm_server).
-behavior(gen_server).

-export([start_link/0]).

% Exports for gen_server.
%-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
%         terminate/2, code_change/3]).
-export([init/1, handle_info/2, terminate/2]).

-include_lib("bgp.hrl").

-record(state, {
  data,
  data_processor,
  msg_type,
  msg_len
}).

start_link() ->
  gen_server:start_link(?MODULE, ok, []).


%
% Callbacks for gen_server.
%

init(ok) ->
  io:format("Starting server ~w~n", [self()]),
  process_flag(trap_exit, true),
  State = #state{data           = <<>>,
                 data_processor = fun(S) -> process_header(S) end},
  {ok, State}.

handle_info({tcp, Socket, Bin},
            #state{data = Data, data_processor = Assemble} = State) ->
  io:format("Got data on socket~n", []),
  inet:setopts(Socket, [{active, once}]),
  NewState = Assemble(State#state{data = list_to_binary([Data, Bin])}),
  {noreply, NewState};

handle_info({tcp_closed, _Socket}, State) ->
  {stop, normal, State}.

terminate(_Reason, _State) ->
  ok.

%
% Internal functions.
%

process_header(#state{data = Data} = State)
               when bit_size(Data) >= ?BGP_HEADER_LENGTH * 8 ->
  {Hdr, Rest} = split_binary(Data, ?BGP_HEADER_LENGTH),
  NewState = State#state{data = Rest,
                         data_processor = fun(S) -> process_message(S) end},
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


process_message(#state{data = Data, msg_len = Length} = State)
                when size(Data) >= Length ->
  {Msg, Rest} = split_binary(Data, Length),
  NewState = State#state{data = Rest,
                         data_processor = fun(S) -> process_header(S) end},
  io:format("Message received: ~w~n", [Msg]),
  process_header(NewState);
  %case rtm_parser:parse_message(Msg, Type) of
  %  % TODO
  %end;

process_message(State) ->
  io:format("Incomplete message~n", []),
  State.
