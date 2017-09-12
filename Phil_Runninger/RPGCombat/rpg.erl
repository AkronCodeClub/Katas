%%% vim:foldmethod=marker
-module(rpg).  % {{{1

-behaviour(gen_server).

%% API
-export([ start/1, start/2, stop/1, attack/2, heal/2 ]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-record(state, {name, health = 1000, level = 1}).

-include_lib("eunit/include/eunit.hrl").

%%% API   {{{1

start(Name) ->
    start(Name, 1000).

start(Name, InitialHealth) ->
    gen_server:start({local, Name}, ?MODULE, [Name, InitialHealth], []).

stop(Name) ->
    gen_server:stop(Name).

attack(Self, Self) ->
    not_allowed;
attack(Attacker, Defender) ->
    gen_server:call(Attacker, {attack, Defender}).

heal(Self, Self) ->
    gen_server:call(Self, {heal, 100});
heal(_, _) ->
    not_allowed.

%%% gen_server callbacks   {{{1

init([Name, InitialHealth]) ->
    random:seed(os:timestamp()),
    {ok, #state{name = Name, health = InitialHealth}}.

handle_call({attack, _}, _, #state{health = 0}=State) ->
    io:format("Dead men can't attack.~n"),
    {reply, not_allowed, State};

handle_call({attack, Defender}, _, #state{name = Name}=State) ->
    DamageDealt = random:uniform(100),
    io:format("~p is attacking: ~p~n", [Name, DamageDealt]),
    DamageReceived = gen_server:call(Defender, {defend, DamageDealt}),
    NewState = take_damage(DamageReceived, State),
    {reply, NewState, NewState};

handle_call({defend, DamageDealt}, _, #state{name = Name, health = Health}=State) ->
    Defense = lists:min([Health, random:uniform(100)]),
    io:format("~p is defending: ~p~n", [Name, Defense]),
    {reply, Defense - DamageDealt, take_damage(DamageDealt - Defense, State)};

handle_call({heal, AddedHealth}, _, State) ->
    NewState = be_healed(AddedHealth, State),
    {reply, NewState, NewState};

handle_call(_, _, State) ->
    {reply, ok, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%% Internal functions   {{{1

take_damage(Damage, State) when Damage < 0 ->
    State;
take_damage(Damage, #state{name = Name, health = Health}=State) when Health > Damage ->
    io:format("~p lost ~p health~n", [Name, Damage]),
    State#state{health = Health - Damage};
take_damage(_, #state{name = Name}=State) ->
    io:format("~p died.~n", [Name]),
    State#state{health = 0}.

be_healed(AddedHealth, State) when AddedHealth < 0 ->
    State;
be_healed(_, #state{health = 0} = State) ->
    State#state{health = 0};
be_healed(AddedHealth, #state{name = Name, health = Health} = State) when Health + AddedHealth =< 1000 ->
    io:format("~p gained ~p health~n", [Name, AddedHealth]),
    State#state{health = Health + AddedHealth};
be_healed(_, State) ->
    State#state{health = 1000}.

alive(#state{health = Health}) ->
    Health > 0.

%%% Unit Tests   To test, :w|! (erlc % && erl -s %:r test -s init stop -noinput)   {{{1

take_damage_test_() ->
    [
     ?_assertEqual(900, (take_damage(100, #state{}))#state.health),
     ?_assertEqual(50, (take_damage(-100, #state{health = 50}))#state.health),
     ?_assertEqual(0, (take_damage(100, #state{health = 50}))#state.health),
     ?_assertEqual(0, (take_damage(100, #state{health = 0}))#state.health)
    ].

be_healed_test_() ->
    [
     ?_assertEqual(1000, (be_healed(100, #state{}))#state.health),
     ?_assertEqual(50, (be_healed(-100, #state{health = 50}))#state.health),
     ?_assertEqual(150, (be_healed(100, #state{health = 50}))#state.health),
     ?_assertEqual(0, (be_healed(100, #state{health = 0}))#state.health)
    ].

alive_test_() ->
    [
     ?_assertEqual(true, alive(#state{})),
     ?_assertEqual(false, alive(#state{health = 0}))
    ].
