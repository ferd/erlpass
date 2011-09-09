-module(erlpass_tests).
-include_lib("proper/include/proper.hrl").
-include_lib("eunit/include/eunit.hrl").
-define(PROPMOD, proper).
-define(PROP(A), {timeout, 45, ?_assert(?PROPMOD:quickcheck(A(), [{max_shrinks,15}]))}).

-define(LIGHT_WORK_FACTOR, 4).
-define(MEDIUM_WORK_FACTOR, 9).
-define(NORMAL_WORK_FACTOR, 12).
-define(HEAVY_WORK_FACTOR, 31).

%% EUNIT TESTS
setup() ->
    application:start(crypto),
    application:start(bcrypt).

good_default_work_factor_test_() ->
    {"The default work factor should be high enough to be safe (usually ~12)",
     {setup, fun setup/0,
      [?_assert(element(1, timer:tc(erlpass, hash, ["abc", ?LIGHT_WORK_FACTOR])) <
                element(1, timer:tc(erlpass, hash, ["abc"])))]}}.

proper_test_() ->
    {"Run all property-based tests",
     {setup, fun setup/0,
      [?PROP(prop_support_str_formats), ?PROP(prop_hashed), ?PROP(prop_match),
       ?PROP(prop_change_work_factor), ?PROP(prop_change_password),
       ?PROP(prop_change_error)]}}.

%%% PROPERTY-BASED TESTS
prop_support_str_formats() ->
    ?FORALL(S, password(),
        begin process_flag(trap_exit, true),
        try erlpass:hash(S, ?LIGHT_WORK_FACTOR) of
            _ -> true
        catch
            error:function_clause -> false;
            A:B -> io:format(user, "failure: ~p~n",[{S,A,B}]), false
        end end).

prop_hashed() ->
    ?FORALL(S, password(),
        %% check for bcrypt encoding. Contains versions and whatnot. Defaults to list
        begin process_flag(trap_exit, true),
        case catch erlpass:hash(S, ?LIGHT_WORK_FACTOR) of
            <<"$2$", Rest/binary>> when Rest =/= <<>> -> true;
            <<"$2a$", Rest/binary>> when Rest =/= <<>> -> true;
            Hash -> io:format("~p~n",[{err, S, Hash}]), false
        end end).

prop_match() ->
    ?FORALL(S, password(),
     begin
        Hash = erlpass:hash(S, ?LIGHT_WORK_FACTOR),
        erlpass:match(S, Hash) andalso
        not erlpass:match(S, <<Hash/binary, $a>>)
    end).

prop_change_work_factor() ->
    ?FORALL(S, password(),
     begin
        Hash1 = erlpass:hash(S, ?LIGHT_WORK_FACTOR),
        Hash2 = erlpass:change(S, Hash1, ?MEDIUM_WORK_FACTOR),
        erlpass:match(S, Hash1) andalso erlpass:match(S, Hash2)
     end).

prop_change_password() ->
    ?FORALL(S1, password(),
     begin
        S2 = ["new"|S1],
        Hash1 = erlpass:hash(S1, ?LIGHT_WORK_FACTOR),
        Hash2 = erlpass:change(S1, Hash1, S2, ?LIGHT_WORK_FACTOR),
        erlpass:match(S1, Hash1) andalso erlpass:match(S2, Hash2) andalso
        not erlpass:match(S2, Hash1) andalso not erlpass:match(S1, Hash2)
     end).

prop_change_error() ->
    ?FORALL(S, password(),
      begin
        FailPass = ["fail"|S],
        Hash = erlpass:hash(S, ?LIGHT_WORK_FACTOR),
        erlpass:match(S, Hash) andalso
        not erlpass:match(FailPass, Hash) andalso
        {error, bad_password} =:= erlpass:change(FailPass, Hash, ?LIGHT_WORK_FACTOR)
      end).

%%% GENERATORS
password() ->
    ?LAZY(oneof([unicode_string(), binary(), [password1()]])).

password1() ->
    ?LAZY(oneof([binary(),
                 byte(),
                 password1()])).

unicode_string() ->
    ?SUCHTHAT(X, list(char()),
        lists:all(fun(Y) -> Y < 16#D800 orelse Y > 16#DFFF end, X)).
