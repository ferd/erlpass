%% @author Fred Hebert <mononcqc@gmail.com> [http://ferd.ca/]
%% @doc Erlpass is a simple wrapper library trying to abstract away common
%% password operations using safe algorithms, in this case, bcrypt.
-module(erlpass).
-export([hash/1, hash/2, match/2, change/3, change/4]).
-define(DEFAULT_WORK_FACTOR, 12).

%% @type password() = binary() | list() | iolist(). A password, supports valid unicode.
-type password() :: binary() | list() | iolist().
%% @type work_factor() = 4..31. Work factor of the bcrypt algorithm
-type work_factor() :: 4..31.
%% @type hash() = binary(). The hashed password with a given work factor.
-type hash() :: binary().

-export_type([password/0, work_factor/0, hash/0]).

%% @doc Similar to {@link hash/2. <code>hash(Password, 12)</code>}.
-spec hash(password()) -> hash().
hash(S) when is_binary(S); is_list(S) -> hash(S, ?DEFAULT_WORK_FACTOR).


%% @doc Hashes a given {@link password(). <code>password</code>} with a given
%% {@link work_factor(). work factor}. Bcrypt will be used to create
%% a {@link hash(). hash} of the password to be stored by the application.
%% Compare the password to the hash by using {@link match/2. <code>match/2</code>}.
%% Bcrypt takes care of salting the hashes for you so this does not need to be
%% done. The higher the work factor, the longer the password will take to be
%% hashed and checked.
-spec hash(password(), work_factor()) -> hash().
hash(Str, Factor) ->
    {ok, Hash} = bcrypt:hashpw(format_pass(Str), element(2, bcrypt:gen_salt(Factor))),
    list_to_binary(Hash).

%% @doc Compares a given password to a hash. Returns <code>true</code> if
%% the password matches, and <code>false</code> otherwise.
-spec match(password(), hash()) -> boolean().
match(Pass, Hash) ->
    LHash = binary_to_list(Hash),
    case bcrypt:hashpw(format_pass(Pass), LHash) of
        {ok, LHash} -> true;
        {ok, _} -> false
    end.

%% @doc If a given {@link password(). password} matches a given
%% {@link hash(). hash}, the password is re-hashed again using
%% the new {@link work_factor(). work factor}. This allows to update a
%% given work factor to something stronger.
%% Equivalent to {@link change/4. <code>change(Pass, Hash, Pass, Factor)</code>}.
-spec change(password(), hash(), work_factor()) -> hash() | {error, bad_password}.
change(Pass, Hash, Factor) ->
    change(Pass, Hash, Pass, Factor).

%% @doc If a given old {@link password(). password} matches a given old
%% {@link hash(). hash}, a new {@link password(). password} is hashed using the
%% {@link work_factor(). work factor} passed in as an argument.
%% Allows to safely change a password, only if the previous one was given
%% with it.
-spec change(password(), hash(), password(), work_factor()) -> hash() | {error, bad_password}.
change(OldPass, Hash, NewPass, Factor) ->
    case match(OldPass, Hash) of
        true -> hash(NewPass, Factor);
        false -> {error, bad_password}
    end.

%%% PRIVATE
%% This 'list_to_binary' stuff is risky -- no idea what the implementation
%% is like.
%% We have to support unicode
%% @doc transforms a given {@link password(). password} in a safe binary format
%% that can be understood by the bcrypt library.
-spec format_pass(iolist()) -> binary().
format_pass(Str) when is_list(Str) ->
    case unicode:characters_to_binary(Str) of
        {error, _Good, _Bad} -> list_to_binary(Str);
        {incomplete, _Good, _Bad} -> list_to_binary(Str);
        Bin -> Bin
    end;
format_pass(Bin) when is_binary(Bin) -> Bin.
