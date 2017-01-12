# Erlpass #

A library to handle password hashing and changing in a safe manner, independent from any kind of storage whatsoever. The library is a thin wrapper around the [erlang-bcrypt library from smarkets](https://github.com/smarkets/erlang-bcrypt), handling special cases such as unicode passwords, and forcing hashes in binary. Moreover, the library takes care of providing common operations such as matching passwords, changing the work factor of a hash, or changing a password as a whole.

## Current Status ##
[![Build Status](https://travis-ci.org/ferd/erlpass.png)](https://travis-ci.org/ferd/erlpass)

## Build Instructions ##

Call `rebar3 compile`.

## How do I use this ##

This library application depends on bcrypt (which in turn depends on crypto). You thus need to call `application:start(crypto)` and `application:start(bcrypt)` before being able to call the `erlpass` functions. The module has these two applications in its dependencies and it should be safe to use in releases. The possible calls are:

    1> 1> application:ensure_all_started(erlpass).
    {ok,[crypto, bcrypt,erlpass]}
    ok
    2> Hash = erlpass:hash("my voice is my password").
    <<"$2a$12$85jwhagKAzosjJeUktveYuh26e6xFySob5oIKkWdc27SNL3A443OG">>
    3> erlpass:match("hello, sir", Hash).
    false
    4> erlpass:match("my voice is my password", Hash).
    true
    5> erlpass:match(<<"my voice is my password">>, Hash).
    true
    6> erlpass:match([<<"my voice is my ">>, "password"], Hash).
    true
    7> erlpass:change("my voice", Hash, "new pass", 12).
    {error,bad_password}
    8> erlpass:change("my voice is my password", Hash, "new pass", 12).
    <<"$2a$12$5ps2emX.5CgNs3o1RS1mzu8gkF0G9X0j/tKneKPqJOid3YdA7HmaO">>
    9> erlpass:change("my voice is my password", Hash, "new pass", 12).
    <<"$2a$12$4b2p/Hc.PwrTYffQKRkLheLyu2bbNQbVsvN5Hd.00ei67lagutUyq">>

The `hash(Pass)` function takes an optional workload factor argument that specifies how long it should take to run. The longer the work factor, the harder the brute force attack. The default work factor is 12.

There is also a `change(Pass, Hash, Factor)` function allowing to re-hash a password using a different work factor. This makes sense if a product stays in production for a long time or breakthrough in computing make the current work factor too short. The password can then be re-hashed based on that work factor to make it stronger.

## Why should I use this? ##

Avoid using MD5 or SHA-x hashing functions. MD5 is collision-prone, some of the SHA functions too. MD5 and SHA hashing functions were made to be really fast and we want to avoid that. The reason is that it makes it easier to brute-force passwords if the table is compromised. Protect your users first. Bcrypt and Scrypt, by comparison, will salt the passwords for you and give each of them a work factor. If you take 100 millisecond to check a password (something that happens once per session, so it's fine to be slow) compared to 10 microseconds, it becomes a real pain for crackers to do their thing. During that time, you can warn your users to change their passwords in other services.

This library uses the erlang-bcrypt port from the Smarkets team to work in a safe manner. The library isn't attached to any kind of storage and only gives a wrapper to common password operations that you can store in whatever database you want or need.

## Other Dependencies ##
You will need to have PropEr to run the tests. It's a fantastic testing library.

You can run the tests with `rebar3 eunit`.

## Authors ##

- [Fred Hebert](http://ferd.ca)
