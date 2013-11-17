# Rock-scissors-paper game server #

This is written for educational purposes as a demo of an interactive web
application.

## How to build, start and stop ##

```
$ make deps
$ make
$ make start
```

After starting the server, you can check.

```
$ make state
```

You can stop the server any time.

```
$ make stop
```

## Concept of the game ##

You (an event manager) should launch an event first.

```
$ make debug

Eshell V5.10.2  (abort with ^G)
(rsp@yourhost)1> 
```

Now in the erlang shell, you should start a new event.

```
(rsp@yourhost)1> rsp_event:start([{id,<<"e1">>}]).
{ok,<0.262.0>}
(rsp@yourhost)2> 
```

After creating the event, press CTRL+C twice to escape from the erlang shell.

Now a player, knowing the launch of the event somehow, tries to join by
posting.

```
$ curl "http://localhost:8000/event/e1" -d "player=p1"

```

He might end up getting ``timeout`` if there is no other player.  ``p1`` can
just retry joining the event by repeating the post again.  Then another
player (from another terminal session) comes while ``p1`` is waiting.

```
$ curl "http://localhost:8000/event/e1" -d "player=p2"
df4cq43xyzh7hp3ehix45vp4ry
```

Now both the players got the same match ``df4cq43xyzh7hp3ehix45vp4ry``.

They play a game in the match by posting like.

```
$ curl "http://localhost:8000/match/df4cq43xyzh7hp3ehix45vp4ry" -d "player=p1&move=rock"

```

(from another terminal session)
```
$ curl "http://localhost:8000/match/df4cq43xyzh7hp3ehix45vp4ry" -d "player=p2&move=paper"
win
```

In this case, ``p1`` must have got ``loss`` as ``p2`` got ``win``.  The rest
is just standard rock-scissors-paper fashion.

## A little bit internal ##

* You can watch what is happening inside the server by
  ``tail -f log/console.log``.
* You will see the web logics in *src/rsp_xxx_handler.erl* files.
* Event and match are implemented in *gen_server* callbacks.
  Consult *src/rsp_event.erl* and *src/rsp_match.erl* respectively.
* You will see how the server initializes from *src/rsp.erl*,
  *src/rsp_app.erl* and *src/rsp_sup.erl*.
* You will see how the shell command works from *Makefile* and
  *src/rsp_control.erl*.
* You will see some of the configurations from *rsp.config*.

You may consider this code structure as quite standard in erlang system.