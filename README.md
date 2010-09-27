RouteMachine
============

Intro
-----

This is still a very incomplete BGP daemon. It's being tested only against
Quagga so far. If you want to give it a try, do the following:

    $ git clone git://github.com/andrenth/routemachine.git
    $ cd routemachine
    $ vim routemachine.conf
    $ rebar compile
    $ gcc -Wall -o priv/rtm c_src/rtm.c
    $ sudo chown root priv/rtm
    $ sudo chmod u+s priv/rtm
    $ cd ..
    $ sudo iptables -t nat -A PREROUTING -i eth0 -p tcp --dport 179 \
                    -j REDIRECT --to-port 1179
    $ erl -pa routemachine/ebin -boot start_sasl \
          -eval 'application:start(routemachine).'

Debug messages should start showing up in the Erlang shell.

When you finish, be sure to run

    application:stop(routemachine).

in the Erlang shell to ensure the routes added by RouteMachine are removed.
