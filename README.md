# haskell-hole

Haskell version of [hole](https://github.com/Lupino/hole)


When I visit raspberry pi' ssh server on some places,
I must set port forwarding on the home route, and set a dynamic DNS.
If the route is not your's, you will helpless.

I think it may have another way, so I try ssh port forwarding `ssh -CfNgR remote-port:localhost:local-port user@remote`, then visit the remote-port.

The hole is an other way similar ssh port forwarding.
On the global server create a `holed`, and the target host start `hole`.
Last you can visit the `holed` to replace the real server.

The hole suit the situation: A(private) can connect B(global), C(private) can connect B,
but B can't connect C, B can't connect A, and A can't connect C.

The hole support protocol `tcp` `tcp6` `udp` `udp6` and `unix socket`.

# Compile from source

## Compile with nix

    $(nix-build --no-link -A fullBuildScript)


## Compile with stack

    stack build

# Usage

## Local to Remote

- On server.

    holed -H tcp://server:port --addr tcp://server:portout

- On local mathine

    hole -H tcp://server:port --addr tcp://localhost:portin

Then you can access the local server via. tcp://server:portout

## Remote to Local

- On server

    holed -H tcp://server:port --addr tcp://localhost:portin --use-remote-to-local

- On local machine

    hole -H tcp://server:port --addr tcp://localhost:portout --use-remote-to-local

Then you can access the remote server via. tcp://localhost:portout


# Example

## Forward local ssh server to remote

    # Local  IP is 192.168.1.101
    # Server IP is 120.26.120.168

    # Now on server
    holed -H tcp://120.26.120.168:4000 --addr tcp://120.26.120.168:4001

    # On local machine
    hole -H tcp://120.26.120.168:4000 --addr tcp://127.0.0.1:22

    # Now visit Local machine
    ssh root@120.26.120.168 -p 4001

## Forward remote mysql port to local

    # Local  IP is 192.168.1.101
    # Server IP is 120.26.120.168

    # Now on server
    holed -H tcp://120.26.120.168:4000 --addr tcp://127.0.0.1:3306 --use-remote-to-local

    # On local machine
    hole -H tcp://120.26.120.168:4000 --addr tcp://127.0.0.1:3306 --use-remote-to-local

    # Now visit remote mysql server
    mysql -h 127.0.0.1 -P 3306
