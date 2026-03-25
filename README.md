# haskell-hole

Haskell implementation of [hole](https://github.com/Lupino/hole).

`haskell-hole` is a reverse tunneling tool, similar to SSH reverse port forwarding.
It is useful when direct inbound access is not available.

Instead of opening ports on a home router and configuring dynamic DNS, you can run:
- `holed` on a globally reachable server
- `hole` on the target private host

Traffic is then forwarded through the `holed` server.

This fits network topologies where:
- A (private) can connect to B (public)
- C (private) can connect to B (public)
- B cannot connect directly to A or C
- A and C cannot connect directly to each other

Supported protocols: `tcp`, `tcp6`, `udp`, `udp6`, and `unix socket`.

# Compile from source

## Compile with nix

    nix-build default.nix


## Compile with stack

    stack build

## Test with stack (same as Stack CI)

    stack --no-terminal test

## CI integration tests

Stack CI also validates both tunnel modes end-to-end with localhost services.

### Local to Remote (CI)

- `holed`:

```
stack exec -- holed -H tcp://127.0.0.1:19100 --addr tcp://127.0.0.1:19101
```

- `hole`:

```
stack exec -- hole -H tcp://127.0.0.1:19100 --addr tcp://127.0.0.1:18101
```

- Verify:

```
curl -fsS http://127.0.0.1:19101/
```

### Remote to Local (CI)

- `holed`:

```
stack exec -- holed -H tcp://127.0.0.1:19200 --addr tcp://127.0.0.1:18102 --use-remote-to-local
```

- `hole`:

```
stack exec -- hole -H tcp://127.0.0.1:19200 --addr tcp://127.0.0.1:19201 --use-remote-to-local
```

- Verify:

```
curl -fsS http://127.0.0.1:19201/
```

# Usage

## Local to Remote

- On server

```
holed -H tcp://server:port --addr tcp://server:portout
```

- On local machine

```
hole -H tcp://server:port --addr tcp://localhost:portin
```

Then you can access the local server via `tcp://server:portout`.

## Remote to Local

- On server

```
holed -H tcp://server:port --addr tcp://localhost:portin --use-remote-to-local
```

- On local machine

```
hole -H tcp://server:port --addr tcp://localhost:portout --use-remote-to-local
```

Then you can access the remote server via `tcp://localhost:portout`.

# Arguments

## hole

```
$ hole -h
hole - Hole Client

Usage: hole [-H|--hole-addr ADDRESS] [-a|--addr ADDRESS] [--use-remote-to-local]
            [-l|--log-level LEVEL] [-n|--name NAME] [-m|--method METHOD]
            [-c|--cipher CIPHER] [-k|--key KEY]

  Hole Client

Available options:
  -h,--help                Show this help text
  -H,--hole-addr ADDRESS   Hole address
  -a,--addr ADDRESS        Address
  --use-remote-to-local    Use remote to local mode, default is local to remote
  -l,--log-level LEVEL     Log level. support DEBUG INFO NOTICE WARNING ERROR
                           CRITICAL ALERT EMERGENCY
  -n,--name NAME           Client name
  -m,--method METHOD       Crypto method. support cbc cfb ecb ctr. default cfb
  -c,--cipher CIPHER       Crypto cipher. support aes128 aes192 aes256 blowfish
                           blowfish64 blowfish128 blowfish256 blowfish448 cast5
                           camellia128 des des_eee3 des_ede3 des_eee2 des_ede2
                           twofish128 twofish192 twofish256 none. default none
  -k,--key KEY             Crypto key.
```

## holed

```
$ holed -h
holed - Hole Server

Usage: holed [-H|--hole-addr ADDRESS] [-a|--addr ADDRESS]
             [--use-remote-to-local] [-l|--log-level LEVEL] [-m|--method METHOD]
             [-c|--cipher CIPHER] [-k|--key KEY]

  Hole Server

Available options:
  -h,--help                Show this help text
  -H,--hole-addr ADDRESS   Hole address
  -a,--addr ADDRESS        Address
  --use-remote-to-local    Use remote to local mode, default is local to remote
  -l,--log-level LEVEL     Log level. support DEBUG INFO NOTICE WARNING ERROR
                           CRITICAL ALERT EMERGENCY
  -m,--method METHOD       Crypto method. support cbc cfb ecb ctr. default cfb
  -c,--cipher CIPHER       Crypto cipher. support aes128 aes192 aes256 blowfish
                           blowfish64 blowfish128 blowfish256 blowfish448 cast5
                           camellia128 des des_eee3 des_ede3 des_eee2 des_ede2
                           twofish128 twofish192 twofish256 none. default none
  -k,--key KEY             Crypto key.
```


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
