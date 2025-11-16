-module(p2prc).
-export([list_server/0,map_port/3]).


list_server () ->
    os:cmd("p2prc --ls").

map_port (local_port,domain_name,remote_address) ->
  Cmd = io_lib:format("p2prc --mp ~s --~s ~s",
                      [local_port, domain_name, remote_address]),
  os:cmd(lists:flatten(Cmd)).

