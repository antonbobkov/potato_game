-module(my_serializer).

-export([serialize_object/1]).

serialize_object(Obj) -> list_to_binary(io_lib:format("~w", [Obj])).
