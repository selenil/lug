-module(lug_ffi).
-export([drop_byte/1, compile_binary_pattern/1, split_before/2]).

%% borrowed from https://github.com/DanielleMaywood/glexer
drop_byte(String) ->
    case String of
        <<_, Rest/bytes>> -> Rest;
        _ -> String
    end.

%% borrowed from https://github.com/lpil/splitter
compile_binary_pattern(Pattern) ->
    binary:compile_pattern(Pattern).

%% borrowed from https://github.com/lpil/splitter
split_before(Pattern, String) ->
    case binary:match(String, Pattern) of
        % No delimiter found
        nomatch ->
            {String, <<"">>};
        {Index, _Length} ->
            {binary:part(String, 0, Index), binary:part(String, Index, byte_size(String) - Index)}
    end.
