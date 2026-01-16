-module(users_lookup_handler).
-export([init/2]).

-include_lib("kernel/include/logger.hrl").

init(Req0, State) ->
    Method = cowboy_req:method(Req0),
    case Method of
        <<"POST">> ->
            {ok, Body, Req1} = cowboy_req:read_body(Req0),
            case decode_body(Body) of
                {ok, Phones} ->
                    case lookup_users(Phones) of
                        {ok, Users} ->
                            send_json(Req1, 200, #{users => Users});
                        {error, Reason} ->
                            ?LOG_ERROR("❌ users_lookup_handler DB error: ~p", [Reason]),
                            send_json(Req1, 500, #{error => <<"internal_error">>})
                    end,
                    {ok, Req1, State};
                {error, Reason, Status} ->
                    send_json(Req1, Status, #{error => Reason}),
                    {ok, Req1, State}
            end;
        _ ->
            send_json(Req0, 405, #{error => <<"method_not_allowed">>}),
            {ok, Req0, State}
    end.

decode_body(Body) ->
    try jsx:decode(Body, [return_maps]) of
        #{<<"phones">> := Phones} when is_list(Phones) ->
            Normalized = normalize_inputs(Phones),
            case Normalized of
                [] -> {error, <<"no_valid_phones">>, 400};
                _ -> {ok, Normalized}
            end;
        _ ->
            {error, <<"invalid_payload">>, 400}
    catch
        _:_ ->
            {error, <<"invalid_json">>, 400}
    end.

normalize_inputs(Phones) ->
    lists:foldl(
        fun(Value, Acc) ->
            case normalize_phone(Value) of
                undefined -> Acc;
                Phone -> [Phone | Acc]
            end
        end,
        [],
        Phones
    ).

normalize_phone(Value) ->
    Bin = auth_util:ensure_binary_utf8(Value),
    Digits = re:replace(Bin, "[^0-9]", "", [global, {return, binary}]),
    case Digits of
        <<>> -> undefined;
        _ -> Digits
    end.

lookup_users(Phones) ->
    db_util:with_connection(fun(Conn) ->
        % ✅ BUSCA FLEXÍVEL - por substring
        Sql = "
            SELECT id, name, phone 
            FROM users 
            WHERE regexp_replace(phone, '[^0-9]', '', 'g') = $1
            LIMIT 10
            ",
            
        Users = lists:foldl(fun(Phone, Acc) ->
            case epgsql:equery(Conn, Sql, [Phone]) of
                {ok, _, Rows} ->
                    Acc ++ lists:map(fun map_user_row/1, Rows);
                _ ->
                    Acc
            end
        end, [], Phones),
            
        {ok, Users}
    end).

map_user_row({Id, Name, Phone}) ->
    Normalized = normalize_phone(Phone),
    #{
        <<"id">> => auth_util:ensure_binary_utf8(Id),
        <<"name">> => auth_util:ensure_binary_utf8(Name),
        <<"phone">> => auth_util:ensure_binary_utf8(Phone),
        <<"phone_normalized">> => Normalized
    }.

send_json(Req, Status, Map) ->
    Json = jsx:encode(Map),
    cowboy_req:reply(Status, #{<<"content-type">> => <<"application/json">>}, Json, Req).

