%% @author Hunter Morris <hunter.morris@smarkets.com>
%% @copyright 2008 Smarkets Limited.
%%
%% @doc Smak URL handling methods.
%% @end
%%
%% @reference See <a href="http://www.faqs.org/rfcs/rfc1808.html">RFC
%% 1808</a> for more details.
%%
%% Licensed under the MIT license:
%% http://www.opensource.org/licenses/mit-license.php
%%
%% Including the Python license as some of the methods here were derivative
%% works of urlparse.py in Python 2.6. For example, the unit tests are taken
%% directly from urlparse.py:
%%
%% PYTHON SOFTWARE FOUNDATION LICENSE VERSION 2
%% --------------------------------------------
%%
%% 1. This LICENSE AGREEMENT is between the Python Software Foundation
%% ("PSF"), and the Individual or Organization ("Licensee") accessing and
%% otherwise using this software ("Python") in source or binary form and
%% its associated documentation.
%%
%% 2. Subject to the terms and conditions of this License Agreement, PSF
%% hereby grants Licensee a nonexclusive, royalty-free, world-wide
%% license to reproduce, analyze, test, perform and/or display publicly,
%% prepare derivative works, distribute, and otherwise use Python
%% alone or in any derivative version, provided, however, that PSF's
%% License Agreement and PSF's notice of copyright, i.e., "Copyright (c)
%% 2001, 2002, 2003, 2004, 2005, 2006, 2007, 2008 Python Software Foundation; 
%% All Rights Reserved" are retained in Python alone or in any derivative 
%% version prepared by Licensee.
%%
%% 3. In the event Licensee prepares a derivative work that is based on
%% or incorporates Python or any part thereof, and wants to make
%% the derivative work available to others as provided herein, then
%% Licensee hereby agrees to include in any such work a brief summary of
%% the changes made to Python.
%%
%% 4. PSF is making Python available to Licensee on an "AS IS"
%% basis.  PSF MAKES NO REPRESENTATIONS OR WARRANTIES, EXPRESS OR
%% IMPLIED.  BY WAY OF EXAMPLE, BUT NOT LIMITATION, PSF MAKES NO AND
%% DISCLAIMS ANY REPRESENTATION OR WARRANTY OF MERCHANTABILITY OR FITNESS
%% FOR ANY PARTICULAR PURPOSE OR THAT THE USE OF PYTHON WILL NOT
%% INFRINGE ANY THIRD PARTY RIGHTS.
%%
%% 5. PSF SHALL NOT BE LIABLE TO LICENSEE OR ANY OTHER USERS OF PYTHON
%% FOR ANY INCIDENTAL, SPECIAL, OR CONSEQUENTIAL DAMAGES OR LOSS AS
%% A RESULT OF MODIFYING, DISTRIBUTING, OR OTHERWISE USING PYTHON,
%% OR ANY DERIVATIVE THEREOF, EVEN IF ADVISED OF THE POSSIBILITY THEREOF.
%%
%% 6. This License Agreement will automatically terminate upon a material
%% breach of its terms and conditions.
%%
%% 7. Nothing in this License Agreement shall be deemed to create any
%% relationship of agency, partnership, or joint venture between PSF and
%% Licensee.  This License Agreement does not grant permission to use PSF
%% trademarks or trade name in a trademark sense to endorse or promote
%% products or services of Licensee, or any third party.
%%
%% 8. By copying, installing or otherwise using Python, Licensee
%% agrees to be bound by the terms and conditions of this License
%% Agreement.

-module(smak_url).
-author('Hunter Morris <hunter.morris@smarkets.com>').

-compile(export_all).
-export([unquote/1, quote/1, quote/2]).
-export([parse/1, parse/2, parse/3, unparse/6,
         split/1, unsplit/5,
         join/2, join/3,
         defrag/1]).

-include("smak.hrl").

-define(IS_ALWAYS_SAFE(C), C >= $A, C =< $Z; C >= $a, C =< $z; C >= $0, C =< $9;
                           C =:= $_; C =:= $.; C =:= $-; C =:= $~).

-define(IS_HEX_CHAR(C), C >= $0, C =< $9; C >= $a, C =< $f; C >= $A, C =< $f).

-define(USES_RELATIVE(S), ((S =:= "ftp") or (S =:= "http")) or (S =:= "gopher")
        or (S =:= "nntp") or (S =:= "imap") or (S =:= "wais") or (S =:= "file")
        or (S =:= "https") or (S =:= "shttp") or (S =:= "mms")
        or (S =:= "prospero") or (S =:= "rtsp") or (S =:= "rtspu")
        or (S =:= "") or (S =:= "sftp")).

-define(USES_NETLOC(S), (S =:= "ftp") or (S =:= "http") or (S =:= "gopher")
        or (S =:= "nntp") or (S =:= "telnet") or (S =:= "imap")
        or (S =:= "wais") or (S =:= "file") or (S =:= "mms") or (S =:= "https")
        or (S =:= "shttp") or (S =:= "snews") or (S =:= "prospero")
        or (S =:= "rtsp") or (S =:= "rtspu") or (S =:= "rsync") or (S =:= "")
        or (S =:= "svn") or (S =:= "svn+ssh") or (S =:= "sftp")).

-define(NON_HIERARCHICAL(S), (S =:= "gopher") or (S =:= "hdl")
        or (S =:= "mailto") or (S =:= "news") or (S =:= "telnet")
        or (S =:= "wais") or (S =:= "imap") or (S =:= "snews")
        or (S =:= "sip") or (S =:= "sips")).

-define(USES_FRAGMENT(S), (S =:= "ftp") or (S =:= "hdl") or (S =:= "http")
        or (S =:= "gopher") or (S =:= "news") or (S =:= "nntp")
        or (S =:= "wais") or (S =:= "https") or (S =:= "shttp")
        or (S =:= "snews") or (S =:= "file") or (S =:= "prospero")
        or (S =:= "")).

-define(USES_PARAMS(S), (S =:= "ftp") or (S =:= "hdl") or (S =:= "prospero")
        or (S =:= "http") or (S =:= "imap") or (S =:= "https")
        or (S =:= "shttp") or (S =:= "rtsp") or (S =:= "rtspu")
        or (S =:= "sip") or (S =:= "sips") or (S =:= "mms") or (S =:= "")
        or (S =:= "sftp")).

-define(USES_QUERY(S), (S =:= "http") or (S =:= "wais") or (S =:= "imap")
        or (S =:= "https") or (S =:= "shttp") or (S =:= "mms")
        or (S =:= "gopher") or (S =:= "rtsp") or (S =:= "rtspu")
        or (S =:= "sip") or (S =:= "sips") or (S =:= "")).

-define(IS_VALID_SCHEME_CHAR(C), (((Ch >= $a) and (Ch =< $z))
                                  or ((Ch >= $A) and (Ch =< $Z))
                                  or ((Ch >= $0) and (Ch =< $9))
                                  or (Ch =:= $+)
                                  or (Ch =:= $-)
                                  or (Ch =:= $.))).

%% Put the parts of a URL back together. It should result in an equivalent URL.
unparse(Scheme, Netloc, Path, [], Query, Fragment)
  when is_list(Scheme), is_list(Netloc), is_list(Path), is_list(Query), is_list(Fragment) ->
    unsplit(Scheme, Netloc, Path, Query, Fragment);
unparse(Scheme, Netloc, Path, Params, Query, Fragment)
  when is_list(Scheme), is_list(Netloc), is_list(Path), is_list(Query), is_list(Fragment), is_list(Params) ->
    unsplit(Scheme, Netloc, Path ++ ";" ++ Params, Query, Fragment).

unparse(P) when is_record(P, urlparts) ->
    unparse(P#urlparts.scheme, P#urlparts.netloc, P#urlparts.path,
            P#urlparts.params, P#urlparts.qry, P#urlparts.fragment).

unsplit(Scheme, Netloc, Path, Query, Fragment)
  when is_list(Scheme), is_list(Netloc), is_list(Path), is_list(Query), is_list(Fragment) ->
    unsplit(#urlparts{scheme=Scheme, netloc=Netloc, path=Path,
                      qry=Query, fragment=Fragment}).

%% Put it back together
unsplit(#urlparts{netloc=N, path="/" ++ _=P}=Parts) when N =/= [] ->
    unsplit_scheme("//" ++ N ++ P, Parts);
unsplit(#urlparts{netloc=N, path=P}=Parts) when N =/= [], P =/= [] ->
    unsplit_scheme("//" ++ N ++ "/" ++ P, Parts);
unsplit(#urlparts{netloc=N}=Parts) when N =/= [] ->
    unsplit_scheme("//" ++ N, Parts);
unsplit(#urlparts{path="//" ++ _=P}=Parts) ->
    unsplit_scheme(P, Parts);
unsplit(#urlparts{netloc=N, scheme=Scheme, path="/" ++ _=P}=Parts)
  when Scheme =/= [], ?USES_NETLOC(Scheme) ->
    unsplit_scheme("//" ++ N ++ P, Parts);
unsplit(#urlparts{netloc=N, scheme=Scheme, path=P}=Parts)
  when Scheme =/= [], ?USES_NETLOC(Scheme), P =/= [] ->
    unsplit_scheme("//" ++ N ++ "/" ++ P, Parts);
unsplit(#urlparts{netloc=N, scheme=Scheme}=Parts)
  when Scheme =/= [], ?USES_NETLOC(Scheme) ->
    unsplit_scheme("//" ++ N, Parts).

unsplit_scheme(Url, #urlparts{scheme=[]}=P) ->
    unsplit_query(Url, P);
unsplit_scheme(Url, #urlparts{scheme=Scheme}=P) ->
    unsplit_query(Scheme ++ ":" ++ Url, P).

unsplit_query(Url, #urlparts{qry=[]}=P) ->
    unsplit_fragment(Url, P);
unsplit_query(Url, #urlparts{qry=Query}=P) ->
    unsplit_fragment(Url ++ "?" ++ Query, P).

unsplit_fragment(Url, #urlparts{fragment=[]}) ->
    Url;
unsplit_fragment(Url, #urlparts{fragment=F}) ->
    Url ++ "#" ++ F.

%% Parses a URL into 6 components:
%% <scheme>://<netloc>/<path>;<params>?<query>#<fragment>
%% Components don't get broken up into smaller bits.
parse(Url) when is_list(Url) ->
    parse(Url, []).

parse(Url, Scheme) when is_list(Url), is_list(Scheme) ->
    parse(Url, Scheme, true).

parse(Url, Scheme, AllowFragments)
  when is_list(Url), is_list(Scheme), is_boolean(AllowFragments) ->
    Rec = split(Url, Scheme, AllowFragments),
    case lists:member($\;, Rec#urlparts.path) of
        true when ?USES_PARAMS(Rec#urlparts.scheme) ->
            split_params(Rec);
        _ ->
            Rec
    end.

%% Parses a URL into 5 components:
%% <scheme>://<netloc>/<path>?<query>#<fragment>
%%
%% TODO: Should the http: test be case sensitive??
%% First clause is a (potentially unnecessary) optimisation for http
split(Url) when is_list(Url) ->
    split(Url, [], true).

split([HCh, TCh1, TCh2, PCh, $:|UrlRest], _, AllowFragments)
  when (((HCh =:= $H) or (HCh =:= $h))
        and ((TCh1 =:= $T) or (TCh1 =:= $t))
        and ((TCh2 =:= $T) or (TCh2 =:= $t))
        and ((PCh =:= $P) or (PCh =:= $p))),
       is_boolean(AllowFragments) ->
    lists:foldl(fun(F, Acc) -> F(Acc) end, #urlparts{scheme="http", path=UrlRest},
                [fun(Rec) -> split_netloc(true, Rec) end,
                 fun(Rec) -> split_fragment(AllowFragments, true, Rec) end,
                 fun(Rec) -> split_query(true, Rec) end]);
split(Url, Scheme, AllowFragments)
  when is_list(Url), is_list(Scheme), is_boolean(AllowFragments) ->
    lists:foldl(fun(F, Acc) -> F(Acc) end, #urlparts{path=Url, scheme=Scheme},
                [fun split_scheme/1,
                 fun(Rec) -> split_netloc(false, Rec) end,
                 fun(Rec) -> split_fragment(AllowFragments, false, Rec) end,
                 fun(Rec) -> split_query(false, Rec) end]).

split_scheme(Rec) ->
    case string:chr(Rec#urlparts.path, $:) of
        Colon when Colon > 1 ->
            TmpScheme = string:substr(Rec#urlparts.path, 1, Colon - 1),
            case lists:all(fun scheme_char/1, TmpScheme) of
                true ->
                    Rec#urlparts{scheme=string:to_lower(TmpScheme),
                                 path=string:substr(Rec#urlparts.path, Colon + 1)};
                false ->
                    Rec
            end;
        _ ->
            Rec
    end.

split_netloc(Opt, #urlparts{path="//" ++ _}=Rec) when Opt; ?USES_NETLOC(Rec#urlparts.scheme) ->
    {N, U} = split_netloc_url(Rec#urlparts.path, 3),
    Rec#urlparts{path=U, netloc=N};
split_netloc(_, Rec) ->
    Rec.

split_netloc_url(Url) ->
    split_netloc_url(Url, 1).

split_netloc_url(Url, Start) when is_list(Url) ->
    Delim0 = length(Url) + 1,
    SearchUrl = string:substr(Url, Start),
    Delim = lists:foldl(fun(E, Acc) ->
                                case string:chr(SearchUrl, E) of
                                    0 -> Acc;
                                    I -> lists:min([I + Start - 1, Acc])
                                end
                        end, Delim0, "/?#"),
    {string:substr(Url, Start, Delim - Start), string:substr(Url, Delim)}.

split_fragment(true, Opt, Rec) when Opt; ?USES_FRAGMENT(Rec#urlparts.scheme) ->
    case lists:member($#, Rec#urlparts.path) of
        true ->
            [U, F] = smak_string:split(Rec#urlparts.path, $#, 1),
            Rec#urlparts{path=U, fragment=F};
        _ ->
            Rec
    end;
split_fragment(_, _, Rec) ->
    Rec.

split_query(Opt, Rec) when Opt; ?USES_QUERY(Rec#urlparts.scheme) ->
    case lists:member($?, Rec#urlparts.path) of
        true ->
            [U, Q] = smak_string:split(Rec#urlparts.path, $?, 1),
            Rec#urlparts{path=U, qry=Q};
        _ ->
            Rec
    end;
split_query(_, Rec) ->
    Rec.

split_params(Rec) ->
    case lists:member($/, Rec#urlparts.path) of
        true ->
            Slash = string:rchr(Rec#urlparts.path, $/),
            Str = string:substr(Rec#urlparts.path, Slash),
            I = string:chr(Str, $\;),
            if I < 1 ->
                    Rec;
               true ->
                    Rec#urlparts{path=string:substr(Rec#urlparts.path, 1, Slash + I - 2),
                                 params=string:substr(Rec#urlparts.path, Slash + I)}
            end;
        _ ->
            I = string:chr(Rec#urlparts.path, $\;),
            Rec#urlparts{path=string:substr(Rec#urlparts.path, 1, I - 1),
                         params=string:substr(Rec#urlparts.path, I + 1)}
    end.

%% Join a base URL and a relative URL to form an absolute URL.
join(Base, Url) when is_list(Base), is_list(Url) ->
    join(Base, Url, true).

join([], Url, _) when is_list(Url) ->
    Url;
join(Base, [], _) when is_list(Base) ->
    Base;
join(Base, Url, AllowFragments)
  when is_list(Base), is_list(Url), is_boolean(AllowFragments) ->
    BaseParts = parse(Base, [], AllowFragments),
    Parts = parse(Url, BaseParts#urlparts.scheme, AllowFragments),
    if (Parts#urlparts.scheme =/= BaseParts#urlparts.scheme);
       (not ?USES_RELATIVE(Parts#urlparts.scheme)) ->
            Url;
       ?USES_NETLOC(Parts#urlparts.scheme), (Parts#urlparts.netloc =/= []) ->
            unparse(Parts);
       ?USES_NETLOC(Parts#urlparts.scheme) ->
            join1(BaseParts, Parts#urlparts{netloc=BaseParts#urlparts.netloc});
       true ->
            join1(BaseParts, Parts)
    end.

join1(_, #urlparts{path="/" ++ _}=Parts) ->
    unparse(Parts);
join1(#urlparts{path=Path, params=Params, qry=Query}, #urlparts{path=[], params=[], qry=[], scheme=Scheme, netloc=Netloc, fragment=Fragment}) ->
    unparse(#urlparts{scheme=Scheme,
                         netloc=Netloc,
                         path=Path,
                         params=Params,
                         qry=Query,
                         fragment=Fragment});
join1(#urlparts{path=Path, params=Params}, #urlparts{path=[], params=[], qry=Query, scheme=Scheme, netloc=Netloc, fragment=Fragment}) ->
    unparse(#urlparts{scheme=Scheme,
                         netloc=Netloc,
                         path=Path,
                         params=Params,
                         qry=Query,
                         fragment=Fragment});
join1(#urlparts{path=Path0}, #urlparts{path=[], params=Params, qry=Query, scheme=Scheme, netloc=Netloc, fragment=Fragment}) ->
    Path = string:substr(Path0, 1, length(Path0) - 1),
    unparse(#urlparts{scheme=Scheme,
                         netloc=Netloc,
                         path=Path,
                         params=Params,
                         qry=Query,
                         fragment=Fragment});
join1(#urlparts{path=Bpath}, #urlparts{path=Path}=Parts) ->
    BL = smak_string:split(Bpath, $/),
    {BL0, _} = lists:split(length(BL)-1, BL),
    Segments = BL0 ++ smak_string:split(Path, $/),
    join_segments(Parts, Segments).

%% Apparently, the stuff here is fairly bogus..
join_segments(Parts, Segments0) ->
    JoinedSegments = string:join(lists:foldl(
                                   fun(F, S) -> F(S) end, Segments0,
                                   [fun(Segments) ->
                                            case lists:last(Segments) of
                                                "." ->
                                                    lists:reverse([[]|tl(lists:reverse(Segments))]);
                                                _ ->
                                                    Segments
                                            end
                                    end,
                                    fun(Segments) ->
                                            lists:filter(fun(A) when A =:= "." -> false; (_) -> true end, Segments)
                                    end,
                                    fun prune_dots/1,
                                    fun([[], ".."]) ->
                                            [[], []];
                                       (Segments) when length(Segments) >= 2 ->
                                            case lists:last(Segments) of
                                                ".." ->
                                                    lists:reverse([[]|lists:reverse(lists:sublist(Segments, length(Segments) - 2))]);
                                                _ ->
                                                    Segments
                                            end;
                                       (Segments) ->
                                            Segments
                                    end]), "/"),
    unparse(Parts#urlparts{path=JoinedSegments}).

prune_dots(L) when length(L) < 3 ->
    L;
prune_dots([A|Segments]) ->
    prune_dots(Segments, [A]).

prune_dots([], Acc) ->
    lists:reverse(Acc);
prune_dots([A], Acc) ->
    prune_dots([], [A|Acc]);
prune_dots([A, ".."], Acc) ->
    prune_dots([], ["..",A|Acc]);
prune_dots([A, ".."|R], Acc) when (A =/= []), (A =/= "..") ->
    prune_dots(lists:reverse(Acc) ++ R);
prune_dots([A|R], Acc) ->
    prune_dots(R, [A|Acc]).

unquote(Str) when is_list(Str) ->
    unquote(Str, []).

unquote([], Acc) ->
    Acc;
unquote([$+ | Rest], Acc) ->
    unquote(Rest, [$\s|Acc]);
unquote([Lo, Hi, $\%|Rest], Acc) when ?IS_HEX_CHAR(Lo), ?IS_HEX_CHAR(Hi) ->
    unquote(Rest, [(smak_hex:unhexdigit(Lo) bor (smak_hex:unhexdigit(Hi) bsl 4))|Acc]);
unquote([C | Rest], Acc) ->
    unquote(Rest, [C|Acc]).

quote(Str) when is_list(Str) ->
    quote(Str, $/).

quote_plus(Str0) ->
    Str = quote(Str0, $\ ),
    quote_plus1(Str, []).

quote_plus1([], Acc) ->
    lists:reverse(Acc);
quote_plus1([$\ |T], Acc) ->
    quote_plus1(T, [$+|Acc]);
quote_plus1([H|T], Acc) ->
    quote_plus1(T, [H|Acc]).

quote(Str, Safe) ->
    quote1(Str, Safe, []).

quote1([], _, Acc) ->
    lists:reverse(Acc);
quote1([H|T], Safe, Acc) when ?IS_ALWAYS_SAFE(H); H =:= Safe ->
    quote1(T, Safe, [H|Acc]);
quote1([C|T], Safe, Acc) ->
    <<Hi:4, Lo:4>> = <<C>>,
    quote1(T, Safe, [smak_hex:hexdigit(Lo),smak_hex:hexdigit(Hi),$\%|Acc]).

scheme_char(Ch) when ?IS_VALID_SCHEME_CHAR(Ch) ->
    true;
scheme_char(_) ->
    false.

%% Remove any fragment of a URL, returning a tuple {<url-no-frag>, <frag>}.
defrag(Url) when is_list(Url) ->
    case lists:member($#, Url) of
        true ->
            Rec = parse(Url),
            {unparse(Rec#urlparts{fragment=[]}), Rec#urlparts.fragment};
        _ ->
            {Url, []}
    end.

%% TODO: Move this to eunit or something useful
-define(_TEST_BASE, "http://a/b/c/d").
-define(TESTS, [{"g:h", [?_TEST_BASE, "g:h"]},
                {"http://a/b/c/g", [?_TEST_BASE, "http:g"]},
                {"http://a/b/c/d", [?_TEST_BASE, "http:"]},
                {"http://a/b/c/g", [?_TEST_BASE, "g"]},
                {"http://a/b/c/g", [?_TEST_BASE, "./g"]},
                {"http://a/b/c/g/", [?_TEST_BASE, "g/"]},
                {"http://a/g", [?_TEST_BASE, "/g"]},
                {"http://g", [?_TEST_BASE, "//g"]}, %% Error
                {"http://a/b/c/d?y", [?_TEST_BASE, "?y"]},
                {"http://a/b/c/g?y", [?_TEST_BASE, "g?y"]},
                {"http://a/b/c/g?y/./x", [?_TEST_BASE, "g?y/./x"]},
                {"http://a/b/c/", [?_TEST_BASE, "."]},
                {"http://a/b/c/", [?_TEST_BASE, "./"]},
                {"http://a/b/", [?_TEST_BASE, ".."]}, %% Error
                {"http://a/b/", [?_TEST_BASE, "../"]},
                {"http://a/b/g", [?_TEST_BASE, "../g"]},
                {"http://a/", [?_TEST_BASE, "../.."]},
                {"http://a/g", [?_TEST_BASE, "../../g"]}, %% Error
                {"http://a/../g", [?_TEST_BASE, "../../../g"]}, %% Error
                {"http://a/b/g", [?_TEST_BASE, "./../g"]},
                {"http://a/b/c/g/", [?_TEST_BASE, "./g/."]},
                {"http://a/./g", [?_TEST_BASE, "/./g"]},
                {"http://a/b/c/g/h", [?_TEST_BASE, "g/./h"]},
                {"http://a/b/c/h", [?_TEST_BASE, "g/../h"]}, %% Error
                {"http://a/b/c/g", [?_TEST_BASE, "http:g"]},
                {"http://a/b/c/d", [?_TEST_BASE, "http:"]},
                {"http://a/b/c/d?y", [?_TEST_BASE, "http:?y"]},
                {"http://a/b/c/g?y", [?_TEST_BASE, "http:g?y"]},
                {"http://a/b/c/g?y/./x", [?_TEST_BASE, "http:g?y/./x"]}].

test() ->
    lists:map(fun({Result, Args}) ->
                      io:format("~p = join(~p)...", [Result, Args]),
                      case apply(?MODULE, join, Args) of
                          Result ->
                              io:format("ok~n"),
                              ok;
                          Err ->
                              io:format("ERROR (got ~p)~n", [Err]),
                              {error, Err}
                      end
              end, ?TESTS).
