%%% @author Tony Rogvall <tony@rogvall.se>
%%% @copyright (C) 2023, Tony Rogvall
%%% @doc
%%%    Convert a RGB image into black and white image
%%% @end
%%% Created :  8 Apr 2023 by Tony Rogvall <tony@rogvall.se>

-module(form_bw).

-export([convert/2]).

-include_lib("epx/include/epx_image.hrl").
-export([write_png_file/2]).
-compile(export_all).

-define(LINE_INTERSECT_LEN, 1).
-define(MIN_HLINE_LEN, 18).
-define(MIN_VLINE_LEN, 18).

-record(hline, { y, x0=0, x1=0 }).
-record(vline, { x, y0=0, y1=0 }).
-record(box, { x, y, w, h }).
-record(bbox, { x0, y0, x1, y1 }).

%% FIXME:
%% 1 - run avg to the l8 map
%% 2 - each pixel check if luminance of src is > l8 map
%%
convert(InputName) ->
    BaseName = filename:basename(InputName, filename:extension(InputName)),
    convert(InputName, BaseName ++ "8.png").
convert(InputName, OutputName) ->
    {ok,IMG} = epx_image:load(InputName),
    [In|_] = epx_image:pixmaps(IMG),
    %% convert into black/white 8 bit image
    %% idea is to average a large enough area left to right
    %% up to down and translate image into a black and white one
    [{width,W},{height,H}] = epx:pixmap_info(In, [width,height]),
    Out = epx:pixmap_create(W, H, l8),
    N = 8,
    epx:pixmap_filter_area(In, Out,
			   %% optionally use a bitmap? circular?
			   {N, N, lists:duplicate(N*N,1)},
			   0, 0, 0, 0, W, H),
    %% filter_data(In, Out, W, H, 8, 8),
    bw_data(In, Out),
    write_png_file(OutputName, Out).

%%
tesseract(InputName) ->
    tesseract(InputName,[]).
tesseract(InputName,LangList) ->
    OutputBase = filename:basename(InputName, filename:extension(InputName)),
    LOpt = case tesseract_lang_opt(LangList) of
	       {true,""} -> "";
	       {true,Lang} -> " -l "++Lang;
	       {false,_,[L]} -> error({language,L,not_installed});
	       {false,_,Ls=[_|_]} -> error({languages,Ls,not_installed});
	       {false,[L],_} -> error({language,L,unknown});
	       {false,Ls,_} -> error({languages,Ls,unknown})
	   end,
    _ = os:cmd("tesseract "++InputName++ " " ++ OutputBase ++
		   LOpt ++ " hocr"),
    %% read_tsv_file(OutputBase++".tsv"),
    read_hocr_file(OutputBase++".hocr").


tesseract_lang_opt([]) -> {true,""};
tesseract_lang_opt(Ls) ->
    case tesseract_lang_list(Ls) of
	{false,Ls1,Ms1}  -> {false,Ls1,Ms1};
	{true,Ls1} -> {true,string:join(Ls1, "+")}
    end.

tesseract_lang_list(Ls) ->
    IMap = maps:from_list([{L,true} || L <- tesseract_installed()]),
    tesseract_lang_list(Ls, IMap, [], [], []).

tesseract_lang_list([L|Ls], IMap, As, Bs, Cs) ->
    case maps:get(L, IMap, false) of
	false ->
	    case maps:get(L, tesseract_package_map(), false) of
		false -> tesseract_lang_list(Ls, IMap, As, [L|Bs], Cs);
		true -> tesseract_lang_list(Ls, IMap, As, Bs, [L|Cs]);
		Alias -> tesseract_lang_list([Alias|Ls], IMap, As, Bs, Cs)
	    end;
	true ->
	    tesseract_lang_list(Ls, IMap, [L|As], Bs, Cs)
    end;
tesseract_lang_list([], _IMap, As, [], []) ->
    {true, lists:reverse(As)};
tesseract_lang_list([], _IMap, _As, Bs, Cs) ->
    {false, lists:reverse(Bs), lists:reverse(Cs)}.


tesseract_installed() ->
    tl(string:tokens(os:cmd("tesseract --list-langs"), "\n")).

%% Known (so far)
tesseract_package_map() ->
    #{
      %% ocr - 3 letter
      "afr" => true,
      "all" => true,
      "amh" => true,
      "ara" => true,
      "asm" => true,
      "aze-cyrl" => true,
      "aze" => true,
      "bel" => true,
      "ben" => true,
      "bod" => true,
      "bos" => true,
      "bre" => true,
      "bul" => true,
      "cat" => true,
      "ceb" => true,
      "ces" => true,
      "chi-sim-vert" => true,
      "chi-sim" => true,
      "chi-tra-vert" => true,
      "chi-tra" => true,
      "chr" => true,
      "cos" => true,
      "cym" => true,
      "dan" => true,
      "deu" => true,
      "div" => true,
      "dzo" => true,
      "ell" => true,
      "eng" => true,
      "enm" => true,
      "epo" => true,
      "est" => true,
      "eus" => true,
      "fao" => true,
      "fas" => true,
      "fil" => true,
      "fin" => true,
      "fra" => true,
      "frk" => true,
      "frm" => true,
      "fry" => true,
      "gla" => true,
      "gle" => true,
      "glg" => true,
      "grc" => true,
      "guj" => true,
      "hat" => true,
      "heb" => true,
      "hin" => true,
      "hrv" => true,
      "hun" => true,
      "hye" => true,
      "iku" => true,
      "ind" => true,
      "isl" => true,
      "ita-old" => true,
      "ita" => true,
      "jav" => true,
      "jpn-vert" => true,
      "jpn" => true,
      "kan" => true,
      "kat-old" => true,
      "kat" => true,
      "kaz" => true,
      "khm" => true,
      "kir" => true,
      "kmr" => true,
      "kor-vert" => true,
      "kor" => true,
      "lao" => true,
      "lat" => true,
      "lav" => true,
      "lit" => true,
      "ltz" => true,
      "mal" => true,
      "mar" => true,
      "mkd" => true,
      "mlt" => true,
      "mon" => true,
      "mri" => true,
      "msa" => true,
      "mya" => true,
      "nep" => true,
      "nld" => true,
      "nor" => true,
      "oci" => true,
      "ori" => true,
      "osd" => true,
      "pan" => true,
      "pol" => true,
      "por" => true,
      "pus" => true,
      "que" => true,
      "ron" => true,
      "rus" => true,
      "san" => true,
      "sin" => true,
      "slk" => true,
      "slv" => true,
      "snd" => true,
      "spa-old" => true,
      "spa" => true,
      "sqi" => true,
      "srp-latn" => true,
      "srp" => true,
      "sun" => true,
      "swa" => true,
      "swe" => true,
      "syr" => true,
      "tam" => true,
      "tat" => true,
      "tel" => true,
      "tgk" => true,
      "tha" => true,
      "tir" => true,
      "ton" => true,
      "tur" => true,
      "uig" => true,
      "ukr" => true,
      "urd" => true,
      "uzb-cyrl" => true,
      "uzb" => true,
      "vie" => true,
      "yid" => true,
      "yor" => true,
      %% script lang 4-letter + alias
      "jpan-vert" => "Japanese_vert",
      "jpan" => "Japanese",
      "arab" => "Arabic",
      "Arabic" => true,
      "Japanese" => true,
      "Japanese_vert" => true,

      "armn" => true,
      "beng" => true,
      "cans" => true,
      "cher" => true,
      "cyrl" => true,
      "deva" => true,
      "ethi" => true,
      "frak" => true,
      "geor" => true,
      "grek" => true,
      "gujr" => true,
      "guru" => true,
      "hang-vert" => true,
      "hang" => true,
      "hans-vert" => true,
      "hans" => true,
      "hant-vert" => true,
      "hant" => true,
      "hebr" => true,

      "khmr" => true,
      "knda" => true,
      "laoo" => true,
      "latn" => true,
      "mlym" => true,
      "mymr" => true,
      "orya" => true,
      "sinh" => true,
      "syrc" => true,
      "taml" => true,
      "telu" => true,
      "thaa" => true,
      "thai" => true,
      "tibt" => true,
      "viet" => true 
}.

read_hocr_file(Filename) ->
    XML = read_hocr_xml(Filename),
    case XML of
	{html, _, [{head,_,_}, {body,_,Es}]} ->
	    Es0 = hocr_elems(Es),
	    hocr_text(Es0);
	_ ->
	    []
    end.

%% clean up - return "text" items
hocr_text(Es) ->
    hocr_text(Es, []).

hocr_text([T={word,_Opts,_Text} | Es], Acc) ->
    hocr_text(Es, add_word(T, Acc));
hocr_text([{_Tag,_Opts,Es0} | Es], Acc) ->
    Acc1 = hocr_text(Es0, Acc),
    hocr_text(Es, Acc1);
hocr_text(T={word,_Opts,_Text}, Acc) ->
    add_word(T, Acc);
hocr_text({line,Opts,Es}, Acc) ->
    [{line,Opts,hocr_text(Es,[])} | Acc];
hocr_text({Tag,Opts,Es0}, Acc) when is_atom(Tag), is_list(Opts) ->
    hocr_text(Es0, Acc);
hocr_text([], Acc) ->
    lists:reverse(Acc).

add_word({word,_Opts,{cdata,""}}, Acc) -> Acc;
add_word(T, Acc) -> [T|Acc].


hocr_elems(Es) ->
    hocr_elems(Es, "eng").

hocr_elems([E], Lang) -> % only one element!
    hocr_elem(E, Lang);
hocr_elems(Es, Lang) -> 
    hocr_elems_(Es, Lang).
			      
%%hocr_elems_([{'div',_Opts,[E]}|Es], Lang0) -> 
%%    [hocr_elem(E,Lang0) | hocr_elems(Es,Lang0)];
hocr_elems_([E|Es], Lang0) -> 
    [hocr_elem(E,Lang0) | hocr_elems(Es, Lang0)];
hocr_elems_([], _) -> 
    [].

hocr_elem({'div',_,Es}, Lang0) ->
    {'div',[],hocr_elems(Es,Lang0)};
hocr_elem({p,Opts,Es}, Lang0) ->
    Lang1 = lang_opt(Opts, Lang0),
    {p,[],hocr_elems(Es, Lang1)};
hocr_elem({span,Opts,Es}, Lang0) ->
    case proplists:get_value(class, Opts) of
	"ocr_line" ->
	    BOpts = box_opt(Opts),
	    {line,BOpts,hocr_elems(Es,Lang0)};
	"ocr_textfloat" ->
	    BOpts = box_opt(Opts),
	    {textfloat,BOpts,hocr_elems(Es,Lang0)};
	"ocr_caption" ->
	    BOpts = box_opt(Opts),
	    {caption,[{lang,Lang0}|BOpts],hocr_elems(Es,Lang0)};
	"ocr_header" ->
	    BOpts = box_opt(Opts),
	    {header,[{lang,Lang0}|BOpts],hocr_elems(Es,Lang0)};
	"ocrx_word" ->
	    BOpts = box_opt(Opts),
	    {word,[{lang,Lang0}|BOpts],Es}
    end.

lang_opt(Opts, Default) ->
    proplists:get_value(lang, Opts, Default).

box_opt(Opts) ->
    Title = proplists:get_value(title, Opts, ""),
    KeyValueList = string:split(Title, "; ", all),
    key_values(KeyValueList).

key_values([KeyValue|KeyValueList]) ->
    {ok,Ts,_} = erl_scan:string(KeyValue),
    [list_to_tuple(normalise(Ts)) | key_values(KeyValueList)];
key_values([]) ->
    [].

normalise([{atom,_,A}|Es]) -> [A | normalise(Es)];
normalise([{'-',_},{integer,_,I}|Es]) -> [-I | normalise(Es)];
normalise([{integer,_,I}|Es]) -> [I | normalise(Es)];
normalise([{'-',_},{float,_,F}|Es]) -> [-F | normalise(Es)];
normalise([{float,_,F}|Es]) -> [F | normalise(Es)];
normalise([]) -> [].

read_hocr_xml(Filename) ->
    {XML,[]} = xmerl_scan:file(Filename, [{space,normalize}]),    
    XML1 = xmerl_lib:simplify_element(XML),
    remove_blanks(XML1).

remove_blanks({Tag,Opts,Body}) ->
    {Tag, Opts, remove_blanks_list(Body)};
remove_blanks({Tag,Opts}) ->
    {Tag, Opts}.

remove_blanks_list([String | Body]) when is_list(String) ->
    case String of
	" " -> 
	    remove_blanks_list(Body);
	_ ->
	    [{cdata,String} | remove_blanks_list(Body)]
    end;
remove_blanks_list([Elem | Body]) ->
    [remove_blanks(Elem) | remove_blanks_list(Body)];
remove_blanks_list([]) ->
    [].

	
read_tsv_file(Filename) ->
    {ok,Fd} = file:open(Filename, [read]),
    {ok,_Header} = file:read_line(Fd),
    Res = read_tsv(Fd, []),
    file:close(Fd),
    Res.

read_tsv(Fd, Acc) ->
    case file:read_line(Fd) of
	{ok, L} ->
	    case string:tokens(L, "\t") of
		[_Level,_PageNum,_BlockNum,_ParNum,_LineNum,_WordNum,
		 Left,Top,Width,Height,_Conf,Text] ->
		    case string:trim(Text) of
			[] ->
			    %% io:format("empty: ~p\n", [L]),
			    read_tsv(Fd, Acc);
			TText ->
			    X = list_to_integer(Left),
			    Y = list_to_integer(Top),
			    W = list_to_integer(Width),
			    H = list_to_integer(Height),
			    read_tsv(Fd, 
				     [{text,#box{x=X,y=Y,w=W,h=H},TText}|Acc])
		    end;
		_Ts ->
		    %% io:format("skip: ~p\n", [L]),
		    read_tsv(Fd, Acc)
	    end;
	eof ->
	    lists:reverse(Acc)
    end.

%% FIXME: scan for horizontal/vertical lines and boxes
boxes(InputName) ->
    BaseName = filename:basename(InputName, filename:extension(InputName)),
    boxes(InputName, BaseName ++ "_boxs.png", ["eng"]).
boxes(InputName, OutputName, LangList) ->
    catch epx:start(),
    {ok,IMG} = epx_image:load(InputName),
    [In|_] = epx_image:pixmaps(IMG),
    [{width,W},{height,H}] = epx:pixmap_info(In, [width,height]),
    {HLines,VLines} = lines_hv(In),
    HBoxes = hlines_to_boxes(HLines),
    VBoxes = vlines_to_boxes(VLines),
    HBoxesMap = create_box_map([{B#box.y, B} || B <- HBoxes]),
    VBoxesMap = create_box_map([{B#box.x, B} || B <- VBoxes]),
    HBoxesMap2 = merge_box_map(HBoxesMap, #box.h, H-1),
    VBoxesMap2 = merge_box_map(VBoxesMap, #box.w, W-1),

    Out = epx:pixmap_create(W, H, rgb),
    epx:pixmap_fill(Out, {255,255,255}),
    epx_gc:set_fill_style(solid),
    epx_gc:set_fill_color({0,0,255}),
    HBoxes1 = box_map_to_list(HBoxesMap2),
    draw_boxes(Out, HBoxes1),
    epx_gc:set_fill_color({255,0,0}),
    VBoxes1 = box_map_to_list(VBoxesMap2),
    draw_boxes(Out, VBoxes1),
    TextBoxes = tesseract(InputName, LangList),
    FontMap = match_fonts(TextBoxes, #{}),
    epx_gc:set_fill_style(none),
    draw_text_boxes(Out, TextBoxes, FontMap),
    write_png_file(OutputName, Out),
    TextBoxes.


match_fonts([{word,Opts,_Data}|Es], FontMap) ->
    case lists:keyfind(bbox, 1, Opts) of
	false -> match_fonts(Es, FontMap);
	Box -> 
	    H = bbox_font_height(Box),
	    case maps:get(H, FontMap, false) of
		false ->
		    %% io:format("h=~w\n", [H]),
		    case epx_font:match([{name, "Arial"},{weight,medium},
					 {slant,roman},{size, H}]) of
			false -> 
			    io:format("warning: font size ~w not found\n", [H]),
			    match_fonts(Es, FontMap);
			{ok,Font} ->
			    io:format("info: loaded font size ~w\n", [H]),
			    match_fonts(Es, FontMap#{ H => Font })
		    end;
		_Font ->
		    match_fonts(Es, FontMap)
	    end
    end;
match_fonts([{_Tag,_Opts,Es0}|Es], FontMap) ->
    FontMap1 = match_fonts(Es0, FontMap),
    match_fonts(Es, FontMap1);
match_fonts([], FontMap) ->
    FontMap.

%% Make next bigger even number height
bbox_font_height(Box) ->
    H = (((Box#bbox.y1 - Box#bbox.y0)+1)-1) band (bnot 1),
    if H < 10 -> 10;
       H > 48 -> 48;
       true -> H
    end.
	     

box_map_to_list(Map) ->
    lists:append([BL || {_, BL} <- maps:to_list(Map)]).

create_box_map(KeyBoxes) ->
    create_box_map(KeyBoxes, #{}).
create_box_map([{Key,Box}|KeyBoxes], Map) ->
    BoxList = maps:get(Key, Map, []),
    Map1 = maps:put(Key, BoxList++[Box], Map),
    create_box_map(KeyBoxes, Map1);
create_box_map([], Map) ->
    Map.

draw_boxes(Pixmap, [#box{x=X,y=Y,w=W,h=H}|Bs]) ->
    if W > H, W > ?MIN_HLINE_LEN ->
	    epx:draw_rectangle(Pixmap, X, Y, W, 2);
       H > ?MIN_VLINE_LEN ->
	    epx:draw_rectangle(Pixmap, X, Y, 2, H);
       true ->
	    ok
    end,
    draw_boxes(Pixmap, Bs);
draw_boxes(_Pixmap, []) ->
    ok.

draw_text_boxes(Pixmap, [{line,Opts,Boxes} | Es], FontMap) ->
    Box = lists:keyfind(bbox, 1, Opts),
    epx_gc:set_border_color({255,0,0}),
    epx:draw_rectangle(Pixmap, Box#bbox.x0, Box#bbox.y0, 
		       (Box#bbox.x1 - Box#bbox.x0) + 1,
		       (Box#bbox.y1 - Box#bbox.y0) + 1),
    draw_text_boxes(Pixmap, Boxes, FontMap),
    draw_text_boxes(Pixmap, Es, FontMap);
draw_text_boxes(Pixmap, [{word,Opts,[{cdata,Text}]} | Es], FontMap) ->
    Box = lists:keyfind(bbox, 1, Opts),
    H = bbox_font_height(Box),
    Font = get_font(H, FontMap),
    epx_gc:set_font(Font),
    epx_gc:set_foreground_color({0,0,0,0}),
    [{_, Ascent}] = epx:font_info(Font, [ascent]),
    UTF8 = unicode:characters_to_binary(Text, utf8),
    epx:draw_utf8(Pixmap, Box#bbox.x0, Box#bbox.y0+Ascent, UTF8),
    epx_gc:set_border_color({0,255,0}),
    epx:draw_rectangle(Pixmap, Box#bbox.x0, Box#bbox.y0, 
		       (Box#bbox.x1 - Box#bbox.x0) + 1,
		       (Box#bbox.y1 - Box#bbox.y0) + 1),
    draw_text_boxes(Pixmap, Es, FontMap);
draw_text_boxes(Pixmap, [{_Tag,_Opts,Boxes} | Es], FontMap) ->
    %% relative boxes?
    draw_text_boxes(Pixmap, Boxes, FontMap),
    draw_text_boxes(Pixmap, Es, FontMap);
draw_text_boxes(Pixmap, [_ | Es], FontMap) ->
    draw_text_boxes(Pixmap, Es, FontMap);
draw_text_boxes(_Pixmap, [], _FontMap) ->
    ok.

get_font(0, FontMap) ->
    maps:get(12, FontMap, undefined);
get_font(Size, FontMap) ->
    case maps:get(Size, FontMap, undefined) of
	undefined ->
	    get_font(Size-1, FontMap);
	Font ->
	    Font
    end.

lines_hv(In) ->
    [{width,W},{height,H}] = epx:pixmap_info(In, [width,height]),
    lines_hv_(In, W, H).

lines_hv_(In, W, H) ->
    HLines = hlines(In, H, W-1, []),
    VLines = vlines(In, W, H-1, []),
    {lists:keysort(#hline.y,HLines), lists:keysort(#vline.x,VLines)}.

hlines(In, H, Xn, Acc) ->
    Ls = [#hline{y=Y} || Y <- lists:seq(0,H-1)],
    hlines_(In, Ls, [], Xn, Acc).

hlines_(In, [L=#hline{y=Y,x1=X,x0=X0}|Ls], Ls1, Xn, Acc) ->
    if X0 >= Xn ->
	    hlines_(In, Ls, Ls1, Xn, add_hline(L,Acc));
       true ->
	    case read_bw(In, X, Y) of
		white ->
		    hlines_(In,Ls,[L#hline{x1=X+1,x0=X+1}|Ls1],Xn,
			    add_hline(L,Acc));
		black ->
		    hlines_(In,Ls,[L#hline{x1=X+1}|Ls1],Xn,Acc)
	    end
    end;
hlines_(_In, [], [], _W, Acc) ->
    Acc;
hlines_(In, [], Ls1, W, Acc) ->
    hlines_(In, Ls1, [], W, Acc).

add_hline(#hline{x1=X1,x0=X0}, Acc) when X1-X0 < ?MIN_HLINE_LEN -> Acc;
add_hline(L=#hline{}, Acc) -> [L|Acc].

vlines(In, W, Ym, Acc) ->
    Ls = [#vline{x=X} || X <- lists:seq(0,W-1)],
    vlines_(In, Ls, [], Ym, Acc).

vlines_(In, [L=#vline{x=X,y1=Y,y0=Y0}|Ls], Ls1, Ym, Acc) ->
    if Y0 >= Ym ->
	    vlines_(In, Ls, Ls1, Ym, add_vline(L,Acc));
       true ->
	    case read_bw(In, X, Y) of
		white ->
		    vlines_(In, Ls,[L#vline{y0=Y+1,y1=Y+1}|Ls1],Ym, 
			    add_vline(L,Acc));
		black ->
		    vlines_(In,Ls,[L#vline{y1=Y+1}|Ls1],Ym,Acc)
	    end
    end;
vlines_(_In, [], [], _Ym, Acc) ->
    Acc;
vlines_(In, [], Ls1, Ym, Acc) ->
    vlines_(In, Ls1, [], Ym, Acc).

add_vline(#vline{y1=Y1,y0=Y0}, Acc) when Y1-Y0 < ?MIN_VLINE_LEN -> Acc;
add_vline(L = #vline{}, Acc) -> [L|Acc].

box_union(#box{x=X0,y=Y0,w=W0,h=H0},#box{x=X1,y=Y1,w=W1,h=H1}) ->
    L = min(X0,X1),
    R = max(X0+W0-1, X1+W1-1),
    T = min(Y0,Y1),
    B = max(Y0+H0-1, Y1+H1-1),
    #box{x=L,y=T,w=(R-L)+1,h=(B-T)+1}.

box_intersect(#box{x=X0,y=Y0,w=W0,h=H0},#box{x=X1,y=Y1,w=W1,h=H1}) ->
    L = max(X0,X1),
    R = min(X0+W0-1, X1+W1-1),
    T = max(Y0,Y1),
    B = min(Y0+H0-1, Y1+H1-1),
    if L > R; T > B -> false;
       true -> #box{x=L,y=T,w=(R-L)+1,h=(B-T)+1}
    end.

hlines_to_boxes(Ls) ->
    [#box{x=X0,y=Y,w=(X1-X0)+1,h=1} || #hline{y=Y,x1=X1,x0=X0} <- Ls].

vlines_to_boxes(Ls) ->
    [#box{x=X,y=Y0,w=1,h=(Y1-Y0)+1} || #vline{x=X,y1=Y1,y0=Y0} <- Ls].

merge_box_map(Map, _Key, 0) ->
    Map;
merge_box_map(Map, Key, I) ->
    case maps:get(I, Map, undefined) of
	undefined -> merge_box_map(Map, Key, I-1);
	Bi ->
	    case maps:get(I-1, Map, undefined) of
		undefined ->	    
		    merge_box_map(Map, Key, I-1);
		Bj ->
		    {Bi1, Bj1} = merge_boxes2(Bi, Bj, Key),
		    Map1 = maps:put(I, Bi1, Map),
		    Map2 = maps:put(I-1, Bj1, Map1),
		    merge_box_map(Map2, Key, I-1)
	    end
    end.


%% Merge boxes from Line Li into line Lj
merge_boxes2(Li, Lj, Key) -> merge_boxes2_(Li, Lj, Key, []).
merge_boxes2_([B|Li], Lj, Key, Li1) ->
    case merge_box_list(B, Lj, Key) of
	false ->
	    merge_boxes2_(Li, Lj, Key, [B|Li1]);
	Lj1 -> 
	    merge_boxes2_(Li, Lj1, Key, Li1)
    end;
merge_boxes2_([], Lj, _Key, Li1) ->
    {lists:reverse(Li1), Lj}.

merge_box_list(B, L, Key) ->
    merge_box_list(B, L, Key, []).

merge_box_list(B1, [B2|L], Key, Acc) ->
    B12 = setelement(Key, B2, 2),
    case box_intersect(B1, B12) of
	false ->
	    merge_box_list(B1, L, Key, [B2|Acc]);  %% not merged
	_B3 ->
	    B4 = box_union(B1,B2),
	    %% io:format("union ~w, ~w = ~w\n", [B1, B2, B4]),
	    lists:reverse([B4|Acc], L)
    end;
merge_box_list(_B1, [], _Key, _) ->
    false.

    
%% Map the filtered gray scale map into 2 level white {255,255,255}
%% and black {0,0,0}
bw_data(In, Out) ->
    fold_pixels(
      fun(X,Y,Pixel,_Acc) ->
	      L0 = rgb_to_luminance(Pixel), %% original luminance 
	      %% averaged luminance
	      {_A1,L1,L1,L1} = epx:pixmap_get_pixel(Out, X, Y), 
	      if 5*L0 > 4*L1 -> %% 80%
		      epx:pixmap_put_pixel(Out, X, Y, {255,255,255});
		 true ->
		      epx:pixmap_put_pixel(Out, X, Y, {0,0,0})
	      end
      end, ok, In).
	      
rgb_to_luminance({_A, R, G, B}) ->
    rgb_to_luminance(R, G, B);
rgb_to_luminance({R, G, B}) ->
    rgb_to_luminance(R, G, B).
rgb_to_luminance(R, G, B) -> %% rgb8!
    (R*299 + G*587 + B*114) div 1000.


fold_pixels(Fun, Acc, Pixmap) ->
    [{width,W},{height,H}] = epx:pixmap_info(Pixmap, [width,height]),
    fold_y_pixel_(Fun, Acc, 0, H, W, Pixmap).

fold_y_pixel_(_Fun, Acc, H, H, _W, _Pixmap) ->
    Acc;
fold_y_pixel_(Fun, Acc, Y, H, W, Pixmap) ->
    fold_xy_pixel_(Fun, Acc, 0, W, Y, Pixmap),
    fold_y_pixel_(Fun, Acc, Y+1, H, W, Pixmap).

fold_xy_pixel_(_Fun, Acc, W, W, _Y, _Pixmap) ->
    Acc;
fold_xy_pixel_(Fun, Acc, X, W, Y, Pixmap) ->
    Pixel = epx:pixmap_get_pixel(Pixmap, X, Y),
    Acc1 = Fun(X, Y, Pixel, Acc),
    fold_xy_pixel_(Fun, Acc1, X+1, W, Y, Pixmap).

read_bw(Pixmap, X, Y) ->
    case epx:pixmap_get_pixel(Pixmap, X, Y) of
	{_,255,255,255} -> white;
	{_,0,0,0}       -> black
    end.

write_png_file(Filename, Pixels) ->
    case file:open(Filename, [raw, binary, write]) of
	{ok,Fd} ->
	    IMG = #epx_image { type = epx_pixmap_png, 
			       filename = Filename,
			       pixmaps = [Pixels] },
	    %% epx_image_png:write_info(Fd, IMG),
	    Res = epx_image_png:write(Fd, IMG),
	    file:close(Fd),
	    Res;
	Error ->
	    Error
    end.
