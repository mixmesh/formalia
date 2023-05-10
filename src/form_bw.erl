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

%% FIXME:
%% 1 - run avg to the l8 map
%% 2 - each pixel check if luminance of src is > l8 map
%%
convert(InputFilename, OutputFilename) ->
    {ok,IMG} = epx_image:load(InputFilename),
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
    bw_data(In, Out, W, H),
    write_png_file(OutputFilename, Out).

%%
tesseract(InputName) ->
    Base = filename:basename(InputName, filename:extension(InputName)),
    _ = os:cmd("tesseract "++InputName++ " " ++ Base ++ " tsv"),
    {ok,Fd} = file:open(Base++".tsv", [read]),
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
			    io:format("empty: ~p\n", [L]),
			    read_tsv(Fd, Acc);
			TText ->
			    X = list_to_integer(Left),
			    Y = list_to_integer(Top),
			    W = list_to_integer(Width),
			    H = list_to_integer(Height),
			    read_tsv(Fd, [{box,{X,Y,W,H},TText}|Acc])
		    end;
		_Ts ->
		    io:format("skip: ~p\n", [L]),
		    read_tsv(Fd, Acc)
	    end;
	eof ->
	    lists:reverse(Acc)
    end.

%% FIXME: scan for horizontal/vertical lines and boxes
boxes(InputFilename) ->
    {ok,IMG} = epx_image:load(InputFilename),
    [In|_] = epx_image:pixmaps(IMG),
    HBoxes = hboxes(In, []),
    VBoxes = vboxes(In, []),
    %% now sort for lines and frame boxes
    {HBoxes, VBoxes}.

hboxes(_Pixels, _Acc) ->
    [].

vboxes(_Pixels, _Acc) ->
    [].
    

bw_data(In, Out, W, H) ->
    bw_data_y(In, Out, H, 0, W).

bw_data_y(_In, _Out, 0, _Y, _W) ->
    ok;
bw_data_y(In, Out, I, Y, W) ->
    bw_data_xy(In, Out, W, 0, Y),
    bw_data_y(In, Out, I-1, Y+1, W).

bw_data_xy(_In, _Out, 0, _X, _Y) ->
    ok;
bw_data_xy(In, Out, I, X, Y) ->
    {_A0,R0,G0,B0} = epx:pixmap_get_pixel(In, X, Y),
    L0 = rgb_to_luminance(R0,G0,B0), %% original luminance
    {_A1,L1,L1,L1} = epx:pixmap_get_pixel(Out, X, Y), %% averaged luminance
    if 5*L0 > 4*L1 -> %% 80%
	    epx:pixmap_put_pixel(Out, X, Y, {255,255,255});
       true ->
	    epx:pixmap_put_pixel(Out, X, Y, {0,0,0})
    end,
    bw_data_xy(In, Out, I-1, X+1, Y).


filter_data(Pixmap, Output, W, H, Wd, Hd) ->
    filter_data_y(Pixmap, Output, 0, W, H, Wd, Hd).

filter_data_y(_Pixmap, _Output, _Y, _W, 0, _Wd, _Hd) ->
    ok;
filter_data_y(Pixmap, Output, Y, W, N, Wd, Hd) ->
    filter_data_x(Pixmap, Output, 0, Y, W, Wd, Hd),
    filter_data_y(Pixmap, Output, Y+1, W, N-1, Wd, Hd).

filter_data_x(_Pixmap, _Ouptut, _X, _Y, 0, _Wd, _Hd) ->
    ok;
filter_data_x(Pixmap, Output, X, Y, N, Wd, Hd) ->
    {_A0,R0,G0,B0} = epx:pixmap_get_pixel(Pixmap, X, Y),
    %% {_H0,_S0,L0} = epx_color:rgb_to_hsl({R0,G0,B0}),
    L0 = rgb_to_luminance(R0,G0,B0),

    Area = read_area(Pixmap, Hd, X, Y, Wd),
    %%{_H1,_S1,L1} = epx_color:rgb_to_hsl(avg_area(Area)),
    L1 = rgb_to_luminance(avg_area(Area)),

    epx:pixmap_put_pixel(Output, X, Y, avg_area(Area)),
    if 5*L0 > 4*L1 ->
	    epx:pixmap_put_pixel(Output, X, Y, {255,255,255});
       true ->
	    epx:pixmap_put_pixel(Output, X, Y, {0,0,0})
    end,
    filter_data_x(Pixmap, Output, X+1, Y, N-1, Wd, Hd).

read_area(_Pixmap, 0, _X, _Y, _Width) ->
    [];
read_area(Pixmap, N, X, Y, Width) ->
    R = read_row(Pixmap, Width, X, Y),
    [R|read_area(Pixmap, N-1, X, Y+1, Width)].

read_row(_Pixmap, 0, _X, _Y) ->
    [];
read_row(Pixmap, N, X, Y) ->
    ARGB = epx:pixmap_get_pixel(Pixmap, X, Y),
    [ARGB | read_row(Pixmap, N-1, X+1, Y)].

avg_area(Area) ->
    avg_area(Area,0,{0,0,0}).

avg_area([],N,{R,G,B}) ->
    {trunc(R/N), trunc(G/N), trunc(B/N)};
avg_area([Row|Rs],I,{R,G,B}) ->
    Sum = sum_row(Row,R,G,B),
    avg_area(Rs,I+length(Row),Sum).

sum_row([],R,G,B) ->
    {R,G,B};
sum_row([{_A,R,G,B}|Row], R1,G1,B1) ->
    sum_row(Row, R1+R,G1+G,B1+B).

rgb_to_luminance({R, G, B}) ->
    rgb_to_luminance(R, G, B).
rgb_to_luminance(R, G, B) -> %% rgb8!
    (R*299 + G*587 + B*114) div 1000.

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
