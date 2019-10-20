-module(diff).
-export([evaluate_expression/1 ]).
-compile([export_all]).

% Program purpose:
%   take the symbolic derivative of polynomial term

-spec evaluate_expression(string) -> any().

    evaluate_expression(Expression) ->
      {ok, Tokens, _} = erl_scan:string(Expression),

    % scan the code into tokens
      {ok, Parsed} = erl_parse:parse_exprs(Tokens),

    % parse the tokens into an abstract form
      {value, Result, _} = erl_eval:exprs(Parsed, []),

    % evaluate the expression, return the value
      Result.

start() ->
    Fun  = "fun(X) -> 1 * math:pow(X,2) end.",
    Fun2 = "fun(X) -> 1 * math:sin(X) end.",

    {ok, Tokens, _}    = erl_scan:string(Fun),
    {ok, STokens,_}    = erl_scan:string(Fun2),

    {ok, Parsed}       = erl_parse:parse_exprs(Tokens),
    {ok, SParsed}      = erl_parse:parse_exprs(STokens),

    {value, Result, _} = erl_eval:exprs(Parsed, []),
    {value, SResult,_} = erl_eval:exprs(SParsed, []),
    % ---- ---- ---- ---- ----
    % Tokens for a Term
    %   cx^p
    %   where
    %   c is Coef
    %   x is Var
    %   p is Exp
    % ---- ---- ---- ---- ----
    % Tokens =
    % [{'fun',1},
    %   {'(',1},
    %   {var,1,'X'},
    %   {')',1},
    % {'->',1},
    %   {integer,1,1},
    %     {'*',1},
    %   {atom,1,math},
    %     {':',1},
    %       {atom,1,pow},
    %       {'(',1},
    %         {var,1,'X'},
    %         {',',1},
    %         {integer,1,2},
    %         {')',1},
    % {'end',1},
    % {dot,1}
    % ----------
    Term1          = Tokens,
    Dx_Term1       = apply_Dx(Term1),
    Dx_Dx_Term1    = apply_Dx(Dx_Term1),
    Dx_Dx_Dx_Term1 = apply_Dx(Dx_Dx_Term1),
    %
    Term2          = STokens,
    Dx_Term2       = apply_Dx_trig(Term2),
    Dx_Dx_Term2    = apply_Dx_trig(Dx_Term2),
    Dx_Dx_Dx_Term2 = apply_Dx_trig(Dx_Dx_Term2),
    %
    {ok, Dx_Parsed}         = erl_parse:parse_exprs(Dx_Term1),
    {ok, Dx_Dx_Parsed}      = erl_parse:parse_exprs(Dx_Dx_Term1),
    {ok, Dx_Dx_Dx_Parsed}   = erl_parse:parse_exprs(Dx_Dx_Dx_Term1),
    %
    {ok, Dx_SParsed}        = erl_parse:parse_exprs(Dx_Term2),
    {ok, Dx_Dx_SParsed}     = erl_parse:parse_exprs(Dx_Dx_Term2),
    {ok, Dx_Dx_Dx_SParsed}  = erl_parse:parse_exprs(Dx_Dx_Dx_Term2),
    %
    {value, Dx_Result, _}      = erl_eval:exprs(Dx_Parsed, []),
    {value, Dx_Dx_Result,_}    = erl_eval:exprs(Dx_Dx_Parsed, []),
    {value, Dx_Dx_Dx_Result,_} = erl_eval:exprs(Dx_Dx_Dx_Parsed, []),
    %
    {value, Dx_SResult, _}       = erl_eval:exprs(Dx_SParsed, []),
    {value, Dx_Dx_SResult, _}    = erl_eval:exprs(Dx_Dx_SParsed, []),
    {value, Dx_Dx_Dx_SResult, _} = erl_eval:exprs(Dx_Dx_Dx_SParsed, []),
    %
    ["F2(X) = math:sin(X)","calculate the derivative symbolically",
    {"Dx(F2(x))", Dx_Term2},
    "X takes values from 1 to 4 for the function F(X)=1*pow(X,2)",
    "Take multiple derivatives and evaluate at X",
     [{"F(x)=",                 Result(X),
       "Dx(F(x))=",          Dx_Result(X),
       "Dx^2(F(x))=",     Dx_Dx_Result(X),
       "Dx^3(F(x))=",  Dx_Dx_Dx_Result(X)}
        || X <- lists:seq(1,4)],
     "                                                ",
     "F2 = 1 * sin(X) where X takes the values 0 and pi",
     "for multiple derivatives of x",
     [{"F2(X)=",                 SResult(X),
       "Dx(F2(X))=",          Dx_SResult(X),
       "Dx^2(F2(X)))=",    Dx_Dx_SResult(X),
       "Dx^3(F2(X))))=",Dx_Dx_Dx_SResult(X)}
        || X <- [0.0, math:pi()]   ]
     ].

apply_Dx(Term) ->
   % one term ax^b in the form of a function in the form of tokens
   % for example is a normal form of a term
   %
   %    Fun = fun(X) -> "1 * math:pow(X,2) end".
   %    {ok, Tokens, _}    = erl_scan:string(Fun),
   %    Term = Tokens,
   % --------------------
   % input         ax^b
   % calculate Dx( ax^b )
   % output       b*ax^(b-1)
   % --------------------
   [
   F1, Open, Var, Close, Arrow,
      {integer,1,Coef}, Times, Math, Colon, Pow,
      Open, Var, Comma, {integer,1,Exp}, Close,
   End, Period
   ] = _Tokens = Term,
   % ----------------
   NTokens =
   [
   F1, Open, Var, Close, Arrow,
      {integer,1,Coef*Exp}, Times, Math, Colon, Pow,
      Open, Var, Comma, {integer,1,Exp-1}, Close,
   End, Period
   ],
   NTokens.

% ---- Trig Derivatives

apply_Dx_trig([
    F1, Open, Var, Close, Arrow,
    Coef, Times, Math, Colon, {atom, 1, sin},
    Open, Var, Close,
    End, Period
    ]) ->
      % one term in the form of a function in the form of tokens
      % for example is a normal form of a term
      %
      %    Fun = fun(X) -> "1 * math:sin(X) end."
      %    {ok, Tokens, _}    = erl_scan:string(Fun),
      %    Term = Tokens,
      % --------------------
      % input         a*sin(x)
      % calculate Dx( a*sin(x) )
      % output        a*cos(x)
      % --------------------
      %[
      %F1, Open, Var, Close, Arrow,
      %   Coef, Times, Math, Colon, {atom, 1, sin},
      %   Open, Var, Close,
      %End, Period
      %] = Term,
      % ----------------
      NTokens =
      [
      F1, Open, Var, Close, Arrow,
         Coef, Times, Math, Colon, {atom, 1, cos},
         Open, Var, Close,
      End, Period
      ],
      NTokens;

apply_Dx_trig([
        F1, Open, Var, Close, Arrow,
        Coef, Times, Math, Colon, {atom, 1, cos},
        Open, Var, Close,
        End, Period
        ]) ->
        % --------------------
        % input         a*cos(x)
        % calculate Dx( a*cos(x) )
        % output       -a*sin(x)
        % --------------------
        io:format("Coef~w~n",[Coef]),
        {integer,1,CVal} = Coef,
        NTokens =
        [
        F1, Open, Var, Close, Arrow,
           {integer,1,-CVal}, Times, Math, Colon, {atom, 1, sin},
           Open, Var, Close,
        End, Period
        ],
        NTokens.

test() ->
    Foo =
        fun(Max) ->
            Fun = fun(F, X) when X > Max -> [];
                  (F, X) -> [X | F(F, X+1)]
            end,
            Fun(Fun, 0)
        end,
    Foo(10).

mono() ->
    fun(Fun,_Msg) ->
        io:format("test ~n"),
        Fun
    end.
