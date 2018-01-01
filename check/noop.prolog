
writeall(X) :- forall(X,writeln(X)).

object(1). object(2). object(3).

action1(X,Y):- object(X), object(Y), sort([at(X)],POS), sort([at(Y)],NEG), POS \== NEG.

action2_ng(X,Y,Z,W):- object(X), object(Y), object(Z), object(W),
                      list_to_set([at(X),at(Z)],POS), % no duplicates, but unordered, so cannot be matched
                      list_to_set([at(Y),at(W)],NEG),
                      POS \== NEG.

%% action2_ng(1,2,2,1) succeeds

action2(X,Y,Z,W):- object(X), object(Y), object(Z), object(W),
                   sort([at(X),at(Z)],POS), % create a set
                   sort([at(Y),at(W)],NEG),
                   POS \== NEG.

%% effects forall U
action3(X,Y):- object(X), object(Y), sort([at(X,U)],POS), sort([at(Y,U)],NEG), POS \== NEG.

%% effects forall U and forall W
%% U and W should be matched and considered as cancelled.

action4_ng(X,Y,U,W):- object(X), object(Y), sort([at(X,U)],POS), sort([at(Y,W)],NEG), POS \== NEG.
%% U \== W, therefore action4_ng(1,1,_6622,_6624) succeeds where U = _6622 and W = _6624.

action4(X,Y,U,W):- object(X), object(Y), sort([at(X,U)],POS), sort([at(Y,W)],NEG), POS \= NEG.
%% U = W. action4(1,1,_6622,_6624) fails, where at(1,_6622) and at(1,_6624) unifies with _6622 = _6624.
