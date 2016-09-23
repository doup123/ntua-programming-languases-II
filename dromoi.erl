-module(dromoi).
-export([file/1,correctness_check/0,path_check/0]).
-include_lib("proper/include/proper.hrl").


file(Filename)->
	Rs=main(Filename),
	Fs=element(1,Rs),
	Lst=element(2,Rs),
	N=lists:nth(1,Fs),
	L=lists:nth(2,Fs),
	X=lists:nth(3,Fs),
	if X==L ->
                0;
 	true->
binary_search(0,N,Lst,X,N,L)
	end.


bruteforce(L,X,N,N,Len)->
	MX=max_path(L,0,0,0,0,0,true,Len),
        if
          MX<X -> N;
        true-> -1
        end;
bruteforce(L,X,N,I,Len)->
L1=[Z || Z <- L ,element(2,Z)=<I],
MX=max_path(L1,0,0,0,0,0,true,Len),
        if
        	MX=<X -> I;
        	true-> bruteforce(L,X,N,I+1,Len)
        end.
%% Η bruteforce ελέγχει σειριακά
%% τις μέρες μέχρι να βρεί το επιθυμητό αποτέλεσμα αν,
%% δεν το βρεί επιστρέφει -1


binary_search(First,Last,_,_,_,_) when (First>Last) -> -1 ; 
%% Η binary_search κάνει δυαδική αναζήτηση στις μέρες και   
%% δίνει στην max_path την αντίστοιχη λίστα για την εκάστοτε
%% μέρα.
binary_search(First,Last,L,X,N,Len) when (First=<Last) ->
 
Middle=(First+Last) div 2,
LLst1=[Z ||Z <-L ,
element(2,Z)=<Middle],LLst2=[Z ||Z <- L,element(2,Z)=<Middle-1],
Mx1=max_path(LLst1,0,0,0,0,0,true,Len),
Mx2=max_path(LLst2,0,0,0,0,0,true,Len), 
	if Mx1>X-> binary_search(Middle+1,Last,L,X,N,Len);
	Mx2>X->Middle;
	true -> binary_search(First,Middle-1,L,X,N,Len)
	end.


max_path([],Max,_,_,_,Arxh,_,Len) -> 
	if Len-Arxh>Max ->Len-Arxh ;
 	true -> Max end; 
%% Οι συναρτήσεις max_path βρίσκουν το μέγιστο μέγεθος δρόμου που
%% δεν έχει χτιστει, ωστόσο δεν περνάμε όλη τη λίστα αλλά μόνο
%% τα διαστήματα που υπάρχουν μέχρι εκείνη τη μέρα.   

max_path(L,Max,Starts,Ends,Kom,Arxh,Flag,Len)  -> 
max_path0(L,Max,Starts,Ends,Kom,Arxh,Flag,Len).
	
max_path0(L,Max,Starts,Ends,Kom,Arxh,Flag,Len)-> 
	Y=element(3,lists:nth(1,L)),
	if Y==0 -> 
                max_path1(L,Max,Starts+1,Ends,Kom,Arxh,Flag,Len);
        true -> max_path1(L,Max,Starts,Ends+1,Kom,Arxh,Flag,Len)
        end.

	
max_path1(L,Max,Starts,Ends,Kom,Arxh,false,Len) ->
max_path2(L,Max,Starts,Ends,Kom,Arxh,false,Len);
max_path1(L,Max,Starts,Ends,Kom,Arxh,true,Len)  ->
	A=element(1,lists:nth(1,L))-Arxh,
	if A>Max -> max_path2(L,A,Starts,Ends,Kom,Arxh,false,Len);
	true -> max_path2(L,Max,Starts,Ends,Kom,Arxh,false,Len)
	end.

max_path2(L,Max,Starts,Starts,Kom,_,_,Len) ->
	ZZ=element(1,lists:nth(1,L)),
	max_path(lists:nthtail(1,L),Max,Starts,Starts,Kom,ZZ,true,Len); 
max_path2(L,Max,Starts,Ends,Kom,Arxh,Flag,Len) -> 
	max_path(lists:nthtail(1,L),Max,Starts,Ends,Kom,Arxh,Flag,Len).

%% Συνάρτηση για άνοιγμα αρχείου και τοποθέτηση των στοιχείων σε μια λίστα
%% απο tuples όπου το πρώτο στοιχείο της tuple είναι το σημείο του δρόμου,
%% δεύτερο η μέρα και τρίτο η διάκριση μεταξύ αρχής και τέλους.
%% Θεωρήσαμε 0 για αρχή και 1 για τέλος.
main(Filename)->
read_integers(element(2,file:open(Filename,read))).

read_integers(Device) ->
    {
    element(2,io:fread(Device,[],"~d~d~d")),
    sort(read_integers(Device, [],1))
    }
.
read_integers(Device, Acc,Cnt) ->
    case io:fread(Device, [], "~d~d") of
    eof  ->
        Acc;
    {ok, [D1, D2]} ->
        read_integers(Device, [{D1,Cnt,0},{D2,Cnt,1}]++Acc,Cnt+1);
    {error,_} ->
        read_integers(Device, Acc,Cnt+1)
    end.


sort([])-> [];
sort([{P,B,D}|Xs]) ->
sort([X || X <- Xs, element(1,X) < P])
++ [{P,B,D}] ++ sort([X || X <- Xs, P =< element(1,X)]).
%% Το sorting αργεί για μεγάλο αριθμό ημερών.

helper(L)->
help(L,[],1).

help([],Acc,_)->Acc;
help(L,Acc,Cnt)->
Take=lists:nth(1,L),F=element(1,Take),S=element(2,Take),
help(lists:nthtail(1,L),[{F,Cnt,0}]++[{S,Cnt,1}]++Acc,Cnt+1).
%% Δημιουργία επιθυμητού τύπου λίστας ώστε να γίνει ύστερα έλεγχος σε αυτήν .


correctness_check() ->
 ?FORALL(Len,integer(1,10000000),
    ?FORALL({L,X}, {non_empty(list(sortedPair(Len))),integer(0,Len-1)},
	?FORALL({L1,Ln},{helper(L),2*length(L)},
	binary_search(0,Ln,sort(L1),X,Ln,Len)=:=
	bruteforce(sort(L1),X,Ln,1,Len)
	))).
%% Συνάρτηση που συγκρίνει την ορθότητα της δυαδικής αναζήτησης με τη
%% σειριακή αναζήτηση με τα μεγέθη να βρίσκονται στα όρια που μας δίνει
%% η εκφώνηση της άσκησης.

path_check()->
 ?FORALL(Len,integer(1,10000000),
    ?FORALL({L}, {non_empty(list(sortedPair(Len)))},
        ?FORALL({L1,Ln},{helper(L),2*length(L)},
	max_path(L1,0,0,0,0,0,true,Ln) =< L
        ))).
%% Συνάρτηση που ελέγχει οτι το διάστημα που δεν έχει χτιστεί είναι 
%% μικρότερο απο το μέγεθος της διαδρομής.


sortedPair(Len) ->
	?LET({A,B}, {integer(1,Len), integer(1,Len)}, sortPair({A,B})).


sortPair({A, B}) when A > B -> {B, A};
sortPair({X, X}) -> {X, X+1};
sortPair(T) -> T.





