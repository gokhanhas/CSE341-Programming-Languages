%GOKHAN HAS - 161044067
%CSE341 - HOMEWORK #4

%flights...
flight(istanbul,izmir).
flight(izmir,istanbul).

flight(istanbul,antalya).
flight(antalya,izmir).

flight(istanbul,gaziantep).
flight(gaziantep,istanbul).

flight(istanbul,ankara).
flight(ankara,istanbul).

flight(istanbul,van).
flight(van,istanbul).

flight(istanbul,rize).
flight(rize,istanbul).

flight(edirne,edremit).
flight(edremit,edirne).

flight(edremit,erzincan).
flight(erzincan,edremit).

flight(izmir,isparta).
flight(isparta,izmir).

flight(isparta,burdur).
flight(burdur,isparta).

flight(konya,antalya).
flight(antalya,konya).

flight(antalya,gaziantep).
flight(gaziantep,antalya).

flight(ankara,konya).
flight(konya,ankara).

flight(van,ankara).
flight(ankara,van).

flight(rize,van).
flight(van,rize).

%rules for part-1 ...
route(X,Y) :- flight(X,Y),
              flight(Y,X),
              X\=Y.
route(X,Y) :- flight(X,Z),
              flight(Z,Y),
              X\=Y.



get_min_destination([F|R],M) :- min(R,F,M). min([],M,M). min([[P,L]|R],[_,M],Min) :- L < M, !, min(R,[P,L],Min). min([_|R],M,Min) :- min(R,M,Min).
distance(istanbul,izmir,328).
distance(izmir,istanbul,328).

distance(istanbul,antalya,481).
distance(antalya,izmir,481).

distance(istanbul,gaziantep,850).
distance(gaziantep,istanbul,850).

distance(istanbul,ankara,352).
distance(ankara,istanbul,352).

distance(istanbul,van,1264).
distance(van,istanbul,1264).

distance(istanbul,rize,970).
distance(rize,istanbul,970).

distance(edirne,edremit,252).
distance(edremit,edirne,252).

distance(edremit,erzincan,1044).
distance(erzincan,edremit,1044).

distance(izmir,isparta,307).
distance(isparta,izmir,307).

distance(isparta,burdur,24).
distance(burdur,isparta,24).

distance(konya,antalya,191).
distance(antalya,konya,191).

distance(antalya,gaziantep,594).
distance(gaziantep,antalya,594).

distance(ankara,konya,230).
distance(konya,ankara,230).

distance(van,ankara,920).
distance(ankara,van,920).

distance(rize,van,373).
distance(van,rize,373).
distanceto(X,Y,L) :- travel(X,Y,[X],C,L).

%rules for part2 ...
travel(X,Y,P,[Y|P],L) :- distance(X,Y,L).
travel(X,Y,Visited,Path,L) :-
       distance(X,A,D),
       A \== Y,
       \+member(A,Visited),
       travel(A,Y,[A|Visited],Path,L1),
       L is D + L1.

sroute(A,B,Length) :- setof([P,L],distanceto(A,B,L),Set), get_min_destination(Set,[P,Length]).


whenX(102,10).
whenX(108,12).
whenX(341,14).
whenX(455,16).
whenX(452,17).
whenX(402,14).

where(102,z23).
where(108,z11).
where(341,z06).
where(455,207).
where(452,207).
where(402,z06).

enroll(a,102).
enroll(a,108).
enroll(b,102).
enroll(c,108).
enroll(d,341).
enroll(e,455).

%rules for part3 ...
schedule(S,P,T):- where(X,P), enroll(S,X), whenX(X,T).
usage(P,T):- where(C,P), whenX(C,T).
sameTime(X,Y):- whenX(X,T1), whenX(Y,T2), T1==T2.
sameRoom(X,Y) :- where(X,R1), where(Y,R2), R1==R2.
conflict(X,Y) :- sameRoom(X,Y),sameTime(X,Y).
meet(X,Y) :- enroll(X,C1), enroll(Y,C2), C1==C2,!.


% rules for part4 ...
element(E,S) :- member(E,S),!.

intersection([],L1,L2,L3).
intersection([H|T],L2,L3,[H|L4]):- element(H,L2),intersection(T,L3,L3,L4).


unionx([X|Y],Z,W) :- element(X,Z),  unionx(Y,Z,W).
unionx([X|Y],Z,[X|W]) :- \+ element(X,Z), unionx(Y,Z,W).
unionx([],Z,Z).

equalivalent(X, Y) :- subtract(X, Y, []), subtract(Y, X, []).
