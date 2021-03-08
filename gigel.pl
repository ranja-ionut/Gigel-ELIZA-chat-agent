:- ensure_loaded('chat.pl').
% Returneaza true dacă regula dată ca argument se potriveste cu
% replica data de utilizator. Replica utilizatorului este
% reprezentata ca o lista de tokens. Are nevoie de
% memoria replicilor utilizatorului pentru a deduce emoția/tag-ul
% conversației.
% match_rule/3
% match_rule(_Tokens, _UserMemory, rule(_, _, _, _, _)) :- fail.
match_rule(Tokens, UserMemory, rule(Expr, _, _, Emot, Tags)) :-
   Expr = Tokens, length(Emot, LE), length(Tags, LT),
   (LE == 0, LT == 0;
   LE == 0, LT == 1, get_head(Tags, Tag), get_tag(UserMemory, Tag);
   LE == 1, LT == 0, get_head(Emot, Emotion), get_emotion(UserMemory, Emotion)).

% Primeste replica utilizatorului (ca lista de tokens) si o lista de
% reguli, iar folosind match_rule le filtrează doar pe cele care se
% potrivesc cu replica dată de utilizator.
% find_matching_rules/4
% find_matching_rules(+Tokens, +Rules, +UserMemory, -MatchingRules)
find_matching_rules(Tokens, Rules, UserMemory, MatchingRules) :-
    findall(Rule, (member(Rule, Rules), match_rule(Tokens, UserMemory, Rule)), AllRules),
    include(has_tags(_), AllRules, TagRules),
    include(has_emotions(_), AllRules, EmotionRules),
    include(has_nothing(_), AllRules, NormalRules),
    append(EmotionRules, NormalRules, TempRules),
    append(TagRules, TempRules, MatchingRules).

has_emotions(rule(_, _, _, Emotion, Tag), rule(_, _, _, Emotion, Tag)) :- length(Emotion, LE), length(Tag, LT), LE > 0, LT == 0.
has_tags(rule(_, _, _, Emotion, Tag), rule(_, _, _, Emotion, Tag)) :- length(Emotion, LE), length(Tag, LT), LE == 0, LT > 0.
has_nothing(rule(_, _, _, Emotion, Tag), rule(_, _, _, Emotion, Tag)) :- length(Emotion, LE), length(Tag, LT), LE == 0, LT == 0.

% Intoarce in Answer replica lui Gigel. Selecteaza un set de reguli
% (folosind predicatul rules) pentru care cuvintele cheie se afla in
% replica utilizatorului, in ordine; pe setul de reguli foloseste
% find_matching_rules pentru a obtine un set de raspunsuri posibile.
% Dintre acestea selecteaza pe cea mai putin folosita in conversatie.
%
% Replica utilizatorului este primita in Tokens ca lista de tokens.
% Replica lui Gigel va fi intoarsa tot ca lista de tokens.
%
% UserMemory este memoria cu replicile utilizatorului, folosita pentru
% detectarea emotiei / tag-ului.
% BotMemory este memoria cu replicile lui Gigel și va si folosită pentru
% numararea numarului de utilizari ale unei replici.
%
% In Actions se vor intoarce actiunile de realizat de catre Gigel in
% urma replicii (e.g. exit).
%
% Hint: min_score, ord_subset, find_matching_rules

% select_answer/5
% select_answer(+Tokens, +UserMemory, +BotMemory, -Answer, -Actions)
select_answer(Tokens, UserMemory, BotMemory, Answer, Actions) :-
   findall(Rule, (rules(Keywords, TempRules), ord_subset(Keywords, Tokens), !, member(Rule, TempRules)), Rules),
   find_matching_rules(Tokens, Rules, UserMemory, MatchingRules),
   findall(Replies, (member(X, MatchingRules), parse_rule(X, rule(_, Replies, _, _, _))), Answers),
   findall(GoodAnswer, (member(W, Answers), member(GoodAnswer, W)), GoodAnswers),
   add_answers(BotMemory, GoodAnswers, NewBotMemory),
   findall(Key, (member(V, GoodAnswers), unwords(V, Key)), Keys),
   findall(Tuple, (member(Y, Keys), get_value(NewBotMemory, Y, Val), create_tuple(Y, Val, Tuple)), Tuples),
   min_element(Tuples, UnwordsAnswer),
   words(UnwordsAnswer, Answer),
   findall(TempRule, (member(A, MatchingRules), parse_rule(A, TempRule), parse_rule(TempRule, rule(_, Replies, _, _, _)), member(B, Replies), Answer == B), UsedRuleList),
   member(UsedRule, UsedRuleList),
   parse_rule(UsedRule, rule(_, _, Actions, _, _)).

parse_rule(rule(Expr, Replies, Act, Emot, Tags), rule(Expr, Replies, Act, Emot, Tags)).

create_tuple(A, B, C) :- C = (A, B).

add_answers(Dict, [], New) :- New = Dict, !.
add_answers(Dict, [H|T], New) :-
   add_answer(H, Dict, NewDict), add_answers(NewDict, T, New).


% Esuează doar daca valoarea exit se afla in lista Actions.
% Altfel, returnează true.
% handle_actions/1
% handle_actions(+Actions)
handle_actions(Actions) :- \+member(exit, Actions).


% Caută frecvența (numărul de apariți) al fiecarui cuvânt din fiecare
% cheie a memoriei.
% e.g
% ?- find_occurrences(memory{'joc tenis': 3, 'ma uit la box': 2, 'ma uit la un film': 4}, Result).
% Result = count{box:2, film:4, joc:3, la:6, ma:6, tenis:3, uit:6, un:4}.
% Observați ca de exemplu cuvântul tenis are 3 apariți deoarce replica
% din care face parte a fost spusă de 3 ori (are valoarea 3 în memorie).
% Recomandăm pentru usurința să folosiți înca un dicționar în care să tineți
% frecvențele cuvintelor, dar puteți modifica oricum structura, această funcție
% nu este testată direct.

% find_occurrences/2
% find_occurrences(+UserMemory, -Result)
find_occurrences(UserMemory, Result) :-
    dict_keys(UserMemory, Keys),
    findall(Key, (member(X, Keys), words(X, Key)), WordsKeys),
    setof(Z, (findall(E, (member(Y, WordsKeys), member(E, Y)), Words), member(Z, Words)), Uniques),
    findall(Item, (setof(A, (member(U, Uniques), findall(B, (member(B, WordsKeys), member(U,B)), A)), Items), member(C, Items), findall(N, (member(D, C), unwords(D, F), get_value(UserMemory, F, N)), Numbers), add(Numbers, 0, Val), create_tuple(U, Val, Item)), Result).

add([], O, N) :- N is O.
add([H|T], Origin, N) :- add(T, Origin, N1), N is N1 + H.


% Atribuie un scor pentru fericire (de cate ori au fost folosit cuvinte din predicatul happy(X))
% cu cât scorul e mai mare cu atât e mai probabil ca utilizatorul să fie fericit.
% get_happy_score/2
% get_happy_score(+UserMemory, -Score)
get_happy_score(UserMemory, Score) :- find_occurrences(UserMemory, Occ),
    findall(N, (member(X, Occ), parse_tuple(X, (K, N)), happy(K)), Temp),
    length(Temp, L), (L == 0, Score = 0; L > 0, get_head(Temp, Score)), !.

parse_tuple((X, Y), (X, Y)).
get_head([H], H).

% Atribuie un scor pentru tristețe (de cate ori au fost folosit cuvinte din predicatul sad(X))
% cu cât scorul e mai mare cu atât e mai probabil ca utilizatorul să fie trist.
% get_sad_score/2
% get_sad_score(+UserMemory, -Score)
get_sad_score(UserMemory, Score) :- find_occurrences(UserMemory, Occ),
    findall(N, (member(X, Occ), parse_tuple(X, (K, N)), sad(K)), Temp),
    length(Temp, L), (L == 0, Score = 0; L > 0, get_head(Temp, Score)), !.

% Pe baza celor doua scoruri alege emoția utilizatorul: `fericit`/`trist`,
% sau `neutru` daca scorurile sunt egale.
% e.g:
% ?- get_emotion(memory{'sunt trist': 1}, Emotion).
% Emotion = trist.
% get_emotion/2
% get_emotion(+UserMemory, -Emotion)
get_emotion(UserMemory, Emotion) :-
    get_happy_score(UserMemory, HS), get_sad_score(UserMemory, SS),
    (HS > SS, Emotion = fericit; HS < SS, Emotion = trist; HS == SS, Emotion = neutru).

% Atribuie un scor pentru un Tag (de cate ori au fost folosit cuvinte din lista tag(Tag, Lista))
% cu cât scorul e mai mare cu atât e mai probabil ca utilizatorul să vorbească despre acel subiect.
% get_tag_score/3
% get_tag_score(+Tag, +UserMemory, -Score)
get_tag_score(Tag, UserMemory, Score) :-
    dict_keys(UserMemory, Keys),
    length(Keys, L), (L == 0, Score = 0;
    findall(Key, (member(K, Keys), words(K, Key)), WordKeys),
    setof(X, (findall(Y, (member(Z, WordKeys), member(Y, Z)), Words), member(X, Words)), Uniques),
    tag(Tag, Tags),
    findall(T, (member(T, Uniques), member(T, Tags)), TagList),
    length(TagList, Score)).


% Pentru fiecare tag calculeaza scorul și îl alege pe cel cu scorul maxim.
% Dacă toate scorurile sunt 0 tag-ul va fi none.
% e.g:
% ?- get_tag(memory{'joc fotbal': 2, 'joc box': 3}, Tag).
% Tag = sport.
% get_tag/2
% get_tag(+UserMemory, -Tag)
get_tag(UserMemory, Tag) :-
    get_tag_score(sport, UserMemory, SS), get_tag_score(film, UserMemory, FS),
    (SS > FS, Tag = sport; SS < FS, Tag = film; SS == FS, Tag = none).
