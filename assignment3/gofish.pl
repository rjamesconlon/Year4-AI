% go fish game
%
% Rules
% Deck shuffles
% Each player dealt 7 cards
% Pairs found are declared
% Players ask for a face (number) of a card
% If the opponent has any, they give all their cards with that face
% The player who asked for the cards then gets another turn
% players draw each turn unless there are no cards in the deck
% whoever has an empty hand first wins
% the computer can remember 6 cards asked by the player
% the remembered cards are only the ones which had no matching pairs


:- dynamic(oppMemory/1).
:- dynamic(hand/2).

% Player (You), and opponent (Computer) hands
hand(player, []).
hand(opponent, []).

% The opponents memory of cards asked by you
% limited to a max of 6
% if you ask a card, and it's successful, it does not go into memory
oppMemory([]).

% make the faces for suit S
make_suit(L, S, NL) :-
    append(L, [
        card(S, ace),
        card(S, 2),
        card(S, 3),
        card(S, 4),
        card(S, 5),
        card(S, 6),
        card(S, 7),
        card(S, 8),
        card(S, 9),
        card(S, 10),
        card(S, jack),
        card(S, queen),
        card(S, king)
        ], NL).

% make the deck with each suit
make_deck(D) :-
    make_suit(_, heart, L1),
    make_suit(L1, diamond, L2),
    make_suit(L2, club, L3),
    make_suit(L3, spade, D).

% randomize the deck
randomized_deck(RD) :-
    make_deck(D),
    random_permutation(D, RD).

% remove amount Size of cards from the deck
remove_cards_from_deck(Deck, Size, Cards, NewDeck) :-
    length(Skip, 0),
    append(Skip, Rest, Deck),

    length(Cards, Size),
    append(Cards, NewDeck, Rest).

% if the deck is empty, just continue
draw_hand([], _, _, _) :- true.

% draw a hand of Size for Player, re asserts the hand to update
draw_hand(Deck, Player, Size, NDeck) :-
    remove_cards_from_deck(Deck, Size, Cards, NDeck),
    hand(Player, H),
    append(H, Cards, NewHand),
    retract(hand(Player, H)),
    asserta(hand(Player, NewHand)).

% Alter the memory of the opponent
% does this by checking if the length is 6
% if length is 6, then takes the tail of the memory, removing the head and appending the new value
% otherwise just appends the new value to the end
% follow FILO with size 6
alter_memory(Card) :-
    oppMemory(X),
    (   (   length(X, 6),
        append(X, [Card], [_|NewMemory]),
        retract(oppMemory(X)),
        asserta(oppMemory(NewMemory)))
    ;
    (   append(X, [Card], NewMemory),
        retract(oppMemory(X)),
        asserta(oppMemory(NewMemory)))).


%remove a card from a hand by subtracting the cards that match
remove_from_hand(Player, Cards) :-
    write('Removing : '),
    write(Cards),
    hand(Player, Hand),
    subtract(Hand, Cards, NewHand),
    retract(hand(Player, Hand)),
    asserta(hand(Player, NewHand)).

% ask for a card
askForCard(Player, Opponent, Card) :-
    write(' Card being asked : '),
    writeln(Card),
    Card =.. [_, _, Face],
    hand(Player, Hand),
    hand(Opponent, HandTwo),
    member(Card, Hand),
    % find all matching cards in the other hand
    % if cards are found, remove relevant cards from hands,
    % then enter player who asked's turn again
    % if no cards are found, and the player who asked is the player (you)
    % then add card to computer memory,
    % otherwise just true
    findall(card(_, Face), member(card(_, Face), HandTwo), Cards),
    (
    (  Cards \= []  ->  (
                        remove_from_hand(Opponent, Cards),
				remove_from_hand(Player, [Card]),
                        (
                        (   (Player == player ->  playerTurn())
                        ; (Player == opponent ->  opponentTurn()))
                        )))
    ;
    ( (   (Cards == [], Player == player) ->  alter_memory(Card)))
    ;
    true
    ).

% check pairs by finding all cards in hand which have the same face, and are not the same term
% then remove them from the hand and update the hand
find_pairs(Player, NewHand) :-
    hand(Player, Hand),
    findall((card(S1, V)), (member(card(S1, V), Hand),
                               member(card(S2, F), Hand),
                               F == V, S1 \== S2), Pairs),
    write(Player),
    write(' pairs found: '),
    writeln(Pairs),
    subtract(Hand, Pairs, NewHand),
    retract(hand(Player, Hand)),
    asserta(hand(Player, NewHand)).

% if no pairs are found
find_pairs(Player, Hand) :-
    hand(Player,Hand),
    findall((card(S1, V)), (member(card(S1, V), Hand), member(card(S2, F), Hand), F == V, S1 \== S2), Pairs),
    Pairs == [].

% player win condition
playerWin(P) :- hand(P, []).

% players turn
% lets you enter a card
playerTurn() :-
    writeln(''),
    writeln('Players turn!'),
    find_pairs(player, NewHand),
    writeln("Your hand is : "),
    writec(NewHand),
    writeln("Enter a card to ask for"),
    writeln("> "),
    read(X),
    askForCard(player, opponent, X).

% opponents turn, check memory, if memory card exists with a matching face, ask for that card
% otherwise ask for the first card in the list
opponentTurn() :-
    writeln(''),
    writeln('Opponents Turn!'),
    oppMemory(X),
    write('Memory : '),
    writeln(X),
    find_pairs(opponent, _),
    hand(opponent, [H|T]),
    %write(' Opponents hand: '),
    %writeln([H|T]),
    ((  findall(card(A,B), (member(card(A,B), [H|T]), member(card(_,B), X)), [CardToAsk|_]),
        askForCard(opponent, player, CardToAsk))
    ;
    (   writeln(H),askForCard(opponent, player, H))
    ).


% set up the game:
% randomisze deck,
% deal 7 cards to each player
% enter game loop
setup_game(NND) :-
    randomized_deck(D),
    draw_hand(D, player, 7, ND),
    draw_hand(ND, opponent, 7, NND),
    game_loop(NND).

% if the deck is empty
game_loop([]) :-
    writeln('No one wins!').

% end the game loop if someone wins
game_loop(_) :-
    (   playerWin(player),
        writeln(' Player wins!!! ')) ;
    (   playerWin(opponent),
        writeln(' Opponent wins!!! ')).

% basic game loop, recursively calling
game_loop(D) :-
    writeln('New turn!'),
    draw_hand(D, player, 1, NewDeck),
    playerTurn(),
    draw_hand(NewDeck, opponent, 1, NND),
    opponentTurn(),
    game_loop(NND).

% write cards
writec([]).
writec([H|T]) :-
    writeln(H),
    writec(T).
