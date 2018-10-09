% You can use this code to get started with your fillin puzzle solver.
% Make sure you replace this comment with your own documentation.
:- ensure_loaded(library(clpfd)).
% :- use_module(library(clpfd), []).

main(PuzzleFile, WordlistFile, SolutionFile) :-
    ensure_loaded(library(clpfd)),
    read_file(PuzzleFile, Puzzle),
    read_file(WordlistFile, Wordlist),
    valid_puzzle(Puzzle),
    solve_puzzle(Puzzle, Wordlist, Solved),
    print_puzzle(SolutionFile, Solved).

read_file(Filename, Content) :-
    open(Filename, read, Stream),
    read_lines(Stream, Content),
    close(Stream).

read_lines(Stream, Content) :-
    read_line(Stream, Line, Last),
    (   Last=true
    ->  (   Line=[]
        ->  Content=[]
        ;   Content=[Line]
        )
    ;   Content=[Line|Content1],
        read_lines(Stream, Content1)
    ).

read_line(Stream, Line, Last) :-
    get_char(Stream, Char),
    (   Char=end_of_file
    ->  Line=[],
        Last=true
    ;   Char='\n'
    ->  Line=[],
        Last=false
    ;   Line=[Char|Line1],
        read_line(Stream, Line1, Last)
    ).

print_puzzle(SolutionFile, Puzzle) :-
    open(SolutionFile, write, Stream),
    maplist(print_row(Stream), Puzzle),
    close(Stream).

print_row(Stream, Row) :-
    maplist(put_puzzle_char(Stream), Row),
    nl(Stream).

put_puzzle_char(Stream, Char) :-
    (   var(Char)
    ->  put_char(Stream, '_')
    ;   put_char(Stream, Char)
    ).

valid_puzzle([]).
valid_puzzle([Row|Rows]) :-
    maplist(same_length(Row), Rows).

solve_puzzle(EmptyPuzzle, WordList, FilledPuzzle) :-
    process_empty_puzzle(EmptyPuzzle, ProcessedEmptyPuzzle),
    clpfd:transpose(EmptyPuzzle, TransposedPuzzle),
    process_empty_puzzle(TransposedPuzzle, ProcessedTransposedPuzzle),
    process_word(WordList, ProcessedWordData),
    process_puzzle(EmptyPuzzle, ProcessedEmptyPuzzle, ProcessedTransposedPuzzle, ProcessedWordData, FilledPuzzle).

process_puzzle(X,_,_,[],X).
process_puzzle(PuzzleToFill, Locations, TransposedLocations, [WordData|ProcessedWordList], FilledPuzzle) :-
    [_, WordLength, Words]=WordData,
    find_data_of_length(WordLength, Locations, MatchingLocations),
    find_data_of_length(WordLength, TransposedLocations, TransposedMatchingLocations),
    possible_puzzle(PuzzleToFill, MatchingLocations, TransposedMatchingLocations, Words, PossiblePuzzle),
    % print_puzzle("./test", PossiblePuzzle),
    process_puzzle(PossiblePuzzle, Locations, TransposedLocations, ProcessedWordList, FilledPuzzle).
% process_puzzle(PuzzleToFill, Locations, TransposedLocations,
%      [WordData|ProcessedWordList], FilledPuzzle) :-
%     [_, WordLength, Words]=WordData,
%     find_data_of_length(WordLength, Locations, MatchingLocations),
%     find_data_of_length(WordLength, TransposedLocations,
%          TransposedMatchingLocations),
%     processing_fixed_locations(PuzzleToFill, MatchingLocations, 
%         TransposedMatchingLocations, Words, RemainingWords, ReducedLocations, 
%         ReducedTransposedLocations, PossibleFixedPuzzle),
%     possible_puzzle(PossibleFixedPuzzle, ReducedLocations, 
%         ReducedTransposedLocations, RemainingWords, PossiblePuzzle),
%     % print_puzzle("./test", PossiblePuzzle),
%     process_puzzle(PossiblePuzzle, Locations, TransposedLocations, 
%         ProcessedWordList, FilledPuzzle).

% outputting possible puzzles from a set of locations and possible words
% possible_puzzle(Puzzle, Locations, TransposedLocations, Words, PossiblePuzzle) :-
%     find_filled_locations(Puzzle, Locations, FixedLocations, ReducedLocations),
%     placing_fixed_words(Puzzle, FixedLocations, Words, ReducedWords1, UpdatedPuzzle1),
%     permutation(ReducedWords1, WordsPermutation),
%     fill_puzzle(UpdatedPuzzle1, ReducedLocations, WordsPermutation, ReducedWords2, UpdatedPuzzle2),
%     clpfd:transpose(UpdatedPuzzle2, TransposedPuzzle),
%     find_filled_locations(TransposedPuzzle, TransposedLocations, FixedTransposedLocations, ReducedTransposedLocations),
%     placing_fixed_words(TransposedPuzzle, FixedTransposedLocations, ReducedWords2, ReducedWords3, UpdatedTransposedPuzzle),
%     fill_puzzle(UpdatedTransposedPuzzle, ReducedTransposedLocations, ReducedWords3, [], FilledTransposedPuzzle),
%     clpfd:transpose(FilledTransposedPuzzle, PossiblePuzzle).

% possible_puzzle(Puzzle, Locations, TransposedLocations, Words, PossiblePuzzle) :-
%     permutation(Words, WordsPermutation),
%     fill_puzzle(Puzzle, Locations, WordsPermutation, ReducedWords1, UpdatedPuzzle1),
%     clpfd:transpose(UpdatedPuzzle1, TransposedPuzzle),
%     find_filled_locations(TransposedPuzzle, TransposedLocations, FixedTransposedLocations, ReducedTransposedLocations),
%     placing_fixed_words(TransposedPuzzle, FixedTransposedLocations, ReducedWords1, ReducedWords2, UpdatedTransposedPuzzle),
%     fill_puzzle(UpdatedTransposedPuzzle, ReducedTransposedLocations, ReducedWords2, [], FilledTransposedPuzzle),
%     clpfd:transpose(FilledTransposedPuzzle, PossiblePuzzle).
  
possible_puzzle(Puzzle, Locations, TransposedLocations, Words, PossiblePuzzle) :-
    permutation(Words, WordsPermutation),
    fill_puzzle(Puzzle, Locations, WordsPermutation, ReducedWords, UpdatedPuzzle),
    clpfd:transpose(UpdatedPuzzle, TransposedPuzzle),
    fill_puzzle(TransposedPuzzle, TransposedLocations, ReducedWords, [], FilledTransposedPuzzle),
    clpfd:transpose(FilledTransposedPuzzle, PossiblePuzzle).

% possible_puzzle(Puzzle, Locations, TransposedLocations, Words, PossiblePuzzle).
    

processing_fixed_locations(Puzzle, Locations, TransposedLocations, Words, RemainingWords, ReducedLocations, ReducedTransposedLocations, FixedPuzzle) :-
    find_filled_locations(Puzzle, Locations, FixedLocations, ReducedLocations),
    placing_fixed_words(Puzzle, FixedLocations, Words, ProcessedWords, UpdatedPuzzle),
    clpfd:transpose(UpdatedPuzzle, TransposedPuzzle),
    find_filled_locations(TransposedPuzzle ,TransposedLocations, FixedTransposedLocations, ReducedTransposedLocations),
    placing_fixed_words(TransposedPuzzle, FixedTransposedLocations, ProcessedWords, RemainingWords, PossibleFixedPuzzle),
    clpfd:transpose(PossibleFixedPuzzle, FixedPuzzle).

% processing_fixed_locations(Puzzle, Locations, TransposedLocations, Words, RemainingWords, ReducedLocations, ReducedTransposedLocations, FixedPuzzle) :-
%     find_filled_locations(Puzzle, Locations, FixedLocations, ReducedLocations),
%     clpfd:transpose(Puzzle, TransposedPuzzle),
%     find_filled_locations(TransposedPuzzle ,TransposedLocations, FixedTransposedLocation, ReducedTransposedLocations),
%     filling_fixed_words(Puzzle, FixedLocations, FixedTransposedLocation, Words, RemainingWords, FixedPuzzle).

% filling_fixed_words(Puzzle, Locations, TransposedLocations, Words, 
%     RemainingWords, UpdatedPuzzle) :-
%     placing_fixed_words(Puzzle, Locations, Words, 
%         UpdatedWordList, FilledPuzzle),
%     clpfd:transpose(FilledPuzzle, TransposedPuzzle),
%     placing_fixed_words(TransposedPuzzle, TransposedLocations, UpdatedWordList, 
%         RemainingWords, ProcessedPuzzle),
%     clpfd:transpose(ProcessedPuzzle, UpdatedPuzzle).




% filling_fixed_words(Puzzle, Locations, Words, 
%     RemainingWords, UpdatedPuzzle) :-
%     placing_fixed_words(Puzzle, Locations, Words, 
%         UpdatedWordList, FilledPuzzle),
%     clpfd:transpose(FilledPuzzle, TransposedPuzzle),
%     placing_fixed_words(TransposedPuzzle, TransposedLocations, UpdatedWordList, 
%         RemainingWords, ProcessedPuzzle),
%     clpfd:transpose(ProcessedPuzzle, UpdatedPuzzle).



placing_fixed_words(X,[],Y,Y,X).
placing_fixed_words(PuzzleToFill, [Location|Locations], Words, UnusedWords, ReplacedPuzzle) :-
    [RowNumber, ColumnNumber] = Location,
    try_word(PuzzleToFill, RowNumber,ColumnNumber, Words, MatchingWord, UpdatedPuzzle),
    delete(Words, MatchingWord, UpdatedWordList),
    placing_fixed_words(UpdatedPuzzle, Locations, UpdatedWordList, UnusedWords, ReplacedPuzzle).

try_word([EmptyRow|Puzzle], 0, Column, [Word|_], Word, [ReplacedRow|Puzzle]) :-
    replace_row(EmptyRow, Word, Column, ReplacedRow).
try_word(Puzzle, 0, Column, [_|Words], Word, UpdatedPuzzle) :-
    try_word(Puzzle, 0, Column, Words, Word, UpdatedPuzzle).
try_word([Row|Puzzle], Row, Column, Words, Word, [Row|UpdatedPuzzle]) :-
    NextRow is Row-1,
    NextRow >= 0,
    try_word(Puzzle, NextRow, Column, Words, Word, UpdatedPuzzle).

find_filled_locations(_,[],[],[]).
find_filled_locations(Puzzle, [Location|Locations], [Location|FilledLocation], NonFixedLocations) :-
    [Row, Column] = Location,
    location_is_filled(Puzzle, Row, Column),
    find_filled_locations(Puzzle, Locations, FilledLocation, NonFixedLocations).
find_filled_locations(Puzzle, [Location|Locations], FilledLocation, [Location|NonFixedLocations]) :- 
    find_filled_locations(Puzzle, Locations, FilledLocation, NonFixedLocations).


location_is_filled([Row|_], 0, Column) :-
    slot_is_filled(Row, Column).
location_is_filled([_|Puzzle], Row, Column) :-
    NextRow is Row-1,
    NextRow >= 0,
    location_is_filled(Puzzle, NextRow, Column).

slot_is_filled([Character|_], 0) :-
    Character \= '_',
    Character \= '#'.
slot_is_filled([Character|Slot], 0) :-
    Character \= '#',
    slot_is_filled(Slot, 0).
slot_is_filled([_|Slot], Index) :-
    NextIndex is Index - 1,
    NextIndex >= 0,
    slot_is_filled(Slot, NextIndex).

find_matching_word(Row, ColumnNumber, [Word|_], Word) :-
    replace_row(Row, Word, ColumnNumber, _).
find_matching_word(Row, ColumnNumber, [_|Words], Word) :-
    find_matching_word(Row, ColumnNumber, Words, Word).

fetch_row([Row|_], 0, Row).
fetch_row([_|Puzzle], RowNumber, Row) :-
    NextRow is RowNumber-1,
    NextRow >= 0,
    fetch_row(Puzzle, NextRow, Row).


fill_puzzle(X,[],Y,Y,X).
fill_puzzle(EmptyPuzzle, [Location|Locations], [Word|Words], UnusedWords, ReplacedPuzzle) :-
    [RowNumber, ColumnNumber] = Location,
    RowNumber >= 0,
    fill_word(EmptyPuzzle, RowNumber, ColumnNumber, Word, FilledPuzzle),
    fill_puzzle(FilledPuzzle, Locations, Words, UnusedWords, ReplacedPuzzle).
% process puzzle data into a list of [[Amount, Length, [[RowNumber, ColumnNumber]]]
process_empty_puzzle(EmptyPuzzle, GroupedPuzzleData) :-
    puzzle_data(EmptyPuzzle, 0, [], PuzzleData),
    sort(PuzzleData, [H|SortedPuzzleData]),
    [Length, Location]=H,
    group_data(SortedPuzzleData, 1, [Location], Length, GroupedPuzzleData).

find_data_of_length(_,[],[]).
find_data_of_length(Length, [ProcessedData|_], BoundDatas) :-
    [_, Length, BoundDatas]=ProcessedData.
find_data_of_length(Length, [_|ProcessedDatas], BoundDatas) :-
    find_data_of_length(Length, ProcessedDatas, BoundDatas).

% fill_word takes in the empty puzzle, row number, column number, word to fill
% and outputs the puzzle filled with that word in that specific location
fill_word([Row|Puzzle], 0, ColumnNumber, Word, [ReplacedRow|Puzzle]) :-
    replace_row(Row, Word, ColumnNumber, ReplacedRow).
fill_word([Row|Puzzle], RowNumber, ColumnNumber, Word, [Row|ReplacedPuzzle]) :-
    NextRow is RowNumber-1,
    NextRow >= 0,
    fill_word(Puzzle, NextRow, ColumnNumber, Word, ReplacedPuzzle).

replace_row([], [], 0, []).
replace_row([H|Row], [], 0, [H|Row]) :-
    H=='#'.
replace_row([H|Row], [W|Word], 0, [W|FilledRow]) :-
    (   H=='_'
    ;   H==W
    ),
    replace_row(Row, Word, 0, FilledRow).
replace_row([H|Row], Word, ColumnNumber, [H|FilledRow]) :-
    ColumnNumber>=0,
    Index is ColumnNumber-1,
    replace_row(Row, Word, Index, FilledRow).

puzzle_data([],_,X,X).
puzzle_data([Row|EmptyPuzzle], RowNumber, DataToAppend, PuzzleData) :-
    analyze_row(Row, [], 0, RowNumber, RowData),
    append(DataToAppend, RowData, UpdatedData),
    NextRow is RowNumber+1,
    puzzle_data(EmptyPuzzle, NextRow, UpdatedData, PuzzleData).

analyze_row([],[],_, _,[]).
analyze_row([], Slot, ColumnNumber, RowNumber, [[SlotLength, [RowNumber, Index]]]) :-
    \+ memberchk('#', Slot),
    length(Slot, SlotLength), SlotLength \= 0, Index is ColumnNumber-SlotLength.
analyze_row(['#'|Row], Slot, ColumnNumber, RowNumber, [[SlotLength, [RowNumber, Index]]|RowData]) :-
    \+ memberchk('#', Slot),
    length(Slot, SlotLength),
    SlotLength \= 0,
    Index is ColumnNumber - SlotLength,
    NextIndex is ColumnNumber+1,
    analyze_row(Row, [], NextIndex, RowNumber, RowData).
analyze_row([H|Row], Slot, ColumnNumber, RowNumber, RowData) :-
    H \= '#',
    \+ memberchk('#', Slot),
    append([H], Slot, NewSlot),
    NextIndex is ColumnNumber+1,
    analyze_row(Row, NewSlot, NextIndex, RowNumber, RowData).
analyze_row([_|Row], Slot, ColumnNumber, RowNumber, RowData) :-
    \+ memberchk('#', Slot),
    NextIndex is ColumnNumber + 1,
    analyze_row(Row, Slot, NextIndex,RowNumber, RowData).


process_word(WordList, SortedProcessedWordList) :-
    word_length(WordList, WordDataList),
    sort(WordDataList, [H|SortedWordList]),
    [H|SortedWordList] \= [],
    [Length, Word] = H,
    group_data(SortedWordList, 1, [Word], Length, ProcessedWordList),
   sort(ProcessedWordList, SortedProcessedWordList).

quicksort_word_data([],[]).
quicksort_word_data([X|Xs],Ys) :-
    pivot(Xs,X,Left,Right),
    quicksort_word_data(Left,Ls),
    quicksort_word_data(Right,Rs),
    append(Ls,[X|Rs],Ys).

pivot([],_,[],[]).
pivot([X|Xs],Y,[X|Ls],Rs) :-
    [Amount1, Length1, _] = X,
    [Amount2, Length2, _] = Y,
    (Amount1 < Amount2 
    ; Amount1 == Amount2, Length1 > Length2), 
    pivot(Xs,Y,Ls,Rs).
pivot([X|Xs],Y,Ls,[X|Rs]) :-
    [Amount1, _, _] = X,
    [Amount2, _, _] = Y,
    Amount1 > Amount2, pivot(Xs,Y,Ls,Rs).


% process WordList or RowData into a list of [Amount, Length, [Word of that length or Location of that length]]
group_data([], Amount, CurrentGroup, CurrentCategory, [[Amount, CurrentCategory, CurrentGroup]]).
group_data([Data|DataList], Amount, CurrentGroup, CurrentCategory, [[Amount, CurrentCategory, CurrentGroup]| GroupedData]) :-
    [Length, DataToGroup]=Data,
    Length \= CurrentCategory,
    group_data(DataList, 1, [DataToGroup], Length, GroupedData).
group_data([Data|DataList], Amount, CurrentGroup, CurrentCategory, GroupedData) :-
    [Length, DataToGroup]=Data,
    Length == CurrentCategory,
    NewAmount is Amount+1,
    append(CurrentGroup, [DataToGroup], NewCurrentGroup),
    group_data(DataList, NewAmount, NewCurrentGroup, CurrentCategory, GroupedData).

word_length([],[]).
word_length([Word|WordList], [[Length, Word] | ProcessedWord]) :-
    length(Word, Length),
    word_length(WordList, ProcessedWord).

mydebug(Length) :-
    read_file("./samples/puzzle1", X),
    read_file("./samples/words1", Y),
    process_word(Y, ProcessedY),
    find_data_of_length(Length, ProcessedY, Z),
    process_empty_puzzle(X, EmptyX),
    find_data_of_length(Length, EmptyX, EmptyXData),
    clpfd:transpose(X, TX),
    process_empty_puzzle(TX, EmptyTX),
    find_data_of_length(Length, EmptyTX, EmptyTXData),
    possible_puzzle(X, EmptyXData, EmptyTXData, Z, A),
    print_puzzle("./test", A).

% TODO fix the infinite loop, might be in find fixed location