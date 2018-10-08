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
    process_puzzle(PossiblePuzzle, Locations, TransposedLocations, ProcessedWordList, FilledPuzzle).

% outputting possible puzzles from a set of locations and possible words
possible_puzzle(EmptyPuzzle, Locations, TransposedLocations, Words, PossiblePuzzle) :-
    permutation(Words, WordsPermutation),
    fill_puzzle(EmptyPuzzle, Locations, WordsPermutation, RemainingWords, FilledPuzzle),
    clpfd:transpose(FilledPuzzle, TransposedPuzzle),
    fill_puzzle(TransposedPuzzle, TransposedLocations, RemainingWords, UnusedWords, FilledTransposedPuzzle),
    UnusedWords == [],
    clpfd:transpose(FilledTransposedPuzzle, PossiblePuzzle).



fill_puzzle(X,[],Y,Y,X).
fill_puzzle(EmptyPuzzle, [Location|Locations], [Word|Words], UnusedWords, ReplacedPuzzle) :-
    [RowNumber, ColumnNumber] = Location,
    RowNumber >= 0,
    fill_word(EmptyPuzzle, RowNumber, ColumnNumber, Word, FilledPuzzle),
    fill_puzzle(FilledPuzzle, Locations, Words, UnusedWords, ReplacedPuzzle).
    % (fill_word(EmptyPuzzle, RowNumber, ColumnNumber, Word, FilledPuzzle)
    % -> fill_puzzle(FilledPuzzle, Locations, Words, UnusedWords, ReplacedPuzzle)
    % ;  append(Words,[Word], NewWordList),
    %    fill_puzzle(EmptyPuzzle, [Location|Locations], NewWordList, UnusedWords, ReplacedPuzzle)
    % ).
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
    length(Slot, SlotLength), SlotLength \= 0, Index is ColumnNumber-SlotLength.
analyze_row(['#'|Row], Slot, ColumnNumber, RowNumber, [[SlotLength, [RowNumber, Index]]|RowData]) :-
    length(Slot, SlotLength),
    SlotLength \= 0,
    Index is ColumnNumber - SlotLength,
    NextIndex is ColumnNumber+1,
    analyze_row(Row, [], NextIndex, RowNumber, RowData).
analyze_row([H|Row], Slot, ColumnNumber, RowNumber, RowData) :-
    H \= '#',
    append([H], Slot, NewSlot),
    NextIndex is ColumnNumber+1,
    analyze_row(Row, NewSlot, NextIndex, RowNumber, RowData).
analyze_row([_|Row], Slot, ColumnNumber, RowNumber, RowData) :-
    NextIndex is ColumnNumber + 1,
    analyze_row(Row, Slot, NextIndex,RowNumber, RowData).


process_word(WordList, SortedProcessedWordList) :-
    word_length(WordList, WordDataList),
    sort(WordDataList, [H|SortedWordList]),
    [H|SortedWordList] \= [],
    [Length, Word] = H,
    group_data(SortedWordList, 1, [Word], Length, ProcessedWordList),
    sort(ProcessedWordList, SortedProcessedWordList).

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
