% You can use this code to get started with your fillin puzzle solver.
% Make sure you replace this comment with your own documentation.
:- ensure_loaded(library(clpfd)).

main(PuzzleFile, WordlistFile, SolutionFile) :-
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


% solve_puzzle(Puzzle0, WordList, Puzzle)
% should hold when Puzzle is a solved version of Puzzle0, with the
% empty slots filled in with words from WordList.  Puzzle0 and Puzzle
% should be lists of lists of characters (single-character atoms), one
% list per puzzle row.  WordList is also a list of lists of
% characters, one list per word.
%
% This code is obviously wrong: it just gives back the unfilled puzzle
% as result.  You'll need to replace this with a working
% implementation.
solve_puzzle(_,_,FilledPuzzle) :- 
solve_puzzle(Puzzle, _, Puzzle).

% To find words of certain length
% Predicate should terminate when there is no more words to process
words_of_certain_length([], _, []).
words_of_certain_length([Word|WordList], Length, [Word|MatchingWords]) :-
    length(Word, Length),
    words_of_certain_length(WordList, Length, MatchingWords).
words_of_certain_length([_|WordList], Length, MatchingWords) :-
    words_of_certain_length(WordList, Length, MatchingWords).

% fill_puzzle takes in the empty puzzle, Row and Column number, the word to 
% fill and output the filled puzzle
fill_puzzle([RowToReplace|Puzzle], 0, ColumnNumber, Word, [Replaced|Puzzle]) :-
    ColumnNumber >= 0,
    replace_row(RowToReplace, Word, ColumnNumber, Replaced).
fill_puzzle([H|Puzzle], RowNumber, ColumnNumber, Word, [H|FilledPuzzle]) :-
    RowNumber >= 0,
    ColumnNumber >= 0,
    Index is RowNumber-1,
    fill_puzzle(Puzzle, Index, ColumnNumber, Word, FilledPuzzle).  

replace_row(X,[],0,X).
replace_row([H|Row], [W|Word], 0, [W|FilledRow]) :-
    (H=='_' ; H==W),
    replace_row(Row, Word, 0, FilledRow).
replace_row([H|Row], Word, ColumnNumber, [H|FilledRow]) :-
    ColumnNumber >= 0,
    Index is ColumnNumber-1,
    replace_row(Row, Word, Index, FilledRow).

row_is_filled([]).
row_is_filled([H|Row]) :-
    H \= "_",
    row_is_filled(Row).

puzzle_is_filled([]).
puzzle_is_filled([Row|Puzzle]) :-
    row_is_filled(Row),
    puzzle_is_filled(Puzzle).


% unique_length_word(WordList, Length, Word) :-
%     words_of_certain_length(WordList, Length, [Word]),
%     length(Word, Length).
% unique_length_word(WordList, Length, Word) :-
%     unique_length_word(WordList, Length+1, Word).
