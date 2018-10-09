/*****************************************************************************/
%
% @Author Novan Allanadi, built from the skeleton code provided by the subject
% COMP30020 Semester 2
% @StudentID 917830
% This is a project for the second project of the Declarative Programming 
% Subject, Semester 2
% This program solves a fillin puzzle, a crossword-like puzzle that given, an
% empty puzzle and words, must fit all those words in that empty puzzle.
% the program reads in empty puzzle and words and outputs the filled in puzzle
%
/*****************************************************************************/

% Using the CLPFD library
:- ensure_loaded(library(clpfd)).

/*****************************************************************************/

/**
 * The main entry of the program
 */
main(PuzzleFile, WordlistFile, SolutionFile) :-

    /**
     * reading the necessary files
     */
    read_file(PuzzleFile, Puzzle),
    read_file(WordlistFile, Wordlist),

    /**
     * check puzzle validity
     */
    valid_puzzle(Puzzle),

    /**
     * solving the puzzle and print the output
     */
    solve_puzzle(Puzzle, Wordlist, Solved),
    print_puzzle(SolutionFile, Solved).

/*****************************************************************************/

/**
 * reads file 
 */
read_file(Filename, Content) :-
    open(Filename, read, Stream),
    read_lines(Stream, Content),
    close(Stream).

/*****************************************************************************/

/**
 * go through the file
 */
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

/*****************************************************************************/

/**
 * reads a line of a file
 */
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

/*****************************************************************************/

/**
 * outputting the puzzle
 */
print_puzzle(SolutionFile, Puzzle) :-
    open(SolutionFile, write, Stream),
    maplist(print_row(Stream), Puzzle),
    close(Stream).

/*****************************************************************************/

/**
 * prints the puzzle row
 */
print_row(Stream, Row) :-
    maplist(put_puzzle_char(Stream), Row),
    nl(Stream).

/*****************************************************************************/

/**
 * printing a character
 */
put_puzzle_char(Stream, Char) :-
    (   var(Char)
    ->  put_char(Stream, '_')
    ;   put_char(Stream, Char)
    ).

/*****************************************************************************/

/**
 * checks validity of a puzzle
 */
valid_puzzle([]).
valid_puzzle([Row|Rows]) :-
    maplist(same_length(Row), Rows).

/*****************************************************************************/

/**
 * solve_puzzle takes in the empty puzzle, the list of words and outputs the
 * correctly filled puzzle
 */
solve_puzzle(EmptyPuzzle, WordList, FilledPuzzle) :-
    /**
     * processing the empty puzzle and transposed empty puzzle to a list of
     * grouped locations based on length
     */
    process_empty_puzzle(EmptyPuzzle, ProcessedEmptyPuzzle),
    clpfd:transpose(EmptyPuzzle, TransposedPuzzle),
    process_empty_puzzle(TransposedPuzzle, ProcessedTransposedPuzzle),

    /**
     * process the words into a list of group words based on length
     */
    process_word(WordList, ProcessedWordData),

    /**
     * process the puzzle
     */
    process_puzzle(EmptyPuzzle, ProcessedEmptyPuzzle, ProcessedTransposedPuzzle
        , ProcessedWordData, FilledPuzzle).

/*****************************************************************************/

/**
 * process_puzzle takes in an empty puzzle, list of locations both from normal
 * puzzle and transposed puzzle, list of processed word list, and output the 
 * complete puzzle
 */

/**
 * predicate terminates when all locations are filled
 */
process_puzzle(CompletePuzzle,_,_,[],CompletePuzzle).

process_puzzle(PuzzleToFill, Locations, TransposedLocations, 
    [WordData|ProcessedWordList], FilledPuzzle) :-

    /**
     * unpacking the processed data of wordlist
     */
    [_, WordLength, Words]=WordData,
    
    /**
     * finding the locations of the normal puzzle and the tranposed puzzle
     * based on the length
     */
    find_data_of_length(WordLength, Locations, MatchingLocations),
    find_data_of_length(WordLength, TransposedLocations, 
        TransposedMatchingLocations),
    
    /**
     * Outputting the possible puzzle based on the filled puzzle and next
     * words to fill, will backtrack should a location can not be filled
     */
    possible_puzzle(PuzzleToFill, MatchingLocations, 
        TransposedMatchingLocations, Words, PossiblePuzzle),

    /**
     * recursively process the next set of words
     */
    process_puzzle(PossiblePuzzle, Locations, TransposedLocations, 
        ProcessedWordList, FilledPuzzle).

/*****************************************************************************/

/**
 * possible_puzzle takes in empty puzzle, locations of normal and transposed
 * puzzle based on the length of words given, a wordlist of certain length, and
 * outputs a possible puzzle
 */
possible_puzzle(EmptyPuzzle, Locations, TransposedLocations, Words, 
    PossiblePuzzle) :-
    
    /**
     * calculating the length of each locations
     */
    length(Locations, Amount1),
    length(TransposedLocations, Amount2),

    /**
     * prioritized to process locations with less locations
     */
    (Amount1 =< Amount2

       /**
        * Taking a number of combinations of words based on the amount, and
        * try to match all the words, should not all words can be matched,
        * it will backtrack to generate next combination
        */
    -> combination(Amount1, Words, WordsCombination),
       all_words_matched(EmptyPuzzle, WordsCombination, Locations, 
           UpdatedPuzzle),
        
       /**
        * Calculating the remaining words, and process the tranposed puzzle
        * by matching all the remaining words
        */
       subtract(Words, WordsCombination, RemainingWords),
       clpfd:transpose(UpdatedPuzzle, TransposedPuzzle),
       all_words_matched(TransposedPuzzle, RemainingWords, TransposedLocations, 
           UpdatedTransposedPuzzle),

       /**
        * Transforming the puzzle back
        */
       clpdf:transpose(UpdatedTransposedPuzzle, PossiblePuzzle)


       /**
        * Processing the transposed puzzle first, taking a combination of words
        * and matching all that words
        */
    ;  combination(Amount2, Words, WordsCombination),
       clpfd:transpose(EmptyPuzzle, TransposedPuzzle),
       all_words_matched(TransposedPuzzle, WordsCombination, 
           TransposedLocations, UpdatedPuzzle),

       /**
        * matching the remaining words
        */
       subtract(Words, WordsCombination, RemainingWords),
       clpfd:transpose(UpdatedPuzzle, NormalFilledPuzzle),
       all_words_matched(NormalFilledPuzzle, RemainingWords, Locations, 
           PossiblePuzzle)
    ).

/*****************************************************************************/

/**
 * all_words_matched takes in the puzzle to fill, a list of words, a list of 
 * locations, and output a puzzle where all the words are filled into the
 * given locations
 */

/**
 * The predicate terminates should the loactions and words are empty
 */
all_words_matched(X,[],[],X).

all_words_matched(Puzzle, Words, [Location|Locations], PossiblePuzzle) :-
    /**
     * trying the all the words in the location
     */
    try_word(Puzzle, Location, Words, WordToRemove, UpdatedPuzzle),

    /**
     * removing the matched words from the Wordlist
     */
    delete(Words, WordToRemove, UpdatedWordList),

    /**
     * recursively try the next location
     */
    all_words_matched(UpdatedPuzzle, UpdatedWordList, Locations, 
        PossiblePuzzle).

/*****************************************************************************/

/**
 * try_word takes in the row number and column number of a location, a list of
 * words, and outputs a word that matched in that location and an updated 
 * puzzle
 */

/**
 * should a word match in that location, it will terminate and outputting the
 * matched words and updated puzzle
 */
try_word([Row|Puzzle], [0, ColumnNumber], [MatchingWord|_], MatchingWord, 
    [ReplacedRow|Puzzle]) :-
    replace_row(Row, MatchingWord, ColumnNumber, ReplacedRow).

/**
 * trying the next word should it does not match the location
 */
try_word(Puzzle, [0, ColumnNumber], [_|Words], MatchingWord, UpdatedPuzzle) :-
    try_word(Puzzle, [0, ColumnNumber], Words, MatchingWord, UpdatedPuzzle).

/**
 * recursively adding the ignored rows in the updated puzzle and iterates to
 * the determined row
 */
try_word([Row|Puzzle], Location, Words, MatchingWord, [Row|UpdatedPuzzle]) :-
    [RowNumber, ColumnNumber]=Location,
    NextRow is RowNumber-1,
    NextRow >= 0,
    try_word(Puzzle, [NextRow,ColumnNumber], Words, MatchingWord, 
        UpdatedPuzzle).

/*****************************************************************************/

/**
 * process_empty_puzzle takes in empty puzzle and process them into a list of
 * list of Number of Locations, Length Category, and all Locations, a location
 * is represented as a two element list of Row Number and Column Number
 */
process_empty_puzzle(EmptyPuzzle, GroupedPuzzleData) :-

    /**
     * processing the empty puzzle into a list of length and location of that
     * slot
     */
    puzzle_data(EmptyPuzzle, 0, [], PuzzleData),

    /**
     * sorting the processed data
     */
    sort(PuzzleData, [H|SortedPuzzleData]),

    /**
     * grouping the data based on the length of the slots
     */
    [Length, Location]=H,
    group_data(SortedPuzzleData, 1, [Location], Length, GroupedPuzzleData).

/*****************************************************************************/

/**
 * find_data_of_length takes is a length, the list of processed data, and 
 * outputs the bound data of that length
 */

/**
 * predicate terminates should if the data is not found
 */
find_data_of_length(_,[],[]).

/**
 * predicate terminates if the data of that length is found and outputs the
 * bound data
 */
find_data_of_length(Length, [[_, Length, BoundDatas]|_], BoundDatas).

/**
 * recursively going through the list of processed data
 */
find_data_of_length(Length, [_|ProcessedDatas], BoundDatas) :-
    find_data_of_length(Length, ProcessedDatas, BoundDatas).

/*****************************************************************************/

/**
 * replace_row takes in the row to replace, the word to fill in that row, the
 * starting index of where to start filling the word, and outputs the replaced
 * row
 */

/**
 * terminates when finish recursively iterating through the row, and finishing
 * putting all characters of the word
 */
replace_row([], [], 0, []).

/**
 * terminates when finding a #
 */
replace_row([H|Row], [], 0, [H|Row]) :-
    H=='#'.

/**
 * start replacing when finish recursively iterate through the row, and only
 * fill the character should it be to an empty slot or a slot with the same
 * character
 */
replace_row([H|Row], [W|Word], 0, [W|FilledRow]) :-
    (   H=='_'
    ;   H==W
    ),
    replace_row(Row, Word, 0, FilledRow).

/**
 * recursively iterating through the row to the correct starting slot
 */
replace_row([H|Row], Word, ColumnNumber, [H|FilledRow]) :-
    ColumnNumber>=0,
    Index is ColumnNumber-1,
    replace_row(Row, Word, Index, FilledRow).

/*****************************************************************************/

/**
 * puzzle_data takes in a puzzle to process, starting row number, initially
 * empty list but it is data to append to the output, and outputs  a list of 
 * starting indexes of a slot and length of that slot
 */

/**
 * predicate terminates when there is no longer any row to analyze
 */
puzzle_data([],_,X,X).

/**
 * recursively iterate through the rows of the puzzle and analyze the row
 */
puzzle_data([Row|EmptyPuzzle], RowNumber, DataToAppend, PuzzleData) :-

    /**
     * getting a list of starting index and length of slot of that row
     * and appending them to the output
     */
    analyze_row(Row, [], 0, RowNumber, RowData),
    append(DataToAppend, RowData, UpdatedData),

    /**
     * iterate to the next row
     */
    NextRow is RowNumber+1,
    puzzle_data(EmptyPuzzle, NextRow, UpdatedData, PuzzleData).

/*****************************************************************************/

/**
 * analyze_row takes in a row of a puzzle, initially empty slot but it stores
 * the currently processed slot, current row number, and it outputs a list of
 * list of length of the slot and its location
 */

/**
 * predicate terminate when there is no longer any slot to process
 */
analyze_row([],[],_, _,[]).

/**
 * terminates when there is no more slot to process, and it puts in the last
 * slot to the output
 */
analyze_row([], Slot, ColumnNumber, RowNumber, [[SlotLength, [RowNumber, Index]]]) :-
    /**
     * # can not be a member of the slot any time
     */
    \+ memberchk('#', Slot),
    length(Slot, SlotLength), SlotLength \= 0, 
    Index is ColumnNumber-SlotLength.

/**
 * should it find a #, it will processed the slot and restart the process again
 */
analyze_row(['#'|Row], Slot, ColumnNumber, RowNumber, 
    [[SlotLength, [RowNumber, Index]]|RowData]) :-
    \+ memberchk('#', Slot),
    length(Slot, SlotLength),
    SlotLength \= 0,
    Index is ColumnNumber - SlotLength,
    NextIndex is ColumnNumber+1,
    analyze_row(Row, [], NextIndex, RowNumber, RowData).

/**
 * keep adding valid character to the slot and recursively iterate to the next
 * character
 */
analyze_row([H|Row], Slot, ColumnNumber, RowNumber, RowData) :-
    H \= '#',
    \+ memberchk('#', Slot),
    append([H], Slot, NewSlot),
    NextIndex is ColumnNumber+1,
    analyze_row(Row, NewSlot, NextIndex, RowNumber, RowData).

/**
 * will only happen if there is multiple #, this is done so that # is not
 * appended into the currently processed slot
 */
analyze_row([_|Row], Slot, ColumnNumber, RowNumber, RowData) :-
    \+ memberchk('#', Slot),
    NextIndex is ColumnNumber + 1,
    analyze_row(Row, Slot, NextIndex,RowNumber, RowData).

/*****************************************************************************/

/**
 * process_word takes in wordlist and outputs a processed word list which is a
 * list of list of Number of words, group length of the words, and the group
 * of words, which is sorted based on the number of words
 */
process_word(WordList, SortedProcessedWordList) :-

    /**
     * processing all the words into a list of list of word and length of that
     * word
     */
    word_length(WordList, WordDataList),

    /**
     * sorting the processed word list based on the length
     */
    sort(WordDataList, [H|SortedWordList]),

    /**
     * grouping based on the length of the words and sorting based on the 
     * number of words and length of that word
     */
    [H|SortedWordList] \= [],
    [Length, Word] = H,
    group_data(SortedWordList, 1, [Word], Length, ProcessedWordList),
    quicksort_word_data(ProcessedWordList, SortedProcessedWordList).

/*****************************************************************************/

/**
 * quicksort_word_data takes is processed word data and outputs a sorted list
 * of processed word data
 */

/**
 * predicate terminates when there is no more pivot
 */
quicksort_word_data([],[]).

/**
 * picking a pivot and recursively quicksort the left side and right side of
 * the pivot
 */
quicksort_word_data([ProcessedWord|ProcessedWordList],Sorted) :-
    pivot_word(ProcessedWordList,ProcessedWord,Left,Right),
    quicksort_word_data(Left,SortedLeft),
    quicksort_word_data(Right,SortedRight),
    append(SortedLeft,[ProcessedWord|SortedRight],Sorted).

/*****************************************************************************/

/**
 * pivot_word takes in a list without the pivot, the pivot and outputs the 
 * left side of the pivot and the right side of the pivot
 */

/**
 * predicate terminates when the list is empty
 */
pivot_word([],_,[],[]).

/**
 * should number of words is smaller than the number of words of the pivot or
 * if they are the same and the length of the words is larger than the length
 * of the words of the pivot it will be put in the left side of the pivot
 */
pivot_word([ProcessedWord|ProcessedWordList],WordPivot,[ProcessedWord|Left],
    Right) :-
    [Amount1, Length1, _] = ProcessedWord,
    [Amount2, Length2, _] = WordPivot,
    (Amount1 < Amount2 
    ; Amount1 == Amount2, Length1 > Length2), 
    pivot_word(ProcessedWordList,WordPivot,Left,Right).

/**
 * should a number of words is larger than the number of the pivot, it will
 * be put on the right side
 */
pivot_word([ProcessedWord|ProcessedWordList],WordPivot,Left,
    [ProcessedWord|Right]) :-
    [Amount1, _, _] = ProcessedWord,
    [Amount2, _, _] = WordPivot,
    Amount1 > Amount2, pivot_word(ProcessedWordList,WordPivot,Left,Right).

/*****************************************************************************/

/**
 * grouping_data takes in a sorted list of processed data, whether it is word 
 * or slots, initally 1 but recursively incremented as the group increases, 
 * initially empty list but recursively appended to process the currently 
 * processed group, current category of the group that is being processed,
 * and outputs a grouped list of list number of data, length of that data,
 * and the bound data
 */

/**
 * predicate terminates when there is no more data process
 */
group_data([], Amount, CurrentGroup, CurrentCategory,
     [[Amount, CurrentCategory, CurrentGroup]]).

/**
 * Should it meet a data of different category, it will replace the current
 * category, currently processed group, and append the previously processed
 * data to the output
 */
group_data([Data|DataList], Amount, CurrentGroup, CurrentCategory, 
    [[Amount, CurrentCategory, CurrentGroup]| GroupedData]) :-
    [Length, DataToGroup]=Data,
    Length \= CurrentCategory,
    group_data(DataList, 1, [DataToGroup], Length, GroupedData).

/**
 * recursively append data of the same category to the currently processed 
 * group, and process the next data
 */
group_data([Data|DataList], Amount, CurrentGroup, CurrentCategory, 
    GroupedData) :-
    [Length, DataToGroup]=Data,
    Length == CurrentCategory,
    NewAmount is Amount+1,
    append(CurrentGroup, [DataToGroup], NewCurrentGroup),
    group_data(DataList, NewAmount, NewCurrentGroup, CurrentCategory,
        GroupedData).

/*****************************************************************************/

/**
 * word_length takes in a list of words and output a list of word and length of
 * that word
 */

/**
 * terminates when there is no more word to process
 */
word_length([],[]).

/**
 * calculate the length, append the length and the word to the output, and
 * iterates to the next word
 */
word_length([Word|WordList], [[Length, Word] | ProcessedWord]) :-
    length(Word, Length),
    word_length(WordList, ProcessedWord).

/*****************************************************************************/

/**
 * combination takes a number n, a list, and outputs a list of n that is a 
 * subset of the list
 */

/**
 * predicate terminates when the limit of data to append is reached
 */
combination(0,_,[]).

/**
 * append the data to the output and iterate to the next data
 */
combination(N,[Data|Datas],[Data|Combination]) :-
    N>0, 
    NextAmound is N-1,
    combination(NextAmound,Datas,Combination).

/**
 * to generate the next combination of data through backtracking
 */
combination(N,[_|Datas],Combination) :-
    N>0,
    combination(N,Datas,Combination).

/*****************************************************************************/