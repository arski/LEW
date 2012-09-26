%%
%% TCD Language Evolution Workbench
%%
%% tools.pl
%% Utility functions for Prolog
%%

:- use_module(library(samsort)).
:- use_module(library(random)).
:- use_module(library(lists)).
:- use_module(library(sets)).

%% Merges an array of string into one adding a separator between the strings if one is provided
joinstrings(StrList,OutString) :-
  joinstrings(StrList,'',OutString).
  
joinstrings(StrList,Separator,OutString) :-
  joinstrings(StrList,Separator,[],OutString).

joinstrings([],_,WorkList,OutString) :-
  atom_codes(OutString,WorkList).
  
joinstrings([InElem|InRest],Separator,WorkList,OutString) :-
	name(InElem,InElemUnicode),
	append(WorkList,InElemUnicode,WL1),
	(
		Separator \== '', InRest \== []
	-> 
		name(Separator,SeparatorUnicode),
		append(WL1,SeparatorUnicode,WL2)
	;
		WL1=WL2
	),
	joinstrings(InRest,Separator,WL2,OutString).
	
%% Moves a file to a specified location
os_mv(File,Location) :-
	process_create('/bin/mv',[file(File),file(Location)],[process(PIDint),detached(false)]),
	process_wait(PIDint,_).
	
%% Creates a new directory
os_mkdir(DirPath) :-
	process_create('/bin/mkdir',[DirPath],[process(PIDint),detached(false)]),
	process_wait(PIDint,_).
	
%% Reports data to standard output and also to a logstream if one is defined
report(Data) :-
	report(Data,' ').
report(Data,Separator) :-
	reportreal(Data,Separator),
	true.

%% Used for reporting specific stuff only
reportreal(Data,Separator) :-
	(trace(trace),report_stdout(Data,Separator);true),
	(
		logstream(LogStream),
		nonvar(LogStream)
	->
		report_stream(Data,Separator,LogStream)
	;
		true
	).

%% Reports some data to a stream with a new line at the end of the list
report_stream([H|[]],_,Stream) :-
	write(Stream,H),
	nl(Stream).

report_stream([H|T],Separator,Stream) :-
	write(Stream,H),
	write(Stream,Separator),
	report_stream(T,Separator,Stream).

%% Reports some data to console with a new line at the end of the list
report_stdout([H|[]],_) :-
	write(H),
	nl.

report_stdout([H|T],Separator) :-
	write(H),
	write(Separator),
	report_stdout(T,Separator).

%% Flushes data from both streams
flush_streams :-
	logstream(LogStream),
	flush_output(LogStream),
	dumpstream(Stream),
	flush_output(Stream).
	
%% Counts time elapsed since marked start
elapsedtime(Seconds) :-
	datime(_,_,_,H0,Mi0,S0),
	datime(datime(_,_,_,H1,Mi1,S1)),
	H is (H1 - H0)*60*60,
	Mi is (Mi1 - Mi0)*60,
	S is S1 - S0,
	Seconds is H + Mi + S.

	
%% Counts the number of elements in a list that are not equal to X
count_elements_ne(_,[],0).
count_elements_ne(X,[X|Y],Z) :-
	count_elements_ne(X,Y,Z).

count_elements_ne(X,[_|Y],Z) :-
	count_elements_ne(X,Y,Z1),
	Z is Z1 + 1.
	
%% Returns 1 if the list has at least one element that is not equal to X, otherwise returns 0
has_element_ne(_,[],0).
has_element_ne(X,[X|Y],Z) :-
	has_element_ne(X,Y,Z),!.

has_element_ne(_,_,1).

% Calculate the percentage of X by Y
calc_percentage(Part,Total,Psreal) :-
  (Total \= 0, Ps is Part/Total; Ps = +nan),
	(Ps = +inf, Psreal=1; Ps = +nan, Psreal=0; Psreal=Ps).

% Calculate average over list elements
avg(List,Av) :- 
  avg(List,0,0,Av).
  
avg([],Sum,Count,Av) :-
  Av is Sum/Count.

avg([H|T],Sum,Count,Av) :-
	Sum1 is Sum + H,
	Count1 is Count + 1,
	avg(T,Sum1,Count1,Av).

% Sort a list in reverse order
asort(List,ASortedList) :-
	samsort(List,SortedList),
	reverse(SortedList,ASortedList).

% Get a random element from a list
random_element(List,Element) :-
	length(List,ListLength),
	random(0,ListLength,Index),
	nth0(Index,List,Element).

% Get a random element from a list with weighted entries
random_element_weighted(List,Element) :-
	RoundUp = 1000,
	calculate_total_weight(List,TotalWeight),
	MaxWeight is round(TotalWeight*RoundUp),
	random(0,MaxWeight,SelectWeight),
	select_within_range(List,SelectWeight,RoundUp,Element).

% Calculate the total weight of a weighted list
calculate_total_weight([],0).
calculate_total_weight([[_,Weight]|List],TotalWeight) :-
	calculate_total_weight(List,TotalWeight1),
	TotalWeight is TotalWeight1 + Weight.

% Select an element of a weighted list is within a given range
select_within_range(ItemList,Range,RoundUp,Item) :-
	select_within_range(ItemList,Range,RoundUp,Item,0).

select_within_range([[_,Weight]|Tail],Range,RoundUp,Item,Sum) :-
	NewSum is Sum + round(Weight*RoundUp),
	Range >= NewSum,
	select_within_range(Tail,Range,RoundUp,Item,NewSum).

select_within_range([[Item,_]|_],_,_,Item,_).

% Build up an indexed list from the given weights
weighted_list([],[],_).
weighted_list([Weight|WeightTail],[[Index,Weight]|ListTail],Index) :-
	Index1 is Index + 1,
	weighted_list(WeightTail,ListTail,Index1).

% Generate an N-long list of the same element
n_copies(_,0,[]) :- !.
n_copies(Element,N,[Element|T]) :-
	N > 0,  % why is this caution only now necessary due to backtracking?
	Nleft is N - 1,
	n_copies(Element,Nleft,T).

% A safe, two-way implementation of append/2
append_safe([],[]).
append_safe([L|Ls], List0) :-
	append(L, List1, List0),
	L \== [],
	append_safe(Ls, List1).

% Generate a list of all non-empty partitions of the list
all_partitions(List,Partitions) :-
	findall(Partition,append_safe(Partition,List),Partitions).

% Generate a list of all non-empty partition units of the list
all_partition_units(List,AllPartitionUnits) :-
	forceseg(ForceSeg),
	(
		ForceSeg == 1
	->
		findall(Segment,(proper_segment(List,Segment),Segment\=[]),AllPartitionUnits)
	;
		findall(Segment,(segment(List,Segment),Segment\=[]),AllPartitionUnits)
	).

% Return one random partition of a list
random_partition(List,Partition) :-
	forceseg(ForceSeg),
	random_partition(List,Partition,ForceSeg).

random_partition([],[],_).
random_partition([El|[]],[[El]],_).
random_partition(List,[Prefix|Partition],ForceSeg) :-
	length(List,ListLen),
	% If at least some segmentation is forced, don't allow for all list elements to be in one partition
	RandLim is ListLen + 1 - ForceSeg,
	random(1,RandLim,PrefixLen),
	append_length(Prefix,Suffix,List,PrefixLen),
	random_partition(Suffix,Partition,0).

% Return one random partition of a list that is fully disjunct from the given partition
random_partition_disjunct(List,TargetPartition,Partition) :-
	all_partitions(List,Partitions),
	forceseg(ForceSeg),
	(ForceSeg == 1, delete(Partitions,[List],Partitions1); Partitions1 = Partitions),
	remove_intersecting(Partitions1,TargetPartition,Partitions2),
	random_element(Partitions2,Partition).

% Merges partitions into one flat list
merge_partitions([],[]).
merge_partitions([El|T],List) :-
	merge_partitions(T,List1),
	append(El,List1,List).

% Generate all possible subsets of a set of a particular length
subset_len(_,[],MaxLen,MaxLen) :- !.
subset_len([X|Xs],[X|Ys],MyLen,MaxLen) :-
	NewLen is MyLen + 1,
	NewLen =< MaxLen,
	subset_len(Xs,Ys,NewLen,MaxLen).
subset_len([_|Xs],Ys,MyLen,MaxLen) :-
	subset_len(Xs,Ys,MyLen,MaxLen).

% Initialize some empty distributions
dist_init(0.0,[]).
dist_init(DistCount,[[]|Dists]) :- 
	DistCountLeft is DistCount - 1.0,
	dist_init(DistCountLeft,Dists).

% Place an item inside a distribution that doesn't contain any of its subelements
place_item([],[],[],_).
place_item(Item,[Distribution|Distributions],[[Item|Distribution]|DistributionsNew],MaxDistLen) :-
	\+ Item == [],
	length(Distribution,MyDistLen),
	MyDistLen < MaxDistLen,
	merge_partitions(Distribution,List),
	\+ intersect(Item,List),
	place_item([],Distributions,DistributionsNew,MaxDistLen).

place_item(Item,[Distribution|Distributions],[Distribution|DistributionsNew],MaxDistLen) :-
	place_item(Item,Distributions,DistributionsNew,MaxDistLen).

% Distribute elements into a number of distributions lists without subelement overlap
distribute_len(Sets,Distributions,DistLen) :-
	length(Sets,SetCount),
	DistCount is SetCount/DistLen,
	dist_init(DistCount,EmptyDists),
	distribute_len(Sets,EmptyDists,Distributions,DistLen).

distribute_len([],Distributions,Distributions,_).
distribute_len([Set|Sets],WorkDist,Distributions,DistLen) :-
	place_item(Set,WorkDist,WorkDistNew,DistLen),
	distribute_len(Sets,WorkDistNew,Distributions,DistLen).

% Generate all non-intersecting lists of PLen-subsets of a list
% Eg. divide 6 speakers into 5 rounds of unique pairwise conversations
separate_len(List,Distributions,PartitionLen) :-
	length(List,ListLen),
	PartitionCount is ListLen/PartitionLen,
	findall(Subset,(subset_len(List,Subset,0,PartitionLen)),LenSets),
	distribute_len(LenSets,Distributions,PartitionCount).

% Split a list of pairs into a list of left-sided and right-sided values
split_pairs([],[],[]).
split_pairs([[L,R]|T],[L|LT],[R|RT]) :-
	split_pairs(T,LT,RT).

% Subtracts every element from the 2nd list _once_ from the first list
lists_subtract(A,[],A).
lists_subtract([],_,[]).
lists_subtract([H|T],B,Out) :-
	memberchk(H,B),!,
	delete(B,H,BRest),
	lists_subtract(T,BRest,Out).

lists_subtract([H|T],B,[H|Out]) :-
	lists_subtract(T,B,Out).

% Deletes all occurencies of every element in the 2nd list from the first list
lists_delete(A,[],A).
lists_delete([],_,[]).
lists_delete(A,[H|T],Out) :-
	delete(A,H,ARest),
	lists_delete(ARest,T,Out).

% Inverts a list
invert([],[]).
invert([H|T],B) :-
	invert(T,B2),
	append(B2,[H],B).

% Removes duplicates from a list while preserving the initial order
remove_duplicates([],[]).
remove_duplicates([H|T],[H|Rest]) :-
	delete(T,H,TClean),
	remove_duplicates(TClean,Rest).

% Removes all sublists from a list that intersect with the target list
remove_intersecting([],_,[]).
remove_intersecting([List|InLists],TargetList,OutLists) :-
	intersect(List,TargetList),
	remove_intersecting(InLists,TargetList,OutLists).

remove_intersecting([List|InLists],TargetList,[List|OutLists]) :-
	remove_intersecting(InLists,TargetList,OutLists).


% Negation
not(X) :- X, !, fail.
not(_).
