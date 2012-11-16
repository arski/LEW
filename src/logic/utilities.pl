%%
%% TCD Language Evolution Workbench
%%
%% utilities.pl
%% Utility functions for post-processing simulation results
%%

:- use_module(library(samsort)).
:- use_module(library(sets)).

atleastasused(wc(_,_,C,_),wc(_,_,C1,_)) :-
	C >= C1.

%% Dump the wc data to a file
dump_lexicon(File) :-
	findall([Meaning,Form,Count,Agents],wc(Meaning,Form,Count,Agents),MappingList),
	open(File,write,Stream),
	process_wc(MappingList,Stream),
	close(Stream).

%% Dump the LBs to a file
dump_agent_lexicon(File) :-
	findall([Agent,Group,Meaning,Form,Count],agentwc(Agent,Group,Meaning,Form,Count),MappingList),
	open(File,write,Stream),
	process_agentwc(MappingList,Stream),
	close(Stream).

process_wc([],_).
process_wc([Entry|Rest],Stream) :-
	report_stream(Entry,' ',Stream),
	process_wc(Rest,Stream).

process_agentwc([],_).
process_agentwc([Entry|Rest],Stream) :-
	report_stream(Entry,' ',Stream),
	process_agentwc(Rest,Stream).

	
%% Plot the lexicon convergence graph
gnuplot_lexicon(Plotfile) :- 
	absolute_file_name(Plotfile,Name),
	process_create('/usr/bin/gnuplot',[file(Name)],[process(PIDint),detached(false)]),
	process_wait(PIDint,_).


%% Report all kinds of stats and log them after each epoch
post_process :-
	% Report on re-formulations (should be moved into group-based reporting)
	newforms(NewForms),
	report(['Re-formulations:',NewForms]),
	
	% Report on forgotten forms
	forgotten(Forgotten),
	report(['Forgotten:',Forgotten]),
	
	% Report on the agents and calculate population change stats
	agents(Agents),
	startagents(StartAgents),
	PopulationChange is Agents/StartAgents,
	LogPopulationChange is log(10,PopulationChange),!,
	report(['Population change from the start (log):',LogPopulationChange]),
	
	% Report on innovations (should be moved into group-based reporting)
	innovations(Innovations),
	report(['Innovative forms:',Innovations]),
	
	% Calculate the ratio of homonymous forms
	findall(Form,wc(_,Form,_,_),Forms),
	remove_dups(Forms,UniqueForms),
	length(UniqueForms,UniqueFormCount),
	
	lists_subtract(Forms,UniqueForms,Homonyms),
	remove_dups(Homonyms,HomonymousForms),
	length(HomonymousForms,HomonymousFormCount),

	calc_percentage(HomonymousFormCount,UniqueFormCount,GlobalHomPs),
	report(['Percentage of words with homonymns:',GlobalHomPs]),
	
	% Calculate the ratio of synonymous meanings
	findall(Meaning,wc(Meaning,_,_,_),Meanings),
	remove_dups(Meanings,UniqueMeanings),
	length(UniqueMeanings,UniqueMeaningCount),
	
	lists_subtract(Meanings,UniqueMeanings,Synonyms),
	remove_dups(Synonyms,SynonymousMeanings),
	length(SynonymousMeanings,SynonymousMeaningCount),
	
	calc_percentage(SynonymousMeaningCount,UniqueMeaningCount,GlobalSynPs),
	report(['Percentage of meanings with synonymous names:',GlobalSynPs]),
	
	% Process all agent groups (skip if there is only 1 group)
	setof(G,A^agentgroup(A,G),Groups),
	length(Groups,GroupCount),
	(groupstats(gs_on),GroupCount > 1,post_process_group(Groups);true),
	
	% Now calculate and report overall stats
	report(['--- Overall Summary']),
	
	% Report agent count
	report(['Number of agents:',Agents]),
	
	% Report agent interactions
	findall([GINT1,GINT2,GINTR],groupintratio(GINT1,GINT2,GINTR),GIRatios),
	report(['Group interaction ratios are:',GIRatios]),
	findall([G3,G4,GInf],groupinfluence(G3,G4,GInf),GroupInfluences),
	report(['Group influences are:',GroupInfluences]),
	
	% Report total number of spoken & heard form-meaning mappings
	wordobservations(W),
	report(['Word mapping observations:',W]),
	
	% Report the average understanding rates
	understanding_averages(_,MeaningPrecisionAvg,MeaningRecallAvg,MeaningF1Avg,LBUseAvg,LBPrecisionAvg),
	report(['Average meaning understanding precision:',MeaningPrecisionAvg]),
	report(['Average meaning understanding recall:',MeaningRecallAvg]),
	report(['Average meaning understanding F1:',MeaningF1Avg]),
	report(['Average LB usage rate:',LBUseAvg]),
	report(['Average LB-based decoding correctness:',LBPrecisionAvg]),
	
	% Retrieve all the lexicons
	agentnames(Names),
	agent_lexicons(Names,TotalLBLen,AgentMeanings,AgentForms),
	global_lexicon(GlLBLen,GlMeanings,GlForms),
	
	% Calculate cumulative stats over all agents for hom-/synonymy ratio and forms/meanings
	agent_hom(AgentForms,TotalHomsPs,AgentUniqueForms),
	agent_syn(AgentMeanings,TotalSynsPs,AgentUniqueMeanings),
	
	% Calculate and report average hom-/synonymy ratios over all agents
	AgentHomPs is TotalHomsPs/Agents,
	report(['Average agent lexica homonymy is:',AgentHomPs]),
	AgentSynPs is TotalSynsPs/Agents,
	report(['Average agent lexica synonymy is:',AgentSynPs]),
	
	% Calculate average lexicon size
	(Agents \= 0, AvgLBLen is TotalLBLen/Agents; AvgLBLen is 0.0),
	report(['Average lexicon length is:',AvgLBLen]),
	
	% Calculate average meaning/form count over all agents
	AvgUniqueMeanings is AgentUniqueMeanings/Agents,
	report(['Average expressible meanings:',AvgUniqueMeanings]),
	AvgUniqueForms is AgentUniqueForms/Agents,
	report(['Average interpretable forms:',AvgUniqueForms]),
	
	% Report population lexicon size
	report(['Population lexicon length is:',GlLBLen]),
	
	% Report population unique meaning/form count
	report(['Population unique meanings:',GlMeanings]),
	report(['Population unique forms:',GlForms]),
	
	% Calculate (and report) some stats about LB intersections between agents
	intersect_stats(AvgLBLen,AvgUniqueMeanings,AvgUniqueForms,AvgMappingShare,SharedMappingRatio,SharedMeaningRatio,SharedFormRatio),
	
	% Write all the generated data to the run dump file (including global stats)
	dumpstream(Stream),
	report_stream(['all',Agents,AgentHomPs,AgentSynPs,AvgLBLen,AvgUniqueMeanings,AvgUniqueForms,
	MeaningPrecisionAvg,MeaningRecallAvg,MeaningF1Avg,LBUseAvg,LBPrecisionAvg,W,
	AvgMappingShare,SharedMappingRatio,SharedMeaningRatio,SharedFormRatio,Innovations,NewForms,Forgotten,
	LogPopulationChange,GlobalHomPs,GlobalSynPs,GlLBLen,GlMeanings,GlForms,GIRatios],' ',Stream).


% Calculate stats for groups of agents
post_process_group([]).
post_process_group([Group|Groups]) :-
	report(['--- Group',Group,'Summary']),
	
	% Get the agents of the group
	findall(Agent,agentgroup(Agent,Group),Names),
	length(Names,AgentCount),
	report(['Number of agents:',AgentCount]),
	
	% Report total number of spoken & heard form-meaning mappings
	wordobservations(Group,W),
	report(['Word mapping observations:',W]),
	
	% Report the average understanding rates
	understanding_averages(Group,MeaningPrecisionAvg,MeaningRecallAvg,MeaningF1Avg,LBUseAvg,LBPrecisionAvg),
	report(['Average meaning understanding precision:',MeaningPrecisionAvg]),
	report(['Average meaning understanding recall:',MeaningRecallAvg]),
	report(['Average meaning understanding F1:',MeaningF1Avg]),
	report(['Average LB usage rate:',LBUseAvg]),
	report(['Average LB-based decoding correctness:',LBPrecisionAvg]),
	
	% Retrieve agent lexicon data
	agent_lexicons(Names,TotalLBLen,AgentMeanings,AgentForms),
	
	% Calculate cumulative stats over all agents for hom-/synonymy ratio and forms/meanings
	agent_hom(AgentForms,TotalHomsPs,AgentUniqueForms),
	agent_syn(AgentMeanings,TotalSynsPs,AgentUniqueMeanings),
	
	% Calculate and report average hom-/synonymy ratios over all agents
	AgentHomPs is TotalHomsPs/AgentCount,
	report(['Average agent lexica homonymy is:',AgentHomPs]),
	AgentSynPs is TotalSynsPs/AgentCount,
	report(['Average agent lexica synonymy is:',AgentSynPs]),
	
	% Calculate average lexicon size
	(AgentCount \= 0, AvgLBLen is TotalLBLen/AgentCount; AvgLBLen is 0.0),
	report(['Average lexicon length is:',AvgLBLen]),
	
	% Calculate average meaning/form count over all agents
	AvgUniqueMeanings is AgentUniqueMeanings/AgentCount,
	report(['Average expressible meanings:',AvgUniqueMeanings]),
	AvgUniqueForms is AgentUniqueForms/AgentCount,
	report(['Average interpretable forms:',AvgUniqueForms]),
	
	% Write the group data to the run dump file
	dumpstream(Stream),
	report_stream([Group,AgentCount,AgentHomPs,AgentSynPs,AvgLBLen,AvgUniqueMeanings,AvgUniqueForms,
	MeaningPrecisionAvg,MeaningRecallAvg,MeaningF1Avg,LBUseAvg,LBPrecisionAvg,W],' ',Stream),
	
	% Go on to the next group
	post_process_group(Groups).


%% cmv
%% Generate individual homonymy/synonymy stats
%% - summed count of distinct forms/meanings
%% - summed percentage of words with h/s 

% Generate totals about homonyms
agent_hom([],0,0).
agent_hom([Forms|FormsTail],TotalHomsPs,TotalForms) :-
	% Count the number of unique forms
	remove_dups(Forms,UniqueForms),
	length(UniqueForms,UniqueFormCount),
	% Count the number of homonymous forms
	lists_subtract(Forms,UniqueForms,Homonyms),
	remove_dups(Homonyms,HomonymousForms),
	length(HomonymousForms,HomonymousFormCount),
	% Calculate the percentage and go on with the next agent
	calc_percentage(HomonymousFormCount,UniqueFormCount,HomPs),
	agent_hom(FormsTail,H2,TF2),
	TotalForms is TF2 + UniqueFormCount,
	TotalHomsPs is H2 + HomPs,!.

% Generate totals about synonyms
agent_syn([],0,0).
agent_syn([Meanings|MeaningsTail],TotalSynsPs,TotalMeanings) :-
	% Count the number of unique meanings
	remove_dups(Meanings,UniqueMeanings),
	length(UniqueMeanings,UniqueMeaningCount),
	% Count the number of snynonymous forms
	lists_subtract(Meanings,UniqueMeanings,Synonyms),
	remove_dups(Synonyms,SynonymousMeanings),
	length(SynonymousMeanings,SynonymousMeaningCount),
	% Calculate the percentage and go on with the next agent
	calc_percentage(SynonymousMeaningCount,UniqueMeaningCount,SynPs),
	agent_syn(MeaningsTail,S2,TM2),
	TotalMeanings is TM2 + UniqueMeaningCount,
	TotalSynsPs is S2 + SynPs,!.


%% cmv
%% Calculate (and report) some stats about LB intersections between agents
intersect_stats(AvgLBLen,AvMeanings,AvForms,AvgMappingShare,SharedMappingRatio,SharedMeaningRatio,SharedFormRatio) :-
	% Calculate the average share of all mappings (ignore those that aren't shared by anyone)
	findall(L,(wc(_,_,_,L),L>0),MappingShares),
	length(MappingShares,MappingCount),
	sumlist(MappingShares,MappingShareTotal),
	
	(MappingCount \= 0, AvgMappingShare is MappingShareTotal/MappingCount; AvgMappingShare = 0),
	report(['Average mapping share:',AvgMappingShare]),
	
	% Calculate the intersection, i.e. mappings present in every agent's lexicon
	agents(AgentCount),
	findall([M,P],wc(M,P,_,AgentCount),LBIntersect),
	report(['Mappings shared by everyone:',LBIntersect]),
	length(LBIntersect,LBIntersectLen),
	
	% Calculate the ratio of the lexicon size that is shared among all agents
	(AvgLBLen \= 0.0, SharedMappingRatio is LBIntersectLen/AvgLBLen; SharedMappingRatio = 0),
	report(['Ratio of shared unique mappings to avg no of mappings is:',SharedMappingRatio]),
	
	% Split the shared mappings into meanings and forms and remove the duplicates
	split_pairs(LBIntersect,LBIntersectMeanings,LBIntersectForms),
	remove_dups(LBIntersectMeanings,MeaningIntersect),
	length(MeaningIntersect,MeaningIntersectLen),
	remove_dups(LBIntersectForms,FormIntersect),
	length(FormIntersect,FormIntersectLen),
	
	% Calculate the ratio of meanings shared among all agents
	(AvMeanings \= 0.0, SharedMeaningRatio is MeaningIntersectLen/AvMeanings; SharedMeaningRatio = 0),
	report(['Ratio of shared unique meanings to avg no unique meanings is:',SharedMeaningRatio]),
	
	% Calculate the ratio of forms shared among all agents
	(AvForms \= 0.0, SharedFormRatio is FormIntersectLen/AvForms; SharedFormRatio = 0),
	report(['Ratio of shared unique forms to avg no unique forms is:',SharedFormRatio]).


% Generate a list of LBs for the given agents and calculate the total number of all mappings over all agents
agent_lexicons([],0,[],[]).
agent_lexicons([Name|NameT],TotalLen,[Meanings|MeaningsTail],[Forms|FormsTail]) :-
	% Find all forms and meanings of an agent
	findall(Meaning,agentwc(Name,_,Meaning,_,_),Meanings),
	findall(Form,agentwc(Name,_,_,Form,_),Forms),
	% The above will be added later to the list, go on with next agent
	agent_lexicons(NameT,TailLen,MeaningsTail,FormsTail),
	length(Meanings,LBLen),
	TotalLen is TailLen + LBLen.


% Calculate the number of unique mappings, meanings and forms in the population lexicon
global_lexicon(GlLBLen,GlMeanings,GlForms) :-
	findall([M,F],wc(M,F,_,_),Mappings),
	remove_dups(Mappings,UniqueMappings),
	findall(Meaning,wc(Meaning,_,_,_),Meanings),
	remove_dups(Meanings,UniqueMeanings),
	findall(Form,wc(_,Form,_,_),Forms),
	remove_dups(Forms,UniqueForms),
	length(UniqueMappings,GlLBLen),
	length(UniqueMeanings,GlMeanings),
	length(UniqueForms,GlForms).


%% Calculate understanding averages
understanding_averages(Group,MeaningPrecisionAvg,MeaningRecallAvg,MeaningF1Avg,LBUseAvg,LBPrecisionAvg) :-
	% Get the data
	findall(HI,agentinfo(_,Group,_,HI,_,_,_,_,_,_),HearerInts),
	findall(MP,agentinfo(_,Group,_,_,MP,_,_,_,_,_),MeaningPrecisions),
	findall(MR,agentinfo(_,Group,_,_,_,MR,_,_,_,_),MeaningRecalls),
	findall(MF,agentinfo(_,Group,_,_,_,_,MF,_,_,_),MeaningF1s),
	findall(LBI,agentinfo(_,Group,_,_,_,_,_,LBI,_,_),LBInts),
	findall(LBU,agentinfo(_,Group,_,_,_,_,_,_,LBU,_),LBUses),
	findall(LBP,agentinfo(_,Group,_,_,_,_,_,_,_,LBP),LBPrecisions),
	
	% Calculate sums for the group
	comactions(IntTotal),
	sumlist(HearerInts,HITotal),
	sumlist(MeaningPrecisions,MPTotal),
	sumlist(MeaningRecalls,MRTotal),
	sumlist(MeaningF1s,MFTotal),
	sumlist(LBInts,LBITotal),
	sumlist(LBUses,LBUTotal),
	sumlist(LBPrecisions,LBPTotal),
	
	% Calculate averages
	MeaningPrecisionAvg is MPTotal/IntTotal,
	MeaningRecallAvg is MRTotal/IntTotal,
	MeaningF1Avg is MFTotal/IntTotal,
	LBUseAvg is LBUTotal/HITotal, % LB use is only recorded for hearers!
	(LBITotal \= 0, LBPrecisionAvg is LBPTotal/LBITotal; LBPrecisionAvg = 0.0). % LB precision is only recorded when LB was used and is checkable (i.e. when the form looked up in the hearer's lexicon was actually present in the speaker's utterance)!
