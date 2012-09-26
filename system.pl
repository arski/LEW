%% (c) 1999-2011 C. Vogel
%%
%% C. Vogel, M. Burke, E. Maguire, J. Woods
%% M. Buckley, M. Flanagan, J. Kelly, C. O'Sullivan, S. Ryan
%%
%% Currently responsible: M. Bachwerk
%%
%% TCD Language Evolution Workbench
%%
%% system.pl
%% Main system code
%%

%% Dynamic variables
:- dynamic comactions/1, wordobservations/1, wordobservations/2, innovations/1, uid/1, agentutt/5, agentwc/5, wc/4, wordindex/3, phonemeindex/2, phonemetotal/1, event/3, eventtypes/1, eventcount/1, eventinstances/1, entitycount/1.

:- dynamic groupindex/1, groupsizeratio/2, groupspeakratio/2, groupintratio/3, groupinfluence/3, groupsize/2, agentinfo/9, agentgroup/2, agentindex/1, agentnames/1, speakers/1, agents/1, startagents/1, overhearers/1, overhearersuccess/1, agentadd/1, newagents/1, agentdel/1, speakerselect/1, speakerround/1, hearerselect/1, selftalk/1, interactiontype/1.

:- dynamic success/1, forgotten/1, newforms/1, questions/1.

:- dynamic asksyn/1, segsync/1, interprule/1, selfsegsync/1, selfinterprule/1, tendency/1, interpeventrole/1, minimumunderstanding/1, datime/6, zrank/3, zipfian/1, randomform/1, forceseg/1, lexupdate/1, lexupdatethres/1, lexupdaterate/1, intratioupdate/1, intratioupdatethres/1, intratioupdaterate/1, influenceupdate/1, influenceupdatethres/1, influenceupdaterate/1, fitness/1, minfitness/1, agerange/1, fgtfunction/1, fgtfactor/1, fgtoffset/1.

:- dynamic epochspan/1, epochfactor/1, statspan/1, groupstats/1, dumpstream/1, logstream/1, trace/1.

%% Sicstus libraries
:- use_module(library(gauge)).
:- use_module(library(random)).
:- use_module(library(lists)).
:- use_module(library(system)).
:- use_module(library(file_systems)).
:- use_module(library(process)).
:- use_module(library(sets)).

%% LEW modules
:- compile(utilities).
:- compile(tools).


%%%%%%%%%%
%% Runners
%%%%%%%%%%
%% General runner with a re-initialization
doall(Agents,SpeakerSelect,HearerSelect,SelfTalk,InteractionType,GroupSizeRatios,GroupSpeakRatios,GroupIntRatios,InfluenceRatios,
	OverHearers,OverHearerSuccess,Phonemes,Entities,EventTypes,Events,EventInstances,Epochs,EpochFactor,EpochSpan,StatSpan,GroupStats,FgtFunction,FgtFactor,FgtOffset,
	SuccessMatters,MinSuccess,LexUpdate,LexUpdateThres,LexUpdateRate,IntRatioUpdate,IntRatioUpdateThres,IntRatioUpdateRate,
	InfluenceUpdate,InfluenceUpdateThres,InfluenceUpdateRate,
	Questions,AskSyn,SegSync,InterpRule,SelfSegSync,SelfInterpRule,MemTendency,InterpEventRole,Zipfism,
	AgentAdd,NewAgents,AgentDel,Fit,FitThres,AgeRange,RandomForm,ForceSeg,RunDump,RunLog,SubDumpPath,PrintTrace) :-
	% Open streams for writing
	((nonvar(RunDump),open(RunDump,write,DumpStream));true),
	((nonvar(RunLog),open(RunLog,write,LogStream));true),
	% Write the used settings down in the log
	report_stream([Agents,SpeakerSelect,HearerSelect,SelfTalk,InteractionType,GroupSizeRatios,GroupSpeakRatios,GroupIntRatios,InfluenceRatios,
	              OverHearers,OverHearerSuccess,Phonemes,Entities,EventTypes,Events,EventInstances,Epochs,EpochFactor,EpochSpan,StatSpan,GroupStats,FgtFunction,FgtFactor,FgtOffset,
	              SuccessMatters,MinSuccess,LexUpdate,LexUpdateThres,LexUpdateRate,IntRatioUpdate,IntRatioUpdateThres,IntRatioUpdateRate,
								InfluenceUpdate,InfluenceUpdateThres,InfluenceUpdateRate,
	              Questions,AskSyn,SegSync,InterpRule,SelfSegSync,SelfInterpRule,MemTendency,InterpEventRole,Zipfism,
	              AgentAdd,NewAgents,AgentDel,Fit,FitThres,AgeRange,RandomForm,ForceSeg],';',LogStream),
	% Run the simulation
	init(Agents,SpeakerSelect,HearerSelect,SelfTalk,InteractionType,GroupSizeRatios,GroupSpeakRatios,GroupIntRatios,InfluenceRatios,
			OverHearers,OverHearerSuccess,Phonemes,Entities,EventTypes,Events,EventInstances,EpochFactor,EpochSpan,StatSpan,GroupStats,FgtFunction,FgtFactor,FgtOffset,
			SuccessMatters,MinSuccess,LexUpdate,LexUpdateThres,LexUpdateRate,IntRatioUpdate,IntRatioUpdateThres,IntRatioUpdateRate,
			InfluenceUpdate,InfluenceUpdateThres,InfluenceUpdateRate,
			Questions,AskSyn,SegSync,InterpRule,SelfSegSync,SelfInterpRule,MemTendency,InterpEventRole,Zipfism,
			AgentAdd,NewAgents,AgentDel,Fit,FitThres,AgeRange,RandomForm,ForceSeg,DumpStream,LogStream,PrintTrace),
	domore(Epochs,SubDumpPath),
	elapsedtime(Seconds),report(['Seconds Elapsed:',Seconds]),
	% Close streams
	((nonvar(RunDump),close(DumpStream));true),
	((nonvar(RunLog),close(LogStream));true).

%% Iterative runner	
domore(0,_).
domore(Epochs,WriteDir) :- 
	% We're here
	report(['Iteration:',Epochs]),
	epochspan(EpochSpan),
	statspan(StatSpan),
	
	% Perform some agent operations at the beginning of an epoch span
	(
		NewEpochIn is Epochs mod EpochSpan,
		NewEpochIn == 0
	->
		% Report some agent-group related stuff
		findall([A,G],agentgroup(A,G),AgentGroups),
		report(['Agents are divided into following groups:',AgentGroups]),
		findall([G1,Size],groupsize(G1,Size),GroupSizes),
		report(['Group sizes are:',GroupSizes]),
		% (Re-)set group interaction rates if interlocutors pairs should be alterated
		(
			hearerselect(hs_turns)
		->
			findall(GroupIdx,groupsize(GroupIdx,_),Groups),
			separate_len(Groups,Distributions,2),
			length(Distributions,DistCount),
			DistIdx is Epochs//EpochSpan mod DistCount,
			nth0(DistIdx,Distributions,Pairings),
			report(['New pairings:', Pairings]),
			gen_group_interactions(Pairings,DistCount,NewGroupIntRatios),
			reset_group_interactions(NewGroupIntRatios)
		;
			true
		)
	;
		true
	),!,
	
	% Simulate some interactions
	speakers(AgentCount),
	agentnames(Names),
	epochfactor(EpochFactor),
	EpochLength is AgentCount*EpochFactor,
	doepoch(EpochLength),
	EpochsLeft is Epochs - 1,
		
	% Check if questions need to be asked
	(
		questions(Questions),
		Questions > 0,
		SkipQuestions is EpochsLeft mod Questions,
		SkipQuestions == 0
	->
		random_element(Names,Agent),
		ask_questions(Agent)
	;
		true
	),
	
	% Perform some actions at the turn of the epoch
	(
		OldEpochOut is EpochsLeft mod EpochSpan,
		OldEpochOut == 0
	->
		% Gradually decay the mapping weights
		forget_decay,
		
		% Remove all mappings with 0-weights
		forget_clean,
		
		% Remove/add agents
		agent_elimination,
		add_agents
	;
		true
	),!,
	
	% Perform some actions at the turn of the epoch
	(
		StatEpochOut is EpochsLeft mod StatSpan,
		StatEpochOut == 0
	->
		% Report a bunch of stats and log some of those
		post_process,
		joinstrings([WriteDir,'epoch','-',Epochs,'.lex'],LexDumpPath),
		dump_lexicon(LexDumpPath),
		joinstrings([WriteDir,'epoch','-',Epochs,'-detailed.lex'],AgentLexDumpPath),
		dump_agent_lexicon(AgentLexDumpPath)
	;
		true
	),!,
	
	% Also perform some post-processing if we ran out of agents and this is the end
	(
		speakers(NewAgentCount),
		NewAgentCount > 0
	->
		EpochsLeftConfirm = EpochsLeft
	;
		EpochsLeftConfirm = 0,
		report(['Ran out of agents']),
		% Report a bunch of stats and log some of those
		post_process,
		joinstrings([WriteDir,'epoch','-',Epochs,'.lex'],LexDumpPath),
		dump_lexicon(LexDumpPath),
		joinstrings([WriteDir,'epoch','-',Epochs,'-detailed.lex'],AgentLexDumpPath),
		dump_agent_lexicon(AgentLexDumpPath)
	;
		true
	),!,
	
	% Clean up a little to avoid RAM overflow
	flush_streams,
	garbage_collect,
	garbage_collect_atoms,!,
	
	% On to next epoch
	domore(EpochsLeftConfirm,WriteDir).

%% cmv
%% Run a single epoch
doepoch(0).
doepoch(InteractionID) :-
	% Select a speaker
	select_speaker(Speaker),
	
	% Select a random event and generate an utterance for the event
	select_event(Event),
	gen_utterance(Speaker,Event,Utterance),
	all_partition_units(Event,EventUnits),
	
	% Run the interaction (returns the MeaningPrecision of the last hearer)
	select_hearers(Speaker,Hearers),
	doint(InteractionID,Speaker,Hearers,Event,Utterance,EventUnits,0,_,[MeaningSuccess|MeaningSuccessTail]),!,
	
	% Update LB only if success was above the minimum understanding rate
	(
		lexupdate(upd_none)
	;
		% Calculate appropriate update value
		(
			lexupdate(upd_stat_fixed),
			UpdateVal = 1
		;
			% Calculate the average precision of all listeners
			length(Hearers,HearerCount),
			sumlist([MeaningSuccess|MeaningSuccessTail],MeaningSuccessSum),
			ReferenceSuccess is MeaningSuccessSum/HearerCount,
			
			% If the threshold is based on past success, retrieve that
			(
				lexupdatethres(success),
				success(LexUpdateThres)
			;
				lexupdatethres(LexUpdateThres)
			),
			
			% Calculate the update value
			(
				lexupdate(upd_dyn_rel),
				UpdateVal is ReferenceSuccess - LexUpdateThres
			;
				(
					ReferenceSuccess < LexUpdateThres,
					UpdateVal = -1
				;
					UpdateVal = 1
				)
			)
		),
		
		% Multiply the update value by the learning rate
		lexupdaterate(LexURate),
		LexUpdateVal is UpdateVal*LexURate,
		
		% Update the weights for the experienced mappings
	  update_lb(Speaker,Utterance,LexUpdateVal),
	
	  % Record the utterances
	  record_utt(Speaker,Utterance)
	),!,
	
	% Re-run the interaction for overhearers
	overhearers(OverHearers),
	(
	  OverHearers == 0
	;
	  select_overhearer(OverHearer),
	  doint(InteractionID,Speaker,[OverHearer],Event,Utterance,EventUnits,1,MeaningSuccess,_)
	),
	
	% Go on to next epoch
	InteractionsLeft is InteractionID - 1,
	doepoch(InteractionsLeft).


%% Perform an interaction between two agents
doint(_,_,[],_,_,_,_,_,[]).
doint(InteractionID,Speaker,[Hearer|HearersT],Event,Utterance,EventUnits,OH,HearerSuccess,[MeaningSuccess|MeaningSuccessTail]) :-
  % Interprete the utterance by the hearer
	(
		Hearer == Speaker
	->
		selfsegsync(SegSync),
		selfinterprule(InterpRule)
	;
		segsync(SegSync),
		interprule(InterpRule)
	),
	
	interp(Speaker,Hearer,SegSync,InterpRule,Utterance,Event,EventUnits,FinalUtterance,Heard,LBUseBitsInv,NewForms),!,
	
  % Update the re-formulation stats
  retract(newforms(NFOld)),
  NFNew is NFOld + NewForms,
  assert(newforms(NFNew)),
	
	% Calculate understanding ratio
	rev(LBUseBitsInv,LBUseBits),
	understanding_ratio(FinalUtterance,Heard,MeaningHits,LBUseBits,LBUsed,LBCheckable,LBHits),
	
	% Calculate precision, recall and success (F1 measure)
	length(Utterance,UttLen),
	length(Heard,HeardLen),
	MeaningPrecision is MeaningHits/HeardLen,
	MeaningRecall is MeaningHits/UttLen,
	(MeaningPrecision+MeaningRecall > 0, MeaningSuccess is 2*(MeaningPrecision*MeaningRecall)/(MeaningPrecision+MeaningRecall);MeaningSuccess=0),
	
	% Calculate the use of LB mappings during decoding
	LBUse is LBUsed/HeardLen,
	
	% Calculate the precision of LB-based decoding
	(
		LBCheckable > 0,
		LBInt = 1,
		LBPrecision is LBHits/LBCheckable
	;
		LBInt = 0,
		LBPrecision = 0
	),
	
	% Update the agent interaction stats
	(OH == 0,update_agent_info(Speaker,1,0,MeaningPrecision,MeaningRecall,MeaningSuccess,0,0,0);true),
	update_agent_info(Hearer,0,1,MeaningPrecision,MeaningRecall,MeaningSuccess,LBInt,LBUse,LBPrecision),
	
	% Determine which precision to use for recording
	(
		(OH == 0;overhearersuccess(ohs_self)),
		ReferenceSuccess = MeaningSuccess
	;
		ReferenceSuccess = HearerSuccess
	),
	
	% Retrieve agent groups
	agentgroup(Speaker,SGroup),
	agentgroup(Hearer,HGroup),
	
	% Update LB only if success was above the minimum understanding rate
	(
		lexupdate(upd_none)
	;
		% Calculate appropriate update value
		(
			lexupdate(upd_stat_fixed),
			UpdateVal = 1
		;
			% If the threshold is based on past success, retrieve that
			(
				lexupdatethres(success),
				success(LexUpdateThres)
			;
				lexupdatethres(LexUpdateThres)
			),
			
			% Calculate the update value
			(
				lexupdate(upd_dyn_rel),
				UpdateVal is ReferenceSuccess - LexUpdateThres
			;
				(
					ReferenceSuccess < LexUpdateThres,
					UpdateVal = -1
				;
					UpdateVal = 1
				)
			)
		),
		
		% Multiply the update value by the learning rate
		lexupdaterate(LexURate),
		LexUpdateVal is UpdateVal*LexURate,
		
		% Calculate the hearer's update value based on speaker's influence
		groupinfluence(SGroup,HGroup,HearerInfluence),
		HearerLexUpdateVal is LexUpdateVal*HearerInfluence,
		
		% Update the weights for the experienced mappings
		update_lb(Hearer,Heard,HearerLexUpdateVal),
		
		% Record the utterances
		record_utt(Hearer,Heard)
	),!,
	
	% Update interaction ratio values (decided by the speaker only for now)
	(
		(intratioupdate(upd_none);OH == 1)
	;
		update_intratio(SGroup,HGroup,ReferenceSuccess)
	),
	
	% Update influence values (decided by the hearer)
	(
		influenceupdate(upd_none)
	;
		update_influence(SGroup,HGroup,ReferenceSuccess)
	),!,
	
	% Clean up a little to avoid RAM overflow
	% garbage_collect_atoms,!,
	
	% Report some stats and go to next interaction
	report([InteractionID,OH,Speaker,SGroup,Hearer,HGroup,Event,Utterance,FinalUtterance,Heard,
					MeaningPrecision,MeaningRecall,MeaningSuccess,LBUse,LBPrecision,NewForms],';'),
	
	% Proceed with the rest of hearers
	doint(InteractionID,Speaker,HearersT,Event,Utterance,EventUnits,OH,HearerSuccess,MeaningSuccessTail).


%%%%%%%%%%
%% Initializer
%%%%%%%%%%
init(Agents,SpeakerSelect,HearerSelect,SelfTalk,InteractionType,GroupSizeRatios,GroupSpeakRatios,GroupIntRatios,InfluenceRatios,
		OverHearers,OverHearerSuccess,Phonemes,Entities,EventTypes,Events,EventInstances,EpochFactor,EpochSpan,StatSpan,GroupStats,FgtFunction,FgtFactor,FgtOffset,
		SuccessMatters,MinSuccess,LexUpdate,LexUpdateThres,LexUpdateRate,IntRatioUpdate,IntRatioUpdateThres,IntRatioUpdateRate,
		InfluenceUpdate,InfluenceUpdateThres,InfluenceUpdateRate,
		Questions,AskSyn,SegSync,InterpRule,SelfSegSync,SelfInterpRule,MemTendency,InterpEventRole,Zipfism,
		AgentAdd,NewAgents,AgentDel,Fit,FitThres,AgeRange,RandomForm,ForceSeg,DumpStream,LogStream,Trace) :- 
	% Clean up everything from a possible previous run
	retractall(agentutt(_,_,_,_,_)),
	retractall(lexupdate(_)),
	retractall(lexupdatethres(_)),
	retractall(lexupdaterate(_)),
	retractall(intratioupdate(_)),
	retractall(intratioupdatethres(_)),
	retractall(intratioupdaterate(_)),
	retractall(influenceupdate(_)),
	retractall(influenceupdatethres(_)),
	retractall(influenceupdaterate(_)),
	retractall(comactions(_)),
	retractall(wordobservations(_)),
	retractall(wordobservations(_,_)),
	retractall(wordindex(_,_,_)),
	retractall(uid(_)),
	retractall(wc(_,_,_,_)),
	retractall(agentwc(_,_,_,_,_)),
	retractall(success(_)),
	retractall(newforms(_)),
	retractall(forgotten(_)),
	retractall(innovations(_)),
	retractall(agentinfo(_,_,_,_,_,_,_,_,_,_)),
	retractall(phonemeindex(_,_)),
	retractall(phonemetotal(_)),
	retractall(event(_,_,_)),
	retractall(eventtypes(_)),
	retractall(eventcount(_)),
	retractall(eventinstances(_)),
	retractall(entitycount(_)),
	retractall(zrank(_,_,_)),
	retractall(speakers(_)),
	retractall(agents(_)),
	retractall(startagents(_)),
	retractall(agentnames(_)),
	retractall(agentindex(_)),
	retractall(agentgroup(_,_)),
	retractall(groupindex(_)),
	retractall(groupsizeratio(_,_)),
	retractall(groupspeakratio(_,_)),
	retractall(groupintratio(_,_,_)),
	retractall(groupinfluence(_,_,_)),
	retractall(groupsize(_,_)),
	retractall(overhearers(_)),
	retractall(overhearersuccess(_)),
	retractall(questions(_)),
	retractall(asksyn(_)),
	retractall(segsync(_)),
	retractall(interprule(_)),
	retractall(selfsegsync(_)),
	retractall(selfinterprule(_)),
	retractall(tendency(_)),
	retractall(interpeventrole(_)),
	retractall(zipfian(_)),
	retractall(agentadd(_)),
	retractall(newagents(_)),
	retractall(agentdel(_)),
	retractall(randomform(_)),
	retractall(forceseg(_)),
	retractall(fitness(_)),
	retractall(minfitness(_)),
	retractall(agerange(_)),
	retractall(fgtfunction(_)),
	retractall(fgtfactor(_)),
	retractall(fgtoffset(_)),
	retractall(datime(_,_,_,_,_,_)),
	retractall(dumpstream(_)),
	retractall(logstream(_)),
	retractall(epochfactor(_)),
	retractall(epochspan(_)),
	retractall(statspan(_)),
	retractall(groupstats(_)),
	retractall(trace(_)),
	retractall(speakerround(_)),
	retractall(speakerselect(_)),
	retractall(hearerselect(_)),
	retractall(selftalk(_)),
	retractall(interactiontype(_)),
	
	% Initialize predicates for the new run
	assert(dumpstream(DumpStream)),
	assert(logstream(LogStream)),
	assert(epochfactor(EpochFactor)),
	assert(epochspan(EpochSpan)),
	assert(statspan(StatSpan)),
	assert(groupstats(GroupStats)),
	assert(trace(Trace)),
	datime(X),assert(X),
	assert(lexupdate(LexUpdate)),
	assert(lexupdatethres(LexUpdateThres)),
	assert(lexupdaterate(LexUpdateRate)),
	assert(intratioupdate(IntRatioUpdate)),
	assert(intratioupdatethres(IntRatioUpdateThres)),
	assert(intratioupdaterate(IntRatioUpdateRate)),
	assert(influenceupdate(InfluenceUpdate)),
	assert(influenceupdatethres(InfluenceUpdateThres)),
	assert(influenceupdaterate(InfluenceUpdateRate)),
	assert(success(0)),
	assert(newforms(0)),
	assert(forgotten(0)),
	assert(innovations(0)),
	assert(phonemetotal(Phonemes)),
	assert(eventtypes(EventTypes)),
	assert(questions(Questions)),
	assert(asksyn(AskSyn)),
	assert(segsync(SegSync)),
	assert(interprule(InterpRule)),
	assert(selfsegsync(SelfSegSync)),
	assert(selfinterprule(SelfInterpRule)),
	assert(tendency(MemTendency)),
	assert(interpeventrole(InterpEventRole)),
	assert(zipfian(Zipfism)),
	assert(agentadd(AgentAdd)),
	assert(newagents(NewAgents)),
	assert(agentdel(AgentDel)),
	assert(randomform(RandomForm)),
	assert(forceseg(ForceSeg)),
	assert(fitness(Fit)),
	assert(minfitness(FitThres)),
	assert(agerange(AgeRange)),
	assert(fgtfunction(FgtFunction)),
	assert(fgtfactor(FgtFactor)),
	assert(fgtoffset(FgtOffset)),
	assert(comactions(0)),
	assert(wordobservations(0)),
	assert(uid(1)),
	
	% Generate agents
	assert(speakers(0)),
	assert(agents(Agents)),
	assert(startagents(Agents)),
	assert(agentindex(Agents)),!,
	gen_strings(Agents,Names),
	assert(agentnames(Names)),
	assert(speakerround(Names)),
	assert(speakerselect(SpeakerSelect)),
	assert(hearerselect(HearerSelect)),
	assert(selftalk(SelfTalk)),
	assert(interactiontype(InteractionType)),
	assert(overhearers(OverHearers)),
	assert(overhearersuccess(OverHearerSuccess)),
	
	% Generate groups
	assert(groupindex(0)),
	init_groups(GroupSizeRatios,GroupSpeakRatios,GroupIntRatios),
	init_group_influences(InfluenceRatios),
	assign_group(Names),
	
	% Generate entities, events and phonemes
	assert(entitycount(Entities)),
	assert(eventcount(Events)),
	assert(eventinstances(EventInstances)),
	gen_entities(Entities),
	gen_events(Events),
	gen_event_instances(EventInstances),
	gen_strings(Phonemes).
	

%%%%%%%%%%
%% Generators
%%%%%%%%%%
%% Generate a specified number of strings that can be used as agent names or phonemes
gen_strings(SCount) :-
  gen_strings(SCount,_).

gen_strings(0,[]).
gen_strings(SCount,[String|Strings]) :-
	Length is (SCount // 26) + 1,
	CharCode is (SCount rem 26) + 97,
	n_copies(CharCode,Length,StringCode),
	name(String,StringCode),
	SCountRemaining is SCount - 1,
	asserta(phonemeindex(SCount,String)),
	gen_strings(SCountRemaining,Strings).


%% Generate a number of events from the given entities
% if zipfian is off (zoff), do things as they were always done
gen_events(EventCount) :-
	zipfian(zoff),
	gen_events_do(EventCount).

% if zipfian is on (zon), do JW's new code
gen_events(EventCount) :-
	zipfian(zon),
	gen_z_events(EventCount,EventCount).


% JW
/* gen_z_events/3
queries below addressed 

0 is the initial value for the Aggregate probability 

TotalEs will be same as EventCount here, when it is used in gen_events_do, it will
record the original number of events, i.e. it will never be decremented 

Aggregate probability keeps a running total of the sums of probabilities
so far, these values are recorded within each zrank and are used as a
range for selection of verb, as above.
AggregateProb works as follows

ZProb 0.13 AggregateProb 0.13 evnt1
ZProb 0.065 AggregateProb 0.195 evnt2
etc.

At selection time a random number is generated, if it were between 0.13 &
0.195 evnt2 would be chosen, this gives it a 0.065 chance as required.
*/

gen_z_events(TotalEvents,EventCount) :-
	zipfprob(TotalEvents,EventCount,Zprob), %% only call to zipfprob/3
	%% higher arity event initialization
	% it depends on -- Z
	% 0 means?
	% TotalEvents -- same as EventCount at this stage
	% EventCount -- same as formerly -- total number of events
	gen_events_do(Zprob,0,TotalEvents,EventCount).

/*

TotalEs is the total number of events to be generated. It will never be
incremented or decremented. 
The predicate addseries/4 calculates the total number of events which will
have occurred by the time R1 has occurred TotalEs times if a Zipfian
distribution is assumed (the other events will have occurred
correspondingly fewer than TotalEs times, e.g. R2 will have occurred half
as many times, R3 a third as many etc.).
*/
	
gen_events_do(0).
gen_events_do(EventCount) :-
	random(97,123,L1),
	random(97,123,L2),
	random(97,123,L3),
	random(97,123,L4),
	random(97,123,L5),
	random(97,123,L6),
	name(E,[L1,L2,L3,L4,L5,L6,115]),
	eventtypes(EventTypes),
	random_element(EventTypes,Type),
	entitycount(EntityCount),
	Index is EntityCount + EventCount,
	asserta(wordindex(Index,Type,E)),
	Events is EventCount - 1,
	gen_events_do(Events).

/* 
The probability of each event is calculated and asserted to the Prolog
database in the dynamic predicate zrank */

gen_events_do(_,_,_,0).
gen_events_do(Prob,AggregateProb,TotalEs,EventCount) :-
	random(97,123,L1),
	random(97,123,L2),
	random(97,123,L3),
	random(97,123,L4),
	random(97,123,L5),
	random(97,123,L6),
	name(E,[L1,L2,L3,L4,L5,L6,115]),
	eventtypes(EventTypes),
	random_element(EventTypes,Type),
	entitycount(EntityCount),
	Index is EntityCount + EventCount,
	asserta(wordindex(Index,Type,E)),
	Events is EventCount - 1,
	zipfprob(Prob,TotalEs,EventCount,Zprob),
	AggregateProb2 is AggregateProb + Zprob, 
	assertz(zrank(E,Zprob,AggregateProb2)),
	gen_events_do(Prob,AggregateProb2,TotalEs,Events).

% JW
addseries(_,Events,1,Events).
addseries(TotalEs,Events,Denominator,Sum) :-
	D is Denominator - 1,
	addseries(TotalEs,Events,D,S),
	Sum is S + (TotalEs/Denominator).

/* zipfprob/3 JW.

zipfprob/3 only calculates the probability of the most highly ranked
event. Only here is the series calculation performed. All other
probabilities are calculated from this one using zipfprob/4 in gen_events_do/5
*/

zipfprob(TEs,Events,Zprob) :-
	addseries(TEs,Events,Events,Series), %% only call to addseries/4
	Zprob is TEs/Series.

/* zipfprob/4 JW 

zipfprob/4 is called in gen_events_do/5. it is the predicate which works out
the probabilities of each event after R1, recording them by asserting
their zrank to the Prolog database. zrank is then used to selection in
select_verb/2
*/

zipfprob(Prob,TEs,Events,Zprob) :-
	Zprob is Prob/(TEs-Events+1).


%% Generate event instances
gen_event_instances(0).
gen_event_instances(InstanceCount) :-
	select_verb(Verb,Type),
	select_arguments(Type,AttList),
	assert(event(InstanceCount,Verb,AttList)),
	InstancesLeft is InstanceCount - 1,
	gen_event_instances(InstancesLeft).


%% Generate entities
gen_entities(0).
gen_entities(EntityCount) :-
	random(97,123,L1),
	random(97,123,L2),
	random(97,123,L3),
	random(97,123,L4),
	name(Entity,[L1,L2,L3,L4]),
	assert(wordindex(EntityCount,7,Entity)),
	EntitiesLeft is EntityCount - 1,
	gen_entities(EntitiesLeft).

%% Generate utterance
gen_utterance(_,[],[]).
gen_utterance(Agent,Meaning,Utterance) :-
	forceseg(ForceSeg),
	random_partition(Meaning,StructuredMeaning,ForceSeg),
	select_forms(Agent,StructuredMeaning,Utterance).

%% Generate group interactions from a pairing distribution
gen_group_interactions(Pairings,NullGroups,GroupIntRatios) :-
	gen_group_interactions(Pairings,NullGroups,1,GroupIntRatios).

% Stop if we've generated at least as many groups as there are other groups
gen_group_interactions(_,NullGroups,GroupIdx,[]) :-
	GroupIdx > NullGroups + 1.
	
gen_group_interactions(Pairings,NullGroups,GroupIdx,[ThisGroupIntRatios|OtherGroupIntRatios]) :-
	gen_group_interaction(Pairings,NullGroups,GroupIdx,ThisGroupIntRatios),
	NextGroupIdx is GroupIdx + 1,
	gen_group_interactions(Pairings,NullGroups,NextGroupIdx,OtherGroupIntRatios).

%% Generate an interaction ratio distribution string based on the current group index and the given pairings
gen_group_interaction([],_,[]).
gen_group_interaction([[GroupIdx,PartnerIdx]|_],NullGroups,GroupIdx,GroupIntRatios) :-
	n_copies(0,NullGroups,NullInteractions),
	nth1(PartnerIdx,GroupIntRatios,1,NullInteractions).

gen_group_interaction([[PartnerIdx,GroupIdx]|_],NullGroups,GroupIdx,GroupIntRatios) :-
	n_copies(0,NullGroups,NullInteractions),
	nth1(PartnerIdx,GroupIntRatios,1,NullInteractions).

gen_group_interaction([_|Pairings],NullGroups,GroupIdx,GroupIntRatios) :-
	gen_group_interaction(Pairings,NullGroups,GroupIdx,GroupIntRatios).


%%%%%%%%%%
%% Agents and Groups
%%%%%%%%%%
%% Setup group ratios from a list of groups
init_groups([],[],[]).
init_groups([GroupSizeRatio|GroupSizeRatios],[GroupSpeakRatio|GroupSpeakRatios],[GroupIntRatios|GroupIntRatiosAll]) :-
	retract(groupindex(GroupCount)),
	NewGroupCount is GroupCount + 1,
	assert(groupsizeratio(NewGroupCount,GroupSizeRatio)),
	assert(groupspeakratio(NewGroupCount,GroupSpeakRatio)),
	init_group_interactions(NewGroupCount,GroupIntRatios,1),
	assert(groupsize(NewGroupCount,0)),
	assert(wordobservations(NewGroupCount,0)),
	assert(groupindex(NewGroupCount)),
	init_groups(GroupSizeRatios,GroupSpeakRatios,GroupIntRatiosAll).


%% Set the interaction ratio value between groups
init_group_interactions(_,[],_).
init_group_interactions(GID,[GroupIntRatio|GroupIntRatios],GID2) :-
	assert(groupintratio(GID,GID2,GroupIntRatio)),
	GID2Next is GID2 + 1,
	init_group_interactions(GID,GroupIntRatios,GID2Next).


%% Set the initial influence value between all groups
init_group_influences(Influence) :-
	findall(GID,groupsize(GID,_),GIDs),
	init_group_influences(GIDs,GIDs,GIDs,Influence).

%% Goes through all GIDs of the first list and saves a value for the first GID of the second list
init_group_influences(_,[],_,_).
init_group_influences([],[_|GIDs],AllGIDs,Influence) :-
	init_group_influences(AllGIDs,GIDs,AllGIDs,Influence).
init_group_influences([GID1|GIDs1],[GID2|GIDs2],AllGIDs,Influence) :-
	assert(groupinfluence(GID1,GID2,Influence)),
	init_group_influences(GIDs1,[GID2|GIDs2],AllGIDs,Influence).


%% Update influence value between two groups
update_influence(SGroup,HGroup,ReferenceSuccess) :-
	% Calculate appropriate update value
	(
		influenceupdate(upd_stat_fixed),
		UpdateVal = 1
	;
		influenceupdatethres(InfluenceUpdateThres),
		% Calculate the update value
		(
			influenceupdate(upd_dyn_rel),
			UpdateVal is ReferenceSuccess - InfluenceUpdateThres
		;
			(
				ReferenceSuccess < InfluenceUpdateThres,
				UpdateVal = -1
			;
				UpdateVal = 1
			)
		)
	),
	% Perform the update
	retract(groupinfluence(SGroup,HGroup,Influence)),
	influenceupdaterate(InfURate),
	InfUpdateVal is UpdateVal*InfURate,
	NewInfluence is Influence + InfUpdateVal,
	(NewInfluence < 0,FinalInfluence = 0;FinalInfluence = NewInfluence),
	assert(groupinfluence(SGroup,HGroup,FinalInfluence)).


%% Reset group interaction ratios
reset_group_interactions(GroupIntRatiosAll) :-
	retractall(groupintratio(_,_,_)),
	reset_group_interactions(1,GroupIntRatiosAll).

reset_group_interactions(_,[]).
reset_group_interactions(GID,[GroupIntRatios|GroupIntRatiosAll]) :-
	init_group_interactions(GID,GroupIntRatios,1),
	GIDNext is GID + 1,
	reset_group_interactions(GIDNext,GroupIntRatiosAll).


%% Update interaction ratio between two groups
update_intratio(SGroup,HGroup,ReferenceSuccess) :-
	% Calculate appropriate update value
	(
		intratioupdate(upd_stat_fixed),
		UpdateVal = 1
	;
		intratioupdatethres(IntRatioUpdateThres),
		% Calculate the update value
		(
			intratioupdate(upd_dyn_rel),
			UpdateVal is ReferenceSuccess - IntRatioUpdateThres
		;
			(
				ReferenceSuccess < IntRatioUpdateThres,
				UpdateVal = -1
			;
				UpdateVal = 1
			)
		)
	),
	% Perform the update
	retract(groupintratio(SGroup,HGroup,IntRatio)),
	intratioupdaterate(IRURate),
	IRUpdateVal is UpdateVal*IRURate,
	NewIntRatio is IntRatio + IRUpdateVal,
	(NewIntRatio < 0,FinalIntRatio = 0;FinalIntRatio = NewIntRatio),
	assert(groupintratio(SGroup,HGroup,FinalIntRatio)).


%% Assign agents to groups, make sure the group ratios are preserved
assign_group([]).
assign_group([Agent|Names]) :-
	findall(GIndex,groupsizeratio(GIndex,_),AllGroups),
	findall(GSR,groupsizeratio(_,GSR),AllSizeRatios),
	findall([GIndex,GSRatio],groupsizeratio(GIndex,GSRatio),GroupSizeRatios),
	sumlist(AllSizeRatios,SizeRatioSum),
	agents(AgentCount),
	
	% Find a group that is below the allowed ratio. If no group is below ratio, add the agent to any random one.
	select_extendable_groups(GroupSizeRatios,SizeRatioSum,AgentCount,OpenGroups),
	(
		OpenGroups \== []
	->
		AgentGroups = OpenGroups
	;
		AgentGroups = AllGroups
	),
	random_element(AgentGroups,AgentGroup),
	
	% Assign the agent to the selected group and increase the group size
	retract(groupsize(AgentGroup,GroupSize)),
	assert(agentgroup(Agent,AgentGroup)),
	NewGroupSize is GroupSize + 1,
	assert(groupsize(AgentGroup,NewGroupSize)),
	
	% Increase the number of speakers if the agent belongs to a speaker group
	groupspeakratio(AgentGroup,GSRatio),
	(
		GSRatio == 0
	;
		retract(speakers(SpeakerCount)),
		SpeakerCountNew is SpeakerCount + 1,
		assert(speakers(SpeakerCountNew))
	),
	
	% Initialize the agent in the group
  assert(agentinfo(Agent,AgentGroup,0,0,0,0,0,0,0,0)),
  
  % Continue with the next agent
	assign_group(Names).


%% Select all groups that are below the group ratio
select_extendable_groups([],_,_,[]).

select_extendable_groups([[GIndex,GRatio]|GRatiosTail],RatioSum,AgentCount,OpenGroups) :-
	groupsize(GIndex,GSize),
	CurrentRatio is GSize/AgentCount,
	GRatioRel is GRatio/RatioSum,
	CurrentRatio < GRatioRel,
	select_extendable_groups(GRatiosTail,RatioSum,AgentCount,OpenGroups1),
	append([GIndex],OpenGroups1,OpenGroups).

select_extendable_groups([_|GRatiosTail],RatioSum,AgentCount,OpenGroups) :-
	select_extendable_groups(GRatiosTail,RatioSum,AgentCount,OpenGroups).


%%%%%%%%%%
%% Selectors
%%%%%%%%%%
%% Select an agent that acts as a speaker
select_speaker(Speaker) :-
	speakerselect(ss_rand),!,
	findall([GIndex,GSpeakRatio],groupspeakratio(GIndex,GSpeakRatio),GroupsWeighted),
	random_element_weighted(GroupsWeighted,GroupIndex),
	findall(Agent,agentgroup(Agent,GroupIndex),Names),
	random_element(Names,Speaker),!.

%% Select an agent that acts as a speaker
select_speaker(Speaker) :-
	speakerselect(ss_turns),!,
	retract(speakerround(Names)),
	random_element(Names,Speaker),
	delete(Names,Speaker,OtherNames),
	%% If there are no names left then we're into the next round
	(
		OtherNames == []
	->
		agentnames(AllNames),
		assert(speakerround(AllNames))
	;
		assert(speakerround(OtherNames))
	).

%% Select an agent that acts as a hearer
select_hearers(Speaker,Hearers) :-
	agentgroup(Speaker,SpeakerGroup),
	findall([HearerGroup,IntRatio],groupintratio(SpeakerGroup,HearerGroup,IntRatio),GroupIntRatios),
	% Select a group from where to pick a hearer in proportion to the group interaction ratios
	random_element_weighted(GroupIntRatios,GroupIndex),
	findall(Agent,agentgroup(Agent,GroupIndex),AllNames),
	% Check if self-talk is allowed
	selftalk(SelfTalk),
	(SelfTalk == 0,lists_subtract(AllNames,[Speaker],Names);Names = AllNames),
	interactiontype(InteractionType),
	(InteractionType == int_dia,random_element(Names,Hearer),Hearers = [Hearer];Hearers = Names).

%% Select an agent that acts as an overhearer
select_overhearer(OverHearer) :-
	% Currently assumes that there is at least/most one group with speakratio = 0
	groupspeakratio(GroupIndex,0),
	findall(Agent,agentgroup(Agent,GroupIndex),Names),
	random_element(Names,OverHearer).


%% Select a random event instance
select_event([Verb|Comps]) :-
	eventinstances(EventInstances),
	EventInstancesTo is EventInstances + 1,
	random(1,EventInstancesTo,InstanceID),
	event(InstanceID,Verb,Comps).


%% Select the next unused entity
select_entity(Entity,Used) :- 
	findall(X,(wordindex(_,7,X)),AllEntities),
	lists_subtract(AllEntities,Used,Entities),
	random_element(Entities,Entity).


%% Select a random verb from the meaning/word pool
% Random implementation
select_verb(Verb,Verbtype) :-
	zipfian(zoff),
	entitycount(EntityCount),
	IndexFrom is EntityCount + 1,
	eventcount(EventCount),
	IndexTo is IndexFrom + EventCount,
	random(IndexFrom,IndexTo,VID),
	wordindex(VID,Verbtype,Verb).

% Zipfian implementation
select_verb(Verb,Verbtype) :-
	zipfian(zon),
	var(Verbtype),
	random(Seed),	
	zrank(Verb,_,AggZipf),
	Seed =< AggZipf,
	wordindex(_,Verbtype,Verb).


%% Select a complement to the given verb type, i.e. the arguments
% R1 - one argument - intransitive verb
select_arguments(1,[Subj]) :-
	select_entity(Subj,[]).

% R2 - two arguments - transitive verb
select_arguments(2,[Subj,Obj]) :-
	select_entity(Subj,[]),
	select_entity(Obj,[Subj]).

% R3 - three arguments - ditransitive verb
select_arguments(3,[Subj,Obj,Obj2]) :-
	select_entity(Subj,[]),
	select_entity(Obj,[Subj]),
	select_entity(Obj2,[Subj,Obj]).

% R4 - one human argument and one relation argument
select_arguments(4,[Subj|Statement]) :-
	select_entity(Subj,[]),
	select_event(Statement).

	
% Select a random phoneme pair from the db
select_phonemes([[P,Q]]) :-
	retract(innovations(FormIndex)),
	
	phonemetotal(N),
	X is FormIndex//N + 1,
	Y is FormIndex mod N + 1,
	phonemeindex(X,P),
	phonemeindex(Y,Q),
	
	FormIndex1 is FormIndex + 1,
	assert(innovations(FormIndex1)).


% Returns all mappings within a given usage range
select_mappings_by_count(C,C,[]).
select_mappings_by_count(C,FThresh,MappingList) :-
	C1 is C + 1,
	select_mappings_by_count(C1,FThresh,MappingListNext),
	findall([Meaning,Form],wc(Meaning,Form,C,_),MappingListCurrent),
	append(MappingListNext,MappingListCurrent,MappingList).


% Returns all utterances that included the given mappings
select_utterances_with_mapping([],[]).
select_utterances_with_mapping([[Agent,Meaning,Form]|MappingList],UIDList) :-
	select_utterances_with_mapping(MappingList,UIDListNext),
	findall(UID,agentutt(Agent,UID,_,Meaning,Form),UIDListCurrent),
	append(UIDListNext,UIDListCurrent,UIDList).


%%%%%%%%%%
%% Conversation process
%%%%%%%%%%
%% Generate an utterance for a meaning
% Select a form for every meaning in the event
select_forms(_,[],[]).
select_forms(Agent,[Meaning|T],[[Meaning,Form]|Utterance]) :-
	% Select a previously used form from the LB
	select_form(Agent,Meaning,UsedForm),!,
	(
		% Generate a new form at random every N interactions
	  randomform(N),
	  Temp is (N+1),
	  random(1,Temp,Ran),
		Ran == N,
		select_phonemes(Form)
	;
		Form = UsedForm
	),
	% Continue with the list
	select_forms(Agent,T,Utterance).

%% Select a form for a given meaning
% Try to find a previously used form - frequency based
select_form(Agent,Meaning,Form) :-
	tendency(f),
	findall([Form,C],agentwc(Agent,_,Meaning,Form,C),FormList),
	random_element_weighted(FormList,Form).

% Try to find a previously used form - recency based
select_form(Agent,Meaning,Form) :-
	tendency(r),
	agentutt(Agent,_,_,Meaning,Form).

% If the above fails, select a random form
select_form(_,_,Form) :-
	select_phonemes(Form).

%% Select an alternative form (synonym) for a given meaning
% Try to find a previously used form - frequency based
select_form_syn(Agent,Meaning,Form,BadForms) :-
	tendency(f),!,
	findall([Form,C],(agentwc(Agent,_,Meaning,Form,C),nonmember(Form,BadForms)),FormList),
	random_element_weighted(FormList,Form).

% Try to find a previously used form - recency based
select_form_syn(Agent,Meaning,Form,BadForms) :-
	tendency(r),!,
	agentutt(Agent,_,_,Meaning,Form),
	nonmember(Form,BadForms).


%% Interpret the utterance
% cmv: June 29, 2009 -- add in synchrony vs asynchrony in segmenting the speech signal.
% Wrong segmentation interpretation
interp(Speaker,Hearer,SegSync,InterpRule,Utterance,Event,EventUnits,FinalUtterance,Heard,LBUse,NewForms) :-
	SegSync == seg_wrong,!,
	split_pairs(Utterance,UttMeanings,UttForms),
	merge_partitions(UttForms,FlatUtterance),
	random_partition_disjunct(FlatUtterance,UttForms,HeardForms),!,
	fm(Speaker,Hearer,SegSync,InterpRule,UttMeanings,HeardForms,Event,EventUnits,_,Heard,LBUse,NewForms),
	FinalUtterance = Utterance.

% Random (most probably asynchronous) segmentation interpretation
interp(Speaker,Hearer,SegSync,InterpRule,Utterance,Event,EventUnits,FinalUtterance,Heard,LBUse,NewForms) :-
	SegSync == seg_rand,!,
	split_pairs(Utterance,UttMeanings,UttForms),
	merge_partitions(UttForms,FlatUtterance),
	random_partition(FlatUtterance,HeardForms),!,
	fm(Speaker,Hearer,SegSync,InterpRule,UttMeanings,HeardForms,Event,EventUnits,_,Heard,LBUse,NewForms),
	FinalUtterance = Utterance.

% Synchronous segmentation interpretation
interp(Speaker,Hearer,SegSync,InterpRule,Utterance,Event,EventUnits,FinalUtterance,Heard,LBUse,NewForms) :-
	SegSync == seg_sync,!,
	split_pairs(Utterance,UttMeanings,HeardForms),!,
	fm(Speaker,Hearer,SegSync,InterpRule,UttMeanings,HeardForms,Event,EventUnits,FinalUttForms,Heard,LBUse,NewForms),
	split_pairs(FinalUtterance,UttMeanings,FinalUttForms).


%% Decode the utterance by mapping every form to a meaning
fm(_,_,_,_,_,[],_,_,[],[],[],0).

% Look for a corresponding meaning in the LB
fm(Speaker,Hearer,SegSync,InterpRule,[RealMeaning|UttMeanings],[Form|Partition],Event,EventUnits,[Form|FUtt],[[Meaning,Form]|PM],[1|LBUse],NewForms) :-
	search_meaning(Hearer,Form,EventUnits,Meaning),!,
	(SegSync == seg_sync, UttMeanings1=UttMeanings; UttMeanings1=[RealMeaning|UttMeanings]),
	fm(Speaker,Hearer,SegSync,InterpRule,UttMeanings1,Partition,Event,EventUnits,FUtt,PM,LBUse,NewForms).

% Ask for an alternative form if no meaning was found yet
fm(Speaker,Hearer,SegSync,InterpRule,[RealMeaning|UttMeanings],[FailForm|Partition],Event,EventUnits,[Form|FUtt],[[Meaning,Form]|PM],[1|LBUse],NewForms) :-
	asksyn(1),
	search_meaning_syn(Speaker,Hearer,RealMeaning,[FailForm],EventUnits,Meaning,Form,Attempts),!,
	(SegSync == seg_sync, UttMeanings1=UttMeanings; UttMeanings1=[RealMeaning|UttMeanings]),
	fm(Speaker,Hearer,SegSync,InterpRule,UttMeanings1,Partition,Event,EventUnits,FUtt,PM,LBUse,NewForms1),
	NewForms is NewForms1 + Attempts.

% Assign a random meaning from the event to the form
% !!! This can possibly assign overlapping or even same meaning to all forms
% Random interpretation
fm(Speaker,Hearer,SegSync,InterpRule,[RealMeaning|UttMeanings],[Form|Partition],Event,EventUnits,[Form|FUtt],[[Meaning,Form]|PM],[0|LBUse],NewForms) :-
	InterpRule == interp_rand,!,
	random_meaning(Event,Meaning),
	(SegSync == seg_sync, UttMeanings1=UttMeanings; UttMeanings1=[RealMeaning|UttMeanings]),
	fm(Speaker,Hearer,SegSync,InterpRule,UttMeanings1,Partition,Event,EventUnits,FUtt,PM,LBUse,NewForms).

% Wrong interpretation
fm(Speaker,Hearer,SegSync,InterpRule,[RealMeaning|UttMeanings],[Form|Partition],Event,EventUnits,[Form|FUtt],[[Meaning,Form]|PM],[0|LBUse],NewForms) :-
	InterpRule == interp_wrong,!,
	lists_delete(EventUnits,UttMeanings,WrongEventUnits),
	random_element(WrongEventUnits,Meaning),
	(SegSync == seg_sync, UttMeanings1=UttMeanings; UttMeanings1=[RealMeaning|UttMeanings]),
	fm(Speaker,Hearer,SegSync,InterpRule,UttMeanings1,Partition,Event,EventUnits,FUtt,PM,LBUse,NewForms).

% Omniscient interpretation
fm(Speaker,Hearer,SegSync,InterpRule,[RealMeaning|UttMeanings],[Form|Partition],Event,EventUnits,[Form|FUtt],[[RealMeaning,Form]|PM],[0|LBUse],NewForms) :-
	InterpRule == interp_omni,!,
	(SegSync == seg_sync, UttMeanings1=UttMeanings; UttMeanings1=[RealMeaning|UttMeanings]),
	fm(Speaker,Hearer,SegSync,InterpRule,UttMeanings1,Partition,Event,EventUnits,FUtt,PM,LBUse,NewForms).

% Contrastive interpretation
fm(Speaker,Hearer,SegSync,InterpRule,[RealMeaning|UttMeanings],[Form|Partition],Event,EventUnits,[Form|FUtt],[[Meaning,Form]|PM],[0|LBUse],NewForms) :-
	InterpRule == interp_contrast,!,
	random_meaning_contrast(Hearer,EventUnits,Meaning),
	(SegSync == seg_sync, UttMeanings1=UttMeanings; UttMeanings1=[RealMeaning|UttMeanings]),
	fm(Speaker,Hearer,SegSync,InterpRule,UttMeanings,Partition,Event,EventUnits,FUtt,PM,LBUse,NewForms).


%% Request alternative synonyms for a form
% Attempts to get synonyms from the speaker until one of them makes sense, or the speaker has none left
search_meaning_syn(Speaker,Hearer,RealMeaning,FailForms,EventUnits,Meaning,Form,Attempts) :-
	(select_form_syn(Speaker,RealMeaning,NewForm,FailForms);fail),!,
	(
		search_meaning(Hearer,NewForm,EventUnits,NewMeaning)
	->
		Attempts = 1,
		Form = NewForm,
		Meaning = NewMeaning
	;
		!,search_meaning_syn(Speaker,Hearer,RealMeaning,[NewForm|FailForms],EventUnits,Meaning,Form,Attempts1),
		Attempts is Attempts1 + 1
	).


%% Find a meaning for a given form
% Returns a meaning for the given form - frequency based
search_meaning(Agent,Form,EventUnits,Meaning) :-
	tendency(f),!,
	(
		interpeventrole(event_ignore)
	->
		% If we don't care about the event, just get all meanings from the LB
		findall([Meaning,C],agentwc(Agent,_,Meaning,Form,C),MeaningList)
	;
		% If we care about the event in some way, get only those meanings from the LB that are also in the event
		remove_dups(EventUnits,EventUnitsSet),
		findall([Meaning,C],(agentwc(Agent,_,Meaning,Form,C),member(Meaning,EventUnitsSet)),MeaningListCandidate),
		% However, if that list is empty, and being in event is only a preference, get the full list from the LB
		(
			MeaningListCandidate == [],
			interpeventrole(event_pref)
		->
			findall([Meaning,C],agentwc(Agent,_,Meaning,Form,C),MeaningList)
		;
			% If being in event is not a preference (event_only), or if the list is not empty, then it is what it is
			MeaningList = MeaningListCandidate
		)
	),!,
	random_element_weighted(MeaningList,Meaning).

% Returns a meaning for the given form - recency based
search_meaning(Agent,Form,EventUnits,Meaning) :-
	tendency(r),!,
	(
		interpeventrole(event_ignore)
	->
		% If we don't care about the event, just get the last used meaning
		agentutt(Agent,_,_,Meaning,Form)
	;
		% If we care about the event in some way, get only those meanings from the LB that are also in the event
		findall(M,agentutt(Agent,_,_,M,Form),MeaningList),
		invert(MeaningList,MeaningListOrdered),
		remove_duplicates(MeaningListOrdered,MeaningListUnique),
		intersection(MeaningListUnique,EventUnits,LBMeaningsInEvent),!,
		% However, if that list is empty, and being in event is only a preference, get the full list from the LB
		(
			LBMeaningsInEvent == [],
			interpeventrole(event_pref)
		->
			MeaningListUnique = [Meaning|_]
		;
			% If being in event is not a preference (event_only), or if the list is not empty, then it is what it is
			LBMeaningsInEvent = [Meaning|_]
		)
	).


%% Select some random meaning from an event
% Returns a random meaning for the given event
random_meaning(Event,Meaning) :-
	random_partition(Event,EventUnits),
	random_element(EventUnits,Meaning).


%% Select some random meaning from a given list of partitions according to the principle of contrast
% Makes sure the meaning is not in the agent's lexicon already
% If every possible meaning partition already has a form, don't assign any meaning
random_meaning_contrast(_,[],[]).
random_meaning_contrast(Agent,Meanings,Meaning) :-
	random_element(Meanings,CandidateMeaning),
	(
		agentwc(Agent,_,CandidateMeaning,_,_)
	->
		delete(Meanings,CandidateMeaning,OtherMeanings),
		random_meaning_contrast(Agent,OtherMeanings,Meaning)
	;
		Meaning = CandidateMeaning
	).


%% Decode the utterance
% Decode the utterance into one unsegmented list of all forms
hear([],[]).
hear([[_,Form]|T],Forms) :-
	hear(T,TailForms),
	append(Form,TailForms,Forms).

% Decode the utterance directly into a segmented list of forms, corresponding to meanings
synchronoushear([],[]).
synchronoushear([[_,Form]|T],[Form|TailForms]) :-
	synchronoushear(T,TailForms).

% Make sure that the decoded partition is wrong - a 1-element list can not be partitioned wrongly
wrong_partition([],_).
wrong_partition([_|[]],_).
wrong_partition([[_,Form]|T],Heard) :-
	nonmember(Form,Heard),!,
	wrong_partition(T,Heard).

%% Calculate the understanding ratio
understanding_ratio(Utterance,Heard,MeaningHits,LBUse,LBUsed,LBCheckable,LBHits) :-
	understanding_matches(Utterance,Heard,0,0,MeaningHits,LBUse,LBUsed,LBCheckable,LBHits).

% If reached the end of partitions, we're done
understanding_matches([],_,_,_,0,_,0,0,0).
understanding_matches(_,[],_,_,0,_,0,0,0).

% If the meaning of the utterance matches with the heard one, increase the meaning count
understanding_matches([[Meaning,FormS]|SpokenT],[[Meaning,FormH]|HeardT],PosS,PosH,MeaningHits,[LBBit|LBUse],LBUsed,LBCheckable,LBHits) :-
	length(FormS,FormSLen),
	length(FormH,FormHLen),
	PosSNext is PosS + FormSLen,
	PosHNext is PosH + FormHLen,
	% Check if the next partitions are on the same level form-wise
	(
		PosSNext == PosHNext
	->
		SpokenTLevelled = SpokenT, PosSLevelled = PosSNext,
		HeardTLevelled = HeardT, PosHLevelled = PosHNext
	;
		% If they're not, level up to avoid multiple matching
		(
			PosSNext < PosHNext
		->
			MoveS is PosHNext - PosSNext,
			skip_mfs(SpokenT,PosSNext,MoveS,SpokenTLevelled,PosSLevelled),
			HeardTLevelled = HeardT, PosHLevelled = PosHNext
		;
			MoveH is PosSNext - PosHNext,
			SpokenTLevelled = SpokenT, PosSLevelled = PosSNext,
			skip_mfs(HeardT,PosHNext,MoveH,HeardTLevelled,PosHLevelled)
		)
	),!,
	understanding_matches(SpokenTLevelled,HeardTLevelled,PosSLevelled,PosHLevelled,MeaningHits1,LBUse,LBUsed1,LBCheckable1,LBHits1),
	MeaningHits is MeaningHits1 + 1,
	LBUsed is LBUsed1 + LBBit,
	% If LB was used and the form was the same, increment both counters
	(
		LBBit == 1,
		FormS == FormH
	->
		LBCheckable is LBCheckable1 + 1,
		LBHits is LBHits1 + 1
	;
		LBCheckable = LBCheckable1,
		LBHits = LBHits1
	).

% Otherwise just go on
understanding_matches([[MeaningS,FormS]|SpokenT],[[MeaningH,FormH]|HeardT],PosS,PosH,MeaningHits,[LBBit|LBUse],LBUsed,LBCheckable,LBHits) :-
	length(FormS,FormSLen),
	length(FormH,FormHLen),
	PosSNext is PosS + FormSLen,
	PosHNext is PosH + FormHLen,
	% Check if to proceed with either partition or both
	(
		(
			% If spoken and heard partitions were aligned already, move to next ones
			(PosSNext == PosHNext)
		;
			% If they weren't aligned, but moving one side only wouldn't align them either, move both
			(PosS < PosH, PosH < PosSNext, PosSNext < PosHNext)
		;
			(PosH < PosS, PosS < PosHNext, PosHNext < PosSNext)
		)
	->
		understanding_matches(SpokenT,HeardT,PosSNext,PosHNext,MeaningHits,LBUse,LBUsed1,LBCheckable1,LBHits1),
		LBUsed is LBUsed1 + LBBit
	;
		% If they weren't aligned and might still align, proceed with the shorter partition only
		(
			PosSNext < PosHNext
		->
			understanding_matches(SpokenT,[[MeaningH,FormH]|HeardT],PosSNext,PosH,MeaningHits,[LBBit|LBUse],LBUsed1,LBCheckable1,LBHits1),
			LBUsed = LBUsed1
		;
			understanding_matches([[MeaningS,FormS]|SpokenT],HeardT,PosS,PosHNext,MeaningHits,LBUse,LBUsed1,LBCheckable1,LBHits1),
			LBUsed is LBUsed1 + LBBit
		)
	),
	% If the two forms were aligned, and LB was used increment the "checkable" counter
	(
		PosSNext == PosHNext,
		LBBit == 1,
		FormS == FormH
	->
		LBCheckable is LBCheckable1 + 1
	;
		LBCheckable = LBCheckable1
	),
	% No hits obviously as meanings didn't match
	LBHits = LBHits1.

% Skip a certain number of meaning-form pairs, but not beyond the parallel string
skip_mfs([[Meaning,Form]|T],OldPos,SkipCount,NewList,NewPos) :-
	length(Form,FormLen),
	SkipCount1 is SkipCount - FormLen,
	% Only skip if still needed
	(
		SkipCount1 > 0
	->
		skip_mfs(T,OldPos,SkipCount1,NewList,NewPos1),
		NewPos is NewPos1 + FormLen
	;
	  % If skipped beyond the current pair, restore the last mapping to ensure that all overlaps are caught
		(
			SkipCount1 < 0
		->
			NewList = [[Meaning,Form]|T],
			NewPos = OldPos
		;
			NewList = T,
			NewPos is OldPos + FormLen
		)
	).


%%%%%%%%%%
%% Forgetting
%%%%%%%%%%
%% Gradually forget mappings by decreasing their strength
forget_decay :-
	(
		fgtfunction(fgt_none)
	;
		fgtfactor(FF),
		fgtoffset(FO),
		findall([Name,Group,Meaning,Form,C],agentwc(Name,Group,Meaning,Form,C),MeaningList),
		(fgtfunction(fgt_const),forget_decay_constant(MeaningList,FF,FO);true),
		(fgtfunction(fgt_lin),forget_decay_linear(MeaningList,FF,FO);true),
		(fgtfunction(fgt_poly),forget_decay_polynomial(MeaningList,FF,FO);true)
	).

% Constant decay = C - FgtFactor
forget_decay_constant([],_,_).
forget_decay_constant([[Name,Group,Meaning,Form,C]|MeaningList],FgtFactor,FgtOffset) :-
	retract(agentwc(Name,Group,Meaning,Form,C)),
	CTemp is C-1/(FgtFactor+FgtOffset),
	(CTemp < 0.0, CNew = 0.0; CNew = CTemp),
	assert(agentwc(Name,Group,Meaning,Form,CNew)),
	forget_decay_constant(MeaningList,FgtFactor,FgtOffset).

% Linear decay = C - 1/(FgtFactor*C + 1)
forget_decay_linear([],_,_).
forget_decay_linear([[Name,Group,Meaning,Form,C]|MeaningList],FgtFactor,FgtOffset) :-
	retract(agentwc(Name,Group,Meaning,Form,C)),
	CTemp is C-1/(FgtFactor*C+FgtOffset),
	(CTemp < 0.0, CNew = 0.0; CNew = CTemp),
	assert(agentwc(Name,Group,Meaning,Form,CNew)),
	forget_decay_linear(MeaningList,FgtFactor,FgtOffset).

% Polynomial decay = C - 1/(C^FgtFactor + 1)
forget_decay_polynomial([],_,_).
forget_decay_polynomial([[Name,Group,Meaning,Form,C]|MeaningList],FgtFactor,FgtOffset) :-
	retract(agentwc(Name,Group,Meaning,Form,C)),
	CTemp is C-1/(exp(C,FgtFactor)+FgtOffset),
	(CTemp < 0.0, CNew = 0.0; CNew = CTemp),
	assert(agentwc(Name,Group,Meaning,Form,CNew)),
	forget_decay_polynomial(MeaningList,FgtFactor,FgtOffset).


%% Clean all words with 0-weight mappings and all corresponding utterances
% Iterate through every name and look for mappings/sentences to forget
forget_clean :-
	findall([Name,Meaning,Form],agentwc(Name,_,Meaning,Form,0.0),MappingList),
	select_utterances_with_mapping(MappingList,UIDList),
	forget_lb(MappingList),
	forget_utt(UIDList).


% Removes the mapping from the lexicons
forget_lb([]).
forget_lb([[Name,Meaning,Form]|MappingList]) :-
	retract(agentwc(Name,_,Meaning,Form,_)),
	decrease_wc_l([[Meaning,Form]]),
	report(['Retracting:',agentwc(Name,_,Meaning,Form,_)]),
	forget_lb(MappingList).


% Removes the record of an utterance from the utterance database
forget_utt([]).
forget_utt([UID|UIDList]) :-
	retractall(agentutt(_,UID,_,_,_)),
	forget_utt(UIDList).


%%%%%%%%%%
%% Questions
%%%%%%%%%%
%% Collect all of the agent's mappings and start the questioning
ask_questions(Agent) :-
	findall([Meaning,Form],agentwc(Agent,_,Meaning,Form,_),LB),
	ask_questions(Agent,LB).

%% Ask a question about each m-f mapping in the LB
ask_questions(_,[]).
ask_questions(Agent,[Mapping|Rest]) :-
	ask_question(Agent,Mapping),
	ask_questions(Agent,Rest).

% Find a hearer and inquire if he has the given m-f mapping
ask_question(_,[]).
ask_question(Speaker,Mapping) :-
	agentnames(Names),
	delete(Names,Speaker,OtherNames),
	random_element(OtherNames,Hearer),
	%report(['Agent',Speaker,'asks agent',Hearer,'if he understands the mapping',Mapping]),
	get_answer(Hearer,Mapping,AnswerMapping),
	report(['QS',Speaker,Hearer,Mapping,AnswerMapping],';'),
	revise_lb(Speaker,Hearer,Mapping,AnswerMapping).


% Checks if the hearer has the same m-f mapping or at least a form that has an intersection with the given utterance form
get_answer(Hearer,[_,Form],[Meaning,Form]) :-
	agentwc(Hearer,_,Meaning,Form,_),!.
	%report(['Agent',Hearer,'answers with',[Meaning,Form]]),!.

get_answer(Hearer,[_,Form],HearerMapping) :-
	findall([M,F],agentwc(Hearer,_,M,F,_),Mappings),
	find_intersecting_form(Form,Mappings,HearerMapping),!.
	%report(['Agent',Hearer,'answers with',HearerMapping]),!.

% Try to find a mapping with at least intersecting form. Failing to do so, return 0.
find_intersecting_form(_,[],0).
find_intersecting_form(TargetForm,[[Meaning,Form]|_],[Meaning,Form]) :-
	intersect(TargetForm,Form),!.

find_intersecting_form(TargetForm,[_|Mappings],Mapping) :-
	find_intersecting_form(TargetForm,Mappings,Mapping).


%%%%%%%%%%
%% Agent addition/removal
%%%%%%%%%%
%% Eliminates some agents
% Check if elimination enabled
agent_elimination :-
	agentdel(doff),!.

% Based on fitness
agent_elimination :-
	fitness(fon),
	% Compute the minimum understanding ratio given the average and the deviation parameter
	comactions(IntTotal),
	findall(MP,agentinfo(_,_,_,_,MP,_,_,_,_,_),MeaningPrecisions),
	sumlist(MeaningPrecisions,MPTotal),
	MeaningPrecisionAvg is MPTotal/IntTotal,
	minfitness(MaxUndDeviation),
	MinUndRatio is MeaningPrecisionAvg*(1-MaxUndDeviation),
	% Get all the unfit agents and delete some at random
	agentnames(Names),
	unfits(Names,Unfits,MinUndRatio),
	del_agents(Unfits).

% Or randomly
agent_elimination :-
	fitness(foff),
	agentnames(Names),
	del_agents(Names).


%% Returns list of unfits.
%% If fitness is on, fitness function is done first
unfits([],[],_).
unfits([Name|Names],[Name|Unfits],MinUndRatio) :-
	% Compute the fitness level of an agent
	agentinfo(Name,_,SpeakerInts,HearerInts,MeaningPrecision,_,_,_,_,_),
	InteractionCount is SpeakerInts + HearerInts,
	InteractionCount > 0,
	AvgUndRatio is MeaningPrecision/InteractionCount,
	% Agent fitness is equal to his average understanding level minues the aging factor
	agerange(AgeRange),
	AgeFitLoss is InteractionCount/AgeRange,
	AgentFitness is (1-AgeFitLoss)*AvgUndRatio,
	% Add him to the unfits list if it's below the minimum
	report([Name,AvgUndRatio,InteractionCount,AgentFitness,MinUndRatio]),
	AgentFitness < MinUndRatio,
	unfits(Names,Unfits,MinUndRatio).

unfits([_|Names],Unfits,MinUndRatio) :-
	unfits(Names,Unfits,MinUndRatio).


%% Delete some agents from a list of unfits
del_agents([]).
del_agents([Agent|Agents]) :-
	agentdel(don),
	random(0,2,1),
	report(['Deleting agent:',Agent]),
	% Delete all the agent-relevant information
	findall([Meaning,Form],agentwc(Agent,_,Meaning,Form,_),AgentMappings),
	retractall(agentwc(Agent,_,_,_,_)),
	decrease_wc_l(AgentMappings),
	retractall(agentutt(Agent,_,_,_,_)),
	% Mark agent removal in his group
	retract(agentgroup(Agent,AgentGroup)),
	retract(groupsize(AgentGroup,GroupSize)),
	NewGroupSize is GroupSize - 1,
	assert(groupsize(AgentGroup,NewGroupSize)),
	% Remove the agent from the names list
	retract(agentnames(Names)),
	delete(Names,Agent,NewNames),
	assert(agentnames(NewNames)),
	% Decrease agent count
	retract(agents(AgentCount)),
	NewAgentCount is AgentCount - 1,
	assert(agents(NewAgentCount)),
	% Decrease speaker count if the agent was from a speaking group
	groupspeakratio(AgentGroup,GSRatio),
	(
		GSRatio == 0
	;
		retract(speakers(SpeakerCount)),
		SpeakerCountNew is SpeakerCount - 1,
		assert(speakers(SpeakerCountNew))
	),
	% Go on with the list
	del_agents(Agents).

del_agents([_|Agents]) :-
	del_agents(Agents).


%% Add some agents
% If agent addition is off, skip
add_agents :-
	agentadd(aoff).

% If agent addition is on, proceed
add_agents :-
	agentadd(aon),
	newagents(NewAgents),
	NewAgentsLim is NewAgents + 1,
	random(0,NewAgentsLim,NewAgentsNow),
	% Generate new agents and initialize them
	gen_agent_names(NewAgentsNow,NewNames),
	(NewNames == []; report(['Adding agents:',NewNames])),
	assign_group(NewNames),
	% Add the name to the agents list
	retract(agentnames(Names)),
	append(NewNames,Names,AllNames),
	assert(agentnames(AllNames)).

% Generates N new agents
gen_agent_names(0,[]).
gen_agent_names(NewCount,[Name|Names]) :-
	random(0,2,1),
	% Increase the agent index
	retract(agentindex(MaxIndex)),
	NewIndex is MaxIndex + 1,
	assert(agentindex(NewIndex)),
	% Generate a new name (will be unique)
	Length is (NewIndex // 26) + 1,
	CharCode is (NewIndex rem 26) + 97,
	n_copies(CharCode,Length,StringCode),
	name(Name,StringCode),
	% Increase the number of agents in the system
	retract(agents(AgentCount)),
	AgentCount1 is AgentCount + 1,
	assert(agents(AgentCount1)),
	% Add new name string to the string index list
	retractall(phonemeindex(AgentCount1,_)),
	assert(phonemeindex(AgentCount1,Name)),
	% Proceed to the next one
	NewCount1 is NewCount - 1,
	gen_agent_names(NewCount1, Names).

gen_agent_names(NewCount,Names) :-
	NewCount1 is NewCount - 1,
	gen_agent_names(NewCount1,Names).


%%%%%%%%%%
%% Database manipulation
%%%%%%%%%%
%% Records an utterance
record_utt(Agent,Utterance) :-
	retract(uid(UID)),
	UID1 is UID + 1,
	assert(uid(UID1)),
	record_utt(Agent,Utterance,UID,1).

%% Don't record parts of the utterance that weren't attached to any meaning
record_utt(_,[],_,_).
record_utt(Agent,[[[],_]|Utterance],UID,MID) :-
	record_utt(Agent,Utterance,UID,MID).
record_utt(Agent,[[Meaning,Form]|Utterance],UID,MID) :-
	asserta(agentutt(Agent,UID,MID,Meaning,Form)),
	MID1 is MID + 1,
	record_utt(Agent,Utterance,UID,MID1).


%% Update agents successful interaction count and cumulative understanding ratio
update_agent_info(Agent,Speaker,Hearer,MP,MR,MS,LBI,LBU,LBP) :-
	% Update recorded communicative action count
	retract(comactions(CA)),
	CA1 is CA + 1,
	assert(comactions(CA1)),
	
	% Update global average success
	retract(success(S)),
	ST is S * CA,
	STNew is ST + MS,
	SNew is STNew/CA1,
	assert(success(SNew)),
	
	% Update the agent's cumulative stats
	retract(agentinfo(Agent,Group,SpeakerInts,HearerInts,MeaningPrecision,MeaningRecall,MeaningSuccess,LBInts,LBUse,LBPrecision)),
	SITotal is SpeakerInts + Speaker,
	HITotal is HearerInts + Hearer,
	MPTotal is MeaningPrecision + MP,
	MRTotal is MeaningRecall + MR,
	MSTotal is MeaningSuccess + MS,
	LBITotal is LBInts + LBI,
	LBUTotal is LBUse + LBU,
	LBPTotal is LBPrecision + LBP,
	assert(agentinfo(Agent,Group,SITotal,HITotal,MPTotal,MRTotal,MSTotal,LBITotal,LBUTotal,LBPTotal)).


%% Add the mapping to agent's lexicon if it's new and record the addition
% If no meaning was extracted, just skip to next mapping
update_lb(_,[],_).
update_lb(Agent,[[[],_]|MappingList],UpdateVal) :-
	update_lb(Agent,MappingList,UpdateVal).

update_lb(Agent,[[Meaning,Form]|MappingList],UpdateVal) :-
	% If the mapping existed already, increase the usage count by 1
	(
		retract(wc(Meaning,Form,C,L)),!,
		C1 is C + 1
	; % If it's a new mapping, set usage count to 1
		C1 = 1,
		L = 0
	),
	% If the agent had this mapping already, increase his personal stats by 1
	% Note: This will never be true if the mapping is not in the wc/4
	(
		retract(agentwc(Agent,Group,Meaning,Form,AC)),!,
		AC1 is AC + UpdateVal,
		L1 = L
	; % If it's a new mapping for the agent, mark that a new agent has learned the mapping
		agentgroup(Agent,Group),
		AC1 = UpdateVal,
		L1 is L + 1
	),
	% If the new mapping value is not > 0, then it can just be forgotten
	(
		AC1 =< 0,
		L2 is L1 - 1,
		
		% Record that the mapping was actually forgotten (can only be the case if update was negative)
    (
      UpdateVal < 0,
      retract(forgotten(FOld)),
      FNew is FOld + 1,
      assert(forgotten(FNew))
    ;
      true
    )
	;
		assert(agentwc(Agent,Group,Meaning,Form,AC1)),
		L2 = L1
	),
	% Only save the mapping again if it was actually saved in someone's lexicon
	(
		L2 =< 0
	;
		assert(wc(Meaning,Form,C1,L2))
	),
	% Update the global total word observation count
	retract(wordobservations(N)),
	N1 is N + 1,
	assert(wordobservations(N1)),
	% Update the agent's group's word observation count
	retract(wordobservations(Group,GN)),
	GN1 is GN + 1,
	assert(wordobservations(Group,GN1)),
	% Go on with the next mapping
	update_lb(Agent,MappingList,UpdateVal).


% Decrease the wc/4 mapping usage stats by 1 for every mapping that was previously known by the deleted agent
decrease_wc_l([]).
decrease_wc_l([[]]).
decrease_wc_l([[Meaning,Form]|Mappings]) :-
	retract(wc(Meaning,Form,C,L)),
	% If no agent is using the mapping anymore it belongs fully forgotten
	(
		L =< 1
	;
		L1 is L - 1,
		assert(wc(Meaning,Form,C,L1))
	),
	decrease_wc_l(Mappings).


%% Remove a specific mapping if it doesn't match with the 2nd one
% Agent deletes the mapping
revise_lb(Agent,_,[Meaning,Form],0) :-
	retract(agentwc(Agent,_,Meaning,Form,_)),
	%report(['Retracting:',agentwc(Agent,_,Meaning,Form,_)]),
	decrease_wc_l([[Meaning,Form]]).

% Same form with whatever meanings - LBs remain unchanged
revise_lb(_,_,[_,Form],[_,Form]) :- !.

% Different mappings - remove both
revise_lb(Speaker,Hearer,[MeaningS,FormS],[MeaningH,FormH]) :-
	% Remove for speaker
	retract(agentwc(Speaker,_,MeaningS,FormS,_)),
	%report(['Retracting:',agentwc(Speaker,_,MeaningS,FormS,_)]),
	decrease_wc_l([[MeaningS,FormS]]),
	% Remove for hearer
	retract(agentwc(Hearer,_,MeaningH,FormH,_)),
	%report(['Retracting:',agentwc(Hearer,_,MeaningH,FormH,_)]),
	decrease_wc_l([[MeaningH,FormH]]).
