%%
%% TCD Language Evolution Workbench
%%
%% batch_run.pl
%% Control functions for running the simulations
%%

:- compile(system).
:- compile(tools).

%% Launcher. Generates directories with timestamps for dumps, graphs, logs
runbatch(Goes) :-
  % Generate directory path
	datime(datime(Year,Month,Day,Hour,Min,Sec)),
	(Month < 10 -> joinstrings(['0',Month],MonthStr); MonthStr=Month),
	(Day < 10 -> joinstrings(['0',Day],DayStr); DayStr=Day),
	(Hour < 10 -> joinstrings(['0',Hour],HourStr); HourStr=Hour),
	(Min < 10 -> joinstrings(['0',Min],MinStr); MinStr=Min),
	(Sec < 10 -> joinstrings(['0',Sec],SecStr); SecStr=Sec),
	joinstrings([Year,MonthStr,DayStr],'-',DateStr),
	joinstrings([HourStr,MinStr,SecStr],'-',TimeStr),
	joinstrings(['../../data/',DateStr,'_',TimeStr,'/'],WriteDir),
	joinstrings([WriteDir,'dumps/'],DumpDir),
	joinstrings([WriteDir,'logs/'],LogDir),
	% Create directory structure
	os_mkdir(WriteDir),
	os_mkdir(DumpDir),
	os_mkdir(LogDir),
	% Launch simulation
	batch(Goes,WriteDir).

%% Runner. Iterates through batches and performs all the post-processing operations
batch(0,_).
batch(Goes,WriteDir) :-
	% Set random seed
	datime(datime(_,Month,Day,Hour,Min,Sec)),
	Seed is (Month*Day*(Hour+1)*(Min+1)*(Sec+1)),
	%Seed is (4*10*(16+1)*(25+1)*(4+1)),
	SeedX is (Seed mod 30268) + 1,
	SeedY is (Seed mod 30306) + 1,
	SeedZ is (Seed mod 30322) + 1,
	setrand(random(SeedX,SeedY,SeedZ,Seed)),
	% Generate full file paths for dumps and graphs
	joinstrings([WriteDir,'dumps/run','-',Goes,'.dmp'],RunDumpPath),
	joinstrings([WriteDir,'dumps/run','-',Goes,'.lex'],LexDumpPath),
	joinstrings([WriteDir,'dumps/run','-',Goes,'-detailed.lex'],AgentLexDumpPath),
	joinstrings([WriteDir,'logs/run','-',Goes,'.log'],RunLogPath),
	% Run the simulation
	joinstrings([WriteDir,'dumps/subdumps-',Goes,'/'],SubDumpPath),
	os_mkdir(SubDumpPath),
	doit(RunDumpPath,RunLogPath,SubDumpPath),
	% Plot some temporary graphs
	dump_lexicon(LexDumpPath),
	dump_agent_lexicon(AgentLexDumpPath),
	% Proceed to next run
	RemainingGoes is Goes - 1,!,
	batch(RemainingGoes,WriteDir).

%% Some recent configurations
% 10 agents in 10 groups, use hs_turns/hs_rand to simulate Garrod's setups
% GroupRatios = [1,1,1,1,1,1,1,1,1,1],
%	GroupInteractions = [[0,1,0,0,0,0,0,0,0,0],[1,0,0,0,0,0,0,0,0,0],[0,0,0,1,0,0,0,0,0,0],[0,0,1,0,0,0,0,0,0,0],[0,0,0,0,0,1,0,0,0,0],[0,0,0,0,1,0,0,0,0,0],[0,0,0,0,0,0,0,1,0,0],[0,0,0,0,0,0,1,0,0,0],[0,0,0,0,0,0,0,0,0,1],[0,0,0,0,0,0,0,0,1,0]],

%% Start the actual simulation
doit(RunDump,RunLog,SubDumpPath) :-
	Agents = 10, %% Number of starting agents
	SpeakerSelect = ss_rand, %% Options: ss_rand, ss_turns
	HearerSelect = hs_rand, %% Options: hs_turns is the only one to have any effect
	SelfTalk = 0, %% Turn self-talk on/off
	InteractionType = int_dia, %% Options: int_dia stands for normal 1-1 dialogue, int_mono for 1-all monologue
	GroupSizeRatios = [1,1,1,1,1,1,1,1,1,1],
	GroupSpeakRatios = [1,1,1,1,1,1,1,1,1,1],
	GroupIntRatios = [[0,8,3,3,3,3,3,3,3,3],[8,0,3,3,3,3,3,3,3,3],[3,3,0,8,3,3,3,3,3,3],[3,3,8,0,3,3,3,3,3,3],[3,3,3,3,0,8,3,3,3,3],[3,3,3,3,8,0,3,3,3,3],[3,3,3,3,3,3,0,8,3,3],[3,3,3,3,3,3,8,0,3,3],[3,3,3,3,3,3,3,3,0,8],[3,3,3,3,3,3,3,3,8,0]],
	%GroupIntRatios = [[0,1,1,1,1,1,1,1,1,1],[1,0,1,1,1,1,1,1,1,1],[1,1,0,1,1,1,1,1,1,1],[1,1,1,0,1,1,1,1,1,1],[1,1,1,1,0,1,1,1,1,1],[1,1,1,1,1,0,1,1,1,1],[1,1,1,1,1,1,0,1,1,1],[1,1,1,1,1,1,1,0,1,1],[1,1,1,1,1,1,1,1,0,1],[1,1,1,1,1,1,1,1,1,0]],
	InfluenceRatios = 1, %% QUESTION: Is influence = 10 equivalent to influence = 1 with no updates?
	OverHearers = 0, %% (TODO: Number of overhearers) Is an overhearer present at any given interaction
	OverHearerSuccess = ohs_self, %% Options: ohs_self, ohs_hearer
	Phonemes = 410,
	Entities = 25, %% How many entities to generate
	EventTypes = [1,2,3], %% Currently supported: 1..3
	Events = 10,
	EventInstances = 50,
	Epochs = 500, %% Number of epochs in a batch
	EpochFactor = 1, %% Factor of the epoch length, multiplied by agent count for normalization
	EpochSpan = 50, %% Number of epochs in one 'span' - used for agent addition/removal and forgetting. Make sure Epochs mod EpochSpan = 0.
	StatSpan = 50, %% Same as above, but used for stats only. Make sure Epochs mod StatSpan = 0.
	GroupStats = gs_on, %% Options: gs_on/off (TODO: Group summary can lead to 0 error with 1 agent!)
	FgtFunction = fgt_none, %% Options: fgt_none, fgt_const, fgt_lin, fgt_poly
	FgtFactor = 1.0, %% Use 1.0 for fgt_const and fgt_lin; 2.0 for fgt_poly
	FgtOffset = 99, %% Higher levels will result in slower forgetting
	SuccessMatters = 0, %% If 1, only interactions reaching a certain success level will be recorded
	MinSuccess = 0, %% The minimum level of success required for the preceding parameter
	LexUpdate = upd_dyn_rel, %% Options: upd_none, upd_stat_fixed, upd_dyn_fixed, upd_dyn_rel
	LexUpdateThres = 0.35, %% The level of success considered as minimum required for the preceding parameter
	LexUpdateRate = 1, %% The -eta- value for lexicon update.
	IntRatioUpdate = upd_none, %% Options: upd_none, upd_stat_fixed, upd_dyn_fixed, upd_dyn_rel
	IntRatioUpdateThres = 0, %% The level of success considered as minimum required for the preceding parameter
	IntRatioUpdateRate = 0, %% The -eta- value for interaction ratio update.
	InfluenceUpdate = upd_none, %% Options: upd_none, upd_stat_fixed, upd_dyn_fixed, upd_dyn_rel
	InfluenceUpdateThres = 0, %% The level of success considered as minimum required for the preceding parameter
	InfluenceUpdateRate = 0, %% The -eta- value for influence update.
	Questions = 0,
	AskSyn = 0, %% If 1, hearer will ask for alternative forms if he can't find a meaning
	SegSync = seg_sync, %% Options: seg_sync, seg_rand, seg_wrong
	InterpRule = interp_rand, %% Options: interp_omni, interp_contrast, interp_rand, interp_wrong
	SelfSegSync = seg_sync, %% Options: seg_sync, seg_rand, seg_wrong
	SelfInterpRule = interp_rand, %% Options: interp_omni, interp_contrast, interp_rand, interp_wrong
	MemTendency = f, %% Options: f, r
	InterpEventRole = event_ignore, %% Options: event_ignore, event_pref, event_only
	Zipfism = zoff, %% Options: zon, zoff
	AgentAdd = aoff, %% Options: aon, aoff
	NewAgents = 1, %% Max number of agents to add if addition selected
	AgentDel = doff, %% Options: don, doff
	Fit = fon, %% Options: fon, foff
	FitThres = 0.5,
	AgeRange = 800,
	RandomForm = 0, %% How often to generate a new form, regardless if one is known for the given meaning
	ForceSeg = 0, %% 0 to allow holistic 'segments'; 1 to enforce at least some segmentation
	PrintTrace = notrace, %% Options: trace, notrace
	doall(Agents,SpeakerSelect,HearerSelect,SelfTalk,InteractionType,GroupSizeRatios,GroupSpeakRatios,GroupIntRatios,InfluenceRatios,
	OverHearers,OverHearerSuccess,Phonemes,Entities,EventTypes,Events,EventInstances,Epochs,EpochFactor,EpochSpan,StatSpan,GroupStats,FgtFunction,FgtFactor,FgtOffset,
	SuccessMatters,MinSuccess,LexUpdate,LexUpdateThres,LexUpdateRate,IntRatioUpdate,IntRatioUpdateThres,IntRatioUpdateRate,
	InfluenceUpdate,InfluenceUpdateThres,InfluenceUpdateRate,
	Questions,AskSyn,SegSync,InterpRule,SelfSegSync,SelfInterpRule,MemTendency,InterpEventRole,Zipfism,
	AgentAdd,NewAgents,AgentDel,Fit,FitThres,AgeRange,RandomForm,ForceSeg,RunDump,RunLog,SubDumpPath,PrintTrace).
	
:- runbatch(500).
