import sys, os, re
from lew_analysis_tools import processSettingsLine

data_dirs = []
out_file = None
stages = 1

argv = sys.argv[1:]
for arg in argv:
  try:
    if arg == '--dir':
      flagindex = argv.index(arg)
      rest = argv[flagindex+1:]
      for restarg in rest:
        if re.search('--', restarg):
          break
        data_dirs.append(restarg)
    if arg == '--out':
      flagindex = argv.index(arg)
      out_file = argv[flagindex+1]
    if arg == '--stages':
      flagindex = argv.index(arg)
      stages = int(argv[flagindex+1])
  except:
    break

if not data_dirs or not out_file:
  print "Please run the program using the following format:"
  print "python SCRIPTNAME.py --dir data_directory_with_timestamp1 ddwt2 ddwt3.. --out summary_filename [--stages x]"
  sys.exit()

out = open(out_file, 'w')
out.write('idx stage agents epochs epochfactor speakerselect hearerselect selftalk interactiontype groups groupsizeratios groupspeakratios groupintratios influenceratios overhearers overhearersuccess entities eventtypes events eventinstances qlevel asksyn segsync interprule selfsegsync rf eventrole agentadd agentdel fit fitthres agerange randomform forceseg fgtfunction fgtfactor fgtoffset successmatters minsuccess lexupdate lexupdatethres lexupdaterate intratioupdate intratioupdatethres intratioupdaterate influenceupdate influenceupdatethres influenceupdaterate runidx Group GroupAgents AgentHomPs AgentSynPs AvgLBLen AvgUniqueMeanings AvgUniqueForms MeaningPrecisionAvg MeaningRecallAvg MeaningF1Avg LBUseAvg LBPrecisionAvg W AvgMappingShare SharedMappingRatio SharedMeaningRatio SharedFormRatio Innovations NewForms Forgotten LogPopulationChange GlobalHomPs GlobalSynPs GlLBLen GlMeanings GlForms GIRatios\n')
i = 1

for data_dir in data_dirs:
  print "processing directory " + data_dir
  data_dir_list = os.listdir(data_dir)
  if 'dumps' not in data_dir_list:
    print "The directory " + data_dir + " doesn't seem to contain a 'dumps' directory to read data from. Please check your input and try again."
    sys.exit()
  
  if 'logs' not in data_dir_list:
    print "The directory " + data_dir + " doesn't seem to contain a 'logs' directory to read data from. Please check your input and try again."
    sys.exit()
  
  logs_dir = data_dir+'/logs'
  dumps_dir = data_dir+'/dumps'
  dumps_dir_list = os.listdir(dumps_dir)
  dumps_dir_list.sort()
  
  for filename in dumps_dir_list:
    if not re.search('.dmp$', filename):
      continue
    
    runname = '.'.join(filename.split('.')[:-1])
    iteration = runname.split('-')[1]
    settingsline = os.popen('head -1 '+logs_dir+'/run-'+iteration+'.log').readline().strip().strip('\[\]').split(';')
    settings = processSettingsLine(settingsline)
    
    try:
      agents,speakerselect,hearerselect,selftalk,interactiontype,groupsizeratios,groupspeakratios,groupintratios,influenceratios, overhearers,overhearersuccess,phonemes,entities,eventtypes,events,eventinstances,epochs,epochfactor,epochspan,statspan,groupstats, fgtfunction,fgtfactor,fgtoffset,successmatters,minsuccess,lexupdate,lexupdatethres,lexupdaterate, intratioupdate,intratioupdatethres,intratioupdaterate,influenceupdate,influenceupdatethres,influenceupdaterate,qlevel,asksyn,segsync,interprule, selfsegsync,selfinterprule,rf,eventrole,zipfism,agentadd,newagents,agentdel,fit,fitthres,agerange,randomform,forceseg = settings
    except:
      try:
        agents,speakerselect,hearerselect,selftalk,interactiontype,groupsizeratios,groupspeakratios,groupintratios,influenceratios, overhearers,overhearersuccess,phonemes,entities,eventtypes,events,epochs,epochfactor,epochspan,statspan,groupstats, fgtfunction,fgtfactor,fgtoffset,successmatters,minsuccess,lexupdate,lexupdatethres,lexupdaterate, intratioupdate,intratioupdatethres,intratioupdaterate,influenceupdate,influenceupdatethres,influenceupdaterate,qlevel,asksyn,segsync,interprule, selfsegsync,selfinterprule,rf,eventrole,zipfism,agentadd,newagents,agentdel,fit,fitthres,agerange,randomform,forceseg = settings
      except:
        try:
          agents,speakerselect,hearerselect,selftalk,interactiontype,groupsizeratios,groupspeakratios,groupintratios,influenceratios, overhearers,overhearersuccess,phonemes,entities,events,epochs,epochfactor,epochspan,statspan,groupstats, fgtfunction,fgtfactor,fgtoffset,successmatters,minsuccess,lexupdate,lexupdatethres,lexupdaterate, intratioupdate,intratioupdatethres,intratioupdaterate,influenceupdate,influenceupdatethres,influenceupdaterate,qlevel,asksyn,segsync,interprule, selfsegsync,selfinterprule,rf,eventrole,zipfism,agentadd,newagents,agentdel,fit,fitthres,agerange,forceseg = settings
        except:
          try:
            agents,speakerselect,hearerselect,selftalk,interactiontype,groupsizeratios,groupspeakratios,groupintratios,influenceratios, overhearers,overhearersuccess,phonemes,entities,events,epochs,epochfactor,epochspan,statspan,groupstats, fgtfunction,fgtfactor,fgtoffset,successmatters,minsuccess,lexupdate,lexupdaterate,updatethres, intratioupdate,intratioupdaterate,influenceupdate,influenceupdaterate,qlevel,asksyn,segsync,interprule, selfsegsync,selfinterprule,rf,eventrole,zipfism,agentadd,newagents,agentdel,fit,fitthres,agerange,forceseg = settings
          except:
            try:
              agents,speakerselect,hearerselect,groupsizeratios,groupspeakratios,groupintratios,influenceratios,overhearers,overhearersuccess, phonemes,entities,events,epochs,epochfactor,epochspan,statspan,groupstats,fgtfunction,fgtfactor,fgtoffset,successmatters,minsuccess, lexupdate,lexupdaterate,updatethres,intratioupdate,intratioupdaterate,influenceupdate,influenceupdaterate,qlevel,asksyn,segsync,interprule, selfsegsync,selfinterprule,rf,eventrole,zipfism,agentadd,newagents,agentdel,fit,fitthres,agerange,forceseg = settings
            except:
              try:
                agents,speakerselect,hearerselect,groupsizeratios,groupspeakratios,groupintratios,influenceratios,overhearers,overhearersuccess, phonemes,entities,events,epochs,epochfactor,epochspan,statspan,groupstats,fgtfunction,fgtfactor,fgtoffset,successmatters,minsuccess, lexupdate,updatethres,intratioupdate,influenceupdate,qlevel,asksyn,segsync,interprule,selfsegsync,selfinterprule,rf,eventrole,zipfism,agentadd, newagents,agentdel,fit,fitthres,agerange,forceseg = settings
              except:
                try:
                  agents,speakerselect,hearerselect,groupsizeratios,groupspeakratios,groupintratios,overhearers,overhearersuccess,phonemes,entities,events, epochs,epochfactor,epochspan,statspan,fgtfunction,fgtfactor,fgtoffset,successmatters,minsuccess,lexupdate,updatethres,qlevel,asksyn, segsync,interprule,selfsegsync,selfinterprule,rf, eventrole,zipfism,agentadd,newagents,agentdel,fit,fitthres,agerange,forceseg = settings
                except:
                  agents,speakerselect,hearerselect,groupsizeratios,groupspeakratios,groupintratios,overhearers,phonemes,entities,events, epochs,epochfactor,epochspan,statspan,fgtfunction,fgtfactor,fgtoffset,successmatters,minsuccess,lexupdate,updatethres,qlevel,asksyn, segsync,interprule,selfsegsync,selfinterprule,rf, eventrole,zipfism,agentadd,newagents,agentdel,fit,fitthres,agerange,forceseg = settings
                  overhearersuccess = 'ohs_self'
                influenceratios = '1'
                groupstats = 'gs_on'
                intratioupdate = '0'
                influenceupdate = '0'
              lexupdaterate = '1.0'
              intratioupdaterate = '1.0'
              influenceupdaterate = '1.0'
            selftalk = '1'
            interactiontype = 'int_dia'
          lexupdatethres = updatethres
          intratioupdatethres = updatethres
          influenceupdatethres = updatethres
        eventtypes = '0-1-2-3-4-5-6'
        randomform = 'N'
      eventinstances = '0'
    
    if lexupdate == '0':
      lexupdate = 'upd_stat_fixed'
    
    gr = groupsizeratios.split('-')
    groups = len(gr)
    statgroups = 1
    if groupstats == 'gs_on' and groups > 1:
      statgroups = groups + 1
    
    if agentadd == 'aoff': agentadd = 'AddNone'
    else: agentadd = 'AddAgents'
    
    if qlevel == '0': questions = 'ZeroQs'
    else: questions = qlevel + 'IntervalQs'
    
    if forceseg == '0': forceseg = 'NoForceSeg'
    else: forceseg = 'ForceSeg'
    
    dumpfile = open(dumps_dir+'/'+filename, 'r')
    dumpdata = dumpfile.readlines()
    if not dumpdata:
      print "dumpfile " + dumps_dir+'/'+filename + " empty, did something go wrong?"
      continue
    
    dumplines = len(dumpdata)
    stagespan = float(dumplines)/stages
    
    prevstagestats = {}
    for gi in range(groups):
      prevstagestats[gi] = {}
    newprevstagestats = {}
    
    for stage in range(1, stages+1):
      for gi in range(statgroups):
        stageindex = int(stagespan*stage)-1-gi
        statline = dumpdata[stageindex]
        statlinesplit = statline.split()
        stagestats = []
        
        newprevstagestats[gi] = {}
        for k in range(len(statlinesplit)):
          subTotalThisStage = statlinesplit[k]
          if k not in (0,26):
            subTotalThisStage = float(subTotalThisStage)
          if stage > 1:
            try:
              subTotalPrevStage = prevstagestats[gi][k]
              if k not in (0,26):
                subTotalPrevStage = float(subTotalPrevStage)
            except:
              print dumpfile
            #For lexicon precision we need to consider lexicon use as well
            if k == 11:
              lbuseprev = float(prevstagestats[gi][10])
              lbusethis = float(statlinesplit[10])
              wordsprev = int(prevstagestats[gi][12])
              wordsthis = int(statlinesplit[12])
              lbintsprevtotal = lbuseprev*wordsprev
              lbintsthistotal = lbusethis*wordsthis
              lbintsstage = lbintsthistotal - lbintsprevtotal
              if lbintsstage > 0:
                thisStageValue = (subTotalThisStage*lbintsthistotal - subTotalPrevStage*lbintsprevtotal)/lbintsstage
              else:
                thisStageValue = subTotalThisStage
            #For percentages and other averages, can print the rate over the last stage
            elif k in (7,8,9,10):
              thisStageValue = subTotalThisStage*stage - subTotalPrevStage*(stage-1)
            #For most just print the current value
            else:
              thisStageValue = subTotalThisStage
          else:
            thisStageValue = subTotalThisStage
          newprevstagestats[gi][k] = subTotalThisStage
          stagestats.append(str(thisStageValue))
        prevstagestats[gi] = newprevstagestats[gi]
        
        statline = ' '.join(stagestats)
        
        out.write(str(i)+' '+str(stage)+' ')
        out.write(agents+' '+epochs+' '+epochfactor+' '+speakerselect+' '+hearerselect+' '+selftalk+' '+interactiontype+' '+str(groups)+' '+groupsizeratios+' '+groupspeakratios+' '+groupintratios+' '+influenceratios+' '+overhearers+' '+overhearersuccess+' '+entities+' '+eventtypes+' '+events+' '+eventinstances+' '+questions+' '+asksyn+' '+segsync+' '+interprule+' '+selfsegsync+' '+rf+' '+eventrole+' '+agentadd+' '+agentdel+' '+fit+' '+fitthres+' '+agerange+' '+randomform+' '+forceseg+' '+fgtfunction+' '+fgtfactor+' '+fgtoffset+' '+successmatters+' '+minsuccess+' '+lexupdate+' '+lexupdatethres+' '+lexupdaterate+' '+intratioupdate+' '+intratioupdatethres+' '+intratioupdaterate+' '+influenceupdate+' '+influenceupdatethres+' '+influenceupdaterate+' '+iteration+' ')
        out.write(statline+'\n')
    
    i += 1

out.close()
