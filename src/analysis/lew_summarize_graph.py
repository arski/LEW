import sys, os, re
from lew_analysis_tools import processSettingsLine

data_dirs = []
out_file = None
stages = 1
wthres = None

argv = sys.argv[1:]
for arg in argv:
  try:
    if arg == '--wthres':
      flagindex = argv.index(arg)
      wthres = float(argv[flagindex+1])
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

if not wthres or not data_dirs or not out_file:
  print "Please run the program using the following format:"
  print "python SCRIPTNAME.py --wthres weight_thres --dir data_directory_with_timestamp1 ddwt2 ddwt3.. --out summary_filename [--stages x]"
  sys.exit()

out = open(out_file, 'w')
#out.write('idx stage agents epochs speakerselect hearerselect selftalk interactiontype groups groupsizeratios groupspeakratios groupintratios influenceratios overhearers overhearersuccess qlevel asksyn segsync interprule selfsegsync rf eventrole agentadd agentdel fit fitthres agerange forceseg fgtfunction fgtfactor fgtoffset successmatters minsuccess lexupdate lexupdatethres lexupdaterate intratioupdate intratioupdatethres intratioupdaterate influenceupdate influenceupdatethres influenceupdaterate runidx i j w\n')
out.write('idx stage agents epochs speakerselect hearerselect selftalk interactiontype groups influenceratios overhearers overhearersuccess qlevel asksyn segsync interprule selfsegsync rf eventrole agentadd agentdel fit fitthres agerange forceseg fgtfunction fgtfactor fgtoffset successmatters minsuccess lexupdate lexupdatethres lexupdaterate intratioupdate intratioupdatethres intratioupdaterate influenceupdate influenceupdatethres influenceupdaterate runidx i reldegree absdegree wthres\n')
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
        agents,speakerselect,hearerselect,selftalk,interactiontype,groupsizeratios,groupspeakratios,groupintratios,influenceratios, overhearers,overhearersuccess,phonemes,entity,events,epochs,epochfactor,epochspan,statspan,groupstats, fgtfunction,fgtfactor,fgtoffset,successmatters,minsuccess,lexupdate,lexupdatethres,lexupdaterate, intratioupdate,intratioupdatethres,intratioupdaterate,influenceupdate,influenceupdatethres,influenceupdaterate,qlevel,asksyn,segsync,interprule, selfsegsync,selfinterprule,rf,eventrole,zipfism,agentadd,newagents,agentdel,fit,fitthres,agerange,forceseg = settings
      except:
        try:
          agents,speakerselect,hearerselect,selftalk,interactiontype,groupsizeratios,groupspeakratios,groupintratios,influenceratios, overhearers,overhearersuccess,phonemes,entity,events,epochs,epochfactor,epochspan,statspan,groupstats, fgtfunction,fgtfactor,fgtoffset,successmatters,minsuccess,lexupdate,lexupdaterate,updatethres, intratioupdate,intratioupdaterate,influenceupdate,influenceupdaterate,qlevel,asksyn,segsync,interprule, selfsegsync,selfinterprule,rf,eventrole,zipfism,agentadd,newagents,agentdel,fit,fitthres,agerange,forceseg = settings
        except:
          try:
            agents,speakerselect,hearerselect,groupsizeratios,groupspeakratios,groupintratios,influenceratios,overhearers,overhearersuccess, phonemes,entity,events,epochs,epochfactor,epochspan,statspan,groupstats,fgtfunction,fgtfactor,fgtoffset,successmatters,minsuccess, lexupdate,lexupdaterate,updatethres,intratioupdate,intratioupdaterate,influenceupdate,influenceupdaterate,qlevel,asksyn,segsync,interprule, selfsegsync,selfinterprule,rf,eventrole,zipfism,agentadd,newagents,agentdel,fit,fitthres,agerange,forceseg = settings
          except:
            try:
              agents,speakerselect,hearerselect,groupsizeratios,groupspeakratios,groupintratios,influenceratios,overhearers,overhearersuccess, phonemes,entity,events,epochs,epochfactor,epochspan,statspan,groupstats,fgtfunction,fgtfactor,fgtoffset,successmatters,minsuccess, lexupdate,updatethres,intratioupdate,influenceupdate,qlevel,asksyn,segsync,interprule,selfsegsync,selfinterprule,rf,eventrole,zipfism,agentadd, newagents,agentdel,fit,fitthres,agerange,forceseg = settings
            except:
              try:
                agents,speakerselect,hearerselect,groupsizeratios,groupspeakratios,groupintratios,overhearers,overhearersuccess,phonemes,entity,events, epochs,epochfactor,epochspan,statspan,fgtfunction,fgtfactor,fgtoffset,successmatters,minsuccess,lexupdate,updatethres,qlevel,asksyn, segsync,interprule,selfsegsync,selfinterprule,rf, eventrole,zipfism,agentadd,newagents,agentdel,fit,fitthres,agerange,forceseg = settings
              except:
                agents,speakerselect,hearerselect,groupsizeratios,groupspeakratios,groupintratios,overhearers,phonemes,entity,events, epochs,epochfactor,epochspan,statspan,fgtfunction,fgtfactor,fgtoffset,successmatters,minsuccess,lexupdate,updatethres,qlevel,asksyn, segsync,interprule,selfsegsync,selfinterprule,rf, eventrole,zipfism,agentadd,newagents,agentdel,fit,fitthres,agerange,forceseg = settings
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
    
    gr = groupsizeratios.split('-')
    groups = len(gr)
    
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
    
    for stage in range(1, stages+1):
      stageindex = int(stagespan*stage)-1
      statline = dumpdata[stageindex]
      statlinesplit = statline.split()
      
      giratios = [giratio.strip('\[\]') for giratio in statlinesplit[26].split('],[')]
      
      myratios = {}
      for giratio in giratios:
        gi,gj,gw = giratio.split(',')
        gw = float(gw)
        if (gw < 0): gw = 0.0
        
        if gi == gj: continue
        
        if gi not in myratios:
          myratios[gi] = {}
          myratios[gi]['targets'] = {}
          myratios[gi]['sum'] = 0.0
          myratios[gi]['deg'] = 0
        
        myratios[gi]['targets'][gj] = gw
        myratios[gi]['sum'] += gw
      
      mydegrees = {}
      for ggi in myratios.keys():
        for ggj in myratios[ggi]['targets'].keys():
          if myratios[ggi]['sum'] > 0:
            ggw_rel = round(myratios[ggi]['targets'][ggj]/myratios[ggi]['sum'], 2)
          else:
            ggw_rel = 0.0
            
          if (ggw_rel < wthres): continue
          
          myratios[ggi]['deg'] += 1 
      
      for ggi in myratios.keys():
        # reldegree is the number of connections that are above the wthres
        # absdegree is the total weight of connections (useless)
        statline = ' '.join([ggi,str(myratios[ggi]['deg']),str(myratios[ggi]['sum']),str(wthres)])
      
        out.write(str(i)+' stage'+str(stage)+' ')
        #out.write(agents+' '+epochs+' '+speakerselect+' '+hearerselect+' '+selftalk+' '+interactiontype+' '+str(groups)+' '+groupsizeratios+' '+groupspeakratios+' '+groupintratios+' '+influenceratios+' '+overhearers+' '+overhearersuccess+' '+questions+' '+asksyn+' '+segsync+' '+interprule+' '+selfsegsync+' '+rf+' '+eventrole+' '+agentadd+' '+agentdel+' '+fit+' '+fitthres+' '+agerange+' '+forceseg+' '+fgtfunction+' '+fgtfactor+' '+fgtoffset+' '+successmatters+' '+minsuccess+' '+lexupdate+' '+lexupdatethres+' '+lexupdaterate+' '+intratioupdate+' '+intratioupdatethres+' '+intratioupdaterate+' '+influenceupdate+' '+influenceupdatethres+' '+influenceupdaterate+' '+iteration+' ')
        out.write(agents+' '+epochs+' '+speakerselect+' '+hearerselect+' '+selftalk+' '+interactiontype+' '+str(groups)+' '+influenceratios+' '+overhearers+' '+overhearersuccess+' '+questions+' '+asksyn+' '+segsync+' '+interprule+' '+selfsegsync+' '+rf+' '+eventrole+' '+agentadd+' '+agentdel+' '+fit+' '+fitthres+' '+agerange+' '+forceseg+' '+fgtfunction+' '+fgtfactor+' '+fgtoffset+' '+successmatters+' '+minsuccess+' '+lexupdate+' '+lexupdatethres+' '+lexupdaterate+' '+intratioupdate+' '+intratioupdatethres+' '+intratioupdaterate+' '+influenceupdate+' '+influenceupdatethres+' '+influenceupdaterate+' '+iteration+' ')
        out.write(statline+'\n')
    
    i += 1

out.close()
