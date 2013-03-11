# Processes the settings line and extracts parameters properly
# Takes into consideration the complex group parameters
def processSettingsLine(settingsline):
  settings = []
  settinglist = []
  settingsublist = []
  subsetting = listsetting = False
  for setting in settingsline:
    if setting[0] == '[':
      if setting[1] == '[':
        subsettings = [subsetting.strip('\[\]').replace(',', '.') for subsetting in setting.split('],[')]
        settings.append('-'.join(subsettings))
      else:
        settings.append(setting.strip('\[\]').replace(',', '-'))
    else:
      settings.append(setting)
  
  return settings
