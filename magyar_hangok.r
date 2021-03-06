
source('mhlib.r')

#Change default audio player for macs
if(get_os() == 'osx') {
    setWavPlayer('/usr/bin/afplay')
}

#Get Sound Files and Characters
char = get_character_map()
print_intro()
custom_chars = custom_char_set(char$lookup)
print_character_map(character_map=char$map,subset=custom_chars)

sound_files = list.files('sounds')

#initialize log df
log = data.frame()

#Loop over stimuli
for(i in sample(1:length(sound_files))) {

  actual_letter = letter_from_file(sound_files[i])
  if(!anyNA(custom_chars) & !(char$lookup[actual_letter] %in% custom_chars)){
    next
  }
  cat('...playing sound... What sound do you hear? Letter: ')
  s = readMP3(file.path('sounds',sound_files[i]))
  play(s)

  user_letter = get_user_letter(char$all)
  perf = check_accuracy(user_letter,actual_letter,char)

  log = rbind(log,perf)
  cat("\nPress [enter] to continue.")
  invisible(readLines("stdin",n=1))
}

cat('\n\nTrials Complete!')

names(log) = c('file','actual','response')
log[] = lapply(log, as.character)
log['correct'] = log['actual'] == log['response']

if(anyNA(custom_chars)){
  cat('\nSave results to log file (y/n)?')
  write_to_log = invisible(readLines("stdin",n=1))

  if( write_to_log != 'n') {
    if(!is.element('performance.csv',list.files())) {
      log['date'] = format(Sys.Date())
      log['ts'] = format(Sys.time())
      header <- paste(names(log),collapse=',')
      write.table(header, file='performance.csv', row.names=FALSE, col.names=FALSE, quote=FALSE)
    }
    write.table(log, file='performance.csv', row.names=FALSE, col.names=FALSE, append=TRUE, sep=",")
  }
}

cat(paste('\n\nPercent Accuracy:',100*sum(log$correct)/(dim(log)[1])))
cat("\nMotivational message! bleh blah good job / try harder :p :)\n \n")
