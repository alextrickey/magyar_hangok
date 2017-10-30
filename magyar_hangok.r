#Libs
library(tuneR)

#TODO: Enable custom letter set practice
#TODO: Allow user to control n trials

# #Constants
# VOWELS = c(
#   'a', 'a_accent',
#   'e', 'e_accent',
#   'i', 'i_accent',
#   'o', 'o_accent','o_umlaut','o_umlaut_accent',
#   'u', 'u_accent','u_umlaut','u_umlaut_accent'
#   )
# EQUIV = c('y','ly') #These are actually phoenetically equivalent?

get_os <- function(){
#Source: https://www.r-bloggers.com/identifying-the-os-from-r/
  sysinf <- Sys.info()
  if (!is.null(sysinf)){
    os <- sysinf['sysname']
    if (os == 'Darwin')
      os <- "osx"
  } else { ## mystery machine
    os <- .Platform$OS.type
    if (grepl("^darwin", R.version$os))
      os <- "osx"
    if (grepl("linux-gnu", R.version$os))
      os <- "linux"
  }
  tolower(os)
}

get_character_map <- function(subset=NA) {
  #Load mapping between ascii/special characters
  character_map = read.csv('character_map.csv',
                           stringsAsFactors=FALSE,
                           strip.white = TRUE)

  #Build a lookup table
  character_lookup = unlist(character_map['special'])
  names(character_lookup) = unlist(character_map['ascii'])

  #Get a list of all letters/formats
  all_letters = unlist(union(character_map['ascii'],character_map['special']))

  char = list('map'=character_map,'lookup'=character_lookup,'all'=all_letters)
  return(char)
}

print_intro <- function(character_map) {
  cat('\nWelcome to your Hungarian Perceptual Trainer!
    The letters to be used in the following exercises
    are listed below. You may use either the ascii or
    special characters to respond to the prompt.\n\n')
  print(character_map)
  cat("\n\n    Press [enter] to begin (or 'ctrl+z' to exit).")
  invisible(readLines("stdin",n=1))
}

letter_from_file <- function(sound_files) {
  letter_set = unique(gsub('_[0-9].mp3','',sound_files))
  return(letter_set)
}

get_user_letter <- function(letter_set) {

  user_letter = invisible(readLines("stdin",n=1))
  while(!is.element(user_letter,letter_set)) {
      cat('Unrecognized character, try again please: ')
      user_letter = invisible(readLines("stdin",n=1))
  }
  return(user_letter)
}

check_user_input <- function(user_letter,actual_letter,char) {

  #Look up non-ascii form
  actual_diacritic = char$lookup[actual_letter]
  user_diacritic = char$lookup[user_letter]

  #Check answer, give feedback
  correct = actual_diacritic == user_diacritic
  cat(ifelse(correct,'Correct!',
             paste('Oops, shoud be:',actual_diacritic)))

  #Return response for log
  perf = matrix(c(sound_files[i],
                  actual_diacritic,
                  user_diacritic),nrow=1)
  return(perf)
}

#Change default audio player for macs
if(get_os() == 'osx') {
    setWavPlayer('/usr/bin/afplay')
}

#Get Sound Files and Characters
sound_files = list.files('sounds')
char = get_character_map()
print_intro(character_map=char$map)

#initialize log df
log = data.frame()

#Loop over stimuli
for(i in sample(1:length(sound_files))) {

  cat('...playing sound... What sound do you hear? Letter: ')
  s = readMP3(file.path('sounds',sound_files[i]))
  play(s)

  user_letter = get_user_letter(char$all)
  actual_letter = letter_from_file(sound_files[i])

  perf = check_user_input(user_letter,actual_letter,char)

  log = rbind(log,perf)
  cat("\nPress [enter] to continue.")
  invisible(readLines("stdin",n=1))
}

cat('Trials Complete!')
cat('\nSave results to log file (y/n)?')

names(log) = c('file','actual','response')
log['correct'] = log['actual'] == log['response']

write_to_log = invisible(readLines("stdin",n=1))
if( !(write_to_log == 'n')) {
  if(!is.element('performance.csv',list.files())) {
    log['date'] = format(Sys.Date())
    log['ts'] = format(Sys.time())
    header <- paste(names(log),collapse=',')
    write.table(header, file='performance.csv', row.names=FALSE, col.names=FALSE, quote=FALSE)
  }
  write.table(log, file='performance.csv', row.names=FALSE, col.names=FALSE, append=TRUE, sep=",")
}

cat(paste('\n\nAccuracy:',100*sum(log$correct)/(dim(log)[1])))
cat("\n Motivational message!\n \n ")
