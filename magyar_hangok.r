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

print_intro <- function() {
  cat('\nWelcome to your Hungarian Perceptual Trainer!
    The available letters are listed below. Use these letters/notations to
    identify your selections at the prompt.\n\n')
  character_map = read.csv('character_map.csv')
  print(character_map)
  cat("\n\n    Press [enter] to begin (or 'ctrl+z' to exit).")
  invisible(readLines("stdin",n=1))
}

letter_from_file <- function(sound_files) {
  letter_set = unique(gsub('_[0-9].mp3','',sound_files))
  return(letter_set)
}

get_user_letter <- function() {
  cat("Which sound did you here? Letter: ")
  user_letter = invisible(readLines("stdin",n=1))
  return(user_letter)
}

#Change default audio player for macs
if(get_os() == 'osx') {
    setWavPlayer('/usr/bin/afplay')
}

#Get Sound Files and Letters
sound_files = list.files('sounds')
letter_set = letter_from_file(sound_files)

print_intro()

#initialize log df
log = data.frame()
for(i in sample(1:length(sound_files))) {

  cat('...playing sound...')
  s = readMP3(file.path('sounds',sound_files[i]))
  play(s)

  user_letter = get_user_letter()
  while(!is.element(user_letter,letter_set)) {
    'Character not in letter set, try again please.'
    user_letter = get_user_letter()
  }

  actual_letter = letter_from_file(sound_files[i])
  correct = user_letter==actual_letter
  cat(ifelse(correct,'Correct!',
             paste('Oops, shoud be:',actual_letter)))
  perf = matrix(c(sound_files[i],
                  actual_letter,
                  user_letter,
                  as.numeric(correct)),nrow=1)
  log = rbind(log,perf)
  cat("\nPress [enter] when ready for next sound!")
  invisible(readLines("stdin",n=1))
  break
}

names(log) = c('file','letter','response','correct')
log['date'] = format(Sys.Date())
log['ts'] = format(Sys.time())

cat('Save results to log file (y/n)?')
write_to_log = invisible(readLines("stdin",n=1))
if( !(write_to_log == 'n')) {
  write.csv(log,file='performance.csv',append=TRUE)
}
print('ok... and ... ???')
cat(paste('Accuracy:',sum(log$correct)/length(log)))
cat("Motivational message... blah blah... yay! keep going!")
