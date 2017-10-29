#Libs
library(tuneR)

#Constants
VOWELS = c(
  'a', 'a_accent',
  'e', 'e_accent',
  'i', 'i_accent',
  'o', 'o_accent','o_umlaut','o_umlaut_accent',
  'u', 'u_accent','u_umlaut','u_umlaut_accent'
  )
EQUIV = c('y','ly') #These are actually phoenetically equivalent?

#Source: https://www.r-bloggers.com/identifying-the-os-from-r/
get_os <- function(){
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
  cat(letter_set)
  cat("\n\n    Press [enter] to begin (or 'ctrl+z' to exit).")
  invisible(readLines("stdin",n=1))
}

letter_from_file <- function(sound_files) {
  letter_set = unique(gsub('_[0-9].mp3','',sound_files))
  return(letter_set)
}

#Change default audio player for macs
if(get_os() == 'osx') {
    setWavPlayer('/usr/bin/afplay')
}


#Get Sound Files and Letters
sound_files = list.files('sounds')
letter_set = letter_from_file(sound_files)

print_intro()

for(i in sample(1:length(sound_files))) {

  cat('...playing sound...')
  s = readMP3(file.path('sounds',sound_files[i]))
  play(s)

  cat("Which sound did you here? Letter: ")
  user_letter = invisible(readLines("stdin",n=1))
  actual_letter = letter_from_file(sound_files[i])
  cat(ifelse(user_letter==actual_letter,
             'Correct!',
             paste('Oops, shoud be:',actual_letter)))
  cat("\nPress [enter] when ready for next sound!")
  invisible(readLines("stdin",n=1))
}
