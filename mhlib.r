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
  cat('\no------------------------------------------------------o')
  cat('\n| Welcome to the Hungarian Phoneme Perceptual Trainer! |')
  cat('\no------------------------------------------------------o')
  cat("\n\nPress [enter] to begin (or use 'ctrl+z' to exit at any time).")
  invisible(readLines("stdin",n=1))

  cat('\nThe letters to be used in the following exercises are listed',
      '\nbelow. You may use either the ascii or special characters to',
      '\nrespond to the prompt.\n\n')
  print(character_map)

  cat("\nPress [enter] to begin the trials.")
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
