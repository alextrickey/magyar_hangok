#Libs
library(tuneR)

#TODO: Allow user to control n trials 

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
}


custom_char_set <- function(char_lookup) {

  cat('\nThe available sound files correspond to the following characters:\n\n')
  cat('\t')
  for(l in char_lookup){cat(l)}

  cat("\n\nWould you like to use a custom subset of these characters (y/n)? ")
  use_custom = invisible(readLines("stdin",n=1))
  while(!is.element(use_custom,c('y','n'))) {
      cat('Please respond with y or n: ')
      use_custom = invisible(readLines("stdin",n=1))
  }

  custom_chars = NA
  if(use_custom == 'y') {
      cat('\n\n')
      cat('Please list your character selection without spaces using diacritic')
      cat('\nnotation. Here are some suggested sets:')
      cat('\n Vowels: aáeéiíoöóőuüúű')
      cat('\n Short O/U Variations: oöuü')
      cat('\n Long O/U Variations: óőúű')
      cat('\n Tricky Consonants: ccsggydzdzstyzsz')
      cat('\nCharacters: ')
      selection = invisible(readLines("stdin",n=1))
      custom_chars = unique(parse_word(selection))

      #remove bad characters
      insufficient_chars = length(custom_chars)<2
      unrecognized_inputs = setdiff(custom_chars,char_lookup)
      while(insufficient_chars | length(unrecognized_inputs)>0){
        if(length(unrecognized_inputs)>0){
          cat("\nThese characters are not recognized: ")
          for(l in unrecognized_inputs){cat(l)}
        }
        if(insufficient_chars){
          cat("\nPlease enter at least two characters. ")
          for(l in unrecognized_inputs){cat(l)}
        }
        cat('\nTry Again? Characters: ')
        selection = invisible(readLines("stdin",n=1))
        custom_chars = unique(parse_word(selection))
        unrecognized_inputs = setdiff(custom_chars,char_lookup)
        insufficient_chars = length(custom_chars)<2
      }
    }
    return(custom_chars)
}


print_character_map <- function(character_map,subset=NA) {
  cat('\nThe letters to be used in the following exercises are listed',
      '\nbelow. You may use either the ascii or special characters to',
      '\nrespond to the sounds.\n\n')
  if(anyNA(subset)){
    print(character_map)
  } else {
      print(character_map[character_map$special %in% subset,])
  }
  cat("\nPress [enter] to begin trials.\n")
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


check_accuracy <- function(user_letter,actual_letter,char) {

  #Look up non-ascii form
  actual_diacritic = char$lookup[actual_letter]
  user_diacritic = ifelse(is.element(user_letter,char$lookup),
                          user_letter, char$lookup[user_letter])

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


sound_it_out <- function(word,char=NA) {

  if(is.na(char)) {
    char = get_character_map()
  }

  file_lookup = paste(unlist(char$map$ascii),'_1.mp3',sep='')
  names(file_lookup) = unlist(char$map$special)

  word_chars = parse_word(word)
  sound_files = file_lookup[word_chars]

  #Change default audio player for macs
  if(get_os() == 'osx') {
      setWavPlayer('/usr/bin/afplay')
  }

  for(f in sound_files) {
    s = readMP3(file.path('sounds',f))
    play(s)
  }
}


parse_word <- function(word,char=NA) {

  #Load character map if needed
  if(is.na(char)) {
    char = get_character_map()
  }

  #Get characters
  single_chars = unlist(strsplit(word,''))
  n_singles = length(single_chars)

  #Find and isolate multicharacter letters
  word_chars = c()
  l=1
  while (l <= n_singles) {
    for (i in 2:0) {
      letter_chunk = paste(single_chars[l:min(l+i,n_singles)],collapse='')
      if(is.element(letter_chunk,char$map$special)){
        word_chars = c(word_chars,letter_chunk)
        l=l+i
        break
      }
    }
    l=l+1
  }
  return(word_chars)
}
