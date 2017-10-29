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

#Function to Check OS
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

#Change default audio player for macs
if(get_os() == 'osx') {
    setWavPlayer('/usr/bin/afplay')
}

#play sound
a1 = readMP3('sounds/a_1.mp3')
play(a1)
