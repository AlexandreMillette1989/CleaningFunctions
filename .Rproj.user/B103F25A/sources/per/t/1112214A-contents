#' @title Rm_Emojis
#'
#' @description this function removes emojis
#'
#' @usage Rm_Emojis(Sstring)
#'
#' @param Sstring string value input
#'
#' @export
Rm_Emojis <- Vectorize(function(Sstring){
  Sstring <- gsub("[^\x01-\x7F]", "", Sstring)
  return(Sstring)
})

#' @title Rm_Hashtags
#'
#' @description this function removes hashtags
#'
#' @usage Rm_Hashtags(Sstring)
#'
#' @param Sstring string value input
#'
#' @export
Rm_Hashtags <- Vectorize(function(Sstring){

  Sstring <- gsub("#[_a-zA-Z0-9]+", "", Sstring)
  Sstring <- trimws(Sstring)

  return(Sstring)
})

#' @title Rm_Mentions
#'
#' @description this function removes mentions
#'
#' @usage Rm_Mentions(Sstring)
#'
#' @param Sstring string value input
#'
#' @export
Rm_Mentions <- Vectorize(function(Sstring){

  Sstring <- gsub("@[_a-zA-Z0-9]+", "", Sstring)
  Sstring <- trimws(Sstring)

  return(Sstring)
})

#' @title Rm_Numbers
#'
#' @description this function removes numbers
#'
#' @usage Rm_Numbers(Sstring)
#'
#' @param Sstring string value input
#'
#' @export
Rm_Numbers <- Vectorize(function(Sstring){
  Sstring <- gsub("[0-9]+", "", Sstring)
  Sstring <- trimws(Sstring)

  return(Sstring)
})

#' @title Rm_Punctuation
#'
#' @description this function removes punctuation
#'
#' @usage Rm_Punctuation(Sstring)
#'
#' @param Sstring string value input
#'
#' @export
Rm_Punctuation <- Vectorize(function(Sstring){
  Sstring <- gsub("[!?.,:;]", "", Sstring)
  Sstring <- trimws(Sstring)

  return(Sstring)
})

#' @title Rm_URLs
#'
#' @description this function removes URLS
#'
#' @usage Rm_URLs(Sstring)
#'
#' @param Sstring string value input
#'
#' @export
Rm_URLs <- Vectorize(function(Sstring){

  url_pattern <- "http[s]?://(?:[a-zA-Z]|[0-9]|[$-_@.&+]|[!*\\(\\),]|(?:%[0-9a-fA-F][0-9a-fA-F]))+"

  Sstring <- gsub(url_pattern, "", Sstring)
  Sstring <- trimws(gsub("\\s+", " ", Sstring))
  return(Sstring)
})

#' @title To_Lower
#'
#' @description this function changes all characters to lower case
#'
#' @usage To_Lower(Sstring)
#'
#' @param Sstring string value input
#'
#' @export
To_Lower <- Vectorize(function(Sstring){
  Sstring <- tolower(Sstring)
  return(Sstring)
})

#' @title To_Singular_String
#'
#' @description This function changes all words to singular. Only applicable to a df or tibble of word tokens
#'
#' @usage To_Singular_String(Sstring)
#'
#' @param Sstring string value input
#'
#' @import pluralize
#'
#' @export
To_Singular_String <- Vectorize(function(Sstring){

  Singularity <- singularize(Sstring)

  return(Singularity)
})


