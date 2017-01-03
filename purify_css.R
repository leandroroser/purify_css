
#' Clean a css file, removing the indicated set of rules in a file  or a vector
#'
#' @param myFile input css file
#' @param rules_as_path set of css rules to remove in a file, each rule in a new line
#' @param rules_as_vector a character vector with css rules to remove
#' @author leandro roser <learoser@gmail.com>


purify_css <- function(myFile, rules_as_path = NULL, rules_as_vector = NULL) {
  
  
  # check arguments -------------------------------------
  x <- readLines(myFile)
  # remove spaces
  x <- .format_css(x)

   if(length(x) == 0){
    stop("The file is empty")
  }
  
  if(is.null(rules_as_path) && is.null(rules_as_vector)) {
    stop("Any argument found for css_path or css_as_vector")
  }
  
  if(!is.null(rules_as_path) && !is.null(rules_as_vector)) {
    stop("only one of both css_path or css_as_vector must be set")
  }
  
  if(!is.null(rules_as_path)) {
    css_to_remove <- readLines(rules_as_path)
  } else if(!is.null(rules_as_vector)) {
    css_to_remove <- rules_as_vector
  } 
  # remove spaces
  css_to_remove <- gsub(" ", "", css_to_remove)
  
  # end of check ----------
  
  # set output path
  outdir <- sprintf(paste0(substitute(myFile), "%s"), "_out.css")
  
 
  # metacharacters substitution-------------------------------------------------------------------#

  meta <- c( "\\\\", "\\.","\\|", "\\[", "\\{", 
             "\\(", "\\)", "\\^", "\\*", "\\?", "\\+", "\\$")
  
  for(i in seq_along(meta)){
    cuales <- lapply(css_to_remove, function(x) gregexpr(meta[i], x))
    for(k in 1:length(cuales)) {
      regmatches(css_to_remove[[k]], cuales[[k]]) <- list(rep(meta[i], length(cuales[[k]][[1]])))
    }
  }
  # end of meta substitution-------------
  
  # capture strings ending with "{"
  css_to_remove <- paste0("^", css_to_remove, "{")
  rules.length <- length(css_to_remove)
  
  
  #start an anidated search for "{" and "}" -------------------------
  # iterate over css_to_remove
  
  for(rule in seq_along(css_to_remove)) {
    
    # find matching strings in file
    start_search <- grep(css_to_remove[rule], x, perl = TRUE) # perl TRUE to solve the presence of metacharacters
    
    #### Algorithm: when the number of "}" counted is == to the number of "{" counted, #####
    #### the last "}" indicates the end of all css rules (nested and not nested)       #####
    #### start_search can contain more than 1 match                                    #####
    
    if(length(start_search) != 0) {
      temp <- list()
      counter <- 1
      for(i in start_search) { # the element can be more of 1 time in the file
        open_nested <- 1
        close_nested <- 0
        end_search <- i
        while(close_nested < open_nested){
          end_search <- end_search + 1
          open <-  grep("\\{", x[end_search])
          close <- grep("\\}", x[end_search])
          if(length(open) != 0) {
            open_nested <- open_nested + 1
          }
          if(length(close) != 0) {
            close_nested <- close_nested + 1
          }
        }
        temp[[counter]] <- i:end_search
        counter <- counter + 1
      }
      # all the selected must be masked at the same time 
      # to maintain the line numbers
      for(y in seq_along(temp)) {
        x[c(temp[[y]])] <- ""
      }
      # now remove
      x <- x[-which(x == "")]
    }
    cat("Processed ", rule, "rules of ", rules.length, "\n")
  }
  
  # end of anidated search -------------
  
  
  # remove comments -----------------------------------------
  
  x <- gsub("\\*/", "\n*/\n", x)
  x <- gsub("/\\*", "\n/*\n", x)
  temp <- tempfile()
  cat(x, file= temp, sep = "\n")
  x <- readLines(temp)
  file.remove(temp)
  
  maxLen <- length(x)
  
  start <- grep("/\\*", x)
  if(length(start) != 0) {
    for(i in start) {
      k <- i + 1
      while(k < maxLen) {
        if(x[k] == "*/") {
          x[i:k] <- ""
          break
        } 
        k <- k + 1
      }
    }
  }
  
  # end remove comments-----------
  
  
  # remove empty rules i.e. rule{} ----------------------
  i <- 1
  while(i < length(x)) {
    if(!is.na(grep(".+\\{$", x[i]) && grep("}", x[i+1]))) {
      x[c(i,i+1)] <- ""
      i <- i + 2
    } else {
      i <- i + 1
    }
  }
  x <- x[-which(x == "")]
  x[grep("^}", x)] <- "}\n"
  
  # end remove empty rules ------------
  
  
  # remove empty lines between two "}"---------------------------
  i <- 1
  maxLen <- length(x) - 2
  while(i <  maxLen) {
    if(x[i] == "}\n" && x[i+1] == "}\n") {
      x[i] <- "}"
    }
    i <- i + 1
  }
  # end remove empty lines --------------------
  
    
  # write the myFile_out.css in the directory---------------------
  cat("\n")
  writeLines(x, con = outdir, sep = "\n")
  cat("File written to: ", outdir, "\n")
  
} # end of program


#########################################################
#' format a css file  ###################################
#' ######################################################
#'
#'@param css_file  css file

.format_css <- function(css_file) {
  x <- gsub(" ", "", css_file)
  # insert a newline in lines starting with "{" and followed by text
  #x <- gsub("(^})(.+)", "\\1\n\\2", x)
  #x <- gsub("(.+)(}$)", "\\1\n\\2", x)
  #x <- gsub("(.+\\{)(.+$)", "\\1\n\\2", x)
  x <- gsub("\\{", "{\n", x)
  x <- gsub("}", "\n}\n", x)
  
  temp <- tempfile()
  cat(x, file= temp, sep = "\n")
  x <- readLines(temp)
  file.remove(temp)
  x <- x[x != ""]
  
  x
} # end of program

# EOF
