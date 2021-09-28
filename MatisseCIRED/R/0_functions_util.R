

#' @title array2list
#' @description Array2List is a basic function that transforms a vector of strings into a list with values 0 attached to each element.
#'
#'     Useful for several functions, notably 'replace_na' which accepts a list as the replace argument
#'
#' @param x A vector of strings
#'
#' @return A list with each item being an element of the argument vector and values initialized at 0
#' @export
#'
#' @examples
#' array2list(c("test1", "test2"))
array2list <- function(x){
  temp_list <- as.list(matrix(0, nrow = 1, ncol = length(x)))
  names(temp_list) <- x
  return(temp_list)
}


#' @title mutate_when
#' @description Similar to mutate from dplyr, to allow mutate if condition is OK
#'
#' @param data A condition and a list of changes to apply (see example)
#' @param ...
#'
#' @return
#' @export
#'
#' @examples
#'  a <- data.frame(num = 1:20, isover10 = FALSE)
#'  a <- a %>% mutate_when(num > 10, isover10 = TRUE)
#'  a
mutate_when <- function(data, ...) {
  dots <- eval(substitute(alist(...)))
  for (i in seq(1, length(dots), by = 2)) {
    condition <- eval(dots[[i]], envir = data)
    mutations <- eval(dots[[i + 1]], envir = data[condition, , drop = FALSE])
    data[condition, names(mutations)] <- mutations
  }
  data
}


# Chronometer -------------------------------------------------------------

#' @title ChrStart
#' @description Starts the chronometer with value in global var chrono_timest
#'
#' @return No return
#' @export
#'
#' @examples
#' ChrStart()
ChrStart <- function(){
  chrono_timest <<- Sys.time()
  return(TRUE)
}

#' @title ChrPrint
#' @description Prints the value of the chronometer
#'
#' @return No return
#' @export
#'
#' @examples
#' ChrPrint()
ChrPrint <- function(){
  print(Sys.time()-chrono_timest)
  return(TRUE)
}

#' @title ChrEnd
#' @description Prints the value of the chronometer and cleans the global variable chrono_timest
#'
#' @return No return
#' @export
#'
#' @examples
#' ChrEnd()
ChrEnd <- function(){
  print(Sys.time()-chrono_timest)
  glob_var <- ls(pos = ".GlobalEnv")
  rm(list = glob_var[grep("chrono_timest", glob_var)] , pos=".GlobalEnv")
  return(TRUE)
}
