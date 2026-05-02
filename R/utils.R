#' Copy contents of a folder to another
#'
#' @param src.dir Source directory
#' @param dest.dir Destination directory
#' @param overwrite Logical, if true overwrites the existing directory, default = T
#' @param ask Logical, should ask whether to create/delete directories if required, default = FALSE
#' @param ...
#' @export

dir.copy<- function(src.dir, dest.dir, overwrite = TRUE, ask = FALSE, ...){

      if(!file.exists(src.dir)){

            stop("Specified source directory is not valied...\n")

      }

      if(!file.exists(dest.dir) && ask == FALSE){
            stop("the destination folder specified is either invalid or doesn't exist...\n")
            return(FALSE)

      } else if(!file.exists(dest.dir) && ask == TRUE){
            cat("the destination folder specified is either invalid or doesn't exist...\n")

            askUser <- paste("would you like to create this directory?, y/n/c")

            userAnswer <- readline(askUser)
            userInput <- substr(toupper(userAnswer), start = 1, stop = 1)

            if(userInput == "N"){
                  return(FALSE)

            } else if(userInput == "Y"){

                  dir.create(dest.dir)
            }

      }

      all_files = dir(src.dir, full.names = TRUE)
      file.copy(from = all_files, to = dest.dir, recursive = TRUE)
}

#############
#' @description Get the number of decimal digits in a numeric value
#' @title get.decimalplaces
#' @param x A double numerical
#' @export
#' @return integer, the number of dec places
#' @example
#' get.decimalplaces(10.343434)

get.decimalplaces <- function(x) {
  if(is.numeric(x)){
    if(is.na(x)){ return(NA_integer_) }
    if ((x %% 1) != 0) {
      dec.val <- nchar(strsplit(sub('0+$', '', as.character(x)), ".", fixed=TRUE)[[1]][[2]])
      return(dec.val)
    } else {
      return(0)
    }
  } else {
    #else it is not numeric
    return(NA_integer_)
  }
}
############
#' Format number to scientific format according to HYDRUS inputs
#'
#' @param x  A numeric vector to be formatted
#' @param ndec Number of decimal places to apply (e.g, .000xx)
#' @param power.digits Number of power digits to apply (e.g., e+002)
#' @param ...
#' @return a character string of a value formatted to scientific notation
#' @export
#' @example
#' format2sci(120.5, ndec = 5, power.digits = 2)

format2sci<- function (x, ndec, power.digits, ...) {

        format.scalar<- function(x, ndec, power.digits) {

                dformat_sci = format(x, scientific = T)
                dnum = unlist(strsplit(dformat_sci, "e"))
                dnum_psign = substr(dnum[2], start = 1, stop = 1)

                dnuml = dnum[1]
                dnums = gsub(pattern = "\\+|\\-", replacement = "", x = dnum[2])

                dnuml = format(round(as.numeric(dnuml),ndec), nsmall = ndec)
                dnums = sprintf(fmt = paste0("e", dnum_psign, "%0",power.digits, "d"), as.numeric(dnums))
                dformat_new = paste0(dnuml, dnums)
                return(dformat_new)
        }

        if(length(x) > 1000){
                ncores = (parallel::detectCores()) -1
                cl = parallel::makeCluster(ncores)
                parallel::clusterExport(cl, varlist = c("format.scalar", "ndec", "power.digits"), envir = environment())
                fmt_vec_out = parallel::parSapply(cl, X = x, FUN = format.scalar, ndec = ndec, power.digits = power.digits, simplify = TRUE)
                parallel::stopCluster(cl)
        } else {
                fmt_vec_out = sapply(X = x, FUN = format.scalar, ndec, power.digits)
        }

        return(fmt_vec_out)
}



############
#' aka unique()
#'
#' @param x  any vector class, any length
#' @return vector of unique values
#' @export
#' @examples
#' x <- c(1,1,1,2,3,4,5,5,5,5,5)
#' u(x) #vector 1:5
#' identical(unique(x), u(x)) #TRUE, both are 1:5
u <- function(x){ unique(x) }


############
#' aka length(unique())
#'
#' @param x  any vector class, any length
#' @return integer, how many unique values in the input
#' @export
#' @examples
#' x <- c(1,1,1,2,3,4,5,5,5,5,5)
#' lu(x) # 5
#' identical(length(unique(x)), lu(x)) #TRUE, both are 5
lu <- function(x){ length(unique(x)) }


############
#' aka sort(unique())
#'
#' @param x  any vector class, any length
#' @return vector of unique values sorted with sort()
#' @export
#' @examples
#' x <- c(10,1,1,1,2,3,4,5,5,5,5,5)
#' su(x) # 1  2  3  4  5 10
#' identical(sort(unique(x)), su(x)) #TRUE
su <- function(x){ sort(unique(x)) }


############
#' aka length()
#'
#' @param x  any vector class, any length
#' @return integer, length of vector
#' @export
#' @examples
#' x <- c(10,1,1,1,2,3,4,5,5,5,5,5)
#' len(x) # 12
#' identical(length(x), len(x)) #TRUE
len <- function(x){ length(x) }




#####################
#' Get unit multiples for converting cm and days into project units
#'
#' @return vector, length 3. L.multiple, T.multiple, LT.multiple. Multiply cm, d, or cm/d values by these to get L, T, or L/T in your project units.
#' @param SpaceUnit
#' @param TimeUnit
#' @export
unit_mult <- function(SpaceUnit = "cm", TimeUnit = "hours"){

  if(!TimeUnit %in% c("seconds","minutes","hours","days")){
    TimeUnit <- paste0(TimeUnit,"s") #days not day, etc.
    #if it passes after adding the s then leave the function to run.
    if(!TimeUnit %in% c("seconds","minutes","hours","days")){ stop("TimeUnit not valid.") }}
  if(!SpaceUnit %in% c("mm","cm","m")){ stop("SpaceUnit not valid") }

  #cm/d units converted to project units:
  T.multiple <- c(24*60*60, 24*60, 24, 1)[which(c("seconds","minutes","hours","days") == TimeUnit)]
  L.multiple <- c(10, 1, 0.01)[which(c("mm","cm","m") == SpaceUnit)]
  LT.multiple <- L.multiple / T.multiple

  out.vec <- c(L.multiple, T.multiple, LT.multiple)
  names(out.vec) <- c("L.multiple", "T.multiple", "LT.multiple")
  return(out.vec)

} #end fn



##################
#' Pipe operator
#'
#' move object from left-hand side into first argument of right-hand side function
#' @name %>%
#' @rdname pipe
#' @keywords internal
#' @export
#' @importFrom dplyr %>%
#' @usage lhs \%>\% rhs
NULL



#' sapply wrapper for which(grepl())
#'
#' allows vector to be given to grepl(pattern) rather than single values. function called just like grepl() would be.
#' @returns returns a vector if one match in x for each y. returns a list if there are less than or more than one match for any y.
#' @param pattern vector of values to be searched for. same as grepl's 'pattern' argument but allows vector.
#' @param x the value (e.g., character string) being searched. same as grepl's 'x' argument.
#' @examples
#' which(grepl(c("a", "b", "z"), "abcdefghij")) #will not work. ignores b and z
#' c(which(grepl("a", "abcdefghij")),  which(grepl("b", "abcdefghij")), which(grepl("z", "abcdefghij"))) #doesn't work because the non-match position isn't given
#' sapply(c("a", "b", "z"), function(y){ which(grepl(y, "abcdefghij")) }) #works and is exactly what grapl function does.
#' grapl(c("a", "b", "z"), "abcdefghij") #returns list because z has no match
#' grapl(c("a", "b"), "abcdefghij") #returns vector because both have one match
grapl <- function(pattern,x){ sapply(pattern, function(z){ which(grepl(z,x)) })}
