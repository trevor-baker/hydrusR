#' Executes H1D_CALC.EXE from  Hydrus 1D directory
#'
#' @param project.path Path of the hydrus project
#' @param hydrus.path  Path of the Hydrus 1D executable
#' @param show.output Logical (Default = TRUE) whether the shell
#'                    output should be visible on R console (relevant only on windows)
#' @param timeout Maximum run time in seconds (Default = NULL, no limit). On
#'                timeout (windows) the process is killed and a warning is issued.
#' @export

call.H1D <- function(project.path,
                     hydrus.path = NULL,
                     show.output = TRUE,
                     timeout = NULL, ...){

   os.type = .Platform$OS.type

   if(is.null(hydrus.path)) {

      if(os.type == "windows"){
         win_def_hdir = "C:/Program Files (x86)/PC-Progress"
         h1d_version_dir = list.files(win_def_hdir, full.names = T, pattern = "Hydrus-1D")
         h1d_versions = sapply(basename(h1d_version_dir),
                                        function(x) {
                                          name_split = unlist(strsplit(x, " |\\."))
                                          return(as.numeric(name_split[2]))
                                          })

          hydrus.path = h1d_version_dir[which.max(h1d_versions)]

      }

  }

  #the Hydrus executable that will be called
  hydrus.exe = "H1D_CALC.EXE"  #### Windows sepcific executable name

  #Hydrus needs LEVEL_01.DIR to find your path. It needs to be saved in the same folder as the exe file
  file_level01 = file.path(hydrus.path, "LEVEL_01.DIR")
  Sys.chmod(file_level01, "0777")
  if(file.access(file_level01,2) < 0){
    stop("Cannot write to hydrus.path. Likely because Hydrus directory is in Program Files. RStudio must be run as an administrator (launch via right-click menu).")
  }

  write(x = noquote(project.path), #path needs to be written without quotes to be read properly
        file = file_level01,
        append = F)


  oldwd = getwd() #save to restore after
  on.exit(setwd(oldwd), add = TRUE)
  setwd(hydrus.path)

  if(os.type == "unix") {
        system(paste0("./", hydrus.exe))
  } else {
        t0 = Sys.time()
        status = suppressWarnings(
          system2(hydrus.exe,
                  stdout = if(show.output) "" else FALSE,
                  stderr = if(show.output) "" else FALSE,
                  timeout = if(is.null(timeout)) 0 else ceiling(timeout),
                  minimized = TRUE, invisible = TRUE)
        )

        # system2 returns 124 on timeout and kills its own child process
        if(!is.null(timeout) && identical(as.integer(status), 124L)){
          elapsed = round(as.numeric(difftime(Sys.time(), t0, units = "secs")), 1)
          warning("H1D_CALC.EXE timed out and was killed after ", elapsed,
                  " s (timeout = ", timeout, " s). Project path: ", project.path,
                  call. = FALSE)
          return(invisible(status))
        }
  }

}#end fn
