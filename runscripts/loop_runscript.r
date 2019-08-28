## R

rm(list=ls()); graphics.off()
library(tools)

##### user input start #####

replace_string <- "years <- "
replace_by <- c("1948:1957",
                "1958:1967",
                "1968:1977",
                "1978:1987",
                "1988:1997",
                "1998:2007",
                "2008:2009")

##### user input end #####

njobs <- length(replace_by)
if (njobs == 0) stop("njobs = ", njobs, ". check your 'replace_by'") 

# read original myrunids
myrunids <- readLines("~/scripts/r/myrunids.r")

replace_ind <- which(regexpr(replace_string, myrunids) != -1)
if (length(replace_ind) == 0) {
    stop("Could not find replace_string ", replace_string, " in original myrunids.r")
}
replace_ind <- max(replace_ind) # overwrite the _last_ entry in myrunids

# read original myrunscript 
myrunscript <- readLines("myrunscript.r")
myrunids_ind <- which(regexpr("source", myrunscript) != -1 &
                      regexpr("myrunids.r", myrunscript) != -1)
if (length(myrunids_ind) != 1) {
    stop(length(myrundids_ind), " positions to replace found in original runscript.r")
}

message("submit ", njobs, " jobs ...")
for (jobi in 1:njobs) {

    message("*** ", jobi, " ***")
    myrunidstmp <- myrunids
    myrunidstmp[replace_ind] <- paste0(replace_string, replace_by[jobi])
    message("replaced\n", myrunids[replace_ind], "\nwith\n", myrunidstmp[replace_ind], 
            "\nin myrunids")

    # save temporary myrunids as file
    dir.create("tmp", recursive=T, showWarnings=F)
    myrunidstmp_fname <- paste0("tmp/myrunidstmp_", 
                                paste0(gsub("[:punct:]", "_", replace_by[jobi]),
                                       collapse="_"), ".r") 
    write(myrunidstmp, myrunidstmp_fname)

    # replace myrunids in actual runscript
    myrunscripttmp <- myrunscript
    myrunscripttmp[myrunids_ind] <- paste0("source(\"", myrunidstmp_fname, "\")")
    message("replaced\n", myrunscript[myrunids_ind], "\nwith\n", myrunscripttmp[myrunids_ind], 
            "\nin myrunscript")
    
    # save temporary myrunscript as file
    myrunscripttmp_fname <- paste0("tmp/myrunscripttmp_", 
                                   paste0(gsub("[:punct:]", "_", replace_by[jobi]),
                                          collapse="_"), ".r") 
    write(myrunscripttmp, myrunscripttmp_fname)

    # submit jobscript
    ms <- format(as.numeric(Sys.time())*1000, digits=15)
    logfile <- paste0(tools::file_path_sans_ext(myrunscripttmp_fname), "_", ms, ".log")
    cmd <- paste0("rnohup ", myrunscripttmp_fname, " ", logfile)
    message(cmd)
    system(cmd)

} # for jobi njobs
