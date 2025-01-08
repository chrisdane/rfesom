## R

## this runscript wrapper reads runscripts/myrunscript.r

rm(list=ls()); graphics.off()
library(tools)

##### user input start #####

submit_via_nohup <- F
submit_via_sbatch <- T # uses account resources!!!
dry <- F # do not submit final jobs

#myrunscript_fname <- "~/scripts/r/rfesom/runscripts/myrunscript.r"
myrunscript_fname <- "~/scripts/r/rfesom/runscripts/rfesom.run.r"

if (F) {
    replace_string <- list(string=" years <- ", between_lines=c(51, 56))
    # 1pct 4co2
    replace_by <- apply(cbind(seq(1850, 2090, b=10), 
                              seq(1859, 2099, b=10)), 
                        1, paste, collapse=":")
    # hist:
    #replace_by <- apply(cbind(c(seq(1850, 2000, b=10), 2010),
    #                          c(seq(1859, 2009, b=10), 2014)),
    #                    1, paste, collapse=":")
    # pi last 30:
    #replace_by <- apply(cbind(c(1912, seq(1920, 1930, b=10), 1940),
    #                          c(1919, seq(1929, 1939, b=10), 1941)),
    #                    1, paste, collapse=":")
    #replace_by <- apply(cbind(c(1842, seq(1850, 1930, b=10), 1940),
    #                          c(1849, seq(1859, 1939, b=10), 1941)),
    #                    1, paste, collapse=":")
} else if (F) { # phd
    replace_string <- list(string=" years <- ", between_lines=c(501, 501))
    replace_by <- c("1948:1957",
                    "1958:1967",
                    "1968:1977",
                    "1978:1987",
                    "1988:1997",
                    "1998:2007",
                    "2008:2009")
} else if (T) {
    replace_string <- list(string=" years <- ", between_lines=c(890, 890))
    replace_by <- 2001:2018
}

##### user input end #####

njobs <- length(replace_by)
if (njobs == 0 || !is.null(dim(replace_by))) {
    stop("njobs = ", njobs, ". check your 'replace_by':", replace_by) 
}

# read original runscript
myrunscript_fname <- normalizePath(myrunscript_fname)
message("read myrunscript_fname = ", myrunscript_fname, " ...")
myrunscript <- readLines(myrunscript_fname)

# find line to replace years between wanted lines
replace_inds <- replace_string$between_lines[1]:replace_string$between_lines[2]
replace_ind <- which(regexpr(replace_string$string, myrunscript[replace_inds]) != -1)
if (length(replace_ind) != 1) {
    stop("Could not find 1 replace_string \"", replace_string$string, 
         "\" in between lines ", paste(replace_string$between_lines, collapse="-"), 
         " in original runscript file ", myrunscript_fname)
}

message("submit ", njobs, " jobs ...")
for (jobi in seq_len(njobs)) {

    message("*** job ", jobi, "/", njobs, " ***")
    
    # save temporary runscript as file
    ms <- format(as.numeric(Sys.time())*1000, digits=10) # some unique number for log file name
    suffix <- paste0(gsub("[:punct:]", "_", replace_by[jobi]), collapse="_")
    if (jobi == 1) {
        tmppath <- "tmp"
        dir.create(tmppath, recursive=T, showWarnings=F)
    }
    myrunscripttmp_fname <- paste0(tmppath, "/", basename(myrunscript_fname), 
                                   "_",  suffix, "_", ms, ".r")
    
    # replace years
    myrunscripttmp <- myrunscript
    myrunscripttmp[replace_inds][replace_ind] <- trimws(paste0(replace_string$string, replace_by[jobi]))
    message("replaced \"", 
            myrunscript[replace_inds][replace_ind], "\" with \"", 
            myrunscripttmp[replace_inds][replace_ind], "\"")

    # write temporary runscript 
    write(myrunscripttmp, myrunscripttmp_fname)

    # submit jobscript
    if (submit_via_nohup) {
        logfile <- paste0(tools::file_path_sans_ext(myrunscripttmp_fname), "_", ms, ".log")
        cmd <- paste0("rnohup ", myrunscripttmp_fname, " ", logfile)
    } else if (submit_via_sbatch) {
        # mistral example scripts: https://www.dkrz.de/up/systems/mistral/running-jobs/example-batch-scripts
        # mistral partition limits: https://www.dkrz.de/up/systems/mistral/running-jobs/partitions-and-limits 
        # ollie example scripts: https://swrepo1.awi.de/plugins/mediawiki/wiki/hpc/index.php/Slurm_Example_Scripts
        # ollie partition limits: https://swrepo1.awi.de/plugins/mediawiki/wiki/hpc/index.php/SLURM#Partitions
        cmd <- c("#!/bin/bash",
                 paste0("#SBATCH --job-name=loop", suffix, "      # Specify job name"),
                 "#SBATCH --partition=shared     # Specify partition name",
                 #"#SBATCH --partition=prepost     # Specify partition name",
                 #"#SBATCH --ntasks=1             # Specify max. number of tasks to be invoked",
                 "#SBATCH --time=04:00:00        # Set a limit on the total run time",
                 #"#SBATCH --time=08:00:00        # Set a limit on the total run time",
                 #"#SBATCH --time=36:00:00        # Set a limit on the total run time",
                 #"#SBATCH --mail-type=FAIL       # Notify user by email in case of job failure",
                 #"#SBATCH --account=ab0246       # Charge resources on this project account",
                 "#SBATCH --account=ab1095       # Charge resources on this project account",
                 #"#SBATCH --account=ba0989       # Charge resources on this project account",
                 # memory:
                 #"#SBATCH --mem=0                    # 0 = use all mem",
                 "#SBATCH --mem=15000M                    # 0 = use all mem",
                 # logs and errors in different files:
                 #paste0("#SBATCH --output=", tools::file_path_sans_ext(myrunscripttmp_fname), ".o%j    # File name for standard output"),
                 #paste0("#SBATCH --error=", tools::file_path_sans_ext(myrunscripttmp_fname), ".e%j     # File name for standard error output"),
                 # logs and errors in same file:
                 paste0("#SBATCH --output=", tools::file_path_sans_ext(myrunscripttmp_fname), ".%j.log    # File name for standard output"),
                 paste0("#SBATCH --error=", tools::file_path_sans_ext(myrunscripttmp_fname), ".%j.log     # File name for standard error output"),
                 "",
                 "# Execute serial programs, e.g.",
                 "module load r",
                 paste0("Rscript ", myrunscripttmp_fname))
        slurmrunscript_fname <- paste0(tmppath, "/slurmrunscript_", suffix, "_", ms, ".job")
        write(cmd, slurmrunscript_fname)
        cmd <- paste0("sbatch ", slurmrunscript_fname)
    } # which job submission method
   
    message("run `", cmd, "` ...")

    # submit job
    if (!dry) {
        system(cmd)
    } else {
        message("`dry` = T --> do not run this command")
    }

} # for jobi njobs
