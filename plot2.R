plot2 <- function(data.filepath = "https://d396qusza40orc.cloudfront.net/exdata/data/household_power_consumption.zip",
                  plot.filepath = "./plot2.png",
                  verbose = FALSE) {
   # Plots given data to screen or PNG file (480w x 480h), using spec given at
   # https://github.com/EricByrnes/ExData_Plotting1/blob/master/README.md
   #
   # Args:
   #   data.filepath (string) - path to file containing data in the 'Electric
   #     power consumption' format. Several options are supported:
   #     - if data.filepath starts with "http://", "https://" or "ftp://", the
   #       function will attempt to download the file, otherwise it will attempt
   #       to load the file locally
   #     - if the file extension is ".zip", the file is unzipped to the current
   #       directory and the first (or only) file is assumed to be the data file
   #   plot.filepath (string) - path to file to plot to; if NULL/empty, the
   #     plot is sent to the screen
   #   verbose (boolean) - if TRUE, the function emits verbose diagnostic
   #     messages.
   #
   # Returns: Nothing
   #
   # Example:
   #   # plot to screen
   #   plot2(verbose = TRUE)
   #   # plot to named PNG file
   #   plot2(plot.filepath = "./plot2.png")
   
   gen.plot <- function(data, verbose = FALSE) {
      # Inner function to perform plotting.
      #
      # Args:
      #   data (data.frame) - data frame to plot.
      #   verbose (boolean) - if TRUE, the function emits verbose diagnostic
      #     messages.
      
      # set global parameters
      #  font point size
      currentPar <- par(ps = 12)
      
      # generate plot
      with(this.data.filtered,
           plot(DateTime, Global_active_power, type = "l",
                xlab = "",
                ylab = "Global Active Power (kilowatts)"))
      
      # reset global parameters
      par(currentPar)
   }
   
   # BEGIN - Common file handling
   # check for valid input file
   if (is.null(data.filepath) || data.filepath == "")
      stop("data.filepath is required")
   
   # get file extension for download file, i.e. ".zip" for default
   #  adapted from http://stackoverflow.com/questions/15073753/regex-return-file-name-remove-path-and-file-extension
   data.filepath.ext <- sub("(.*\\/)([^.]+)(\\.[[:alnum:]]+$)", "\\3",
                            data.filepath)
   
   # download the file if necessary - check whether the file path starts with
   # 'http://', 'https://', 'ftp://'
   if (grepl("^(http://|https://|ftp://)", data.filepath)) {
      # construct download target filename
      data.downloaded <- paste("./household_power_consumption",
                               data.filepath.ext, sep = "")
      
      # a URL was provided, so download the file (use quiet option to match
      # output detail to our own verbose flag)
      if (verbose)
         cat(" Downloading filename [", data.filepath, "]\n", sep = "")
      download.file(data.filepath, data.downloaded, "curl",
                    quiet = !verbose)
      data.filepath <- data.downloaded
   }
   
   # unzip the file if the extension is ".zip"
   if (data.filepath.ext == ".zip") {
      if (verbose)
         cat(" Unzipping filename [", data.filepath, "]\n", sep = "")
      data.filepaths <- unzip(data.filepath, junkpaths = TRUE)
      data.filepath <- data.filepaths[1]
      if (verbose)
         cat(" Extracted filename [", data.filepath, "] for loading\n", sep = "")
   }
   
   #  define classes for data being read
   data.classes <- c(rep("character", 2), rep("numeric", 7))
   data.na <- c("?")
   
   # check for valid parameters
   #  read the given text file - warn if there is a problem
   this.data <- NULL
   if (verbose) {
      cat(" Reading filename [", data.filepath, "]", sep = "")
      #  explicitly trap the most likely error - file doesn't exist
      if (!file.exists(data.filepath)) {
         cat(" - WARNING: Invalid data file\n")
      } else {
         #  perform read - trap any errors or warnings
         tryCatch({
            this.data <- read.table(data.filepath, sep = ";", header = TRUE,
                                    colClasses = data.classes,
                                    na.strings = data.na)
            cat(" - OK,", nrow(this.data), "frames\n")
         },
         warning = function(w) {
            cat(" - WARNING: File load warning\n")
            print(w)
         },
         error = function(e) {
            cat(" - WARNING: File load error\n")
            print(e)
         })
      }
   } else {
      # read data from file
      this.data <- read.table(data.filepath, sep = ";", header = TRUE,
                              colClasses = data.classes,
                              na.strings = data.na)
   }
   
   #  create date/time column
   this.data$DateTime <- strptime(paste(this.data$Date,
                                        this.data$Time,
                                        sep=" "),
                                  "%d/%m/%Y %H:%M:%S")

   # subset to the 2 days we are interested in; this would be better done while
   # loading, but that wasn't easy to figure out
   if (verbose)
      cat(" Filtering to desired rows", sep = "")
   this.data.filtered <- this.data[this.data$DateTime >= "2007-02-01" &
                                      this.data$DateTime < "2007-02-03", ]
   if (verbose)
      cat(", ", nrow(this.data.filtered), " frames remaining\n", sep = "")
   # END - Common file handling
   
   # plot
   #  to screen
   gen.plot(this.data.filtered, verbose = verbose)
   #  to file
   if (!is.null(plot.filepath) && plot.filepath != "") {
      if (verbose)
         cat(" Generating PNG file [", plot.filepath, "]\n", sep = "")
      # open PNG plot device (default dimensions 480x480)
      png(file = plot.filepath)
      # write plot
      gen.plot(this.data.filtered, verbose = verbose)
      # close plot device
      dev.off()
   }
}
