loadplot <- function(data.filepath = "https://d396qusza40orc.cloudfront.net/exdata/data/household_power_consumption.zip",
                     verbose = FALSE) {
   # Loads data used for plotting, using spec given at
   # https://github.com/EricByrnes/ExData_Plotting1/blob/master/README.md
   #
   # Args:
   #   data.filepath (string) - path to file containing data in the 'Electric
   #     power consumption' format.
   #   verbose (boolean) - if TRUE, the function emits verbose diagnostic
   #     messages.
   #
   # Returns:
   #   data.frame - data frame containing filtered data.
   #
   # Example:
   #   loadplot(verbose = TRUE)

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

   this.data.filtered
}
