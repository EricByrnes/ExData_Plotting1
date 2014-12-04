plot1 <- function(data.filepath = "./household_power_consumption.txt",
                  plot.filepath = "./plot1.png",
                  verbose = FALSE) {
   # Plots given data to PNG file (480w x 480h), using spec given at
   # https://github.com/EricByrnes/ExData_Plotting1/blob/master/README.md
   #
   # Args:
   #   data.filepath: character vector, length 1 - path to file containing data
   #     in the 'Electric power consumption' format.
   #   plot.filepath: character vector, length 1 - path to file to write.
   #   verbose: TRUE/FALSE - if TRUE, the function emits verbose diagnostic
   #     messages.
   #
   # Returns: Nothing
   #
   # Example:
   #   plot1(verbose=TRUE)
   
   #  define classes for data being read
   data.classes <- c(rep("character", 2), rep("numeric", 7))
   data.na <- c("?")
   
   # check for valid parameters
   #  read the given text file - warn if there is a problem
   this.data <- NULL
   if (verbose) {
      cat(" Reading filename [", filename, "]", sep = "")
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
   this.data.filtered <- this.data[this.data$DateTime >= "2007-02-01" &
                                   this.data$DateTime < "2007-02-03", ]
   if (verbose)
      cat(" Filtered ", nrow(this.data.filtered), " rows\n", sep = "")

   # plot
   #  to screen
   hist(this.data.filtered$Global_active_power, col = "red",
        main = "Global Active Power",
        xlab = "Global Active Power (kilowatts)")
   #  to file
   if (!is.null(plot.filepath) || plot.filepath != "") {
      # open PNG plot device (default dimensions 480x480)
      png(file = plot.filepath)
      # write plot
      hist(this.data.filtered$Global_active_power, col = "red",
           main = "Global Active Power",
           xlab = "Global Active Power (kilowatts)")
      # close plot device
      dev.off()
   }
}
