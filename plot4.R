plot4 <- function(data.filepath = "./household_power_consumption.txt",
                  plot.filepath = NULL,
                  verbose = FALSE) {
   # Plots given data to PNG file (480w x 480h), using spec given at
   # https://github.com/EricByrnes/ExData_Plotting1/blob/master/README.md
   #
   # Args:
   #   data.filepath (string) - path to file containing data in the 'Electric
   #     power consumption' format.
   #   plot.filepath (string) - path to file to plot to; if NULL/empty, the
   #     plot is sent to the screen
   #   verbose (boolean) - if TRUE, the function emits verbose diagnostic
   #     messages.
   #
   # Returns: Nothing
   #
   # Example:
   #   # plot to screen
   #   plot4(verbose=TRUE)
   #   # plot to named PNG file
   #   plot4(plot.filepath = "./plot4.png")
   
   gen.plot1 <- function(data, verbose = FALSE) {
      # Inner function to perform plotting.
      #
      # Args:
      #   data (data.frame) - data frame to plot.
      #   verbose (boolean) - if TRUE, the function emits verbose diagnostic
      #     messages.
      
      # create base plot with global active power
      with(this.data.filtered,
           plot(DateTime, Global_active_power, type = "l",
                xlab = "",
                ylab = "Global Active Power"))
   }
   
   gen.plot2 <- function(data, verbose = FALSE) {
      # Inner function to perform plotting.
      #
      # Args:
      #   data (data.frame) - data frame to plot.
      #   verbose (boolean) - if TRUE, the function emits verbose diagnostic
      #     messages.
      
      # create base plot with voltage
      with(this.data.filtered,
           plot(DateTime, Voltage, type = "l",
                xlab = "datetime",
                ylab = "Voltage"))
   }
   
   gen.plot3 <- function(data, verbose = FALSE) {
      # Inner function to perform plotting.
      #
      # Args:
      #   data (data.frame) - data frame to plot.
      #   verbose (boolean) - if TRUE, the function emits verbose diagnostic
      #     messages.
      
      # create base plot with voltage
      with(this.data.filtered,
           plot(DateTime, Sub_metering_1, type = "s",
                xlab = "", ylab = "Energy sub metering"))
      # add sub metering 2, 3
      with(this.data.filtered,
           points(DateTime, Sub_metering_2, type = "s", col = "red"))
      with(this.data.filtered,
           points(DateTime, Sub_metering_3, type = "s", col = "blue"))
      # add legend (some default values included for clarity)
      legend("topright",
             lty = "solid",
             bty = "n",
             col = c("black", "red", "blue"),
             legend = c("Sub_metering_1", "Sub_metering_2", "Sub_metering_3"))
   }
   
   gen.plot4 <- function(data, verbose = FALSE) {
      # Inner function to perform plotting.
      #
      # Args:
      #   data (data.frame) - data frame to plot.
      #   verbose (boolean) - if TRUE, the function emits verbose diagnostic
      #     messages.
      
      # create base plot with global reactive power
      with(this.data.filtered,
           plot(DateTime, Global_reactive_power, type = "s",
                xlab = "datetime",
                ylab = "Global_reactive_power"))
   }
   
   gen.plots <- function(data, verbose = FALSE) {
      # Inner function to perform plotting.
      #
      # Args:
      #   data (data.frame) - data frame to plot.
      #   verbose (boolean) - if TRUE, the function emits verbose diagnostic
      #     messages.
      
      # set global parameters
      #  font point size
      par(cex = 0.75)
      #  set up for 4 plots, 2 per row
      par(mfrow = c(2, 2))
      #  adjust margins
      par(mar = c(4.1, 4.1, 4.1, 2.1))
      
      # create plots
      gen.plot1(data, verbose)
      gen.plot2(data, verbose)
      gen.plot3(data, verbose)
      gen.plot4(data, verbose)
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
   this.data.filtered <- this.data[this.data$DateTime >= "2007-02-01" &
                                   this.data$DateTime < "2007-02-03", ]
   if (verbose)
      cat(" Filtered to ", nrow(this.data.filtered), " rows\n", sep = "")
   
   # plot
   #  to screen
   gen.plots(this.data.filtered, verbose = verbose)
   #  to file
   if (!is.null(plot.filepath) && plot.filepath != "") {
      # open PNG plot device (default dimensions 480x480)
      png(file = plot.filepath)
      # write plot
      gen.plots(this.data.filtered, verbose = verbose)
      # close plot device
      dev.off()
   }
}
