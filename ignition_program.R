if (require("grid") == FALSE) install.packages("grid")
if (require("ggplot2") == FALSE) install.packages("ggplot2")
if (require("gridExtra") == FALSE) install.packages("gridExtra")

#### Variable Data // number of runs, filename prefix, working directory ####
numruns <- 3
prefix <- "2014-01-31,105A"
setwd("F://gitHub//ignition_curves//test_data")
export_to_png <- FALSE
####

# Width & height of exported .png file
pngw <- 600
pngh <- 600
# filename of exported .png file
fname <- prefix

options("digits.secs"=6, digits=6)

### Important function variables
## ign_time_thresh requires lowering if receive error:
    #Error in if (x[i] <= b && i > c) { : 
    #missing value where TRUE/FALSE needed
baseline_drop_thresh <- -25
drop_time_thresh <- -25
ign_time_thresh <- 20
ign_time_delay <- 25

#### Process datafiles // read, combine, extract useful columns
#create list of filenames, read.csv on each, combine into larger list
file_names <- paste0(prefix, '-', seq_len(numruns), '.log')
df_list <- lapply(file_names, function(x) read.csv(x, head=T, sep='\t', skip=11))
df_total <- do.call(cbind, df_list)
#adjust useful columns to keep based on numruns
if (numruns == 3) keeps <- c(2,4,8,12)
if (numruns == 2) keeps <- c(2,4,8)
if (numruns == 1) keeps <- c(2,4)
#remove useless columns/rows, start time at 0
df_trim <- df_total[-nrow(df_total), keeps]
time_1 <- as.POSIXct(df_trim$Time,format="%H:%M:%OS")
time_diff <- difftime(time_1[1:length(time_1)], time_1[1])
#pyro data separated, decimal removed
pyro_1 <- df_trim$PYRO.V.   * 1000
pyro_2 <- df_trim$PYRO.V..1 * 1000
pyro_3 <- df_trim$PYRO.V..2 * 1000
# List of pyro names for use in calculations
ign_names <- paste0("pyro_", seq_len(numruns))


#### Function to nicely format lists for calculations
func_format_list <- function(func_list, x=1){
        func_list <- rbind(func_list,apply(func_list,2,mean))     
        func_list <- rbind(func_list,apply(func_list,2,sd))
        rownames(func_list)[(numruns+1):(numruns+2)] <- c("avg","stdev") 
        rownames(func_list)[1:numruns] <- ign_names
        #if conditions needed to addign proper col name
        if(x == 1) colnames(func_list)[1] <- "Time to ignition"
        if(x == 2) colnames(func_list)[1] <- "Delta Temp"
        if(x == 3) colnames(func_list)[1] <- "Duration"
        #assign variables to ignition values
        return(func_list)
}

#### Functions to find coordinates
func_baseline_two_coord <- function(x){
        #returns time_diff coordinates for when temp reaches baseline 5 sec after ignition
        #add 10 seconds to func_ign_time, find next time temp goes below baseline (convert 0.5sec intervals + add 5secs)
        a <- func_ign_time(x)
        b <- round(func_baseline_temp(x), digits=0)
        c <- (func_ign_time(x))*2+10
        for(i in 1:length(x)){
                if (x[i] <= b && i > c){
                        time <- as.numeric(time_diff[i])
                        temp <- as.numeric(x[i])
                        return(c(time, temp))
                }
        }
        
}         
func_drop_time_coord <- function(x, threshold = drop_time_thresh){
        # threshold = minimum diff[] value to register drop event
        diff_data <- diff(x[1:length(x)])
        flag <- 0
        for (i in 1:length(diff_data)){
                if (diff_data[i] < threshold && flag == 0){
                        flag <- 1
                        time <- as.numeric(time_diff[i])
                        temp <- as.numeric(x[i])
                        return(c(time, temp))
                }
        }
}
func_ign_time_coord <- function(x, delay = ign_time_delay, threshold = ign_time_thresh){
        # delay = time to wait before reading values
        # threshold = minimum diff[] value to register ignition event
        delay <- delay * 2      #convert from 1/2 to whole seconds
        diff_data <- diff(x[1:length(x)])
        flag <- 0
        for (i in 1:length(diff_data)){
                if (diff_data[i] > threshold && diff_data[i+1] > threshold && i > delay){
                        flag <- 1
                        time <- as.numeric(time_diff[i])
                        temp <- as.numeric(x[i])
                        return(c(time, temp))
                }
        }
}  
func_max_temp_coord <- function(x){
        max <- 0
        for (i in 1:length(x)){
                if(x[i] > max){
                        max <- x[i]
                        time <- as.numeric(time_diff[i])
                }
        }
        return(c(time, max))
}

#### Functions for secondary calculations
func_baseline_one <- function(x, threshold = baseline_drop_thresh){
        diff_data <- diff(x[1:length(x)])
        flag <- 0
        for (i in 1:length(x)){
                if (diff_data[i] < threshold && flag == 0){
                        flag <- 1
                        time <- as.numeric(time_diff[i])
                        #print(paste0("Last point before drop below baseline: ", time, "sec"))
                        return(1:time)
                }
        }
}                 #returns time_diff[1:x] values of baseline
func_baseline_two <- function(x){
        #returns time_diff coordinates for when temp reaches baseline 5 sec after ignition
        #add 10 seconds to func_ign_time, find next time temp goes below baseline (convert 0.5sec intervals + add 5secs)
        a <- func_ign_time(x)
        b <- round(func_baseline_temp(x), digits=0)
        c <- (func_ign_time(x))*2+10
        for(i in 1:length(x)){
                if (x[i] <= b && i > c){
                        time <- as.numeric(time_diff[i])
                        #print(paste0("Time when temp lowers to baseline: ", time, "sec"))
                        return(time)
                }
        }
        
}                                                   #returns time_diff[x] value of drop to baseline
func_baseline_temp <- function(x){
        mean_baseline <- mean(x[(func_baseline_one(x))])
        return(mean_baseline)
}                                                  #returns mean of pyro_x[func_baseline_one[x]] 
func_drop_time <- function(x, threshold = drop_time_thresh){
        # threshold = minimum diff[] value to register drop event
        diff_data <- diff(x[1:length(x)])
        flag <- 0
        for (i in 1:length(diff_data)){
                if (diff_data[i] < threshold && flag == 0){
                        flag <- 1
                        time <- as.numeric(time_diff[i])
                        return(time)
                }
        }
}                        #returns time_diff[x] when drop in temp < threshold
func_ign_time <- function(x, delay = ign_time_delay, threshold = ign_time_thresh){
        # delay = time to wait before reading values
        # threshold = minimum diff[] value to register ignition event
        delay <- delay * 2      #convert from 1/2 to whole seconds
        diff_data <- diff(x[1:length(x)])
        flag <- 0
        for (i in 1:length(diff_data)){
                if (diff_data[i] > threshold && diff_data[i+1] > threshold && i > delay){
                        flag <- 1
                        time <- as.numeric(time_diff[i])
                        #print(paste0("Plug ignition time: ", time, " seconds. ", "Row num=", i))
                        return (time)
                }
        }
}  #returns time_diff[x] when rise in temp > threshold & after delay

#### Functions for primary calculations
func_ign <- function(x){
        ign <- func_ign_time(x) - func_drop_time(x)
        return(ign)
}                    
func_delta_temp <- function(x){
        dT <- max(x) - func_baseline_temp(x)
        return(dT)
}
func_duration <- function(x){
        dur <- func_baseline_two(x) - func_ign_time(x)
        return(dur)
}

### Calculate times to ignition // (func ignition time - func plug drop time) // get avg +- sd
ign_list <- as.matrix(as.numeric(lapply(mget(ign_names)[1:numruns], func_ign)))
ign_avg <- func_format_list(ign_list)[(numruns+1),1]
ign_sd <- func_format_list(ign_list)[(numruns+2),1]

### Calculate delta temps // (calls func to sub baseline from max)
#row labels "pyro-x" appear, converting to numeric and back to matrix solves..
delta_temp_list <- as.matrix(as.numeric(lapply(mget(ign_names)[1:numruns], func_delta_temp)))
dt_avg <- func_format_list(delta_temp_list)[(numruns+1),1]
dt_sd <- func_format_list(delta_temp_list)[(numruns+2),1]

### Calculate duration of exotherm // (calls func to sub ignition end from ignition start)
# ignition end defined as when temp lowers back to baseline after ignition
duration_list <- as.matrix(as.numeric(lapply(mget(ign_names)[1:numruns], func_duration)))
dur_avg <- func_format_list(duration_list)[(numruns+1),1]
dur_sd <- func_format_list(duration_list)[(numruns+2),1]

#### For reporting data // Date / time to ign / delta temp / ign duration
test_date <- paste("Testing performed:", as.POSIXlt(as.character(df_total[1,1]), format="%d/%m/%Y"))
#test_date <- paste(as.POSIXlt(as.character(df_total[1,1]), format="%d/%m/%Y"))
time_to_ignition <- paste("Time to ignition:", ign_avg, "+-", ign_sd)
delta_temp <- paste("Delta temp:", dt_avg, "+-", dt_sd)
ign_duration <- paste("Ignition duration:", dur_avg, "+-", dur_sd)

### Function to format data for printing
print_data <- function(){
        a <- ign_list
        b <- delta_temp_list
        c <- duration_list
        d <- c(a,b,c)
        e <- matrix(unlist(d),nrow=numruns)
        rownames(e) <- rownames(e, do.NULL = FALSE, prefix = "Pyro")
        f <- rbind(e,apply(e,2,mean))     
        g <- rbind(f,apply(f,2,sd))
        colnames(g) <- c("Time to Ign.(s)","Delta temp(C)","Duration(s)")
        rownames(g)[(numruns+1):(numruns+2)] <- c("avg","stdev")
        #print(test_date)
        return (round(g, digits=1))
}
all_data <- print_data()

#### For plotting data

func_make_graph <- function (x, y=deparse(substitute(x))){
        time <- as.numeric(time_diff)
        dfp <- data.frame(time, x)
        ggplot (data=dfp, aes(time, x)) +
                ggtitle(y) +
                geom_point(alpha=1/4) +
                geom_point(x=func_drop_time_coord(x)[1], y=func_drop_time_coord(x)[2], size=4, color="blue", alpha=1/200) +
                geom_point(x=func_baseline_two_coord(x)[1], y=func_baseline_two_coord(x)[2], size=4, color="blue", alpha=1/200) +
                geom_hline(aes(yintercept=func_drop_time_coord(x)[2]),linetype="dashed", alpha=1/4, color="blue") +
                geom_point(x=func_ign_time_coord(x)[1], y=func_ign_time_coord(x)[2], size=4, color="red", alpha=1/200) +
                geom_point(x=func_max_temp_coord(x)[1], y=func_max_temp_coord(x)[2], size=4, color="red", alpha=1/200) +
                scale_x_continuous("Time(s)", seq(0,420,60)) +
                scale_y_continuous("Temperature(C)", limits=c(900,1400))
}

#create list of pyro_graphs + tableGrob, apply grid.arrange to list
pyro_graphs_list <- Map(func_make_graph, mget(ign_names), ign_names)
all_data_list_grob <- lapply(list(all_data), tableGrob)
all_graphs <- c(pyro_graphs_list, all_data_list_grob)
do.call("grid.arrange", all_graphs)
#add test date to bottom right corner
grid.draw(textGrob(test_date, x=0.99, y=0.01, hjust=1, vjust=0.1))


if (export_to_png == TRUE){
        #### Code to export plotted data to .png file
        png(filename=paste0(fname,".png"), width = pngw, height = pngh)
        pyro_graphs_list <- Map(func_make_graph, mget(ign_names), ign_names)
        all_data_list_grob <- lapply(list(all_data), tableGrob)
        all_graphs <- c(pyro_graphs_list, all_data_list_grob)
        do.call("grid.arrange", all_graphs)
        grid.draw(textGrob(test_date, x=0.99, y=0.01, hjust=1, vjust=0.1))
        dev.off()
}
