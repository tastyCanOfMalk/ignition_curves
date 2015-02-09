if (require("grid") == FALSE) install.packages("grid")
if (require("ggplot2") == FALSE) install.packages("ggplot2")
if (require("gridExtra") == FALSE) install.packages("gridExtra")
if (require("reshape") == FALSE) install.packages("reshape")

# ======================
# TABLE OF CONTENTS
# ==========================
# OPTIONS
# IMPORT & PREPARE
# FUNCTIONS & LIST CREATION
# CREATE SUMMARY & CALCULATIONS TABLE
# REMOVE ROW FUNCTION
#   round(removeRow(1),1)  # removes pyro1
#   round(removeRow(2),1)  # removes pyro2
#   round(removeRow(3),1)  # removes pyro3
# GRAPHIC SETTINGS
# GRAPH CREATION
# GRAPHIC OUTPUT
# EXPORT FUNCTIONS
#   exportPng() 
#   exportPdf() 

# ======================
# OPTIONS
# ==========================
prefix       <- "2014-09-18,E"  # filename 'prefix'

setwd("C://Users//yue.GLOBAL//Documents//R//ignition")

skip.lines <- 13  # default 11, 13 otherwise
#   Needs tweaking if:
#     Error in read.table(file = file, header = header, 
#      sep = sep, quote = quote,  : more columns than column names

read.rows  <- 800  # default 840, less if file missing lines
exo.threshold <- 5  # default 10, low exo need around 5

remove1 <- FALSE
remove2 <- TRUE
remove3 <- FALSE # set to TRUE if removal desired

# ======================
# IMPORT & PREPARE
# ==========================
file.names <- paste0(prefix, '-', seq_len(3), '.log')
df.list <- lapply(file.names, 
                  function(x) read.csv(x, 
                                       sep='\t', 
                                       skip=skip.lines, 
                                       nrows=read.rows))

df.total <- do.call(cbind, df.list)  # merge all data

time      <- seq.int(0,419.5, 0.5)  # create time variable axis
time.rows <- read.rows - 1  # match length(time) with imported data length
time      <- time[0:time.rows]  # modify length(time)

keeps    <- c(4, 8, 12)  # keep PYRO (temperature) columns
df.trim  <- cbind(time, df.total[-nrow(df.total), keeps]) 

colnames(df.trim) <- c("time", "pyro1", "pyro2", "pyro3")

pyro1 <- df.trim$pyro1 * 1000
pyro2 <- df.trim$pyro2 * 1000
pyro3 <- df.trim$pyro3 * 1000  # remove decimals from temperature data

df.trim <- as.data.frame(cbind(time, pyro1, pyro2, pyro3)) # relevant data

# ======================
# FUNCTIONS & LIST CREATION
# ===========================
ign.names <- paste0("pyro", seq_len(3))  # for easier function calls

variables.df <- data.frame(pyro1=as.numeric(),
                               pyro2=as.numeric(),
                               pyro3=as.numeric())  # empty variables dataframe

dropTime <- function(x){
  # Computes drop time  coordinates & average baseline temperature
  #   Based on a negative decrease in slope value
  #
  # Args:
  #   x: numeric vector whose droptime & baseline is to be calculated
  #
  # Returns: 
  #   list of values: drop time(x1), drop temp(y1), baseline(y-value)
  for (i in 1:length(x)){
    if (abs(x[i] - x[i+1] > 5)){
      drop.time <- time[i]
      drop.temp <- x[i]
      baseline <- mean(x[1:i])
      A <- list(drop.time, drop.temp, baseline)
      return(A)
      break
    }
  }
}

drop.times <- cbind((lapply(mget(ign.names)[1:3], dropTime)))
drop.times.table <- setNames(do.call(rbind.data.frame,drop.times),
                             dimnames(drop.times)[[2]])
rownames(drop.times.table) <- c("pyro1", "pyro2", "pyro3")
colnames(drop.times.table) <- c("drop.time(x1)", 
                                "drop.temp(y1)",
                                "baseline.temp")

exoTime <- function(x){
  # Computes exotherm coordinates
  #   Based on a positive increase in slope, "exo.threshold",
  #     may need adjustment based on speed of exotherm reaction
  #     (slower reaction needs a lower threshold value)
  #   
  # Args:
  #   x: numeric vector whose exotherm time is to be calculated
  #
  # Returns: 
  #   list of values: exotherm time(x2), exotherm temp(y2)
  for (i in dropTime(x)[[1]]:length(x)){
    if (abs(x[i+1] - x[i] > exo.threshold && x[i+10] > dropTime(x)[[3]])){
      # value must be within 5-seconds from baseline.temp
      exo.time <- time[i]
      exo.temp <- x[i]
      A <- list(exo.time, exo.temp)
      return(A)
      break
    }
  }
}

exo.times <- cbind((lapply(mget(ign.names)[1:3], exoTime)))
exo.times.table <- setNames(do.call(rbind.data.frame,exo.times),
                            dimnames(exo.times)[[2]])
rownames(exo.times.table) <- c("pyro1", "pyro2", "pyro3")
colnames(exo.times.table) <- c("exo.time(x2)", 
                               "exo.temp(y2)")

maxTemp <- function(x){
  # Computes maximum temperature coordinates
  #
  # Args:
  #   x: numeric vector whose max temperature is to be calculated
  #
  # Returns: 
  #   list of values: max temp time(x3), max temp temp(y3)
  max.temp <- max(x)
  max.time <- match(max(x),x)/2
  A <- list(max.time, max.temp)
  return(A)    
}

max.temps <- cbind((lapply(mget(ign.names)[1:3], maxTemp)))
max.temps.table <- setNames(do.call(rbind.data.frame,max.temps), 
                            dimnames(max.temps)[[2]])
rownames(max.temps.table) <- c("pyro1", "pyro2", "pyro3")
colnames(max.temps.table) <- c("max.temp.time(x3)", 
                               "max.temp(y3)")

exoDuration <- function(x){
  # Computes exotherm duration coordinates
  #
  # Args:
  #   x: numeric vector whose exotherm duration is to be calculated
  #
  # Returns: 
  #   list of values: exo duration(x4), exo duration temp temp(y4)
  for (i in maxTemp(x)[[1]]:length(x)){
    if (x[i*2] <= dropTime(x)[3]){
      baseline1.time <- (i)
      baseline1.temp <- (x[i*2])
      A <- list(baseline1.time, baseline1.temp)
      return(A)
      break
    }
  }
}

exo.durations <- cbind((lapply(mget(ign.names)[1:3], exoDuration)))
exo.durations.table <- setNames(do.call(rbind.data.frame, exo.durations),
                                dimnames(exo.durations)[[2]])
rownames(exo.durations.table) <- c("pyro1", "pyro2", "pyro3")
colnames(exo.durations.table) <- c("exo.duration.time(x4)", 
                                   "exo.duration.temp(y4)")

# ======================
# CREATE SUMMARY & CALCULATIONS TABLE
# ===========================
summary.table <- cbind(drop.times.table, 
                       exo.times.table, 
                       max.temps.table, 
                       exo.durations.table)  # all results in one table

calculations.table <- cbind(exo.times.table[[1]] - drop.times.table[[1]],
                            max.temps.table[[2]],
                            exo.durations.table[[1]] - exo.times.table[[1]])

rownames(calculations.table) <- c("pyro1", "pyro2", "pyro3")
colnames(calculations.table) <- c("ignite time", 
                                   "max temp",
                                   "exo duration")

ign.time.mean     <- round(mean(calculations.table[1:3, 1]), 0)
max.temp.mean     <- round(mean(calculations.table[1:3, 2]), 0)
exo.duration.mean <- round(mean(calculations.table[1:3, 3]), 0)

means           <- cbind(ign.time.mean, max.temp.mean, exo.duration.mean)
rownames(means) <- c("mean")


ign.time.sd       <- round(sd(calculations.table[1:3, 1]), 0)
max.temp.sd       <- round(sd(calculations.table[1:3, 2]), 0)
exo.duration.sd   <- round(sd(calculations.table[1:3, 3]), 0)

sds           <-  cbind(ign.time.sd, max.temp.sd, exo.duration.sd)
rownames(sds) <- c("std.dev")

calculations.table.total <- rbind (calculations.table, means, sds)

# ======================
# REMOVE ROW FUNCTION
# ===========================
removeRow <- function(x){
  # Allows removal of suspected outlier sample row from calculations.table
  # 
  # Args:
  #   x: row number to be removed (1, 2, or 3)
  # 
  # Returns: 
  #   a new table with specified row values replaced with NA
  temprow <- matrix(c(rep.int(NA, length(data))), nrow = 1, ncol = 3)
  calc.table.new <- rbind(calculations.table[-x,], temprow)
  
  ign.time.mean     <- mean(calc.table.new[1:3, 1], na.rm = TRUE)
  max.temp.mean     <- mean(calc.table.new[1:3, 2], na.rm = TRUE)
  exo.duration.mean <- mean(calc.table.new[1:3, 3], na.rm = TRUE)
  means           <- cbind(ign.time.mean, max.temp.mean, exo.duration.mean)
  rownames(means) <- c("mean")
  
  ign.time.sd     <- sd(calc.table.new[1:3, 1], na.rm = TRUE)
  max.temp.sd     <- sd(calc.table.new[1:3, 2], na.rm = TRUE)
  exo.duration.sd <- sd(calc.table.new[1:3, 3], na.rm = TRUE)
  sds           <- cbind(ign.time.sd, max.temp.sd, exo.duration.sd)
  rownames(sds) <- c("std.dev")
  
  calc.table.new <- rbind(calculations.table[-x,], temprow, means, sds)
  return(calc.table.new)
}

# Function call removes sample data from table based on conditional
#   does not allow for removal of more than 1 line

if (remove1 == TRUE){
  calculations.table.total <- round(removeRow(1),1)  # removes pyro1
  prefix <- paste0(prefix, "-R1")  # for filename
}

if (remove2 == TRUE){
  calculations.table.total <- round(removeRow(2),1)  # removes pyro2
  prefix <- paste0(prefix, "-R2")  # for filename
}

if (remove3 == TRUE){
  calculations.table.total <- round(removeRow(3),1)  # removes pyro3
  prefix <- paste0(prefix, "-R3") # for filename
}

# ======================
# GRAPHIC SETTINGS
# ===========================
color.list <- list("black",     #1  pyro values
                   "red",       #2  drop time
                   "red",       #3  baseline avg
                   "blue",      #4  ignition time
                   "orange",    #5  max temp
                   "green",     #6  duration
                   "gray92",    #7  graph panel bg
                   "gray80",    #8  graph panel outline
                   "white",     #9  major gridlines
                   "gray90",    #10 minor gridlines
                   "lightblue", #11 p5 results fill
                   "white"      #12 p6 fill 
                   )

size.list <- list(1,     #1 pyro values
                  3,     #2 coordinate values
                  0.25,  #3 baseline avg
                  .7,    #4 major gridlines
                  .5,    #5 minor gridlines
                  20     #6 p6 font size
                  )

alpha.list <- list(.25,   #1 pyro values
                   1,     #2 coordinate values
                   0.10,  #3 baseline avg
                   1,     #4 combined graph pyro values
                   .08    #5 combined graph baseline alpha
)

xy.scale.list <- list(0,    #1 x-axis min
                      420,  #2 x-axis max
                      30,   #3 x-axis scale/breaks
                      950,  #4 y-axis min
                      1400, #5 y-axis max
                      100,  #6 y-axis scale/breaks
                      0.5,  #7 x-axis vjust
                      1.0   #8 y-axis vjust
                      )

# ======================
# GRAPH CREATION
# ===========================
p1 <- ggplot(data = df.trim, aes(x = time, y  = pyro1)) + 
  
  theme(panel.background = element_rect(fill  = color.list[[7]],
                                        color = color.list[[8]]),
        panel.grid.major = element_line(color = color.list[[9]],
                                        size  = size.list[[4]]),
        panel.grid.minor = element_line(color = color.list[[10]], 
                                        size  = size.list[[5]]),
        panel.grid.major.y = element_line(color = color.list[[9]],
                                          size  = size.list[[4]]),
        panel.grid.minor.y = element_line(color = color.list[[10]],
                                          size  = size.list[[5]]),
        axis.title.x = element_text(vjust = xy.scale.list[[7]]),
        axis.title.y = element_text(vjust = xy.scale.list[[8]])) + 
  scale_x_continuous("time (sec)",
                     limits = c(xy.scale.list[[1]],
                                xy.scale.list[[2]]), 
                     breaks = seq.int(xy.scale.list[[1]],
                                      xy.scale.list[[2]],
                                      xy.scale.list[[3]])) +
  scale_y_continuous("temperature (°C)",
                     limits = c(xy.scale.list[[4]],
                                xy.scale.list[[5]]), 
                     breaks = seq.int(xy.scale.list[[4]],
                                      xy.scale.list[[5]],
                                      xy.scale.list[[6]])) + 
  ggtitle("pyro1") +
  geom_point(color = color.list[[1]],  # pyro values 
             size  = size.list[[1]],
             alpha = alpha.list[[1]]) +
  geom_point(x = summary.table[[1, 1]],  # drop time x1
             y = summary.table[[1, 2]],  # drop time y1
             color = color.list[[2]],
             size  = size.list[[2]],
             alpha = alpha.list[[2]]) +
  geom_point(y = summary.table[[1, 3]],  # baseline average y-value
             color = color.list[[3]],
             size  = size.list[[3]],
             alpha = alpha.list[[3]]) +
  geom_point(x = summary.table[[1, 4]],  # ignition time x2
             y = summary.table[[1, 5]],  # ignition time y2
             color = color.list[[4]],
             size  = size.list[[2]],
             alpha = alpha.list[[2]]) +
  geom_point(x = summary.table[[1, 6]],  # max temp x3
             y = summary.table[[1, 7]],  # max temp y3
             color = color.list[[5]], 
             size  = size.list[[2]], 
             alpha = alpha.list[[2]]) +
  geom_point(x = summary.table[[1, 8]],  # exo duration x4
             y = summary.table[[1, 9]],  # exo duration y4
             color = color.list[[6]], 
             size  = size.list[[2]],
             alpha = alpha.list[[2]])

p2 <- ggplot(data = df.trim, aes(x = time, y  = pyro2)) + 
  
  theme(panel.background = element_rect(fill  = color.list[[7]],
                                        color = color.list[[8]]),
        panel.grid.major = element_line(color = color.list[[9]],
                                        size  = size.list[[4]]),
        panel.grid.minor = element_line(color = color.list[[10]], 
                                        size  = size.list[[5]]),
        panel.grid.major.y = element_line(color = color.list[[9]],
                                          size  = size.list[[4]]),
        panel.grid.minor.y = element_line(color = color.list[[10]],
                                          size  = size.list[[5]]),
        axis.title.x = element_text(vjust = xy.scale.list[[7]]),
        axis.title.y = element_text(vjust = xy.scale.list[[8]])) + 
  scale_x_continuous("time (sec)",
                     limits = c(xy.scale.list[[1]],
                                xy.scale.list[[2]]), 
                     breaks = seq.int(xy.scale.list[[1]],
                                      xy.scale.list[[2]],
                                      xy.scale.list[[3]])) +
  scale_y_continuous("temperature (°C)",
                     limits = c(xy.scale.list[[4]],
                                xy.scale.list[[5]]), 
                     breaks = seq.int(xy.scale.list[[4]],
                                      xy.scale.list[[5]],
                                      xy.scale.list[[6]])) + 
  ggtitle("pyro2") +
  geom_point(color = color.list[[1]],  # pyro values 
             size  = size.list[[1]],
             alpha = alpha.list[[1]]) +
  geom_point(x = summary.table[[2, 1]],  # drop time x1
             y = summary.table[[2, 2]],  # drop time y1
             color = color.list[[2]],
             size  = size.list[[2]],
             alpha = alpha.list[[2]]) +
  geom_point(y = summary.table[[2, 3]],  # baseline average y-value
             color = color.list[[3]],
             size  = size.list[[3]],
             alpha = alpha.list[[3]]) +
  geom_point(x = summary.table[[2, 4]],  # ignition time x2
             y = summary.table[[2, 5]],  # ignition time y2
             color = color.list[[4]],
             size  = size.list[[2]],
             alpha = alpha.list[[2]]) +
  geom_point(x = summary.table[[2, 6]],  # max temp x3
             y = summary.table[[2, 7]],  # max temp y3
             color = color.list[[5]], 
             size  = size.list[[2]], 
             alpha = alpha.list[[2]]) +
  geom_point(x = summary.table[[2, 8]],  # exo duration x4
             y = summary.table[[2, 9]],  # exo duration y4
             color = color.list[[6]], 
             size  = size.list[[2]],
             alpha = alpha.list[[2]])

p3 <- ggplot(data = df.trim, aes(x = time, y  = pyro3)) + 

  theme(panel.background = element_rect(fill  = color.list[[7]],
                                        color = color.list[[8]]),
        panel.grid.major = element_line(color = color.list[[9]],
                                        size  = size.list[[4]]),
        panel.grid.minor = element_line(color = color.list[[10]], 
                                        size  = size.list[[5]]),
        panel.grid.major.y = element_line(color = color.list[[9]],
                                          size  = size.list[[4]]),
        panel.grid.minor.y = element_line(color = color.list[[10]],
                                          size  = size.list[[5]]),
        axis.title.x = element_text(vjust = xy.scale.list[[7]]),
        axis.title.y = element_text(vjust = xy.scale.list[[8]])) + 
  scale_x_continuous("time (sec)",
                     limits = c(xy.scale.list[[1]],
                                xy.scale.list[[2]]), 
                     breaks = seq.int(xy.scale.list[[1]],
                                      xy.scale.list[[2]],
                                      xy.scale.list[[3]])) +
  scale_y_continuous("temperature (°C)",
                     limits = c(xy.scale.list[[4]],
                                xy.scale.list[[5]]), 
                     breaks = seq.int(xy.scale.list[[4]],
                                      xy.scale.list[[5]],
                                      xy.scale.list[[6]])) + 
  ggtitle("pyro3") +
  geom_point(color = color.list[[1]],  # pyro values 
             size  = size.list[[1]],
             alpha = alpha.list[[1]]) +
  geom_point(x = summary.table[[3, 1]],  # drop time x1
             y = summary.table[[3, 2]],  # drop time y1
             color = color.list[[2]],
             size  = size.list[[2]],
             alpha = alpha.list[[2]]) +
  geom_point(y = summary.table[[3, 3]],  # baseline average y-value
             color = color.list[[3]],
             size  = size.list[[3]],
             alpha = alpha.list[[3]]) +
  geom_point(x = summary.table[[3, 4]],  # ignition time x2
             y = summary.table[[3, 5]],  # ignition time y2
             color = color.list[[4]],
             size  = size.list[[2]],
             alpha = alpha.list[[2]]) +
  geom_point(x = summary.table[[3, 6]],  # max temp x3
             y = summary.table[[3, 7]],  # max temp y3
             color = color.list[[5]], 
             size  = size.list[[2]], 
             alpha = alpha.list[[2]]) +
  geom_point(x = summary.table[[3, 8]],  # exo duration x4
             y = summary.table[[3, 9]],  # exo duration y4
             color = color.list[[6]], 
             size  = size.list[[2]],
             alpha = alpha.list[[2]])

df.trim.melt <- melt(df.trim, id = c("time"))

p4 <- ggplot(data = df.trim.melt, aes(x = time,
                                      y = value, 
                                      group = variable, 
                                      color = variable)) + 
  theme(panel.background = element_rect(fill  = color.list[[7]],
                                        color = color.list[[8]]),
        panel.grid.major = element_line(color = color.list[[9]],
                                        size  = size.list[[4]]),
        panel.grid.minor = element_line(color = color.list[[10]], 
                                        size  = size.list[[5]]),
        panel.grid.major.y = element_line(color = color.list[[9]],
                                          size  = size.list[[4]]),
        panel.grid.minor.y = element_line(color = color.list[[10]],
                                          size  = size.list[[5]]),
        axis.title.x = element_text(vjust = xy.scale.list[[7]]),
        axis.title.y = element_text(vjust = xy.scale.list[[8]])) + 
  scale_x_continuous("time (sec)",
                     limits = c(xy.scale.list[[1]],
                                xy.scale.list[[2]]), 
                     breaks = seq.int(xy.scale.list[[1]],
                                      xy.scale.list[[2]],
                                      xy.scale.list[[3]])) +
  scale_y_continuous("temperature (°C)",
                     limits = c(xy.scale.list[[4]],
                                xy.scale.list[[5]]), 
                     breaks = seq.int(xy.scale.list[[4]],
                                      xy.scale.list[[5]],
                                      xy.scale.list[[6]])) + 
  ggtitle("All samples overlay") +
  scale_colour_discrete(name = "Sample") +
  geom_line(alpha = alpha.list[[4]]) +
  geom_line(y     = summary.table[[1, 3]], 
            color = "red",
            alpha = alpha.list[[5]]) +
  geom_line(y     = summary.table[[2, 3]], 
            color = "green", 
            alpha = alpha.list[[5]]) +
  geom_line(y     = summary.table[[3, 3]], 
            color = "blue", 
            alpha = alpha.list[[5]])

p5 <- qplot(1:10, 1:10, geom = "blank") + 
  theme_bw() + 
  theme(panel.grid.major = element_line(color = "white")) + 
  scale_x_discrete("",breaks = NULL) +
  scale_y_discrete("",breaks = NULL) +
  annotation_custom(grob = tableGrob(calculations.table.total,
                                   gpar.corefill = gpar(fill = color.list[[11]],
                                                        alpha=0.5, 
                                                        col = NA),
                                   h.even.alpha = 0.5))

p6.sample.title <- c("Sample: ", 
                  paste0(prefix),
                  "Report created: ", 
                  date())  # appears in last graph

p6 <- qplot(1:10, 1:10, geom = "blank") + 
  theme_bw() + 
  theme(panel.grid.major = element_line(color = "white")) + 
  scale_x_discrete("",breaks = NULL) +
  scale_y_discrete("",breaks = NULL) +
  annotation_custom(grob = tableGrob(p6.sample.title,
                               gpar.corefill = gpar(fill = color.list[[12]],
                                                    alpha=0.5, 
                                                    col = NA),
                               h.even.alpha = 0.5,
                               gpar.coretext = gpar(fontsize = size.list[[6]])))

# ======================
# GRAPHIC OUTPUT
# ===========================
multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
  # function taken from http://goo.gl/6PJFSB
  # comments removed
  require(grid)
  plots <- c(list(...), plotlist)
  numPlots = length(plots)
  if (is.null(layout)) {
    layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                     ncol = cols, nrow = ceiling(numPlots/cols))
  }
  if (numPlots==1) {
    print(plots[[1]])
    } else {
      grid.newpage()
      pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))
      for (i in 1:numPlots) {
        matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))
        print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                        layout.pos.col = matchidx$col))
      }
    }
}

multiplot(p1, p2, p3, p4, p5, p6, cols=2)

# ======================
# EXPORT FUNCTIONS
# ===========================
exportPdf <- function(){
  file.name.pdf <- paste0(prefix, ".pdf")
  pdf(file = file.name.pdf,
      width = 11,
      height = 8.5)
multiplot(p1, p2, p3, p4, p5, p6, cols=2)
dev.off()
}

exportPng <- function(){
  file.name.png <- paste0(prefix, ".png")
  dev.copy(png, file = file.name.png,
           width = 1200,
           height = 900)
  multiplot(p1, p2, p3, p4, p5, p6, cols=2)
  dev.off()
}

exportEPS <- function(){
  file.name.eps <- paste0(prefix, ".eps")
  dev.copy(eps, file = file.name.png,
           width = 1200,
           height = 900)
  multiplot(p1, p2, p3, p4, p5, p6, cols=2)
  dev.off()
}
