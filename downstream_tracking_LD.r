##########################################################################
##########################################################################
# Project: Deniz's axolotl tracking video
# Script purpose: organize videos and change the video folder names with metadata
# Usage example: 
# Author: Jingkui Wang (jingkui.wang@imp.ac.at)
# Date of creation: Fri May 29 12:55:39 2026
##########################################################################
##########################################################################
rm(list = ls())

inputDir = "/groups/tanaka/Collaborations/Jingkui-Deniz/run_1/"
resDir = "/groups/tanaka/People/current/jiwang/projects/image_analysis/results/light_dark_test"

workDir = "/groups/tanaka/People/current/jiwang/projects/image_analysis/scripts"

if(!dir.exists(resDir)) dir.create(resDir)

## interpolation function for missing frames
linear_interpolate_missing_frames = function(df)
{
  
  # df <- data.frame(
  #   frame = c(10, 13, 14, 17),
  #   x = c(20, 26, 28, 34),
  #   y = c(40, 49, 52, 61)
  # )
  
  # Validate frame indices
  stopifnot(
    nrow(df) > 0,
    all(is.finite(df$frame)),
    all(df$frame == floor(df$frame)),
    !anyDuplicated(df$frame)
  )
  
  df <- df[order(df$frame), ]
  
  # Include every frame between the first and last observations
  all_frames <- seq.int(min(df$frame), max(df$frame))
  
  # Interpolate a coordinate using its available observations
  interpolate_position <- function(position) {
    valid <- is.finite(position)
    
    if (sum(valid) < 2) {
      stop("At least two valid observations are needed per coordinate.")
    }
    
    approx(
      x = df$frame[valid],
      y = position[valid],
      xout = all_frames,
      method = "linear",
      rule = 1  # Return NA outside the observed range; no extrapolation
    )$y
  }
  
  result <- data.frame(
    frame = all_frames,
    x = interpolate_position(df$x),
    y = interpolate_position(df$y)
  )
  
  #print(result)
  
  return(result)
  
}

derivative <- function(values) {
  n <- length(values)
  
  if (n < 3L) {
    stop("At least three time points are required.")
  }
  #if (any(!is.finite(time)) || any(diff(time) <= 0)) {
  #  stop("Time must be finite and strictly increasing.")
  #}
  
  # Central differences (for regularly spaced time points)
  #middle <- (values[3:n] - values[1:(n - 2)])
    
  return(
    values[2:n] - values[1:(n-1)]     
  )
}


########################################################
########################################################
# Section I: downstream analysis of light-dark test 
# 
########################################################
########################################################

##########################################
# first run, to move videos of A15, A27, A34, A39, A60
# each animal has 5 time points, each time point has 6 videos
# total 5*5*6 = 150 videos
##########################################
inputDir = "/groups/tanaka/People/current/jiwang/projects/image_analysis/results/light_dark_test/tables_tracking"

all_animals = c('A39', 'A60', 'A51', 'A42', 'A34', 'A37', 'A27', 'A74', 'A58', 'A15', 'A5')

res = c()

for(animal in all_animals)
{
  # animal = 'A60'
  cat(animal, '\n')
  
  #outDir = paste0(inputDir, animal)
  
  traces = list.files(path = inputDir, pattern = '*.csv', full.names = TRUE)
  traces = traces[grep(paste0(animal, '_'), traces)]
  
  traces = traces[grep('_error.csv', traces, invert = TRUE)]
  
  cycles = traces[grep('_circle.csv', traces, invert = TRUE)]
  traces = traces[grep('_circle.csv', traces, invert = TRUE)]
  
  cat(length(traces), ' traces and ', length(cycles), ' cycles\n')
  
  ff = basename(traces)
  ff = gsub('.csv', '', ff)
  
  for(n in 1:length(ff))
  {
    # n = 1 
    cc = unlist(strsplit(ff[n], '_'))
    cc = c(cc[1], cc[2], paste0(cc[3], '_', cc[4]))
    
    trc = read.csv(file = traces[n], row.names = c(1), header = TRUE)
    trc = data.frame(trc)
    colnames(trc) = c('frame', 'x', 'y')
    
    cyl = read.csv(file = paste0(inputDir, '/', ff[n], '_circle.csv'), row.names = c(1), header = TRUE)
    cycle_y = cyl$y 
    
    total_frames = max(trc$frame) - min(trc$frame) + 1
    missing_frames = max(trc$frame) - min(trc$frame) + 1 - nrow(trc)
    
    cat('-- trace -- ', ff[n], '-- missing frame :', missing_frames,  '--\n')
    
    if(missing_frames > 50){
      res = rbind(res, c(cc, total_frames, missing_frames, NA, NA, NA, NA))
    }else{
      if(missing_frames > 0){
        trc = linear_interpolate_missing_frames(trc)
      }
      
      nb_upper = length(which(trc$y < cycle_y))
      nb_lower = length(which(trc$y > cycle_y))
      nb_onDiameter = length(which(trc$y == cycle_y))
      
      
      pct_upper = nb_upper/nrow(trc)
      pct_lower = nb_lower/nrow(trc)
      
      ## maximum of speed for QC
      velocity_x = derivative(trc$x)
      velocity_y = derivative(trc$y)
      speed = sqrt(velocity_x^2 + velocity_y^2)
      
      max_speed = max(speed)
      total_dist = sum(speed)
      
      res = rbind(res, c(cc, total_frames, missing_frames, pct_upper, pct_lower, max_speed, total_dist))
      
    }
    
  }
  
}

res = data.frame(res)


colnames(res) = c('animal', 'condition', 'test_index',
                  'nb_totalFrame', 'nb_missingFrame', 'pct_upper', 'pct_lower', 'max_speed', 'total_dist')

res$pct_upper = as.numeric(res$pct_upper)
res$pct_lower = as.numeric(res$pct_lower)
res$nb_totalFrame = as.numeric(res$nb_totalFrame)
res$nb_missingFrame = as.numeric(res$nb_missingFrame)
res$max_speed = as.numeric(res$max_speed)
res$total_dist = as.numeric(res$total_dist)


saveRDS(res, file = paste0(resDir, '/res_tables_all_11animals_allTimepoints_updated_v1.rds'))


##########################################
# QC and double check the numbers 
##########################################
res = readRDS(file = paste0(resDir, '/res_tables_all_11animals_allTimepoints_updated_v1.rds'))

res = res[order(res$nb_totalFrame), ]

head(res, 10)

#res = res[, c(1, 2, 3, 4, 8, 9)]
hist(as.numeric(res$pct_upper), breaks = 100); abline(v = 0.5, lwd = 2.0, col = 'red')

res = res[order(-res$nb_missingFrame), ]
#head(res[which(!is.na(res$pct_upper)), ])

head(res[, c(1:7)], 15)

res = res[order(-res$max_speed), ]

head(res, 15)


res = res[order(res$total_dist), ]

head(res, 15)

res = res[order(res$pct_upper), ]


head(res[which(res$condition == "preONT"), c(1,2,3,6, 8,9)], 20)

kk = which(res$condition == 'preONT')
plot(res$total_dist[kk], res$pct_upper[kk])

xx = res[which(res$condition == "preONT" & res$animal == 'A39'), ]
xx = xx[order(xx$test_index), ]


pcts = res$pct_upper[which(res$condition == 'preONT')]
pcts = pcts[!is.na(pcts)]


t.test(pcts, mu = 0.5, alternative = 'greater')


pcts = res$pct_upper
pcts = pcts[!is.na(pcts)]

t.test(pcts, mu = 0.5, alternative = 'greater')


require(ggplot2)
res$animal = factor(res$animal, levels = c(
  "A5", "A34", "A37", "A39", "A42", "A51", "A15", "A27", "A58", "A60", "A74"))
res$condition = factor(res$condition, levels = c('preONT', '1wpt', '4wpt', '6wpt', '8wpt'))

p1 <- ggplot(res, aes(x=condition, y=pct_upper, fill=condition)) + 
  geom_boxplot() +
  geom_jitter(color="darkgreen", size= 2.0, alpha=0.9) + 
  geom_hline(yintercept = 0.5, color = "red") + 
  facet_wrap(~ animal)

plot(p1)

ggsave(filename =  paste0(resDir, '/results_timepoints_forAnaimals_updated_v2.pdf'), width = 16, height = 8)


p2 <- ggplot(res, aes(x=animal, y=pct_upper, fill=animal)) + 
  geom_boxplot() + 
  geom_jitter(color="darkgreen", size= 2.0, alpha=0.9) + 
  geom_hline(yintercept = 0.5, color = "red") + 
  facet_wrap(~condition)

plot(p2)

ggsave(filename =  paste0(resDir, '/results_animals_perTimepoints_updated_v2.pdf'), width = 20, height = 8)



##########################################
# double check the updated video processing  
##########################################
#errs = list.files(path = paste0(resDir, '/updated_tracking'), pattern = '*_error.csv', full.names = TRUE)
#traces = traces[grep('_error.csv', traces, invert = TRUE)]




