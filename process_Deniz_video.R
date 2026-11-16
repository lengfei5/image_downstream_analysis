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

inputDir = "/groups/tanaka/People/current/Deniz/Data/VBA/phototaxis/exp222_ONT/"
destDir = "/groups/tanaka/People/current/jiwang/projects/image_analysis/Deinz_axolot_tracking/run_1"

workDir = "/groups/tanaka/People/current/jiwang/projects/image_analysis/scripts"

##########################################
# first run, to move videos of A15, A27, A34, A39, A60
# each animal has 5 time points, each time point has 6 videos
# total 5*5*6 = 150 videos
##########################################
#all_animals = c('A15', 'A27', 'A34', 'A39')

aninmal = 'A37'

aninmal = 'A42'

aninmal = 'A60'

aninmal = 'A51'

aninmal = 'A58'

aninmal = 'A74'

aninmal = 'A5'

print(getwd())
nb_video = 0
cat(aninmal, '\n')

# cd animal folder
setwd(paste0(inputDir, aninmal))
#system(paste0('cd ', inputDir, aninmal))
print(getwd())

## loop over time points 
list_timePoints = list.dirs(path = paste0(inputDir, aninmal), full.names = FALSE, recursive = FALSE)

for(time in list_timePoints)
{
  setwd(paste0(inputDir, aninmal, '/', time))
  
  list_videos = list.dirs(path = paste0(inputDir, aninmal, '/', time), 
                          full.names = FALSE, recursive = FALSE)
  for(v in list_videos)
  {
    #print(v)
    newName = paste0(aninmal, '_', time, '_', v)
    cat(v, ' ---> ', newName, '\n')
    nb_video = nb_video + 1
    
    system(paste0('mv ', v, ' ', newName)) # change video names
    system(paste0('mv ', newName, ' ', destDir)) # move the video to destination
    
  }
}

cat('total videos for ', aninmal, ': ', nb_video, '\n')
# back to inputDir  
setwd(inputDir)
#print(getwd())

setwd(workDir)
print(getwd())



