library(tuneR)
library(seewave)


library(phonTools)

snd <- loadsound("WAV FILE URL")
pitch <- pitchtrack(snd)
pitch$frames <- ceiling(pitch$time*0.025)
write.csv(pitch, "FILE_prosodic.csv", row.names = FALSE)

