library(trend)

df.f <- read.csv("face_data/face_analysis.csv", header = T) # data from RoboNLP_facial_data_collection.ipynb
faces <- read.csv("face_annotations.csv") # data from RoboNLP_facial_data_collection.ipynb 
                                          # merged with ELAN annotations of speaking_actor and pauses

face.while.speak.stat = faces %>%
  group_by(speaking_actor, condition) %>%
  summarise(happ.avg = mean(happiness, na.rm=TRUE),
            happ.sd = sd(happiness, na.rm=TRUE),
            happ.min = min(happiness, na.rm=TRUE),
            happ.max = max(happiness, na.rm=TRUE),
            surprise.avg = mean(surprise, na.rm=TRUE),
            surprise.sd = sd(surprise, na.rm=TRUE),
            surprise.min = min(surprise, na.rm=TRUE),
            surprise.max = max(surprise, na.rm=TRUE))

face.while.pause.stat = faces %>%
  group_by(pauses, condition) %>%
  summarise(happ.avg = mean(happiness, na.rm=TRUE),
            happ.sd = sd(happiness, na.rm=TRUE),
            happ.min = min(happiness, na.rm=TRUE),
            happ.max = max(happiness, na.rm=TRUE),
            surprise.avg = mean(surprise, na.rm=TRUE),
            surprise.sd = sd(surprise, na.rm=TRUE),
            surprise.min = min(surprise, na.rm=TRUE),
            surprise.max = max(surprise, na.rm=TRUE))


face.while.speak.stat.by.subj = faces %>%
  group_by(subejct, speaking_actor, condition) %>%
  filter(!is.na(speaking_actor)) %>%
  summarise(happ.avg = mean(happiness, na.rm=TRUE),
            happ.min = min(happiness, na.rm=TRUE),
            happ.max = max(happiness, na.rm=TRUE),
            surprise.avg = mean(surprise, na.rm=TRUE),
            surprise.min = min(surprise, na.rm=TRUE),
            surprise.max = max(surprise, na.rm=TRUE))
face.while.speak.stat.by.subj$subejct <- gsub("_", "", face.while.speak.stat.by.subj$subejct)
scores.faces2 <- merge(x = face.while.speak.stat.by.subj, y = scores, by.x = "subejct", 
                       by.y = "subject_ID", all.x = TRUE)


face.while.pause.stat.by.subj = faces %>%
  group_by(subejct, pauses, condition) %>%
  filter(!is.na(pauses)) %>%
  summarise(happ.avg = mean(happiness, na.rm=TRUE),
            happ.min = min(happiness, na.rm=TRUE),
            happ.max = max(happiness, na.rm=TRUE),
            surprise.avg = mean(surprise, na.rm=TRUE),
            surprise.min = min(surprise, na.rm=TRUE),
            surprise.max = max(surprise, na.rm=TRUE))
face.while.pause.stat.by.subj$subejct <- gsub("_", "", face.while.pause.stat.by.subj$subejct)
scores.faces3 <- merge(x = face.while.pause.stat.by.subj, y = scores, by.x = "subejct", 
                      by.y = "subject_ID", all.x = TRUE)

scores.faces2.robot <- subset(scores.faces2, speaking_actor == "robot_speaking")
scores.faces2.human <- subset(scores.faces2, speaking_actor == "human_speaking")


cor.mat.sp.all <- as.data.frame(cor(scores.faces2[,c(4:31)], method = "spearman", use="complete.obs"))[7:28,1:6]
cor.mat.sp.h <- as.data.frame(cor(scores.faces2.human[,c(4:31)], method = "spearman", use="complete.obs"))[7:28,1:6]
cor.mat.sp.r <- as.data.frame(cor(scores.faces2.robot[,c(4:31)], method = "spearman", use="complete.obs"))[7:28,1:6]

names(scores.faces2.robot) <- names(scores.faces2.human) <- 
  c("id", "actor", "cond", "Hap", "H.mn", "H.mx", "Srp", "S.mn", "S.mx", 
    "FkNt","McHum","Cns","Lflk","Fr", "Knd", "Pls","AwNc",
    "AnyEnj","Lk","Cmp", "IgKnw", "Rsp", "Intlg", "FlSns", "Exp", "ObsSpr", "Prd",
    "CnfCl", "CmplE", "Und", "Scr", "cond")

corrplot::corrplot.mixed(cor(scores.faces2.robot[,c(4,7,10:31)], method = "spearman", use="complete.obs"),
                         lower = "circle", upper = "number", 
                         main = "How emotions during robot speech correlate with perception of robot", mar=c(0,0,1,0),
                         tl.cex = .8, number.cex = 0.8, number.digits = 1,
                         col=colorRampPalette(c("red","grey45","blue"))(200))
corrplot::corrplot.mixed(cor(scores.faces2.human[,c(4,7,10:31)], method = "spearman", use="complete.obs"),
                         lower = "circle", upper = "number", 
                         main = "How emotions during human speech correlate with perception of robot", mar=c(0,0,1,0),
                         tl.cex = .8, number.cex = 0.8, number.digits = 1,
                         col=colorRampPalette(c("red","grey45","blue"))(200))

corrplot::corrplot.mixed(cor(scores.faces3[,c(4,7,10:31)], method = "spearman", use="complete.obs"),
                         lower = "circle", upper = "number", 
                         main = "How emotions during pauses correlate with perception of robot", mar=c(0,0,1,0),
                         tl.cex = .8, number.cex = 0.8, number.digits = 1,
                         col=colorRampPalette(c("red","grey45","blue"))(200))

names(scores.faces3) <- c("id", "pause", "cond", "Hap", "H.mn", "H.mx", "Srp", "S.mn", "S.mx", 
    "FkNt","McHum","Cns","Lflk","Fr", "Knd", "Pls","AwNc",
    "AnyEnj","Lk","Cmp", "IgKnw", "Rsp", "Intlg", "FlSns", "Exp", "ObsSpr", "Prd",
    "CnfCl", "CmplE", "Und", "Scr", "cond")
scores.faces3.robot <- subset(scores.faces3, pauses == "pause") # pauses after human, those that robot makes
scores.faces3.human <- subset(scores.faces3, pauses == "pause after robot") # those of humans

cor.mat.ps.all <- as.data.frame(cor(scores.faces3[,c(4:31)], method = "spearman", use="complete.obs"))[7:28,1:6]
cor.mat.ps.h <- as.data.frame(cor(scores.faces3.human[,c(4:31)], method = "spearman", use="complete.obs"))[7:28,1:6]
cor.mat.ps.r <- as.data.frame(cor(scores.faces3.robot[,c(4:31)], method = "spearman", use="complete.obs"))[7:28,1:6]



corrplot::corrplot.mixed(cor(scores.faces3.robot[,c(4,7,10:31)], method = "spearman", use="complete.obs"),
                         lower = "circle", upper = "number", 
                         main = "How emotions during robot pauses correlate with perception of robot", mar=c(0,0,1,0),
                         tl.cex = .8, number.cex = 0.8, number.digits = 1,
                         col=colorRampPalette(c("red","grey45","blue"))(200))
corrplot::corrplot.mixed(cor(scores.faces3.human[,c(4,7,10:31)], method = "spearman", use="complete.obs"),
                         lower = "circle", upper = "number", 
                         main = "How emotions during human pauses correlate with perception of robot", mar=c(0,0,1,0),
                         tl.cex = .8, number.cex = 0.8, number.digits = 1,
                         col=colorRampPalette(c("red","grey45","blue"))(200))


ggplot(subset(faces, !is.na(subject_id)), aes(frame, happiness)) +
  geom_smooth(method = "loess", n = 100000, se = F,  span = 0.3) + 
  theme_minimal() + 
  facet_wrap( ~ subject_id, ncol=2)


subj2 <- subset(faces, subject_id == 2)
subj3 <- subset(faces, subject_id == 3)
subj4 <- subset(faces, subject_id == 4)
subj5 <- subset(faces, subject_id == 5)

subj2c <- subset(subj2, condition=="chat")
subj2t <- subset(subj2, condition == "task")
plot(ts(subj2c$happiness, frequency = 25*60))
abline(reg=lm(ts(subj2c$happiness, frequency = 25*60)~time(ts(subj2c$happiness, frequency = 25*60))))
plot(decompose(ts(subj2c$happiness, frequency = 25*30))$trend) # per half-min
plot(ts(subj2t$happiness, frequency = 25*60))
abline(reg=lm(ts(subj2t$happiness, frequency = 25*60)~time(ts(subj2t$happiness, frequency = 25*60))))
plot(decompose(ts(subj2t$happiness, frequency = 25*30))$trend) # per half-min

plot(ts(subj2c$surprise, frequency = 25*60))
abline(reg=lm(ts(subj2c$surprise, frequency = 25*60)~time(ts(subj2c$surprise, frequency = 25*60))))
plot(decompose(ts(subj2c$surprise, frequency = 25*30))$trend) # per half-min
plot(ts(subj2t$surprise, frequency = 25*60))
abline(reg=lm(ts(subj2t$surprise, frequency = 25*60)~time(ts(subj2t$surprise, frequency = 25*60))))
plot(decompose(ts(subj2t$surprise, frequency = 25*30))$trend) # per half-min


subj3c <- subset(subj3, condition=="chat")
subj3t <- subset(subj3, condition == "task")
plot(ts(subj3c$happiness, frequency = 25*60))
abline(reg=lm(ts(subj3c$happiness, frequency = 25*60)~time(ts(subj3c$happiness, frequency = 25*60))))
plot(decompose(ts(subj3c$happiness, frequency = 25*60))$trend) # per min
plot(ts(subj3t$happiness, frequency = 25*60))
abline(reg=lm(ts(subj3t$happiness, frequency = 25*60)~time(ts(subj3t$happiness, frequency = 25*60))))
plot(decompose(ts(subj3t$happiness, frequency = 25*60))$trend) # per min

plot(ts(subj3c$surprise, frequency = 25*60))
abline(reg=lm(ts(subj3c$surprise, frequency = 25*60)~time(ts(subj3c$surprise, frequency = 25*60))))
plot(decompose(ts(subj3c$surprise, frequency = 25*60))$trend) # per min
plot(ts(subj3t$surprise, frequency = 25*60))
abline(reg=lm(ts(subj3t$surprise, frequency = 25*60)~time(ts(subj3t$surprise, frequency = 25*60))))
plot(decompose(ts(subj3t$surprise, frequency = 25*60))$trend) # per min



subj4c <- subset(subj4, condition=="chat")
subj4t <- subset(subj4, condition == "task")
plot(ts(subj4c$happiness, frequency = 25*60))
abline(reg=lm(ts(subj4c$happiness, frequency = 25*60)~time(ts(subj4c$happiness, frequency = 25*60))))
plot(decompose(ts(subj4c$happiness, frequency = 25*10))$trend) # per min
plot(ts(subj4t$happiness, frequency = 25*60))
abline(reg=lm(ts(subj4t$happiness, frequency = 25*60)~time(ts(subj4t$happiness, frequency = 25*60))))
plot(decompose(ts(subj4t$happiness, frequency = 25*10))$trend) # per min

plot(ts(subj4c$surprise, frequency = 25*60))
abline(reg=lm(ts(subj4c$surprise, frequency = 25*60)~time(ts(subj4c$surprise, frequency = 25*60))))
plot(decompose(ts(subj4c$surprise, frequency = 25*10))$trend) # per min
plot(ts(subj4t$surprise, frequency = 25*60))
abline(reg=lm(ts(subj4t$surprise, frequency = 25*60)~time(ts(subj4t$surprise, frequency = 25*60))))
plot(decompose(ts(subj4t$surprise, frequency = 25*10))$trend) # per min



subj5c <- subset(subj5, condition=="chat")
subj5t <- subset(subj5, condition == "task")
plot(ts(subj5c$happiness, frequency = 25*60))
abline(reg=lm(ts(subj5c$happiness, frequency = 25*60)~time(ts(subj5c$happiness, frequency = 25*60))))
plot(decompose(ts(subj5c$happiness, frequency = 25*60))$trend) # per min
plot(ts(subj5t$happiness, frequency = 25*60))
abline(reg=lm(ts(subj5t$happiness, frequency = 25*60)~time(ts(subj5t$happiness, frequency = 25*60))))
plot(decompose(ts(subj5t$happiness, frequency = 25*60))$trend) # per min

plot(ts(subj5c$surprise, frequency = 25*60))
abline(reg=lm(ts(subj5c$surprise, frequency = 25*60)~time(ts(subj5c$surprise, frequency = 25*60))))
plot(decompose(ts(subj5c$surprise, frequency = 25*60))$trend) # per min
plot(ts(subj5t$surprise, frequency = 25*60))
abline(reg=lm(ts(subj5t$surprise, frequency = 25*60)~time(ts(subj5t$surprise, frequency = 25*60))))
plot(decompose(ts(subj5t$surprise, frequency = 25*60))$trend) # per min


require(trend)
hap.002c.mk <- mk.test(ts(subj2c$happiness))
summary.trend.test(hap.002c.mk)
#hap.002c.slope <- sens.slope(ts(subj2c$happiness, frequency = 25*30))
hap.002c.poch <- pettitt.test(ts(subj2c$happiness))
sea.sens.slope(ts(subj2t$happiness, frequency = 25*60))

a<-lm(ts(df.f$happiness, frequency = 25*60)~time(ts(df.f$happiness, frequency = 25*60)))$coefficients[2]


