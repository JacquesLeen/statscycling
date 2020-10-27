#CX Data Analysis
## ================================================================================##
# ==============================READ ME=======================================#

# Always check the number of laps in the race, editing on the dataframe must be done accordingly.
# Always check presence of a starting loop in the race. If not present comment all _NSL script lines.
# modfiy nlaps only during the generation of the variable
# modify lap_length only during generation of the variable (meters)
# when computing mean and sd check the presence of startloop: if it wasnt in the race all occurences of 
#DF_LapTime_NSL should become DF_LapTime
# fix the ratio for the jpeg table -> recomended: 800x400

#Data and variables
DF = read.csv("MTB-CX.csv")

nlaps= #FILL WITH NLAPS 
lap_length =  #FILL WITH LAP LENGTH

## ================================================================================##
#Data frame manip -> conversion to seconds and creation of timegaps
#install.packages("lubridate")
library(lubridate)
DF_LapTime= DF[-c(11:13),]
for (i in 2:11) {
  DF_LapTime[[i]]= period_to_seconds(hms(DF_LapTime[[i]]))
}

# no start loop
DF_LapTime_NSL= DF_LapTime[-1,]
DF_LapTime_NSL

#timegaps
DF_TimeGap = DF_LapTime[1:6]
for (j in 1:nlaps) {
  temp = DF_TimeGap[j,2]
  for (t in 2:6) {
    if(j == 1){
      DF_TimeGap[j,t] = (DF_TimeGap[j,t] - temp) 
    }
    else{
      DF_TimeGap[j,t] = DF_TimeGap[j-1,t] + (DF_TimeGap[j,t] - temp)
    }
  }
}

# Mean and SD Laps -> "index" of how steady the rider was tempo-wise
Mean = c(1:10)
SD = c(1:10)
AVGSpeed = c(1:10)
for(i in 2:11){
  Mean[i-1]= round( mean(DF_LapTime_NSL[,i]), digits = 4 )
  SD[i-1]= round ( sd(DF_LapTime_NSL[,i]), digits= 4 )
  AVGSpeed[i-1] = round( (lap_length/Mean[i-1]) * 3.6, digits=4 )
}
DF_Stats = data.frame(matrix (ncol = 10, nrow = 3))
colnames(DF_Stats)= c(colnames(DF[2:11]))
DF_Stats[1,]= Mean
DF_Stats[2,]= SD
DF_Stats[3,]= AVGSpeed
row.names(DF_Stats)= c("LT_Mean (s)", "LT_SD (s)", "Lap_Speed (km/h)")
DF_Stats= t(DF_Stats)
Finish = c(1:10)
DF_Stats= cbind(DF_Stats, Finish)


## ================================================================================##
#melting the dataframe -> easy access for ggplot
library(reshape2)
DFMelt = melt(DF_LapTime, id.var="Lap")
names(DFMelt)[2] = "Rider"
names(DFMelt)[3] = "Time"
DFMelt$Lap = as.numeric(as.character(DFMelt$Lap))

DFMelt_NSL = melt(DF_LapTime_NSL, id.var="Lap")
names(DFMelt_NSL)[2] = "Rider"
names(DFMelt_NSL)[3] = "Time"
DFMelt_NSL$Lap = as.numeric(as.character(DFMelt_NSL$Lap))

DFMelt_TimeGap = melt(DF_TimeGap, id.var="Lap")
names(DFMelt_TimeGap)[2] = "Rider"
names(DFMelt_TimeGap)[3] = "Gap"
DFMelt_TimeGap$Lap = as.numeric(as.character(DFMelt_TimeGap$Lap))


## ================================================================================##
#Graphical objects
#full
library(ggplot2)
ggplot(DFMelt, aes(x = Lap, y = Time, col= Rider, group = Rider)) +
  geom_point()+
  geom_line()+
  xlab("Lap")+ ylab("Time (s)")+
  ggtitle("Race Name Lap Time (Full)")+
  scale_color_manual(values = c ("darkorange","chocolate","orangered1","red4", "black",
                                 "violetred", "darkviolet", "blue4", "blue2","deepskyblue2"))+
  scale_x_continuous(breaks = 0:nlaps)
ggsave("RACE_FULL.jpg")

#no start loop
ggplot(DFMelt_NSL, aes(x = Lap, y = Time, col= Rider, group = Rider)) +
  geom_point()+
  geom_line()+
  xlab("Lap")+ ylab("Time (s)")+
  ggtitle("Race Name Lap Time (No start Loop)")+
  scale_color_manual(values = c ("darkorange","chocolate","orangered1","red4", "black",
                                 "violetred", "darkviolet", "blue4", "blue2", "deepskyblue2"))+
  scale_x_continuous(breaks = 0:nlaps)
ggsave("RACE_NSL.jpg")

#timegaps
ggplot(DFMelt_TimeGap, aes(x = Lap, y = Gap, col= Rider, group = Rider)) +
  geom_point()+
  geom_line()+
  xlab("Lap")+ ylab("Gap (s)")+
  ggtitle("Race Name Gap Top5")+
  ggtitle("SP Boom Gap Top10")+
  scale_color_manual(values = c ("darkorange","chocolate","orangered1","red4", "black",
                                 "violetred", "darkviolet", "blue4", "blue2", "deepskyblue2"))+
  scale_x_continuous(breaks = 0:nlaps)
ggsave("RACE_GAP.jpg")

#mean-sd data
#install.packages("gridExtra")

library(gridExtra)
library(grid)
grid.newpage()
grid.table( as.data.frame(DF_Stats) )



