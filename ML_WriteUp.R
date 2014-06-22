

if(!(exists("raw_data"))){
  raw_data <- read.csv("repdata_data_StormData.csv")
}

raw_data <- raw_data[,c("EVTYPE","FATALITIES","INJURIES","PROPDMG","PROPDMGEXP","CROPDMG","CROPDMGEXP")]
new_data<-raw_data
EVType <- tolower(as.character(read.csv("eventType.txt",head=FALSE)$V1))

new_data$EVTYPE=tolower(as.character(new_data$EVTYPE))
new_data$EVTYPE=gsub("\\s+|/|-|\\+|&|and|[0-9]+","",new_data$EVTYPE)

tmp<-as.character(levels(factor(new_data$EVTYPE)))

#distE<-rep(0,nrow(new_data))
  #edit distance
  distE<-sapply(tmp,function(x) which.min(mapply(adist,x,EVType)))
names(distE)<-tmp

new_data$New_EVTYPE<-factor(EVType[distE[new_data$EVTYPE]])
## population health: fatalities + injuries

library(plyr)
tmp1<-ddply(new_data,"New_EVTYPE", function(df) sum(df$FATALITIES+df$INJURIES))
tmp1$New_EVTYPE[which.max(tmp1$V1)]


levels(new_data$PROPDMGEXP)
new_data$N_PROPDMGEXP[new_data$PROPDMGEXP %in% c("","-","?","+")]<-0
new_data$N_PROPDMGEXP[tolower(new_data$PROPDMGEXP) =="h"]<-2
new_data$N_PROPDMGEXP[tolower(new_data$PROPDMGEXP) =="k"]<-3
new_data$N_PROPDMGEXP[tolower(new_data$PROPDMGEXP) =="m"]<-6
new_data$N_PROPDMGEXP[tolower(new_data$PROPDMGEXP) =="b"]<-9
new_data$N_PROPDMGEXP<-as.numeric(new_data$N_PROPDMGEXP)

levels(new_data$CROPDMGEXP)
new_data$N_CROPDMGEXP[new_data$CROPDMGEXP %in% c("","?")]<-0
new_data$N_CROPDMGEXP[tolower(new_data$CROPDMGEXP) =="k"]<-3
new_data$N_CROPDMGEXP[tolower(new_data$CROPDMGEXP) =="m"]<-6
new_data$N_CROPDMGEXP[tolower(new_data$CROPDMGEXP) =="b"]<-9
new_data$N_CROPDMGEXP<-as.numeric(new_data$CROPDMGEXP)                    
                    
tmp2<-ddply(new_data,"New_EVTYPE", function(df) sum(df$PROPDMG*10^df$N_PROPDMGEXP+
                                                      df$CROPDMG*10^df$N_CROPDMGEXP))
tmp2$New_EVTYPE[which.max(tmp2$V1)]
          




# levels(new_data$EVTYPE)
# new_data$EVTYPE=as.factor(gsub("^\\s+|\\s+$","",tolower(as.character(new_data$EVTYPE))))
# levels(new_data$EVTYPE)
# new_data$EVTYPE=as.factor(gsub("\\s+|/|-"," ",as.character(new_data$EVTYPE)))
# levels(new_data$EVTYPE)
# new_data$EVTYPE=as.factor(gsub("flooding|floodin|floods|fldg","flood",as.character(new_data$EVTYPE)))
# levels(new_data$EVTYPE)
# new_data$EVTYPE=as.factor(gsub("wnd|winds","wind",as.character(new_data$EVTYPE)))
# levels(new_data$EVTYPE)
# new_data$EVTYPE=as.factor(gsub("shower","rain",as.character(new_data$EVTYPE)))
# levels(new_data$EVTYPE)
# new_data$EVTYPE=as.factor(gsub("thundestorm|thundeerstorm|thunderstrom|thunerstorm|thundertsorm","thunderstorm",as.character(new_data$EVTYPE)))
# levels(new_data$EVTYPE)
# new_data$EVTYPE[grep("tstm",as.character(new_data$EVTYPE))]="tstm"
# levels(factor((new_data$EVTYPE)))
# new_data$EVTYPE[grep("^thunderstorm",as.character(new_data$EVTYPE))]="thunderstorm"
# levels(factor((new_data$EVTYPE)))
# new_data$EVTYPE[grep("^high wind",as.character(new_data$EVTYPE))]="high wind"
# levels(factor((new_data$EVTYPE)))
# new_data$EVTYPE[grep("^heavy snow",as.character(new_data$EVTYPE))]="heavy snow"
# levels(factor((new_data$EVTYPE)))
# new_data$EVTYPE[grep("^heavy rain",as.character(new_data$EVTYPE))]="heavy rain"
# levels(factor((new_data$EVTYPE)))
# new_data$EVTYPE[grep("^summary",as.character(new_data$EVTYPE))]="summary"
# levels(factor((new_data$EVTYPE)))
# new_data$EVTYPE[grep("^tornado|^torndao",as.character(new_data$EVTYPE))]="tornado"
# levels(factor((new_data$EVTYPE)))
# new_data$EVTYPE[grep("^snow",as.character(new_data$EVTYPE))]="snow"
# levels(factor((new_data$EVTYPE)))
# new_data$EVTYPE[grep("^blizzard",as.character(new_data$EVTYPE))]="blizzard"
# levels(factor((new_data$EVTYPE)))
# new_data$EVTYPE[grep("^urban",as.character(new_data$EVTYPE))]="urban flood"
# levels(factor((new_data$EVTYPE)))
# new_data$EVTYPE[grep("^beach",as.character(new_data$EVTYPE))]="beach flood"
# levels(factor((new_data$EVTYPE)))
# new_data$EVTYPE[grep("^coast",as.character(new_data$EVTYPE))]="coastal flood"
# levels(factor((new_data$EVTYPE)))
# new_data$EVTYPE[grep("^freez",as.character(new_data$EVTYPE))]="freezing rain"
# levels(factor((new_data$EVTYPE)))
# new_data$EVTYPE[grep("^wild",as.character(new_data$EVTYPE))]="wildfire"
# levels(factor((new_data$EVTYPE)))
# new_data$EVTYPE[grep("^water",as.character(new_data$EVTYPE))]="waterspout"
# levels(factor((new_data$EVTYPE)))
# new_data$EVTYPE[grep("^wint",as.character(new_data$EVTYPE))]="winter weather"
# levels(factor((new_data$EVTYPE)))
# new_data$EVTYPE[grep("^wind",as.character(new_data$EVTYPE))]="wind"
# levels(factor((new_data$EVTYPE)))
# new_data$EVTYPE[grep("^dry",as.character(new_data$EVTYPE))]="dry"
# levels(factor((new_data$EVTYPE)))
# new_data$EVTYPE[grep("^glaze",as.character(new_data$EVTYPE))]="glaze"
# levels(factor((new_data$EVTYPE)))
# new_data$EVTYPE[grep("^small stream|^sml stream",as.character(new_data$EVTYPE))]="small stream"
# levels(factor((new_data$EVTYPE)))
# new_data$EVTYPE[grep("^dust",as.character(new_data$EVTYPE))]="dust storm"
# levels(factor((new_data$EVTYPE)))
# new_data$EVTYPE[grep("^blowing snow",as.character(new_data$EVTYPE))]="blowing snow"
# levels(factor((new_data$EVTYPE)))
# new_data$EVTYPE[grep("^heat",as.character(new_data$EVTYPE))]="heat"
# levels(factor((new_data$EVTYPE)))
# new_data$EVTYPE[grep("^heavy surf|high surf",as.character(new_data$EVTYPE))]="high surf"
# levels(factor((new_data$EVTYPE)))

EVType <- read.csv("eventType.txt",head=FALSE)

levels(new_data$PROPDMGEXP)
DMGEXP <- data.frame(cha = c(0:9,"B", "M", "K", "H"), 
                     val = c(1e+09, 1e+06, 1000,100))
multi <- c(1e+09, 1e+06, 1000, 100)
