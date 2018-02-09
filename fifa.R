## First analysis to find out soccer players who have the highest Rating * Skill Rating scores for each position and each country


library(dplyr)

# Read data
ff <- read.csv("fifa-18-dataset.csv", stringsAsFactors = F)
fifa <- ff

# Cleaning the data
# Adjust the variable "Value" to have same unit.
MM <- fifa[grep("M", fifa$Value),]
KK <- fifa[grep("K", fifa$Value),]
MM$Value <- as.numeric(sub("M","",MM$Value))*1000
KK$Value <- as.numeric(sub("K","",KK$Value))
fifa <- rbind(MM, KK)

# Change the characters into numeric for position ratings and skill ratings
for(cols in 14:47){
      fifa[colnames(fifa)[cols]] <- as.numeric(as.matrix(fifa[colnames(fifa)[cols]]))
}

# Create separate position data sets that have players whose preferred position
# belong to the specific position
## Each player has at leat one preferred position. 
cam <- fifa[grep("CAM", fifa$Preferred.Positions),]#1
cb <- fifa[grep("CB", fifa$Preferred.Positions),]#2
cdm <- fifa[grep("CDM", fifa$Preferred.Positions),]#3
cf <- fifa[grep("CF", fifa$Preferred.Positions),]#4
cm <- fifa[grep("CM", fifa$Preferred.Positions),]#5
lam <- fifa[grep("LAM", fifa$Preferred.Positions),]#6 no data
lb <- fifa[grep("LB", fifa$Preferred.Positions),]#7
lcb <- fifa[grep("LCB", fifa$Preferred.Positions),]#8 no data
lcm <- fifa[grep("LCM", fifa$Preferred.Positions),]#9 no data
ldm <- fifa[grep("LDM", fifa$Preferred.Positions),]#10 no data
lf <- fifa[grep("LF", fifa$Preferred.Positions),]#11 no data
lm <- fifa[grep("LM", fifa$Preferred.Positions),]#12
ls <- fifa[grep("LS", fifa$Preferred.Positions),]#13 no data
lw <- fifa[grep("LW", fifa$Preferred.Positions),]#14
lwb <- fifa[grep("LWB", fifa$Preferred.Positions),]#15
ram <- fifa[grep("RAM", fifa$Preferred.Positions),]#16 no data
rb <- fifa[grep("RB", fifa$Preferred.Positions),]#17 
rcb <- fifa[grep("RCB", fifa$Preferred.Positions),]#18 no data
rcm <- fifa[grep("RCM", fifa$Preferred.Positions),]#19 no data
rdm <- fifa[grep("RDM", fifa$Preferred.Positions),]#20 no data
rf <- fifa[grep("RF", fifa$Preferred.Positions),]#21 no data
rm <- fifa[grep("RM", fifa$Preferred.Positions),]#22
rs <- fifa[grep("RS", fifa$Preferred.Positions),]#23 no data
rw <- fifa[grep("RW", fifa$Preferred.Positions),]#24
rwb <- fifa[grep("RWB", fifa$Preferred.Positions),]#25
st <- fifa[grep("ST", fifa$Preferred.Positions),]#26

# Remove NAs from each dataset
cam <- na.omit(cam) #1
cb <- na.omit(cb)#2
cdm <- na.omit(cdm)#3
cf <- na.omit(cf)#4
cm <- na.omit(cm)#5
lam <- na.omit(lam)#6
lb <- na.omit(lb)#7
lcb <- na.omit(lcb)#8
lcm <- na.omit(lcm)#9
ldm <- na.omit(ldm)#10
lf <- na.omit(lf)#11
lm <- na.omit(lm)#12
ls <- na.omit(ls)#13
lw <- na.omit(lw)#14
lwb <- na.omit(lwb)#15
ram <- na.omit(ram)#16
rb <- na.omit(rb)#17
rcb <- na.omit(rcb)#18
rcm <- na.omit(rcm)#19
rdm <- na.omit(rdm)#20
rf <- na.omit(rf)#21
rm <- na.omit(rm)#22
rs <- na.omit(rs)#23
rw <- na.omit(rw)#24
rwb <- na.omit(rwb)#25
st <- na.omit(st)#26


## Perform regression analysis for each dataset in order to find out which 
## skills have high coefficient for each position. 

#1 cam
lm.cam <- lm(Value ~ Age + Special+Acceleration+Aggression+Agility+Balance+Ball.control+Composure+Crossing+Curve+Dribbling+
            Finishing+Free.kick.accuracy+Heading.accuracy+Interceptions+Jumping+
            +Long.passing+Long.shots+Marking+Positioning+Reactions+Short.passing+
            +Shot.power+Sliding.tackle+Sprint.speed+Stamina+Standing.tackle+Strength+
            +Vision+Volleys, data=cam)
step.lm.cam <- step(lm.cam, data=cam, direction="both")

#2 cb
lm.cb <- lm(Value ~ Age + Special+Acceleration+Aggression+Agility+Balance+Ball.control+Composure+Crossing+Curve+Dribbling+
                   Finishing+Free.kick.accuracy+Heading.accuracy+Interceptions+Jumping+
                   +Long.passing+Long.shots+Marking+Positioning+Reactions+Short.passing+
                   +Shot.power+Sliding.tackle+Sprint.speed+Stamina+Standing.tackle+Strength+
                   +Vision+Volleys, data=cb)
step.lm.cb <- step(lm.cb, data=cb, direction="both")

#3 cdm
lm.cdm <- lm(Value ~ Age + Special+Acceleration+Aggression+Agility+Balance+Ball.control+Composure+Crossing+Curve+Dribbling+
                   Finishing+Free.kick.accuracy+Heading.accuracy+Interceptions+Jumping+
                   +Long.passing+Long.shots+Marking+Positioning+Reactions+Short.passing+
                   +Shot.power+Sliding.tackle+Sprint.speed+Stamina+Standing.tackle+Strength+
                   +Vision+Volleys, data=cdm)
step.lm.cdm <- step(lm.cdm, data=cdm, direction="both")

#4 cf
lm.cf <- lm(Value ~ Age + Special+Acceleration+Aggression+Agility+Balance+Ball.control+Composure+Crossing+Curve+Dribbling+
                   Finishing+Free.kick.accuracy+Heading.accuracy+Interceptions+Jumping+
                   +Long.passing+Long.shots+Marking+Positioning+Reactions+Short.passing+
                   +Shot.power+Sliding.tackle+Sprint.speed+Stamina+Standing.tackle+Strength+
                   +Vision+Volleys, data=cf)
step.lm.cf <- step(lm.cf, data=cf, direction="both")

#5 cm
lm.cm <- lm(Value ~ Age + Special+Acceleration+Aggression+Agility+Balance+Ball.control+Composure+Crossing+Curve+Dribbling+
                   Finishing+Free.kick.accuracy+Heading.accuracy+Interceptions+Jumping+
                   +Long.passing+Long.shots+Marking+Positioning+Reactions+Short.passing+
                   +Shot.power+Sliding.tackle+Sprint.speed+Stamina+Standing.tackle+Strength+
                   +Vision+Volleys, data=cm)
step.lm.cm <- step(lm.cm, data=cm, direction="both")

#6 lam
lm.lam <- lm(Value ~ Age + Special+Acceleration+Aggression+Agility+Balance+Ball.control+Composure+Crossing+Curve+Dribbling+
                   Finishing+Free.kick.accuracy+Heading.accuracy+Interceptions+Jumping+
                   +Long.passing+Long.shots+Marking+Positioning+Reactions+Short.passing+
                   +Shot.power+Sliding.tackle+Sprint.speed+Stamina+Standing.tackle+Strength+
                   +Vision+Volleys, data=lam)
step.lm.lam <- step(lm.lam, data=lam, direction="both")

#7 lb
lm.lb <- lm(Value ~ Age + Special+Acceleration+Aggression+Agility+Balance+Ball.control+Composure+Crossing+Curve+Dribbling+
                   Finishing+Free.kick.accuracy+Heading.accuracy+Interceptions+Jumping+
                   +Long.passing+Long.shots+Marking+Positioning+Reactions+Short.passing+
                   +Shot.power+Sliding.tackle+Sprint.speed+Stamina+Standing.tackle+Strength+
                   +Vision+Volleys, data=lb)
step.lm.lb <- step(lm.lb, data=lb, direction="both")

#8 lcb
lm.lcb <- lm(Value ~ Age + Special+Acceleration+Aggression+Agility+Balance+Ball.control+Composure+Crossing+Curve+Dribbling+
                   Finishing+Free.kick.accuracy+Heading.accuracy+Interceptions+Jumping+
                   +Long.passing+Long.shots+Marking+Positioning+Reactions+Short.passing+
                   +Shot.power+Sliding.tackle+Sprint.speed+Stamina+Standing.tackle+Strength+
                   +Vision+Volleys, data=lcb)
step.lm.lcb <- step(lm.lcb, data=lcb, direction="both")

#9 lcm
lm.lcm <- lm(Value ~ Age + Special+Acceleration+Aggression+Agility+Balance+Ball.control+Composure+Crossing+Curve+Dribbling+
                   Finishing+Free.kick.accuracy+Heading.accuracy+Interceptions+Jumping+
                   +Long.passing+Long.shots+Marking+Positioning+Reactions+Short.passing+
                   +Shot.power+Sliding.tackle+Sprint.speed+Stamina+Standing.tackle+Strength+
                   +Vision+Volleys, data=lcm)
step.lm.lcm <- step(lm.lcm, data=lcm, direction="both")

#10 ldm
lm.ldm <- lm(Value ~ Age + Special+Acceleration+Aggression+Agility+Balance+Ball.control+Composure+Crossing+Curve+Dribbling+
                   Finishing+Free.kick.accuracy+Heading.accuracy+Interceptions+Jumping+
                   +Long.passing+Long.shots+Marking+Positioning+Reactions+Short.passing+
                   +Shot.power+Sliding.tackle+Sprint.speed+Stamina+Standing.tackle+Strength+
                   +Vision+Volleys, data=ldm)
step.lm.ldm <- step(lm.ldm, data=ldm, direction="both")

#11 lf
lm.lf <- lm(Value ~ Age + Special+Acceleration+Aggression+Agility+Balance+Ball.control+Composure+Crossing+Curve+Dribbling+
                   Finishing+Free.kick.accuracy+Heading.accuracy+Interceptions+Jumping+
                   +Long.passing+Long.shots+Marking+Positioning+Reactions+Short.passing+
                   +Shot.power+Sliding.tackle+Sprint.speed+Stamina+Standing.tackle+Strength+
                   +Vision+Volleys, data=lf)
step.lm.lf <- step(lm.lf, data=lf, direction="both")

#12 lm
lm.lm <- lm(Value ~ Age + Special+Acceleration+Aggression+Agility+Balance+Ball.control+Composure+Crossing+Curve+Dribbling+
                   Finishing+Free.kick.accuracy+Heading.accuracy+Interceptions+Jumping+
                   +Long.passing+Long.shots+Marking+Positioning+Reactions+Short.passing+
                   +Shot.power+Sliding.tackle+Sprint.speed+Stamina+Standing.tackle+Strength+
                   +Vision+Volleys, data=lm)
step.lm.lm <- step(lm.lm, data=lm, direction="both")

#13 ls
lm.ls <- lm(Value ~ Age + Special+Acceleration+Aggression+Agility+Balance+Ball.control+Composure+Crossing+Curve+Dribbling+
                   Finishing+Free.kick.accuracy+Heading.accuracy+Interceptions+Jumping+
                   +Long.passing+Long.shots+Marking+Positioning+Reactions+Short.passing+
                   +Shot.power+Sliding.tackle+Sprint.speed+Stamina+Standing.tackle+Strength+
                   +Vision+Volleys, data=ls)
step.lm.ls <- step(lm.ls, data=ls, direction="both")


#14 lw
lm.lw <- lm(Value ~ Age + Special+Acceleration+Aggression+Agility+Balance+Ball.control+Composure+Crossing+Curve+Dribbling+
                   Finishing+Free.kick.accuracy+Heading.accuracy+Interceptions+Jumping+
                   +Long.passing+Long.shots+Marking+Positioning+Reactions+Short.passing+
                   +Shot.power+Sliding.tackle+Sprint.speed+Stamina+Standing.tackle+Strength+
                   +Vision+Volleys, data=lw)
step.lm.lw <- step(lm.lw, data=lw, direction="both")

#15 lwb
lm.lwb <- lm(Value ~ Age + Special+Acceleration+Aggression+Agility+Balance+Ball.control+Composure+Crossing+Curve+Dribbling+
                   Finishing+Free.kick.accuracy+Heading.accuracy+Interceptions+Jumping+
                   +Long.passing+Long.shots+Marking+Positioning+Reactions+Short.passing+
                   +Shot.power+Sliding.tackle+Sprint.speed+Stamina+Standing.tackle+Strength+
                   +Vision+Volleys, data=lwb)
step.lm.lwb <- step(lm.lwb, data=lwb, direction="both")

#16 ram
lm.ram <- lm(Value ~ Age + Special+Acceleration+Aggression+Agility+Balance+Ball.control+Composure+Crossing+Curve+Dribbling+
                   Finishing+Free.kick.accuracy+Heading.accuracy+Interceptions+Jumping+
                   +Long.passing+Long.shots+Marking+Positioning+Reactions+Short.passing+
                   +Shot.power+Sliding.tackle+Sprint.speed+Stamina+Standing.tackle+Strength+
                   +Vision+Volleys, data=ram)
step.lm.ram <- step(lm.ram, data=ram, direction="both")

#17 rb
lm.rb <- lm(Value ~ Age + Special+Acceleration+Aggression+Agility+Balance+Ball.control+Composure+Crossing+Curve+Dribbling+
                   Finishing+Free.kick.accuracy+Heading.accuracy+Interceptions+Jumping+
                   +Long.passing+Long.shots+Marking+Positioning+Reactions+Short.passing+
                   +Shot.power+Sliding.tackle+Sprint.speed+Stamina+Standing.tackle+Strength+
                   +Vision+Volleys, data=rb)
step.lm.rb <- step(lm.rb, data=rb, direction="both")

#18 rcb
lm.rcb <- lm(Value ~ Age + Special+Acceleration+Aggression+Agility+Balance+Ball.control+Composure+Crossing+Curve+Dribbling+
                   Finishing+Free.kick.accuracy+Heading.accuracy+Interceptions+Jumping+
                   +Long.passing+Long.shots+Marking+Positioning+Reactions+Short.passing+
                   +Shot.power+Sliding.tackle+Sprint.speed+Stamina+Standing.tackle+Strength+
                   +Vision+Volleys, data=rcb)
step.lm.rcb <- step(lm.rcb, data=rcb, direction="both")

#19 rcm
lm.rcm <- lm(Value ~ Age + Special+Acceleration+Aggression+Agility+Balance+Ball.control+Composure+Crossing+Curve+Dribbling+
                   Finishing+Free.kick.accuracy+Heading.accuracy+Interceptions+Jumping+
                   +Long.passing+Long.shots+Marking+Positioning+Reactions+Short.passing+
                   +Shot.power+Sliding.tackle+Sprint.speed+Stamina+Standing.tackle+Strength+
                   +Vision+Volleys, data=rcm)
step.lm.rcm <- step(lm.rcm, data=rcm, direction="both")

#20 rdm
lm.rdm <- lm(Value ~ Age + Special+Acceleration+Aggression+Agility+Balance+Ball.control+Composure+Crossing+Curve+Dribbling+
                   Finishing+Free.kick.accuracy+Heading.accuracy+Interceptions+Jumping+
                   +Long.passing+Long.shots+Marking+Positioning+Reactions+Short.passing+
                   +Shot.power+Sliding.tackle+Sprint.speed+Stamina+Standing.tackle+Strength+
                   +Vision+Volleys, data=rdm)
step.lm.rdm <- step(lm.rdm, data=rdm, direction="both")

#21 rf
lm.rf <- lm(Value ~ Age + Special+Acceleration+Aggression+Agility+Balance+Ball.control+Composure+Crossing+Curve+Dribbling+
                   Finishing+Free.kick.accuracy+Heading.accuracy+Interceptions+Jumping+
                   +Long.passing+Long.shots+Marking+Positioning+Reactions+Short.passing+
                   +Shot.power+Sliding.tackle+Sprint.speed+Stamina+Standing.tackle+Strength+
                   +Vision+Volleys, data=rf)
step.lm.rf <- step(lm.rf, data=rf, direction="both")

#22 rm
lm.rm <- lm(Value ~ Age + Special+Acceleration+Aggression+Agility+Balance+Ball.control+Composure+Crossing+Curve+Dribbling+
                   Finishing+Free.kick.accuracy+Heading.accuracy+Interceptions+Jumping+
                   +Long.passing+Long.shots+Marking+Positioning+Reactions+Short.passing+
                   +Shot.power+Sliding.tackle+Sprint.speed+Stamina+Standing.tackle+Strength+
                   +Vision+Volleys, data=rm)
step.lm.rm <- step(lm.rm, data=rm, direction="both")


#23 rs
lm.rs <- lm(Value ~ Age + Special+Acceleration+Aggression+Agility+Balance+Ball.control+Composure+Crossing+Curve+Dribbling+
                   Finishing+Free.kick.accuracy+Heading.accuracy+Interceptions+Jumping+
                   +Long.passing+Long.shots+Marking+Positioning+Reactions+Short.passing+
                   +Shot.power+Sliding.tackle+Sprint.speed+Stamina+Standing.tackle+Strength+
                   +Vision+Volleys, data=rs)
step.lm.rs <- step(lm.rs, data=rs, direction="both")

#24 rw
lm.rw <- lm(Value ~ Age + Special+Acceleration+Aggression+Agility+Balance+Ball.control+Composure+Crossing+Curve+Dribbling+
                   Finishing+Free.kick.accuracy+Heading.accuracy+Interceptions+Jumping+
                   +Long.passing+Long.shots+Marking+Positioning+Reactions+Short.passing+
                   +Shot.power+Sliding.tackle+Sprint.speed+Stamina+Standing.tackle+Strength+
                   +Vision+Volleys, data=rw)
step.lm.rw <- step(lm.rw, data=rw, direction="both")

#25 rwb
lm.rwb <- lm(Value ~ Age + Special+Acceleration+Aggression+Agility+Balance+Ball.control+Composure+Crossing+Curve+Dribbling+
                   Finishing+Free.kick.accuracy+Heading.accuracy+Interceptions+Jumping+
                   +Long.passing+Long.shots+Marking+Positioning+Reactions+Short.passing+
                   +Shot.power+Sliding.tackle+Sprint.speed+Stamina+Standing.tackle+Strength+
                   +Vision+Volleys, data=rwb)
step.lm.rwb <- step(lm.rwb, data=rwb, direction="both")

#26 st
lm.st <- lm(Value ~ Age + Special+Acceleration+Aggression+Agility+Balance+Ball.control+Composure+Crossing+Curve+Dribbling+
                   Finishing+Free.kick.accuracy+Heading.accuracy+Interceptions+Jumping+
                   +Long.passing+Long.shots+Marking+Positioning+Reactions+Short.passing+
                   +Shot.power+Sliding.tackle+Sprint.speed+Stamina+Standing.tackle+Strength+
                   +Vision+Volleys, data=st)
step.lm.st <- step(lm.st, data=st, direction="both")

## Based on coefficient choose top 5 skills that are important to each position 

### cam Short.passing, Reactions, Ball.control, Finishing, Vision
### cb Sliding.tackle, Standing.tackle, Reactions, Interception, Vision
### cdm Short.passing, Reactions, Standing.tackle, Sliding.tackle, Heading.accuracy
### cf Finishing, Ball.control, Dribbling, Reactions, Short.passing
### cm Short.passing, Reactions, Ball.control, Dribbling, Ball.control
### lb Reactions, Standing.tackle, Sliding.tackle, Interceptions, Heading.accuracy
### lm Reactions, Short.passing, Ball.control, Crossing, Dribbing
### lw Reactions, Ball.control, Composure, Short.passing, Agility
### lwb Reactions, Interceptions, Crossing, Driblling, Ball.control
### rb Reactions, Sliding.tackle, Standing.tackle, Interceptions, Short.passing
### rm Reactions, Short.passing, Ball.control, Dribbling, Crossing
### rw Reactions, Ball.control, Acceleration, Short.passing, Free.kick.accuracy
### rwb Reactions, Short.passing, Heading.accuracy, Dribbling, Strength, Crossing
### st Finishing, Reactions, Vision, Composure, Positioning

criteria <- data.frame(cam_mid_attack=c("Short.passing", "Reactions", "Ball.control", "Finishing", "Vision"),
                       cb_mid_defend=c("Sliding.tackle", "Standing.tackle", "Reactions", "Interception", "Vision"),
                       cdm_mid_defend=c("Short.passing", "Reactions", "Standing.tackle", "Sliding.tackle", "Heading.accuracy"),
                       cf_aattack=c("Finishing", "Ball.control", "Dribbling", "Reactions", "Short.passing"),
                       cm_mid_mid=c("Short.passing", "Reactions", "Ball.control", "Dribbling", "Ball.control"),
                       lb_left_defend=c("Reactions", "Standing.tackle", "Sliding.tackle", "Interception", "Heading.accuracy"),
                       lm_left_mid=c("Reactions", "Short.passing", "Ball.control", "Crossing", "Dribbing"),
                       lw_left_attack=c("Reactions", "Ball.control", "Composure", "Special", "Agility"),
                       lwb_left_defend=c("Reactions", "Interceptions", "Crossing", "Driblling", "Ball.control"),
                       rb_right_defend=c("Reactions", "Sliding.tackle", "Standing.tackle", "Interceptions", "Short.passing"),
                       rm_right_md=c("Reactions", "Short.passing", "Ball.control", "Dribbling", "Crossing"),
                       rw_right_attack=c("Reactions", "Ball.control", "Acceleration", "Short.passing", "Free.kick.accuracy"),
                       rwb_right_defend=c("Reactions", "Short.passing", "Heading.accuracy", "Dribbling", "Strength"),
                       st_attack=c("Finishing", "Reactions", "Vision", "Composure", "Positioning"))

# Filter the fifa dataset with 8 countries. 
bra <- filter(fifa, Nationality=="Brazil")
eng <- filter(fifa, Nationality=="England")
spa <- filter(fifa, Nationality=="Spain")
beg <- filter(fifa, Nationality=="Belgium")
arg <- filter(fifa, Nationality=="Argentina")
fra <- filter(fifa, Nationality=="France")
por <- filter(fifa, Nationality=="Portugal")
ger <- filter(fifa, Nationality=="Germany")
cyp <- filter(fifa, Nationality=="Cyprus")


## Based on the top 5 skills to each position, create new Rating variable that 
## is multiplication between the position rating and the 5 top skill ratings
## Ultimately, you can get the table for players arranged by the new Rating variable

### cam
cam_plyr <- fifa %>%
      select(Name,Nationality,Preferred.Positions, Short.passing,Reactions,
             Ball.control,Finishing,Vision,CAM) %>%
      group_by(Nationality) %>%
      mutate(Rating=(Short.passing+Reactions+Ball.control+Finishing+Vision)*CAM) %>%
      arrange(Nationality, desc(Rating))
      
### cb
cb_plyr <- fifa %>%
      select(Name,Nationality,Preferred.Positions, Sliding.tackle, Standing.tackle,
             Reactions, Interceptions, Vision,CB) %>%
      group_by(Nationality) %>%
      mutate(Rating=(Sliding.tackle+ Standing.tackle+
                     Reactions+ Interceptions+ Vision)*CB) %>%
      arrange(Nationality, desc(Rating))
### cdm 
cdm_plyr <- fifa %>%
      select(Name,Nationality,Preferred.Positions, Short.passing, Reactions,
             Standing.tackle, Sliding.tackle, Heading.accuracy,CDM) %>%
      group_by(Nationality) %>%
      mutate(Rating=(Short.passing+ Reactions+ Standing.tackle+ Sliding.tackle+ 
                     Heading.accuracy)*CDM) %>%
      arrange(Nationality, desc(Rating))
### cf
cf_plyr <- fifa %>%
      select(Name,Nationality,Preferred.Positions, Finishing, Ball.control, Dribbling, 
             Reactions, Short.passing,CF) %>%
      group_by(Nationality) %>%
      mutate(Rating=(Finishing+ Ball.control+ Dribbling+
                     Reactions+ Short.passing)*CF) %>%
      arrange(Nationality, desc(Rating))
### cm 
cm_plyr <- fifa %>%
      select(Name,Nationality,Preferred.Positions, Short.passing, Reactions, 
             Ball.control, Dribbling, Ball.control,CM) %>%
      group_by(Nationality) %>%
      mutate(Rating=(Short.passing+ Reactions+ Ball.control+Dribbling+Ball.control)*CM) %>%
      arrange(Nationality, desc(Rating))

### lb 
lb_plyr <- fifa %>%
      select(Name,Nationality,Preferred.Positions,  Reactions, Standing.tackle, 
             Sliding.tackle, Interceptions, Heading.accuracy,LB) %>%
      group_by(Nationality) %>%
      mutate(Rating=(Reactions+ Standing.tackle+ 
                     Sliding.tackle+ Interceptions+ Heading.accuracy)*LB) %>%
      arrange(Nationality, desc(Rating))
### lm 
lm_plyr <- fifa %>%
      select(Name,Nationality,Preferred.Positions, Reactions, Short.passing, 
             Ball.control, Crossing, Dribbling,LM) %>%
      group_by(Nationality) %>%
      mutate(Rating=(Reactions+ Short.passing+ Ball.control+ Crossing+ Dribbling)*LM) %>%
      arrange(Nationality, desc(Rating))
### lw 
lw_plyr <- fifa %>%
      select(Name,Nationality,Preferred.Positions, Reactions, Ball.control,
             Composure, Short.passing, Agility,LW) %>%
      group_by(Nationality) %>%
      mutate(Rating=(Reactions+ Ball.control+ Composure+ Short.passing+ Agility)*LW) %>%
      arrange(Nationality, desc(Rating))
### lwb
lwb_plyr <- fifa %>%
      select(Name,Nationality,Preferred.Positions, Reactions, Interceptions, 
             Crossing, Dribbling, Ball.control,LWB) %>%
      group_by(Nationality) %>%
      mutate(Rating=(Reactions+Interceptions+Crossing+Dribbling+Ball.control)*LWB) %>%
      arrange(Nationality, desc(Rating))
### rb
rb_plyr <- fifa %>%
      select(Name,Nationality,Preferred.Positions, Reactions, Sliding.tackle, 
             Standing.tackle, Interceptions, Short.passing,RB) %>%
      group_by(Nationality) %>%
      mutate(Rating=(Reactions+ Sliding.tackle+ Standing.tackle+ Interceptions+
                           Short.passing)*RB) %>%
      arrange(Nationality, desc(Rating))
### rm 
rm_plyr <- fifa %>%
      select(Name,Nationality,Preferred.Positions, Reactions, Short.passing, 
             Ball.control, Dribbling, Crossing,RM) %>%
      group_by(Nationality) %>%
      mutate(Rating=(Reactions+ Short.passing+Ball.control+Dribbling+Crossing)*RM) %>%
      arrange(Nationality, desc(Rating))
write.csv(rm_plyr, "rm_plyr.csv")
### rw 
rw_plyr <- fifa %>%
      select(Name,Nationality,Preferred.Positions, Reactions, Ball.control, 
             Acceleration, Short.passing, Free.kick.accuracy,RW) %>%
      group_by(Nationality) %>%
      mutate(Rating=(Reactions+ Ball.control+Acceleration+ Short.passing+
                           Free.kick.accuracy)*RW) %>%
      arrange(Nationality, desc(Rating))
### rwb 
rwb_plyr <- fifa %>%
      select(Name,Nationality,Preferred.Positions,Reactions, Short.passing, 
             Heading.accuracy, Dribbling, Strength, Crossing,RWB) %>%
      group_by(Nationality) %>%
      mutate(Rating=(Reactions+Short.passing+Heading.accuracy+ Dribbling+
                           Strength+Crossing)*RWB) %>%
      arrange(Nationality, desc(Rating))
### st 
st_plyr <- fifa %>%
      select(Name,Nationality,Preferred.Positions,Finishing, Reactions, Vision, 
             Composure, Positioning,ST) %>%
      group_by(Nationality) %>%
      mutate(Rating=(Finishing+Reactions+Vision+Composure+Positioning)*ST) %>%
      arrange(Nationality, desc(Rating))

tbl_name <- deparse(substitute(tbl_name))


## Function that takes table name, country name, and number of rows produce 
## arranged players in descending order of Rating. 

Position <- function(tbl_name) {
      splitted <- strsplit(tbl_name, "_")
      position <- toupper(splitted[[1]][1])
      "%!in%" <- Negate("%in%")
      if(position %!in% c("CAM", "CB", "CDM", "CF", "CM", "LB", "LM", "LW", "LWB",
                         "RB", "RM", "RW", "RWB", "ST")) {
            stop("Wrong function name")}
      return(position)
}

PositionCountry <- function(tbl_name, country, num) {
      position <- Position(tbl_name)
      df <- as.data.frame(eval(parse(text=tbl_name)))
      df %>%
            select(Nationality, Name, position, Preferred.Positions, Rating) %>%
            filter(Nationality == country) %>%
            arrange(desc(Rating)) %>%
            head(num)
}

## Example

PositionCountry("cam_plyr", "France", 10)





