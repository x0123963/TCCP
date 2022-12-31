# Calculation of lexical statistics
# Lisa Chang 12.24.2022
# required packages: vwr(ver 0.3.0), tidyr

# Define a function to load syllable frequency tables
read.tt <- function(fname){
  T <- read.csv(fname, check.names = FALSE)
  row.names(T)<- as.character(T[,1])
  T <- T[,-1]
  return(T);
}

## Tone-consonant co-occurrence probability ####
# based on type frequency

setwd("./Materials/")
T1 <- read.tt("T1_type.csv")
T2 <- read.tt("T2_type.csv")
T3 <- read.tt("T3_type.csv")
T4 <- read.tt("T4_type.csv")

# Sum of each onset's type frequency paired with a tone
for(i in 1:22){
  T1$Sum_Onset[i]<- sum(T1[i,1:38], na.rm = TRUE)
  T2$Sum_Onset[i]<- sum(T2[i,1:38], na.rm = TRUE)
  T3$Sum_Onset[i]<- sum(T3[i,1:38], na.rm = TRUE)
  T4$Sum_Onset[i]<- sum(T4[i,1:38], na.rm = TRUE)
}

# Divide by sum of each onset's type frequency paired with all tones
denominator<- T1$Sum_Onset+T2$Sum_Onset+T3$Sum_Onset+T4$Sum_Onset
T1$TCCP<- T1$Sum_Onset/denominator
T2$TCCP<- T2$Sum_Onset/denominator
T3$TCCP<- T3$Sum_Onset/denominator
T4$TCCP<- T4$Sum_Onset/denominator

## Neighborhood density ####
# based on CVCT schema (see N&H 2019)
# and type frequency of monosyllables

library(vwr)
library(tidyr)

# map complex nuclei into its column number
sg_char<- c(as.character(5:9), 
            "#", "$", "%", "^", "&", "*", "+", "[", "<", "{",
            "an", "@n", "aN", "@N", "oN", "in", "iN", "7n", 
            "6N", "9n", "un", "9N", "$n", "yn", "=N", "?")

map.sg.char <- function(tt){
  tt_sg_ch<- tt[,1:38]
  colnames(tt_sg_ch)[8:38]<- sg_char
  return(tt_sg_ch);
}

T1_sg_char<- map.sg.char(T1)
T2_sg_char<- map.sg.char(T2)
T3_sg_char<- map.sg.char(T3)
T4_sg_char<- map.sg.char(T4)

# create syllable list
long.syll <- function(tt_sg_ch, Tone, keep_all){
  tt_sg_ch["Onset"]<- row.names(tt_sg_ch)
  tt_long<- pivot_longer(tt_sg_ch, !Onset, names_to = "Rime", 
                              values_to = "Frequency", values_drop_na=!keep_all)
  tt_long$Tones<- Tone
  tt_long$Syllables<- paste(tt_long$Onset, tt_long$Rime, Tone, sep="")
  tt_long$Syllables[tt_long$Onset=="0"]<- paste(tt_long$Rime[tt_long$Onset=="0"]
                                                , Tone, sep="")
  return(tt_long);
}

# excluding syllables with no type frequency (i.e., equals zero)
T1_real<- long.syll(T1_sg_char, "1", FALSE)
T2_real<- long.syll(T2_sg_char, "2", FALSE)
T3_real<- long.syll(T3_sg_char, "3", FALSE)
T4_real<- long.syll(T4_sg_char, "4", FALSE)
real<- rbind.data.frame(T1_real, T2_real, T3_real, T4_real)

# including nonexistent syllables 
T1_all<- long.syll(T1_sg_char, "1", TRUE)
T2_all<- long.syll(T2_sg_char, "2", TRUE)
T3_all<- long.syll(T3_sg_char, "3", TRUE)
T4_all<- long.syll(T4_sg_char, "4", TRUE)
all<- rbind.data.frame(T1_all, T2_all, T3_all, T4_all)

all$ND <- NA
for (i in 1:nrow(all)){
  neighbors<- levenshtein.neighbors(all$Syllables[i], real$Syllables)[[1]]
  all$ND[i]<- length(neighbors)
}

# convert single characters back to SAMPA
names(sg_char)<- c(colnames(T1)[8:38])

all$Syllables_SAMPA<- NA
for (i in 1:nrow(all)){
  if (all$Rime[i] %in% sg_char){
    rime_SAMPA<- names(sg_char)[sg_char==all$Rime[i]]
    if (all$Onset[i]!="0"){
      all$Syllables_SAMPA[i]<- paste(all$Onset[i], 
                                     rime_SAMPA, all$Tones[i], sep="")
    } else{
      all$Syllables_SAMPA[i]<- paste(rime_SAMPA, all$Tones[i], sep="")
    }
  } else{
    all$Syllables_SAMPA[i]<- all$Syllables[i]
  }
}
