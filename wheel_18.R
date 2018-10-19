#I assume at this point that you will load relevant libraries...
library(stringr)
library(dplyr)
library(ggplot2)
library(textfeatures)
library(lubridate)
library(tidyr)

#there is a dataset on github called counted_wheel, it is a vast selection of wheel of fortune bonus puzzles from this website
#http://www.angelfire.com/mi4/malldirectories/wheel/wheelbonus.html

#this will end up being really handy, a vector of letters
LETTERS<-c("A","B",'C','E','F','G','H','I','J','K','L','M','N','O','P','Q','R','S','T','U','V','W','X','Y','Z')

#get a mess of features
PUZZLE_features<-textfeatures(Wheel$PUZZLE)
GUESS_features<-textfeatures(Wheel$LETTERS)
str_count(Wheel$PUZZLE, 'A')


#PARSE THE PUZZLES
#now we need some letter counts
letter_counts<-paste(LETTERS, "<-str_count(Wheel$PUZZLE",",","'", LETTERS, "')", sep = "")
writeLines(letter_counts)
eval(parse(text = letter_counts))

#make those into dataframes
LETTER_frames<-paste(LETTERS,"<-data.frame(", LETTERS, ",", "stringsAsFactors = FALSE)", sep = "")
writeLines(LETTER_frames)
eval(parse(text = LETTER_frames))

LETTER_counts<-bind_cols(
A,
B,
C,
E,
F,
G,
H,
I,
J,
K,
L,
M,
N,
O,
P,
Q,
R,
S,
T,
U,
V,
W,
X,
Y,
Z)


#ADJUST INPUT
letter_counts<-paste(LETTERS, "<-str_count(Wheel$LETTERS",",","'", LETTERS, "')", sep = "")
writeLines(letter_counts)
eval(parse(text = letter_counts))

#make those into dataframes
LETTER_frames<-paste(LETTERS,"<-data.frame(", LETTERS, ",", "stringsAsFactors = FALSE)", sep = "")
writeLines(LETTER_frames)
eval(parse(text = LETTER_frames))

LETTER_countsB<-bind_cols(
  A,
  B,
  C,
  E,
  F,
  G,
  H,
  I,
  J,
  K,
  L,
  M,
  N,
  O,
  P,
  Q,
  R,
  S,
  T,
  U,
  V,
  W,
  X,
  Y,
  Z)

#ADJUST JOIN


#full wheel
View(LETTER_countsB)
Counts<-bind_cols(LETTER_counts, LETTER_countsB)
FullWheel<-bind_cols(FullWheel, Counts)
FullWheel<-bind_cols(FullWheel, PUZZLE_features)
View(FullWheel)
dim(FullWheel)

#get a word
str_extract(FullWheel$PUZZLE, "GO")

#starts with A
str_view_all(FullWheel$PUZZLE, "^F|^G")


#ends with A
str_view_all(FullWheel$PUZZLE, "A$")

#now we start the fun with the math...
#time for some grouping and summary functions...


#time for some dplyr
summarise_each(FullWheel, funs(mean))

