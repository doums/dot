#!/bin/bash

#  0   8  black
#  1   9  red
#  2  10  green
#  3  11  yellow
#  4  12  blue
#  5  13  magenta
#  6  14  cyan
#  7  15  white

RS="\e[0m" # ANSI reset

fg() {
  echo -ne "\e[38;5;$1m$2$RS"
}

bg() {
  echo -ne "\e[48;5;$1m$2$RS"
}

fg 0 "0 black "
fg 1 "1 red "
fg 2 "2 green "
fg 3 "3 yellow "
fg 4 "4 blue "
fg 5 "5 magenta "
fg 6 "6 cyan "
fg 7 "7 white "

echo ""
fg 8 "0 black "
fg 9 "1 red "
fg 10 "2 green "
fg 11 "3 yellow "
fg 12 "4 blue "
fg 13 "5 magenta "
fg 14 "6 cyan "
fg 15 "7 white "

echo ""
echo ""

bg 0 "   "
echo -n "  "
bg 1 "   "
echo -n "  "
bg 2 "   "
echo -n "  "
bg 3 "   "
echo -n "  "
bg 4 "   "
echo -n "  "
bg 5 "   "
echo -n "  "
bg 6 "   "
echo -n "  "
bg 7 "   "

echo ""

bg 8 "   "
echo -n "  "
bg 9 "   "
echo -n "  "
bg 10 "   "
echo -n "  "
bg 11 "   "
echo -n "  "
bg 12 "   "
echo -n "  "
bg 13 "   "
echo -n "  "
bg 14 "   "
echo -n "  "
bg 15 "   "

echo ""
echo ""
