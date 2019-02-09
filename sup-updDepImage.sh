#!/bin/sh
# -*- Mode:Shell-script; Coding:us-ascii-unix; fill-column:132 -*-

####################################################################################################################################
##
# @file      sup-updDepImage.sh
# @author    Mitch Richling <https://www.mitchr.me>
# @Copyright Copyright 2015,2019 by Mitch Richling.  All rights reserved.
# @Revision  $Revision: 1.14 $ 
# @brief     Update the dependency graph images used on the web page.@EOL
# @Keywords  dot
# @Std       bash graphviz
#
#            
#            

##----------------------------------------------------------------------------------------------------------------------------------

rm -f grnt.pdf grnt.png gr.png gr.pdf grZ.png grntZ.png

cat gr.dot |                       sed 's/MJR_//g' | dot -Earrowsize=3,1,1,1,1 -Nfontsize=60 -Gsize=22,22 -Gratio=1.0 -T png -o gr.png
              
cat gr.dot |                       sed 's/MJR_//g' | dot -Earrowsize=3,1,1,1,1 -Nfontsize=60 -Gsize=22,22 -Gratio=1.0 -T pdf -o gr.pdf

cat gr.dot | sed '/-TESTS/d'     | sed 's/MJR_//g' | dot -Earrowsize=3,1,1,1,1 -Nfontsize=60 -Gsize=22,22 -Gratio=0.6 -T pdf -o grnt.pdf

cat gr.dot | sed '/-TESTS/d'     | sed 's/MJR_//g' | dot -Earrowsize=3,1,1,1,1 -Nfontsize=60 -Gsize=22,22 -Gratio=0.6 -T png -o grnt.png

convert -resize 700x gr.png   grZ.png
convert -resize 700x grnt.png grntZ.png

#xpdf -fullscreen -z page gr.pdf
#xpdf -fullscreen -z page grnt.pdf

# start gr.pdf
# start grnt.pdf

#pqiv -f grnt.png gr.png

start gr.png
start grnt.png

