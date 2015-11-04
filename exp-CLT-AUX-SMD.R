#!/usr/bin/Rscript

####################################################################################################################################
# @file      exCLT-AUX-SMD.R
# @author    Mitch Richling <http://www.mitchr.me>
# @Copyright Copyright 2002,2008,2012 by Mitch Richling.  All rights reserved.
# @Revision  $Revision: 1.1 $ 
# @SCMdate   $Date: 2012/09/07 18:46:04 $
# @brief     Draw PDFs for the Sample Mean Distribution for various sample sizes.@EOL
# @Keywords  graph pdf sums random variable central limit theorem 
#
#            Make a movie like this:
#               for f in exCLT-ART-SMD-??.png; do convert -colorspace RGB -type TrueColor $f $f; done
#               ffmpeg -loop_output 0 -t 72 -r 2 -pix_fmt rgb24 -i exCLT-ART-SMD-%02d.png exCLT-ART-SMD.gif
#            Make a tiny movie like this:
#               convert -define gif:size=200x200 exCLT-ART-SMD.gif -thumbnail '200x200>' -background white -gravity Center -extent 190x190 exCLT-ART-SMD-t.gif
#            

##----------------------------------------------------------------------------------------------------------------------------------
# Read in the data
a<-read.csv("exCLT-OUT-SMD.csv")

##----------------------------------------------------------------------------------------------------------------------------------
# Draw the graphs for various sample sizes -- use file names that make it easy to create a movie
darange <- NA
for(sampSize in 11:1) {
  print(sampSize)
  png(sprintf("exCLT-ART-SMD-%02d.png", sampSize), width=1024, height=768)
  xser <- (1:length(a[,1]))/sampSize
  yser <- tapply(a[,sampSize], xser, sum)
  xfac <- as.numeric(names(yser))
  yser <- (yser/sum(yser))
  if(is.na(darange))
    darange <- c(0, max(4*yser))
  plot(xfac, yser, type='h',
       xlim=c(0,100),
       ylim=darange,
       main=sprintf('Sample Mean Distribution (Sample Size: %2d)', sampSize),
       xlab='',
       ylab='')
  dev.off()
}

