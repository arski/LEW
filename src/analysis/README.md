Having performed a set of experiments, you will now want to analyze the data for different trends and correlations.

The first step towards doing so is transforming the LEW output into easily processable CSV summaries. The lew_summarize script does just that.

To combine a set of experiments into one summary CSV (which is the easiest way if you want to process the data in R or something similar later), run:

python lew_summarize.py --dir data_directory1 dd2 dd3.. --out summary_filename [--stages x]

Example:

python lew_summarize.py --dir 2012-09-21_00-52-05  2012-09-21_00-52-14  2012-09-21_00-52-23 --stages 10 --out experiment-stages.csv

The above will take the data from the three specified directories, divide every executed run into 10 "stages" of equal length (with the end of last stage corresponding to the end of the experiment), and put the evaluated properties at that point in time inside the specified .csv file. The stages parameter is optional, if ommitted, the results at the end of the simulations will be written to the output file.

The lew_summarize script does nothing more than take the data, clean it, compute averages over last stage if necessary, and put it into a single CSV file. Two further summary scripts are of particular interest here. The lew_summarize_giratios writes down the interaction ratios between groups, useful for social network analysis, while lew_summarize_graph directly outputs the number of groups that any particular group communicates with with a probability higher or equal to the specified --wthres parameter.

Analyzing the resulting CSV is not too complicated, but requires some experience in R (or similar). The following are some basic examples of how to draw an interaction plot with one particular property of the data:


c <- read.csv('summary.csv', TRUE, ' ')
attach(c)

par('cex'=1.3)
par('pin'=c(5,3.5))
par('font.main'=1)

interaction.plot(factor1,factor2,MeaningF1Avg,legend=F,fixed=T,xlab="",ylab="",ylim=c(0,1),main="Understanding F1")
legend('topleft',legend=c('Fixed partner', 'Community'),lty=c(2,1),inset=0.04,cex=0.9,bty='n',text.width=strwidth("ABCDEFGHIJ"))
dev.copy(postscript,'exp1-interaction-f1.eps')
dev.off()


While the basic R plotting tools are pretty extensive, the ggplot2 library is really recommended for graphs that can be properly configured and presented in papers.


Notes:

1. In the above examples, it is assumed that the Python script file and the data directories are in the same folder, which will usually not be the case. The recommended approach is to go into the parent folder of the data directories and execute the command from there, accessing the script with an adjusted [../]../src/analysis/ path prefixed.

2. The number of stages must be a multiplier of the stat_span setting in the model, otherwise some weird stuff will happen.
