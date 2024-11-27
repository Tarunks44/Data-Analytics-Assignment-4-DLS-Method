# Data-Analytics-Assignment-4-DLS-Method



This file contains data on ODI matches from 1999 to 2011. It is taken from this site. There is an
R code for finding the 'run production functions' in this site, but you will do something marginally
different in the following assignment.
Discussion is encouraged. But write your own code. Please comply with the ethics policy. You
must sign the submission statement and click the submit button to submit your work.
Using the first innings data alone in the above data set, find the best fit 'run production functions'
in terms of wickets-in-hand w and overs-to-go u. Assume the model:
Z(u, w) = Z0(w)[1 - exp{-L(w)u/Z0(w)}].
Denoting y' to be the model prediction and y to be the actual runs scored, use the following loss
function
loss(y', y) = (y'+1) log((y'+1)/(y+1)) - y' + y,
summed across overs, wickets, and the data points for those overs and wickets, then
normalised by the total number of points. Note that your regression does not force all the slopes
to be equal at u = 0.
Then set the common slope L to be the weighted average of the above-obtained slopes L(w),
with the weight for a particular L(w) coming from the number of points for that w.
With the above L, re-do the regression to get the best fit Z0(w) for each w.
