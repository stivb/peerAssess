r<-as.numeric(readline(prompt="Choose weighting: (1) quadratic (2) ordinal (3) linear (4) radical (5) ratio (6) circular (7) bipolar (8) unweighted: "))

z<-switch(r, 
       "quadratic",
       "ordinal",
       "linear",
       "radical",
       "ratio",
       "circular",
       "bipolar",
       "unweighted"
)
if (is.null(z)) z<-"unweighted"

print(z)