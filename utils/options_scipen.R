# check the defaults
options("scipen")
options("digits")
1 / 3
1000 / 3
100000 / 3
1 / 300
1 / 30000

# disable scientific notation, display 0 significant digits
options(scipen = 999, digits=0)
1 / 3
1000 / 3
100000 / 3
1 / 300
1 / 30000

# disable scientific notation, display 3 significant digits
options(scipen = 999, digits=3)
1 / 3
1000 / 3
100000 / 3
1 / 300
1 / 30000

# enable scientific notation, display 7 significant digits
options(scipen = 0, digits=7)
1 / 3
1000 / 3
100000 / 3
1 / 300
1 / 30000

# force scientific notation or decimal notation for a single command
format(100000 / 3, scientific=FALSE)
format(100000 / 3, scientific=TRUE)

