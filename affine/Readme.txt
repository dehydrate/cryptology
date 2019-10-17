Running main.hs:

The program can be run either as a script or a compiled program. It
accepts input from stdin. The compiled version is vastly faster.


To run as a script:

	$ runhaskell main.hs < {testfile} > {resultsfile}


To compile and then run:

	$ ghc -o aaronsprogram main.hs
	$ ./aaronsprogram < {testfile} > {resultsfile}


If you run the program without redirecting in the input, you'll have
to exit with Ctrl-C.
