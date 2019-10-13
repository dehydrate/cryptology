Aaron Cogbill
CSCI 391
Lab 2



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



Explanation of algorithm:

There is a constant definition in Keys.hs: matches = 26. This tells
the program to compare the top 26 most frequent letters in English
with the top 26 most frequent letters in the ciphertext; changing
matches will compare more or less of the most frequent
plaintext/ciphertext letters. There's no reason the value of matches
couldn't be passed at runtime, but it's pointless to use a number
higher than 26, since the character set is also hard-coded (since I
had to specify the order of letter frequencies) and it only has 26
elements.

Based on the value of matches, I create a list of all possible
plaintext to ciphertext letter mappings, looking at the top 26 (by
default) most common letters. So there will be 26^2 possible single
plaintext-ciphertext character mappings.

Then I select two pairs from the list of mappings, creating (26^2
choose 2) sets of paired tuples ((p1, c1), (p2, c2)).  From this list
all the pairings that create an invalid key A are removed, leaving
only a list of tuples that can be solved for valid keys. All repeated
key combinations are also removed.

The ciphertext is decrypted with each possible key combination,
creating a list of possible plaintexts. Finally, I score each
plaintext based on how closely its letter frequencies match English
(the scoring function is called "badness" in Stats.hs). The score is
used to sort the list of possible plaintexts in order of ascending
badness. The program returns only the first (i.e. least "bad") element
of the list of possible plaintexts. This is for user convenience. The
"badness" score might not be useful for very short plaintexts, though
(e.g. if plaintext is just the word "syzygy").

Input is checked for invalid characters, which produce an error
message. Otherwise, the spaces are filtered out of all input, and all
uppercase characters are translated to lowercase. Results are written
to stdout.
