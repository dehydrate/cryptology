To-do: in-depth explanation of the Vigenere cipher.

Syntax:

	$ # run as a script
	$ runhaskell main.hs < {inputfile} > {outputfile}

	$ # compile and run
	$ ghc main.hs
	$ ./main < {inputfile} > {outputfile}

The main function takes a ciphertext input string, and, after checking
that it contains only valid characters, finds the most probable key
and plaintext for key lengths from 0 to 9. Then it finds the "best of
bests" by comparing the plaintexts produced by the 10 different length
keys. 

The module Keys implements three attacks on ciphertext. In one, given
known key length k, the ciphertext is blocked into k-sized chunks.
Individual alphabets shifted by each each key letter are then split
apart, creating k separate shift ciphers to solve. Since the alphabet
is only 26 letters, it is possible to test each letter as a shift key.
The 26 possible keys are therefore sorted by the "badness" score of
the plaintext they produce. The full Vigenere key is a combination of
the least bad shift key for each of the k shift ciphers that have to
be solved. It is also possible to to produce a list of all possible
k-length keys in order of least to most "bad," but this would cost a
lot of extra computation, and it hasn't been necessary to examine any
alternative keys for the tests I've run.

The second attack guesses key length with the index of coincidence
measure, then feeds this value to the first attack. Unfortunately, the
index of coincidence method did not predict key length very accurately
for relatively short ciphertexts.  

The third attack takes a maximum key length n and tries the first
attack for each length from 1 to n. Each of these produces a "best"
k-length key. The badness of the plaintext as determined by each of
these keys is then compared to select the best of the best keys. This
attack can produce false positives if the maximum length is too high,
because a long enough key will allow "overfitting" to minimize the
badness score (the badness of an English string decreases slowly as
the length increases, but some strings of nonsense characters can
produce arbitrarily low badness). 

The badness function, which is somewhat opaque in code, simply finds
the difference between the frequency of a letter in a given string and
the expected English frequency. This difference is summed over all the
letters of the alphabet to produce the final score.
