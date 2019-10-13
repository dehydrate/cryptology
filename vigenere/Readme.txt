Aaron Cogbill
CSCI 391
Vigenere lab

Syntax:

	$ # run as a script
	$ runhaskell main.hs < {inputfile} > {outputfile}

	$ # compile and run
	$ ghc -o aaronsprogram main.hs
	$ ./aaronsprogram < {inputfile} > {outputfile}


Explanation of contents:

This program contains some of the same modules as my previous affine
cracking program did; comments have been added to explain the purpose
of functions now. Stats.hs now contains an additional
indexOfCoincidence function (which ended up not being very useful).
Preprocessing functions that used to belong to main.hs are now part of
Utils.hs.

Keys.hs and Vigenere.hs are new or changed. The Vigenere module
implements the shift and Vigenere ciphers. Keys.hs contains functions
for blocking an input string by key length and for finding the key
from either known or unknown key length. 


Explanation of algorithm:

The main function takes an input string, and, after checking that it
contains only valid characters, finds the most probable key for key
lengths from 0 to 9. Then it finds the "best of bests" by comparing
the plaintexts produced by the 10 different length keys. Checking keys
of length 0 to 9 is a workaround, since I couldn't get the index of
coincidence approach to accurately predict key lengths.

The module Keys implements three attacks on ciphertext. In one, given
known key length k, the ciphertext is blocked into k-sized chunks.
Individual alphabets shifted by each individual key letter are then
split apart, creating k separate shift ciphers to solve. Since the
alphabet is only 26 letters, it is possible to try each letter as a
shift key. The 26 possible keys are therefore sorted by the "badness"
score of the plaintext they produce. The full Vigenere key is a
combination of the least bad shift key for each of the k shift ciphers
that have to be solved. It is also possible to to produce a list of
all possible k-length keys in order of least to most "bad," but this
would cost a lot of extra computation, and it hasn't been necessary to
examine any alternative keys for the tests I've run.

The second attack guesses key length with the index of coincidence
measure, then feeds this value to the first attack. Unfortunately, the
index of coincidence method did not predict key length very
accurately.  The third attack takes a maximum key length n and tries
the first attack for each length from 1 to n. Each of these produces a
"best" k-length key.  The badness of the plaintext as determined by
each of these keys is then compared to select the best of the best
keys. This attack can produce false positives if the maximum length is
too high, because a long enough key will allow "overfitting" to
minimize the badness score (the badness of an English string decreases
slowly as the length increases, but some strings of nonsense
characters can produce arbitrarily low badness). One workaround, which
I have not implemented, would be to allow the user to specify a
maximum key length m and a step size s. The program would then find
the "best of bests" key for lengths from 1 to s, then from s+1 to 2s,
then 2s+1 to 3s, etc. up to m, and allow the user to select the best
solution from m/s possibilities. For example, a maximum length of 50
and a step size of 5 would find the best key with 1-5 characters, the
best key with 6-10, etc.

The badness function, which is somewhat opaque in code, simply finds
the difference between the frequency of a letter in a given string and
the expected English frequency. This difference is summed over all the
letters of the alphabet to produce the final score.
