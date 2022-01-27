# WuRSA

This project, in addition to implementing the RSA algorithm for CSCI
391 (Practical Cryptology) at Widener University, is an exploration of
Haskell's Cabal package system and HUnit testing framework. The two
software engineering goals here are (1) to experiment with Test-Driven
Development and (2) to explore better approaches to organizing the
projects I've written for this course.


## Installation

	$ # To install with cabal:
	$ cabal new-install wursa	
	$ # To compile with ghc:
	$ ghc -i:exe:src exe/Main.hs -o wursa

Cabal will probably install to the ~/.cabal/bin/ directory.


## Usage

	$ /path/to/binary -m {mode} --{keyarg} -t {text}

* The -m flag can have one of four arguments:
	- encrypt, decrypt to signify encryption or decryption
	- generate to create a new key
	- validate to confirm a private key is correct

* The key argument can be one of two options:
	--public m e
	or
	--private p q e
	where p, q are primes, m = p x q, and e is a number relatively
	prime with (p-1) x (q-1).

	A public key consists of a modulus (m) and exponent (e). This is
	needed for encrypt mode.

	A private key consists of both primes (p, q) and exponent (e).
	This is needed for decrypt and validate modes.

* The text argument is the plaintext/ciphertext. Text should consist
	only of alphabetic characers.

Optional: 

* -s {length}: Used in generate mode, this flag specifies the
	approximate number of digits desired for primes p and q (base 10).
	If no length is specified, generate mode defaults to about 50
	digits.
