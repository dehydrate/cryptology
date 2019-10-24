# Hill

Command line program implementing the Hill cipher with a 2x2 matrix.
The Hill cipher encrypts a plaintext by multiplying each block of n
characters by an invertible n by n key matrix.

Syntax:

	# compile
	ghc Main.hs -o hill

	# a b c d are key matrix elements
	# to encrypt: 
	./hill -k a b c d -m encrypt {myplaintext}
	
	# to decrypt: 
	./hill -k a b c d -m decrypt {myciphertext}
	
	# to execute a known plaintext/known ciphertext attack:
	./hill -m crack -t {myciphertext} {myplaintext}


# Flags:

* -m	Specify mode: encrypt, decrypt, or crack. Must be included.
* -k	Specify key: four integers a b c d, such that the key is
			(a b)
			(c d)
* -t	Specify ciphertext and plaintext to extract a key. Note that
        if ciphertext is shorter than plaintext, you will get nothing.
        The order of arguments MUST be -t {ciphertext} {plaintext}.

If the mode is 'encrypt' or 'decrypt', a plaintext or ciphertext
argument must also be included.
