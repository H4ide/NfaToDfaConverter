# NfaToDfaConverter

run the following commands to build the project:

`cabal clean`

`cabal update`

`cabal build`

After it NFAtoDFA.exe will be created somethere in the `dist-newstyle` dir. You can move it where you wish

Usage:
`./NFAtoDFA.exe "input_file"`

Example:
`./NFAtoDFA.exe "test/nfa.txt"`

to run tests:
`cabal test`

you can see examples of input and output in the test folder. The corresponding outputs are located in the test/keys folder
