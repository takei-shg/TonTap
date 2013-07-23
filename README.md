TonTap
======

TonTap = Today's on Tap. This app crawls the web and provides the Today's on Tap Info in the Beer Pubs.

### Install

Currently, riak-haskell-client binary from Hackage doesn't work, because of the mistake in version description.  
Please checkout the source code and do `cabal-dev add-source {path-to-riak-haskell-client}`.

```
$ cabal-dev add-source {path-to-riak-haskell-client}
$ cabal-dev install
```

### Setup Riak

You need VirtualBox and Vagrant to setup the Riak.
After installed them, just input,

```
$ vagrant up
```

With this command, Vagrant reads Vagrantfile from the current directory.  
It setups Riak on Ubuntu with 3-nodes.

### Run & Check

```
$ ./cabal-dev/bin/Tontap
```

with HTTPie, command this

```
$ http localhost:8098/riak/craftheads/craftheads
```

Then you will get the onTap info (currently, you can only get the info about craftheads).
