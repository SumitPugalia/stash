# Stash

> ### Stash is a `distributed cache system` which can be used independently or as dependency for other projects.

[![Build Status](https://travis-ci.com/spawnfest/stash.svg?token=okHpxnXEEGaxWEb3oShA&branch=master)](https://travis-ci.com/spawnfest/stash)

## Summary

Configuration
-------------
```
{stash, [
  {nodes, ['node1@127.0.0.1', 'node2@127.0.0.1']},
  {gc_interval, 15000} % in milliseconds
]}
```
```
nodes: list of all the nodes we want our current node to be connected with.
	All the data is replicated from above mentioned nodes.
	By default its empty ([]).

gc_interval: Its the time interval in milliseconds after which the expired data 
	will be removed from the system. By default its 15 seconds.
```

Example
-------
 ## Functionality
```
 stash:set(b, 2, 10). (set/update, here 10 is ttl so after 10 sec `b` will expire automatically).
```
``` 
 stash:set(a, 1).  (set/update, if ttl is not passed it will never expire).
```
```
 1 = stash:get(a). (get the value of the `a` from the system).
```
``` 
 stash:delete(a). (deletes `a` from the system).
```
 ## Distributed
```
 ## Node1 
(stash@Sumit-Mac)1> stash:set(a, 1).
ok
(stash@Sumit-Mac)2> stash:get(a).
1
(stash@Sumit-Mac)3> stash:get(b).
10
```
```
 ## Node2
(stash2@Sumit-Mac)1> stash:get(a).
1
(stash2@Sumit-Mac)2> stash:set(b, 10).
ok
(stash2@Sumit-Mac)3> stash:get(b).    
10
```
Coverage
--------
```
  |------------------------|------------|
  |                module  |  coverage  |
  |------------------------|------------|
  |                    gc  |      100%  |
  |                 stash  |       93%  |
  |             stash_sup  |      100%  |
  |             stash_app  |      100%  |
  |                  time  |      100%  |
  |------------------------|------------|
  |                 total  |       95%  |
  |------------------------|------------|
```
Build
-----
	To run the test:

```
$ rebar3 ct
```