# agent

A solution for the IOHK ch/otp problem.

## usage

There is one executable `agent`, that does the right thing depending
on how you call it.

The basic architecture is built around a leader/follower model using
`distributed-process-simplelocalnet`.

You start `n` followers. Then you kick off the leader to run the
problem on the followers.

To start a follower:

```
# This will bind to a random free port on localhost.
agent &

# To expose to a wider network you can specify a host and port.
agent --host 10.1.1.9 --port 9999 &
```

To start the leader:

```
# This will work if working with agents on a single machine.
agent --send-for 10 --wait-for 2

# Expose to the network just like the followers.
agent --send-for 10 --wait-for 2 --host 10.1.1.9 --port 8888

# You can also specify a seed if you would like.
agent --send-for 10 --wait-for 2 --with-seed 123

# By default, the leader will leave the followers up so they
# can be called again latter to run the problem multiple times,
# however if you want to terminate after you are done, just say
# so. This is useful if you are profiling and need the follower
# to exit.
agent --send-for 10 --wait-for 2 --terminate-on-completion
```

The leader will discover the followers via standard
`distributed-process-simplelocalnet` discovery.


## building

To get an executable out in: `dist/build/agent/agent`.

I have built and tested this with GHC 8.0.2 and 8.2.1.

You can build it with mafia or cabal (stack should also work, but
I didn't test it).

Cabal:

```
cabal sandbox init
cabal install --only-dependencies
cabal configure
cabal build
````

Mafia:

```
./mafia build
```

If there are any serious issues you can get a linux build out of docker:

```
docker build -t markhibberd-iohk .
docker run -d -it markhibberd-iohk
docker ps # pull out container-id
docker cp <container-id>:/usr/local/bin/agent .
```

Running tests:

````
./dist/build/test/test
````

Profiling builds are easiest to get out of mafia:
```
./mafia build -p

# then run a follower with RTS options:
./dist/build/agent/agent +RTS -p -RTS &

# be sure to terminate on completion
./dist/build/agent/agent --send-for 1 --wait-for 1 --terminate-on-completion

# this will produce agent.prof
less agent.prof
```

## problem

Several nodes continuously send messages to other nodes in such way,
that every message reaches every node. You are free to use any model
of communication.  Each message contains a deterministic random number
n ∈ (0, 1]. That happens for some time, afterwards, during the grace
period, each node prints out the following tuple:

```
(|m|, sum(map (\ (i, mi) -> i * mi) $ zip [1..] m))
```

where m is the list of all messages sent by all nodes, ordered by
sending time, and mi is the i-th message sent by some node. You are
most welcome to print out debug information to stderr.  The larger
your score with a given sequence of random numbers is, the better.
Your code will be run under different network failure scenarios. For
now we don’t reveal “the failure maps”, please use your best judgement
of the most applicable communication model. You are free to tell us
under which assumptions you chose it.

## solution

There are a couple of interesting challenges to be addressed, with
tradeoffs to be made for each of them. In guiding my implementation, I
have taken the following view of the problem (in order of priority):

 1. A strong emphasis on making sure that all messages get to all
 nodes. Reading this as an emphasis on _reliability_.

 2. The message _ordering_ problem is significant, and under the
 assumption that all messages do get to all nodes, then the result
 will be _identical_ for every node. At no point should all messages
 be shared and we arrive at different answers. Reading this as an
 emphasis on _correctness_.

 3. We should be trying to achieve the highest _score_, which
 translates to throughput, and an emphasis on _performance_. This
 is obviously an important part of the problem, but given it is
 already a decent size interview problem I have opted for focusing
 on (1) and (2).

### time & order of messages

Given the problem as stated, we have to work out a total ordering for
all messages. Unfortunately this isn't straight forward. But, based on
how the problem is specified, it seems that it would be reasonable to
deal with this by utilising a logical clock, such as a _Lamport
Clock_. This logical clock keeps track of the order of events relative
to other events in the system. This is a neat solution that gives us a
partial order between events that can be further discriminated into a
total order using the process identifier of each node in the system.

Alternatives to the logical clock approach are possible, but on the
surface would appear impossibly error prone (using physical clocks) or
far too restrictive (establishing co-ordination between the nodes at
the cost of availability in the face of failure) for this problem.


### distribution

In order to address the distribution of messages between nodes, the
program is structured around a replicated log structure.

The log keeps track of the sorted events.

To send a message, a node just appends to its local log, and then
replicates the log to all other nodes.

When a node receives a log replica, it merges it with its own copy and
then forwards the updated copy to every other node.

This is a pretty simple protocol, but as the number of nodes and
messages increases this gets very chatty, and we would spend most of
our time replicating the same messages over and over between each
node. In order to address this we want to only replicate a delta
of the log that is relevant for each peer.

To achieve this delta replication we need to have an idea of what each
other node _knows_ about. This is accomplished by extending our
logical _Lamport Clock_ into a _Vector Clock_ that allows a node to
keep track of what logical time horizon it knows for each other node,
and then extending our _Vector Clock_ into a _Matrix Clock_ so we can
track what every other node knows about, not just ourselves. The
_Matrix Clock_ establishes a lower bounds on what each node knows
about, allowing us to trim the log to only contain relevant messages.

### future

The current implementation is pretty naive (and subsequently not the
fastest), and there are a number of interesting optimisations that
could be pursued in the quest for the _largest score_, and for
stabilising the drop off the current implementation has as you add
nodes.

Of particular note is the log structure maintained by each node. At
the moment a complete log is kept for each node. This keeps it simple,
but the _Matrix Clock_ provides enough information that it would be
possible to incrementally calculate the final result and discard no
longer required events.

Another big improvement that could be made is just being less chatty,
the approach at the moment is to be broadcast every send/replication,
every loop. A non-trivial amount of overhead could be avoided just by
being smarter with broadcasts, for example picking a replica-set from
a sub-set of peers, rather than replicating to every know, the size of
the replica-set could trade off resilience/recovery vs overheads. I
have left the naive approach as is for the submission to keep in the
spirit of _repeatedly sends message_.

There are also a few constant factors that could also be improved. For
the purpose of this exercise I decided to spend my time verifying the
correctness of the replication rather that worrying about these. A
profile will show up a couple of critical areas:

 - The implementation of the Vector & Matrix clocks are just a
   `Data.Map.Strict`. It gets a heavy work-out with horizon checks
   performed for every replication message, and some benefit could be
   had from spending some time optimising these data types.

 - The delta log calculation. Partly because of the previously mention
   clocks, but also because of the naive vector recreation.

 - The communication overheads dominate the rest.
