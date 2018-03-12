# agent

A solution for the IOHK ch/otp problem.

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

There are a couple of interesting challenges to be addressed.

The first is: what does _send time_ mean across a distributed set of
nodes.

The second is: how do we (with some reasonable level of efficiency)
ensure nodes see as many of their peers messages as possible in the
face of potential point-to-point communication failures.

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
