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
