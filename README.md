ptree
=====

# Motivation

The primary idea was to implement prunable prefix tree in Erlang binaries
instead of using tuples. The main advantage of such approach would be that
trees could be shared across many processes without being copied, i.e. coping
of binaries is avoided, if they are handled in the [smart][1] [way][2].

# Results

However, after running a few benchmarks it became clear that for write
intensive applications such approach is inefficient (compared to tree
implementation with tuples) due to the linear traverse of children.

[1]: http://www.erlang.org/doc/efficiency_guide/binaryhandling.html
[2]: https://github.com/erlang/otp/blob/maint/erts/emulator/beam/erl_bits.c
