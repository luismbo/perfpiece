Perfpiece
=========

Perfpiece is a tool for measuring the performance of Lisp code, not
unlike the standard <code>CL:TIME</code>.


Features
--------

Here are its main features:

1. Lispy interface to [PAPI (Performance Application Programming
   Interface)][1]. PAPI is a library that enables access modern CPU’s
   hardware counters. This allows us to measure several events such as
   processor cycles, cache misses, number of floating-point
   instructions, and almost two hundred other events. Perfpiece
   dynamically inspects the current platform’s supported events at
   runtime and enables the user to inspect this list and measure these
   events. We also use this library to measure real (wall-clock) time
   and user (virtual) time.

2. Support for other non-PAPI events such as the number of GC runs,
   CPU usage, and operating system resource usage (via the getrusage()
   system call).

3. Transparent support for multi-threaded programs. It includes a
   helper C library that when loaded through the POSIX LD_PRELOAD
   mechanism will preempt pthread creation/termination calls and allow
   for the individual measurement of events across threads created
   during a measurement session. This includes both Lisp threads as
   well as threads created by C code. This is rather limited at the
   moment; only real/user/cpu time is measured for new threads.

4. Segregation of measurements between mutator and GC.

5. Support for sampling. Perfpiece can repeat a given measurement a
   number of times then calculate and report basic statistic analysis:
   minimums, maximums, geometric means, and standard deviations for
   each measured event.


Usage
-----

### ascertain

The simplest way to interact with this library is through the
ascertain macro, which works very much like <code>cl:time</code>. The
following example shows the default events measured for a very simple
arithmetic form:

<pre>
PERFPIECE> (ascertain (+ 1 1))

                                          non-GC            GC         Total
────────────────────────────────────────────────────────────────────────────
                     Total cycles:         8,343             0         8,343
           Instructions completed:           481             0           481
        Level 2 data cache misses:            78             0            78
 Level 2 instruction cache misses:            24             0            24
                         GC count:             -             -             0
     Involuntary context-switches:             0             0             0
       Voluntary context-switches:             1             0             1
                      Page faults:             0             0             0
                    Page reclaims:             6             0             6
                      System time:             0             0             0
                        CPU usage:       100.00%             -       100.00%
                        User time:       6.00 µs             0       6.00 µs
                        Real time:        613 ns             0        613 ns

0 new threads were spawned

2 ; printed result of (+ 1 1)
</pre>

Having loaded the helper library using <code>LD_PRELOAD</code>, we can
measure multi-threaded code:

<pre>
PERFPIECE> (ascertain (loop repeat 2 do
                        (sb-thread:join-thread
                         (sb-thread:make-thread (lambda () (sleep 0.5)))))
                      :events '(:real-time :user-time :cpu-usage))

                                          non-GC            GC         Total
────────────────────────────────────────────────────────────────────────────
                        CPU usage:         0.02%             -         0.02%
                        User time:     206.00 µs             0     206.00 µs
                        Real time:       1.335 s             0       1.335 s

2 new threads were spawned
  #0 real: 667.54 ms, user: 101.00 µs, cpu: 0.02%
  #1 real: 667.60 ms, user: 144.00 µs, cpu: 0.02%
</pre>


### sample

The other main function is <code>sample</code>. In the following
example, we're measuring FP instructions, invoking some code 10 times,
and aggregating measurements in several ways:

<pre>
PERFPIECE> (sample (lambda () (* 2 pi)) :events '(:papi-fp-ins) :samples 10)

[Floating point instructions]       Min           Max          Mean     Stddev
──────────────────────────────────────────────────────────────────────────────
                   total:            35            36         35.60  ±  1.376%
                  non-gc:            35            36         35.60  ±  1.376%
                 gc-only:             0             0             0  ±  0.000%
</pre>


<code>sample</code>'s got a <code>:report</code> keyword argument that
you can use to get machine-readable results:

<pre>
PERFPIECE> (sample (lambda () (* 2 pi)) :events '(:papi-fp-ins) :samples 10
                   :report nil)

((:PAPI-FP-INS :MIN (35 35 0)
               :MAX (38 38 0)
               :MEAN (184/5 184/5 0)
               :STDDEV (1.0770329 1.0770329 0.0)))
</pre>




  [1]: http://icl.cs.utk.edu/papi/
