Wagon 
= 


1 
== 


A. I used `foldl` because I remember reading that it provided composable single-pass folds. The two core ideas are just that the Applicative instance (1) is pairwise application (2) with strict pairs.


B. Both concrete rows and a summary of rows share the same type (`Row_`). I took an idea* I had recently to the limit, and I think it works out.

*Recently, on a personal project, I generalized a grammatical `Production` type to `Production t n` (where `t` means "terminal" and `n` means "non-terminal"). `t` can be any type, but it would only really be unified with either `String` or `ValidTerminal` 
(a refinement), since `Production` is very domain-specific. This in contrast to `[a]`, for example, which is generic enough that it tends to be used with hundreds of types. I don't know a name for this pattern ("gratuitous polymorphism"?), probably because it's obvious, but at the time it was a big deal for me. Whenever I wanted to transform data, I could just "poke a hole" in the fields that I wanted to transformation; bringing them to the type-level by making the conrete type into a type variable, so the transformation's type would reflect what's being transformed (e.g. `validateProduction :: Production String String -> Maybe (Production ValidTerminal ValidNonTerminal)`). 


C. I got constant residency with a `pipes` one-liner (see `summarizeStdinPipes`). But to make sure I understood the space leak, I profiled an ad-hoc iterator (`foldI`), which still leaked. Then, I tried to make the accumulators totally strict (hence strict `Maybe'` and `Pair` and `BangPatterns`), which indeed sealed the space leak (see `summarizeStdinStreaming`). I could have used enough `seq`s, but I found totally-strict data-structures easier to reason about (c.f. "I know the fields of strict type X' can't be* thunks, ever" v. "I hope I forced the fields of lazy type X in each function that uses it").

1,000,000 rows takes less than a minute with <50kb max residency, which seems fast enough / lean enough. 

Since it streams, we could implement the "./generator PULL" extension just by turning the fold into a scan.

*though, they can still have thunks, if only in whnf. 


D. Profiling shows that parsing is the bottleneck. We could switch to attoparsec. Or, just split the String on commas, since the CSV format is simple enough (i.e. no quoted commas in fields). 

Since all our summaries (count, sum, average, minimum/maximum) are commutative, we might be able to parallelize the folds without wasting time preserving the order. 


E. Dependencies: the `parsec` package I've used before, `lens` is used in a minor way, `pipes` is to compare against my iterator, and I thought a simple streaming problem would be a good excuse to learn the `foldl` package. I think this is what I would do (1) on the job (2) to save the most time. This way, I can focus my efforts on Haskell-y things like rich types and purity. In a follow-up, I could also use only the standard library, if you want me to. 


2 
== 

