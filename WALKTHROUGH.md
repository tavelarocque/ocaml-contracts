# Phase 1 walkthrough

This explains each Phase 1 commit in order: what changed, why, and the OCaml or finance idea behind it. Read one section, then look at that commit's diff with `git show <hash>` (the hashes are in `git log --oneline`), then go on to the next. The code comments repeat some of this, but here the ideas are connected into one story.

## 1. Add Anytime; the pricer rejects path-dependent contracts

The paper's `anytime` combinator describes American-style exercise: you will acquire the inner contract, and you get to choose the moment, as long as it is no later than a deadline. Note what is optional and what is not. The *timing* is your choice; acquiring the contract is not. That's why an American call is written `anytime T (or_ (receive S − K) zero)`. The `or_ … zero` inside is what makes exercising optional, and `anytime` only adds the choice of when.

Adding the constructor was the first real test of why this project is in OCaml. The moment `Anytime of time * t` went into the type, `dune build` refused to compile and pointed at every `match` that no longer covered all cases: `dates` in `contract.ml` and `value` in `pricer.ml`. In Python, the same change would have compiled and run, and the missing case would only show up as a wrong answer, possibly months later. Here, the compiler gives you the list of places to update.

The interesting design question was what the pricer should do. A single-date density tells you how the price is distributed on one day. It says nothing about the path in between, and the best moment to exercise depends entirely on that path. So the honest answer is a typed error, `Path_dependent`, rather than a number. Adding that error constructor meant editing both `pricer.ml` and `pricer.mli`. The `.mli` is a promise to every other module about what the pricer exposes, and OCaml refuses to build if the implementation and the promise disagree.

There is one subtle case, and it's a good interview point. If you acquire an `Anytime` *at its own deadline*, the window "any moment up to now" has shrunk to "this moment", so it is just the inner contract and can be priced. A test shows that `at T (anytime T call_payoff)` prices exactly like the European call.

## 2. Add Kalshi.below and the above + below identity

"BTC above K" and "BTC below K" together cover every outcome exactly once, so holding both pays $1 for certain. Something that pays $1 for certain at T is worth the discount factor today, whatever the model, the strike, or the shape of the density. That makes this a model-free identity, the best kind of test: if it fails, the pricer is wrong, not the model. The test checks it at strikes deep in both tails as well as near the money, and the difference is zero to nine decimal places.

## 3. Add Contract.to_english, a third interpreter

This is the deep-embedding idea in action. The same contract tree that the pricer turns into a number, `to_english` turns into a sentence: "at day 7: if Spot >= 102000 then receive $1 else nothing". The function has the same shape as the pricer, one case per constructor, recursing into sub-contracts, but each case means something different. That's the payoff for making contracts data rather than functions: adding an interpreter never requires touching the contracts themselves.

A smaller lesson is in `paren`, which puts brackets around sub-contracts unless they are a single word. `go` and `paren` are defined together with `let rec … and …`, which is how OCaml writes two functions that call each other.

## 4. Add Density.of_csv

This is groundwork for Milestone 2: reading a density from a file of `price,probability` lines. The OCaml idea here is *error handling as data*. Each line is parsed into one of three outcomes: skip it (blank, comment or header), a good point, or an error carrying the line number. `Or_error.all` then combines the results. If every line is fine, you get the list of points. If not, you get every error together, so one run shows all the problems in a file. The result then goes through `Density.create`, which is the only way to make a density, so a file can never sneak an invalid distribution past the checks.

The file version is three lines on top of the line version. Splitting the logic this way means the tests can feed in lists of strings directly, without creating files.

## 5. Explain and test the digital error

This answers the question an interviewer will ask about your pricer: why are digitals off in the fourth decimal when calls agree with Black-Scholes almost to the cent?

The density is a grid, and all the probability inside a bin sits on one point. A digital's payoff jumps from 0 to 1 at the strike, so the bin containing the strike is counted either wholly in or wholly out. The error is therefore a fraction of one bin's probability, and it depends on where in the bin the strike lands. The first test makes this visible: slide the strike across a $30 bin in $5 steps, and the error climbs in a straight line from −7.9 to +7.8 (in units of 10⁻⁴), then jumps back when the strike crosses into the next bin. That's a sawtooth.

The second test shows the error shrinking in proportion to bin width: four times as many points gives a quarter of the error. A call's payoff is continuous: just past the strike it pays almost nothing, so misplacing mass inside that one bin costs almost nothing. Elsewhere the payoff is linear, and a point placed at the bin's conditional mean prices a linear payoff exactly. So the call's error is a few cents at worst and shrinks faster than the digital's.

The practical lesson for Milestone 2: don't price Kalshi digitals by summing grid points. Price them from smooth call prices, as minus the slope of the call curve, which is exactly what Implied Odds' `bl.py` does.

## 6. Add property tests

An expect test checks an example you thought of. A property test states a law and lets Quickcheck search for a counterexample among hundreds of random contracts, including ugly nested ones nobody would write by hand.

The core of `test_laws.ml` is a *generator*: a recipe for random contracts that mirrors the language. It starts with leaves (zero, one, receive $X) and combines them with give, and, or, scale and cond, using `recursive_union`, which grows trees up to a size limit. The generated contracts are real: typically about 8 nodes and up to around 90.

Be honest with yourself about what each law proves. Several of them, such as `give (give c) = c` and "or chosen today = max", restate the pricer's own definitions, so today they cannot fail. They are guards for the future: if you rewrite the pricer for speed in Milestone 2, they pin down what it must still do.

The laws with real teeth involve time. "At T, (and a b) = (at T a) + (at T b)" holds only because the `At` case averages correctly over the density. "Choosing at T is worth at least as much as choosing today" is the straddle result from the paper, now checked on random contracts. It is a strict inequality in about a third of trials, so it is genuinely tested. A final test checks the checker itself: a deliberately false law must be rejected, with a counterexample.

## 7. Fixes from an independent review

A fresh reviewer read the six commits above and found real problems. Each one is worth understanding, because each is a kind of mistake you'll make again.

**Dates in three directions.** The first version of `Anytime` returned `Path_dependent` for every date except the horizon, which was the wrong *reason* in two cases. A deadline that has already passed isn't path-dependent: the opportunity is gone, so the error is now `Already_passed`. A deadline beyond the horizon isn't really path-dependent either: the model has no dates that far out, so the error is `Unsupported_date`. The pricer now compares each date with "now" and with the horizon in every direction. A test prints all nine combinations side by side. The general lesson: when you return an error, its *reason* is part of the answer, and it should be right.

**Rounding that changes meaning.** `to_english` used `%g`, which keeps six significant digits, so a strike of 101,999.99 printed as 102000. Kalshi's real strikes end in .99, so the printed contract would have described a different contract from the one being priced. It now prints ten significant digits, and hourly times print as hours rather than "day 0.04".

**NaN.** "Not a number" fails every comparison, so the check `p < 0.` quietly let a NaN probability through. `create` now rejects anything that isn't a finite number. The same commit stops `Density.points` from handing out the internal array, which a caller could have modified after validation, and adds a `?tolerance` argument. Real exported densities lose a little probability in the tails, and the default tolerance of one in a million would reject them.

**CSV details from real files.** Headers written as `price, probability` with a space, the invisible byte-order mark some editors add, and Windows line endings are now all accepted. Hex numbers and OCaml's `1_00` digit separator are now rejected: OCaml's own parser accepts them, but in a data file they're almost certainly mistakes.

**A workflow trap with `dune promote`.** One fix, a corrected comment, was silently lost along the way. Here is what happened: `dune runtest` writes the actual output into a corrected copy of the test file *as it was when the tests ran*. Editing the source after that and then running `dune promote` copies that older version back over your edit. The rule: run `dune runtest`, read the diff, run `dune promote`, and only then edit.

# Milestone 2, step 1: matching Implied Odds

These three commits connect the project to real data. The goal was concrete: price every Kalshi contract in Implied Odds' export through the contract language, and match Implied Odds' own probability `p_opt` to 1e-9 on every row.

## 8. Smile and Export: loading the data

`Smile` is the SVI volatility smile for one Kalshi settlement time. Total implied variance is a hyperbola in log-moneyness. From it comes a volatility at every strike, a Black-76 call price at every strike, and the probability of finishing above a strike as minus the slope of the call curve. That is Breeden-Litzenberger again, now applied to a smooth curve instead of a grid. Every formula follows the export's README exactly, including the size of the finite-difference step, because matching to 1e-9 means computing the same thing, not merely something close.

One deliberate deviation: the normal CDF is written with `erfc`, not `(1 + erf) / 2`. Far in the lower tail, N(x) is tiny, and adding it to 1 then halving throws away every significant digit. Near the money the two forms differ only in the last bit or two, far below the 1e-9 target.

`Export` reads the CSVs by column name, not position, so a reordered export still loads correctly. Its `read` helper takes a function `get : string -> string Or_error.t` and hands it to the code for each row. Combined with `let%bind … and …`, this collects every bad field in every row with its line number: the error-as-data idea from section 4, scaled up.

## 9. A second pricer: static replication

This is the conceptual core of the step. Section 5 showed that pricing a digital by summing grid points is off by up to one bin's probability, around 1e-4. That's nowhere near 1e-9. So a Kalshi contract has to be priced from the smooth call curve directly, but still through the contract language, or the project would be bypassing its own point.

The answer is a fourth interpreter, `Payoff.of_contract`. It turns the payoff at the horizon into a piecewise-linear function of the price:
- a digital is 0 then 1;
- a call is 0 then S − K;
- a butterfly is a tent.

Each operation of the language becomes an operation on these functions: `And` adds them, `Give` negates, and `Or` takes the pointwise maximum, splitting a piece where two lines cross. That crossing is exactly where a call's kink comes from. `Cond` turns its predicate into a 0/1 step and picks a branch on each side.

Once a payoff is linear pieces, pricing it needs only two curves. A piece paying a + bS on [lo, hi) is worth a·(P(lo) − P(hi)) + b·(M(lo) − M(hi)), where P is the probability of finishing above a strike and M is the partial mean, read off call prices as M(K) = C(K) + K·P(K). This is Carr & Madan's static replication: every piecewise-linear payoff is a portfolio of bonds, digitals and calls.

How do we know replication is right? Feed it the same grid density the first pricer uses, and the two must agree on random contracts, both in value and in which contracts they refuse. They share no pricing code beyond the contract type, so a bug in either would show up as a disagreement. A Kalshi contract written as `Kalshi.above`, compiled to a step and integrated against the smile, then reproduces `p_opt` to 2.7e-12 on all 13,804 rows. `bin/check_export.exe` runs that check on any export. Feeding it a copy with one probability altered by 1e-8 makes it fail, so the check can actually fail.

## 10. Fixes from the second review

A reviewer again found real bugs. Each one teaches something.

**Midpoints of adjacent floats.** `combine` originally asked each payoff "which piece covers the middle of this interval?" When two breakpoints are adjacent floating-point numbers, as in a crossing computed as 0.1k / 0.1 that lands one float below k, the middle rounds onto an endpoint, no piece matches, and the code raised an exception. The fix walks each payoff's pieces in step with the intervals, so no arithmetic is involved. The lesson: never locate something by computing a point between two floats if you can locate it structurally.

**Which side owns the boundary.** "S ≥ K" and "S > K" are the same under a continuous distribution, but not under a grid that has a point exactly at K. Pieces now include their left end, [lo, hi), so "S ≥ c" starts a piece exactly at c. "S ≤ c" needs c included, so its cut goes at `Float.one_ulp \`Up c`, the next float above c, with nothing in between. With that, both pricers count a point mass on a strike the same way, and a test with round prices and round strikes proves it.

**Branches no price can reach.** `Cond (Spot >= 0, one, at 0 one)` can never take its second branch, but replication used to compile it anyway and report its error. It now skips a branch whose indicator is constant.

**One source of truth for dates.** The date rules had been copied into three files. They now live in `Unpriceable`, and all three pricers call the same two functions. Copied logic drifts; shared logic can't.

**A finding about the data, not the code.** On a few fitted smiles the SVI parameter s is tiny, around 3e-6, which makes the smile almost a sharp V. Near that kink, the probability you get depends on the finite-difference step. For 2 of the 13,804 contracts, both hourly and near the money, shrinking the step a hundredfold moves the probability by about 6 points. Nothing in this repo is wrong there; it matches `p_opt` exactly. But an apparent mispricing on such a contract may be an artefact of the fit rather than of the market. That's worth checking in Implied Odds before trusting a trade there.

# Milestone 2, step 2: the comparison

## 11. Kalshi against the options, made fair

With every Kalshi contract priced from the options, the question becomes how the two markets differ. `Analysis` and `Report` answer it in three ways. `bin/analyze.exe` prints the answers and saves them as CSVs.

**A seal first.** This comparison is Implied Odds' pre-registered layer-5 verdict, sealed until 2026-11-30, and the exports cover that window. It was run on that window before the seal was known here, and twice more afterwards by mistake from a stale build; those results have been removed from this project and should be noted as a deviation in layer 5's log. `analyze.exe` now refuses to run on real data before 2026-11-30, and its tests use synthetic quotes. A pre-registered test only means something if nobody looks at the outcome first, and the code now enforces that rather than relying on memory.

**Gap by probability.** For each quote with both a bid and an ask, Kalshi's mid price minus the options' probability, grouped by that probability.

**Width of the distribution.** If Kalshi expected bigger moves than the options do, it would price unlikely outcomes higher and likely ones lower. To test that directly, find the strikes where each price curve crosses 75% and 25%. That distance is the middle half of the distribution, its interquartile range. Then divide Kalshi's by the options'.

The first version of this measurement was biased, and the bias is worth understanding. A probability curve is S-shaped, and Kalshi only quotes strikes $100 or more apart. Joining neighbouring strikes with straight lines pushes both quartiles outward, so any distribution measured that way looks wider than it is. The test that exposed it: run the options' own probabilities through the same method, which should give a ratio of exactly 1. It gave 1.03.

There were two fixes. First, interpolate on the normal-quantile scale (`Analysis.probit`), where an S-curve is close to a straight line; that cut the stretch to about 1%. Second, measure the options the same way, from their probabilities at the same strikes. Then whatever the method does to one side it does to the other, and the ratio compares like with like. `shape.csv` still reports the method's own stretch, so the bias stays visible rather than hidden.

The general lesson: before trusting a measurement, feed it a case where you know the answer.

**Quotes that beat the fee.** Under the options' probabilities, buying YES at the ask is worth q − ask − fee, and selling YES at the bid is worth bid − q − fee. The fee is Kalshi's 0.07 × price × (1 − price) per contract, rounded up to the cent per order. The counts only include quotes the options put between 5% and 95% with enough contracts on offer. Below 5% and above 95%, the only quotes that survive are ones with a bid, often at the 1¢ minimum, and including them inflated the counts. That's a selection effect, which is easy to miss because nothing about any single quote looks wrong.

Any gaps found are differences between two markets, not profits. The options' probabilities are risk-neutral, and whether a gap pays is a question for settled outcomes over many days, in Implied Odds, where the trading rules are pre-registered.

# Time in the language

## 12. An implied binomial tree

Until now, every pricer knew only how the Bitcoin price is distributed on one date. That's enough for anything settled by the price at the close, which covers all of Kalshi's BTC contracts. It isn't enough for anything that depends on the path: an `Anytime` right, or "will BTC touch $90,000 before 5pm?". The language could already describe those contracts; it just couldn't price them, and the pricers said so with `Path_dependent`.

`Lattice` adds time. It's a recombining binomial tree: from each node the price moves up or down, and after n steps it has n + 1 possible values. The hard part is making the tree agree with the options, which only tell us the final distribution. Rubinstein's 1994 construction does this:

1. **Place the final nodes** on a standard binomial spacing in log price, scaled to the distribution's own spread (taken from its quartiles). For a roughly normal distribution, this spacing makes up and down moves roughly equally likely. That means variance builds up evenly through time instead of all at once.
2. **Give each final node** the probability the smile puts near it, and the average price in that region. Using the average rather than the midpoint makes expected values exact. It's the same trick as `Density.lognormal`.
3. **Assume every path to the same final node is equally likely.** Then each path to node j has probability q_j / C(n, j). An earlier node's path probability is the sum of its two children's, so the up-probability falls out as the ratio of the two. Node prices are the average of their children's, which keeps the tree free of arbitrage.

Two implementation details carry over from earlier sections:
- **Logs:** C(800, 400) is about 10^239, so path probabilities are kept as logarithms and added with `log_add`.
- **Memoisation:** the evaluator works one step at a time, and an `At` or `Anytime` at step m is built from the same contract at step m + 1. Each pair of (contract, step) is computed once, so a whole tree costs O(steps²).

The new constructor is `Anytime` from section 1, finally priced. At a node, the holder takes the better of acquiring now and waiting one more step; at the deadline, acquiring is compulsory. American options and touch contracts need no new language features:

```ocaml
let one_touch_above ~expiry ~barrier =
  anytime expiry (cond (Ge (Spot, Const barrier)) (at expiry one) zero)
```

Read it as: you may lock in "$1 at expiry" at any moment the price is at or above the barrier; at expiry you must take whatever is there. Locking in at the first touch is obviously right, so this is exactly a one-touch, built from four combinators that existed before the tree did. That's the paper's argument for combinators, in one line.

**The tests check answers known in advance:**
- **Agreement with the grid pricer.** For European contracts, the tree must match the grid pricer on the tree's own final distribution, on random contracts.
- **Fairness.** Receiving Spot at any step must be worth today's spot, which makes the discounted price fair at every step.
- **An American right is worth at least its alternatives.** `anytime T c` must be worth at least the better of acquiring c at T and acquiring it now, on random payoffs.
- **Merton's theorem.** With no dividends, an American call is never worth exercising early, so it equals the European call (3072.54 both). A put can be: at a 4% rate the American put is worth 2999.88 against 2995.85. At a zero rate the two puts are equal.
- **A closed form.** For a lognormal price with no drift, the chance of ever reaching a barrier has a closed form. The tree checks the barrier only at its steps, so it touches a little less often. Broadie, Glasserman and Kou (1997) showed that checking at m steps is like checking continuously with the barrier moved out by exp(0.5826 σ √(T/m)). The tree matches that corrected formula to within 0.002 at 200 and 800 steps.

At 50 steps the answers wobble, as the digital column in that test shows. The barrier lands at a different place between nodes each time the spacing changes. This is the same sawtooth as section 5. For barriers it's known from Boyle and Lau (1994), and the cure is enough steps.

## 13. Touching versus finishing: real smiles, and a warning

`bin/touch.exe` prices both contracts at Kalshi's strikes from your latest smiles. For a strike above today's price, touching it before the close comes out about twice as likely as finishing above it. The reflection principle explains why: once the price has touched the strike, it's roughly as likely to end below as above, so P(touch) ≈ 2 × P(finish above).

The ratio isn't exactly 2. On smooth daily smiles it runs from about 1.8 in the tails to about 2.7 near the money. The equal-paths assumption is a modelling choice, and for a distribution with fatter tails and a sharper peak than a normal one, it implies paths that wander and come back. So a touch probability depends on the model in a way the closing price doesn't. Polymarket's "will BTC reach $X" markets are exactly these contracts. Fabi et al. found those prices deviate from the options more than closing-price markets do, which fits the idea that the path is the harder part to agree on.

**The warning.** In the latest snapshot, the hourly smile shows a strike where touching is 3.9 times as likely as finishing below. That's an artifact of the fit, not the market. Its SVI parameter s is 4×10⁻⁶, which makes the smile almost a sharp V at $81,249 and packs 39 percentage points of probability into the $100 band around it. 11 of 48 hourly smiles and 5 of 77 daily ones in this export have such a kink, and `Smile.kink` now finds them; `touch.exe` flags those events.

These kinks matter for the sealed comparison too. When it runs on November 30, kinked smiles should be reported on their own, or part of the answer will be measuring the fit rather than the market. `Report` already drops quotes whose probability moves when the finite-difference step shrinks (`kink_sensitive`), which catches the worst of them. The real fix belongs in Implied Odds: a floor on s in the fit. Make that change after the 30th, not before. Changing the fit now would change the inputs to a pre-registered test.

# Swapping the distribution

## 14. One interface, several distributions

Every pricer since section 9 sees the distribution through just two curves: P(K), the chance of finishing at or above K, and M(K), the partial mean. `Replication.Model.t` is exactly that pair. So anything that can supply the two curves can be swapped in, and the same contracts priced again. Which prices move tells you what they depend on.

`Model_choice` offers three:

- **The smile** as exported.
- **A lognormal** at the smile's at-the-money vol. It has the same centre and roughly the same width, but no skew and no fat tails. `Replication.Model.lognormal` writes it in closed form, P = N(d₂) and M = F·N(d₁), with no finite differences at all.
- **A scaled smile.** `Smile.scale_vol` multiplies every implied vol by a factor. Below 1 the distribution narrows, and the skew keeps its shape. Since total variance is linear in SVI's a and b, scaling both by the factor squared does it exactly.

The closed-form lognormal and a flat smile (SVI with b = 0) describe the same distribution by two independent routes, so they make a check. They differ by 5×10⁻⁷, and that difference isn't a bug. It's the export's own method. A central difference with step h is off by about h²/6 times the slope of the density. With h = F × 10⁻⁴ = $10, on a one-day distribution about $2,600 wide, that comes to a few 10⁻⁷. A step 10 times smaller gives 5×10⁻⁹, exactly the 100 times better that h² predicts. So two things are true at once. Matching p_opt to 10⁻⁹ requires using its step. And no single probability in the export is accurate to better than about 10⁻⁶.

Comparing the three on one smile shows what the skew does. It puts more probability far below the forward. Because the mean is pinned at the forward, the middle shifts up to compensate: "above $100,000" is 0.504 under the smile and 0.495 under the lognormal. A contract's sensitivity to the choice of model is a measure of how much its price depends on the smile's shape rather than its level.

## 15. Ranges, ladders and butterflies

Two new combinators make the textbook relationships between contracts concrete:

- **`digital_by_spread`** is a call spread scaled by 1/width. Its payoff ramps from 0 to 1 across the spread instead of jumping. As the width shrinks it becomes the digital, with an error proportional to the width squared. The $20-wide spread is worth a second look: it's exactly Implied Odds' central difference, ±$10 around the strike. So p_opt is, literally, the price of a narrow call spread. That's the Breeden–Litzenberger idea as a trade: a dealer hedging a Kalshi contract with options would hold this spread.
- **`density_butterfly`** is a butterfly scaled so its triangular payoff has area 1. Its price is a local average of the density, so the density itself is a tradeable contract. A narrow range divided by its width measures the same thing, and the tests check that both match a closed-form lognormal density.

`bin/structures.exe` prices a ladder of ranges around the forward, plus butterfly densities, for each event in your latest snapshot, under all three distributions. It reads only `surfaces.csv`, with no Kalshi quotes, so it is pricing, not the sealed comparison. Each ladder sums to $1 under every model, as it must.

Two things stand out on the latest snapshot:

1. **The flagged hourly smile.** Its kink at $81,249 shows up as a $100 range worth 0.39, against 0.26 under the lognormal. That's the spike from section 13, visible as a price.
2. **The daily smile.** It has a sharper peak and fatter tails than the lognormal. The middle $500 range is worth 0.46 against 0.34, and "$83,000 or above" is worth 0.016 against 0.002. That's the shape the lognormal throws away.

# Testing what you can't yet look at

## 16. A synthetic bench for the sealed analysis

The gap analysis can't run on real quotes until November 30. It still has to be right on that day, so it is tested on markets where the answer is known in advance.

`Synthetic.pairs` builds 20 daily and 20 hourly snapshots. Each is a made-up smile, randomly perturbed in level, skew and forward. For each snapshot it generates "Kalshi" quotes from a chosen truth: at each strike, the truth's probability, less and plus a spread, then rounded out to whole cents the way Kalshi's tick forces. These are the export's own types, so `Report` runs on them unchanged, through the same code `analyze.exe` will use.

Each test plants something and checks that it comes back out:

| Planted | Recovered |
| --- | --- |
| Nothing (truth = the model) | Width ratio 1.000 hourly and 0.999 daily; gaps in each probability band within about a tenth of a cent; no quote beats spread plus fee |
| Vols ×1.3 | Width ratio 1.29 and 1.28, against an exact 1.28 for the base smile. The gaps show the wide-distribution signature: Kalshi above the model where the model says unlikely, below where it says likely |
| Forward +$300 | Median shift +298 and +301 |
| Vols ×1.6, half-cent spread | The edge detector fires on both sides |

Why ×1.3 comes back as 1.28 rather than 1.30: with a skew, scaling every vol by 1.3 widens the quartile range by a bit less than 1.3 (section 14). The bench compares against the exact figure, not the planted factor. This is the section 11 lesson again: feed a measurement a case where you know the answer.

**The null test is the important one.** A method that finds a gap when there is none would have produced a confident, wrong verdict on November 30. This one finds nothing when nothing is there, finds the right amount when something is planted, and does both through cent-rounded quotes. That's the evidence that its answer on the real window will mean something.

# Risk: what a market maker holds

## 17. Greeks by bump and reprice

A market maker who sells Kalshi contracts is exposed to the market moving. Three numbers describe that exposure. **Delta** is how much the contract's value changes per $1 move in BTC. **Gamma** is how fast delta itself changes. **Vega** is how much the value changes when the options market's vols rise. `Greeks.compute` gets all three without any new pricing code: it prices the contract under the smile as it is, then under the smile with the forward nudged up and down, then with every vol nudged up and down 1%, and takes differences. Because every contract in the language goes through the same pricer, this works for a digital, a range, a butterfly, or anything you compose.

The nudge has to fit the contract. A fixed $100 bump is a rounding error on a daily distribution $2,600 wide, but on the last hour's distribution, about $130 wide, it would smear the delta across most of the curve. So the default bump is a fiftieth of the distribution's own standard deviation. The tests check the result against Black's formulas for a lognormal, where every Greek has a closed form, and agree to about 10⁻⁴. That residual is the central difference's own error, (1/50)²/6.

Moving the forward raises a question the closed forms hide: **what happens to the smile when the price moves?** There are two textbook answers, and `Smile.with_forward` implements both:

- **Sticky moneyness.** The smile travels with the price, so the vol at 2% below the money stays the vol at 2% below the money. In SVI the parameters simply stay put.
- **Sticky strike.** The vol at each fixed strike stays the same, so as the price moves it slides along the smile. In SVI that's a shift of m by −ln(F′/F).

Under sticky moneyness, a digital's delta has a neat meaning. P(S ≥ K) then depends only on K/F, and differentiating gives delta = (K/F) × density(K). **A Kalshi contract's delta is the height of the distribution at its strike.** The test confirms this against a butterfly, which prices the density by a completely different route. Under sticky strike, the delta picks up an extra term, vega times the smile's slope at the strike. On the synthetic skew that moves the delta by 4 to 9%.

On real smiles the difference is bigger. On the latest daily smile, "above 80,000" has a delta of 0.28 cents per $100 under one rule and 0.88 under the other, three times as much. A hedge sized on the wrong rule is off by that factor. Which rule BTC follows is an empirical question. With Implied Odds' snapshots you could answer it after the seal lifts, by watching how the fitted smile actually moves between snapshots.

## 18. Selling a contract and hedging it

`Hedge.simulate` plays a market maker who sells a contract, collects the tree price, and hedges by holding delta units of the forward. It walks random paths through the implied tree and records what's left at the close: premium, plus hedge gains, minus payout.

**Re-hedging at every step leaves exactly nothing, on every path.** That isn't luck. Each tree step has two outcomes, and two instruments, cash and the forward, can match any payoff over two outcomes. A binomial tree is a complete market, and this is the replication argument behind every price in it. The test shows a worst error of 7×10⁻¹⁴ over 4,000 paths.

Real hedgers adjust less often than the price moves, and then the two contracts behave differently:

- **The call behaves as the textbooks say.** Between re-hedges its value curves away from the straight-line hedge (that's gamma), and the error grows like the square root of the gap. Quadrupling the gap doubles it: 118, 245, 471 dollars.
- **The digital doesn't.** With only 4-step gaps it already has a 13-cent standard deviation, and at 64 steps it's only 23. Its risk isn't steady curvature. It's the jump at the strike in the last minutes, which even a short gap fails to catch, so a longer gap adds little.

The last test shows where that risk lives. Paths finishing within $500 of the strike carry four times the error of paths finishing $2,000 or more away. Traders call it **pin risk**.

This is the market maker's problem in one table. A Kalshi contract struck near the price, close to expiry, can't be hedged away. `bin/greeks.exe` shows the real version: selling the at-the-money contract and re-hedging every 4 steps leaves about 13 cents of standard deviation per contract, on a contract worth about 35–55 cents. Whatever spread a market maker quotes has to pay for that. That's where Avellaneda–Stoikov comes in next: quotes that widen with the risk you can't hedge, and lean against the inventory you already hold.

# Making a market

## 19. Quoting a Kalshi contract

`Market_maker.quotes` is Avellaneda and Stoikov's 2008 rule, adapted, as Feil and Nendel do, to a contract that pays $0 or $1. A market maker with fair value p who already holds q contracts does three things.

**It quotes around a reservation price, r = p − q·γ·p(1 − p), rather than around p.** Holding inventory is risky, so the market maker leans its quotes to get rid of it. Long 20 contracts, its quotes sit 5 cents lower than flat, which brings in buyers and discourages sellers. The p(1 − p) is the variance of a $0/$1 payout, the risk of carrying a position to settlement. It's largest at 50 cents and vanishes near 0 and 1. γ is how much the market maker minds that risk.

**It sets a half-spread of γ·p(1 − p)/2 + ln(1 + γ/k)/γ.** The second term is the heart of the model. Order flow thins out as quotes move away from fair value, like e^(−k·distance). A wider spread earns more per fill but gets fewer fills, and this term is where those balance. For small γ it's about 1/k: with k = 50, two cents.

**It adds α times the standard deviation of the fair value's next move.** Avellaneda–Stoikov assume nobody knows more than the market maker. Kalshi's BTC contracts are the opposite case: Deribit, Coinbase and Binance all move before Kalshi's book can, so some traders effectively see the next move first. A spread has to cover how far the fair value can jump before the quotes can follow. From section 18, that jump is largest for a contract struck near the price close to the close.

## 20. A market to test it in

`Market_maker.simulate` runs the market maker along random paths through the implied tree. Each step works like this:

1. The market maker hedges the inventory it holds, if hedging.
2. It posts quotes.
3. An informed trader may arrive. It sees the fair value some steps ahead, and buys if that future value is above the ask or sells if it's below the bid.
4. Uninformed traders may arrive, more often the closer the quote is to fair value.
5. Then the price moves.

A contract bought during a step rides that step's move unhedged, because a hedge can't be placed before the fill it's hedging.

**The test where the answer is known.** Fair value is a martingale, so against traders who know nothing, inventory gains and losses average zero, and the P&L should average exactly the edge captured at each fill. It does: the gap is −0.020 ± 0.024 unhedged and −0.002 ± 0.004 hedged. Every other result rests on this one.

**Informed traders take the difference.** At the moment of each informed fill the edge still looks positive, but the loss arrives with the next move. Their cost grows with how often they come: 0.42, 0.84, then 1.70 dollars a day as their arrival rate doubles from 0.1 to 0.2 to 0.4 a step. A market maker that judges itself by edge at the time of the fill will think it's doing better the more it's being picked off. That's why fills should be marked against where the price went afterwards, a measure called markout.

**Inventory risk has two cures.** With γ = 0 the inventory drifts like a random walk, and the P&L's standard deviation is 2.26 dollars a day. Leaning the quotes (γ = 0.2) cuts it to 0.26 at some cost in profit. Hedging in the forward cuts it to 0.25 while keeping nearly all the profit. Hedging is the better tool when it's available. Leaning is what you do when it isn't, or as well.

**Widening for jumps has limits.** The informed traders here arrive at a rate of 0.3 a step:

- **Seeing 1 step ahead:** widening by half a standard deviation is the best α tried, 0.54 against 0.32 with no widening. It turns most of them away and keeps half the uninformed trade.
- **Seeing 4 steps ahead:** widening helps but never gets the market maker to a profit.
- **Seeing 10 steps ahead:** every spread loses. Widening drives away the uninformed faster than the informed, whose information is worth more than any spread tried, and not quoting at all beats every row. That's Glosten and Milgrom's 1985 result: with enough informed flow, the market breaks down.

The table also hides a lesson about α itself. It's scaled to the one-step move, but someone who sees h steps ahead knows a move about √h times larger. So the right α depends on who you think is on the other side.

`bin/market_make.exe` runs the same experiment on the latest real smiles, with simulated flow. The numbers come out close to the synthetic ones, because a 200-step tree puts every contract on the same footing in steps. What differs between the hourly and daily contract is how many real minutes a step is: 0.2 against 5.3.

**What's next.** After Nov 30, replay the quoting rule against Kalshi's real book instead of simulated traders. Mark each fill's profit against where the fair value went next, to measure how informed Kalshi's flow actually is. That one number, the cost of informed flow per fill, decides whether quoting these contracts can pay at all.

# Your turn

## 21. Exercise: a contract simplifier

This one is yours to write. The spec is in `lib/simplify.mli`, the stub in `lib/simplify.ml`, and the checks run with:

```bash
dune exec exercises/simplify/check.exe
```

It prints 17 examples, PASS or FAIL with what was expected, then four properties on 500 random contracts. Right now 14 checks fail. It's done when it says "All passing." It sits outside `dune runtest`, so the main test suite stays green while you work.

**What you're building.** A function that rewrites a contract into a smaller one with the same meaning, using laws like "giving away a contract you were given is nothing at all" (`give (give c) = c`). It's the fifth interpreter over the contract tree, and the first that returns a contract rather than a value. Compilers do the same thing to programs, and Jane Street's OCaml work is full of it.

**Why random testing is the right tool.** Each rule is a claim that two contracts are worth the same. The soundness property checks that claim on hundreds of random contracts by pricing both. When a rule is wrong, Quickcheck doesn't just say so: it shrinks the failing contract to the smallest one that still fails and prints it. That's the debugging loop to get used to.

**The structure that works.** Simplify the children first, then apply the rules to the node. Be careful when a rule's result can match another rule. `scale_k 2. (scale_k 0.5 c)` becomes `scale_k 1. c` under R7, and that still needs R5. A simplifier that applies one rule and stops fails the idempotence check, which says simplifying twice must give the same as simplifying once.

Why does applying rules until none match always stop? Every rule makes the contract strictly smaller, and a size can't shrink forever. That one sentence is the whole termination argument, and it's why there's a `never bigger` check.

**The traps.** Two examples look simplifiable and aren't:

- `or_ c zero` is an option on c. It's worth max(c, 0), not c.
- `scale (Const 0.) c` always prices at 0, yet it isn't a rule. If c can't be priced, say it names a date the model doesn't cover, then the contract shouldn't price either, and deleting c would hide the problem.

That gives the asymmetry in the last question of the spec. A simplifier may make an unpriceable contract priceable (R9: `at t zero` is nothing, whatever t is). It must never do the opposite, and it must never change a price.

When it passes, send me the file and I'll review it the way a code reviewer would: correctness first, then whether the code says what it means.

# Measuring Kalshi's real order flow

## 22. Markouts, and why the hourly export couldn't answer the question

Section 20 ended on one number: how informed Kalshi's takers are. The hourly export can't measure it. 44 of its 49 events appear in a single snapshot, and a market maker's question is about the minutes after each trade. So this step adds a high-frequency collector to Implied Odds (`collect/kalshi_hf.py`). It records:

- the Deribit index every 5 seconds,
- the top of book of every near-the-money KXBTCD contract, each time it changes,
- every trade print within 2% of the money.

`tools/export_hf_for_ocaml.py` turns that into four CSVs, and `Hf` loads them here.

**The markout** (`Markout`) is the market maker's report card. Every trade has a taker, who crossed the spread, and a passive side, whose resting quote was hit. The passive side's markout at horizon h is its profit on the trade, marked against fair value h later: P − V(t + h) if the taker bought YES at P, and the reverse if the taker sold.

At h = 0 the markout is just the half-spread the taker paid. If takers know nothing, fair value is a martingale, so it stays there at every horizon. If takers see where BTC is going, it falls with h, and once their information is worth more than the spread it goes negative. You don't need to know who anyone is, only where the price went next.

**Fair value at any moment** comes from the hourly smile for that event, carried forward between snapshots in two steps:

1. **Move it with the index.** This is sticky moneyness, `Smile.with_forward` from section 17.
2. **Shorten it to the time left at the same vols.** That's the new `Smile.with_years`.

Then it's priced through the contract language like everything else.

The tests caught one thing worth knowing. Near the close, the export's finite-difference step ($10 either side) is too coarse: a minute before settlement the whole distribution is only a few tens of dollars wide, and fair value came out up to 0.1 cents off. Markout now scales the step to a hundredth of the distribution's width, and the gap from Black's formula is 4×10⁻⁶ at every time to close.

**The test with a known answer** is a synthetic 12-hour market with 8 events and takers paying a 2-cent half-spread:

- **Uninformed takers:** the markout is +2.00, +2.02, +1.94 and +2.04 cents at 0, 10, 60 and 300 seconds, flat within its standard error.
- **Nine in ten takers seeing a minute ahead:** +2.00 at the trade, +1.55 at 10 seconds, then −0.97 at a minute with the 95% interval below zero. The method finds the informed flow and says how much it costs.
- **At the close it's too noisy.** With only eight events (clusters), the interval at the close misses the truth. That's why the draft pre-registration decides at 60 seconds and asks for 20 days of data.

**The replay** (`Replay`) runs the section 19 quoting rule against the real book and real trades. It assumes that whenever a trade prints at or through your quote, you were filled. That's optimistic, because it assumes you were first in the queue, so treat its P&L as a ceiling and its markouts as the honest part. On the synthetic market it makes $23 against uninformed takers and loses $28 against informed ones.

**The seal, enforced in code.** Comparing Kalshi trades with option fair values is exactly what layer 5 will do first, for anything up to 2026-11-27. `Seal` holds the sealed windows, and `bin/markout.exe` refuses with exit status 3 if its data falls inside a window that hasn't opened. The tests check this both ways, and the tool was run once on dates moved into the window to confirm it refuses. `bin/check_hf.exe` only loads the files and prints counts and time ranges, so it's safe to run on anything.

**What to do with it:**

1. Run the collector on the Mac: `cd ~/CODE/implied-odds/collect && caffeinate -i python3 kalshi_hf.py`.
2. Read the draft pre-registration, `research/PREREGISTRATION_LAYER16_DRAFT.md`, change what you disagree with, and register it before Nov 28.
3. On Nov 30, the October–November data becomes development data for the method.
4. On Dec 28, run the registered test once.

**Status.** Implemented in `lib/simplify.ml`, with the termination argument and answers to the three questions as comments. All 17 examples and 4 properties pass. On 500 random contracts, 315 got smaller, and the total size went from 6,221 constructors to 4,740. The subtle case is R7: merging two constant scales can produce `scale_k 1. c`, so its result goes back through the rules. Every other rule's result is already in final form.

**Revised the same day: backfill, not a machine left running.** The first day of live collection lost most of an hour to the Mac sleeping. The fix wasn't a server. Neither the 60-second markout nor the verdict needs anything recorded live:

- Kalshi keeps every market's full trade history after settlement.
- Kraken serves every BTC trade, which gives a price each second.
- The hourly smiles already come from the cloud workflow.

So Implied Odds' `collect/kalshi_trades_backfill.py` fetches the trades and prices weekly, after the fact, with no gaps. `tools/export_hf_for_ocaml.py --source backfill` exports them for the same OCaml tools. Only the quote replay still needs live quotes, and it's descriptive.

Two details in `Markout.Fair` make a different price source safe, and a test checks each:

- **The level cancels.** Fair value moves each smile by the price's own change since the snapshot, using the same series at both ends. So a level difference between Kraken and Deribit's index (0.03%, about $30, in the test) cancels exactly. Using one source at the snapshot and another at the trade would have shifted last-hour digitals by cents.
- **Resolution matters.** Prices sampled once a minute leave fair value up to a minute stale, which is about a cent of error on average and up to 19 cents near the close. That's why the backfill takes Kraken's trades, which give a price every second, and not Coinbase's 1-minute candles.
