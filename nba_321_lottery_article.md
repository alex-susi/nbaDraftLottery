# Putting a Posterior on the NBA's New Lottery: Valuing Every Draft Pick Under the 3-2-1 Rules

*A Bayesian Monte Carlo engine for pricing NBA draft assets after the biggest lottery overhaul in a generation — built in R, Stan, and a Shiny dashboard you can poke at.*

---

## The thing that kicked this off

On May 28, 2026, the NBA's Board of Governors voted 29–1 to blow up the draft lottery. Memphis was the lone "no." Starting with the 2027 draft, the league is running what everyone is calling the **"3-2-1" lottery**, and it's a genuinely different animal from the system we've had since the last big reform in 2019.

Here's the short version of what changed, because the rest of this post only makes sense if you know the new rules:

- **The lottery grew from 14 teams to 16.** All 16 slots are now drawn from the machine, not just the top 4.
- **There are 37 lottery balls, handed out in tiers.** The seven teams with the 4th-through-10th-worst records get **3 balls each**. The two 9/10 play-in seeds in each conference and — here's the spicy part — the **three worst teams in the entire league get only 2 balls each**. The two 7-vs-8 play-in losers get **1 ball each**.
- **Yes, you read that right: the three worst teams get *fewer* balls than the teams just ahead of them.** The league is openly trying to kill tanking by making it actively dumb to finish dead last. They softened the blow with a floor — those three "relegated" teams can't fall below pick 12 — but their *expected* outcome is worse than the teams in the 4–10 range.
- **A pile of new anti-tank restrictions.** No team can land the #1 pick in back-to-back years, or a top-5 pick three years running (this looks back to 2025). Newly traded picks can no longer be protected in the 12–15 band. And the second round now *inverts* the first-round lottery order, so the worst lottery team picks 31st.

If you want the official odds, the league published them: the 4–10 group sits at 8.1% for the #1 pick and 73% for a top-10 pick, while the three worst teams get 5.4% at #1 and 61% top-10. The play-in losers are way down at 2.7% / 35%.

The natural question for anyone who cares about roster building is: **what is each pick actually *worth* now, and who won and lost in the reshuffle?**

That's the project. I built an end-to-end Bayesian simulation that re-prices every NBA draft asset — every team's own picks *and* the dense web of traded, protected, and swapped picks out to 2032 — under both the old system and the new 3-2-1 system, then reports the difference with full uncertainty bands. There's a Shiny dashboard on top of it with a working trade machine.

Let me walk through it. I've tried to write this so a casual fan can follow the story, a stats-literate fan can see *why* each piece works, and a fellow data scientist can check my math (and tell me where I'm wrong).

> **Quick note on prior work.** I'm standing on a lot of shoulders here. The draft-slot-value-curve idea goes back to [Aaron Barzilai at 82games](https://www.82games.com/barzilai1.htm), [Kevin Pelton's WARP-based trade value charts at ESPN](https://www.espn.com/nba/draft2015/insider/story/_/id/13143349), and Jacob Goldstein's PIPM work. The 3-2-1-specific re-pricing was kicked off by a great cluster of Substack posts from **Nick Thoreson** and **Luke McCartney** in the weeks right after the vote — Thoreson's headline that the new #1 lottery seed is worth about what the old #9 seed was worth is the kind of finding I wanted to stress-test. And the gold-standard rigorous template is the 2019 MIT Sloan paper **"Valuing Protections on NBA Draft Picks"** by Foster & Binns, which treats protected picks like financial options. My goal was to take those (mostly deterministic, point-estimate) approaches and put a real posterior distribution on everything.

---

## Part 1: What does a pick produce? (The data)

Before you can say a pick is worth something, you have to define "worth." I went with the most boring, defensible thing I could: **a player's total Win Shares over their first four NBA seasons** — the rookie-scale contract window. That's the cost-controlled period a front office actually cares about when it acquires a pick.

The data pipeline:

- **Player production:** Basketball-Reference's season-level "Advanced" export. For every drafted player, I collapse their first four seasons into a single first-4-year Win Shares total (`ws4`), de-duplicating the multi-team "2TM/3TM" rows so a player who got traded mid-season isn't double-counted.
- **Draft slots:** scraped the draft-slot → player mapping from Basketball-Reference draft pages (joining on the BBRef player ID, not the name, to avoid the "two guys named Marcus Williams" problem). I use draft classes **1985–2021**, so every player in the sample has had at least four seasons to accumulate.
- **Standings:** scraped 2005–2026 league standings (with a hoopR → BBRef fallback chain) to build the team-movement model in Part 3.
- **Pick ownership:** the actual, locked 2026 post-lottery draft order plus a hand-encoded map of every traded/protected/swapped future pick from 2027–2032, compiled from RealGM and prosportstransactions.

A couple of honest notes on the production metric. Win Shares is a *cumulative* stat, so it rewards both quality and availability — a star who plays 70 games a year racks up more than an equally-good player who's hurt. I think that's fine, even desirable, for valuing the asset. But WS is also a flawed, era-sensitive box-score metric, and EPM or BPM-derived numbers (which McCartney and Thoreson use) are arguably better at isolating impact. I'd call this a reasonable choice, not the only choice.

---

## Part 2: Pricing the picks themselves (the pick-value models)

This is the part where "a pick is worth a number" becomes "a pick is a *distribution* of outcomes, and here's its shape."

The hard truth about draft picks is that they're wildly noisy. The #1 pick has produced both Anthony Davis and Anthony Bennett. So a point estimate of "pick 5 is worth X Win Shares" hides the entire interesting story. I model the full outcome distribution at every slot, and — crucially — I let **neighboring slots borrow strength from each other**, because there's no real basketball reason that pick 14 should be dramatically more volatile than pick 13 or 15. The raw per-slot samples *look* like that, but it's noise.

I split this into two models, because the first round and second round are fundamentally different objects.

### 2a. The first round: a Student-t curve with smoothed variance

For picks 1–30, every drafted player is one row, and I model their first-4-year Win Shares as a Student-t draw around a declining mean curve:

$$
\text{ws4}_n \sim \text{Student-}t\!\left(\nu,\; \mu_{p[n]},\; \sigma_{p[n]}\right)
$$

where $p[n]$ is the draft slot of player $n$.

**The mean curve** is a simple power-law decay with a floor:

$$
\mu_p = \frac{\alpha}{p^{\beta}} + \gamma
$$

Intuitively: $\alpha$ sets how high the curve starts, $\beta$ controls how fast value falls off as the pick number climbs, and $\gamma$ is the late-first-round baseline the curve flattens toward. This captures the shape every prior author has found — a steep cliff over picks 1–5, then a long flattening tail.

**Why Student-t instead of Normal?** Because of exactly the Davis/Bennett problem. Draft outcomes have fat tails: occasional superstars, occasional total busts. The degrees-of-freedom parameter $\nu$ lets the data decide how heavy those tails are, and constraining $\nu > 2$ keeps the variance finite. A Normal likelihood would get yanked around by every outlier star and systematically misjudge the risk.

**The genuinely nice part — the variance model.** Rather than assume one global noise level, or fit 30 independent and noisy per-slot variances, I let each slot's (log) residual scale follow a **random walk across adjacent picks**:

$$
\log \sigma_1 = \log\sigma_{(1)}, \qquad
\log \sigma_p = \log \sigma_{p-1} + \tau \, z_p, \quad z_p \sim \mathcal{N}(0,1)
$$

The smoothing parameter $\tau$ (itself given a tight half-normal prior) forces volatility to evolve *smoothly* from pick to pick. Small $\tau$ → neighboring slots share almost the same risk profile; large $\tau$ → the data is allowed to insist on local jumps. This is a 1-D Gaussian-random-walk smoother baked directly into the generative model, and it's far more honest than the ad-hoc neighbor-averaging you'll see in most public pick curves.

The priors are weakly informative and live on log scales so the positive parameters stay positive:

$$
\log\alpha \sim \mathcal{N}(\log 20,\, 0.6), \quad
\log\beta \sim \mathcal{N}(\log 0.55,\, 0.5), \quad
\gamma \sim \mathcal{N}(2, 3)
$$
$$
\log\sigma_{(1)} \sim \mathcal{N}(\log 8,\, 0.5), \quad
\tau \sim \text{Half-}\mathcal{N}(0, 0.15), \quad
\nu - 2 \sim \text{Exponential}(0.20)
$$

That last one centers $\nu$ around 7 — moderately fat tails — while leaving room for the data to pull it toward near-Normal or toward something much heavier.

### 2b. The second round: a right-skewed hurdle model

Second-round picks break the first-round model completely, for one reason: **most of them never produce anything.** A huge share of picks 31–60 play minimal or zero NBA minutes. A symmetric curve is the wrong tool — you can't have a Student-t centered at "slightly positive" when the modal outcome is *literally zero career value*.

So the second round gets a **hurdle (two-part) model**. First, did the player even stick?

$$
\text{played}_n \sim \text{Bernoulli}\big(\pi_{p[n]}\big)
$$

Then, *conditional on playing*, their value is a shifted, right-skewed lognormal mixture:

$$
\text{ws4}_n - \text{floor} \;\sim\;
\begin{cases}
\text{LogNormal}\big(m_p + \delta,\; \kappa\,s_p\big) & \text{with prob } u_p \;\;(\text{rare upside})\\[4pt]
\text{LogNormal}\big(m_p,\; s_p\big) & \text{with prob } 1 - u_p \;\;(\text{typical})
\end{cases}
$$

A few things are doing real work here:

- **The shift/floor.** I model $\text{ws4} - \text{floor}$ on a lognormal, where `floor` is a small negative constant (a few WS below the worst observed played-player outcome). This guarantees positivity for the lognormal while still allowing the mildly-negative Win Share totals that bench players genuinely post. It also kills the unrealistic symmetric *negative* tail a Student-t would invent.
- **The rare-upside mixture.** Second-round picks occasionally produce a Jokić, a Draymond, a Ginóbili. If I let one fat lognormal absorb those, every slot's tail gets contaminated and the model "expects" a superstar from pick 55. Instead there's a separate low-probability upside component, with a strongly regularized mixture weight $u_p$ that is **forced to decline across the round** ($\delta_{\text{logit }u} < 0$). Pick 31 gets meaningful star equity; pick 60 gets almost none. The component's scale multiplier $\kappa$ is capped (between 1 and 2.25) so the right tail can't explode.
- **Adjacent-pick borrowing again.** All three pick-specific curves — the play probability $\pi_p$ on the logit scale, the typical median $m_p$ on the log scale, and the lognormal scale $s_p$ — evolve via the same random-walk-across-slots smoothing as the first round:

$$
\text{logit}\,\pi_p = \text{logit}\,\pi_{p-1} + \delta_\pi + \tau_\pi z_p, \qquad
\log m_p = \log m_{p-1} + \delta_m + \tau_m z_p
$$

The expected asset value at each second-round slot then falls out analytically as the hurdle product:

$$
\text{EV}_p = \pi_p \cdot \mathbb{E}[\text{ws4} \mid \text{played}, p]
$$

with the conditional mean computed from the lognormal mixture moments. The model spits out one posterior-predictive outcome draw per exact slot, which is what the Monte Carlo engine consumes downstream — preserving the structural zeros, the skew, and the rare upside, all at once.

> **For the casual reader, here's the whole of Part 2 in one breath:** I figured out the realistic range of careers that come from each draft slot — not just the average, but the full spread of booms and busts — treating the first round (where almost everyone plays) and the second round (where most picks flame out, but a rare one becomes a star) with two different statistical tools.

---

## Part 3: Where will teams *finish*? (The Markov chain)

A pick in the 2030 draft is only valuable insofar as you can guess how good its original team will be in 2030. Projecting a single team's record five years out is basically impossible — but projecting the *distribution* of where they'll land is very doable, and that's all the expected-value math needs.

I model team strength as a **first-order Markov chain over five states** — and the states are deliberately chosen to be exactly the 3-2-1 lottery tiers, because *that's what determines a team's lottery seeding*:

1. **Relegation** — the 3 worst records (2 balls)
2. **Non-play-in** — the next 7 teams (3 balls)
3. **Play-in seed** — the four 9/10 seeds (2 balls)
4. **Play-in loser** — the two 7-vs-8 losers (1 ball)
5. **Playoff** — the 14 playoff teams (0 balls)

Each row of the 5×5 transition matrix — the probabilities of moving from one tier to another next season — gets a **Dirichlet prior**, updated by the observed historical transition counts:

$$
\theta_{i\cdot} \sim \text{Dirichlet}(\alpha_{i\cdot}), \qquad
\mathbf{counts}_{i\cdot} \sim \text{Multinomial}(\theta_{i\cdot})
$$

The Dirichlet-Multinomial is conjugate, so the posterior is just $\text{Dirichlet}(\alpha + \text{counts})$ — I could write the answer in closed form (and the code does, as a cross-check). I still fit it in Stan so I get MCMC diagnostics and clean posterior draws for the downstream simulation in one consistent toolchain.

**The prior encodes a basketball fact: tier movement is sticky.** I build the concentration matrix $\alpha$ so that staying put or moving one tier is a priori much likelier than leaping from relegation to the playoffs in a single year:

$$
\alpha_{ij} =
\begin{cases}
3.0 & \text{if } i = j \;\;(\text{stay})\\
1.5 & \text{if } |i-j| = 1 \;\;(\text{adjacent})\\
\max\!\big(0.4,\; 1.5 \cdot 0.6^{\,|i-j|-1}\big) & \text{otherwise}
\end{cases}
$$

This regularization matters: relegation → playoff in one season is rare in the data, but it shouldn't be assigned probability *exactly zero*, or the model becomes overconfident about the bad teams staying bad. The prior gives those big jumps a small but nonzero floor.

To project forward, I seed each team in its actual 2025–26 tier and evolve the chain year by year. As a sanity check, the code computes the chain's **stationary distribution** (the long-run tier occupancy) and its **mixing time** from the second eigenvalue $\lambda_2$ — i.e., roughly how many years it takes for a team's current standing to "wash out" toward the league baseline. Spoiler from the literature and from my own runs: it washes out *fast*. By four or five years ahead, almost every team's projected pick is drifting toward the middle of the draft. McCartney found the same thing and called it either "my model sucks" or "it's genuinely hard to predict five years out." It's the second one. That uncertainty is real, and the value of far-future picks should reflect it.

---

## Part 4: Running the lotteries (the simulation engine)

Now we tie it together. The core is a Monte Carlo loop (10,000 iterations) where each pass plays out the full 2026–2032 draft future **twice** — once under the old rules, once under 3-2-1 — using the *same* underlying randomness so the comparison is apples-to-apples.

Within each simulation:

1. **Draw one posterior sample** of the Markov matrix and one of the pick-value curve parameters. This is what propagates model uncertainty all the way through to the final answer — every sim is a slightly different plausible universe.
2. **Lock 2026.** The Wizards won the actual 2026 lottery; that's history. So 2026 pick *slots* are fixed to reality, and only the pick *value* carries uncertainty. Years 2027–2032 are fully projected.
3. **Evolve every team's tier** one year via the Markov draw, then order teams worst-to-best to seed the lottery.
4. **Run both lotteries.** The old one is the familiar 14-team, top-4-drawn weighted system. The new one implements the full 3-2-1 structure: 16 seeds, the 2/3/2/1 ball tiering, all 16 slots drawn, and — the fiddly bit — the **relegation floor enforced *during* the draw**, not patched afterward. If the number of undrawn relegated teams ever equals the number of slots left before pick 12, the next pick *has* to come from that relegated group, so all three are guaranteed to be seated by pick 12. Everyone else shifts down naturally.
5. **Apply the new anti-tank restrictions** to the 3-2-1 order: the no-#1-in-consecutive-years and no-top-5-in-three-years rules, tracked per original team against a rolling history that's seeded with the real 2025 and 2026 results. When a restriction binds, that team is bumped to its first legal slot and everyone between shifts up one — preserving a valid one-team-per-slot order.
6. **Resolve the entire ownership web.** This is the unglamorous heart of the thing. The lottery assigns a slot to each *original* team; a separate layer then figures out who actually *receives* that pick after every outright trade, protection, swap, and multi-team ranked-pool obligation. Protected picks that don't convey correctly fall back to the original owner; swaps hand the better slot to the holder; nested ranked pools (the genuinely nightmarish ones — looking at you, every Nets-adjacent obligation) get resolved in slot order. The new 12–15 protection ban is enforced here too.
7. **Value every resulting pick** by sampling from the appropriate posterior-predictive curve from Part 2, and accumulate it to whichever team ends up owning it.

The second round gets its own ordering logic, including the new 3-2-1 inversion rule (slots 31–46 are the reverse of the final first-round lottery order). The engine also validates itself before exporting — checking that round-1 picks land in slots 1–30, round-2 in 31–60, and that the inversion identity $\text{slot}_2 = 47 - \text{slot}_1$ holds for every lottery team in every sim.

I track two flavors of value for every asset:
- **Sampled outcome value** — draw an actual player career from the slot's predictive distribution. This carries the full boom/bust risk and is what you want for "what might this pick *become*."
- **Expected asset value (EAV)** — value the slot at the posterior *mean* curve. Smoother, and the right lens for "what is this pick worth *on average*" trade comparisons.

And because each sim records every team's realized slot and the curve parameters it used, the dashboard's trade machine can retroactively apply a *hypothetical* protection or swap to any pick — "what's this worth if I protect it top-8?" — and recompute conveyance with the correct within-simulation correlation. That's a detail most pick-value tools get wrong by treating picks as independent.

---

## Part 5: Why these models, and where they'll bite you

Quick, honest accounting before the results.

**Why these modeling choices are appropriate:**

- **Bayesian throughout** because the entire problem *is* uncertainty propagation. A draft pick's value is a question about an unknown future career of an unknown future player on an unknown future team. Point estimates throw away the most important part. Carrying full posteriors means the final "this pick gained X value" comes with a credible interval, not false precision.
- **Student-t and lognormal-hurdle likelihoods** because they match the actual shape of draft outcomes — fat tails up top, a wall of zeros in the second round — instead of forcing a Normal onto data that is emphatically not Normal.
- **Adjacent-pick random-walk smoothing** because it's a principled way to share information between neighboring slots, replacing the hand-tuned neighbor-averaging in most public curves with something the data can actually push back on.
- **A tier-based Markov chain** because the 3-2-1 tiers *are* the lottery's seeding mechanism, so modeling movement between them maps directly onto how picks get seeded — no translation layer needed.

**Assumptions I'm making (and you should know about):**

- First-4-year Win Shares is a good enough proxy for "the value of a pick." Defensible, debatable.
- Team strength is first-order Markov — next year depends on this year, but not explicitly on the trajectory that got you here. Real front offices have multi-year arcs (aging cores, cap cliffs) this doesn't capture.
- Within-tier seeding order is treated as a random draw (a record proxy), since the tier defines the ball count anyway.
- The historical transition and production patterns from 2005–2021 still apply going forward — no structural break from new CBA aprons, load management, or the lottery change itself altering tanking behavior.

**Limitations I want to be loud about:**

- **Far-future picks are mostly noise, by construction.** The Markov chain washes out to the league baseline within a few years. That's *correct* — it's genuinely hard to predict — but it means a 2032 pick's valuation is more a statement about base rates than about that specific team. Read those with humility.
- **No time discounting (yet).** A 2031 pick and a 2027 pick of the same expected value are treated as equal. In reality teams prefer value sooner. McCartney pegs the pure time-preference rate somewhere in the 3–6% range and admits it's a guess; I'd rather report undiscounted value than bake in a number I can't defend. Easy to layer on.
- **The ranked-pool obligations are approximations.** Some of the deeply nested multi-team swap chains (again: the Nets) are encoded as close-but-not-exact resolutions. For valuation purposes the error is small, but it's not a literal reading of every contract clause.
- **No surplus value.** I'm valuing on-court production, not production *net of rookie-scale salary*. The Sloan/Pelton lineage does the latter, and it's the natural next step (more below).
- **Win Shares, era effects, injury luck** — all the usual single-metric caveats apply.

---

## Part 6: Results

> *This section is where the charts and tables from the Shiny dashboard go. Below I've laid out the figures with placeholders and described what each one shows and the takeaway it supports. Swap in the exported plots/screens.*

### 6.1 — The model recovers the official odds

Before trusting anything downstream, the lottery engine has to reproduce the league's published 3-2-1 odds. It does.

> **[TABLE 1: Simulated vs. official 3-2-1 odds by tier]**
> *Columns: tier, simulated P(#1), official P(#1), simulated top-3 / top-5 / top-10, official top-3 / top-5 / top-10, simulated avg pick, official avg pick. Rows: Three worst / 4th–10th / 9-10 play-in seeds / 7v8 play-in losers. Takeaway: simulated values land within Monte Carlo error of the official 8.1% / 5.4% / 2.7% numbers, validating the draw mechanics including the relegation floor.*

### 6.2 — The pick-value curves

> **[FIGURE 1: First-round pick-value curve, picks 1–30]**
> *The posterior mean first-4-year Win Shares by slot, with a credible-interval ribbon for the curve estimate and a separate wider band for the player-outcome spread. Empirical slot means overlaid as dots. Takeaway: the steep 1–5 cliff and the long flattening tail, now with honest, smoothly-varying uncertainty.*

> **[FIGURE 2: Second-round pick-value curve, picks 31–60]**
> *Expected asset value by slot (the hurdle product $\pi_p \cdot \mathbb{E}[\text{ws4}\mid\text{play}]$), plus the play-probability curve $\pi_p$ on a secondary axis, plus the declining rare-upside probability $u_p$. Takeaway: how fast both the chance of sticking and the chance of a star outcome decay across the round.*

### 6.3 — The headline: who won and lost under 3-2-1

This is the money chart. For every team, the change in expected draft-asset value (summed over all their owned picks, 2026–2032) when you switch from the old system to 3-2-1.

> **[FIGURE 3: Impact chart — Δ expected asset value by team]**
> *Horizontal bar chart, teams sorted by delta, diverging colors for winners vs. losers, with credible-interval whiskers on each bar. Takeaway: which franchises' pick portfolios appreciated or depreciated under the reform, and whether the change is distinguishable from zero given the uncertainty.*

The structural story I expect this to tell — and which lines up with the Thoreson/McCartney findings — is that **value shifts away from the very top and toward the 7–16 range.** Flattening the odds means the worst teams capture less of the top-end value, while the middle-lottery and back-of-lottery seeds become meaningfully more valuable. The three relegated teams are an interesting special case: despite having the same 2 balls as the 9/10 seeds, their top-12 floor preserves a small edge in expectation.

> **[FIGURE 4: Lottery seed value/odds — current vs. 3-2-1]**
> *Expected pick and P(#1) by lottery seed under both systems, overlaid. Takeaway: visualizes the "race to the middle" — the flattening that makes finishing dead last no longer the dominant strategy, and the kink at the relegation floor.*

### 6.4 — Full team table and individual assets

> **[TABLE 2: Full 30-team value table]**
> *Per team: current expected value, 3-2-1 expected value, delta, delta %, and 90% credible intervals for each. Takeaway: the complete, sortable ledger behind Figure 3.*

> **[FIGURE 5: Markov transition heatmap + state diagram]**
> *The posterior-mean 5×5 tier transition matrix as a heatmap, with the tier-to-tier flow diagram. Takeaway: tier stickiness is visible on the diagonal; the off-diagonal decay shows how quickly teams regress toward the middle.*

### 6.5 — The dashboard

Everything above lives in an interactive Shiny app with seven tabs: **Impact** (the delta chart), **Side by Side** (value/quality/quantity comparisons), **Lottery Odds**, **Full Table**, **Markov + Curve** (the model internals and validation), **Single Pick** (drill into any one asset's distribution), and a **Trade Machine** that values arbitrary pick packages — including hypothetical protections and swaps applied on the fly — under either system.

> **[FIGURE 6: Dashboard screenshot — Trade Machine]**
> *Screenshot of the trade machine valuing a multi-pick package. Takeaway: the practical front-office tool — drop in the assets each side is sending and read the expected-value differential with uncertainty.*

---

## Part 7: Where this goes next

A few directions I'm actively thinking about, roughly in priority order:

1. **Rebuild the Sloan 2019 protection-pricing model under 3-2-1.** The option-pricing / Wang-transform framework is the natural rigorous home for protected-pick valuation, and the new top-12 floor plus the 12–15 protection ban materially change those option payoffs. This is the most interesting open lane.
2. **Net out rookie-scale salary to get *surplus* value.** None of the public 3-2-1 work does this yet. Layering CBA rookie-scale costs onto the value curve would show how the reform shifts *surplus* (not just raw production) toward the 7–16 range — which is what actually drives front-office decisions.
3. **Add a defensible time-discount layer.** Ideally estimated empirically from how freely teams trade picks at different horizons, rather than a guessed rate. Carefully, so I'm not double-counting the future-uncertainty that's already in the Markov projection.
4. **Hierarchical pooling across draft classes** for the value curve, so strong and weak draft years partially pool instead of being treated as exchangeable.
5. **Open-source the lottery engine.** There's still no public R/Stan implementation of the official 37-ball / 16-team / relegation / retroactive-constraint structure. Releasing a validated, configurable version (so any future ball reallocation is a config change, given the sunset clause means the odds could be tweaked before 2030) feels like a genuinely useful contribution.

---

If you've read this far: thanks. I built this partly to answer a real question about the new rules and partly as a portfolio piece while I look to move into a front-office analytics role, so I'd love feedback — especially the "here's where your model is wrong" kind. The pick-value likelihoods, the Markov smoothing, the obligation-resolution logic — all of it is fair game. Tear it apart.

*Built in R, with Stan (cmdstanr) for the Bayesian models, the tidyverse for wrangling, and Shiny for the dashboard.*
