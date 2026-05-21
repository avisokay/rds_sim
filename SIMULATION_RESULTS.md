# Simulation Results: RRDS Robustness Analysis

## Overview

This document reports results from the updated simulation study, revised in response to reviewer comments. The simulation tests respondent-driven sampling (RDS) and the proposed random referral data sampling (RRDS) method on a synthetic social network, comparing their ability to recover known population parameters from biased starting seeds. Three additional analyses address (1) real-world barriers to contact list enumeration, (2) parameter disambiguation and realistic network degree, and (3) the expected bias behavior of preferential recruitment.

---

## Simulation Design

### Network

A synthetic social network of **n ≈ 9,999 nodes** was generated with a **mean degree of 10**, reflecting realistic social network structure. Node attributes were drawn from population distributions:

- **Age**: Normal distribution, mean = 41.5 years, SD = 10, truncated [18, 65]
- **Gender**: 70% female, 30% male

### Two Homophily Parameters

Two conceptually distinct parameters govern the simulation (addressing Reviewer 2 and 4):

- **α_net = 0.40** — *Network structural homophily*: the probability that any given network edge connects two individuals of the same gender. This is a fixed property of the network that RRDS cannot address; it shapes whose contacts appear on anyone's contact list.

- **α_rec = 0.90** — *Preferential recruitment*: in the RDS algorithm, the probability that a participant actively recruits a same-gender contact from among their eligible neighbors, rather than selecting randomly. This behavioral bias is what RRDS is designed to eliminate by requiring random contact list sampling.

### Seeding and Stopping

All simulations were initialized with **exactly 10 biased seeds** — young men (age < 22, male). This creates a worst-case starting condition: seed mean age ≈ 20 years, 0% female. Chains recruit up to 3 contacts per participant and stop when the **cumulative sample reaches N = 3,000**, simulating a real study rather than running until network saturation.

### Scenarios

| Scenario | Description |
|---|---|
| RRDS Baseline | Random referral from full contact list |
| RDS (Preferential Recruitment) | Participants recruit most similar contacts (α_rec = 0.90) |
| RRDS: 20% Non-response | 20% of nominated contacts decline participation |
| RRDS: 40% Non-response | 40% of nominated contacts decline participation |
| RRDS: 70% Contact Enumeration | Participants list only 70% of their contacts (random subset) |
| RRDS: 50% Contact Enumeration | Participants list only 50% of their contacts (random subset) |
| RRDS: Same-group Listing Bias | Partial enumeration (70%) biased toward same-gender contacts |
| RRDS: High-degree Listing Bias | Partial enumeration (70%) biased toward higher-degree contacts |
| RRDS: 40% NR + 50% Enumeration | Combined worst-case: 40% non-response and 50% enumeration |

---

## Results

### 1. Baseline Comparison: RRDS vs. RDS

*(Figures: `main_age.pdf`, `main_female.pdf`, `main_by_n_age.pdf`, `main_by_n_female.pdf`)*

Starting from 10 all-male young seeds, the two methods diverge substantially in how quickly they recover population values.

**Mean age**: Both methods begin around age 20 and trend toward the population mean of 41.5 years. RRDS converges approximately 1–2 waves faster than RDS, reaching the population value before the chain hits N = 3,000. RDS with preferential recruitment lags because it repeatedly recruits within the same-age, same-gender cluster before branching into other subgroups.

**% Female**: This is where the methods diverge most clearly. Seeds begin at 0% female (population: 70%). RRDS recovers toward the population proportion substantially faster than RDS. Because RRDS randomly samples from the full contact list, it immediately recruits across gender lines at the rate dictated by the network structure. RDS with α_rec = 0.90 stays predominantly within male contacts for several additional waves, producing a persistent lag. At any given wave or cumulative sample size, RRDS is considerably closer to the true population proportion than RDS.

**Key finding**: By the time each chain reaches N = 3,000, RRDS estimates are substantially closer to population truth than RDS estimates for both age and gender proportion. The RDS chain remains meaningfully biased at the stopping criterion — this is the central result.

> **Note on degree (Reviewer 3)**: In the original simulation (mean degree 2, 3 recruits per person), participants could exhaust nearly all eligible neighbors regardless of recruitment preferences, leaving little room for preferential selection to accumulate bias. With mean degree 10, participants have genuine choice, and α_rec = 0.90 now produces the expected, theoretically predicted lag in convergence.

---

### 2. VH Estimator vs. Naive Sample Mean

*(Figure: `robustness_vh_estimator.pdf`)*

The Volz-Heckathorn (VH) inverse-degree-weighted estimator corrects for differential sampling probability by down-weighting participants with higher network degree (who are inherently more likely to be recruited).

**Findings:**

- In early waves, the VH estimator converges slightly faster toward the population mean than the naive sample mean for both RDS and RRDS. The gap is most visible in the first 2–3 waves.
- By wave 4–5, naive and VH estimates are nearly indistinguishable from one another and from the population mean.
- The RRDS + VH combination provides the fastest early-wave convergence among all four combinations.
- The modest difference between naive and VH in this network reflects moderate degree variation. In networks with heavier-tailed degree distributions (e.g., scale-free), the VH correction would be more consequential.

> **Implication for Reviewer 2**: Reporting VH estimates alongside naive means is recommended, particularly for early-wave and early-n results. The VH correction is especially important for RDS because preferential recruitment creates stronger correlation between degree and sampling probability.

---

### 3. Robustness to Real-World Barriers

*(Figures: `robustness_age.pdf`, `robustness_female.pdf`, `robustness_by_n_age.pdf`, `robustness_by_n_female.pdf`)*

#### 3a. Non-response

At **20% non-response**, both mean age and gender proportion convergence are slightly slower than the RRDS baseline, but the method still tracks clearly toward the population values within the target sample size. The effect is modest.

At **40% non-response**, the impact is more visible, particularly for gender proportion, where convergence is delayed by approximately 1–2 waves relative to baseline. Crucially, even at 40% non-response, RRDS remains substantially better than RDS at any given wave or sample size.

**Interpretation**: Non-response reduces the effective number of recruits per wave, slowing the chain's traversal of the network. Because RRDS recruits randomly from listed contacts, non-response does not introduce systematic bias — it only reduces efficiency. This contrasts with RDS, where non-response among dissimilar contacts would compound preferential recruitment bias.

#### 3b. Partial Contact Enumeration

At **70% enumeration**, estimates are nearly indistinguishable from the RRDS baseline across both outcomes. Listing 70% of contacts at random produces minimal deviation from full enumeration.

At **50% enumeration**, convergence is slightly slower, most visibly in % female during early waves. Despite this, the method trends toward population values and remains substantially better than RDS.

**Interpretation**: Partial enumeration functions similarly to non-response in its effect — it reduces the effective contact pool, slowing but not biasing the chain. The key assumption is that unenumerated contacts are a random subset of the true list. When this holds (scenarios above), the impact is modest.

#### 3c. Systematic Contact Listing Bias

**Same-group listing bias**: Participants list contacts who are 80% same-gender. This mimics natural recall bias — people tend to remember and name similar contacts more readily. Despite this systematic restriction, RRDS still converges toward the population proportion faster than RDS and reaches population values within the target sample.

**High-degree listing bias**: Participants disproportionately name higher-degree contacts (hubs), mirroring findings from real-world network research. The effect on population mean estimates is minimal — this scenario tracks nearly identically to the RRDS baseline.

#### 3d. Combined Worst-Case: 40% Non-response + 50% Enumeration

This scenario simultaneously applies the most severe non-response and enumeration challenges tested. Convergence is slower than any individual RRDS robustness scenario. Nevertheless, RRDS under worst-case conditions still outperforms RDS with preferential recruitment in recovery of both mean age and gender proportion. The combined scenario demonstrates that even when practical barriers substantially reduce effective chain capacity, random referral maintains a meaningful advantage over preferential recruitment.

---

## Summary Table of Implications

| Reviewer Concern | Simulation Finding |
|---|---|
| **R1: Non-response** | 40% non-response delays convergence by ~1–2 waves but does not introduce bias; RRDS still outperforms RDS |
| **R1: Biased contact listing** | Same-group listing bias delays convergence modestly; high-degree bias has negligible effect on means |
| **R1: Partial enumeration** | 50% enumeration delays convergence slightly; 70% enumeration is nearly equivalent to full enumeration; RRDS still outperforms RDS in all cases |
| **R1: Combined worst-case** | 40% NR + 50% enumeration is the most challenging scenario; RRDS still recovers better than RDS with preferential recruitment |
| **R2: Two homophily concepts** | α_net (structural) and α_rec (preferential recruitment) are now parameterized separately and independently |
| **R2: Low mean degree** | Degree increased from 2 to 10; RDS and RRDS now differ meaningfully in convergence behavior |
| **R2: VH estimator** | VH provides faster early convergence, especially for RDS; both estimators largely agree by wave 4–5 |
| **R3: RDS appeared unbiased** | With degree 10 and N_TARGET = 3,000, RDS preferential recruitment produces a persistent, clearly visible bias gap relative to RRDS |

---

## Limitations and Next Steps

- **Monte Carlo confidence intervals**: All current results are single-run simulations. The `RUN_MC <- TRUE` flag in the script will run 30 replications per scenario and produce ribbon plots with 95% intervals. This should be run for the final version before submission.
- **Scale-free degree distribution**: The current network uses a random graph with approximately normally distributed degrees. Real social networks have heavier-tailed degree distributions, which would make the VH correction more consequential.
- **Non-random non-response**: The current non-response model assumes contacted individuals decline independently of their characteristics. If non-response is correlated with group membership (e.g., men are more likely to decline), it would compound bias in a way not captured here.
