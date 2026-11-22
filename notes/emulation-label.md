# Emulation as a success label

See also [`narrative-devices.md`](narrative-devices.md): allusion is emulation at the scale of a sentence.

A design note, not a plan. It is an excerpt from an early conversation with
another agent on this project, kept for the idea; the chat interface's
artefacts (timestamps, search counters, citation chips) are removed, and the
sources those chips pointed at are named where known. Notes added on
2026-11-22 follow at the end.

## The question

> Now that you mention it, a great indicator of a successful book is a book
> that was emulated by another book which was also a success, e.g., *Mrs.
> Dalloway* and *The Hours*.

## The reply (excerpt)

You've independently derived eigenvector centrality. "Emulated by another
book which was also a success" is the PageRank recursion stated in English: a
node's weight is the sum of the weights of the nodes pointing at it. Say it
once in graph terms and the whole idea snaps into focus: you're proposing to
measure literary standing by what other successful writers chose to answer,
not by what consumers bought.

### Why this is a better label than either of the others

| Signal | Whose behaviour it measures | What it's confounded by |
|---|---|---|
| Gutenberg downloads | readers, today | school syllabi |
| Goodreads ratings | readers, cumulative | platform era, exposure time |
| Emulation | writers | critical legibility |

Emulation is the only producer-side success measure available, and it's a
different thing in kind. A book can move two million copies and generate zero
descendants: airport thrillers are consumed and discarded. A book with modest
sales can found a lineage. Woolf's sales in her lifetime were respectable but
not enormous; her descendant count is enormous.

The *Hours* case is almost too clean: Pulitzer and PEN/Faulkner in 1999
(source: Wikipedia), now sold bound together with *Mrs. Dalloway* in a single
Macmillan volume (source: the publisher's page). The pairing is itself a
published artefact: the influence edge is a product.

### The censoring problem, and why it's fixable

You only observe emulations that got published and remembered. A book
imitated by forty failed manuscripts has in-degree zero. That's the Gutenberg
survivorship problem promoted one level up, but it's better, because the
denominator is observable. The Gutenberg label has no way to represent the
books that never made it into Gutenberg at all. The emulation graph can: you
can sample from the population of published novels rather than the
population of remembered ones, and then in-degree is a real rate rather than
a survivor's medal.

### The paradox in the middle of the idea

Salganik, Dodds and Watts built an artificial music market with 14,341
participants and found that increasing the strength of social influence
increased both the inequality and the unpredictability of success, and that
success was only partly determined by quality (*Science*, 2006, "Experimental
study of inequality and unpredictability in an artificial cultural market").
The same songs landed in wildly different ranks across independent worlds.

Emulation is social influence. So the statistic is at once the best predictor
available and a direct measure of the mechanism that makes prediction
impossible. Prestige, imitation, prestige is a self-reinforcing loop;
in-degree is the loop's current state, not its cause. It is a superb
explanation of canon formation and a circular predictor of a new book's fate.
Anyone using it to forecast will be reading the graph's memory of the past
and calling it foresight.

Worth stating plainly in whatever you write: this measures whether a book
entered the conversation, not whether it will. That's a real contribution,
and it's the honest one.

### The reframe it suggests

A third target variable, distinct from the two discussed before:

- **Will it sell?** Consumer attention: heavy-tailed, barely predictable from
  text.
- **Is it good?** Contested; needs pairwise human judgements.
- **Will it be imitated?** Descendant count: legacy, not sales.

The third is the one a novelist actually cares about, and it's also the one
most likely to be predictable from text, because emulation is triggered by
teachable formal features. *The Hours* is imitable because its architecture
is legible: three timelines, one day, a party, a death. A device, a structure
or a voice can be copied; "sold well" cannot. Salient, describable form is
exactly what a text model can detect.

That's a much better-posed prediction problem than bestseller
classification. (See the notes below on the claim that nobody has posed it.)

### How to build it: the method matters

Do not try lexical text-reuse detection. *The Hours* shares essentially no
sentences with *Mrs. Dalloway*; the relationship is architectural. Automated
reuse detection of the kind used in humanities computing is built to find
quotation, paraphrase and plagiarism (an arXiv survey; the link was not
kept). It will find nothing here, or find the wrong things.

The right family is narrative-structure comparison: event sequences,
character-function mapping, act-level shape. That's an active but immature
area. Work formalising syuzhet and event annotation across texts is the
closest thing to infrastructure (*Journal of Computational Literary Studies*;
the article was not kept). The nearest existing influence study took the
easier route of measuring semantic similarity between an author's reading and
his writing (arXiv; not kept), which requires the author's library and only
works for one documented reader.

Which means: you are the feature extractor. A novelist can look at two books
and say "this is a *Dalloway* retelling" with a reliability no current model
approaches. Hand-curating even 200 to 300 influence pairs, with the edge type
labelled, is a genuine dataset contribution, and it's what makes the whole
project possible. That's a comparative advantage a competitor with more
compute can't shortcut.

### Weight the edges by kind, or Christie swallows the graph

A naive in-degree count conflates distinct relations:

| Edge type | Example | What it indicates |
|---|---|---|
| Retelling | *The Hours* → *Mrs. Dalloway* | canon status, high specificity |
| Stylistic descent | minimalists → Carver | craft influence, diffuse |
| Genre foundation | later detective fiction → Poe | historical priority, saturating |
| Negative | modernism → Victorian realism | the strongest edge, invisible to similarity metrics |

The last breaks every automated method, and a novelist will see it
instantly. A book that defines itself against a predecessor is more
decisively shaped by it than one that imitates it: the manifesto, the
reaction, the anti-genre. Anti-influence is influence with the sign flipped,
and no embedding model will find it, because the surface is maximally
dissimilar. If you build the graph, that edge type is where the intellectual
novelty is, and it's the part only a practitioner would think to encode.

### One confound to declare up front

Imitating a famous book is legible to reviewers. Publishers and MFA programmes
reward visible lineage, so the emulation graph partly measures critical
legibility rather than reader attachment. Cunningham's Pulitzer is not
independent of Woolf's prestige: the prestige transfers, and it transfers
through the institution, not the market. Same critique as the syllabus
problem, one level up. Declared rather than solved, that's fine; it's arguably
the most interesting thing the graph can show.

If one move had to be picked from all of this: treat the emulation graph as a
**validation metric for the frozen historical corpus, not as a feature in the
predictor**. It's the one label whose meaning is defensible, whose bias can be
named, and whose collection needs the specific expertise a novelist has. And
the thesis that literary success is transmitted through writers rather than
through readers has an edge.

## Notes added 2026-11-22

- **The corpus suits it.** The Gutenberg books are mostly from before about
  1930, so they have had a century or more to be answered: *Jane Eyre* by
  *Wide Sargasso Sea* (a negative edge as much as a retelling), the *Odyssey*
  by *Ulysses*, *Pride and Prejudice* by many later novels. The flip side:
  exposure time varies too, so in-degree needs adjusting for the years a book
  has had to collect descendants, the same exposure confound the table above
  gives Goodreads.
- **"Nobody has posed it" is unverified.** Author-level influence data
  already exist: Wikidata's "influenced by" property (P737) has been used in
  literary network studies. A book-level, typed-edge prediction target may
  still be new, but that needs a literature search before anyone claims it.
  Wikidata is also a cheap first source of candidate edges, author-level and
  untyped, for hand confirmation and typing.
- **Outside the pre-registration.** The 2027–28 wave's endpoint is downloads
  (PREREGISTRATION.md). An emulation label would be a separate dataset, like
  the planned relabelled forks in CHANGELOG.md, and needs no amendment unless
  it is used inside the registered analysis.
