# Allusion and foreshadowing as signals

A design note, not a plan, from a conversation on 2026-11-22. The two
theses are the project owner's; the measurement ideas were worked out in
discussion. It pairs with [`emulation-label.md`](emulation-label.md):
emulation is allusion at the scale of a whole book's architecture, and
allusion is emulation at the scale of a sentence.

Both devices matter for the pre-registered H2 (PREREGISTRATION.md, section
1) for the same reason: the features they suggest depend on form, not on
subject. They carry almost no information about era, topic or setting, which
is what a TF-IDF text model leaks, so they are the strongest candidates for a
test that could support a claim about *style* rather than about text in
general.

## Allusion: the less it is noticed, the more it works

**Thesis.** The power of an allusion is roughly in inverse proportion to how
detectable it is: hence the "sly allusion". Detection has two levels,
conscious and not:

| Level | What the reader does | Effect |
|---|---|---|
| Recognised | catches it, enjoys it, and judges it ("is this clever?") | real, but the judgement keeps the reader at a slight distance |
| Felt, not recognised | registers it below awareness: a cadence, a parable's shape, a rhythm last heard in church | works on mood without raising the reader's critical guard; possibly the strongest |
| Neither | nothing | private to the author |

An earlier draft of this argument said an allusion no reader catches has no
effect. That was wrong: it took "detected" to mean recognised. Echoes work
through implicit memory, and readers are moved by what they never name.
(The phenomenon is well established; some of the priming literature around
it has replicated poorly, so a write-up should claim the effect, not its
size.)

**Measurement.** Subconscious resonance has to travel through something on
the page, and what carries it is mostly surface form: rhythm, sentence shape,
word order, sound. So the felt-but-unrecognised level may be the one a model
reaches *best*:

- **Cadence echo:** compare a text's prosody and syntax (stress patterns,
  clause rhythm, part-of-speech sequences, sentence-length shape) with
  source corpora a period reader would have absorbed: the King James Bible,
  the Book of Common Prayer, Shakespeare. Not shared words: shared shape.
  The README's hybrid-measure bullet ("stylistic similarities to revealed
  word, e.g. the Old and New Testament") is the same idea; this thesis gives
  it a reason.
- **Overt echo** (near-quotation, found by text-reuse detection) is easy to
  measure but sits at the weak, recognised end of the table. It is a
  contrast feature, not the signal; by the thesis, successful books might
  carry *fewer* overt echoes.
- **Recognised allusion** of the Joycean kind needs a human annotator, as
  emulation edges do.

## Foreshadowing: a scorecard of credibility

**Thesis.** Chekhov's gun is a running account between author and reader.
The author suggests something, gives the reader every opportunity to forget
it, and then the gun goes off: the reader is surprised, but not so
surprised. After a few guns have gone off, the reader really starts to pay
attention. It is credibility built up inside the storytelling itself, and
the juggler with knives in the air (README Roadmap) is the same account seen
from the middle of the book: how many promises are open, and which comes
down next.

**Measurement.** Parts of it are computable without deep language models:

- **Dormant returns:** something introduced early (a named object, a place, a
  minor character) that disappears for a long stretch and comes back with
  weight late. "Surprised, but not so surprised" is a long gap and a return
  that makes the earlier mention suddenly matter. A first version can track
  content-word lemmas or named entities across a book's length: count of
  returns, gap lengths, and where in the book they land.
- **Knives in the air:** the number of setups still open at each point, as a
  curve over the book. Suspense is where the curve is high and payoffs are
  near.
- **Unpaid promises** are the hard part: most early mentions are not setups.
  Leave them until the first two work.

## Where these would enter

Not the pre-registered primary analysis, whose text model is fixed (section
6). As candidates for a declared, style-restricted secondary model, filed as
an amendment before the window opens on 2027-01-01, or for a later wave.
