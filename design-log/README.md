# Design Log

This directory contains screen shots. I explain each shot and the design decisions behind it here.

## *2015-01-11* Letter boxing

**Git commit**: 44d5b8315bd330f4a22afe0e7b8dc3a2cc8a1647

The game field is now a 4:3 aspect ratio. On most mobile devices this will mean letterboxing
at the left and right of the screen. For those rare devices that have more square screens this will
mean letterboxing at the top and bottom. So I'm not using some of the screen real estate. I don't
know whether this is stupid or not, but providing a larger playing area on certain devices
just because they have a larger aspect ratio means that you have a distinct gameplay experience
(and probably an advantage over those playing on devices with a smaller aspect ratio).

The game area is divided into a *side bar* and a *game field*. The side bar takes up 1/3 of the 4:3
aspect ratio and the game field takes up the remaining 3/3. This makes the side bar big enough
to contain the antibiotics that players will unlock while provide a square game field.

You'll notice in the pic that the "Infected!" message only covers the game field. I might
change this in the future.

![Letter boxing](2015-01-11-letter-boxing.png)

## *2015-01-21* Antibiotics

**Git commit**: f7c29d8e5c1d55e9ec51ed626dfbc12bad2d6d6e

The antibiotics are shown as a big circle with an "effectiveness" written inside them.
The term "x% effective" means that *on average* x% of the germs die. What this really means
is that each germ has a small chance, equal to (100 - x)%, of being immune to the antibiotic.

At the moment you click on the antibiotic to use it. In future I will require a drag-and-drop
action in order to use. I am still undecided about whether the antibiotics applies to all the germs
or an area of effect based on where it was dropped. Probably the former as the latter has
implications for strategy and design. The implication on design is that I'll have to signify
somehow the area of effect.

![Antibiotics](2015-01-21-antibiotics.png)

## *2015-01-22* Mutation

**Git commit**: 68565a0be335d4e65fd55485acb9c994846f246e

The germs now mutate and inherit from each other.

![Mutation](2015-01-22-mutation.png)

## *2015-01-24* Score

**Git commit**: 42cc56818e663d36ddc45e273807850571e6e794

Score has been added.

![Score](2015-01-24-score.png)


## *2015-01-25* Gradient

**Git commit**: eaa5f33065a1e4eb0da51215ef4e201fe02b5841

![Gradient](2015-01-25-gradients.png)

Gradients to text added. "Tap to continue" message added. This appears a small time after
the "Epidemic averted!" or "Infected!" message so that people don't inadvertently tap into the
next level without seeing that they completed a level or died.

## 2015-02-26 Antibiotic flasks

**Git commit: 723fd72027f255b7e2bc1bae21eee0f72a659568**

Wrote some code to draw the antibiotic flask. Here's an evolution of my work over the last 30 minutes.

![Flask 1](flask-0.png)
![Flask 2](flask-1.png)
![Flask 3](flask-2.png)
