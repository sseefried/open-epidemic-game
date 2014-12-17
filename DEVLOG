---------------
Wed 17 Dec 2014
---------------

So I was experiencing a lot of "tearing" when I was running the game
on external monitor but not on my laptop alone.
According to the SDL migration guide (https://wiki.libsdl.org/MigrationGuide)

I was directly rendering into a texture, and then copying that to video
memory using `renderCopy`.

The recommended practise is to
  - draw the entire frame to a buffer in RAM
  - Copy that to the texture
  - Use `renderCopy`

I'm still trying to work out why this is fast at all. Wouldn't copying
straight from a surface to video memory be the way to go? But looking
at the SDL2 library it seems that textures are the *only* way to render
to video memory. Either you copy into textures or you use
`createSurfaceFromTexture`.

---------------
Sun 14 Dec 2014
---------------

Breakthrough. The game is now running on iOS simulator.

---------------
Sat 29 Nov 2014
---------------

I finally thought of a more "strategic" redesign of the game. Here are some
ideas:

1. The germs don't have to be killed with one tap. The "cell wall" can get
   thinner and thinner until they "pop".
2. There can be different types of germs.
   a) ones that inhibit the growth of others
   b) ones that multiply very fast.
   c) one that actually kill others that they are in contact with

3. You can pick up and drag a germ?

4. Antibiotics are dragged and dropped. Then a new vial or beaker appears.
   As they get more and more ineffective their colour fades.

---------------
Fri 14 Nov 2014
---------------

I finished the Android build on Mon 03 Nov 2014. I'm amazed it worked the
first time because just today I tried to build again and deploy. I discovered
that you get no information on what went wrong when your program
crashes because the stdio and stderr are redirected to /dev/null.

You can get set a property to redirect to the log

$ adb shell stop
$ adb shell setprop log.redirect-stdio true
$ adb shell start

Once you've done this you can

$ adb logcat | grep 'I/stdout'

or

$ adb logcat | grep 'I/stderr'

to see output from the program


---------------
Sat 01 Nov 2014
---------------

[15:54]

So I've decided to create a number of "third party" repos (which I will
host on GitHub). In these I will check out a particular version of a C
library that I require on Android and port it to build on Android.
I've become a little obsessed with being able to match the files I check in
against a very particular version of the library. I thought that I might
be able to do this by creating a script the re-tars all the files at a
specific git commit hash and then checks the SHA1 hash of this tarball against
the original. This didn't work because tar keeps a whole lot of extra
information around than just the file contents such as file permissions,
time stamps and user/group information. *Sigh*. Annoying.

However, there is another way that we can check. We do keep around the SHA1
hash and the git commit hash in a file in the repo. Then you can download
that file for yourself, untar it into the repo over the top of the files
and see if there is any difference. Then you will know that everything is okay.

-------

I've started trying to build the game on Android and this has been a deep,
dark pit of despair so far. I tried following some instructions I found on the
Haskell wiki on building a GHC Arm cross compiler. These did not work too
well for me so I wrote some emails to people who had succeeded before. Ivan
Perez from Keera Studios was very helpful and pointed me to a fork with
which I was able to build a cross compiler successfully.

But there are more issues. In order to build Haskell bindings to Cairo, SDL2,
SDL2_mixer and Chipmunk you first need to be able to build the requisite C
libraries. This has been quite annoying so far and I can see it's going to take
me at least a week or two.

What I've decided to do is what the Chromium project supposedly does and
that is to create one giant repo which contains all the source code I need.
I'm not sure about the best way to go about this. Should I pull in the
source repos, not change them, and then apply patches during the build process
or is it okay for me make changes to specific sub-repos? What is better in the
long run?

Having a read about how Chromium does things
(http://www.chromium.org/developers/adding-3rd-party-libraries) I note the
following interesting features:

- they store the SHA512 hash of the tar ball they got the source from. For git
  it's not clear what they do but a hash of the source tree and the commit
  hash would probably be enough to uniquely identify it.

- they update the code when something is fixed upstream.

- they do license file checks.


---------------
Fri 17 Oct 2014
---------------

[09:30]

I'd now like to work out what is remaining on the game.

1. I need to get the radii of the germs right, which I think is going
   to be a bit of a chore. The problem is that the edges of the germs are
   Bezier curves where the control points lie outside the actual
   physical edge of the curve. (The control points lie on what I'll dub the
   "notional radius" of the germs.) Unless I calculate what the outermost point
   of the germs are using Bezier maths I can't really know what the *true*
   radius of the germs is. Since the notional radius is used for collision
   detection it looks like the germs are colliding when their edges aren't
   touching. Perhaps this isn't a problem, and I can live with it. It's
   not like the collisions were ever going to be "pixel perfect".

2. I'd like to add a blur effect to the game to make it truly look like
   a petrie dish. It's going to be hard to get the performance acceptable.
   After just a little preliminary reading I don't think I just want to do
   a Guassian blur but rather to use a more sophisticated depth of field
   effect or "bokeh" effect.

3. I need to create a playground in which Rauri can make the germs look
   better.
      - Docker
      - blog posts to teach him about Haskell
      - crib the Cairo wrapper from Helm.

4. I want the antibiotics to be dragged and dropped on top of the germs
   and to have an area of effect that is not the complete screen. As the
   antibiotics get less potent they fade in colour until they become
   clear eventually.

5. I want to add squish sound effects.

6. I want to add the "Crystal Harmony" music in the background. Eventually
   I will have to get some custom music made. The piece is way too good
   for this game and goes places emotionally that don't really fit with the
   mood of the game.

[08:50]

I tried both solutions. Of course the pure Hipmunk solution worked but
ultimately I was unhappy with it. It took me a long time to get right
because I had to ensure that actions evaluated with 'unsafePerformIO'
happened in the right order. It just felt ugly.

So I decided to go with the free monad approach. I didn't write about this
before but one thing that concerned me is that I didn't just want to shove
the entire Hipmunk interface into this monad. What I really wanted was
to be able to evaluate another, smaller, Hipmunk free monad inside the
Game free monad. I didn't know this was possible before I started but
thankfully it was!

It turns out that you need to enable GADTs (or at least ExistentialTypes)
in order to have operations in your free monad that are polymorphic.
I wanted to have an operation:

runHipM :: HipSpace -> HipM a -> GameM a

I'm lying a little about the type. In the end both the GameM monad
and the HipM monad are run inside the IO monad, but the basic idea is there.

Two data types, GameScript and HipScript are defined that are then turned
into free monads by application of the Free type constructor.

type GameM = Free GameScript
type HipM  = Free HipScript

The GameScript type looked a little like this:

data GameScript next =
    Get     (GameState -> next)
  | Put     GameState next
  | forall a. RunHipM HipSpace HipM a (a -> next)

See how I needed to introduce an existential type? It turns out you'd need
it for even more mundane situations. What if you wanted to put the "read"
operation in GameScript (corresponding to read :: Read a => String -> a).
This would be encoded as:

  | forall a. Read a => Read String (a -> next)

(the first occurrence of Read is a type class name, whereas the second is
a constructor name)

Anyway, back to the matter at hand, how do we evaluation one free monad
inside another? Really it's quite easy! You just need to make sure
that the target monad of both free monad interpreters is the same.
In my case that was IO.

runGameM :: GameState -> GameM a -> (IO a, GameState)
runHipM :: HipSpace -> HipM a -> IO a

Then you implement the RunHipM case as:

  case gameM of
    (Impure (RunHipM space hipM cont)) ->
      do a <- runHipM space hipM
         runGameM <current state> (cont a)

---------------
Sat 20 Sep 2014
---------------

I've become quite obsessed with keeping the GameM monad pure. The problem
is that I'm using the Hipmunk library (a Haskell binding to the Chipmunk
physics engine) and it's firmly stuck in the IO monad. I've been doing a lot
of thinking about how to resolve this issue and I've come up with two solutions,
the second one thanks to a discussion with Lyndon. (The second one involves
using a free monad if that whets your appetite.)

Let's talk about the architecture. Currently we have a mainloop, written
in SDL and running inside the IO monad. This loop takes a "game" function
which has type signature FSMState -> Event -> GameM FSMState.

The GameM monad is a state monad with random number generation
("StateT GameState (Rand StdGen) a"). I wanted very much to keep this
monad pure but I run into the snag that I want to store the current
physics engine state inside the GameState data structure. Sure, I can *store*
an IO value inside the monad, but I certainly can't evaluate it inside
the GameM monad.

My first solution to this problem is to create a pure wrapper for Hipmunk.
I've prototyped this solution, and although it uses unsafePerformIO liberally,
it works. In order to ensure that the IO actions occur in the right order
we thread a dummy state through the functions that I defined in the API
to this pure wrapper.

The second solution would be to create a giant "super monad" for the entire
game to be in -- a free monad based on a Game DSL I define. Then I'm free
to evaluate whatever the hell I want in the IO monad but without the problem
that if I *were* just in the IO monad *anything* could be happening inside it
at any point. With the Game DSL free monad I know exactly what the actions
that can be performed inside it are.

The more I think about it the more appealing a Game DSL free monad is.
This will necessitate yet another rewrite... do I go ahead with it?

I know what I'll do. I'll try both solutions in different branches of
the git repository. I'll try the pure Hipmunk solution first.


---------------
Tue 16 Sep 2014
---------------

I'm trying to think of a good data structure for storing germs. At the moment
I've just got a list of germs. It might be okay to add new germs to the
beginning of the list, which can be done in constant time, but what about
deleting them? If I use Data.Map I'm going to need a notion of germId which
seems cumbersome. I do probably want a k-d tree or BSP tree to reduce the number
of objects I need to check for collisions with taps. Do these need an ID?

------------------
Sunday 14 Sep 2014
------------------

Small germs need to animate faster to look like they are moving at all.
It's much more noticeable when a big germ is moving. To simulate this
I've added an extra field to the Germ data type call germCumulativeTime.
Each frame we add the duration of the frame to this cumulative time, but we
also scale it by a factor inversely proportional to the size of the germ.
In fact, it's proportional to 1/sqrt size. For some reason this just looks
right visually.

---------------
Fri 12 Sep 2014
---------------

I'm back to animating germs. Up until now I've been returning a bunch
of higher order functions to animate various things about the germs but now
I realise that it's all going to have to be data driven if I want to
mutate the germs properly. You can't inspect functions!

I've decided that there are three inheritable characteristics of a germ
- number of spikes
- body gradient
- nucleus gradient

but I will also pre-calculate collections of so-called "moving points".
The moving points are polar points where each component also as a list of
"periodic functions" associated with it. However, these periodic functions
are represented as data. Each periodic function has amplitude, period and
phase.

---------------
Thu 11 Sep 2014
---------------

Today I finished writing the FSM module but then deleted it, realising it
was clunky and didn't actually simplify anything. The problem was
that the finite state machine had states with associated data.
e.g. [FSMLevel Int] is on of the variants.

My FSM module essentially created an association list between FSM states
and transition functions which conditionally took us to a new FSM state.
However, what you really want to do is a pattern match against the FSM state.
e.g. case fsmState of
       FSMLevel i -> ... some code ...

Thinking about how I would rewrite my module I realised I didn't even need it.

---------------
Wed 10 Sep 2014
---------------

In the JavaScript version of the game I created a finite state machine
that captured the essence of the game. I'd like to do that again but I want
to think carefully about it. You see, there is only a need to run at
several frames per second when the game is in the FSMLevel state, otherwise
it makes sense to pause the main loop. At present I'm pretty sure I don't
have a way to do that. If the frame updates were occurring in a separate
thread and I triggered each frame update with a very simple message then
it would be easy to stop and start it. Is this too heavy weight a solution?

Also, if I start using threads will this interact well with an iOS
backend? I seem to remember there being issues.

Let's attack this problem from the other direction. Can we pause the
frame loop if our program is just single threaded? Clearly not. If our
program is single threaded we are forced to either be in this loop or
somewhere else, and that is simply how it is.

What we really want are three threads:

1. frame loop.
2. event loop.
3. Control flow loop. Where all the game logic happens.

In the Cocoa and iOS world we have a notion of "run loops" which handle input
sources and can also have timers set on them. They are responsible for
detecting input and dispatching events.

Think from the perspective of the backend. You pass the backend
the game functions, which are essentially callbacks.

Okay, you're going to have to think carefully about architecture.

Things to do:
[ ] Get clear on what 'callback hell' is, and what the alternatives are.
[ ] Look into using STM possibly.

My suspicions about callback hell are this. You start your game loop
and you set a bunch of callbacks. Now you are in a position where
you have to think about 2 or more actors interacting with each other.
You need to make sure that race conditions don't occur where both actors
are manipulating shared memory at the same time. You cannot avoid this.
As soon as you've put callbacks into place this is a real possibility!

-------
14:44

I did a little bit of study into GLUT and discovered that the callbacks are
all essentially called in sequence in a single thread. The "idle callback" is
to be used whenever user input has finished being handled. Based on this,
I'm going to go back and remove my threaded code. It required MVars to ensure
that race conditions occurred and will be a lot simpler. However, I am
going to add some more state to BackendState to track how long it takes
to a) update the game state and b) render the frame.

This way we can dynamically update the rate at which the game displays. Even
in completely pathological cases we can reduce the frame rate to once every
few seconds if it starts taking forever to render.

-------
16:28

Okay, it's getting a bit ridiculous that I still haven't implemented the
state machine data structure, so I'm going to implement that right now. Then
I will come back to the issue of avoiding "death spirals" in rendering.

---------------
Mon 08 Sep 2014
---------------

At the moment I'm checking for events (such as keypresses/mouseclicks)
on every frame. This is not really the way I should be doing it. I should
be handling the events asynchronously in a separate thread. In order not to
have a race conditions when updating the state of the game I'm going to need
to use MVars or some such mechanism.

However, I'll do this later. I've put it in the TODO file.


Things I learned today:

It is good to log information such as framerate. There was a point where
I was doing what I thought was a blocking call to wait for an SDL event
but in fact it was repeatedly polling and eating up valuable CPU cycles.
The game looked just the same to me but my logging told me differently.
It reminds me of the phenomenon pilots go through when their eyes are telling
them something different to their instruments.
