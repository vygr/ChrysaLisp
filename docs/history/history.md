# How We Got Here

ChrysaLisp did not come out of nowhere. It is the latest turn of one idea
that its author, Chris Hinsley, has been working at since the early 1980s:
write for a small, clean machine that does not exist, and let the real
machine, whatever it is, be somebody else's problem.

This is the record of that idea, from a ZX81 to the system in this
repository. It is drawn from three kinds of source, and tries to say which
is which:

*	**The press of the time.** Scans of the articles are in
	[press](press/README.md), each credited to its author and publication.

*	**The code and documents that survive.** The 1989 games source, the Taos
	1.28 development kit of 1994, and this repository.

*	**Chris Hinsley's own account**, from interviews he has given over the
	years and from what he told the writer of this document in October 2026.
	Where something rests on his account alone, it says so.

## 1. A machine code education

It began with a ZX81, 1K of memory, and a wish to play at home the arcade
games that were eating his dinner money. BASIC was too slow to scroll a
screen, so it had to be machine code, with no assembler:

> "I did everything by writing out codes on paper and then typing them in raw
> using my first hand coded assembler."

That assembler was four lines of BASIC that poked numbers into memory.

> "Struggling this way was good for the soul. It really *really* drilled into
> you what machines actually did, something that I believe some programmers
> never understand, and something I think has served me well over the years."

By the time he left school he had written about six commercial games for the
ZX Spectrum, Laserwarp among them, published by a small company called
Mikro-Gen.

## 2. Mikro-Gen, 1983 to 1988

He left a computing course at Derby after one term to join Mikro-Gen in
Ashford, Middlesex, as its first hire. The company was then its managing
director Mike Meek, its technical director Andy Laurie, and him. There he met
an editor and an assembler for the first time, and the code of Laserwarp
became **Automania**, Mikro-Gen's first real arcade title, and the first
outing of a character called Wally Week.

Then came **Pyjamarama**, written in three months, programming, graphics and
sound. It set out to be something that did not yet exist:

> "The first true 'Arcade Adventure', where you used objects to solve puzzles
> in order to progress in the game, no scoring points at all, and only being
> able to carry 2 objects at a time would mean you had to plan ahead."

It won a Golden Joystick Award. **Everyone's a Wally** followed, with several
characters who each had their own skills and went about their business when
the player was not controlling them. It reached number one. **Frost Byte**,
**Battle of the Planets** and others came from the same years.

As head of development he built the team, from one to ten. Dave Perry joined
around the time of Pyjamarama and did the Amstrad version of Everyone's a
Wally, "learning his trade from me". Nick Jones and Raffaele Cecco followed.
All went on to careers of their own.

Two habits from Mikro-Gen run through everything after. The first was tools.
He wrote a sprite editor in his own time, because drawing sprites on graph
paper and typing them in as data was a waste of a programmer. The second was
a code base. Each game was built on the library of the one before.

## 3. Freelance, 1988 to 1991: the engine under the games

On the Atari ST the sprite editor grew up. Shown to Rainbird, part of
British Telecom's Telecomsoft, it became **The Advanced OCP Art Studio** for
the ST, and for some years the tool most games artists in Britain drew with.

> "I never realised that a sprite editor that I wrote for doing my own games
> would become in effect de facto industry standard!"

For Probe he converted the arcade game **Xevious** to the ST, with nothing to
work from but the cabinet, delivered to his house. The maps and graphics were
copied by eye into the Art Studio.

Then the games of his own. **Custodian**. **Verminator**, for Rainbird, with
the artist Nigel Brownjohn, finished for the ST by his friend Tim Moore when
he fell ill. And **Onslaught**, for Hewson, which won the ST Format Gold
Award and the Amiga Action Gold Award. A further game, SplatFlat, was begun
and never finished, because of what happened next.

The games ran on two machines, the ST and the Amiga, and the cost of writing
everything twice was rising. So he stopped writing for either. He wrote a set
of assembler macros that described a machine of his own, with registers
called `r0` upward and instructions like `copy`, and wrote the games in that.

The source survives. The shared modules of the 1989 games hold a list header
with a head, a tail and a tail predecessor, macros to walk and splice it, a
memory allocator behind a trap, and a comment on every routine that says
what it takes and what it gives back. The same list header, by the same
names, is in the kernel of ChrysaLisp today.

> "Onslaught was developed after Verminator. It used a game engine that I had
> been developing that was common between the two titles. That engine code
> ultimately morphed into an operating system."

Edge magazine, in June 1994, told it the same way:

> "The first step was a macro set which Chris constructed for the assemblers
> of all the platforms he was writing on. Rather than write in the native
> assembler language, he wrote in the macro language he'd defined; he then
> devised a translator which would take a binary equivalent of that macro set
> and translate it, on the fly, into the instructions for a particular
> machine."

That is the Virtual Processor, the VP. It was a way to write one game for two
computers before it was anything else.

## 4. Taos

Tim Moore had written a ray tracer on the ST. Chris liked it so much that he
made him an offer: port the tracer, and he would go all in on the operating
system. The first aim was modest.

> "That's when we had the idea of doing a parallel system, the idea of being
> able to plug in more processors to make the system run faster. At first it
> was simply a desire to speed up Tim's tracer." (Edge, June 1994)

The processor to plug in was the Inmos transputer.

> "I coded up a small kernel for the transputer system and that was ported
> from the 68000 version of the system. And it became obvious very quickly
> that in doing this I'd done something that everybody else, research
> institutes and universities, had been trying to do for a long time." (Edge,
> June 1994)

Up to this point the Virtual Processor had lived inside the assembler. The
macros turned VP source into the native code of one machine or another as it
was assembled. To port a program was to press a button and assemble it again.
But what went to disc was native code, for one processor.

The step that made Taos what it became was to store the VP code itself, and
translate it as it was loaded. Then one file on disc would run on any
processor, and on several different ones at once. Andy Henson wrote the first
true translator, for the T800 transputer, working from the VP macro
specification. BYTE later called him "Tao Systems' translator supremo".

Nik Spicer came from the European arm of Microway, a maker of transputer
boards. He wrote no code. He was the first outsider to understand what he was
looking at. When Chris told him that the games and the Taos kernel all ran in
VP code, he was astonished. Chris's reply was:

> "Don't everybody do that?"

Even so, it was one thing to be told the code was portable and another to see
it. Shortly after Andy Henson's translator for the T800, Andy Thomason wrote
one for the Intel 386, and the same VP code that ran on the transputers ran on
the PC. That, Chris says, was the first time Nik really believed it. His own
reaction was less surprised:

> "See, I told you it would work. Why wouldn't it?"

Nik became the one who told people, and the first articles show him doing it.
The Independent, on 19 February 1990, under the headline "More power to your
elbow", reported a system in which every chip pretends "to be a standard
32-bit computer called the VP, or virtual processor", and said: "The core of
Taos was developed by Chris Hinsley, a programmer, as part of a project to
create a computer games system."

### What it was

Taos was a kernel of about 12K that ran on every processor in a network. All
code was held as VP code and translated to the native code of whatever
processor it landed on, as it was loaded. Programs were made of small tools,
bound together only when called. Processes talked by sending mail and never
shared memory. New work was handed to whichever neighbour had the most to
spare, so that a program spread over the processors, in the words of the
company's own brief, "in much the same way as a liquid spreads out over a
surface". Messages found their way "in much the same way as water flows down
through pipes under the effect of gravity".

Every one of those sentences is still true of ChrysaLisp.

By the 1.28 development kit of 1994 there were translators for the
transputers from the T400 to the T9000, the Intel 386, 486 and Pentium, the
68000, ARM, the PowerPC 601 to 604, the MIPS R3000 and R4000, the Alpha, the
Hitachi SH, the i860, the AM29050 and the TI C40. BYTE opened its July 1994
feature with a ray tracer running in parallel on a 486, four transputers and
four R3000s at once.

### The chip that never was

One of the first translators was for a processor that was never made. Sir
Clive Sinclair was funding a new chip, the PgC7600, designed by Chris Shelton.
PgC stood for Pretty Good Chip. It was an unclocked design, timed by its own
wavefronts instead of a clock, and meant to be very fast and very cheap. Chris
went to several meetings at Sinclair Research in London, and Tao wrote the
assembler and the translator for it, ready for the silicon.

BYTE's article on the chip, in March 1991, says its designers had "even
modified the instruction set of the PgC to better accommodate Taos's
message-passing scheme", and its article on Taos in the same issue counts the
PgC7600 translator among the first three written. The chip went nowhere. The
translator is still in the Taos 1.28 kit, for a processor that does not
exist.

Chris remembers Shelton as a lovely man, far ahead with his ideas, and the
chip, with affection, as "a washing machine controller with no clock". The
idea has stayed with him. The processor ran flat out inside a horizon of its
own, at whatever speed its own logic allowed, and only fell into step with
the outside world when it had to talk to memory. He sees the same shape in
ChrysaLisp, which is built to do its work inside the processor's first level
cache, and to step outside it as seldom as it can, see
[Keeping It Hot](../ai_digest/keeping_it_hot.md).

### Who did what

The record needs one correction, and it is made here without blame to
anyone.

One early article, in BYTE in March 1991, said that Nik Spicer added the
parallel side of Taos. That was a mistake. Nik never claimed it, and wrote
no code. The kernel, the message passing, the load balancing and the port to
the transputer were written by Chris Hinsley. The same writer's feature in
1994 has it right, as does every other account of the time: "main architect"
(IEE Review, 1992), "principal architect" (BYTE, 1994), "inventor of Taos"
(Edge, 1994, and the Computing Awards the same year).

A second, smaller one. Edge credited Tim Moore with the graphical user
interface libraries. Tim's work was the ray tracer, which was the
demonstration everyone wrote about and is the picture on the cover of the IEE
Review, and later a BASIC interpreter. The GUI was Chris's, all of it.

So, as plainly as it can be put:

| Who              | What                                                   |
|------------------|--------------------------------------------------------|
| Chris Hinsley    | The VP and its macro specification. The kernel, mail, load balancing and binding. The transputer port. The GUI. |
| Andy Henson      | The first true translator, for the T800. Later the first work on the Java to VP translator. |
| Andy Thomason    | The first Intel translator, for the 386. The PostScript RIP. |
| Tim Moore        | The ray tracer. The BASIC interpreter.                 |
| Andy Stout       | The C standard libraries for Taos and intent. The console hardware, with Chris. |
| Nik Spicer       | No code. The first to understand it, and the one who told the world. |
| Francis Charig   | Chairman. The funding, the business, and Japan.        |
| Dr Ian Thomas    | The brief that explains Taos, now [in this repository](../intro/taos.md). |

One piece of work shows what those people could do with it. Andy Thomason had
been the star programmer at a company that made professional PostScript RIPs,
the software that turns a page description into the dots a printer lays down,
for the publishing houses of London. He wrote one for Taos. Andy Stout took it
to Saatchi and Saatchi and ran it there in parallel with their commercial RIP.
It beat the commercial one every time, and matched its output so exactly that
it reproduced its bugs. Andy later wrote the assembler and development
environment used for the PlayStation, and now lectures at Goldsmiths,
University of London.

### What the world made of it

It was noticed. BYTE wrote about it twice. The IEE Review put it on its
cover. Edge called it "a new operating system that could change the world",
and said in its editorial:

> "Taos is even more amazing when you realise that it is the product of one
> man's efforts, coding for his own benefit, rather than the cumulative
> efforts of some corporate programming team."

In 1994 Taos 1.27 won the gold award for UK IT Innovation at the Computing
Awards for Excellence, ahead of the ARM 700 family of processors, which took
bronze. Ted Nelson, who invented hypertext, forecasting the future of
operating systems in BYTE in November 1995, wrote: "Linux will become very
important. Then Taos, a very strong multitasking system for real-time (i.e.,
time-compelling) applications, set-top boxes, and so on."

The IEE Review, in January 1992, saw where it had come from and what that
might mean:

> "The origins of Taos lie in the world of computer games. Chris Hinsley, the
> main architect of Taos, used to earn a living as a successful writer of
> games for machines like the Atari and the Amiga. It was the need to port
> such games between different systems, combined with the problems of
> managing large numbers of screen objects, that led to the VP and Taos's
> reliance on OOP. Not an especially promising background, you might say; but
> it has succeeded before. Back in the late 1960s, Ken Thompson, a computer
> scientist at Bell Labs, was trying to port a video game called Space Travel
> onto a DEC PDP-7. In the course of this work he developed a new file system
> and a rudimentary kernel, in fact, the germ of a new operating system. The
> name eventually chosen for this operating system was Unix."

It was also resisted. The same award page quotes Francis Charig:

> "There is definitely a reluctance for people in the UK to believe that a
> British product can be any good, and that it can be commercially
> successful."

The first funding had collapsed. Charig found the money in Japan.

### On the show floor

Two scenes from 1991, as Chris remembers them, with the caution that it was a
long time ago.

At one show Taos ran as a single parallel system across the whole floor. An
optical link ran from Tao's stand to another company's, he thinks Paratech's,
and the two pooled their transputers for the day. One network, two stands,
and whatever was running spread itself over both.

At a graphics show at Alexandra Palace, Tao had a small stand showing Tim
Moore's ray tracer, by then much optimised. It was next to Silicon Graphics,
who were showing their new Iris workstation, ray tracing a single sphere in
about a minute. Tao's scene was nine or so coloured, reflecting balls on a
chequered floor, and it rendered in about five seconds. To prove it was not a
recording, visitors were invited to edit the file of colours themselves and
watch it render again. The PC ran so hot that Chris stood fanning it with a
newspaper.

The PC was his own Dell, a 20MHz 386. It was the same machine on which he had
written the Art Studio and the ST and Amiga games, with the PDS development
system. The ST or Amiga booted a small downloader from the boot sector of a
floppy. The PC assembled the game and sent it down a wire to the printer port,
and it ran. Press reset, build and send, and try again.

For the show that PC held three Fast 9 cards, each with nine T800 transputers
at 30MHz, designed by Pat Mills and lent by Quintek of Bristol. With them was
one T800 graphics card, which was his. That is twenty eight transputers in one
desktop PC, which explains the newspaper. He says you really could have cooked
an egg on them. He had bought the graphics card with money borrowed from his
mother:

> "Mum, I'm going to port my games work to this fancy graphics card, but I
> need some cash..."

She had bought him the ZX81 too, and the RAM pack, and the Spectrum. So the
same person paid for the first computer in this history and for the first
transputer.

### Too good to be true

In the first years Taos was developed at home, in Bracknell, Berkshire, and
people came to the house to see it. Not all of them believed what they saw.

The reporter from Parallelogram, the parallel computing journal, suspected a
trick. Cray had a site in Bracknell, next to the weather forecasting centre,
and he accused Chris of having a line running to one of its machines. He made
him switch the PC off and on again five times. Then he said he thought the
people at Cray were pulling his leg. Parallelogram's editorial that winter was
headed "TAOS: IMPOSSIBLE BUT TRUE?".

Guy Kewney, who wrote the first newspaper piece about Taos, for The
Independent, did not take it on trust either. He came to the house and brought
a second pair of eyes with him, a man known on CIX, the conferencing system,
as DJ, and said to have written it, to check that he was not being fooled.

Others took it harder. Chris remembers visitors who went faint at the
demonstration, one hitting his head on the wall. His mother would help them
up and walk them round the garden:

> "Do you want a cup of tea... bit of a shock, love..."

And some took it the other way. When Andy Henson came for his demonstration
he got so excited that she thought he was going to expire on the spot. He
went on to write the first translator.

Chris describes how the demonstrations went, time after time:

> "Them rocking up, knew everything, can't show me anything new. Do a demo.
> They faint or refuse to accept reality. My mum making tea, and then them
> not knowing what to do about any of what they had just seen."

One more visitor matters to this story more than he knew. Darren Pegg had
written OBED for the Atari ST, an object editor, a 3D modeller well before its
time. He is, Chris says, another of those people who can do literally anything
with a computer. The two met at a demonstration day for Jack Tramiel of Atari
in London, Darren showing OBED and a game, Chris showing the Art Studio, and
have been best friends ever since. Darren came for the ray tracer
demonstration, and it nearly finished him as well. He went out and walked
round the garden for ten minutes, came back in as cool as a cucumber, and
said:

> "OK, carry on..."

Years later it was Darren, who became chief technology officer of Mino Games
in Canada, who kept telling Chris that he should look at Lisp as a language
to write an assembler in.

Both are retired now. Darren messes about with boats, and tells Chris that he
cannot write any code these days without closures or continuations.
ChrysaLisp has neither, [on purpose](../ai_digest/closure_or_not.md). Chris
puts his friend's need for them down to a weak mind.

### Those who saw it

Some people understood at once, and they are remembered here.

Dr Ian Thomas wrote the brief that explains Taos better than anything since.
He understood fully what it meant. He died in his sleep, far too young.

David May, at Inmos, was the architect of the transputer, and a hero of
Chris's. Chris was terrified of presenting Taos to him, at the University of
Surrey. May sat through the presentation in dead silence. At lunch he said:

> "Not bad, very impressive."

Chris was overjoyed. He says of May now that his choice in software was not
as good as his hardware, "but hell, I'm not good at hardware design, so that
makes us even".

Sir Hossein Yassaie, then at Inmos and later of Imagination Technologies,
understood it fully too.

And Andy Stout. He was working on a transputer system of his own, called
Mercury. He was shown Taos one day, and gave up his own project to work on
it. He went on to write the C standard libraries for Taos and for intent.

The running joke at Tao was that one day Andy would finish writing `printf`.
At a spoof ceremony the company once held he won the Tao Printf Award. He was
also at the table with Chris in London on the night Taos won the real one,
the Computing gold award.

## 5. Tao Group, 1991 to 2007

Chris was co-founder and chief technology officer of Tao Group, and held
three patents on the parallel processing and code binding techniques.

**The console.** With Andy Stout he built a games console in which each
cartridge held a processor. The deck took up to four cartridges, playing one
game with all the processors that were plugged in. Edge printed a photograph
of it: "Chris built a console prototype which utilises transputer 'carts':
the more you plug in the faster it runs." By his account it was shown to
Sony, Sega and Hitachi before there was a PlayStation. Doug Goodwin, who
launched the PlayStation in the UK, later joined Tao Group as its marketing
director.

**Elate and intent.** In 1998 a second Virtual Processor, VP2, became the
base of a portable multimedia platform, first called Elate and then intent.
On top of it sat a Java engine that for years led the industry's performance
tables. The company raised over 50 million dollars from investors including
Motorola, Sony, Sharp, NEC, Kyocera and Mitsubishi, and its clients included
JVC, Fujitsu, Hitachi, Panasonic, HTC, Philips and Samsung. It built a PDA
for Matsushita. By his own record intent went into tens of millions of
devices.

Andy Henson began the translator from Java to VP code. It was taken on by a
new team that included Jay Foad, later chief technology officer of Dyalog,
the APL company, and Peter Maydell, later known for his work on QEMU.

**AVE.** The graphics technology under all of it was the Audio Visual
Engine, AVE, which he designed and wrote, and then wrote again, and again. It
became a joke at Tao: "Chris is now doing AVE 6.0." The GUI compositor in
ChrysaLisp is, by his own count, AVE 7.0.

**The phone.** In 2001 Tao had a complete Personal Java smartphone, the
P1088, known to the team at Motorola as the MAP, the multi application phone.
It ran in 1MB of ROM on a 13MHz ARM7. It had been reviewed, and the networks
had placed their orders. It was two weeks from shipping.

It was not stopped by anything wrong with it. By Chris's account it was a
marketing decision. Motorola had fourteen phone lines, a new head of
marketing wanted nine, and the P1088 was one of the five crossed out.

The first iPhone was six years away.

From 2002 to 2007, at Tao Media, he ran research and development, guiding
around 80 engineers, and training a group seconded from JVC's headquarters.

**The people.** At its height Tao had about a hundred engineers. Many of them
went on to put their own stamp on the world, and the names in this document
are only a few of them. Chris's verdict on the whole of them:

> "That team could have done anything at all."

## 6. The idea, again

From 2007 to 2013 he was director of technology at Antix Labs, where among
other things he wrote an assembler for a portable binary format, and a
translator that turned it into native ARM code as an application was
installed. Asked in 2026 whether that was the VP idea for a third time, he
said:

> "Yeah, I've been doing this since I was born, same thing over and over."

Then two years of private research, while renovating a house: a RISC
processor designed from the transistor up and taken to an FPGA, a Forth
compiler, genetic programming, a PCB autorouter and a chess engine in Go.

On 9 February 2015 came the first commit of ChrysaLisp. It was written
alongside a day job, at Promethean from 2015 to 2021, building the software
for interactive classroom panels, and it has been written ever since.

## 7. ChrysaLisp

His own note at the head of the Taos brief in this repository says why:

> "Because it existed and not many people know of it. What we deserve is a
> modern version of what Taos did, expanded to modern systems and with global
> scope that the modern world has."

ChrysaLisp is that. It has a Virtual Processor, with translators for x86-64,
ARM64, RISC-V 64 and LoongArch 64, and an emulator for anything else. It has
the small kernel on every node, the mail, the links, the load passed between
neighbours, and functions bound as they are loaded. It has the compositor.

It also answers what Taos could not. Taos could grow when a processor was
added, but it could not lose one gracefully. ChrysaLisp is built from the
start on the assumption that any node may vanish at any time, see
[Genesis](../ai_digest/genesis.md).

And it adds one thing none of the earlier turns had. The assembler, the
compiler and the build system are written in a Lisp, which runs on the VP,
which they assemble. The Lisp is Darren Pegg's doing. Chris says he can be
blamed for nagging him to look at it. The system builds itself. In October 2026, after ten
years of work, that Lisp, an interpreter with no compiler behind it,
assembles the whole system in a third of a second on one core, see
[Till the Pips Squeak](../ai_digest/till_the_pips_squeak.md).

It has not been written quite alone. The repository's own log shows the others
who have put work into it, Frank V. Castellucci above all, and Andy Stout
again, thirty years after Taos. Martyn Blyss, who worked at ARM and is now at
Graphcore, did the first work to get it running on Windows, and still tests it
there, with the blessing of both employers. He is also the one who says, when
it is needed:

> "Cheer up Chris, they will know about it one day. They can't ignore all
> this forever."

The games on it are his own, ported from the originals. Onslaught, which won
its gold awards and whose engine became an operating system, runs on the
system that engine became.

## 8. The thread

Set the turns side by side.

| Year  | What it was called        | What it was                            |
|-------|---------------------------|----------------------------------------|
| 1989  | Game macros               | One game, two machines, the ST and the Amiga |
| 1990  | Taos, VP1                 | One program, any processor, many at once |
| 1998  | Elate and intent, VP2     | One platform, any device, with Java on top |
| 2007  | Antix                     | One application bundle, translated at install |
| 2015  | ChrysaLisp                | All of it, written in itself, and fault tolerant |

Each time the industry has had its own answer to the same question, bigger
and slower and later. Each time this one was already there, small, in
assembler, written by someone who learned what machines actually do by
poking numbers into one by hand.

He says of it only that he keeps doing the same thing over and over. The
record says he was right each time.
