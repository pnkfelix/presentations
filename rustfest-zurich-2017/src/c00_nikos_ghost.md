# Matsakis' Ghost <!-- (5 min) -->

## IRC snippets

```irc
<manishearth> RIIR!
<pnkscrooge> Bah! Humbug!
```

. . .

Scrooge's optimism about Rust's future had faltered

```irc
<pnkscrooge> What’s Rust to you but a language
<pnkscrooge> for writing types without deploying apps;
```

. . .

```irc
<pnkscrooge> a language for finding yourself a compile-cycle older,
<pnkscrooge> but not an object file richer?
```

. . .



```irc
<pnkscrooge> If I could work my will,
<pnkscrooge> every idiot who goes about with "RIIR" on their lips
<pnkscrooge> should be nailed up in their own crate
<pnkscrooge> and buried with a bicycle gear through their heart
```

```irc
* pnkscrooge was kicked by &manishearth: eeew
```

. . .

and Scrooge was unkind to his comrades as well

```irc
<pnkscrooge> Crichit! Cargo is broken!
<acrichto> Are you talking to me?
```

<!--

----

```irc
<manishearth> Don't be cross, Scrooge!
<pnkscrooge> What else can I be,
<pnkscrooge> when I live in such a world of fools as this?
```

-->

## The Why of Scrooge

```irc
<pnkscrooge> why invest in adding feature after feature?
<pnkscrooge> forcing people to learn new idioms
<pnkscrooge> they'll just leave and learn Java or C++ instead
<pnkscrooge> change is *death*
```

 . . .

```irc
* pnkscrooge was kicked by &manishearth: off-topic philosophizing
```

<!--

## Footnote: PnkScrooge's struggle

Rust is Research. No, not research.

<div class="fragment"><p>(Yes research?)</p></div>

Language evolution

 * static drop semantics (abandoned; stuck w/ existing dynamic drop)

 * fallible allocation

 * ~~`pub(path)`~~ <div class="fragment">`pub(in path)`</div>

. . .

-->

## Boo boo

After ~~Crichit~~ Crichton had gone home, Scrooge pondered over the
borrowck error on his screen.

## Boo boo {data-transition="concave"}

``` {.rust}
error[E0597]: `yyyyyyyyyyysssssssssssssssssoooooosoosssssssssssyyyyyyyyhhhhhhhhhhhhhhhhhhhhyyyyyyyyyyyssssoooooosyyyyyyyyyyyyyyyyyyyyyyysssssyhhhhhhhhyhhhhddddddhyssyyss` does not live long enough
  --> errmsg.rs:11:5
   |
10 |         x = &yyyyyyyyyyysssssssssssssssssoooooosoosssssssssssyyyyyyyyhhhhhhhhhhhhhhhhhhhhyyyyyyyyyyyssssoooooosyyyyyyyyyyyyyyyyyyyyyyysssssyhhhhhhhhyhhhhddddddhyssyyss;
   |              ---------------------------------------------------------------------------------------------------------------------------------------------------------- borrow occurs here
11 |     }
   |     ^ `yyyyyyyyyyysssssssssssssssssoooooosoosssssssssssyyyyyyyyhhhhhhhhhhhhhhhhhhhhyyyyyyyyyyyssssoooooosyyyyyyyyyyyyyyyyyyyyyyysssssyhhhhhhhhyhhhhddddddhyssyyss` dropped here while still borrowed
12 |     println!("x: {}", x);
13 | }
   | - borrowed value needs to live until here

error: aborting due to 6 previous errors
```

Scrooge decided he needed a larger screen to get perspective


## Booooooo {data-transition="fade-out"}


``` {.rust .quartsize}
 --> errmsg.rs:5:9
  |
5 |         x = &yyyyyyyyyyysssssssssssssssssoooooosoosssssssssssyyyyyyyyhhhhhhhhhhhhhhhhhhhhyyyyyyyyyyyssssoooooosyyyyyyyyyyyyyyyyyyyyyyysssssyhhhhhhhhyhhhhddddddhyssyyss;
  |         ^
  |
  = note: #[warn(unused_assignments)] on by default

warning: value assigned to `x` is never read
 --> errmsg.rs:6:9
  |
6 |         x = &yyyyyyyyyyysssssssssssssssssoooooosoosssssssssssyyyyyyyyhhhhhhhhhhhhhhhhhhhhyyyyyyyyyyyssssoooooosyyyyyyyyyyyyyyyyyyyyyyysssssyhhhhhhhhyhhhhddddddhyssyyss;
  |         ^

warning: value assigned to `x` is never read
 --> errmsg.rs:7:9
  |
7 |         x = &yyyyyyyyyyysssssssssssssssssoooooosoosssssssssssyyyyyyyyhhhhhhhhhhhhhhhhhhhhyyyyyyyyyyyssssoooooosyyyyyyyyyyyyyyyyyyyyyyysssssyhhhhhhhhyhhhhddddddhyssyyss;
  |         ^

warning: value assigned to `x` is never read
 --> errmsg.rs:8:9
  |
8 |         x = &yyyyyyyyyyysssssssssssssssssoooooosoosssssssssssyyyyyyyyhhhhhhhhhhhhhhhhhhhhyyyyyyyyyyyssssoooooosyyyyyyyyyyyyyyyyyyyyyyysssssyhhhhhhhhyhhhhddddddhyssyyss;
  |         ^

warning: value assigned to `x` is never read
 --> errmsg.rs:9:9
  |
9 |         x = &yyyyyyyyyyysssssssssssssssssoooooosoosssssssssssyyyyyyyyhhhhhhhhhhhhhhhhhhhhyyyyyyyyyyyssssoooooosyyyyyyyyyyyyyyyyyyyyyyysssssyhhhhhhhhyhhhhddddddhyssyyss;
  |         ^

error[E0597]: `yyyyyyyyyyysssssssssssssssssoooooosoosssssssssssyyyyyyyyhhhhhhhhhhhhhhhhhhhhyyyyyyyyyyyssssoooooosyyyyyyyyyyyyyyyyyyyyyyysssssyhhhhhhhhyhhhhddddddhyssyyss` does not live long enough
  --> errmsg.rs:11:5
   |
5  |         x = &yyyyyyyyyyysssssssssssssssssoooooosoosssssssssssyyyyyyyyhhhhhhhhhhhhhhhhhhhhyyyyyyyyyyyssssoooooosyyyyyyyyyyyyyyyyyyyyyyysssssyhhhhhhhhyhhhhddddddhyssyyss;
   |              ---------------------------------------------------------------------------------------------------------------------------------------------------------- borrow occurs here
...
11 |     }
   |     ^ `yyyyyyyyyyysssssssssssssssssoooooosoosssssssssssyyyyyyyyhhhhhhhhhhhhhhhhhhhhyyyyyyyyyyyssssoooooosyyyyyyyyyyyyyyyyyyyyyyysssssyhhhhhhhhyhhhhddddddhyssyyss` dropped here while still borrowed
12 |     println!("x: {}", x);
13 | }
   | - borrowed value needs to live until here

error[E0597]: `yyyyyyyyyyysssssssssssssssssoooooosoosssssssssssyyyyyyyyhhhhhhhhhhhhhhhhhhhhyyyyyyyyyyyssssoooooosyyyyyyyyyyyyyyyyyyyyyyysssssyhhhhhhhhyhhhhddddddhyssyyss` does not live long enough
  --> errmsg.rs:11:5
   |
6  |         x = &yyyyyyyyyyysssssssssssssssssoooooosoosssssssssssyyyyyyyyhhhhhhhhhhhhhhhhhhhhyyyyyyyyyyyssssoooooosyyyyyyyyyyyyyyyyyyyyyyysssssyhhhhhhhhyhhhhddddddhyssyyss;
   |              ---------------------------------------------------------------------------------------------------------------------------------------------------------- borrow occurs here
...
11 |     }
   |     ^ `yyyyyyyyyyysssssssssssssssssoooooosoosssssssssssyyyyyyyyhhhhhhhhhhhhhhhhhhhhyyyyyyyyyyyssssoooooosyyyyyyyyyyyyyyyyyyyyyyysssssyhhhhhhhhyhhhhddddddhyssyyss` dropped here while still borrowed
12 |     println!("x: {}", x);
13 | }
   | - borrowed value needs to live until here

error[E0597]: `yyyyyyyyyyysssssssssssssssssoooooosoosssssssssssyyyyyyyyhhhhhhhhhhhhhhhhhhhhyyyyyyyyyyyssssoooooosyyyyyyyyyyyyyyyyyyyyyyysssssyhhhhhhhhyhhhhddddddhyssyyss` does not live long enough
  --> errmsg.rs:11:5
   |
7  |         x = &yyyyyyyyyyysssssssssssssssssoooooosoosssssssssssyyyyyyyyhhhhhhhhhhhhhhhhhhhhyyyyyyyyyyyssssoooooosyyyyyyyyyyyyyyyyyyyyyyysssssyhhhhhhhhyhhhhddddddhyssyyss;
   |              ---------------------------------------------------------------------------------------------------------------------------------------------------------- borrow occurs here
...
11 |     }
   |     ^ `yyyyyyyyyyysssssssssssssssssoooooosoosssssssssssyyyyyyyyhhhhhhhhhhhhhhhhhhhhyyyyyyyyyyyssssoooooosyyyyyyyyyyyyyyyyyyyyyyysssssyhhhhhhhhyhhhhddddddhyssyyss` dropped here while still borrowed
12 |     println!("x: {}", x);
13 | }
   | - borrowed value needs to live until here

error[E0597]: `yyyyyyyyyyysssssssssssssssssoooooosoosssssssssssyyyyyyyyhhhhhhhhhhhhhhhhhhhhyyyyyyyyyyyssssoooooosyyyyyyyyyyyyyyyyyyyyyyysssssyhhhhhhhhyhhhhddddddhyssyyss` does not live long enough
  --> errmsg.rs:11:5
   |
8  |         x = &yyyyyyyyyyysssssssssssssssssoooooosoosssssssssssyyyyyyyyhhhhhhhhhhhhhhhhhhhhyyyyyyyyyyyssssoooooosyyyyyyyyyyyyyyyyyyyyyyysssssyhhhhhhhhyhhhhddddddhyssyyss;
   |              ---------------------------------------------------------------------------------------------------------------------------------------------------------- borrow occurs here
...
11 |     }
   |     ^ `yyyyyyyyyyysssssssssssssssssoooooosoosssssssssssyyyyyyyyhhhhhhhhhhhhhhhhhhhhyyyyyyyyyyyssssoooooosyyyyyyyyyyyyyyyyyyyyyyysssssyhhhhhhhhyhhhhddddddhyssyyss` dropped here while still borrowed
12 |     println!("x: {}", x);
13 | }
   | - borrowed value needs to live until here

error[E0597]: `yyyyyyyyyyysssssssssssssssssoooooosoosssssssssssyyyyyyyyhhhhhhhhhhhhhhhhhhhhyyyyyyyyyyyssssoooooosyyyyyyyyyyyyyyyyyyyyyyysssssyhhhhhhhhyhhhhddddddhyssyyss` does not live long enough
  --> errmsg.rs:11:5
   |
9  |         x = &yyyyyyyyyyysssssssssssssssssoooooosoosssssssssssyyyyyyyyhhhhhhhhhhhhhhhhhhhhyyyyyyyyyyyssssoooooosyyyyyyyyyyyyyyyyyyyyyyysssssyhhhhhhhhyhhhhddddddhyssyyss;
   |              ---------------------------------------------------------------------------------------------------------------------------------------------------------- borrow occurs here
10 |         x = &yyyyyyyyyyysssssssssssssssssoooooosoosssssssssssyyyyyyyyhhhhhhhhhhhhhhhhhhhhyyyyyyyyyyyssssoooooosyyyyyyyyyyyyyyyyyyyyyyysssssyhhhhhhhhyhhhhddddddhyssyyss;
11 |     }
   |     ^ `yyyyyyyyyyysssssssssssssssssoooooosoosssssssssssyyyyyyyyhhhhhhhhhhhhhhhhhhhhyyyyyyyyyyyssssoooooosyyyyyyyyyyyyyyyyyyyyyyysssssyhhhhhhhhyhhhhddddddhyssyyss` dropped here while still borrowed
12 |     println!("x: {}", x);
13 | }
   | - borrowed value needs to live until here

error[E0597]: `yyyyyyyyyyysssssssssssssssssoooooosoosssssssssssyyyyyyyyhhhhhhhhhhhhhhhhhhhhyyyyyyyyyyyssssoooooosyyyyyyyyyyyyyyyyyyyyyyysssssyhhhhhhhhyhhhhddddddhyssyyss` does not live long enough
  --> errmsg.rs:11:5
   |
10 |         x = &yyyyyyyyyyysssssssssssssssssoooooosoosssssssssssyyyyyyyyhhhhhhhhhhhhhhhhhhhhyyyyyyyyyyyssssoooooosyyyyyyyyyyyyyyyyyyyyyyysssssyhhhhhhhhyhhhhddddddhyssyyss;
   |              ---------------------------------------------------------------------------------------------------------------------------------------------------------- borrow occurs here
11 |     }
   |     ^ `yyyyyyyyyyysssssssssssssssssoooooosoosssssssssssyyyyyyyyhhhhhhhhhhhhhhhhhhhhyyyyyyyyyyyssssoooooosyyyyyyyyyyyyyyyyyyyyyyysssssyhhhhhhhhyhhhhddddddhyssyyss` dropped here while still borrowed
12 |     println!("x: {}", x);
13 | }
   | - borrowed value needs to live until here

error: aborting due to 6 previous errors
```

## Boooooo! {data-transition="fade-in"}

``` {.rust .quartsize}
ssyyyhhhhhhhhhhyyyhhhhhddhhhdddddddhhhdddddddddddddhhhhdddddddddddmmmmddmmmmdddddddddddddddddddddddddmmdddddddmmdddhhhyssooooooooooooosssssssyyyyyyyyyyyyyhhhhyys+:-````````````````````````````````````
yyyyhhhhhhhhhhhhhyyyyyhddhhhddddddddhhdddddddddddddddhddddddddddddmmmddddmmddddddddddddddhhhhhhddddddddddddddmmmddhhyyssooooo++++++ooooosssssssyyyyyyyyyyyyyyyyyso/-.```````````````````````````````````
yyhhhhhhhhhhhhhhhyyyyyhddhhhdddhddddddddddddddddddddddddddmdddhhddddddddddddddddddddddhhhhhhhhhhhhdddddddddddddddhyyssoo++++++++++++++ooooooosssssssssssssyyyyyyyo+:..``````````````````````````````````
hhhhhddhhhhhhdhhhyyyyyhhhhhdddhhhdddddddddddddddddddddddddddddhhhdddddddddddddddddddhhhhhhhhhyhhhhhdddddddddddhhhyssso++++++++++++++++++++++ooossssoooooosssyyyyyso/-.``````````````````````````````````
yhhhdddddddddhhyyyyyyyyhhhhddddhddddmddddddddddddddddddddddddddhddddddddddddddddhhhhhhyyyyyyyyyyyhhhdddddhhhhyyyssooo++++++++++/++++++++++++++ooooo++++++oosssyyyyo/:.``````````````````````````````````
yyhhddddddddddhyyyyyyyyhhhddmmdddddmmddddddddddddddddddddddddddhhdddddddddddhhhhhyyyyyssssssssssyyyhhhhhhhyyysssoo++++++++++/////////+++++++++++++////////++ooosyso+:-``````````````````````````````````
yyyhhddddddmddhyyyyyyyyhhdddmmmdddddmddddddddddddddddddddddddddhhhddddddhhhhhhhyyyyyssssoosssssssssyyyyyyysssooo+++++++++++//////////////////////////////////+++oso+/-.`````````````````````````````````
ssyyhhdddddmmddhyyyyyyhhdddmmmmmmdddddddddddddddddddddddddddddhhhhhhhhhhhhhhhhyyyyssssooooooooooooooosssssoooo++++/////++++//////////////////////////////::::/:/++++/-.`````````````````````````````````
ssssyyhhhddddddhyyyyyyhhddddmmmmddddddmdddddddddddddddhdddhhhhhhhhhhhhhhyyhhhyyysssssooooooooooo++++++ooo+++++++++//////++++/////////////////////////////::::::::/+++:...```````````````````````````````
yysssyyhhhhddddhyyyyyyyhdddddmmmddddddmddddddddddddddhhhhhhhhhhhhhhhhyyyyyyyyyyssoooooooooooooo++++++++++++/+++++++//++++///////////////////////////////::::::::::/++:-..`````````...```````````````````
yyyssyyyyyhhdddhhyyyyyyhhddddddddddddddddhhddddddddddhhhhhhhhhyyyyyyyyyyysssssssoooooooooooooo+++++++++++++////+/+++++++++/////////////////////////////////:::::::://:-..```````````````````````````````
yyyssyyyyyhhddhhhyyyyyhhhdddddddddddmddddhhdddddddddhhhhhhhyyyyyyyyyysssssssooooooooooooooooo++++++++++++++///////++++++++/////////////////////////////////::::::::::::-..``````.```````````````````````
yssssyyysyhhhhyhhhhyyyhhdddddddddddddddddhhdddddddddhhhhhyyyyysssssssssssoooooooooooooooooooo++++++++++++++/////++++/+++++////////////////////////////////:::::::::::::-..``````.....```````````````````
sssssyyyssyhhyyyhhhhhhhhhhdddddddddddddddddddddddddhhhhhyyyyysssssssssooooooooooooooooooooooo+++++++++++++++++++++++/+++++///////////////////////////////::::::::::::::-.``````````..```......``````````
ssssssyyyyyyyyyyyyhhhhhyhhhhddddddddddddddddddddddhhhhhhyyyyssssssssooooooooooooo++oooooooooo++++++++++++++++++++++++++++/////////////////////////////////::::::::::::--.`````````...........```````````
sssssssyyyyyyyyysyyhhhhyyhhhddddddddddddddddddddddhhhhhyyyyysssssssooooooooooooooooooooooooooo++++++++++++++++++++++++++////////////////////////////////////:::::::::::-.````..```..````.```````````````
ssssssssyyyssyyyssyyyhhyyhhdddddddddddddddddddddddhhhhyyyyysssssssoooooooooooooooooooooo+ooo+++++++++++++++++++++++++///////////////////////////////////////:::::::::::-..```..`````````````````````````
ssssssosyyysssssssssyyyyhhddddddddddhhddddddddddddhhhhyyyysssssssooooooooooooooooooo+o++++++++++++++++++++++++/+//////////////////////////////////////////////:::::::::--..``..`````````````````````````
ssssssssyyssssssssyyyyyyyhddhhddddddhhdddddddddddddhhhyyyysssssssooooooooooooooooooo+oo++ooooo++++++++++++++++/////////////////////////////////////////////////////:::::---..```````````````...`````````
ssssssssssssssssssyyyyyyyhdhhhddmmdddddddddddddddddhhhhyyyysssssssooooooooooooooooooooooooooooo+++++++++++++++/////////////////////////////////////+////////+++++///////::--..``..```..`````...`````````
oosssssssssssoosssyyyyyyyhhhhhdmmddddddddddddddddddhhhhyyyyssssssssooooooooooooooooooooooo++ooo++++++++++++++//////////////////////////////////+++++++++++++oooooo++++/////:-..`.``...........``````````
+osyyyssssssssssssyyyyyyyhhhhhddddhhdddddddddddddddhhhhyyyssssssssssoooooooooooooooooooooo+++o+++++++++++++++++/++++++//////////+++++++++++++++++++++ooooooossyyyssssooo++++/:-..`....``.......`````````
oosyyysssooosssssssysssyyhhhhhhhhhhddddddddddddddddhhhhyyyssssssoooooooooooooooooooooooooooooo++++++++++++++++++++++++++++++++++++++++++++++++++oooosssssssyyhhhhhhhyyyyyssooo+:.......`````....```...``
oosyyssooooossssossysssyhhhhhhhhhhhddddddddddddddddhhhhyysssssssooooooooooooooooooooooooo+++++++++++++++++++++++++++++++++++++++oooooooooooooooosssyyyyyyyhhhddddhhhdhhhhhyyyyo/-..``....```..``````.```
ooossssooossssssossyssyyhhhhdhhyyyhhhddddddddddddddhhhyyysssssssoooosoooooooooooooooo++++++++++++++++++++++++++++oooo++++ooooooooooooooooooooooossyyyhhhhhddddddddddddddhhhyyys+/:-..-----..-......```..
ooossssooosyysssssssyyyhhhhhhhyyyyyhhhddddddddddddhhhhyyyssssssooooooooooooooooooo+++++++++++++++++++++oooooooosssssssoooossssssssssssssssoooooossyyhhhhdddddddddddhhhhhyyyyyyssoo+++++++ooo+++/:--.``..
oooosssoosyyyysssssyyyyhhyyyyyysyyyhhhhddddddddddhhhhyyssssssooooooooooo+oooooooo++++++ooooo+++++++oooooossssyyyyhhyyyyssssyyyyyyyssssssssssoooossyyhhhhddddddhhhhhyyyyyyysssyyyyyyhhhhyhhhdddhhso/-.```
oooooooossyyyyyyyyyyyyyhhyyyyyssyhhhhhhddddddddddhhyyysssssooooooooo+++++++++o+oooooooooooooooooooosssyyyhhhhhddddddddhhhyyhhhhhyyyssssssssssssosssyhhhhhhhhhhhhhhhhhhyyyyysssssyyyyyhyyyyyhdmmdhy+:.``.
+++oo++osyyyyyyyyyhyysyyhhyyysssyyhhhhdddddddddhhhyyysssoooooooooo+++////++++++ooooooooooooosssyyyyyhhhddddmmmddddddddddhhhhhhhhhyyssssssssooooossyhhhddddhhddhhhhhhhhyyhhysssssyyhhhhhhyssydmmdhyo:.`..
+++ooooossssyyyyhhhhyyyyyyyssssyyhhhyhdddddddhhhhyysssooooooo++++++//////+++oooooosssssssssyyyhhhhhddddddmmmdddddmmdddddddhhhhhyyyssssoooooooooosyyhdddddddddddhhyyyyyhhhhhysso+++ooossssssydmmmhy+:.``.
ooooooossssssyyhhhhhyyyyyyssossyhhhhhhddddddhhhyyysssoooo++++++++///::///++ooooossyyyyyhhhyhhhhddddddddddddddddddddddddddddhhhhyyysssooooooo++osyhhddddddddddddhyssossyhhdhyso/:-----:/ooosydmmdyo:-....
sso++oosssssyyyhhhhyyyyyyssoosyyhhhhhdddmdddhyyysssooo++++++++/////::://++oosssyyyhhhhhhhhhhhddddhhhhdddddddddhhhhhhhhhdddddhhhhhyysssooo++++ooyhhdddddddddhhhhyyso++osyhhhyo/:-....`.-+oooyddhs/-......
ssoooooosssyysyyhhhhhhhhyyssssyhhhhhhddddddhhyyssooo++++++++++//::::://++ossyyyhhdddddddddhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhdddddddddhyyssoooooosyhhdddddddddddhhhhyyo++osyyyso+/--......:+oooydho:......``
oosssoooossssssyyhhhhhhhhyyyyyhhdddhhddddhhhyyssoo++++++++++++/::://///+osyyhhhdddhhhhhhhhhhhyyhhhhhhhhhhhhhhhddddhhhhhhhhhhhhhddddddhhhhhhhhhdddddddddddddhhhhhyyssssyyyso+/:-.......:+ooosys/.`.......
ssssssooossssssyyhhhhhhhhhhhhhdddddhhhddhhhyyysoo+++++++++++//:::////++osyyhhhhhhhhhhhhhhhhhhhhhhdddddhhyyyyyhhdddhhhhyyyyyyyyhhddddddmmmmmmddddddddhhhhhhhhyyyyyysssssssoo+/:-......-/+ossso+:.`..``...
yyysssssssssyyyyyyhhyyyhhhhhhhhhhhhhyhhhhyyyssoo++///////////////+++oossssyyyhyyyhhhhhdddddddddddddddhhhysssssyyhhhhhhyyyyyyyyhhhddddddmmmddhyyyyhddhhyyyyyyyyssssssssooooo+/:-....-:///+syo/:-.`.`.....
yyyssssssyyhhyysoooooooooooooooosssooossoooo+++///::////++++oooosssssyyysssyyhhdddddddddddddddddddddhhyyysooosssyyyyyyyyyyyyyyyhhhddddddhhysoo+++oshhhyssyyyyyyssssssssoooo+/::--..-:///+syo:..........`
yyyssyyyyyhhhso+/:::::::::::--:::::::::::::://///////++oossssyyhhhhhhhhhyyyyhddddmmmddddddddddddddddddhhys+oossyyyyyyyysssssyyhhhhdddddhysooo++//++oyyyssyyyyyyyysssssssoo++//:-...-:/+osyys/::----.....
sso+++ooossssoooooooo++++++++++++++//////////+++++++ooossyyyyhhddddhhhhhhyyhhddmmmmmmdddddddddddddddddhhysossyyyyyyyyyssssssyyyhhhhddddhysooo+++///++osssssssssssssooooooo+++/:---:/+osyyyssoo++++//////
///////+++oooossyhhhhhhhhhyhhhhhhhyyyyysssssssssssssssyyyyhhhhdddddddhhhhhhhhddmmmmmmmddddddddddddddhhhyyyyyyyyyyyyyyysssssssyyhhhhddddhyssoo++++////++oooooooooooooo++++oooo+++++ossyyyysssssssssoooooo
-::/++oooooooooosyhhddddddddmmmdddddddddddhhhyyyyssssyyyyyyhhhhhhddddddhhhhhhddmmmmmmddddhhhhhhhhhhhhyyyyyyyyyyyyyyyyysssssssyhhhhddddhysssoo+++++//////+oooooooooooooooooooosssssssyyssssssyssssssoosss
//+oossssssssooossyyhhddddddmmmmddddddddddhhhyssooooooooooosssssyyhhddhhhhhhdddmmmmmmddhhhhhhhhyyyyyyysssssssssyyyyysssssssssyyhhhdddhyysssoo+++++//////+oosssssssssyyyyssssso++//+osyyssssyyyysssssssss
oossyyyyyyyyyysssssyyhhhdddddmmmdddddddddhhhhysoo+++++++ooooooossyyyhhhhhhhhhhhddddmdhhhhhyyyyyyyyyysssssssyyyyyyysssssssssssyhhhhhhhysssssoooo++++//////+oossssssssssooooo+++//::/+oyyssssyyyyyssssssss
ssyyyyyyyyyyyyyyyyyyyyyhhhdddddddddddddddhhhyysoo++++++++ooooooossyyyyyyyyhhhhhhhhhddhhhhhhyyyyyyyyyyyyyyyssysssssssssssoosssyhhhhhhyysssssoooo+++++///////+ooooooo+++++++//////:::/ossssssyyyyyssssssss
syyyyysssssssyyyhhyyyyyyhhhddddddmdddddddhhhyssoo+++++++++oo+++oossyyyyyyyyyyyyyyyyhhdhhyyyyyyyyyyyyyyyyyssssssoooooooossssyyyhhhhyyysssoooooooo+++++/////:///+++++++///++///////::/+osssssyyyyyssssssss
ssyyysssooooossyyyyyyyyyyhhhhhhddmmmdddddhhyyssoo+++++++++++++++osssyyyyyyyyyyysssssyhdhyyyyyyyyyyysssssssssssssssssssssyyyyhhyyyyssssoooooooooooo+++++///////////+++/////////////:/+oosssssyyyyysssssss
ssyyyssooo+oooossyyyyyyyyhhhhhhhddmmddddhhhyyssoo+++++++++++++++oosssyssssssyssssssssyhhhhyyysssssssssssssssssssyyyyyhhhhhhhyyyssssooooooooooooooo++++++//////////++++////////////://+osysssyyyyysssssss
ssyyysooooooooossyyhhhhhhhhhhhyhhdddddddhhyyysooo+++++++++++++++ooossssssssssssssssssyyhddhhhhyyyyyyyyyyyyyyyhhhhyyyyyyyyyyyyssssssooooooooooooooo++++++////:::////++////////////:::/+osyyssyyyyyyssssss
ssyyysooooooosssyyhhhhhhhhhhhyyyyhhddddddhhyysooo+++++++++++oooooooosssssssssssssssssssyyhyyyyyyyyyyyyyyyyyyyyyyyyssssssssssssssssoosossooooooooooo++++++/////:://////////////////:::/oyyyssyyyyyyysssss
ssyyysooooooosssyhhhdddhhhhhhyyyyyhhdmmmddhyyssooo+++++++++oooooooooossssssssssssssssssssssssssssssssssssssooooooooooooooooooossssssssssssoooooooooo+++++++////////////////////////::/oyhyssyyyyyyysssss
ssyyysooooooosssyyyhhhhhhhhhhyyyyyyhddmmmddhyyssoooo+++++++oooooooooooooossssssssssssssooooooooooooooooooooooooooooooooooooooossssssssssssooooooooooo+++++++/////::://++++/////////::/oyhyssyyyyyyyyssss
ssyyysooooooosssyyyyhhhhhhhyyyyyyyyyhddmmddhhhysssooo++++++oooooo++++ooooossssoooooooooooooooooooooooooooooo++++ooooooooooooooossssyssssssssoooooooooo++++++//////:::/+++++//++///////oyhyssyyyyyyysssss
ssyyyysooooooossyysyyyhhhhyyysyyyyyyhhdmmdddhhhyyssooo+oooooooooo++++++ooooooooooooooooooooooooooooo++++++++++++++++++oooooooosssssysssssssoooooooooooo++++++/////:::://++++++++++////oyhyysyyyyyyyyssss
sssyyyysooooooosssssyyhhhyyssssyyyyyyhddmddddhhhyyssooooooooooooo++++++oooooooooooooooo+++++oooo+++++++++++++++++++++ooooooossssssssssssssoooooooooooooo+++++///////::::/++oo+++o+++//oyhyyyyyyyyyyyssss
hhyssyyssooooossssssyyhhhhyysssyyyyyyhddddddddhhhyyssooooooooooooo+++++ooooooooooooo++++++++++++++++++++++++++++++++oooooosssyyysssooooooooooooooooooooo++++/////////:::/+ooo+++ooo+++oyhhyyssyyyyyyysss
ddhsssyyssoooooooossyyhhdhhysssyyyyyyyhddddddddhhyyyssooooooooooooooooooooooooooooooo+++++++++++++++++++++++++++++ooooooossyyyyssoooooooooooooooooooooooo++++///////////++ooo++++ooo++syhhyyssyyyyyyysss
mmdhyysyyysoooooosssyyhdddhyssssyyyyyyhhddddddhhhyyyssssoooooooooooooooooooooooooooooo+++++++++++++++++++++++++ooooooooossyyyyysooooooooooooooooooooooooooo++++++++////+++oooooo+++++osyhhyyyssyyyyyysss
mmmmdhysysssoooooossyyhhhhhyysssyyyyyyyyhhhhhhhhyyyyssssssooooooooooooooooooooooooooooo+++++++++++++++++++ooooooooooooosssyhhyyssooooooooooossssssssssssssoooo++oo+++++++++oooooo++++osyhhyyyssyyyyyysss
NNNNmdhyssssssooooossyyyyyyyyyyhhyyyyyyyyyyyyyyyyysssssssssssoooooooooooooooooooooooooooooooooooooo+oooooooooooooooossssssyhhhyyysssssssosssssyyyyyyyyyyyssssoooooo+++/++++++++ooo++osyyhhyyyyysyyyyssss
NNNNNmdhysssssssooooossssssyyhhdhhhhyyyyyyyyyyyyyyssssssssssssooooooooooooooooooooooooooooooooooooooooooooooooooosssssossssyyyyyyyyyyyyyyyyyyyyyhhhhhhhhyyyyssssooo+++//////+++ooo++osyyhhyyyyyyyyyyssss
mmNNNNmmdyysssssssoosssssooossyhhhhhhhyyyyyyyyyyyysssssssssssssssooosssssssoooooooooooooooooooooooooooooooooooosssssssoossssyyyyyyyyyyhhhyyyyyyhhhhhhhhhhyyyysssooo++//////++++oo++oosyhhyyyyyysyyysssss
mmmNNNNmmdhyssoooooosssssooooossyyyyyhyyyyyyyyyyyyyysssssssssssssssssssssssssssssssooooooooooooooooooosososssssssssossosssssssssssyyyyyyyyyyyyyyyyhhhhhyyyyssssoooo++++///+++++++++osyyyyyyyyyyyyyysssss
mmNNNNNNmmdhyssoooooossssooooooosssyyyyhyyyhyyyyyyyyysssssssssssssssssssssssssssssssssssoooooooooososssyyyyysssssssssssssssssssssssssssssssssssssssssssssssssssyyssssoo+++++++++oooosyyyyyyyhyyyyyysssss
mmNNNNNNmmdhyyyssoooooooooooooooossssyyyyyhhyyyyyyyyyysssssssssssssssssssssyssssssssssssooooossssssososyyyyysssssssssssssssssssssssssssssssssssssssssssssoossssssssssoooo+++++//ooossyyyyyyyhhyyyyysssss
mmmNNNNNmmdhhyyyssssoooooooooooooooossyyyhhyhhhhyyyyyyyssssssssssssssssssyyyyysssssssssssooossssssssssyyyyyyssssssssssssssssssssssssssssssssssssssssssssssoooossssssooooo++++///ooosssyyyyyyhhhyyyssssss
mmmmNNNNmmdhhyyyyyyyyssoooooooooooooossyyhhhhhhhyyyyyyyyssssssssssssssssyyyyyyysssssssssssssssssssssssyyyyyssssssssssssssssssssssssssssssssssssssssssssssoooooooooooooooo++++///+oosssyyyyyyyhhhyyssssss
mmmmmNNNNmddhyyyyhhhhhysssoooooooooooossyhhhhhhhhyyyyyyyyysssssssssssysyyyyyyyyysssssssssssssyysssssssyyyyssssssssssssssssssssssssssssssssssssssssssooooooooooooooooooooo++++///+ossssyyyyyyyhhhyyssssss
mmmmmmNNNNmdhyyyyyhhdddhyysssooooooooossyhdddhhhhhyyyyyyyyysyysssssyyyysyyyyyyyyyyyysssssssssyyssssssssysssssssssssssssssssssssssssssssssssssssssssssssoooooooooooooooooo++++///osssssyyyyyyyhhhhyssssss
ddddmmmNNNmdhhyyyyyhdmmmddhyyysssssssssyhdddddhhhhhyyyyyyyyyyyysyyyyyyyyyyyyyyyyyyyyssssssssyysssssssssssssssssssssssssssssssssssyyyysssssysssssssssssssssoooooooooooooo++++////sssssssyyyyyyhhhhyysssss
dddddmmNNNmmdhyyyssydmNNNmmddhhyyyyyyyhhhdddddhhhhhhyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyssssssyyyssssssssssssosssssssssssssssssssyyyyhyysssssyssooosssssoooooosssssoooooooo+++/////ssssssssyyyyyhhhhyysssss
hddddmmmNNmmddhyyysyhmmNNNNmmddddhhhhhhhdddddddhhhhhhhyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyssssyyyssssssssssosssssssssssssssyyyyyyyyyyhhyssoossso+++oooo+++++++++++++ossssooo++/////yyssssssyyyyyhhhhhyysssy
hhhdddmmNNNmmdhhyysyhdmNNNNNmmmdddddddddddddddddhhhhhhhyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyssssyyysssssssssssssssysssssyyyyhhhhhhhyyyyhyyssoosso++++oooo++////++++///++osssso+++////yyssssssyyyyyhhhhhyyssss
hhhdddmmmNNNmmdhhysyyhdNNNNNNmmdddhddddhhhhhhhdhhhhhhhhhyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyssyyyyysssssssssssssyyyyyyyyhhhhhhhhhhyyyyyyyyysssssoooossssoo++++++++/////++ooooo++////yysssssssyyyyhhhhhyyssss
dddddddmmmNNmmddhyyyyhdmNNNNNmmmddhhhhhhhhhhhhhhhhhhhhhhyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyssssssssssssyyyyyyyhhhhhhhyyyyyyyyyyyyyyyyyyyyssyyysssssssssooo++////++osoo++///yysssssssyyyyhhhhhhyssss
dddddddmmmNNNmmdhhhyyyhdmNNNNNmmdddhhhhhhhhhhhhhhhhhhhhhhhhhyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyysssoossssssssyyyyyyyyyyyssssssssssssssssssssssssssssyysssssssssoo++//+ossso++//yysssssssyyyyhhhhhhyssss
mmddddddmmmNNNmddhhhyyhdmNNMNNNmmddhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyysssssssssssssssssssssssssssssssssoooooooooooooooooooooooooossssooo++ossssso+++syssssssssyyyhhhhhhyssss
mmddhhdddmmmNNmmddhhyyhhdmNNNNNmmdddhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyssssssssssssssssssssssssssssssssssooooooooooooooooo+++ooooo+oooooooossssso+++ssssssssssyyyhhhhhhyysss
mmdhhhhddmmmmNNmmddhhhhhdmNNNNNNmmddhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhyyyyyyyyyyyyyyyyyyyyyyyyyyyyyssssssssssssssssssssssssssssssssssoooooooooooooo++++ooo+++++++++oooooooo+++osssssssssyyyyhhhhhyysss
mmdhhhhhdddmmNNNNmmddhhhhdmNNNNNmmddhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyssssssssssoooosssssssssssssssssssoooooooooossooooooooooo++++++++++++++++++oossssssssyyyyhhhhhyysss
mmddhhhhdddmmmNNNNmmddhhhddmNNNNmmdddhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhyyyyyyyyyyyyyyyyyyyyyyyyyyyyyssssssssssssssososssssssssssssssssssoossssssssssoooooooooo+++++++++/+///+++ossyyssssyyyyyhhhhyysss
mmddhhhhhdddmmmNNNNNmdhhhhhdmNNNmmmdddhhhhhhhhhhhhhhhhhdddhhhhhhhhhhhhhhhhyyyyyyyyyyyyyyyyyyyyyyyyyyyyyssssssssssssssssssssssssssssssssssssssssssssssssssssssssooooo++++++/////++ossyyysssyyyyyhhhhyysss
mmddddhhhhddmmmNNNNNmddhhhhdmmdddmmddddhhhhhhhhhhhhhhhhhdddhhhhhhhhhhhhhhhhhyyyyyyyyyyyyyyyyyyyyyyyyyyyyssssssssssssssssssssssssssssssssssssssssssssoossssyyyysssoo+++++++///++/+ossyyysssyyyyyhhhhyysss
dddmmdhhhhdddmmNNNNNmmdhhhhdddysydmmdddhhhhhhhhhhhhhhhhhhhddddhhhhhhhhhhhhhhhhhhhyyyyyyyyyyyyyyyyyyyyyyyyssssssssssssssssssssssssssssssssssssssssssssssssyyyyyyssoo++++++//++++++ossyyysssyyyyyhhhhyysss
dddmmmddddddmmmNNNNNNNmdhhhhhhoosdmmddddhhhhhhhhhhhhhhhhhhdddddddhhhhhhhhhhhhhhhhhhhhyyyyyyyyyyyyyyyyyyyyyssssssssssssssssssssssssssssssssssssssssssssssyyhhhyysso++++++//+++oooooossssysssyyyyyhhhyysss
hhdmmmmmddddmmmmNNNNNNmmdhhhhy+/ohmmdddddhhhhhhhhhhhhhhhhhhddddddddhhhhhhhhhhhhhhhhhhhhyyyyyyyyyyyyyyyyyyyysssssssssssssssssssssssssssssssssssssssssosssyyhhhhhyso++++++//+++ooooossssssyssyyyyhhhhyysss
hhddmmmmmdddmmmmNNNNNNNmddhhhy+/ohmmdddddhhhhhhhhhhhhhhhhhhhdddddddddhhhhhhhhhhhhhhhhhhhhhyyyyyyyyyyyyyyyyyyssssssssssssssssssssssssssssssssssssssssosssyyhhhhyyso+++++//++oooooooosssyyysssyyyhhhhyssss
hhhddmmmmddmmmmmmNNNNNNmddhhhy+/oydmmmdddhhhhhhhhhhhhhhhhhhhhdddddddddddhhhhhhhhhhhhhhhhhhhhyyyyyyyyyyyyyyyyysssssssssssssssssssssssssssssssssoosssssssssyyyyssooo+++////+oossssoooossssssssyyyyhhhyysss
hhhhdmmmmmmmmmmmmNNNNNNmmddddho+oyhdmmdddhhhhhhhhhhhhhhhhhhhhhhddddddddddddhhhhhhhhhhhhhhhhhyyyyyyyyyyyyyyyyyysssssssssssssssssssssssssssssoooooooooooooooooooo++++++++//+ossyyyssssoooooosssyyyhhhyysss
hhhhddmmmmmmmmmmmNNNNNNNmdddmhsooshdddddddhhhhhhhhhhhyyyhhhhhhhhhddddddddddddhhhhhhhhhhhhhhhyyyyyyyyyyyyyyyyyyyyssssssssssssssssssssssssssoooooooooo+++++++++++++++++++///+ossyyyyyyssooooooosssssssssss
hhhhhdmmmmmmmmmmmmNNNNNNmmddmdysosyhddddddhhhhhhhhhhyyyyyyyyyhhhhhhddddddddddddddhhhhhhhhhhhhhyyyyyyyyyyyyyyyyyyyyyysssssssssssssssssssssoooooooooo+++++++++++++++++++///+ossyyyyyyyyyssoooooooooooooooo
hhhhhdmmNNmmmmmmmmmNNNNNNmddddyssoshddddddhhhhhhhhhyyyyyyyyyyyyhhhhhhhddddddddddddddhhhhhhhhhhhhhhyyyyyyyyyyyyyyyyyyyyyysssssssssssssssssooooooooo++++++++++++++++++++//++syyyyyyyssyyyyysssssssoooo+ooo
ddhhdddmNNmmmmmmmmmmNNNNmmdddhhysoshddddddhhhhhhhhyyyyyyyyyyyyyyyhhhhhhhddddddddddddddhhhhhhhhhhhhhhhhyyyyyyhhyyyyyyyyyyyysssssssssssssssooooooooo++++++++++++++++++++++++syhhhhyyyssssyyyyyyssssssssooo
dddddddmNNmmmmmmmmmmNNNNmddhhhhhysshddddddhhhhhhhhyyyyyyyyyyyyyyyyyyhhhhhhhddddddddddddddddddhhhhhhhhhhhhhhhhyyyyyyyyyyyyyyyyssssssssssssooooooooooo+++++oo++++++o+++++++oshhhhhhyyyyyssyyhhyyyyyyyyysoo
ddddddmmNNmmmmmmmmmmNNNmmdhhhdddhyyhddddddhhhhhhhhyyyyyyyyysssssyyyyyyyhhhhhhdddddddddddddddddddhhhhhhhhhhhhhhhhhhhhyyyyyyyyyysssssssssssoooooooooooo++++ooo+++++o++ooooosyhhhhhhhhhyyyssyyyyyyyyyyyysoo
ddddddmmmNmmmmmmmmmmNNNmmdhhddddhyyhddddddhhhhhhhhyyyyyyyysssssssssyyyyyyyyhhhhhhddddddddddddddddddddhhhhhhhhhhhhhhhhhhhyyyyyyyyyyyysssssssssoooooooooooooooooooooooooossyyhhhhhhhhhhyyyyyyyyyhhhhyyysoo
ddddddmmmNmmmmmmmmmmmNmmdhhhddddhyyhddddddhhhhhhhyyyyyyyyssssssssssssssyyyyyyyyhhhhhhhddddddddddddddddddddhhhhhhhhhhhhhhhhhhhhyyyyyyyyyssssssssooosooooooooooooooooooossyyyyyhhhhhhhhhhhyyyyyyyhhhhyssoo
ddddddmmmNmmmmmmmmmmmmmmddhhdddhhyyhdddddhhhhhhhhyyyyyyyysssssssssssssssssyyyyyyyhhhhhhhhhhhhhhhdddddddddddddddhdhhhhhhhhhhhhhhhhhyyyyyyyyyssssssssssssssssssssssssssssyyyssyyhhhhhhhhhhhhyyyyyhhhhysooo
mmmdddmmmmmmmmdddddmmmmmmdhhhhhhhyhhdddddhhhhhhhhyyyyyyyyssssssssssssssssssssssyyyyyyyhhhhhhhhhhhhdddddddddddddddddddhhhhhhhhhhhhhhhhhhyyyyyyyyysssssssssssyyssssssyyyyyyyyyyyyyhhhdddhhhhhyyyyyhhyssooo
mmmddddmmmmmmddddddddmmmddhhhhhhhhhdddddhhhhhhhhhyyyyyyyyssssssssssssssssssssssssssyyyyyyyhhhhhhhhhhdddddddddddddddddddddddhhhhhhhhhhhhhhhhhhyyyyyyyyyyyyyyyyyyyssssyyyyyyyyyyyyyyhhhdddddhhhyyyyyysooss
mmmddddmmmmmmddhhddddmmmddhhhhhhhhhdddddhhhhhhhyyyyyyyyyysssssssssssssssssssssssssssssyyyyyyyhhhhhhhhhhhhhhhhhhhhhhhhhhhdhhhhhhhhhhhhhhhhhhyyyyhhhyyyyyyyyyyyyyysssssssyyyyhhyyyyyyyhhddddddhhyyyyysssss
mmmdddddmmmmmdhhhhddddmdddhhhhhhhhhdddddhhhhhhhyyyyyyyyyyssssssssssssssssssssssssssssssssyyyyyyyyyyhhhhhhhhhhhhhhhhhhhhhhhhhhhhyyyyyyyyyyssssssyyyyyyyyyyyyyyyyyyyyyssssyyyyyhhhhyyyhhhddddddhhhyyysssss
mmmdddddmmmmdhhhhhhdddddddhhhhhhhhhdddddhhhhhhyyyyyyyyyyysssssssssssssssssoooooosoosssssssssssyyyyyyyyhhhhhhhhhhhhhhhhhhhhyyyyyyyyyyyssssoooooosyyyyyyyyyyyyyyyyyyyyyyysssssyhhhhhhhhyhhhhddddddhyssyyss
```

## Tonight You Will Be Visited

```irc
<pnkscrooge> its the ghost of nmatsakis!
```
```irc
<nmatsakis> the chains of stability and backwards compat are heavy!
<nmatsakis> you must change your way of thinking
<nmatsakis> tonight you will be visited by three spirits
```

## Footnote

Niko is not dead

(Well, maybe he is in this story)

(Or at least overwhelmed)
