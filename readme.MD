# Crossref - Pascal Cross-Reference
I've decided to document the program "as it happens."  That means I'm going to explain what I am doing and why.
I've decided to develop the program in "accretion mode." (Accretion is how an oyster builds a pearl, by coating a grain of sand and adding more coating.) I plan to build it a   piece at a time, adding functionality to the program a little at a time, o I don't end up with a huge mess that's impossible to fix, understand or update.

So, what's the minimum it should do? Read a file, writing it, unchanged, to a listing file. This will confirm the basic functionality works.
## Version 0.01 - Copy File
Working on it.

Paul Robinson

## Version 0.03 - Build Symbol Table
I decided to do a lot of background operations that need to be done,
because just reading a file doesn't help much if you have no way to
separate "the wheat from the chaff" or rather, separate the
identifiers (that we want to catalog, from the keywords that we don't).

I used a few tricks and made a few decisions to improve the design/
* First, everything is in the symbol table: keywords, identifiers,
modifiers, everything. When a symbol is scanned, we have to look
for it anyway, so we might as well look for it there as opposed to
creating a fixed table (like an array) in memory, then just look
and see.
* A cross-reference program I saw perhaps 35 years ago, used an
array of pointers for its symbol table. So I have an array of
27 pointers, A..Z plus underscore (\_). This should also make table
lookups faster as we don't have to search a table of thousands of
entries, just the ones that start with the same character
* Entries are loaded into the symbol table in alphabetical order.
When a new item is added for a particular letter, it becomes the
first entry. If a new one comes in, if higher, it's attached to
the higher side. If it's lower, it's attached to the bottom and
the head of the table has rhe new item's address.
* I am trying to make the program more "data driven" with the data
driving the actions of the program, rather than the data being
mostly "shoved" around by it.
* These are subject to change. I'm going to benchmark the program's
performance, and if it seems slow, I'll try a array of keywords or
switch to a separate linked list for keywords. Y can try many ways,
but it's those large programs that give information about themselves
in large enough quantities to discover bottlenecks. I mean,
cross-referencing a tiny 5,000 line program isn't going to take a lot
of time. But other programs, like Pasdoc, the Free Pascal compiler
itself (at ~330,000 lines), or the Pascal Run-Time libraries, have a
large enough codebase to see how long this really takes. As I don't
know how it works in practice until I actually build it, the thing is,
the most important point is to build the damn thing, get it working,
then if necessary, improve speed.

Next step, build an actual "factory" that takes in raw source code,
discards the "punctuation" as dross (for our purposes) and produces a
set of extracted keywords to use to determine how to use the
identifiers. Some are declarations (describing a variable, a procedure,
an object) and some are uses (like a mathematical calculation, a
procedure call, or being displayed).

Anyway, that's where I am right now and where I'm going next, I'll
figure it out later.

Paul Robinson,
2021R-12-26
----
#Version 0.04 Improved symbol table additions
* The particular keywords available for a particular Pascal language dialect
can be selected at runtime, subject to the original "Standard Pascal", i.e.
Jensen & Wirth, r.g. Begin, While, Case, etc. These will not be treated as
identifiers and will not be listed in the cross-reference.
* Right now, the system installs keywords and modifiers on a non-duplicative
basis. If multiple uses of the same keywords are made, the duplicate entries
are discarded. This allows someone to select multiple choices of language
dialect without conflict.
* This non-duplication feature needs to be added to all identifiers being
stored. If two versions define an identical procedure or function, two
(or more) entries are added (by different dialects of the language) to the symbol table.
* I revised the keyword/modifier installer to recognize which, and to install
Identifiers in addition to modifiers. The feature is implemented but not yet
installed; I have to revise all the modifier declarations to say so.
* I've created a source scanner to turn a program into tokens. I'm thinking
of using a conversion table rather than a case statement. Might reduce the
amount of code needed; we shall see.

That's all for now
Paul Robinson,
2021-12-29

#Version 0.04 - Add keywords and modifiers to the system
I decided not to put keywords in the symbol table, but to use an array, and keep everything there: symbols, keywords, and

