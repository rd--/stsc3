# What's new with 2.0?

Everything.  SuperCollider has been completely rewritten.

Here's just a few of the main differences.

SuperCollider 2.0 in general is faster, more expressive, higher quality, and easier to use than version 1.

There are many more unit generators.

Control rate signals are always linearly interpolated to audio rate everywhere that it is necessary to prevent a discontinuity.

Many unit generators are of better quality than those in version 1.

Many unit generators are faster than those in version 1.

The programming language is now fully object oriented. Everything is an object.  There is a set of Collection classes including Dictionaries, Sets, SortedLists, etc.

There are no proprietary format patch files. SuperCollider compiles text files and reads sound files as independant objects rather than being bundled together as they were in SuperCollider 1.

The program has an interpreter front-ended by a styled text editor so that examples may be executed directly from a document by selecting them. This allows you to write pieces that are their own documentation.

The old mechanisms of DSP stages, DSP tasks, dspAdd and dspRemove are all gone now. All you do is write your synthesis expression and play the resulting structure. This is both easier to use and results in clearer programs.

Events can spawn events in a recursive manner.

Each event can have its own calculation block size.

Event start and stop times are single sample accurate.

You can have multiple control panel windows and create and destroy them dynamically.

You can read and compile text at runtime. Therefore scores may contain any executable text.

Instruments and unit generator networks can be built dynamically on a per event basis and can be parameterized by other unit generator networks. So you can put an Lfo in your score to control pitch if you like. Scores don't have to be just lists of floating point numbers.

Programmatic patch building and multi channel expansion allow creation of large patches with a few lines of code.

The synthesis engine is a separate virtual machine from the language virtual machine This makes it more efficient than in version 1 where the synthesis loop was executing language code every control period.

Audio buffers such as sample files and delay lines can be allocated dynamically.  Delay line unit generators do not require that their buffer be zero filled to begin with, which means there is no glitch if you decide to allocate and begin using an 8 second delay line on a moment's notice.
