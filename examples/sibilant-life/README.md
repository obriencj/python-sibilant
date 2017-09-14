
This is an implementation of [Conway's Game of Life][1] in Sibilant.
I'm using it to refine and profile the compiler and trampoline. It's
also acting as a testing bed for how the language "feels" to develop
in.

The life implementation is my own. Its original impl was entirely
functional but incredibly naive (using a byte array to store the
entire playfield, and processing it linearly every age). The current
implementation is slightly better (only storing and processing over
coordinates of interest) but there's room for improvement.

The curses code segments were snagged pretty directly from the
[curses life demo][2] in the Python distribution, which was authored
by Andrew Kuchling, with Mouse support and color by Dafydd Crosby. I
didn't port the mouse/color stuff over yet, but I still might.

[1]: https://en.wikipedia.org/wiki/Conway%27s_Game_of_Life
[2]: https://github.com/python/cpython/blob/master/Tools/demo/life.py
