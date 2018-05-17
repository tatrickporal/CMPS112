#!/afs/cats.ucsc.edu/courses/cmps112-wm/usr/smalltalk/bin/gst -f
"$Id: hzip.st,v 1.2 2018-05-17 14:02:20-07 - - $"
execname := '$RCSfile: hzip.st,v $' =~ '.*: (.*),v.*' at: 1.
stdout << 'Starting ' << execname << '.'; nl.
FileStream fileIn: 'tree.st'./st
stdout << 'Finished ' << execname << '.'; nl.
