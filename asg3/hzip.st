#!/afs/cats.ucsc.edu/courses/cmps112-wm/usr/smalltalk/bin/gst -f
"$Id: hzip.st,v 1.2 2018-05-17 14:02:20-07 - - $"
execname := '$RCSfile: hzip.st,v $' =~ '.*: (.*),v.*' at: 1.
stdout << 'Starting ' << execname << '.'; nl.


"Getting the input file from the command line arguments"

FileStream extend [
   nl := Character nl.asg3
   catFile [
      |count|
      count := 0.
      [self atEnd not] whileTrue: [
         count := count + 1.
         count asString size to: 5 do: [:skip| stdout << ' '].
         stdout << count << ': ' << self nextLine << nl.
      ]
   ]
]

"nl := Character nl.
Smalltalk arguments size = 0
   ifTrue: [stdin catFile]
   ifFalse: [
      Smalltalk arguments do: [:filename |
         |separator|
         separator := '::::::::::::::::::::::::::::::::'.
         stdout << separator << nl << filename << nl << separator << nl.
         filename = '-'
            ifTrue: [stdin catFile]
            ifFalse: [
               [
                  |infile|
                  infile := FileStream open: filename
                                       mode: FileStream read.
                  infile catFile.
                  infile close.
               ] on: SystemExceptions.FileError do: [:signal |
                  |errno|
                  errno := File errno.
                  stdout flush.
                  stderr << execname << ': ' << filename << ': '
                         << signal messageText << nl.
                  stderr flush.
               ]
            ]
      ]
   ]"



"Reading file and creating the char frequency table"
"FileStream fileIn: 'tree.st'./st"














stdout << 'Finished ' << execname << '.'; nl.