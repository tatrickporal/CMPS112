::::::::::::::::::::::::::::::::
wc.st
::::::::::::::::::::::::::::::::
     1: #!/afs/cats.ucsc.edu/courses/cmps112-wm/usr/smalltalk/bin/gst -f
     2: "$Id: wc.st,v 1.5 2016-11-14 16:31:24-08 - - $"
     3: 
     4: chars := 0.
     5: lines := 0.
     6: 
     7: [stdin atEnd not] whileTrue: [
     8:    |byte|
     9:    byte := stdin next.
    10:    chars := chars + 1.
    11:    (byte = Character nl) ifTrue: [ lines := lines + 1 ].
    12: ].
    13: 
    14: stdout << lines << ' ' << chars << Character nl.
    15: 
