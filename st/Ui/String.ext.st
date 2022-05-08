!String methodsFor: 'user interface'!
viewImageFile
    | i w |
    i := (Form fromFileNamed: self) asMorph.
    w := SystemWindow new.
    w extent: (i extent).
    w setLabel: self.
    w model: (Model new).
    w addMorph: i frame: (0@0 corner: 1@1).
    w openInWorldExtent: (i extent + (10@32))!
viewTextFile
    | w t b m |
    w := SystemWindow new extent: 700@432.
    w setLabel: self.
    w model: (Model new).
    t := PluggableTextMorph on: (self readTextFile) text: #contents accept: #acceptContents: readSelection: nil menu: nil.
    w addMorph: t frame: (0@0 corner: 1@1).
    w openInWorldExtent: 700@432 "800@494 700@432 600@370 400@248 500@309"!
!
