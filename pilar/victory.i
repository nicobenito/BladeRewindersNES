victory_header:
    .byte $01           ;4 streams
    
    .byte MUSIC_TRI     ;which stream
    .byte $01           ;status byte (stream enabled)
    .byte TRIANGLE      ;which channel
    .byte $80           ;initial volume (on)
    .byte ve_tgl_2      ;volume envelope
    .word victory_tri1  ;pointer to stream
    .byte $53           ;tempo
    
victory_tri1:
    .byte sixteenth
    .byte C4, thirtysecond, rest, sixteenth, C4, D4, E4, F4
    .byte endsound