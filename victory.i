victory_header:
    .byte $04           ;4 streams
    
    .byte SFX_1         ;which stream
    .byte $01           ;status byte (stream enabled)
    .byte SQUARE_1      ;which channel
    .byte $70           ;duty (01)
    .byte ve_short_staccato  ;volume envelope
    .word victory_square1 ;pointer to stream
    .byte $80           ;tempo
    
victory_square1:
    .byte sixteenth
    .byte C5, E5, G4, C4
    .byte endsound