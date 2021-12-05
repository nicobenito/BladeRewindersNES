catch_header:
    .byte $01           ;1 stream
    
    .byte SFX_1         ;which stream
    .byte $01           ;status byte (stream enabled)
    .byte SQUARE_1      ;which channel
    .byte $70           ;duty (01)
    .byte ve_short_staccato  ;volume envelope
    .word sfx_catch ;pointer to stream
    .byte $80           ;tempo
    
    
sfx_catch:
    .byte sixteenth, E3, C3, Cs3
    .byte endsound