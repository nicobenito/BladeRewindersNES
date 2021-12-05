pause_header:
    .byte $01           ;1 stream
    
    .byte SFX_1         ;which stream
    .byte $01           ;status byte (stream enabled)
    .byte SQUARE_2      ;which channel
    .byte $70           ;duty (01)
    .byte ve_short_staccato  ;volume envelope
    .word sfx_pause ;pointer to stream
    .byte $80           ;tempo
    
    
sfx_pause:
    .byte sixteenth, F2, F2
    .byte endsound