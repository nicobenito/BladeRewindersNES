rewind_header:
    .byte $01           ;1 stream
    
    .byte SFX_2         ;which stream
    .byte $01           ;status byte (stream enabled)
    .byte SQUARE_1     ;which channel
    .byte $70           ;duty (01)
    .byte ve_short_staccato  ;volume envelope
    .word sfx_rewind ;pointer to stream
    .byte $80           ;tempo
    
    
sfx_rewind:
    .byte thirtysecond, C3, D3, E3, F3, G3, A3, B3
    .byte endsound