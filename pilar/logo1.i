logo1_header:
    .byte $04           ;4 streams
    
    .byte MUSIC_SQ1     ;which stream
    .byte $01           ;status byte (stream enabled)
    .byte SQUARE_1      ;which channel
    .byte $B0           ;initial duty (10)
    .byte ve_blip_echo  ;volume envelope
    .word logo1_square1 ;pointer to stream
    .byte $60           ;tempo
    
    .byte MUSIC_SQ2     ;which stream
    .byte $01           ;status byte (stream enabled)
    .byte SQUARE_2      ;which channel
    .byte $30           ;initial duty (00)
    .byte ve_blip_echo ;volume envelope
    .word logo1_square2 ;pointer to stream
    .byte $60           ;tempo
    
    .byte MUSIC_TRI     ;which stream
    .byte $01           ;status byte (stream enabled)
    .byte TRIANGLE      ;which channel
    .byte $81           ;initial volume (on)
    .byte ve_blip_echo  ;volume envelope
    .word logo1_tri     ;pointer to stream
    .byte $60           ;tempo
    
    .byte MUSIC_NOI     ;which stream
    .byte $00           ;disabled.  Our load routine will skip the
                        ;   rest of the reads if the status byte is 0.
                        ;   We are disabling Noise because we haven't covered it yet.

logo1_square1:
    .byte sixteenth, rest, eighth, C3, G3, D4, E4, half, B4
    .byte endsound

logo1_square2:
    .byte eighth, C3, G3, D4, E4, half, B4
    .byte endsound
    
logo1_tri:
    .byte half, C3, half, B3
    .byte endsound