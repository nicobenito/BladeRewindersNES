;Main Game Action Theme
;Fast-paced, energetic melody for gameplay

song8_header:
    .byte 3          ;3 streams (Square 1, Square 2, Triangle)
    
    .byte MUSIC_SQ1
    .byte $01        ;enable
    .byte SQUARE_1   ;channel
    .byte $70        ;duty 01
    .byte ve_tgl_1   ;volume envelope
    .word song8_square1
    .byte $30        ;tempo (faster for action)
    
    .byte MUSIC_SQ2
    .byte $01        ;enable
    .byte SQUARE_2   ;channel
    .byte $B0        ;duty 10
    .byte ve_tgl_2   ;volume envelope
    .word song8_square2
    .byte $30        ;tempo (faster for action)
    
    .byte MUSIC_TRI
    .byte $01        ;enable
    .byte TRIANGLE   ;channel
    .byte $80        ;volume on
    .byte ve_tgl_2   ;volume envelope
    .word song8_triangle
    .byte $30        ;tempo (faster for action)

;Main melody - energetic and rhythmic
song8_square1:
    .byte quarter
    .byte E4, E4, G4, A4, G4, E4, D4, C4
    .byte E4, E4, G4, A4, G4, A4, G4, E4
    .byte loop
    .word song8_square1

;Harmony - supporting melody
song8_square2:
    .byte quarter
    .byte C4, C4, E4, F4, E4, C4, A3, G3
    .byte C4, C4, E4, F4, E4, F4, E4, C4
    .byte loop
    .word song8_square2

;Bass - driving rhythm
song8_triangle:
    .byte quarter
    .byte C3, G2, C3, G2, A2, G2, A2, C3
    .byte C3, G2, C3, G2, A2, C3, G2, C3
    .byte loop
    .word song8_triangle

