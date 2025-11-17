;Pilar's Theme - Minimal romantic melody
;Ultra-short 4-bar loop to fit in memory

song7_header:
    .byte 3          ;3 streams (Square 1, Square 2, Triangle)
    
    .byte MUSIC_SQ1
    .byte $01        ;enable
    .byte SQUARE_1   ;channel
    .byte $70        ;duty 01
    .byte ve_tgl_1   ;volume envelope
    .word song7_square1
    .byte $50        ;tempo
    
    .byte MUSIC_SQ2
    .byte $01        ;enable
    .byte SQUARE_2   ;channel
    .byte $B0        ;duty 10
    .byte ve_tgl_2   ;volume envelope
    .word song7_square2
    .byte $50        ;tempo
    
    .byte MUSIC_TRI
    .byte $01        ;enable
    .byte TRIANGLE   ;channel
    .byte $80        ;volume on
    .byte ve_tgl_2   ;volume envelope
    .word song7_triangle
    .byte $50        ;tempo

;Main melody - minimal
song7_square1:
    .byte whole
    .byte E4, G4, A4, G4
    .byte loop
    .word song7_square1

;Harmony - minimal
song7_square2:
    .byte whole
    .byte C4, E4, F4, E4
    .byte loop
    .word song7_square2

;Bass - minimal
song7_triangle:
    .byte whole
    .byte C3, C3, A2, C3
    .byte loop
    .word song7_triangle
