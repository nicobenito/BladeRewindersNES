  .inesprg 1   ; 1x 16KB PRG code
  .ineschr 1   ; 1x  8KB CHR data
  .inesmap 0   ; mapper 0 = NROM, no bank swapping
  .inesmir 1   ; background mirroring
  

;;;;;;;;;;;;;;;

;; DECLARE SOME VARIABLES HERE
  .rsset $0000  ;;start variables at ram location 0
  
pointerLo  .rs 1   ; pointer variables are declared in RAM
pointerHi  .rs 1   ; low byte first, high byte immediately after
gamestate  .rs 1  ; .rs 1 means reserve one byte of space
posx      .rs 1  ; ball horizontal position
posy      .rs 1  ; ball vertical position
playerCoorX  .rs 1; coordinate x 1-8
playerCoorY .rs 1; coordinate y 1-8
playerPossibleCoorX .rs 1 ;next move
playerPossibleCoorY .rs 1 ; next move
canMove   .rs 1 ; flag
boardLength .rs 1
boardHeight .rs 1
ballup     .rs 1  ; 1 = ball moving up
balldown   .rs 1  ; 1 = ball moving down
ballleft   .rs 1  ; 1 = ball moving left
ballright  .rs 1  ; 1 = ball moving right
ballspeedx .rs 1  ; ball horizontal speed per frame
ballspeedy .rs 1  ; ball vertical speed per frame
paddle1ytop   .rs 1  ; player 1 paddle top vertical position
paddle2ybot   .rs 1  ; player 2 paddle bottom vertical position
buttons1   .rs 1  ; player 1 gamepad buttons, one bit per button
buttons2   .rs 1  ; player 2 gamepad buttons, one bit per button
buttons1pre .rs 1; player 1 pre state for release
score1     .rs 1  ; player 1 score, 0-15
score2     .rs 1  ; player 2 score, 0-15
levelNumber .rs 1 ; level number 0-?
levelSprites .rs 2; two bytes pointer/address
spritesAmount .rs 1; total number of sprites on a level
levelBlocks .rs 2; two bytes pointer for blocks, is required pointer for each type?
blocksAmount .rs 1; total number of blocks per level, used to limit loop
exitX     .rs 1
exitY     .rs 1
;test
blockX  .rs 1
blockY  .rs 1
blockCoorX  .rs 1
blockCoorY  .rs 1


;; DECLARE SOME CONSTANTS HERE
STATETITLE     = $00  ; displaying title screen
STATEPLAYING   = $01  ; move paddles/ball, check for collisions
STATEGAMEOVER  = $02  ; displaying game over screen
  
RIGHTWALL      = $F4  ; when ball reaches one of these, do something
TOPWALL        = $20
BOTTOMWALL     = $E0
LEFTWALL       = $04
  
PADDLE1X       = $08  ; horizontal position for paddles, doesnt move
PADDLE2X       = $F0

PLAYERY        = $0200
PLAYERX        = $0203
PLAYERHMOV     = $10
PLAYERYMOV     = $08

;;;;;;;;;;;;;;;;;;




  .bank 0
  .org $C000 
RESET:
  SEI          ; disable IRQs
  CLD          ; disable decimal mode
  LDX #$40
  STX $4017    ; disable APU frame IRQ
  LDX #$FF
  TXS          ; Set up stack
  INX          ; now X = 0
  STX $2000    ; disable NMI
  STX $2001    ; disable rendering -- turn off PPU
  STX $4010    ; disable DMC IRQs

vblankwait1:       ; First wait for vblank to make sure PPU is ready
  BIT $2002
  BPL vblankwait1

clrmem:
  LDA #$00
  STA $0000, x
  STA $0100, x
  STA $0300, x
  STA $0400, x
  STA $0500, x
  STA $0600, x
  STA $0700, x
  LDA #$FE
  STA $0200, x
  INX
  BNE clrmem
   
vblankwait2:      ; Second wait for vblank, PPU is ready after this
  BIT $2002
  BPL vblankwait2


LoadPalettes:
  LDA $2002             ; read PPU status to reset the high/low latch
  LDA #$3F
  STA $2006             ; write the high byte of $3F00 address
  LDA #$00
  STA $2006             ; write the low byte of $3F00 address
  LDX #$00              ; start out at 0
LoadPalettesLoop:
  LDA palette, x        ; load data from address (palette + the value in x)
                          ; 1st time through loop it will load palette+0
                          ; 2nd time through loop it will load palette+1
                          ; 3rd time through loop it will load palette+2
                          ; etc
  STA $2007             ; write to PPU
  INX                   ; X = X + 1
  CPX #$20              ; Compare X to hex $10, decimal 16 - copying 16 bytes = 4 sprites
  BNE LoadPalettesLoop  ; Branch to LoadPalettesLoop if compare was Not Equal to zero
                        ; if compare was equal to 32, keep going down

  LDA #$00
  STA levelNumber

  JSR LoadLevel

;;;Set some initial ball stats
  LDA #$01
  STA balldown
  STA ballright
  LDA #$00
  STA ballup
  STA ballleft
  
  ; set screen player position
  LDA #$A3
  STA posy  
  LDA #$68
  STA posx

  ;TEST
  ; LDA #$83
  ; STA blockY
  ; LDA #$6C
  ; STA blockX
  ; ;block coors
  ; LDA #$04
  ; STA blockCoorX
  ; STA blockCoorY

  ;set coordinates
  LDA #$01
  STA playerCoorX
  STA playerCoorY
  ;set board
  LDA #$07
  STA boardHeight
  LDA #$06
  STA boardLength
  
  LDA #$02
  STA ballspeedx
  STA ballspeedy

  LDA #$06
  STA exitX
  STA exitY


;;:Set starting game state
  LDA #STATEPLAYING
  STA gamestate


              
  LDA #%10010000   ; enable NMI, sprites from Pattern Table 0, background from Pattern Table 1
  STA $2000

  LDA #%00011110   ; enable sprites, enable background, no clipping on left side
  STA $2001

Forever:
  JMP Forever     ;jump back to Forever, infinite loop, waiting for NMI
  
 

NMI:
  LDA #$00
  STA $2003       ; set the low byte (00) of the RAM address
  LDA #$02
  STA $4014       ; set the high byte (02) of the RAM address, start the transfer

  JSR DrawScore

  ;;This is the PPU clean up section, so rendering the next frame starts properly.
  LDA #%10010000   ; enable NMI, sprites from Pattern Table 0, background from Pattern Table 1
  STA $2000
  LDA #%00011110   ; enable sprites, enable background, no clipping on left side
  STA $2001
  LDA #$00        ;;tell the ppu there is no background scrolling
  STA $2005
  STA $2005
    
  ;;;all graphics updates done by here, run game engine


  JSR ReadController1  ;;get the current button data for player 1
  JSR ReadController2  ;;get the current button data for player 2
  
GameEngine:  
  LDA gamestate
  CMP #STATETITLE
  BEQ EngineTitle    ;;game is displaying title screen
    
  LDA gamestate
  CMP #STATEGAMEOVER
  BEQ EngineGameOver  ;;game is displaying ending screen
  
  LDA gamestate
  CMP #STATEPLAYING
  BEQ EnginePlaying   ;;game is playing
GameEngineDone:  
  
  JSR UpdateSprites  ;;set ball/paddle sprites from positions

  RTI             ; return from interrupt
 
 
 
 
;;;;;;;;
 
EngineTitle:
  ;;if start button pressed
  ;;  turn screen off
  ;;  load game screen
  ;;  set starting paddle/ball position
  ;;  go to Playing State
  ;;  turn screen on
  JMP GameEngineDone

;;;;;;;;; 
 
EngineGameOver:
  ;;if start button pressed
  ;;  turn screen off
  ;;  load title screen
  ;;  go to Title State
  ;;  turn screen on 
  JMP GameEngineDone
 
;;;;;;;;;;;
 
EnginePlaying:

; MoveBallRight:
;   LDA ballright
;   BEQ MoveBallRightDone   ;;if ballright=0, skip this section

;   LDA ballx
;   CLC
;   ADC ballspeedx        ;;ballx position = ballx + ballspeedx
;   STA ballx

;   LDA ballx
;   CMP #RIGHTWALL
;   BCC MoveBallRightDone      ;;if ball x < right wall, still on screen, skip next section
;   LDA #$00
;   STA ballright
;   LDA #$01
;   STA ballleft         ;;bounce, ball now moving left
;   ;;in real game, give point to player 1, reset ball
; MoveBallRightDone:


; MoveBallLeft:
;   LDA ballleft
;   BEQ MoveBallLeftDone   ;;if ballleft=0, skip this section

;   LDA ballx
;   SEC
;   SBC ballspeedx        ;;ballx position = ballx - ballspeedx
;   STA ballx

;   LDA ballx
;   CMP #LEFTWALL
;   BCS MoveBallLeftDone      ;;if ball x > left wall, still on screen, skip next section
;   LDA #$01
;   STA ballright
;   LDA #$00
;   STA ballleft         ;;bounce, ball now moving right
;   ;;in real game, give point to player 2, reset ball
; MoveBallLeftDone:


; MoveBallUp:
;   LDA ballup
;   BEQ MoveBallUpDone   ;;if ballup=0, skip this section

;   LDA bally
;   SEC
;   SBC ballspeedy        ;;bally position = bally - ballspeedy
;   STA bally

;   LDA bally
;   CMP #TOPWALL
;   BCS MoveBallUpDone      ;;if ball y > top wall, still on screen, skip next section
;   LDA #$01
;   STA balldown
;   LDA #$00
;   STA ballup         ;;bounce, ball now moving down
; MoveBallUpDone:


; MoveBallDown:
;   LDA balldown
;   BEQ MoveBallDownDone   ;;if ballup=0, skip this section

;   LDA bally
;   CLC
;   ADC ballspeedy        ;;bally position = bally + ballspeedy
;   STA bally

;   LDA bally
;   CMP #BOTTOMWALL
;   BCC MoveBallDownDone      ;;if ball y < bottom wall, still on screen, skip next section
;   LDA #$00
;   STA balldown
;   LDA #$01
;   STA ballup         ;;bounce, ball now moving down
; MoveBallDownDone:

MovePaddleUp:
  ;;if up button pressed
  ;;  if paddle top > top wall
  ;;    move paddle top and bottom up
MovePaddleUpDone:

MovePaddleDown:
  ;;if down button pressed
  ;;  if paddle bottom < bottom wall
  ;;    move paddle top and bottom down
MovePaddleDownDone:
  
CheckPaddleCollision:
  ;;if ball x < paddle1x
  ;;  if ball y > paddle y top
  ;;    if ball y < paddle y bottom
  ;;      bounce, ball now moving left
CheckPaddleCollisionDone:

ReadLeftBtn:
  LDA buttons1
  AND #%00000010
  BEQ ReadLeftBtnDone ;btn not pressed
  LDA buttons1pre
  AND #%00000010
  BEQ ReadLeftBtnDone ;btn still
  ; check coordinate X
  LDA playerCoorX
  SEC
  SBC boardLength
  BEQ ReadLeftBtnDone
  ; check blocks
  LDA playerCoorX
  ADC #$01
  STA playerPossibleCoorX
  LDA playerCoorY
  STA playerPossibleCoorY
  JSR CheckNextPosition
  LDA canMove
  CMP #$01
  BNE ReadRightBtnDone
  LDA buttons1
  STA buttons1pre
  LDA posx
  SEC             
  SBC #PLAYERHMOV
  STA posx
  CLC
  LDA posy
  SEC             
  SBC #PLAYERYMOV
  STA posy
  CLC
  LDA playerCoorX
  ADC #$01
  STA playerCoorX
  ; LDA ballspeedx
  ; CMP #$00
  ; BEQ IncSpeed
  ; CLC
  ; BCC FreezeBall
; IncSpeed:
;   LDA #$02
;   STA ballspeedx
;   STA ballspeedy
;   CLC
;   BCC ReadLeftBtnDone
; FreezeBall:
;   LDA #$00
;   STA ballspeedx
;   STA ballspeedy
ReadLeftBtnDone:
  ; LDA buttons1
  ; ;AND #%11111101
  ; STA buttons1pre

ReadRightBtn:
  LDA buttons1
  AND #%00000001
  BEQ ReadRightBtnDone ;btn not pressed
  LDA buttons1pre
  AND #%00000001
  BEQ ReadRightBtnDone ;btn still
  ; check coordinate X
  LDA playerCoorX
  SEC
  SBC #$01
  BEQ ReadRightBtnDone
  ; check blocks
  LDA playerCoorX
  SEC
  SBC #$01
  STA playerPossibleCoorX
  LDA playerCoorY
  STA playerPossibleCoorY
  JSR CheckNextPosition
  LDA canMove
  CMP #$01
  BNE ReadRightBtnDone
  LDA buttons1
  STA buttons1pre
  CLC
  LDA posx
  ADC #PLAYERHMOV
  STA posx
  CLC
  LDA posy
  ADC #PLAYERYMOV
  STA posy
  LDA playerCoorX
  SEC
  SBC #$01
  STA playerCoorX
ReadRightBtnDone:
  ;LDA buttons1
  ; AND #%11111110
  ; ;EOR #$01
  ; STA buttons1pre

ReadDownBtn:
  LDA buttons1
  AND #%00000100
  BEQ ReadDownBtnDone ;btn not pressed
  LDA buttons1pre
  AND #%00000100
  BEQ ReadDownBtnDone ;btn still
  ; check coordinate Y
  LDA playerCoorY
  SEC
  SBC #$01
  BEQ ReadDownBtnDone
  ; check blocks
  LDA playerCoorY
  SEC
  SBC #$01
  STA playerPossibleCoorY
  LDA playerCoorX
  STA playerPossibleCoorX
  JSR CheckNextPosition
  LDA canMove
  CMP #$01
  BNE ReadDownBtnDone
  LDA buttons1
  STA buttons1pre
  CLC
  LDA posy
  ADC #PLAYERYMOV
  STA posy
  LDA posx
  CLC
  SEC             
  SBC #PLAYERHMOV
  STA posx
  LDA playerCoorY
  SEC
  SBC #$01
  STA playerCoorY
ReadDownBtnDone:
  ; LDA buttons1
  ; EOR #$04
  ; STA buttons1pre

ReadUpBtn:
  LDA buttons1
  AND #%00001000
  BEQ ReadUpBtnDone ;btn not pressed
  LDA buttons1pre
  AND #%00001000
  BEQ ReadUpBtnDone ;btn still
  ; check coordinate Y
  LDA playerCoorY
  SEC
  SBC boardHeight
  BEQ ReadUpBtnDone
  ; check blocks
  LDA playerCoorY
  ADC #$01
  STA playerPossibleCoorY
  LDA playerCoorX
  STA playerPossibleCoorX
  JSR CheckNextPosition
  LDA canMove
  CMP #$01
  BNE ReadUpBtnDone
  LDA buttons1
  STA buttons1pre
  LDA posy
  SEC             
  SBC #PLAYERYMOV
  STA posy
  LDA posx
  CLC
  ADC #PLAYERHMOV
  STA posx
  LDA playerCoorY
  ADC #$01
  STA playerCoorY
ReadUpBtnDone:
  ; LDA buttons1
  ; EOR #$08
  ; STA buttons1pre

  LDA buttons1
  EOR #$FF
  STA buttons1pre
  JSR CheckIfExit
  JMP GameEngineDone

UpdateSprites:
  LDA posx
  STA PLAYERX
  LDX #$08
  STA PLAYERX, X
  LDX #$10
  STA PLAYERX, X
  CLC
  ADC #$08
  LDX #$04
  STA PLAYERX, X
  LDX #$0C
  STA PLAYERX, X
  LDX #$14
  STA PLAYERX, X
  LDA posy
  STA PLAYERY
  LDX #$04
  STA PLAYERY, X
  CLC
  ADC #$08
  LDX #$08
  STA PLAYERY, X
  LDX #$0C
  STA PLAYERY, X
  CLC
  ADC #$08
  LDX #$10
  STA PLAYERY, X
  LDX #$14
  STA PLAYERY, X
  ; LDA bally  ;;update all ball sprite info
  ; STA $0200
  
  ; LDA #$00
  ; STA $0201
  
  ; LDA #$00
  ; STA $0202
  
  ; LDA ballx
  ; STA $0203

  ; LDA bally  
  ; STA $0204
  
  ; LDA #$01
  ; STA $0205
  
  ; LDA #$00
  ; STA $0206
  
  ; clc
  ; lda ballx
  ; adc #$08 
  ; STA $0207
  
  ;;update paddle sprites
  RTS
 
DrawScore:
  ;;draw score on screen using background tiles
  ;;or using many sprites
  RTS
 
CheckNextPosition:
  LDA #$01
  STA canMove
  LDX #$00
  LDY #$00  
BlockLoop:
  INX
  LDA [levelBlocks], Y
  INY
  CMP playerPossibleCoorX
  BNE BlockLoopContinue ;X pos different
  LDA [levelBlocks], Y
  CMP playerPossibleCoorY
  BNE BlockLoopContinue ;Y pos different
  LDA #$00
  STA canMove
  RTS
BlockLoopContinue:
  INY
  INY
  INY
  CPX blocksAmount
  BNE BlockLoop
  RTS

CheckIfExit:
  LDA playerCoorX
  CMP exitX
  BNE CheckIfExitDone
  LDA playerCoorY
  CMP exitY
  BNE CheckIfExitDone
  JSR LoadNxtLevel
CheckIfExitDone:
  RTS

LoadNxtLevel:
  LDA levelNumber
  CLC
  ADC #$01
  STA levelNumber
  ; reset player pos
  LDA #$01
  STA playerCoorX
  STA playerCoorY
  LDA #$A3
  STA posy  
  LDA #$68
  STA posx
  ; turn PPU off
  LDA #$00
  STA $2001
  ; draw new lvl
  JSR LoadLevel
  ; turn PPU on
  LDA #%00011110   ; enable sprites, enable background, no clipping on left side
  STA $2001
  RTS

ReadController1:
  LDA #$01
  STA $4016
  LDA #$00
  STA $4016
  LDX #$08
ReadController1Loop:
  LDA $4016
  LSR A            ; bit0 -> Carry
  ROL buttons1     ; bit0 <- Carry
  DEX
  BNE ReadController1Loop
  RTS
  
ReadController2:
  LDA #$01
  STA $4016
  LDA #$00
  STA $4016
  LDX #$08
ReadController2Loop:
  LDA $4017
  LSR A            ; bit0 -> Carry
  ROL buttons2     ; bit0 <- Carry
  DEX
  BNE ReadController2Loop
  RTS  

LoadLevel:
  lda levelNumber ;gets the number of the current level
	asl A ;multiplies it by 2 since each pointer is 2 bytes
	tax ;use it as an index
	lda spritesPointers+0, x ;copies the low byte to ZP
	sta levelSprites+0
	lda spritesPointers+1, x ;copies the high byte to ZP
	sta levelSprites+1
  LDY levelNumber
  LDA spritesTotalPerLvl, y
  STA spritesAmount
  ; set blocks
  lda blocksPointers+0, X
  sta levelBlocks+0
  lda blocksPointers+1, X
  sta levelBlocks+1
  LDA blocksTotalPerLvl, y
  sta blocksAmount

LoadSprites:
  LDY #$00              ; start at 0
LoadSpritesLoop:
  LDA [levelSprites], y        ; load data from address (sprites +  x)
  STA $0200, y          ; store into RAM address ($0200 + x)
  INY                   ; X = X + 1
  CPY spritesAmount     ; Compare X to hex $10, decimal 16
  BNE LoadSpritesLoop   ; Branch to LoadSpritesLoop if compare was Not Equal to zero
                        ; if compare was equal to 16, keep going down

LoadBackground:
  LDA $2002             ; read PPU status to reset the high/low latch
  LDA #$20
  STA $2006             ; write the high byte of $2000 address
  LDA #$00
  STA $2006             ; write the low byte of $2000 address

  LDA #$00
  STA pointerLo       ; put the low byte of the address of background into pointer
  LDA #HIGH(background)
  STA pointerHi       ; put the high byte of the address into pointer
  
  LDX #$00            ; start at pointer + 0
  LDY #$00
OutsideLoop:
  
InsideLoop:
  LDA [pointerLo], y  ; copy one background byte from address in pointer plus Y
  STA $2007           ; this runs 256 * 4 times
  
  INY                 ; inside loop counter
  CPY #$00
  BNE InsideLoop      ; run the inside loop 256 times before continuing down
  
  INC pointerHi       ; low byte went 0 to 256, so high byte needs to be changed now
  
  INX
  CPX #$04
  BNE OutsideLoop     ; run the outside loop 256 times before continuing down
  RTS
  
    
        
;;;;;;;;;;;;;;  
  
  
  
  .bank 1
  .org $E000

background:
  incbin "bladerewindertest.nam"

palette:
  .db $22,$29,$1A,$0F,  $22,$36,$17,$0F,  $22,$30,$21,$0F,  $22,$27,$17,$0F   ;;background palette
  .db $22,$13,$23,$33,  $22,$02,$38,$3C,  $22,$13,$23,$33,  $22,$1C,$20,$2B   ;;sprite palette

spritesTotalPerLvl: ;value multiplied by four because of attributes
  .db $26, $20, $38

spritesLvl1:
     ;vert tile attr horiz
  .db $80, $50, $03, $80
  .db $80, $51, $03, $88
  .db $88, $60, $03, $80
  .db $88, $61, $03, $88
  .db $90, $70, $03, $80
  .db $90, $71, $03, $88
  ;.db $80, $40, $00, $80   ;sprite 0
  .db $83, $41, $00, $6C   ;sprite 1
  .db $73, $41, $00, $8C   ;sprite 1
  .db $8B, $41, $00, $9C   ;sprite 1

spritesLvl2:
     ;vert tile attr horiz
  .db $80, $50, $03, $80
  .db $80, $51, $03, $88
  .db $88, $60, $03, $80
  .db $88, $61, $03, $88
  .db $90, $70, $03, $80
  .db $90, $71, $03, $88
  .db $83, $41, $00, $6C   ;sprite 1
  .db $8B, $41, $00, $9C   ;sprite 1

spritesLvl3:
     ;vert tile attr horiz
  .db $80, $50, $03, $80
  .db $80, $51, $03, $88
  .db $88, $60, $03, $80
  .db $88, $61, $03, $88
  .db $90, $70, $03, $80
  .db $90, $71, $03, $88
  .db $83, $41, $00, $4C   ;sprite 1
  .db $7B, $41, $00, $5C   ;sprite 1
  .db $73, $41, $00, $6C   ;sprite 1
  .db $6B, $41, $00, $7C   ;sprite 1
  .db $93, $41, $00, $6C   ;sprite 1
  .db $8B, $41, $00, $7C   ;sprite 1
  .db $83, $41, $00, $8C   ;sprite 1
  .db $7B, $41, $00, $9C   ;sprite 1

blocksTotalPerLvl: ;no need to multiply, I'm jumping over extra values
  .db $03, $02, $08

blocksLvl1:
  .db $04, $04, $6C, $83
  .db $04, $06, $8C, $73
  .db $02, $05, $8C, $73

blocksLvl2:
  .db $04, $04, $6C, $83
  .db $02, $05, $8C, $73

blocksLvl3:
  .db $05, $03, $6C, $83
  .db $05, $04, $8C, $73
  .db $05, $05, $8C, $73
  .db $05, $06, $6C, $83
  .db $03, $03, $8C, $73
  .db $03, $04, $8C, $73
  .db $03, $05, $6C, $83
  .db $03, $06, $8C, $73

spritesPointers:
  .dw spritesLvl1
  .dw spritesLvl2
  .dw spritesLvl3

blocksPointers:
  .dw blocksLvl1
  .dw blocksLvl2
  .dw blocksLvl3

  .org $FFFA     ;first of the three vectors starts here
  .dw NMI        ;when an NMI happens (once per frame if enabled) the 
                   ;processor will jump to the label NMI:
  .dw RESET      ;when the processor first turns on or is reset, it will jump
                   ;to the label RESET:
  .dw 0          ;external interrupt IRQ is not used in this tutorial
  
  
;;;;;;;;;;;;;;  
  
  
  .bank 2
  .org $0000
  .incbin "bladerewinder.chr"   ;includes 8KB graphics file from SMB1