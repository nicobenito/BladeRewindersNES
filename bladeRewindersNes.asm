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
buttons1   .rs 1  ; player 1 gamepad buttons, one bit per button
buttons2   .rs 1  ; player 2 gamepad buttons, one bit per button
buttons1pre .rs 1; player 1 pre state for release
levelNumber .rs 1 ; level number 0-?
levelSprites .rs 2; two bytes pointer/address
levelBackground .rs 2; two bytes pointer for BG
spritesAmount .rs 1; total number of sprites on a level
levelBlocks .rs 2; two bytes pointer for blocks, is required pointer for each type? depends, on this case it is because it needs to keep value for later use
blocksAmount .rs 1; total number of blocks per level, used to limit loop
levelButtons .rs 2; buttons/elements of level.
buttonsAmount .rs 1
levelBRs .rs 2
brsAmount .rs 1
exitX     .rs 1
exitY     .rs 1
brOnePosX .rs 1
brOnePosY .rs 1
brOneCoorX .rs 1
brOneCoorY .rs 1
brOnePaused .rs 1
brTwoPosX .rs 1
brTwoPosY .rs 1
brTwoCoorX .rs 1
brTwoCoorY .rs 1
brTwoPaused .rs 1
brCurrentPosX .rs 1 ;current br being modified, then, results are applied on ONE or TWO
brCurrentPosY .rs 1
brCurrentCoorX .rs 1
brCurrentCoorY .rs 1
brCurrentPaused .rs 1
brPossibleUp .rs 1
brPossibleLeft .rs 1
brPossibleDown .rs 1
brPossibleRight  .rs 1
brSmallerDistance .rs 1
;multiplication and square root
xone .rs 1
yone .rs 1
xtwo .rs 1
ytwo .rs 1
xTotal .rs 1
yTotal .rs 1
rootRegisterD .rs 1
numberToRoot .rs 1
rootRegisterE .rs 1
rootRemanent .rs 1
rootResult .rs 1
multTempResult .rs 1
multResultOne .rs 1
multResultTwo .rs 1
numberToMult .rs 1
playerHasWon .rs 1
playerLost .rs 1
currentCharacterCoorX .rs 1
currentCharacterCoorY .rs 1
currentCharacterPaused .rs 1
currentIsPlayer .rs 1
brOneLastCoorX .rs 1
brOneLastCoorY .rs 1
brOneLastPosX .rs 1
brOneLastPosY .rs 1
brTwoLastCoorX .rs 1
brTwoLastCoorY .rs 1
brTwoLastPosX .rs 1
brTwoLastPosY .rs 1
hasRewinded .rs 1
playerHasMove .rs 1
wcount .rs 1
initialBGNumber .rs 1 ; initial/other screens number
timerOn .rs 1; nes waiting test 
timerIsRunning .rs 1
charPosX .rs 1
charPosY .rs 1
charSpriteX .rs 2
charSpriteY .rs 2
;ppu buffer
;simple buffer text
letterCursor .rs 1
ppuCursorLow .rs 1
ppuCursorHigh .rs 1
writerWait .rs 1
; sound
sound_ptr .rs 2
jmp_ptr .rs 2           ;a pointer variable for indirect jumps
current_song .rs 1


;; DECLARE SOME CONSTANTS HERE
STATELOGO      = $03  ; display bencom logo screen
STATETITLE     = $00  ; displaying title screen
STATEPLAYING   = $01  ; move paddles/ball, check for collisions
STATEGAMEOVER  = $02  ; displaying game over screen
STATEINTRO     = $04  ; display intro text

PLAYERY        = $0200
PLAYERX        = $0203
PLAYERHMOV     = $10
PLAYERYMOV     = $08

BRONEY         = $0218
BRONEX         = $021B
BRTWOY         = $021C
BRTWOX         = $021F
CANTMOVE       = $10

PPU_BUFFER     = $0400

; buttons
PAUSEDBTN      = $01
REWINDBTN      = $02

; text
TEXTSIZE       = $8A

;;;;;;;;;;;;;;;;;;




  ; .bank 0
  ; .org $C000 
;----- first 8k bank of PRG-ROM
  .bank 0
  .org $8000  ;we have two 16k PRG banks now.  We will stick our sound engine in the first one, which starts at $8000.
  
  .include "sound_engine.asm"

;----- second 8k bank of PRG-ROM    
  ; .bank 1
  ; .org $A000

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

;Enable sound channels
  jsr sound_init
  
  lda #$01
  sta current_song


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

  LDA #$00 ;lvl number - 1
  STA levelNumber
  LDA #$00
  STA letterCursor

  LDA #$20
  STA ppuCursorHigh
  LDA #$40
  STA ppuCursorLow

  ;deactive intial screens
  ; JSR LoadLevel
  
  ; LOADING TITLE SCREEN
  LDA #$00
  STA initialBGNumber
  JSR LoadBlackScreen
  
  ; set screen player position
  LDA #$A3
  STA posy  
  LDA #$68
  STA posx

  LDA #$00
  STA playerHasWon

  ;set coordinates
  LDA #$01
  STA playerCoorX
  STA playerCoorY
  ;set board
  LDA #$07
  STA boardHeight
  LDA #$06
  STA boardLength

  LDA #$06
  STA exitX
  STA exitY


;;:Set starting game state
  LDA #STATELOGO
  ; LDA #STATEPLAYING
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

  ; JSR DrawScore
  JSR BufferToPPU

  ;;This is the PPU clean up section, so rendering the next frame starts properly.
  LDA #%10010000   ; enable NMI, sprites from Pattern Table 0, background from Pattern Table 1
  STA $2000
  LDA #%00011110   ; enable sprites, enable background, no clipping on left side
  STA $2001
  LDA #$00        ;;tell the ppu there is no background scrolling
  STA $2005
  STA $2005
    
  ;;;all graphics updates done by here, run game engine
  jsr sound_play_frame    ;run our sound engine after all drawing code is done. 
                            ;this ensures our sound engine gets run once per frame.

  JSR ReadController1  ;;get the current button data for player 1
  JSR ReadController2  ;;get the current button data for player 2
  
GameEngine:  
  LDA gamestate
  CMP #STATELOGO
  BEQ EngineLogo    ;;game is displaying logo screen

  LDA gamestate
  CMP #STATETITLE
  BEQ EngineTitle    ;;game is displaying title screen
    
  LDA gamestate
  CMP #STATEGAMEOVER
  BEQ EngineGameOver  ;;game is displaying ending screen
  
  LDA gamestate
  CMP #STATEPLAYING
  BNE .gameEngineContinue   ;;game is playing
  JMP EnginePlaying

.gameEngineContinue
  LDA gamestate
  CMP #STATEINTRO
  BEQ EngineIntro
GameEngineDone:  
  
  JSR UpdateSprites

  RTI             ; return from interrupt

EngineLogo: ; need to fix timer
  JSR TimeWait
  LDA wcount
  CMP #$00
  BNE EngineLogoDone
  ; ReadStartLogoBtn:
  ; LDA buttons1
  ; AND #%00100000
  ; BEQ EngineLogoDone ;btn not pressed
  LDA #$01
  STA initialBGNumber
  ; LDA gamestate
  ; turn PPU off
  LDA #$00
  STA $2001
  JSR LoadInitialBackground
  LDA #STATETITLE
  STA gamestate
  LDA #$09
  JSR sound_load
EngineLogoDone:
  JMP GameEngineDone

;;;;;;;;;

EngineTitle:
  ;;if start button pressed
  ;;  turn screen off
  ;;  load game screen
  ;;  set starting paddle/ball position
  ;;  go to Playing State
  ;;  turn screen on
ReadStartBtn:
  LDA buttons1
  AND #%00010000
  BEQ ReadStartBtnDone ;btn not pressed
  LDA #$02
  STA initialBGNumber
  ; LDA gamestate
  ; turn PPU off
  LDA #$00
  STA $2001
  JSR LoadBlackScreen
  LDA #STATEINTRO
  STA gamestate
ReadStartBtnDone:
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

EngineIntro:
  LDA writerWait
  CLC
  ADC #$01
  STA writerWait
  LDA writerWait
  CMP #$05
  BNE ReadIntroStartBtnDone
  LDY letterCursor
  LDA #LOW(firstText)
  STA pointerLo
  LDA #HIGH(firstText)
  STA pointerHi
  LDA [pointerLo], Y
  CMP #$24
  BEQ ReadIntroStartBtnDone
  LDA #$02
  JSR sound_load
  LDA letterCursor
  CMP #TEXTSIZE
  BNE ReadIntroStartBtnDone
ReadIntroStartBtn:
  LDA #$00 
  JSR sound_load
  LDA buttons1
  AND #%00010000
  BEQ ReadIntroStartBtnDone ;btn not pressed
  ; LDA gamestate
  ; turn PPU off
  LDA #$00
  STA $2001
  JSR LoadLevel
  LDA #STATEPLAYING
  STA gamestate
ReadIntroStartBtnDone:
  JMP GameEngineDone

EnginePlaying:
; check if player has lost, if yes, skip read arrows and only read Start for reset.
  LDA #$00
  STA playerHasMove
  STA hasRewinded
  LDA playerLost
  CMP #$01
  BNE ReadLeftBtn
  JMP SkipButtonRead
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
  BNE ReadLeftBtnDone
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
  LDA #$01
  STA playerHasMove
  ; ; test move BR ONE
  ; JSR CheckIfExit
  ; JSR MoveBRs
ReadLeftBtnDone:

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
  LDA #$01
  STA playerHasMove
  ; test move BR ONE
  ; JSR CheckIfExit
  ; JSR MoveBRs
ReadRightBtnDone:

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
  LDA #$01
  STA playerHasMove
  ; test move BR ONE
  ; JSR CheckIfExit
  ; JSR MoveBRs
ReadDownBtnDone:

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
  LDA #$01
  STA playerHasMove
  ; test move BR ONE
  ; JSR CheckIfExit
  ; JSR MoveBRs
ReadUpBtnDone:
  LDA playerHasMove
  CMP #$01
  BNE StoreMovement
  ; sound movement
  LDA #$02
  JSR sound_load
  JSR CheckIfExit
  LDA playerCoorX
  STA currentCharacterCoorX
  LDA playerCoorY
  STA currentCharacterCoorY
  LDA #$01
  STA currentIsPlayer
  JSR CheckForButtons
  LDA hasRewinded
  CMP #$01
  BEQ StoreMovement
  JSR MoveBRs
StoreMovement:
  LDA buttons1
  EOR #$FF
  STA buttons1pre
SkipButtonRead:
  LDA buttons1
  AND #%00010000
  BEQ GameEngineContinue
  LDA playerLost
  CMP #$01
  BNE GameEngineContinue
  JSR ResetLevel
GameEngineContinue:
  JMP GameEngineDone

UpdateSprites:
  LDA gamestate
  CMP #STATEPLAYING
  BNE UpdateSpritesDone
  ; update player
  LDA posx
  STA charPosX
  LDA posy
  STA charPosY
  LDA #LOW(PLAYERX)
  STA charSpriteX+0
  LDA #HIGH(PLAYERX)
  STA charSpriteX+1
  LDA #LOW(PLAYERY)
  STA charSpriteY+0
  LDA #HIGH(PLAYERY)
  STA charSpriteY+1
  JSR UpdateCharactersSprites
  ; update BR one
  LDA brOnePosX
  STA charPosX
  LDA brOnePosY
  STA charPosY
  LDA #LOW(BRONEX)
  STA charSpriteX+0
  LDA #HIGH(BRONEX)
  STA charSpriteX+1
  LDA #LOW(BRONEY)
  STA charSpriteY+0
  LDA #HIGH(BRONEY)
  STA charSpriteY+1
  JSR UpdateCharactersSprites
  ; update BR two
  LDA brsAmount
  CMP #$02
  BNE UpdateSpritesDone
  LDA brTwoPosX
  STA charPosX
  LDA brTwoPosY
  STA charPosY
  LDA #LOW(BRTWOX)
  STA charSpriteX+0
  LDA #HIGH(BRTWOX)
  STA charSpriteX+1
  LDA #LOW(BRTWOY)
  STA charSpriteY+0
  LDA #HIGH(BRTWOY)
  STA charSpriteY+1
  JSR UpdateCharactersSprites
UpdateSpritesDone:  
  RTS

UpdateCharactersSprites:
  LDY #$00
  LDA charPosX 
  STA [charSpriteX], Y
  LDY #$08
  STA [charSpriteX], Y
  LDY #$10
  STA [charSpriteX], Y
  CLC
  ADC #$08
  LDY #$04
  STA [charSpriteX], Y
  LDY #$0C
  STA [charSpriteX], Y
  LDY #$14
  STA [charSpriteX], Y
  LDA charPosY
  LDY #$00
  STA [charSpriteY], Y
  LDY #$04
  STA [charSpriteY], Y
  CLC
  ADC #$08
  LDY #$08
  STA [charSpriteY], Y
  LDY #$0C
  STA [charSpriteY], Y
  CLC
  ADC #$08
  LDY #$10
  STA [charSpriteY], Y
  LDY #$14
  STA [charSpriteY], Y
  RTS

DrawScore:
  ;;draw score on screen using background tiles
  ;;or using many sprites
  RTS

BufferToPPU:
  ; reads buffer
  ; if zero, done
  ; set count with length value
  ; set high and low byte
  ; loop comparing against count and filling ppu

  LDA gamestate
  CMP #STATEINTRO
  BNE BufferDone
  LDA writerWait
  CMP #$05
  BNE BufferDone
  LDA #$00
  STA writerWait
  ; read ppu cursor
  LDA letterCursor
  CMP #TEXTSIZE
  BEQ BufferDone
  ; set ppu address
  LDA $2002             ; read PPU status to reset the high/low latch
  LDA ppuCursorHigh
  STA $2006             ; write the high byte of $2000 address
  LDA ppuCursorLow
  STA $2006             ; write the low byte of $2000 address
  ; read letter buffer with letter cursor
  LDY letterCursor
  LDA #LOW(firstText)
  STA pointerLo
  LDA #HIGH(firstText)
  STA pointerHi
  LDA [pointerLo], Y
  STA $2007
  ; if 'space' add 1 to cursor
  ; if 'break' add 1 to high ppu?
  ; write to PPu
  ; add 1 to ppu cursor and 1 to letter buffer
  ; LDA [pointerLo], Y
  ; CMP #$24
  ; BEQ .writeContinue
  ; LDA #$02
  ; JSR sound_load
.writeContinue
  LDA letterCursor
  ADC #$01
  STA letterCursor
  LDA ppuCursorLow
  ADC #$01
  STA ppuCursorLow
  CMP #$00
  BNE BufferDone
  LDA ppuCursorHigh
  CLC
  ADC #$01
  STA ppuCursorHigh
BufferDone:
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
  STA canMove ; player wont move
  RTS
BlockLoopContinue:
  INY
  CPX blocksAmount
  BNE BlockLoop
  JSR CheckForBRs
  RTS

CheckForBRs:
  LDA brOneCoorX
  CMP playerPossibleCoorX
  BNE BrContinue ;x different
  LDA brOneCoorY
  CMP playerPossibleCoorY
  BNE BrContinue ; y different
  LDA #$00
  STA canMove
  RTS
BrContinue:
  LDA brsAmount
  CMP #$02
  BNE CheckBRDone
  LDA brTwoCoorX
  CMP playerPossibleCoorX
  BNE CheckBRDone
  LDA brTwoCoorY
  CMP playerPossibleCoorY
  BNE CheckBRDone
  LDA #$00
  STA canMove
CheckBRDone:
  RTS

CheckForButtons:
  LDX #$00
  LDY #$00
  STX currentCharacterPaused
CheckButtonsLoop:
  INX
  LDA [levelButtons], Y
  INY
  CMP currentCharacterCoorX
  BNE ButtonsLoopContinue ;X pos different
  LDA [levelButtons], Y
  CMP currentCharacterCoorY
  BNE BlockLoopContinue ;Y pos different
  ; if here is the same position that certain button
  ; get button type

  ; if 1 is paused
  ; if is player, do nothing
  ; set current Paused
  INY 
  LDA [levelButtons], Y
  CMP #PAUSEDBTN
  BNE CheckRewindBtn
  LDA currentIsPlayer
  CMP $01
  BEQ ButtonsLoopDone
  LDA #$01
  STA currentCharacterPaused
  ;pause sound
  LDA #$07
  JSR sound_load
  RTS
CheckRewindBtn:
  CMP #REWINDBTN
  BNE ButtonsLoopDone
  ; if 2 is rewind if its not player, do nothing
  LDA currentIsPlayer
  CMP #$01
  BNE ButtonsLoopDone
  ; set rewindaction
  JSR RewindBRs
  RTS
ButtonsLoopContinue:
  INY
  INY
  CPX buttonsAmount
  BNE CheckButtonsLoop
ButtonsLoopDone:
  RTS

RewindBRs:
  ; set rewindtrigger to prevent move
  LDA #$01
  STA hasRewinded
  ; get last position of one and apply
  LDA brOneLastCoorX
  STA brOneCoorX
  LDA brOneLastCoorY
  STA brOneCoorY
  LDA brOneLastPosX
  STA brOnePosX
  LDA brOneLastPosY
  STA brOnePosY
  ; if two, get last position of two and applied
  LDA brsAmount
  CMP #$01
  BEQ RewindDone
  LDA brTwoLastCoorX
  STA brTwoCoorX
  LDA brTwoLastCoorY
  STA brTwoCoorY
  LDA brTwoLastPosX
  STA brTwoPosX
  LDA brTwoLastPosY
  STA brTwoPosY
RewindDone:
  RTS


CheckNextPositionBR:
  LDA xone
  STA playerPossibleCoorX
  LDA yone
  STA playerPossibleCoorY
  JSR CheckNextPosition
  RTS

MoveBRs:
  LDA playerHasWon
  CMP #$01
  BNE ContinueMove
  JMP MoveDone
ContinueMove:
  LDA brOnePaused
  CMP #$01
  BEQ NextBrMove
  LDA brOnePosX
  STA brCurrentPosX
  LDA brOnePosY
  STA brCurrentPosY
  LDA brOneCoorX
  STA brCurrentCoorX
  LDA brOneCoorY
  STA brCurrentCoorY
  JSR MoveBROne
  ; store previous position
  LDA brOnePosX
  STA brOneLastPosX
  LDA brOnePosY
  STA brOneLastPosY
  LDA brOneCoorX
  STA brOneLastCoorX
  LDA brOneCoorY
  STA brOneLastCoorY
  ;apply results to brOne
  LDA brCurrentPosX
  STA brOnePosX
  LDA brCurrentPosY
  STA brOnePosY
  LDA brCurrentCoorX
  STA brOneCoorX
  LDA brCurrentCoorY
  STA brOneCoorY
  LDA brCurrentPaused
  STA brOnePaused
NextBrMove:
  LDA brsAmount
  CMP #$02
  BNE MoveBrsDone
  LDA brTwoPaused
  CMP #$01
  BEQ MoveBrsDone
  LDA brTwoPosX
  STA brCurrentPosX
  LDA brTwoPosY
  STA brCurrentPosY
  LDA brTwoCoorX
  STA brCurrentCoorX
  LDA brTwoCoorY
  STA brCurrentCoorY
  JSR MoveBROne
  ; store previous position
  LDA brTwoPosX
  STA brTwoLastPosX
  LDA brTwoPosY
  STA brTwoLastPosY
  LDA brTwoCoorX
  STA brTwoLastCoorX
  LDA brTwoCoorY
  STA brTwoLastCoorY
  ;apply results to brTwo
  LDA brCurrentPosX
  STA brTwoPosX
  LDA brCurrentPosY
  STA brTwoPosY
  LDA brCurrentCoorX
  STA brTwoCoorX
  LDA brCurrentCoorY
  STA brTwoCoorY
  LDA brCurrentPaused
  STA brTwoPaused

MoveBrsDone:
  RTS
  ;check BR total number on level
  ;if two, apply same on second

MoveBROne:
  JSR CheckForPlayer
  LDA playerLost
  CMP #$01
  BNE CalculateMoveUp
  JMP MoveDone
CalculateMoveUp:
  ;Get BR possible UP distance result
  LDA brCurrentCoorY
  CLC
  ADC #$01
  STA yone
  LDA brCurrentCoorX
  STA xone
  JSR CheckNextPositionBR
  LDX CANTMOVE
  STX brPossibleUp
  LDA canMove
  CMP #$01
  BNE CalculateMoveLeft ;need to skip calculatedistance if canMove is zero.
  JSR CalculateDistance
  LDA rootResult
  STA brPossibleUp ;save result as UP result
  ;Get BR possible LEFT distance result
CalculateMoveLeft:
  LDA brCurrentCoorX
  CLC
  ADC #$01
  STA xone
  LDA brCurrentCoorY
  STA yone
  JSR CheckNextPositionBR
  LDX CANTMOVE
  STX brPossibleLeft
  LDA canMove
  CMP #$01
  BNE CalculateMoveDown
  JSR CalculateDistance
  LDA rootResult
  STA brPossibleLeft
  ;JMP MoveDone
  ;Get BR possible DOWN distance result
CalculateMoveDown:
  LDA brCurrentCoorY
  SEC
  SBC #$01
  STA yone
  LDA brCurrentCoorX
  STA xone
  JSR CheckNextPositionBR
  LDX CANTMOVE
  STX brPossibleDown
  LDA canMove
  CMP #$01
  BNE CalculateMoveRight
  JSR CalculateDistance
  LDA rootResult
  STA brPossibleDown
  ; TEST ### HERE IT FAILS, leaving on LEFT it does not
  ;JMP MoveDone
  ;Get BR possible RIGHT distance result
CalculateMoveRight:
  LDA brCurrentCoorX
  SEC
  SBC #$01
  STA xone
  LDA brCurrentCoorY
  STA yone
  JSR CheckNextPositionBR
  LDX CANTMOVE
  STX brPossibleRight
  LDA canMove
  CMP #$01
  BNE SetComparing
  JSR CalculateDistance
  LDA rootResult
  STA brPossibleRight
SetComparing:
  ; save up as minor
  LDA brPossibleUp
  STA brSmallerDistance
StartComparing:
  ;TEST #####
  ;JMP MoveDone
  LDA brPossibleLeft
  SEC
  SBC brSmallerDistance
  BMI SaveLeftAsMinorDistance

    ; if negative flag branch to saveMinor
  ; compare minor to down
  LDA brPossibleDown
  SEC
  SBC brSmallerDistance
  BMI SaveDownAsMinorDistance
    ; if negative flag branch to saveMinor
  ; compare minor to right
  LDA brPossibleRight
  SEC
  SBC brSmallerDistance
  BMI SaveRightAsMinorDistance
  JMP ComparingDone ;smaller value was found or up was smaller and never branched
    ; if negative flag branch to saveMinor
  ;if minor has been found, jump to comparingDone
  ;save minor= STA minorValue, branch to step1
SaveLeftAsMinorDistance:
  LDA brPossibleLeft
  STA brSmallerDistance
  JMP StartComparing
SaveDownAsMinorDistance:
  LDA brPossibleDown
  STA brSmallerDistance
  JMP StartComparing
SaveRightAsMinorDistance:
  LDA brPossibleRight
  STA brSmallerDistance
  JMP StartComparing
ComparingDone:
  LDA brSmallerDistance
  ; test
  ;JMP MoveDone
  ;comparingDone
  ;get direction
    ;compare minorValue to up
  CMP brPossibleUp
  BEQ MoveBRUp
  CMP brPossibleLeft
  BEQ MoveBRLeft
  CMP brPossibleDown
  BEQ MoveBRDown
  CMP brPossibleRight
  BEQ MoveBRRight
MoveBRUp:
  LDA brCurrentPosY
  SEC
  SBC #PLAYERYMOV
  STA brCurrentPosY
  LDA brCurrentPosX
  CLC       
  ADC #PLAYERHMOV
  STA brCurrentPosX
  LDA brCurrentCoorY
  CLC
  ADC #$01
  STA brCurrentCoorY
  JMP MoveDone
MoveBRLeft:
  LDA brCurrentPosX
  SEC
  SBC #PLAYERHMOV
  STA brCurrentPosX
  LDA brCurrentPosY
  SEC      
  SBC #PLAYERYMOV
  STA brCurrentPosY
  LDA brCurrentCoorX
  CLC
  ADC #$01
  STA brCurrentCoorX
  JMP MoveDone
MoveBRDown:
  CLC
  LDA brCurrentPosY
  ADC #PLAYERYMOV
  STA brCurrentPosY
  LDA brCurrentPosX
  SEC      
  SBC #PLAYERHMOV
  STA brCurrentPosX
  LDA brCurrentCoorY
  SEC
  SBC #$01
  STA brCurrentCoorY
  JMP MoveDone
MoveBRRight:
  LDA brCurrentPosX
  CLC
  ADC #PLAYERHMOV
  STA brCurrentPosX
  LDA brCurrentPosY   
  CLC       
  ADC #PLAYERYMOV
  STA brCurrentPosY
  LDA brCurrentCoorX
  SEC
  SBC #$01
  STA brCurrentCoorX
  JMP MoveDone
MoveDone:
  LDA brCurrentCoorX
  STA currentCharacterCoorX
  LDA brCurrentCoorY
  STA currentCharacterCoorY
  LDA #$00
  STA currentIsPlayer
  JSR CheckForButtons
  LDA currentCharacterPaused
  STA brCurrentPaused
  CMP #$01
  BEQ AvoidCheckPlayer
  JSR CheckForPlayer
AvoidCheckPlayer:
  RTS

CheckForPlayer:
  LDA playerLost
  CMP #$01
  BEQ CheckPlayerDone
  ; (xa - xb) + (ya - yb)
  ; check result, if 1, is next to player
  LDA playerCoorX
  SEC
  SBC brCurrentCoorX
  STA xTotal
  BPL DoneWithX
  EOR #$FF
  CLC
  ADC #$01
  STA xTotal
DoneWithX:
  LDA playerCoorY
  SEC
  SBC brCurrentCoorY
  STA yTotal
  BPL DoneWithY
  EOR #$FF
  CLC
  ADC #$01
  STA yTotal
DoneWithY:
  LDA xTotal
  CLC
  ADC yTotal
  CMP #$01
  BNE CheckPlayerDone
  LDA #$01
  STA playerLost
  LDA #$08
  JSR sound_load
CheckPlayerDone:
  RTS

CalculateDistance:
  ;formula: sqr((Xa - Xb)^2 + (Ya - Yb)^2)
  ;calculate X and Y and sum
  LDA xone
  SEC
  SBC playerCoorX
  STA xTotal
  ; check if negative to get difference with #$00
  BPL DoneWithSubtractX
  LDA #$00
  SEC
  SBC xTotal
  STA xTotal
DoneWithSubtractX:
  LDA yone
  SEC
  SBC playerCoorY
  STA yTotal
  BPL DoneWithSubtractY
  LDA #$00
  SEC
  SBC yTotal
  STA yTotal
DoneWithSubtractY:
  ; exponential x - skip if zero
  LDA xTotal
  STA multResultOne
  CMP #$00
  BEQ DoneExponentialX
  STA numberToMult
  JSR DistanceMultiplication
  LDA multTempResult
  STA multResultOne
  ; exponential Y - also skip if zero
DoneExponentialX:
  LDA yTotal
  STA multResultTwo
  CMP #$00
  BEQ DoneExponentialY
  STA numberToMult
  JSR DistanceMultiplication
  LDA multTempResult
  STA multResultTwo
DoneExponentialY:
  ; sum results and sqr root
  LDA multResultOne
  CLC
  ADC multResultTwo
  STA numberToRoot
  ;calculate sqr root
  JSR SQRCalculation
  RTS

DistanceMultiplication:
  LDX #$01
  MultLoop:
  CLC
  ADC numberToMult
  INX
  CPX numberToMult
  BNE MultLoop
  STA multTempResult
  RTS
 
CheckIfExit:
  LDA #$00
  STA playerHasWon
  LDA playerCoorX
  CMP exitX
  BNE CheckIfExitDone
  LDA playerCoorY
  CMP exitY
  BNE CheckIfExitDone
  LDA #$01
  STA playerHasWon
  JSR LoadNxtLevel
CheckIfExitDone:
  RTS

SetPlayerInitialPos:
  LDA levelNumber
  ASL A
  TAX
  LDA playerInitialPositions+0, x
	STA pointerLo
	LDA playerInitialPositions+1, x
	STA pointerHi
  LDY #$00
  LDA [pointerLo], y
  STA playerCoorX
  INY
  LDA [pointerLo], y
  STA playerCoorY
  INY
  LDA [pointerLo], y
  STA posx
  INY
  LDA [pointerLo], y
  STA posy
  RTS

SetExitPosition:
  LDA levelNumber
  ASL A
  TAX
  LDA exitsPositions+0, X
  STA pointerLo
  LDA exitsPositions+1, X
  STA pointerHi
  LDY #$00
  LDA [pointerLo], Y
  STA exitX
  INY
  LDA [pointerLo], Y
  STA exitY
  RTS

LoadNxtLevel:
  LDA levelNumber
  CLC
  ADC #$01
  STA levelNumber
ResetLevel:
  LDA #$00
  STA playerLost
  STA brOnePaused
  STA brTwoPaused
  ; reset player pos
  ; LDA #$01
  ; STA playerCoorX
  ; STA playerCoorY
  ; LDA #$A3
  ; STA posy  
  ; LDA #$68
  ; STA posx
  JSR SetPlayerInitialPos
  JSR SetExitPosition
  ; turn PPU off
  LDA #$00
  STA $2001
  ; CLEAN PPU?????
  LDX #$00
CleanPPULoop:
  STA BRONEY, x
  INX
  CPX spritesAmount
  BNE CleanPPULoop
  ; draw new lvl
  JSR LoadLevel
  ; turn PPU on
  ; LDA #%00011110   ; enable sprites, enable background, no clipping on left side
  ; STA $2001
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
  ; set bladeRewinders
  LDA brPointers+0, x
  STA levelBRs+0
  LDA brPointers+1, x
  STA levelBRs+1
  LDA bladeRewindersTotalPerLvl, y
  STA brsAmount
  ; set buttons/elements
  LDA buttonsPositions+0, X
  STA levelButtons+0
  LDA buttonsPositions+1, X
  STA levelButtons+1
  LDA buttonsPerLevelTotal, Y
  STA buttonsAmount
  ;set bg
  lda bgLevelsPointers+0, X
  sta levelBackground+0
  lda bgLevelsPointers+1, X
  sta levelBackground+1

LoadSprites:
  LDY #$00              ; start at 0
LoadSpritesLoop:
  LDA [levelSprites], y        ; load data from address (sprites +  x)
  STA $0200, y          ; store into RAM address ($0200 + x)
  INY                   ; X = X + 1
  CPY spritesAmount     ; Compare X to hex $10, decimal
  BNE LoadSpritesLoop   ; Branch to LoadSpritesLoop if compare was Not Equal to zero
                        ; if compare was equal to 16, keep going down

LoadBackground:
  LDA $2002             ; read PPU status to reset the high/low latch
  LDA #$20
  STA $2006             ; write the high byte of $2000 address
  LDA #$00
  STA $2006             ; write the low byte of $2000 address

  ; LDA #$00
  LDA levelBackground+0
  STA pointerLo       ; put the low byte of the address of background into pointer
  ;LDA #HIGH(levelBackground)
  LDA levelBackground+1
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
  ; RTS

LoadBladeRewinders:
  LDY #$00
  LDX #$01
LoadBRLoop:
  LDA [levelBRs], Y
  STA brCurrentCoorX
  INY
  LDA [levelBRs], Y
  STA brCurrentCoorY
  INY
  LDA [levelBRs], Y
  STA brCurrentPosX
  INY
  LDA [levelBRs], Y
  STA brCurrentPosY
  INY
  CPX #$02
  BEQ AssignBRTwo
  ; asign to one
  INX
  LDA brCurrentPosX
  STA brOnePosX
  LDA brCurrentPosY
  STA brOnePosY
  LDA brCurrentCoorX
  STA brOneCoorX
  LDA brCurrentCoorY
  STA brOneCoorY
  LDA brsAmount
  CMP #$02
  BEQ LoadBRLoop
  JMP LoadBRDone
AssignBRTwo:
  LDA brCurrentPosX
  STA brTwoPosX
  LDA brCurrentPosY
  STA brTwoPosY
  LDA brCurrentCoorX
  STA brTwoCoorX
  LDA brCurrentCoorY
  STA brTwoCoorY
LoadBRDone:
  RTS

;;;;;;;; TEST LOAD BG
LoadInitialBackground:
  LDA initialBGNumber
  asl A ;multiplies it by 2 since each pointer is 2 bytes
	tax ;use it as an index
  lda initialScreenPointers+0, X
  sta levelBackground+0
  lda initialScreenPointers+1, X
  sta levelBackground+1
  LDA $2002             ; read PPU status to reset the high/low latch
  LDA #$20
  STA $2006             ; write the high byte of $2000 address
  LDA #$00
  STA $2006             ; write the low byte of $2000 address

  ; LDA #$00
  LDA levelBackground+0
  STA pointerLo       ; put the low byte of the address of background into pointer
  ;LDA #HIGH(levelBackground)
  LDA levelBackground+1
  STA pointerHi       ; put the high byte of the address into pointer
  
  LDX #$00            ; start at pointer + 0
  LDY #$00
InitialOutsideLoop:
  
InitialInsideLoop:
  LDA [pointerLo], y  ; copy one background byte from address in pointer plus Y
  STA $2007           ; this runs 256 * 4 times
  
  INY                 ; inside loop counter
  CPY #$00
  BNE InitialInsideLoop      ; run the inside loop 256 times before continuing down
  
  INC pointerHi       ; low byte went 0 to 256, so high byte needs to be changed now
  
  INX
  CPX #$04
  BNE InitialOutsideLoop     ; run the outside loop 256 times before continuing down
  RTS

;;;; black screen
LoadBlackScreen:
  LDA $2002             ; read PPU status to reset the high/low latch
  LDA #$20
  STA $2006             ; write the high byte of $2000 address
  LDA #$00
  STA $2006             ; write the low byte of $2000 address
  
  LDX #$00            ; start at pointer + 0
  LDY #$00
.initialOutsideLoop:
  
.initialInsideLoop:
  LDA #$24  ; copy one background byte from address in pointer plus Y
  STA $2007           ; this runs 256 * 4 times
  
  INY                 ; inside loop counter
  CPY #$00
  BNE .initialInsideLoop      ; run the inside loop 256 times before continuing down  
  INX
  CPX #$04
  BNE .initialOutsideLoop     ; run the outside loop 256 times before continuing down
  
  ;; write attributes, in this case black and white (3rd palette)
  LDA $2002             ; read PPU status to reset the high/low latch
  LDA #$23
  STA $2006             ; write the high byte of $23C0 address
  LDA #$C0
  STA $2006             ; write the low byte of $23C0 address
  LDX #$00              ; start out at 0
.loadAttributeLoop:
  LDA #%11111111     ; load data from address (attribute + the value in x)
  STA $2007             ; write to PPU
  INX                   ; X = X + 1
  CPX #$40              ; Compare X to hex $08, decimal 8 - copying 8 bytes
  BNE .loadAttributeLoop
  RTS

;;;;;;;;;;;;
;;;;;;;;;;;;
; Text ENGINE

;;;;;;;;;;;;
;;;;;;;;;;;;;
; SQR ROUTINE
SQRCalculation:
 LDA #$01
 STA rootRegisterD ;D
 STA rootRegisterE ;E
 LDA numberToRoot
SQRLoop: 
 SEC
 SBC rootRegisterD
 CMP #$00
 BEQ Result
 LDX rootRegisterD
 INX
 INX
 STX rootRegisterD
 CMP rootRegisterD
 BCC Result
 LDX rootRegisterE
 INX
 STX rootRegisterE
 JMP SQRLoop 	
Result:
 STA rootRemanent ; remanent
 LDA rootRegisterE
 ASL A
 ASL A
 CLC
 ADC rootRemanent
 STA rootResult ;result 
 RTS
;;;;;;;;;;;;;;  
TimeWait:
  LDA timerOn
  CMP #$01
  BEQ TimeWaitDone
  LDA #$01
  STA timerOn
  LDA	#10
	STA	wcount
bw1:
  JSR	delay
	DEC	wcount
	BNE	bw1
	JMP Forever ; jump to forever to wait for NMI for good rendering

delay:
	LDY	#255		;about 0.16s @ 2MHz 
dloop2:
	LDX	#255
dloop1:
	DEX
	BNE	dloop1
	DEY
	BNE	dloop2
TimeWaitDone:
	RTS
;;;;;; level sprites data
spritesTotalPerLvl: ;value multiplied by four because of attributes
  .db $50, $70, $50, $50, $50, $50

spritesLvl1:
     ;vert tile attr horiz
  .db $80, $50, $03, $80
  .db $80, $51, $03, $88
  .db $88, $60, $03, $80
  .db $88, $61, $03, $88
  .db $90, $70, $03, $80
  .db $90, $71, $03, $88
  ; .db $63, $40, $00, $6C   ;BR 1
  ;BR 1 full body
  .db $73, $52, $02, $64
  .db $73, $53, $02, $6C
  .db $6B, $62, $02, $64
  .db $6B, $63, $02, $6C
  .db $63, $72, $02, $64
  .db $63, $73, $02, $6C
  ;.db $63, $40, $00, $6C   ;BR 2
  ;.db $63, $41, $03, $6C   ; exit
  .db $5F, $04, $01, $62
  .db $5F, $05, $01, $6A
  .db $5F, $06, $01, $72
  .db $5F, $07, $01, $7A
  .db $67, $14, $01, $62
  .db $67, $15, $01, $6A
  .db $67, $16, $01, $72
  .db $67, $17, $01, $7A
  ; .db $83, $41, $00, $6C   ;sprite 1
  ; .db $73, $41, $00, $8C   ;sprite 1
  ; .db $8B, $41, $00, $9C   ;sprite 1

spritesLvl2:
     ;vert tile attr horiz
  .db $80, $50, $03, $80
  .db $80, $51, $03, $88
  .db $88, $60, $03, $80
  .db $88, $61, $03, $88
  .db $90, $70, $03, $80
  .db $90, $71, $03, $88
  ;BR 1 full body
  .db $73, $52, $02, $64
  .db $73, $53, $02, $6C
  .db $6B, $62, $02, $64
  .db $6B, $63, $02, $6C
  .db $63, $72, $02, $64
  .db $63, $73, $02, $6C
  ; .db $83, $41, $00, $6C   ;sprite 1
  ; .db $8B, $41, $00, $9C   ;sprite 1
  .db $67, $04, $01, $92 ; exit
  .db $67, $05, $01, $9A
  .db $67, $06, $01, $A2
  .db $67, $07, $01, $AA
  .db $6F, $14, $01, $92
  .db $6F, $15, $01, $9A
  .db $6F, $16, $01, $A2
  .db $6F, $17, $01, $AA
  ; .db $98, $20, $01, $32 ; pause btn
  ; .db $98, $21, $01, $3A
  ; .db $98, $22, $01, $42
  ; .db $98, $23, $01, $4A
  ; .db $A0, $30, $01, $32
  ; .db $A0, $31, $01, $3A
  ; .db $A0, $32, $01, $42
  ; .db $A0, $33, $01, $4A

spritesPointers:
  .dw spritesLvl2
  .dw spritesLvl2
  .dw spritesLvl2
  .dw spritesLvl2
  .dw spritesLvl2
  .dw spritesLvl2
  .dw spritesLvl2

;;;;;;;;;;;;;
  
  
  .bank 1
  .org $E000

; logoScreen:
;   incbin "logoscreen1.nam"

titleScreen:
  incbin "titlescreen1.nam"

; intro1:
;   incbin "intro1.nam"

bglvl01:
  incbin "bladeRewinderslvl01v2.nam"

bglvl02:
  incbin "lvl2.nam"

bglvl03:
  incbin "lvl3.nam"

bglvl04:
  incbin "lvl4.nam"

bglvl05:
  incbin "lvl5.nam"

bglvl06:
  incbin "bladeRewinderslvl01v2.nam"

palette:
  .db $0F,$29,$01,$25,  $0F,$36,$17,$0F,  $0F,$05,$16,$26,  $0F,$20,$00,$0F   ;;background palette
  .db $0F,$13,$23,$33,  $0F,$20,$10,$24,  $0F,$1C,$37,$16,  $0F,$1C,$37,$23   ;;sprite palette

; spritesLvl1:
;      ;vert tile attr horiz
;   .db $80, $50, $03, $80
;   .db $80, $51, $03, $88
;   .db $88, $60, $03, $80
;   .db $88, $61, $03, $88
;   .db $90, $70, $03, $80
;   .db $90, $71, $03, $88
;   ; .db $63, $40, $00, $6C   ;BR 1
;   ;BR 1 full body
;   .db $73, $52, $02, $64
;   .db $73, $53, $02, $6C
;   .db $6B, $62, $02, $64
;   .db $6B, $63, $02, $6C
;   .db $63, $72, $02, $64
;   .db $63, $73, $02, $6C
;   ;.db $63, $40, $00, $6C   ;BR 2
;   ;.db $63, $41, $03, $6C   ; exit
;   .db $5F, $04, $01, $62
;   .db $5F, $05, $01, $6A
;   .db $5F, $06, $01, $72
;   .db $5F, $07, $01, $7A
;   .db $67, $14, $01, $62
;   .db $67, $15, $01, $6A
;   .db $67, $16, $01, $72
;   .db $67, $17, $01, $7A
;   ; .db $83, $41, $00, $6C   ;sprite 1
;   ; .db $73, $41, $00, $8C   ;sprite 1
;   ; .db $8B, $41, $00, $9C   ;sprite 1

;   spritesLvl4:
;      ;vert tile attr horiz
;   .db $80, $50, $03, $80
;   .db $80, $51, $03, $88
;   .db $88, $60, $03, $80
;   .db $88, $61, $03, $88
;   .db $90, $70, $03, $80
;   .db $90, $71, $03, $88
;   ; .db $63, $40, $00, $6C   ;BR 1
;   ;BR 1 full body
;   .db $73, $52, $02, $64
;   .db $73, $53, $02, $6C
;   .db $6B, $62, $02, $64
;   .db $6B, $63, $02, $6C
;   .db $63, $72, $02, $64
;   .db $63, $73, $02, $6C
;   ;.db $63, $40, $00, $6C   ;BR 2
;   ;.db $63, $41, $03, $6C   ; exit
;   .db $5F, $04, $01, $62
;   .db $5F, $05, $01, $6A
;   .db $5F, $06, $01, $72
;   .db $5F, $07, $01, $7A
;   .db $67, $14, $01, $62
;   .db $67, $15, $01, $6A
;   .db $67, $16, $01, $72
;   .db $67, $17, $01, $7A
;   ; .db $83, $41, $00, $6C   ;sprite 1
;   ; .db $73, $41, $00, $8C   ;sprite 1
;   ; .db $8B, $41, $00, $9C   ;sprite 1

; spritesLvl2:
;      ;vert tile attr horiz
;   .db $80, $50, $03, $80
;   .db $80, $51, $03, $88
;   .db $88, $60, $03, $80
;   .db $88, $61, $03, $88
;   .db $90, $70, $03, $80
;   .db $90, $71, $03, $88
;   ;BR 1 full body
;   .db $73, $52, $02, $64
;   .db $73, $53, $02, $6C
;   .db $6B, $62, $02, $64
;   .db $6B, $63, $02, $6C
;   .db $63, $72, $02, $64
;   .db $63, $73, $02, $6C
;   ; .db $83, $41, $00, $6C   ;sprite 1
;   ; .db $8B, $41, $00, $9C   ;sprite 1
;   .db $67, $04, $01, $92 ; exit
;   .db $67, $05, $01, $9A
;   .db $67, $06, $01, $A2
;   .db $67, $07, $01, $AA
;   .db $6F, $14, $01, $92
;   .db $6F, $15, $01, $9A
;   .db $6F, $16, $01, $A2
;   .db $6F, $17, $01, $AA
;   .db $98, $20, $01, $32 ; pause btn
;   .db $98, $21, $01, $3A
;   .db $98, $22, $01, $42
;   .db $98, $23, $01, $4A
;   .db $A0, $30, $01, $32
;   .db $A0, $31, $01, $3A
;   .db $A0, $32, $01, $42
;   .db $A0, $33, $01, $4A

; spritesLvl3:
;   .db $80, $50, $03, $80
;   .db $80, $51, $03, $88
;   .db $88, $60, $03, $80
;   .db $88, $61, $03, $88
;   .db $90, $70, $03, $80
;   .db $90, $71, $03, $88
;   ; .db $63, $40, $00, $6C   ;BR 1
;   ;BR 1 full body
;   .db $73, $52, $02, $64
;   .db $73, $53, $02, $6C
;   .db $6B, $62, $02, $64
;   .db $6B, $63, $02, $6C
;   .db $63, $72, $02, $64
;   .db $63, $73, $02, $6C
;   ;.db $63, $40, $00, $6C   ;BR 2
;   ;.db $63, $41, $03, $6C   ; exit
;   .db $5F, $04, $01, $62
;   .db $5F, $05, $01, $6A
;   .db $5F, $06, $01, $72
;   .db $5F, $07, $01, $7A
;   .db $67, $14, $01, $62
;   .db $67, $15, $01, $6A
;   .db $67, $16, $01, $72
;   .db $67, $17, $01, $7A

blocksTotalPerLvl: ;no need to multiply, I'm jumping over extra values
  .db $05, $0A, $0D, $05, $08, $05, $05

blocksLvl1:
      ;x   y
  .db $02, $02
  .db $04, $02
  .db $04, $04
  .db $04, $06
  .db $02, $05

blocksLvl2:
  .db $03, $03  
  .db $02, $02
  ; .db $03, $02, $6C, $83
  .db $04, $06
  .db $05, $01
  .db $05, $02
  .db $05, $03
  .db $05, $04
  .db $05, $05
  .db $05, $06
  .db $05, $07

blocksLvl3:
  .db $07, $01
  .db $07, $02
  ;.db $06, $03, $00, $00
  .db $06, $04
  .db $06, $05
  .db $06, $06
  .db $06, $07
  .db $02, $02
  .db $04, $02
  .db $02, $04
  .db $04, $04
  ;.db $05, $04, $00, $00
  .db $04, $06
  .db $05, $06
  .db $02, $06
  .db $01, $07

blocksLvl4:
      ;x   y
  .db $05, $02
  .db $02, $05
  .db $03, $06
  .db $04, $04
  .db $05, $05

blocksLvl5:
      ;x   y
  .db $03, $05
  .db $03, $02
  .db $04, $02
  .db $04, $03
  .db $01, $05
  .db $05, $05
  .db $05, $07
  .db $06, $07

blocksLvl6:
      ;x   y
  .db $05, $02
  .db $02, $05
  .db $03, $06
  .db $04, $04
  .db $05, $05

bladeRewindersTotalPerLvl:
  .db $01, $01, $01, $01, $01, $01

bladeRewindersLvl1:
  ; coorX, coorY, sprX, sprY
  .db $06, $06, $69, $53
  ;.db $06, $07, $7C, $5B

bladeRewindersLvl2:
  ; coorX, coorY, sprX, sprY
  .db $02, $03, $79, $8B

bladeRewindersLvl3:
  ; coorX, coorY, sprX, sprY
  .db $01, $06, $B9, $7B

bladeRewindersLvl4:
  ; coorX, coorY, sprX, sprY
  .db $06, $01, $19, $7B

bladeRewindersLvl5:
  ; coorX, coorY, sprX, sprY
  .db $05, $06, $79, $5B

bladeRewindersLvl6:
  ; coorX, coorY, sprX, sprY
  .db $06, $06, $69, $53

initialPlayerPosLvl1:
  ; coorx. coory, X, Y
  .db $01, $01, $68, $A3

initialPlayerPosLvl5:
  ; coorx. coory, X, Y
  .db $02, $02, $68, $93

exitsPosLvl1:
  ; X, Y
  .db $06, $06

exitsPosLvl2:
  .db $04, $07

exitsPosLvl3:
  ; X, Y
  .db $03, $05

exitsPosLvl4:
  ; X, Y
  .db $05, $06

exitsPosLvl5:
  ; X, Y
  .db $02, $07

exitsPosLvl6:
  ; X, Y
  .db $06, $06

buttonsPerLevelTotal:
  .db $01, $01, $01, $01, $01, $01

buttonsLvl1:
  ; X, Y, type (1=pause, 2=rewind, 3=exit?)
  .db $03, $03, $01

buttonsLvl2:
  ; X, Y, type (1=pause, 2=rewind, 3=exit?)
  .db $04, $01, $01

buttonsLvl3:
  ; X, Y, type (1=pause, 2=rewind, 3=exit?)
  .db $05, $01, $02

buttonsLvl4:
  ; X, Y, type (1=pause, 2=rewind, 3=exit?)
  .db $05, $02, $01

buttonsLvl5:
  ; X, Y, type (1=pause, 2=rewind, 3=exit?)
  .db $01, $06, $01

buttonsLvl6:
  ; X, Y, type (1=pause, 2=rewind, 3=exit?)
  .db $03, $03, $01

initialScreenPointers:
  ; .dw logoScreen
  ; .dw titleScreen
  ; .dw intro1
  .dw titleScreen
  .dw titleScreen
  .dw titleScreen

; introTextPointers:
;   .dw intro1

bgLevelsPointers:
  .dw bglvl01
  .dw bglvl02
  .dw bglvl03
  .dw bglvl04
  .dw bglvl05
  .dw bglvl06

blocksPointers:
  .dw blocksLvl1
  .dw blocksLvl2
  .dw blocksLvl3
  .dw blocksLvl4
  .dw blocksLvl5
  .dw blocksLvl6

brPointers:
  .dw bladeRewindersLvl1
  .dw bladeRewindersLvl2
  .dw bladeRewindersLvl3
  .dw bladeRewindersLvl4
  .dw bladeRewindersLvl5
  .dw bladeRewindersLvl6

playerInitialPositions:
  .dw initialPlayerPosLvl1
  .dw initialPlayerPosLvl1
  .dw initialPlayerPosLvl1
  .dw initialPlayerPosLvl1
  .dw initialPlayerPosLvl5
  .dw initialPlayerPosLvl1

exitsPositions:
  .dw exitsPosLvl1
  .dw exitsPosLvl2
  .dw exitsPosLvl3
  .dw exitsPosLvl4
  .dw exitsPosLvl5
  .dw exitsPosLvl6

buttonsPositions:
  .dw buttonsLvl1
  .dw buttonsLvl2
  .dw buttonsLvl3
  .dw buttonsLvl4
  .dw buttonsLvl5
  .dw buttonsLvl6

;;;;;
;TEXTS
;;;;;

firstText:
  .db $24, $12, $17, $24, $1D, $11, $0E, $24, $09, $00, $1C, $24, $16, $18, $1F, $12, $0E, $1C, $24, $20, $0E, $1B, $0E, $24, $1D, $11, $0E, $24, $24, $24, $24, $24, $24, $0E, $17, $1D, $0E, $1B, $1D, $0A, $12, $17
  .db $16, $0E, $17, $1D, $24, $0F, $12, $1B, $1C, $1D, $24, $0C, $11, $18, $12, $0C, $0E, $2F, $24, $24, $24, $24, $24, $16, $18, $1F, $12, $0E, $1C, $24, $20, $0E, $1B, $0E, $24, $1C, $1D, $18, $1B, $0E, $0D, $24
  .db $18, $17, $24, $0A, $24, $24, $24, $24, $24, $24, $24, $24, $24, $1B, $0E, $1F, $18, $15, $1E, $1D, $12, $18, $17, $0A, $1B, $22, $24, $1D, $0E, $0C, $11, $17, $18, $15, $18, $10, $22, $24, $0C, $0A, $15, $15
  .db $0E, $0D, $24, $1D, $11, $0E, $24, $1F, $11, $1C, $2F





  .org $FFFA     ;first of the three vectors starts here
  .dw NMI        ;when an NMI happens (once per frame if enabled) the 
                   ;processor will jump to the label NMI:
  .dw RESET      ;when the processor first turns on or is reset, it will jump
                   ;to the label RESET:
  .dw 0          ;external interrupt IRQ is not used in this tutorial
  
  
;;;;;;;;;;;;;;  
  
  
  .bank 2
  .org $0000
  .incbin "bladerewinderV2.chr"   ;includes 8KB graphics file from SMB1