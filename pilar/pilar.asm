  .inesprg 1   ; 1x 16KB PRG code
  .ineschr 3   ; 3x  8KB CHR data (for title, gameplay, and secret image graphics)
  .inesmap 3   ; mapper 3 = CNROM, supports CHR-ROM bank switching
  .inesmir 1   ; background mirroring
  

;;;;;;;;;;;;;;;

;; DECLARE SOME VARIABLES HERE
  .rsset $0000  ;;start variables at ram location 0
  
gamestate     .rs 1  ; .rs 1 means reserve one byte of space
ballx         .rs 1  ; player 1 horizontal position
bally         .rs 1  ; player 1 vertical position
ball2x        .rs 1  ; player 2 horizontal position  
ball2y        .rs 1  ; player 2 vertical position
ballup        .rs 1  ; 1 = ball moving up
balldown      .rs 1  ; 1 = ball moving down
ballleft      .rs 1  ; 1 = ball moving left
ballright     .rs 1  ; 1 = ball moving right
ballspeedx    .rs 1  ; ball horizontal speed per frame
ballspeedy    .rs 1  ; ball vertical speed per frame
paddle1ytop   .rs 1  ; player 1 paddle top vertical position
paddle2ybot   .rs 1  ; player 2 gamepad bottom vertical position
buttons1      .rs 1  ; player 1 gamepad buttons, one bit per button
buttons2      .rs 1  ; player 2 gamepad buttons, one bit per button
scoreOnes     .rs 1  ; byte for each digit in the decimal score (Player 1)
scoreTens     .rs 1
scoreHundreds .rs 1
heartCounter  .rs 1  ; secondary counter (0-4, resets at 5) (Player 1)
; Player 2 score variables
score2Ones    .rs 1  ; byte for each digit in Player 2's decimal score
score2Tens    .rs 1
score2Hundreds .rs 1
heartCounter2 .rs 1  ; secondary counter for Player 2 (0-4, resets at 5)
; falling item variables (Left Zone - Player 1)
itemx         .rs 1  ; falling item X position
itemy         .rs 1  ; falling item Y position
itemactive    .rs 1  ; 1 = item is falling, 0 = no item
itemtype      .rs 1  ; 0 = good item (heart), 1 = bad item (broken heart), 2 = cake
itemspeed     .rs 1  ; falling speed
randomseed    .rs 1  ; simple random number seed
; falling item variables (Right Zone - Player 2)
item2x        .rs 1  ; right zone item X position
item2y        .rs 1  ; right zone item Y position
item2active   .rs 1  ; 1 = right zone item is falling, 0 = no item
item2type     .rs 1  ; 0 = good item (heart), 1 = bad item (broken heart), 2 = cake
item2speed    .rs 1  ; right zone falling speed
randomseed2   .rs 1  ; separate random seed for right zone
; cake variables
cakex         .rs 1  ; cake X position
cakeactive    .rs 1  ; 1 = cake is moving, 0 = no cake
cakespeed     .rs 1  ; cake horizontal speed
cakedirection .rs 1  ; 0 = moving right (targets player 2), 1 = moving left (targets player 1)
; physics variables
velocity_y    .rs 1  ; player vertical velocity (signed: $00-$7F = down, $80-$FF = up)
on_ground     .rs 1  ; 1 = player 1 is on ground, 0 = player 1 is in air
on_ground2    .rs 1  ; 1 = player 2 is on ground, 0 = player 2 is in air
jump_pressed  .rs 1  ; 1 = jump button was pressed this frame
jump_counter  .rs 1  ; frames remaining in jump (0 = not jumping)
jump_counter2 .rs 1  ; frames remaining in jump for player 2 (0 = not jumping)
; progressive speed system
speed_level   .rs 1  ; current speed level (0-3)
total_score   .rs 1  ; combined score of both players for speed calculation
; win condition system
winner        .rs 1  ; 0 = no winner, 1 = player 1 wins, 2 = player 2 wins
p1_anim_frame .rs 1  ; Player 1 animation frame (0, 1, or 2)
p1_anim_timer .rs 1  ; Player 1 animation timer
p1_facing     .rs 1  ; Player 1 facing direction (0=right, 1=left)
p2_anim_frame .rs 1  ; Player 2 animation frame (0, 1, or 2)  
p2_anim_timer .rs 1  ; Player 2 animation timer
p2_facing     .rs 1  ; Player 2 facing direction (0=right, 1=left)
; pointer variables for background loading
pointerLo     .rs 1  ; pointer low byte
pointerHi     .rs 1  ; pointer high byte
; title screen menu variables
menu_selection .rs 1  ; 0 = start, 1 = secretos
menu_cursor_y  .rs 1  ; Y position of cursor sprite
menu_cursor_x  .rs 1  ; X position of cursor sprite
input_timer    .rs 1  ; timer to prevent immediate button triggers (counts down from 120)
; secretos screen variables
secret_digit1  .rs 1  ; first digit (0-9)
secret_digit2  .rs 1  ; second digit (0-9)
secret_digit3  .rs 1  ; third digit (0-9)
secret_digit4  .rs 1  ; fourth digit (0-9)
secret_cursor  .rs 1  ; which digit is selected (0-3)
secret_message .rs 1  ; 0=no message, 1=error, 2=success
secret_input_delay .rs 1  ; delay timer for button inputs (30 frames)
secret_draw_flag .rs 1  ; 1=need to redraw UI during next NMI, 0=no draw needed
secret_msg_index .rs 1  ; which secret message to display (0, 1, 2, etc)
secret_msg_char_index_lo .rs 1  ; current character in message data (low byte)
secret_msg_char_index_hi .rs 1  ; current character in message data (high byte)
secret_msg_screen_pos_lo .rs 1  ; current screen position for writing (low byte)
secret_msg_screen_pos_hi .rs 1  ; current screen position for writing (high byte)
secret_msg_timer .rs 1  ; timer for character writing speed
secret_msg_draw_flag .rs 1  ; 1=need to write next character during NMI, 0=no write needed
loading_timer    .rs 1  ; timer for loading screen (counts frames, 600 frames = 10 seconds)
loading_timer_hi .rs 1  ; high byte of loading timer (16-bit counter)


;; DECLARE SOME CONSTANTS HERE
STATETITLE     = $00  ; displaying title screen
STATELOADING   = $01  ; displaying loading/instructions screen
STATEPLAYING   = $02  ; move paddles/ball, check for collisions
STATEGAMEOVER  = $03  ; displaying game over screen
STATEWINSCREEN = $04  ; displaying winner screen
STATESECRETOS  = $05  ; displaying secretos screen
STATESECRETMSG = $06  ; displaying secret message
STATESECRETIMG = $07  ; displaying secret image
  
RIGHTWALL      = $F4  ; when ball reaches one of these, do something
TOPWALL        = $20
BOTTOMWALL     = $E0
LEFTWALL       = $04
  
PADDLE1X       = $08  ; horizontal position for paddles, doesnt move
PADDLE2X       = $F0

; Physics constants
GRAVITY        = $01  ; gravity acceleration per frame (slow for debugging)
JUMP_VELOCITY  = $F8  ; initial jump velocity (negative = upward)
JUMP_SPEED     = $05  ; pixels per frame when jumping up
JUMP_DURATION  = $15  ; frames to jump upward (48 frames)
FALL_SPEED     = $02  ; pixels per frame when falling down
GROUND_Y       = $D0  ; Y position of ground (higher level)
ANIM_SPEED     = $08  ; frames per animation frame (slower = higher number)
IDLE_FRAME     = $FF  ; special value to indicate idle state

; Title screen menu constants
MENU_START_Y   = $68  ; Y position for "start" option (13 * 8 = 104 = $68)
MENU_START_X   = $40  ; X position for "start" cursor (8 * 8 = 64 = $40)
MENU_SECRETOS_Y = $78 ; Y position for "secretos" option (15 * 8 = 120 = $78)
MENU_SECRETOS_X = $50 ; X position for "secretos" cursor (10 * 8 = 80 = $50)

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
  STX $2001    ; disable rendering
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

  ; Load title screen CHR bank (bank 0)
  LDA #$00          ; put bank 0 (title CHR) into A
  JSR Bankswitch    ; switch to title screen graphics

  JSR LoadTitlePalettes  ; load title screen palettes
  JSR LoadTitleScreen    ; load title screen nametable
  JMP InitializeGame

LoadTitlePalettes:
  LDA $2002             ; read PPU status to reset the high/low latch
  LDA #$3F
  STA $2006             ; write the high byte of $3F00 address
  LDA #$00
  STA $2006             ; write the low byte of $3F00 address
  LDX #$00              ; start out at 0
LoadTitlePalettesLoop:
  LDA titlepalette, x   ; load data from title palette
  STA $2007             ; write to PPU
  INX                   ; X = X + 1
  CPX #$20              ; Compare X to hex $20, decimal 32 - copying 32 bytes
  BNE LoadTitlePalettesLoop
  RTS

LoadLoadingPalettes:
  ; Load special palettes for loading screen (black background, white text)
  LDA $2002             ; read PPU status to reset the high/low latch
  LDA #$3F
  STA $2006             ; write the high byte of $3F00 address
  LDA #$00
  STA $2006             ; write the low byte of $3F00 address
  
  ; Background palette 0: all black
  LDA #$0F              ; black
  STA $2007
  LDA #$0F              ; black
  STA $2007
  LDA #$0F              ; black
  STA $2007
  LDA #$0F              ; black
  STA $2007
  
  ; Background palettes 1-2: not used, set to black
  LDX #$00
LoadingBgPalLoop:
  LDA #$0F              ; black
  STA $2007
  INX
  CPX #$08              ; 8 more bytes (2 palettes * 4 colors)
  BNE LoadingBgPalLoop
  
  ; Background palette 3: white text
  LDA #$0F              ; black background
  STA $2007
  LDA #$30              ; white
  STA $2007
  LDA #$30              ; white
  STA $2007
  LDA #$30              ; white
  STA $2007
  
  ; Sprite palettes (keep same as game)
  LDX #$00
LoadingSpritePalLoop:
  LDA palette+16, x     ; load sprite palettes from game palette
  STA $2007
  INX
  CPX #$10              ; 16 bytes (4 sprite palettes)
  BNE LoadingSpritePalLoop
  
  RTS

LoadSecretImagePalette:
  ; Load custom palette for secret image
  LDA $2002             ; read PPU status to reset the high/low latch
  LDA #$3F
  STA $2006             ; write the high byte of $3F00 address
  LDA #$00
  STA $2006             ; write the low byte of $3F00 address
  LDX #$00              ; start out at 0
LoadSecretImagePaletteLoop:
  LDA secretimagepalette, x   ; load data from secret image palette
  STA $2007             ; write to PPU
  INX                   ; X = X + 1
  CPX #$20              ; Compare X to hex $20, decimal 32 - copying 32 bytes
  BNE LoadSecretImagePaletteLoop
  RTS

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

LoadBackground:
  LDA $2002             ; read PPU status to reset the high/low latch
  LDA #$20
  STA $2006             ; write the high byte of $2000 address
  LDA #$00
  STA $2006             ; write the low byte of $2000 address

  ; Load background nametable (960 tiles + 64 attributes) from pilarbg1.nam
  LDA #LOW(gameBackground)
  STA pointerLo
  LDA #HIGH(gameBackground)
  STA pointerHi

  LDX #$00            ; outer loop counter (4 pages)
  LDY #$00            ; inner loop counter (256 bytes per page)
LoadBgOutsideLoop:
LoadBgInsideLoop:
  LDA [pointerLo], y  ; copy one background byte from address in pointer plus Y
  STA $2007           ; write to PPU
  INY                 ; Y = Y + 1
  BNE LoadBgInsideLoop      ; if Y != 0, keep looping (runs 256 times)
  
  ; Y wrapped to 0, we've read 256 bytes
  INC pointerHi       ; move to next page of data
  INX                 ; increment outer counter
  CPX #$04            ; have we done 4 pages? (4 * 256 = 1024 bytes)
  BNE LoadBgOutsideLoop
  RTS

InitializeGame:
  ; Clear any win message from previous game
  JSR ClearWinMessage

;;;Set some initial ball stats (now player controlled)
  LDA #$00
  STA balldown
  STA ballright
  STA ballup
  STA ballleft
  
  LDA #$60              ; start high in the air (above ground)
  STA bally
  STA ball2y            ; player 2 same Y position
  
  LDA #$40              ; player 1 starts on left side
  STA ballx
  
  LDA #$C0              ; player 2 starts on right side  
  STA ball2x
  
  LDA #$02
  STA ballspeedx
  STA ballspeedy

;;;Set initial falling item values (Left Zone)
  LDA #$00
  STA itemactive       ; no left zone item active at start
  STA itemtype         ; start with good item type
  LDA #$02
  STA itemspeed        ; falling speed
  LDA #$01
  STA randomseed       ; initial seed for left zone random

;;;Set initial falling item values (Right Zone)
  LDA #$00
  STA item2active      ; no right zone item active at start
  STA item2type        ; start with good item type
  LDA #$02
  STA item2speed       ; falling speed
  LDA #$17             ; different initial seed for right zone
  STA randomseed2      ; initial seed for right zone random
  
;;;Set initial cake values
  LDA #$00
  STA cakeactive       ; no cake active at start
  STA cakedirection    ; initialize direction
  LDA #$02
  STA cakespeed        ; cake horizontal speed

;;;Set initial physics values
  LDA #$00
  STA velocity_y       ; start with no vertical velocity
  STA jump_pressed     ; no jump pressed initially
  STA jump_counter     ; not jumping initially
  STA jump_counter2    ; player 2 not jumping initially
  STA on_ground        ; player 1 start in air
  STA on_ground2       ; player 2 start in air
  STA speed_level      ; start at speed level 0
  STA total_score      ; start with combined score 0
  STA winner           ; no winner at start
  STA p1_anim_frame    ; start Player 1 animation at frame 0
  STA p1_anim_timer    ; start Player 1 animation timer at 0
  STA p1_facing        ; start Player 1 facing right (0)
  STA p2_anim_frame    ; start Player 2 animation at frame 0
  STA p2_anim_timer    ; start Player 2 animation timer at 0
  STA p2_facing        ; start Player 2 facing right (0)

;;;Set initial score values
  ; Start both players with 0 points
  LDA #$00
  STA scoreOnes
  STA scoreTens
  STA scoreHundreds
  STA score2Ones       ; Player 2 score
  STA score2Tens
  STA score2Hundreds


;;:Set starting game state
  LDA #STATETITLE
  STA gamestate
  
;;:Initialize title screen menu
  LDA #$00              ; start with "start" option selected
  STA menu_selection
  LDA #MENU_START_Y
  STA menu_cursor_y
  LDA #MENU_START_X
  STA menu_cursor_x
  STA input_timer       ; initialize timer to 0 (no delay on boot)


              
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
  
  ; Check if we need to draw secretos UI (only if in secretos state)
  LDA gamestate
  CMP #STATESECRETOS
  BNE SkipSecretDraw    ; not in secretos state, skip
  LDA secret_draw_flag
  BEQ SkipSecretDraw
  JSR DrawSecretosUI
  LDA #$00
  STA secret_draw_flag  ; clear flag after drawing
SkipSecretDraw:

  ; Check if we need to write next character of secret message
  LDA secret_msg_draw_flag
  BEQ SkipSecretMsgDraw
  JSR WriteNextCharacter
  LDA #$00
  STA secret_msg_draw_flag  ; clear flag after writing
SkipSecretMsgDraw:

  ;;This is the PPU clean up section, so rendering the next frame starts properly.
  ; Check if we're displaying secret image - use pattern table 0 for background
  LDA gamestate
  CMP #STATESECRETIMG
  BEQ SetupSecretImagePPU
  
  ; Normal PPU setup (pattern table 1 for background)
  LDA #%10010000   ; enable NMI, sprites from Pattern Table 0, background from Pattern Table 1
  STA $2000
  JMP ContinuePPUSetup
  
SetupSecretImagePPU:
  ; Secret image PPU setup (pattern table 0 for background)
  LDA #%10000000   ; enable NMI, sprites from Pattern Table 0, background from Pattern Table 0
  STA $2000
  
ContinuePPUSetup:
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
  BNE CheckLoading
  JMP EngineTitle    ;;game is displaying title screen
  
CheckLoading:
  CMP #STATELOADING
  BNE CheckSecretos
  JMP EngineLoading  ;;game is displaying loading/instructions screen
  
CheckSecretos:
  CMP #STATESECRETOS
  BNE CheckSecretMsg
  JMP EngineSecretos  ;;game is displaying secretos screen

CheckSecretMsg:
  CMP #STATESECRETMSG
  BNE CheckSecretImg
  JMP EngineSecretMessage  ;;game is displaying secret message

CheckSecretImg:
  CMP #STATESECRETIMG
  BNE CheckGameOver
  JMP EngineSecretImage  ;;game is displaying secret image
    
CheckGameOver:
  CMP #STATEGAMEOVER
  BNE CheckWinScreen
  JMP EngineGameOver  ;;game is displaying ending screen
  
CheckWinScreen:
  CMP #STATEWINSCREEN
  BNE CheckPlaying
  JMP EngineWinScreen  ;;game is displaying winner screen
  
CheckPlaying:
  CMP #STATEPLAYING
  BNE GameEngineDone
  JMP EnginePlaying   ;;game is playing
GameEngineDone:  
  
  JSR UpdateSprites  ;;set ball/paddle sprites from positions

  RTI             ; return from interrupt
 
 
 
 
;;;;;;;;
 
EngineTitle:
  ; Decrement input timer if active
  LDA input_timer
  BEQ TitleCheckButtons  ; if timer is 0, check buttons
  DEC input_timer        ; decrement timer
  JMP GameEngineDone     ; skip button checks while timer is active

TitleCheckButtons:
  ; Check for UP button press to move cursor up
  LDA buttons1
  AND #%00001000      ; UP button
  BEQ TitleCheckDown  ; if not pressed, check down
  
  ; Move to "start" option
  LDA menu_selection
  BEQ TitleCheckDown  ; already at start (0), don't move up
  LDA #$00
  STA menu_selection
  LDA #MENU_START_Y
  STA menu_cursor_y
  LDA #MENU_START_X
  STA menu_cursor_x
  JMP GameEngineDone

TitleCheckDown:
  ; Check for DOWN button press to move cursor down
  LDA buttons1
  AND #%00000100      ; DOWN button
  BEQ TitleCheckA     ; if not pressed, check A
  
  ; Move to "secretos" option
  LDA menu_selection
  CMP #$01
  BEQ TitleCheckA     ; already at secretos (1), don't move down
  LDA #$01
  STA menu_selection
  LDA #MENU_SECRETOS_Y
  STA menu_cursor_y
  LDA #MENU_SECRETOS_X
  STA menu_cursor_x
  JMP GameEngineDone

TitleCheckA:
  ; Check for A button press (only if timer is 0)
  LDA buttons1
  AND #%10000000      ; A button
  BEQ TitleCheckStart ; if not pressed, check START
  
  ; A button pressed - check which menu item is selected
  LDA menu_selection
  BEQ StartGameFromTitle  ; if 0 (start), begin game
  CMP #$01
  BEQ EnterSecretosScreen ; if 1 (secretos), go to secretos screen
  JMP GameEngineDone

TitleCheckStart:
  ; Check for START button press (works immediately, no timer)
  LDA buttons1
  AND #%00010000      ; START button
  BNE StartGameFromTitle
  
  LDA buttons2  
  AND #%00010000      ; START button
  BNE StartGameFromTitle
  
  JMP GameEngineDone

EnterSecretosScreen:
  ; Turn screen off
  LDA #%00000000
  STA $2001
  STA $2000             ; also turn off NMI
  
  ; Wait for vblank to ensure PPU is ready
  LDA $2002             ; read PPU status to reset latch
WaitVBlankSecretos:
  BIT $2002
  BPL WaitVBlankSecretos
  
  ; Load black screen (already on correct CHR bank from title)
  JSR LoadSecretosScreen
  
  ; Initialize secretos variables
  LDA #$00
  STA secret_digit1
  STA secret_digit2
  STA secret_digit3
  STA secret_digit4
  STA secret_cursor     ; start at first digit
  STA secret_message    ; no message initially
  STA secret_input_delay ; no input delay initially
  STA secret_draw_flag  ; no draw flag initially
  
  ; Draw the secretos UI (safe to do here, screen is off)
  JSR DrawSecretosUI
  
  ; Set game state to secretos
  LDA #STATESECRETOS
  STA gamestate
  
  ; Set input timer to 120 frames (2 seconds at 60fps)
  LDA #60
  STA input_timer
  
  ; Turn screen back on
  LDA #%10010000        ; enable NMI, sprites from Pattern Table 0, background from Pattern Table 1
  STA $2000
  LDA #%00011110        ; enable sprites, enable background, no clipping on left side
  STA $2001
  
  JMP GameEngineDone

StartGameFromTitle:
  ; Transition to loading screen
  ; Turn screen off
  LDA #%00000000
  STA $2001
  STA $2000             ; also turn off NMI
  
  ; Wait for vblank to ensure PPU is ready
  LDA $2002             ; read PPU status to reset latch
WaitVBlankTransition:
  BIT $2002
  BPL WaitVBlankTransition
  
  ; Switch to gameplay CHR bank (bank 1) for loading screen
  LDA #$01          ; put bank 1 (gameplay CHR) into A
  JSR Bankswitch    ; switch to gameplay graphics
  
  ; Load title screen palettes (which have black background)
  JSR LoadTitlePalettes
  
  ; Now overwrite sprite palettes with game sprite palettes for items
  LDA $2002             ; reset PPU latch
  LDA #$3F
  STA $2006
  LDA #$10              ; sprite palette start at $3F10
  STA $2006
  
  LDX #$00
LoadingSpritePalettes:
  LDA palette+16, x     ; load game sprite palettes
  STA $2007
  INX
  CPX #$10              ; 16 bytes (4 sprite palettes)
  BNE LoadingSpritePalettes
  
  ; Load loading screen (black background with instructions)
  JSR LoadLoadingScreen
  
  ; Initialize loading timer (600 frames = 10 seconds at 60 FPS)
  LDA #LOW(600)
  STA loading_timer
  LDA #HIGH(600)
  STA loading_timer_hi
  
  ; Set game state to loading
  LDA #STATELOADING
  STA gamestate
  
  ; Turn screen back on
  LDA #%10010000        ; enable NMI, sprites from Pattern Table 0, background from Pattern Table 1
  STA $2000
  LDA #%00011110        ; enable sprites, enable background, no clipping on left side
  STA $2001
  
  JMP GameEngineDone

;;;;;;;;; 

EngineLoading:
  ; Decrement loading timer (16-bit)
  LDA loading_timer
  BNE DecrementLoadingLow
  LDA loading_timer_hi
  BEQ LoadingTimerExpired  ; both bytes are 0, timer expired
  DEC loading_timer_hi
  LDA #$FF
  STA loading_timer
  JMP CheckLoadingStart

DecrementLoadingLow:
  DEC loading_timer

CheckLoadingStart:
  ; Check if START button is pressed
  LDA buttons1
  AND #%00010000        ; START button
  BEQ LoadingEngineDone  ; not pressed, stay on loading screen
  
  ; START pressed, transition to game
  JMP StartGameFromLoading

LoadingTimerExpired:
  ; Timer reached 0, automatically start game
  JMP StartGameFromLoading

LoadingEngineDone:
  JMP GameEngineDone

StartGameFromLoading:
  ; Turn screen off
  LDA #%00000000
  STA $2001
  STA $2000             ; also turn off NMI
  
  ; Wait for vblank
  LDA $2002
WaitVBlankLoadingToGame:
  BIT $2002
  BPL WaitVBlankLoadingToGame
  
  ; Load game palettes
  JSR LoadPalettes
  
  ; Load game background
  JSR LoadBackground
  
  ; Set game state to playing
  LDA #STATEPLAYING
  STA gamestate
  
  ; Turn screen back on
  LDA #%10010000
  STA $2000
  LDA #%00011110
  STA $2001
  
  JMP GameEngineDone

;;;;;;;;; 

EngineSecretos:
  ; Decrement input timer if active
  LDA input_timer
  BEQ SecretosCheckButtons  ; if timer is 0, check buttons
  DEC input_timer           ; decrement timer
  JMP GameEngineDone        ; skip button checks while timer is active

SecretosCheckButtons:
  ; Decrement input delay timer if active
  LDA secret_input_delay
  BEQ CheckSecretInputs
  DEC secret_input_delay
  JMP SecretosDone

CheckSecretInputs:
  ; Check for B button to return to title screen (no delay)
  LDA buttons1
  AND #%01000000      ; B button
  BEQ CheckSecretUp   ; not pressed, check other buttons
  JMP ReturnToTitleFromSecretos

CheckSecretUp:
  ; Check for UP button to increment current digit
  LDA buttons1
  AND #%00001000      ; UP button
  BEQ CheckSecretDown
  JSR IncrementSecretDigit
  LDA #$01                ; set flag to redraw during NMI
  STA secret_draw_flag
  LDA #10                 ; set input delay
  STA secret_input_delay
  JMP SecretosDone

CheckSecretDown:
  ; Check for DOWN button to decrement current digit
  LDA buttons1
  AND #%00000100      ; DOWN button
  BEQ CheckSecretLeft
  JSR DecrementSecretDigit
  LDA #$01                ; set flag to redraw during NMI
  STA secret_draw_flag
  LDA #10                 ; set input delay
  STA secret_input_delay
  JMP SecretosDone

CheckSecretLeft:
  ; Check for LEFT button to move cursor left
  LDA buttons1
  AND #%00000010      ; LEFT button
  BEQ CheckSecretRight
  LDA secret_cursor
  BEQ CheckSecretRight  ; already at leftmost
  DEC secret_cursor
  LDA #$01                ; set flag to redraw during NMI
  STA secret_draw_flag
  LDA #10                 ; set input delay
  STA secret_input_delay
  JMP SecretosDone

CheckSecretRight:
  ; Check for RIGHT button to move cursor right
  LDA buttons1
  AND #%00000001      ; RIGHT button
  BEQ CheckSecretA
  LDA secret_cursor
  CMP #$03
  BEQ CheckSecretA      ; already at rightmost
  INC secret_cursor
  LDA #$01                ; set flag to redraw during NMI
  STA secret_draw_flag
  LDA #10                 ; set input delay
  STA secret_input_delay
  JMP SecretosDone

CheckSecretA:
  ; Check for A button to check the code
  LDA buttons1
  AND #%10000000      ; A button
  BEQ SecretosDone
  JSR CheckSecretCode
  LDA #$01                ; set flag to redraw during NMI
  STA secret_draw_flag
  LDA #10                 ; set input delay
  STA secret_input_delay
  JMP SecretosDone

ReturnToTitleFromSecretos:
  ; Return to title screen
  LDA #%00000000
  STA $2001
  STA $2000
  
  ; Wait for vblank
  LDA $2002
WaitVBlankReturnToTitle:
  BIT $2002
  BPL WaitVBlankReturnToTitle
  
  ; Load title screen CHR bank (bank 0)
  LDA #$00
  JSR Bankswitch
  
  ; Load title palettes and screen
  JSR LoadTitlePalettes
  JSR LoadTitleScreen
  
  ; Reset menu to start position
  LDA #$00
  STA menu_selection
  LDA #MENU_START_Y
  STA menu_cursor_y
  LDA #MENU_START_X
  STA menu_cursor_x
  
  ; Set input timer to 120 frames (2 seconds) to prevent immediate re-entry
  LDA #60
  STA input_timer
  
  ; Set state back to title
  LDA #STATETITLE
  STA gamestate
  
  ; Turn screen back on
  LDA #%10010000
  STA $2000
  LDA #%00011110
  STA $2001
  JMP GameEngineDone

SecretosDone:
  JMP GameEngineDone

EngineSecretMessage:
  ; Display secret message character by character
  ; Decrement timer
  DEC secret_msg_timer
  LDA secret_msg_timer
  BNE CheckSecretMsgButtons  ; if timer not 0, check buttons
  
  ; Timer reached 0, set flag to write next character during NMI
  LDA #$03                    ; reset timer
  STA secret_msg_timer
  LDA #$01                    ; set flag to write during NMI
  STA secret_msg_draw_flag
  JMP CheckSecretMsgButtons

CheckSecretMsgButtons:
  ; Check for B button to return to title screen
  LDA buttons1
  AND #%01000000      ; B button
  BEQ SecretMsgDone
  JMP ReturnToTitleFromSecret

SecretMsgDone:
  JMP GameEngineDone

EngineSecretImage:
  ; Display secret image, wait for B button to return to title
  ; Check for B button to return to title screen
  LDA buttons1
  AND #%01000000      ; B button
  BEQ SecretImgDone
  JMP ReturnToTitleFromSecretImage

SecretImgDone:
  JMP GameEngineDone

ReturnToTitleFromSecretImage:
  ; Return to title screen from secret image
  LDA #%00000000
  STA $2001
  STA $2000
  
  ; Wait for vblank
  LDA $2002
WaitVBlankReturnFromImage:
  BIT $2002
  BPL WaitVBlankReturnFromImage
  
  ; Switch back to title CHR bank (bank 0)
  LDA #$00
  JSR Bankswitch        ; use the bankswitch subroutine
  
  ; Load title palettes and screen
  JSR LoadTitlePalettes
  JSR LoadTitleScreen
  
  ; Reset to title state
  LDA #STATETITLE
  STA gamestate
  
  ; Reset menu selection
  LDA #$00
  STA menu_selection
  LDA #$B0
  STA menu_cursor_y
  LDA #$40
  STA menu_cursor_x
  
  ; Set input timer to prevent immediate re-trigger
  LDA #120
  STA input_timer
  
  ; Turn screen back on
  LDA #%10010000
  STA $2000
  LDA #%00011110
  STA $2001
  RTS

ReturnToTitleFromSecret:
  ; Return to title screen
  LDA #%00000000
  STA $2001
  STA $2000
  
  ; Wait for vblank
  LDA $2002
WaitVBlankReturnToTitle2:
  BIT $2002
  BPL WaitVBlankReturnToTitle2
  
  ; Load title screen CHR bank (bank 0)
  LDA #$00
  JSR Bankswitch
  
  ; Load title palettes and screen
  JSR LoadTitlePalettes
  JSR LoadTitleScreen
  
  ; Reset menu to start position
  LDA #$00
  STA menu_selection
  LDA #MENU_START_Y
  STA menu_cursor_y
  LDA #MENU_START_X
  STA menu_cursor_x
  
  ; Set input timer
  LDA #60
  STA input_timer
  
  ; Set state back to title
  LDA #STATETITLE
  STA gamestate
  
  ; Turn screen back on
  LDA #%10010000
  STA $2000
  LDA #%00011110
  STA $2001
  JMP GameEngineDone
 
EngineGameOver:
  ;;if start button pressed
  ;;  turn screen off
  ;;  load title screen
  ;;  go to Title State
  ;;  turn screen on 
  JMP GameEngineDone

EngineWinScreen:
  ; Check if start button pressed to restart game
  LDA buttons1
  AND #%00010000        ; Start button (bit 4)
  BNE RestartGame       ; Player 1 start pressed
  
  LDA buttons2
  AND #%00010000        ; Start button (bit 4)
  BNE RestartGame       ; Player 2 start pressed
  
  JMP GameEngineDone    ; no start pressed, stay in win screen

RestartGame:
  ; Clear win message
  JSR ClearWinMessage

  ; Reset player positions
  LDA #$60              ; start high in the air (above ground)
  STA bally
  STA ball2y            ; player 2 same Y position
  
  LDA #$40              ; player 1 starts on left side
  STA ballx
  
  LDA #$C0              ; player 2 starts on right side  
  STA ball2x

  ; Reset items
  LDA #$00
  STA itemactive       ; no left zone item active
  STA item2active      ; no right zone item active
  STA cakeactive       ; no cake active
  
  ; Reset physics
  STA velocity_y       ; no vertical velocity
  STA jump_pressed     ; no jump pressed
  STA jump_counter     ; not jumping
  STA jump_counter2    ; player 2 not jumping
  STA on_ground        ; player 1 start in air
  STA on_ground2       ; player 2 start in air
  STA speed_level      ; reset speed level
  STA total_score      ; reset combined score
  STA winner           ; clear winner
  STA p1_anim_frame    ; reset Player 1 animation
  STA p1_anim_timer
  STA p1_facing
  STA p2_anim_frame    ; reset Player 2 animation
  STA p2_anim_timer
  STA p2_facing

  ; Reset scores to 0
  STA scoreOnes
  STA scoreTens
  STA scoreHundreds
  STA score2Ones
  STA score2Tens
  STA score2Hundreds

  ; Set game state to PLAYING (not title screen)
  LDA #STATEPLAYING
  STA gamestate
  
  JMP GameEngineDone
 
;;;;;;;;;;;

EnginePlaying:
  JSR HandlePlayerMovement
  JSR HandleJump
  JSR ApplyPhysics
  JSR UpdatePlayerAnimation
  JSR UpdateProgressiveSpeed
  JSR HandleFallingItemLeftZone
  JSR HandleFallingItemRightZone
  JSR HandleCakeMovement
  JMP GameEngineDone

; Physics-based player movement
HandlePlayerMovement:
  ; Handle Player 1 horizontal movement - ONLY when on ground
  LDA on_ground
  BEQ HandlePlayer2Movement  ; if player 1 not on ground, skip to player 2

; Player 1 movement (left side, can move right to center)
MoveBall1Left:
  LDA buttons1
  AND #%00000010
  BEQ MoveBall1LeftDone   ; left button not pressed
  
  LDA ballx
  SEC
  SBC ballspeedx        ; ballx position = ballx - ballspeedx
  STA ballx
  
  LDA ballx
  CMP #LEFTWALL
  BCS MoveBall1LeftDone  ; if ball x > left wall, still on screen
  LDA #LEFTWALL
  STA ballx             ; clamp to left wall
MoveBall1LeftDone:

MoveBall1Right:
  LDA buttons1
  AND #%00000001
  BEQ MoveBall1RightDone   ; right button not pressed

  LDA ballx
  CLC
  ADC ballspeedx        ; ballx position = ballx + ballspeedx
  STA ballx

  ; Player 1 can't go past center (x = $80)
  LDA ballx
  CMP #$80
  BCC MoveBall1RightDone ; if ball x < center, still on left side
  LDA #$7F
  STA ballx             ; clamp to just left of center
MoveBall1RightDone:

HandlePlayer2Movement:
  ; Handle Player 2 horizontal movement - ONLY when on ground
  LDA on_ground2
  BEQ SkipHorizontalMovement  ; if player 2 not on ground, skip horizontal movement

; Player 2 movement (right side, can move left to center)  
MoveBall2Left:
  LDA buttons2
  AND #%00000010
  BEQ MoveBall2LeftDone   ; left button not pressed
  
  LDA ball2x
  SEC
  SBC ballspeedx        ; ball2x position = ball2x - ballspeedx
  STA ball2x
  
  ; Player 2 can't go past center (x = $80)
  LDA ball2x
  CMP #$80
  BCS MoveBall2LeftDone  ; if ball x >= center, still on right side
  LDA #$81
  STA ball2x             ; clamp to just right of center
MoveBall2LeftDone:

MoveBall2Right:
  LDA buttons2
  AND #%00000001
  BEQ MoveBall2RightDone   ; right button not pressed

  LDA ball2x
  CLC
  ADC ballspeedx        ; ball2x position = ball2x + ballspeedx
  STA ball2x

  LDA ball2x
  CMP #RIGHTWALL
  BCC MoveBall2RightDone ; if ball x < right wall, still on screen
  LDA #RIGHTWALL
  STA ball2x             ; clamp to right wall
MoveBall2RightDone:

SkipHorizontalMovement:
  RTS

  ; Handle jump input (NO BARRIERS - for debugging)
HandleJump:
  ; Visual debug: show on_ground state with sprite tile
  LDA on_ground
  BEQ NotOnGround
  LDA #$32              ; tile $32 when on ground
  STA $0201             ; sprite 0 tile
  JMP CheckJumpButton
NotOnGround:
  LDA #$33              ; tile $33 when in air
  STA $0201             ; sprite 0 tile

CheckJumpButton:
  ; Debug: Show the raw buttons1 value as sprite tile
  LDA buttons1
  STA $0201             ; show raw button value as tile number
  
  LDA buttons1
  AND #%10000000        ; A button for jump (bit 7)
  BEQ HandleJumpPlayer2    ; Player 1 A button not pressed, check Player 2
  
  ; Check if already jumping or in air
  LDA on_ground
  BEQ HandleJumpPlayer2    ; can't jump if not on ground, check Player 2
  
  ; Visual debug: A button was pressed - change sprite to tile $34
  LDA #$34
  STA $0201             ; sprite 0 tile (different from $32/$33)
  
  ; Start jump with counter (simple approach)
  LDA #JUMP_DURATION    ; jump for specified duration
  STA jump_counter
  LDA #$00
  STA on_ground         ; no longer on ground
  
  ; Set jump_pressed for debugging
  LDA #$01
  STA jump_pressed

HandleJumpPlayer2:
  ; Check Player 2 jump button
  LDA buttons2
  AND #%10000000        ; A button for jump (bit 7)
  BEQ HandleJumpDone    ; A button not pressed
  
  ; Check if player 2 already jumping or in air
  LDA on_ground2
  BEQ HandleJumpDone    ; can't jump if not on ground
  
  ; Start jump with counter for player 2
  LDA #JUMP_DURATION    ; jump for specified duration
  STA jump_counter2
  LDA #$00
  STA on_ground2        ; player 2 no longer on ground

HandleJumpDone:
  RTS

  ; SIMPLE COUNTER-BASED PHYSICS
ApplyPhysics:
  ; Handle Player 1 jumping (if jump_counter > 0)
  LDA jump_counter
  BEQ Player1NotJumping        ; if counter = 0, not jumping
  
  ; Still jumping - move up and decrease counter
  DEC jump_counter      ; decrease jump counter
  LDA bally
  SEC
  SBC #JUMP_SPEED       ; move up at jump speed
  STA bally
  
  ; Check ceiling collision
  CMP #TOPWALL
  BCS Player1NotJumping        ; if y >= top wall, no ceiling hit
  LDA #TOPWALL
  STA bally
  LDA #$00
  STA jump_counter      ; stop jumping

Player1NotJumping:
  ; Apply gravity to Player 1 (fall down) if not on ground
  LDA on_ground
  BNE ApplyPhysicsPlayer2      ; skip if on ground

ApplyGravityPlayer1:
  ; Simple gravity - move down at fall speed
  LDA bally
  CLC
  ADC #FALL_SPEED       ; move down at fall speed
  STA bally

CheckGroundHitPlayer1:
  ; Debug: Show current bally position as sprite attribute
  LDA bally
  STA $0202             ; show Y position as sprite color
  
  ; Check if player 1 hit or passed through the ground
  LDA bally
  CMP #GROUND_Y
  BCC ApplyPhysicsPlayer2      ; if y < ground level, still in air
  
  ; Hit ground - land
  LDA #GROUND_Y
  STA bally
  LDA #$00
  STA jump_counter      ; stop any jumping
  LDA #$01
  STA on_ground         ; mark as on ground
  
  ; Debug: Show ground hit with distinctive color
  LDA #$FF
  STA $0202             ; show ground hit with color $FF

ApplyPhysicsPlayer2:
  ; Handle Player 2 jumping (if jump_counter2 > 0)
  LDA jump_counter2
  BEQ Player2NotJumping        ; if counter = 0, not jumping
  
  ; Still jumping - move up and decrease counter
  DEC jump_counter2     ; decrease jump counter
  LDA ball2y
  SEC
  SBC #JUMP_SPEED       ; move up at jump speed
  STA ball2y
  
  ; Check ceiling collision
  CMP #TOPWALL
  BCS Player2NotJumping        ; if y >= top wall, no ceiling hit
  LDA #TOPWALL
  STA ball2y
  LDA #$00
  STA jump_counter2     ; stop jumping

Player2NotJumping:
  ; Apply gravity to Player 2 (fall down) if not on ground
  LDA on_ground2
  BNE SkipMovement      ; skip if on ground

ApplyGravityPlayer2:
  ; Simple gravity - move down at fall speed
  LDA ball2y
  CLC
  ADC #FALL_SPEED       ; move down at fall speed
  STA ball2y

CheckGroundHitPlayer2:
  ; Check if player 2 hit or passed through the ground
  LDA ball2y
  CMP #GROUND_Y
  BCC SkipMovement      ; if y < ground level, still in air
  
  ; Hit ground - land
  LDA #GROUND_Y
  STA ball2y
  LDA #$00
  STA jump_counter2     ; stop any jumping
  LDA #$01
  STA on_ground2        ; mark player 2 as on ground

SkipMovement:
  RTS

UpdatePlayerAnimation:
  ; Update Player 1 animation
  ; Check if Player 1 is moving horizontally and update facing direction
  LDA buttons1
  AND #%00000001        ; check right button
  BEQ CheckP1Left
  ; Moving right
  LDA #$01
  STA p1_facing         ; 1 = facing right
  JMP P1IsMoving
CheckP1Left:
  LDA buttons1
  AND #%00000010        ; check left button
  BEQ Player1NotMoving  ; if no horizontal movement, reset animation
  ; Moving left
  LDA #$00
  STA p1_facing         ; 0 = facing left
  
P1IsMoving:
  ; Player 1 is moving - update animation timer
  INC p1_anim_timer
  LDA p1_anim_timer
  CMP #ANIM_SPEED       ; check if it's time to change frame
  BCC UpdatePlayer2Anim ; if timer < ANIM_SPEED, don't change frame yet
  
  ; Time to change frame
  LDA #$00
  STA p1_anim_timer     ; reset timer
  LDA p1_anim_frame
  CLC
  ADC #$01              ; increment frame
  CMP #$03              ; check if we've reached frame 3
  BCC StoreP1Frame      ; if frame < 3, store it
  LDA #$00              ; else reset to frame 0
StoreP1Frame:
  STA p1_anim_frame
  JMP UpdatePlayer2Anim

Player1NotMoving:
  ; Set Player 1 to idle frame when not moving
  LDA #IDLE_FRAME
  STA p1_anim_frame
  LDA #$00
  STA p1_anim_timer

UpdatePlayer2Anim:
  ; Update Player 2 animation
  ; Check if Player 2 is moving horizontally and update facing direction
  LDA buttons2
  AND #%00000001        ; check right button
  BEQ CheckP2Left
  ; Moving right
  LDA #$01
  STA p2_facing         ; 1 = facing right
  JMP P2IsMoving
CheckP2Left:
  LDA buttons2
  AND #%00000010        ; check left button
  BEQ Player2NotMoving  ; if no horizontal movement, reset animation
  ; Moving left
  LDA #$00
  STA p2_facing         ; 0 = facing left
  
P2IsMoving:
  ; Player 2 is moving - update animation timer
  INC p2_anim_timer
  LDA p2_anim_timer
  CMP #ANIM_SPEED       ; check if it's time to change frame
  BCC UpdateAnimationDone ; if timer < ANIM_SPEED, don't change frame yet
  
  ; Time to change frame
  LDA #$00
  STA p2_anim_timer     ; reset timer
  LDA p2_anim_frame
  CLC
  ADC #$01              ; increment frame
  CMP #$03              ; check if we've reached frame 3
  BCC StoreP2Frame      ; if frame < 3, store it
  LDA #$00              ; else reset to frame 0
StoreP2Frame:
  STA p2_anim_frame
  JMP UpdateAnimationDone

Player2NotMoving:
  ; Set Player 2 to idle frame when not moving
  LDA #IDLE_FRAME
  STA p2_anim_frame
  LDA #$00
  STA p2_anim_timer

UpdateAnimationDone:
  RTS

; Handle falling item (Left Zone - Player 1)
HandleFallingItemLeftZone:
  LDA itemactive
  BEQ JumpToSpawnNewItem ; if no item active, try to spawn one
  
  ; Move item down (using progressive speed)
  JSR GetCurrentItemSpeed  ; get current speed in A
  STA itemspeed            ; update itemspeed variable
  LDA itemy
  CLC
  ADC itemspeed
  STA itemy
  
  ; Check if item hit bottom of screen
  LDA itemy
  CMP #BOTTOMWALL
  BCC CheckItemCollision ; if item y < bottom wall, check collision
  
  ; Item hit bottom, destroy it
  LDA #$00
  STA itemactive
  JMP HandleFallingItemDone

JumpToSpawnNewItem:
  JMP SpawnNewItemLeftZone

CheckItemCollision:
  ; Check collision with Player 1 first
  JSR CheckPlayer1ItemCollision
  ; Check collision with Player 2
  JSR CheckPlayer2ItemCollision
  JMP NoCollision

CheckPlayer1ItemCollision:
  ; Check X collision with Player 1
  LDA ballx
  CLC
  ADC #$08              ; player 1 right edge
  CMP itemx
  BCC CheckPlayer1ItemDone       ; if player right < item left, no collision
  
  LDA itemx
  CLC  
  ADC #$08              ; item right edge
  CMP ballx
  BCC CheckPlayer1ItemDone       ; if item right < player left, no collision
  
  ; Check Y collision with Player 1
  LDA bally
  CLC
  ADC #$08              ; player 1 bottom edge
  CMP itemy
  BCC CheckPlayer1ItemDone       ; if player bottom < item top, no collision
  
  LDA itemy
  CLC
  ADC #$08              ; item bottom edge  
  CMP bally
  BCC CheckPlayer1ItemDone       ; if item bottom < player top, no collision
  
  ; Player 1 collision detected! Check item type
  LDA itemtype
  BEQ GoodItemCollisionPlayer1  ; if itemtype = 0, good item for Player 1
  CMP #$01
  BEQ BadItemCollisionPlayer1   ; if itemtype = 1, bad item for Player 1
  ; itemtype = 2, cake item caught by Player 1
  JMP CakeItemCollisionPlayer1

CheckPlayer1ItemDone:
  RTS

CheckPlayer2ItemCollision:
  ; Check X collision with Player 2
  LDA ball2x
  CLC
  ADC #$08              ; player 2 right edge
  CMP itemx
  BCC CheckPlayer2ItemDone       ; if player right < item left, no collision
  
  LDA itemx
  CLC  
  ADC #$08              ; item right edge
  CMP ball2x
  BCC CheckPlayer2ItemDone       ; if item right < player left, no collision
  
  ; Check Y collision with Player 2
  LDA ball2y
  CLC
  ADC #$08              ; player 2 bottom edge
  CMP itemy
  BCC CheckPlayer2ItemDone       ; if player bottom < item top, no collision
  
  LDA itemy
  CLC
  ADC #$08              ; item bottom edge  
  CMP ball2y
  BCC CheckPlayer2ItemDone       ; if item bottom < player top, no collision
  
  ; Player 2 collision detected! Check item type
  LDA itemtype
  BEQ GoodItemCollisionPlayer2  ; if itemtype = 0, good item for Player 2
  CMP #$01
  BEQ BadItemCollisionPlayer2   ; if itemtype = 1, bad item for Player 2
  ; itemtype = 2, cake item caught by Player 2
  JMP CakeItemCollisionPlayer2

CheckPlayer2ItemDone:
  RTS
  
BadItemCollisionPlayer1:
  ; Bad item (broken heart) for Player 1 - subtract 1 point
  JSR DecrementScore
  JMP ItemCollisionDone

BadItemCollisionPlayer2:
  ; Bad item (broken heart) for Player 2 - subtract 1 point
  JSR DecrementScore2
  JMP ItemCollisionDone
  
GoodItemCollisionPlayer1:
  ; Good item (heart) for Player 1 - add 1 point
  JSR IncrementScore
  JMP ItemCollisionDone

GoodItemCollisionPlayer2:
  ; Good item (heart) for Player 2 - add 1 point
  JSR IncrementScore2
  JMP ItemCollisionDone

CakeItemCollisionPlayer1:
  ; Player 1 caught cake - it moves RIGHT (targets Player 2)
  LDA #$01
  STA cakeactive        ; activate cake
  LDA itemx             ; start cake at the X position where it was caught
  STA cakex
  LDA #$00              ; 0 = moving right (targets Player 2)
  STA cakedirection
  JMP ItemCollisionDone

CakeItemCollisionPlayer2:
  ; Player 2 caught cake - it moves LEFT (targets Player 1)
  LDA #$01
  STA cakeactive        ; activate cake
  LDA itemx             ; start cake at the X position where it was caught
  STA cakex
  LDA #$01              ; 1 = moving left (targets Player 1)
  STA cakedirection
  ; Cake Y position is fixed at floor level (GROUND_Y)
  
ItemCollisionDone:
  LDA #$00
  STA itemactive
  JMP HandleFallingItemDone

NoCollision:
  JMP HandleFallingItemDone

SpawnNewItemLeftZone:
  ; Generate random numbers for position and type (Left Zone)
  LDA randomseed
  CLC
  ADC #$17              ; add prime number
  STA randomseed
  
  ; Use bits 6-7 to determine item type (0, 1, or 2)
  AND #$C0              ; check bits 6-7
  CMP #$00              ; 00 = good item (heart)
  BEQ SetGoodItemLeft
  CMP #$40              ; 01 = bad item (broken heart)  
  BEQ SetBadItemLeft
  ; 10 or 11 = cake
  LDA #$02              ; cake item
  JMP SetItemTypeLeft
SetBadItemLeft:
  LDA #$01              ; bad item
  JMP SetItemTypeLeft
SetGoodItemLeft:
  LDA #$00              ; good item
SetItemTypeLeft:
  STA itemtype
  
  ; Generate random X position (LEFT ZONE ONLY: $10 to $7F)
  LDA randomseed
  CLC
  ADC #$23              ; add another prime number for X position
  STA randomseed
  AND #$3F              ; keep in range 0-63
  CLC
  ADC #$10              ; add offset: $10 to $4F (left zone)
  STA itemx
  LDA #TOPWALL
  STA itemy
  LDA #$01
  STA itemactive

HandleFallingItemDone:
  RTS

; Handle falling item (Right Zone - Player 2)
HandleFallingItemRightZone:
  LDA item2active
  BEQ JumpToSpawnNewItemRight ; if no item active, try to spawn one
  
  ; Move item down (using progressive speed)
  JSR GetCurrentItemSpeed  ; get current speed in A
  STA item2speed           ; update item2speed variable
  LDA item2y
  CLC
  ADC item2speed
  STA item2y
  
  ; Check if item hit bottom of screen
  LDA item2y
  CMP #BOTTOMWALL
  BCC CheckItem2Collision ; if item y < bottom wall, check collision
  
  ; Item hit bottom, destroy it
  LDA #$00
  STA item2active
  JMP HandleFallingItemRightZoneDone

JumpToSpawnNewItemRight:
  JMP SpawnNewItemRightZone

CheckItem2Collision:
  ; Right zone items can only be collected by Player 2
  JSR CheckPlayer2Item2Collision
  JMP NoCollision2

CheckPlayer2Item2Collision:
  ; Check X collision with Player 2
  LDA ball2x
  CLC
  ADC #$08              ; player 2 right edge
  CMP item2x
  BCC CheckPlayer2Item2Done       ; if player right < item left, no collision
  
  LDA item2x
  CLC  
  ADC #$08              ; item right edge
  CMP ball2x
  BCC CheckPlayer2Item2Done       ; if item right < player left, no collision
  
  ; Check Y collision with Player 2
  LDA ball2y
  CLC
  ADC #$08              ; player 2 bottom edge
  CMP item2y
  BCC CheckPlayer2Item2Done       ; if player bottom < item top, no collision
  
  LDA item2y
  CLC
  ADC #$08              ; item bottom edge  
  CMP ball2y
  BCC CheckPlayer2Item2Done       ; if item bottom < player top, no collision
  
  ; Player 2 collision detected! Check item type
  LDA item2type
  BEQ GoodItem2CollisionPlayer2  ; if itemtype = 0, good item for Player 2
  CMP #$01
  BEQ BadItem2CollisionPlayer2   ; if itemtype = 1, bad item for Player 2
  ; itemtype = 2, cake item caught by Player 2
  JMP CakeItem2CollisionPlayer2

CheckPlayer2Item2Done:
  RTS

GoodItem2CollisionPlayer2:
  ; Good item (heart) for Player 2 - add 1 point
  JSR IncrementScore2
  JMP Item2CollisionDone

BadItem2CollisionPlayer2:
  ; Bad item (broken heart) for Player 2 - subtract 1 point
  JSR DecrementScore2
  JMP Item2CollisionDone

CakeItem2CollisionPlayer2:
  ; Player 2 caught cake - it moves LEFT (targets Player 1)
  LDA #$01
  STA cakeactive        ; activate cake
  LDA item2x            ; start cake at the X position where it was caught
  STA cakex
  LDA #$01              ; 1 = moving left (targets Player 1)
  STA cakedirection
  JMP Item2CollisionDone

Item2CollisionDone:
  LDA #$00
  STA item2active
  JMP HandleFallingItemRightZoneDone

NoCollision2:
  JMP HandleFallingItemRightZoneDone

SpawnNewItemRightZone:
  ; Generate random numbers for position and type (Right Zone)
  LDA randomseed2
  CLC
  ADC #$17              ; add prime number
  STA randomseed2
  
  ; Use bits 6-7 to determine item type (0, 1, or 2)
  AND #$C0              ; check bits 6-7
  CMP #$00              ; 00 = good item (heart)
  BEQ SetGoodItemRight
  CMP #$40              ; 01 = bad item (broken heart)  
  BEQ SetBadItemRight
  ; 10 or 11 = cake
  LDA #$02              ; cake item
  JMP SetItemTypeRight
SetBadItemRight:
  LDA #$01              ; bad item
  JMP SetItemTypeRight
SetGoodItemRight:
  LDA #$00              ; good item
SetItemTypeRight:
  STA item2type
  
  ; Generate random X position (RIGHT ZONE ONLY: $81 to $E0)
  LDA randomseed2
  CLC
  ADC #$23              ; add another prime number for X position
  STA randomseed2
  AND #$3F              ; keep in range 0-63
  CLC
  ADC #$81              ; add offset: $81 to $C0 (right zone)
  STA item2x
  LDA #TOPWALL
  STA item2y
  LDA #$01
  STA item2active

HandleFallingItemRightZoneDone:
  RTS

; Handle cake movement
HandleCakeMovement:
  LDA cakeactive
  BEQ HandleCakeMovementDone  ; if no cake active, skip
  
  ; Check collision with players first (before moving)
  JSR CheckCakePlayerCollision
  
  ; Move cake based on direction
  LDA cakedirection
  BEQ MoveCakeRight     ; if direction = 0, move right
  
MoveCakeLeft:
  ; Move cake to the left
  LDA cakex
  SEC
  SBC cakespeed
  STA cakex
  
  ; Check if cake moved off left edge
  CMP #$04              ; left edge
  BCS HandleCakeMovementDone
  
  ; Cake moved off screen, deactivate it
  LDA #$00
  STA cakeactive
  JMP HandleCakeMovementDone

MoveCakeRight:
  ; Move cake to the right
  LDA cakex
  CLC
  ADC cakespeed
  STA cakex
  
  ; Check if cake moved off right edge
  CMP #$F8              ; right edge + some margin
  BCC HandleCakeMovementDone
  
  ; Cake moved off screen, deactivate it
  LDA #$00
  STA cakeactive

HandleCakeMovementDone:
  RTS

CheckCakePlayerCollision:
  ; Check collision based on cake direction (target player only)
  LDA cakedirection
  BEQ CheckPlayer2CakeCollision  ; if direction = 0 (right), check Player 2
  
CheckPlayer1CakeCollision:
  ; Cake moving left - only check Player 1 collision
  LDA on_ground
  BEQ CakeCollisionDone  ; if Player 1 not on ground, no collision
  
  ; Check X collision with Player 1
  LDA ballx
  CLC
  ADC #$08              ; player 1 right edge
  CMP cakex
  BCC CakeCollisionDone  ; if player right < cake left, no collision
  
  LDA cakex
  CLC  
  ADC #$08              ; cake right edge
  CMP ballx
  BCC CakeCollisionDone  ; if cake right < player left, no collision
  
  ; Check Y collision with Player 1 (both should be at ground level)
  LDA bally
  CMP #GROUND_Y
  BNE CakeCollisionDone  ; if Player 1 not at ground level, no collision
  
  ; Collision detected with Player 1!
  ; Cake was moving left (thrown by Player 2)
  JSR DecrementScore    ; Player 1 loses a point
  JSR IncrementScore2   ; Player 2 gains a point
  LDA #$00
  STA cakeactive        ; destroy the cake
  RTS

CheckPlayer2CakeCollision:
  ; Cake moving right - only check Player 2 collision
  LDA on_ground2
  BEQ CakeCollisionDone  ; if Player 2 not on ground, no collision possible
  
  ; Check X collision with Player 2
  LDA ball2x
  CLC
  ADC #$08              ; player 2 right edge
  CMP cakex
  BCC CakeCollisionDone  ; if player right < cake left, no collision
  
  LDA cakex
  CLC  
  ADC #$08              ; cake right edge
  CMP ball2x
  BCC CakeCollisionDone  ; if cake right < player left, no collision
  
  ; Check Y collision with Player 2 (both should be at ground level)
  LDA ball2y
  CMP #GROUND_Y
  BNE CakeCollisionDone  ; if Player 2 not at ground level, no collision
  
  ; Collision detected with Player 2!
  ; Cake was moving right (thrown by Player 1)
  JSR DecrementScore2   ; Player 2 loses a point
  JSR IncrementScore    ; Player 1 gains a point
  LDA #$00
  STA cakeactive        ; destroy the cake

CakeCollisionDone:
  RTS

UpdatePlayer1Sprites:
  ; Player 1 16x16 character (sprites 0-3)
  ; Set Y positions for all sprites first
  LDA bally
  STA $0200             ; sprite 0 Y position
  STA $0204             ; sprite 1 Y position
  CLC
  ADC #$08              ; Y position + 8 pixels for bottom sprites
  STA $0208             ; sprite 2 Y position
  STA $020C             ; sprite 3 Y position
  
  ; Set attributes and positions based on facing direction
  LDA p1_facing
  BEQ P1FacingLeft
  
P1FacingRight:
  ; Facing right - no flip, normal positioning  
  LDA #%00000000        ; palette 0, no flip
  STA $0202             ; sprite 0 attributes
  STA $0206             ; sprite 1 attributes
  STA $020A             ; sprite 2 attributes
  STA $020E             ; sprite 3 attributes
  
  ; Normal X positions (left sprite, then right sprite)
  LDA ballx
  STA $0203             ; sprite 0 X position (left)
  STA $020B             ; sprite 2 X position (left)
  CLC
  ADC #$08              ; X position + 8 pixels for right sprites
  STA $0207             ; sprite 1 X position (right)
  STA $020F             ; sprite 3 X position (right)
  JMP P1PositionsDone
  
P1FacingLeft:
  ; Facing left - flip horizontally and swap positions
  LDA #%01000000        ; palette 0 + horizontal flip
  STA $0202             ; sprite 0 attributes
  STA $0206             ; sprite 1 attributes
  STA $020A             ; sprite 2 attributes
  STA $020E             ; sprite 3 attributes
  
  ; Swapped X positions (what was right is now left due to flip)
  LDA ballx
  CLC
  ADC #$08              ; X position + 8 pixels
  STA $0203             ; sprite 0 X position (was left, now right)
  STA $020B             ; sprite 2 X position (was left, now right)
  LDA ballx
  STA $0207             ; sprite 1 X position (was right, now left)
  STA $020F             ; sprite 3 X position (was right, now left)
  
P1PositionsDone:
  
  ; Check if player is jumping first
  LDA on_ground
  BEQ JumpToPlayer1JumpingTiles  ; if not on ground, use jumping tiles
  
  ; Set tiles based on animation frame (0, 1, 2, or IDLE)
  LDA p1_anim_frame
  CMP #IDLE_FRAME
  BEQ Player1IdleTiles
  
  ; Walking animation frames (0, 1, or 2)
  ; Base tiles: $20,$21,$30,$31 + (frame * 2)
  ASL A                 ; multiply by 2 (frame * 2)
  TAX                   ; use as offset
  
  ; Calculate the four tile values
  LDA #$20
  CLC
  ADC p1_anim_frame
  ADC p1_anim_frame     ; top-left tile
  TAY                   ; store in Y
  
  LDA #$21
  CLC
  ADC p1_anim_frame
  ADC p1_anim_frame     ; top-right tile
  TAX                   ; store in X
  
  ; Check facing direction to assign tiles correctly
  LDA p1_facing
  BEQ P1WalkingLeft
  
P1WalkingRight:
  ; Normal assignment (not flipped)
  STY $0201             ; top-left
  STX $0205             ; top-right
  
  LDA #$30
  CLC
  ADC p1_anim_frame
  ADC p1_anim_frame     ; bottom-left tile
  STA $0209
  
  LDA #$31
  CLC
  ADC p1_anim_frame
  ADC p1_anim_frame     ; bottom-right tile
  STA $020D
  RTS
  
P1WalkingLeft:
  ; Swapped assignment (flipped sprites) - swap both positions AND tile numbers
  ; When flipped: left tile becomes right tile, right tile becomes left tile
  STY $0201             ; top-left tile (20) goes to sprite 0 (left position, but will be flipped to look right)
  STX $0205             ; top-right tile (21) goes to sprite 1 (right position, but will be flipped to look left)
  
  LDA #$30
  CLC
  ADC p1_anim_frame
  ADC p1_anim_frame     ; bottom-left tile
  STA $0209             ; goes to sprite 2 (left position, but will be flipped to look right)
  
  LDA #$31
  CLC
  ADC p1_anim_frame
  ADC p1_anim_frame     ; bottom-right tile
  STA $020D             ; goes to sprite 3 (right position, but will be flipped to look left)
  RTS

JumpToPlayer1JumpingTiles:
  JMP Player1JumpingTiles

Player1IdleTiles:
  ; Idle animation tiles (standing still) - original standing tiles
  LDA p1_facing
  BEQ P1IdleLeft
  
P1IdleRight:
  ; Normal assignment (not flipped)
  LDA #$04              ; idle top-left
  STA $0201
  LDA #$05              ; idle top-right
  STA $0205
  LDA #$14              ; idle bottom-left
  STA $0209
  LDA #$15              ; idle bottom-right
  STA $020D
  RTS
  
P1IdleLeft:
  ; Swapped assignment (flipped sprites) - keep original tile numbers but swap positions
  LDA #$04              ; idle top-left tile goes to sprite 0 (left position, flipped to look right)
  STA $0201
  LDA #$05              ; idle top-right tile goes to sprite 1 (right position, flipped to look left)
  STA $0205
  LDA #$14              ; idle bottom-left tile goes to sprite 2 (left position, flipped to look right)
  STA $0209
  LDA #$15              ; idle bottom-right tile goes to sprite 3 (right position, flipped to look left)
  STA $020D
  RTS

Player1JumpingTiles:
  ; Jumping animation tiles (no flipping needed)
  LDA #$26              ; jump top-left
  STA $0201
  LDA #$27              ; jump top-right
  STA $0205
  LDA #$36              ; jump bottom-left
  STA $0209
  LDA #$37              ; jump bottom-right
  STA $020D
  RTS

UpdatePlayer2Sprites:
  ; Player 2 16x16 character (sprites 4-7)
  ; Set Y positions for all sprites first
  LDA ball2y
  STA $0210             ; sprite 4 Y position
  STA $0214             ; sprite 5 Y position
  CLC
  ADC #$08              ; Y position + 8 pixels for bottom sprites
  STA $0218             ; sprite 6 Y position
  STA $021C             ; sprite 7 Y position
  
  ; Set attributes and positions based on facing direction
  LDA p2_facing
  BEQ P2FacingLeft
  
P2FacingRight:
  ; Facing right - no flip, normal positioning
  LDA #%00000001        ; palette 1, no flip
  STA $0212             ; sprite 4 attributes
  STA $0216             ; sprite 5 attributes
  STA $021A             ; sprite 6 attributes
  STA $021E             ; sprite 7 attributes
  
  ; Normal X positions (left sprite, then right sprite)
  LDA ball2x
  STA $0213             ; sprite 4 X position (left)
  STA $021B             ; sprite 6 X position (left)
  CLC
  ADC #$08              ; X position + 8 pixels for right sprites
  STA $0217             ; sprite 5 X position (right)
  STA $021F             ; sprite 7 X position (right)
  JMP P2PositionsDone
  
P2FacingLeft:
  ; Facing left - flip horizontally and swap positions
  LDA #%01000001        ; palette 1 + horizontal flip
  STA $0212             ; sprite 4 attributes
  STA $0216             ; sprite 5 attributes
  STA $021A             ; sprite 6 attributes
  STA $021E             ; sprite 7 attributes
  
  ; Swapped X positions (what was right is now left due to flip)
  LDA ball2x
  CLC
  ADC #$08              ; X position + 8 pixels
  STA $0213             ; sprite 4 X position (was left, now right)
  STA $021B             ; sprite 6 X position (was left, now right)
  LDA ball2x
  STA $0217             ; sprite 5 X position (was right, now left)
  STA $021F             ; sprite 7 X position (was right, now left)
  
P2PositionsDone:
  
  ; Check if player is jumping first
  LDA on_ground2
  BEQ JumpToPlayer2JumpingTiles  ; if not on ground, use jumping tiles
  
  ; Set tiles based on animation frame (0, 1, 2, or IDLE)
  LDA p2_anim_frame
  CMP #IDLE_FRAME
  BEQ Player2IdleTiles
  
  ; Walking animation frames (0, 1, or 2)
  ; Base tiles: $40,$41,$50,$51 + (frame * 2)
  ASL A                 ; multiply by 2 (frame * 2)
  TAX                   ; use as offset
  
  ; Calculate the four tile values
  LDA #$40
  CLC
  ADC p2_anim_frame
  ADC p2_anim_frame     ; top-left tile
  TAY                   ; store in Y
  
  LDA #$41
  CLC
  ADC p2_anim_frame
  ADC p2_anim_frame     ; top-right tile
  TAX                   ; store in X
  
  ; Check facing direction to assign tiles correctly
  LDA p2_facing
  BEQ P2WalkingLeft
  
P2WalkingRight:
  ; Normal assignment (not flipped)
  STY $0211             ; top-left
  STX $0215             ; top-right
  
  LDA #$50
  CLC
  ADC p2_anim_frame
  ADC p2_anim_frame     ; bottom-left tile
  STA $0219
  
  LDA #$51
  CLC
  ADC p2_anim_frame
  ADC p2_anim_frame     ; bottom-right tile
  STA $021D
  RTS
  
P2WalkingLeft:
  ; Swapped assignment (flipped sprites) - swap both positions AND tile numbers
  ; When flipped: left tile becomes right tile, right tile becomes left tile
  STY $0211             ; top-left tile (40) goes to sprite 4 (left position, but will be flipped to look right)
  STX $0215             ; top-right tile (41) goes to sprite 5 (right position, but will be flipped to look left)
  
  LDA #$50
  CLC
  ADC p2_anim_frame
  ADC p2_anim_frame     ; bottom-left tile
  STA $0219             ; goes to sprite 6 (left position, but will be flipped to look right)
  
  LDA #$51
  CLC
  ADC p2_anim_frame
  ADC p2_anim_frame     ; bottom-right tile
  STA $021D             ; goes to sprite 7 (right position, but will be flipped to look left)
  RTS

JumpToPlayer2JumpingTiles:
  JMP Player2JumpingTiles

Player2IdleTiles:
  ; Idle animation tiles (standing still) - original standing tiles
  LDA p2_facing
  BEQ P2IdleLeft
  
P2IdleRight:
  ; Normal assignment (not flipped)
  LDA #$06              ; idle top-left
  STA $0211
  LDA #$07              ; idle top-right
  STA $0215
  LDA #$16              ; idle bottom-left
  STA $0219
  LDA #$17              ; idle bottom-right
  STA $021D
  RTS
  
P2IdleLeft:
  ; Swapped assignment (flipped sprites) - keep original tile numbers but swap positions
  LDA #$06              ; idle top-left tile goes to sprite 4 (left position, flipped to look right)
  STA $0211
  LDA #$07              ; idle top-right tile goes to sprite 5 (right position, flipped to look left)
  STA $0215
  LDA #$16              ; idle bottom-left tile goes to sprite 6 (left position, flipped to look right)
  STA $0219
  LDA #$17              ; idle bottom-right tile goes to sprite 7 (right position, flipped to look left)
  STA $021D
  RTS

Player2JumpingTiles:
  ; Jumping animation tiles (no flipping needed)
  LDA #$46              ; jump top-left
  STA $0211
  LDA #$47              ; jump top-right
  STA $0215
  LDA #$56              ; jump bottom-left
  STA $0219
  LDA #$57              ; jump bottom-right
  STA $021D
  RTS
 
 


UpdateSprites:
  ; Check if we're on title screen - if so, show menu cursor only
  LDA gamestate
  CMP #STATETITLE
  BEQ JumpToDrawMenuCursor
  
  ; Check if we're on loading screen - if so, show loading items
  CMP #STATELOADING
  BEQ DrawLoadingSprites
  
  ; Check if we're on secretos screen - if so, hide all sprites
  CMP #STATESECRETOS
  BEQ JumpToHideAllSprites
  
  ; Check if we're on secret message screen - if so, hide all sprites
  CMP #STATESECRETMSG
  BEQ JumpToHideAllSprites
  
  ; Check if we're on secret image screen - if so, hide all sprites
  CMP #STATESECRETIMG
  BEQ JumpToHideAllSprites
  
  ; Update Player 1 (16x16 character using sprites 0-3)
  JSR UpdatePlayer1Sprites
  
  ; Update Player 2 (16x16 character using sprites 4-7)  
  JSR UpdatePlayer2Sprites
  JMP ContinueUpdateSprites

JumpToDrawMenuCursor:
  JMP DrawMenuCursor

JumpToHideAllSprites:
  JMP HideAllSprites

DrawLoadingSprites:
  ; Draw the three item sprites for the loading screen
  ; Sprite 0: Heart at row 8, column 15 (Y=$40, X=$78)
  LDA #$40              ; Y position (row 8 * 8 = 64 = $40)
  STA $0200
  LDA #$01              ; heart tile
  STA $0201
  LDA #$02              ; palette 2 (same as in-game)
  STA $0202
  LDA #$78              ; X position (column 15 * 8 = 120 = $78)
  STA $0203
  
  ; Sprite 1: Broken heart at row 14, column 15 (Y=$70, X=$78)
  LDA #$70              ; Y position (row 14 * 8 = 112 = $70)
  STA $0204
  LDA #$02              ; broken heart tile
  STA $0205
  LDA #$02              ; palette 2
  STA $0206
  LDA #$78              ; X position
  STA $0207
  
  ; Sprite 2: Cake at row 20, column 15 (Y=$A0, X=$78)
  LDA #$A0              ; Y position (row 20 * 8 = 160 = $A0)
  STA $0208
  LDA #$00              ; cake tile
  STA $0209
  LDA #$03              ; palette 3 (cake uses palette 3)
  STA $020A
  LDA #$78              ; X position
  STA $020B
  
  ; Hide all other sprites (sprites 3-9)
  LDX #$0C              ; start at sprite 3 (offset 12)
HideLoadingSpritesLoop:
  LDA #$FF              ; Y position off screen
  STA $0200, X          ; set Y position
  INX
  INX
  INX
  INX
  CPX #$28              ; 40 bytes = 10 sprites * 4 bytes
  BNE HideLoadingSpritesLoop
  
  JMP UpdateSpritesDone

ContinueUpdateSprites:
  ; Update falling item sprite (sprite 8) - Left Zone
  LDA itemactive
  BEQ HideItemSprite    ; if item not active, hide sprite
  
  LDA itemy
  STA $0220             ; sprite 8 Y position
  
  ; Set tile based on item type
  LDA itemtype
  BEQ SetGoodItemTileLeft   ; if itemtype = 0, use tile 1 (heart)
  CMP #$01
  BEQ SetBadItemTileLeft    ; if itemtype = 1, use tile 2 (broken heart)
  ; itemtype = 2, cake
  LDA #$00              ; cake uses tile $00
  JMP SetItemTileLeft
SetBadItemTileLeft:
  LDA #$02              ; bad item uses tile 2 (broken heart)
  JMP SetItemTileLeft
SetGoodItemTileLeft:
  LDA #$01              ; good item uses tile 1 (heart)
SetItemTileLeft:
  STA $0221
  
  ; Set palette based on item type (cake uses palette 3, others use palette 2)
  LDA itemtype
  CMP #$02              ; is it a cake?
  BEQ SetCakePaletteLeft
  LDA #$02              ; hearts use palette 2
  JMP StorePaletteLeft
SetCakePaletteLeft:
  LDA #$03              ; cake uses palette 3
StorePaletteLeft:
  STA $0222
  
  LDA itemx
  STA $0223             ; sprite 8 X position
  JMP UpdateRightZoneItemSprite

HideItemSprite:
  LDA #$FF              ; move sprite off screen
  STA $0220
  STA $0221
  STA $0222  
  STA $0223

UpdateRightZoneItemSprite:
  ; Update falling item sprite (sprite 10) - Right Zone
  LDA item2active
  BEQ HideItem2Sprite    ; if item not active, hide sprite
  
  LDA item2y
  STA $0228             ; sprite 10 Y position
  
  ; Set tile based on item type
  LDA item2type
  BEQ SetGoodItemTileRight   ; if itemtype = 0, use tile 1 (heart)
  CMP #$01
  BEQ SetBadItemTileRight    ; if itemtype = 1, use tile 2 (broken heart)
  ; itemtype = 2, cake
  LDA #$00              ; cake uses tile $00
  JMP SetItemTileRight
SetBadItemTileRight:
  LDA #$02              ; bad item uses tile 2 (broken heart)
  JMP SetItemTileRight
SetGoodItemTileRight:
  LDA #$01              ; good item uses tile 1 (heart)
SetItemTileRight:
  STA $0229
  
  ; Set palette based on item type (cake uses palette 3, others use palette 2)
  LDA item2type
  CMP #$02              ; is it a cake?
  BEQ SetCakePaletteRight
  LDA #$02              ; hearts use palette 2
  JMP StorePaletteRight
SetCakePaletteRight:
  LDA #$03              ; cake uses palette 3
StorePaletteRight:
  STA $022A
  
  LDA item2x
  STA $022B             ; sprite 10 X position
  JMP UpdateCakeSprite

HideItem2Sprite:
  LDA #$FF              ; move sprite off screen
  STA $0228
  STA $0229
  STA $022A  
  STA $022B

UpdateCakeSprite:
  ; Update cake sprite (sprite 9)
  LDA cakeactive
  BEQ HideCakeSprite    ; if cake not active, hide sprite
  
  LDA #GROUND_Y         ; cake Y position at floor level
  STA $0224             ; sprite 9 Y position
  
  LDA #$00              ; cake tile
  STA $0225             ; sprite 9 tile
  
  LDA #$03              ; attributes (palette 3)
  STA $0226             ; sprite 9 attributes
  
  LDA cakex
  STA $0227             ; sprite 9 X position
  JMP UpdateSpritesDone

HideCakeSprite:
  LDA #$FF              ; move sprite off screen
  STA $0224
  STA $0225
  STA $0226  
  STA $0227
  JMP UpdateSpritesDone

DrawMenuCursor:
  ; Draw menu cursor sprite (sprite 0) using tile $04
  LDA menu_cursor_y
  STA $0200             ; sprite 0 Y position
  
  LDA #$04              ; tile $04 for cursor
  STA $0201             ; sprite 0 tile
  
  LDA #$02              ; attributes (palette 2, no flip)
  STA $0202             ; sprite 0 attributes
  
  LDA menu_cursor_x
  STA $0203             ; sprite 0 X position
  
  ; Hide all other sprites (sprites 1-9)
  LDX #$04              ; start at sprite 1 (offset 4)
HideTitleSpritesLoop:
  LDA #$FF              ; Y position off screen
  STA $0200, X          ; set Y position
  INX
  INX
  INX
  INX                   ; move to next sprite (4 bytes per sprite)
  CPX #$28              ; check if we've done sprites 1-9 = 36 bytes (4 to 40)
  BNE HideTitleSpritesLoop
  RTS

HideAllSprites:
  ; Hide all sprites by moving them off screen
  LDX #$00              ; start with sprite 0
HideSpritesLoop:
  LDA #$FF              ; Y position off screen
  STA $0200, X          ; set Y position
  INX
  INX
  INX
  INX                   ; move to next sprite (4 bytes per sprite)
  CPX #$28              ; check if we've done 10 sprites (0-9) = 40 bytes
  BNE HideSpritesLoop
  RTS

UpdateSpritesDone:
  RTS
 
 
DrawScore:
  ; Check if we're on title screen - if so, skip drawing score
  LDA gamestate
  CMP #STATETITLE
  BNE CheckLoadingScore
  JMP SkipDrawScore
  
CheckLoadingScore:
  ; Check if we're on loading screen - if so, skip drawing score
  CMP #STATELOADING
  BNE CheckSecretosScore
  JMP SkipDrawScore
  
CheckSecretosScore:
  ; Check if we're on secretos screen - if so, skip drawing score
  CMP #STATESECRETOS
  BNE CheckSecretMsgScore
  JMP SkipDrawScore
  
CheckSecretMsgScore:
  ; Check if we're on secret message screen - if so, skip drawing score
  CMP #STATESECRETMSG
  BNE CheckSecretImgScore
  JMP SkipDrawScore

CheckSecretImgScore:
  ; Check if we're on secret image screen - if so, skip drawing score
  CMP #STATESECRETIMG
  BNE CheckWinScore
  JMP SkipDrawScore
  
CheckWinScore:
  ; Check if we're in win screen state
  CMP #STATEWINSCREEN
  BNE DrawScoreNormal
  JMP DrawWinMessage

DrawScoreNormal:
  
  ; Draw Player 1 score at PPU $2020 (top left)
  LDA $2002
  LDA #$20
  STA $2006
  LDA #$20
  STA $2006          ; start drawing Player 1 score at PPU $2020
  
  LDA scoreHundreds  ; get first digit
  STA $2007          ; draw to background
  LDA scoreTens      ; next digit
  STA $2007
  LDA scoreOnes      ; last digit
  STA $2007
  
  ; Draw Player 2 score at PPU $203C (top right)
  LDA $2002
  LDA #$20
  STA $2006
  LDA #$3C
  STA $2006          ; start drawing Player 2 score at PPU $203C
  
  LDA score2Hundreds ; get Player 2 first digit
  STA $2007          ; draw to background
  LDA score2Tens     ; next digit
  STA $2007
  LDA score2Ones     ; last digit
  STA $2007
  
  ; Set attribute for score areas to use palette 3 (white)
  ; Top-left corner (covers Player 1 score area) is at $23C0
  LDA $2002
  LDA #$23
  STA $2006
  LDA #$C0
  STA $2006
  LDA #%11111111     ; all 4 quadrants use palette 3
  STA $2007
  
  ; Top-right corner (covers Player 2 score area) is at $23C7
  LDA $2002
  LDA #$23
  STA $2006
  LDA #$C7
  STA $2006
  LDA #%11111111     ; all 4 quadrants use palette 3
  STA $2007
  
SkipDrawScore:
  RTS

DrawWinMessage:
  ; Draw centered winner message at row 2, column 8 (centered)
  LDA $2002
  LDA #$20
  STA $2006
  LDA #$48              ; row 2, column 8 = $2048
  STA $2006
  
  ; Check which player won
  LDA winner
  CMP #$01
  BEQ DrawPlayer1Wins
  
  ; Player 2 wins message "Ganador Player 2"
  LDA #$10              ; 'G' (tile $10)
  STA $2007
  LDA #$0A              ; 'a' (tile $0A)
  STA $2007
  LDA #$17              ; 'n' (tile $17)
  STA $2007
  LDA #$0A              ; 'a' (tile $0A)
  STA $2007
  LDA #$0D              ; 'd' (tile $0D)
  STA $2007
  LDA #$18              ; 'o' (tile $18)
  STA $2007
  LDA #$1B              ; 'r' (tile $1B)
  STA $2007
  LDA #$24              ; space (tile $24)
  STA $2007
  LDA #$19              ; 'P' (tile $19)
  STA $2007
  LDA #$15              ; 'l' (tile $15)
  STA $2007
  LDA #$0A              ; 'a' (tile $0A)
  STA $2007
  LDA #$22              ; 'y' (tile $22)
  STA $2007
  LDA #$0E              ; 'e' (tile $0E)
  STA $2007
  LDA #$1B              ; 'r' (tile $1B)
  STA $2007
  LDA #$24              ; space (tile $24)
  STA $2007
  LDA #$02              ; '2' (tile $02)
  STA $2007
  JMP SetWinMessageWhite

DrawPlayer1Wins:
  ; Player 1 wins message "Ganador Player 1"
  LDA #$10              ; 'G' (tile $10)
  STA $2007
  LDA #$0A              ; 'a' (tile $0A)
  STA $2007
  LDA #$17              ; 'n' (tile $17)
  STA $2007
  LDA #$0A              ; 'a' (tile $0A)
  STA $2007
  LDA #$0D              ; 'd' (tile $0D)
  STA $2007
  LDA #$18              ; 'o' (tile $18)
  STA $2007
  LDA #$1B              ; 'r' (tile $1B)
  STA $2007
  LDA #$24              ; space (tile $24)
  STA $2007
  LDA #$19              ; 'P' (tile $19)
  STA $2007
  LDA #$15              ; 'l' (tile $15)
  STA $2007
  LDA #$0A              ; 'a' (tile $0A)
  STA $2007
  LDA #$22              ; 'y' (tile $22)
  STA $2007
  LDA #$0E              ; 'e' (tile $0E)
  STA $2007
  LDA #$1B              ; 'r' (tile $1B)
  STA $2007
  LDA #$24              ; space (tile $24)
  STA $2007
  LDA #$01              ; '1' (tile $01)
  STA $2007

SetWinMessageWhite:
  ; Set attribute for win message area to use palette 3 (white)
  ; Row 2 is covered by attribute byte at $23C1 (second byte in attribute table)
  LDA $2002
  LDA #$23
  STA $2006
  LDA #$C1              ; attribute byte for row 2, columns 8-15
  STA $2006
  LDA #%11111111        ; all quadrants use palette 3
  STA $2007
  
  LDA $2002
  LDA #$23
  STA $2006
  LDA #$C2              ; attribute byte for row 2, columns 16-23
  STA $2006
  LDA #%11111111        ; all quadrants use palette 3
  STA $2007
  
DrawRestartMessage:
  ; Just return - no "PRESS START" message needed
  RTS

ClearWinMessage:
  ; Clear the win message area by filling with spaces (centered position)
  LDA $2002
  LDA #$20
  STA $2006
  LDA #$48              ; row 2, column 8 = $2048
  STA $2006
  
  ; Clear 16 tiles for "Ganador Player X" message
  LDX #$10
ClearWinLoop:
  LDA #$24              ; space tile
  STA $2007
  DEX
  BNE ClearWinLoop
  
  RTS
 
 
IncrementScore:
IncOnes:
  LDA scoreOnes      ; load the lowest digit of the number
  CLC 
  ADC #$01           ; add one
  STA scoreOnes
  CMP #$0A           ; check if it overflowed, now equals 10
  BNE IncDone        ; if there was no overflow, all done
IncTens:
  LDA #$00
  STA scoreOnes      ; wrap digit to 0
  LDA scoreTens      ; load the next digit
  CLC 
  ADC #$01           ; add one, the carry from previous digit
  STA scoreTens
  CMP #$0A           ; check if it overflowed, now equals 10
  BNE IncDone        ; if there was no overflow, all done
IncHundreds:
  LDA #$00
  STA scoreTens      ; wrap digit to 0
  LDA scoreHundreds  ; load the next digit
  CLC 
  ADC #$01           ; add one, the carry from previous digit
  STA scoreHundreds
IncDone:
  RTS

DecrementScore:
DecOnes:
  LDA scoreOnes      ; load the lowest digit of the number
  SEC 
  SBC #$01           ; subtract one
  BPL DecOnesOK      ; if result >= 0, we're done with ones
  LDA #$09           ; wrap to 9
  STA scoreOnes
  JMP DecTens        ; need to borrow from tens
DecOnesOK:
  STA scoreOnes
  JMP DecDone        ; done, no borrowing needed

DecTens:
  LDA scoreTens      ; load the tens digit
  SEC 
  SBC #$01           ; subtract one (borrow)
  BPL DecTensOK      ; if result >= 0, we're done
  LDA #$09           ; wrap to 9
  STA scoreTens
  JMP DecHundreds    ; need to borrow from hundreds
DecTensOK:
  STA scoreTens
  JMP DecDone

DecHundreds:
  LDA scoreHundreds  ; load the hundreds digit
  SEC 
  SBC #$01           ; subtract one (borrow)
  BPL DecHundredsOK  ; if result >= 0, we're done
  ; Player 1 score went negative - Player 2 wins!
  LDA #$02           ; Player 2 wins
  STA winner
  LDA #STATEWINSCREEN
  STA gamestate
  LDA #$00           ; clamp score to 000
  STA scoreHundreds
  STA scoreTens
  STA scoreOnes
  JMP DecDone
DecHundredsOK:
  STA scoreHundreds
DecDone:
  RTS

IncrementScore2:
IncOnes2:
  LDA score2Ones      ; load the lowest digit of Player 2's number
  CLC 
  ADC #$01           ; add one
  STA score2Ones
  CMP #$0A           ; check if it overflowed, now equals 10
  BNE IncDone2        ; if there was no overflow, all done
IncTens2:
  LDA #$00
  STA score2Ones      ; wrap digit to 0
  LDA score2Tens      ; load the next digit
  CLC 
  ADC #$01           ; add one, the carry from previous digit
  STA score2Tens
  CMP #$0A           ; check if it overflowed, now equals 10
  BNE IncDone2        ; if there was no overflow, all done
IncHundreds2:
  LDA #$00
  STA score2Tens      ; wrap digit to 0
  LDA score2Hundreds  ; load the next digit
  CLC 
  ADC #$01           ; add one, the carry from previous digit
  STA score2Hundreds
IncDone2:
  RTS

DecrementScore2:
DecOnes2:
  LDA score2Ones      ; load the lowest digit of Player 2's number
  SEC 
  SBC #$01           ; subtract one
  BPL DecOnesOK2      ; if result >= 0, we're done with ones
  LDA #$09           ; wrap to 9
  STA score2Ones
  JMP DecTens2        ; need to borrow from tens
DecOnesOK2:
  STA score2Ones
  JMP DecDone2        ; done, no borrowing needed

DecTens2:
  LDA score2Tens      ; load the tens digit
  SEC 
  SBC #$01           ; subtract one (borrow)
  BPL DecTensOK2      ; if result >= 0, we're done
  LDA #$09           ; wrap to 9
  STA score2Tens
  JMP DecHundreds2    ; need to borrow from hundreds
DecTensOK2:
  STA score2Tens
  JMP DecDone2

DecHundreds2:
  LDA score2Hundreds  ; load the hundreds digit
  SEC 
  SBC #$01           ; subtract one (borrow)
  BPL DecHundredsOK2  ; if result >= 0, we're done
  ; Player 2 score went negative - Player 1 wins!
  LDA #$01           ; Player 1 wins
  STA winner
  LDA #STATEWINSCREEN
  STA gamestate
  LDA #$00           ; clamp score to 000
  STA score2Hundreds
  STA score2Tens
  STA score2Ones
  JMP DecDone2
DecHundredsOK2:
  STA score2Hundreds
DecDone2:
  RTS

UpdateProgressiveSpeed:
  ; Calculate total score (Player 1 + Player 2)
  ; For simplicity, we'll use just the ones digits
  LDA scoreOnes
  CLC
  ADC score2Ones
  STA total_score
  
  ; Determine speed level based on total score
  CMP #$05              ; 5 points
  BCC SpeedLevel0       ; if < 5, stay at level 0
  CMP #$0A              ; 10 points  
  BCC SpeedLevel1       ; if < 10, go to level 1
  CMP #$14              ; 20 points (hex $14 = decimal 20)
  BCC SpeedLevel2       ; if < 20, go to level 2
  ; else level 3
  LDA #$03
  JMP SetSpeedLevel
SpeedLevel2:
  LDA #$02
  JMP SetSpeedLevel
SpeedLevel1:
  LDA #$01
  JMP SetSpeedLevel
SpeedLevel0:
  LDA #$00
SetSpeedLevel:
  STA speed_level
  RTS

GetCurrentItemSpeed:
  ; Return current item speed in A register based on speed_level
  LDA speed_level
  CMP #$00
  BEQ Speed0
  CMP #$01  
  BEQ Speed1
  CMP #$02
  BEQ Speed2
  ; Speed level 3
  LDA #$05              ; 5 pixels/frame (fastest)
  RTS
Speed2:
  LDA #$04              ; 4 pixels/frame
  RTS
Speed1:
  LDA #$03              ; 3 pixels/frame  
  RTS
Speed0:
  LDA #$02              ; 2 pixels/frame (normal)
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

Bankswitch:
  ; For CNROM (mapper 3), write to ROM address containing bank number
  ; The value in A is the bank number to switch to
  TAX               ; copy A into X
  STA Bankvalues, X ; write to ROM location with bank value
  RTS

Bankvalues:
  .db $00, $01, $02, $03  ; bank numbers

LoadTitleScreen:
  ; Load title screen nametable from pilartitle.nam
  LDA $2002             ; read PPU status to reset the high/low latch
  LDA #$20
  STA $2006             ; write the high byte of $2000 address
  LDA #$00
  STA $2006             ; write the low byte of $2000 address

  LDA #LOW(titleScreen)
  STA pointerLo         ; put the low byte of the address into pointer
  LDA #HIGH(titleScreen)
  STA pointerHi         ; put the high byte of the address into pointer

  LDX #$00              ; start at pointer + 0
  LDY #$00
TitleOutsideLoop:
TitleInsideLoop:
  LDA [pointerLo], y    ; copy one background byte from address in pointer plus Y
  STA $2007             ; this runs 256 * 4 times

  INY                   ; inside loop counter
  CPY #$00
  BNE TitleInsideLoop   ; run the inside loop 256 times before continuing down

  INC pointerHi         ; low byte went 0 to 256, so high byte needs to be changed now

  INX
  CPX #$04
  BNE TitleOutsideLoop  ; run the outside loop 4 times before continuing down
  RTS

LoadSecretosScreen:
  ; Fill entire screen with spaces (tile $24)
  LDA $2002             ; read PPU status to reset the high/low latch
  LDA #$20
  STA $2006             ; write the high byte of $2000 address
  LDA #$00
  STA $2006             ; write the low byte of $2000 address

  LDX #$00              ; outer loop counter (4 pages)
  LDY #$00              ; inner loop counter (256 bytes per page)
SecretosOutsideLoop:
SecretosInsideLoop:
  LDA #$24              ; tile $24 (space/blank)
  STA $2007             ; write to PPU
  INY                   ; increment inner counter
  CPY #$00
  BNE SecretosInsideLoop ; run 256 times
  
  INX                   ; increment outer counter
  CPX #$04
  BNE SecretosOutsideLoop ; run 4 times (4 * 256 = 1024 tiles)
  
  ; Set all attributes to palette 0
  ; Attribute table starts at $23C0 and is 64 bytes (8x8 grid)
  LDA $2002             ; reset PPU latch
  LDA #$23
  STA $2006             ; high byte of $23C0
  LDA #$C0
  STA $2006             ; low byte of $23C0
  
  LDX #$00              ; counter for 64 bytes
SecretosAttributeLoop:
  LDA #%00000000        ; all tiles use palette 0 (bits: 00 00 00 00)
  STA $2007
  INX
  CPX #$40              ; 64 bytes ($40 in hex)
  BNE SecretosAttributeLoop
  
  RTS

LoadLoadingScreen:
  ; Fill entire screen with spaces (tile $24)
  ; The background will be black due to palette 0
  LDA $2002             ; read PPU status to reset the high/low latch
  LDA #$20
  STA $2006             ; write the high byte of $2000 address
  LDA #$00
  STA $2006             ; write the low byte of $2000 address

  LDX #$00              ; outer loop counter (4 pages)
  LDY #$00              ; inner loop counter (256 bytes per page)
LoadingOutsideLoop:
LoadingInsideLoop:
  LDA #$24              ; tile $24 (space - will show background color)
  STA $2007             ; write to PPU
  INY                   ; increment inner counter
  CPY #$00
  BNE LoadingInsideLoop ; run 256 times
  
  INX                   ; increment outer counter
  CPX #$04
  BNE LoadingOutsideLoop ; run 4 times (4 * 256 = 1024 tiles)
  
  ; Set all attributes to palette 0 (black background from title palette)
  LDA $2002             ; reset PPU latch
  LDA #$23
  STA $2006             ; high byte of $23C0
  LDA #$C0
  STA $2006             ; low byte of $23C0
  
  LDX #$00              ; counter for 64 bytes
LoadingAttributeLoop:
  LDA #%00000000        ; all tiles use palette 0
  STA $2007
  INX
  CPX #$40              ; 64 bytes ($40 in hex)
  BNE LoadingAttributeLoop
  
  ; Now draw the text messages (items will be sprites)
  ; "Suma 1 punto" at row 10, column 10 = $214A
  LDA $2002
  LDA #$21
  STA $2006
  LDA #$4A
  STA $2006
  ; S=1C, u=1E, m=16, a=0A, space=24, 1=01, space=24, p=19, u=1E, n=17, t=1D, o=18
  LDA #$1C              ; S
  STA $2007
  LDA #$1E              ; u
  STA $2007
  LDA #$16              ; m
  STA $2007
  LDA #$0A              ; a
  STA $2007
  LDA #$24              ; space
  STA $2007
  LDA #$01              ; 1
  STA $2007
  LDA #$24              ; space
  STA $2007
  LDA #$19              ; p
  STA $2007
  LDA #$1E              ; u
  STA $2007
  LDA #$17              ; n
  STA $2007
  LDA #$1D              ; t
  STA $2007
  LDA #$18              ; o
  STA $2007
  
  ; Draw "Resta 1 punto" at row 16, column 9 = $2209
  LDA $2002
  LDA #$22
  STA $2006
  LDA #$09
  STA $2006
  ; R=1B, e=0E, s=1C, t=1D, a=0A, space=24, 1=01, space=24, p=19, u=1E, n=17, t=1D, o=18
  LDA #$1B              ; R
  STA $2007
  LDA #$0E              ; e
  STA $2007
  LDA #$1C              ; s
  STA $2007
  LDA #$1D              ; t
  STA $2007
  LDA #$0A              ; a
  STA $2007
  LDA #$24              ; space
  STA $2007
  LDA #$01              ; 1
  STA $2007
  LDA #$24              ; space
  STA $2007
  LDA #$19              ; p
  STA $2007
  LDA #$1E              ; u
  STA $2007
  LDA #$17              ; n
  STA $2007
  LDA #$1D              ; t
  STA $2007
  LDA #$18              ; o
  STA $2007
  
  ; Draw "Roba un punto" at row 22, column 9 = $22C9
  LDA $2002
  LDA #$22
  STA $2006
  LDA #$C9
  STA $2006
  ; R=1B, o=18, b=0B, a=0A, space=24, u=1E, n=17, space=24, p=19, u=1E, n=17, t=1D, o=18
  LDA #$1B              ; R
  STA $2007
  LDA #$18              ; o
  STA $2007
  LDA #$0B              ; b
  STA $2007
  LDA #$0A              ; a
  STA $2007
  LDA #$24              ; space
  STA $2007
  LDA #$1E              ; u
  STA $2007
  LDA #$17              ; n
  STA $2007
  LDA #$24              ; space
  STA $2007
  LDA #$19              ; p
  STA $2007
  LDA #$1E              ; u
  STA $2007
  LDA #$17              ; n
  STA $2007
  LDA #$1D              ; t
  STA $2007
  LDA #$18              ; o
  STA $2007
  
  RTS

DrawSecretosUI:
  ; Draw the 4-digit number selector and messages
  ; Position: center of screen, row 14
  ; PPU address = $2000 + (14 * 32) + 12 = $2000 + $1C0 + $0C = $21CC
  
  LDA $2002             ; reset PPU latch
  LDA #$21
  STA $2006             ; high byte
  LDA #$CC
  STA $2006             ; low byte ($21CC = row 14, col 12)
  
  ; Draw first digit
  LDA secret_digit1
  STA $2007
  
  ; Space
  LDA #$24
  STA $2007
  
  ; Draw second digit
  LDA secret_digit2
  STA $2007
  
  ; Space
  LDA #$24
  STA $2007
  
  ; Draw third digit
  LDA secret_digit3
  STA $2007
  
  ; Space
  LDA #$24
  STA $2007
  
  ; Draw fourth digit
  LDA secret_digit4
  STA $2007
  
  ; Draw cursor indicator (underscore) below selected digit
  ; Row 15, starting at column 12
  LDA $2002
  LDA #$21
  STA $2006
  LDA #$EC              ; $21EC = row 15, col 12
  STA $2006
  
  ; Draw underscores based on cursor position
  LDX #$00
DrawCursorLoop:
  CPX secret_cursor
  BEQ DrawCursorHere
  LDA #$24              ; space
  JMP DrawCursorTile
DrawCursorHere:
  LDA #$28              ; dash/underscore (tile $28 = "-")
DrawCursorTile:
  STA $2007
  LDA #$24              ; space after each position
  STA $2007
  INX
  CPX #$04
  BNE DrawCursorLoop
  
  ; Draw message if any (row 17)
  LDA secret_message
  BEQ DrawSecretosUIDone  ; no message
  
  LDA $2002
  LDA #$22
  STA $2006
  LDA #$2C              ; $222C = row 17, col 12
  STA $2006
  
  LDA secret_message
  CMP #$01
  BEQ DrawErrorMessage
  CMP #$02
  BEQ DrawSuccessMessage
  JMP DrawSecretosUIDone

DrawErrorMessage:
  ; "ERROR"
  LDA #$0E              ; E
  STA $2007
  LDA #$1B              ; R
  STA $2007
  LDA #$1B              ; R
  STA $2007
  LDA #$18              ; O
  STA $2007
  LDA #$1B              ; R
  STA $2007
  JMP DrawSecretosUIDone

DrawSuccessMessage:
  ; "SECRETO SI"
  LDA #$1C              ; S
  STA $2007
  LDA #$0E              ; E
  STA $2007
  LDA #$0C              ; C
  STA $2007
  LDA #$1B              ; R
  STA $2007
  LDA #$0E              ; E
  STA $2007
  LDA #$1D              ; T
  STA $2007
  LDA #$18              ; O
  STA $2007
  LDA #$24              ; space
  STA $2007
  LDA #$1C              ; S
  STA $2007
  LDA #$12              ; I
  STA $2007

DrawSecretosUIDone:
  RTS

IncrementSecretDigit:
  ; Increment the current digit (0-9 wrap around)
  LDX secret_cursor
  CPX #$00
  BEQ IncDigit1
  CPX #$01
  BEQ IncDigit2
  CPX #$02
  BEQ IncDigit3
  ; else digit 4
  LDA secret_digit4
  CMP #$09
  BEQ WrapDigit4To0
  INC secret_digit4
  RTS
WrapDigit4To0:
  LDA #$00
  STA secret_digit4
  RTS

IncDigit1:
  LDA secret_digit1
  CMP #$09
  BEQ WrapDigit1To0
  INC secret_digit1
  RTS
WrapDigit1To0:
  LDA #$00
  STA secret_digit1
  RTS

IncDigit2:
  LDA secret_digit2
  CMP #$09
  BEQ WrapDigit2To0
  INC secret_digit2
  RTS
WrapDigit2To0:
  LDA #$00
  STA secret_digit2
  RTS

IncDigit3:
  LDA secret_digit3
  CMP #$09
  BEQ WrapDigit3To0
  INC secret_digit3
  RTS
WrapDigit3To0:
  LDA #$00
  STA secret_digit3
  RTS

DecrementSecretDigit:
  ; Decrement the current digit (0-9 wrap around)
  LDX secret_cursor
  CPX #$00
  BEQ DecDigit1
  CPX #$01
  BEQ DecDigit2
  CPX #$02
  BEQ DecDigit3
  ; else digit 4
  LDA secret_digit4
  BEQ WrapDigit4To9
  DEC secret_digit4
  RTS
WrapDigit4To9:
  LDA #$09
  STA secret_digit4
  RTS

DecDigit1:
  LDA secret_digit1
  BEQ WrapDigit1To9
  DEC secret_digit1
  RTS
WrapDigit1To9:
  LDA #$09
  STA secret_digit1
  RTS

DecDigit2:
  LDA secret_digit2
  BEQ WrapDigit2To9
  DEC secret_digit2
  RTS
WrapDigit2To9:
  LDA #$09
  STA secret_digit2
  RTS

DecDigit3:
  LDA secret_digit3
  BEQ WrapDigit3To9
  DEC secret_digit3
  RTS
WrapDigit3To9:
  LDA #$09
  STA secret_digit3
  RTS

CheckSecretCode:
  ; Check if the entered code matches any secrets
  ; Check for code 1234 (secret message 0)
  LDA secret_digit1
  CMP #$01
  BNE CheckCode2
  LDA secret_digit2
  CMP #$02
  BNE CheckCode2
  LDA secret_digit3
  CMP #$03
  BNE CheckCode2
  LDA secret_digit4
  CMP #$04
  BNE CheckCode2
  
  ; Success! Code 1234 found
  LDA #$00              ; message index 0
  STA secret_msg_index
  JMP ShowSecretMessage

CheckCode2:
  ; Check for code 1998 (secret message 1)
  LDA secret_digit1
  CMP #$01
  BNE CheckCode3
  LDA secret_digit2
  CMP #$09
  BNE CheckCode3
  LDA secret_digit3
  CMP #$09
  BNE CheckCode3
  LDA secret_digit4
  CMP #$08
  BNE CheckCode3
  
  ; Success! Code 1998 found
  LDA #$01              ; message index 1
  STA secret_msg_index
  JMP ShowSecretMessage

CheckCode3:
  ; Check for code 1111 (secret image)
  LDA secret_digit1
  CMP #$01
  BNE CheckCodeError
  LDA secret_digit2
  CMP #$01
  BNE CheckCodeError
  LDA secret_digit3
  CMP #$01
  BNE CheckCodeError
  LDA secret_digit4
  CMP #$01
  BNE CheckCodeError
  
  ; Success! Code 1111 found - show image
  JMP ShowSecretImage

CheckCodeError:
  ; No match found, show error
  LDA #$01
  STA secret_message
  RTS

ShowSecretMessage:
  ; Transition to secret message display state
  ; Turn screen off
  LDA #%00000000
  STA $2001
  STA $2000
  
  ; Wait for vblank
  LDA $2002
WaitVBlankForMessage:
  BIT $2002
  BPL WaitVBlankForMessage
  
  ; Clear screen completely (fill with spaces)
  JSR LoadSecretosScreen
  
  ; Clear the area where the old secretos UI was (rows 14-17)
  ; This ensures no leftover digits or messages
  LDA $2002
  LDA #$21
  STA $2006
  LDA #$C0              ; Start of row 14
  STA $2006
  
  LDX #$00
ClearOldUILoop:
  LDA #$24              ; Space tile
  STA $2007
  INX
  CPX #$80              ; Clear 128 tiles (4 rows * 32 columns)
  BNE ClearOldUILoop
  
  ; Initialize message display
  LDA #$00
  STA secret_msg_char_index_lo  ; start at character 0 in message data (low byte)
  STA secret_msg_char_index_hi  ; start at character 0 in message data (high byte)
  STA secret_msg_screen_pos_lo  ; start at screen position 0 (low byte)
  STA secret_msg_screen_pos_hi  ; start at screen position 0 (high byte)
  STA secret_msg_draw_flag   ; no message draw flag
  STA secret_draw_flag       ; clear secretos UI draw flag (important!)
  STA secret_message         ; clear any error messages
  LDA #$03                   ; 3 frames per character
  STA secret_msg_timer
  
  ; Set state to secret message
  LDA #STATESECRETMSG
  STA gamestate
  
  ; Turn screen back on
  LDA #%10010000
  STA $2000
  LDA #%00011110
  STA $2001
  RTS

ShowSecretImage:
  ; Display the secret image (code 1111)
  ; Turn screen off
  LDA #%00000000
  STA $2001
  STA $2000
  
  ; Wait for vblank
  LDA $2002
WaitVBlankForImage:
  BIT $2002
  BPL WaitVBlankForImage
  
  ; Switch to CHR bank 2 (pilarart2.chr)
  LDA #$02
  JSR Bankswitch        ; use the bankswitch subroutine
  
  ; Load custom palette for secret image
  JSR LoadSecretImagePalette
  
  ; Load the image nametable + attributes (1024 bytes)
  LDA #LOW(SecretImageData)
  STA pointerLo
  LDA #HIGH(SecretImageData)
  STA pointerHi
  
  ; Set PPU address to $2000 (start of nametable)
  LDA $2002
  LDA #$20
  STA $2006
  LDA #$00
  STA $2006
  
  ; Load 1024 bytes (nametable + attributes)
  LDX #$00              ; outer loop (4 pages)
  LDY #$00              ; inner loop (256 bytes per page)
LoadImageLoop:
  LDA [pointerLo], y
  STA $2007
  INY
  BNE LoadImageLoop
  INC pointerHi
  INX
  CPX #$04              ; 4 * 256 = 1024 bytes
  BNE LoadImageLoop
  
  ; Set state to secret image
  LDA #STATESECRETIMG
  STA gamestate
  
  ; Turn screen back on
  ; Use pattern table 0 ($0000-$0FFF) for background
  LDA #%10000000        ; NMI on, pattern table 0 for background
  STA $2000
  LDA #%00011110
  STA $2001
  RTS

WriteNextCharacter:
  ; Write the next character of the secret message
  ; Check which message to display
  LDA secret_msg_index
  CMP #$00
  BEQ WriteMessage0
  CMP #$01
  BEQ WriteMessage1
  JMP WriteNextCharDone  ; unknown message
  
WriteMessage0:
  ; Get character from message 0 (uses 8-bit index, message is short)
  LDX secret_msg_char_index_lo
  LDA SecretMessage0, X
  CMP #$FF              ; $FF = end of message marker
  BEQ JumpToWriteNextCharDone
  
  ; Calculate PPU address: $2000 + char_index
  ; Start at row 10, column 2 = $2000 + (10 * 32) + 2 = $2000 + $142 = $2142
  LDA $2002             ; reset PPU latch
  LDA #$21
  STA $2006
  
  ; Calculate low byte: $42 + char_index
  LDA #$42
  CLC
  ADC secret_msg_char_index_lo
  STA $2006
  
  ; Write the character
  LDX secret_msg_char_index_lo
  LDA SecretMessage0, X
  STA $2007
  
  ; Increment 16-bit character index
  INC secret_msg_char_index_lo
  BNE WriteMsg0Done
  INC secret_msg_char_index_hi
WriteMsg0Done:
  JMP WriteNextCharDone

JumpToWriteNextCharDone:
  JMP WriteNextCharDone

WriteMessage1:
  ; Get character from message 1 (uses 16-bit index for long messages)
  ; Use Y-indexed addressing with base pointer
  
  ; Check if we need to adjust the base pointer
  ; We'll use Y register for offset (0-255) and adjust base when needed
  LDY secret_msg_char_index_lo
  
  ; Check if char_index_hi is 0 (first 256 chars)
  LDA secret_msg_char_index_hi
  BEQ WriteMsg1Page0
  
  ; For pages 1+ (256+ chars), read from SecretMessage1 + 256*hi + lo
  ; This is tricky - we need to use absolute indexed addressing
  ; Let's set up a pointer to the right page
  CMP #$01
  BEQ WriteMsg1Page1
  CMP #$02
  BEQ WriteMsg1Page2
  CMP #$03
  BEQ WriteMsg1Page3
  JMP WriteNextCharDone  ; beyond our message
  
WriteMsg1Page0:
  LDA SecretMessage1, Y
  JMP WriteMsg1CheckEnd
  
WriteMsg1Page1:
  LDA SecretMessage1+256, Y
  JMP WriteMsg1CheckEnd
  
WriteMsg1Page2:
  LDA SecretMessage1+512, Y
  JMP WriteMsg1CheckEnd
  
WriteMsg1Page3:
  LDA SecretMessage1+768, Y
  JMP WriteMsg1CheckEnd
  
WriteMsg1CheckEnd:
  CMP #$FF              ; $FF = end of message marker
  BEQ JumpToWriteNextCharDone
  CMP #$FE              ; $FE = line break marker (fill rest of line with spaces)
  BEQ HandleLineBreak
  
  ; Normal character - save temporarily
  PHA
  
  ; Calculate PPU address: $2020 + screen_pos (start at row 1, col 0)
  LDA $2002             ; reset PPU latch
  
  ; Calculate address: $2020 + screen_pos (16-bit addition)
  ; First add $20 to screen_pos_lo
  LDA secret_msg_screen_pos_lo
  CLC
  ADC #$20
  TAX                   ; save low byte in X
  
  ; Then add carry to screen_pos_hi and add $20 to result
  LDA secret_msg_screen_pos_hi
  ADC #$20              ; add $20 + carry from previous addition
  STA $2006             ; write high byte
  
  ; Write low byte
  TXA
  STA $2006
  
  ; Write the character (restore from stack)
  PLA
  STA $2007
  
  ; Increment screen position
  INC secret_msg_screen_pos_lo
  BNE NormalCharDone
  INC secret_msg_screen_pos_hi
  JMP NormalCharDone

HandleLineBreak:
  ; Line break: advance screen position to start of next line
  ; First, increment char_index to skip past the $FE marker
  INC secret_msg_char_index_lo
  BNE LineBreakNoCarry
  INC secret_msg_char_index_hi
LineBreakNoCarry:
  
  ; Calculate how many spaces needed: 32 - (screen_pos_lo % 32)
  LDA secret_msg_screen_pos_lo
  AND #%00011111        ; get position in current line (0-31)
  BEQ LineBreakDone     ; if at start of line (0), already aligned
  
  ; Calculate spaces needed: 32 - position
  STA pointerLo         ; temp store position
  LDA #$20              ; 32
  SEC
  SBC pointerLo         ; 32 - position = spaces needed
  STA pointerLo         ; store count
  
  ; Set up PPU address for filling
  LDA $2002             ; reset PPU latch
  LDA secret_msg_screen_pos_lo
  CLC
  ADC #$20
  TAX
  LDA secret_msg_screen_pos_hi
  ADC #$20
  STA $2006
  TXA
  STA $2006
  
  ; Fill with spaces
  LDY pointerLo         ; Y = number of spaces to write
FillSpacesLoop:
  LDA #$24              ; space tile
  STA $2007
  DEY
  BNE FillSpacesLoop
  
  ; Add spaces count to screen_pos
  LDA secret_msg_screen_pos_lo
  CLC
  ADC pointerLo
  STA secret_msg_screen_pos_lo
  BCC LineBreakDone
  INC secret_msg_screen_pos_hi
  
LineBreakDone:
  JMP WriteNextCharDone

NormalCharDone:
  
  ; Increment 16-bit character index (in message data)
  INC secret_msg_char_index_lo
  BNE CheckIfSpace
  INC secret_msg_char_index_hi
  
CheckIfSpace:
  ; Check if we just wrote a space ($24)
  ; If so, immediately trigger next character write (skip timer delay)
  CMP #$24
  BNE WriteNextCharDone  ; not a space, done normally
  
  ; It was a space - check if next character is also a space
  ; We need to peek at the next character without incrementing yet
  LDY secret_msg_char_index_lo
  LDA secret_msg_char_index_hi
  BEQ CheckSpacePage0
  CMP #$01
  BEQ CheckSpacePage1
  CMP #$02
  BEQ CheckSpacePage2
  CMP #$03
  BEQ CheckSpacePage3
  JMP WriteNextCharDone
  
CheckSpacePage0:
  LDA SecretMessage1, Y
  JMP CheckSpaceResult
CheckSpacePage1:
  LDA SecretMessage1+256, Y
  JMP CheckSpaceResult
CheckSpacePage2:
  LDA SecretMessage1+512, Y
  JMP CheckSpaceResult
CheckSpacePage3:
  LDA SecretMessage1+768, Y
  
CheckSpaceResult:
  CMP #$24              ; is next char also a space?
  BNE WriteNextCharDone ; no, done normally
  CMP #$FF              ; is it end marker?
  BEQ WriteNextCharDone ; yes, stop
  
  ; Next char is also a space - set flag to write it immediately on next NMI
  LDA #$01
  STA secret_msg_draw_flag
  ; Also reset timer to 1 so it triggers immediately next frame
  STA secret_msg_timer
  
WriteNextCharDone:
  RTS

; Secret message data
; Message 0 (for code 1234): "THIS IS A SECRET MESSAGE"
SecretMessage0:
  .db $1D,$11,$12,$1C,$24,$12,$1C,$24,$0A,$24,$1C,$0E,$0C,$1B,$0E,$1D,$24,$16,$0E,$1C,$1C,$0A,$10,$0E,$FF
  ; T   H   I   S   _   I   S   _   A   _   S   E   C   R   E   T   _   M   E   S   S   A   G   E   (end)

SecretMessage1:
  ; Compressed format: actual text only, $FE = line break (fill rest of line with spaces)
  ; Line 1: " 1998, tambien conocido"
  .db $24,$01,$09,$09,$08,$2D,$24,$1D,$0A,$16,$0B,$12,$0E,$17,$24,$0C,$18,$17,$18,$0C,$12,$0D,$18,$FE
  ; Line 2: " como el 1 de la Era"
  .db $24,$0C,$18,$16,$18,$24,$0E,$15,$24,$01,$24,$0D,$0E,$24,$15,$0A,$24,$0E,$1B,$0A,$FE
  ; Line 3: " Pilar."
  .db $24,$19,$12,$15,$0A,$1B,$2F,$FE
  ; Line 4: (empty line)
  .db $FE
  ; Line 5: " Desde las canciones de"
  .db $24,$0D,$0E,$1C,$0D,$0E,$24,$15,$0A,$1C,$24,$0C,$0A,$17,$0C,$12,$18,$17,$0E,$1C,$24,$0D,$0E,$FE
  ; Line 6: " Miguelito hasta hoy,"
  .db $24,$16,$12,$10,$1E,$0E,$15,$12,$1D,$18,$24,$11,$0A,$1C,$1D,$0A,$24,$11,$18,$22,$2D,$FE
  ; Line 7: " pasando por el cielo"
  .db $24,$19,$0A,$1C,$0A,$17,$0D,$18,$24,$19,$18,$1B,$24,$0E,$15,$24,$0C,$12,$0E,$15,$18,$FE
  ; Line 8: " color Barbie, las"
  .db $24,$0C,$18,$15,$18,$1B,$24,$0B,$0A,$1B,$0B,$12,$0E,$2D,$24,$15,$0A,$1C,$FE
  ; Line 9: " repartijas de chocolates,"
  .db $24,$1B,$0E,$19,$0A,$1B,$1D,$12,$13,$0A,$1C,$24,$0D,$0E,$24,$0C,$11,$18,$0C,$18,$15,$0A,$1D,$0E,$1C,$2D,$FE
  ; Line 10: " los VHS sucios de Mi"
  .db $24,$15,$18,$1C,$24,$1F,$11,$1C,$24,$1C,$1E,$0C,$12,$18,$1C,$24,$0D,$0E,$24,$16,$12,$FE
  ; Line 11: " Pequeno Pony y los juegos"
  .db $24,$19,$0E,$1A,$1E,$0E,$17,$18,$24,$19,$18,$17,$22,$24,$22,$24,$15,$18,$1C,$24,$13,$1E,$0E,$10,$18,$1C,$FE
  ; Line 12: " de PC de la tortuga"
  .db $24,$0D,$0E,$24,$19,$0C,$24,$0D,$0E,$24,$15,$0A,$24,$1D,$18,$1B,$1D,$1E,$10,$0A,$FE
  ; Line 13: " Manuelita que siempre"
  .db $24,$16,$0A,$17,$1E,$0E,$15,$12,$1D,$0A,$24,$1A,$1E,$0E,$24,$1C,$12,$0E,$16,$19,$1B,$0E,$FE
  ; Line 14: " terminaban rompiendo"
  .db $24,$1D,$0E,$1B,$16,$12,$17,$0A,$0B,$0A,$17,$24,$1B,$18,$16,$19,$12,$0E,$17,$0D,$18,$FE
  ; Line 15: " todo. Con arte y"
  .db $24,$1D,$18,$0D,$18,$2F,$24,$0C,$18,$17,$24,$0A,$1B,$1D,$0E,$24,$22,$FE
  ; Line 16: " canciones compartimos lo"
  .db $24,$0C,$0A,$17,$0C,$12,$18,$17,$0E,$1C,$24,$0C,$18,$16,$19,$0A,$1B,$1D,$12,$16,$18,$1C,$24,$15,$18,$FE
  ; Line 17: " que creimos la ultima era"
  .db $24,$1A,$1E,$0E,$24,$0C,$1B,$0E,$12,$16,$18,$1C,$24,$15,$0A,$24,$1E,$15,$1D,$12,$16,$0A,$24,$0E,$1B,$0A,$FE
  ; Line 18: " de la familia, pero hoy"
  .db $24,$0D,$0E,$24,$15,$0A,$24,$0F,$0A,$16,$12,$15,$12,$0A,$2D,$24,$19,$0E,$1B,$18,$24,$11,$18,$22,$FE
  ; Line 19: " sabemos que esa historia"
  .db $24,$1C,$0A,$0B,$0E,$16,$18,$1C,$24,$1A,$1E,$0E,$24,$0E,$1C,$0A,$24,$11,$12,$1C,$1D,$18,$1B,$12,$0A,$FE
  ; Line 20: " sigue creciendo,"
  .db $24,$1C,$12,$10,$1E,$0E,$24,$0C,$1B,$0E,$0C,$12,$0E,$17,$0D,$18,$2D,$FE
  ; Line 21: " expandiendose mas que"
  .db $24,$0E,$21,$19,$0A,$17,$0D,$12,$0E,$17,$0D,$18,$1C,$0E,$24,$16,$0A,$1C,$24,$1A,$1E,$0E,$FE
  ; Line 22: " nunca."
  .db $24,$17,$1E,$17,$0C,$0A,$2F,$FE
  ; Line 23: (empty line)
  .db $FE
  ; Line 24: (empty line)
  .db $FE
  ; Line 25: " Te amamos."
  .db $24,$1D,$0E,$24,$0A,$16,$0A,$16,$18,$1C,$2F,$FE
  ; Line 26: " Tu familia."
  .db $24,$1D,$1E,$24,$0F,$0A,$16,$12,$15,$12,$0A,$2F,$FF
  ; (end marker)
  
  
    
        
;;;;;;;;;;;;;;  
  
gameBackground:
  .incbin "pilarbg1.nam"

titleScreen:
  .incbin "pilartitle.nam"
  
  
  .bank 1
  .org $E000

SecretImageData:
  ; Secret image nametable + attributes (1024 bytes)
  .incbin "pilarart2.nam"

titlepalette:
  .db $0F,$20,$10,$0F,  $0F,$21,$20,$31,  $0F,$15,$20,$26,  $0F,$00,$10,$30   ;;title background palette
  .db $0F,$20,$10,$0F,  $0F,$21,$20,$31,  $0F,$15,$20,$26,  $0F,$00,$10,$30   ;;title sprite palette (same as bg)

palette:
  .db $22,$29,$1A,$0F,  $22,$36,$17,$0F,  $22,$30,$21,$0F,  $22,$30,$30,$0F   ;;background palette
  .db $21,$30,$36,$16,  $21,$0D,$36,$30,  $21,$0F,$20,$15,  $21,$30,$36,$15   ;;sprite palette

secretimagepalette:
  .db $05,$37,$26,$03,  $05,$27,$26,$03,  $05,$27,$26,$03,  $05,$27,$26,$03   ;;secret image background palette
  .db $05,$37,$26,$03,  $05,$27,$26,$03,  $05,$27,$26,$03,  $05,$27,$26,$03   ;;secret image sprite palette (same)

sprites:
     ;vert tile attr horiz
  .db $80, $32, $00, $80   ;sprite 0
  .db $80, $33, $00, $88   ;sprite 1
  .db $88, $34, $00, $80   ;sprite 2
  .db $88, $35, $00, $88   ;sprite 3



  .org $FFFA     ;first of the three vectors starts here
  .dw NMI        ;when an NMI happens (once per frame if enabled) the 
                   ;processor will jump to the label NMI:
  .dw RESET      ;when the processor first turns on or is reset, it will jump
                   ;to the label RESET:
  .dw 0          ;external interrupt IRQ is not used in this tutorial
  
  
;;;;;;;;;;;;;;  
  
  
  .bank 2
  .org $0000
  .incbin "pilartitle.chr"   ; title screen graphics (8KB CHR bank 0)

  .bank 3
  .org $0000
  .incbin "pilar.chr"        ; gameplay graphics (8KB CHR bank 1)

  .bank 4
  .org $0000
  .incbin "pilarart2.chr"    ; secret image graphics (8KB CHR bank 2)