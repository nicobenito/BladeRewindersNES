  .inesprg 1   ; 1x 16KB PRG code
  .ineschr 1   ; 1x  8KB CHR data
  .inesmap 0   ; mapper 0 = NROM, no bank swapping
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

; Physics constants
GRAVITY        = $01  ; gravity acceleration per frame (slow for debugging)
JUMP_VELOCITY  = $F8  ; initial jump velocity (negative = upward)
JUMP_SPEED     = $05  ; pixels per frame when jumping up
JUMP_DURATION  = $15  ; frames to jump upward (48 frames)
FALL_SPEED     = $02  ; pixels per frame when falling down
GROUND_Y       = $D0  ; Y position of ground (higher level)

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

  JSR LoadPalettes
  JSR LoadBackground
  JMP InitializeGame

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

  LDX #$00            ; start at pointer + 0
  LDY #$00
InitialOutsideLoop:
InitialInsideLoop:
  LDA #$24            ; load tile $24 for background
  STA $2007           ; this runs 256 * 4 times
  INY                 ; inside loop counter
  CPY #$00
  BNE InitialInsideLoop      ; run the inside loop 256 times before continuing down
  INX
  CPX #$04
  BNE InitialOutsideLoop     ; run the outside loop 4 times before continuing down

  ; Draw floor - 2 lines at bottom of screen
  ; First draw the top line of floor (tile $48) at row 27
  LDA $2002             ; read PPU status to reset the high/low latch
  LDA #$23
  STA $2006             ; write the high byte 
  LDA #$60              ; write to $2360 (row 27, column 0)
  STA $2006
  
  LDX #$00              ; counter for 32 tiles across
DrawFloorTopLine:
  LDA #$48              ; top floor tile
  STA $2007
  INX
  CPX #$20              ; 32 tiles across (full width)
  BNE DrawFloorTopLine
  
  ; Now draw the bottom line of floor (tile $78) at row 28
  LDA #$78              ; bottom floor tile
  LDX #$00              ; counter for 32 tiles across
DrawFloorBottomLine:
  STA $2007             ; write tile $78
  INX
  CPX #$20              ; 32 tiles across (full width)
  BNE DrawFloorBottomLine

  ; Draw center line (tile $26) - simplified approach
  ; Draw a few tiles in the middle rows for visibility
  LDA $2002             ; read PPU status to reset the high/low latch
  LDA #$21              ; row 8
  STA $2006
  LDA #$10              ; column 16 (middle)
  STA $2006
  LDA #$26              ; center line tile
  STA $2007
  
  LDA $2002             ; read PPU status to reset the high/low latch  
  LDA #$21              ; row 12
  STA $2006
  LDA #$90              ; column 16 + (4*32)
  STA $2006
  LDA #$26              ; center line tile
  STA $2007
  
  LDA $2002             ; read PPU status to reset the high/low latch
  LDA #$22              ; row 16  
  STA $2006
  LDA #$10              ; column 16
  STA $2006
  LDA #$26              ; center line tile
  STA $2007

  ; Write attributes
  LDA $2002             ; read PPU status to reset the high/low latch
  LDA #$23
  STA $2006             ; write the high byte of $23C0 address
  LDA #$C0
  STA $2006             ; write the low byte of $23C0 address
  LDX #$00              ; start out at 0
LoadAttributeLoop:
  LDA #%00000000        ; load attribute data (palette 0 for all)
  STA $2007             ; write to PPU
  INX                   ; X = X + 1
  CPX #$40              ; Compare X to hex $40, decimal 64 - copying 64 bytes
  BNE LoadAttributeLoop
  RTS

InitializeGame:
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

;;;Set initial score values
  LDA #$00
  STA scoreOnes
  STA scoreTens
  STA scoreHundreds
  STA heartCounter      ; start Player 1 heart counter at 0
  STA score2Ones        ; Player 2 score
  STA score2Tens
  STA score2Hundreds
  STA heartCounter2     ; start Player 2 heart counter at 0


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
  JSR HandlePlayerMovement
  JSR HandleJump
  JSR ApplyPhysics
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

; Handle falling item (Left Zone - Player 1)
HandleFallingItemLeftZone:
  LDA itemactive
  BEQ JumpToSpawnNewItem ; if no item active, try to spawn one
  
  ; Move item down
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
  ; Bad item (broken heart) for Player 1 - check heart counter first
  LDA heartCounter
  BEQ BadHeartDecrementScorePlayer1  ; if heart counter is 0, decrement Player 1 score
  
  ; Heart counter is not 0, just reset it to 0
  LDA #$00
  STA heartCounter      ; reset Player 1 heart counter to 0
  JMP ItemCollisionDone

BadHeartDecrementScorePlayer1:
  ; Heart counter was already 0, decrement Player 1 main score
  JSR DecrementScore    ; decrement Player 1 main score
  JMP ItemCollisionDone

BadItemCollisionPlayer2:
  ; Bad item (broken heart) for Player 2 - check heart counter first
  LDA heartCounter2
  BEQ BadHeartDecrementScorePlayer2  ; if heart counter is 0, decrement Player 2 score
  
  ; Heart counter is not 0, just reset it to 0
  LDA #$00
  STA heartCounter2     ; reset Player 2 heart counter to 0
  JMP ItemCollisionDone

BadHeartDecrementScorePlayer2:
  ; Heart counter was already 0, decrement Player 2 main score
  JSR DecrementScore2   ; decrement Player 2 main score
  JMP ItemCollisionDone
  
GoodItemCollisionPlayer1:
  ; Good item (heart) for Player 1 - increment heart counter
  LDA heartCounter
  CLC
  ADC #$01
  STA heartCounter
  
  ; Check if heart counter reached 3
  CMP #$03
  BNE ItemCollisionDone ; if not 3, we're done
  
  ; Heart counter reached 3 - reset to 0 and increment main score
  LDA #$00
  STA heartCounter      ; reset heart counter to 0
  JSR IncrementScore    ; add 1 to Player 1 main score
  JMP ItemCollisionDone

GoodItemCollisionPlayer2:
  ; Good item (heart) for Player 2 - increment heart counter
  LDA heartCounter2
  CLC
  ADC #$01
  STA heartCounter2
  
  ; Check if heart counter reached 3
  CMP #$03
  BNE ItemCollisionDone ; if not 3, we're done
  
  ; Heart counter reached 3 - reset to 0 and increment main score
  LDA #$00
  STA heartCounter2     ; reset heart counter to 0
  JSR IncrementScore2   ; add 1 to Player 2 main score
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
  
  ; Move item down
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
  ; Good item (heart) for Player 2 - increment heart counter
  LDA heartCounter2
  CLC
  ADC #$01
  STA heartCounter2
  
  ; Check if heart counter reached 3
  CMP #$03
  BNE Item2CollisionDone ; if not 3, we're done
  
  ; Heart counter reached 3 - reset to 0 and increment main score
  LDA #$00
  STA heartCounter2     ; reset heart counter to 0
  JSR IncrementScore2   ; add 1 to Player 2 main score
  JMP Item2CollisionDone

BadItem2CollisionPlayer2:
  ; Bad item (broken heart) for Player 2 - check heart counter first
  LDA heartCounter2
  BEQ BadHeart2DecrementScorePlayer2  ; if heart counter is 0, decrement Player 2 score
  
  ; Heart counter is not 0, just reset it to 0
  LDA #$00
  STA heartCounter2     ; reset Player 2 heart counter to 0
  JMP Item2CollisionDone

BadHeart2DecrementScorePlayer2:
  ; Heart counter was already 0, decrement Player 2 main score
  JSR DecrementScore2   ; decrement Player 2 main score
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
  JSR DecrementScore    ; Player 1 loses a point
  LDA #$00
  STA heartCounter      ; reset Player 1 heart counter to 0
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
  JSR DecrementScore2   ; Player 2 loses a point
  LDA #$00
  STA heartCounter2     ; reset Player 2 heart counter to 0
  STA cakeactive        ; destroy the cake

CakeCollisionDone:
  RTS
 
 
 
 
UpdateSprites:
  ; Update player 1 sprite (sprite 0)
  LDA bally
  STA $0200
  
  LDA #$32              ; tile for player 1
  STA $0201
  
  LDA #$00              ; attributes (palette 0)
  STA $0202
  
  LDA ballx
  STA $0203
  
  ; Update player 2 sprite (sprite 3) - using sprite 3 to avoid conflicts
  LDA ball2y
  STA $020C             ; sprite 3 Y position
  
  LDA #$33              ; tile for player 2 (different tile)
  STA $020D             ; sprite 3 tile
  
  LDA #$01              ; attributes (palette 1)
  STA $020E             ; sprite 3 attributes
  
  LDA ball2x
  STA $020F             ; sprite 3 X position
  
  ; Update falling item sprite (sprite 1) - Left Zone
  LDA itemactive
  BEQ HideItemSprite    ; if item not active, hide sprite
  
  LDA itemy
  STA $0204             ; sprite 1 Y position
  
  ; Set tile based on item type
  LDA itemtype
  BEQ SetGoodItemTileLeft   ; if itemtype = 0, use tile 1 (heart)
  CMP #$01
  BEQ SetBadItemTileLeft    ; if itemtype = 1, use tile 2 (broken heart)
  ; itemtype = 2, cake
  LDA #$A0              ; cake uses tile $A0
  JMP SetItemTileLeft
SetBadItemTileLeft:
  LDA #$02              ; bad item uses tile 2 (broken heart)
  JMP SetItemTileLeft
SetGoodItemTileLeft:
  LDA #$01              ; good item uses tile 1 (heart)
SetItemTileLeft:
  STA $0205
  
  LDA #$01              ; attributes (different palette)
  STA $0206
  
  LDA itemx
  STA $0207             ; sprite 1 X position
  JMP UpdateRightZoneItemSprite

HideItemSprite:
  LDA #$FF              ; move sprite off screen
  STA $0204
  STA $0205
  STA $0206  
  STA $0207

UpdateRightZoneItemSprite:
  ; Update falling item sprite (sprite 4) - Right Zone
  LDA item2active
  BEQ HideItem2Sprite    ; if item not active, hide sprite
  
  LDA item2y
  STA $0210             ; sprite 4 Y position
  
  ; Set tile based on item type
  LDA item2type
  BEQ SetGoodItemTileRight   ; if itemtype = 0, use tile 1 (heart)
  CMP #$01
  BEQ SetBadItemTileRight    ; if itemtype = 1, use tile 2 (broken heart)
  ; itemtype = 2, cake
  LDA #$A0              ; cake uses tile $A0
  JMP SetItemTileRight
SetBadItemTileRight:
  LDA #$02              ; bad item uses tile 2 (broken heart)
  JMP SetItemTileRight
SetGoodItemTileRight:
  LDA #$01              ; good item uses tile 1 (heart)
SetItemTileRight:
  STA $0211
  
  LDA #$02              ; attributes (different palette)
  STA $0212
  
  LDA item2x
  STA $0213             ; sprite 4 X position
  JMP UpdateCakeSprite

HideItem2Sprite:
  LDA #$FF              ; move sprite off screen
  STA $0210
  STA $0211
  STA $0212  
  STA $0213

UpdateCakeSprite:
  ; Update cake sprite (sprite 2)
  LDA cakeactive
  BEQ HideCakeSprite    ; if cake not active, hide sprite
  
  LDA #GROUND_Y         ; cake Y position at floor level
  STA $0208             ; sprite 2 Y position
  
  LDA #$A0              ; cake tile
  STA $0209             ; sprite 2 tile
  
  LDA #$02              ; attributes (palette 2)
  STA $020A             ; sprite 2 attributes
  
  LDA cakex
  STA $020B             ; sprite 2 X position
  JMP UpdateSpritesDone

HideCakeSprite:
  LDA #$FF              ; move sprite off screen
  STA $0208
  STA $0209
  STA $020A  
  STA $020B

UpdateSpritesDone:
  RTS
 
 
DrawScore:
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
  
  ; Draw Player 1 heart counter at PPU $2028 (offset by 8 tiles)
  LDA $2002
  LDA #$20
  STA $2006
  LDA #$28
  STA $2006          ; start drawing Player 1 heart counter at PPU $2028
  
  LDA heartCounter   ; get Player 1 heart counter value
  STA $2007          ; draw heart counter to background
  
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
  
  ; Draw Player 2 heart counter at PPU $2034 (offset by -8 tiles from score)
  LDA $2002
  LDA #$20
  STA $2006
  LDA #$34
  STA $2006          ; start drawing Player 2 heart counter at PPU $2034
  
  LDA heartCounter2  ; get Player 2 heart counter value
  STA $2007          ; draw heart counter to background
  
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
  LDA #$00           ; can't go below 0, clamp to 000
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
  LDA #$00           ; can't go below 0, clamp to 000
  STA score2Hundreds
  STA score2Tens
  STA score2Ones
  JMP DecDone2
DecHundredsOK2:
  STA score2Hundreds
DecDone2:
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
  
  
    
        
;;;;;;;;;;;;;;  
  
  
  
  .bank 1
  .org $E000
palette:
  .db $22,$29,$1A,$0F,  $22,$36,$17,$0F,  $22,$30,$21,$0F,  $22,$27,$17,$0F   ;;background palette
  .db $22,$1C,$15,$14,  $22,$02,$38,$3C,  $22,$1C,$15,$14,  $22,$02,$38,$3C   ;;sprite palette

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
  .incbin "pilar.chr"   ;includes 8KB graphics file from SMB1