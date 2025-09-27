  .inesprg 1   ; 1x 16KB PRG code
  .ineschr 1   ; 1x  8KB CHR data
  .inesmap 0   ; mapper 0 = NROM, no bank swapping
  .inesmir 1   ; background mirroring
  

;;;;;;;;;;;;;;;

;; DECLARE SOME VARIABLES HERE
  .rsset $0000  ;;start variables at ram location 0
  
gamestate     .rs 1  ; .rs 1 means reserve one byte of space
ballx         .rs 1  ; ball horizontal position
bally         .rs 1  ; ball vertical position
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
scoreOnes     .rs 1  ; byte for each digit in the decimal score
scoreTens     .rs 1
scoreHundreds .rs 1
heartCounter  .rs 1  ; secondary counter (0-4, resets at 5)
; falling item variables
itemx         .rs 1  ; falling item X position
itemy         .rs 1  ; falling item Y position
itemactive    .rs 1  ; 1 = item is falling, 0 = no item
itemtype      .rs 1  ; 0 = good item (heart), 1 = bad item (broken heart), 2 = cake
itemspeed     .rs 1  ; falling speed
randomseed    .rs 1  ; simple random number seed
; cake variables
cakex         .rs 1  ; cake X position
cakeactive    .rs 1  ; 1 = cake is moving, 0 = no cake
cakespeed     .rs 1  ; cake horizontal speed
; physics variables
velocity_y    .rs 1  ; player vertical velocity (signed: $00-$7F = down, $80-$FF = up)
on_ground     .rs 1  ; 1 = player is on ground, 0 = player is in air
jump_pressed  .rs 1  ; 1 = jump button was pressed this frame
jump_counter  .rs 1  ; frames remaining in jump (0 = not jumping)


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
  
  LDA #$80
  STA ballx
  
  LDA #$02
  STA ballspeedx
  STA ballspeedy

;;;Set initial falling item values
  LDA #$00
  STA itemactive       ; no item active at start
  STA itemtype         ; start with good item type
  LDA #$02
  STA itemspeed        ; falling speed
  LDA #$01
  STA randomseed       ; initial seed for random
  
;;;Set initial cake values
  LDA #$00
  STA cakeactive       ; no cake active at start
  LDA #$02
  STA cakespeed        ; cake horizontal speed

;;;Set initial physics values
  LDA #$00
  STA velocity_y       ; start with no vertical velocity
  STA jump_pressed     ; no jump pressed initially
  STA jump_counter     ; not jumping initially
  STA on_ground        ; start in air

;;;Set initial score value
  LDA #$00
  STA scoreOnes
  STA scoreTens
  STA scoreHundreds
  STA heartCounter      ; start heart counter at 0


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

; Physics-based player movement
HandlePlayerMovement:
  ; Handle horizontal movement (left/right) - ONLY when on ground
  LDA on_ground
  BEQ SkipHorizontalMovement  ; if not on ground (jumping OR falling), skip horizontal movement

MoveBallLeft:
  LDA buttons1
  AND #%00000010
  BEQ MoveBallLeftDone   ; left button not pressed
  
  LDA ballx
  SEC
  SBC ballspeedx        ; ballx position = ballx - ballspeedx
  STA ballx
  
  LDA ballx
  CMP #LEFTWALL
  BCS MoveBallLeftDone  ; if ball x > left wall, still on screen
  LDA #LEFTWALL
  STA ballx             ; clamp to left wall
MoveBallLeftDone:

MoveBallRight:
  LDA buttons1
  AND #%00000001
  BEQ MoveBallRightDone   ; right button not pressed

  LDA ballx
  CLC
  ADC ballspeedx        ; ballx position = ballx + ballspeedx
  STA ballx

  LDA ballx
  CMP #RIGHTWALL
  BCC MoveBallRightDone ; if ball x < right wall, still on screen
  LDA #RIGHTWALL
  STA ballx             ; clamp to right wall
MoveBallRightDone:

SkipHorizontalMovement:

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
  BEQ HandleJumpDone    ; A button not pressed
  
  ; Check if already jumping or in air
  LDA on_ground
  BEQ HandleJumpDone    ; can't jump if not on ground
  
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

HandleJumpDone:

  ; SIMPLE COUNTER-BASED PHYSICS
ApplyPhysics:
  ; Handle jumping (if jump_counter > 0)
  LDA jump_counter
  BEQ NotJumping        ; if counter = 0, not jumping
  
  ; Still jumping - move up and decrease counter
  DEC jump_counter      ; decrease jump counter
  LDA bally
  SEC
  SBC #JUMP_SPEED       ; move up at jump speed
  STA bally
  
  ; Check ceiling collision
  CMP #TOPWALL
  BCS NotJumping        ; if y >= top wall, no ceiling hit
  LDA #TOPWALL
  STA bally
  LDA #$00
  STA jump_counter      ; stop jumping
  JMP ApplyGravity

NotJumping:
  ; Apply gravity (fall down) if not on ground
  LDA on_ground
  BNE SkipMovement      ; skip if on ground

ApplyGravity:
  ; Simple gravity - move down at fall speed
  LDA bally
  CLC
  ADC #FALL_SPEED       ; move down at fall speed
  STA bally

CheckGroundHit:
  ; Debug: Show current bally position as sprite attribute
  LDA bally
  STA $0202             ; show Y position as sprite color
  
  ; Check if player hit or passed through the ground
  LDA bally
  CMP #GROUND_Y
  BCC SkipMovement      ; if y < ground level, still in air
  
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

SkipMovement:

; Handle falling item
HandleFallingItem:
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
  JMP SpawnNewItem

CheckItemCollision:
  ; Simpler collision detection - check if sprites overlap
  ; Check X collision (item must be within 16 pixels of player)
  LDA ballx
  CLC
  ADC #$08              ; player right edge
  CMP itemx
  BCC NoCollision       ; if player right < item left, no collision
  
  LDA itemx
  CLC  
  ADC #$08              ; item right edge
  CMP ballx
  BCC NoCollision       ; if item right < player left, no collision
  
  ; Check Y collision (item must be within 16 pixels of player)
  LDA bally
  CLC
  ADC #$08              ; player bottom edge
  CMP itemy
  BCC NoCollision       ; if player bottom < item top, no collision
  
  LDA itemy
  CLC
  ADC #$08              ; item bottom edge  
  CMP bally
  BCC NoCollision       ; if item bottom < player top, no collision
  
  ; Collision detected! Check item type
  LDA itemtype
  BEQ GoodItemCollision  ; if itemtype = 0, good item
  CMP #$01
  BEQ BadItemCollision   ; if itemtype = 1, bad item
  ; itemtype = 2, cake item
  JMP CakeItemCollision
  
BadItemCollision:
  ; Bad item (broken heart) - decrement main score and reset heart counter
  ; JSR DecrementScore
  LDA #$00
  STA heartCounter      ; reset heart counter to 0
  JMP ItemCollisionDone
  
GoodItemCollision:
  ; Good item (heart) - increment heart counter
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
  JSR IncrementScore    ; add 1 to main score
  JMP ItemCollisionDone

CakeItemCollision:
  ; Cake collected - start it moving horizontally at floor level
  LDA #$01
  STA cakeactive        ; activate cake
  LDA itemx             ; start cake at the X position where it was caught
  STA cakex
  ; Cake Y position is fixed at floor level (GROUND_Y)
  
ItemCollisionDone:
  LDA #$00
  STA itemactive
  JMP HandleFallingItemDone

NoCollision:
  JMP HandleFallingItemDone

SpawnNewItem:
  ; Generate random numbers for position and type
  LDA randomseed
  CLC
  ADC #$17              ; add prime number
  STA randomseed
  
  ; Use bits 6-7 to determine item type (0, 1, or 2)
  AND #$C0              ; check bits 6-7
  CMP #$00              ; 00 = good item (heart)
  BEQ SetGoodItem
  CMP #$40              ; 01 = bad item (broken heart)  
  BEQ SetBadItem
  ; 10 or 11 = cake
  LDA #$02              ; cake item
  JMP SetItemType
SetBadItem:
  LDA #$01              ; bad item
  JMP SetItemType
SetGoodItem:
  LDA #$00              ; good item
SetItemType:
  STA itemtype
  
  ; Generate random X position
  LDA randomseed
  CLC
  ADC #$23              ; add another prime number for X position
  STA randomseed
  AND #$7F              ; keep in range 0-127
  CLC
  ADC #$10              ; add offset to keep away from edges
  CMP #$E0              ; check if too far right
  BCC SpawnItemOK
  LDA #$80              ; if too far, use middle position
SpawnItemOK:
  STA itemx
  LDA #TOPWALL
  STA itemy
  LDA #$01
  STA itemactive

HandleFallingItemDone:

; Handle cake movement
HandleCakeMovement:
  LDA cakeactive
  BEQ HandleCakeMovementDone  ; if no cake active, skip
  
  ; Move cake to the right
  LDA cakex
  CLC
  ADC cakespeed
  STA cakex
  
  ; Check if cake moved off screen
  CMP #$F8              ; right edge + some margin
  BCC HandleCakeMovementDone
  
  ; Cake moved off screen, deactivate it
  LDA #$00
  STA cakeactive

HandleCakeMovementDone:

  JMP GameEngineDone
 
 
 
 
UpdateSprites:
  ; Update player ball sprite (sprite 0)
  LDA bally
  STA $0200
  
  ; LDA #$00              ; tile 0 for player ball (commented out for debugging)
  ; STA $0201
  
  ; LDA #$00              ; attributes (commented out for debugging)
  ; STA $0202
  
  LDA ballx
  STA $0203
  
  ; Update falling item sprite (sprite 1)
  LDA itemactive
  BEQ HideItemSprite    ; if item not active, hide sprite
  
  LDA itemy
  STA $0204             ; sprite 1 Y position
  
  ; Set tile based on item type
  LDA itemtype
  BEQ SetGoodItemTile   ; if itemtype = 0, use tile 1 (heart)
  CMP #$01
  BEQ SetBadItemTile    ; if itemtype = 1, use tile 2 (broken heart)
  ; itemtype = 2, cake
  LDA #$A0              ; cake uses tile $A0
  JMP SetItemTile
SetBadItemTile:
  LDA #$02              ; bad item uses tile 2 (broken heart)
  JMP SetItemTile
SetGoodItemTile:
  LDA #$01              ; good item uses tile 1 (heart)
SetItemTile:
  STA $0205
  
  LDA #$01              ; attributes (different palette)
  STA $0206
  
  LDA itemx
  STA $0207             ; sprite 1 X position
  JMP UpdateCakeSprite

HideItemSprite:
  LDA #$FF              ; move sprite off screen
  STA $0204
  STA $0205
  STA $0206  
  STA $0207

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
  ; Draw main score at PPU $2020
  LDA $2002
  LDA #$20
  STA $2006
  LDA #$20
  STA $2006          ; start drawing the score at PPU $2020
  
  LDA scoreHundreds  ; get first digit
  STA $2007          ; draw to background
  LDA scoreTens      ; next digit
  STA $2007
  LDA scoreOnes      ; last digit
  STA $2007
  
  ; Draw heart counter at PPU $2028 (offset by 8 tiles)
  LDA $2002
  LDA #$20
  STA $2006
  LDA #$28
  STA $2006          ; start drawing heart counter at PPU $2028
  
  LDA heartCounter   ; get heart counter value
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