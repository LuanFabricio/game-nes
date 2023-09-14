.segment "HEADER"
  .byte $4E, $45, $53, $1A ; iNES header identifier ("NES", $1A)
  .byte 2             		 ; 2x 16KB PRG code
  .byte 1 		             ; 1x 8KB CHR data
  .byte $01, $00           ; mapper 0, vertical mirroring

.segment "VECTORS"
  ;; When an NMI happens the label nmi will be called
  .addr nmi
  ;; When the processor first turns on or reset
  .addr reset
  ;; External interrupt IRQ
  .addr 0

.segment "STARTUP"

; Main code segment
.segment "CODE"

reset:
  sei         ; disable IRQs
  cld         ; disable decimal mode
  ldx #$40
  stx $4017   ; disable APU frame IRQ
  ldx #$ff    ; Set up stack
  txs
  inx         ; 0xff -> 0x00
  stx $2000   ; disable NMI
  stx $2001   ; disable rendering
  stx 4010    ; disable DMC IRQs

;; first wait for vblank (to make sure the PPU is ready)
vblankwait1:
  bit $2002
  bpl vblankwait1

clear_memory:
  lda #$00
  sta $0000, x
  sta $0100, x
  sta $0200, x
  sta $0300, x
  sta $0400, x
  sta $0500, x
  sta $0600, x
  sta $0700, x
  inx
  bne clear_memory

main:
  lda #$6f
  sta $00 ; store player's y
  sta $02 ; store balls's y
  lda #$88
  sta $01 ; store ball's x

  lda #%11  ; 1st bit = 0 -> left | 1st bit = 1 -> right
            ; 2st bit = 0 -> down | 2st bit = 1 -> up
  sta $03 ; store ball's direction

load_palettes:
  lda $2002  ; read PPU status to reset high/low latch
  lda #$3f
  sta $2006
  lda #$00
  sta $2006
  ldx #$00
@loop:
  lda palettes, x
  sta $2007
  inx
  cpx #$20
  bne @loop

enable_rendering:
  lda #%10000000    ; Enable NMI
  sta $2000
  lda #%00010000    ; Enable Sprites
  sta $2001

;; Infinite loop
forever:
  jmp forever

nmi:
  ldx #$00          ; Set SPR_RAM address to 0
  stx $2003

@read_player_input:
  ; controller read setup
  lda #$01
  sta $4016
  lda #$00
  sta $4016
  ; Skipping some buttons
  ldy #$00
  @skip_buttons_loop: ;; skips btn A, B, Select and Start
    lda $4016
    iny
    cpy #$04
    bne @skip_buttons_loop

  lda $4016           ;; reading btn Up
  and #$01
  bne player_move_up
  lda $4016           ;; reading btn Down
  and #$01
  bne player_move_down
  jmp player_draw

player_move_up:
  dec $00
  lda $00
  cmp #$08            ;; checkin if is the min y
  bcs player_move_up_ok
  ldy #$08            ;; reseting to the lowest y possible
  sty $00
player_move_up_ok:
  jmp player_draw
player_move_down:
  inc $00
  lda $00
  cmp #$d0            ;; checkin if is the max y
  bcc player_move_down_ok
  ldy #$d0            ;; reseting to the highest y possible
  sty $00
player_move_down_ok:

;; Draw loop
; loop: lda hello, x ; Loading a hello message into SPR-RAM
;   sta $2004
;   inx
;   cpx #$14
;   bne loop

player_draw:
;; Player draw
ldx #$00
@player_loop: lda player, x ; Loading a hello message into SPR-RAM
  clc
  adc $00
  sta $2004
  inx
  ldy #$00
  @player_loop2:
    lda player, x
    sta $2004
    inx
    iny
    cpy #$03
    bne @player_loop2
  cpx #$0c
  bne @player_loop

move_ball:
  lda $03           ; loading ball direction
  and #%1           ; checking first bit (up or down)
  cmp #$01
  beq move_up
  move_down:
    inc $02
    jmp :+
  move_up:
    dec $02
  :
  lda $03           ; loading ball direction
  and #$02          ; checking second bit (left or right)
  cmp #$02
  beq move_right
  move_left:
    dec $01
    jmp :+
  move_right:
    inc $01

:
  ldx #$00
  lda $02       ; loading ball's y position
  sta $2004     ; setting sprite's y position
@ball_loop: lda ball, x
  sta $2004
  inx
  cpx #$02
  bne @ball_loop
  lda $01       ; loading ball's x position
  sta $2004     ; setting sprite's x position

check_ball_y_position:
  check_up_position:
    lda $03
    and #$01
    cmp #$01
    bne check_down_position
    lda $02
    cmp #$06
    bne check_ball_x_position
    inc $02
    jmp check_ball_y_position_flip
  check_down_position:
    lda $02
    cmp #$e0
    bne check_ball_x_position
    dec $02
  check_ball_y_position_flip:
    jsr flip_ball_y

check_ball_x_position:
  check_right_position:
    lda $03
    and #$02
    cmp #$02
    bne check_left_position
    lda $01
    cmp #$fa
    bne check_ball_x_position_end
    dec $01
    jsr flip_ball_x
    jmp check_ball_x_position_end
  check_left_position:
    lda $01
    cmp #$06
    bne check_ball_x_position_end
    inc $01
  check_ball_x_position_flip:
    jsr flip_ball_x

check_ball_x_position_end:

  jsr check_player_collision
  rti

;; Subroutine to flip ball x direction
flip_ball_x:
  lda $03           ; loading ball direction
  and #%10
  cmp #%10
  bne set_ball_right
set_ball_left:
  lda $03
  and #%01
  sta $03
  jmp end
set_ball_right:
  lda $03
  ora #%10
  sta $03
end:
  rts

;; Subroutine to flip ball y direction
flip_ball_y:
  lda $03           ; loading ball direction
  and #%01
  cmp #%01
  bne set_ball_up
set_ball_down:
  lda $03
  and #%10
  sta $03
  jmp flip_ball_y_end
set_ball_up:
  lda $03
  ora #%01
  sta $03
flip_ball_y_end:
  rts

check_player_collision:
  check_x_collision:
    lda $01             ; loading ball x position
    cmp #$08            ; ball_x == player_x
    bne check_player_collision_end
    lda $00
    ldx #$00
  check_y_collision:
    cmp $02
    bcc check_next_part
    jsr flip_ball_x
    jsr add_score
    jmp check_player_collision_end
  check_next_part:
    clc
    adc #$08
    inx
    cpx #$03
    bne check_y_collision

check_player_collision_end:
  rts

;; subroutine do increment player's score
add_score:
  inc $04
  rts

hello:
  ; Y / Sprite_num / Attributes / X
  .byte $6f, $00, $00, $6c ; y = $6f / sprite = 00 (H) / attributes = 0 / x = $6c
  .byte $6f, $01, $00, $76 ; y = $6f / sprite = 01 (E) / attributes = 0 / x = $76
  .byte $6f, $02, $00, $80 ; y = $6f / sprite = 02 (L) / attributes = 0 / x = $80
  .byte $6f, $02, $00, $8A ; y = $6f / sprite = 02 (L) / attributes = 0 / x = $8A
  .byte $6f, $03, $00, $94 ; y = $6f / sprite = 03 (O) / attributes = 0 / x = $94

player:
  .byte $00, $04, $00, $08 ; y = $67 / sprite = 00 (|) / attributes = 0 / x = $08
  .byte $08, $04, $00, $08 ; y = $6f / sprite = 00 (|) / attributes = 0 / x = $08
  .byte $10, $04, $00, $08 ; y = $77 / sprite = 00 (|) / attributes = 0 / x = $08

ball:
  .byte $05, $00, $88 ; sprite = 05 (ball) / attributes = 0 / x = $88

palettes:
  ; Background palette
  .byte $0f, $00, $00, $00
  .byte $0f, $00, $00, $00
  .byte $0f, $00, $00, $00
  .byte $0f, $00, $00, $00

  ; Sprite palette
  .byte $0f, $20, $00, $00
  .byte $0f, $00, $00, $00
  .byte $0f, $00, $00, $00
  .byte $0f, $00, $00, $00

; Character memory
.segment "CHARS"
  ; H (00)
  .byte %11000011
  .byte %11000011
  .byte %11000011
  .byte %11111111
  .byte %11111111
  .byte %11000011
  .byte %11000011
  .byte %11000011
  .byte $00, $00, $00, $00, $00, $00, $00, $00

  ; E (01)
  .byte %11111111
  .byte %11111111
  .byte %11000000
  .byte %11111100
  .byte %11111100
  .byte %11000000
  .byte %11111111
  .byte %11111111
  .byte $00, $00, $00, $00, $00, $00, $00, $00

  ; L (02)
  .byte %11000000
  .byte %11000000
  .byte %11000000
  .byte %11000000
  .byte %11000000
  .byte %11000000
  .byte %11111111
  .byte %11111111
  .byte $00, $00, $00, $00, $00, $00, $00, $00

  ; O (03)
  .byte %11111111
  .byte %11111111
  .byte %11000011
  .byte %11000011
  .byte %11000011
  .byte %11000011
  .byte %11111111
  .byte %11111111
  .byte $00, $00, $00, $00, $00, $00, $00, $00

  ; | (04)
  .byte %11000000
  .byte %11000000
  .byte %11000000
  .byte %11000000
  .byte %11000000
  .byte %11000000
  .byte %11000000
  .byte %11000000
  .byte $00, $00, $00, $00, $00, $00, $00, $00

  ; ball (05)
  .byte %00111100
  .byte %01111110
  .byte %11111111
  .byte %11111111
  .byte %11111111
  .byte %11111111
  .byte %01111110
  .byte %00111100
  .byte $00, $00, $00, $00, $00, $00, $00, $00
