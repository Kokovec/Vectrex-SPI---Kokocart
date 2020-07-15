; ==========================================================================================
; Vectrex Kokocart Demo
;    This program is a demo for the Vectrex Kokocart.
;    Created with C64Studio (https://www.georg-rottensteiner.de/en/index.html)
;    For more information see Vectorbolt issue #9 (http://furyunlimited.com/_sgg/m3_1.htm).
;    by Dan Siewers
;
;    This program is free software: you can redistribute it and/or modify
;    it under the terms of the GNU General Public License as published by
;    the Free Software Foundation, either version 3 of the License, or
;    (at your option) any later version.
;
;    This program is distributed in the hope that it will be useful,
;    but WITHOUT ANY WARRANTY; without even the implied warranty of
;    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;    GNU General Public License for more details.
;
;    You should have received a copy of the GNU General Public License
;    along with this program.  If not, see <https://www.gnu.org/licenses/>.
; ===========================================================================================


;compile to this filename
!to "jmain.prg",cbm

;Define Registers
PORT_B_DATA     = 56577
PORT_B_DDR      = 56579
Sprite_Enable   = 53269
Sprite_Pos_Base = 53248
Sprite_XX_MSB   = $d010
Joy_2_Register  = $dc00

;Define Delay Routine Registers and Memory Locations
;(from here: https://www.lemon64.com/forum/viewtopic.php?t=48363)
ISRADDR         = $0314
ISRFUNC         = $EA31
STOPADDR        = $0328
STOPFUNC        = $F6ED
PALNORMAL       = $4025
NTSCNORMAL      = $4295
PALFAST         = $2012
NTSCFAST        = $214B
RASTERLINELO    = $D012
CIA1TIMERA      = $DC04
SYSTEMRESET     = $FCE2
TIMERTICK       = $081B
WAITTICK        = $081C
ISPAL           = $081A


;Define Kernal Routines
Clear_Screen = $e544

;Define Memory Locations
Counter1      = $fb
Counter2      = $fc
Counter3      = $fd
Counter4      = $fe
RX_Buffer     = 248
ScreenLSB     = 249
ScreenMSB     = 250
ShipPos_X     = $61
ShipPos_X_MSB = $62
ShipPos_Y     = $63
Sprite_State  = $64             ;


*=$C000

Program_Start
  jsr Setup_PortB                 ;Set up Port B
  jsr Black_Screen                ;Black border and background
  jsr Clear_Screen                ;Clear screen
  jsr Initialize_Sprites          ;Set up Sprites
  jsr Initialize_Screen_Pointer   ;Set up screen mem pointer to top left of screen
  jsr SetupIRQTimer               ;Set up the IRQ frame timer
  
  
  lda #00
  sta Counter3
Ping_Vectrex:
  lda #$5A
  jsr SPI_Transfer

Program_Loop
  jsr Read_Joy_2
  jsr SPI_Transfer
  jsr Handle_Sprite
  lda #4
  jsr Delay_Frame
  jmp Program_Loop
  
  
;*********************************
;*Subroutine: Set up User Port B *
;*********************************
Setup_PortB
  lda #%10000110
  sta PORT_B_DATA                 ;Port B: Clock high, SS high, Data low
  lda #%00000111                  ;Set Port B direction registers (0=in, 1=out)
  sta PORT_B_DDR
  rts

;***************************************
;*Subroutine: SPI Transfer / Receive   *
;*Params:                              *
;*        In: A = Data to transfer out *
;*    Return: X = Data received        *
;***************************************
; Initialize the transfer
SPI_Transfer
  pha                             ;Save data to stack
  ldx #$0
  stx RX_Buffer                   ;Clear the RX buffer
  ldy #07                         ;Loop counter for 8 bits of data
  and #%00000001                  ;Get bit 0 of transmit data
  ora #%00000100                  ;Set Clock high, SS Low, Keep data intact
  sta PORT_B_DATA                 ;Port B: Clock high, SS Low, Data = Acc A
Loop_Bits
  and #%00000001
  sta PORT_B_DATA                 ;Port B: Clock low, SS Low, Data = Acc A
  ora #%00000100
  sta PORT_B_DATA                 ;Port B: Clock high, SS Low, Data = Acc A
  lda PORT_B_DATA                 ;Retreive Data from adapter
  and #%10000000
  ora RX_Buffer
  cpy #00
  beq Receive_bits_Exit
  lsr a
Receive_bits_Exit
  sta RX_Buffer
  pla                             ;Retrieve data from stack
  dey
  bmi SPI_Exit                    ;Exit if done with all 8 bits of data
  lsr a                           ;Prep for next data bit
  pha                             ;Save data to stack
  jmp Loop_Bits
SPI_Exit
  lda #%000000110
  sta PORT_B_DATA                 ;Port B: Clock high, SS high, Data low
  ldx RX_Buffer
  rts

;********************
;* Clear the Screen *
;********************
Black_Screen
  lda #$01                        ;Screen and border are black
  sta $d020
  lda #0
  sta $d021
  rts

  
;**********************
;* Initialize Sprites *
;**********************
Initialize_Sprites
  ;Set Sprite forground colors to white
  lda #01                         ;Set Sprite forground colors to white
  sta $D027                       ;Sprite 0
  sta $D028                       ;Sprite 1
  sta $D029                       ;Sprite 2
  sta $D02a                       ;Sprite 3
  ;Copy Sprites to end of screen ram (where they belong)
  ldx #0
Sprite_Copy_Loop
  lda Sprite_0,x
  sta $2000,x
  inx
  bne Sprite_Copy_Loop
  ;Set Sprite pointer locations in memory (Sprite start=$2000)
  ldx #$80                        ;80*40 = 2000  (Sprite 0)
  stx $07f8
  inx                             ;80*41 = 2080  (Sprite 1)
  stx $07f9
  inx
  stx $07fa
  inx
  stx $07fb
  ;Set Sprite to center screen and turn on
  lda #127
  sta ShipPos_Y
  sta Sprite_Pos_Base+1
  lda #160
  sta ShipPos_X
  sta Sprite_Pos_Base
  ldx #0
  stx ShipPos_X_MSB
  stx Sprite_Pos_Base+16
  stx Sprite_State
  inx                             ;Turn on Sprite 0
  stx Sprite_Enable
  rts

;****************************************************************
;* Delay Initialization Routine                                 *
;* Sets ISPAL memory address to zero for NTSC, non-zero for PAL *
;****************************************************************
SetupIRQTimer
  sei
  ;Disable keyboard buffering here for convenience
  lda  #$01
  sta  $0289
  ;Load timer handler
  lda  #<timerISR
  sta  ISRADDR
  lda  #>timerISR
  sta  ISRADDR+1
  ;Insert into 'stop' a safe exit
  lda  #<SYSTEMRESET
  sta  STOPADDR
  lda  #>SYSTEMRESET
  sta  STOPADDR+1
  ;Test for NTSC or PAL
LDAScanLine
  lda  RASTERLINELO
CMPScanLine
  cmp  RASTERLINELO
  beq  CMPScanLine
  bmi  LDAScanLine
  and  #$F0
  sta  ISPAL
  bne  palTimerSet
  ;NTSC Timer Set
  lda  #<NTSCFAST
  sta  CIA1TIMERA
  lda  #>NTSCFAST
  sta  CIA1TIMERA+1
  jmp  SetupIRQTimer_Done
palTimerSet:
   lda  #<PALFAST
   sta  CIA1TIMERA
   lda  #>PALFAST
   sta  CIA1TIMERA+1
SetupIRQTimer_Done:
   cli
   rts

;********************************************************************
;* Delay Timer  Handler                                             *
;* (from here: https://www.lemon64.com/forum/viewtopic.php?t=48363) *
;********************************************************************
timerISR:
  inc  TIMERTICK
  lda  TIMERTICK
  ora  #1
  beq  localExit
  jmp  ISRFUNC
localExit:
  pla
  tay
  pla
  tax
  pla
  rti   

  
;********************************************************************
;* Delay Routine                                                    *
;* Params:                                                          *
;*        A = Delay in frames                                       *
;*                                                                  *
;* (from here: https://www.lemon64.com/forum/viewtopic.php?t=48363) *
;********************************************************************
Delay_Frame:
   tay
   lda  WAITTICK
Delay_Frame_Test:
   cmp  TIMERTICK
   beq  Delay_Frame_Test
   lda  TIMERTICK
   dey
   bne  Delay_Frame_Test
   sta  WAITTICK
   rts
   
   
;********************************************************
;* Read Joystick 2 Direction                            *
;* Output:                                              *
;*         A = Joy Direction                            *
;*         (01=Up, 02=Right, 03=Down, 04=Left, 0=None)  *
;********************************************************
Read_Joy_2:
  lda $dc00      ; store new value in memory location 2.
  sta $02    

  lda #%00000001 ; mask joystick up movement 
  and $02        ; bitwise AND with address 56320
  bne cont1      ; no movement up -> do not increase border color
  lda #01
  jmp cont5
cont1:   
  lda #%00000010 ; mask joystick down movement 
  and $02        ; bitwise AND with address 56320
  bne cont2      ; no movement down -> do not decrease border color
  lda #03
  jmp cont5
cont2:
  lda #%00000100 ; mask joystick left movement 
  and $02        ; bitwise AND with address 56320
  bne cont3      ; no movement left -> do not increase background color
  lda #04
  jmp cont5
cont3:
  lda #%00001000 ; mask joystick right movement 
  and $02        ; bitwise AND with address 56320
  bne cont4      ; no movement right -> do not decrease background color
  lda #02
  jmp cont5
cont4:
  lda #05        ; no joystick movement detected
cont5:
  rts            


;***********************************************
; Set Sprite Direction and Movement            *
; Input:                                       *
;       X = 01=Up, 02=Right, 03=Down, 04=Left  *
;***********************************************
Handle_Sprite
Check_Up  
  cpx #01                       ;Move ship UP?
  bne Check_Right
  jsr Calc_Move_Sprite_Up
  jmp Handle_Sprite_Move
Check_Right
  cpx #02                       ;Move ship RIGHT?
  bne Check_Down
  jsr Calc_Move_Sprite_Right
  jmp Handle_Sprite_Move
Check_Down
  cpx #03                       ;Move ship DOWN?
  bne Check_Left
  jsr Calc_Move_Sprite_Down
  jmp Handle_Sprite_Move
Check_Left
  cpx #04                       ;Move ship LEFT?
  bne Check_End
  jsr Calc_Move_Sprite_Left
Handle_Sprite_Move
  dex
  txa
  clc
  rol a
  tay
  lda ShipPos_X
  sta Sprite_Pos_Base,y
  txa
  pha
  lda ShipPos_X_MSB
Handle_Sprite_Move_Loop1
  cpx #0
  beq Handle_Sprite_Move_Loop1_End
  clc
  rol a
  dex
  bne Handle_Sprite_Move_Loop1
Handle_Sprite_Move_Loop1_End
  sta Sprite_Pos_Base+16
  pla
  tax
  iny
  lda ShipPos_Y
  sta Sprite_Pos_Base,y
  inx
  lda #0
  sec
Handle_Sprite_Move_Loop2
  rol a
  dex
  bne Handle_Sprite_Move_Loop2
  sta Sprite_Enable
Check_End
  rts

Calc_Move_Sprite_Up
  sec
  lda ShipPos_Y
  sbc #1
  cmp #49
  bne Calc_Move_Sprite_Up_End
  lda #229
Calc_Move_Sprite_Up_End
  sta ShipPos_Y
  rts

Calc_Move_Sprite_Down
  clc
  lda ShipPos_Y
  adc #01
  cmp #230
  bne Calc_Move_Sprite_Down_End
  lda #50
Calc_Move_Sprite_Down_End
  sta ShipPos_Y
  rts

Calc_Move_Sprite_Right
  clc
  lda #01
  adc ShipPos_X
  sta ShipPos_X
  bcc Calc_Move_Sprite_Right_Check
  inc ShipPos_X_MSB
  jmp Calc_Move_Sprite_Right_End
Calc_Move_Sprite_Right_Check
  lda ShipPos_X_MSB
  beq Calc_Move_Sprite_Right_End 
  lda ShipPos_X
  cmp #65
  bne Calc_Move_Sprite_Right_End
  lda #24
  sta ShipPos_X
  lda #0
  sta ShipPos_X_MSB
Calc_Move_Sprite_Right_End
  rts

Calc_Move_Sprite_Left
  sec
  lda ShipPos_X
  sbc #01
  sta ShipPos_X
  bcs Calc_Move_Sprite_Left_Check
  dec ShipPos_X_MSB
  jmp Calc_Move_Sprite_Left_End
Calc_Move_Sprite_Left_Check
  lda ShipPos_X_MSB
  bne Calc_Move_Sprite_Left_End
  lda ShipPos_X
  cmp #23
  bne Calc_Move_Sprite_Left_End
  lda #64
  sta ShipPos_X
  lda #1
  sta ShipPos_X_MSB
Calc_Move_Sprite_Left_End
  rts

;***********************************************
;Set up screen pointer for printing characters *
;***********************************************  
Initialize_Screen_Pointer
  lda #04                         ;Set screen pointer to top left
  sta ScreenMSB
  lda #$00
  sta ScreenLSB
  rts

  
;*****************************************************
;Print Hex byte as character to screen               *
;After printing screen pointer is moved to the right *
;Params:                                             *
;                 X:  Hex byte to print              *
;         ScreenLSB: LSB of 16 bit screen address    *
;         ScreenMSB: MSB of 16 bit screen address    *
;                                                    *
;Note: Top left of screen is $0400                   *
;*****************************************************
Print_To_Screen
  txa                             ;Preserve Hex data in X
  lsr a                           ;We want high nibble (left character)
  lsr a
  lsr a
  lsr a
  jsr Hex_To_Screen_Code          ;Get Screen Code
  sta Counter1                    ;Save left character
  txa                             ;Retrieve passed in Hex data
  and #%00001111                  ;We want lower nibble (right character)
  jsr Hex_To_Screen_Code          ;Get screen code
  sta Counter2                    ;Save right character
  ldy #0                          ;Reset screen location offset
  lda Counter1                    ;Get left character
  sta (ScreenLSB),y               ;Push it to screen  
  iny                             ;Next screen position
  lda Counter2                    ;Get right character
  sta (ScreenLSB),y               ;Push it to screen
  iny                             ;Next screen position
  lda #32                         ;Get space character
  sta (ScreenLSB),y               ;Push it to screen
  iny                             ;Next screen position
  sta (ScreenLSB),y               ;Push another space char to screen
  ;[Start of Note]
  ;Add 16 bit number to 8 bit constant
  ;Found this handy bit of code here:
  ;https://codebase64.org/doku.php?id=base:16bit_addition_and_subtraction
  clc   
  lda ScreenLSB                   ;Get screen location memory location
  adc #04                         ;Move screen pointer 4 positions to the right
  sta ScreenLSB
  bcc Check_Screen_End            ;Check if rolled over 255 ($ff)
  inc ScreenMSB                   ;Yes, add one to screen location memory pointer MSB
Check_Screen_End
  ;[End of Note]
  lda ScreenMSB                   ;Get screen location memory pointer MSB
  cmp #$07                        ;Nearing the end of screen memory?
  bne Print_Screen_Exit           ;No, we're done here
  lda ScreenLSB                   ;Yes, we're near the end of screen memory
  beq Print_Screen_Exit           ;Exit if 0 in screen location mem pointer LSB
  lda ScreenLSB                   
  cmp #$e8                        ;Check if we're at or went over screen memory
  bcs Print_Screen_Reset          ;Yes, reset pointer to upper left of screen
Print_Screen_Exit
  rts
Print_Screen_Reset
  lda #$04                         ;Set screen location memory pointer to top left
  sta ScreenMSB
  lda #$00
  sta ScreenLSB
  rts
  
  
; In: A = Hex number to convert (lower nibble)
; Out: A = Screen Code
Hex_To_Screen_Code
  cmp #10                     ;Check for number
  bpl Get_Letter
  clc                         ;Return number (0-9)
  adc #48
  rts
Get_Letter
  cld                         ;Return letter (A-F)
  sec
  sbc #9
  rts
  
  
Sprite_0
!byte  $00,$10,$00,$00,$10,$00,$00,$10
!byte  $00,$00,$28,$00,$00,$28,$00,$00
!byte  $28,$00,$00,$44,$00,$00,$44,$00
!byte  $00,$44,$00,$00,$82,$00,$00,$82
!byte  $00,$00,$82,$00,$00,$44,$00,$08
!byte  $28,$20,$08,$10,$20,$08,$28,$20
!byte  $08,$44,$20,$08,$82,$20,$09,$01
!byte  $20,$0e,$00,$e0,$00,$00,$00,$01

Sprite_1
!byte  $00,$00,$00,$00,$00,$00,$00,$00
!byte  $00,$7f,$00,$00,$40,$00,$00,$40
!byte  $00,$00,$20,$00,$00,$10,$70,$00
!byte  $08,$8e,$00,$05,$01,$c0,$02,$00
!byte  $38,$05,$01,$c0,$08,$8e,$00,$10
!byte  $70,$00,$20,$00,$00,$40,$00,$00
!byte  $40,$00,$00,$7f,$00,$00,$00,$00
!byte  $00,$00,$00,$00,$00,$00,$00,$01

Sprite_2
!byte  $1c,$01,$c0,$12,$02,$40,$11,$04
!byte  $40,$10,$88,$40,$10,$50,$40,$10
!byte  $20,$40,$10,$50,$40,$00,$88,$00
!byte  $01,$04,$00,$01,$04,$00,$01,$04
!byte  $00,$00,$88,$00,$00,$88,$00,$00
!byte  $88,$00,$00,$50,$00,$00,$50,$00
!byte  $00,$50,$00,$00,$20,$00,$00,$20
!byte  $00,$00,$20,$00,$00,$00,$00,$01

Sprite_3
!byte  $00,$00,$00,$00,$00,$00,$00,$00
!byte  $00,$00,$07,$f0,$00,$00,$10,$00
!byte  $00,$10,$00,$00,$20,$00,$70,$40
!byte  $03,$88,$80,$1c,$05,$00,$e0,$02
!byte  $00,$1c,$05,$00,$03,$88,$80,$00
!byte  $70,$40,$00,$00,$20,$00,$00,$10
!byte  $00,$00,$10,$00,$07,$f0,$00,$00
!byte  $00,$00,$00,$00,$00,$00,$00,$01

  
  