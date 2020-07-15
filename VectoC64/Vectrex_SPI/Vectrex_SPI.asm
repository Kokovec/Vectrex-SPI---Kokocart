; ==========================================================================
; Vectrex Kokocart Demo
;    This program is a demo for the Vectrex Kokocart.
;    Created with VIDE (http://vide.malban.de/what-is-vide)
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
; ===========================================================================



;***************************************************************************
; DEFINE SECTION
;***************************************************************************
; load vectrex bios routine definitions
                    INCLUDE  "VECTREX.I"                  ; vectrex function includes
scale               EQU      20 
;***************************************************************************
; Variable / RAM SECTION
;***************************************************************************
; insert your variables (RAM usage) in the BSS section
; user RAM starts at $c880 
ship_y              EQU      $c880                        ; 1 byte 
ship_x              EQU      $c881                        ; 1 byte 
ship_angle          EQU      $c882                        ; 2 bytes 
c64_ship_angle      EQU      $c884 
counter             EQU      $c885 
counter2            EQU      $c886 
rot_VL              EQU      $c887                        ; 13 bytes 
;***************************************************************************
; HEADER SECTION
;***************************************************************************
; The cartridge ROM starts at address 0
                    CODE     
                    ORG      0 
; the first few bytes are mandatory, otherwise the BIOS will not load
; the ROM file, and will start MineStorm instead
                    DB       "g GCE 2020", $80 ; 'g' is copyright sign
                    DW       music1                       ; music from the rom 
                    DB       $F8, $50, $20, -$30          ; height, width, rel y, rel x (from 0,0) 
                    DB       "VECTREX SPI", $80           ; some game information, ending with $80
                    DB       0                            ; end of game header 
initialization: 
                    lda      # 'A'                        ; Buffer for print hex char routine
                    sta      $cb00 
                    lda      # ' '
                    sta      $cb01 
                    lda      #$80 
                    sta      $cb02 
                    LDD      #0                           ;ship y,x position 
                    STD      ship_y 
                    LDA      #$ff                         ;VL draw pattern (solid line) 
                    STA      $C829 
                    LDD      #$0001                       ; Vectrex ship starts pointing up
                    STD      ship_angle 
                    LDA      #50                          ; draw scale to 50 
                    STA      VIA_t1_cnt_lo               
                    LDA      #1                           ; these set up the joystick 
                    STA      Vec_Joy_Mux_1_X              ; enquiries 
                    LDA      #3                           ; allowing only all directions 
                    STA      Vec_Joy_Mux_1_Y              ; for joystick one 
                    LDA      #0                           ; this setting up saves a few 
                    STA      Vec_Joy_Mux_2_X              ; hundred cycles 
                    STA      Vec_Joy_Mux_2_Y              ; don't miss it, if you don't 
                                                          ; need the second joystick! 
                    LDA      #0                           ; set c64 ship angle to: no move 
                    STA      c64_ship_angle 
                    STA      $7FFF                        ; clear the SPI TX data mem location
                    STA      counter                      ; clear general variable
;***************************************************************************
; CODE SECTION
;***************************************************************************
; here the cartridge program starts off
main: 
                    JSR      SPI_Start                    ; Wait for C64
main_loop: 
                    JSR      Joy_Digital                  ; read joystick positions 
                    LDA      Vec_Joy_1_X                  ; load joystick 1 position 
                    BEQ      x_done                       ; if zero, than no x position 
                    BMI      left_move                    ; if negative, than left 
                                                          ; otherwise right 
right_move: 
                    LDA      #02                          ; C64 ship moves right 
                    STA      c64_ship_angle 
                    BRA      y_done                       ; goto x done 

left_move: 
                    LDA      #04                          ; C64 ship moves left 
                    STA      c64_ship_angle 
                    BRA      y_done                       ; goto x done 

x_done: 
                    LDA      Vec_Joy_1_Y                  ; load joystick 1 position 
                                                          ; Y to A 
                    BEQ      no_y_movement                ; if zero, than no y position 
                    BMI      down_move                    ; if negative, than down 
                                                          ; otherwise up 
up_move: 
                    LDA      #01                          ; C64 ship moves up 
                    STA      c64_ship_angle 
                    BRA      y_done                       ; goto y done 

down_move: 
                    LDA      #03                          ; C64 ship moves down 
                    STA      c64_ship_angle 
                    BRA      y_done                       ; goto y done 

no_y_movement: 
                    LDA      #$FF                         ; C64 ship doesn't move 
                    STA      c64_ship_angle 
y_done: 
                    lda      counter                      ;check if received command from C64 last go'round 
                    BNE      command_from_C64             ;yep, go take care of that 
                    LDB      #0                           ;nope, check again without sending out data to SPI 
                    BRA      get_SPI 

command_from_C64: 
                    LDA      #0                           ;reset the command recieved flag 
                    STA      counter 
                    LDB      c64_ship_angle               ;get the new C64 ship angle 
get_SPI: 
                    STB      $7FFF                        ;store TX data in SPI out memory location 
                    LDY      $8000                        ;ping the SPI interface 
                    JSR      Delay_1 
                    LDA      $7FFE                        ;check if anything from C64 
                    BEQ      draw_ship                    ;nope, 
                    CMPA     #5                           ;yep, see if just a ping from c64 
                    BEQ      get_c6_data                  ;it's not, go draw Vectrex ship 
                    CMPA     #1                           ;move up? 
                    BNE      check_right 
                    INC      ship_y 
                    LDA      #0 
                    STA      ship_angle+1 
check_right: 
                    CMPA     #2                           ;move right? 
                    BNE      check_down 
                    STA      ship_angle+1 
                    INC      ship_x 
check_down: 
                    CMPA     #3                           ;move down? 
                    BNE      check_left 
                    STA      ship_angle+1 
                    DEC      ship_y 
check_left: 
                    CMPA     #4                           ;move left? 
                    BNE      get_c6_data: 
                    STA      ship_angle+1 
                    DEC      ship_x 
get_c6_data: 
                    LDB      #1                           ; it is! flag it 
                    STB      counter 
draw_ship: 
                    JSR      Wait_Recal                   ; Vectrex BIOS recalibration 
                    JSR      Intensity_5F                 ; Sets the intensity of the 
                    LDX      ship_angle 
                    JSR      rotate_ship 
                    LDA      #$7F                         ; load 50 
                    STA      VIA_t1_cnt_lo                ; 50 as scaling 
                    LDD      ship_y 
                    JSR      Moveto_d 
                    LDA      #scale                       ; load 50 
                    STA      VIA_t1_cnt_lo                ; 50 as scaling 
                    LDX      #rot_VL 
                    JSR      Draw_VL_mode 
                    BRA      main_loop                    ; and repeat forever 

;Establish C64 connection
SPI_Start: 
                    LDB      #$00                         ; wait for C64 to send out a ping ($5A) 
                    STA      $7FFF                        ; store $00 in SPI TX buffer (get data but don't send out) 
                    LDY      $8000 
                    LDB      #30 
                    JSR      Delay_b 
                    JSR      Wait_Recal                   ; Vectrex BIOS recalibration 
                    LDB      #30 
                    JSR      Delay_b 
                    JSR      Intensity_5F 
                    LDD      #0 
                    JSR      Moveto_d 
                    LDU      #spi_string 
                    LDA      #$10                         ; Text position relative Y 
                    LDB      #-110                        ; Text position relative X 
                    JSR      Print_Str_d                  ; Vectrex BIOS print routine 
                    LDA      $7FFE                        ; check to see if C64 has responded 
                    CMPA     #$5A 
                    BNE      SPI_Start 
                    RTS      

; Rotate Vectrex ship
;Param: X = rotation angle
rotate_ship: 
                    JSR      DP_to_C8 
                    LDA      #scale 
                    STA      VIA_t1_cnt_lo                ; 50 as scaling 
                    LDA      angles,X 
                    LDU      #rot_VL 
                    LDX      #ship_vl 
                    JSR      Rot_VL_Mode 
                    JSR      DP_to_D0 
                    RTS      

; Used for dev testing
SPI_Start_Test: 
                    LDA      $00 
                    STA      $7FFF 
                    STA      counter 
Looper: 
                    LDY      $8000 
                    LDB      #30 
                    JSR      Delay_b 
                    JSR      Wait_Recal                   ; Vectrex BIOS recalibration 
                    JSR      Intensity_5F 
                    LDU      #$cb00                       ; address of string 
                    LDA      #$10                         ; Text position relative Y 
                    LDB      #-$50                        ; Text position relative X 
                    JSR      Print_Str_d                  ; Vectrex BIOS print routine 
                    LDA      counter 
                    LSRA     
                    LSRA     
                    LSRA     
                    LSRA     
                    CMPA     #$0A 
                    BLO      ascii_number 
                    ADDA     #65-10 
                    BRA      ascii_next 

ascii_number: 
                    ADDA     #$30 
ascii_next: 
                    STA      $cb00 
                    LDA      counter 
                    ANDA     #$0F 
                    CMPA     #$0A 
                    BLO      ascii_number_lo 
                    ADDA     #65-10 
                    BRA      ascii_next_lo 

ascii_number_lo: 
                    ADDA     #$30 
ascii_next_lo: 
                    STA      $cb01 
                    LDA      $7FFE                        ; check to see if C64 has responded 
                    CMPA     #$00 
                    BNE      new_num 
                    STA      $7FFF 
                    BRA      Looper 

new_num: 
                    STA      counter 
                    STA      $7FFF 
                    BRA      Looper 

                    RTS      

;***************************************************************************
; DATA SECTION
;***************************************************************************
ship_vl: 
                    DB       $00                          ; 
                    DB       $3F                          ; 
                    DB       $00                          ; 
                    DB       $FF                          ; 
                    DB       $C4                          ; 
                    DB       $08                          ; 
                    DB       $FF                          ; 
                    DB       $D8                          ; 
                    DB       $D8                          ; 
                    DB       $FF                          ; 
                    DB       $20                          ; 
                    DB       $00                          ; 
                    DB       $00                          ; 
                    DB       $00                          ; 
                    DB       $40                          ; 
                    DB       $FF                          ; 
                    DB       $E0                          ; 
                    DB       $00                          ; 
                    DB       $FF                          ; 
                    DB       $28                          ; 
                    DB       $D8                          ; 
                    DB       $FF                          ; 
                    DB       $3C                          ; 
                    DB       $08                          ; 
                    DB       $01                          ; 
angles: 
                    DB       0 
                    DB       0                            ;UP 
                    DB       48                           ;RIGHT 
                    DB       32                           ;DOWN 
                    DB       16                           ;LEFT 
spi_string: 
                    DB       "WAITING"                 ; only capital letters
                    DB       $80,$80,$80               ; $80 is end of string 
;***************************************************************************
