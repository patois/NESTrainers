; kungfu trainer for the NES
;
; this is a very simple trainer. my third one written for the
; NES. It doesn't have a graphical interface. in order to activate
; the trainer's options, press the following in-game button combinations:
;
; SELECT + A = refresh energy
; SELECT + B = number of lifes left = 3 (will display "2", but you have a "zero" life :)
; SELECT + START = kill enemy / boss
;

        processor   6502
		  
        seg         code
        org         $7000

; this is the original NMI vector routine address,
; taken from the ROM image
OLD_NMI_VEC         equ $82D6
first_joypad        equ $4016

; ##################################################
;
; this is the entrypoint of the trainer's code ($7000)
; 

; save registers on stack
  php ; save p
  pha ; save a
  txa ; save x
  pha
  tya ; save y
  pha  
  

  jsr joy_init

  lda $4016 ;a
  lda $4016 ;b
  lda $4016 ;select
  and #$01
  beq return_nmi

  jsr joy_init
  
  lda $4016     ; select + a
  and #$01
  beq check_b

  lda #$30      ; refresh energy
  ldx #$A6
  sta $400,x 

check_b:
  lda $4016     ; select + b
  and #$01
  
  beq check_start
  
  lda #$03      ; number of lifes = 3
  sta $5C

check_start:
  lda $4016 ;select ; select + start
  lda $4016 ;start
  and #$01
  beq return_nmi

  lda #$FF      ; kill boss
  ldx #$A5
  sta $400,x

return_nmi:     ; chain to original NMI routine
  pla ; restore y
  tay
  pla ; restore x
  tax
  pla ; restore a
  plp ; restore p

  jmp OLD_NMI_VEC ; jump back to original NMI routine

joy_init:
  lda #$01	; strobe joypad
  sta first_joypad
  lda #$00
  sta first_joypad
  rts
  
; the following lines don't do anything useful
; besides stocking up the file to 512 bytes in size

		  seg	data
		  org	$71ff

end dc.b #$FF
