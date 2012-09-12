; goonies 2 trainer for the NES
;
; this is a very simple trainer. my second one written for the
; NES. it doesn't have a graphical interface/menu. in order to
; activate the trainer hold the start button on joy 1 on bootup.

        processor   6502
		  
        seg         code
        org         $7000

; this is the original NMI vector routine address,
; taken from the ROM image
OLD_NMI_VEC         equ $C07C
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
  

; if(is_game_running)
  lda is_game_running ; game running ?
  bne patch_init

; init joypad 1
  lda #$01	; strobe joypad
  sta first_joypad
  lda #$00
  sta first_joypad

  lda $4016 ;a
  lda $4016 ;b
  lda $4016 ;select
  lda $4016 ;start
  and #$01
  bne cheaton

; is_game_running = true
	lda #$01
	sta is_game_running


return_nmi:
  pla ; restore y
  tay
  pla ; restore x
  tax
  pla ; restore a
  plp ; restore p

  jmp OLD_NMI_VEC ; jump back to original NMI routine

	
cheaton:
  lda #$01
  sta enabled
	
	
; patches various memory locations
; 50(pal)/60(ntsc) times a second

patch_init:
  lda enabled
  beq return_nmi

; number of lifes
  lda #$03
  ldx #$22 ; zeropage offset for numlifes
  sta $0,x

; number of keys
  ldx #$00
  sta $500,x

; number of bombs
  inx
  sta $500,x
  
; number of molotows
  inx
  sta $500,x
  
; energy
  inx
  lda #$20
  sta $500,x
  
; shoes?
  ldx #$09
  lda #$FF
  sta $500,x
  
; items?
  inx
  sta $500,x
  
; weapons?
  inx
  sta $500,x
  
; number of stones for slingshot
  ldx #$0E
  lda #$05
  sta $500,x

  jmp return_nmi


enabled dc.b 0
is_game_running dc.b 0

; the following lines don't do anything useful
; besides stocking up the file to 512 bytes in size

		  seg	data
		  org	$71ff

end dc.b #$FF
