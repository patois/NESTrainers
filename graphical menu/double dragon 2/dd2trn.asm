; double dragon 2 +2 trainer for the NES
;
; How I made this trainer:
; I had very little information on trainers for the NES.
; Unluckily, I haven't seen any trainers for the NES "in the wild",
; nor any assembly/disassembly yet.
; The things I knew (taken from nestech.txt):
; 1. if a 512 byte trainer is present, it precedes the PRG ROM
;    pages right after the iNES header (currently at offset 0x10).
; 2. the presence of a trainer is indicated by bit 2 of offset 0x6
;    in the iNES header being set.
; 3. a 512 byte trainer is mapped to 0x7000 of the NES' RAM
;
; My idea was to patch the NMI vector to point at my trainer, since
; the NMI vector routine is called multiple times a second and therefore
; is very suitable for patching memory locations. The trainer routine would
; then jump back to the original nmi vector routine, after having done the patchwork.
; And that's the way I did it and that's the way it worked.
;
; So I began coding this ugly beast and relatively soon had a working
; trainer - without any menu yet, but the "patch engine" worked. The
; menu was a bit more of hard work, until I got it running somehow.
; One problem was to display any characters, since the trainer is
; very limited in its size and hardly can hold all the gfx.
; Finally I had the idea to create my own CHR ROM holding
; my own graphics, instead of trying to use existing graphics located
; within the original CHR ROMs of the game.
; So I've created my own CHR ROM with a program called "NES CHR-ROM Creator"
; by hand-drawing the alphabet ;P O.K., I could have used existing, better
; ones, but my ugly hand-drawn alphabet has got some nice, unique style :)
; Then I appended my custom CHR-ROM to the ROM image (font.chr) of the game and
; played around a bit with the mapper functions of the MMC3 mapper, until I was
; able to page in my CHR ROM and display its characters.
;
; The user is now able to switch between the two options (SELECT), en- or
; disable options (A) and run the game (START).
; Please note that the menu has some bugs.
; I hope you understand the sourcecode and find it useful, I'd be happy
; to see other trainers, I'm sure this stuff can be done much better.
;
; How this trainer works:
; 1. take control of the NMI vector
; 2. save all registers
; 3. if game is already running, go to 10
; 4. if nmi is executed for the first time, swap CHR banks
; 5. display trainer menu
; 6. handle joypad input, if user presses start,
;    swap original CHR banks and run game
; 7. set flags (enable/disable) options
; 8. restore all registers
; 9. return (via rti!)
; ---------------------------------
;10. patch memory locations
;11. restore all registers
;12. return (via jmp to original nmi vector!)
;
; how to build the trainer:
; 1. save the iNES header (the first 0x10/16 bytes of
;    the ROM image) to an external file (name it header.bin)
; 2. open header.bin with a hexeditor, increment the byte at offset 0x5,
;    OR the byte at offset 0x6 with 0x4 and save the file   
; 3. open the ROM image of double dragon 2 with a hexeditor, remove the
;    iNES header from the ROM image (delete the first 0x10/16 bytes), then go
;    to the file offset where the NMI vector's address is located. This is a word
;    (=2 bytes) at the end of the PRG ROM page loaded at 0xC000, which depends
;    on the mapper in use. For double dragon 2, it's the last PRG ROM page, which
;    is located at file offset 0x4000 (size of one PRG ROM page) * 0x8 (number of
;    PRG ROM pages) - 6 (negative offset of nmi vector) = 0x1FFFA (note down the original
;    value, since your trainer must return control to it) and patch it with the
;    new entrypoint 0x7000 (which is the address of where your trainer is being mapped to).
;    Don't forget about the little-endianness of the m6502 (0x7000 becomes 0x0070 !).
;    Finally, save the file (as game.bin).
; 4. assemble this sourcecode with: dasm dd2trn.asm -f3 (the result
;    will be a file named "a.out"
; 5. have your custom CHR ROM ready (or take the one coming along with
;    this sourcecode (font.chr).
; 6. run a command shell, change the directory to the location of
;    all these files and type:
;    "copy header.bin /b + a.out /b + game.bin /b + font.chr /b dd2_plus_2.nes
; 7. et voila, you got a working trainer

        processor   6502
		  
        seg         code
        org         $7000

; this is the original NMI vector routine address,
; taken from the ROM image
OLD_NMI_VEC         equ $FF50
first_joypad        equ $4016
base                equ $400
patch_struc_size    equ #3

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
  
; if(first_run)  
  ldx first_run
  beq inited

; set first_run = false  
  dex
  stx first_run

; swap in custom CHR ROM page (appended font.chr)
; double dragon 2 uses memory mapper 4 / MMC3
; here, our custom CHR ROM page is swapped in
	lda #$00  ; swap pages
	sta $8000
	
	lda #$80
	sta $8001

  jmp draw ; draw trainer menu

inited:
; if(is_game_running)
  lda is_game_running ; game running ?
  beq screenoff ; if not, switch off screen, redraw menu and poll buttons
  jmp patch_init ; patch memory locations


screenoff:
; set PPU flags
  lda #%00010000     
	sta $2000  
	;screen off        
	lda #%00000000 
	sta $2001

; init joypad 1
  lda #$01	; strobe joypad
  sta first_joypad
  lda #$00
  sta first_joypad

; collect buttonpresses for a,b,select,start
  ldy #$04
nextbutton:
  lda $4016
  and #$01

  sty temp
rotate:
  asl
  dey
  bne rotate
  ldy temp
  
  ora button
  sta button
  dey
  bne nextbutton

; check if button x is pressed
  ;lda button
  bit button_a
  bne toggle_option
  bit button_select
  bne switch_active_option
  bit button_start
  bne restore_page

; restore registers from stack, return to caller
return_rti:
	;nmi on, screen on
	lda #%10000000     
	sta $2000          
	lda #%00001110 
	sta $2001

  pla ; restore y
  tay
  pla ; restore x
  tax
  pla ; restore a
  plp ; restore p
  rti

; restore registers from stack, return to original nmi vector
return_nmi:
  pla ; restore y
  tay
  pla ; restore x
  tax
  pla ; restore a
  plp ; restore p

  jmp OLD_NMI_VEC ; jump back to original NMI routine

; selects either option 1 and 2,
; this trainer only has 2 options
switch_active_option:
  lda #$01
  eor pos
  sta pos
  jmp draw

; toggle option on or off
toggle_option:
  lda pos
  bne toggle_p2lifes
  lda #$1
  eor opt_p1_lifes
  sta opt_p1_lifes
  jmp end_toggle
toggle_p2lifes:
  lda #$1
  eor opt_p2_lifes
  sta opt_p2_lifes
end_toggle:
  jmp draw

; swaps pages that are originally loaded
; by mapper MMC3
restore_page:
 	ldx #$00
	stx $8000
	
; is_game_running = true
	inx
	sta is_game_running
	
	ldx #$80
	stx $8001
	
; patches various memory locations
; 50(pal)/60(ntsc) times a second
; see patches "structure"	below
patch_init:
  ldy #$0
  
patch_next:
  lda patches,y ; index for enabled flag
  beq skip_patch ; skip if disabled

  tya ; save y
  pha
  
  iny ; ptr to address
  lda patches,y ; a = zero page address
  tax ; x = offset
  iny ; y = ptr to value
  lda patches,y ; a = value
  sta base,x ; patch value at base + offset
  
  pla ; restore y
  tay
  
skip_patch:
  iny
  iny
  iny
  cpy patch_count
  bmi patch_next
  jmp return_nmi
	
; draws the trainer menu onto the screen
draw:
	lda #%00010000     
	sta $2000  
	;screen off        
	lda #%00000000 
	sta $2001

; init palette each time, it may be overwritten by the game
  lda #$3F        ;set ppu to start of palette
  sta $2006       
  lda #$00        
  sta $2006

	ldx #$40
loadpal:
  txa
	sta $2007
	inx
	cpx #$20
	bne loadpal

; begin drawing at line 3
  lda #$20 ;set ppu to VRAM
  sta $2006       
  lda #$60     
  sta $2006

; draw caption
  ldy #$FF
a:  
  iny
  ldx caption,y
  stx $2007
  bne a

; line 5
  lda #$20 ;set ppu to VRAM
  sta $2006       
  lda #$A0     
  sta $2006

  ldy #$FF
b:  
  iny
  ldx p1lifes,y
  stx $2007
  bne b
 
  ldx opt_p1_lifes
  lda onoff,x
  sta $2007   


  ldx pos
  lda indic1,x
  sta $2007

; line 6     
  lda #$20 ;set ppu to VRAM  
  sta $2006       
  lda #$C0     
  sta $2006

  ldy #$FF
c:  
  iny
  ldx p2lifes,y
  stx $2007
  bne c
   
  ldx opt_p2_lifes
  lda onoff,x
  sta $2007   
        
  ldx pos
  lda indic2,x
  sta $2007

; reset ppu stuff      
  lda #$00
  sta $2006       
  sta $2006
  sta $2005
  sta $2005

; kill time if a button was pressed
  lda button
  beq jmpreturn
  lda #$00
  sta button

  jsr killtime  

jmpreturn:
  jmp return_rti

; routine was taken from "Programming that 8-bit beast of power, the NES" by joker21
killtime:
	LDY #$06
	LDX #$10
.1      JSR .2
	DEX
	BNE .1
	DEY
	BNE .1
.2	RTS

; patch structure:
; offset 0 : bool enabled (0/1)
; offset 1 : offset (+base = address) to patch
; offset 2 : unsigned char - value to patch
patches:
opt_p1_lifes dc.b #$00 ; enabled (0/1)?
loc_p1_lifes dc.b #$32 ; address to patch
val_p1_lifes dc.b #$04 ; value to patch

opt_p2_lifes dc.b #$00 ; enabled (0/1)?
loc_p2_lifes dc.b #$33 ; address to patch
val_p2_lifes dc.b #$04 ; value to patch
patches_end:  

; number of patches
patch_count dc.b #$02 * patch_struc_size ; number of patches
  
; bool  
is_game_running dc.b  #$00
; bool
first_run       dc.b #$01

; button (see below) will hold
; a combination of these flags
button_a      dc.b #%00010000
button_select dc.b #%00000100
button_start  dc.b #%00000010

; combination of flags (button_a, button_select, button_start)
button dc.b 0

; temporary byte
temp dc.b 0

pos dc.b 0
indic1 dc.b #42,#0
indic2 dc.b #0,#42

onoff     dc.b "#$"
caption   dc.b "  dd2 +2 trainer",0
p1lifes   dc.b "  p1 unlimited lifes ",0
p2lifes   dc.b "  p2 unlimited lifes ",0


; the following lines don't do anything useful
; besides stocking up the file to 512 bytes in size

		  seg	data
		  org	$71ff

end dc.b #$FF
