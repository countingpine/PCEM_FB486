
' "modoplist"
static shared oplist(&hFF) as String

	static shared As String cad
Declare Sub dumpregs() 
Sub printdebug()
	Dim As Long a,b,d,f,g,x,y,c
	Static cadaxveces As integer
	
'               fila    col
'If vram[((80*8)* 25 )+( 1 *8)]<>0 Then deb=2
'If mitemp=1 And vram[160]=Asc("U") Then mitemp=2
'If vram[160]=Asc("F") Then deb=2:mitemp=3'Print #5,vram[164]:mitemp=3
'If MultiKey(SC_A) Then mitemp=2
'vram[160]=66'Rnd*256
'If cs0>&h700000 Then Print Hex(cs0)

	'If (cs0+pc)=&h92d900 Then deb=2 '&h8624
	
	'If cs1=&h24B8 And pc=&h0800 Then deb=2
   ' If cs1=&h25e9 And pc=&h00e8 Then deb=4	
	'If pc=&hf5014 Then deb=2''if deb=0 Then deb=5 Else deb=2
	'If pc=&hf5d21 Then 'if deb=0 Then deb=5 Else deb=2
	'If ESI=&hd8e921eb Then deb=2

	'If (CS1*16+pc)=&hc0100 Then deb=2
	'If vram[&h28c]=&h34 Then deb=2
	'If pc=&heac1 Then deb=2	 ' se queda por aqui, tras hacer el test de ram en amibios

	
	'cadaxveces+=1
	'If cadaxveces>40 Then 		

	if deb=1 Or deb=2 Then
		While InKey<>"":Wend ' necesario, sino, se salta una linea la primera vez que entra
		'a=CsrLin
		'b=Pos
		'If a>31 Then Cls:a=1:b=1 
		Locate 30,1
		Color RGB(255,0,0)  :print "CS:PC="+hex(cs0,8)+":"+hex(pc,8)+" -> "+Hex(cs0+pc,8)+"="+hex(opcode+use32,4)+" "+Hex(rmdat,4)+" "+ mid(oplist(opcode), 3);"                    "
		Color RGB(128,0,128):print "EAX=" + hex(EAX, 8) + "  EBX=" + hex(EBX, 8) + "  ECX=" + hex(ECX, 8) + "  EDX=" + Hex(EDX, 8)
		Color RGB(255,128,0):print "ESP=" + hex(ESP, 8) + "  EBP=" + hex(EBP, 8) + "  ESI=" + hex(ESI, 8) + "  EDI=" + Hex(EDI, 8)
		Color RGB(255,255,0):print "SS0="+Hex(ss0,8);
		Color RGB(0,70,255) :Print "    SS=" + hex(SS1, 4) + "   CS=" + hex(CS1, 4) + "   DS=" + hex(DS1, 4) + "   ES=" + Hex(ES1, 4)
		Color RGB(0,255,0)  :Print "OP32:";Hex(op32,4);" USE32:";Hex(use32,4);" VM86:";IIf(eflags And VM_FLAG,"SI","NO");" PROT:";IIf(modoprotegido,"SI","NO");"  PILA=";IIf(stack32,"32:","16:");Hex(ss0+ESP,8)
		' FLAGS
		Color RGB(0,255,255):print "----VDITNZ-A-P-C"'  V:OVERFLOW D:DIR I:INT S:SIGN Z:ZERO A:AuxCarry P:PARITY C:CARRY"
		print Bin(flags,16);" = ";hex(flags,4)
		Color RGB(128,128,128)
		
		If stack32 Then 
		  d=0
			For f=0 To 5 ' pila 32, en este caso, "ss0" se suma a "pelo", sin multiplicar por 16
				Locate 31+f,77
				print Hex(leeram2((ss0+ESP)+d),4);"-"; Hex(leeram2((ss0+ESP)+(d+2)),4);
				d+=4
			Next
		Else
		  d=0
			For f=0 To 5 ' pila 16, aqui, "ss0" se multiplica por 16 y se suma
				Locate 31+f,77
				print Hex(leeram2((ss0+SP)+d),4)';"-"; Hex(leeram2((ss0*16+SP)+(d+2)),4);
				d+=2
			Next
		End If

		'dumpregs()
	   'if deb>1 Then Sleep
	   'Locate a,b
	   Color RGB(128,128,128)
		screencopy
	EndIf

	'If cadaxveces=234 Then
	'	If vram[1928]=84 Then
	'		Print "hola"
	'		Sleep
	'	EndIf
	'EndIf

	'If cadaxveces=123 Then
	'	If vram[1928]=32 Then
	'		cadaxveces=234
	'	EndIf
	'EndIf

	'If vram[1928]=84 Then
	'	cadaxveces=123
	'EndIf
	'Dim As Integer x,y,c,g
	'cadaxveces+=1
	If cadaxveces>3000 Then 
		cadaxveces=0
	
		 'cad=""
		 Locate 20,1
		 For g=0 To 200*8 Step 8
		 		x=vram[&h0+g]
		 		Print IIf(x>31,Chr(x),".");
		 		'If x>31 Then cad=cad+Chr(x)
		 Next	
		 'If InStr(cad,"Invalid") Then deb=2
		
		 'For x=0 To 479
		 '	For y= 0 To 639
		 '		If ram[&h00+((x*640)+y)] Then PSet (y,x),RGB(255,255,255) Else PSet (y,x),RGB(0,0,0)
		 '	Next
		 'Next

 		'x=0:y=0
		'For f=0 To ((640/8)*480)-1
		'	c=vram[f]
		'	For g=0 To 7	 
		'	 PSet (x+(7-g),y),IIf(c And (2^g),65535*342,65535*1594544)
		'	Next g
		'	x+=8
		'	If x>639 Then y+=1:x=0
		'Next f
		
	EndIf
	

	
	''if deb=3 Then ' directo a fichero
	    'Open "depuracion.txt" For Append As 1
		'print #1, "----------------------------"
		'print #1, "CS:IP -> " + hex(savecs,4) + ":" + hex(saveip,4) + " -> " + Hex(savecs*16+saveip,8) + "  " + hex(opcode,2)+" "+ mid$(oplist(opcode), 3);"      "
		'print #1, "  ax = " + hex(getreg16(regax), 4) + "      bx = " + hex(getreg16(regbx), 4) + "      cx = " + hex(getreg16(regcx), 4) + "      dx = " + hex(getreg16(regdx), 4)
		'print #1, "  cs = " + hex(regcs, 4) + "      ss = " + hex(regss, 4) + "      di = " + hex(regdi, 4) + "      bp = " + hex(regbp, 4)
		'print #1, "  ds = " + hex(regds, 4) + "      es = " + hex(reges, 4) + "      si = " + hex(regsi, 4) + "      sp = " + hex(regsp, 4)
		'print #1, "  flags: cf=" + hex(cf) + "    zf=" + hex(zf) + "    sf=" + hex(sf) + "    of=" + hex(of) + "    if=" + hex(ifl) + "    af=" + hex(af) + "    df=" + hex(df) + "    pf=" + hex(pf)
		'Close 1
	'EndIf

	
	' mostrar una zona de ram, muy lento, intentar no usar
	'a=81:b=1
	'For f=0 To 250
	'	Locate b,a:print Hex(ram(f),2)
	'	a+=3:If a>100 Then a=81:b+=1
	'Next
	

	
	'Locate 5,1
end sub




Sub dumpregs() 

        Dim As Integer c,d=0,e=0 

        nopageerrors=1 
        Print "EAX      EBX      ECX      EDX      EDI      ESI      EBP      ESP     "
        Print Hex(EAX,8);" ";Hex(EBX,8);" ";Hex(ECX,8);" ";Hex(EDX,8);" ";Hex(EDI,8);" ";Hex(ESI,8);" ";Hex(EBP,8);" ";Hex(ESP,8) 
        print "-"
        print "PC   CS   DS   ES   SS   FLAGS   "
        print Hex(pc,4);" ";Hex(CS1,4);" ";Hex(DS1,4);" ";Hex(ES1,4);" ";Hex(SS1,4);" ";Hex(flags,4)
        print "-"
        print "CS1:";Hex(oldcs,4);"  PC1:";Hex(oldpc,4)
        print "-"
        Print "En modo ",IIf(modoprotegido,iif((eflags And VM_FLAG),"V86","protected"),"real") 
        print "-"
        print "CS   base/limit/access ",Hex(cs0,6);" ";Hex(_cs.limit,4);" ";Hex(_cs.access0,2)
        print "DS   base/limit/access ",Hex(ds0,6);" ";Hex(_ds.limit,4);" ";Hex(_ds.access0,2)
        print "ES   base/limit/access ",Hex(es0,6);" ";Hex(_es.limit,4);" ";Hex(_es.access0,2)
        Print " registros solo 386/486:"
        Print "FS   base/limit/access ",Hex(fs0,6);" ";Hex(_fs.limit,4);" ";Hex(_fs.access0,2)
        Print "GS   base/limit/access ",Hex(gs0,6);" ";Hex(_gs.limit,4);" ";Hex(_gs.access0,2)
        print "-"
        print "SS    base/limit/access ",Hex(ss0,6);" ";Hex(_ss.limit,6);" ";Hex(_ss.access0,4) 
        print "GDT   base/limit ",Hex(gdt.base0,6);" ";Hex(gdt.limit,4) 
        print "LDT   base/limit ",Hex(ldt.base0,6);" ";Hex(ldt.limit,4) 
        print "IDT   base/limit ",Hex(idt.base0,6);" ";Hex(idt.limit,4) 
        print "TR    base/limit ",Hex(tr.base0, 6);" ";Hex(tr.limit ,4) 
        print "-"
        Print "386 en modo ";IIf(use32,"32-bit","16-bit");", pila en modo "; IIf(stack32,"32-bit","16-bit")
        print "CR0=";Hex(cr0,8);" CR2=";Hex(cr2,8);" CR3=";Hex(cr3,8)
        print "-"

'        	  print "Entries in readlookup:";readlnum;" writelookup: ";writelnum 
	        for  c=0 To (1024*1024)-1
	                if (readlookup2[c]<>&hFFFFFFFF) Then d+=1 
	                if (writelookup2[c]<>&hFFFFFFFF) Then e+=1 
	        Next
	        print "Entries in readlookup:";d;" writelookup: ";e 

        d=0 
        'x87_dumpregs() 

End Sub





Sub initoplist()
oplist(&h0 ) = "00 add eb gb"
oplist(&h1 ) = "01 add ev gv"
oplist(&h2 ) = "02 add gb eb"
oplist(&h3 ) = "03 add gv ev"
oplist(&h4 ) = "04 add al ib"
oplist(&h5 ) = "05 add eax iv"
oplist(&h6 ) = "06 push es"
oplist(&h7 ) = "07 pop es"
oplist(&h8 ) = "08 or eb gb"
oplist(&h9 ) = "09 or ev gv"
oplist(&ha ) = "0a or gb eb"
oplist(&hb ) = "0b or gv ev"
oplist(&hc ) = "0c or al ib"
oplist(&hd ) = "0d or eax iv"
oplist(&he ) = "0e push cs"
oplist(&hf ) = "0f -- floating point or 286+ op"
oplist(&h10) = "10 adc eb gb"
oplist(&h11) = "11 adc ev gv"
oplist(&h12) = "12 adc gb eb"
oplist(&h13) = "13 adc gv ev"
oplist(&h14) = "14 adc al ib"
oplist(&h15) = "15 adc eax iv"
oplist(&h16) = "16 push ss"
oplist(&h17) = "17 pop ss"
oplist(&h18) = "18 sbb eb gb"
oplist(&h19) = "19 sbb ev gv"
oplist(&h1a) = "1a sbb gb eb"
oplist(&h1b) = "1b sbb gv ev"
oplist(&h1c) = "1c sbb al ib"
oplist(&h1d) = "1d sbb eax iv"
oplist(&h1e) = "1e push ds"
oplist(&h1f) = "1f pop ds"
oplist(&h20) = "20 and eb gb"
oplist(&h21) = "21 and ev gv"
oplist(&h22) = "22 and gb eb"
oplist(&h23) = "23 and gv ev"
oplist(&h24) = "24 and al ib"
oplist(&h25) = "25 and eax iv"
oplist(&h26) = "26 es:"
oplist(&h27) = "27 daa"
oplist(&h28) = "28 sub eb gb"
oplist(&h29) = "29 sub ev gv"
oplist(&h2a) = "2a sub gb eb"
oplist(&h2b) = "2b sub gv ev"
oplist(&h2c) = "2c sub al ib"
oplist(&h2d) = "2d sub eax iv"
oplist(&h2e) = "2e cs:"
oplist(&h2f) = "2f das"
oplist(&h30) = "30 xor eb gb"
oplist(&h31) = "31 xor ev gv"
oplist(&h32) = "32 xor gb eb"
oplist(&h33) = "33 xor gv ev"
oplist(&h34) = "34 xor al ib"
oplist(&h35) = "35 xor eax iv"
oplist(&h36) = "36 ss:"
oplist(&h37) = "37 aaa"
oplist(&h38) = "38 cmp eb gb"
oplist(&h39) = "39 cmp ev gv"
oplist(&h3a) = "3a cmp gb eb"
oplist(&h3b) = "3b cmp gv ev"
oplist(&h3c) = "3c cmp al ib"
oplist(&h3d) = "3d cmp eax iv"
oplist(&h3e) = "3e ds:"
oplist(&h3f) = "3f aas"
oplist(&h40) = "40 inc eax"
oplist(&h41) = "41 inc ecx"
oplist(&h42) = "42 inc edx"
oplist(&h43) = "43 inc ebx"
oplist(&h44) = "44 inc esp"
oplist(&h45) = "45 inc ebp"
oplist(&h46) = "46 inc esi"
oplist(&h47) = "47 inc edi"
oplist(&h48) = "48 dec eax"
oplist(&h49) = "49 dec ecx"
oplist(&h4a) = "4a dec edx"
oplist(&h4b) = "4b dec ebx"
oplist(&h4c) = "4c dec esp"
oplist(&h4d) = "4d dec ebp"
oplist(&h4e) = "4e dec esi"
oplist(&h4f) = "4f dec edi"
oplist(&h50) = "50 push eax"
oplist(&h51) = "51 push ecx"
oplist(&h52) = "52 push edx"
oplist(&h53) = "53 push ebx"
oplist(&h54) = "54 push esp"
oplist(&h55) = "55 push ebp"
oplist(&h56) = "56 push esi"
oplist(&h57) = "57 push edi"
oplist(&h58) = "58 pop eax"
oplist(&h59) = "59 pop ecx"
oplist(&h5a) = "5a pop edx"
oplist(&h5b) = "5b pop ebx"
oplist(&h5c) = "5c pop esp"
oplist(&h5d) = "5d pop ebp"
oplist(&h5e) = "5e pop esi"
oplist(&h5f) = "5f pop edi"
oplist(&h70) = "70 jo jb"
oplist(&h71) = "71 jno jb"
oplist(&h72) = "72 jb jb"
oplist(&h73) = "73 jnb jb"
oplist(&h74) = "74 jz jb"
oplist(&h75) = "75 jnz jb"
oplist(&h76) = "76 jbe jb"
oplist(&h77) = "77 ja jb"
oplist(&h78) = "78 js jb"
oplist(&h79) = "79 jns jb"
oplist(&h7a) = "7a jpe jb"
oplist(&h7b) = "7b jpo jb"
oplist(&h7c) = "7c jl jb"
oplist(&h7d) = "7d jge jb"
oplist(&h7e) = "7e jle jb"
oplist(&h7f) = "7f jg jb"
oplist(&h80) = "80 grp1 eb ib"
oplist(&h81) = "81 grp1 ev iv"
oplist(&h82) = "82 grp1 eb ib"
oplist(&h83) = "83 grp1 ev ib"
oplist(&h84) = "84 test gb eb"
oplist(&h85) = "85 test gv ev"
oplist(&h86) = "86 xchg gb eb"
oplist(&h87) = "87 xchg gv ev"
oplist(&h88) = "88 mov eb gb"
oplist(&h89) = "89 mov ev gv"
oplist(&h8a) = "8a mov gb eb"
oplist(&h8b) = "8b mov gv ev"
oplist(&h8c) = "8c mov ew sw"
oplist(&h8d) = "8d lea gv m"
oplist(&h8e) = "8e mov sw ew"
oplist(&h8f) = "8f pop ev"
oplist(&h90) = "90 nop"
oplist(&h91) = "91 xchg ecx eax"
oplist(&h92) = "92 xchg edx eax"
oplist(&h93) = "93 xchg ebx eax"
oplist(&h94) = "94 xchg esp eax"
oplist(&h95) = "95 xchg ebp eax"
oplist(&h96) = "96 xchg esi eax"
oplist(&h97) = "97 xchg edi eax"
oplist(&h98) = "98 cbw"
oplist(&h99) = "99 cwd"
oplist(&h9a) = "9a call ap"
oplist(&h9b) = "9b wait"
oplist(&h9c) = "9c pushf"
oplist(&h9d) = "9d popf"
oplist(&h9e) = "9e sahf"
oplist(&h9f) = "9f lahf"
oplist(&ha0) = "a0 mov al ob"
oplist(&ha1) = "a1 mov eax ov"
oplist(&ha2) = "a2 mov ob al"
oplist(&ha3) = "a3 mov ov eax"
oplist(&ha4) = "a4 movsb"
oplist(&ha5) = "a5 movsw"
oplist(&ha6) = "a6 cmpsb"
oplist(&ha7) = "a7 cmpsw"
oplist(&ha8) = "a8 test al ib"
oplist(&ha9) = "a9 test eax iv"
oplist(&haa) = "aa stosb"
oplist(&hab) = "ab stosw"
oplist(&hac) = "ac lodsb"
oplist(&had) = "ad lodsw"
oplist(&hae) = "ae scasb"
oplist(&haf) = "af scasw"
oplist(&hb0) = "b0 mov al ib"
oplist(&hb1) = "b1 mov cl ib"
oplist(&hb2) = "b2 mov dl ib"
oplist(&hb3) = "b3 mov bl ib"
oplist(&hb4) = "b4 mov ah ib"
oplist(&hb5) = "b5 mov ch ib"
oplist(&hb6) = "b6 mov dh ib"
oplist(&hb7) = "b7 mov bh ib"
oplist(&hb8) = "b8 mov eax iv"
oplist(&hb9) = "b9 mov ecx iv"
oplist(&hba) = "ba mov edx iv"
oplist(&hbb) = "bb mov ebx iv"
oplist(&hbc) = "bc mov esp iv"
oplist(&hbd) = "bd mov ebp iv"
oplist(&hbe) = "be mov esi iv"
oplist(&hbf) = "bf mov edi iv"
oplist(&hc2) = "c2 ret iw"
oplist(&hc3) = "c3 ret"
oplist(&hc4) = "c4 les gv mp"
oplist(&hc5) = "c5 lds gv mp"
oplist(&hc6) = "c6 mov eb ib"
oplist(&hc7) = "c7 mov ev iv"
oplist(&hca) = "ca retf iw"
oplist(&hcb) = "cb retf"
oplist(&hcc) = "cc int 3"
oplist(&hcd) = "cd int ib"
oplist(&hce) = "ce into"
oplist(&hcf) = "cf iret"
oplist(&hd0) = "d0 grp2 eb 1"
oplist(&hd1) = "d1 grp2 ev 1"
oplist(&hd2) = "d2 grp2 eb cl"
oplist(&hd3) = "d3 grp2 ev cl"
oplist(&hd4) = "d4 aam i0"
oplist(&hd5) = "d5 aad i0"
oplist(&hd7) = "d7 xlat"
oplist(&he0) = "e0 loopnz jb"
oplist(&he1) = "e1 loopz jb"
oplist(&he2) = "e2 loop jb"
oplist(&he3) = "e3 jcxz jb"
oplist(&he4) = "e4 in al ib"
oplist(&he5) = "e5 in eax ib"
oplist(&he6) = "e6 out ib al"
oplist(&he7) = "e7 out ib eax"
oplist(&he8) = "e8 call jv"
oplist(&he9) = "e9 jmp jv"
oplist(&hea) = "ea jmp ap"
oplist(&heb) = "eb jmp jb"
oplist(&hec) = "ec in al dx"
oplist(&hed) = "ed in eax dx"
oplist(&hee) = "ee out dx al"
oplist(&hef) = "ef out dx eax"
oplist(&hf0) = "f0 lock"
oplist(&hf2) = "f2 repnz"
oplist(&hf3) = "f3 repz"
oplist(&hf4) = "f4 hlt"
oplist(&hf5) = "f5 cmc"
oplist(&hf6) = "f6 grp3a eb"
oplist(&hf7) = "f7 grp3b ev"
oplist(&hf8) = "f8 clc"
oplist(&hf9) = "f9 stc"
oplist(&hfa) = "fa cli"
oplist(&hfb) = "fb sti"
oplist(&hfc) = "fc cld"
oplist(&hfd) = "fd std"
oplist(&hfe) = "fe grp4 eb"
oplist(&hff) = "ff grp5 ev"
end sub

