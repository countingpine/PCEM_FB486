

' SET EA
Sub seteab(v As Ubyte) 
	if (modo<>3) Then 
		if (eal_w) Then 
			*eal_w=v
			Print #5,"SETEAB revisar"
			Exit sub 
		else 
			writememb_386l(easeg,eaaddr,v) ' va directo a escritura, sin pasar por cache (por writememb_386)
			Exit sub
		EndIf
	Else
		If (rm And 4) Then 
			regs(rm And 3).hb=v
		Else
			regs(rm).lb=v
		EndIf
	EndIf
End Sub

sub seteaw(v As UShort) 
	if (modo<>3) Then
		if (eal_w) Then 
			*eal_w=v 
			Print #5,"SETEAW revisar"
		else 
			writememw_386l(easeg,eaaddr,v) ' va directo a escritura, sin pasar por cache (por writememw_386)
		EndIf
	else 
		regs(rm).w=v
   EndIf
End Sub

sub seteal(v As ULong)
	if (modo<>3) Then 
		If (eal_w) Then 
			*eal_w=v
			Print #5,"SETEAL revisar"
		Else 
			writememl_386l(easeg,eaaddr,v) ' va directo a escritura, sin pasar por cache (por writememl_386)
		EndIf
	Else
		regs(rm).l=v
	EndIf
End Sub



' GET EA
Function geteab() As UByte 
        if (modo=3) Then return IIf((rm And 4),regs(rm And 3).hb,regs(rm And 3).lb)
        If (eal_r) Then Locate 1,1:Print "por fin GETEAB:";CUByte(*eal_r):sleep
        If (eal_r) Then return CUByte(*eal_r) 
        return readmemb_386(easeg,eaaddr) ' pasa por cache primero
End Function

Function geteaw() As UShort 
        if (modo=3) Then Return regs(rm).w 
        If (eal_r) Then Locate 1,1:Print "por fin GETEAW:";CUShort(*eal_r):Sleep
        If (eal_r) Then Return CUShort(*eal_r) 
        return readmemw_386(easeg,eaaddr) ' pasa por cache primero
End Function

Function geteal() As uLong 
        if (modo=3) Then return regs(rm).l 
        If (eal_r) Then Locate 1,1:Print "por fin GETEAL:"; CULng(*eal_r):Sleep
        If (eal_r) Then Return culng(*eal_r) 
        Return readmeml_386(easeg,eaaddr) ' pasa por cache primero
End Function





Function getbytef() As UByte
	getbytef=CUByte(fetchdat)' And &hFF
	pc+=1
End Function

Function getwordf() As UShort
	getwordf=CUShort(fetchdat)' And &hFFFF
	pc+=2
End Function

Function getbyte2f() As UByte
	getbyte2f=CUByte(fetchdat Shr 8)' And &hFF
	pc+=1
End Function

Function getword2f() As UShort
	getword2f=CUShort(fetchdat Shr 8)' And &hFFFF
	pc+=2
End Function

Function getword() As UShort 
  pc+=2 
  return fastreadw(cs0+(pc-2)) 
End Function

Function getlong() As ULong 
  pc+=4 
  return fastreadl(cs0+(pc-4)) 
End Function







Sub setadd32(a As ULong , b As ULong ) 
        Dim As ULong c=a+b
        flags = flags And inv(&h8D5) 
        flags = flags Or iif(c And &h80000000,N_FLAG,iif(c=0,Z_FLAG,0)) 
        flags = flags Or (znptable8(c And &hFF) And P_FLAG) 
        if (c<a) Then flags = flags Or C_FLAG 
        if (((a Xor b) And &h80000000)=0) And (((a Xor c) And &h80000000)<>0) Then flags = flags Or V_FLAG 
        if (((a And &hF)+(b And &hF)) And &h10) Then flags = flags Or A_FLAG 
End Sub

Sub setadd32nc(a As ULong , b As ULong ) 
        Dim As ULong  c=a+b
        flags = flags And inv(&h8D4) 
        flags = flags Or iif(c And &h80000000,N_FLAG,IIf(c=0,Z_FLAG,0)) 
        flags = flags Or (znptable8(c And &hFF) And P_FLAG) 
        if (((a Xor b) And &h80000000)=0) And (((a Xor c) And &h80000000)<>0) Then flags = flags Or V_FLAG 
        if (((a And &hF)+(b And &hF)) And &h10) Then flags = flags Or A_FLAG 
End Sub

Sub setadc32(a As ULong , b As ULong ) 
        Dim As ULong  c=a+b+tempc 
        flags = flags And inv(&h8D5) 
        flags = flags Or iif(c And &h80000000,N_FLAG,IIf(c=0,Z_FLAG,0)) 
        flags = flags Or (znptable8(c And &hFF) And P_FLAG) 
        if (c<a)  Or  ((c=a)  And  (tempc<>0)) Then flags = flags Or C_FLAG 
        if (((a Xor b) And &h80000000)=0) And (((a Xor c) And &h80000000)<>0) Then flags = flags Or V_FLAG 
        if (((a And &hF)+(b And &hF)+tempc) And &h10) Then flags = flags Or A_FLAG 
End Sub

Sub setsub32(a As ULong , b As ULong ) 
        Dim As ULong  c=a-b
        flags = flags And inv(&h8D5) 
        flags = flags Or iif(c And &h80000000,N_FLAG,IIf(c=0,Z_FLAG,0)) 
        flags = flags Or (znptable8(c And &hFF) And P_FLAG) 
        if (c>a) Then flags = flags Or C_FLAG 
        if ((a Xor b) And (a Xor c) And &h80000000) Then flags = flags Or V_FLAG 
        if (((a And &hF)-(b And &hF)) And &h10) Then flags = flags Or A_FLAG 
End Sub

Sub setsub32nc(a As ULong , b As ULong ) 
        Dim As ULong  c=a-b
        flags = flags And inv(&h8D4) 
        flags = flags Or iif(c And &h80000000,N_FLAG,IIf(c=0,Z_FLAG,0)) 
        flags = flags Or (znptable8(c And &hFF) And P_FLAG) 
        if ((a Xor b) And (a Xor c) And &h80000000) Then flags = flags Or V_FLAG 
        if (((a And &hF)-(b And &hF)) And &h10) Then flags = flags Or A_FLAG 
End Sub

Sub setsbc32(a As ULong , b As ULong ) 
        Dim As ULong  c=a-(b+tempc)
        flags = flags And inv(&h8D5) 
        flags = flags Or iif(c And &h80000000,N_FLAG,iif(c=0,Z_FLAG,0)) 
        flags = flags Or (znptable8(c And &hFF) And P_FLAG) 
        if (c>a)  Or  ((c=a)  And  (tempc<>0)) Then flags = flags Or C_FLAG 
        if ((a Xor b) And (a Xor c) And &h80000000) Then flags = flags Or V_FLAG 
        if (((a And &hF)-((b And &hF)+tempc)) And &h10) Then flags = flags Or A_FLAG 
End Sub





' estos dos provienen de unos "#DEFINE" que tenia el modulo "x86.h"
Function getr8(r As Integer) As UByte
	Return IIf(r And 4,regs(r And 3).hb,regs(r And 3).lb)
End Function

Sub setr8(r As integer, v As UByte)
	If (r And 4) Then
		regs(r And 3).hb=v
	else 
		regs(r And 3).lb=v
	EndIf
End Sub




Sub setznp8(v As UByte ) ' 8bits
        flags = flags And inv(&h8D5)  ' la version 8086 usa C4 en vez de 8D5/
        flags = flags Or znptable8(v) 
End Sub

Sub setznp16(v As UShort ) ' 16 bits
        flags = flags And inv(&h8D5)  ' la version 8086 usa C4 en vez de 8D5
        flags = flags Or znptable16(v) 
End Sub

Sub setznp32(v As ULong ) ' 32 bits
        flags = flags And inv(&h8D5) 
        flags = flags Or IIf(v And &h80000000,N_FLAG,iif(v=0 ,Z_FLAG,0))
        flags = flags Or (znptable8(v And &hFF) And P_FLAG) 
End Sub






Sub setadd8(a As UByte , b As UByte ) 
        Dim As UShort  c=cushort(a)+cushort(b) 
        flags = flags And inv(&h8D5) 
        flags = flags Or znptable8(c And &hFF) 
        if (c And &h100) Then flags = flags Or C_FLAG 
        if (((a Xor b) And &h80)=0) And (((a Xor c) And &h80)<>0) Then flags = flags Or V_FLAG 
        if (((a And &hF)+(b And &hF)) And &h10) Then flags = flags Or A_FLAG 
End Sub

Sub setadd8nc(a As UByte , b As UByte ) 
        Dim As UShort  c=cushort(a)+cushort(b) 
        flags = flags And inv(&h8D4) 
        flags = flags Or znptable8(c And &hFF) 
        if (((a Xor b) And &h80)=0) And (((a Xor c) And &h80)<>0) Then flags = flags Or V_FLAG 
        if (((a And &hF)+(b And &hF)) And &h10) Then flags = flags Or A_FLAG 
End Sub

Sub setadc8(a As UByte , b As UByte ) 
        Dim As UShort  c=cushort(a)+cushort(b)+tempc 
        flags = flags And inv(&h8D5) 
        flags = flags Or znptable8(c And &hFF) 
        if (c And &h100) Then flags = flags Or C_FLAG 
        if (((a Xor b) And &h80)=0) And (((a Xor c) And &h80)<>0) Then flags = flags Or V_FLAG 
        if (((a And &hF)+(b And &hF)) And &h10) Then flags = flags Or A_FLAG 
End Sub

Sub setadd16(a As UShort , b As UShort ) 
        Dim As ULong  c=culng(a)+CULng(b) 
        flags = flags And inv(&h8D5) 
        flags = flags Or znptable16(c And &hFFFF) 
        if (c And &h10000) Then flags = flags Or C_FLAG 
        if (((a Xor b) And &h8000)=0) And (((a Xor c) And &h8000)<>0) Then flags = flags Or V_FLAG 
        if (((a And &hF)+(b And &hF)) And &h10) Then flags = flags Or A_FLAG 
End Sub

Sub setadd16nc(a As UShort , b As UShort ) 
        Dim As ULong  c=CULng(a)+CULng(b) 
        flags = flags And inv(&h8D4) 
        flags = flags Or znptable16(c And &hFFFF) 
        if (((a Xor b) And &h8000)=0) And (((a Xor c) And &h8000)<>0) Then flags = flags Or V_FLAG 
        if (((a And &hF)+(b And &hF)) And &h10) Then flags = flags Or A_FLAG 
End Sub

Sub setadc16(a As UShort , b As UShort ) 
        Dim As ULong  c=culng(a)+CULng(b)+tempc
        flags = flags And inv(&h8D5) 
        flags = flags Or znptable16(c And &hFFFF) 
        if (c And &h10000) Then flags = flags Or C_FLAG 
        if (((a Xor b) And &h8000)=0) And (((a Xor c) And &h8000)<>0) Then flags = flags Or V_FLAG 
        if (((a And &hF)+(b And &hF)) And &h10) Then flags = flags Or A_FLAG 
End Sub

Sub setsub8(a As UByte , b As UByte ) 
        Dim As UShort  c=cushort(a)-cushort(b) 
        flags = flags And inv(&h8D5) 
        flags = flags Or znptable8(c And &hFF) 
        if (c And &h100) Then flags = flags Or C_FLAG 
        If (((a Xor b) And (a Xor c)) And &h80) Then flags = flags Or V_FLAG
        if (((a And &hF)-(b And &hF)) And &h10) Then flags = flags Or A_FLAG 
End Sub

Sub setsub8nc(a As UByte , b As UByte ) 
        Dim As UShort  c=cushort(a)-cushort(b)
        flags = flags And inv(&h8D4) 
        flags = flags Or znptable8(c And &hFF) 
        If (((a Xor b) And (a Xor c)) And &h80) Then flags = flags Or V_FLAG
        if (((a And &hF)-(b And &hF)) And &h10) Then flags = flags Or A_FLAG 
End Sub

Sub setsbc8(a As UByte , b As UByte ) 
        Dim As UShort  c=CUShort(a)-(cushort(b)+tempc) 
        flags = flags And inv(&h8D5) 
        flags = flags Or znptable8(c And &hFF) 
        if (c And &h100) Then flags = flags Or C_FLAG 
        If (((a Xor b) And (a Xor c)) And &h80) Then flags = flags Or V_FLAG
        if (((a And &hF)-(b And &hF)) And &h10) Then flags = flags Or A_FLAG 
End Sub

Sub setsub16(a As UShort , b As UShort ) 
        Dim As ULong  c=culng(a)-CULng(b) 
        flags = flags And inv(&h8D5) 
        flags = flags Or znptable16(c And &hFFFF) 
        if (c And &h10000) Then flags = flags Or C_FLAG 
        If (((a Xor b) And (a Xor c)) And &h8000) Then flags = flags Or V_FLAG
        if (((a And &hF)-(b And &hF)) And &h10  ) Then flags = flags Or A_FLAG 
End Sub

Sub setsub16nc(a As UShort , b As UShort ) 
        Dim As ULong  c=culng(a)-CULng(b) 
        flags = flags And inv(&h8D4) 
        flags = flags Or (znptable16(c And &hFFFF) And inv(4)) 
        flags = flags Or (znptable8 (c And &hFF) And 4) 
        If (((a Xor b) And (a Xor c)) And &h8000) Then flags = flags Or V_FLAG
        if (((a And &hF)-(b And &hF)) And &h10  ) Then flags = flags Or A_FLAG 
End Sub

Sub setsbc16(a As UShort , b As UShort ) 
        Dim As Ulong  c=culng(a)-(CULng(b)+tempc) 
        flags = flags And inv(&h8D5) 
        flags = flags Or (znptable16(c And &hFFFF) And inv(4)) 
        flags = flags Or (znptable8 (c And &hFF) And 4) 
        if (c And &h10000) Then flags = flags Or C_FLAG 
        if (((a Xor b) And (a Xor c)) And &h8000) Then flags = flags Or V_FLAG
        if (((a And &hF)-(b And &hF)) And &h10  ) Then flags = flags Or A_FLAG 
End Sub




'''''''''''''''''''''''''''''''''''
' 
'''''''''''''''''''''''''''''''''''
sub divexcp()
       print #5,"Divide exception:";Hex(CS1),Hex(cs0),Hex(pc)
       ' nota:en la V8.1 TODO lo que sigue SOBRA, y solo emplea un simple "x86_int(0)" !!!!!
       pc=oldpc
       if modoprotegido Then 
       	pmodeint(0,0)
       else 
         writememw_386(ss0,(SP-2) And &hFFFF,flags)
         writememw_386(ss0,(SP-4) And &hFFFF,CS1)
         writememw_386(ss0,(SP-6) And &hFFFF,pc)
         SP-=6
         flags = flags And inv(I_FLAG)
         oxpc=pc
         pc=readmemw_386(0,0)
         loadcs(readmemw_386(0,2))
       EndIf
       ' aqui vendria un RETURN si fuera un DEFINE, pero al no serlo, tengo que ponerlo FUERA
End Sub

Sub divl(ByVal valor As ULong ) 
	'Print #5,"analizar DIVL" ' WINDOWS la emplea
        if (valor=0) Then divexcp() : Return
        Dim As ULongInt  num=(CULngInt(EDX) Shl 32) Or EAX        
        Dim As ULongInt  quo=(num \ valor)
        Dim As ULong  rem0=num Mod valor 
        Dim As ULong  quo32=CULng(quo And &hFFFFFFFF) 
        if (quo<>CuLngInt(quo32)) Then divexcp() : Return
        EDX=rem0 
        EAX=quo32 
End Sub

Sub idivl(ByVal valor As Integer ) 
	'Print #5,"analizar IDIVL" ' el WOLF3d la emplea
        if (valor=0) Then divexcp() : Return
        Dim As LongInt  num=CLngInt((CULngInt(EDX) Shl 32) Or EAX) 
        Dim As LongInt  quo=(num \ Valor)
        Dim As Long  rem0=num Mod Valor 
        Dim As Long  quo32=CLng(quo And &hFFFFFFFF) 
        if (quo<>CLngInt(CLng(quo32))) Then divexcp() : Return 
        EDX=rem0 
        EAX=quo32 
End Sub



#Include "386b.bas"