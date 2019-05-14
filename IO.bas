'Dim Shared As UByte  puertosb(&hffff) ' 1 byte
'Dim Shared As UShort puertosw(&hffff) ' 2 bytes
'Dim Shared As ULong  puertosl(&hffff) ' 4 bytes

'Dim Shared As ULong puertos(&hffff) ' 4 bytes

Declare Sub outb(port As UShort , valor As UByte ) ' 1 byte
Declare Sub outw(port As UShort , valor As UShort ) ' 2 bytes
Declare Sub outl(port As UShort , valor As ULong ) ' 4 bytes

Declare Function inb(port As UShort ) As UByte   ' 1 byte
Declare Function inw(port As UShort ) As UShort   ' 2 byte
Declare Function inl(port As UShort ) As ULong   ' 4 byte


Dim Shared As Integer nvraddr
Dim Shared As UByte nvrram(128)


Sub nvr_rtc() 
        Dim As Integer c 
        if ( (nvrram(&hA) And &hF)=0) Then 
                rtctime=99999999 
                return 
        EndIf
        c=1 Shl ((nvrram(&hA) And &hF)-1) 
        rtctime+=RTCCONST*csng(c) 
        nvrram(&hC)=&h40 
        If (nvrram(&hB) And &h40) Then 
            nvrram(&hC) or = &h80 
            picint(&h100)
        EndIf
End Sub

Sub getnvrtime()
        Dim As integer c,d
        Dim As UByte baknvr(10)
		  Dim As String hora, fecha
      
        hora=Time
        fecha=Date

        'print #5,hora,fecha
        'If (baknvr(0)<>nvrram(0) Or _
        '    baknvr(2)<>nvrram(2) Or _
        '    baknvr(4)<>nvrram(4) Or _
        '    baknvr(6)<>nvrram(6) Or _
        '    baknvr(7)<>nvrram(7) Or _
        '    baknvr(8)<>nvrram(8) or _
        '    baknvr(9)<>nvrram(9)) Then nvrram(&hA) or=&h80
            
        ' segundos
        d=Val(Mid(hora,7,2)) Mod 10
        c=(Val(Mid(hora,7,2))  \ 10)
        nvrram(0)=d Or (c Shl 4)
        ' minutos
        d=Val(Mid(hora,4,2)) Mod 10
        c=(Val(Mid(hora,4,2))  \ 10)
        nvrram(2)=d Or (c Shl 4)
        ' horas
        d=Val(Mid(hora,1,2)) Mod 10
        c=(Val(Mid(hora,1,2))  \ 10)
        nvrram(4)=d Or (c Shl 4)

        
        ' semana
        d=Val(Mid(fecha,1,2)) Mod 10
        c=(Val(Mid(fecha,1,2))  \ 10)
        nvrram(6)=d Or (c Shl 4) ' este no es asi, pero no tengo forma de coger "semana" por ahora.
        ' dia
        d=Val(Mid(fecha,1,2)) Mod 10
        c=(Val(Mid(fecha,1,2))  \ 10)
        nvrram(7)=d Or (c Shl 4)
        ' mes
        d=Val(Mid(fecha,4,2)) Mod 10
        c=(Val(Mid(fecha,4,2))  \ 10)
        nvrram(8)=d Or (c Shl 4)
        ' ano/culo
        d=Val(Mid(fecha,7,2)) Mod 10
        c=(Val(Mid(fecha,7,2))  \ 10)
        nvrram(9)=d Or (c Shl 4)


		  ' reset reloj? si, pero no se debe resetear siempre, solo al cambairlo
 		  'nvrram(&hA) or=&h80
End Sub


Sub write_NVR(addr As UShort, valor As UByte)
	'print #5,"WRITE NVR:";Hex(addr,4),Hex(valor,2)
	'deb=2
        Dim As Integer c 
        if (addr And 1) Then 
                'if (nvraddr >= &he  And  nvrram(nvraddr) <> valor) Then Print #5,"savenvr sin hacer" 'savenvr() 
                if (nvraddr<>&hC)  And  (nvraddr<>&hD) Then nvrram(nvraddr)=valor 
                if (nvraddr=&hA) Then 
                        if (valor And &hF) Then 
                                c=1 Shl ((valor And &hF)-1) 
                                rtctime+=RTCCONST*c 
                        else
                           rtctime=99999999
                        EndIf
                EndIf
        Else
			   nvraddr=valor And 127
	         ' 127 es el maximo de RAM de la CMOS en AMIBIOS (128bytes)
        EndIf
End Sub

function read_NVR(addr As UShort) As UByte
        Dim As ubyte temp

        if (addr And 1) Then 
                if (nvraddr<=&hA) Then getnvrtime() ' hora
                if (nvraddr=&hD) Then nvrram(&hD) Or=&h80
                if (nvraddr=&hA) Then 
                        temp=nvrram(&hA)
                        nvrram(&hA) and=inv(&h80)
                        return temp
                EndIf
                if (nvraddr=&hC) Then    
			               picintc(&h100)
                        temp=nvrram(&hC)
                        nvrram(&hC)=0
                        return temp
                EndIf
                'Print #5,Hex(nvraddr,2),Hex ( nvrram(nvraddr),2)
                'sleep
                return nvrram(nvraddr)
        EndIf

        return nvraddr
        
End Function



Sub loadCMOS(bios As String)
	  If bios="" Then return
	
     'If i486=486 Then
     	Open "bios\"+bios+".nvr" For Binary Access Read As 3
     'EndIf
     
     'if i486=386 Then
     '	Open "bios\ami386.nvr" For Binary Access Read As 3
     'EndIf
         
        Dim As Integer c,d
        Dim As String f=" "
        
        Seek 3,1
        For c=0 To 127 
          	Get #3,,f
            nvrram(c)=Asc(f)
            'Print #5,Hex(nvrram(c),2);",";
        Next

     Close 3
     
        'nvrram(&hA)=6
        'nvrram(&hB)=0
        c=1 Shl ((6 And &hF)-1)
        rtctime+=RTCCONST*c
End Sub

Dim Shared cadaxveces As Integer=0

Sub clockhardware(cycdiff As Integer) 
                
                keybsenddelay -= cycdiff
                if (keybsenddelay<1) Then 
                        keybsenddelay = 1000 
                        keyboard_at_poll() 
                EndIf
                
                pit.c(0)-=cycdiff 
                pit.c(1)-=cycdiff 
                if (ppi.pb And 1) Then pit.c(2)-=cycdiff 

                If ((pit.c(0)<1) Or (pit.c(1)<1) Or (pit.c(2)<1)) Then pit_poll()
                
                vidtime-=cycdiff
                If (vidtime<=0) Then 
                        svga_poll()
                EndIf
                

                'If (disctime) Then 
                '        disctime-=(cycdiff) 
                '        If (disctime<=0) Then 
                '                disctime=0 
                '                fdc_poll() 
                '        EndIf
                'EndIf
                
                If (mousedelay) Then 
                        mousedelay-=20 
                        if (mousedelay=0) Then 'deberia ser =0 , pero no me convence
                                mousecallback() 
                        EndIf
                EndIf

                If (idecallback(0)) Then 
                        idecallback(0)-=1
                        if (idecallback(0)<=0) Then 
                                idecallback(0)=0 
                                callbackide(0) 
                        EndIf
                EndIf
                
                'if (idecallback(1)) Then 
                '        idecallback(1)-=1 
                '        if (idecallback(1)<=0) Then 
                '                idecallback(1)=0 
                '                callbackide(1) 
                '        EndIf
                'EndIf
                
                rtctime-=cycdiff 
                If (rtctime<0) Then 
                       nvr_rtc() 
                EndIf

End Sub




''''''''''''''''''
''' PUERTOS ''''''
''''''''''''''''''

''''''''''''''''''''''''''''''''''''''''''''''''''

Function checkio(ByVal port As Integer ) As Integer 
        Dim As UShort  t 
        Dim As UByte  d 
        cpl_override = 1 
        t = readmemw_386(tr.base0, &h66) 
        cpl_override = 0 
        if abrt Then print #5,"ERROR en CHECKIO":Return 0 ' error
        if (t+(port Shr 3))>tr.limit Then return 1 
        cpl_override = 1 
        d=readmemb_386l(0,tr.base0+t+(port Shr 3)) ' lectura sin pasar por cache (tal como lo hace la V8.1)
        cpl_override = 0 
        return d And (1 Shl (port And 7)) 
End Function

' revisar este define, que no parece correcto, deberia ir integrado en cada linea que lo llama??? por el break 
function checkio_perm(ByVal port As integer) As Integer
	Dim As Integer tempi
	if (IOPLp=0) or ((eflags And VM_FLAG)<>0) Then
        tempi = checkio(port)
        if abrt then print #5,"ERROR en CHECKIO_PERM":Return 0 ' error
        if tempi Then
            x86gpf(0)
            'print #5,"error en CHECKIO_PERM (trampeado)"
            Return 0 ' error
        EndIf
	EndIf
	Return 1 ' dato correcto o no afecta
End Function
'''''''''''''''''''''''''''''''''''''''''''''''''



' SALIDAS PUERTOS PC
Sub outb(ByVal port As UShort, ByVal valor As ubyte)

	
	' DMA 
	If port>=&h00 and port<=&h0F Then 
		dma_write(port,valor)
		Return
	EndIf
	
	' DMA 16
	If port>=&hC0 And port<=&hDF Then 
		dma16_write(port,valor)
		Return
	EndIf	
	
	' DMA PAGE
	If port>=&h80 And port<=&h87 Then 
		dma_page_write(port,valor)
		Return
	EndIf
	
	' DMA PAGE 16
	If port>=&h88 And port<=&h8F Then 
		dma_page_write(port,valor)
		Return
	EndIf
	
	

	' PIT TIMER 8253
	If port>=&h40 And port<=&h43 Then 
		pit_write(port,valor)
		Return
	EndIf
	
	
	' shadowram ALI1429
	If port>=&h22 And port<=&h23 Then 
		ali1429_write(port,valor)
		Return
	EndIf
	
	
	' PIC IRQ 8259
	If port>=&h20 And port<=&h21 Then 
		pic_write(port,valor)
		Return
	EndIf
	
	If port>=&hA0 And port<=&hA1 Then 
		pic2_write(port,valor)
		Return
	EndIf
	
	
	
	
	' CMOS
	If port>=&h70 And port<=&h71 Then 
		write_NVR(port,valor)
		Return
	EndIf
	
	
	
	' FDC
	If port>=&h3F0 And port<=&h3F5 Then 
		Return
	EndIf
	If port=&h3F7 Then 
		return
	EndIf
	
	
	' IDE
	If port>=&h1F0 And port<=&h1F7 Then 
		'Print #5,"Write IDE-1:";Hex(port,4),Hex(valor,2)
		ide_write_pri(port,valor)
		Return
	EndIf
	If port=&h3F6 Then 
		'Print #5,"Write IDE-1:";Hex(port,4),Hex(valor,2)
		ide_write_pri(port,valor)
		Return
	EndIf
	' IDE secundario
	If port>=&h170 And port<=&h177 Then 
		'Print #5,"Write IDE-2:";Hex(port,4),Hex(valor,2)
		ide_write_sec(port,valor)
		Return
	EndIf
	If port=&h376 Then 
		'Print #5,"Write IDE-2:";Hex(port,4),Hex(valor,2)
		ide_write_sec(port,valor)
		Return
	EndIf
		

	' 8255 teclado??	
	If port>=&h60 And port<=&h64 Then 
		keyboard_at_write(port,valor)
		Return
	EndIf
	

	' serie 2	
	If port>=&h2F8 And port<=&h2FF Then 
		serial2_write(port,valor)
		Return
	EndIf
	' serie 1	
	If port>=&h3F8 And port<=&h3FF Then 
		serial_write(port,valor)
		Return
	EndIf
	
	
	' gameblaster
	If port>=&h220 And port<=&h223 Then 
		Return
	EndIf	
	
	
	If port>=&h3C0 And port<=&h3DF Then ' VGA
		et4000_out(port,valor):Return
		Return
	EndIf

' estos puertos, si se usa la ET4000 basica NO SE EMPLEAN!!!!
	If port>=&h3A0 And port<=&h3BF Then ' VGA
		'et4000_out(port,valor):Return
		return
	EndIf	

	puertosb(port)=valor
	print #1,"OUTB:";Hex(port,4),Hex(valor,2)
End Sub



' ENTRADA PUERTOS PC
Function inb(ByVal port As UShort) As UByte
	Dim valor As Integer

	If port>=&h3C0 And port<=&h3DF Then ' VGA
		Return et4000_in(port)
	EndIf

' estos puertos, si se usa la ET4000 basica NO SE EMPLEAN!!!!
	If port>=&h3A0 And port<=&h3BF Then ' MDA mono
		'return et4000_in(port) ' si activo esta, (pero no la de out), se arranca en modo MDA
		Return &hff
	EndIf	
	
	'If port>=&h210A And port<=&h217B Then
	'	'Print "TSENGLABS control de cursor LEE!!!":Sleep
	'	Return et4000w32p_in(port)
	'EndIf			
	
	' DMA 
	If port>=&h00 And port<=&h0F Then 
		Return dma_read(port)
	EndIf
	
	' DMA 16
	If port>=&hC0 And port<=&hDF Then 
		Return dma16_read(port)
	EndIf	
	
	' DMA PAGE
	If port>=&h80 And port<=&h87 Then 
		Return dma_page_read(port)
	EndIf
	
	' DMA PAGE 16
	If port>=&h88 And port<=&h8F Then 
		Return dma_page_read(port)
	EndIf
	
	
	
	

	' PIT TIMER 8253
	If port>=&h40 and port<=&h43 Then 
		Return pit_read(port)
	EndIf
	
	
	' shadowram ALI1429
	If port>=&h22 And port<=&h23 Then 
		Return ali1429_read(port)
	EndIf		
	
		
	' PIC IRQ 8259
	If port>=&h20 And port<=&h21 Then 
		valor=pic_read(port)
		'Print #5,port
		Return valor'pic_read(port)
	EndIf
	
	If port>=&hA0 And port<=&hA1 Then 
		Return pic2_read(port)
	EndIf


	' CMOS
	If port>=&h70 And port<=&h71 Then 
		Return read_NVR(port)
	EndIf
	
	
	
	' FDC
	If port>=&h3F0 And port<=&h3F5 Then 
		Return 255
	EndIf
	If port=&h3F7 Then 
		Return 255
	EndIf

	
	' IDE
	If port>=&h1F0 And port<=&h1F7 Then 
		valor=ide_read_pri(port)
		'Print #5,"Read  IDE-1:";Hex(port,4),Hex(valor,2)
		Return valor 'ide_read_pri(port)
	EndIf
	If port=&h3F6 Then 
		valor=ide_read_pri(port)
		'Print #5,"Read  IDE-1:";Hex(port,4),Hex(valor,2)
		Return valor 'ide_read_pri(port)
	EndIf
	' IDE secundario
	If port>=&h170 And port<=&h177 Then 
		valor=ide_read_sec(port)
		'Print #5,"Read  IDE-2:";Hex(port,4),Hex(valor,2)
		Return valor 'ide_read_pri(port)
	EndIf
	If port=&h376 Then 
		valor=ide_read_sec(port)
		'Print #5,"Read  IDE-2:";Hex(port,4),Hex(valor,2)
		Return valor 'ide_read_pri(port)
	EndIf
	
	

	' 8255 teclado
	If port>=&h60 And port<=&h64 Then 
		Return keyboard_at_read(port)
	EndIf
	

	' serie 2	
	If port>=&h2F8 And port<=&h2FF Then 
		'If port=&h2fd Then Return &hff ' trampa temporal para el programa TEST386 que la parecer, lo necesita
		Return serial2_read(port)
	EndIf
	' serie 1	
	If port>=&h3F8 And port<=&h3FF Then 
		Return serial_read(port)
	EndIf
	

	' desconocido, quizas puerto de juegos MIDI
	If port=&h201 Then 
		Return &hff
	EndIf
	
	' gameblaster
	If port>=&h220 And port<=&h223 Then 
		Return &hff
	EndIf
	
		' desconocido
	If port>=&h388 And port<=&h389 Then 
		Return &hff
	EndIf
	
	
	print #1," INB:";Hex(port,4),Hex(puertosb(port),2)
	'Return puertosb(port) ' nota: si devuelvo este valor, el MOUSE.COm da error
	
	' nota: DEBE devolver FF para que no den error algunos puertos, como los COM3 y 4
	Return &hff 

End Function









Sub outw(ByVal port As UShort, ByVal valor As ushort)

	If port>=&h3A0 And port<=&h3DE Then 
		et4000_out(port,valor And &hff)
		et4000_out(port+1,valor Shr 8)
		Return
	EndIf
	
	' IDE
	If port>=&h1F0 And port<=&h1F6 Then 
		'Print #5,"OUTW:";Hex(port,4),Hex(valor,4)
		ide_write_pri_w(port,valor)
		'ide_write_pri_w(port+1,valor Shr 8)
		Return
	EndIf

	If port>=&h170 And port<=&h176 Then 
		ide_write_sec_w(port,valor)
		'ide_write_sec_w(port+1,valor Shr 8)
		Return
	EndIf

		
	' CRTC 6845, lo llama muchas veces pero no se que hacer aun 
	If port=&h3D4 Then 
		Return
	EndIf

	puertosw(port)=valor	
	print #1,"OUTW:";Hex(port,4),Hex(valor,4)
End Sub

Sub outl(ByVal port As UShort, ByVal valor As ulong)
	puertosl(port)=valor
	print #1,"OUTL:";Hex(port,4),Hex(valor,8)
End Sub




Function inw(ByVal port As UShort) As UShort
	Dim valor As UShort
	
	
	If port>=&h3A0 And port<=&h3DE Then 
		Return et4000_in(port) Or (et4000_in(port+1) Shl 8)
	EndIf
	
	
	' IDE
	If port>=&h1F0 And port<=&h1F6 Then 
		'Print #5,"INW:";Hex(port,4)
		Return ide_read_pri_w(port) 'Or (ide_read_pri_w(port+1) Shl 8)
	EndIf
	If port>=&h170 And port<=&h176 Then 
		Return ide_read_sec_w(port) 'Or (ide_read_sec_w(port+1) Shl 8)
	EndIf
	
	
	print #1,"INW:";Hex(port,4),Hex(puertosw(port),4)
	Return puertosw(port) 
End Function


Function inl(ByVal port As UShort) As ULong
	print #1,"INL:";Hex(port,4),Hex(puertosl(port),8)
	Return puertosl(port)
End Function