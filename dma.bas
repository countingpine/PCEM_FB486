
Dim Shared as UByte dmaregs(16)
Dim Shared as Integer dmaon(4)
Dim Shared as UByte dma16regs(16)
Dim Shared as Integer dma16on(4)
Dim Shared as UByte dmapages(16)


'Sub dma_init() 
        'io_sethandler(&h0000, &h0010, dma_read,      NULL, NULL, dma_write,      NULL, NULL) 
        'io_sethandler(&h0080, &h0008, dma_page_read, NULL, NULL, dma_page_write, NULL, NULL) 
'End Sub

'Sub dma16_init() 
        'io_sethandler(&h00C0, &h0020, dma16_read,    NULL, NULL, dma16_write,    NULL, NULL) 
        'io_sethandler(&h0088, &h0008, dma_page_read, NULL, NULL, dma_page_write, NULL, NULL) 
'End Sub


Sub dma_reset() 
        Dim As Integer c 
        dma.wp=0 
        for  c=0 To 15
        	dmaregs(c)=0
        Next
        for  c=0 To 3
                dma.mode(c)=0 
                dma.ac(c)=0 
                dma.cc(c)=0 
                dma.ab(c)=0 
                dma.cb(c)=0 
        Next
        dma.m=0 
        dma16.wp=0 
        for  c=0 To 15 
        	dma16regs(c)=0
        Next
        for  c=0 To 3
                dma16.mode(c)=0 
                dma16.ac(c)=0 
                dma16.cc(c)=0 
                dma16.ab(c)=0 
                dma16.cb(c)=0 
        Next
        dma16.m=0 
End Sub

Function dma_read(byval addr As Ushort ) As UByte 
        Dim As UByte  temp 
        Select Case As Const  (addr And &hF)
        	case 0, 2, 4, 6  /'Address registers'/
                dma.wp = dma.wp Xor 1 
                if dma.wp Then Return dma.ac((addr Shr 1) And 3) And &hFF 
                Return dma.ac((addr Shr 1) And 3) Shr 8 

        	Case 1, 3, 5, 7  /'Count registers'/
                dma.wp = dma.wp Xor 1 
                if dma.wp Then Return dma.cc((addr Shr 1) And 3) And &hFF 
                Return dma.cc((addr Shr 1) And 3) Shr 8

        	Case 8  /'Status register'/
                temp=dma.stat 
                dma.stat=0 
                return temp Or 1 
                
        	Case &hD 
                return 0 
                
        End Select
        return dmaregs(addr And &hF) 
End Function

Sub dma_write(byval addr As Ushort, ByVal valor As UByte ) 
        dmaregs(addr And &hF)=valor 
        Select Case As Const  (addr And &hF)
        	case 0, 2, 4, 6  /'Address registers'/
                dma.wp = dma.wp Xor 1 
                if dma.wp Then 
                     dma.ab((addr Shr 1) And 3)=(dma.ab((addr Shr 1) And 3) And &hFF00) Or  valor 
                else
                     dma.ab((addr Shr 1) And 3)=(dma.ab((addr Shr 1) And 3) And &hFF  ) Or (valor Shl 8)
                EndIf
                dma.ac((addr Shr 1) And 3)=dma.ab((addr Shr 1) And 3) 
                dmaon ((addr Shr 1) And 3)=1 
                return 
        	case 1, 3, 5, 7  /'Count registers'/
                dma.wp = dma.wp Xor 1 
                if dma.wp Then 
                     dma.cb((addr Shr 1) And 3)=(dma.cb((addr Shr 1) And 3) And &hFF00) Or  valor 
                else
                     dma.cb((addr Shr 1) And 3)=(dma.cb((addr Shr 1) And 3) And &hFF  ) Or (valor Shl 8)
                EndIf
                dma.cc((addr Shr 1) And 3)=dma.cb((addr Shr 1) And 3) 
                dmaon ((addr Shr 1) And 3)=1 
                return 
        	case 8  /'Control register'/
                dma.command0 = valor 
                return 
        	case &hA  /'Mask'/
                if (valor And 4) Then 
                     dma.m = dma.m Or     (1 Shl (valor And 3)) 
                else
                     dma.m = dma.m And inv(1 Shl (valor And 3))
                EndIf
                return 
        	case &hB  /'Mode'/
                dma.mode(valor And 3)=valor 
                return 
        	case &hC  /'Clear FF'/
                dma.wp=0 
                return 
        	case &hD  /'Master clear'/
                dma.wp=0 
                dma.m=&hF 
                return 
        	case &hF  /'Mask write'/
                dma.m=valor And &hF 
                return 
        End Select
end Sub

Function dma16_read(ByVal addr As UShort ) As UByte 
        Dim As UByte  temp 
        addr = addr Shr 1 
        Select Case As Const  (addr And &hF)
        	Case 0 
                if ((dma16.mode(0) Shr 2) And 3)=2 Then 
                        dma16.ac(0)+=1 
                        dma16.cc(0)-=1 
                        if dma16.cc(0)<0 Then 
                                dma16.ac(0)=dma16.ab(0) 
                                dma16.cc(0)=dma16.cb(0) 
                        EndIf
                EndIf
                '''''''''Address registers AC'''''''''''
                ' esto es como "CASE 2,4,6" pero al no llegar, lo pongo aqui
                dma16.wp = dma16.wp Xor 1 
                if dma16.wp Then Return dma16.ac((addr Shr 1) And 3) And &hFF 
                Return dma16.ac((addr Shr 1) And 3) Shr 8 
                '''''''''''''''''''''''''''''''''''''''
                
        	Case 2, 4, 6  /'Address registers AC'/
                dma16.wp = dma16.wp Xor 1 
                if dma16.wp Then Return dma16.ac((addr Shr 1) And 3) And &hFF 
                Return dma16.ac((addr Shr 1) And 3) Shr 8 
                
        	Case 1, 3, 5, 7  /'Count registers CC'/
                dma16.wp = dma16.wp Xor 1 
                if dma16.wp Then Return dma16.cc((addr Shr 1) And 3) And &hFF 
                Return dma16.cc((addr Shr 1) And 3) Shr 8
                
        	Case 8  /'Status register'/
                temp=dma16.stat 
                dma16.stat=0 
                return temp Or 1 
                
        End Select

       return dma16regs(addr And &hF) 
End Function

Sub dma16_write(ByVal addr As UShort , ByVal valor As UByte ) 
        addr = addr Shr 1 
        dma16regs(addr And &hF)=valor 
        Select Case As Const  (addr And &hF)
        	Case 0, 2, 4, 6  /'Address registers'/
                dma16.wp = dma16.wp Xor 1 
                if dma16.wp Then 
                    dma16.ab((addr Shr 1) And 3)=(dma16.ab((addr Shr 1) And 3) And &hFF00) Or  valor 
                else
                    dma16.ab((addr Shr 1) And 3)=(dma16.ab((addr Shr 1) And 3) And &hFF  ) Or (valor Shl 8)
                EndIf
                dma16.ac((addr Shr 1) And 3)=dma16.ab((addr Shr 1) And 3) 
                dma16on ((addr Shr 1) And 3)=1 
                return 
        	Case 1, 3, 5, 7  /'Count registers'/
                dma16.wp = dma16.wp Xor 1 
                if dma16.wp Then 
                    dma16.cb((addr Shr 1) And 3)=(dma16.cb((addr Shr 1) And 3) And &hFF00) Or  valor 
                else
                    dma16.cb((addr Shr 1) And 3)=(dma16.cb((addr Shr 1) And 3) And &hFF  ) Or (valor Shl 8)
                EndIf
                dma16.cc((addr Shr 1) And 3)=dma16.cb((addr Shr 1) And 3) 
                dma16on ((addr Shr 1) And 3)=1 
                return 
        	Case 8  /'Control register'/
                return 
        	Case &hA  /'Mask'/
                if valor And 4 Then 
                    dma16.m = dma16.m   Or   (1 Shl (valor And 3)) 
                else
                    dma16.m = dma16.m And inv(1 Shl (valor And 3))
                EndIf
                return 
        	Case &hB  /'Mode'/
                dma16.mode(valor And 3)=valor 
                return 
        	Case &hC  /'Clear FF'/
                dma16.wp=0 
                return 
        	Case &hD  /'Master clear'/
                dma16.wp=0 
                dma16.m=&hF 
                return 
        	Case &hF  /'Mask write'/
                dma16.m=valor And &hF 
                return 
        End Select
End Sub

Sub dma_page_write(byval addr As Ushort, valor As UByte ) 
        dmapages(addr And &hF)=valor 
        Select Case As Const  (addr And &hF)
        	Case 1 
                dma.page(2)=valor 'IIf(AT,valor,valor And &hF )

        	Case 2 
                dma.page(3)=valor 'IIf(AT,valor,valor And &hF )

        	Case 3 
                dma.page(1)=valor 'IIf(AT,valor,valor And &hF )

        	Case &hB 
                dma16.page(1)=valor 

        End Select
End Sub

Function dma_page_read(byval addr As UShort ) As UByte 
        return dmapages(addr And &hF) 
End Function

' NOTA IMPORTANTE: las rutinas que hay aqui en medio, las he puesto abajo del todo agrupadas
' por que son SOLO para el FDC.BAS, y espero NO usarlo al acabar, y borrarlos

Function readdma1() As UByte 
        Dim As UByte temp=0 

        temp=ram[(dma.ac(1)+(dma.page(1) Shl 16)) And rammask] 
        if dmaon(1)=0 Then 
           Return temp 
        EndIf
        dma.ac(1)+=1 
        dma.cc(1)-=1 
        if (dma.cc(1)<=-1)  And  ((dma.mode(1) And &h10)<>0) Then 
            dma.cc(1)=dma.cb(1) 
            dma.ac(1)=dma.ab(1) 
        ElseIf dma.cc(1)<=-1 Then
				dmaon(1)=0
        EndIf
        return temp 
End Function

Function readdma5() As UShort 
        Dim As UShort temp=0 
        
        temp=ram[((dma16.ac(1) Shl 1)+((dma16.page(1) And inv(1)) Shl 16)  ) And rammask] _ 
         Or (ram[((dma16.ac(1) Shl 1)+((dma16.page(1) And inv(1)) Shl 16)+1) And rammask] Shl 8) 
         
        if dma16on(1)=0 Then 
                return temp 
        EndIf
        dma16.ac(1)+=1 
        dma16.cc(1)-=1 
        if (dma16.cc(1)<=-1)  And  ((dma16.mode(1) And &h10)<>0) Then 
            dma16.cc(1)=dma16.cb(1) 
            dma16.ac(1)=dma16.ab(1) 
        ElseIf dma16.cc(1)<=-1 Then
				dma16on(1)=0
        EndIf
        return temp 
End Function

Sub writedma1(temp As UByte ) 
        if dmaon(1)=0 Then return 
        ram[(dma.ac(1)+(dma.page(1) Shl 16)) And rammask]=temp 
        dma.ac(1)+=1 
        dma.cc(1)-=1 
        if (dma.cc(1)=0)  And  ((dma.mode(1) And &h10)<>0) Then 
            dma.cc(1)=dma.cb(1)+1 
            dma.ac(1)=dma.ab(1) 
        ElseIf dma.cc(1)<=-1 Then
				dmaon(1)=0
        EndIf
End Sub

Sub writedma5(temp As UShort ) 
        if dma16on(1)=0 Then return 
        ram[((dma16.ac(1) Shl 1)+((dma16.page(1) And inv(1)) Shl 16)  ) And rammask]=temp 
        ram[((dma16.ac(1) Shl 1)+((dma16.page(1) And inv(1)) Shl 16)+1) And rammask]=temp Shr 8
        dma16.ac(1)+=1 
        dma16.cc(1)-=1 
        if (dma16.cc(1)<=-1) And ((dma16.mode(1) And &h10)<>0) Then 
            dma16.cc(1)=dma16.cb(1) 
            dma16.ac(1)=dma16.ab(1) 
        ElseIf dma16.cc(1)<=-1 Then
				dma16on(1)=0
        EndIf
End Sub

Function readdma3() As Integer 
        Dim As ubyte temp
        temp=ram[((dma.page(3) Shl 16)+dma.ac(3)) And rammask]
        if (dma.m And 8) Then Return -1 
        if (dma.m And 8)=0 Then 
                dma.ac(3)+=1 
                dma.cc(3)-=1 
                if dma.cc(3)=-1 Then 
                    dma.m = dma.m Or 8 
                    dma.stat = dma.stat Or 8 
                EndIf
        EndIf
        return temp 
End Function

'refresread en X86.bas
Declare Sub refreshread()
Sub readdma0() 
        'if (AT) Then 
        	 ppi.pb = ppi.pb Xor &h10   ' siempre es AT      
        'EndIf
        if dma.command0 And 4 Then return 
        'refreshread() ' lo anulo, por que sin el parece funcionar bien
        if dma.m And 1 Then return 
        
       dma.ac(0)+=2 
       dma.cc(0)-=1 
       if dma.cc(0)=-1 Then 
           dma.stat = dma.stat Or 1 
           dma.ac(0)= dma.ab(0) 
           dma.cc(0)= dma.cb(0) 
       EndIf
End Sub























' al parecer, esta rutina es SOLO para el modulo FDC!!!!
Function _dma_read(addr As ULong ) As UByte 
        Select Case  (addr And &hFFFF8000)
        	Case &hA0000, &hA8000 
                Return vram[addr-&hA0000]
                'Return leeram1(addr) 'video_read_a000(addr) 
        	Case &hB0000 
                Return vram[addr-&hB0000]
                'Return leeram1(addr) 'video_read_b000(addr) 
        	Case &hB8000 
                Return vram[addr-&hB8000]
                'Return leeram1(addr) 'video_read_b800(addr) 
        End Select
        If (isram(addr Shr 16)) Then return ram[addr] 
        return &hff 
End Function

' al parecer, esta rutina es SOLO para el modulo FDC!!!!
Sub _dma_write(addr As ULong , valor As UByte ) 
        Select Case  (addr And &hFFFF8000)
        	Case &hA0000, &hA8000 
                vram[addr-&hA0000]=valor
                'grabaram1(addr,valor)' video_write_a000(addr,valor) 
                return 
        	Case &hB0000 
                vram[addr-&hB0000]=valor
                'grabaram1(addr,valor)' video_write_b000(addr,valor) 
                return 
        	Case &hB8000 
                vram[addr-&hB8000]=valor'
                'grabaram1(addr,valor)' video_write_b800(addr,valor) 
                return 
        	Case &hC0000, &hC8000, &hD0000, &hD8000 
        			 Return
        	Case &hE0000, &hE8000, &hF0000, &hF8000 
                return 
       End Select
       if (isram(addr Shr 16)) Then ram[addr]=valor 
End Sub

