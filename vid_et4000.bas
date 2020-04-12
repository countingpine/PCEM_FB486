' ET4000 (Diamond Stealth 32)

Declare Sub et4000_recalcmapping()  

static shared As Integer unk_state=0
static shared As uByte unk_ctrl

Sub unk_ramdac_out(ByVal addr As UShort , ByVal valor As UByte ) 
        Select Case As Const(addr)
        	Case &h3C6 
                if (unk_state = 4) Then 
                        unk_state = 0 
                        unk_ctrl = valor 
                        Select Case As Const((valor And 1) Or ((valor And &hE0) Shr 4))
                        	Case 0, 1, 2, 3 
                                bpp = 8 

                        	Case 6, 7 
                                bpp = 24 

                        	case 8, 9, &hA, &hB 
                                bpp = 15

                        	Case &hC, &hD, &hE, &hF 
                                bpp = 16 

                        End Select
                        Return 
                EndIf
                unk_state = 0 
               
        	Case &h3C7, &h3C8, &h3C9 
                unk_state = 0 

        End Select
        
       svga_out(addr,valor) 
End Sub

Function unk_ramdac_in(ByVal addr As UShort ) As UByte 
        Select Case As Const(addr)
        	Case &h3C6 
                if (unk_state = 4) Then 
                        unk_state = 0 
                        return unk_ctrl 
                EndIf
                unk_state+=1 
                
        	Case &h3C7, &h3C8, &h3C9 
                unk_state = 0 
                
        End Select
        
       return svga_in(addr) 
End Function


Sub et4000_out(ByVal addr As UShort , ByVal valor As UByte ) 
        Dim As UByte old=any
        
        'Print #5,"  ET4000 out ";Hex(addr,4);" ";Hex(valor,2);" ";(svga_miscout And 1);
        If (((addr And &hFFF0) = &h3D0) Or ((addr And &hFFF0) = &h3B0)) And ((svga_miscout And 1)=0) Then addr = addr Xor &h60
		
        Select Case As Const (addr)
        	Case &h3C6, &h3C7, &h3C8, &h3C9 
                unk_ramdac_out(addr,valor) 
                return 
                
        	Case &h3CD  /'Banking'/
                svgawbank=(valor And &hF)*&h10000 ' bancos de 64k
                svgarbank=(valor Shr   4)*&h10000 
                svgaseg=valor 
                Return
					 
        	'Case &h3CF ' este no se usa, lo anulo
         '       Select Case As Const (gdcaddr And 15)
         '       	Case 6 
         '               et4k_b8000=((crtc(&h36) And &h38)=&h28)  And  ((gdcreg(6) And &hC)=4) 
         '       End Select

        	Case &h3D4 
                crtcreg = valor  And &h3f 
					 Return
					 
        	Case &h3D5 
                if (crtcreg <= 7)  And  ((crtc(&h11) And &h80)<>0) Then return 
                old=crtc(crtcreg) 
                crtc(crtcreg)=valor 
                if (old<>valor) Then 
                        if (crtcreg<&hE)  Or  (crtcreg>&h10) Then 
                                fullchange=changeframecount 
                                svga_recalctimings() 
                        EndIf
                EndIf
                
        	Case &h3D8 
                if (valor=&hA0) Then svgaon=1 
                if (valor=&h29) Then svgaon=0 
				    
        End Select

       svga_out(addr,valor) 
End Sub

Function et4000_in(ByVal addr As UShort ) As UByte 
        Dim As UByte temp =Any

        'If (addr<>&h3da) And (addr<>&h3ba) Then Print #5,"   IN ET4000 ";Hex(addr,4);" ";(svga_miscout And 1);
        If (((addr And &hFFF0) = &h3D0) Or ((addr And &hFFF0) = &h3B0)) And ((svga_miscout And 1)=0) Then addr = addr Xor &h60

        Select Case As Const(addr)
        	case &h3C5 
                if (seqaddr And &hF)=7 Then return seqregs(seqaddr And &hF) Or 4 
              
        	Case &h3C6, &h3C7, &h3C8, &h3C9 
                return unk_ramdac_in(addr) 
                
        	Case &h3CD  /'Banking'/
                Return svgaseg 
                
        	Case &h3D4 
                return crtcreg 
                
        	Case &h3D5 
                return crtc(crtcreg) 
                
        End Select
        
        Return svga_in(addr) 
End Function

Sub et4000_recalctimings() 
        svga_ma = svga_ma Or ((crtc(&h33) And 3) Shl 16) 
        if crtc(&h35) And &h02 Then svga_vtotal+=&h400 
        if crtc(&h35) And &h04 Then svga_dispend+=&h400 
        if crtc(&h35) And &h08 Then svga_vsyncstart+=&h400 
        if crtc(&h35) And &h10 Then svga_split+=&h400 
        if svga_rowoffset=0 Then svga_rowoffset=&h100 
        if crtc(&h3F) And 1 Then svga_htotal+=256 
        if attrregs(&h16) And &h20 Then svga_hdisp = svga_hdisp Shl 1 
        Select Case As Const((svga_miscout Shr 2) And 3)  Or  ((crtc(&h34) Shl 1) And 4)
        	case 0, 1
        		Exit Sub
        	case 3  
        		svga_clock = CPUCLOCK/40000000.0
        	case 5  
        		svga_clock = CPUCLOCK/65000000.0
        	Case else 
        		svga_clock = CPUCLOCK/36000000.0
        End Select
End Sub

sub et4000_init()
        'svga_recalctimings_ex = et4000_recalctimings 
        'et4000_recalctimings()
        svga_vram_limit = VRAM_TOTAL+1 '1 mega en el ET4000 basica
        vrammask = svga_vram_limit-1 '&hFFFFF 
End Sub