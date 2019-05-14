
Declare Sub et4000_recalctimings()

Sub svga_init()
	' rotacion VGA no lo empleo
        'Dim As integer c, d, e 
        'For  c = 0 To 255 
        '     e = c 
        '     for  d = 0 To  7
        '          svga_rotate(d, c) = e 
        '          e = (e Shr 1) Or IIf((e And 1), &h80, 0)
        '     Next
        'Next
        readmode = 0 
End Sub


' mostramos en pantalla la imagen creada en el "vga_buffer"
Sub svga_doblit(y1 As Integer , y2 As Integer ) 
	Static As Integer cadaxveces
   Dim As Integer a,b,c
	
   If y1>=y2 Then 
   	Return ' no se si debe ser asi
   EndIf
   
	ScreenLock
		'cadaxveces-=1
		'If cadaxveces<0 Then 
		'	cadaxveces=2
		For a=y1 To y2-1 'y1=inicio de linea, y2 fin de linea
			For b=0 To wx-1  +32 ' creo que hay que compensar los bordes con este "+32"
				PSet (b-32,a),vga_buffer(a,b)
			Next
		Next
		'End If
	ScreenUnLock
	ScreenCopy 
End Sub




' puertos entrada salida genericos
Sub svga_out(ByVal addr As UShort , ByVal valor As UByte ) 
        Dim As Integer c 
        Dim As UByte o 

   'Print #5," -- SVGA OUT ";Hex(addr,4);" ";Hex(Valor,2);" ";Hex(CS1,4);":";Hex(pc,4);" ";Hex(svgawbank,8)

        Select Case As Const (addr)
        	Case &h3C0 
                if attrff=0 Then 
                  attraddr=valor And 31 
                else
                  attrregs(attraddr And 31)=valor 
                  if (attraddr<16) Then fullchange=changeframecount 
                  if (attraddr=&h10)  Or  (attraddr=&h14)  Or  (attraddr<&h10) Then 
                          for  c=0 To 15 
                                  if (attrregs(&h10) And &h80) Then 
                                  	egapal(c)=(attrregs(c) And &h0F) Or ((attrregs(&h14) And &hF) Shl 4) 
                                  Else
                                    egapal(c)=(attrregs(c) And &h3F) Or ((attrregs(&h14) And &hC) Shl 4) 
                                  EndIf
                          Next
                  EndIf
                  If (attraddr = &h10) Then svga_recalctimings() 
                EndIf
                attrff = attrff Xor 1 
                 
        	case &h3C2
                svga_miscout = valor 
                pallook = @pallook256(0) ' si lo quito , funciona igualmente!!!
                vidclock = valor And 4
                If (valor And 1) Then 
                	'Print #5,"Puertos TSENG SVGA"
                	vga_ports_activos=1
                Else
                	'Print #5,"Puertos VGA"
                	vga_ports_activos=0
                EndIf
                'svga_recalctimings() ' esto lo he sacado de la 9.0, pero funciona sin el tambien
                
        	Case &h3C4 
        		seqaddr=valor  
        		
        	Case &h3C5 
                if (seqaddr > &hf) Then return 
                o=seqregs(seqaddr And &hF) 
                seqregs(seqaddr And &hF)=valor 
                if (o<>valor) And ((seqaddr And &hF)=1) Then svga_recalctimings() 
                Select Case As Const (seqaddr And &hF)
                	case 1  
                		if (scrblank<>0) And ((valor And &h20)=0) Then fullchange=3
                		scrblank=(scrblank And inv(&h20)) Or (valor And &h20) 
                	case 2  
                		writemask=valor And &hF  
                	case 3
                     charset=valor And &hF 
                     charseta=((charset Shr 2)*&h10000)+2 
                     charsetb=((charset And 3)*&h10000)+2  
                     'If valor And &h10 Then charseta+=&h8000 ' de la 9.0
                     'If valor And &h20 Then charsetb+=&h8000 ' de la 9.0
                	case 4 
                     chain4=valor And 8
                     svga_fast =  (( (gdcreg(8)=&hFF) And ((gdcreg(3) And &h18)=0) And (gdcreg(1)=0) ) <>0)  And  (chain4<>0)  
                End Select
                
        	case &h3C6  
        		dacmask=valor ' de la 9.0
        		               
        	case &h3C7  
        		dacread=valor
        		dacpos=0  
        		
        	case &h3C8  
        		dacwrite=valor
        		dacpos=0  
        		
        	Case &h3C9 
                'palchange=1 ' paleta cambia
                dacstatus=0 ' de la 9.0
                fullchange=changeframecount 
                Select Case As Const (dacpos)
                	case 0  
                		vgapal(dacwrite).r=valor And 63
                		pallook256(dacwrite)=makecol32 (vgapal(dacwrite).r*4,vgapal(dacwrite).g*4,vgapal(dacwrite).b*4)
                		dacpos+=1
                	case 1  
                		vgapal(dacwrite).g=valor And 63
                		pallook256(dacwrite)=makecol32 (vgapal(dacwrite).r*4,vgapal(dacwrite).g*4,vgapal(dacwrite).b*4)
                		dacpos+=1 
                	case 2  
                		vgapal(dacwrite).b=valor And 63
                		pallook256(dacwrite)=makecol32 (vgapal(dacwrite).r*4,vgapal(dacwrite).g*4,vgapal(dacwrite).b*4)
                		dacpos=0
                		dacwrite=(dacwrite+1) And 255 
               End Select
                
        	case &h3CE  
        		gdcaddr=valor  
        		
        	Case &h3CF 
                Select Case As Const (gdcaddr And 15)
                	case 2  
                		colourcompare=valor  
                	case 4  
                		readplane=valor And 3  
                	case 5 
                		writemode=valor And 3
                		readmode=valor And 8  
                	Case 6 
                     if (gdcreg(6) And &hc)  <>  (valor And &hc) Then 
                       'mem_removehandler(&ha0000, &h20000, svga_read, svga_readw, svga_readl, svga_write, svga_writew, svga_writel) 
                       
                       ' aqui, como no sirve de nada todo esto de punteros a VRAM, simplemente, borro la pantalla
                       ScreenLock   
                          Cls ' simple y efectivo
                       screenunlock
                       ''''''''''''''''
                       
                       Select Case As Const (valor And &hC)
                         case &h0  /'128k at A0000'/
                         	'mem_sethandler(&ha0000, &h20000, svga_read, svga_readw, svga_readl, svga_write, svga_writew, svga_writel) 
                          
                         case &h4  /'64k at A0000'/
                         	'mem_sethandler(&ha0000, &h10000, svga_read, svga_readw, svga_readl, svga_write, svga_writew, svga_writel) 
                          
                         case &h8  /'32k at B0000'/
                         	'mem_sethandler(&hb0000, &h08000, svga_read, svga_readw, svga_readl, svga_write, svga_writew, svga_writel) 
                          
                         case &hC  /'32k at B8000'/
                         	'mem_sethandler(&hb8000, &h08000, svga_read, svga_readw, svga_readl, svga_write, svga_writew, svga_writel) 
                          
                      End Select
                     EndIf
                	case 7 
                		colournocare=valor  
                End Select
                gdcreg(gdcaddr And 15)=valor             
                svga_fast = (( (gdcreg(8)=&hFF) And ((gdcreg(3) And &h18)=0) And (gdcreg(1)=0) ) <>0)  And  (chain4<>0) 
                ' esta linea es de la 9.0
                'if (( (gdcaddr and 15)=5  and (valor xor o) And &h70) or ((gdcaddr and 15) = 6 and (valor xor o) and 1)) then
                '        svga_recalctimings()
                'End If              
        End Select
end Sub

' puertos entrada salida genericos
Function svga_in(ByVal addr As UShort ) As UByte 
        Dim As UByte  temp 
        
  'If (addr<>&h3da) And (addr<>&h3ba) Then Print #5," -- SVGA IN ";Hex(addr,4)

        Select Case As Const (addr)
        	case &h3C0  
        		return attraddr 
        	case &h3C1  
        		return attrregs(attraddr) 
        	case &h3C2  
        		return &h10 
        	case &h3C4  
        		return seqaddr 
        	case &h3C5
            Return seqregs(seqaddr And &hF) 
        	case &h3C6
        		return dacmask 
        	case &h3C7
        		return dacstatus 
        	case &h3C8
        		return dacwrite 
        	case &h3C9 
                'palchange=1 ' paleta cambia
                dacstatus=3
                Select Case As Const (dacpos)
                	case 0 
                		dacpos+=1
                		return vgapal(dacread).r 
                	case 1  
                		dacpos+=1
                		return vgapal(dacread).g 
                	case 2  
                		dacpos=0
                		dacread=(dacread+1) And 255
                		return vgapal((dacread-1) And 255).b 
                End Select
        	case &h3CC  
        		 Return svga_miscout 
        	case &h3CE  
        		 Return gdcaddr 
        	Case &h3CF 
             Return gdcreg(gdcaddr And &hF) 
        	Case &h3DA 
             attrff=0 
             cgastat = cgastat Xor &h30 
             return cgastat 
        End Select

        return &hFF 
End Function


Sub svga_recalctimings() 
        Dim As double crtcconst 
        Dim As Integer temp 
        
        svga_vtotal=crtc(6) 
        svga_dispend=crtc(&h12) 
        svga_vsyncstart=crtc(&h10) 
        svga_split=crtc(&h18) 
        
        if (crtc(7) And 1)  Then svga_vtotal = svga_vtotal Or &h100 
        if (crtc(7) And 32) Then svga_vtotal = svga_vtotal Or &h200        
        svga_vtotal+=2 
        
        if (crtc(7) And 2)  Then svga_dispend = svga_dispend Or &h100 
        if (crtc(7) And 64) Then svga_dispend = svga_dispend Or &h200 
        svga_dispend+=1 
        
        if (crtc(7) And 4)   Then svga_vsyncstart = svga_vsyncstart Or &h100 
        if (crtc(7) And 128) Then svga_vsyncstart = svga_vsyncstart Or &h200 
        svga_vsyncstart+=1 
        
        if (crtc(7) And &h10) Then svga_split = svga_split Or &h100 
        if (crtc(9) And &h40) Then svga_split = svga_split Or &h200 
        svga_split+=1 
        
        svga_hdisp=crtc(1)
        svga_hdisp+=1 
        
        svga_htotal=crtc(0) 
        svga_htotal+=6
        
        svga_rowoffset=crtc(&h13) 
        
        svga_clock = IIf(vidclock , VGACONST2 , VGACONST1) 
        svga_lowres = attrregs(&h10) And &h40 
        svga_interlace = 0 
        svga_ma = (crtc(&hC) Shl 8) Or crtc(&hD) 
        
        et4000_recalctimings() ' el original llama a la svga_recalctimings_ex
       
        crtcconst=IIf((seqregs(1) And 1),(svga_clock*8.0),(svga_clock*9.0)) 
        disptime  =svga_htotal 
        dispontime=svga_hdisp 
        if (seqregs(1) And 8) Then 
            disptime=disptime*2
            dispontime=dispontime*2  
        EndIf
        
        dispofftime=disptime-dispontime 
        dispontime =dispontime*crtcconst 
        dispofftime=dispofftime*crtcconst 
End Sub

Sub svga_poll() 
        Dim As UByte chr0,dat,attr 
        Dim As ULong charaddr 
        Dim As Integer x,xx 
        Dim As ULong fg,bg 
        Dim As Integer offset 
        Dim As UByte edat(4) 
        Dim As integer drawcursor=0 

        if (linepos=0) Then 
                vidtime+=dispofftime 
                cgastat = cgastat Or 1 
                linepos=1 
                if (svga_dispon) Then 
                        svga_hdisp_on=1 
                        ma = ma And vrammask 
                        if (firstline=2000) Then firstline=displine 
                        if (scrblank) Then 
                                if (firstline_draw = 2000) Then firstline_draw = displine 
                                lastline_draw = displine 
                                for  x=0 To svga_hdisp-1
                                     Select Case As Const (seqregs(1) And 9)
                                     	Case 0 
                                             for  xx=0 To 8
                                             	vga_buffer(displine,(x*9)+xx+32)=0
                                             Next
                                              
                                     	Case 1 
                                             for  xx=0 To 7
                                             	vga_buffer(displine,(x*8)+xx+32)=0
                                             Next
                                              
                                     	Case 8 
                                             for  xx=0 To 17
                                             	vga_buffer(displine,(x*18)+xx+32)=0
                                             Next
                                              
                                     	case 9
                                             for  xx=0 To 15
                                             	vga_buffer(displine,(x*16)+xx+32)=0
                                             Next
                                              
                                     End Select
                                Next
                        elseif (gdcreg(6) And 1)=0 Then 
                        	     If (firstline_draw = 2000) Then firstline_draw = displine 
                                lastline_draw = displine 
                                If (fullchange) Then 
                                     for  x=0 To svga_hdisp-1 
                                          drawcursor=(ma=ca) And (con<>0) And (cursoron<>0) 
                                          chr0=vram[ma Shl 1]
                                          attr=vram[(ma Shl 1)+4]
                                          if (attr And 8) Then 
                                              charaddr=charsetb+(chr0*128) 
                                          else
                                              charaddr=charseta+(chr0*128)
                                          EndIf
                                          if (drawcursor) Then 
                                             bg=pallook[egapal(attr And 15)]
                                             fg=pallook[egapal(attr Shr 4)]   
                                          Else
                                             fg=pallook[egapal(attr And 15)] 
                                             bg=pallook[egapal(attr Shr 4)] 
                                             If ((attr And &h80)<>0)  And  ((attrregs(&h10) And 8)<>0) Then 
                                                    bg=pallook[egapal((attr Shr 4) And 7)] 
                                                    if (cgablink And 16) Then fg=bg 
                                             EndIf
                                          EndIf
                                          dat=vram[charaddr+(sc Shl 2)] 
                                          if (seqregs(1) And 8) Then 
                                               /'40 column'/
                                               if (seqregs(1) And 1) Then 
                                                     for xx=0 To 7  
                                                     	vga_buffer(displine,((x Shl 4)+32+(xx Shl 1)) And 2047)=IIf(dat And (&h80 Shr xx),fg,bg)
                                                     	vga_buffer(displine,((x Shl 4)+33+(xx Shl 1)) And 2047)=IIf(dat And (&h80 Shr xx),fg,bg)
                                                     Next
                                               else
                                                    for xx=0 To 7  
                                                    	vga_buffer(displine,((x*18)+32+(xx Shl 1)) And 2047)=IIf(dat And (&h80 Shr xx),fg,bg)
                                                    	vga_buffer(displine,((x*18)+33+(xx Shl 1)) And 2047)=IIf(dat And (&h80 Shr xx),fg,bg)
                                                    Next
                                                    if (chr0 And inv(&h1F)<>&hC0)  Or  ((attrregs(&h10) And 4)=0) Then 
                                                       vga_buffer(displine,((x*18)+32+16) And 2047)=bg
                                                       vga_buffer(displine,((x*18)+32+17) And 2047)=bg 
                                                    else
                                                       vga_buffer(displine,((x*18)+32+16) And 2047)=IIf(dat And 1,fg,bg)
                                                       vga_buffer(displine,((x*18)+32+17) And 2047)=IIf(dat And 1,fg,bg)
                                                    EndIf
                                               EndIf
                                          else
                                               /'80 column'/
                                               if (seqregs(1) And 1) Then 
                                                    For xx=0 To 7  
                                                     	 vga_buffer(displine,((x Shl 3)+32+xx) And 2047)=IIf(dat And (&h80 Shr xx),fg,bg)
                                                    Next
                                               Else
                                                    for xx=0 To 7  
                                                    	 vga_buffer(displine,((x*9)+32+xx) And 2047)=IIf(dat And (&h80 Shr xx),fg,bg)
                                                    Next
                                                    If (chr0 And inv(&h1F)<>&hC0) Or ((attrregs(&h10) And 4)=0) Then 
                                                       vga_buffer(displine,((x*9)+32+8) And 2047)=bg
                                                    Else
                                                       vga_buffer(displine,((x*9)+32+8) And 2047)=IIf(dat And 1,fg,bg)
                                                    EndIf
                                               EndIf
                                          EndIf
                                          ma+=4: ma = ma And vrammask 
                                     Next
                                EndIf
                        Else
                                Select Case As Const (gdcreg(5) And &h60)
                                	Case &h00  /'16 colours'/
                                        if (firstline_draw = 2000) Then firstline_draw = displine 
                                        lastline_draw = displine 
                                        if (seqregs(1) And 8) Then /'Low res (320)'/
                                             offset=((8-scrollcache) Shl 1)+16 
                                             for x=0 To  svga_hdisp 
                                                  edat(0)=vram[ma] 
                                                  edat(1)=vram[ma Or &h1] 
                                                  edat(2)=vram[ma Or &h2] 
                                                  edat(3)=vram[ma Or &h3] 
                                                  ma+=4: ma = ma And vrammask 
                                                  
                                                  dat=edatlookup(edat(0) And 3, edat(1) And 3) Or (edatlookup(edat(2) And 3, edat(3) And 3) Shl 2) 
                                                  vga_buffer(displine,(x Shl 4)+14+offset)=pallook[egapal(dat And &hF)] 
                                                  vga_buffer(displine,(x Shl 4)+15+offset)=pallook[egapal(dat And &hF)] 
                                                  vga_buffer(displine,(x Shl 4)+12+offset)=pallook[egapal(dat Shr 4)]
                                                  vga_buffer(displine,(x Shl 4)+13+offset)=pallook[egapal(dat Shr 4)] 
                                                  
                                                  dat=edatlookup((edat(0) Shr 2) And 3, (edat(1) Shr 2) And 3) Or (edatlookup((edat(2) Shr 2) And 3, (edat(3) Shr 2) And 3) Shl 2) 
                                                  vga_buffer(displine,(x Shl 4)+10+offset)=pallook[egapal(dat And &hF)]
                                                  vga_buffer(displine,(x Shl 4)+11+offset)=pallook[egapal(dat And &hF)] 
                                                  vga_buffer(displine,(x Shl 4)+ 8+offset)=pallook[egapal(dat Shr 4)]
                                                  vga_buffer(displine,(x Shl 4)+ 9+offset)=pallook[egapal(dat Shr 4)] 
                                                  
                                                  dat=edatlookup((edat(0) Shr 4) And 3, (edat(1) Shr 4) And 3) Or (edatlookup((edat(2) Shr 4) And 3, (edat(3) Shr 4) And 3) Shl 2) 
                                                  vga_buffer(displine,(x Shl 4)+6+offset)=pallook[egapal(dat And &hF)]
                                                  vga_buffer(displine,(x Shl 4)+7+offset)=pallook[egapal(dat And &hF)]
                                                  vga_buffer(displine,(x Shl 4)+4+offset)=pallook[egapal(dat Shr 4)]
                                                  vga_buffer(displine,(x Shl 4)+5+offset)=pallook[egapal(dat Shr 4)] 
                                                  
                                                  dat=edatlookup(edat(0) Shr 6, edat(1) Shr 6) Or (edatlookup(edat(2) Shr 6, edat(3) Shr 6) Shl 2) 
                                                  vga_buffer(displine,(x Shl 4)+2+offset)=pallook[egapal(dat And &hF)]
                                                  vga_buffer(displine,(x Shl 4)+3+offset)=pallook[egapal(dat And &hF)] 
                                                  vga_buffer(displine,(x Shl 4)  +offset)=pallook[egapal(dat Shr 4)]
                                                  vga_buffer(displine,(x Shl 4)+1+offset)=pallook[egapal(dat Shr 4)] 
                                             Next
                                        else
                                             /'High res (640)'/ ' 16 colores
                                             offset=(8-scrollcache)+24 
                                             for x=0 To svga_hdisp                                            
                                                  edat(0)=vram[ma] 
                                                  edat(1)=vram[ma Or &h1] 
                                                  edat(2)=vram[ma Or &h2]
                                                  edat(3)=vram[ma Or &h3] 
                                                  ma+=4: ma = ma And vrammask 
                                                  
                                                  dat=edatlookup(edat(0) And 3, edat(1) And 3) Or (edatlookup(edat(2) And 3, edat(3) And 3) Shl 2) 
                                                  vga_buffer(displine,(x Shl 3)+7+offset)=pallook[egapal(dat And &hF)] 
                                                  vga_buffer(displine,(x Shl 3)+6+offset)=pallook[egapal(dat Shr 4)] 
                                                  
                                                  dat=edatlookup((edat(0) Shr 2) And 3, (edat(1) Shr 2) And 3) Or (edatlookup((edat(2) Shr 2) And 3, (edat(3) Shr 2) And 3) Shl 2) 
                                                  vga_buffer(displine,(x Shl 3)+5+offset)=pallook[egapal(dat And &hF)] 
                                                  vga_buffer(displine,(x Shl 3)+4+offset)=pallook[egapal(dat Shr 4)]
                                                  
                                                  dat=edatlookup((edat(0) Shr 4) And 3, (edat(1) Shr 4) And 3) Or (edatlookup((edat(2) Shr 4) And 3, (edat(3) Shr 4) And 3) Shl 2) 
                                                  vga_buffer(displine,(x Shl 3)+3+offset)=pallook[egapal(dat And &hF)]
                                                  vga_buffer(displine,(x Shl 3)+2+offset)=pallook[egapal(dat Shr 4)]

                                                  dat=edatlookup(edat(0) Shr 6, edat(1) Shr 6) Or (edatlookup(edat(2) Shr 6, edat(3) Shr 6) Shl 2) 
                                                  vga_buffer(displine,(x Shl 3)+1+offset)=pallook[egapal(dat And &hF)]
                                                  vga_buffer(displine,(x Shl 3)  +offset)=pallook[egapal(dat Shr 4)]
                                             Next
                                        EndIf
                                         
                                	case &h20  /'4 colours'/
                                        if (firstline_draw = 2000) Then firstline_draw = displine 
                                        lastline_draw = displine 
                                        offset=((8-scrollcache) Shl 1)+16 
                                        /'Low res (320) only, though high res (640) should be possible'/
                                        for  x=0 To svga_hdisp
                                                if ((sc And 1)<>0) And ((crtc(&h17) And 1)=0) Then 
                                                        edat(0)=vram[(ma Shl 1)+&h8000]
                                                        edat(1)=vram[(ma Shl 1)+&h8004] 
                                                else
                                                        edat(0)=vram[(ma Shl 1)]
                                                        edat(1)=vram[(ma Shl 1)+4] 
                                                EndIf
                                                ma+=4: ma = ma And vrammask 
                                                vga_buffer(displine,(x Shl 4)+14+offset)=pallook[egapal( edat(1) And 3)]
                                                vga_buffer(displine,(x Shl 4)+15+offset)=pallook[egapal( edat(1) And 3)] 
                                                vga_buffer(displine,(x Shl 4)+12+offset)=pallook[egapal((edat(1) Shr 2) And 3)]
                                                vga_buffer(displine,(x Shl 4)+13+offset)=pallook[egapal((edat(1) Shr 2) And 3)]
                                                
                                                dat=edatlookup((edat(0) Shr 2) And 3, (edat(1) Shr 2) And 3) Or (edatlookup((edat(2) Shr 2) And 3, (edat(3) Shr 2) And 3) Shl 2) 
                                                vga_buffer(displine,(x Shl 4)+10+offset)=pallook[egapal((edat(1) Shr 4) And 3)]
                                                vga_buffer(displine,(x Shl 4)+11+offset)=pallook[egapal((edat(1) Shr 4) And 3)] 
                                                vga_buffer(displine,(x Shl 4)+ 8+offset)=pallook[egapal((edat(1) Shr 6) And 3)]
                                                vga_buffer(displine,(x Shl 4)+ 9+offset)=pallook[egapal((edat(1) Shr 6) And 3)]
                                                
                                                dat=edatlookup((edat(0) Shr 4) And 3, (edat(1) Shr 4) And 3) Or (edatlookup((edat(2) Shr 4) And 3, (edat(3) Shr 4) And 3) Shl 2) 
                                                vga_buffer(displine,(x Shl 4)+6+offset)=pallook[egapal((edat(0)      ) And 3)]
                                                vga_buffer(displine,(x Shl 4)+7+offset)=pallook[egapal((edat(0)      ) And 3)]
                                                vga_buffer(displine,(x Shl 4)+4+offset)=pallook[egapal((edat(0) Shr 2) And 3)]
                                                vga_buffer(displine,(x Shl 4)+5+offset)=pallook[egapal((edat(0) Shr 2) And 3)] 
                                                
                                                dat=edatlookup(edat(0) Shr 6, edat(1) Shr 6) Or (edatlookup(edat(2) Shr 6, edat(3) Shr 6) Shl 2) 
                                                vga_buffer(displine,(x Shl 4)+2+offset)=pallook[egapal((edat(0) Shr 4) And 3)]
                                                vga_buffer(displine,(x Shl 4)+3+offset)=pallook[egapal((edat(0) Shr 4) And 3)] 
                                                vga_buffer(displine,(x Shl 4)  +offset)=pallook[egapal((edat(0) Shr 6) And 3)]
                                                vga_buffer(displine,(x Shl 4)+1+offset)=pallook[egapal((edat(0) Shr 6) And 3)] 
                                        Next
                                         
                                	Case &h40, &h60  /'256 colours (and more, with high/true colour RAMDAC)'/
                                       If (changedvram(ma Shr 10)<>0)  Or  (changedvram((ma Shr 10)+1)<>0)  Or  (changedvram((ma Shr 10)+2)<>0) Or  (fullchange<>0) Then 
                                          if (firstline_draw = 2000) Then firstline_draw = displine 
                                          lastline_draw = displine 
                                          If (svga_lowres) Then 
                                               /'Low res (320)'/
                                               offset=(8-(scrollcache And 6))+24 
                                               If (bpp=8) Then 'Regular 8 bpp palette mode
                                                       for  x=0 To svga_hdisp 
                                                               edat(0)=vram[ma]
                                                               edat(1)=vram[ma Or &h1] 
                                                               edat(2)=vram[ma Or &h2] 
                                                               edat(3)=vram[ma Or &h3]
                                                               ma+=4: ma = ma And vrammask 
                                                               vga_buffer(displine,(x Shl 3)+7+offset)=pallook[edat(3)] 
                                                               vga_buffer(displine,(x Shl 3)+6+offset)=pallook[edat(3)]
                                                               vga_buffer(displine,(x Shl 3)+5+offset)=pallook[edat(2)]  
                                                               vga_buffer(displine,(x Shl 3)+4+offset)=pallook[edat(2)] 
                                                               vga_buffer(displine,(x Shl 3)+3+offset)=pallook[edat(1)]  
                                                               vga_buffer(displine,(x Shl 3)+2+offset)=pallook[edat(1)] 
                                                               vga_buffer(displine,(x Shl 3)+1+offset)=pallook[edat(0)]   
                                                               vga_buffer(displine,(x Shl 3)+0+offset)=pallook[edat(0)] 
                                                       Next
                                               ElseIf (bpp=16) Then /'16 bpp 565 mode'/
																		 For  x=0 To svga_hdisp 
                                                               fg=vram[ma] Or (vram[ma Or &h1] Shl 8) 
                                                               bg=vram[ma Or &h2] Or (vram[ma Or &h3] Shl 8) 
                                                               ma+=4: ma = ma And vrammask 
                                                               vga_buffer(displine,(x Shl 2)+3+offset)=((bg And 31) Shl 3) Or (((bg Shr 5) And 63) Shl 10) Or (((bg Shr 11) And 31) Shl 19)
                                                               vga_buffer(displine,(x Shl 2)+2+offset)=((bg And 31) Shl 3) Or (((bg Shr 5) And 63) Shl 10) Or (((bg Shr 11) And 31) Shl 19) 
                                                               vga_buffer(displine,(x Shl 2)+1+offset)=((fg And 31) Shl 3) Or (((fg Shr 5) And 63) Shl 10) Or (((fg Shr 11) And 31) Shl 19) 
                                                               vga_buffer(displine,(x Shl 2)+0+offset)=((fg And 31) Shl 3) Or (((fg Shr 5) And 63) Shl 10) Or (((fg Shr 11) And 31) Shl 19) 
																		 Next
                                               ElseIf (bpp=15) Then /'15 bpp 555 mode'/
                                                       for  x=0 To svga_hdisp 
                                                               fg=vram[ma] Or (vram[ma Or &h1] Shl 8) 
                                                               bg=vram[ma Or &h2] Or (vram[ma Or &h3] Shl 8) 
                                                               ma+=4: ma = ma And vrammask 
                                                               vga_buffer(displine,(x Shl 1)+2+offset)=((bg And 31) Shl 3) Or (((bg Shr 5) And 31) Shl 11) Or (((bg Shr 10) And 31) Shl 19)
                                                               vga_buffer(displine,(x Shl 2)+3+offset)=((bg And 31) Shl 3) Or (((bg Shr 5) And 31) Shl 11) Or (((bg Shr 10) And 31) Shl 19) 
                                                               vga_buffer(displine,(x Shl 1)+0+offset)=((fg And 31) Shl 3) Or (((fg Shr 5) And 31) Shl 11) Or (((fg Shr 10) And 31) Shl 19)
                                                               vga_buffer(displine,(x Shl 2)+1+offset)=((fg And 31) Shl 3) Or (((fg Shr 5) And 31) Shl 11) Or (((fg Shr 10) And 31) Shl 19) 
                                                       Next
                                               ElseIf (bpp=24) Then /'24 bpp 888 mode'/
																			For  x=0 To ((svga_hdisp Shl 3)\3) 
                                                               fg=vram[ma] Or (vram[ma+1] Shl 8) Or (vram[ma+2] Shl 16) 
                                                               ma+=3: ma = ma And vrammask 
                                                               vga_buffer(displine,(x Shl 1)  +offset)=fg
                                                               vga_buffer(displine,(x Shl 1)+1+offset)=fg 
																			Next
                                               ElseIf (bpp=32) Then /'32 bpp 8888 mode'/
                                                       for  x = 0 To svga_hdisp
                                                               fg = vram[ma]  Or  (vram[ma + 1]  Shl  8)  Or  (vram[ma + 2]  Shl  16) 
                                                               ma += 4: ma  = ma And vrammask 
                                                               vga_buffer(displine,(x Shl 1)  +offset)=fg
                                                               vga_buffer(displine,(x Shl 1)+1+offset)=fg 
                                                       Next
                                               EndIf
                                          Else
                                            /'High res (SVGA only)'/
                                            offset=(8-((scrollcache And 6) Shr 1))+24
                                            if (bpp=8) Then 
                                                    for  x=0 To (svga_hdisp Shl 1)
                                                         edat(0)=vram[ma] 
                                                         edat(1)=vram[ma Or &h1] 
                                                         edat(2)=vram[ma Or &h2] 
                                                         edat(3)=vram[ma Or &h3] 
                                                         ma+=4: ma = ma And vrammask 
                                                         vga_buffer(displine,(x Shl 2)+3+offset)=pallook[edat(3)] 
                                                         vga_buffer(displine,(x Shl 2)+2+offset)=pallook[edat(2)] 
                                                         vga_buffer(displine,(x Shl 2)+1+offset)=pallook[edat(1)] 
                                                         vga_buffer(displine,(x Shl 2)+offset)=  pallook[edat(0)] 
                                                    Next
                                            ElseIf (bpp=16) Then
																	 For  x=0 To (svga_hdisp Shl 1)
                                                         fg=vram[ma] Or (vram[ma Or &h1] Shl 8) 
                                                         bg=vram[ma Or &h2] Or (vram[ma Or &h3] Shl 8) 
                                                         ma+=4: ma = ma And vrammask 
                                                         vga_buffer(displine,(x Shl 1)+1+offset)=((bg And 31) Shl 3) Or (((bg Shr 5) And 63) Shl 10) Or (((bg Shr 11) And 31) Shl 19) 
                                                         vga_buffer(displine,(x Shl 1)+0+offset)=((fg And 31) Shl 3) Or (((fg Shr 5) And 63) Shl 10) Or (((fg Shr 11) And 31) Shl 19) 
																	 Next
                                            ElseIf (bpp=15) Then 
                                                    for  x=0 To (svga_hdisp Shl 1)
                                                         fg=vram[ma] Or (vram[ma Or &h1] Shl 8) 
                                                         bg=vram[ma Or &h2] Or (vram[ma Or &h3] Shl 8) 
                                                         ma+=4: ma = ma And vrammask 
                                                         vga_buffer(displine,(x Shl 1)+1+offset)=((bg And 31) Shl 3) Or (((bg Shr 5) And 31) Shl 11) Or (((bg Shr 10) And 31) Shl 19) 
                                                         vga_buffer(displine,(x Shl 1)+0+offset)=((fg And 31) Shl 3) Or (((fg Shr 5) And 31) Shl 11) Or (((fg Shr 10) And 31) Shl 19) 
                                                    Next
                                            ElseIf (bpp=24) Then
																	 For  x=0 To ((svga_hdisp Shl 3)\3) 
                                                         fg=vram[ma] Or (vram[ma+1] Shl 8) Or (vram[ma+2] Shl 16) 
                                                         ma+=3: ma = ma And vrammask 
                                                         vga_buffer(displine,x+offset)=fg 
																	 Next
                                            ElseIf (bpp=32) Then 
                                                    for  x = 0 To (svga_hdisp Shl 2)
                                                         fg = vram[ma]  Or  (vram[ma + 1]  Shl  8)  Or  (vram[ma + 2]  Shl  16) 
                                                         ma += 4: ma  = ma  And  vrammask 
                                                         vga_buffer(displine,x + offset) = fg 
                                                    Next
                                            EndIf
                                          EndIf
                                       EndIf 
                                End Select
                        EndIf 
                        if (lastline<displine) Then lastline=displine 
                EndIf
                displine+=1 
                if (svga_interlace) Then displine+=1 
                if ((cgastat And 8)<>0)  And  ((displine And 15)=(crtc(&h11) And 15))  And  (vslines<>0) Then 
                        cgastat = cgastat And inv(8) 
                EndIf
                vslines+=1 
                if displine>1500 Then displine=0 
        Else
                vidtime+=dispontime 
                if (svga_dispon) Then cgastat = cgastat And inv(1)
                svga_hdisp_on=0 
                linepos=0 
                
                if sc=(crtc(11) And 31) Then con = 0 
                
                if (svga_dispon) Then 
                        if ((crtc(9) And &h80)<>0) And (linecountff=0) Then 
                                linecountff=1 
                                ma=maback 
                        ElseIf sc=(crtc(9) And 31) Then
										  linecountff=0 
                                sc=0 
                                maback += (svga_rowoffset Shl 3) 
                                if (svga_interlace) Then maback += (svga_rowoffset Shl 3) 
                                maback = maback And vrammask 
                                ma=maback 
                        Else
			                       linecountff=0 
			                       sc+=1 
			                       sc = sc And 31 
			                       ma=maback 
                        EndIf
                EndIf
                
                vc+=1 
                vc = vc And 2047 
                
                if (vc=svga_split) Then 
                        ma=0
                        maback=0 
                        if (attrregs(&h10) And &h20) Then scrollcache=0 
                EndIf
                
                if (vc=svga_dispend) Then 
                        svga_dispon=0 
                        if (crtc(10) And &h20) Then 
                              cursoron=0 
                        else
                              cursoron=cgablink And 16
                        EndIf
                        if ((gdcreg(6) And 1)=0)  And  ((cgablink And 15)=0) Then fullchange=2 
                        cgablink+=1
                        for  x=0 To 2047 
                        	if changedvram(x) Then changedvram(x)-=1
                        Next
                        if (fullchange) Then fullchange-=1 
                EndIf
                               
                if (vc=svga_vsyncstart) Then 
                        svga_dispon=0 
                        cgastat = cgastat Or 8 
                        if (seqregs(1) And 8) Then 
                             x=svga_hdisp * ( IIf((seqregs(1) And 1),8,9) ) * 2 
                        else
                             x=svga_hdisp *   IIf((seqregs(1) And 1),8,9) 
                        EndIf
                        if (bpp = 16) Or (bpp = 15) Then x \= 2 
                        if  bpp = 24 Then x \= 3 
                        if  bpp = 32 Then x \= 4 
                        if (svga_interlace<>0) And (oddeven=0) Then lastline+=1 
                        if (svga_interlace<>0) And (oddeven<>0) Then firstline-=1 
                        wx=x 
                        wy=lastline-firstline 
                        
                        ''''''''''''''''''''''''''''''''''''''''''''''
                        svga_doblit(firstline_draw, lastline_draw + 1) 
                        ''''''''''''''''''''''''''''''''''''''''''''''
                        
                        readflash=0 
                        
                        firstline=2000 
                        lastline=0 
                        
                        firstline_draw = 2000 
                        lastline_draw = 0 
                        
                        oddeven = oddeven Xor 1 
                        
                        changeframecount=IIf((svga_interlace),3,2) 
                        vslines=0 
                        if (svga_interlace<>0)  And  (oddeven<>0) Then 
                             ma = svga_ma + (svga_rowoffset  Shl  1) 
                             maback = ma
                        else
                             ma = svga_ma
                             maback = ma
                        EndIf
                        ca=(crtc(&hE) Shl 8) Or crtc(&hF)

                        ma  Shl = 2 
                        maback  Shl = 2 
                        ca  Shl = 2 
                        
                        video_res_x = wx 
                        video_res_y = wy + 1 
                        
                        if (gdcreg(6) And 1)=0 Then /'Text mode'/
                                video_res_x \= IIf((seqregs(1) And 1) , 8 , 9) 
                                video_res_y \= (crtc(9) And 31) + 1 
                                video_bpp = 0 
                        else
                                if (crtc(9) And &h80) Then video_res_y \= 2 
                                if (crtc(&h17) And 1)=0 Then video_res_y *= 2 
                                video_res_y \= (crtc(9) And 31) + 1                                    
                                if ((seqregs(1) And 8)<>0) Or (svga_lowres<>0) Then video_res_x \= 2 
                                Select Case As Const (gdcreg(5) And &h60)
                                	case &h00             
                                		video_bpp = 4   
                                	case &h20             
                                		video_bpp = 2   
                                	case &h40, &h60  
                                		video_bpp = bpp  
                               End Select
                        EndIf
                EndIf
                
                if (vc=svga_vtotal) Then 
                        vc=0 
                        sc=0 
                        svga_dispon=1 
                        displine=IIf((svga_interlace  And  oddeven) , 1 , 0)
                        scrollcache=attrregs(&h13) And 7 
                        linecountff=0 
                EndIf
                
                if sc=(crtc(10) And 31) Then con=1 
        EndIf
        
End Sub






' accesos a memoria de video VRAM
Sub svga_write(ByVal addr As ULong , ByVal valor As UByte ) 
        Dim As UByte valora,valorb,valorc,valord,wm
        Dim As Integer writemask2 = writemask 
 
        cycles -= video_timing_b 
        if (addr>=&hB0000) Then ' modos EGA 
                addr = addr And &h7FFF 
        Else
                addr = addr And &hFFFF 
                addr = addr + svgawbank 
        EndIf
        if (gdcreg(6) And 1)=0 Then fullchange=2 
        if (chain4) Then 
                writemask2=1 Shl (addr And 3) 
                addr = addr And inv(3) 
        else
                addr = addr Shl 2 
        EndIf

        addr = addr And &h7FFFFF
        
        changedvram(addr Shr 10)=changeframecount 

        Select Case As Const (writemode)
        	Case 1 
                if (writemask2 And 1) Then vram[addr]=dla 
                if (writemask2 And 2) Then vram[addr Or &h1]=dlb 
                if (writemask2 And 4) Then vram[addr Or &h2]=dlc 
                If (writemask2 And 8) Then vram[addr Or &h3]=dld               
                 
        	Case 0 
                if (gdcreg(3) And 7) Then valor=svga_rotate(gdcreg(3) And 7, valor) 
                if (gdcreg(8)=&hFF) And ((gdcreg(3) And &h18)=0) And (gdcreg(1)=0) Then 
                        If (writemask2 And 1) Then vram[addr]=valor 
                        If (writemask2 And 2) Then vram[addr Or &h1]=valor 
                        If (writemask2 And 4) Then vram[addr Or &h2]=valor 
                        If (writemask2 And 8) Then vram[addr Or &h3]=valor 

                Else
                        if (gdcreg(1) And 1) Then 
                            valora=IIf((gdcreg(0) And 1),&hFF,0 )
                        else
                            valora=valor
                        EndIf
                        if (gdcreg(1) And 2) Then 
                            valorb=IIf((gdcreg(0) And 2),&hFF,0 )
                        else
                            valorb=valor
                        EndIf
                        if (gdcreg(1) And 4) Then 
                            valorc=IIf((gdcreg(0) And 4),&hFF,0 )
                        else
                            valorc=valor
                        EndIf
                        if (gdcreg(1) And 8) Then 
                            valord=IIf((gdcreg(0) And 8),&hFF,0 )
                        else
                            valord=valor
                        EndIf
                        
                        Select Case As Const (gdcreg(3) And &h18)
                        	Case 0  /'Set'/
                                if (writemask2 And 1) Then vram[addr]       =(valora And gdcreg(8)) Or (dla And inv(gdcreg(8))) 
                                if (writemask2 And 2) Then vram[addr Or &h1]=(valorb And gdcreg(8)) Or (dlb And inv(gdcreg(8))) 
                                if (writemask2 And 4) Then vram[addr Or &h2]=(valorc And gdcreg(8)) Or (dlc And inv(gdcreg(8))) 
                                if (writemask2 And 8) Then vram[addr Or &h3]=(valord And gdcreg(8)) Or (dld And inv(gdcreg(8))) 
                                 
                        	Case 8  /'AND'/
                                if (writemask2 And 1) Then vram[addr]       =(valora Or inv(gdcreg(8))) And dla 
                                if (writemask2 And 2) Then vram[addr Or &h1]=(valorb Or inv(gdcreg(8))) And dlb 
                                if (writemask2 And 4) Then vram[addr Or &h2]=(valorc Or inv(gdcreg(8))) And dlc 
                                if (writemask2 And 8) Then vram[addr Or &h3]=(valord Or inv(gdcreg(8))) And dld 
                                 
                        	Case &h10  /'OR'/
                                if (writemask2 And 1) Then vram[addr]       =(valora And gdcreg(8)) Or dla 
                                if (writemask2 And 2) Then vram[addr Or &h1]=(valorb And gdcreg(8)) Or dlb 
                                if (writemask2 And 4) Then vram[addr Or &h2]=(valorc And gdcreg(8)) Or dlc 
                                if (writemask2 And 8) Then vram[addr Or &h3]=(valord And gdcreg(8)) Or dld 
                                 
                        	Case &h18  /'XOR'/
                                if (writemask2 And 1) Then vram[addr]       =(valora And gdcreg(8)) Xor dla 
                                if (writemask2 And 2) Then vram[addr Or &h1]=(valorb And gdcreg(8)) Xor dlb 
                                if (writemask2 And 4) Then vram[addr Or &h2]=(valorc And gdcreg(8)) Xor dlc 
                                if (writemask2 And 8) Then vram[addr Or &h3]=(valord And gdcreg(8)) Xor dld 
                                 
                        End Select
                EndIf
                 
        	Case 2 
                if ((gdcreg(3) And &h18)=0) And (gdcreg(1)=0) Then 
                        If (writemask2 And 1) Then vram[addr]       =(iif((valor And 1),&hFF,0) And gdcreg(8)) Or (dla And inv(gdcreg(8))) 
                        if (writemask2 And 2) Then vram[addr Or &h1]=(IIf((valor And 2),&hFF,0) And gdcreg(8)) Or (dlb And inv(gdcreg(8))) 
                        if (writemask2 And 4) Then vram[addr Or &h2]=(IIf((valor And 4),&hFF,0) And gdcreg(8)) Or (dlc And inv(gdcreg(8))) 
                        if (writemask2 And 8) Then vram[addr Or &h3]=(IIf((valor And 8),&hFF,0) And gdcreg(8)) Or (dld And inv(gdcreg(8))) 
                else
                        valora=IIf((valor And 1),&hFF,0) 
                        valorb=IIf((valor And 2),&hFF,0) 
                        valorc=IIf((valor And 4),&hFF,0) 
                        valord=iif((valor And 8),&hFF,0) 
                        Select Case As Const (gdcreg(3) And &h18)
                        	Case 0  /'Set'/
                                if (writemask2 And 1) Then vram[addr]       =(valora And gdcreg(8)) Or (dla And inv(gdcreg(8))) 
                                if (writemask2 And 2) Then vram[addr Or &h1]=(valorb And gdcreg(8)) Or (dlb And inv(gdcreg(8))) 
                                if (writemask2 And 4) Then vram[addr Or &h2]=(valorc And gdcreg(8)) Or (dlc And inv(gdcreg(8))) 
                                if (writemask2 And 8) Then vram[addr Or &h3]=(valord And gdcreg(8)) Or (dld And inv(gdcreg(8))) 
                                 
                        	Case 8  /'AND'/
                                if (writemask2 And 1) Then vram[addr]       =(valora Or inv(gdcreg(8))) And dla 
                                if (writemask2 And 2) Then vram[addr Or &h1]=(valorb Or inv(gdcreg(8))) And dlb 
                                if (writemask2 And 4) Then vram[addr Or &h2]=(valorc Or inv(gdcreg(8))) And dlc 
                                if (writemask2 And 8) Then vram[addr Or &h3]=(valord Or inv(gdcreg(8))) And dld 
                                 
                        	case &h10  /'OR'/
                                if (writemask2 And 1) Then vram[addr]       =(valora And gdcreg(8)) Or dla 
                                if (writemask2 And 2) Then vram[addr Or &h1]=(valorb And gdcreg(8)) Or dlb 
                                if (writemask2 And 4) Then vram[addr Or &h2]=(valorc And gdcreg(8)) Or dlc 
                                if (writemask2 And 8) Then vram[addr Or &h3]=(valord And gdcreg(8)) Or dld 
                                 
                        	case &h18  /'XOR'/
                                if (writemask2 And 1) Then vram[addr]       =(valora And gdcreg(8)) Xor dla 
                                if (writemask2 And 2) Then vram[addr Or &h1]=(valorb And gdcreg(8)) Xor dlb 
                                if (writemask2 And 4) Then vram[addr Or &h2]=(valorc And gdcreg(8)) Xor dlc 
                                if (writemask2 And 8) Then vram[addr Or &h3]=(valord And gdcreg(8)) Xor dld 
                                 
                        End Select
                EndIf
                 
        	Case 3 
                if (gdcreg(3) And 7) Then valor=svga_rotate(gdcreg(3) And 7, valor) 
                wm=gdcreg(8) 
                gdcreg(8) = gdcreg(8) And valor 
                valora=IIf((gdcreg(0) And 1),&hFF,0) 
                valorb=IIf((gdcreg(0) And 2),&hFF,0) 
                valorc=IIf((gdcreg(0) And 4),&hFF,0) 
                valord=IIf((gdcreg(0) And 8),&hFF,0) 
                Select Case As Const (gdcreg(3) And &h18)
                	Case 0  /'Set'/
                        if (writemask2 And 1) Then vram[addr]       =(valora And gdcreg(8)) Or (dla And inv(gdcreg(8))) 
                        if (writemask2 And 2) Then vram[addr Or &h1]=(valorb And gdcreg(8)) Or (dlb And inv(gdcreg(8))) 
                        if (writemask2 And 4) Then vram[addr Or &h2]=(valorc And gdcreg(8)) Or (dlc And inv(gdcreg(8))) 
                        if (writemask2 And 8) Then vram[addr Or &h3]=(valord And gdcreg(8)) Or (dld And inv(gdcreg(8))) 
                         
                	Case 8  /'AND'/
                        if (writemask2 And 1) Then vram[addr]       =(valora Or inv(gdcreg(8))) And dla 
                        if (writemask2 And 2) Then vram[addr Or &h1]=(valorb Or inv(gdcreg(8))) And dlb 
                        if (writemask2 And 4) Then vram[addr Or &h2]=(valorc Or inv(gdcreg(8))) And dlc 
                        if (writemask2 And 8) Then vram[addr Or &h3]=(valord Or inv(gdcreg(8))) And dld 
                         
                	Case &h10  /'OR'/
                        if (writemask2 And 1) Then vram[addr]       =(valora And gdcreg(8)) Or dla 
                        if (writemask2 And 2) Then vram[addr Or &h1]=(valorb And gdcreg(8)) Or dlb 
                        if (writemask2 And 4) Then vram[addr Or &h2]=(valorc And gdcreg(8)) Or dlc 
                        if (writemask2 And 8) Then vram[addr Or &h3]=(valord And gdcreg(8)) Or dld 
                         
                	Case &h18  /'XOR'/
                        if (writemask2 And 1) Then vram[addr]       =(valora And gdcreg(8)) Xor dla 
                        if (writemask2 And 2) Then vram[addr Or &h1]=(valorb And gdcreg(8)) Xor dlb 
                        if (writemask2 And 4) Then vram[addr Or &h2]=(valorc And gdcreg(8)) Xor dlc 
                        if (writemask2 And 8) Then vram[addr Or &h3]=(valord And gdcreg(8)) Xor dld 
                         
                End Select
                gdcreg(8)=wm 
                 
       End Select
end Sub

Function svga_read(ByVal addr As ULong ) As UByte 
        Dim As UByte temp,temp2,temp3,temp4 
        Dim As ULong addr2 

        cycles -= video_timing_b 
        'cycles_lost += video_timing_b 
        
        'egareads+=1 
        
        if (addr>=&hB0000) Then ' modos EGA
                addr = addr And &h7FFF 
        else
                addr = addr And &hFFFF 
                addr += svgarbank 
        EndIf
        
        if (chain4) Then 
                addr = addr And &h7FFFFF 
                if (addr >= svga_vram_limit) Then return &hff ' si superamos los megas de la SVGA
                return vram[addr] 
        else
                addr Shl =2
        EndIf
        
        addr = addr And &h7FFFFF
        if (addr >= svga_vram_limit) Then return &hff 
        
        dla=vram[addr] 
        dlb=vram[addr Or &h1] 
        dlc=vram[addr Or &h2] 
        dld=vram[addr Or &h3] 
        if (readmode) Then 
                temp  = IIf((colournocare And 1) ,&hFF,0 )
                temp  = temp  And dla 
                temp  = temp  Xor IIf((colourcompare And 1),&hFF,0 )
                temp2 = IIf((colournocare And 2) ,&hFF,0 )
                temp2 = temp2 And dlb 
                temp2 = temp2 Xor IIf((colourcompare And 2),&hFF,0) 
                temp3 = IIf((colournocare And 4) ,&hFF,0 )
                temp3 = temp3 And dlc 
                temp3 = temp3 Xor IIf((colourcompare And 4),&hFF,0) 
                temp4 = IIf((colournocare And 8) ,&hFF,0) 
                temp4 = temp4 And dld 
                temp4 = temp4 Xor IIf((colourcompare And 8),&hFF,0 )
                return inv(temp Or temp2 Or temp3 Or temp4) 
        EndIf
        
        return vram[addr Or readplane] 
End Function

'Sub svga_writew(ByVal addr As ULong , ByVal valor As UShort ) 
'        If svga_fast=0 Then
'                svga_write(addr, valor) 
'                svga_write(addr + 1, valor Shr 8) 
'                return 
'        EndIf
'        
'        'egawrites += 2 
'        cycles -= video_timing_w 
'        'cycles_lost += video_timing_w 
'        addr = (addr And &hffff) + svgawbank 
'        addr = addr And &h7FFFFF         
'        changedvram(addr Shr 10) = changeframecount 
'        vram[addr] = valor And &hff
'        vram[addr+1] = (valor Shr 8) And &hff
'End Sub
'
'Sub svga_writel(ByVal addr As ulong , ByVal valor As ULong ) 
'        if svga_fast=0 Then 
'                svga_write(addr, valor) 
'                svga_write(addr + 1, valor Shr 8) 
'                svga_write(addr + 2, valor Shr 16) 
'                svga_write(addr + 3, valor Shr 24) 
'                return 
'        EndIf
'
'        'egawrites += 4 
'        cycles -= video_timing_l 
'        'cycles_lost += video_timing_l 
'        addr = (addr And &hffff) + svgawbank 
'        addr = addr And &h7FFFFF 
'        changedvram(addr Shr 10) = changeframecount 
'        vram[addr] = valor And &hff
'        vram[addr+1] = (valor Shr  8) And &hff
'        vram[addr+2] = (valor Shr 16) And &hff
'        vram[addr+3] = (valor Shr 24) And &hff
'End Sub

'Function svga_readw(ByVal addr As ULong ) As UShort 
'        If svga_fast=0 Then 
'        	  Return svga_read(addr) Or (svga_read(addr + 1) Shl 8) 
'        EndIf
'        
'        'egareads += 2 
'        cycles -= video_timing_w 
'        'cycles_lost += video_timing_w 
'        addr = (addr And &hffff) + svgarbank 
'        addr = addr And &h7FFFFF 
'        if (addr >= svga_vram_limit) Then return &hffff 
'        return vram[addr] Or (vram[addr+1] Shl 8)
'End Function
'
'Function svga_readl(ByVal addr As ulong ) As ULong 
'        if svga_fast=0 Then 
'        	return svga_read(addr) Or (svga_read(addr + 1) Shl 8) Or (svga_read(addr + 2) Shl 16) Or (svga_read(addr + 3) Shl 24) 
'        EndIf
'        
'        'egareads += 4 
'        cycles -= video_timing_l 
'        'cycles_lost += video_timing_l 
'        addr = (addr And &hffff) + svgarbank 
'        addr = addr And &h7FFFFF 
'        if (addr >= svga_vram_limit) Then return &hffffffff 
'        return vram[addr] Or (vram[addr+1] Shl 8) Or (vram[addr+2] Shl 16) Or (vram[addr+3] Shl 24) 
'End Function
'
'Sub svga_writew_linear(ByVal addr As ULong , ByVal valor As UShort ) 
'        if svga_fast=0 Then 
'                svga_write_linear(addr, valor) 
'                svga_write_linear(addr + 1, valor  Shr  8) 
'                return 
'        EndIf
'        
'        egawrites += 2 
'        cycles -= video_timing_w 
'        cycles_lost += video_timing_w 
'        addr  = addr  And  &h7FFFFF 
'        changedvram(addr  Shr  10) = changeframecount 
'        vram[addr] = valor And &hff
'        vram[addr+1] = (valor Shr 8) And &hff
'End Sub
'
'Sub svga_writel_linear(ByVal addr As ulong , ByVal valor As ULong ) 
'        if svga_fast=0 Then 
'                svga_write_linear(addr, valor) 
'                svga_write_linear(addr + 1, valor  Shr  8) 
'                svga_write_linear(addr + 2, valor  Shr  16) 
'                svga_write_linear(addr + 3, valor  Shr  24) 
'                return 
'        EndIf
'        
'        egawrites += 4 
'        cycles -= video_timing_l 
'        cycles_lost += video_timing_l 
'        addr  = addr  And  &h7fffff 
'        changedvram(addr  Shr  10) = changeframecount 
'        vram[addr] = valor And &hff
'        vram[addr+1] = (valor Shr 8) And &hff
'        vram[addr+2] = (valor Shr 16) And &hff
'        vram[addr+3] = (valor Shr 24) And &hff
'End Sub
'
'Function svga_readw_linear(ByVal addr As ULong ) As UShort 
'        if svga_fast=0 Then return svga_read_linear(addr)  Or  (svga_read_linear(addr + 1)  Shl  8) 
'        
'        egareads += 2 
'        cycles -= video_timing_w 
'        cycles_lost += video_timing_w 
'        addr  = addr  And  &h7FFFFF 
'        if (addr >= svga_vram_limit) Then return &hffff 
'        return vram[addr] Or (vram[addr+1] Shl 8)
'End Function
'
'Function svga_readl_linear(ByVal addr As ulong ) As ULong 
'        if svga_fast=0 Then 
'        	return svga_read_linear(addr) Or (svga_read_linear(addr + 1)  Shl  8)  Or  (svga_read_linear(addr + 2)  Shl  16)  Or  (svga_read_linear(addr + 3)  Shl  24) 
'        EndIf
'        
'        egareads += 4 
'        cycles -= video_timing_l 
'        cycles_lost += video_timing_l 
'        addr  = addr  And  &h7FFFFF 
'        if (addr >= svga_vram_limit) Then return &hffffffff 
'        return  vram[addr] Or (vram[addr+1] Shl 8) Or (vram[addr+2] Shl 16) Or (vram[addr+3] Shl 24)
'End Function

'
'Sub svga_write_linear(ByVal addr As ULong , ByVal valor As UByte ) 
'        Dim As integer x,y 
'        Dim As Byte s(2)={0,0} 
'        Dim As UByte  valora,valorb,valorc,valord,wm=writemask 
'        Dim As Integer writemask2 = writemask 
'        Dim As Integer bankaddr 
'        
'        cycles -= video_timing_b 
'        cycles_lost += video_timing_b 
'        egawrites+=1 
'        
'        if (gdcreg(6) And 1)=0 Then fullchange=2 
'        if (chain4) Then 
'                writemask2=1 Shl (addr And 3) 
'                addr = addr And inv(3) 
'        else
'                addr Shl =2 
'        EndIf
'        
'        addr  = addr  And  &h7fffff 
'        changedvram(addr Shr 10)=changeframecount 
'        Select Case As Const (writemode)
'        	Case 1 
'                if (writemask2 And 1) Then vram[addr]=la 
'                if (writemask2 And 2) Then vram[addr Or &h1]=lb 
'                if (writemask2 And 4) Then vram[addr Or &h2]=lc 
'                if (writemask2 And 8) Then vram[addr Or &h3]=ld 
'                 
'        	Case 0 
'                if (gdcreg(3) And 7) Then valor=svga_rotate(gdcreg(3) And 7, valor) 
'                if (gdcreg(8)=&hFF) And ((gdcreg(3) And &h18)=0) And (gdcreg(1)=0) Then 
'                        if (writemask2 And 1) Then vram[addr]=valor 
'                        if (writemask2 And 2) Then vram[addr Or &h1]=valor 
'                        if (writemask2 And 4) Then vram[addr Or &h2]=valor 
'                        if (writemask2 And 8) Then vram[addr Or &h3]=valor 
'                else
'                        if (gdcreg(1) And 1) Then 
'                                valora=IIf((gdcreg(0) And 1),&hFF,0 )
'                        else
'                                valora=valor
'                        EndIf
'                        if (gdcreg(1) And 2) Then 
'                                valorb=IIf((gdcreg(0) And 2),&hFF,0 ) 
'                        else
'                                valorb=valor
'                        EndIf
'                        if (gdcreg(1) And 4) Then 
'                                valorc=IIf((gdcreg(0) And 4),&hFF,0 )
'                        else
'                                valorc=valor
'                        EndIf
'                        if (gdcreg(1) And 8) Then 
'                                valord=IIf((gdcreg(0) And 8),&hFF,0 )
'                        else
'                                valord=valor
'                        EndIf
'                        Select Case As Const (gdcreg(3) And &h18)
'                        	Case 0  /'Set'/
'                                if (writemask2 And 1) Then vram[addr]=(valora And gdcreg(8)) Or (la And inv(gdcreg(8))) 
'                                if (writemask2 And 2) Then vram[addr Or &h1]=(valorb And gdcreg(8)) Or (lb And inv(gdcreg(8))) 
'                                if (writemask2 And 4) Then vram[addr Or &h2]=(valorc And gdcreg(8)) Or (lc And inv(gdcreg(8))) 
'                                if (writemask2 And 8) Then vram[addr Or &h3]=(valord And gdcreg(8)) Or (ld And inv(gdcreg(8))) 
'                                 
'                        	Case 8  /'AND'/
'                                if (writemask2 And 1) Then vram[addr]=(valora Or inv(gdcreg(8))) And la 
'                                if (writemask2 And 2) Then vram[addr Or &h1]=(valorb Or inv(gdcreg(8))) And lb 
'                                if (writemask2 And 4) Then vram[addr Or &h2]=(valorc Or inv(gdcreg(8))) And lc 
'                                if (writemask2 And 8) Then vram[addr Or &h3]=(valord Or inv(gdcreg(8))) And ld 
'                                 
'                        	Case &h10  /'OR'/
'                                if (writemask2 And 1) Then vram[addr]=(valora And gdcreg(8)) Or la 
'                                if (writemask2 And 2) Then vram[addr Or &h1]=(valorb And gdcreg(8)) Or lb 
'                                if (writemask2 And 4) Then vram[addr Or &h2]=(valorc And gdcreg(8)) Or lc 
'                                if (writemask2 And 8) Then vram[addr Or &h3]=(valord And gdcreg(8)) Or ld 
'                                 
'                        	Case &h18  /'XOR'/
'                                if (writemask2 And 1) Then vram[addr]=(valora And gdcreg(8)) Xor la 
'                                if (writemask2 And 2) Then vram[addr Or &h1]=(valorb And gdcreg(8)) Xor lb 
'                                if (writemask2 And 4) Then vram[addr Or &h2]=(valorc And gdcreg(8)) Xor lc 
'                                if (writemask2 And 8) Then vram[addr Or &h3]=(valord And gdcreg(8)) Xor ld 
'                                 
'                        End Select
'                EndIf
'                 
'        	Case 2 
'                if ((gdcreg(3) And &h18)=0)  And  (gdcreg(1)=0) Then 
'                        if (writemask2 And 1) Then vram[addr]=(iif((valor And 1),&hFF,0) And gdcreg(8)) Or (la And inv(gdcreg(8))) 
'                        if (writemask2 And 2) Then vram[addr Or &h1]=(IIf((valor And 2),&hFF,0) And gdcreg(8)) Or (lb And inv(gdcreg(8))) 
'                        if (writemask2 And 4) Then vram[addr Or &h2]=(IIf((valor And 4),&hFF,0) And gdcreg(8)) Or (lc And inv(gdcreg(8))) 
'                        if (writemask2 And 8) Then vram[addr Or &h3]=(IIf((valor And 8),&hFF,0) And gdcreg(8)) Or (ld And inv(gdcreg(8))) 
'                else
'                        valora=IIf((valor And 1),&hFF,0) 
'                        valorb=IIf((valor And 2),&hFF,0) 
'                        valorc=IIf((valor And 4),&hFF,0) 
'                        valord=IIf((valor And 8),&hFF,0) 
'                        Select Case As Const (gdcreg(3) And &h18)
'                        	Case 0  /'Set'/
'                                if (writemask2 And 1) Then vram[addr]       =(valora And gdcreg(8)) Or (la And inv(gdcreg(8))) 
'                                if (writemask2 And 2) Then vram[addr Or &h1]=(valorb And gdcreg(8)) Or (lb And inv(gdcreg(8))) 
'                                if (writemask2 And 4) Then vram[addr Or &h2]=(valorc And gdcreg(8)) Or (lc And inv(gdcreg(8))) 
'                                if (writemask2 And 8) Then vram[addr Or &h3]=(valord And gdcreg(8)) Or (ld And inv(gdcreg(8))) 
'                                 
'                        	Case 8  /'AND'/
'                                if (writemask2 And 1) Then vram[addr]       =(valora Or inv(gdcreg(8))) And la 
'                                if (writemask2 And 2) Then vram[addr Or &h1]=(valorb Or inv(gdcreg(8))) And lb 
'                                if (writemask2 And 4) Then vram[addr Or &h2]=(valorc Or inv(gdcreg(8))) And lc 
'                                if (writemask2 And 8) Then vram[addr Or &h3]=(valord Or inv(gdcreg(8))) And ld 
'                                 
'                        	Case &h10  /'OR'/
'                                if (writemask2 And 1) Then vram[addr]       =(valora And gdcreg(8)) Or la 
'                                if (writemask2 And 2) Then vram[addr Or &h1]=(valorb And gdcreg(8)) Or lb 
'                                if (writemask2 And 4) Then vram[addr Or &h2]=(valorc And gdcreg(8)) Or lc 
'                                if (writemask2 And 8) Then vram[addr Or &h3]=(valord And gdcreg(8)) Or ld 
'                                 
'                        	Case &h18  /'XOR'/
'                                if (writemask2 And 1) Then vram[addr]       =(valora And gdcreg(8)) Xor la 
'                                if (writemask2 And 2) Then vram[addr Or &h1]=(valorb And gdcreg(8)) Xor lb 
'                                if (writemask2 And 4) Then vram[addr Or &h2]=(valorc And gdcreg(8)) Xor lc 
'                                if (writemask2 And 8) Then vram[addr Or &h3]=(valord And gdcreg(8)) Xor ld 
'                                 
'                        End Select
'                EndIf
'                 
'        	Case 3 
'                if (gdcreg(3) And 7) Then valor=svga_rotate(gdcreg(3) And 7, valor) 
'                wm=gdcreg(8) 
'                gdcreg(8) = gdcreg(8) And valor 
'                valora=IIf((gdcreg(0) And 1),&hFF,0) 
'                valorb=IIf((gdcreg(0) And 2),&hFF,0) 
'                valorc=IIf((gdcreg(0) And 4),&hFF,0) 
'                valord=IIf((gdcreg(0) And 8),&hFF,0) 
'                Select Case As Const (gdcreg(3) And &h18)
'                	Case 0  /'Set'/
'                        if (writemask2 And 1) Then vram[addr]=(valora And gdcreg(8)) Or (la And inv(gdcreg(8))) 
'                        if (writemask2 And 2) Then vram[addr Or &h1]=(valorb And gdcreg(8)) Or (lb And inv(gdcreg(8))) 
'                        if (writemask2 And 4) Then vram[addr Or &h2]=(valorc And gdcreg(8)) Or (lc And inv(gdcreg(8))) 
'                        if (writemask2 And 8) Then vram[addr Or &h3]=(valord And gdcreg(8)) Or (ld And inv(gdcreg(8))) 
'                         
'                	Case 8  /'AND'/
'                        if (writemask2 And 1) Then vram[addr]=(valora Or inv(gdcreg(8))) And la 
'                        if (writemask2 And 2) Then vram[addr Or &h1]=(valorb Or inv(gdcreg(8))) And lb 
'                        if (writemask2 And 4) Then vram[addr Or &h2]=(valorc Or inv(gdcreg(8))) And lc 
'                        if (writemask2 And 8) Then vram[addr Or &h3]=(valord Or inv(gdcreg(8))) And ld 
'                         
'                	Case &h10  /'OR'/
'                        if (writemask2 And 1) Then vram[addr]=(valora And gdcreg(8)) Or la 
'                        if (writemask2 And 2) Then vram[addr Or &h1]=(valorb And gdcreg(8)) Or lb 
'                        if (writemask2 And 4) Then vram[addr Or &h2]=(valorc And gdcreg(8)) Or lc 
'                        if (writemask2 And 8) Then vram[addr Or &h3]=(valord And gdcreg(8)) Or ld 
'                         
'                	Case &h18  /'XOR'/
'                        if (writemask2 And 1) Then vram[addr]=(valora And gdcreg(8)) Xor la 
'                        if (writemask2 And 2) Then vram[addr Or &h1]=(valorb And gdcreg(8)) Xor lb 
'                        if (writemask2 And 4) Then vram[addr Or &h2]=(valorc And gdcreg(8)) Xor lc 
'                        if (writemask2 And 8) Then vram[addr Or &h3]=(valord And gdcreg(8)) Xor ld 
'                         
'                End Select
'                gdcreg(8)=wm 
'                 
'        End Select
'end Sub
'
'Function svga_read_linear(ByVal addr As ULong ) As UByte 
'        Dim As UByte  temp,temp2,temp3,temp4 
'        
'        cycles -= video_timing_b 
'        cycles_lost += video_timing_b 
'        egareads+=1 
'        if (chain4) Then 
'                addr = addr And &h7fffff 
'                if (addr >= svga_vram_limit) Then return &hff 
'                return vram[addr And &h7fffff] 
'        else
'                addr Shl =2
'        EndIf
'        addr = addr And &h7fffff 
'        if (addr >= svga_vram_limit) Then return &hff 
'        la=vram[addr] 
'        lb=vram[addr Or &h1] 
'        lc=vram[addr Or &h2] 
'        ld=vram[addr Or &h3] 
'        if (readmode) Then 
'                temp  = IIf((colournocare And 1) ,&hFF,0) 
'                temp  = temp  And la 
'                temp  = IIf(temp  Xor (colourcompare And 1),&hFF,0) 
'                temp2 = IIf((colournocare And 2) ,&hFF,0) 
'                temp2 = temp2 And lb 
'                temp2 = IIf(temp2 Xor (colourcompare And 2),&hFF,0) 
'                temp3 = IIf((colournocare And 4) ,&hFF,0) 
'                temp3 = temp3 And lc 
'                temp3 = IIf(temp3 Xor (colourcompare And 4),&hFF,0) 
'                temp4 = IIf((colournocare And 8) ,&hFF,0) 
'                temp4 = temp4 And ld 
'                temp4 = IIf(temp4 Xor (colourcompare And 8),&hFF,0) 
'                return inv(temp Or temp2 Or temp3 Or temp4) 
'        EndIf
'        return vram[addr Or readplane] 
'End Function
'