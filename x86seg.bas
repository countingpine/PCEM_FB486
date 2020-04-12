


' estas variables son para permitir a WIN98 funcionar, segun el fuente original, pero sin probar.
const SEL_ACCESSED=1
const CS_ACCESSED=1



Sub x86_doabrt(x86_abrt As Integer ) 
        CS1 = oldcs 
        pc = oldpc 
        _cs.access0 = oldcpl Shl 5 

		  'Print #5, "PRIMER fallo tipo:";x86_abrt;" gatesize=";intgatesize;" stack32:";stack32
		  
        pmodeint(x86_abrt, 0) 
        
        If abrt then Return   
        if (intgatesize = 16) Then 
                if stack32 Then 
                        writememw_x86(ss0, ESP-2, abrt_error) 
                        ESP-=2 
                else
                        writememw_x86(ss0, ((SP-2) And &hFFFF), abrt_error) 
                        SP-=2 
                EndIf
        Else
                if stack32 Then 
                        writememl_x86(ss0, ESP-4, abrt_error) 
                        ESP-=4 
                else
                        writememl_x86(ss0, ((SP-4) And &hFFFF), abrt_error) 
                        SP-=4 
                EndIf
        EndIf
End Sub

Sub x86gpf(fallo As UShort ) 
        abrt = ABRT_GPF 
        abrt_error = fallo
End Sub

Sub x86ss(fallo As UShort ) 
        abrt = ABRT_SS 
        abrt_error = fallo 
End Sub

Sub x86ts(fallo As UShort ) 
        abrt = ABRT_TS 
        abrt_error = fallo 
End Sub

Sub x86np(fallo As UShort )
        abrt = ABRT_NP 
        abrt_error = fallo 
End Sub


Sub PUSHW(v As UShort ) 
        if stack32 Then 
                writememw_386(ss0,ESP-2,v) 
                if abrt then Exit Sub   
                ESP-=2 
        else
                writememw_386(ss0,((SP-2) And &hFFFF),v) 
                if abrt then Exit sub   
                SP-=2 
        EndIf
End Sub

Sub PUSHL(v As ULong ) 
        if stack32 Then 
                writememl_386(ss0,ESP-4,v) 
                if abrt then return   
                ESP-=4 
        Else
                writememl_386(ss0,((SP-4) And &hFFFF),v) 
                if abrt then return   
                SP-=4 
        EndIf
End Sub

Function POPW() As UShort 
        Dim As UShort tempw =any
        if stack32 Then 
                tempw=readmemw_386(ss0,ESP) 
                if abrt then return 0 
                ESP+=2 
        else
                tempw=readmemw_386(ss0,SP) 
                if abrt then return 0 
                SP+=2 
        EndIf
        return  tempw 
End Function

Function POPL() As ULong 
        Dim As ULong  templ =any
        if stack32 Then 
                templ=readmeml_386(ss0,ESP) 
                if abrt then return 0 
                ESP+=4 
        else
                templ=readmeml_386(ss0,SP) 
                if abrt then return 0 
                SP+=4 
        EndIf
        Return  templ 
End Function










' nota: esta rutina usa la estructura "x86seg" para la variable "S" y TOCA los valores , o sea, "S" sale cambiado
Sub loadseg(seg As UShort ,s As x86seg) 
        Dim As UShort segdat(4) =Any
        Dim As ULong addr =Any
        Dim As long dpl0=Any

        if (modoprotegido=1) And ((eflags And VM_FLAG)=0) Then 

       			 ''if deb=5 Then Print #5,"Load seg:";Hex(seg,4)
       			 
                if (seg And inv(3))=0 Then 
                        if @s=@_ss Then 
                            'Print #5,"SS selector = NULO" 
                            x86ss(0) 
                            Exit sub  
                        EndIf
                        s.seg=0 
                        s.base0=-1 
                        Exit sub  
                EndIf
                addr=seg And inv(7)
                if (seg And 4) Then 
                        if (addr>=ldt.limit) Then 
                            'print #5,"Bigger than LDT limit ",seg,ldt.limit, opcode, opcode2, rmdat
                            x86gpf(seg And inv(3)) 
                            Exit sub  
                        EndIf
                        addr+=ldt.base0 
                        'Print #5,"la hosta:";Hex(addr),Hex(ldt.base0)
                Else
                        If (addr>=gdt.limit) Then 
                            'print #5,"Bigger than GDT limit ",seg,gdt.limit 
                            x86gpf(seg And inv(3)) 
                            Exit sub  
                        EndIf
                        addr+=gdt.base0 
                EndIf
        
                cpl_override=1 
                'print #5,"leyendo SEGCS de la dir:";Hex(addr,8)
                segdat(0)=readmemw_386(0,addr) 
                segdat(1)=readmemw_386(0,addr+2) 
                segdat(2)=readmemw_386(0,addr+4) 
                segdat(3)=readmemw_386(0,addr+6)
                'print #5,"contenido:";Hex(segdat(0),4);" ";Hex(segdat(1),4);" ";Hex(segdat(2),4);" ";Hex(segdat(3),4)
                cpl_override=0
                if abrt then return   

                dpl0=(segdat(2) Shr 13) And 3 
                if @s=@_ss Then 
                        If (seg And inv(3))=0 Then
                            Print #5,"Load SS NULL selector" 
                            x86gpf(seg And inv(3)) 
                            Exit sub  
                        EndIf
                        If ((seg And 3)<>CPL)  Or  (dpl0<>CPL) Then 
                            Print #5,"Permisos SS error dir:";Hex(addr),seg And 3,Hex(dpl0),Hex(CPL)
                            x86gpf(seg And inv(3)) 
                            Exit sub  
                        EndIf
                        Select Case As Const(segdat(2) Shr 8) And &h1F
                        	Case &h12, &h13, &h16, &h17  /'r-w'/
                                Exit Select 
                        	case Else 
                             Print #5,"Invalid SS tipo"
                             x86gpf(seg And inv(3)) 
                             Exit sub  
                        End Select
                        If (segdat(2) And &h8000)=0 Then 
                            Print #5,"Load SS not present" 
                            x86ss(seg And inv(3)) 
                            Exit sub  
                        EndIf
                        if (segdat(3) And &h40) Then 
                        	stack32=1 
                        else
                           stack32=0 
                        EndIf
                ElseIf @s<>@_cs Then 
                			''if deb=5 Then print #5,"Seg data ", segdat(0), segdat(1), segdat(2), segdat(3) 
                        ''if deb=5 Then print #5,"Seg tipo ",segdat(2) And &h1F00
                        Select Case As Const((segdat(2) Shr 8) And &h1F)
                        	 /'Data segments'/
                        	Case &h10, &h11, &h12, &h13 , _
                                &h14, &h15, &h16, &h17 , _ 
                                &h1A, &h1B  /'Readable non-conforming code'/
                                if ((seg And 3)>dpl0)  Or  (CPL>dpl0) Then 
                                    'Print #5,"Data seg fail ",CS1,pc,seg,dpl0,segdat(2)
                                    x86gpf(seg And inv(3)) 
                                    Exit sub  
                                EndIf
                                Exit Select 
                        	Case &h1E, &h1F  /'Readable conforming code'/
                                Exit Select 
                        	Case else 
                             'print #5,"Invalid segment tipo ",seg And &hFFFC,segdat(2) 
                             x86gpf (seg And inv(3))
                             Exit sub  
                        End Select
                EndIf
                
					if (segdat(2) And &h8000)=0 Then 
					    ''if deb=5 Then Print #5,"Load data seg not present "
					    x86np(seg  And &hfffc)
					    Exit sub  
					EndIf
					
					s.seg=seg 
					s.limit=segdat(0) Or ((segdat(3) And &hF) Shl 16) 
					if (segdat(3) And &h80) Then s.limit=(s.limit Shl 12) Or &hFFF 
					s.limitw=IIf((segdat(2) And &h200),1,0 )
					s.base0 = segdat(1) 
					s.base0 = s.base0 Or ((segdat(2) And &hFF) Shl 16) 
					s.base0 = s.base0 Or ((segdat(3) Shr 8) Shl 24) ' antes --> if (is386) Then 
					s.access0=segdat(2) Shr 8 
					if ((segdat(2) Shr 8)  And 4) Then s.limit = &hffffffff 

' --------------------------------------------------------------------------------                      
If CS_ACCESSED=0 And SEL_ACCESSED=1 Then 
         if @s<>@_cs Then 
                 cpl_override = 1 
                 writememw_x86(0, addr+4, segdat(2)  Or  &h100) /'Set accessed bit'/
                 cpl_override = 0 
         EndIf
ElseIf SEL_ACCESSED=1 then  
          cpl_override = 1 
          writememw_x86(0, addr+4, segdat(2)  Or  &h100) /'Set accessed bit'/
          cpl_override = 0 
EndIf
' --------------------------------------------------------------------------------


        else
                s.base0=seg Shl 4
                s.limit=&hFFFF 
                s.seg=seg 
                if (eflags And VM_FLAG) Then 
                        s.access0=3 Shl 5 
                else
                        s.access0=0' Shl 5 ' 0 shl 5=0!!!
                EndIf
                use32=0 
                if (@s=@_ss) Then stack32=0 

        EndIf
End Sub







Sub loadcs(seg As UShort) 
        Dim As UShort  segdat(4) =Any
        Dim As ULong   addr =Any
        Dim As Integer count =Any
        Dim As UShort  oldss,oldsp =Any


        if (modoprotegido=1) And ((eflags And VM_FLAG)=0) Then
        	
        	       'if deb=5 Then Print #5,"Load CS ",seg
        	       
                if (seg And inv(3))=0 Then 
                        'if deb=5 Then print #5,"Trying to load CS with NULO selector lcs" 
                        x86gpf(0) 
                        Exit sub  
                EndIf
                addr=seg And inv(7)
                if (seg And 4) Then 
                        if addr>=ldt.limit Then 
                                'if deb=5 Then print #5,"Bigger than LDT limit   CS",seg,ldt.limit 
                                x86gpf(seg And inv(3)) 
                                Exit sub  
                        EndIf
                        addr+=ldt.base0 
                else
                        if addr>=gdt.limit Then 
                                'if deb=5 Then print #5,"Bigger than GDT limit   CS",seg,gdt.limit
                                x86gpf(seg And inv(3)) 
                                Exit sub  
                        EndIf
                        addr+=gdt.base0 
                EndIf
                
                cpl_override=1 
                segdat(0)=readmemw_386(0,addr) 
                segdat(1)=readmemw_386(0,addr+2) 
                segdat(2)=readmemw_386(0,addr+4) 
                segdat(3)=readmemw_386(0,addr+6)
                cpl_override=0
                if abrt then Exit Sub   
                
                If (optype=JMP) Then print #5,"JMP Code seg - ",seg,segdat(0),segdat(1),segdat(2),segdat(3) 

                if (segdat(2) And &h1000) Then /'Normal code segment'/
                        if (segdat(2) And &h400)=0 Then  /'not conforming'/
                                if ((seg And 3)>CPL) Then 
                                        x86gpf(seg And inv(3)) 
                                        'if deb=5 Then print #5,"loadcs RPL > CPL ",segdat(2),seg,CPL,opcode
                                        Exit sub  
                                EndIf
                                if (CPL <> DPL) Then 
                                        x86gpf(seg And inv(3)) 
                                        Exit sub  
                                EndIf
                        EndIf
                        if CPL < DPL Then 
                                x86gpf(seg And inv(3)) 
                                Exit sub  
                        EndIf
                        if (segdat(2) And &h8000)=0 Then 
                                'if deb=5 Then print #5,"Load CS not present"
                                x86np(seg  And &hfffc)
                                Exit sub  
                        EndIf
                        if (segdat(3) And &h40) Then 
                                use32=&h300 
                        else
                                use32=0
                        EndIf
                        CS1=(seg And inv(3)) Or CPL 
                        _cs.limit=segdat(0) Or ((segdat(3) And &hF) Shl 16) 
                        if (segdat(3) And &h80) Then _cs.limit=(_cs.limit Shl 12) Or &hFFF 
                        _cs.base0 = segdat(1) 
                        _cs.base0 = _cs.base0 Or ((segdat(2) And &hFF) Shl 16) 
                        _cs.base0 = _cs.base0 Or ((segdat(3) Shr 8) Shl 24) ' antes ---> if (is386) Then 
                        _cs.access0=segdat(2) Shr 8 
                        use32=IIf((segdat(3) And &h40),&h300,0) 
                        if (CPL=3)  And  (oldcpl<>3) Then flushmmucache_cr3() 
' --------------------------------------------------------------------------------                        
if  CS_ACCESSED Then                        
      cpl_override = 1 
      writememw_x86(0, addr+4, segdat(2)  Or  &h100) /'Set accessed bit'/
      cpl_override = 0 
EndIf
' --------------------------------------------------------------------------------
                else
                        /'System segment'/
                        if ((segdat(2) And &h8000)=0) Then 
                                'if deb=5 Then print #5,"Load CS system seg not present "
                                x86np(seg  And &hfffc)
                                Exit sub  
                        EndIf
                        'Select Case(segdat(2) And &hF00)
                        '	case Else 
                                'if deb=5 Then print #5,"Bad CS special descriptor ",opcode,rmdat,optype,segdat(2) And &hF00,seg 
                                x86gpf(seg And inv(3)) 
                                Exit sub  
                        'End Select
                EndIf
        else
                _cs.base0=seg Shl 4 
                _cs.limit=&hFFFF 
                CS1=seg 
                if (eflags And VM_FLAG) Then 
                        _cs.access0=3 Shl 5 
                else
                        _cs.access0=0 ' 0 Shl 5 es 0!!!
                EndIf
                if (CPL=3)  And  (oldcpl<>3) Then flushmmucache_cr3() 
        EndIf
End Sub

Sub loadcsjmp(seg As UShort , oxpc As ULong ) 
        Dim As UShort  segdat(4) =Any
        Dim As ULong   addr =Any
        Dim As Integer count =Any
        Dim As UShort  oldss=Any,oldsp =any
        Dim As UShort  tipo=any,seg2 =Any
        Dim As ULong   newpc =Any
 
        If (modoprotegido=1) And ((eflags And VM_FLAG)=0) Then 
         	       
                'if deb=5 Then Print #5,"Load CS JMP revisar:",Hex(seg),Hex(oxpc)

                if (seg And inv(3))=0 Then 
                        'if deb=5 Then print #5,"Trying to load CS with NULO selector lcsjmp" 
                        x86gpf(0) 
                        Exit sub  
                EndIf
                addr=seg And inv(7)
                if (seg And 4) Then 
                        if (addr>=ldt.limit) Then 
                                'if deb=5 Then print #5,"Bigger than LDT limit CS",seg,ldt.limit 
                                x86gpf(seg And inv(3)) 
                                Exit sub  
                        EndIf
                        addr+=ldt.base0 
                Else
                        if (addr>=gdt.limit) Then 
                                'if deb=5 Then print #5,"Bigger than GDT limit CS",seg,gdt.limit 
                                x86gpf(seg And inv(3)) 
                                Exit sub  
                        EndIf
                        addr+=gdt.base0 
                EndIf

                cpl_override=1 
                segdat(0)=readmemw_386(0,addr) 
                segdat(1)=readmemw_386(0,addr+2) 
                segdat(2)=readmemw_386(0,addr+4) 
                segdat(3)=readmemw_386(0,addr+6)
                cpl_override=0
                if abrt then return   
                
                'Print #5,"normal code seg:";Hex(segdat(0),8);"-";Hex(segdat(1),8);"-";Hex(segdat(2),8);"-";Hex(segdat(3),8) 
                if (segdat(2) And &h1000) Then /'Normal code segment'/
                        if (segdat(2) And &h400)=0 Then /'not conforming'/
                                if ((seg And 3)>CPL) Then 
                                        x86gpf(seg And inv(3)) 
                                        Exit sub  
                                EndIf
                                if (CPL <> DPL) Then 
                                        x86gpf(seg And inv(3)) 
                                        Exit sub  
                                EndIf
                        EndIf
                        if CPL < DPL Then 
                                x86gpf(seg And inv(3)) 
                                Exit sub  
                        EndIf
                        if (segdat(2) And &h8000)=0 Then 
                                'if deb=5 Then print #5,"Load CS JMP not present "
                                x86np(seg  And &hfffc) 
                                Exit sub  
                        EndIf
                        use32=IIf((segdat(3) And &h40),&h300,0) 
                       
' --------------------------------------------------------------------------------                        
if CS_ACCESSED Then                        
         cpl_override = 1 
         writememw_x86(0, addr+4, segdat(2)  Or  &h100) /'Set accessed bit'/
         cpl_override = 0 
EndIf
' --------------------------------------------------------------------------------
                        CS1 = (seg  And inv(3))  Or  CPL 
                        segdat(2) = (segdat(2)  And inv(3  Shl  (5+8)))  Or  (CPL  Shl  (5+8)) 
                        _cs.limit=segdat(0) Or ((segdat(3) And &hF) Shl 16) 
                        if (segdat(3) And &h80) Then _cs.limit=(_cs.limit Shl 12) Or &hFFF 
                        _cs.base0 = segdat(1) 
                        _cs.base0 = _cs.base0 Or ((segdat(2) And &hFF) Shl 16) 
                        _cs.base0 = _cs.base0 Or ((segdat(3) Shr 8) Shl 24)  ' antes ---> if (is386) Then 
                        _cs.access0=segdat(2) Shr 8 
                        if (CPL=3)  And  (oldcpl<>3) Then flushmmucache_cr3() 
                        use32=IIf((segdat(3) And &h40),&h300,0)
                        'Print #5,"normal code seg 2: ";Hex(_cs.limit),Hex(_cs.base0),Hex(_cs.access0), Hex(use32)
                Else
                        /'System segment'/
                        if (segdat(2) And &h8000)=0 Then 
                                'if deb=5 Then print #5,"Load CS JMP system selector not present "
                                x86np(seg  And &hfffc)
                                Exit sub  
                        EndIf
                        tipo=segdat(2) And &hF00 
                        if (tipo=&h400) Then 
                                newpc=segdat(0) 
                        else
                                newpc=segdat(0) Or (segdat(3) Shl 16)
                        EndIf
                        Select Case As Const(tipo)
                        	Case &h400, &hC00  /'Call gate'/
                                cgate32=(tipo And &h800) 
                                ' al parecer, en C, "!CGATE" (o sea, NOT), vale 0 si CGATE<>0, y 1 si CGATE=0 !!!!!
                                cgate16= iif(cgate32,0,1) 
                                'Print #5,"estudiar NOT CGAGATE (hay dos), por que igual folla"
                                oldcs=CS1 
                                oldpc=pc 
                                count=segdat(2) And 31 
                                if (DPL < CPL)  Or  (DPL < (seg And 3)) Then 
                                        x86gpf(seg And inv(3)) 
                                        return  
                                EndIf
                                if (segdat(2) And &h8000)=0 Then 
                                        'if deb=5 Then print #5,"Load CS JMP call gate  not present "
                                        x86np(seg  And &hfffc) 
                                        Return 
                                EndIf
                                seg2=segdat(1) 
                                if (seg2 And inv(3))=0 Then 
                                        'if deb=5 Then print #5,"Trying to load CS with NULO selector lcsjmpcg" 
                                        x86gpf(0) 
                                        return  
                                EndIf
                                addr=seg2 And inv(7)
                                if (seg2 And 4) Then 
                                        if (addr>=ldt.limit) Then 
                                                'if deb=5 Then print #5,"Bigger than LDT limit   CSJ",seg2,gdt.limit 
                                                x86gpf(seg2 And inv(3)) 
                                                return  
                                        EndIf
                                        addr+=ldt.base0 
                                else
                                        if (addr>=gdt.limit) Then 
                                                'if deb=5 Then print #5,"Bigger than GDT limit   CSJ",seg2,gdt.limit 
                                                x86gpf(seg2 And inv(3)) 
                                                Exit sub  
                                        EndIf
                                        addr+=gdt.base0 
                                EndIf
                                
                                cpl_override=1 
                                segdat(0)=readmemw_386(0,addr) 
                                segdat(1)=readmemw_386(0,addr+2) 
                                segdat(2)=readmemw_386(0,addr+4) 
                                segdat(3)=readmemw_386(0,addr+6)
                                cpl_override=0
                                if abrt Then return   
                                
                                if (DPL > CPL) Then 
                                        x86gpf(seg2 And inv(3)) 
                                        Return  
                                EndIf
                                if (segdat(2) And &h8000)=0 Then 
                                        'if deb=5 Then print #5,"Load CS JMP from call gate notpresent"
                                        x86np(seg  And &hfffc) 
                                        Return  
                                EndIf
                                Select Case As Const (segdat(2) And &h1F00)
                                	 Case &h1800, &h1900, &h1A00, &h1B00  /'Non-conforming code'/
                                        if (DPL > CPL) Then 
                                                'if deb=5 Then print #5,"Call gate DPL > CPL" 
                                                x86gpf(seg2 And inv(3)) 
                                                Return  
                                        EndIf
' --------------------------------------------------------------------------------
' grupo repetido como en el "CASE &h1C00, &h1D00, &h1E00, &h1F00", por que no se ejecuta en basic
                                        CS1=seg2 
                                        _cs.limit=segdat(0) Or ((segdat(3) And &hF) Shl 16) 
                                        if (segdat(3) And &h80) Then _cs.limit=(_cs.limit Shl 12) Or &hFFF 
                                        _cs.base0 =segdat(1) 
                                        _cs.base0 = _cs.base0 Or ((segdat(2) And &hFF) Shl 16) 
                                        _cs.base0 = _cs.base0 Or ((segdat(3) Shr 8) Shl 24)  ' antes ---> if (is386) Then 
                                        _cs.access0=segdat(2) Shr 8 
                                        if (CPL=3)  And  (oldcpl<>3) Then flushmmucache_cr3() 
                                        use32=IIf((segdat(3) And &h40),&h300,0) 
                                        pc=newpc 
			' --------------------------------------------------------------------------------                                        
			if  CS_ACCESSED Then                                                
			                cpl_override = 1 
			                writememw_x86(0, addr+4, segdat(2)  Or  &h100) /'Set accessed bit'/
			                cpl_override = 0 
			EndIf
' --------------------------------------------------------------------------------
                                        
                                	 Case &h1C00, &h1D00, &h1E00, &h1F00  /'Conforming'/
                                        CS1=seg2 
                                        _cs.limit=segdat(0) Or ((segdat(3) And &hF) Shl 16) 
                                        if (segdat(3) And &h80) Then _cs.limit=(_cs.limit Shl 12) Or &hFFF 
                                        _cs.base0 =segdat(1) 
                                        _cs.base0 = _cs.base0 Or ((segdat(2) And &hFF) Shl 16) 
                                        _cs.base0 = _cs.base0 Or ((segdat(3) Shr 8) Shl 24)  ' antes ---> if (is386) Then 
                                        _cs.access0=segdat(2) Shr 8 
                                        if (CPL=3)  And  (oldcpl<>3) Then flushmmucache_cr3() 
                                        use32=IIf((segdat(3) And &h40),&h300,0) 
                                        pc=newpc 
' --------------------------------------------------------------------------------                                        
if  CS_ACCESSED Then                                                
                cpl_override = 1 
                writememw_x86(0, addr+4, segdat(2)  Or  &h100) /'Set accessed bit'/
                cpl_override = 0 
EndIf
' --------------------------------------------------------------------------------
                                        Exit Select 
                                	 Case Else 
                                        'if deb=5 Then print #5,"JMP Call gate bad segment tipo" 
                                        x86gpf(seg2 And inv(3)) 
                                        Return  
                                End Select

                        	Case &h900  /'386 Task gate'/
                                pc=oxpc 
                                cpl_override=1 
                                '------------------------------------------------
                                taskswitch286(seg,segdat(),segdat(2) And &h800) 
                                '------------------------------------------------
                                cpl_override=0 
                                Return  
                                
                        	case Else 
                                'if deb=5 Then print #5,"Bad JMP CS special descriptor ",opcode,rmdat,optype,segdat(2) And &hF00,seg 
                                x86gpf(0) 
                                Return  
                                
                        End Select
                EndIf
        Else
                _cs.base0=seg Shl 4 
                _cs.limit=&hFFFF 
                CS1=seg 
                if (eflags And VM_FLAG) Then 
                        _cs.access0=3 Shl 5 
                else
                        _cs.access0=0 ' 0 Shl 5 es 0!!!
                EndIf
                if (CPL=3)  And  (oldcpl<>3) Then flushmmucache_cr3() 
        EndIf
End Sub

Sub loadcscall(seg As UShort ) 
        Dim As UShort seg2 =Any
        Dim As UShort segdat(4)=any,segdat2(4)=any,newss=Any 
        Dim As ULong  addr=Any,oldssbase=ss0, oaddr =any
        Dim As ULong  newpc=Any 
        Dim As Integer  count=Any
        Dim As UShort oldcs=CPL 
        Dim As ULong  oldss=Any,oldsp=any,newsp=any,oldpc=any, oldsp2=Any 
        Dim As Integer  tipo=Any 
        Dim As UShort tempw =Any
        
        if (modoprotegido=1) And ((eflags And VM_FLAG)=0) Then 
        	
                'if deb=5 Then Print #5,"LOAD_CS_CALL revisar",seg 
                
                if (seg And inv(3))=0 Then 
                        'if deb=5 Then print #5,"Trying to load CS with NULO selector lcscall" 
                        x86gpf(0) 
                        Return  
                EndIf
                addr=seg And inv(7)
                if (seg And 4) Then 
                        if (addr>=ldt.limit) Then 
                                'if deb=5 Then print #5,"Bigger than LDT limit   CSC",seg,gdt.limit
                                x86gpf(seg And inv(3)) 
                                Return  
                        EndIf
                        addr+=ldt.base0 
                else
                        if (addr>=gdt.limit) Then 
                                'if deb=5 Then print #5,"Bigger than GDT limit   CSC",seg,gdt.limit
                                x86gpf(seg And inv(3)) 
                                Return  
                        EndIf
                        addr+=gdt.base0 
                EndIf
                
                cpl_override=1 
                segdat(0)=readmemw_386(0,addr) 
                segdat(1)=readmemw_386(0,addr+2)
                segdat(2)=readmemw_386(0,addr+4)
                segdat(3)=readmemw_386(0,addr+6)
                cpl_override=0
                if abrt then Return  
                 
                tipo=segdat(2) And &hF00 
                
                if (tipo=&h400) Then 
                        newpc=segdat(0) 
                else
                        newpc=segdat(0) Or (segdat(3) Shl 16)
                EndIf
                
                'if deb=5 Then print #5,"Code seg call ",seg,segdat(0),segdat(1),segdat(2) 

                if (segdat(2) And &h1000) Then 
                        if (segdat(2) And &h400)=0 Then /'not conforming'/
                                if (seg And 3)>CPL Then 
                                        'if deb=5 Then print #5,"not conforming, RPL > CPL " 
                                        x86gpf(seg And inv(3)) 
                                        Return  
                                EndIf
                                if (CPL <> DPL) Then 
                                        'if deb=5 Then print #5,"not conforming, CPL <> DPL ",CPL,DPL 
                                        x86gpf(seg And inv(3)) 
                                        Return  
                                EndIf
                        EndIf
                        if (CPL < DPL) Then 
                                'if deb=5 Then print #5,"CPL < DPL" 
                                x86gpf(seg And inv(3)) 
                                Return  
                        EndIf
                        if (segdat(2) And &h8000)=0 Then 
                                'if deb=5 Then print #5,"Load CS call not present"
                                x86np(seg  And &hfffc)
                                return  
                        EndIf
                        use32=IIf((segdat(3) And &h40),&h300,0)
' --------------------------------------------------------------------------------                        
if  CS_ACCESSED Then                        
         cpl_override = 1 
         writememw_x86(0, addr+4, segdat(2)  Or  &h100) /'Set accessed bit'/
         cpl_override = 0 
EndIf
' --------------------------------------------------------------------------------
                        'Conforming segments don't change CPL, so preserve existing CPL
                        if (segdat(2) And &h400) Then 
                                seg = (seg  And inv(3))  Or  CPL 
                                segdat(2) = (segdat(2)  And inv(3  Shl  (5+8)))  Or  (CPL  Shl  (5+8)) 
                        else
                                /'On non-conforming segments, set RPL = CPL'/
                                seg = (seg  And inv(3))  Or  CPL
                        EndIf
                        CS1=seg 
                        _cs.limit=segdat(0) Or ((segdat(3) And &hF) Shl 16) 
                        if (segdat(3) And &h80) Then _cs.limit=(_cs.limit Shl 12) Or &hFFF 
                        _cs.base0 = segdat(1) 
                        _cs.base0 = _cs.base0 Or ((segdat(2) And &hFF) Shl 16) 
                        _cs.base0 = _cs.base0 Or ((segdat(3) Shr 8) Shl 24)  ' antes ---> if (is386) Then 
                        _cs.access0=segdat(2) Shr 8 
                        if (CPL=3)  And  (oldcpl<>3) Then flushmmucache_cr3() 
                        use32=IIf((segdat(3) And &h40),&h300,0)
                        'if deb=5 Then print #5,"load cs Complete!" 
                else
                        tipo=segdat(2) And &hF00 
                        'if deb=5 Then print #5,"load cs tipo ",tipo 
                        Select Case As Const(tipo)
                        	case &h400, &hC00  /'Call gate'/ /'386 Call gate'/
                                'if deb=5 Then print #5,"Callgate ", pc 
                                cgate32=(tipo And &h800) 
                                ' al parecer, en C, "!CGATE" (o sea, NOT), vale 0 si CGATE<>0, y 1 si CGATE=0 !!!!!
                                cgate16= iif(cgate32,0,1) 
                                'Print #5,"estudiar NOT CGAGATE (hay dos), por que igual folla"
                                oldcs=CS1 
                                oldpc=pc 
                                count=segdat(2) And 31 
                                if (DPL < CPL)  Or  (DPL < (seg And 3)) Then 
                                        x86gpf(seg And inv(3)) 
                                        Exit sub  
                                EndIf
                                if (segdat(2) And &h8000)=0 Then 
                                    'if deb=5 Then print #5,"Call gate not present "
                                    x86np(seg  And &hfffc)
                                    Exit sub  
                                EndIf
                                seg2=segdat(1) 
                                'if deb=5 Then print #5,"New address  ", seg2, newpc 
                                if (seg2 And inv(3))=0 Then 
                                    'if deb=5 Then print #5,"Trying to load CS with NULO selector lcscallcg" 
                                    x86gpf(0) 
                                    Exit sub  
                                EndIf
                                addr=seg2 And inv(7)
                                if (seg2 And 4) Then 
                                        if (addr>=ldt.limit) Then 
                                                'if deb=5 Then print #5,"Bigger than LDT limit   CSC ",seg2,gdt.limit 
                                                x86gpf(seg2 And inv(3)) 
                                                Exit sub  
                                        EndIf
                                        addr+=ldt.base0 
                                else
                                        if (addr>=gdt.limit) Then 
                                                'if deb=5 Then print #5,"Bigger than GDT limit   CSC ",seg2,gdt.limit 
                                                x86gpf(seg2 And inv(3)) 
                                                Exit sub  
                                        EndIf
                                        addr+=gdt.base0 
                                EndIf
                                
                                cpl_override=1 
                                segdat(0)=readmemw_386(0,addr) 
                                segdat(1)=readmemw_386(0,addr+2) 
                                segdat(2)=readmemw_386(0,addr+4) 
                                segdat(3)=readmemw_386(0,addr+6)
                                cpl_override=0
                                if abrt Then return  
                                 
                                'if deb=5 Then print #5,"Code seg2 call ",seg2,segdat(0),segdat(1),segdat(2) 
                                if (DPL > CPL) Then 
                                        x86gpf(seg2 And inv(3)) 
                                        Exit sub  
                                EndIf
                                if (segdat(2) And &h8000)=0 Then 
                                    'if deb=5 Then print #5,"Call gate CS not present"
                                    x86np(seg2  And &hfffc) 
                                    Exit sub  
                                EndIf
                                Select Case As Const(segdat(2) And &h1F00)
                                	Case &h1800, &h1900, &h1A00, &h1B00  /'Non-conforming code'/
                                        if (DPL < CPL) Then 
                                                oaddr = addr 
                                                /'Load new stack'/
                                                oldss=SS1 
                                                oldsp=ESP
                                                oldsp2=ESP 
                                                cpl_override=1 
                                                if (tr.access0 And 8) Then
                                                        addr = 4 + tr.base0 + (DPL * 8) 
                                                        newss=readmemw_386(0,addr+4) 
                                                        newsp=readmeml_386(0,addr) 
                                                else
                                                        addr = 2 + tr.base0 + (DPL * 4) 
                                                        newss=readmemw_386(0,addr+2) 
                                                        newsp=readmemw_386(0,addr) 
                                                EndIf
                                                
                                                cpl_override=0 
                                                if abrt Then return  
                                                 
                                                'if deb=5 Then print #5,"New stack  ",newss,newsp 
                                                if (newss And inv(3))=0 Then 
                                                        'if deb=5 Then print #5,"Call gate loading NULO SS" 
                                                        x86ts(newss And inv(3)) 
                                                        Exit sub  
                                                EndIf
                                                addr=newss And inv(7)
                                                if (newss And 4) Then 
                                                        if (addr>=ldt.limit) Then 
                                                                'if deb=5 Then print #5,"Bigger than LDT limit  CSC SS",newss,addr,ldt.limit 
                                                                x86ts(newss And inv(3)) 
                                                                Exit sub  
                                                        EndIf
                                                        addr+=ldt.base0 
                                                else
                                                        if (addr>=gdt.limit) Then 
                                                                'if deb=5 Then print #5,"Bigger than GDT limit CSC",newss,gdt.limit 
                                                                x86ts(newss And inv(3)) 
                                                                Exit sub  
                                                        EndIf
                                                        addr+=gdt.base0 
                                                EndIf
                                                
                                                cpl_override=1 
                                                'if deb=5 Then print #5,"Read stack seg" 
                                                segdat2(0)=readmemw_386(0,addr) 
                                                segdat2(1)=readmemw_386(0,addr+2) 
                                                segdat2(2)=readmemw_386(0,addr+4) 
                                                segdat2(3)=readmemw_386(0,addr+6)
                                                cpl_override=0
                                                if abrt Then return  
                                                 
                                                'if deb=5 Then print #5,"Read stack seg done" 
                                                if ((newss  And 3) <> DPL)  Or  (DPL2 <> DPL) Then 
                                                    'if deb=5 Then print #5,"Call gate loading SS with wrong permissions ", newss, seg2, DPL, DPL2, segdat(2), segdat2(2)
                                                    x86ts(newss And inv(3)) 
                                                    Exit sub  
                                                EndIf
                                                if (segdat2(2) And &h1A00)<>&h1200 Then 
                                                    'if deb=5 Then print #5,"Call gate loading SS wrong tipo" 
                                                    x86ts(newss And inv(3)) 
                                                    Exit sub  
                                                EndIf
                                                if (segdat2(2) And &h8000)=0 Then  
                                                    'if deb=5 Then print #5,"Call gate loading SS not present"
                                                    x86np( newss  And &hfffc )
                                                    Exit sub  
                                                EndIf
                                                if stack32=0 Then oldsp  = oldsp  And  &hFFFF 
                                                SS1=newss 
                                                stack32=segdat2(3) And &h40 
                                                if stack32 Then 
                                                       ESP=newsp 
                                                else
                                                       SP=newsp
                                                EndIf
                                                _ss.limit=segdat2(0) Or ((segdat2(3) And &hF) Shl 16) 
                                                if (segdat2(3) And &h80) Then _ss.limit=(_ss.limit Shl 12) Or &hFFF 
                                                if ((segdat2(2) Shr 8) And 4) Then _ss.limit = &hffffffff 
                                                _ss.limitw=IIf((segdat2(2) And &h200),1,0) 
                                                _ss.base0 = segdat2(1) 
                                                _ss.base0 = _ss.base0 Or ((segdat2(2) And &hFF) Shl 16) 
                                                _ss.base0 = _ss.base0 Or ((segdat2(3) Shr 8) Shl 24)  ' antes ---> if (is386) Then 
                                                _ss.access0=segdat2(2) Shr 8 
                                                'if deb=5 Then print #5,"Set access 1" 
' --------------------------------------------------------------------------------                                               
if SEL_ACCESSED Then                                                
               cpl_override = 1 
               writememw_x86(0, addr+4, segdat2(2)  Or  &h100) /'Set accessed bit'/
               cpl_override = 0 
EndIf
' --------------------------------------------------------------------------------
                                                CS1=seg2 
                                                _cs.limit=segdat(0) Or ((segdat(3) And &hF) Shl 16) 
                                                if (segdat(3) And &h80) Then _cs.limit=(_cs.limit Shl 12) Or &hFFF 
                                                _cs.base0=segdat(1) 
                                                _cs.base0 = _cs.base0 Or ((segdat(2) And &hFF) Shl 16) 
                                                _cs.base0 = _cs.base0 Or ((segdat(3) Shr 8) Shl 24)  ' antes ---> if (is386) Then 
                                                _cs.access0=segdat(2) Shr 8 
                                                if (CPL=3)  And  (oldcpl<>3) Then flushmmucache_cr3() 
                                                use32=IIf((segdat(3) And &h40),&h300,0) 
                                                pc=newpc 
                                                'if deb=5 Then print #5,"Set access 2" 
' --------------------------------------------------------------------------------                                                
if CS_ACCESSED Then
            cpl_override = 1 
            writememw_x86(0, oaddr+4, segdat(2)  Or  &h100) /'Set accessed bit'/
            cpl_override = 0 
EndIf
' --------------------------------------------------------------------------------
                                                'if deb=5 Then print #5,"tipo ",tipo
                                                if (tipo=&hC00) Then 
                                                        PUSHL(oldss) 
                                                        PUSHL(oldsp2) 
                                                        if abrt Then 
                                                                'if deb=5 Then print #5,"ABRT PUSHL" 
                                                                SS1 = oldss 
                                                                ESP = oldsp2 
                                                                Exit sub  
                                                        EndIf
                                                        if (count) Then 
                                                                while (count)
                                                                        count-=1 
                                                                        PUSHL(readmeml_386(oldssbase,oldsp+(count*4))) 
                                                                        if abrt Then 
                                                                                'if deb=5 Then print #5,"ABRT COPYL" 
                                                                                SS1 = oldss 
                                                                                ESP = oldsp2 
                                                                                Exit sub  
                                                                        EndIf
                                                                Wend
                                                        EndIf
                                                else
                                                        'if deb=5 Then print #5,"Stack ",SP
                                                        PUSHW(oldss) 
                                                        'if deb=5 Then print #5,"Write SS to  ",SS1,SP 
                                                        PUSHW(oldsp2) 
                                                        if abrt Then 
                                                            'if deb=5 Then print #5,"ABRT PUSHW" 
                                                            SS1 = oldss 
                                                            ESP = oldsp2 
                                                            Exit sub  
                                                        EndIf
                                                        'if deb=5 Then print #5,"Write SP to  ",SS1,SP 
                                                        if (count) Then 
                                                                while (count)
                                                                        count-=1 
                                                                        tempw=readmemw_386(oldssbase,(oldsp And &hFFFF)+(count*2)) 
                                                                        'if deb=5 Then print #5,"PUSH ",tempw 
                                                                        PUSHW(tempw) 
                                                                        if abrt Then 
                                                                            'if deb=5 Then print #5,"ABRT COPYW" 
                                                                            SS1 = oldss 
                                                                            ESP = oldsp2 
                                                                            Exit sub  
                                                                        EndIf
                                                                Wend
                                                        EndIf
                                                EndIf
                                                Exit Select 
                                        ElseIf (DPL > CPL) Then 
                                        			print #5,"Call gate DPL > CPL" 
                                                x86gpf(seg2 And inv(3)) 
                                                Exit sub  
                                        EndIf
' -----------------------------------------------------------------------------------------
' todo lo del CASE siguiente, el "Case &h1C00, &h1D00, &h1E00, &h1F00"
                                        CS1=seg2 
                                        _cs.limit=segdat(0) Or ((segdat(3) And &hF) Shl 16) 
                                        if (segdat(3) And &h80) Then _cs.limit=(_cs.limit Shl 12) Or &hFFF 
                                        _cs.base0=segdat(1) 
                                        _cs.base0 = _cs.base0 Or ((segdat(2) And &hFF) Shl 16) 
                                        _cs.base0 = _cs.base0 Or ((segdat(3) Shr 8) Shl 24)  ' antes ---> if (is386) Then 
                                        _cs.access0=segdat(2) Shr 8 
                                        if (CPL=3)  And  (oldcpl<>3) Then flushmmucache_cr3() 
                                        use32=iif((segdat(3) And &h40),&h300,0)
                                        pc=newpc 
			' --------------------------------------------------------------------------------                                        
			if  CS_ACCESSED Then                                                
			                cpl_override = 1 
			                writememw_x86(0, addr+4, segdat(2)  Or  &h100) /'Set accessed bit'/
			                cpl_override = 0 
			EndIf
' --------------------------------------------------------------------------------

                                	Case &h1C00, &h1D00, &h1E00, &h1F00  /'Conforming'/
                                        CS1=seg2 
                                        _cs.limit=segdat(0) Or ((segdat(3) And &hF) Shl 16) 
                                        if (segdat(3) And &h80) Then _cs.limit=(_cs.limit Shl 12) Or &hFFF 
                                        _cs.base0=segdat(1) 
                                        _cs.base0 = _cs.base0 Or ((segdat(2) And &hFF) Shl 16) 
                                        _cs.base0 = _cs.base0 Or ((segdat(3) Shr 8) Shl 24)  ' antes ---> if (is386) Then 
                                        _cs.access0=segdat(2) Shr 8 
                                        if (CPL=3)  And  (oldcpl<>3) Then flushmmucache_cr3() 
                                        use32=iif((segdat(3) And &h40),&h300,0)
                                        pc=newpc 
' --------------------------------------------------------------------------------                                        
if  CS_ACCESSED Then                                                
                cpl_override = 1 
                writememw_x86(0, addr+4, segdat(2)  Or  &h100) /'Set accessed bit'/
                cpl_override = 0 
EndIf
' --------------------------------------------------------------------------------
                                        Exit Select 
                                	Case else 
                                        'if deb=5 Then print #5,"Call gate bad segment tipo" 
                                        x86gpf(seg2 And inv(3)) 
                                        Exit sub  
                               End Select

                        	Case else 
                                'if deb=5 Then print #5,"Bad CALL special descriptor ",segdat(2) And &hF00 
                                x86gpf(seg And inv(3)) 
                                Exit sub  
                       End Select
                EndIf
        else
                _cs.base0=seg Shl 4 
                _cs.limit=&hFFFF 
                CS1=seg 
                if (eflags And VM_FLAG) Then
                     _cs.access0=3 Shl 5 
                else
                     _cs.access0=0 ' 0 Shl 5=0!!!
                EndIf
                if (CPL=3)  And  (oldcpl<>3) Then flushmmucache_cr3() 
        EndIf
End Sub

Sub pmoderetf(ByVal is32 As Integer , ByVal off As UShort ) 
        Dim As ULong  newpc =Any
        Dim As ULong  newsp =Any
        Dim As ULong  addr=any, oaddr=Any 
        Dim As UShort  segdat(4)=any,segdat2(4)=any,seg=any,newss =Any
        Dim As ULong  oldsp=ESP 

        'if deb=5 Then Print #5,"PMODERETF is32, CS1, pc, cr0, flags:";is32;" ";Hex(CS1,8);" ";Hex(pc,8);" ";Hex(cr0,8);" ";Hex(eflags,8) 

        if (is32) Then
                newpc=POPL() 
                seg=POPL(): if abrt then return   
        else
                'if deb=5 Then print #5,"PC read from  ",SS1,SP
                newpc=POPW() 
                'if deb=5 Then print #5,"CS read from  ",SS1,SP
                seg=POPW(): if abrt then return   
        EndIf
        
        'if deb=5 Then print #5,"Exit sub  to  ",seg,newpc 
        
        if (seg And 3)<CPL Then 
            	 'if deb=5 Then print #5,"RETF RPL<CPL  ",seg,CPL,ins,CS1,pc 
                ESP=oldsp 
                x86gpf(seg And inv(3)) 
                Exit sub  
        EndIf
        
        if (seg And inv(3))=0 Then
            	 'if deb=5 Then print #5,"Trying to load CS with NULO selector retf" 
                x86gpf(0) 
                Exit sub  
        EndIf
        
        addr=seg And inv(7)
        if (seg And 4) Then 
                if addr>=ldt.limit Then
                    'if deb=5 Then print #5,"Bigger than LDT limit  RETF",seg,ldt.limit
                        x86gpf(seg And inv(3)) 
                        Exit sub  
                EndIf
                addr+=ldt.base0 
        else
                if addr>=gdt.limit Then 
                    'if deb=5 Then print #5,"Bigger than GDT limit  RETF",seg,gdt.limit 
                        x86gpf(seg And inv(3)) 
                        Exit sub  
                EndIf
                addr+=gdt.base0 
        EndIf
        
        cpl_override=1 
        segdat(0)=readmemw_386(0,addr) 
        segdat(1)=readmemw_386(0,addr+2) 
        segdat(2)=readmemw_386(0,addr+4) 
        segdat(3)=readmemw_386(0,addr+6)
        cpl_override=0
        if abrt Then  ESP=oldsp: Exit sub   

        oaddr = addr 
        'if deb=5 Then print #5,"CPL  RPL ",CPL,seg And 3,is32 
        if stack32 Then
             ESP+=off 
        else
             SP+=off
        EndIf
        
        if CPL=(seg And 3) Then 
                'if deb=5 Then print #5,"RETF CPL = RPL  ", segdat(2) 
                Select Case As Const (segdat(2) And &h1F00)
                	Case &h1800, &h1900, &h1A00, &h1B00  /'Non-conforming'/
                        if (CPL <> DPL) Then 
                            'if deb=5 Then print #5,"RETF non-conforming CPL <> DPL" 
                            ESP=oldsp 
                            x86gpf(seg And inv(3)) 
                            Exit sub  
                        EndIf
                        Exit Select 
                	Case &h1C00, &h1D00, &h1E00, &h1F00  /'Conforming'/
                        if (CPL < DPL) Then 
                            'if deb=5 Then print #5,"RETF non-conforming CPL < DPL" 
                            ESP=oldsp 
                            x86gpf(seg And inv(3)) 
                            Exit sub  
                        EndIf
                        Exit Select 
                	Case else 
                    'if deb=5 Then print #5,"RETF CS not code segment" 
                    x86gpf(seg And inv(3)) 
                    Exit sub  
               End Select
               if (segdat(2) And &h8000)=0 Then 
                    ESP=oldsp 
                    'if deb=5 Then print #5,"RETF CS not present "
                    x86np(seg  And &hfffc)
                    Exit sub  
               EndIf
' --------------------------------------------------------------------------------            
if  CS_ACCESSED Then
       cpl_override = 1 
       writememw_x86(0, addr+4, segdat(2)  Or  &h100) /'Set accessed bit'/
       cpl_override = 0 
EndIf
' --------------------------------------------------------------------------------
                pc=newpc 
                if (segdat(2) And &h400) Then segdat(2) = (segdat(2) And inv(3 Shl (5+8))) Or ((seg And 3) Shl (5+8)) 
                CS1 = seg 
                _cs.limit=segdat(0) Or ((segdat(3) And &hF) Shl 16) 
                if (segdat(3) And &h80) Then _cs.limit=(_cs.limit Shl 12) Or &hFFF 
                _cs.base0=segdat(1) 
                _cs.base0 = _cs.base0 Or ((segdat(2) And &hFF) Shl 16) 
                _cs.base0 = _cs.base0 Or ((segdat(3) Shr 8) Shl 24)  ' antes ---> if (is386) Then 
                _cs.access0=segdat(2) Shr 8 
                if (CPL=3)  And  (oldcpl<>3) Then flushmmucache_cr3() 
                use32=IIf((segdat(3) And &h40),&h300,0) 
        else
                Select Case As Const(segdat(2) And &h1F00)
                	Case &h1800, &h1900, &h1A00, &h1B00  /'Non-conforming'/
                        if (seg And 3) <> DPL Then 
                            'if deb=5 Then print #5,"RETF non-conforming RPL <> DPL" 
                            ESP=oldsp 
                            x86gpf(seg And inv(3)) 
                            Exit sub  
                        EndIf
                        'if deb=5 Then print #5,"RETF non-conforming ",seg And 3, DPL 
                        Exit Select 
                	Case &h1C00, &h1D00, &h1E00, &h1F00  /'Conforming'/
                        if (seg And 3) < DPL Then 
                            'if deb=5 Then print #5,"RETF non-conforming RPL < DPL" 
                            ESP=oldsp 
                            x86gpf(seg And inv(3)) 
                            Exit sub  
                        EndIf
                        'if deb=5 Then print #5,"RETF conforming ",seg And 3, DPL 
                        Exit Select 
                	Case else 
                        'if deb=5 Then print #5,"RETF CS not code segment" 
                        ESP=oldsp 
                        x86gpf(seg And inv(3)) 
                        Exit sub  
               End Select
               if (segdat(2) And &h8000)=0 Then 
                        ESP=oldsp 
                    	   'if deb=5 Then print #5,"RETF CS not present "
                    	   x86np(seg  And &hfffc)
                        Exit sub  
               EndIf
                if (is32) Then 
                        newsp=POPL() 
                        newss=POPL(): if abrt then return   
                else
                        'if deb=5 Then print #5,"SP read from  ",SS1,SP
                        newsp=POPW() 
                        'if deb=5 Then print #5,"SS read from  ",SS1,SP
                        newss=POPW(): if abrt then return   
                EndIf
                'if deb=5 Then print #5,"Read new stack ", newss, newsp, ldt.base0
                if (newss And inv(3))=0 Then 
                        'if deb=5 Then print #5,"RETF loading NULL SS" 
                        ESP=oldsp 
                        x86gpf(newss And inv(3)) 
                        Exit sub  
                EndIf
                addr=newss And inv(7)
                if (newss And 4) Then 
                        if (addr>=ldt.limit) Then 
                            'if deb=5 Then print #5,"Bigger than LDT limit   RETF SS",newss,gdt.limit
                            ESP=oldsp 
                            x86gpf(newss And inv(3)) 
                            Exit sub  
                        EndIf
                        addr+=ldt.base0 
                else
                        if (addr>=gdt.limit) Then 
                            'if deb=5 Then print #5,"Bigger than GDT limit   RETF SS",newss,gdt.limit
                            ESP=oldsp 
                            x86gpf(newss And inv(3)) 
                            Exit sub  
                        EndIf
                        addr+=gdt.base0 
                EndIf
                
                cpl_override=1 
                segdat2(0)=readmemw_386(0,addr) 
                segdat2(1)=readmemw_386(0,addr+2) 
                segdat2(2)=readmemw_386(0,addr+4) 
                segdat2(3)=readmemw_386(0,addr+6)
                cpl_override=0
                if abrt Then ESP=oldsp: Exit sub   

                'if deb=5 Then print #5,"Segment data  ", segdat2(0), segdat2(1), segdat2(2), segdat2(3)
                
                if (newss  And 3) <> (seg  And 3) Then 
                    'if deb=5 Then print #5,"RETF loading SS with wrong permissions ", newss  And 3, seg  And 3, newss, seg
                    ESP=oldsp 
                    x86gpf(newss And inv(3)) 
                    Exit sub  
                EndIf
                if (segdat2(2) And &h1A00)<>&h1200 Then 
                    'if deb=5 Then print #5,"RETF loading SS wrong tipo" 
                    ESP=oldsp 
                    x86gpf(newss And inv(3)) 
                    Exit sub  
                EndIf
                if (segdat2(2) And &h8000)=0 Then 
                        ESP=oldsp 
                    		'if deb=5 Then print #5,"RETF loading SS not present"
                    		x86np(newss  And &hfffc)
                        Exit sub  
                EndIf
                if DPL2 <> (seg  And 3) Then 
                        'if deb=5 Then print #5,"RETF loading SS with wrong permissions 2 ", DPL2, seg  And 3, newss, seg
                        ESP=oldsp 
                        x86gpf(newss And inv(3)) 
                        Exit sub  
                EndIf
                SS1=newss 
                stack32=segdat2(3) And &h40 
                if stack32 Then 
                      ESP=newsp 
                else
                      SP=newsp
                EndIf
                _ss.limit=segdat2(0) Or ((segdat2(3) And &hF) Shl 16) 
                if (segdat2(3) And &h80) Then _ss.limit=(_ss.limit Shl 12) Or &hFFF 
                if (segdat2(2) Shr 8)  And 4 Then _ss.limit = &hffffffff 
                _ss.limitw=IIf((segdat2(2) And &h200),1,0) 
                _ss.base0 =segdat2(1) 
                _ss.base0 = _ss.base0 Or ((segdat2(2) And &hFF) Shl 16) 
                _ss.base0 = _ss.base0 Or ((segdat2(3) Shr 8) Shl 24)  ' antes ---> if (is386) Then 
                _ss.access0=segdat2(2) Shr 8 
                
' --------------------------------------------------------------------------------             
if SEL_ACCESSED then 
         cpl_override = 1 
         writememw_x86(0, addr+4, segdat2(2)  Or  &h100): /'Set accessed bit'/
			If  CS_ACCESSED Then
                writememw_x86(0, oaddr+4, segdat(2)  Or  &h100): /'Set accessed bit'/
			EndIf
         cpl_override = 0 
EndIf                
' --------------------------------------------------------------------------------

                'Conforming segments don't change CPL, so CPL = RPL
                If (segdat(2) And &h400) Then segdat(2)=(segdat(2) And inv(3 Shl (5+8))) Or ((seg And 3) Shl (5+8)) 
                pc=newpc 
                CS1=seg 
                _cs.limit=segdat(0) Or ((segdat(3) And &hF) Shl 16) 
                if (segdat(3) And &h80) Then _cs.limit=(_cs.limit Shl 12) Or &hFFF 
                _cs.base0=segdat(1) 
                _cs.base0 = _cs.base0 Or ((segdat(2) And &hFF) Shl 16) 
                _cs.base0 = _cs.base0 Or ((segdat(3) Shr 8) Shl 24)  ' antes ---> if (is386) Then 
                _cs.access0=segdat(2) Shr 8 
                if (CPL=3)  And  (oldcpl<>3) Then flushmmucache_cr3() 
                use32=IIf((segdat(3) And &h40),&h300,0) 
                if stack32 Then 
                        ESP+=off 
                else
                        SP+=off
                EndIf
        EndIf
End Sub

Sub restore_stack() 
        ss0=oldss
        _ss.limit=oldsslimit
        _ss.limitw=oldsslimitw 
End Sub

Sub pmodeint(ByVal num As Integer , ByVal soft As Integer )
        Dim As UShort  segdat(4)=any,segdat2(4)=any,segdat3(4)=Any
        Dim As ULong  addr=Any, oaddr=any
        Dim As UShort  oldcs=CPL,oldcs2=any,newss=Any 
        Dim As ULong  oldss=Any,oldsp =any
        Dim As UByte  oldaccess =Any
        Dim As Integer tipo =Any
        Dim As ULong  newsp =Any
        Dim As UShort  seg =Any
        Dim As Integer stack_changed=0 

        'if deb=5 Then Print #5,"PMODEINT:";num;soft;" "; Hex(CS1);":";Hex(pc),Hex(SS1);":";Hex(ESP), abrt

        if ((eflags And VM_FLAG)<>0)  And  (IOPL<>3)  And  (soft<>0) Then 
            'if deb=5 Then print #5,"V86 banned INT" 
            x86gpf(0) 
            Exit sub 
        EndIf

        addr=(num Shl 3) 
        
        if (addr>=idt.limit) Then
                if (num=8) Then /'Triple fault - reset!'/
                    print #5,"PMODEINT Triple error!!! reinicio"
                    softresetx86() 
                elseif (num=&hD) Then
						  print #5,"PMODEINT Doble error!!!"
                    pmodeint(8,0) ' esto parece que lo reinicia todo 
                Else
                    'if deb=5 Then print #5,"INT out of range" 
                    x86gpf(IIf((num*8)+2+(soft),0,1)) 
                EndIf
                'if deb=5 Then print #5,"address >= IDT.limit" 
                Exit sub  
        EndIf
        'print #5," Addr ";Hex(addr,8);" ";Hex(idt.base0,8)
        addr+=idt.base0 
        cpl_override=1 

        segdat(0)=readmemw_386(0,addr) 
        segdat(1)=readmemw_386(2,addr) 
        segdat(2)=readmemw_386(4,addr) 
        segdat(3)=readmemw_386(6,addr)
        cpl_override=0
  'Print #5,"PmodeInt 33 ";Hex(addr,8);" ";Hex(segdat(0),4);" ";Hex(segdat(1),4);" ";Hex(segdat(2),4);" ";Hex(segdat(3),4) 
        If abrt Then print #5,"PmodeInt - error leyendo de ";Hex(addr,8): Exit sub  
     
        oaddr = addr 
        'Dim f As Integer:For f=-10 To 10:Print #5,Hex(addr+f),Hex(leeram1(addr+f),2):next
        If (segdat(2) And &h1F00)=0 Then
                x86gpf((num*8)+2) 
                Exit sub  
        EndIf
        if (DPL<CPL)  And  (soft<>0) Then
                x86gpf((num*8)+2) 
                Exit sub  
        EndIf
        tipo=segdat(2) And &h1F00 

        Select Case As const(tipo)
        	Case &h600, &h700, &hE00, &hF00  /'Interrupt and trap gates'/
                        intgatesize=IIf((tipo>=&h800),32,16 )
                        'Print #5,"pmodeint 2:";intgatesize,Hex(oldpc),Hex(pc)
                        if (segdat(2) And &h8000)=0 Then
                            'if deb=5 Then print #5,"Int gate not present "
                            x86np((num Shl 3) Or 2)
                            Exit sub  
                        EndIf
                        seg=segdat(1) 
                        addr=seg And inv(7)
                        'Print #5,"pmodeint 3:";Hex(addr),Hex(gdt.base0),Hex(ldt.base0)
                        if (seg And 4) Then 
                                if (addr>=ldt.limit) Then
                                    'if deb=5 Then print #5,"Bigger than LDT limit ",seg,gdt.limit 
                                    x86gpf(seg And inv(3)) 
                                    Exit sub  
                                EndIf
                                addr+=ldt.base0 
                        else
                                if (addr>=gdt.limit) Then
                                    'if deb=5 Then Print #5,"Bigger than GDT limit ",seg,gdt.limit,ins 
                                    x86gpf(seg And inv(3)) 
                                    Exit sub  
                                EndIf
                                addr+=gdt.base0 
                        EndIf

                        cpl_override=1 

                        segdat2(0)=readmemw_386(0,addr) 
                        segdat2(1)=readmemw_386(0,addr+2) 
                        segdat2(2)=readmemw_386(0,addr+4) 
                        segdat2(3)=readmemw_386(0,addr+6)
                        cpl_override=0: if abrt then return   
  'Print #5,"PmodeInt 34 ";Hex(addr,8);" ";Hex(segdat2(0),4);" ";Hex(segdat2(1),4);" ";Hex(segdat2(2),4);" ";Hex(segdat2(3),4) 
                        oaddr = addr 
                                               
                        if (DPL2 > CPL) Then 
                            'if deb=5 Then print #5,"INT to higher level 2" 
                            x86gpf(seg And inv(3)) 
                            Exit sub  
                        EndIf
                        'Print #5,Hex(segdat2(2)),Hex(segdat2(2) And &h1F00)
                        Select Case As const(segdat2(2) And &h1F00) 
                        	Case &h1800, &h1900, &h1A00, &h1B00  /'Non-conforming'/
                                if (DPL2<CPL) Then 
                                        stack_changed=1 
                                        if (segdat2(2) And &h8000)=0 Then
                                            'if deb=5 Then print #5,"Int gate CS not present"
                                            x86np(segdat(1)  And &hfffc)
                                            Exit sub  
                                        EndIf
                                        if ((eflags And VM_FLAG)<>0)  And  (DPL2<>0) Then 
                                            'if deb=5 Then print #5,"V86 calling INT gate, DPL <> 0" 
                                            x86gpf(segdat(1) And &hFFFC) 
                                            Exit sub  
                                        EndIf
                                        /'Load new stack'/
                                        oldss=SS1 
                                        oldsp=ESP 
                                        cpl_override=1 
                                        if (tr.access0 And 8) Then 
                                                addr = 4 + tr.base0 + (DPL2 * 8) 
                                                newss=readmemw_386(0,addr+4) 
                                                newsp=readmeml_386(0,addr) 
                                        else
                                                addr = 2 + tr.base0 + (DPL2 * 8) 
                                                newss=readmemw_386(0,addr+2) 
                                                newsp=readmemw_386(0,addr) 
                                        EndIf
                                        cpl_override=0 
                                        if (newss And inv(3))=0 Then 
                                            'if deb=5 Then print #5,"Int gate loading NULL SS" 
                                            x86ss(newss And inv(3)) 
                                            Exit sub  
                                        EndIf
                                        addr=newss And inv(7)
                                        if (newss And 4) Then 
                                                if (addr>=ldt.limit) Then 
                                                    'if deb=5 Then print #5,"Bigger than LDT limit   PMODEINT SS ",newss,gdt.limit
                                                    x86ss(newss And inv(3)) 
                                                    Exit sub  
                                                EndIf
                                                addr+=ldt.base0 
                                        else
                                                if (addr>=gdt.limit) Then 
                                                    'if deb=5 Then print #5,"Bigger than GDT limit   CSC ",newss,gdt.limit
                                                    x86ss(newss And inv(3)) 
                                                    Exit sub  
                                                EndIf
                                                addr+=gdt.base0 
                                        EndIf
                                        
                                        cpl_override=1 
                                        segdat3(0)=readmemw_386(0,addr) 
                                        segdat3(1)=readmemw_386(0,addr+2) 
                                        segdat3(2)=readmemw_386(0,addr+4) 
                                        segdat3(3)=readmemw_386(0,addr+6)
                                        cpl_override=0: if abrt then return 
                                          
                                        if ((newss  And 3) <> DPL2)  Or  (DPL3 <> DPL2) Then 
                                            'if deb=5 Then print #5,"Int gate loading SS with wrong permissions" 
                                            x86ss(newss And inv(3)) 
                                            Exit sub  
                                        EndIf
                                        if (segdat3(2) And &h1A00)<>&h1200 Then 
                                            'if deb=5 Then print #5,"Int gate loading SS wrong type" 
                                            x86ss(newss And inv(3)) 
                                            Exit sub  
                                        EndIf
                                        if (segdat3(2) And &h8000)=0 Then 
                                            'if deb=5 Then print #5,"Int gate loading SS not present "
                                            x86np(newss  And &hfffc)
                                            Exit sub  
                                        EndIf
                                        SS1=newss 
                                        stack32=segdat3(3) And &h40 
                                        if stack32 Then 
                                            ESP=newsp 
                                        else
                                            SP=newsp
                                        EndIf
                                        
                                        _ss.limit=segdat3(0) Or ((segdat3(3) And &hF) Shl 16) 
                                        if (segdat3(3) And &h80) Then _ss.limit=(_ss.limit Shl 12) Or &hFFF 
                                        if ((segdat3(2) Shr 8)  And 4) Then _ss.limit = &hffffffff 
                                        _ss.limitw=IIf((segdat3(2) And &h200),1,0) 
                                        _ss.base0=segdat3(1) 
                                        _ss.base0 = _ss.base0 Or ((segdat3(2) And &hFF) Shl 16) 
                                        _ss.base0 = _ss.base0 Or ((segdat3(3) Shr 8) Shl 24)  ' antes ---> if (is386) Then 
                                        _ss.access0=segdat3(2) Shr 8 
' --------------------------------------------------------------------------------
if CS_ACCESSED Then                                        
                   cpl_override = 1 
                   writememw_x86(0, addr+4, segdat3(2)  Or  &h100): /'Set accessed bit'/
                   cpl_override = 0 
EndIf
' --------------------------------------------------------------------------------
                                        'if deb=5 Then print #5,"New stack  ",Hex(SS1),hex(ESP)
                                        cpl_override=1 
                                        if (tipo>=&h800) Then 
                                        	'Print #5,"Push 32 ",Hex(eflags And VM_FLAG)
                                                if (eflags And VM_FLAG) Then 
                                                        PUSHL(GS1) 
                                                        PUSHL(FS1) 
                                                        PUSHL(DS1) 
                                                        PUSHL(ES1)
                                                        if abrt Then Exit sub   
                                                        loadseg(0, _ds) 
                                                        loadseg(0, _es) 
                                                        loadseg(0, _fs) 
                                                        loadseg(0, _gs) 
                                                EndIf
                                                PUSHL(oldss) 
                                                PUSHL(oldsp) 
                                                PUSHL(flags Or (eflags Shl 16)) 
                                                'Print #5,"Pushl CS ", Hex(CS1)
                                                PUSHL(CS1) 
                                                'Print #5,"Pushl PC ", Hex(pc)
                                                PUSHL(pc)
                                                if abrt Then Exit sub    
                                                'Print #5,"32Stack ",Hex(SS1),Hex(ESP)
                                        else
                                                PUSHW(oldss) 
                                                PUSHW(oldsp) 
                                                PUSHW(flags) 
                                                PUSHW(CS1) 
                                                PUSHW(pc)
                                                if abrt Then Exit sub    
                                        EndIf
                                        cpl_override=0 
                                        _cs.access0=0 
                                        Exit Select 
                                elseif (DPL2<>CPL) Then 
                                			 print #5,"Non-conforming int gate DPL <> CPL" 
                                        x86gpf(seg And inv(3)) 
                                        Exit sub  
                                EndIf
' ---------------------------------------------------------------------------------------------
' este grup es IDENTICO al del CASE siguiente "&h1C00, &h1D00, &h1E00, &h1F00"
' en "C", si el CASE no lleva BREAK, continua con el siguiente, pero BASIC interrumpe y no ejecuta este grupo
' por eso, lo copia aqui tal cual, de modo que va todo seguido, como si hubiera ejecutado el CASE a continuacion
' ---------------------------------------------------------------------------------------------
                                if (segdat2(2) And &h8000)=0 Then 
                                    'if deb=5 Then print #5,"Int gate CS notpresent"
                                    x86np(segdat(1)  And &hfffc)
                                    Exit sub  
                                EndIf
                                if ((eflags And VM_FLAG)<>0)  And  (DPL2<CPL) Then 
                                    'if deb=5 Then print #5,"Int gate V86 mode DPL2<CPL" 
                                    x86gpf(seg And inv(3)) 
                                    Exit sub  
                                EndIf
                                if (stack_changed=0)  And  (ssegs<>0) Then restore_stack() 
                                if (tipo>&h800) Then 
                                        PUSHL(flags Or (eflags Shl 16)) 
                                        PUSHL(CS1) 
                                        PUSHL(pc)
                                        if abrt Then Exit sub    
                                else
                                        PUSHW(flags) 
                                        PUSHW(CS1) 
                                        PUSHW(pc)
                                        'Print #5,"PMODEINT PUSHW:";Hex(flags),Hex(CS1),Hex(pc)
                                        if abrt Then Exit sub    
                                EndIf
' -------------------------------------------------------------------------------------------- 

                        	Case &h1C00, &h1D00, &h1E00, &h1F00  /'Conforming'/
                                if (segdat2(2) And &h8000)=0 Then 
                                    'if deb=5 Then print #5,"Int gate CS notpresent"
                                    x86np(segdat(1)  And &hfffc)
                                    Exit sub  
                                EndIf
                                if ((eflags And VM_FLAG)<>0)  And  (DPL2<CPL) Then 
                                    'if deb=5 Then print #5,"Int gate V86 mode DPL2<CPL" 
                                    x86gpf(seg And inv(3)) 
                                    Exit sub  
                                EndIf
                                if (stack_changed=0)  And  (ssegs<>0) Then restore_stack() 
                                if (tipo>&h800) Then 
                                        PUSHL(flags Or (eflags Shl 16)) 
                                        PUSHL(CS1) 
                                        PUSHL(pc)
                                        if abrt Then Exit sub    
                                else
                                        PUSHW(flags) 
                                        PUSHW(CS1) 
                                        PUSHW(pc)
                                        if abrt Then Exit sub    
                                EndIf
                                Exit Select 
                        	Case else 
                             'if deb=5 Then print #5,"Int gate CS not code segment ",segdat2(0),segdat2(1),segdat2(2),segdat2(3) 
                             x86gpf(seg And inv(3)) 
                             Exit sub  
                        End Select
                        
                CS1=(seg And inv(3)) Or CPL 
                _cs.limit=segdat2(0) Or ((segdat2(3) And &hF) Shl 16) 
                if (segdat2(3) And &h80) Then _cs.limit=(_cs.limit Shl 12) Or &hFFF 
                _cs.base0 = segdat2(1) 
                _cs.base0 = _cs.base0 Or ((segdat2(2) And &hFF) Shl 16) 
                _cs.base0 = _cs.base0 Or ((segdat2(3) Shr 8) Shl 24)  ' antes ---> if (is386) Then 
                _cs.access0=segdat2(2) Shr 8 
                if (CPL=3)  And  (oldcpl<>3) Then flushmmucache_cr3() 
                if (tipo>&h800) Then
                     pc=segdat(0) Or (segdat(3) Shl 16) 
                else
                     pc=segdat(0)
                EndIf
                use32=IIf((segdat2(3) And &h40),&h300,0 )
                'Print #5,Hex(_cs.limit),Hex(_cs.base0),Hex(_cs.access0)
' --------------------------------------------------------------------------------
if CS_ACCESSED Then
          cpl_override = 1 
          writememw_x86(0, oaddr+4, segdat2(2)  Or  &h100): /'Set accessed bit'/
          cpl_override = 0 
EndIf
' --------------------------------------------------------------------------------
                eflags = eflags And inv(VM_FLAG) 
                if (tipo And &h100)=0 Then
                        flags = flags And inv(I_FLAG) 
                EndIf
                flags = flags And inv(T_FLAG Or NT_FLAG) 
                'Print #5,"nueva pila:";Hex(SS1),Hex(ESP)
                Exit Select 
                
        	Case &h500  /'Task gate'/
                seg=segdat(1) 
                        addr=seg And inv(7)
                        if (seg And 4) Then
                                if (addr>=ldt.limit) Then
                                    'if deb=5 Then print #5,"Bigger than LDT limit   INT ",seg,gdt.limit 
                                    x86gpf(seg And inv(3)) 
                                    Exit sub  
                                EndIf
                                addr+=ldt.base0 
                        else
                                if (addr>=gdt.limit) Then
                                    'if deb=5 Then print #5,"Bigger than GDT limit   INT ",seg,gdt.limit,ins 
                                    x86gpf(seg And inv(3)) 
                                    Exit sub  
                                EndIf
                                addr+=gdt.base0 
                        EndIf
                        
                        cpl_override=1 
                        segdat2(0)=readmemw_386(0,addr) 
                        segdat2(1)=readmemw_386(0,addr+2) 
                        segdat2(2)=readmemw_386(0,addr+4) 
                        segdat2(3)=readmemw_386(0,addr+6) 
                        cpl_override=0: if abrt then return  
                        
                       if (segdat2(2) And &h8000)=0 Then
                           'if deb=5 Then print #5,"Int task gate not present "
                           x86np(segdat(1)  And &hfffc) 
                           Exit sub  
                       EndIf
                optype=INT0 
                cpl_override=1 
                '--------------------------------------------------
                taskswitch286(seg,segdat2(),segdat2(2) And &h800) 
                '--------------------------------------------------
                cpl_override=0 
                Exit Select 
        	Case else
            'if deb=5 Then print #5,"Bad integer gate tipe ",segdat(2) And &h1F00,segdat(0),segdat(1),segdat(2),segdat(3)
            x86gpf(seg And inv(3))  
        End Select
        'deb=0
        'Print #5,"final de pmodeint "':Sleep
end Sub

Sub pmodeiret(ByVal is32 As Integer )
        Dim As ULong  newsp =Any
        Dim As UShort oldcs=CPL,newss =Any
        Dim As UShort tempw=Any,tempw2 =any
        Dim As ULong  tempflags=any,flagmask =Any
        Dim As ULong  newpc =Any
        Dim As UShort segdat(4)=any,segdat2(4)=Any
        Dim As UShort segs(4)=Any 
        Dim As UShort seg =Any
        Dim As ULong  addr=any, oaddr=Any
        Dim As ULong  oldsp=ESP 

        'if deb=5 Then Print #5,"PMODEIRET:";is32;
        'if deb=5 Then Print #5," VIRTUAL x86:";IIf (eflags And VM_FLAG,"SI", "NO");
        'if deb=5 Then Print #5,"  MULTITAREA:";IIf (eflags And NT_FLAG,"SI", "NO")
        
        'For deb=-8 To 8 Step 2
        '	Print #5,Hex(leeram2(ss0+SP+deb),4)
        'Next
        'Print #5,"aa"
        'deb=0
        'Sleep
        'Sleep
   
        ' si es modo virtual x86 (VM_FLAG=1)
        if (eflags And VM_FLAG) Then
                if (IOPL<>3) Then 
                        'if deb=5 Then print #5,"V86 IRET IOPL<>3" 
                        x86gpf(0) 
                        return  
                EndIf
                oxpc=pc 
                if (is32) Then 
                        newpc=POPL() 
                        seg=POPL() 
                        tempflags=POPL(): if abrt then return   
                else
                        newpc=POPW() 
                        seg=POPW() 
                        tempflags=POPW(): if abrt then return   
                EndIf
                pc=newpc 
                _cs.base0=seg Shl 4 
                _cs.limit=&hFFFF 
                CS1=seg 
                flags=(flags And &h3000) Or (tempflags And &hCFD5) Or 2 
                return 
        EndIf
        
        
        ' modo NT multitarea (NT_FLAG=1)
        if (flags And NT_FLAG) Then 
                seg=readmemw_386(tr.base0,0) 
                addr=seg And inv(7)
                if (seg And 4) Then 
                        if (addr>=ldt.limit) Then 
                                'if deb=5 Then print #5,"TS Bigger than LDT limit   IRET",seg,gdt.limit 
                                x86gpf(seg And inv(3)) 
                                Return  
                        EndIf
                        addr+=ldt.base0 
                else
                        if (addr>=gdt.limit) Then 
                                'if deb=5 Then print #5,"TS Bigger than GDT limit   IRET",seg,gdt.limit 
                                x86gpf(seg And inv(3)) 
                                Return  
                        EndIf
                        addr+=gdt.base0 
                EndIf
                cpl_override=1 
                segdat(0)=readmemw_386(0,addr) 
                segdat(1)=readmemw_386(0,addr+2) 
                segdat(2)=readmemw_386(0,addr+4) 
                segdat(3)=readmemw_386(0,addr+6) 
                			'------------------------------
                			 taskswitch286(seg,segdat(),0)
                			'------------------------------
                cpl_override=0 
                Return  
        EndIf
        
        
        
        inint=0 
        oxpc=pc 
        flagmask=&hFFFF 
        if CPL Then flagmask = flagmask And inv(&h3000) 
        if IOPL<CPL Then flagmask = flagmask And inv(&h200) 
        
        if is32 Then 
                newpc=POPL()
                seg=POPL()
                tempflags=POPL(): if abrt then return   
                'Print #5,"mas putas mierdas:";Hex(newpc),Hex(seg),Hex(tempflags)
                if (tempflags Shr 16) And VM_FLAG Then ' antes "is386 and ..."
                        newsp=POPL() 
                        newss=POPL() 
                        segs(0)=POPL() 
                        segs(1)=POPL() 
                        segs(2)=POPL() 
                        segs(3)=POPL(): if abrt then return   
                        eflags=tempflags Shr 16 
                        loadseg(segs(0), _es) 
                        loadseg(segs(1), _ds) 
                        loadseg(segs(2), _fs) 
                        loadseg(segs(3), _gs) 
                        pc=newpc 
                        _cs.base0=seg Shl 4 
                        _cs.limit=&hFFFF 
                        CS1=seg 
                        _cs.access0=3 Shl 5 
                        if (CPL=3)  And  (oldcpl<>3) Then flushmmucache_cr3() 
                        ESP=newsp 
                        loadseg(newss, _ss) 
                        use32=0 
                        flags=(tempflags And &hFFD5) Or 2 
                        Exit sub  
                EndIf
        Else
        'For deb=-8 To 8 Step 2
        '	Print #5,Hex(leeram2(ss0+SP+deb),4)
        'Next
        'deb=0
        'Sleep
        'Sleep  	
        	
                newpc=POPW()
                seg=POPW() 
                tempflags=POPW(): if abrt then Return
                'Print #5,"pmodeirt popw:";Hex(tempflags,4),Hex(seg,4),Hex(newpc,4)  
        EndIf
        
        if (seg And inv(3))=0 Then 
                'if deb=5 Then print #5,"IRET CS=0" 
                x86gpf(0) 
                Exit sub  
        EndIf
        
        addr=seg And inv(7)
        if (seg And 4) Then 
                if addr>=ldt.limit Then 
                        'if deb=5 Then print #5,"Bigger than LDT limit   IRET",seg,gdt.limit 
                        x86gpf(seg And inv(3)) 
                        Exit sub  
                EndIf
                addr+=ldt.base0 
        Else
        	'Print #5,"pmodeiret da error en PMTUT:";Hex(addr), Hex(seg),Hex(gdt.limit)
                if addr>=gdt.limit Then 
                        'if deb=5 Then print #5,"Bigger than GDT limit   IRET",seg,gdt.limit 
                        x86gpf(seg And inv(3)) 
                        Exit sub  
                EndIf
                addr+=gdt.base0 
        EndIf
        
        if (seg And 3) < CPL Then 
                'if deb=5 Then print #5,"IRET to lower level" 
                x86gpf(seg And inv(3)) 
                Exit sub  
        EndIf
        
        cpl_override=1 
        segdat(0)=readmemw_386(0,addr) 
        segdat(1)=readmemw_386(0,addr+2) 
        segdat(2)=readmemw_386(0,addr+4) 
        segdat(3)=readmemw_386(0,addr+6): cpl_override=0: if abrt Then ESP=oldsp: Exit sub  
        'print #5,"PMODEIRET SEGDAT: ";Hex(segdat(0));" ";Hex(segdat(1));" ";Hex(segdat(2));" ";Hex(segdat(3))
		  'Print #5,"modo pmodeiret:";Hex(segdat(2) And &h1F00)
        Select Case As Const(segdat(2) And &h1F00)
        	Case &h1800, &h1900, &h1A00, &h1B00  /'Non-conforming code'/
                if ((seg And 3) <> DPL) Then 
                        'if deb=5 Then print #5,"IRET NC DPL ", seg, segdat(0), segdat(1), segdat(2), segdat(3) 
                        x86gpf(seg And inv(3)) 
                        Exit sub  
                EndIf
                Exit Select 
        	Case &h1C00, &h1D00, &h1E00, &h1F00  /'Conforming code'/
                if ((seg And 3) < DPL) Then 
                        'if deb=5 Then print #5,"IRET C DPL" 
                        x86gpf(seg And inv(3)) 
                        Exit sub  
                EndIf
                Exit Select 
        	Case else 
                'if deb=5 Then print #5,"IRET CS <> code seg" 
                x86gpf(seg And inv(3)) 
                Exit sub  
        End Select
        
        If (segdat(2) And &h8000)=0 Then 
                ESP=oldsp 
                'if deb=5 Then print #5,"IRET CS not present"
                x86np(seg  And &hfffc)
                Exit sub  
        EndIf
        
        if (seg And 3) = CPL Then 
                CS1=seg 
                _cs.limit=segdat(0) Or ((segdat(3) And &hF) Shl 16) 
                if (segdat(3) And &h80) Then _cs.limit=(_cs.limit Shl 12) Or &hFFF 
                _cs.base0 = segdat(1) 
                _cs.base0 = _cs.base0 Or ((segdat(2) And &hFF) Shl 16) 
                _cs.base0 = _cs.base0 Or ((segdat(3) Shr 8) Shl 24) ' antes --->>> if (is386) Then 
                _cs.access0=segdat(2) Shr 8 
                if (CPL=3)  And  (oldcpl<>3) Then flushmmucache_cr3() 
                use32=IIf((segdat(3) And &h40),&h300,0) 
                
' --------------------------------------------------------------------------------                
if  CS_ACCESSED Then                
       cpl_override = 1 
       writememw_x86(0, addr+4, segdat(2)  Or  &h100): /'Set accessed bit'/
       cpl_override = 0 
EndIf
' --------------------------------------------------------------------------------

        else
                /'return to outer level'/
                oaddr = addr 
                'if deb=5 Then print #5,"Outer level" 
                if (is32) Then 
                        newsp=POPL() 
                        newss=POPL()
                        if abrt Then return   
                else
                        newsp=POPW() 
                        newss=POPW()
                        if abrt Then return   
                EndIf
                'if deb=5 Then print #5,"IRET load stack  ",newss,newsp
                if (newss And inv(3))=0 Then 
                        'if deb=5 Then print #5,"IRET loading NULO SS" 
                        x86gpf(newss And inv(3)) 
                        Exit sub  
                EndIf
                addr=newss And inv(7)
                if (newss And 4) Then 
                        if (addr>=ldt.limit) Then 
                                'if deb=5 Then print #5,"Bigger than LDT limit   PMODEIRET SS",newss,gdt.limit
                                x86gpf(newss And inv(3)) 
                                Exit sub  
                        EndIf
                        addr+=ldt.base0 
                else
                        if (addr>=gdt.limit) Then 
                                'if deb=5 Then print #5,"Bigger than GDT limit   PMODEIRET",newss,gdt.limit 
                                x86gpf(newss And inv(3)) 
                                Exit sub  
                        EndIf
                        addr+=gdt.base0 
                EndIf
                cpl_override=1 
                segdat2(0)=readmemw_386(0,addr) 
                segdat2(1)=readmemw_386(0,addr+2) 
                segdat2(2)=readmemw_386(0,addr+4) 
                segdat2(3)=readmemw_386(0,addr+6): cpl_override=0: if abrt Then  ESP=oldsp: Exit sub   

                if (newss And 3) <> (seg And 3) Then 
                        'if deb=5 Then print #5,"IRET loading SS with wrong permissions  ", newss, seg 
                        x86gpf(newss And inv(3)) 
                        Exit sub  
                EndIf
                if (segdat2(2) And &h1A00)<>&h1200 Then 
                        'if deb=5 Then print #5,"IRET loading SS wrong tipo" 
                        x86gpf(newss And inv(3)) 
                        Exit sub  
                EndIf
                if DPL2 <> (seg And 3) Then 
                        'if deb=5 Then print #5,"IRET loading SS with wrong permissions 2 ", DPL2, seg  And 3, newss, seg
                        ESP=oldsp 
                        x86gpf(newss And inv(3)) 
                        Exit sub  
                EndIf
                if (segdat2(2) And &h8000)=0 Then 
                        'if deb=5 Then print #5,"IRET loading SS not present"
                        x86np(newss  And &hfffc)
                        Exit sub  
                EndIf
                SS1=newss 
                stack32=segdat2(3) And &h40 
                if stack32 Then 
                        ESP=newsp 
                else
                        SP=newsp
                EndIf
                _ss.limit=segdat2(0) Or ((segdat2(3) And &hF) Shl 16) 
                if (segdat2(3) And &h80) Then _ss.limit=(_ss.limit Shl 12) Or &hFFF 
                if ((segdat2(2) Shr 8)  And 4) Then _ss.limit = &hffffffff 
                _ss.limitw=IIf((segdat2(2) And &h200),1,0 )
                _ss.base0=segdat2(1) 
                _ss.base0 = _ss.base0 Or ((segdat2(2) And &hFF) Shl 16) 
                _ss.base0 = _ss.base0 Or ((segdat2(3) Shr 8) Shl 24)  ' antes --->>> if (is386) Then 
                _ss.access0=segdat2(2) Shr 8 
' --------------------------------------------------------------------------------               
if SEL_ACCESSED Then 
       cpl_override = 1 
       writememw_x86(0, addr+4, segdat2(2)  Or  &h100): /'Set accessed bit'/
		 If  CS_ACCESSED Then
       	writememw_x86(0, oaddr+4, segdat(2)  Or  &h100): /'Set accessed bit'/
		 EndIf
       cpl_override = 0 
EndIf     
' --------------------------------------------------------------------------------
                 'Conforming segments don't change CPL, so CPL = RPL
                If (segdat(2) And &h400) Then segdat(2) = (segdat(2) And inv(3 Shl (5+8))) Or ((seg And 3) Shl (5+8)) 
                CS1=seg 
                _cs.limit=segdat(0) Or ((segdat(3) And &hF) Shl 16) 
                if (segdat(3) And &h80) Then _cs.limit=(_cs.limit Shl 12) Or &hFFF 
                _cs.base0 =segdat(1) 
                _cs.base0 = _cs.base0 Or ((segdat(2) And &hFF) Shl 16) 
                _cs.base0 = _cs.base0 Or ((segdat(3) Shr 8) Shl 24)  ' antes --->>> if (is386) Then 
                _cs.access0=segdat(2) Shr 8 
                if (CPL=3)  And  (oldcpl<>3) Then flushmmucache_cr3() 
                use32=IIf((segdat(3) And &h40),&h300,0 )
                
                if (CPL>((_ds.access0 Shr 5) And 3)) Then 
                        _ds.seg=0 
                        _ds.base0=-1 
                EndIf
                if (CPL>((_es.access0 Shr 5) And 3)) Then 
                        _es.seg=0 
                        _es.base0=-1 
                EndIf
                if (CPL>((_fs.access0 Shr 5) And 3)) Then 
                        _fs.seg=0 
                        _fs.base0=-1 
                EndIf
                if (CPL>((_gs.access0 Shr 5) And 3)) Then 
                        _gs.seg=0 
                        _gs.base0=-1 
                EndIf
                
        EndIf
        pc=newpc 
        flags=(flags And inv(flagmask)) Or ((tempflags And flagmask) And &hFFD5) Or 2 
        if (is32) Then eflags=tempflags Shr 16 

End Sub



' IMPORTANTE: en los originales, el "taskswitch386" esta "apagado" (comentado), por lo que SOLO se usa este, el del 286!!!!!
Sub taskswitch286(seg As UShort , segdat() As UShort, ByVal is32 As Integer ) 
   Dim As ULong  Base0=Any,obase=tr.base0 
   Dim As ULong  limit =Any
   Dim As ULong  templ =Any
   Dim As UShort  tempw =Any
	Dim As ULong  new_cr3=0 
	Dim As UShort  new_es=any,new_cs=any,new_ss=any,new_ds=any,new_fs=any,new_gs=Any 
	Dim As UShort  new_ldt 
	Dim As ULong  new_eax=Any,new_ebx=any,new_ecx=any,new_edx=any,new_esp=any,new_ebp=Any
	Dim As ULong  new_esi=any,new_edi=any,new_pc=any,new_flags =any
   Dim As ULong  addr =Any
	Dim As UShort  oldflags =Any
	Dim As UShort  segdat2(4)=Any
	
	Dim As integer depur=0 ' para depurar aqui solo

	 'if deb=5 Then print #5,"RUTINA TASKSWITCH286!!! analizar"':sleep
	 
     Base0=segdat(1) Or ((segdat(2) And &hFF) Shl 16) Or ((segdat(3) Shr 8) Shl 24) 
     limit=segdat(0) Or ((segdat(3) And &hF) Shl 16) 
     'if (is386) Then ' para el 386/486
     
             new_cr3=readmeml_386(Base0,&h1C) 
             new_pc =readmeml_386(Base0,&h20) 
             new_flags=readmeml_386(Base0,&h24) 
             
             new_eax=readmeml_386(Base0,&h28) 
             new_ecx=readmeml_386(Base0,&h2C) 
             new_edx=readmeml_386(Base0,&h30) 
             new_ebx=readmeml_386(Base0,&h34) 
             new_esp=readmeml_386(Base0,&h38) 
             new_ebp=readmeml_386(Base0,&h3C) 
             new_esi=readmeml_386(Base0,&h40) 
             new_edi=readmeml_386(Base0,&h44) 
             
             new_es =readmemw_386(Base0,&h48) 
             new_cs =readmemw_386(Base0,&h4C) 
             new_ss =readmemw_386(Base0,&h50) 
             new_ds =readmemw_386(Base0,&h54) 
             new_fs =readmemw_386(Base0,&h58) 
             new_gs =readmemw_386(Base0,&h5C) 
             new_ldt=readmemw_386(Base0,&h60) 
             
             if abrt then return   
             if (optype=JMP)  Or  (optype=INT0) Then
                     if (tr.seg And 4) Then 
                             tempw=readmemw_386(ldt.base0,(tr.seg And inv(7))+4) 
                     else
                             tempw=readmemw_386(gdt.base0,(tr.seg And inv(7))+4)
                     EndIf
                     if abrt Then return   
                     tempw = tempw And inv(&h200) 
                     if (tr.seg And 4) Then 
                     	writememw_x86(ldt.base0,(tr.seg And inv(7))+4,tempw) 
                     else
                     	writememw_x86(gdt.base0,(tr.seg And inv(7))+4,tempw) 
                     EndIf
             EndIf
             
             if (optype=IRET) Then flags = flags And inv(NT_FLAG) 
             
             writememl_x86(tr.base0,&h1C,cr3) 
             writememl_x86(tr.base0,&h20,pc) 
             writememl_x86(tr.base0,&h24,flags Or (eflags Shl 16)) 
             
             writememl_x86(tr.base0,&h28,EAX) 
             writememl_x86(tr.base0,&h2C,ECX) 
             writememl_x86(tr.base0,&h30,EDX) 
             writememl_x86(tr.base0,&h34,EBX) 
             writememl_x86(tr.base0,&h38,ESP) 
             writememl_x86(tr.base0,&h3C,EBP) 
             writememl_x86(tr.base0,&h40,ESI) 
             writememl_x86(tr.base0,&h44,EDI) 
             
             writememl_x86(tr.base0,&h48,ES1) 
             writememl_x86(tr.base0,&h4C,CS1) 
             writememl_x86(tr.base0,&h50,SS1) 
             writememl_x86(tr.base0,&h54,DS1) 
             writememl_x86(tr.base0,&h58,FS1) 
             writememl_x86(tr.base0,&h5C,GS1) 
             writememl_x86(tr.base0,&h60,ldt.seg) 
             
             if (optype=INT0) Then 
                     writememl_x86(Base0,0,tr.seg) 
                     new_flags = new_flags Or NT_FLAG 
             EndIf
             
             if abrt then return   
             
             if (optype=JMP)  Or  (optype=INT0) Then
                     if (tr.seg And 4) Then 
                             tempw=readmemw_386(ldt.base0,(seg And inv(7))+4) 
                     else
                             tempw=readmemw_386(gdt.base0,(seg And inv(7))+4)
                     EndIf
                     if abrt Then return   
                     tempw = tempw Or &h200 
                     if (tr.seg And 4) Then 
                     	writememw_x86(ldt.base0,(seg And inv(7))+4,tempw) 
                     else
                        writememw_x86(gdt.base0,(seg And inv(7))+4,tempw) 
                     EndIf
             EndIf
             
             cr3=new_cr3 
             flushmmucache() 
             pc=new_pc 
             flags=new_flags 
             eflags=new_flags Shr 16 
             ldt.seg=new_ldt 
             templ=(ldt.seg And inv(7))+gdt.base0 
             ldt.limit=readmemw_386(0,templ) 
             
             if (readmemb_x86(templ+6) And &h80) Then 
                     ldt.limit = ldt.limit Shl 12 
                     ldt.limit = ldt.limit Or &hFFF 
             EndIf
             
             ldt.base0=(readmemw_386(0,templ+2)) Or (readmemb_x86(templ+4) Shl 16) Or (readmemb_x86(templ+7) Shl 24) 
             
             if (eflags And VM_FLAG) Then 
                     If depur Then print #5,"Task Switch to V86"
                     x86gpf(0) 
                     Exit sub  
             EndIf
             if (new_cs And inv(3))=0 Then 
                     If depur Then print #5,"TS loading NULL CS" 
                     x86gpf(0) 
                     Exit sub  
             EndIf
             addr=new_cs And inv(7)
             if (new_cs And 4) Then 
                     if (addr>=ldt.limit) Then 
                             'if deb=5 Then print #5,"Bigger than LDT limit TS ",new_cs,ldt.limit,addr
                             x86gpf(0) 
                             Exit sub  
                     EndIf
                     addr+=ldt.base0 
             else
                     if (addr>=gdt.limit) Then 
                             If depur Then Print #5,"Bigger than GDT limit TS ",new_cs,gdt.limit
                             x86gpf(0) 
                             Exit sub  
                     EndIf
                     addr+=gdt.base0 
             EndIf
             segdat2(0)=readmemw_386(0,addr) 
             segdat2(1)=readmemw_386(0,addr+2) 
             segdat2(2)=readmemw_386(0,addr+4) 
             segdat2(3)=readmemw_386(0,addr+6) 
             if (segdat2(2) And &h8000)=0 Then 
                     If depur Then Print #5,"TS loading CS not present "
                     x86np(new_cs  And &hfffc)
                     Exit sub  
             EndIf
             Select Case As const(segdat2(2) And &h1F00)
             	Case &h1800, &h1900, &h1A00, &h1B00  /'Non-conforming'/
                     if ((new_cs And 3) <> DPL2) Then 
                             If depur Then Print #5,"TS load CS non-conforming RPL <> DPL" 
                             x86gpf(new_cs And inv(3)) 
                             Exit sub  
                     EndIf

             	Case &h1C00, &h1D00, &h1E00, &h1F00  /'Conforming'/
                     if ((new_cs And 3) < DPL2) Then 
                             If depur Then Print #5,"TS load CS non-conforming RPL < DPL" 
                             x86gpf(new_cs And inv(3)) 
                             Exit sub  
                     EndIf

             	Case else 
                 		If depur Then Print #5,"TS load CS not code segment" 
                     x86gpf(new_cs And inv(3)) 
                     Exit Sub  
             End Select
             
             CS1=new_cs 
             _cs.limit  = segdat2(0) Or ((segdat2(3) And &hF) Shl 16) 
             if (segdat2(3) And &h80) Then _cs.limit=(_cs.limit Shl 12) Or &hFFF 
             _cs.base0  = segdat2(1) 
             _cs.base0  = _cs.base0 Or ((segdat2(2) And &hFF) Shl 16) 
             _cs.base0  = _cs.base0 Or ((segdat2(3) Shr 8) Shl 24) ' antes -->> if (is386) Then 
             _cs.access0= segdat2(2) Shr 8 
             if (CPL=3)  And  (oldcpl<>3) Then flushmmucache_cr3() 
             use32=IIf((segdat2(3) And &h40),&h300,0 )
             
             EAX=new_eax 
             ECX=new_ecx 
             EDX=new_edx 
             EBX=new_ebx 
             ESP=new_esp 
             EBP=new_ebp 
             ESI=new_esi 
             EDI=new_edi 
             
             if depur Then print #5,"Load ES ",new_es
             loadseg(new_es, _es) 
             if depur Then print #5,"Load SS ",new_ss
             loadseg(new_ss, _ss) 
             if depur Then print #5,"Load DS ",new_ds
             loadseg(new_ds, _ds) 
             if depur Then print #5,"Load FS ",new_fs
             loadseg(new_fs, _fs) 
             if depur Then print #5,"Load GS ",new_gs
             loadseg(new_gs, _gs) 
             if depur Then print #5,"Resuming at  ",CS1,pc
     'Else ' para el 286!!!!
     '    'if deb=5 Then print #5,"16-bit TSS" 
     '    resetx86() 
     'EndIf
     tr.seg=seg 
     tr.base0=Base0 
     tr.limit=limit 
     tr.access0=segdat(2) Shr 8 
End Sub
