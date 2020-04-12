

Sub exec386(ByVal cycs As Integer) 
        Dim As ULongInt temp64=Any
        'Dim As LongInt temp64i 
        Dim As UByte temp=Any,temp2=any 
        Dim As UShort tempw=any,tempw2=any,tempw3=any,tempw4=Any
        Dim As Byte offset =Any
        Dim As Byte temps=Any
        Dim As Short temps16=Any ' SOLO se usa en "IDIV AX,w", y solo una vez.
        Dim As long tempws=any,tempws2=Any
        Dim As ULong templ=any,templ2=any,templ3=any,addr=Any
        Dim As Integer c=any,cycdiff=Any 
        Dim As Integer oldcyc=Any 
        Dim As integer tempi=Any 
        Dim As integer trap =any

        cycles+=cycs 
        
        while (cycles>0)
            cycdiff=0 
            oldcyc=cycles 
                
            While (cycdiff<100)
                oldcs=CS1 
                oldpc=pc 
                oldcpl=CPL 
                op32=use32 

				' GOTO 
            opcodestart: 

                fetchdat=fastreadl(cs0+pc)
                
                If abrt Then GoTo opcodeend ' se va hasta el final
                
                tempc=flags And C_FLAG 
                trap =flags And T_FLAG 

                ' instruccion principal
                opcode=fetchdat And &hFF
                ' segunda instruccion 
                fetchdat=(fetchdat shr 8) 'And &hffff 

            
''if (deb=4) Then 
'  If ((opcode<>&hF2  And  opcode<>&hF3)  Or  (firstrepcycle>0)) Then 
'     If ( skipnextprint=0) Then 
'		Print #1,Hex(CS1,4);"(";hex(cs0,6);"):";hex(pc,4);" : ";hex(EAX,8);" ";hex(EBX,8);" ";hex(ECX,8);" ";hex(EDX,8);" ";hex(CS1,4);" ";hex(DS1,4);" ";hex(ES1,4);"(";Hex(es0,8);") ";
'		Print #1,Hex(FS1,4);" ";hex(GS1,4);" ";hex(SS1,4);"(";hex(ss0,8);") ";hex(EDI,8);" ";hex(ESI,8);" ";hex(EBP,8);" SP=";hex(SS1,4);":";hex(ESP,8);" ";Hex(opcode,2);" ";hex(flags,4);'" ";
'		Print #1,ins;" ";hex(writelookup2,8);"  ";hex(ldt.base0,8);CPL;stack32;" ";hex(pic.pend,2);" ";hex(pic.mask,2);" ";hex(pic.mask2,2);"   ";hex(pic2.pend,2);" ";hex(pic2.mask,2);" ";hex(readmode,2) 
'     EndIf
'     skipnextprint=0 
'  EndIf
'EndIf



'If EAX=&H001FA000 And EBX=&H0000A8FE And ECX=&H00004000 And EDX=&H00000000 Then deb=2'Print #5,"hola":sleep
               
                ' depuracion
                printdebug()

                pc+=1 
                inhlt=0 

                Select Case As Const  ((opcode Or op32) And &h3FF)
                	Case &h00, &h100, &h200, &h300  /'ADD 8,reg'/
                        fetchea32() : if abrt Then exit Select 
                        if modo=3 Then 
                                temp  = getr8(rm) 
                                temp2 = getr8(reg) 
                                setadd8(temp, temp2) 
                                setr8(rm, temp + temp2) 
                                cycles -= timing_rr 
                        else
                                temp  = geteab(): if abrt Then exit Select 
                                temp2 = getr8(reg) 
                                seteab(temp + temp2): if abrt then exit Select 
                                setadd8(temp, temp2) 
                                cycles -= timing_mr 
                        EndIf
                        Exit Select 
                	case &h01, &h201  /'ADD 16,reg'/
                        fetchea32() : if abrt Then exit Select 
                        if modo=3 Then 
                                setadd16(regs(rm).w, regs(reg).w) 
                                regs(rm).w += regs(reg).w 
                                cycles -= timing_rr 
                        else
                                tempw = geteaw(): if abrt Then exit Select 
                                seteaw(tempw + regs(reg).w):  if abrt then exit Select 
                                setadd16(tempw, regs(reg).w) 
                                cycles -= timing_mr 
                        EndIf
                        Exit Select 
                	case &h101, &h301  /'ADD 32,reg'/
                        fetchea32() : if abrt Then exit Select 
                        if modo=3 Then 
                                setadd32(regs(rm).l, regs(reg).l) 
                                regs(rm).l += regs(reg).l 
                                cycles -= timing_rr 
                        else
                                templ = geteal(): if abrt Then exit Select 
                                seteal(templ + regs(reg).l):  if abrt then exit Select 
                                setadd32(templ, regs(reg).l) 
                                cycles -= timing_mrl 
                        EndIf
                        Exit Select 
                	case &h02, &h102, &h202, &h302  /'ADD reg,8'/
                        fetchea32() : if abrt Then exit Select 
                        temp=geteab(): if abrt Then exit Select 
                        setadd8(getr8(reg),temp) 
                        setr8(reg,getr8(reg)+temp) 
                        cycles -= IIf((modo = 3) , timing_rr , timing_rm) 
                        Exit Select 
                	case &h03, &h203  /'ADD reg,16'/
                        fetchea32() : if abrt Then exit Select 
                        tempw=geteaw(): if abrt Then exit Select 
                        setadd16(regs(reg).w,tempw) 
                        regs(reg).w+=tempw 
                        cycles -= IIf((modo = 3) , timing_rr , timing_rm) 
                        Exit Select 
                	case &h103, &h303  /'ADD reg,32'/
                        fetchea32() : if abrt Then exit Select 
                        templ=geteal(): if abrt Then exit Select 
                        setadd32(regs(reg).l,templ) 
                        regs(reg).l+=templ 
                        cycles -= IIf((modo = 3) , timing_rr , timing_rml) 
                        Exit Select 
                	case &h04, &h104, &h204, &h304  /'ADD AL,#8'/
                        temp=getbytef() 
                        setadd8(AL,temp) 
                        AL+=temp 
                        cycles -= timing_rr 
                        Exit Select 
                	case &h05, &h205  /'ADD AX,#16'/
                        tempw=getwordf() 
                        setadd16(AX,tempw) 
                        AX+=tempw 
                        cycles -= timing_rr 
                        Exit Select 
                	case &h105, &h305  /'ADD EAX,#32'/
                        templ=getlong(): if abrt then exit Select 
                        setadd32(EAX,templ) 
                        EAX+=templ 
                        cycles -= timing_rr 
                        Exit Select 
                        
                        
                        '''''''''''''''''''''''''''
                	case &h06, &h206  /'PUSH ES'/
                        if ssegs Then ss0=oldss 
                        if stack32 Then 
                                writememw_386(ss0,ESP-2,ES1): if abrt Then exit Select 
                                ESP-=2 
                        else
                                writememw_386(ss0,((SP-2) And &hFFFF),ES1): if abrt then exit Select 
                                SP-=2 
                        EndIf
                        cycles-=2 
                        Exit Select 
                	case &h106, &h306  /'PUSH ES'/
                        if ssegs Then ss0=oldss 
                        if stack32 Then 
                                writememl_386(ss0,ESP-4,ES1): if abrt Then exit Select 
                                ESP-=4 
                        else
                                writememl_386(ss0,((SP-4) And &hFFFF),ES1): if abrt then exit Select 
                                SP-=4 
                        EndIf
                        cycles-=2 
                        Exit Select 
                	case &h07, &h207  /'POP ES'/
                        if ssegs Then ss0=oldss 
                        if stack32 Then 
                                tempw=readmemw_386(ss0,ESP): if abrt Then exit Select 
                                loadseg(tempw, _es):            if abrt then exit Select 
                                ESP+=2 
                        else
                                tempw=readmemw_386(ss0,SP): if abrt Then exit Select 
                                loadseg(tempw, _es):            if abrt then exit Select 
                                SP+=2 
                        EndIf
                        cycles-=3
                        Exit Select 
                	case &h107, &h307  /'POP ES'/
                        if ssegs Then ss0=oldss 
                        if stack32 Then 
                                tempw=readmeml_386(ss0,ESP) And &hFFFF:  if abrt then exit Select 
                                loadseg(tempw, _es):            if abrt then exit Select 
                                ESP+=4 
                        else
                                tempw=readmemw_386(ss0,SP): if abrt Then exit Select 
                                loadseg(tempw, _es):            if abrt then exit Select 
                                SP+=4 
                        EndIf
                        cycles-=3
                        Exit Select 
                        '''''''''''''''''''''''
                        
                        
                	case &h08, &h108, &h208, &h308  /'OR 8,reg'/
                        fetchea32() : if abrt Then exit Select 
                        if modo=3 Then 
                                temp  = getr8(rm)  Or  getr8(reg) 
                                setr8(rm, temp) 
                                setznp8(temp) 
                                cycles -= timing_rr 
                        else
                                temp  = geteab(): if abrt Then exit Select 
                                temp2 = getr8(reg) 
                                seteab(temp  Or  temp2): if abrt Then exit Select 
                                setznp8(temp  Or  temp2) 
                                cycles -= timing_mr 
                        EndIf
                        Exit Select 
                	case &h09, &h209  /'OR 16,reg'/
                        fetchea32() : if abrt Then exit Select 
                        if modo=3 Then 
                                regs(rm).w =regs(rm).w or regs(reg).w 
                                setznp16(regs(rm).w) 
                                cycles -= timing_rr 
                        else
                                tempw = geteaw()  Or  regs(reg).w: if abrt then exit Select 
                                seteaw(tempw): if abrt Then exit Select 
                                setznp16(tempw) 
                                cycles -= timing_mr 
                        EndIf
                        Exit Select 
                	case &h109, &h309  /'OR 32,reg'/
                        fetchea32() : if abrt Then exit Select 
                        if modo=3 Then 
                                regs(rm).l = regs(rm).l Or regs(reg).l 
                                setznp32(regs(rm).l) 
                                cycles -= timing_rr 
                        else
                                templ = geteal()  Or  regs(reg).l: if abrt then exit Select 
                                seteal(templ): if abrt Then exit Select 
                                setznp32(templ) 
                                cycles -= timing_mrl 
                        EndIf
                        Exit Select 
                	case &h0A, &h10A, &h20A, &h30A  /'OR reg,8'/
                        fetchea32() : if abrt Then exit Select 
                        temp=geteab(): if abrt Then exit Select 
                        temp = temp Or getr8(reg) 
                        setznp8(temp) 
                        setr8(reg,temp) 
                        cycles -= iif((modo = 3) , timing_rr , timing_rm ) 
                        Exit Select 
                	case &h0B, &h20B  /'OR reg,16'/
                        fetchea32() : if abrt Then exit Select 
                        tempw=geteaw(): if abrt Then exit Select 
                        tempw = tempw Or regs(reg).w 
                        setznp16(tempw) 
                        regs(reg).w=tempw 
                        cycles -= IIf((modo = 3 ) , timing_rr , timing_rm )
                        Exit Select 
                	case &h10B, &h30B  /'OR reg,32'/
                        fetchea32() : if abrt Then exit Select 
                        templ=geteal(): if abrt Then exit Select 
                        templ = templ Or regs(reg).l 
                        setznp32(templ) 
                        regs(reg).l=templ 
                        cycles -= iif((modo = 3) , timing_rr , timing_rml) 
                        Exit Select 
                	case &h0C, &h10C, &h20C, &h30C  /'OR AL,#8'/
                        AL = AL Or getbytef() 
                        setznp8(AL) 
                        cycles -= timing_rr 
                        Exit Select 
                	case &h0D, &h20D  /'OR AX,#16'/
                        AX = AX Or getwordf() 
                        setznp16(AX) 
                        cycles -= timing_rr 
                        Exit Select 
                	Case &h10D, &h30D  /'OR AX,#32'/
                        templ=getlong(): if abrt then exit Select 
                        EAX = EAX Or templ 
                        setznp32(EAX) 
                        cycles -= timing_rr 
                        Exit Select 
                	case &h0E, &h20E  /'PUSH CS'/
                        if ssegs Then ss0=oldss 
                        if stack32 Then 
                                writememw_386(ss0,ESP-2,CS1): if abrt Then exit Select 
                                ESP-=2 
                        else
                                writememw_386(ss0,((SP-2) And &hFFFF),CS1): if abrt Then exit Select 
                                SP-=2 
                        EndIf
                        cycles-=2 
                        Exit Select 
                	case &h10E, &h30E  /'PUSH CS'/
                        if ssegs Then ss0=oldss 
                        if stack32 Then 
                                writememl_386(ss0,ESP-4,CS1): if abrt Then exit Select 
                                ESP-=4 
                        else
                                writememl_386(ss0,((SP-4) And &hFFFF),CS1): if abrt Then exit select 
                                SP-=4 
                        EndIf
                        cycles-=2 
                        Exit Select 
                	case &h0F, &h20F 
                        temp=fetchdat And &hFF: pc+=1 
                        opcode2=temp 
                        Select Case As Const  (temp)
                        	Case 0 
                                if ((cr0 And 1)=0) Or (eflags And VM_FLAG) Then goto inv16 
                                fetchea2() : if abrt then exit Select 
                                Select Case As Const  (rmdat And &h38)
                                	Case &h00  /'SLDT'/
                                        if NOTRM() then exit Select
                                        seteaw(ldt.seg) 
                                        cycles-=4 
                                        Exit Select 
                                	case &h08  /'STR'/
                                        if NOTRM() then exit Select
                                        seteaw(tr.seg) 
                                        cycles-=4 
                                        Exit Select 
                                	case &h10  /'LLDT'/
                                        if ((CPL<>0) Or ((eflags And VM_FLAG)<>0)) And ((cr0 And 1)<>0) Then 
                                                'if deb=3 then print #5,"Invalid LLDT"
                                                x86gpf(0) 
                                                Exit Select
                                        EndIf
                                        if NOTRM() then exit Select
                                        ldt.seg=geteaw() 
                                        templ=(ldt.seg And inv(7))+gdt.base0 
                                        templ3=readmemw_386(0,templ)+((readmemb_386(0,templ+6) And &hF) Shl 16) 
                                        templ2=(readmemw_386(0,templ+2)) Or (readmemb_386(0,templ+4) Shl 16) Or (readmemb_386(0,templ+7) Shl 24) 
                                        if abrt Then exit Select 
                                        ldt.limit=templ3 
                                        ldt.access0=readmemb_386(0,templ+6) 
                                        if (readmemb_386(0,templ+6) And &h80) Then 
                                                ldt.limit = ldt.limit Shl 12 
                                                ldt.limit = ldt.limit Or &hFFF 
                                        EndIf
                                        ldt.base0=templ2 
                                        cycles-=20 
                                        Exit Select 
                                	case &h18  /'LTR'/
                                        if ((CPL<>0) Or ((eflags And VM_FLAG)<>0)) And ((cr0 And 1)<>0) Then 
                                                'if deb=3 then print #5,"Invalid LTR" 
                                                x86gpf(0) 
                                                Exit Select 
                                        EndIf
                                        if NOTRM() then exit Select
                                        tr.seg=geteaw() 
                                        templ=(tr.seg And inv(7))+gdt.base0 
                                        templ3=readmemw_386(0,templ)+((readmemb_386(0,templ+6) And &hF) Shl 16) 
                                        templ2=(readmemw_386(0,templ+2)) Or (readmemb_386(0,templ+4) Shl 16) Or (readmemb_386(0,templ+7) Shl 24) 
                                        temp=readmemb_386(0,templ+5) 
                                        if abrt Then exit Select 
                                        tr.limit=templ3 
                                        tr.access0=readmemb_386(0,templ+6) 
                                        if (readmemb_386(0,templ+6) And &h80) Then 
                                                tr.limit = tr.limit Shl 12 
                                                tr.limit = tr.limit Or &hFFF 
                                        EndIf
                                        tr.base0=templ2 
                                        tr.access0=temp 
                                        cycles-=20 
                                        Exit Select 
                                	case &h20  /'VERR'/
                                        if NOTRM() then exit Select
                                        tempw=geteaw(): if abrt then exit Select 
                                        flags = flags And inv(Z_FLAG) 
                                        if (tempw And &hFFFC)=0 then exit Select: /'NULO selector'/
                                        cpl_override=1 
                                        tempi=(tempw And inv(7))<IIf((tempw And 4),ldt.limit,gdt.limit) 
                                        tempw2=readmemw_386(0,iif((tempw And 4),ldt.base0,gdt.base0)+(tempw And inv(7))+4) 
                                        cpl_override=0 
                                        if abrt Then exit Select 
                                        if (tempw2 And &h1000)=0 Then tempi=0 
                                        if (tempw2 And &hC00)<>&hC00 Then 
                                                /'Exclude conforming code segments'/
                                                tempw3=(tempw2 Shr 13) And 3: /'Check permissions'/
                                                if (tempw3<CPL) Or (tempw3<(tempw And 3)) Then tempi=0 
                                        EndIf
                                        if (tempw2 And &h0800) And ((tempw2 And &h0200)=0) Then tempi=0: /'Non-readable code'/
                                        if (tempi) Then flags = flags Or Z_FLAG 
                                        cycles-=20 
                                        Exit Select 
                                	case &h28  /'VERW'/
                                        if NOTRM() then exit Select
                                        tempw=geteaw(): if abrt then exit Select 
                                        flags = flags And inv(Z_FLAG) 
                                        if (tempw And &hFFFC)=0 then exit Select: /'NULO selector'/
                                        cpl_override=1 
                                        tempi=(tempw And inv(7))<IIf((tempw And 4),ldt.limit,gdt.limit) 
                                        tempw2=readmemw_386(0,iif((tempw And 4),ldt.base0,gdt.base0)+(tempw And inv(7))+4) 
                                        cpl_override=0 
                                        if abrt Then exit Select 
                                        if (tempw2 And &h1000)=0 Then tempi=0 
                                        tempw3=(tempw2 Shr 13) And 3: /'Check permissions'/
                                        if (tempw3<CPL)  Or  (tempw3<(tempw And 3)) Then tempi=0 
                                        if (tempw2 And &h0800) Then tempi=0 /'Code'/
                                        If (tempw2 And &h0200)=0 Then tempi=0 /'Read-only data'/
                                        if (tempi) Then flags = flags Or Z_FLAG 
                                        cycles-=20 
                                        Exit Select 
                                	case Else 
                                        'if deb=3 then print #5,"Bad 0F 00 opcode ",hex(rmdat and &h38,2)
                                        pc-=3 
                                        x86illegal() 
                                        Exit Select 
                               End Select
                               Exit Select 
                        	case 1 
                                fetchea2() : if abrt then exit Select 
                                Select Case As Const  (rmdat And &h38)
                                	case &h00  /'SGDT'/
                                        seteaw(gdt.limit) 
                                        writememw_386(easeg,eaaddr+2,gdt.base0) 
                                        writememw_386(easeg,eaaddr+4,(gdt.base0 Shr 16) And &hFF) 
                                        cycles-=7: ''less seteaw time
                                        Exit Select 
                                	case &h08  /'SIDT'/
                                        seteaw(idt.limit) 
                                        writememw_386(easeg,eaaddr+2,idt.base0) 
                                        writememw_386(easeg,eaaddr+4,(idt.base0 Shr 16) And &hFF) 
                                        cycles-=7 
                                        Exit Select 
                                	case &h10  /'LGDT'/
                                        if ((CPL<>0) Or ((eflags And VM_FLAG)<>0)) And ((cr0 and 1)<>0) Then 
                                                'if deb=3 then print #5,"Invalid LGDT" 
                                                x86gpf(0) 
                                                Exit Select 
                                        EndIf
                                        tempw=geteaw() 
                                        templ=readmeml_386(0,easeg+eaaddr+2) And &hFFFFFF 
                                        if abrt Then exit Select 
                                        gdt.limit=tempw 
                                        gdt.base0=templ 
                                        cycles-=11 
                                        Exit Select 
                                	case &h18  /'LIDT'/
                                        if ((CPL<>0) Or ((eflags And VM_FLAG)<>0)) And ((cr0 and 1)<>0) Then 
                                                'if deb=3 then print #5,"Invalid LIDT" 
                                                x86gpf(0) 
                                                Exit Select 
                                        EndIf
                                        tempw=geteaw() 
                                        templ=readmeml_386(0,easeg+eaaddr+2) And &hFFFFFF 
                                        if abrt Then exit Select 
                                        idt.limit=tempw 
                                        idt.base0=templ 
                                        cycles-=11 
                                        Exit Select 
                                	case &h20  /'SMSW'/
                                        seteaw(msw) 
                                        cycles-=2 
                                        Exit Select 
                                	case &h30  /'LMSW'/
                                        if ((CPL<>0) Or ((eflags And VM_FLAG)<>0))  And (modoprotegido=1) Then 
                                                'if deb=3 then print #5,"LMSW - ring not zero" 
                                                x86gpf(0) 
                                                Exit Select 
                                        EndIf
                                        tempw=geteaw(): if abrt then exit Select 
                                        if modoprotegido Then tempw = tempw Or 1 
                                        msw=tempw 
                                        Exit Select 
                                	case Else 
                                        'if deb=3 then print #5,"Bad 0F 01 opcode ",hex(rmdat and &h38,2) 
                                        pc-=3 
                                        x86illegal() 
                                        Exit Select 
                               End Select
                               Exit Select 
                        	case 2  /'LAR'/
                                if NOTRM() then exit Select
                                fetchea2() : if abrt then exit Select
                                tempw=geteaw(): if abrt then exit Select 
                                if ((tempw And &hFFFC)=0) Then 
                                         flags = flags And inv(Z_FLAG): Exit Select  
                                EndIf /'NULO selector'/
                                tempi=(tempw And inv(7))<IIf((tempw And 4),ldt.limit,gdt.limit) 
                                if (tempi) Then 
                                        cpl_override=1 
                                        tempw2=readmemw_386(0,IIf((tempw And 4),ldt.base0,gdt.base0)+(tempw And inv(7))+4) 
                                        cpl_override=0 
                                        if abrt Then exit Select 
                                EndIf
                                flags = flags And inv(Z_FLAG) 
                                if ((tempw2 And &h1F00)=&h000) Then tempi=0 
                                if ((tempw2 And &h1F00)=&h800) Then tempi=0 
                                if ((tempw2 And &h1F00)=&hA00) Then tempi=0 
                                if ((tempw2 And &h1F00)=&hD00) Then tempi=0 
                                if ((tempw2 And &h1C00)<&h1C00) Then 
                                        /'Exclude conforming code segments'/
                                        tempw3=(tempw2 Shr 13) And 3 
                                        if (tempw3<CPL)  Or  (tempw3<(tempw And 3)) Then tempi=0 
                                EndIf
                                if (tempi) Then 
                                        flags = flags Or Z_FLAG 
                                        cpl_override=1 
                                        regs(reg).w=readmemb_386(0,IIf((tempw And 4),ldt.base0,gdt.base0)+(tempw And inv(7))+5) Shl 8 
                                        cpl_override=0 
                                EndIf
                                cycles-=11 
                                Exit Select 
                        	case 3  /'LSL'/
                                if NOTRM() then exit Select
                                fetchea2() : if abrt then exit Select
                                tempw=geteaw(): if abrt then exit Select 
                                flags = flags And inv(Z_FLAG) 
                                if ((tempw And &hFFFC)=0) then exit Select: /'NULO selector'/
                                tempi=(tempw And inv(7))<IIf((tempw And 4),ldt.limit,gdt.limit) 
                                cpl_override=1 
                                if (tempi) Then 
                                        tempw2=readmemw_386(0,iif((tempw And 4),ldt.base0,gdt.base0)+(tempw And inv(7))+4) 
                                EndIf
                                cpl_override=0 
                                if abrt Then exit Select 
                                if ((tempw2 And &h1400)=&h400) Then tempi=0 /'Interrupt or trap or call gate'/
                                if ((tempw2 And &h1F00)=&h000) Then tempi=0 /'Invalid'/
                                if ((tempw2 And &h1F00)=&hA00) Then tempi=0 /'Invalid'/
                                if ((tempw2 And &h1C00)<>&h1C00) Then 
                                        /'Exclude conforming code segments'/
                                        tempw3=(tempw2 Shr 13) And 3 
                                        if (tempw3<CPL)  Or  (tempw3<(tempw And 3)) Then tempi=0 
                                EndIf
                                if (tempi) Then 
                                        flags = flags Or Z_FLAG 
                                        cpl_override=1 
                                        regs(reg).w=readmemw_386(0,iif((tempw And 4),ldt.base0,gdt.base0)+(tempw And inv(7))) 
                                        cpl_override=0 
                                EndIf
                                cycles-=10 
                                Exit Select 
                        	case 5  /'LOADALL'/
                                flags=(readmemw_386(0,&h818) And &hFFD5) Or 2 
                                '
                                pc=readmemw_386(0,&h81A) 
                                '
                                DS1=readmemw_386(0,&h81E) 
                                SS1=readmemw_386(0,&h820) 
                                CS1=readmemw_386(0,&h822) 
                                ES1=readmemw_386(0,&h824) 
                                '
                                DI=readmemw_386(0,&h826) 
                                SI=readmemw_386(0,&h828) 
                                BP=readmemw_386(0,&h82A) 
                                SP=readmemw_386(0,&h82C) 
                                BX=readmemw_386(0,&h82E) 
                                DX=readmemw_386(0,&h830) 
                                CX=readmemw_386(0,&h832) 
                                AX=readmemw_386(0,&h834) 
                                '
                                es0=readmemw_386(0,&h836) Or (readmemb_386(0,&h838) Shl 16) 
                                cs0=readmemw_386(0,&h83C) Or (readmemb_386(0,&h83E) Shl 16) 
                                ss0=readmemw_386(0,&h842) Or (readmemb_386(0,&h844) Shl 16) 
                                ds0=readmemw_386(0,&h848) Or (readmemb_386(0,&h84A) Shl 16) 
                                '
                                cycles-=195 
                                Exit Select 
                        	case 6  /'CLTS'/
                                if ((CPL<>0) Or ((eflags And VM_FLAG)<>0)) And ((cr0 and 1)<>0) Then 
                                        'if deb=3 then print #5,"Can't CLTS" 
                                        x86gpf(0) 
                                        Exit Select 
                                EndIf
                                cr0 = cr0 And inv(8) 
                                cycles-=5 
                                Exit Select 
                        	case 8  /'INVD'/
                                'if ( is486=0) Then goto inv16 
                                cycles-=1000 
                                Exit Select 
                        	case 9  /'WBINVD'/
                                'if (is486=0) Then goto inv16 
                                cycles-=10000 
                                Exit Select 
                        	case &h20  /'MOV reg32,CRx'/
                                if ((CPL<>0) Or ((eflags And VM_FLAG)<>0)) And ((cr0 and 1)<>0) Then 
                                        'if deb=3 then print #5,"Can't load from CRx" 
                                        x86gpf(0) 
                                        Exit Select 
                                EndIf
                                fetchea2() : if abrt then exit Select
                                Select Case As Const  (reg)
                                	Case 0 
                                        regs(rm).l=cr0
                                        regs(rm).l = regs(rm).l Or &h10 /'ET hardwired on 486'/
                                        Exit Select 
                                	Case 2 
                                        regs(rm).l=cr2 
                                        Exit Select 
                                	Case 3 
                                        regs(rm).l=cr3 
                                        Exit Select 
                                	case Else 
                                        'if deb=3 then print #5,"Bad read of CR ",rmdat And 7,reg 
                                        pc=oldpc 
                                        x86illegal() 
                                        Exit Select 
                                End Select
                                cycles-=6 
                                Exit Select 
                        	case &h21  /'MOV reg32,DRx'/
                                if ((CPL<>0) Or ((eflags And VM_FLAG)<>0)) And ((cr0 and 1)<>0) Then 
                                        'if deb=3 then print #5,"Can't load from DRx" 
                                        x86gpf(0) 
                                        Exit Select 
                                EndIf
                                fetchea2() : if abrt then exit Select 
                                regs(rm).l=0 
                                cycles-=6 
                                Exit Select 
                        	case &h22  /'MOV CRx,reg32'/
                                if ((CPL<>0) Or ((eflags And VM_FLAG)<>0)) And ((cr0 and 1)<>0) Then 
                                        'if deb=3 then print #5,"Can't load CRx" 
                                        x86gpf(0) 
                                        Exit Select 
                                EndIf
                                fetchea2() : if abrt then Exit Select 
                                Select Case As Const  (reg)
                                	case 0 ' EAX
                                        if ((regs(rm).l Xor cr0)  And &h80000001) Then flushmmucache() 
                                        cr0=regs(rm).l:''&~2;
                                        'if (cpu_16bitbus) Then cr0  = cr0  Or  &h10 
                                        if (cr0 And &h80000000)=0 Then 
                                                mmu_perm=4
                                        EndIf
                                        Exit Select 
                                	case 2 ' ECX
                                        cr2=regs(rm).l 
                                        Exit Select 
                                	case 3 ' EDX
                                        cr3=regs(rm).l And inv(&hFFF) 
                                        flushmmucache() 
                                        Exit Select 
                                	case Else 
                                        'if deb=3 then print #5,"Bad load CR ",reg 
                                        pc=oldpc 
                                        x86illegal() 
                                        Exit Select 
                                End Select
                                cycles-=10 
                                Exit Select 
                        	case &h23  /'MOV DRx,reg32'/
                                if ((CPL<>0) Or ((eflags And VM_FLAG)<>0)) And ((cr0 and 1)<>0) Then 
                                        'if deb=3 then print #5,"Can't load DRx" 
                                        x86gpf(0) 
                                        Exit Select 
                                EndIf
                                fetchea2() : if abrt then exit Select
                                cycles-=6 
                                Exit Select 
                        	case &h24  /'MOV reg32,TRx'/
                                if ((CPL<>0) Or ((eflags And VM_FLAG)<>0)) And ((cr0 and 1)<>0) Then 
                                        'if deb=3 then print #5,"Can't load from TRx" 
                                        x86gpf(0) 
                                        Exit Select 
                                EndIf
                                fetchea2() : if abrt then exit Select
                                regs(rm).l=0 
                                cycles-=6 
                                Exit Select 
                        	case &h26  /'MOV TRx,reg32'/
                                if ((CPL<>0) Or ((eflags And VM_FLAG)<>0)) And ((cr0 and 1)<>0) Then 
                                        'if deb=3 then print #5,"Can't load TRx" 
                                        x86gpf(0) 
                                        Exit Select 
                                EndIf
                                fetchea2() : if abrt then exit Select
                                cycles-=6 
                                Exit Select 
                        	case &h32  
                        		  x86illegal()
                        		  Exit Select 
                        	case &h80  /'JO'/
                                tempw=getword2f() 
                                if (flags And V_FLAG) Then 
                                         pc+=CShort(tempw): cycles-=timing_bt  
                                EndIf
                                cycles -= timing_bnt 
                                Exit Select 
                        	case &h81  /'JNO'/
                                tempw=getword2f() 
                                if (flags And V_FLAG)=0 Then 
                                         pc+=CShort(tempw): cycles-=timing_bt  
                                EndIf
                                cycles -= timing_bnt 
                                Exit Select 
                        	case &h82  /'JB'/
                                tempw=getword2f() 
                                if (flags And C_FLAG) Then 
                                         pc+=CShort(tempw): cycles-=timing_bt  
                                EndIf
                                cycles -= timing_bnt 
                                Exit Select 
                        	case &h83  /'JNB'/
                                tempw=getword2f() 
                                if (flags And C_FLAG)=0 Then 
                                         pc+=CShort(tempw): cycles-=timing_bt  
                                EndIf
                                cycles -= timing_bnt 
                                Exit Select 
                        	case &h84  /'JE'/
                                tempw=getword2f() 
                                if (flags And Z_FLAG) Then 
                                         pc+=CShort(tempw): cycles-=timing_bt  
                                EndIf
                                cycles -= timing_bnt 
                                Exit Select 
                        	case &h85  /'JNE'/
                                tempw=getword2f() 
                                if (flags And Z_FLAG)=0 Then 
                                         pc+=CShort(tempw): cycles-=timing_bt  
                                EndIf
                                cycles -= timing_bnt 
                                Exit Select 
                        	case &h86  /'JBE'/
                                tempw=getword2f() 
                                if (flags And (C_FLAG Or Z_FLAG)) Then 
                                         pc+=CShort(tempw): cycles-=timing_bt  
                                EndIf
                                cycles -= timing_bnt 
                                Exit Select 
                        	case &h87  /'JNBE'/
                                tempw=getword2f() 
                                if (flags And (C_FLAG Or Z_FLAG))=0 Then 
                                         pc+=CShort(tempw): cycles-=timing_bt  
                                EndIf
                                cycles -= timing_bnt 
                                Exit Select 
                        	case &h88  /'JS'/
                                tempw=getword2f() 
                                if (flags And N_FLAG) Then 
                                         pc+=CShort(tempw): cycles-=timing_bt  
                                EndIf
                                cycles -= timing_bnt 
                                Exit Select 
                        	case &h89  /'JNS'/
                                tempw=getword2f() 
                                if (flags And N_FLAG)=0 Then 
                                         pc+=CShort(tempw): cycles-=timing_bt  
                                EndIf
                                cycles -= timing_bnt 
                                Exit Select 
                        	case &h8A  /'JP'/
                                tempw=getword2f() 
                                if (flags And P_FLAG) Then 
                                         pc+=CShort(tempw): cycles-=timing_bt  
                                EndIf
                                cycles -= timing_bnt 
                                Exit Select 
                        	case &h8B  /'JNP'/
                                tempw=getword2f() 
                                if (flags And P_FLAG)=0 Then 
                                         pc+=CShort(tempw): cycles-=timing_bt  
                                EndIf
                                cycles -= timing_bnt 
                                Exit Select 
                        	case &h8C  /'JL'/
                                tempw=getword2f() 
                                temp=IIf((flags And N_FLAG),1,0) 
                                temp2=IIf((flags And V_FLAG),1,0 )
                                if (temp<>temp2) Then 
                                          pc+=CShort(tempw): cycles-=timing_bt  
                                EndIf
                                cycles -= timing_bnt 
                                Exit Select 
                        	case &h8D  /'JNL'/
                                tempw=getword2f() 
                                temp=IIf((flags And N_FLAG),1,0) 
                                temp2=IIf((flags And V_FLAG),1,0 )
                                if (temp=temp2) Then 
                                          pc+=CShort(tempw): cycles-=timing_bt  
                                EndIf
                                cycles -= timing_bnt 
                                Exit Select 
                        	case &h8E  /'JLE'/
                                tempw=getword2f() 
                                temp=IIf((flags And N_FLAG),1,0 )
                                temp2=IIf((flags And V_FLAG),1,0 )
                                if ((flags And Z_FLAG)<>0) Or (temp<>temp2) Then 
                                          pc+=CShort(tempw): cycles-=timing_bt  
                                EndIf
                                cycles -= timing_bnt 
                                Exit Select 
                        	case &h8F  /'JNLE'/
                                tempw=getword2f() 
                                temp=IIf((flags And N_FLAG),1,0 )
                                temp2=IIf((flags And V_FLAG),1,0 )
                                if ( ((flags And Z_FLAG)<>0) Or (temp<>temp2) )=0 Then 
                                          pc+=cshort(tempw): cycles-=timing_bt  
                                EndIf
                                cycles -= timing_bnt 
                                Exit Select 
                        	case &h90  /'SETO'/
                                fetchea2() : if abrt then exit Select
                                seteab(IIf((flags And V_FLAG),1,0) )
                                cycles-=4 
                                Exit Select 
                        	case &h91  /'SETNO'/
                                fetchea2() : if abrt then exit Select
                                seteab(IIf((flags And V_FLAG),0,1) )
                                cycles-=4 
                                Exit Select 
                        	case &h92  /'SETC'/
                                fetchea2() : if abrt then exit Select
                                seteab(IIf((flags And C_FLAG),1,0)) 
                                cycles-=4 
                                Exit Select 
                        	case &h93  /'SETAE'/
                                fetchea2() : if abrt then exit Select
                                seteab(IIf((flags And C_FLAG),0,1) )
                                cycles-=4 
                                Exit Select 
                        	case &h94  /'SETZ'/
                                fetchea2() : if abrt then exit Select
                                seteab(IIf((flags And Z_FLAG),1,0)) 
                                cycles-=4 
                                Exit Select 
                        	case &h95  /'SETNZ'/
                                fetchea2() : if abrt then exit Select
                                seteab(IIf((flags And Z_FLAG),0,1)) 
                                cycles-=4 
                                Exit Select 
                        	case &h96  /'SETBE'/
                                fetchea2() : if abrt then exit Select
                                seteab(IIf((flags And (C_FLAG Or Z_FLAG)),1,0)) 
                                cycles-=4 
                                Exit Select 
                        	case &h97  /'SETNBE'/
                                fetchea2() : if abrt then exit Select
                                seteab(IIf((flags And (C_FLAG Or Z_FLAG)),0,1)) 
                                cycles-=4 
                                Exit Select 
                        	case &h98  /'SETS'/
                                fetchea2() : if abrt then exit Select
                                seteab(IIf((flags And N_FLAG),1,0)) 
                                cycles-=4 
                                Exit Select 
                        	case &h99  /'SETNS'/
                                fetchea2() : if abrt then exit Select 
                                seteab(IIf((flags And N_FLAG),0,1)) 
                                cycles-=4 
                                Exit Select 
                        	case &h9A  /'SETP'/
                                fetchea2() : if abrt then exit Select 
                                seteab(IIf((flags And P_FLAG),1,0)) 
                                cycles-=4 
                                Exit Select 
                        	case &h9B  /'SETNP'/
                                fetchea2() : if abrt then exit Select 
                                seteab(IIf((flags And P_FLAG),0,1)) 
                                cycles-=4 
                                Exit Select 
                        	case &h9C  /'SETL'/
                                fetchea2() : if abrt then exit Select 
                                temp=IIf((flags And N_FLAG),1,0) 
                                temp2=IIf((flags And V_FLAG),1,0) 
                                seteab(temp Xor temp2) 
                                cycles-=4 
                                Exit Select 
                        	case &h9D  /'SETGE'/
                                fetchea2() : if abrt then exit Select 
                                temp=IIf((flags And N_FLAG),1,0) 
                                temp2=IIf((flags And V_FLAG),1,0) 
                                seteab(IIf((temp Xor temp2),0,1)) 
                                cycles-=4 
                                Exit Select 
                        	case &h9E  /'SETLE'/
                                fetchea2() : if abrt then exit Select 
                                temp=IIf((flags And N_FLAG),1,0 )
                                temp2=IIf((flags And V_FLAG),1,0 )
                                seteab(IIf(((temp Xor temp2)  Or  (flags And Z_FLAG)),1,0)) 
                                cycles-=4 
                                Exit Select 
                        	case &h9F  /'SETNLE'/
                                fetchea2() : if abrt then exit Select 
                                temp=IIf((flags And N_FLAG),1,0 )
                                temp2=IIf((flags And V_FLAG),1,0) 
                                seteab(IIf(((temp Xor temp2)  Or  (flags And Z_FLAG)),0,1)) 
                                cycles-=4 
                                Exit Select 
                        	case &hA0  /'PUSH FS'/
                                if ssegs Then ss0=oldss 
                                if stack32 Then 
                                        writememw_386(ss0,ESP-2,FS1): if abrt then exit Select 
                                        ESP-=2 
                                else
                                        writememw_386(ss0,((SP-2) And &hFFFF),FS1): if abrt then exit Select 
                                        SP-=2 
                                EndIf
                                cycles-=2 
                                Exit Select 
                        	case &hA1  /'POP FS'/
                                if ssegs Then ss0=oldss 
                                if stack32 Then 
                                        tempw=readmemw_386(ss0,ESP): if abrt Then exit Select 
                                        loadseg(tempw, _fs):            if abrt then exit Select 
                                        ESP+=2 
                                else
                                        tempw=readmemw_386(ss0,SP): if abrt Then exit Select 
                                        loadseg(tempw, _fs):            if abrt then exit Select 
                                        SP+=2 
                                EndIf
                                cycles-=3
                                Exit Select 
                        	case &hA3  /'BT r16'/
                                fetchea2() : if abrt then exit Select 
                                eaaddr+=((regs(reg).w \ 16)*2): eal_r = 0 
                                tempw=geteaw(): if abrt then exit Select 
                                if ((tempw And (1 Shl (regs(reg).w And 15)))) Then 
                                        flags = flags Or C_FLAG 
                                else
                                       flags = flags And inv(C_FLAG)
                                EndIf
                                cycles-=3 
                                Exit Select 
                        	case &hA4  /'SHLD imm'/
                                fetchea2() : if abrt then exit Select 
                                temp=readmemb_386(cs0,pc) And 31: pc+=1 
                                if (temp) Then 
                                        tempw=geteaw(): if abrt then exit Select 
                                        tempc=IIf(((tempw Shl (temp-1)) And &h8000),1,0) 
                                        templ=(tempw Shl 16) Or regs(reg).w 
                                        if (temp<=16) Then 
                                                tempw=templ Shr (16-temp) 
                                        else
                                                tempw=(templ Shl temp) Shr 16
                                        EndIf
                                        seteaw(tempw): if abrt then exit Select 
                                        setznp16(tempw) 
                                        if tempc Then flags = flags Or C_FLAG 
                                EndIf
                                cycles-=3 
                                Exit Select 
                        	case &hA5  /'SHLD CL'/
                                fetchea2() : if abrt then exit Select 
                                temp=CL And 31 
                                if (temp) Then 
                                        tempw=geteaw(): if abrt then exit Select 
                                        tempc=IIf(((tempw Shl (temp-1)) And &h8000),1,0) 
                                        templ=(tempw Shl 16) Or regs(reg).w 
                                        if (temp<=16) Then 
                                                tempw=templ Shr (16-temp) 
                                        else
                                                tempw=(templ Shl temp) Shr 16
                                        EndIf
                                        seteaw(tempw): if abrt then exit Select 
                                        setznp16(tempw) 
                                        if tempc Then flags = flags Or C_FLAG 
                                EndIf
                                cycles-=3 
                                Exit Select 
                        	case &hA8  /'PUSH GS'/
                                if ssegs Then ss0=oldss 
                                if stack32 Then 
                                        writememw_386(ss0,ESP-2,GS1): if abrt then exit Select 
                                        ESP-=2 
                                else
                                        writememw_386(ss0,((SP-2) And &hFFFF),GS1): if abrt then exit Select 
                                        SP-=2 
                                EndIf
                                cycles-=2 
                                Exit Select 
                        	case &hA9  /'POP GS'/
                                if ssegs Then ss0=oldss 
                                if stack32 Then 
                                        tempw=readmemw_386(ss0,ESP): if abrt Then exit Select 
                                        loadseg(tempw, _gs):            if abrt then exit Select 
                                        ESP+=2 
                                else
                                        tempw=readmemw_386(ss0,SP): if abrt Then exit Select 
                                        loadseg(tempw, _gs):            if abrt then exit Select 
                                        SP+=2 
                                EndIf
                                cycles-=3
                                Exit Select 
                        	case &hAB  /'BTS r16'/
                                fetchea2() : if abrt then exit Select 
                                eaaddr+=((regs(reg).w \ 16)*2)
                                eal_r = 0
                                eal_w = 0 
                                tempw=geteaw(): if abrt then exit Select 
                                tempc=IIf((tempw And (1 Shl (regs(reg).w And 15))),1,0) 
                                tempw = tempw Or (1 Shl (regs(reg).w And 15)) 
                                seteaw(tempw): if abrt then exit Select 
                                if tempc Then 
                                        flags = flags Or C_FLAG 
                                else
                                        flags = flags And inv(C_FLAG)
                                EndIf
                                cycles-=6 
                                Exit Select 
                        	case &hAC  /'SHRD imm'/
                                fetchea2() : if abrt then exit Select 
                                temp=readmemb_386(cs0,pc) And 31: pc+=1 
                                if (temp) Then 
                                        tempw=geteaw(): if abrt then exit Select 
                                        tempc=(tempw Shr (temp-1)) And 1 
                                        templ=tempw Or (regs(reg).w Shl 16) 
                                        tempw=templ Shr temp 
                                        seteaw(tempw): if abrt then exit Select 
                                        setznp16(tempw) 
                                        if tempc Then flags = flags Or C_FLAG 
                                EndIf
                                cycles-=3 
                                Exit Select 
                        	case &hAD  /'SHRD CL'/
                                fetchea2() : if abrt then exit Select 
                                temp=CL And 31 
                                if (temp) Then 
                                        tempw=geteaw(): if abrt then exit Select 
                                        tempc=(tempw Shr (temp-1)) And 1 
                                        templ=tempw Or (regs(reg).w Shl 16) 
                                        tempw=templ Shr temp 
                                        seteaw(tempw): if abrt then exit Select 
                                        setznp16(tempw) 
                                        if tempc Then flags = flags Or C_FLAG 
                                EndIf
                                cycles-=3 
                                Exit Select 
                        	case &hAF  /'IMUL reg16,rm16'/
                                fetchea2() : if abrt then exit Select 
                                templ=CLng(CShort(regs(reg).w))*CLng(CShort(geteaw())) 
                                if abrt Then exit Select 
                                regs(reg).w=templ And &hFFFF 
                                if ((templ Shr 16)<>0)  And  ((templ Shr 16) <> &hFFFF) Then 
                                        flags = flags Or C_FLAG Or V_FLAG 
                                else
                                        flags = flags And inv(C_FLAG Or V_FLAG)
                                EndIf
                                cycles-=18 
                                Exit Select 
                        	case &hB0  /'CMPXCHG rm8, reg8'/
                                'if ( is486=0) Then goto inv16 
                                fetchea2() : if abrt then exit Select 
                                temp = geteab() 
                                setsub8(AL, temp) 
                                if (AL = temp) Then 
                                        seteab(getr8(reg)) 
                                else
                                        AL = temp
                                EndIf
                                cycles -= iif((modo = 3) , 6 , 10) 
                                Exit Select 
                        	case &hB1  /'CMPXCHG rm16, reg16'/
                                'If ( is486=0) Then goto inv16 
                                fetchea2() : if abrt then exit Select 
                                tempw = geteaw() 
                                setsub16(AX, tempw) 
                                if (AX = tempw) Then 
                                      seteaw(regs(reg).w) 
                                else
                                      AX = tempw
                                EndIf
                                cycles -= iif((modo = 3) , 6 , 10) 
                                Exit Select 
                        	case &hB3  /'BTR r16'/
                                fetchea2() : if abrt then exit Select 
                                eaaddr+=((regs(reg).w \ 16)*2)
                                eal_r = 0
                                eal_w = 0
                                tempw=geteaw(): if abrt then exit Select 
                                tempc=tempw And (1 Shl (regs(reg).w And 15)) 
                                tempw = tempw And inv(1 Shl (regs(reg).w And 15)) 
                                seteaw(tempw): if abrt then exit Select 
                                if tempc Then 
                                        flags = flags Or C_FLAG 
                                else
                                        flags = flags And inv(C_FLAG)
                                EndIf
                                cycles-=6 
                                Exit Select 
                        	case &hB2  /'LSS'/
                                fetchea2() : if abrt then exit Select 
                                tempw2=readmemw_386(easeg,eaaddr) 
                                tempw =readmemw_386(easeg,(eaaddr+2)): if abrt then exit Select 
                                loadseg(tempw, _ss): if abrt then exit Select 
                                regs(reg).w=tempw2 
                                oldss=ss0 
                                cycles-=7 
                                Exit Select 
                        	case &hB4  /'LFS'/
                                fetchea2() : if abrt then exit Select 
                                tempw2=readmemw_386(easeg,eaaddr) 
                                tempw=readmemw_386(easeg,(eaaddr+2)): if abrt then exit Select 
                                loadseg(tempw, _fs): if abrt then exit Select 
                                regs(reg).w=tempw2 
                                cycles-=7 
                                Exit Select 
                        	case &hB5  /'LGS'/
                                fetchea2() : if abrt then exit Select 
                                tempw2=readmemw_386(easeg,eaaddr) 
                                tempw=readmemw_386(easeg,(eaaddr+2)): if abrt then exit Select 
                                loadseg(tempw, _gs): if abrt then exit Select 
                                regs(reg).w=tempw2 
                                cycles-=7 
                                Exit Select 
                        	case &hB6  /'MOVZX b'/
                                fetchea2() : if abrt then exit Select 
                                tempw=geteab(): if abrt then exit Select 
                                regs(reg).w=tempw 
                                cycles-=3 
                                Exit Select 
                        	case &hB7  /'MOVZX w'/
                                fetchea2() : if abrt then exit Select: /'Slightly pointless?'/
                                tempw=geteaw(): if abrt then exit Select 
                                regs(reg).w=tempw 
                                cycles-=3 
                                Exit Select 
                        	case &hBE  /'MOVSX b'/
                                fetchea2() : if abrt then exit Select 
                                tempw=geteab(): if abrt then exit Select 
                                if (tempw And &h80) Then tempw = tempw Or &hFF00 
                                regs(reg).w=tempw 
                                cycles-=3 
                                Exit Select 
                        	case &hBA  /'MORE?!?!?!'/
                                fetchea2() : if abrt then exit Select 
                                Select Case As Const  (rmdat And &h38)
                                	case &h20  /'BT w,imm'/
                                        tempw=geteaw() 
                                        temp=readmemb_386(cs0,pc): pc+=1: if abrt then exit Select 
                                        if (tempw And (1 Shl temp)) Then 
                                                flags = flags Or C_FLAG 
                                        else
                                                flags = flags And inv(C_FLAG)
                                        EndIf
                                        cycles-=6 
                                        Exit Select 
                                	case &h28  /'BTS w,imm'/
                                        tempw=geteaw() 
                                        temp=readmemb_386(cs0,pc): pc+=1: if abrt then exit Select 
                                        tempc=IIf((tempw And (1 Shl temp)),1,0) 
                                        tempw = tempw Or (1 Shl temp) 
                                        seteaw(tempw):  if abrt then exit Select 
                                        if tempc Then 
                                                flags = flags Or C_FLAG 
                                        else
                                                flags = flags And inv(C_FLAG)
                                        EndIf
                                        cycles-=6 
                                        Exit Select 
                                	case &h30  /'BTR w,imm'/
                                        tempw=geteaw() 
                                        temp=readmemb_386(cs0,pc): pc+=1: if abrt then exit Select 
                                        tempc=IIf((tempw And (1 Shl temp)),1,0) 
                                        tempw = tempw And inv(1 Shl temp) 
                                        seteaw(tempw): if abrt then exit Select 
                                        if tempc Then 
                                                flags = flags Or C_FLAG 
                                        else
                                                flags = flags And inv(C_FLAG)
                                        EndIf
                                        cycles-=6 
                                        Exit Select 
                                	case &h38  /'BTC w,imm'/
                                        tempw=geteaw() 
                                        temp=readmemb_386(cs0,pc): pc+=1: if abrt then exit Select 
                                        tempc=IIf((tempw And (1 Shl temp)),1,0) 
                                        tempw = tempw Xor (1 Shl temp) 
                                        seteaw(tempw): if abrt then exit Select 
                                        if tempc Then 
                                                flags = flags Or C_FLAG 
                                        else
                                                flags = flags And inv(C_FLAG)
                                        EndIf
                                        cycles-=6 
                                        Exit Select 
                                	case Else 
                                        'if deb=3 then print #5,"Bad 0F BA opcode ",hex(rmdat and &h38,2)
                                        pc=oldpc 
                                        x86illegal() 
                                        Exit Select 
                               End Select
                               Exit Select 
                        	case &hBC  /'BSF w'/
                                fetchea2() : if abrt then exit Select 
                                tempw = geteaw(): if abrt then exit Select 
                                if tempw=0 Then 
                                        flags  = flags  Or  Z_FLAG 
                                else
                                        for  tempi = 0 To 15
                                                cycles -= 1
                                                if (tempw  And (1  Shl  tempi)) Then 
                                                        flags  = flags  And  inv(Z_FLAG) 
                                                        regs(reg).w = tempi 
                                                        Exit Select 
                                                EndIf
                                       Next
                                EndIf
                                cycles -=6 
                                Exit Select 
                        	case &hBD  /'BSR w'/
                                fetchea2() : if abrt then exit Select 
                                tempw = geteaw(): if abrt then exit Select 
                                if tempw=0 Then 
                                        flags  = flags  Or  Z_FLAG 
                                else
                                        for  tempi = 15 To 0 Step -1
                                                cycles -= 3 
                                                if (tempw  And (1  Shl  tempi)) Then 
                                                        flags  = flags  And  inv(Z_FLAG) 
                                                        regs(reg).w = tempi 
                                                        Exit Select 
                                                EndIf
                                       Next
                                EndIf
                                cycles -=6
                                Exit Select 
                        	case &hA2  /'CPUID'/
                                If (CPUID) Then 
                                        cpu_CPUID() 
                                        cycles-=9 
                                        Exit Select 
                                EndIf
                                goto inv16 ' aqui abajo, en la INS FF
                        	case &hC0  /'XADD b'/
                                fetchea2() : if abrt then exit Select 
                                temp=geteab(): if abrt then exit Select 
                                seteab(temp+getr8(reg)): if abrt then exit Select 
                                setadd8(temp,getr8(reg)) 
                                setr8(reg,temp) 
                                Exit Select 
                        	case &hC1  /'XADD w'/
                                fetchea2() : if abrt then exit Select 
                                tempw=geteaw(): if abrt then exit Select 
                                seteaw(tempw+regs(reg).w): if abrt then exit Select 
                                setadd16(tempw,regs(reg).w) 
                                regs(reg).w=tempw 
                                Exit Select 
                        	case &hA6  /'XBTS/CMPXCHG486'/
                                print #5,"CMPXCHG486 sin hacer!!!! no existe!!!!":Sleep
                                ' nota, esta INS deberia ir con la siguiente, CREO
                        	case &hFF  /'Invalid - Windows 3.1 syscall trap,'/
                           'goto  
                           inv16:
                                pc-=2 
                                if modoprotegido Then 
                                        pmodeint(6,0) 
                                else
                                        if ssegs Then ss0=oldss 
                                        if stack32 Then 
                                                writememw_386(ss0,ESP-2,flags) 
                                                writememw_386(ss0,ESP-4,CS1) 
                                                writememw_386(ss0,ESP-6,pc) 
                                                ESP-=6 
                                        else
                                                writememw_386(ss0,((SP-2) And &hFFFF),flags) 
                                                writememw_386(ss0,((SP-4) And &hFFFF),CS1) 
                                                writememw_386(ss0,((SP-6) And &hFFFF),pc) 
                                                SP-=6 
                                        EndIf
                                        addr=6 Shl 2 
                                        flags = flags And inv(I_FLAG) 
                                        flags = flags And inv(T_FLAG) 
                                        oxpc=pc 
                                        pc=readmemw_386(0,addr) 
                                        loadcs(readmemw_386(0,addr+2)) 
                                EndIf
                                cycles-=70 
                                Exit Select 
                        	case Else 
                                'if deb=3 then print #5,"Instruccion 16-bit 0F opcode 386 mala ",temp,ins
                                pc=oldpc 
                                x86illegal() 
                                Exit Select 
                        End Select
                       Exit Select 
                       
                	case &h10F, &h30F 
                        temp=fetchdat And &hFF /'readmemb_386(cs+pc)'/
                        pc+=1 
                        opcode2=temp 
                        Select Case As Const  (temp)
                        	case 0 
                                if ((cr0 And 1)=0) Or ((eflags And VM_FLAG)<>0) Then goto inv32 
                                fetchea2() : if abrt then exit Select 
                                Select Case As Const  (rmdat And &h38)
                                	Case &h00  /'SLDT'/
                                        if NOTRM() then exit Select
                                        seteaw(ldt.seg) 
                                        cycles-=4 
                                        Exit Select 
                                	case &h08  /'STR'/
                                        if NOTRM() then exit Select
                                        seteaw(tr.seg) 
                                        cycles-=4 
                                        Exit Select 
                                	case &h10  /'LLDT'/
                                        if ((CPL<>0) Or ((eflags And VM_FLAG)<>0))   And  ((cr0 And 1)<>0) Then 
                                                'if deb=3 then print #5,"Invalid LLDT32" 
                                                x86gpf(0) 
                                                Exit Select 
                                        EndIf
                                        if NOTRM() then exit Select
                                        ldt.seg=geteaw(): if abrt then exit Select 
                                        templ=(ldt.seg And inv(7))+gdt.base0 
                                        templ3=readmemw_386(0,templ)+((readmemb_386(0,templ+6) And &hF) Shl 16) 
                                        templ2=(readmemw_386(0,templ+2)) Or (readmemb_386(0,templ+4) Shl 16) Or (readmemb_386(0,templ+7) Shl 24) 
                                        if abrt Then exit Select 
                                        ldt.limit=templ3 
                                        ldt.base0=templ2 
                                        ldt.access0=readmemb_386(0,templ+6) 
                                        if (readmemb_386(0,templ+6) And &h80) Then 
                                                ldt.limit = ldt.limit Shl 12 
                                                ldt.limit = ldt.limit Or &hFFF 
                                        EndIf
                                        cycles-=20 
                                        Exit Select 
                                	case &h18  /'LTR'/
                                        if ((CPL<>0) Or ((eflags And VM_FLAG)<>0))   And  ((cr0 And 1)<>0) Then 
                                                'if deb=3 then print #5,"Invalid LTR32" 
                                                x86gpf(0) 
                                                Exit Select 
                                        EndIf
                                        if NOTRM() then exit Select
                                        tempw=geteaw(): if abrt then exit Select 
                                        tr.seg=tempw 
                                        templ=(tempw And inv(7))+gdt.base0 
                                        templ3=readmemw_386(0,templ)+((readmemb_386(0,templ+6) And &hF) Shl 16) 
                                        templ2=(readmemw_386(0,templ+2)) Or (readmemb_386(0,templ+4) Shl 16) Or (readmemb_386(0,templ+7) Shl 24) 
                                        temp=readmemb_386(0,templ+5) 
                                        if abrt Then exit Select 
                                        tr.seg=tempw 
                                        tr.limit=templ3 
                                        tr.access0=readmemb_386(0,templ+6) 
                                        if (readmemb_386(0,templ+6) And &h80) Then 
                                                tr.limit = tr.limit Shl 12 
                                                tr.limit = tr.limit Or &hFFF 
                                        EndIf
                                        tr.base0=templ2 
                                        tr.access0=temp 
                                        cycles-=20 
                                        Exit Select 
                                	case &h20  /'VERR'/
                                        if NOTRM() then exit Select
                                        tempw=geteaw(): if abrt then exit Select 
                                        flags = flags And inv(Z_FLAG) 
                                        if (tempw And &hFFFC)=0 then exit Select /'NULO selector'/
                                        cpl_override=1 
                                        tempi=(tempw And inv(7))<IIf((tempw And 4),ldt.limit,gdt.limit) 
                                        tempw2=readmemw_386(0,iif((tempw And 4),ldt.base0,gdt.base0)+(tempw And inv(7))+4) 
                                        cpl_override=0 
                                        if abrt Then exit Select 
                                        if (tempw2 And &h1000)=0 Then tempi=0 
                                        if (tempw2 And &hC00)<>&hC00 Then 
                                                /'Exclude conforming code segments'/
                                                tempw3=(tempw2 Shr 13) And 3: /'Check permissions'/
                                                if (tempw3<CPL) Or (tempw3<(tempw And 3)) Then tempi=0 
                                        EndIf
                                        if ((tempw2 And &h0800)<>0) And ((tempw2 And &h0200)<>0) Then tempi=0 /'Non-readable code'/
                                        if (tempi) Then flags = flags Or Z_FLAG 
                                        cycles-=20 
                                        Exit Select 
                                	case &h28  /'VERW'/
                                        if NOTRM() then exit Select
                                        tempw=geteaw(): if abrt then exit Select 
                                        flags = flags And inv(Z_FLAG) 
                                        if (tempw And &hFFFC)=0 then exit Select /'NULO selector'/
                                        cpl_override=1 
                                        tempi=(tempw And inv(7))<IIf((tempw And 4),ldt.limit,gdt.limit) 
                                        tempw2=readmemw_386(0,IIf((tempw And 4),ldt.base0,gdt.base0)+(tempw And inv(7))+4) 
                                        cpl_override=0 
                                        if abrt Then exit Select 
                                        if (tempw2 And &h1000)=0 Then tempi=0 
                                        tempw3=(tempw2 Shr 13) And 3 /'Check permissions'/
                                        if (tempw3<CPL)  Or  (tempw3<(tempw And 3)) Then tempi=0 
                                        if (tempw2 And &h0800) Then tempi=0: /'Code'/
                                        If ((tempw2 And &h0200)=0) Then tempi=0 'Read-only data
                                        if (tempi) Then flags = flags Or Z_FLAG 
                                        cycles-=20 
                                        Exit Select 
                                	case Else 
                                        'if deb=3 then print #5,"Bad 0F 00 opcode ",hex(rmdat and &h38,2) 
                                        pc=oldpc 
                                        x86illegal() 
                                        Exit Select 
                               End Select
                               Exit Select 
                        	case 1 
                                fetchea2() : if abrt then exit Select
                                Select Case As Const  (rmdat And &h38)
                                	Case &h00  /'SGDT'/
                                        seteaw(gdt.limit) 
                                        writememl_386(easeg,eaaddr+2,gdt.base0) 
                                        cycles-=7 
                                        Exit Select 
                                	case &h08  /'SIDT'/
                                        seteaw(idt.limit) 
                                        writememl_386(easeg,eaaddr+2,idt.base0) 
                                        cycles-=7 
                                        Exit Select 
                                	case &h10  /'LGDT'/
                                        if ((CPL<>0) Or ((eflags And VM_FLAG)<>0)) And ((cr0 and 1)<>0) Then 
                                                'if deb=3 then print #5,"Invalid LGDT32" 
                                                x86gpf(0) 
                                                Exit Select 
                                        EndIf
                                        tempw=geteaw() 
                                        templ=readmeml_386(easeg,eaaddr+2) 
                                        if abrt Then exit Select 
                                        gdt.limit=tempw 
                                        gdt.base0=templ 
                                        cycles-=11 
                                        Exit Select 
                                	case &h18  /'LIDT'/
                                        if ((CPL<>0) Or ((eflags And VM_FLAG)<>0)) And ((cr0 and 1)<>0) Then 
                                                'if deb=3 then print #5,"Invalid LIDT32" 
                                                x86gpf(0) 
                                                Exit Select 
                                        EndIf
                                        tempw=geteaw() 
                                        'Print #5,"LIDT1:";Hex(easeg),Hex(eaaddr+2)
                                        templ=readmeml_386(easeg,eaaddr+2) 
                                        idt.limit=tempw 
                                        idt.base0=templ 
                                        'Print #5,"LIDT2:";Hex(idt.base0,8),Hex(tempw,8)
                                        cycles-=11 
                                        Exit Select 
                                	case &h20  /'SMSW'/
                                        if (modo<3) Then 
                                              seteaw(cr0) 
                                        else
                                              seteal(cr0)
                                        EndIf /'Apparently this is the case!'/
                                        cycles-=2 
                                        Exit Select 
                                	case &h30  /'LMSW'/
                                        if ((CPL<>0) Or ((eflags And VM_FLAG)<>0))   And (modoprotegido=1) Then 
                                                'if deb=3 then print #5,"LMSW - ring not zero" 
                                                x86gpf(0) 
                                                Exit Select 
                                        EndIf
                                        tempw=geteaw(): if abrt then exit Select 
                                        if modoprotegido Then tempw = tempw Or 1 
                                        msw=tempw 
                                        Exit Select 
                                	case &h38  /'INVLPG'/
                                       if ((CPL<>0) Or ((eflags And VM_FLAG)<>0)) And ((cr0 and 1)<>0) Then 
                                               'if deb=3 then print #5,"Invalid INVLPG" 
                                               x86gpf(0) 
                                               Exit Select 
                                       EndIf
                                       'if (deb=3) Then print #5,"INVLPG  + ", eaaddr, DS1
                                       flushmmucache_cr3() ' en lugar de "mmu_invalidate(ds0 + eaaddr)" uso este
                                       cycles-=12 
                                       Exit Select 
                                	Case Else 
                                        'if deb=3 then print #5,"Bad 0F 01 opcode ",hex(rmdat and &h38,2) 
                                        pc=oldpc 
                                        x86illegal() 
                                        Exit Select 
                               End Select
                               Exit Select 
                        	case 2  /'LAR'/
                                if NOTRM() then exit Select
                                fetchea2() : if abrt then exit Select 
                                tempw=geteaw(): if abrt then exit Select 
                                if (tempw And &hFFFC)=0 Then 
                                         flags = flags And inv(Z_FLAG)
                                         Exit Select  
                                EndIf /'NULO selector'/
                                cpl_override=1 
                                tempi=(tempw And inv(7))<IIf((tempw And 4),ldt.limit,gdt.limit) 
                                tempw2=readmemw_386(0,iif((tempw And 4),ldt.base0,gdt.base0)+(tempw And inv(7))+4) 
                                cpl_override=0 
                                if abrt Then exit Select 
                                flags = flags And inv(Z_FLAG) 
                                if ((tempw2 And &h1F00)=&h000) Then tempi=0 
                                if ((tempw2 And &h1F00)=&h800) Then tempi=0 
                                if ((tempw2 And &h1F00)=&hA00) Then tempi=0 
                                if ((tempw2 And &h1F00)=&hD00) Then tempi=0 
                                if ((tempw2 And &h1C00)<&h1C00) Then 
                                        /'Exclude conforming code segments'/
                                        tempw3=(tempw2 Shr 13) And 3 
                                        if (tempw3<CPL)  Or  (tempw3<(tempw And 3)) Then tempi=0 
                                EndIf
                                if (tempi) Then 
                                        flags = flags Or Z_FLAG 
                                        cpl_override=1 
                                        regs(reg).l=readmeml_386(0,iif((tempw And 4),ldt.base0,gdt.base0)+(tempw And inv(7))+4) And &hFFFF00 
                                        cpl_override=0 
                                EndIf
                                cycles-=11 
                                Exit Select 
                        	case 3  /'LSL'/
                                if NOTRM() then exit Select
                                fetchea2() : if abrt then exit Select 
                                tempw=geteaw(): if abrt then exit Select 
                                'if (deb=3) Then print #5,"LSL ",tempw 
                                if (tempw And &hFFFC)=0 Then 
                                      flags = flags And inv(Z_FLAG): Exit Select  
                                EndIf /'NULO selector'/
                                cpl_override=1 
                                tempi=(tempw And inv(7))<iif((tempw And 4),ldt.limit,gdt.limit) 
                                'if (deb=3) Then print #5,"In range? ",tempi 
                                if (tempi) Then 
                                        tempw2=readmemw_386(0,iif((tempw And 4),ldt.base0,gdt.base0)+(tempw And inv(7))+4) 
                                        'if (deb=3) Then print #5,"segdat(2) = ",tempw2 
                                EndIf
                                cpl_override=0 
                                if abrt Then exit Select 
                                flags = flags And inv(Z_FLAG) 
                                if ((tempw2 And &h1400)=&h400) Then tempi=0: /'Interrupt or trap or call gate'/
                                if ((tempw2 And &h1F00)=&h000) Then tempi=0: /'Invalid'/
                                if ((tempw2 And &h1F00)=&hA00) Then tempi=0: /'Invalid'/
                                if ((tempw2 And &h1C00)<>&h1C00) Then 
                                        /'Exclude conforming code segments'/
                                        tempw3=(tempw2 Shr 13) And 3 
                                        if (tempw3<CPL)  Or  (tempw3<(tempw And 3)) Then tempi=0 
                                EndIf
                                if ((deb=3)) Then print #5,"Final tempi ",tempi 
                                if (tempi) Then 
                                        flags = flags Or Z_FLAG 
                                        cpl_override=1 
                                        regs(reg).l=readmemw_386(0,iif((tempw And 4),ldt.base0,gdt.base0)+(tempw And inv(7))) 
                                        regs(reg).l = regs(reg).l Or (readmemb_386(0,iif((tempw And 4),ldt.base0,gdt.base0)+(tempw And inv(7))+6) And &hF) Shl 16 
                                        if (readmemb_386(0,iif((tempw And 4),ldt.base0,gdt.base0)+(tempw And inv(7))+6) And &h80) Then 
                                                regs(reg).l = (regs(reg).l Shl 12 ) Or &hFFF 
                                        EndIf
                                        cpl_override=0 
                                EndIf
                                cycles-=10 
                                Exit Select 
                        	case 6  /'CLTS'/
                                if ((CPL<>0) Or ((eflags And VM_FLAG)<>0)) And ((cr0 and 1)<>0) Then 
                                        'if deb=3 then print #5,"Can't CLTS" 
                                        x86gpf(0) 
                                        Exit Select 
                                EndIf
                                cr0 = cr0 And inv(8) 
                                cycles-=5 
                                Exit Select 
                        	case 8  /'INVD'/
                                'if ( is486=0) Then GoTo inv32 
                                cycles-=1000 
                                Exit Select 
                        	case 9  /'WBINVD'/
                                'if ( is486=0) Then goto inv32 
                                cycles-=10000 
                                Exit Select 
                        	case &h20  /'MOV reg32,CRx'/
                                if ((CPL<>0) Or ((eflags And VM_FLAG)<>0)) And ((cr0 and 1)<>0) Then 
                                        'if deb=2 Then Print #5,"Can't load from CRx" 
                                        x86gpf(0) 
                                        Exit Select 
                                EndIf
                                fetchea2() : if abrt then exit Select 
                                Select Case As Const  (reg)
                                	Case 0 
                                        regs(rm).l=cr0
                                        regs(rm).l = regs(rm).l Or &h10 /'ET hardwired on 486'/
                                        Exit Select 
                                	Case 2 
                                        regs(rm).l=cr2 
                                        Exit Select 
                                	Case 3 
                                        regs(rm).l=cr3 
                                        Exit Select 
                                	case Else 
                                        'if deb=3 then print #5,"Bad read of CR ",rmdat And 7,reg
                                        pc=oldpc 
                                        x86illegal() 
                                        Exit Select 
                                End Select
                                cycles-=6 
                                Exit Select  
                        	case &h21  /'MOV reg32,DRx'/
                                if ((CPL<>0) Or ((eflags And VM_FLAG)<>0)) And ((cr0 and 1)<>0) Then 
                                        'if deb=3 then print #5,"Can't load from DRx" 
                                        x86gpf(0) 
                                        Exit Select 
                                EndIf
                                fetchea2() : if abrt then exit Select 
                                regs(rm).l=0 
                                cycles-=6 
                                Exit Select 
                        	case &h22  /'MOV CRx,reg32'/
                                if ((CPL<>0) Or ((eflags And VM_FLAG)<>0)) And ((cr0 and 1)<>0) Then 
                                        'if deb=3 then print #5,"Can't load CRx" 
                                        x86gpf(0) 
                                        Exit Select 
                                EndIf
                                fetchea2() : if abrt then exit Select 
                                Select Case As Const  (reg)
                                	Case 0 
                                        if ((regs(rm).l Xor cr0)  And &h80000001) Then flushmmucache() 
                                        cr0=regs(rm).l
                                        'if (cpu_16bitbus) Then cr0  = cr0  Or  &h10 
                                        if (cr0 And &h80000000)=0 Then
                                            mmu_perm=4 
                                        EndIf
                                        Exit Select 
                                	Case 2 
                                        cr2=regs(rm).l 
                                        Exit Select 
                                	case 3 
                                        cr3=regs(rm).l And inv(&hFFF) 
                                        flushmmucache() 
                                        Exit Select 
                                	case Else 
                                        'if deb=3 then print #5,"Bad load CR ",reg
                                        pc=oldpc 
                                        x86illegal() 
                                        Exit Select 
                               End Select
                               cycles-=10 
                                Exit Select 
                        	case &h23  /'MOV DRx,reg32'/
                                if ((CPL<>0) Or ((eflags And VM_FLAG)<>0)) And ((cr0 and 1)<>0) Then 
                                        'if deb=3 then print #5,"Can't load DRx" 
                                        x86gpf(0) 
                                        Exit Select 
                                EndIf
                                fetchea2() : if abrt then exit Select 
                                cycles-=6 
                                Exit Select 
                        	case &h24  /'MOV reg32,TRx'/
                                if ((CPL<>0) Or ((eflags And VM_FLAG)<>0)) And ((cr0 and 1)<>0) Then 
                                        'if deb=3 then print #5,"Can't load from TRx" 
                                        x86gpf(0) 
                                        Exit Select 
                                EndIf
                                fetchea2() : if abrt then exit Select 
                                regs(rm).l=0 
                                cycles-=6 
                                Exit Select 
                        	case &h26  /'MOV TRx,reg32'/
                                if ((CPL<>0) Or ((eflags And VM_FLAG)<>0)) And ((cr0 and 1)<>0) Then 
                                        'if deb=3 then print #5,"Can't load TRx" 
                                        x86gpf(0) 
                                        Exit Select 
                                EndIf
                                fetchea2() : if abrt then exit Select 
                                cycles-=6 
                                Exit Select 
                        	case &h80  /'JO'/
                                templ=getlong(): if abrt then exit Select 
                                if (flags And V_FLAG) Then 
                                       pc+=templ: cycles-=2  
                                EndIf
                                cycles-=1 
                                Exit Select 
                        	case &h81  /'JNO'/
                                templ=getlong(): if abrt then exit Select 
                                if (flags And V_FLAG)=0 Then 
                                       pc+=templ: cycles-=2  
                                EndIf
                                cycles-=1 
                                Exit Select 
                        	case &h82  /'JB'/
                                templ=getlong(): if abrt then exit Select 
                                if (flags And C_FLAG) Then 
                                       pc+=templ: cycles-=2  
                                EndIf
                                cycles-=1 
                                Exit Select 
                        	case &h83  /'JNB'/
                                templ=getlong(): if abrt then exit Select 
                                if (flags And C_FLAG)=0 Then 
                                       pc+=templ: cycles-=2  
                                EndIf
                                cycles-=1 
                                Exit Select 
                        	case &h84  /'JE'/
                                templ=getlong(): if abrt then exit Select 
                                if (flags And Z_FLAG) Then pc+=templ 
                                cycles-=4 
                                Exit Select 
                        	case &h85  /'JNE'/
                                templ=getlong(): if abrt then exit Select 
                                if (flags And Z_FLAG)=0 Then pc+=templ 
                                cycles-=4 
                                Exit Select 
                        	case &h86  /'JBE'/
                                templ=getlong(): if abrt then exit Select 
                                if (flags And (C_FLAG Or Z_FLAG)) Then 
                                         pc+=templ: cycles-=2  
                                EndIf
                                cycles-=1 
                                Exit Select 
                        	case &h87  /'JNBE'/
                                templ=getlong(): if abrt then exit Select 
                                if (flags And (C_FLAG Or Z_FLAG))=0 Then 
                                         pc+=templ: cycles-=2  
                                EndIf
                                cycles-=1 
                                Exit Select 
                        	case &h88  /'JS'/
                                templ=getlong(): if abrt then exit Select 
                                if (flags And N_FLAG) Then pc+=templ 
                                cycles-=4 
                                Exit Select 
                        	case &h89  /'JNS'/
                                templ=getlong(): if abrt then exit Select 
                                if (flags And N_FLAG)=0 Then pc+=templ 
                                cycles-=4 
                                Exit Select 
                        	case &h8A  /'JP'/
                                templ=getlong(): if abrt then exit Select 
                                if (flags And P_FLAG) Then pc+=templ 
                                cycles-=4 
                                Exit Select 
                        	case &h8B  /'JNP'/
                                templ=getlong(): if abrt then exit Select 
                                if (flags And P_FLAG)=0 Then pc+=templ 
                                cycles-=4 
                                Exit Select 
                        	case &h8C  /'JL'/
                                templ=getlong(): if abrt then exit Select 
                                temp=iif((flags And N_FLAG),1,0 )
                                temp2=iif((flags And V_FLAG),1,0 )
                                if (temp<>temp2) Then 
                                          pc+=templ: cycles-=2  
                                EndIf
                                cycles-=1 
                                Exit Select 
                        	case &h8D  /'JNL'/
                                templ=getlong(): if abrt then exit Select 
                                temp=iif((flags And N_FLAG),1,0 )
                                temp2=iif((flags And V_FLAG),1,0 )
                                if (temp=temp2) Then 
                                          pc+=templ: cycles-=2  
                                EndIf
                                cycles-=1 
                                Exit Select 
                        	case &h8E  /'JLE'/
                                templ=getlong(): if abrt then exit Select 
                                temp=iif((flags And N_FLAG),1,0 )
                                temp2=iif((flags And V_FLAG),1,0 )
                                if ((flags And Z_FLAG)<>0) Or (temp<>temp2) Then 
                                          pc+=templ: cycles-=2  
                                EndIf
                                cycles-=1 
                                Exit Select 
                        	case &h8F  /'JNLE'/
                                templ=getlong(): if abrt then exit Select 
                                temp =iif((flags And N_FLAG),1,0 )
                                temp2=iif((flags And V_FLAG),1,0 )
                                If ( ((flags And Z_FLAG)<>0) Or (temp<>temp2) )=0 Then 
                                          pc+=templ: cycles-=2  
                                EndIf
                                cycles-=1 
                                Exit Select 
                        	case &h90  /'SETO'/
                                fetchea2() : if abrt then exit Select 
                                seteab(IIf((flags And V_FLAG),1,0)) 
                                cycles-=4 
                                Exit Select 
                        	case &h91  /'SETNO'/
                                fetchea2() : if abrt then exit Select 
                                seteab(IIf((flags And V_FLAG),0,1)) 
                                cycles-=4 
                                Exit Select 
                        	case &h92  /'SETC'/
                                fetchea2() : if abrt then exit Select 
                                seteab(IIf((flags And C_FLAG),1,0)) 
                                cycles-=4 
                                Exit Select 
                        	case &h93  /'SETAE'/
                                fetchea2() : if abrt then exit Select 
                                seteab(IIf((flags And C_FLAG),0,1)) 
                                cycles-=4 
                                Exit Select 
                        	case &h94  /'SETZ'/
                                fetchea2() : if abrt then exit Select 
                                seteab(IIf((flags And Z_FLAG),1,0)) 
                                cycles-=4 
                                Exit Select 
                        	case &h95  /'SETNZ'/
                                fetchea2() : if abrt then exit Select 
                                seteab(IIf((flags And Z_FLAG),0,1)) 
                                cycles-=4 
                                Exit Select 
                        	case &h96  /'SETBE'/
                                fetchea2() : if abrt then exit Select 
                                seteab(IIf((flags And (C_FLAG Or Z_FLAG)),1,0)) 
                                cycles-=4 
                                Exit Select 
                        	case &h97  /'SETNBE'/
                                fetchea2() : if abrt then exit Select 
                                seteab(IIf((flags And (C_FLAG Or Z_FLAG)),0,1)) 
                                cycles-=4 
                                Exit Select 
                        	case &h98  /'SETS'/
                                fetchea2() : if abrt then exit Select 
                                seteab(IIf((flags And N_FLAG),1,0)) 
                                cycles-=4 
                                Exit Select 
                        	case &h99  /'SETNS'/
                                fetchea2() : if abrt then exit Select 
                                seteab(IIf((flags And N_FLAG),0,1)) 
                                cycles-=4 
                                Exit Select 
                        	case &h9A  /'SETP'/
                                fetchea2() : if abrt then exit Select 
                                seteab(IIf((flags And P_FLAG),1,0)) 
                                cycles-=4 
                                Exit Select 
                        	case &h9B  /'SETNP'/
                                fetchea2() : if abrt then exit Select 
                                seteab(IIf((flags And P_FLAG),0,1)) 
                                cycles-=4 
                                Exit Select 
                        	case &h9C  /'SETL'/
                                fetchea2() : if abrt then exit Select 
                                temp=iif((flags And N_FLAG),1,0 )
                                temp2=iif((flags And V_FLAG),1,0 )
                                seteab(temp Xor temp2) 
                                cycles-=4 
                                Exit Select 
                        	case &h9D  /'SETGE'/
                                fetchea2() : if abrt then exit Select 
                                temp=iif((flags And N_FLAG),1,0 )
                                temp2=iif((flags And V_FLAG),1,0 )
                                seteab(IIf((temp Xor temp2),0,1)) 
                                cycles-=4 
                                Exit Select 
                        	case &h9E  /'SETLE'/
                                fetchea2() : if abrt then exit Select 
                                temp=iif((flags And N_FLAG),1,0 )
                                temp2=iif((flags And V_FLAG),1,0 )
                                seteab(IIf(((temp Xor temp2)  Or  (flags And Z_FLAG)),1,0)) 
                                cycles-=4 
                                Exit Select 
                        	case &h9F  /'SETNLE'/
                                fetchea2() : if abrt then exit Select 
                                temp=iif((flags And N_FLAG),1,0 )
                                temp2=iif((flags And V_FLAG),1,0 )
                                seteab(IIf(((temp Xor temp2)  Or  (flags And Z_FLAG)),0,1)) 
                                cycles-=4 
                                Exit Select 
                        	case &hA0  /'PUSH FS'/
                                if ssegs Then ss0=oldss 
                                if stack32 Then 
                                        writememl_386(ss0,ESP-4,FS1): if abrt then exit Select 
                                        ESP-=4 
                                else
                                        writememl_386(ss0,((SP-4) And &hFFFF),FS1): if abrt then exit Select 
                                        SP-=4 
                                EndIf
                                cycles-=2 
                                Exit Select 
                        	case &hA1  /'POP FS'/
                                if ssegs Then ss0=oldss 
                                if stack32 Then 
                                        tempw=readmemw_386(ss0,ESP): if abrt Then exit Select 
                                        loadseg(tempw, _fs):            if abrt then exit Select 
                                        ESP+=4 
                                else
                                        tempw=readmemw_386(ss0,SP): if abrt Then exit Select 
                                        loadseg(tempw, _fs):            if abrt then exit Select 
                                        SP+=4 
                                EndIf
                                cycles-=3
                                Exit Select 
                        	case &hA8  /'PUSH GS'/
                                if ssegs Then ss0=oldss 
                                if stack32 Then 
                                        writememl_386(ss0,ESP-4,GS1): if abrt then exit Select 
                                        ESP-=4 
                                else
                                        writememl_386(ss0,((SP-4) And &hFFFF),GS1): if abrt then exit Select 
                                        SP-=4 
                                EndIf
                                cycles-=2 
                                Exit Select 
                        	case &hA9  /'POP GS'/
                                if ssegs Then ss0=oldss 
                                if stack32 Then 
                                        tempw=readmemw_386(ss0,ESP): if abrt Then exit Select 
                                        loadseg(tempw, _gs):            if abrt then exit Select 
                                        ESP+=4 
                                else
                                        tempw=readmemw_386(ss0,SP): if abrt Then exit Select 
                                        loadseg(tempw, _gs):            if abrt then exit Select 
                                        SP+=4 
                                EndIf
                                cycles-=3 
                                Exit Select 
                        	case &hA3  /'BT r32'/
                                fetchea2() : if abrt then exit Select 
                                eaaddr+=((regs(reg).l \ 32)*4): eal_r = 0 
                                templ=geteal(): if abrt then exit Select 
                                if templ And (1 Shl (regs(reg).l And 31)) Then 
                                      flags = flags Or C_FLAG 
                                else
                                      flags = flags And inv(C_FLAG)
                                EndIf
                                cycles-=3 
                                Exit Select 
                        	case &hA4  /'SHLD imm'/
                                fetchea2() : if abrt then exit Select 
                                temp=readmemb_386(cs0,pc) And 31: pc+=1 
                                if (temp) Then 
                                        templ=geteal(): if abrt then exit Select 
                                        tempc=IIf(((templ Shl (temp-1)) And &h80000000),1,0) 
                                        templ=(templ Shl temp) Or (regs(reg).l Shr (32-temp)) 
                                        seteal(templ): if abrt then exit Select 
                                        setznp32(templ) 
                                        if tempc Then flags = flags Or C_FLAG 
                                EndIf
                                cycles-=3 
                                Exit Select 
                        	case &hA5  /'SHLD CL'/
                                fetchea2() : if abrt then exit Select 
                                temp=CL And 31 
                                if (temp) Then 
                                        templ=geteal(): if abrt then exit Select 
                                        tempc=IIf(((templ Shl (temp-1)) And &h80000000),1,0 )
                                        templ=(templ Shl temp) Or (regs(reg).l Shr (32-temp)) 
                                        seteal(templ): if abrt then exit Select 
                                        setznp32(templ) 
                                        if tempc Then flags = flags Or C_FLAG 
                                EndIf
                                cycles-=3 
                                Exit Select 
                        	case &hAB  /'BTS r32'/
                                fetchea2() : if abrt then exit Select 
                                eaaddr+=((regs(reg).l \ 32)*4)
                                eal_r = 0
                                eal_w = 0 
                                templ=geteal(): if abrt then exit Select 
                                tempc=IIf((templ And (1 Shl (regs(reg).l And 31))),1,0) 
                                templ = templ Or (1 Shl (regs(reg).l And 31)) 
                                seteal(templ): if abrt then exit Select 
                                if tempc Then 
                                      flags = flags Or C_FLAG 
                                else
                                      flags = flags And inv(C_FLAG)
                                EndIf
                                cycles-=6 
                                Exit Select 
                        	case &hAC  /'SHRD imm'/
                                fetchea2() : if abrt then exit Select 
                                temp=readmemb_386(cs0,pc) And 31: pc+=1 
                                if (temp) Then 
                                        templ=geteal(): if abrt then exit Select 
                                        tempc=(templ Shr (temp-1)) And 1 
                                        templ=(templ Shr temp) Or (regs(reg).l Shl (32-temp)) 
                                        seteal(templ): if abrt then exit Select 
                                        setznp32(templ) 
                                        if tempc Then flags = flags Or C_FLAG 
                                EndIf
                                cycles-=3 
                                Exit Select 
                        	case &hAD  /'SHRD CL'/
                                fetchea2() : if abrt then exit Select 
                                temp=CL And 31 
                                if (temp) Then 
                                        templ=geteal(): if abrt then exit Select 
                                        tempc=(templ Shr (temp-1)) And 1 
                                        templ=(templ Shr temp) Or (regs(reg).l Shl (32-temp)) 
                                        seteal(templ): if abrt then exit Select 
                                        setznp32(templ) 
                                        if tempc Then flags = flags Or C_FLAG 
                                EndIf
                                cycles-=3 
                                Exit Select 
                        	case &hAF  /'IMUL reg32,rm32'/
                                fetchea2() : if abrt then exit Select 
                                temp64=CLngInt(CLng(regs(reg).l))*CLngInt(CLng(geteal())) 
                                if abrt Then exit Select 
                                regs(reg).l=temp64 And &hFFFFFFFF 
                                ' OBLIGATORIO el CULNG para pasar de 64 a 32
                                if (culng(temp64 Shr 32)<>0) And (CuLng(temp64 Shr 32) <> &hFFFFFFFF) Then 
                                        flags = flags Or (C_FLAG Or V_FLAG) 
                                Else
                                        flags = flags And inv(C_FLAG Or V_FLAG)
                                EndIf
                                cycles-=30 
                                Exit Select 
                        	case &hB0  /'CMPXCHG rm8, reg8'/
                                'if is486=0 Then goto inv32 
                                fetchea2() : if abrt then exit Select 
                                temp = geteab() 
                                setsub8(AL, temp) 
                                if AL = temp Then 
                                    seteab(getr8(reg)) 
                                else
                                    AL = temp
                                EndIf
                                cycles -= iif((modo = 3) , 6 , 10 )
                                Exit Select 
                        	case &hB1  /'CMPXCHG rm32, reg32'/
                                'if (is486=0) Then goto inv32 
                                fetchea2() : if abrt then exit Select 
                                templ = geteal() 
                                setsub32(EAX, templ) 
                                if EAX = templ Then 
                                    seteal(regs(reg).l) 
                                else
                                    EAX = templ
                                EndIf
                                cycles -= iif((modo = 3) , 6 , 10) 
                                Exit Select 
                        	case &hB3  /'BTR r32'/
                                fetchea2() : if abrt then exit Select
                                eaaddr+=((regs(reg).l \ 32)*4)
                                eal_r = 0
                                eal_w = 0 
                                templ=geteal(): if abrt then exit Select 
                                tempc=IIf((templ And (1 Shl (regs(reg).l And 31))),1,0) 
                                templ = templ And inv(1 Shl (regs(reg).l And 31)) 
                                seteal(templ): if abrt then exit Select 
                                if tempc Then 
                                     flags = flags Or C_FLAG 
                                else
                                     flags = flags And inv(C_FLAG)
                                EndIf
                                cycles-=6 
                                Exit Select 
                        	case &hB2  /'LSS'/
                                fetchea2() : if abrt then exit Select 
                                templ=readmeml_386(easeg,eaaddr)      
                                tempw=readmemw_386(easeg,(eaaddr+4)): if abrt then exit Select 
                                loadseg(tempw, _ss): if abrt Then exit Select 
                                regs(reg).l=templ 
                                oldss=ss0 
                                cycles-=7 
                                Exit Select 
                        	case &hB4  /'LFS'/
                                fetchea2() : if abrt then exit Select 
                                templ=readmeml_386(easeg,eaaddr) 
                                tempw=readmemw_386(easeg,(eaaddr+4)): if abrt then exit Select 
                                loadseg(tempw, _fs): if abrt Then exit Select 
                                regs(reg).l=templ 
                                cycles-=7 
                                Exit Select 
                        	case &hB5  /'LGS'/
                                fetchea2() : if abrt then exit Select 
                                templ=readmeml_386(easeg,eaaddr) 
                                tempw=readmemw_386(easeg,(eaaddr+4)): if abrt then exit Select 
                                loadseg(tempw, _gs): if abrt Then exit Select 
                                regs(reg).l=templ 
                                cycles-=7 
                                Exit Select 
                        	case &hB6  /'MOVZX b'/
                                fetchea2() : if abrt then exit Select 
                                templ=geteab(): if abrt then exit Select 
                                regs(reg).l=templ 
                                cycles-=3 
                                Exit Select 
                        	case &hB7  /'MOVZX w'/
                                fetchea2() : if abrt then exit Select 
                                templ=geteaw(): if abrt then exit Select 
                                regs(reg).l=templ 
                                cycles-=3 
                                Exit Select 
                        	case &hBA  /'MORE?!?!?!'/
                                fetchea2() : if abrt then exit Select 
                                Select Case As Const  (rmdat And &h38)
                                	Case &h20  /'BT l,imm'/
                                        templ=geteal() 
                                        temp=readmemb_386(cs0,pc): pc+=1 
                                        if abrt Then exit Select 
                                        if (templ And (1 Shl temp)) Then 
                                             flags = flags Or C_FLAG 
                                        else
                                             flags = flags And inv(C_FLAG)
                                        EndIf
                                        cycles-=6 
                                        Exit Select 
                                	case &h28  /'BTS l,imm'/
                                        templ=geteal() 
                                        temp=readmemb_386(cs0,pc): pc+=1 
                                        if abrt Then exit Select 
                                        tempc=IIf((templ and (1 Shl temp)),1,0) 
                                        templ = templ Or (1 Shl temp) 
                                        seteal(templ): if abrt then exit Select 
                                        if tempc Then 
                                                flags = flags Or C_FLAG 
                                        else
                                                flags = flags And inv(C_FLAG)
                                        EndIf
                                        cycles-=6 
                                        Exit Select 
                                	case &h30  /'BTR l,imm'/
                                        templ=geteal() 
                                        temp=readmemb_386(cs0,pc): pc+=1 
                                        if abrt Then exit Select 
                                        tempc=IIf((templ and (1 Shl temp)),1,0) 
                                        templ = templ And inv(1 Shl temp) 
                                        seteal(templ): if abrt then exit Select 
                                        if tempc Then 
                                               flags = flags Or C_FLAG 
                                        else
                                               flags = flags And inv(C_FLAG)
                                        EndIf
                                        cycles-=6 
                                        Exit Select 
                                	case &h38  /'BTC l,imm'/
                                        templ=geteal() 
                                        temp=readmemb_386(cs0,pc): pc+=1 
                                        if abrt Then exit Select 
                                        tempc=IIf((templ and (1 shl temp)),1,0) 
                                        templ = templ Xor (1 Shl temp) 
                                        seteal(templ): if abrt then exit Select 
                                        if tempc Then 
                                              flags = flags Or C_FLAG 
                                        else
                                              flags = flags And inv(C_FLAG)
                                        EndIf
                                        cycles-=6 
                                        Exit Select 
                                	case Else 
                                        'if deb=3 then print #5,"Bad 32-bit 0F BA opcode ",hex(rmdat and &h38,2)
                                        pc=oldpc 
                                        x86illegal() 
                                        Exit Select 
                               End Select
                               Exit Select 
                        	case &hBB  /'BTC r32'/
                                fetchea2() : if abrt then exit Select 
                                eaaddr+=((regs(reg).l \ 32)*4)
                                eal_r = 0
                                eal_w = 0 
                                templ=geteal(): if abrt then exit Select 
                                tempc=iif((templ And (1 Shl (regs(reg).l And 31))),1,0) 
                                templ = templ Xor (1 Shl (regs(reg).l And 31)) 
                                seteal(templ):  if abrt then exit Select 
                                if tempc Then 
                                        flags = flags Or C_FLAG 
                                else
                                        flags = flags And inv(C_FLAG)
                                EndIf
                                cycles-=6 
                                Exit Select 
                        	case &hBC  /'BSF l'/
                                fetchea2() : if abrt then exit Select 
                                templ=geteal(): if abrt then exit Select 
                                if templ=0 Then 
                                        flags = flags Or Z_FLAG 
                                else
                                        for  tempi=0 To  31 
                                                cycles -=1 
                                                if (templ And (1 Shl tempi)) Then 
                                                        flags = flags And inv(Z_FLAG) 
                                                        regs(reg).l=tempi 
                                                        Exit Select
                                                EndIf
                                       Next
                                EndIf
                                cycles-=6 
                                Exit Select 
                        	case &hBD  /'BSR l'/
                                fetchea2() : if abrt then exit Select 
                                templ=geteal(): if abrt then exit Select 
                                if templ=0 Then 
                                        flags = flags Or Z_FLAG 
                                else
                                        for  tempi=31 To  0 Step -1 
                                                cycles -= 3 
                                                if (templ And (1 Shl tempi)) Then 
                                                        flags = flags And inv(Z_FLAG) 
                                                        regs(reg).l=tempi 
                                                        Exit Select 
                                                EndIf
                                       Next
                                EndIf
                                cycles-=6 
                                Exit Select 
                        	case &hBE  /'MOVSX b'/
                                fetchea2() : if abrt then exit Select 
                                templ=geteab(): if abrt then exit Select 
                                if (templ And &h80) Then templ = templ Or &hFFFFFF00 
                                regs(reg).l=templ 
                                cycles-=3 
                                Exit Select 
                        	case &hBF  /'MOVSX w'/
                                fetchea2() : if abrt then exit Select 
                                templ=geteaw(): if abrt then exit Select 
                                if (templ And &h8000) Then templ = templ Or &hFFFF0000 
                                regs(reg).l=templ 
                                cycles-=3 
                                Exit Select 
                        	case &hC0  /'XADD b'/
                                fetchea2() : if abrt then exit Select 
                                temp=geteab(): if abrt then exit Select 
                                seteab(temp+getr8(reg)): if abrt then exit Select 
                                setadd8(temp,getr8(reg)) 
                                setr8(reg,temp) 
                                Exit Select 
                        	case &hC1  /'XADD l'/
                                fetchea2() : if abrt then exit Select 
                                templ=geteal(): if abrt then exit Select 
                                templ2=regs(reg).l 
                                seteal(templ+templ2): if abrt then exit Select 
                                setadd32(templ,templ2) 
                                regs(reg).l=templ 
                                Exit Select 
                            /'BSWAP'/  /'486!!!'/
                        	case &hC8, &hC9, &hCA, &hCB ,_ 
                        	     &hCC, &hCD, &hCE, &hCF 
                                regs(temp And 7).l = (regs(temp And 7).l Shr 24) Or ((regs(temp And 7).l Shr 8) And &hFF00) _ 
                                     Or ((regs(temp And 7).l Shl 8) And &hFF0000) Or ((regs(temp And 7).l Shl 24) And &hFF000000) 
                                cycles-=1 
                                Exit Select 
                        	case &hA2  /'CPUID'/
                                if (CPUID) Then 
                                        cpu_CPUID() 
                                        cycles-=9 
                                        Exit Select 
                                EndIf
                                goto inv32 
                        	case &hFF 
                       
                       ' GOTO (justo aqui arriba)
                       inv32: 
                                'if deb=3 then print #5,"INV32"
                        	case Else 
                                'if deb=3 then print #5,"Bad 32-bit 0F opcode  386",temp 
                                pc=oldpc 
                                x86illegal() 
                                Exit Select 
                       End Select
                       Exit Select 
                	case &h10, &h110, &h210, &h310  /'ADC 8,reg'/
                        fetchea32() : if abrt Then exit Select 
                        if modo=3 Then 
                                temp  = getr8(rm) 
                                temp2 = getr8(reg) 
                                setadc8(temp, temp2) 
                                setr8(rm, temp + temp2 + tempc) 
                                cycles -= timing_rr 
                        else
                                temp  = geteab():               if abrt then exit Select 
                                temp2 = getr8(reg) 
                                seteab(temp + temp2 + tempc): if abrt Then exit Select 
                                setadc8(temp, temp2) 
                                cycles -= timing_mr 
                        EndIf
                        Exit Select 
                	case &h11, &h211  /'ADC 16,reg'/
                        fetchea32() : if abrt Then exit Select 
                        if modo=3 Then 
                                setadc16(regs(rm).w, regs(reg).w) 
                                regs(rm).w += regs(reg).w + tempc 
                                cycles -= timing_rr 
                        else
                                tempw  = geteaw(): if abrt Then exit Select 
                                tempw2 = regs(reg).w 
                                seteaw(tempw + tempw2 + tempc): if abrt then exit Select 
                                setadc16(tempw, tempw2) 
                                cycles -= timing_mr 
                        EndIf
                        Exit Select 
                	case &h111, &h311  /'ADC 32,reg'/
                        fetchea32() : if abrt Then exit Select 
                        if modo=3 Then 
                                setadc32(regs(rm).l, regs(reg).l) 
                                regs(rm).l += regs(reg).l + tempc 
                                cycles -= timing_rr 
                        else
                                templ  = geteal(): if abrt then exit Select 
                                templ2 = regs(reg).l 
                                seteal(templ + templ2 + tempc): if abrt then exit Select 
                                setadc32(templ, templ2) 
                                cycles -= timing_mrl 
                        EndIf
                        Exit Select 
                	case &h12, &h112, &h212, &h312  /'ADC reg,8'/
                        fetchea32() : if abrt Then exit Select 
                        temp=geteab(): if abrt Then exit Select 
                        setadc8(getr8(reg),temp) 
                        setr8(reg,getr8(reg)+temp+tempc) 
                        cycles -= iif((modo = 3) , timing_rr , timing_rm) 
                        Exit Select 
                	case &h13, &h213  /'ADC reg,16'/
                        fetchea32() : if abrt Then exit Select 
                        tempw=geteaw(): if abrt then exit Select 
                        setadc16(regs(reg).w,tempw) 
                        regs(reg).w+=tempw+tempc 
                        cycles -= iif((modo = 3) , timing_rr , timing_rm) 
                        Exit Select 
                	case &h113, &h313  /'ADC reg,32'/
                        fetchea32() : if abrt Then exit Select 
                        templ=geteal(): if abrt Then exit Select 
                        setadc32(regs(reg).l,templ) 
                        regs(reg).l+=templ+tempc 
                        cycles -= iif((modo = 3) , timing_rr , timing_rml) 
                        Exit Select 
                	case &h14, &h114, &h214, &h314  /'ADC AL,#8'/
                        tempw=getbytef() 
                        setadc8(AL,tempw) 
                        AL+=tempw+tempc 
                        cycles -= timing_rr 
                        Exit Select 
                	case &h15, &h215  /'ADC AX,#16'/
                        tempw=getwordf() 
                        setadc16(AX,tempw) 
                        AX+=tempw+tempc 
                        cycles -= timing_rr 
                        Exit Select 
                	case &h115, &h315  /'ADC EAX,#32'/
                        templ=getlong(): if abrt then exit Select 
                        setadc32(EAX,templ) 
                        EAX+=templ+tempc 
                        cycles -= timing_rr 
                        Exit Select 
                	case &h16, &h216  /'PUSH SS'/
                        if ssegs Then ss0=oldss 
                        if stack32 Then 
                                writememw_386(ss0,ESP-2,SS1): if abrt Then exit Select 
                                ESP-=2 
                        else
                                writememw_386(ss0,((SP-2) And &hFFFF),SS1): if abrt then exit Select 
                                SP-=2 
                        EndIf
                        cycles-=2 
                        Exit Select 
                	case &h116, &h316  /'PUSH SS'/
                        if ssegs Then ss0=oldss 
                        if stack32 Then 
                                writememl_386(ss0,ESP-4,SS1): if abrt then exit Select 
                                ESP-=4 
                        else
                                writememl_386(ss0,((SP-4) And &hFFFF),SS1): if abrt then exit Select 
                                SP-=4 
                        EndIf
                        cycles-=2 
                        Exit Select 
                	case &h17, &h217  /'POP SS'/
                        if ssegs Then ss0=oldss 
                        if stack32 Then 
                                tempw=readmemw_386(ss0,ESP): if abrt then exit Select 
                                loadseg(tempw, _ss): if abrt then exit Select 
                                ESP+=2 
                        else
                                tempw=readmemw_386(ss0,SP):  if abrt then exit Select 
                                loadseg(tempw, _ss): if abrt then exit Select 
                                SP+=2 
                        EndIf
                        cycles-=3 
                        Exit Select 
                	case &h117, &h317  /'POP SS'/
                        if ssegs Then ss0=oldss 
                        if stack32 Then 
                                tempw=readmemw_386(ss0,ESP): if abrt then exit Select 
                                loadseg(tempw, _ss): if abrt then exit Select 
                                ESP+=4 
                        else
                                tempw=readmemw_386(ss0,SP):  if abrt then exit Select 
                                loadseg(tempw, _ss): if abrt then exit Select 
                                SP+=4 
                        EndIf
                        cycles-=3 
                        Exit Select 
                	case &h18, &h118, &h218, &h318  /'SBB 8,reg'/
                        fetchea32() : if abrt Then exit Select 
                        if modo=3 Then 
                                temp  = getr8(rm) 
                                temp2 = getr8(reg) 
                                setsbc8(temp, temp2) 
                                setr8(rm, temp - (temp2 + tempc)) 
                                cycles -= timing_rr 
                        else
                                temp  = geteab(): if abrt Then exit Select 
                                temp2 = getr8(reg) 
                                seteab(temp - (temp2 + tempc)): if abrt Then exit Select 
                                setsbc8(temp, temp2) 
                                cycles -= timing_mr 
                        EndIf
                        Exit Select 
                	case &h19, &h219  /'SBB 16,reg'/
                        fetchea32() : if abrt Then exit Select 
                        if modo=3 Then 
                                setsbc16(regs(rm).w, regs(reg).w) 
                                regs(rm).w -= (regs(reg).w + tempc) 
                                cycles -= timing_rr 
                        else
                                tempw  = geteaw(): if abrt then exit Select 
                                tempw2 = regs(reg).w 
                                seteaw(tempw - (tempw2 + tempc)):  if abrt then exit Select 
                                setsbc16(tempw, tempw2) 
                                cycles -= timing_mr 
                        EndIf
                        Exit Select 
                	case &h119, &h319  /'SBB 32,reg'/
                        fetchea32() : if abrt Then exit Select 
                        if modo=3 Then 
                                setsbc32(regs(rm).l, regs(reg).l) 
                                regs(rm).l -= (regs(reg).l + tempc) 
                                cycles -= timing_rr 
                        else
                                templ  = geteal(): if abrt then exit Select 
                                templ2 = regs(reg).l 
                                seteal(templ - (templ2 + tempc)):  if abrt then exit Select 
                                setsbc32(templ, templ2) 
                                cycles -= timing_mrl 
                        EndIf
                        Exit Select 
                	case &h1A, &h11A, &h21A, &h31A  /'SBB reg,8'/
                        fetchea32() : if abrt Then exit Select 
                        temp=geteab(): if abrt then exit Select 
                        setsbc8(getr8(reg),temp) 
                        setr8(reg,getr8(reg)-(temp+tempc)) 
                        cycles -= iif((modo = 3) , timing_rr , timing_rm) 
                        Exit Select 
                	case &h1B, &h21B  /'SBB reg,16'/
                        fetchea32() : if abrt Then exit Select 
                        tempw=geteaw(): if abrt then exit Select 
                        tempw2=regs(reg).w 
                        setsbc16(tempw2,tempw) 
                        tempw2-=(tempw+tempc) 
                        regs(reg).w=tempw2 
                        cycles -= iif((modo = 3) , timing_rr , timing_rm) 
                        Exit Select 
                	case &h11B, &h31B  /'SBB reg,32'/
                        fetchea32() : if abrt Then exit Select 
                        templ=geteal():  if abrt then exit Select 
                        templ2=regs(reg).l 
                        setsbc32(templ2,templ) 
                        templ2-=(templ+tempc) 
                        regs(reg).l=templ2 
                        cycles -= iif((modo = 3) , timing_rr , timing_rml) 
                        Exit Select 
                	case &h1C, &h11C, &h21C, &h31C  /'SBB AL,#8'/
                        temp=getbytef() 
                        setsbc8(AL,temp) 
                        AL-=(temp+tempc) 
                        cycles-=1 
                        Exit Select 
                	case &h1D, &h21D  /'SBB AX,#16'/
                        tempw=getwordf() 
                        setsbc16(AX,tempw) 
                        AX-=(tempw+tempc) 
                        cycles-=1 
                        Exit Select 
                	case &h11D, &h31D  /'SBB AX,#32'/
                        templ=getlong(): if abrt then exit Select 
                        setsbc32(EAX,templ) 
                        EAX-=(templ+tempc) 
                        cycles-=1 
                        Exit Select 
                	case &h1E, &h21E  /'PUSH DS'/
                        if ssegs Then ss0=oldss 
                        if stack32 Then 
                                writememw_386(ss0,ESP-2,DS1): if abrt then exit Select 
                                ESP-=2 
                        else
                                writememw_386(ss0,((SP-2) And &hFFFF),DS1):  if abrt then exit Select 
                                SP-=2 
                        EndIf
                        cycles-=2 
                        Exit Select 
                	case &h11E, &h31E  /'PUSH DS'/
                        if ssegs Then ss0=oldss 
                        if stack32 Then 
                                writememl_386(ss0,ESP-4,DS1):if abrt then exit Select 
                                ESP-=4 
                        else
                                writememl_386(ss0,((SP-4) And &hFFFF),DS1):if abrt then exit Select 
                                SP-=4 
                        EndIf
                        cycles-=2 
                        Exit Select 
                	case &h1F, &h21F  /'POP DS'/
                        if ssegs Then ss0=oldss 
                        if stack32 Then 
                                tempw=readmemw_386(ss0,ESP): if abrt then exit Select 
                                loadseg(tempw, _ds): if abrt then exit Select 
                                ESP+=2 
                        else
                                tempw=readmemw_386(ss0,SP):  if abrt then Exit Select 
                                loadseg(tempw, _ds): if abrt then Exit Select 
                                SP+=2 
                        EndIf
                        cycles-=3 
                        Exit Select 
                	case &h11F, &h31F  /'POP DS'/
                        if ssegs Then ss0=oldss 
                        if stack32 Then 
                                tempw=readmemw_386(ss0,ESP): if abrt then exit Select 
                                loadseg(tempw, _ds): if abrt then exit Select 
                                ESP+=4 
                        else
                                tempw=readmemw_386(ss0,SP):  if abrt then exit Select 
                                loadseg(tempw, _ds): if abrt then exit Select 
                                SP+=4 
                        EndIf
                        cycles-=3
                        Exit Select 
                	case &h20, &h120, &h220, &h320  /'AND 8,reg'/
                        fetchea32() : if abrt Then exit Select 
                        if modo=3 Then 
                                temp  = getr8(rm)  And getr8(reg) 
                                setr8(rm, temp) 
                                setznp8(temp) 
                                cycles -= timing_rr 
                        else
                                temp  = geteab(): if abrt Then exit Select 
                                temp  = temp  And  getr8(reg) 
                                seteab(temp): if abrt Then exit Select 
                                setznp8(temp) 
                                cycles -= timing_mr 
                        EndIf
                        Exit Select 
                	case &h21, &h221  /'AND 16,reg'/
                        fetchea32() : if abrt Then exit Select 
                        if modo=3 Then 
                                regs(rm).w  and=  regs(reg).w 
                                setznp16(regs(rm).w) 
                                cycles -= timing_rr 
                        else
                                tempw = geteaw()  And regs(reg).w: if abrt then exit Select 
                                seteaw(tempw): if abrt Then exit Select 
                                setznp16(tempw) 
                                cycles -= timing_mr 
                        EndIf
                        Exit Select 
                	case &h121, &h321  /'AND 32,reg'/
                        fetchea32() : if abrt Then exit Select 
                        if modo=3 Then 
                                regs(rm).l  And= regs(reg).l 
                                setznp32(regs(rm).l) 
                                cycles -= timing_rr 
                        else
                                templ = geteal() And regs(reg).l: if abrt then exit Select 
                                seteal(templ): if abrt Then exit Select 
                                setznp32(templ) 
                                cycles -= timing_mrl 
                        EndIf
                        Exit Select 
                	case &h22, &h122, &h222, &h322  /'AND reg,8'/
                        fetchea32() : if abrt Then exit Select 
                        temp=geteab(): if abrt then exit Select 
                        temp = temp And getr8(reg) 
                        setznp8(temp) 
                        setr8(reg,temp) 
                        cycles -= iif((modo = 3) , timing_rr , timing_rm )
                        Exit Select 
                	case &h23, &h223  /'AND reg,16'/
                        fetchea32() : if abrt Then exit Select 
                        tempw=geteaw(): if abrt then exit Select 
                        tempw = tempw And regs(reg).w 
                        setznp16(tempw) 
                        regs(reg).w=tempw 
                        cycles -= iif((modo = 3) , timing_rr , timing_rm) 
                        Exit Select 
                	case &h123, &h323  /'AND reg,32'/
                        fetchea32() : if abrt Then exit Select 
                        templ=geteal(): if abrt Then exit Select 
                        templ = templ And regs(reg).l 
                        setznp32(templ) 
                        regs(reg).l=templ 
                        cycles -= iif((modo = 3) , timing_rr , timing_rml) 
                        Exit Select 
                	case &h24, &h124, &h224, &h324  /'AND AL,#8'/
                        AL = AL And getbytef() 
                        setznp8(AL) 
                        cycles -= timing_rr 
                        Exit Select 
                	case &h25, &h225  /'AND AX,#16'/
                        AX = AX And getwordf() 
                        setznp16(AX) 
                        cycles -= timing_rr 
                        Exit Select 
                	case &h125, &h325  /'AND EAX,#32'/
                        templ=getlong(): if abrt then exit Select 
                        EAX = EAX And templ 
                        setznp32(EAX) 
                        cycles -= timing_rr 
                        Exit Select 
                	case &h26, &h126, &h226, &h326  /'ES:'/
                        oldss=ss0 
                        oldds=ds0 
                        ds0=es0
                        ss0=es0 
                        'rds=ES1 
                        ssegs=2 
                        cycles-=4 
                        goto opcodestart ' salta al principio, arriba
                        
                	case &h27, &h127, &h227, &h327  /'DAA'/
                        if ((flags  And A_FLAG)<>0)  Or  ((AL  And &hF) > 9) Then 
                                tempi = cushort(AL) + 6 
                                AL += 6 
                                flags  = flags  Or  A_FLAG 
                                if (tempi  And &h100) Then flags  = flags  Or  C_FLAG 
                        EndIf
                        if ((flags And C_FLAG)<>0)  Or  (AL>&h9F) Then 
                                AL+=&h60 
                                flags = flags Or C_FLAG 
                        EndIf
                        tempw = flags  And (C_FLAG  Or  A_FLAG) 
                        setznp8(AL) 
                        flags  = flags  Or  tempw 
                        cycles-=4 
                        Exit Select 
                	case &h28, &h128, &h228, &h328  /'SUB 8,reg'/
                        fetchea32() : if abrt Then exit Select 
                        if modo=3 Then 
                                temp  = getr8(rm) 
                                temp2 = getr8(reg) 
                                setsub8(temp, temp2) 
                                setr8(rm, temp - temp2) 
                                cycles -= timing_rr 
                        else
                                temp  = geteab(): if abrt Then exit Select 
                                temp2 = getr8(reg) 
                                seteab(temp - temp2): if abrt then exit Select 
                                setsub8(temp, temp2) 
                                cycles -= timing_mr 
                        EndIf
                        Exit Select 
                	case &h29, &h229  /'SUB 16,reg'/
                        fetchea32() : if abrt Then exit Select 
                        if modo=3 Then 
                                setsub16(regs(rm).w, regs(reg).w) 
                                regs(rm).w -= regs(reg).w 
                                cycles -= timing_rr 
                        else
                                tempw = geteaw(): if abrt Then exit Select 
                                seteaw(tempw - regs(reg).w):  if abrt then exit Select 
                                setsub16(tempw, regs(reg).w) 
                                cycles -= timing_mr 
                        EndIf
                        Exit Select 
                	case &h129, &h329  /'SUB 32,reg'/
                        fetchea32() : if abrt Then exit Select 
                        if modo=3 Then 
                                setsub32(regs(rm).l, regs(reg).l) 
                                regs(rm).l -= regs(reg).l 
                                cycles -= timing_rr 
                        else
                                templ = geteal(): if abrt then exit Select 
                                seteal(templ - regs(reg).l):  if abrt then exit Select 
                                setsub32(templ, regs(reg).l) 
                                cycles -= timing_mrl 
                        EndIf
                        Exit Select 
                	case &h2A, &h12A, &h22A, &h32A  /'SUB reg,8'/
                        fetchea32() : if abrt Then exit Select 
                        temp=geteab(): if abrt Then exit Select 
                        setsub8(getr8(reg),temp) 
                        setr8(reg,getr8(reg)-temp) 
                        cycles -= iif((modo = 3) , timing_rr , timing_rm) 
                        Exit Select 
                	case &h2B, &h22B  /'SUB reg,16'/
                        fetchea32() : if abrt Then exit Select 
                        tempw=geteaw(): if abrt Then exit Select 
                        setsub16(regs(reg).w,tempw) 
                        regs(reg).w-=tempw 
                        cycles -= iif((modo = 3) , timing_rr , timing_rm) 
                        Exit Select 
                	Case &h12B, &h32B  /'SUB reg,32'/
                        fetchea32() : if abrt Then exit Select 
                        templ=geteal(): if abrt Then exit Select 
                        setsub32(regs(reg).l,templ) 
                        regs(reg).l-=templ 
                        cycles -= iif((modo = 3) , timing_rr , timing_rml) 
                        Exit Select 
                	case &h2C, &h12C, &h22C, &h32C  /'SUB AL,#8'/
                        temp=getbytef() 
                        setsub8(AL,temp) 
                        AL-=temp 
                        cycles -= timing_rr 
                        Exit Select 
                	case &h2D, &h22D  /'SUB AX,#16'/
                        tempw=getwordf() 
                        setsub16(AX,tempw) 
                        AX-=tempw 
                        cycles -= timing_rr 
                        Exit Select 
                	case &h12D, &h32D  /'SUB EAX,#32'/
                        templ=getlong(): if abrt then exit Select 
                        setsub32(EAX,templ) 
                        EAX-=templ 
                        cycles -= timing_rr 
                        Exit Select 
                	case &h2E, &h12E, &h22E, &h32E  /'CS:'/
                        oldss=ss0
                        oldds=ds0 
                        ds0=cs0 
                        ss0=cs0
                        'rds=CS1 
                        ssegs=2 
                        cycles-=4 
                        goto opcodestart ' salta al principio, arriba
                        
                	case &h2F, &h12F, &h22F, &h32F  /'DAS'/
                        if ((flags And A_FLAG)<>0) Or ((AL And &hF)>9) Then 
                                tempi=cushort(AL)-6 
                                AL-=6 
                                flags = flags Or A_FLAG 
                                if (tempi And &h100) Then flags = flags Or C_FLAG 
                        EndIf
                        if ((flags And C_FLAG)<>0) Or (AL>&h9F) Then 
                                AL-=&h60 
                                flags = flags Or C_FLAG 
                        EndIf
                        tempw = flags  And (C_FLAG  Or  A_FLAG) 
                        setznp8(AL) 
                        flags  = flags  Or  tempw 
                        cycles-=4 
                        Exit Select 
                	case &h30, &h130, &h230, &h330  /'XOR 8,reg'/
                        fetchea32() : if abrt Then exit Select 
                        if modo=3 Then 
                                temp = getr8(rm)  Xor  getr8(reg) 
                                setr8(rm, temp) 
                                setznp8(temp) 
                                cycles -= timing_rr 
                        else
                                temp  = geteab(): if abrt then exit Select 
                                temp  = temp  Xor  getr8(reg) 
                                seteab(temp): if abrt then exit Select 
                                setznp8(temp) 
                                cycles -= timing_mr 
                        EndIf
                        Exit Select 
                	case &h31, &h231  /'XOR 16,reg'/
                        fetchea32() : if abrt Then exit Select 
                        if modo=3 Then 
                                regs(rm).w = regs(rm).w Xor regs(reg).w 
                                setznp16(regs(rm).w) 
                                cycles -= timing_rr 
                        else
                                tempw = geteaw()  Xor  regs(reg).w: if abrt then exit Select 
                                seteaw(tempw): if abrt Then exit Select 
                                setznp16(tempw) 
                                cycles -= timing_mr 
                        EndIf
                        Exit Select 
                	case &h131, &h331  /'XOR 32,reg'/
                        fetchea32() : if abrt Then exit Select 
                        if modo=3 Then 
                                regs(rm).l = regs(rm).l Xor regs(reg).l 
                                setznp32(regs(rm).l) 
                                cycles -= timing_rr 
                        else
                                templ = geteal()  Xor  regs(reg).l: if abrt then exit Select 
                                seteal(templ): if abrt Then exit Select 
                                setznp32(templ) 
                                cycles -= timing_mrl 
                        EndIf
                        Exit Select 
                	case &h32, &h132, &h232, &h332  /'XOR reg,8'/
                        fetchea32() : if abrt Then exit Select 
                        temp=geteab(): if abrt Then exit Select 
                        temp = temp Xor getr8(reg) 
                        setznp8(temp) 
                        setr8(reg,temp) 
                        cycles -= iif((modo = 3) , timing_rr , timing_rm) 
                        Exit Select 
                	case &h33, &h233  /'XOR reg,16'/
                        fetchea32() : if abrt Then exit Select 
                        tempw=geteaw(): if abrt Then exit Select 
                        tempw = tempw Xor regs(reg).w 
                        setznp16(tempw) 
                        regs(reg).w=tempw 
                        cycles -= iif((modo = 3) , timing_rr , timing_rm )
                        Exit Select 
                	case &h133, &h333  /'XOR reg,32'/
                        fetchea32() : if abrt Then exit Select 
                        templ=geteal(): if abrt Then exit Select 
                        templ = templ Xor regs(reg).l 
                        setznp32(templ) 
                        regs(reg).l=templ 
                        cycles -= iif((modo = 3) , timing_rr , timing_rml) 
                        Exit Select 
                	case &h34, &h134, &h234, &h334  /'XOR AL,#8'/
                        AL = AL Xor getbytef() 
                        setznp8(AL) 
                        cycles -= timing_rr 
                        Exit Select 
                	case &h35, &h235  /'XOR AX,#16'/
                        AX = AX Xor getwordf() 
                        setznp16(AX) 
                        cycles -= timing_rr 
                        Exit Select 
                	case &h135, &h335  /'XOR EAX,#32'/
                        templ=getlong(): if abrt then exit Select 
                        EAX = EAX Xor templ 
                        setznp32(EAX) 
                        cycles -= timing_rr 
                        Exit Select 
                	case &h36, &h136, &h236, &h336  /'SS:'/
                        oldss=ss0 
                        oldds=ds0 
                        ds0=ss0 ''''
                        'ss0=ds0 ' esta igualdad es rara, pero he comprobado que es correcta 
                        'rds=SS1 
                        ssegs=2 
                        cycles-=4 
                        goto opcodestart ' salta al principio, arriba
                        
                	case &h37, &h137, &h237, &h337  /'AAA'/
                        if ((flags And A_FLAG)<>0) Or ((AL And &hF)>9) Then 
                          AL+=6 
                          AH+=1 
                          flags = flags Or (A_FLAG Or C_FLAG) 
                        else
                          flags = flags And inv(A_FLAG Or C_FLAG)
                        EndIf
                        AL = AL And &hF 
                        cycles-=3 
                        Exit Select 
                	case &h38, &h138, &h238, &h338  /'CMP 8,reg'/
                        fetchea32() : if abrt Then exit Select 
                        temp=geteab(): if abrt Then exit Select 
                        setsub8(temp,getr8(reg)) 
                        cycles-=iif((modo=3),1,2) 
                        Exit Select 
                	case &h39, &h239  /'CMP 16,reg'/
                        fetchea32() : if abrt Then exit Select 
                        tempw=geteaw(): if abrt Then exit Select 
                        setsub16(tempw,regs(reg).w) 
                        cycles-=iif((modo=3),1,2) 
                        Exit Select 
                	case &h139, &h339  /'CMP 32,reg'/
                        fetchea32() : if abrt Then exit Select 
                        templ=geteal(): if abrt Then exit Select 
                        setsub32(templ,regs(reg).l) 
                        cycles-=iif((modo=3),1,2) 
                        Exit Select 
                	case &h3A, &h13A, &h23A, &h33A  /'CMP reg,8'/
                        fetchea32() : if abrt Then exit Select 
                        temp=geteab(): if abrt Then exit Select 
                        setsub8(getr8(reg),temp) 
                        cycles -= iif((modo = 3) , timing_rr , timing_rm) 
                        Exit Select 
                	case &h3B, &h23B  /'CMP reg,16'/
                        fetchea32() : if abrt Then exit Select 
                        tempw=geteaw(): if abrt Then exit Select 
                        setsub16(regs(reg).w,tempw) 
                        cycles -= iif((modo = 3) , timing_rr , timing_rm) 
                        Exit Select 
                	case &h13B, &h33B  /'CMP reg,32'/
                        fetchea32() : if abrt Then exit Select 
                        templ=geteal(): if abrt Then exit Select 
                        setsub32(regs(reg).l,templ) 
                        cycles -= iif((modo = 3) , timing_rr , timing_rml) 
                        Exit Select 
                	case &h3C, &h13C, &h23C, &h33C  /'CMP AL,#8'/
                        temp=getbytef() 
                        setsub8(AL,temp) 
                        cycles -= timing_rr 
                        Exit Select 
                	case &h3D, &h23D  /'CMP AX,#16'/
                        tempw=getwordf() 
                        setsub16(AX,tempw) 
                        cycles -= timing_rr 
                        Exit Select 
                	case &h13D, &h33D  /'CMP EAX,#32'/
                        templ=getlong(): if abrt then exit Select 
                        setsub32(EAX,templ) 
                        cycles -= timing_rr 
                        Exit Select 
                	case &h3E, &h13E, &h23E, &h33E  /'DS:'/
                        oldss=ss0
                        oldds=ds0
                        'ds0=ds0 ''''
                        ss0=ds0 ''' igualdad rara, pero correctas
                        ssegs=2 
                        cycles-=4 
                        goto opcodestart ' salta al principio, arriba
                        
                	case &h3F, &h13F, &h23F, &h33F  /'AAS'/
                        if ((flags And A_FLAG)<>0) Or ((AL And &hF)>9) Then 
                          AL-=6 
                          AH-=1 
                          flags = flags Or (A_FLAG Or C_FLAG) 
                        else
                          flags = flags And inv(A_FLAG Or C_FLAG)
                        EndIf
                        AL = AL And &hF 
                        cycles-=3 
                        Exit Select 
                  /'INC r16'/
                	case &h40, &h41, &h42, &h43 , _ 
                	 		&h44, &h45, &h46, &h47 , _
                	 		&h240, &h241, &h242, &h243 , _ 
                	 		&h244, &h245, &h246, &h247 
                        setadd16nc(regs(opcode And 7).w,1) 
                        regs(opcode And 7).w+=1 
                        cycles -= timing_rr 
                        Exit Select 
                  /'INC r32'/
                	case &h140, &h141, &h142, &h143 , _  
                	 		&h144, &h145, &h146, &h147  , _
                	 		&h340, &h341, &h342, &h343  , _
                	 		&h344, &h345, &h346, &h347  
                        setadd32nc(regs(opcode And 7).l,1) 
                        regs(opcode And 7).l+=1 
                        cycles -= timing_rr 
                        Exit Select 
                  /'DEC r16'/
                	case &h48, &h49, &h4A, &h4B  , _
                	 		&h4C, &h4D, &h4E, &h4F  , _
                	 		&h248, &h249, &h24A, &h24B  , _
                	 		&h24C, &h24D, &h24E, &h24F 
                        setsub16nc(regs(opcode And 7).w,1) 
                        regs(opcode And 7).w-=1 
                        cycles -= timing_rr 
                        Exit Select 
                  /'DEC r32'/
                	case &h148, &h149, &h14A, &h14B  , _
                	 		&h14C, &h14D, &h14E, &h14F  , _
                	 		&h348, &h349, &h34A, &h34B  , _
                	 		&h34C, &h34D, &h34E, &h34F 
                        setsub32nc(regs(opcode And 7).l,1) 
                        regs(opcode And 7).l-=1 
                        cycles -= timing_rr 
                        Exit Select 
                  /'PUSH r16'/
                	case &h50, &h51, &h52, &h53 , _ 
                	 		&h54, &h55, &h56, &h57 , _
                	 		&h250, &h251, &h252, &h253 , _
                	 		&h254, &h255, &h256, &h257 
                        if ssegs Then ss0=oldss 
                        if stack32 Then 
                                writememw_386(ss0,ESP-2,regs(opcode And 7).w): if abrt Then exit Select 
                                ESP-=2 
                        else
                                writememw_386(ss0,(SP-2) And &hFFFF,regs(opcode And 7).w): if abrt Then exit Select 
                                SP-=2 
                        EndIf
                        cycles-=1
                        Exit Select 
                  /'PUSH r32'/
                	case &h150, &h151, &h152, &h153 , _ 
                	 		&h154, &h155, &h156, &h157 , _
                	 		&h350, &h351, &h352, &h353 , _
                	 		&h354, &h355, &h356, &h357 
                        if ssegs Then ss0=oldss 
                        if stack32 Then 
                                writememl_386(ss0,ESP-4,regs(opcode And 7).l): if abrt then exit Select 
                                ESP-=4 
                        else
                                writememl_386(ss0,(SP-4) And &hFFFF,regs(opcode And 7).l): if abrt Then exit Select 
                                SP-=4 
                        EndIf
                        cycles-=1
                        Exit Select 
                   /'POP r16'/
                	case &h58, &h59, &h5A, &h5B , _
                	 		&h5C, &h5D, &h5E, &h5F , _
                			&h258, &h259, &h25A, &h25B , _
                	 		&h25C, &h25D, &h25E, &h25F 
                        if ssegs Then ss0=oldss 
                        if stack32 Then 
                                ESP+=2 
                                tempw=readmemw_386(ss0,ESP-2): if abrt Then  ESP-=2: Exit Select   
                        else
                                SP+=2
                                tempw=readmemw_386(ss0,(SP-2) And &hFFFF): if abrt Then SP-=2: Exit Select  
                        EndIf
                        regs(opcode And 7).w=tempw 
                        cycles-=1
                        Exit Select 
                  /'POP r32'/
                	case &h158, &h159, &h15A, &h15B , _ 
                	 		&h15C, &h15D, &h15E, &h15F , _
                		 	&h358, &h359, &h35A, &h35B , _
                		 	&h35C, &h35D, &h35E, &h35F 
                        if ssegs Then ss0=oldss 
                        if stack32 Then 
                                ESP+=4 
                                templ=readmeml_386(ss0,ESP-4): If abrt Then ESP-=4: Exit Select   
                        else
                                SP+=4
                                templ=readmeml_386(ss0,(SP-4) And &hFFFF): if abrt Then SP-=4: Exit Select  
                        EndIf
                        regs(opcode And 7).l=templ 
                        cycles-=1
                        Exit Select 
                	case &h60, &h260  /'PUSHA'/
                        if stack32 Then 
                                writememw_386(ss0,ESP-2,AX) 
                                writememw_386(ss0,ESP-4,CX) 
                                writememw_386(ss0,ESP-6,DX) 
                                writememw_386(ss0,ESP-8,BX) 
                                writememw_386(ss0,ESP-10,SP) 
                                writememw_386(ss0,ESP-12,BP) 
                                writememw_386(ss0,ESP-14,SI) 
                                writememw_386(ss0,ESP-16,DI) 
                                if abrt=0 Then ESP-=16 
                        else
                                writememw_386(ss0,((SP-2) And &hFFFF),AX) 
                                writememw_386(ss0,((SP-4) And &hFFFF),CX) 
                                writememw_386(ss0,((SP-6) And &hFFFF),DX) 
                                writememw_386(ss0,((SP-8) And &hFFFF),BX) 
                                writememw_386(ss0,((SP-10) And &hFFFF),SP) 
                                writememw_386(ss0,((SP-12) And &hFFFF),BP) 
                                writememw_386(ss0,((SP-14) And &hFFFF),SI) 
                                writememw_386(ss0,((SP-16) And &hFFFF),DI) 
                                if abrt=0 Then SP-=16 
                        EndIf
                        cycles-=11
                        Exit Select 
                	case &h61, &h261  /'POPA'/
                        if stack32 Then 
                                DI=readmemw_386(ss0,ESP): if abrt Then exit Select 
                                SI=readmemw_386(ss0,ESP+2):  if abrt then exit Select 
                                BP=readmemw_386(ss0,ESP+4):  if abrt then exit Select 
                                BX=readmemw_386(ss0,ESP+8):  if abrt then exit Select 
                                DX=readmemw_386(ss0,ESP+10): if abrt then exit Select 
                                CX=readmemw_386(ss0,ESP+12): if abrt then exit Select 
                                AX=readmemw_386(ss0,ESP+14): if abrt then exit Select 
                                ESP+=16 
                        else
                                DI=readmemw_386(ss0,((SP) And &hFFFF)): if abrt Then exit Select 
                                SI=readmemw_386(ss0,((SP+2) And &hFFFF)): if abrt Then exit Select 
                                BP=readmemw_386(ss0,((SP+4) And &hFFFF)): if abrt Then exit Select 
                                BX=readmemw_386(ss0,((SP+8) And &hFFFF)): if abrt Then exit Select 
                                DX=readmemw_386(ss0,((SP+10) And &hFFFF)): if abrt Then exit Select 
                                CX=readmemw_386(ss0,((SP+12) And &hFFFF)): if abrt Then exit Select 
                                AX=readmemw_386(ss0,((SP+14) And &hFFFF)): if abrt Then exit Select 
                                SP+=16 
                        EndIf
                        cycles-=9
                        Exit Select 
                	case &h160, &h360  /'PUSHA'/
                        if stack32 Then 
                                writememl_386(ss0,ESP-4,EAX) 
                                writememl_386(ss0,ESP-8,ECX) 
                                writememl_386(ss0,ESP-12,EDX) 
                                writememl_386(ss0,ESP-16,EBX) 
                                writememl_386(ss0,ESP-20,ESP) 
                                writememl_386(ss0,ESP-24,EBP) 
                                writememl_386(ss0,ESP-28,ESI) 
                                writememl_386(ss0,ESP-32,EDI) 
                                if abrt=0 Then ESP-=32 
                        else
                                writememl_386(ss0,((SP-4) And &hFFFF),EAX) 
                                writememl_386(ss0,((SP-8) And &hFFFF),ECX) 
                                writememl_386(ss0,((SP-12) And &hFFFF),EDX) 
                                writememl_386(ss0,((SP-16) And &hFFFF),EBX) 
                                writememl_386(ss0,((SP-20) And &hFFFF),ESP) 
                                writememl_386(ss0,((SP-24) And &hFFFF),EBP) 
                                writememl_386(ss0,((SP-28) And &hFFFF),ESI) 
                                writememl_386(ss0,((SP-32) And &hFFFF),EDI) 
                                if abrt=0 Then SP-=32 
                        EndIf
                        cycles-=11
                        Exit Select 
                	case &h161, &h361  /'POPA'/
                        if stack32 Then 
                                EDI=readmeml_386(ss0,ESP): if abrt Then exit Select 
                                ESI=readmeml_386(ss0,ESP+4): if abrt Then exit Select 
                                EBP=readmeml_386(ss0,ESP+8): if abrt Then exit Select 
                                EBX=readmeml_386(ss0,ESP+16): if abrt Then exit Select 
                                EDX=readmeml_386(ss0,ESP+20): if abrt Then exit Select 
                                ECX=readmeml_386(ss0,ESP+24): if abrt Then exit Select 
                                EAX=readmeml_386(ss0,ESP+28): if abrt Then exit Select 
                                ESP+=32 
                        else
                                EDI=readmeml_386(ss0,((SP) And &hFFFF)): if abrt Then exit Select 
                                ESI=readmeml_386(ss0,((SP+4) And &hFFFF)): if abrt Then exit Select 
                                EBP=readmeml_386(ss0,((SP+8) And &hFFFF)): if abrt Then exit Select 
                                EBX=readmeml_386(ss0,((SP+16) And &hFFFF)): if abrt Then exit Select 
                                EDX=readmeml_386(ss0,((SP+20) And &hFFFF)): if abrt Then exit Select 
                                ECX=readmeml_386(ss0,((SP+24) And &hFFFF)): if abrt Then exit Select 
                                EAX=readmeml_386(ss0,((SP+28) And &hFFFF)): if abrt Then exit Select 
                                SP+=32 
                        EndIf
                        cycles-=9
                        Exit Select 
                	case &h62, &h262  /'BOUND'/
                        fetchea32() : if abrt Then exit Select 
                        tempw=geteaw() 
                        tempw2=readmemw_386(easeg,eaaddr+2): if abrt then exit Select 
                        If (cshort(regs(reg).w)<cshort(tempw))  Or  (cshort(regs(reg).w)>CShort(tempw2)) Then 
                                x86_int(5) 
                        EndIf
                        cycles-=7
                        Exit Select 
                	case &h162, &h362  /'BOUND'/
                        fetchea32() : if abrt Then exit Select 
                        templ=geteal() 
                        templ2=readmeml_386(easeg,eaaddr+4): if abrt then exit Select 
                        if (clng(regs(reg).l)<clng(templ))  Or  (clng(regs(reg).l)>CLng(templ2)) Then 
                                x86_int(5) 
                        EndIf
                        cycles-=7
                        Exit Select 
                	case &h63, &h163, &h263, &h363  /'ARPL'/
                        if NOTRM() then exit select
                        fetchea32() : if abrt Then exit Select 
                        tempw=geteaw(): if abrt then exit Select 
                        if (tempw And 3)<(regs(reg).w And 3) Then 
                                tempw=(tempw And &hFFFC) Or (regs(reg).w And 3) 
                                seteaw(tempw): if abrt then exit Select 
                                flags = flags Or Z_FLAG 
                        else
                           flags = flags And inv(Z_FLAG)
                        EndIf
                        cycles-=9
                        Exit Select 
                	case &h64, &h164, &h264, &h364  /'FS:'/
                        oldss=ss0 
                        oldds=ds0 
                        'rds=FS1 
                        ds0=fs0 
                        ss0=fs0
                        ssegs=2 
                        cycles-=4 
                        goto opcodestart ' salta al principio
                        
                	case &h65, &h165, &h265, &h365  /'GS:'/
                        oldss=ss0 
                        oldds=ds0 
                        'rds=GS1 
                        ds0=gs0 
                        ss0=gs0
                        ssegs=2 
                        cycles-=4 
                        goto opcodestart ' salta al principio
                        
                	case &h66, &h166, &h266, &h366  /'Data size select'/
                        op32=((use32 And &h100) Xor &h100) Or (op32 And &h200) 
                        cycles-=2 
                        goto opcodestart ' salta al principio
                        
                	case &h67, &h167, &h267, &h367  /'Address size select'/
                        op32=((use32 And &h200) Xor &h200) Or (op32 And &h100) 
                        cycles-=2 
                        goto opcodestart ' salta al principio
                        
                	case &h68, &h268  /'PUSH #w'/
                        tempw=getword() 
                        if stack32 Then 
                                writememw_386(ss0,ESP-2,tempw): if abrt Then exit Select 
                                ESP-=2 
                        else
                                writememw_386(ss0,((SP-2) And &hFFFF),tempw): if abrt Then exit Select 
                                SP-=2 
                        EndIf
                        cycles-=2 
                        Exit Select 
                	case &h168, &h368  /'PUSH #l'/
                        templ=getlong() 
                        if stack32 Then 
                                writememl_386(ss0,ESP-4,templ): if abrt Then exit Select 
                                ESP-=4 
                        else
                                writememl_386(ss0,((SP-4) And &hFFFF),templ): if abrt Then exit Select 
                                SP-=4 
                        EndIf
                        cycles-=2 
                        Exit Select 
                	case &h69, &h269  /'IMUL r16'/
                        fetchea32() : if abrt Then exit Select 
                        tempw=geteaw(): if abrt Then exit Select 
                        tempw2=getword(): if abrt Then exit Select 
                        templ=(Clng(CShort(tempw)))*(Clng(CShort(tempw2))) 
                        if ((templ Shr 16)<>0)  And  ((templ Shr 16)<>&hFFFF) Then 
                                flags = flags Or (C_FLAG Or V_FLAG) 
                        else
                               flags = flags And inv(C_FLAG Or V_FLAG)
                        EndIf
                        regs(reg).w=templ And &hFFFF 
                        cycles-=iif((modo=3),14,17) 
                        Exit Select 
                	case &h169, &h369  /'IMUL r32'/
                        fetchea32() : if abrt Then exit Select 
                        templ=geteal(): if abrt Then exit Select 
                        templ2=getlong(): if abrt Then exit Select 
                        temp64=CLngInt(CLng(templ))*CLngInt(CLng(templ2)) 
                        if (culng(temp64 Shr 32)<>0)  And  (CULng(temp64 Shr 32)<>&hFFFFFFFF) Then 
                              flags = flags Or (C_FLAG Or V_FLAG) 
                        else
                              flags = flags And inv(C_FLAG Or V_FLAG)
                        EndIf
                        regs(reg).l=temp64 And &hFFFFFFFF 
                        cycles-=25 
                        Exit Select 
                	case &h6A, &h26A /'PUSH #eb'/
                        tempw=readmemb_386(cs0,pc): pc+=1 
                        if (tempw And &h80) Then tempw = tempw Or &hFF00 
                        ''if (deb=3) Then print #5,"PUSH 1:",tempw,stack32 
                        if stack32 Then 
                                writememw_386(ss0,ESP-2,tempw): if abrt Then exit Select 
                                ESP-=2 
                        else
                                writememw_386(ss0,((SP-2) And &hFFFF),tempw): if abrt Then exit Select 
                                SP-=2 
                        EndIf
                        cycles-=2 
                        Exit Select 
                	case &h16A, &h36A /'PUSH #eb'/
                        templ=readmemb_386(cs0,pc): pc+=1 
                        if (templ And &h80) Then templ = templ Or &hFFFFFF00 
                        ''if (deb=3) Then print #5,"PUSH 2:",templ,stack32
                        if stack32 Then 
                                writememl_386(ss0,ESP-4,templ): if abrt Then exit Select 
                                ESP-=4 
                        else
                                writememl_386(ss0,((SP-4) And &hFFFF),templ): if abrt Then exit Select 
                                SP-=4 
                        EndIf
                        cycles-=2 
                        Exit Select 
                	case &h6B, &h26B  /'IMUL r8'/
                        fetchea32() : if abrt Then exit Select 
                        tempw=geteaw(): if abrt Then exit Select 
                        tempw2=readmemb_386(cs0,pc): pc+=1: if abrt Then exit Select 
                        if (tempw2 And &h80) Then tempw2 = tempw2 Or &hFF00 
                        templ=(CLng(CShort(tempw)))*(CLng(CShort(tempw2))) 
                        if ((templ Shr 16)<>0) And (((templ Shr 16) And &hFFFF)<>&hFFFF) Then 
                                flags = flags Or C_FLAG Or V_FLAG 
                        else
                                flags = flags And inv(C_FLAG Or V_FLAG)
                        EndIf
                        regs(reg).w=templ And &hFFFF 
                        cycles-=iif((modo=3),14,17) 
                        Exit Select 
                	case &h16B, &h36B  /'IMUL r8'/
                        fetchea32() : if abrt Then exit Select 
                        templ=geteal(): if abrt Then exit Select 
                        templ2=readmemb_386(cs0,pc): pc+=1: if abrt Then exit Select 
                        if (templ2 And &h80) Then templ2 = templ2 Or &hFFFFFF00 
                        temp64=CLngInt(CLng(templ))*CLngInt(CLng(templ2)) 
                        if (culng(temp64 Shr 32)<>0)  And (CULng(temp64 Shr 32)<>&hFFFFFFFF) Then 
                                flags = flags Or C_FLAG Or V_FLAG 
                        else
                                flags = flags And inv(C_FLAG Or V_FLAG)
                        EndIf
                        regs(reg).l=temp64 And &hFFFFFFFF 
                        cycles-=20 
                        Exit Select 
                	case &h6C, &h16C  /'INSB'/
                        If checkio_perm(DX)=0 Then Exit Select ' error IO
                        temp=inb(DX) 
                        writememb_386(es0,DI,temp): if abrt then exit Select 
                        if (flags And D_FLAG) Then 
                              DI-=1 
                        else
                              DI+=1
                        EndIf
                        cycles-=15 
                        Exit Select 
                	case &h26C, &h36C  /'INSB'/
                        If checkio_perm(DX)=0 Then Exit Select ' error IO
                        temp=inb(DX) 
                        writememb_386(es0,EDI,temp): if abrt Then exit Select 
                        if (flags And D_FLAG) Then 
                               EDI-=1 
                        else
                               EDI+=1
                        EndIf
                        cycles-=15 
                        Exit Select 
                	case &h6D  /'INSW'/
                        If checkio_perm(DX)=0 Then Exit Select ' error IO
                        If checkio_perm(DX+1)=0 Then Exit Select ' error IO
                        tempw=inw(DX) 
                        writememw_386(es0,DI,tempw): if abrt Then exit Select 
                        if (flags And D_FLAG) Then 
                               DI-=2 
                        else
                               DI+=2
                        EndIf
                        cycles-=15 
                        Exit Select 
                	case &h16D  /'INSL'/
                        If checkio_perm(DX)=0 Then Exit Select ' error IO 
                        If checkio_perm(DX+1)=0 Then Exit Select ' error IO 
                        If checkio_perm(DX+2)=0 Then Exit Select ' error IO 
                        If checkio_perm(DX+3)=0 Then Exit Select ' error IO 
                        templ=inl(DX) 
                        writememl_386(es0,DI,templ): if abrt Then exit Select 
                        if (flags And D_FLAG) Then 
                               DI-=4 
                        else
                               DI+=4
                        EndIf
                        cycles-=15 
                        Exit Select 
                	case &h26D  /'INSW'/
                        If checkio_perm(DX)=0 Then Exit Select ' error IO 
                        If checkio_perm(DX+1)=0 Then Exit Select ' error IO 
                        tempw=inw(DX) 
                        writememw_386(es0,EDI,tempw): if abrt Then exit Select 
                        if (flags And D_FLAG) Then 
                               EDI-=2 
                        else
                               EDI+=2
                        EndIf
                        cycles-=15 
                        Exit Select 
                	case &h36D  /'INSL'/
                        If checkio_perm(DX)=0 Then Exit Select ' error IO 
                        If checkio_perm(DX+1)=0 Then Exit Select ' error IO 
                        If checkio_perm(DX+2)=0 Then Exit Select ' error IO 
                        If checkio_perm(DX+3)=0 Then Exit Select ' error IO 
                        templ=inl(DX) 
                        writememl_386(es0,EDI,templ): if abrt Then exit Select 
                        if (flags And D_FLAG) Then 
                               EDI-=4 
                        else
                               EDI+=4
                        EndIf
                        cycles-=15 
                        Exit Select 
                	case &h6E, &h16E  /'OUTSB'/
                        temp=readmemb_386(ds0,SI): if abrt Then exit Select 
                        If checkio_perm(DX)=0 Then Exit Select ' error IO  
                        if (flags And D_FLAG) Then 
                               SI-=1 
                        else
                               SI+=1
                        EndIf
                        outb(DX,temp) 
                        cycles-=14 
                        Exit Select 
                	case &h26E, &h36E  /'OUTSB'/
                        If checkio_perm(DX)=0 Then Exit Select ' error IO  
                        temp=readmemb_386(ds0,ESI): if abrt Then exit Select 
                        if (flags And D_FLAG) Then 
                               ESI-=1 
                        else
                               ESI+=1
                        EndIf
                        outb(DX,temp) 
                        cycles-=14 
                        Exit Select 
                	case &h6F  /'OUTSW'/
                        tempw=readmemw_386(ds0,SI): if abrt Then exit Select 
                        If checkio_perm(DX)=0 Then Exit Select ' error IO 
                        If checkio_perm(DX+1)=0 Then Exit Select ' error IO 
                        if (flags And D_FLAG) Then 
                               SI-=2 
                        else
                               SI+=2
                        EndIf
                        outw(DX,tempw) 
                        cycles-=14 
                        Exit Select 
                	case &h16F  /'OUTSL'/
                        tempw=readmemw_386(ds0,SI): if abrt Then exit Select 
                        If checkio_perm(DX)=0 Then Exit Select ' error IO 
                        If checkio_perm(DX+1)=0 Then Exit Select ' error IO 
                        If checkio_perm(DX+2)=0 Then Exit Select ' error IO 
                        If checkio_perm(DX+3)=0 Then Exit Select ' error IO 
                        if (flags And D_FLAG) Then 
                             SI-=4 
                        else
                             SI+=4
                        EndIf
                        outl(EDX,templ) 
                        cycles-=14 
                        Exit Select 
                	case &h26F  /'OUTSW'/
                        tempw=readmemw_386(ds0,ESI): if abrt Then exit Select 
                        If checkio_perm(DX)=0 Then Exit Select ' error IO 
                        If checkio_perm(DX+1)=0 Then Exit Select ' error IO 
                        if (flags And D_FLAG) Then 
                             ESI-=2 
                        else
                             ESI+=2
                        EndIf
                        outw(DX,tempw) 
                        cycles-=14 
                        Exit Select 
                	case &h36F  /'OUTSL'/
                        tempw=readmemw_386(ds0,ESI): if abrt Then exit Select 
                        If checkio_perm(DX)=0 Then Exit Select ' error IO 
                        If checkio_perm(DX+1)=0 Then Exit Select ' error IO 
                        If checkio_perm(DX+2)=0 Then Exit Select ' error IO 
                        If checkio_perm(DX+3)=0 Then Exit Select ' error IO 
                        if (flags And D_FLAG) Then 
                             ESI-=4 
                        else
                             ESI+=4
                        EndIf
                        outl(EDX,templ) 
                        cycles-=14 
                        Exit Select 
                	case &h70, &h170, &h270, &h370  /'JO'/
                        offset=cbyte(getbytef())
                        if (flags And V_FLAG) Then 
                                 pc += offset: cycles -= timing_bt  
                        EndIf
                        cycles -= timing_bnt 
                        Exit Select 
                	case &h71, &h171, &h271, &h371  /'JNO'/
                        offset=cbyte(getbytef())
                        if (flags And V_FLAG)=0 Then 
                                 pc += offset: cycles -= timing_bt  
                        EndIf
                        cycles -= timing_bnt 
                        Exit Select 
                	case &h72, &h172, &h272, &h372  /'JB'/
                        offset=cbyte(getbytef()) 
                        if (flags And C_FLAG) Then 
                                 pc += offset: cycles -= timing_bt  
                        EndIf
                        cycles -= timing_bnt 
                        Exit Select 
                	case &h73, &h173, &h273, &h373  /'JNB'/
                        offset=cbyte(getbytef()) 
                        if (flags And C_FLAG)=0 Then 
                                 pc += offset: cycles -= timing_bt  
                        EndIf
                        cycles -= timing_bnt 
                        Exit Select 
                	case &h74, &h174, &h274, &h374  /'JZ'/
                        offset=cbyte(getbytef()) 
                        if (flags And Z_FLAG) Then 
                                 pc += offset: cycles -= timing_bt  
                        EndIf
                        cycles -= timing_bnt 
                        Exit Select 
                	case &h75, &h175, &h275, &h375  /'JNZ'/
                        offset=cbyte(getbytef()) 
                        if (flags And Z_FLAG)=0 Then 
                                 pc += offset: cycles -= timing_bt  
                        EndIf
                        cycles -= timing_bnt 
                        Exit Select 
                	case &h76, &h176, &h276, &h376  /'JBE'/
                        offset=cbyte(getbytef()) 
                        if (flags And (C_FLAG Or Z_FLAG)) Then 
                                 pc += offset: cycles -= timing_bt  
                        EndIf
                        cycles -= timing_bnt 
                        Exit Select 
                	case &h77, &h177, &h277, &h377  /'JNBE'/
                        offset=cbyte(getbytef()) 
                        if (flags And (C_FLAG Or Z_FLAG))=0 Then 
                                 pc += offset: cycles -= timing_bt  
                        EndIf
                        cycles -= timing_bnt 
                        Exit Select 
                	case &h78, &h178, &h278, &h378  /'JS'/
                        offset=cbyte(getbytef()) 
                        if (flags And N_FLAG) Then 
                                  pc += offset: cycles -= timing_bt  
                        EndIf
                        cycles -= timing_bnt 
                        Exit Select 
                	case &h79, &h179, &h279, &h379  /'JNS'/
                        offset=cbyte(getbytef()) 
                        if (flags And N_FLAG)=0 Then 
                                  pc += offset: cycles -= timing_bt  
                        EndIf
                        cycles -= timing_bnt 
                        Exit Select 
                	case &h7A, &h17A, &h27A, &h37A  /'JP'/
                        offset=cbyte(getbytef()) 
                        if (flags And P_FLAG) Then 
                                  pc += offset: cycles -= timing_bt  
                        EndIf
                        cycles -= timing_bnt 
                        Exit Select 
                	case &h7B, &h17B, &h27B, &h37B  /'JNP'/
                        offset=cbyte(getbytef()) 
                        if (flags And P_FLAG)=0 Then 
                                  pc += offset: cycles -= timing_bt  
                        EndIf
                        cycles -= timing_bnt 
                        Exit Select 
                	case &h7C, &h17C, &h27C, &h37C  /'JL'/
                        offset=cbyte(getbytef()) 
                        temp=iif((flags And N_FLAG),1,0 )
                        temp2=iif((flags And V_FLAG),1,0 )
                        if (temp<>temp2) Then 
                                  pc += offset: cycles -= timing_bt  
                        EndIf
                        cycles -= timing_bnt 
                        Exit Select 
                	case &h7D, &h17D, &h27D, &h37D  /'JNL'/
                        offset=cbyte(getbytef()) 
                        temp=iif((flags And N_FLAG),1,0 )
                        temp2=iif((flags And V_FLAG),1,0 )
                        if (temp=temp2) Then 
                                  pc += offset: cycles -= timing_bt  
                        EndIf
                        cycles -= timing_bnt 
                        Exit Select 
                	case &h7E, &h17E, &h27E, &h37E  /'JLE'/
                        offset=cbyte(getbytef()) 
                        temp=iif((flags And N_FLAG),1,0 )
                        temp2=iif((flags And V_FLAG),1,0 )
                        if (flags And Z_FLAG)  Or  (temp<>temp2) Then 
                                  pc += offset: cycles -= timing_bt  
                        EndIf
                        cycles -= timing_bnt 
                        Exit Select 
                	case &h7F, &h17F, &h27F, &h37F  /'JNLE'/
                        offset=cbyte(getbytef()) 
                        temp=IIf((flags And N_FLAG),1,0 )
                        temp2=iif((flags And V_FLAG),1,0 )
                        if ( ((flags And Z_FLAG)<>0) Or (temp<>temp2) )=0 Then 
                                  pc += offset: cycles -= timing_bt  
                        EndIf
                        cycles -= timing_bnt 
                        Exit Select 
                	case &h80, &h180, &h280, &h380 ,_
		                 &h82, &h182, &h282, &h382 
                        fetchea32() : if abrt Then exit Select 
                        temp=geteab(): if abrt Then exit Select 
                        temp2=readmemb_386(cs0,pc): pc+=1: if abrt Then exit Select 
                        Select Case As Const  (rmdat And &h38)
                        	Case &h00  /'ADD b,#8'/
                                seteab(temp+temp2): if abrt Then exit Select 
                                setadd8(temp,temp2) 
                                cycles -= iif((modo = 3) , timing_rr , timing_mr )
                                Exit Select 
                        	case &h08  /'OR b,#8'/
                                temp = temp Or temp2 
                                seteab(temp): if abrt Then exit Select 
                                setznp8(temp) 
                                cycles -= iif((modo = 3) , timing_rr , timing_mr )
                                Exit Select 
                        	case &h10  /'ADC b,#8'/
                                seteab(temp+temp2+tempc): if abrt then exit Select 
                                setadc8(temp,temp2) 
                                cycles -= iif((modo = 3) , timing_rr , timing_mr )
                                Exit Select 
                        	case &h18  /'SBB b,#8'/
                                seteab(temp-(temp2+tempc)): if abrt then exit Select 
                                setsbc8(temp,temp2) 
                                cycles -= iif((modo = 3) , timing_rr , timing_mr )
                                Exit Select 
                        	case &h20  /'AND b,#8'/
                                temp = temp And temp2 
                                seteab(temp): if abrt Then exit Select 
                                setznp8(temp) 
                                cycles -= iif((modo = 3) , timing_rr , timing_mr )
                                Exit Select 
                        	case &h28  /'SUB b,#8'/
                                seteab(temp-temp2): if abrt Then exit Select 
                                setsub8(temp,temp2) 
                                cycles -= iif((modo = 3) , timing_rr , timing_mr )
                                Exit Select 
                        	case &h30  /'XOR b,#8'/
                                temp = temp Xor temp2 
                                seteab(temp): if abrt Then exit Select 
                                setznp8(temp) 
                                cycles -= iif((modo = 3) , timing_rr , timing_mr )
                                Exit Select 
                        	case &h38  /'CMP b,#8'/
                                setsub8(temp,temp2) 
                                cycles-=iif((modo=3),1,2) 
                                Exit Select 
                        End Select
                       Exit Select 
                	case &h81, &h281 
                        fetchea32() : if abrt Then exit Select 
                        tempw=geteaw(): if abrt Then exit Select 
                        tempw2=getword(): if abrt Then exit Select 
                        Select Case As Const  (rmdat And &h38)
                        	case &h00  /'ADD w,#16'/
                                seteaw(tempw+tempw2): if abrt Then exit Select 
                                setadd16(tempw,tempw2) 
                                cycles -= iif((modo = 3) , timing_rr , timing_mr )
                                Exit Select 
                        	case &h08  /'OR w,#16'/
                                tempw = tempw Or tempw2 
                                seteaw(tempw): if abrt Then exit Select 
                                setznp16(tempw) 
                                cycles -= iif((modo = 3) , timing_rr , timing_mr )
                                Exit Select 
                        	case &h10  /'ADC w,#16'/
                                seteaw(tempw+tempw2+tempc): if abrt then exit Select 
                                setadc16(tempw,tempw2) 
                                cycles -= iif((modo = 3) , timing_rr , timing_mr )
                                Exit Select 
                        	case &h20  /'AND w,#16'/
                                tempw = tempw And tempw2 
                                seteaw(tempw): if abrt Then exit Select 
                                setznp16(tempw) 
                                cycles -= iif((modo = 3) , timing_rr , timing_mr )
                                Exit Select 
                        	case &h18  /'SBB w,#16'/
                                seteaw(tempw-(tempw2+tempc)): if abrt Then exit Select 
                                setsbc16(tempw,tempw2) 
                                cycles -= iif((modo = 3) , timing_rr , timing_mr )
                                Exit Select 
                        	case &h28  /'SUB w,#16'/
                                seteaw(tempw-tempw2): if abrt Then exit Select 
                                setsub16(tempw,tempw2) 
                                cycles -= iif((modo = 3) , timing_rr , timing_mr )
                                Exit Select 
                        	case &h30  /'XOR w,#16'/
                                tempw = tempw Xor tempw2 
                                seteaw(tempw): if abrt Then exit Select 
                                setznp16(tempw) 
                                cycles -= iif((modo = 3) , timing_rr , timing_mr )
                                Exit Select 
                        	case &h38  /'CMP w,#16'/
                                setsub16(tempw,tempw2) 
                                cycles-=iif((modo=3),1,2) 
                                Exit Select 
                       End Select
                       Exit Select 
                	case &h181, &h381 
                        fetchea32() : if abrt Then exit Select 
                        templ=geteal(): if abrt Then exit Select 
                        templ2=getlong(): if abrt Then exit Select 
                        Select Case As Const  (rmdat And &h38)
                        	case &h00  /'ADD l,#32'/
                                seteal(templ+templ2): if abrt Then exit Select 
                                setadd32(templ,templ2) 
                                cycles -= iif((modo = 3) , timing_rr , timing_mrl )
                                Exit Select 
                        	case &h08  /'OR l,#32'/
                                templ = templ Or templ2 
                                seteal(templ): if abrt Then exit Select 
                                setznp32(templ) 
                                cycles -= iif((modo = 3) , timing_rr , timing_mrl )
                                Exit Select 
                        	case &h10  /'ADC l,#32'/
                                seteal(templ+templ2+tempc): if abrt Then exit Select 
                                setadc32(templ,templ2) 
                                cycles -= iif((modo = 3) , timing_rr , timing_mrl )
                                Exit Select 
                        	case &h20  /'AND l,#32'/
                                templ = templ And templ2 
                                seteal(templ): if abrt Then exit Select 
                                setznp32(templ) 
                                cycles -= iif((modo = 3) , timing_rr , timing_mrl )
                                Exit Select 
                        	case &h18  /'SBB l,#32'/
                                seteal(templ-(templ2+tempc)): if abrt Then exit Select 
                                setsbc32(templ,templ2) 
                                cycles -= iif((modo = 3) , timing_rr , timing_mrl )
                                Exit Select 
                        	case &h28  /'SUB l,#32'/
                                seteal(templ-templ2): if abrt Then exit Select 
                                setsub32(templ,templ2) 
                                cycles -= iif((modo = 3) , timing_rr , timing_mrl )
                                Exit Select 
                        	case &h30  /'XOR l,#32'/
                                templ = templ Xor templ2 
                                seteal(templ): if abrt Then exit Select 
                                setznp32(templ) 
                                cycles -= iif((modo = 3) , timing_rr , timing_mrl )
                                Exit Select 
                        	case &h38  /'CMP l,#32'/
                                setsub32(templ,templ2) 
                                cycles-=iif((modo=3),1,2) 
                                Exit Select 
                       End Select
                       Exit Select 
                	case &h83, &h283 
                        fetchea32() : if abrt Then exit Select 
                        tempw=geteaw(): if abrt Then exit Select 
                        tempw2=readmemb_386(cs0,pc): pc+=1: if abrt Then exit Select 
                        if (tempw2 And &h80) Then tempw2 = tempw2 Or &hFF00 
                        Select Case As Const  (rmdat And &h38)
                        	case &h00  /'ADD w,#8'/
                                seteaw(tempw+tempw2): if abrt Then exit Select 
                                setadd16(tempw,tempw2) 
                                cycles -= iif((modo = 3) , timing_rr , timing_mr )
                                Exit Select 
                        	case &h08  /'OR w,#8'/
                                tempw = tempw Or tempw2 
                                seteaw(tempw): if abrt Then exit Select 
                                setznp16(tempw) 
                                cycles -= iif((modo = 3) , timing_rr , timing_mr )
                                Exit Select 
                        	case &h10  /'ADC w,#8'/
                                seteaw(tempw+tempw2+tempc): if abrt Then exit Select 
                                setadc16(tempw,tempw2) 
                                cycles -= iif((modo = 3) , timing_rr , timing_mr )
                                Exit Select 
                        	case &h18  /'SBB w,#8'/
                                seteaw(tempw-(tempw2+tempc)): if abrt Then exit Select 
                                setsbc16(tempw,tempw2) 
                                cycles -= iif((modo = 3) , timing_rr , timing_mr )
                                Exit Select 
                        	case &h20  /'AND w,#8'/
                                tempw = tempw And tempw2 
                                seteaw(tempw): if abrt Then exit Select 
                                setznp16(tempw) 
                                cycles -= iif((modo = 3) , timing_rr , timing_mr )
                                Exit Select 
                        	case &h28  /'SUB w,#8'/
                                seteaw(tempw-tempw2): if abrt Then exit Select 
                                setsub16(tempw,tempw2) 
                                cycles -= iif((modo = 3) , timing_rr , timing_mr )
                                Exit Select 
                        	case &h30  /'XOR w,#8'/
                                tempw = tempw Xor tempw2 
                                seteaw(tempw): if abrt Then exit Select 
                                setznp16(tempw) 
                                cycles -= iif((modo = 3) , timing_rr , timing_mr )
                                Exit Select 
                        	case &h38  /'CMP w,#8'/
                                setsub16(tempw,tempw2) 
                                cycles-=iif((modo=3),1,2) 
                                Exit Select 
                       End Select
                       Exit Select 
                	case &h183, &h383 
                        fetchea32() : if abrt Then exit Select 
                        templ=geteal(): if abrt Then exit Select 
                        templ2=readmemb_386(cs0,pc): pc+=1: if abrt Then exit Select 
                        if (templ2 And &h80) Then templ2 = templ2 Or &hFFFFFF00 
                        'If ((opcode Or op32) And &h3FF) =&h383 Then Print #5,Hex(rmdat And &h38)
                        Select Case As Const  (rmdat And &h38)
                        	case &h00  /'ADD l,#32'/
                                seteal(templ+templ2): if abrt Then exit Select 
                                setadd32(templ,templ2) 
                                cycles -= iif((modo = 3) , timing_rr , timing_mr )
                                Exit Select 
                        	case &h08  /'OR l,#32'/
                                templ = templ Or templ2 
                                seteal(templ): if abrt Then exit Select 
                                setznp32(templ) 
                                cycles -= iif((modo = 3) , timing_rr , timing_mr )
                                Exit Select 
                        	case &h10  /'ADC l,#32'/
                                seteal(templ+templ2+tempc): if abrt Then exit Select 
                                setadc32(templ,templ2) 
                                cycles -= iif((modo = 3) , timing_rr , timing_mr )
                                Exit Select 
                        	case &h20  /'AND l,#32'/
                                templ = templ And templ2 
                                seteal(templ): if abrt Then exit Select 
                                setznp32(templ) 
                                cycles -= iif((modo = 3) , timing_rr , timing_mr )
                                Exit Select 
                        	case &h18  /'SBB l,#32'/
                                seteal(templ-(templ2+tempc)): if abrt Then exit Select 
                                setsbc32(templ,templ2) 
                                cycles -= iif((modo = 3) , timing_rr , timing_mr )
                                Exit Select 
                        	case &h28  /'SUB l,#32'/
                                seteal(templ-templ2): if abrt then exit Select 
                                setsub32(templ,templ2) 
                                cycles -= iif((modo = 3) , timing_rr , timing_mr )
                                Exit Select 
                        	case &h30  /'XOR l,#32'/
                                templ = templ Xor templ2 
                                seteal(templ): if abrt Then exit Select 
                                setznp32(templ) 
                                cycles -= iif((modo = 3) , timing_rr , timing_mr )
                                Exit Select 
                        	case &h38  /'CMP l,#32'/
                                setsub32(templ,templ2) 
                                cycles-=iif((modo=3),1,2) 
                                Exit Select 
                       End Select
                       Exit Select 
                	case &h84, &h184, &h284, &h384  /'TEST b,reg'/
                        fetchea32() : if abrt Then exit Select 
                        temp=geteab(): if abrt Then exit Select 
                        temp2=getr8(reg) 
                        setznp8(temp And temp2) 
                        cycles-=iif((modo=3),1,2) 
                        Exit Select 
                	case &h85, &h285  /'TEST w,reg'/
                        fetchea32() : if abrt Then exit Select 
                        tempw=geteaw(): if abrt Then exit Select 
                        tempw2=regs(reg).w 
                        setznp16(tempw And tempw2) 
                        cycles-=iif((modo=3),1,2) 
                        Exit Select 
                	case &h185, &h385  /'TEST l,reg'/
                        fetchea32() : if abrt Then exit Select 
                        templ=geteal(): if abrt Then exit Select 
                        templ2=regs(reg).l 
                        setznp32(templ And templ2) 
                        cycles-=iif((modo=3),1,2) 
                        Exit Select 
                	case &h86, &h186, &h286, &h386  /'XCHG b,reg'/
                        fetchea32() : if abrt Then exit Select 
                        temp=geteab(): if abrt Then exit Select 
                        seteab(getr8(reg)): if abrt Then exit Select 
                        setr8(reg,temp) 
                        cycles-=iif((modo=3),3,5) 
                        Exit Select 
                	case &h87, &h287  /'XCHG w,reg'/
                        fetchea32() : if abrt Then exit Select 
                        tempw=geteaw(): if abrt Then exit Select 
                        seteaw(regs(reg).w): if abrt Then exit Select 
                        regs(reg).w=tempw 
                        cycles-=iif((modo=3),3,5) 
                        Exit Select 
                	case &h187, &h387  /'XCHG l,reg'/
                        fetchea32() : if abrt Then exit Select 
                        templ=geteal(): if abrt Then exit Select 
                        seteal(regs(reg).l): if abrt Then exit Select 
                        regs(reg).l=templ 
                        cycles-=iif((modo=3),3,5) 
                        Exit Select 
                	case &h88, &h188, &h288, &h388  /'MOV b,reg'/
                        fetchea32() : if abrt Then exit Select 
                        seteab(getr8(reg)) 
                        cycles-=1 
                        Exit Select 
                	case &h89, &h289  /'MOV w,reg'/
                        fetchea32() : if abrt Then exit Select 
                        seteaw(regs(reg).w) 
                        cycles-=1 
                        Exit Select 
                	case &h189, &h389  /'MOV l,reg'/
                        fetchea32() : if abrt Then exit Select 
                        seteal(regs(reg).l) 
                        cycles-=1 
                        Exit Select 
                	case &h8A, &h18A, &h28A, &h38A  /'MOV reg,b'/
                        fetchea32() : if abrt Then exit Select 
                        temp=geteab(): if abrt then Exit Select 
                        setr8(reg,temp) 
                        cycles-=1 
                        Exit Select 
                	case &h8B, &h28B  /'MOV reg,w'/
                        fetchea32() : if abrt Then exit Select 
                        tempw=geteaw(): if abrt Then exit Select 
                        regs(reg).w=tempw 
                        cycles-=1 
                        Exit Select 
                	case &h18B, &h38B  /'MOV reg,l'/
                        fetchea32() : if abrt Then exit Select 
                        templ=geteal(): if abrt Then exit Select 
                        regs(reg).l=templ 
                        cycles-=1 
                        Exit Select 
                	case &h8C, &h28C  /'MOV w,sreg'/
                        fetchea32() : if abrt Then exit Select 
                        Select Case As Const  (rmdat And &h38)
                        	Case &h00  /'ES'/
                                seteaw(ES1) 
                                Exit Select 
                        	case &h08  /'CS'/
                                seteaw(CS1) 
                                Exit Select 
                        	case &h18  /'DS'/
                                if ssegs Then ds0=oldds 
                                seteaw(DS1) 
                                Exit Select 
                        	case &h10  /'SS'/
                                if ssegs Then ss0=oldss 
                                seteaw(SS1) 
                                Exit Select 
                        	case &h20  /'FS'/
                                seteaw(FS1) 
                                Exit Select 
                        	case &h28  /'GS'/
                                seteaw(GS1) 
                                Exit Select 
                       End Select
                       cycles-=iif((modo=3),2,3) 
                        Exit Select 
                	case &h18C, &h38C  /'MOV l,sreg'/
                        fetchea32() : if abrt Then exit Select 
                        Select Case As Const  (rmdat And &h38)
                        	Case &h00  /'ES'/
                                if modo=3 Then 
                                        regs(rm).l=ES1 
                                else
                                        seteaw(ES1)
                                EndIf
                                Exit Select 
                        	case &h08  /'CS'/
                                if modo=3 Then 
                                        regs(rm).l=CS1 
                                else
                                        seteaw(CS1)
                                EndIf
                                Exit Select 
                        	case &h18  /'DS'/
                                if ssegs Then ds0=oldds 
                                if modo=3 Then 
                                        regs(rm).l=DS1 
                                else
                                        seteaw(DS1)
                                EndIf
                                Exit Select 
                        	case &h10  /'SS'/
                                if ssegs Then ss0=oldss 
                                if modo=3 Then 
                                        regs(rm).l=SS1 
                                else
                                        seteaw(SS1)
                                EndIf
                                Exit Select 
                        	case &h20  /'FS'/
                                if modo=3 Then 
                                        regs(rm).l=FS1 
                                else
                                        seteaw(FS1)
                                EndIf
                                Exit Select 
                        	case &h28  /'GS'/
                                if modo=3 then 
                                        regs(rm).l=GS1 
                                else
                                        seteaw(GS1)
                                EndIf
                                Exit Select 
                       End Select
                       cycles-=iif((modo=3),2,3) 
                        Exit Select 
                	case &h8D, &h28D  /'LEA'/
                        fetchea32() : if abrt Then exit Select 
                        regs(reg).w=eaaddr 
                        cycles -= timing_rr 
                        Exit Select 
                	case &h18D  /'LEA'/
                        fetchea32() : if abrt Then exit Select 
                        regs(reg).l=eaaddr And &hFFFF 
                        cycles -= timing_rr 
                        Exit Select 
                	case &h38D  /'LEA'/
                        fetchea32() : if abrt Then exit Select 
                        regs(reg).l=eaaddr 
                        cycles -= timing_rr 
                        Exit Select 
                	case &h8E, &h18E, &h28E, &h38E  /'MOV sreg,w'/
                        fetchea32() : if abrt Then exit Select 
                        Select Case As Const  (rmdat And &h38)
                        	case &h00  /'ES'/
                                tempw=geteaw(): if abrt Then exit Select 
                                'If tempw=23 Then Print #5,"_ES:";Hex(_es.base0,8);" ";Hex(_es.limit,8);" ";Hex(_es.limitw,8);" ";Hex(_es.access0,2);" ";Hex(_es.seg,4)
                                loadseg(tempw, _es)
                                Exit Select 
                        	'case &h08 /'CS - 8088/8086 only'/ ' este lo he cogido de la V8.1, en la V7 no venia
                           '     tempw=geteaw()
                           '     loadseg(tempw, _cs)
                           '     Exit Select
                        	case &h10  /'SS'/
                                tempw=geteaw(): if abrt Then exit Select 
                                loadseg(tempw, _ss) 
                                if ssegs Then oldss=ss0 
                                skipnextprint=1 ' esta variable es SOLO para depuracion al inicio del exec386 (ver el comando PRINT del inicio)
										  noint=1 
                                Exit Select 
                        	case &h18  /'DS'/
                                tempw=geteaw(): if abrt Then exit Select 
                                loadseg(tempw, _ds) 
                                if ssegs Then oldds=ds0 
                                Exit Select 
                        	case &h20  /'FS'/
                                tempw=geteaw(): if abrt Then exit Select 
                                loadseg(tempw, _fs) 
                                Exit Select 
                        	case &h28  /'GS'/
                                tempw=geteaw(): if abrt Then exit Select 
                                loadseg(tempw, _gs) 
                                Exit Select 
                       End Select
                       cycles-=iif((modo=3),2,5) 
                       Exit Select 
                	case &h8F, &h28F  /'POPW'/
                        if ssegs Then 
                             templ2=oldss 
                        else
                             templ2=ss0
                        EndIf
                        if stack32 Then 
                                tempw=readmemw_386(templ2,ESP): if abrt Then exit Select 
                                ESP+=2 
                        else
                                tempw=readmemw_386(templ2,SP): if abrt then exit Select 
                                SP+=2 
                        EndIf
                        fetchea32() : if abrt Then exit Select 
                        if ssegs Then ss0=oldss 
                        seteaw(tempw) 
                        if abrt Then 
                                if stack32 Then 
                                	 ESP-=2 
                                else
                                  SP-=2 
                                EndIf
                        EndIf
                        cycles-=iif((modo=3),1,6) 
                        Exit Select 
                	case &h18F, &h38F  /'POPL'/
                        if ssegs Then 
                                templ2=oldss 
                        else
                                templ2=ss0
                        EndIf
                        if stack32 Then 
                                templ=readmeml_386(templ2,ESP): if abrt Then exit Select 
                                ESP+=4 
                        else
                                templ=readmeml_386(templ2,SP): if abrt Then exit Select 
                                SP+=4 
                        EndIf
                        fetchea32() : if abrt Then exit Select 
                        if ssegs Then ss0=oldss 
                        seteal(templ) 
                        if abrt Then 
                              If stack32 Then
                                	ESP-=4 
                              Else
                                	SP-=4 
                              EndIf
                        EndIf
                        cycles-=iif((modo=3),1,6) 
                        Exit Select 
                	case &h90, &h190, &h290, &h390  /'NOP'/
                        cycles-=1 
                        Exit Select 
                  /'XCHG AX'/
                	case &h91, &h92, &h93  , _
                	 		&h94, &h95, &h96, &h97  , _
                	 		&h291, &h292, &h293 , _
                	 		&h294, &h295, &h296, &h297 
                        tempw=AX 
                        AX=regs(opcode And 7).w 
                        regs(opcode And 7).w=tempw 
                        cycles-=3 
                        Exit Select 
                  /'XCHG EAX'/ 
                	case &h191, &h192, &h193  , _
                	 		&h194, &h195, &h196, &h197 , _ 
                	 		&h391, &h392, &h393   , _
                	 		&h394, &h395, &h396, &h397 
                        templ=EAX 
                        EAX=regs(opcode And 7).l 
                        regs(opcode And 7).l=templ 
                        cycles-=3 
                        Exit Select 
                	case &h98, &h298  /'CBW'/
                        AH=IIf ((AL And &h80),&hFF,0 )
                        cycles-=3 
                        Exit Select 
                	case &h198, &h398  /'CWDE'/
                        EAX=IIf ((AX And &h8000),(&hFFFF0000 Or AX),AX )
                        cycles-=3 
                        Exit Select 
                	case &h99, &h299  /'CWD'/
                        DX=IIf ((AX And &h8000),&hFFFF,0 )
                        cycles-=2 
                        Exit Select 
                	case &h199, &h399  /'CDQ'/
                        EDX=IIf ((EAX And &h80000000),&hFFFFFFFF,0 )
                        cycles-=2 
                        Exit Select 
                        
                        
                        
                        
       '''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''                 
                	case &h9A, &h29A  /'CALL FAR'/
                        tempw=getword() 
                        tempw2=getword(): if abrt Then exit Select 
                        ''if (deb = 3) Then print #5,"Call far A ",Hex(tempw2,8),Hex(tempw ,8)
                        tempw3=CS1 
                        templ2 = pc 
                        ''if (deb=3) Then print #5,"Call far B ",Hex(templ2 ,8)
                        if ssegs Then ss0=oldss 
                        oxpc=pc 
                        pc=tempw 
                        optype=CALL0 
                        cgate32=0 
                        ''if (deb = 3) Then print #5,"Load CS..."; 
                        if modoprotegido Then 
                               loadcscall(tempw2) 
                        else
                               loadcs(tempw2)
                        EndIf
                        ''if (deb = 3) Then print #5,"...abrt,cgate32;"; abrt, cgate32
                        optype=0 
                        if abrt Then exit Select 
                        oldss=ss0 
                        if (cgate32) Then goto writecall32 ' llamada 32bits, abajo
                
                'GOTO
                writecall16: 
                        cgate16=0 
                        if stack32 Then 
                                writememw_386(ss0,ESP-2,tempw3) 
                                writememw_386(ss0,ESP-4,templ2): if abrt Then exit Select 
                                ESP-=4 
                        Else
                                writememw_386(ss0,(SP-2) And &hFFFF,tempw3) 
                                ''if (deb=3) Then print #5,"Write CS to ";Hex(SS1,8),Hex(SP-2,8) 
                                writememw_386(ss0,(SP-4) And &hFFFF,templ2): if abrt Then exit Select 
                                ''if (deb=3) Then print #5,"Write PC16 to ";Hex(templ2,8), Hex(SS1,8),Hex(SP-4,8)                     
                                SP-=4 
                        EndIf
                        cycles-=18
                        Exit Select 
            ''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''' 
                	case &h19A, &h39A  /'CALL FAR'/
                        templ=getword(): templ = templ Or (getword() Shl 16) 
                        tempw2=getword(): if abrt Then exit Select 
                        tempw3=CS1 
                        templ2 = pc 
                        if ssegs Then ss0=oldss 
                        oxpc=pc 
                        pc=templ 
                        optype=CALL0 
                        cgate16=0 
                        if modoprotegido Then 
                                loadcscall(tempw2) 
                        else
                                loadcs(tempw2)
                        EndIf
                        optype=0 
                        if abrt Then exit Select 
                        oldss=ss0 
                        if (cgate16) Then goto writecall16 ' llamada 16 bits, arriba
                        
                'GOTO 
                writecall32: 
                        cgate32=0 
                        if stack32 Then 
                                writememl_386(ss0,ESP-4,tempw3) 
                                writememl_386(ss0,ESP-8,templ2): if abrt then exit Select 
                                'if (deb=3) Then print #5,"Write PC32 to ", templ2, SS1,ESP-8 
                                ESP-=8 
                        else
                                writememl_386(ss0,(SP-4) And &hFFFF,tempw3) 
                                writememl_386(ss0,(SP-8) And &hFFFF,templ2): if abrt Then exit Select 
                                'if (deb=3) Then print #5,"Write PC32 to ", templ2, SS1,SP-8 
                                SP-=8 
                        EndIf
                        cycles-=18
                        Exit Select 
        '''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''                
                        
                        
                        
                        
                	case &h9B, &h19B, &h29B, &h39B  /'WAIT'/
                        cycles-=4 
                        Exit Select 
                	case &h9C, &h29C  /'PUSHF'/
                        if ssegs Then ss0=oldss 
                        if ((eflags And VM_FLAG)<>0)  And  (IOPL<3) Then 
                                x86gpf(0) 
                                Exit Select 
                        EndIf
                        if stack32 Then 
                                writememw_386(ss0,ESP-2,flags): if abrt Then exit Select 
                                ESP-=2 
                        Else
                                writememw_386(ss0,((SP-2) And &hFFFF),flags): if abrt Then exit Select 
                                SP-=2 
                        EndIf
                        cycles-=4 
                        Exit Select 
                	case &h19C, &h39C  /'PUSHFD'/
                        if ssegs Then ss0=oldss 
                        if ((eflags And VM_FLAG)<>0) And (IOPL<3) Then 
                                x86gpf(0) 
                                Exit Select 
                        EndIf
                        if (CPUID) Then 
                                tempw=eflags And &h24 
                        else
                                tempw=eflags And 4
                        EndIf
                        if stack32 Then 
                                writememw_386(ss0,ESP-2,tempw) 
                                writememw_386(ss0,ESP-4,flags): if abrt Then exit Select 
                                ESP-=4 
                        else
                                writememw_386(ss0,((SP-2) And &hFFFF),tempw) 
                                writememw_386(ss0,((SP-4) And &hFFFF),flags): if abrt Then exit Select 
                                SP-=4 
                        EndIf
                        cycles-=4 
                        Exit Select 
                	case &h9D, &h29D  /'POPF'/
                        if ssegs Then ss0=oldss 
                        if ((eflags And VM_FLAG)<>0)  And  (IOPL<3) Then 
                                x86gpf(0) 
                                Exit Select  
                        EndIf
                        if stack32 Then 
                                tempw=readmemw_386(ss0,ESP): if abrt then exit Select 
                                ESP+=2 
                        else
                                tempw=readmemw_386(ss0,SP): if abrt Then exit Select 
                                SP+=2 
                        EndIf

                        If (CPL=0) Or (modoprotegido=0) Then 
                           flags=(tempw And &hFFD5) Or 2 
                        ElseIf IOPLp Then 
                        	flags=(flags And &h3000) Or (tempw And &hCFD5) Or 2
                        Else
                           flags=(flags And &hF200) Or (tempw And &h0DD5) Or 2
                        EndIf
                        
                        cycles-=5 
                        Exit Select 
                	case &h19D, &h39D  /'POPFD'/
                        if ssegs Then ss0=oldss 
                        if ((eflags And VM_FLAG)<>0)  And  (IOPL<3) Then 
                                x86gpf(0) 
                                Exit Select  
                        EndIf
                        if stack32 Then 
                                tempw=readmemw_386(ss0,ESP) 
                                tempw2=readmemw_386(ss0,ESP+2): if abrt then exit Select 
                                ESP+=4 
                        else
                                tempw=readmemw_386(ss0,SP) 
                                tempw2=readmemw_386(ss0,SP+2): if abrt then exit Select 
                                SP+=4 
                        EndIf
                        
                        if (CPL=0) Or (modoprotegido=0) Then 
                           flags=(tempw And &hFFD5) Or 2 
                        ElseIf IOPLp Then 
                        	flags=(flags And &h3000) Or (tempw And &hCFD5) Or 2
                        else
                           flags=(flags And &hF200) Or (tempw And &h0DD5) Or 2
                        EndIf
                        
                        tempw2 = tempw2 And &h24
                        tempw2 = tempw2 Or (eflags And 3) 
                        if (CPUID) Then 
                           eflags=tempw2 And &h27 
                        Else
                        	eflags=tempw2 And 7
                        EndIf
                        cycles-=5 
                        Exit Select 
                	case &h9E, &h19E, &h29E, &h39E  /'SAHF'/
                        flags=(flags And &hFF00) Or (AH And &hD5) Or 2 
                        cycles-=3 
                        Exit Select 
                	case &h9F, &h19F, &h29F, &h39F  /'LAHF'/
                        AH=flags And &hFF 
                        cycles-=3 
                        Exit Select 
                	case &hA0, &h1A0  /'MOV AL,(w)'/
                        addr=getword(): if abrt then exit Select 
                        temp=readmemb_386(ds0,addr): if abrt Then exit Select 
                        AL=temp 
                        cycles-=1
                        Exit Select 
                	case &h2A0, &h3A0  /'MOV AL,(l)'/
                        addr=getlong(): if abrt then exit Select 
                        temp=readmemb_386(ds0,addr): if abrt Then exit Select 
                        AL=temp 
                        cycles-=1
                        Exit Select 
                	case &hA1  /'MOV AX,(w)'/
                        addr=getword(): if abrt then exit Select 
                        tempw=readmemw_386(ds0,addr): if abrt Then exit Select 
                        AX=tempw 
                        cycles-=1
                        Exit Select 
                	case &h1A1  /'MOV EAX,(w)'/
                        addr=getword(): if abrt then exit Select 
                        templ=readmeml_386(ds0,addr): if abrt Then exit Select 
                        EAX=templ 
                        cycles-=1
                        Exit Select 
                	case &h2A1  /'MOV AX,(l)'/
                        addr=getlong(): if abrt then exit Select 
                        tempw=readmemw_386(ds0,addr): if abrt Then exit Select 
                        AX=tempw 
                        cycles-=1
                        Exit Select 
                	case &h3A1  /'MOV EAX,(l)'/
                        addr=getlong(): if abrt then exit Select 
                        templ=readmeml_386(ds0,addr): if abrt Then exit Select 
                        EAX=templ 
                        cycles-=1
                        Exit Select 
                	case &hA2, &h1A2  /'MOV (w),AL'/
                        addr=getword(): if abrt then exit Select 
                        writememb_386(ds0,addr,AL) 
                        cycles-=1
                        Exit Select 
                	case &h2A2, &h3A2  /'MOV (l),AL'/
                        addr=getlong(): if abrt then exit Select 
                        writememb_386(ds0,addr,AL) 
                        cycles-=1
                        Exit Select 
                	case &hA3  /'MOV (w),AX'/
                        addr=getword(): if abrt then exit Select 
                        writememw_386(ds0,addr,AX) 
                        cycles-=1
                        Exit Select 
                	case &h1A3  /'MOV (w),EAX'/
                        addr=getword(): if abrt then exit Select 
                        writememl_386(ds0,addr,EAX) 
                        cycles-=1
                        Exit Select 
                	case &h2A3  /'MOV (l),AX'/
                        addr=getlong(): if abrt then exit Select 
                        writememw_386(ds0,addr,AX) 
                        cycles-=1
                        Exit Select 
                	case &h3A3  /'MOV (l),EAX'/
                        addr=getlong(): if abrt then exit Select 
                        writememl_386(ds0,addr,EAX) 
                        cycles-=1
                        Exit Select 
                	case &hA4, &h1A4  /'MOVSB'/
                        temp=readmemb_386(ds0,SI):  if abrt then exit Select 
                        writememb_386(es0,DI,temp): if abrt then exit Select 
                        if (flags And D_FLAG) Then 
                             DI-=1: SI-=1   
                        else
                             DI+=1: SI+=1  
                        EndIf
                        cycles-=7 
                        Exit Select 
                	Case &h2A4, &h3A4  /'MOVSB'/
                        temp=readmemb_386(ds0,ESI):  if abrt then exit Select 
                        writememb_386(es0,EDI,temp): if abrt then exit Select 
                        if (flags And D_FLAG) Then 
                               EDI-=1: ESI-=1   
                        else
                               EDI+=1: ESI+=1  
                        EndIf
                        cycles-=7 
                        Exit Select 
                	case &hA5  /'MOVSW'/
                        tempw=readmemw_386(ds0,SI):  if abrt then exit Select 
                        writememw_386(es0,DI,tempw): if abrt then exit Select 
                        if (flags And D_FLAG) Then 
                                 DI-=2: SI-=2   
                        else
                                 DI+=2: SI+=2  
                        EndIf
                        cycles-=7 
                        Exit Select 
                	case &h2A5  /'MOVSW'/
                        tempw=readmemw_386(ds0,ESI):  if abrt then exit Select 
                        writememw_386(es0,EDI,tempw): if abrt then exit Select 
                        if (flags And D_FLAG) Then 
                                 EDI-=2: ESI-=2   
                        else
                                 EDI+=2: ESI+=2  
                        EndIf
                        cycles-=7 
                        Exit Select 
                	case &h1A5  /'MOVSL'/
                        templ=readmeml_386(ds0,SI):  if abrt then exit Select 
                        writememl_386(es0,DI,templ): if abrt then exit Select 
                        if (flags And D_FLAG) Then 
                                 DI-=4: SI-=4   
                        else
                                 DI+=4: SI+=4  
                        EndIf
                        cycles-=7 
                        Exit Select 
                	case &h3A5  /'MOVSL'/
                        templ=readmeml_386(ds0,ESI):  if abrt then exit Select 
                        writememl_386(es0,EDI,templ): if abrt then exit Select 
                        if (flags And D_FLAG) Then 
                                 EDI-=4: ESI-=4   
                        else
                                 EDI+=4: ESI+=4  
                        EndIf
                        cycles-=7 
                        Exit Select 
                	case &hA6, &h1A6  /'CMPSB'/
                        temp =readmemb_386(ds0,SI) 
                        temp2=readmemb_386(es0,DI) 
                        if abrt Then exit Select 
                        setsub8(temp,temp2) 
                        if (flags And D_FLAG) Then 
                                 DI-=1: SI-=1   
                        else
                                 DI+=1: SI+=1  
                        EndIf
                        cycles-=8
                        Exit Select 
                	case &h2A6, &h3A6  /'CMPSB'/
                        temp =readmemb_386(ds0,ESI) 
                        temp2=readmemb_386(es0,EDI) 
                        if abrt Then exit Select 
                        setsub8(temp,temp2) 
                        if (flags And D_FLAG) Then 
                                 EDI-=1: ESI-=1   
                        else
                                 EDI+=1: ESI+=1  
                        EndIf
                        cycles-=8
                        Exit Select 
                	case &hA7  /'CMPSW'/
                        tempw =readmemw_386(ds0,SI) 
                        tempw2=readmemw_386(es0,DI) 
                        if abrt Then exit Select 
                        setsub16(tempw,tempw2) 
                        if (flags And D_FLAG) Then 
                                 DI-=2: SI-=2   
                        else
                                 DI+=2: SI+=2  
                        EndIf
                        cycles-=8
                        Exit Select 
                	case &h1A7  /'CMPSL'/
                        templ =readmeml_386(ds0,SI) 
                        templ2=readmeml_386(es0,DI) 
                        if abrt Then exit Select 
                        setsub32(templ,templ2) 
                        if (flags And D_FLAG) Then 
                                 DI-=4: SI-=4   
                        else
                                 DI+=4: SI+=4  
                        EndIf
                        cycles-=8
                        Exit Select 
                	case &h2A7  /'CMPSW'/
                        tempw =readmemw_386(ds0,ESI) 
                        tempw2=readmemw_386(es0,EDI) 
                        if abrt Then exit Select 
                        setsub16(tempw,tempw2) 
                        if (flags And D_FLAG) Then 
                                 EDI-=2: ESI-=2   
                        else
                                 EDI+=2: ESI+=2  
                        EndIf
                        cycles-=8
                        Exit Select 
                	case &h3A7  /'CMPSL'/
                        templ =readmeml_386(ds0,ESI) 
                        templ2=readmeml_386(es0,EDI) 
                        if abrt Then exit Select 
                        setsub32(templ,templ2) 
                        if (flags And D_FLAG) Then 
                                 EDI-=4: ESI-=4   
                        else
                                 EDI+=4: ESI+=4  
                        EndIf
                        cycles-=8
                        Exit Select 
                	case &hA8, &h1A8, &h2A8, &h3A8  /'TEST AL,#8'/
                        temp=getbytef() 
                        setznp8(AL And temp) 
                        cycles -= timing_rr 
                        Exit Select 
                	case &hA9, &h2A9  /'TEST AX,#16'/
                        tempw=getwordf() 
                        setznp16(AX And tempw) 
                        cycles -= timing_rr 
                        Exit Select 
                	case &h1A9, &h3A9  /'TEST EAX,#32'/
                        templ=getlong(): if abrt Then exit Select 
                        setznp32(EAX And templ) 
                        cycles -= timing_rr 
                        Exit Select 
                	case &hAA, &h1AA  /'STOSB'/
                        writememb_386(es0,DI,AL): if abrt Then exit Select 
                        if (flags And D_FLAG) Then 
                                DI-=1 
                        else
                                DI+=1
                        EndIf
                        cycles-=4 
                        Exit Select 
                	case &h2AA, &h3AA  /'STOSB'/
                        writememb_386(es0,EDI,AL): if abrt Then exit Select 
                        if (flags And D_FLAG) Then 
                                EDI-=1 
                        else
                                EDI+=1
                        EndIf
                        cycles-=4 
                        Exit Select 
                	case &hAB  /'STOSW'/
                        writememw_386(es0,DI,AX): if abrt Then exit Select 
                        if (flags And D_FLAG) Then 
                                DI-=2 
                        else
                                DI+=2
                        EndIf
                        cycles-=4 
                        Exit Select 
                	case &h1AB  /'STOSL'/
                        writememl_386(es0,DI,EAX): if abrt Then exit Select 
                        if (flags And D_FLAG) Then 
                                DI-=4 
                        else
                                DI+=4
                        EndIf
                        cycles-=4 
                        Exit Select 
                	case &h2AB  /'STOSW'/
                        writememw_386(es0,EDI,AX): if abrt Then exit Select 
                        if (flags And D_FLAG) Then 
                                EDI-=2 
                        else
                                EDI+=2
                        EndIf
                        cycles-=4 
                        Exit Select 
                	case &h3AB  /'STOSL'/
                        writememl_386(es0,EDI,EAX):  if abrt then exit Select 
                        if (flags And D_FLAG) Then 
                                EDI-=4 
                        else
                                EDI+=4
                        EndIf
                        cycles-=4 
                        Exit Select 
                	case &hAC, &h1AC  /'LODSB'/
                        temp=readmemb_386(ds0,SI) 
                        if abrt Then exit Select 
                        AL=temp 
                        if (flags And D_FLAG) Then 
                                SI-=1 
                        else
                                SI+=1
                        EndIf
                        cycles-=5 
                        Exit Select 
                	case &h2AC, &h3AC  /'LODSB'/
                        temp=readmemb_386(ds0,ESI) 
                        if abrt Then exit Select 
                        AL=temp 
                        if (flags And D_FLAG) Then 
                                ESI-=1 
                        else
                                ESI+=1
                        EndIf
                        cycles-=5 
                        Exit Select 
                	case &hAD  /'LODSW'/
                        tempw=readmemw_386(ds0,SI) 
                        if abrt Then exit Select 
                        AX=tempw 
                        if (flags And D_FLAG) Then 
                                SI-=2 
                        else
                                SI+=2
                        EndIf
                        cycles-=5 
                        Exit Select 
                	case &h1AD  /'LODSL'/
                        templ=readmeml_386(ds0,SI) 
                        if abrt Then exit Select 
                        EAX=templ 
                        if (flags And D_FLAG) Then 
                                SI-=4 
                        else
                                SI+=4
                        EndIf
                        cycles-=5 
                        Exit Select 
                	case &h2AD  /'LODSW'/
                        tempw=readmemw_386(ds0,ESI) 
                        if abrt Then exit Select 
                        AX=tempw 
                        if (flags And D_FLAG) Then 
                                ESI-=2 
                        else
                                ESI+=2
                        EndIf
                        cycles-=5 
                        Exit Select 
                	case &h3AD  /'LODSL'/
                        templ=readmeml_386(ds0,ESI) 
                        if abrt Then exit Select 
                        EAX=templ 
                        if (flags And D_FLAG) Then 
                                ESI-=4 
                        else
                                ESI+=4
                        EndIf
                        cycles-=5 
                        Exit Select 
                	case &hAE, &h1AE  /'SCASB'/
                        temp=readmemb_386(es0,DI) 
                        if abrt Then exit Select 
                        setsub8(AL,temp) 
                        if (flags And D_FLAG) Then 
                                DI-=1 
                        else
                                DI+=1
                        EndIf
                        cycles-=7 
                        Exit Select 
                	case &h2AE, &h3AE  /'SCASB'/
                        temp=readmemb_386(es0,EDI) 
                        if abrt Then exit Select 
                        setsub8(AL,temp) 
                        if (flags And D_FLAG) Then 
                                EDI-=1 
                        else
                                EDI+=1
                        EndIf
                        cycles-=7 
                        Exit Select 
                	case &hAF  /'SCASW'/
                        tempw=readmemw_386(es0,DI) 
                        if abrt Then exit Select 
                        setsub16(AX,tempw) 
                        if (flags And D_FLAG) Then 
                                DI-=2 
                        else
                                DI+=2
                        EndIf
                        cycles-=7 
                        Exit Select 
                	case &h1AF  /'SCASL'/
                        templ=readmeml_386(es0,DI) 
                        if abrt Then exit Select 
                        setsub32(EAX,templ) 
                        if (flags And D_FLAG) Then 
                                DI-=4 
                        else
                                DI+=4
                        EndIf
                        cycles-=7 
                        Exit Select 
                	case &h2AF  /'SCASW'/
                        tempw=readmemw_386(es0,EDI) 
                        if abrt Then exit Select 
                        setsub16(AX,tempw) 
                        if (flags And D_FLAG) Then 
                                EDI-=2 
                        else
                                EDI+=2
                        EndIf
                        cycles-=7 
                        Exit Select 
                	case &h3AF  /'SCASL'/
                        templ=readmeml_386(es0,EDI) 
                        if abrt Then exit Select 
                        setsub32(EAX,templ) 
                        if (flags And D_FLAG) Then 
                                EDI-=4 
                        else
                                EDI+=4
                        EndIf
                        cycles-=7 
                        Exit Select 
                	case &hB0, &h1B0, &h2B0, &h3B0  /'MOV AL,#8'/
                        AL=getbytef() 
                        cycles -= timing_rr 
                        Exit Select 
                	case &hB1, &h1B1, &h2B1, &h3B1  /'MOV CL,#8'/
                        CL=getbytef() 
                        cycles -= timing_rr 
                        Exit Select 
                	case &hB2, &h1B2, &h2B2, &h3B2  /'MOV DL,#8'/
                        DL=getbytef() 
                        cycles -= timing_rr 
                        Exit Select 
                	case &hB3, &h1B3, &h2B3, &h3B3  /'MOV BL,#8'/
                        BL=getbytef() 
                        cycles -= timing_rr 
                        Exit Select 
                	case &hB4, &h1B4, &h2B4, &h3B4  /'MOV AH,#8'/
                        AH=getbytef() 
                        cycles -= timing_rr 
                        Exit Select 
                	case &hB5, &h1B5, &h2B5, &h3B5  /'MOV CH,#8'/
                        CH=getbytef() 
                        cycles -= timing_rr 
                        Exit Select 
                	case &hB6, &h1B6, &h2B6, &h3B6  /'MOV DH,#8'/
                        DH=getbytef() 
                        cycles -= timing_rr 
                        Exit Select 
                	case &hB7, &h1B7, &h2B7, &h3B7  /'MOV BH,#8'/
                        BH=getbytef() 
                        cycles -= timing_rr 
                        Exit Select 
                  /'MOV reg,#16'/
                	case &hB8, &hB9, &hBA, &hBB , _ 
                	 		&hBC, &hBD, &hBE, &hBF , _ 
                	 		&h2B8, &h2B9, &h2BA, &h2BB , _ 
                	 		&h2BC, &h2BD, &h2BE, &h2BF 
                        regs(opcode And 7).w=getwordf() 
                        cycles -= timing_rr 
                        temp=0
                        Exit Select 
                  /'MOV reg,#32'/
                	case &h1B8, &h1B9, &h1BA, &h1BB , _ 
                	 		&h1BC, &h1BD, &h1BE, &h1BF , _ 
                	 		&h3B8, &h3B9, &h3BA, &h3BB , _ 
                	 		&h3BC, &h3BD, &h3BE, &h3BF 
                        templ=getlong(): if abrt then exit Select 
                        regs(opcode And 7).l=templ 
                        cycles -= timing_rr 
                        Exit Select 
                        
                                      
                        
                        
                  ' instrucciones de rotaciones 8bits   
                	case &hC0, &h1C0, &h2C0, &h3C0 
                        fetchea32() : if abrt Then exit Select 
                        c=readmemb_386(cs0,pc): pc+=1 
                        temp=geteab(): if abrt then exit Select 
                        c = c And 31 
                        if (c=0) then exit Select 
                        Select Case As Const  (rmdat And &h38)
                        	case &h00  /'ROL b,CL'/
                                while (c>0)
                                       temp2=IIf((temp And &h80),1,0) 
                                       temp=(temp Shl 1) Or temp2 
                                       c-=1 
                                Wend
                                seteab(temp): if abrt then exit Select 
                                flags = flags And inv(C_FLAG Or V_FLAG) 
                                if temp2 Then flags = flags Or C_FLAG 
                                if ((flags And C_FLAG) Xor (temp Shr 7)) Then flags = flags Or V_FLAG 
                                cycles-=iif((modo=3),3,7) 
                                Exit Select 
                        	case &h08  /'ROR b,CL'/
                                while (c>0)
                                        temp2=temp And 1 
                                        temp=temp Shr 1 
                                        if temp2 Then temp = temp Or &h80 
                                        c-=1 
                                Wend
                                seteab(temp): if abrt Then exit Select 
                                flags = flags And inv(C_FLAG Or V_FLAG) 
                                if temp2 Then flags = flags Or C_FLAG 
                                if ((temp Xor (temp Shr 1)) And &h40) Then flags = flags Or V_FLAG 
                                cycles-=iif((modo=3),3,7) 
                                Exit Select 
                        	case &h10  /'RCL b,CL'/
                                temp2=flags And C_FLAG 
                                while (c>0)
                                        tempc=IIf((temp2),1,0) 
                                        temp2=temp And &h80 
                                        temp=(temp Shl 1) Or tempc 
                                        c-=1 
                                        cycles-=1 
                                Wend
                                seteab(temp): if abrt Then exit Select 
                                flags = flags And inv(C_FLAG Or V_FLAG) 
                                if temp2 Then flags = flags Or C_FLAG 
                                if ((flags And C_FLAG) Xor (temp Shr 7)) Then flags = flags Or V_FLAG 
                                cycles-=iif((modo=3),9,10) 
                                Exit Select 
                        	case &h18  /'RCR b,CL'/
                                temp2=flags And C_FLAG 
                                while (c>0)
                                        tempc=IIf((temp2),&h80,0) 
                                        temp2=temp And 1 
                                        temp=(temp Shr 1) Or tempc 
                                        c-=1 
                                        cycles-=1 
                                Wend
                                seteab(temp): if abrt Then exit Select 
                                flags = flags And inv(C_FLAG Or V_FLAG) 
                                if temp2 Then flags = flags Or C_FLAG 
                                if ((temp Xor (temp Shr 1)) And &h40) Then flags = flags Or V_FLAG 
                                cycles-=iif((modo=3),9,10) 
                                Exit Select 
                        	case &h20, &h30  /'SHL b,CL'/
                                seteab(temp Shl c): if abrt Then exit Select 
                                setznp8(temp Shl c) 
                                if ((temp Shl (c-1)) And &h80) Then flags = flags Or C_FLAG 
                                if (((temp Shl c) Xor (temp Shl (c-1))) And &h80) Then flags = flags Or V_FLAG 
                                cycles-=iif((modo=3),3,7) 
                                Exit Select 
                        	case &h28  /'SHR b,CL'/
                                seteab(temp Shr c): if abrt then Exit Select 
                                setznp8(temp Shr c) 
                                if ((temp Shr (c-1)) And 1) Then flags = flags Or C_FLAG 
                                if (c=1)  And  (temp And &h80) Then flags = flags Or V_FLAG 
                                cycles-=iif((modo=3),3,7) 
                                Exit Select 
                        	case &h38  /'SAR b,CL'/
                                tempc=((temp Shr (c-1)) And 1) 
                                while (c>0)
                                        temp = temp Shr 1 
                                        if (temp And &h40) Then temp = temp Or &h80 
                                        c-=1 
                                Wend
                                seteab(temp): if abrt Then exit Select 
                                setznp8(temp) 
                                if tempc Then flags = flags Or C_FLAG 
                                cycles-=iif((modo=3),3,7) 
                                Exit Select 
                       End Select
                       Exit Select 
                       
                  ' instrucciones de rotacion 16bits
                	case &hC1, &h2C1 
                        fetchea32() : if abrt Then exit Select 
                        c=readmemb_386(cs0,pc) And 31: pc+=1 
                        tempw=geteaw(): if abrt Then exit Select 
                        if (c=0) then exit Select 
                        Select Case As Const  (rmdat And &h38)
                        	case &h00  /'ROL w,CL'/
                                while (c>0)
                                        temp=IIf((tempw And &h8000),1,0) 
                                        tempw=(tempw Shl 1) Or temp 
                                        c-=1 
                                Wend
                                seteaw(tempw):  if abrt then exit Select 
                                flags = flags And inv(C_FLAG Or V_FLAG) 
                                if (temp) Then flags = flags Or C_FLAG 
                                if ((flags And C_FLAG) Xor (tempw Shr 15)) Then flags = flags Or V_FLAG 
                                cycles-=iif((modo=3),3,7) 
                                Exit Select 
                        	case &h08  /'ROR w,CL'/
                                while (c>0)
                                        tempw2=IIf((tempw And 1),&h8000,0) 
                                        tempw=(tempw Shr 1) Or tempw2 
                                        c-=1 
                                Wend
                                seteaw(tempw):  if abrt then exit Select 
                                flags = flags And inv(C_FLAG Or V_FLAG) 
                                if (tempw2) Then flags = flags Or C_FLAG 
                                if ((tempw Xor (tempw Shr 1)) And &h4000) Then flags = flags Or V_FLAG 
                                cycles-=iif((modo=3),3,7) 
                                Exit Select 
                        	case &h10  /'RCL w,CL'/
                                temp2=flags And C_FLAG 
                                while (c>0)
                                        tempc=IIf((temp2),1,0) 
                                        temp2=(tempw Shr 15) 
                                        tempw=(tempw Shl 1) Or tempc 
                                        c-=1 
                                        cycles-=1 
                                Wend
                                seteaw(tempw):  if abrt then exit Select 
                                flags = flags And inv(C_FLAG Or V_FLAG) 
                                if temp2 Then flags = flags Or C_FLAG 
                                if ((flags And C_FLAG) Xor (tempw Shr 15)) Then flags = flags Or V_FLAG 
                                cycles-=iif((modo=3),9,10) 
                                Exit Select 
                        	case &h18  /'RCR w,CL'/
                                temp2=flags And C_FLAG 
                                while (c>0)
                                        tempc=IIf((temp2),&h8000,0) 
                                        temp2=tempw And 1 
                                        tempw=(tempw Shr 1) Or tempc 
                                        c-=1 
                                        cycles-=1 
                                Wend
                                seteaw(tempw):  if abrt then exit Select 
                                flags = flags And inv(C_FLAG Or V_FLAG) 
                                if temp2 Then flags = flags Or C_FLAG 
                                if ((tempw Xor (tempw Shr 1)) And &h4000) Then flags = flags Or V_FLAG 
                                cycles-=iif((modo=3),9,10) 
                                Exit Select 
                        	case &h20, &h30  /'SHL w,CL'/
                                seteaw(tempw Shl c): if abrt then exit Select 
                                setznp16(tempw Shl c) 
                                if ((tempw Shl (c-1)) And &h8000) Then flags = flags Or C_FLAG 
                                if (((tempw Shl c) Xor (tempw Shl (c-1))) And &h8000) Then flags = flags Or V_FLAG 
                                cycles-=iif((modo=3),3,7) 
                                Exit Select 
                        	case &h28       /'SHR w,CL'/
                                seteaw(tempw Shr c): if abrt then exit Select 
                                setznp16(tempw Shr c) 
                                if ((tempw Shr (c-1)) And 1) Then flags = flags Or C_FLAG 
                                if (c=1)  And  ((tempw And &h8000)<>0) Then flags = flags Or V_FLAG 
                                cycles-=iif((modo=3),3,7) 
                                Exit Select 
                        	case &h38      /'SAR w,CL'/
                                tempw2=tempw And &h8000 
                                tempc=(tempw Shr (c-1)) And 1 
                                while (c>0)
                                        tempw=(tempw Shr 1) Or tempw2 
                                        c-=1 
                                Wend
                                seteaw(tempw):  if abrt then exit Select 
                                setznp16(tempw) 
                                if tempc Then flags = flags Or C_FLAG 
                                cycles-=iif((modo=3),3,7) 
                                Exit Select 
                        End Select
                       Exit Select 
                       
                  ' instrucciones de rotacion 32bits
                	case &h1C1, &h3C1 
                        fetchea32() : if abrt Then exit Select 
                        c=readmemb_386(cs0,pc): pc+=1 
                        c = c And 31 
                        templ=geteal(): if abrt Then exit Select 
                        if c=0 then exit Select 
                        Select Case As Const  (rmdat And &h38)
                        	case &h00  /'ROL l,CL'/
                                while (c>0)
                                        temp=IIf((templ And &h80000000),1,0 )
                                        templ=(templ Shl 1) Or temp 
                                        c-=1 
                                Wend
                                seteal(templ):  if abrt then exit Select 
                                flags = flags And inv(C_FLAG Or V_FLAG) 
                                if (temp) Then flags = flags Or C_FLAG 
                                if ((flags And C_FLAG) Xor (templ Shr 31)) Then flags = flags Or V_FLAG 
                                cycles-=iif((modo=3),9,10) 
                                Exit Select 
                        	case &h08  /'ROR l,CL'/
                                while (c>0)
                                        templ2=IIf((templ And 1),&h80000000,0 )
                                        templ=(templ Shr 1) Or templ2 
                                        c-=1 
                                Wend
                                seteal(templ):  if abrt then exit Select 
                                flags = flags And inv(C_FLAG Or V_FLAG) 
                                if (templ2) Then flags = flags Or C_FLAG 
                                if ((templ Xor (templ Shr 1)) And &h40000000) Then flags = flags Or V_FLAG 
                                cycles-=iif((modo=3),9,10) 
                                Exit Select 
                        	case &h10  /'RCL l,CL'/
                                temp2=flags And C_FLAG 
                                while (c>0)
                                        tempc=IIf((flags And C_FLAG),1,0) 
                                        temp2=templ Shr 31 
                                        templ=(templ Shl 1) Or tempc 
                                        c-=1 
                                        cycles-=1 
                                Wend
                                seteal(templ):  if abrt then exit Select 
                                flags = flags And inv(C_FLAG Or V_FLAG) 
                                if temp2 Then flags = flags Or C_FLAG 
                                if ((flags And C_FLAG) Xor (templ Shr 31)) Then flags = flags Or V_FLAG 
                                cycles-=iif((modo=3),9,10) 
                                Exit Select 
                        	case &h18  /'RCR l,CL'/
                                temp2=flags And C_FLAG 
                                while (c>0)
                                        tempc=IIf((temp2),&h80000000,0) 
                                        temp2=templ And 1 
                                        templ=(templ Shr 1) Or tempc 
                                        c-=1 
                                        cycles-=1 
                                Wend
                                seteal(templ):  if abrt then exit Select 
                                flags = flags And inv(C_FLAG Or V_FLAG) 
                                if temp2 Then flags = flags Or C_FLAG 
                                if ((templ Xor (templ Shr 1)) And &h40000000) Then flags = flags Or V_FLAG 
                                cycles-=iif((modo=3),9,10) 
                                Exit Select 
                        	case &h20, &h30  /'SHL l,CL'/
                                seteal(templ Shl c): if abrt then exit Select 
                                setznp32(templ Shl c) 
                                if ((templ Shl (c-1)) And &h80000000) Then flags = flags Or C_FLAG 
                                if (((templ Shl c) Xor (templ Shl (c-1))) And &h80000000) Then flags = flags Or V_FLAG 
                                cycles-=iif((modo=3),3,7) 
                                Exit Select 
                        	case &h28  /'SHR l,CL'/
                                seteal(templ Shr c): if abrt Then exit Select 
                                setznp32(templ Shr c) 
                                if ((templ Shr (c-1)) And 1) Then flags = flags Or C_FLAG 
                                if (c=1) And ((templ And &h80000000)<>0) Then flags = flags Or V_FLAG 
                                cycles-=iif((modo=3),3,7) 
                                Exit Select 
                        	case &h38  /'SAR l,CL'/
                                templ2=templ And &h80000000 
                                tempc=(templ Shr (c-1)) And 1 
                                while (c>0)
                                        templ=(templ Shr 1) Or templ2 
                                        c-=1 
                                Wend
                                seteal(templ):  if abrt then exit Select 
                                setznp32(templ) 
                                if tempc Then flags = flags Or C_FLAG 
                                cycles-=iif((modo=3),3,7) 
                                Exit Select 
                       End Select
                       Exit Select 
                       
                       
                       
                       
                       
                       
                	case &hC2, &h2C2  /'RET'/
                        tempw=getword() 
                        if ssegs Then ss0=oldss 
                        if stack32 Then 
                                tempw2=readmemw_386(ss0,ESP): if abrt then exit Select 
                                ESP+=2+tempw 
                        else
                                tempw2=readmemw_386(ss0,SP): if abrt then exit Select 
                                SP+=2+tempw 
                        EndIf
                        pc=tempw2 
                        cycles-=5 
                        Exit Select 
                	case &h1C2, &h3C2  /'RET'/
                        tempw=getword() 
                        if ssegs Then ss0=oldss 
                        if stack32 Then 
                                templ=readmeml_386(ss0,ESP): if abrt then exit Select 
                                ESP+=4+tempw 
                        else
                                templ=readmeml_386(ss0,SP):  if abrt then exit Select 
                                SP+=4+tempw 
                        EndIf
                        pc=templ 
                        cycles-=5
                        Exit Select 
                	case &hC3, &h2C3  /'RET'/
                        if ssegs Then ss0=oldss 
                        if stack32 Then 
                                tempw=readmemw_386(ss0,ESP): if abrt then exit Select 
                                ESP+=2 
                        else
                                tempw=readmemw_386(ss0,SP):  if abrt then exit Select 
                                SP+=2 
                        EndIf
                        pc=tempw 
                        cycles-=5
                        Exit Select 
                	case &h1C3, &h3C3  /'RET'/
                        if ssegs Then ss0=oldss 
                        if stack32 Then 
                                templ=readmeml_386(ss0,ESP): if abrt then exit Select 
                                ESP+=4 
                        else
                                templ=readmeml_386(ss0,SP):  if abrt then exit Select 
                                SP+=4 
                        EndIf
                        pc=templ 
                        cycles-=5
                        Exit Select 
                	case &hC4, &h2C4  /'LES'/
                        fetchea32() : if abrt Then exit Select 
                        tempw2=readmemw_386(easeg,eaaddr) 
                        tempw=readmemw_386(easeg,eaaddr+2) 
                        if abrt Then exit Select 
                        loadseg(tempw, _es) 
                        if abrt Then exit Select 
                        regs(reg).w=tempw2 
                        cycles-=7 
                        Exit Select 
                	case &h1C4, &h3C4  /'LES'/
                        fetchea32() : if abrt Then exit Select 
                        templ=readmeml_386(easeg,eaaddr) 
                        tempw=readmemw_386(easeg,eaaddr+4) 
                        if abrt Then exit Select 
                        loadseg(tempw, _es) 
                        if abrt Then exit Select 
                        regs(reg).l=templ 
                        cycles-=7 
                        Exit Select 
                	case &hC5, &h2C5  /'LDS'/
                        fetchea32() : if abrt Then exit Select 
                        tempw2=readmemw_386(easeg,eaaddr) 
                        tempw=readmemw_386(easeg,eaaddr+2) 
                        if abrt Then exit Select 
                        loadseg(tempw, _ds) 
                        if abrt Then exit Select 
                        if ssegs Then oldds=ds0 
                        regs(reg).w=tempw2 
                        cycles-=7 
                        Exit Select 
                	case &h1C5, &h3C5  /'LDS'/
                        fetchea32() : if abrt Then exit Select 
                        templ=readmeml_386(easeg,eaaddr) 
                        tempw=readmemw_386(easeg,eaaddr+4) 
                        if abrt Then exit Select 
                        loadseg(tempw, _ds) 
                        if abrt Then exit Select 
                        if ssegs Then oldds=ds0 
                        regs(reg).l=templ 
                        cycles-=7 
                        Exit Select 
                	case &hC6, &h1C6, &h2C6, &h3C6  /'MOV b,#8'/
                        fetchea32() : if abrt Then exit Select 
                        temp=readmemb_386(cs0,pc): pc+=1: if abrt Then exit Select 
                        seteab(temp) 
                        cycles -= timing_rr 
                        Exit Select 
                	case &hC7, &h2C7  /'MOV w,#16'/
                        fetchea32() : if abrt Then exit Select 
                        tempw=getword(): if abrt then exit Select
                        seteaw(tempw) 
                        cycles -= timing_rr 
                        Exit Select 
                	case &h1C7, &h3C7  /'MOV l,#32'/
                        fetchea32() : if abrt Then exit Select 
                        templ=getlong(): if abrt then exit Select 
                        seteal(templ) 
                        cycles -= timing_rr 
                        Exit Select 
                	case &hC8, &h2C8  /'ENTER'/
                        tempw2=getword() 
                        tempi=readmemb_386(cs0,pc): pc+=1 
                        templ=EBP 
                        if stack32 Then 
                                 writememw_386(ss0,(ESP-2),BP)
                                 if abrt Then exit Select
                                 ESP-=2
                        else
                                 writememw_386(ss0,((SP-2) And &hFFFF),BP)
                                 if abrt Then exit Select
                                 SP-=2
                        EndIf
                        templ2=ESP 
                        if (tempi<>0) Then 
                        	temp-=1
                        	Print #5,"OJO con --TEMPI-1, revisar por que no se si resta antes o despues"
                        	' segun mi investigacion (con unas pruebas en C), se resta antes de entrar y se sale antes de llegar al "0"
                                while (tempi>0) ' OJO, original es --TEMPI, o sea, resta antes de entrar?????
                                			 temp-=1
                                        EBP-=2 
                                        if stack32 Then 
                                             tempw=readmemw_386(ss0,EBP) 
                                        else
                                             tempw=readmemw_386(ss0,BP)
                                        EndIf
                                        if abrt Then 
                                             ESP=templ2: EBP=templ
                                             Exit Select 
                                        EndIf
                                        if stack32 Then 
                                             writememw_386(ss0,(ESP-2),tempw): ESP-=2   
                                        else
                                             writememw_386(ss0,((SP-2) And &hFFFF),tempw): SP-=2  
                                        EndIf
                                        if abrt Then 
                                             ESP=templ2: EBP=templ
                                             Exit Select 
                                        EndIf
                                        cycles-=3
                                       'tempi-=1
                                Wend
                                if stack32 Then 
                                      writememw_386(ss0,(ESP-2),templ2): ESP-=2   
                                else
                                      writememw_386(ss0,((SP-2) And &hFFFF),templ2): SP-=2 
                                EndIf
                                if abrt Then 
                                     ESP=templ2: EBP=templ
                                     Exit Select 
                                EndIf
                                cycles-=3 
                        EndIf
                        BP = templ2 
                        if stack32 Then 
                             ESP-=tempw2 
                        else
                             SP-=tempw2
                        EndIf
                        cycles-=14
                        Exit Select 
                	case &h1C8, &h3C8  /'ENTER'/
                        tempw=getword() 
                        tempi=readmemb_386(cs0,pc): pc+=1 
                        if stack32 Then 
                                 writememl_386(ss0,(ESP-4),EBP)
                                 if abrt Then exit Select
                                 ESP-=4
                        else
                                 writememl_386(ss0,((SP-4) And &hFFFF),EBP)
                                 if abrt Then exit Select
                                 SP-=4
                        EndIf
                        templ2=ESP
                        templ3=EBP 
                        if (tempi<>0) Then 
                        	temp-=1
                        	    Print #5,"OJO con --TEMPI-2, revisar por que no se si resta antes o despues"
                        	    ' segun mi investigacion (con unas pruebas en C), se resta antes de entrar y se sale antes de llegar al "0"
                                while (tempi>0) ' OJO, original es --TEMPI, o sea, resta antes de entrar?????
                                			 tempi-=1
                                        EBP-=4 
                                        if stack32 Then 
                                              templ=readmeml_386(ss0,EBP) 
                                        else
                                              templ=readmeml_386(ss0,BP)
                                        EndIf
                                        if abrt Then 
                                                 ESP=templ2: EBP=templ3
                                                 Exit Select  
                                        EndIf
                                        if stack32 Then 
                                               writememl_386(ss0,(ESP-4),templ):        ESP-=4   
                                        else
                                               writememl_386(ss0,((SP-4) And &hFFFF),templ): SP-=4  
                                        EndIf
                                        if abrt Then 
                                                 ESP=templ2: EBP=templ3
                                                 Exit Select 
                                        EndIf
                                        cycles-=3
                                        'tempi-=1
                                Wend
                                if stack32 Then 
                                       writememl_386(ss0,(ESP-4),templ2):        ESP-=4   
                                else
                                       writememl_386(ss0,((SP-4) And &hFFFF),templ2): SP-=4  
                                EndIf
                                if abrt Then 
                                         ESP=templ2: EBP=templ3
                                         Exit Select  
                                EndIf
                                cycles-=3
                        EndIf
                        EBP=templ2 
                        if stack32 Then 
                              ESP-=tempw 
                        else
                              SP-=tempw
                        EndIf
                        cycles-=14
                        Exit Select 
                	case &hC9, &h2C9  /'LEAVE'/
                        templ=ESP 
                        SP=BP 
                        if stack32 Then 
                               tempw=readmemw_386(ss0,ESP): ESP+=2   
                        else
                               tempw=readmemw_386(ss0,SP):   SP+=2  
                        EndIf
                        if abrt Then 
                                 ESP=templ
                                 Exit Select   
                        EndIf
                        BP=tempw 
                        cycles-=4 
                        Exit Select 
                	case &h3C9, &h1C9  /'LEAVE'/
                        templ=ESP 
                        ESP=EBP 
                        if stack32 Then 
                              templ2=readmeml_386(ss0,ESP): ESP+=4   
                        else
                              templ2=readmeml_386(ss0,SP):  SP+=4   
                        EndIf
                        if abrt Then 
                                 ESP=templ
                                 Exit Select   
                        EndIf
                        EBP=templ2 
                        cycles-=4 
                        Exit Select 
                	case &hCA, &h2CA  /'RETF'/
                        tempw=getword() 
                        if (modoprotegido=1) And ((eflags And VM_FLAG)=0) Then 
                                pmoderetf(0,tempw) 
                                Exit Select 
                        EndIf
                        tempw2=CPL 
                        if ssegs Then ss0=oldss 
                        oxpc=pc 
                        if stack32 Then 
                                pc=readmemw_386(ss0,ESP) 
                                loadcs(readmemw_386(ss0,ESP+2)) 
                        else
                                pc=readmemw_386(ss0,SP) 
                                loadcs(readmemw_386(ss0,SP+2)) 
                        EndIf
                        if abrt Then exit Select 
                        if stack32 Then 
                               ESP+=4+tempw 
                        else
                               SP+=4+tempw
                        EndIf
                        cycles-=13
                        Exit Select 
                	case &h1CA, &h3CA  /'RETF'/
                        tempw=getword() 
                        if (modoprotegido=1) And ((eflags And VM_FLAG)=0) Then 
                                pmoderetf(1,tempw) 
                                Exit Select
                        EndIf
                        tempw2=CPL 
                        if ssegs Then ss0=oldss 
                        oxpc=pc 
                        if stack32 Then 
                                pc=readmeml_386(ss0,ESP) 
                                loadcs(readmeml_386(ss0,ESP+4) And &hFFFF) 
                        else
                                pc=readmeml_386(ss0,SP) 
                                loadcs(readmeml_386(ss0,SP+4) And &hFFFF) 
                        EndIf
                        if abrt Then exit Select 
                        if stack32 Then 
                               ESP+=8+tempw 
                        else
                               SP+=8+tempw
                        EndIf
                        cycles-=13
                        Exit Select 
                	case &hCB, &h2CB  /'RETF'/
                        if (modoprotegido=1) And ((eflags And VM_FLAG)=0) Then 
                                pmoderetf(0,0) 
                                Exit Select
                        EndIf
                        tempw2=CPL 
                        if ssegs Then ss0=oldss 
                        oxpc=pc 
                        if stack32 Then 
                                pc=readmemw_386(ss0,ESP) 
                                loadcs(readmemw_386(ss0,ESP+2)) 
                        else
                                pc=readmemw_386(ss0,SP) 
                                loadcs(readmemw_386(ss0,SP+2)) 
                        EndIf
                        if abrt Then exit Select 
                        if stack32 Then 
                               ESP+=4 
                        else
                               SP+=4
                        EndIf
                        cycles-=13
                        Exit Select 
                	case &h1CB, &h3CB  /'RETF'/
                        if (modoprotegido=1) And ((eflags And VM_FLAG)=0) Then 
                                pmoderetf(1,0) 
                                Exit Select 
                        EndIf
                        tempw2=CPL 
                        if ssegs Then ss0=oldss 
                        oxpc=pc 
                        if stack32 Then 
                                pc=readmeml_386(ss0,ESP) 
                                loadcs(readmemw_386(ss0,ESP+4)) 
                        else
                                pc=readmeml_386(ss0,SP) 
                                loadcs(readmemw_386(ss0,SP+4)) 
                        EndIf
                        if abrt Then exit Select 
                        if stack32 Then 
                               ESP+=8 
                        else
                               SP+=8
                        EndIf
                        if (modoprotegido=1) And (CPL>tempw2) Then 
                                if stack32 Then 
                                        templ=readmeml_386(ss0,ESP) 
                                        loadseg(readmeml_386(ss0,ESP+4), _ss) 
                                        ESP=templ 
                                else
                                        templ=readmeml_386(ss0,SP) 
                                        loadseg(readmeml_386(ss0,SP+4), _ss) 
                                        ESP=templ 
                                EndIf
                        EndIf
                        cycles-=13
                        Exit Select 
                	case &hCC, &h1CC, &h2CC, &h3CC  /'INT 3'/
                        if modoprotegido Then 
                                pmodeint(3,1) 
                                cycles-=44 
                        else
                                if ssegs Then ss0=oldss 
                                if stack32 Then 
                                        writememw_386(ss0,ESP-2,flags) 
                                        writememw_386(ss0,ESP-4,CS1) 
                                        writememw_386(ss0,ESP-6,pc) 
                                        ESP-=6 
                                else
                                        writememw_386(ss0,((SP-2) And &hFFFF),flags) 
                                        writememw_386(ss0,((SP-4) And &hFFFF),CS1) 
                                        writememw_386(ss0,((SP-6) And &hFFFF),pc) 
                                        SP-=6 
                                EndIf
                                addr=3 Shl 2 
                                flags = flags And inv(T_FLAG) 
                                oxpc=pc 
                                pc=readmemw_386(0,addr) 
                                loadcs(readmemw_386(0,addr+2)) 
                                cycles-=26
                        EndIf
                        cycles-=23 
                        Exit Select 
                        
                        
                	case &hCD, &h1CD, &h2CD, &h3CD  /'INT'/
                        if ((cr0  And 1)<>0) And ((eflags And VM_FLAG)<>0) And (IOPL <> 3) Then 
                                x86gpf(0) 
                                Exit Select
                        EndIf        
                        lastpc=pc 
                        lastcs=CS1 
                        temp=readmemb_386(cs0,pc): pc+=1 

                   ' GOTO para la INS &hCE (INTO) (la posterior a esta)
                   intrt: 
                       if modoprotegido Then 
                               pmodeint(temp,1) 
                               cycles-=44
                       Else
                               if ssegs Then ss0=oldss 
                               if stack32 Then 
                                       writememw_386(ss0,ESP-2,flags) 
                                       writememw_386(ss0,ESP-4,CS1) 
                                       writememw_386(ss0,ESP-6,pc) 
                                       ESP-=6 
                               else
                                       writememw_386(ss0,((SP-2) And &hFFFF),flags) 
                                       writememw_386(ss0,((SP-4) And &hFFFF),CS1) 
                                       writememw_386(ss0,((SP-6) And &hFFFF),pc) 
                                       SP-=6 
                               EndIf
                               addr=temp Shl 2 
                               flags = flags And inv(T_FLAG) 
                               oxpc=pc 
                               pc=readmemw_386(0,addr) 
                               loadcs(readmemw_386(0,addr+2)) 
                               cycles-=30
                       EndIf
                       Exit Select 
                        
                        
                	case &hCE  /'INTO'/
                        if (flags And V_FLAG) Then 
                                temp=4 
                                goto intrt ' salta a INT (ins. &hCD) (la anterior a esta)
                        EndIf
                        cycles-=3 
                        Exit Select 
                        
                        
                	case &hCF, &h2CF  /'IRET'/
                        if ((cr0 And 1)<>0) And ((eflags And VM_FLAG)<>0) And (IOPL <> 3) Then 
                                x86gpf(0) 
                                Exit Select 
                        EndIf
                        if ssegs Then ss0=oldss 
                        if modoprotegido Then 
                        	optype=0
                                optype=IRET 
                                pmodeiret(0) 
                                optype=0 
                        else
                                tempw=CS1 
                                tempw2=pc 
                                inint=0 
                                oxpc=pc 
                                if stack32 Then 
                                        pc=readmemw_386(ss0,ESP) 
                                        loadcs(readmemw_386(ss0,ESP+2)) 
                                else
                                        pc=readmemw_386(ss0,SP) 
                                        loadcs(readmemw_386(ss0,((SP+2) And &hFFFF))) 
                                EndIf
                                if stack32 Then 
                                        flags=(readmemw_386(ss0,ESP+4) And &hFFD5) Or 2 
                                        ESP+=6 
                                else
                                        flags=(readmemw_386(ss0,((SP+4) And &hFFFF)) And &hFFD5) Or 2 
                                        SP+=6 
                                EndIf
                        EndIf
                        cycles-=15 
                        Exit Select 
                	case &h1CF, &h3CF  /'IRETD'/
                        if ((cr0 And 1)<>0) And ((eflags And VM_FLAG)<>0) And (IOPL <> 3) Then 
                                x86gpf(0) 
                                Exit Select 
                        EndIf
                        if ssegs Then ss0=oldss 
                        if modoprotegido Then 
                                optype=IRET 
                                pmodeiret(1) 
                                optype=0 
                        else
                                tempw=CS1 
                                tempw2=pc 
                                inint=0 
                                oxpc=pc 
                                if stack32 Then 
                                        pc=readmeml_386(ss0,ESP) 
                                        templ=readmeml_386(ss0,ESP+4) 
                                else
                                        pc=readmeml_386(ss0,SP) 
                                        templ=readmeml_386(ss0,((SP+4) And &hFFFF)) 
                                EndIf
                                if stack32 Then 
                                        flags=(readmemw_386(ss0,ESP+8) And &hFFD5) Or 2 
                                        eflags=readmemw_386(ss0,ESP+10) 
                                        ESP+=12 
                                else
                                        flags=(readmemw_386(ss0,(SP+8) And &hFFFF) And &hFFD5) Or 2 
                                        eflags=readmemw_386(ss0,(SP+10) And &hFFFF) 
                                        SP+=12 
                                EndIf
                                loadcs(templ) 
                        EndIf
                        cycles-=15 
                        Exit Select 
                        
                        
                  ' ROTACIONES
                	case &hD0, &h1D0, &h2D0, &h3D0 
                        fetchea32() : if abrt Then exit Select 
                        temp=geteab(): if abrt then exit Select 
                        Select Case As Const  (rmdat And &h38)
                        	case &h00  /'ROL b,1'/
                                seteab((temp Shl 1) Or IIf((temp And &h80),1,0)): if abrt then exit Select 
                                if (temp And &h80) Then 
                                      flags = flags Or C_FLAG 
                                else
                                      flags = flags And inv(C_FLAG)
                                EndIf
                                temp Shl =1 
                                if  (flags And C_FLAG) Then temp = temp Or 1 
                                if ((flags And C_FLAG) Xor (temp Shr 7)) Then 
                                        flags = flags Or V_FLAG 
                                else
                                        flags = flags And inv(V_FLAG)
                                EndIf
                                cycles-=iif((modo=3),3,7) 
                                Exit Select 
                        	case &h08  /'ROR b,1'/
                                seteab((temp Shr 1) Or IIf((temp And 1),&h80,0)): if abrt then exit Select 
                                if (temp And 1) Then 
                                        flags = flags Or C_FLAG 
                                else
                                        flags = flags And inv(C_FLAG)
                                EndIf
                                temp = temp Shr 1 
                                if (flags And C_FLAG) Then temp = temp Or &h80 
                                if ((temp Xor (temp Shr 1)) And &h40) Then 
                                        flags = flags Or V_FLAG 
                                else
                                        flags = flags And inv(V_FLAG)
                                EndIf
                                cycles-=iif((modo=3),3,7) 
                                Exit Select 
                        	case &h10  /'RCL b,1'/
                                temp2=flags And C_FLAG 
                                seteab((temp Shl 1) Or temp2): if abrt then exit Select 
                                if (temp And &h80) Then 
                                        flags = flags Or C_FLAG 
                                else
                                        flags = flags And inv(C_FLAG)
                                EndIf
                                temp = temp Shl 1
                                if temp2 Then temp = temp Or 1 
                                if ((flags And C_FLAG) Xor (temp Shr 7)) Then 
                                        flags = flags Or V_FLAG 
                                else
                                        flags = flags And inv(V_FLAG)
                                EndIf
                                cycles-=iif((modo=3),3,7) 
                                Exit Select 
                        	case &h18  /'RCR b,1'/
                                temp2=flags And C_FLAG 
                                seteab((temp Shr 1) Or iif(temp2,&h80,0)): if abrt then exit Select 
                                if (temp And 1) Then 
                                        flags = flags Or C_FLAG 
                                else
                                        flags = flags And inv(C_FLAG)
                                EndIf
                                temp = temp Shr 1 
                                if temp2 Then temp = temp Or &h80 
                                if ((temp Xor (temp Shr 1)) And &h40) Then 
                                        flags = flags Or V_FLAG 
                                else
                                        flags = flags And inv(V_FLAG)
                                EndIf
                                cycles-=iif((modo=3),3,7) 
                                Exit Select 
                        	case &h20, &h30  /'SHL b,1'/
                                seteab(temp Shl 1): if abrt then exit Select 
                                setznp8(temp Shl 1) 
                                if (temp And &h80) Then flags = flags Or C_FLAG 
                                if ((temp Xor (temp Shl 1)) And &h80) Then flags = flags Or V_FLAG 
                                cycles-=iif((modo=3),3,7) 
                                Exit Select 
                        	case &h28  /'SHR b,1'/
                                seteab(temp Shr 1): if abrt then exit Select 
                                setznp8(temp Shr 1) 
                                if (temp And 1) Then flags = flags Or C_FLAG 
                                if (temp And &h80) Then flags = flags Or V_FLAG 
                                cycles-=iif((modo=3),3,7) 
                                Exit Select 
                        	case &h38  /'SAR b,1'/
                                seteab((temp Shr 1) Or (temp And &h80)): if abrt then exit Select 
                                setznp8((temp Shr 1) Or (temp And &h80)) 
                                if (temp And 1) Then flags = flags Or C_FLAG 
                                cycles-=iif((modo=3),3,7) 
                                Exit Select 
                       End Select
                       Exit Select 
                	case &hD1, &h2D1 
                        fetchea32() : if abrt Then exit Select 
                        tempw=geteaw(): if abrt then exit Select 
                        Select Case As Const  (rmdat And &h38)
                        	case &h00  /'ROL w,1'/
                                seteaw((tempw Shl 1) Or (tempw Shr 15)): if abrt then exit Select 
                                if (tempw And &h8000) Then 
                                        flags = flags Or C_FLAG 
                                else
                                        flags = flags And inv(C_FLAG)
                                EndIf
                                tempw = tempw Shl 1
                                if (flags And C_FLAG) Then tempw = tempw Or 1 
                                if ((flags And C_FLAG) Xor (tempw Shr 15)) Then 
                                        flags = flags Or V_FLAG 
                                else
                                        flags = flags And inv(V_FLAG)
                                EndIf
                                cycles-=iif((modo=3),3,7) 
                                Exit Select 
                        	case &h08  /'ROR w,1'/
                                seteaw((tempw Shr 1) Or (tempw Shl 15)): if abrt then exit Select 
                                if (tempw And 1) Then 
                                        flags = flags Or C_FLAG 
                                else
                                        flags = flags And inv(C_FLAG)
                                EndIf
                                tempw = tempw Shr 1 
                                if (flags And C_FLAG) Then tempw = tempw Or &h8000 
                                if ((tempw Xor (tempw Shr 1)) And &h4000) Then 
                                        flags = flags Or V_FLAG 
                                else
                                        flags = flags And inv(V_FLAG)
                                EndIf
                                cycles-=iif((modo=3),3,7) 
                                Exit Select 
                        	case &h10  /'RCL w,1'/
                                temp2=flags And C_FLAG 
                                seteaw((tempw Shl 1) Or temp2): if abrt Then exit Select 
                                if (tempw And &h8000) Then 
                                        flags = flags Or C_FLAG 
                                else
                                        flags = flags And inv(C_FLAG)
                                EndIf
                                tempw = tempw Shl 1 
                                if temp2 Then tempw = tempw Or 1 
                                if ((flags And C_FLAG) Xor (tempw Shr 15)) Then 
                                        flags = flags Or V_FLAG 
                                else
                                        flags = flags And inv(V_FLAG)
                                EndIf
                                cycles-=iif((modo=3),3,7) 
                                Exit Select 
                        	case &h18  /'RCR w,1'/
                                temp2=flags And C_FLAG 
                                seteaw((tempw Shr 1) Or iif(temp2,&h8000,0)): if abrt then Exit Select 
                                if (tempw And 1) Then 
                                        flags = flags Or C_FLAG 
                                else
                                         flags = flags And inv(C_FLAG)
                                EndIf
                                tempw = tempw Shr 1 
                                if temp2 Then tempw = tempw Or &h8000 
                                if ((tempw Xor (tempw Shr 1)) And &h4000) Then 
                                        flags = flags Or V_FLAG 
                                else
                                        flags = flags And inv(V_FLAG)
                                EndIf
                                cycles-=iif((modo=3),3,7) 
                                Exit Select 
                        	case &h20, &h30  /'SHL w,1'/
                                seteaw(tempw Shl 1): if abrt Then exit Select 
                                setznp16(tempw Shl 1) 
                                if (tempw And &h8000) Then flags = flags Or C_FLAG 
                                if ((tempw Xor (tempw Shl 1)) And &h8000) Then flags = flags Or V_FLAG 
                                cycles-=iif((modo=3),3,7) 
                                Exit Select 
                        	case &h28  /'SHR w,1'/
                                seteaw(tempw Shr 1): if abrt Then exit Select 
                                setznp16(tempw Shr 1) 
                                if (tempw And 1) Then flags = flags Or C_FLAG 
                                if (tempw And &h8000) Then flags = flags Or V_FLAG 
                                cycles-=iif((modo=3),3,7) 
                                Exit Select 
                        	case &h38  /'SAR w,1'/
                                seteaw((tempw Shr 1) Or (tempw And &h8000)): if abrt then exit Select 
                                setznp16((tempw Shr 1) Or (tempw And &h8000)) 
                                if (tempw And 1) Then flags = flags Or C_FLAG 
                                cycles-=iif((modo=3),3,7) 
                                Exit Select 
                        	Case else 
                                'if deb=3 then print #5,"Bad D1 opcode ",hex(rmdat and &h38,2)
                       End Select
                       Exit Select 
                	case &h1D1, &h3D1 
                        fetchea32() : if abrt Then exit Select 
                        templ=geteal(): if abrt then Exit Select 
                        Select Case As Const  (rmdat And &h38)
                        	case &h00  /'ROL l,1'/
                                seteal((templ Shl 1) Or (templ Shr 31)): if abrt then exit Select 
                                if (templ And &h80000000) Then 
                                        flags = flags Or C_FLAG 
                                else
                                        flags = flags And inv(C_FLAG)
                                EndIf
                                templ = templ Shl 1
                                if (flags And C_FLAG) Then templ = templ Or 1 
                                if ((flags And C_FLAG) Xor (templ Shr 31)) Then 
                                        flags = flags Or V_FLAG 
                                else
                                        flags = flags And inv(V_FLAG)
                                EndIf
                                cycles-=iif((modo=3),3,7) 
                                Exit Select 
                        	case &h08  /'ROR l,1'/
                                seteal((templ Shr 1) Or (templ Shl 31)): if abrt then exit Select 
                                if (templ And 1) Then 
                                        flags = flags Or C_FLAG 
                                else
                                        flags = flags And inv(C_FLAG)
                                EndIf
                                templ = templ Shr 1
                                if (flags And C_FLAG) Then templ = templ Or &h80000000 
                                if ((templ Xor (templ Shr 1)) And &h40000000) Then 
                                        flags = flags Or V_FLAG 
                                else
                                        flags = flags And inv(V_FLAG)
                                EndIf
                                cycles-=iif((modo=3),3,7) 
                                Exit Select 
                        	case &h10  /'RCL l,1'/
                                temp2=flags And C_FLAG 
                                seteal((templ Shl 1) Or temp2): if abrt then Exit Select 
                                if (templ And &h80000000) Then 
                                        flags = flags Or C_FLAG 
                                else
                                        flags = flags And inv(C_FLAG)
                                EndIf
                                templ = templ Shl 1 
                                if temp2 Then templ = templ Or 1 
                                if ((flags And C_FLAG) Xor (templ Shr 31)) Then 
                                        flags = flags Or V_FLAG 
                                else
                                        flags = flags And inv(V_FLAG)
                                EndIf
                                cycles-=iif((modo=3),3,7) 
                                Exit Select 
                        	case &h18  /'RCR l,1'/
                                temp2=flags And C_FLAG 
                                seteal((templ Shr 1) Or iif(temp2,&h80000000,0)): if abrt then exit Select 
                                temp2=flags And C_FLAG 
                                if (templ And 1) Then 
                                        flags = flags Or C_FLAG 
                                else
                                        flags = flags And inv(C_FLAG)
                                EndIf
                                templ = templ Shr 1 
                                if temp2 Then templ = templ Or &h80000000 
                                if ((templ Xor (templ Shr 1)) And &h40000000) Then 
                                        flags = flags Or V_FLAG 
                                else
                                        flags = flags And inv(V_FLAG)
                                EndIf
                                cycles-=iif((modo=3),3,7) 
                                Exit Select 
                        	case &h20, &h30  /'SHL l,1'/
                                seteal(templ Shl 1): if abrt then exit Select 
                                setznp32(templ Shl 1) 
                                if (templ And &h80000000) Then flags = flags Or C_FLAG 
                                if ((templ Xor (templ Shl 1)) And &h80000000) Then flags = flags Or V_FLAG 
                                cycles-=iif((modo=3),3,7) 
                                Exit Select 
                        	case &h28  /'SHR l,1'/
                                seteal(templ Shr 1): if abrt then exit Select 
                                setznp32(templ Shr 1)
                                if (templ And 1) Then flags = flags Or C_FLAG 
                                if (templ And &h80000000) Then flags = flags Or V_FLAG 
                                cycles-=iif((modo=3),3,7) 
                                Exit Select 
                        	case &h38  /'SAR l,1'/
                                seteal((templ Shr 1) Or (templ And &h80000000)): if abrt then exit Select 
                                setznp32((templ Shr 1) Or (templ And &h80000000)) 
                                if (templ And 1) Then flags = flags Or C_FLAG 
                                cycles-=iif((modo=3),3,7) 
                                Exit Select 
                        	case Else 
                                'if deb=3 then print #5,"Bad D1 opcode ",hex(rmdat and &h38,2) 
                       End Select
                       Exit Select 
                	case &hD2, &h1D2, &h2D2, &h3D2 
                        fetchea32() : if abrt Then exit Select 
                        temp=geteab():  if abrt then exit Select 
                        c=CL And 31 
                        if c=0 then exit Select 
                        Select Case As Const  (rmdat And &h38)
                        	case &h00  /'ROL b,CL'/
                                while (c>0)
                                        temp2=IIf((temp And &h80),1,0 )
                                        temp=(temp Shl 1) Or temp2 
                                        c-=1 
                                Wend
                                seteab(temp): if abrt then exit Select 
                                flags = flags And inv(C_FLAG Or V_FLAG) 
                                if temp2 Then flags = flags Or C_FLAG 
                                if ((flags And C_FLAG) Xor (temp Shr 7)) Then flags = flags Or V_FLAG 
                                cycles-=iif((modo=3),3,7) 
                                Exit Select 
                        	case &h08  /'ROR b,CL'/
                                while (c>0)
                                        temp2= temp And 1 
                                        temp = temp Shr 1 
                                        if temp2 Then temp = temp Or &h80 
                                        c-=1 
                                Wend
                                seteab(temp): if abrt then exit Select 
                                flags = flags And inv(C_FLAG Or V_FLAG) 
                                if temp2 Then flags = flags Or C_FLAG 
                                if ((temp Xor (temp Shr 1)) And &h40) Then flags = flags Or V_FLAG 
                                cycles-=iif((modo=3),3,7) 
                                Exit Select 
                        	case &h10  /'RCL b,CL'/
                                tempc=flags And C_FLAG 
                                while (c>0)
                                        templ=tempc 
                                        tempc=temp And &h80 
                                        temp = temp Shl 1 
                                        if (templ) Then temp = temp Or 1 
                                        c-=1 
                                Wend
                                seteab(temp): if abrt then exit Select 
                                flags = flags And inv(C_FLAG Or V_FLAG) 
                                if tempc Then flags = flags Or C_FLAG 
                                if ((flags And C_FLAG) Xor (temp Shr 7)) Then flags = flags Or V_FLAG 
                                cycles-=iif((modo=3),3,7) 
                                Exit Select 
                        	case &h18  /'RCR b,CL'/
                                tempc=flags And C_FLAG 
                                while (c>0)
                                        templ=tempc 
                                        tempc=temp And 1 
                                        temp = temp Shr 1 
                                        if (templ) Then temp = temp Or &h80 
                                        c-=1 
                                Wend
                                seteab(temp): if abrt then exit Select 
                                flags = flags And inv(C_FLAG Or V_FLAG) 
                                if tempc Then flags = flags Or C_FLAG 
                                if ((temp Xor (temp Shr 1)) And &h40) Then flags = flags Or V_FLAG 
                                cycles-=iif((modo=3),3,7) 
                                Exit Select 
                        	case &h20, &h30  /'SHL b,CL'/
                                seteab(temp Shl c): if abrt then exit Select 
                                setznp8(temp Shl c) 
                                if ((temp Shl (c-1)) And &h80) Then flags = flags Or C_FLAG 
                                if (((temp Shl c) Xor (temp Shl (c-1))) And &h80) Then flags = flags Or V_FLAG 
                                cycles-=iif((modo=3),3,7) 
                                Exit Select
                        	case &h28  /'SHR b,CL'/
                                seteab(temp Shr c): if abrt then exit Select 
                                setznp8(temp Shr c) 
                                if ((temp Shr (c-1)) And 1) Then flags = flags Or C_FLAG 
                                if (c=1)  And  ((temp And &h80)<>0) Then flags = flags Or V_FLAG 
                                cycles-=iif((modo=3),3,7) 
                                Exit Select 
                        	case &h38  /'SAR b,CL'/
                                tempc=(temp Shr (c-1)) And 1 
                                while (c>0)
                                        temp = temp Shr 1 
                                        if (temp And &h40) Then temp = temp Or &h80 
                                        c-=1 
                                Wend
                                seteab(temp): if abrt then exit Select 
                                setznp8(temp) 
                                if tempc Then flags = flags Or C_FLAG 
                                cycles-=iif((modo=3),3,7) 
                                Exit Select 
                       End Select
                       Exit Select 
                	case &hD3, &h2D3 
                        fetchea32() : if abrt Then exit Select 
                        tempw=geteaw(): if abrt then exit Select 
                        c=CL And 31 
                        if c=0 then exit Select 
                        Select Case As Const  (rmdat And &h38)
                        	case &h00  /'ROL w,CL'/
                                while (c>0)
                                        temp=IIf((tempw And &h8000),1,0 )
                                        tempw=(tempw Shl 1) Or temp 
                                        c-=1 
                                Wend
                                seteaw(tempw): if abrt then exit Select 
                                flags = flags And inv(C_FLAG Or V_FLAG) 
                                if (temp) Then flags = flags Or C_FLAG 
                                if ((flags And C_FLAG) Xor (tempw Shr 15)) Then flags = flags Or V_FLAG 
                                cycles-=iif((modo=3),3,7) 
                                Exit Select 
                        	case &h08  /'ROR w,CL'/
                                while (c>0)
                                        tempw2=Iif((tempw And 1),&h8000,0 )
                                        tempw=(tempw Shr 1) Or tempw2 
                                        c-=1 
                                Wend
                                seteaw(tempw): if abrt then exit Select 
                                flags = flags And inv(C_FLAG Or V_FLAG) 
                                if (tempw2) Then flags = flags Or C_FLAG 
                                if ((tempw Xor (tempw Shr 1)) And &h4000) Then flags = flags Or V_FLAG 
                                cycles-=iif((modo=3),3,7) 
                                Exit Select 
                        	case &h10  /'RCL w,CL'/
                                tempc=flags And C_FLAG 
                                while (c>0)
                                        templ=tempc 
                                        tempc=tempw And &h8000 
                                        tempw=(tempw Shl 1) Or templ 
                                        c-=1 
                                Wend
                                seteaw(tempw): if abrt then exit Select 
                                flags = flags And inv(C_FLAG Or V_FLAG) 
                                if tempc Then flags = flags Or C_FLAG 
                                if ((flags And C_FLAG) Xor (tempw Shr 15)) Then flags = flags Or V_FLAG 
                                cycles-=iif((modo=3),3,7) 
                                Exit Select 
                        	case &h18  /'RCR w,CL'/
                                tempc=flags And C_FLAG 
                                while (c>0)
                                        templ=tempc 
                                        tempw2=IIf ((templ And 1),&h8000,0)
                                        tempc=tempw And 1 
                                        tempw=(tempw Shr 1) Or tempw2 
                                        c-=1 
                                Wend
                                seteaw(tempw): if abrt then exit Select 
                                flags = flags And inv(C_FLAG Or V_FLAG) 
                                if tempc Then flags = flags Or C_FLAG 
                                if ((tempw Xor (tempw Shr 1)) And &h4000) Then flags = flags Or V_FLAG 
                                cycles-=iif((modo=3),3,7) 
                                Exit Select 
                        	case &h20, &h30  /'SHL w,CL'/
                                seteaw(tempw Shl c): if abrt then exit Select 
                                setznp16(tempw Shl c) 
                                if ((tempw Shl (c-1)) And &h8000) Then flags = flags Or C_FLAG 
                                if (((tempw Shl c) Xor (tempw Shl (c-1))) And &h8000) Then flags = flags Or V_FLAG 
                                cycles-=iif((modo=3),3,7) 
                                Exit Select 
                        	case &h28  /'SHR w,CL'/
                                seteaw(tempw Shr c): if abrt then exit Select 
                                setznp16(tempw Shr c) 
                                if ((tempw Shr (c-1)) And 1) Then flags = flags Or C_FLAG 
                                if (c=1)  And  ((tempw And &h8000)<>0) Then flags = flags Or V_FLAG 
                                cycles-=iif((modo=3),3,7) 
                                Exit Select 
                        	case &h38  /'SAR w,CL'/
                                tempw2=tempw And &h8000 
                                tempc=(CShort(tempw) Shr (c-1)) And 1 
                                while (c>0)
                                        tempw=(tempw Shr 1) Or tempw2 
                                        c-=1 
                                Wend
                                seteaw(tempw): if abrt then exit Select 
                                setznp16(tempw) 
                                if tempc Then flags = flags Or C_FLAG 
                                cycles-=iif((modo=3),3,7) 
                                Exit Select 
                       End Select
                       Exit Select 
                        
                	case &h1D3, &h3D3 
                        fetchea32() : if abrt Then exit Select 
                        templ=geteal(): if abrt then exit Select 
                        c=CL And 31 
                        if c=0 Then exit Select 
                        Select Case As Const  (rmdat And &h38)
                        	case &h00  /'ROL l,CL'/
                                while (c>0)
                                        temp=IIf((templ And &h80000000),1,0) 
                                        templ=(templ Shl 1) Or temp 
                                        c-=1 
                                Wend
                                seteal(templ): if abrt then exit Select 
                                flags = flags And inv(C_FLAG Or V_FLAG) 
                                if (temp) Then flags = flags Or C_FLAG 
                                if ((flags And C_FLAG) Xor (templ Shr 31)) Then flags = flags Or V_FLAG 
                                cycles-=iif((modo=3),3,7) 
                                Exit Select 
                        	case &h08  /'ROR l,CL'/
                                while (c>0)
                                        templ2=IIf((templ And 1),&h80000000,0) 
                                        templ=(templ Shr 1) Or templ2 
                                        c-=1 
                                Wend
                                seteal(templ): if abrt then exit Select 
                                flags = flags And inv(C_FLAG Or V_FLAG) 
                                if (templ2) Then flags = flags Or C_FLAG 
                                if ((templ Xor (templ Shr 1)) And &h40000000) Then flags = flags Or V_FLAG 
                                cycles-=iif((modo=3),3,7) 
                                Exit Select 
                        	case &h10  /'RCL l,CL'/
                                tempc=flags And C_FLAG 
                                while (c>0)
                                        templ2=tempc 
                                        tempc=templ Shr 31
                                        templ=(templ Shl 1) Or templ2 
                                        c-=1 
                                Wend
                                seteal(templ): if abrt then exit Select 
                                flags = flags And inv(C_FLAG Or V_FLAG) 
                                if (templ2) Then flags = flags Or C_FLAG 
                                if ((flags And C_FLAG) Xor (templ Shr 31)) Then flags = flags Or V_FLAG 
                                cycles-=iif((modo=3),3,7) 
                                Exit Select 
                        	case &h18  /'RCR l,CL'/
                                tempc=flags And C_FLAG 
                                while (c>0)
                                        templ2=IIf((tempc),&h80000000,0 )
                                        tempc=templ And 1 
                                        templ=(templ Shr 1) Or templ2 
                                        c-=1 
                                Wend
                                seteal(templ): if abrt then exit Select 
                                flags = flags And inv(C_FLAG Or V_FLAG) 
                                if (templ2) Then flags = flags Or C_FLAG 
                                if ((templ Xor (templ Shr 1)) And &h40000000) Then flags = flags Or V_FLAG 
                                cycles-=iif((modo=3),3,7) 
                                Exit Select 
                        	case &h20, &h30  /'SHL l,CL'/
                                seteal(templ Shl c): if abrt then exit Select 
                                setznp32(templ Shl c) 
                                if ((templ Shl (c-1)) And &h80000000) Then flags = flags Or C_FLAG 
                                if (((templ Shl c) Xor (templ Shl (c-1))) And &h80000000) Then flags = flags Or V_FLAG 
                                cycles-=iif((modo=3),3,7) 
                                Exit Select 
                        	case &h28  /'SHR l,CL'/
                                seteal(templ Shr c): if abrt then exit Select 
                                setznp32(templ Shr c) 
                                if ((templ Shr (c-1)) And 1) Then flags = flags Or C_FLAG 
                                if (c=1)  And  ((templ And &h80000000)<>0) Then flags = flags Or V_FLAG 
                                cycles-=iif((modo=3),3,7) 
                                Exit Select 
                        	case &h38  /'SAR w,CL'/
                                templ2=templ And &h80000000 
                                tempc=(templ Shr (c-1)) And 1 
                                while (c>0)
                                        templ=(templ Shr 1) Or templ2 
                                        c-=1 
                                Wend
                                seteal(templ): if abrt then exit Select 
                                setznp32(templ) 
                                if tempc Then flags = flags Or C_FLAG 
                                cycles-=iif((modo=3),3,7) 
                                Exit Select 
                       End Select
                       Exit Select 
                        
                        
                        
                	case &hD4, &h1D4, &h2D4, &h3D4  /'AAM'/
                        tempws=readmemb_386(cs0,pc): pc+=1 
                        ' anulo esta, por que SIEMPRE va a ser INTEL
                        'If ( 0 =tempws  Or  cpu_manufacturer <> MANU_INTEL) Then tempws = 10 
                        If (tempws=0) Then tempws = 10 ' dejo solo este en su lugar
                        AH=(AL \ tempws) 
                        AL = AL Mod tempws 
                        setznp16(AX) 
                        cycles-=15
                        Exit Select 
                	case &hD5, &h1D5, &h2D5, &h3D5  /'AAD'/
                        tempws=readmemb_386(cs0,pc): pc+=1 
                        ' anulo esta, por que SIEMPRE va a ser INTEL, jeje
                        'if (cpu_manufacturer <> MANU_INTEL) Then tempws = 10 
                        AL=(AH*tempws)+AL 
                        AH=0 
                        setznp16(AX) 
                        cycles-=14
                        Exit Select 
                        
                        
                        
                	case &hD6, &h1D6, &h2D6, &h3D6  /'SETALC'/
                        AL=IIf((flags And C_FLAG),&hFF,0) 
                        cycles -= timing_rr 
                        Exit Select 
                	case &hD7, &h1D7  /'XLAT'/
                        addr=(BX+AL) And &hFFFF 
                        temp=readmemb_386(ds0,addr): if abrt then exit Select 
                        AL=temp 
                        cycles-=5 
                        Exit Select 
                	case &h2D7, &h3D7  /'XLAT'/
                        addr=EBX+AL 
                        temp=readmemb_386(ds0,addr): if abrt then exit Select 
                        AL=temp 
                        cycles-=5 
                        Exit Select 



'''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''
'''''''''                       COPROCESADOR                              '''''''''''''''''
'''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''
						/'ESCAPE'/
                	case  &hD9, &hDA, &hDB, &hDD  , _  
                			&h1D9, &h1DA, &h1DB, &h1DD , _
                			&h2D9, &h2DA, &h2DB, &h2DD , _
                			&h3D9, &h3DA, &h3DB, &h3DD , _
                			&hD8, &h1D8, &h2D8, &h3D8 , _
                			&hDC, &h1DC, &h2DC, &h3DC , _
                			&hDE, &h1DE, &h2DE, &h3DE , _
                			&hDF, &h1DF, &h2DF, &h3DF 
                        if ((cr0 And 6)=4) Then 
                                pc=oldpc 
                                pmodeint(7,0) 
                                cycles-=59 
                        else
                                'fpucount+=1 
                                fetchea32() : if abrt Then exit Select 
                                if (hasfpu) Then 
                                        x87_pc_off=oldpc 
                                        x87_pc_seg=CS1 
                                        x87_op_off=eaaddr 
                                        x87_op_seg=ea_rseg 
                                        Select Case As Const  (opcode)
                                        	case &hD8  
                                        		x87_d8()
                                        	case &hD9  
                                        		x87_d9() '
                                        	case &hDA  
                                        		x87_da()
                                        	case &hDB  
                                        		x87_db() ' d.exe
                                        	case &hDC  
                                        		x87_dc() '
                                        	case &hDD  
                                        		x87_dd() '
                                        	case &hDE  
                                        		x87_de() ' quizas
                                        	case &hDF  
                                        		x87_df() ' este
                                       End Select
                                EndIf
                        EndIf
                        Exit Select 
'''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''

                       
                        
                        
                	case &hE0, &h1E0  /'LOOPNE'/
                        offset=cbyte(readmemb_386(cs0,pc)): pc+=1 
                        CX-=1 
                        if (CX<>0) And ((flags And Z_FLAG)=0) Then 
                                 pc+=offset  
                        EndIf
                        cycles-=7
                        Exit Select 
                	case &h2E0, &h3E0  /'LOOPNE'/
                        offset=cbyte(readmemb_386(cs0,pc)): pc+=1 
                        ECX-=1 
                        if (ECX<>0) And ((flags And Z_FLAG)=0) Then 
                                 pc+=offset  
                        EndIf
                        cycles-=7
                        Exit Select 
                	case &hE1, &h1E1  /'LOOPE'/
                        offset=cbyte(readmemb_386(cs0,pc)): pc+=1 
                        CX-=1 
                        if (CX<>0) And ((flags And Z_FLAG)<>0) Then 
                                 pc+=offset  
                        EndIf
                        cycles-=7
                        Exit Select 
                	case &h2E1, &h3E1  /'LOOPE'/
                        offset=cbyte(readmemb_386(cs0,pc)): pc+=1 
                        ECX-=1 
                        if (ECX<>0) And ((flags And Z_FLAG)<>0) Then 
                                 pc+=offset  
                        EndIf
                        cycles-=7
                        Exit Select 
                	case &hE2, &h1E2  /'LOOP'/
                        offset=cbyte(readmemb_386(cs0,pc)): pc+=1 
                        CX-=1 
                        if (CX<>0) Then
                                 pc+=offset
                        EndIf
                        cycles-=7
                        Exit Select 
                	case &h2E2, &h3E2  /'LOOP'/
                        offset=cbyte(readmemb_386(cs0,pc)): pc+=1 
                        ECX-=1 
                        if (ECX<>0) Then 
                                 pc+=offset  
                        EndIf
                        cycles-=7
                        Exit Select 
                	case &hE3, &h1E3  /'JCXZ'/
                        offset=cbyte(readmemb_386(cs0,pc)): pc+=1 
                        if (CX=0) Then 
                                 pc+=offset: cycles-=4  
                        EndIf
                        cycles-=5 
                        Exit Select 
                	case &h2E3, &h3E3  /'JECXZ'/
                        offset=cbyte(readmemb_386(cs0,pc)): pc+=1 
                        if (ECX=0) Then 
                                 pc+=offset: cycles-=4  
                        EndIf
                        cycles-=5 
                        Exit Select 
                        
                        
                        
                        
                        '''''''''''''''''''''''''''
                	case &hE4, &h1E4, &h2E4, &h3E4  /'IN AL'/
                        temp=readmemb_386(cs0,pc) 
                        If checkio_perm(temp)=0 Then Exit Select ' error IO 
                        pc+=1 
                        AL=inb(temp) 
                        cycles-=12 
                        Exit Select 
                	case &hE5, &h2E5  /'IN AX'/
                        temp=readmemb_386(cs0,pc) 
                        If checkio_perm(temp)=0 Then Exit Select ' error IO  
                        If checkio_perm(temp+1)=0 Then Exit Select ' error IO  
                        pc+=1 
                        AX=inw(temp) 
                        cycles-=12 
                        Exit Select 
                	case &h1E5, &h3E5  /'IN EAX'/
                        temp=readmemb_386(cs0,pc) 
                        If checkio_perm(temp)=0 Then Exit Select ' error IO 
                        If checkio_perm(temp+1)=0 Then Exit Select ' error IO 
                        If checkio_perm(temp+2)=0 Then Exit Select ' error IO 
                        If checkio_perm(temp+3)=0 Then Exit Select ' error IO 
                        pc+=1 
                        EAX=inl(temp) 
                        cycles-=12 
                        Exit Select 
                        ''''''''''''''''''''''''
                	case &hE6, &h1E6, &h2E6, &h3E6  /'OUT AL'/
                        temp=readmemb_386(cs0,pc) 
                        If checkio_perm(temp)=0 Then Exit Select ' error IO  
                        pc+=1 
                        outb(temp,AL) 
                        cycles-=10 
                        Exit Select 
                	case &hE7, &h2E7  /'OUT AX'/
                        temp=readmemb_386(cs0,pc) 
                        If checkio_perm(temp)=0 Then Exit Select ' error IO 
                        If checkio_perm(temp+1)=0 Then Exit Select ' error IO 
                        pc+=1 
                        outw(temp,AX) 
                        cycles-=10 
                        Exit Select 
                	case &h1E7, &h3E7  /'OUT EAX'/
                        temp=readmemb_386(cs0,pc) 
                        If checkio_perm(temp)=0 Then Exit Select ' error IO 
                        If checkio_perm(temp+1)=0 Then Exit Select ' error IO 
                        If checkio_perm(temp+2)=0 Then Exit Select ' error IO 
                        If checkio_perm(temp+3)=0 Then Exit Select ' error IO 
                        pc+=1 
                        outl(temp,EAX) 
                        cycles-=10 
                        Exit Select 
                        '''''''''''''''''''''''''
                        
                        
                        
                	case &hE8  /'CALL rel 16'/
                        tempw=getword(): if abrt then exit Select 
                        if ssegs Then ss0=oldss 
                        if stack32 Then 
                                writememw_386(ss0,ESP-2,pc): if abrt then exit Select 
                                ESP-=2 
                        Else
                                writememw_386(ss0,((SP-2) And &hFFFF),pc): if abrt then exit Select 
                                SP-=2 
                        EndIf
                        pc+=cshort(tempw) 
                        cycles-=3
                        Exit Select 
                	case &h3E8  /'CALL rel 16'/
                        templ=getlong(): if abrt then exit Select 
                        if ssegs Then ss0=oldss 
                        if stack32 Then 
                                writememl_386(ss0,ESP-4,pc): if abrt then exit Select 
                                ESP-=4 
                        else
                                writememl_386(ss0,((SP-4) And &hFFFF),pc): if abrt then exit Select 
                                SP-=4 
                        EndIf
                        pc+=templ 
                        cycles-=3
                        Exit Select 
                	case &hE9, &h2E9  /'JMP rel 16'/
                        tempw=getword(): if abrt then exit Select 
                        pc+=cshort(tempw) 
                        cycles-=3 
                        Exit Select 
                	case &h1E9, &h3E9  /'JMP rel 32'/
                        templ=getlong(): if abrt then exit Select 
                        pc+=templ 
                        cycles-=3
                        Exit Select 
                	case &hEA, &h2EA  /'JMP far'/
                        addr=getword()
                        ''if (deb=3) Then print #5,"JMP ABRT"
                        tempw=getword(): if abrt Then exit Select 
                        oxpc=pc 
                        pc=addr 
                        loadcsjmp(tempw,oxpc) 
                        cycles-=17
                        Exit Select 
                	case &h1EA, &h3EA  /'JMP far'/
                        templ=getlong() 
                        tempw=getword(): if abrt then exit Select 
                        oxpc=pc 
                        pc=templ
                        loadcsjmp(tempw,oxpc) 
                        cycles-=17
                        Exit Select 
                	case &hEB, &h1EB, &h2EB, &h3EB  /'JMP rel'/
                        offset=cbyte(readmemb_386(cs0,pc)): pc+=1 
                        pc+=offset 
                        cycles-=3
                        Exit Select 
                        
                        
                        '''''''''''''''''''''''''''''''''
                	case &hEC, &h1EC, &h2EC, &h3EC  /'IN AL,DX'/
                        If checkio_perm(DX)=0 Then Exit Select ' error IO  
                        AL=inb(DX) 
                        cycles-=13 
                        Exit Select 
                	case &hED, &h2ED  /'IN AX,DX'/
                        If checkio_perm(DX)=0 Then Exit Select ' error IO 
                        If checkio_perm(DX+1)=0 Then Exit Select ' error IO 
                        AX=inw(DX) 
                        cycles-=13 
                        Exit Select 
                	case &h1ED, &h3ED  /'IN EAX,DX'/
                        If checkio_perm(DX)=0 Then Exit Select ' error IO 
                        If checkio_perm(DX+1)=0 Then Exit Select ' error IO 
                        If checkio_perm(DX+2)=0 Then Exit Select ' error IO 
                        If checkio_perm(DX+3)=0 Then Exit Select ' error IO 
                        EAX=inl(DX) 
                        cycles-=13 
                        Exit Select 
                        ''''''''''''''''''''
                	case &hEE, &h1EE, &h2EE, &h3EE  /'OUT DX,AL'/
                        If checkio_perm(DX)=0 Then Exit Select ' error IO  
                        outb(DX,AL) 
                        cycles-=11 
                        Exit Select 
                	case &hEF, &h2EF  /'OUT DX,AX'/
                        If checkio_perm(DX)=0 Then Exit Select ' error IO 
                        If checkio_perm(DX+1)=0 Then Exit Select ' error IO 
                        outw(DX,AX) 
                        cycles-=11 
                        Exit Select 
                	case &h1EF, &h3EF  /'OUT DX,EAX'/
                        If checkio_perm(DX)=0 Then Exit Select ' error IO 
                        If checkio_perm(DX+1)=0 Then Exit Select ' error IO 
                        If checkio_perm(DX+2)=0 Then Exit Select ' error IO 
                        If checkio_perm(DX+3)=0 Then Exit Select ' error IO 
                        outl(DX,EAX) 
                        cycles-=11 
                        Exit Select 
                        '''''''''''''''''''''''''''''''''
                        
                        
                        
                        
                        
                	case &hF0, &h1F0, &h2F0, &h3F0  /'LOCK'/
                        cycles-=4 
                        Exit Select 
                	case &hF2, &h1F2, &h2F2, &h3F2  /'REPNE'/
                        rep386(0) 
                        Exit Select 
                	case &hF3, &h1F3, &h2F3, &h3F3  /'REPE'/
                        rep386(1) 
                        Exit Select 
                        
                        
                        
                        
                	case &hF4, &h1F4, &h2F4, &h3F4  /'HLT'/
                        If ((CPL<>0) Or ((eflags And VM_FLAG)<>0)) And ((cr0 and 1)<>0) Then 
                            x86gpf(0) 
                            Exit Select
                        EndIf
                        inhlt=1 
                        pc-=1 
                        If ( ((flags And I_FLAG)<>0) And ((  ((pic.pend And inv(pic.mask)) And inv(pic.mask2) ) Or _
                			   ((pic2.pend And inv(pic2.mask) ) And inv(pic2.mask2))  )<>0) And (noint=0) And (ssegs=0) )=0 Then 
                                cycles = oldcyc - 100 
                        Else 
                                cycles-=5
                        EndIf 
                        Exit Select 
                        
                        
                        
                        
                	case &hF5, &h1F5, &h2F5, &h3F5  /'CMC'/
                        flags = flags Xor C_FLAG 
                        cycles-=2 
                        Exit Select 
                	case &hF6, &h1F6, &h2F6, &h3F6 
                        fetchea32() : if abrt Then exit Select 
                        temp=geteab(): if abrt then exit Select 
                        Select Case As Const  (rmdat And &h38)
                        	Case &h00  /'TEST b,#8'/
                                temp2=readmemb_386(cs0,pc): pc+=1: if abrt then exit Select 
                                temp = temp And temp2 
                                setznp8(temp) 
                                cycles-=iif((modo=3),1,2) 
                                Exit Select 
                        	case &h10  /'NOT b'/
                                seteab(not(temp)) ' NOT=invertir bits 
                                cycles -= iif((modo = 3) , timing_rr , timing_mm )
                                Exit Select 
                        	case &h18  /'NEG b'/
                                setsub8(0,temp) 
                                temp=0-temp 
                                seteab(temp) 
                                cycles -= iif((modo = 3) , timing_rr , timing_mm )
                                Exit Select 
                        	case &h20  /'MUL AL,b'/
                                AX=AL*temp 
                                if (AH<>0) Then 
                                        flags = flags Or (C_FLAG Or V_FLAG) 
                                else
                                        flags = flags And inv(C_FLAG Or V_FLAG)
                                EndIf
                                cycles-=13 
                                Exit Select 
                        	case &h28  /'IMUL AL,b'/
                                tempws=Clng(CByte(AL))*Clng(CByte(temp)) 
                                AX=tempws And &hFFFF 
                                if (AH<>0) And (AH<>&hFF) Then 
                                        flags = flags Or (C_FLAG Or V_FLAG) 
                                else
                                        flags = flags And inv(C_FLAG Or V_FLAG)
                                EndIf
                                cycles-=14 
                                Exit Select 
                        	case &h30  /'DIV AL,b'/
                                tempw=AX 
                                if (temp) Then tempw2=(tempw \ temp) 
                                if (temp<>0) And ((tempw2 And &hFF00)=0) Then 
                                        tempw2=tempw Mod temp 
                                        AH=tempw2 
                                        tempw=(tempw \ temp) 
                                        AL=tempw And &hFF 
                                        'if ( 0 =cpu_iscyrix) Then ' anulo esto, NUNCA sera CYRIX, solo INTEL
                                        	flags = flags Or &h8D5: /'NO es Cyrix'/
                                        'EndIf
                                else
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
                                EndIf
                                cycles-=16 
                                Exit Select 
                        	case &h38  /'IDIV AL,b'/
                                tempws=Clng(CShort(AX)) 
                                if (temp<>0) Then tempws2=(tempws \ CLng(CByte(temp))) 
                                temps=tempws2 And &hFF 
                                if (temp<>0) And ((CLng(temps)=tempws2)) Then 
                                        tempw2=tempws Mod CLng(CByte(temp))
                                        AH=tempw2 And &hFF 
                                        AL=tempws2 And &hFF 
                                        'if ( 0 =cpu_iscyrix) Then ' anulo esto, NUNCA sera CYRIX, solo INTEL
                                        	flags = flags Or &h8D5: /'NO es Cyrix'/
                                        'EndIf
                                else
                                        'if deb=3 then print #5,"IDIVb exception ",tempws,temp,tempws2 
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
                                EndIf
                                cycles-=19 
                                Exit Select 
                        	case Else 
                                'if deb=3 then print #5,"Bad F6 opcode ",hex(rmdat and &h38,2) 
                                x86illegal() 
                       End Select
                       Exit Select 
                	case &hF7, &h2F7 
                        fetchea32() : if abrt Then exit Select 
                        tempw=geteaw(): if abrt then exit Select 
                        Select Case As Const  (rmdat And &h38)
                        	case &h00  /'TEST w'/
                                tempw2=getword(): if abrt then exit Select 
                                setznp16(tempw And tempw2) 
                                cycles-=iif((modo=3),1,2) 
                                Exit Select 
                        	case &h10  /'NOT w'/
                                seteaw(Not(tempw))  ' NOT=invertir bits
                                cycles -= iif((modo = 3) , timing_rr , timing_mm ) 
                                Exit Select 
                        	case &h18  /'NEG w'/
                                setsub16(0,tempw) 
                                tempw=0-tempw 
                                seteaw(tempw) 
                                cycles -= iif((modo = 3) , timing_rr , timing_mm )
                                Exit Select 
                        	case &h20  /'MUL AX,w'/
                                templ=AX*tempw 
                                AX=templ And &hFFFF 
                                DX=templ Shr 16 
                                if (DX<>0) Then 
                                           flags = flags Or (C_FLAG Or V_FLAG) 
                                else
                                           flags = flags And inv(C_FLAG Or V_FLAG)
                                EndIf
                                cycles-=21 
                                Exit Select 
                        	case &h28  /'IMUL AX,w'/
                                templ=Clng(CShort(AX))*Clng(CShort(tempw))
                                AX=templ And &hFFFF 
                                DX=templ Shr 16 
                                if (DX<>0) And (DX<>&hFFFF) Then 
                                        flags = flags Or (C_FLAG Or V_FLAG) 
                                else
                                        flags = flags And inv(C_FLAG Or V_FLAG)
                                EndIf
                                cycles-=22 
                                Exit Select 
                        	case &h30  /'DIV AX,w'/
                                templ=(DX Shl 16) Or AX 
                                if (tempw) Then templ2=(templ \ tempw) 
                                if (tempw<>0) And ((templ2 And &hFFFF0000)=0) Then 
                                        tempw2=templ Mod tempw 
                                        DX=tempw2 
                                        templ=(templ \ tempw) 
                                        AX=templ And &hFFFF 
                                        'if ( 0 =cpu_iscyrix) Then ' anulo esto, NUNCA sera CYRIX, solo INTEL
                                        	setznp16(AX): /'NO es Cyrix'/
                                        'EndIf
                                else
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
                                EndIf
                                cycles-=24
                                Exit Select 
                        	case &h38  /'IDIV AX,w'/
                                tempws=cint((CShort(DX) Shl 16) Or AX) 
                                if (tempw<>0) Then tempws2=(tempws \ CInt(CShort(tempw)))
                                temps16=tempws2 And &hFFFF 
                                if (tempw<>0) And (cint(temps16)=tempws2) Then 
                                        DX=tempws Mod CInt((CShort(tempw)))
                                        AX=tempws2 And &hFFFF 
                                        'if ( 0 =cpu_iscyrix) Then ' anulo esto, NUNCA sera CYRIX, solo INTEL
                                        	setznp16(AX): /'NO es Cyrix'/
                                        'EndIf
                                Else
                                        'if deb=3 then print #5,"IDIVw exception ",tempws,tempw,tempws2 
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
                                EndIf
                                cycles-=27 
                                Exit Select 
                        	case Else 
                                'if deb=3 then print #5,"Bad F7 opcode ",hex(rmdat and &h38,2) 
                                x86illegal() 
                       End Select
                       Exit Select 
                	case &h1F7, &h3F7 
                        fetchea32() : if abrt Then exit Select 
                        templ=geteal(): if abrt then exit Select 
                        Select Case As Const  (rmdat And &h38)
                        	case &h00  /'TEST l'/
                                templ2=getlong(): if abrt then exit Select 
                                setznp32(templ And templ2) 
                                cycles-=iif((modo=3),1,2) 
                                Exit Select 
                        	case &h10  /'NOT l'/
                                seteal(Not(templ))  ' NOT=invertir bits
                                cycles -= iif((modo = 3) , timing_rr , timing_mml )
                                Exit Select 
                        	case &h18  /'NEG l'/
                                setsub32(0,templ) 
                                templ=0-templ 
                                seteal(templ) 
                                cycles -= iif((modo = 3) , timing_rr , timing_mml )
                                Exit Select 
                        	case &h20  /'MUL EAX,l'/
                                temp64=culngint(EAX)*culngint(templ) 
                                EAX=temp64 And &hFFFFFFFF 
                                EDX=temp64 Shr 32 
                                if (EDX<>0) Then 
                                        flags = flags Or (C_FLAG Or V_FLAG) 
                                else
                                        flags = flags And inv(C_FLAG Or V_FLAG)
                                EndIf
                                cycles-=21 
                                Exit Select 
                        	case &h28  /'IMUL EAX,l'/
                                temp64=CLngInt(CLng(EAX))*CLngInt(CLng(templ))
                                EAX=temp64 And &hFFFFFFFF 
                                EDX=temp64 Shr 32 
                                'if (EDX<>0) And (EDX<>&hFFFFFFFF) Then 
                                If ((CLngInt(temp64) Shr 31)<>0) And ((CLngint(temp64) Shr 31)<>-1) then ' diferente en la 9.1
                                        flags = flags Or (C_FLAG Or V_FLAG) 
                                else
                                        flags = flags And inv(C_FLAG Or V_FLAG)
                                EndIf
                                cycles-=38 
                                Exit Select 
                        	case &h30  /'DIV EAX,l'/
                                divl(templ) 
                                'if ( 0 =cpu_iscyrix) Then ' anulo esto, NUNCA sera CYRIX, solo INTEL
                                	setznp32(EAX) /'si NO es Cyrix'/
                                'EndIf
                                cycles-=40
                                Exit Select 
                        	case &h38  /'IDIV EAX,l'/
                                idivl(clng(templ)) 
                                'if ( 0 =cpu_iscyrix) Then ' anulo esto, NUNCA sera CYRIX, solo INTEL
                                	setznp32(EAX) /'si NO es Cyrix'/
                                'EndIf
                                cycles-=43 
                                Exit Select 
                        	case Else 
                                'if deb=3 then print #5,"Bad F7 opcode ",hex(rmdat and &h38,2)
                                x86illegal() 
                       End Select
                       Exit Select 
                	case &hF8, &h1F8, &h2F8, &h3F8  /'CLC'/
                        flags = flags And inv(C_FLAG) 
                        cycles-=2 
                        Exit Select 
                	case &hF9, &h1F9, &h2F9, &h3F9  /'STC'/
                        flags = flags Or C_FLAG 
                        cycles-=2 
                        Exit Select 
                	case &hFA, &h1FA, &h2FA, &h3FA  /'CLI'/
                        if (IOPLp=0) Then 
                            x86gpf(0) 
                        else
                           flags = flags And inv(I_FLAG)
                        EndIf
                        cycles-=3 
                        Exit Select 
                	case &hFB, &h1FB, &h2FB, &h3FB  /'STI'/
                        if (IOPLp=0) Then 
                            x86gpf(0) 
                        else
                           flags = flags Or I_FLAG
                        EndIf
                        cycles-=2 
                        Exit Select 
                	case &hFC, &h1FC, &h2FC, &h3FC  /'CLD'/
                        flags = flags And inv(D_FLAG) 
                        cycles-=2 
                        Exit Select 
                	case &hFD, &h1FD, &h2FD, &h3FD  /'STD'/
                        flags = flags Or D_FLAG 
                        cycles-=2 
                        Exit Select 
                	case &hFE, &h1FE, &h2FE, &h3FE  /'INC/DEC b'/
                        fetchea32() : if abrt Then exit Select 
                        temp=geteab(): if abrt then exit Select 
                        if (rmdat And &h38) Then 
                                seteab(temp-1): if abrt then exit Select 
                                flags = flags And inv(V_FLAG) 
                                setsub8nc(temp,1) 
                                temp2=temp-1 
                                if ((temp And &h80)<>0) And ((temp2 And &h80)=0) Then flags = flags Or V_FLAG 
                        Else
                                seteab(temp+1): if abrt then exit Select 
                                flags = flags And inv(V_FLAG) 
                                setadd8nc(temp,1) 
                                temp2=temp+1 
                                if ((temp2 And &h80)<>0) And ((temp And &h80)=0) Then flags = flags Or V_FLAG 
                        EndIf
                        cycles -= iif((modo = 3) , timing_rr , timing_mm )
                        Exit Select 
                	case &hFF, &h2FF 
                        fetchea32() : if abrt Then exit Select 
                        Select Case As Const  (rmdat And &h38)
                        	Case &h00  /'INC w'/
                                tempw=geteaw():  if abrt then exit Select 
                                seteaw(tempw+1): if abrt then exit Select 
                                setadd16nc(tempw,1) 
                                cycles -= iif((modo = 3) , timing_rr , timing_mm )
                                Exit Select 
                        	case &h08  /'DEC w'/
                                tempw=geteaw():  if abrt then exit Select 
                                seteaw(tempw-1): if abrt then exit Select 
                                setsub16nc(tempw,1) 
                                cycles -= iif((modo = 3) , timing_rr , timing_mm )
                                Exit Select 
                        	case &h10  /'CALL'/
                                tempw=geteaw() 
                                ''if (deb=3) Then print #5,"CALL    ", tempw, easeg, eaaddr
                                if abrt Then exit Select 
                                if ssegs Then ss0=oldss 
                                if stack32 Then 
                                        writememw_386(ss0,ESP-2,pc): if abrt then exit Select 
                                        ESP-=2 
                                else
                                        writememw_386(ss0,(SP-2) And &hFFFF,pc): if abrt then exit Select 
                                        SP-=2 
                                EndIf
                                pc=tempw 
                                cycles-=5 
                                Exit Select 
                                
                                
                                
                                
                    ''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''            
                        	case &h18  /'CALL far'/
                                tempw=readmemw_386(easeg,eaaddr) 
                                tempw2=readmemw_386(easeg,(eaaddr+2))
                                ''if (deb=3) Then print #5,"CALL FAR (INS18) ",tempw,tempw2
                                if abrt Then exit Select 
                                tempw3=CS1 
                                templ2=pc 
                                if ssegs Then ss0=oldss 
                                oxpc=pc 
                                pc=tempw 
                                optype=CALL0 
                                cgate32=0 
                                if modoprotegido Then 
                                      loadcscall(tempw2) 
                                else
                                      loadcs(tempw2)
                                EndIf
                                optype=0 
                                if abrt Then exit Select 
                                oldss=ss0 
                                if (cgate32) Then goto writecall32_2 ' salta a la INS 18 abajo, linea 6848 aprox
                         ' goto       
                         writecall16_2:
                                if stack32 Then 
                                        writememw_386(ss0,ESP-2,tempw3) 
                                        writememw_386(ss0,ESP-4,templ2) 
                                        ESP-=4 
                                else
                                        writememw_386(ss0,(SP-2) And &hFFFF,tempw3) 
                                        writememw_386(ss0,((SP-4) And &hFFFF),templ2) 
                                        SP-=4 
                                EndIf
                                cycles-=17 
                                Exit Select 
                     ''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''        
                                
                                
                                
                                
                        	case &h20  /'JMP'/
                                tempw=geteaw(): if abrt then exit Select 
                                pc=tempw 
                                cycles-=5 
                                Exit Select 
                        	case &h28  /'JMP far'/
                                oxpc=pc 
                                tempw=readmemw_386(easeg,eaaddr) 
                                tempw2=readmemw_386(easeg,eaaddr+2): if abrt then exit Select 
                                pc=tempw                                 
                                loadcsjmp(tempw2,oxpc): if abrt then exit Select 
                                cycles-=13 
                                Exit Select 
                        	case &h30  /'PUSH w'/
                                tempw=geteaw(): if abrt then exit Select 
                                if ssegs Then ss0=oldss 
                                if stack32 Then 
                                        writememw_386(ss0,ESP-2,tempw): if abrt then exit Select 
                                        ESP-=2 
                                else
                                        writememw_386(ss0,((SP-2) And &hFFFF),tempw): if abrt then exit Select 
                                        SP-=2 
                                EndIf
                                cycles-=iif((modo=3),2,5) 
                                Exit Select 
                        	case Else 
                                'if deb=3 then print #5,"Bad FF opcode ",hex(rmdat and &h38,2)
                                x86illegal() 
                       End Select
                       Exit Select 
                	Case &h1FF, &h3FF 
                        fetchea32() : if abrt Then exit Select 
                        Select Case As Const  (rmdat And &h38)
                        	case &h00  /'INC l'/
                                templ=geteal():  if abrt then exit Select 
                                seteal(templ+1): if abrt then exit Select 
                                setadd32nc(templ,1) 
                                cycles -= iif((modo = 3) , timing_rr , timing_mml )
                                Exit Select 
                        	case &h08  /'DEC l'/
                                templ=geteal():  if abrt then exit Select 
                                seteal(templ-1): if abrt then exit Select 
                                setsub32nc(templ,1) 
                                cycles -= iif((modo = 3) , timing_rr , timing_mml )
                                Exit Select 
                        	case &h10  /'CALL'/
                                templ=geteal(): if abrt then exit Select 
                                if ssegs Then ss0=oldss 
                                if stack32 Then 
                                        writememl_386(ss0,ESP-4,pc): if abrt then exit Select 
                                        ESP-=4 
                                else
                                        writememl_386(ss0,(SP-4) And &hFFFF,pc): if abrt then exit Select 
                                        SP-=4 
                                EndIf
                                pc=templ 
                                if (pc=&hFFFFFFFF) Then print #5,"Failed CALL" 
                                cycles-=5 
                                Exit Select 
                                
                                
                                
                                
                                
                    ''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''            
                        	case &h18  /'CALL far'/
                                templ=readmeml_386(easeg,eaaddr) 
                                tempw2=readmemw_386(easeg,(eaaddr+4))
                                if abrt Then exit Select 
                                tempw3=CS1 
                                templ2=pc 
                                if ssegs Then ss0=oldss 
                                oxpc=pc 
                                pc=templ 
                                optype=CALL0 
                                cgate16=0 
                                if modoprotegido Then 
                                        loadcscall(tempw2) 
                                else
                                        loadcs(tempw2)
                                EndIf
                                optype=0 
                                if abrt Then exit Select 
                                oldss=ss0 
                                if (cgate16) Then goto writecall16_2 ' salta a la INS18 arriba (linea 6741 aprox)
                          'goto
                          writecall32_2: 
                                if stack32 Then 
                                        writememl_386(ss0,ESP-4,tempw3) 
                                        writememl_386(ss0,ESP-8,templ2) 
                                        ESP-=8 
                                else
                                        writememl_386(ss0,(SP-4) And &hFFFF,tempw3) 
                                        writememl_386(ss0,(SP-8) And &hFFFF,templ2) 
                                        SP-=8 
                                EndIf
                                if (pc=&hFFFFFFFF) Then print #5,"Failed CALL far " 
                                cycles-=17 
                                Exit Select 
                     '''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''
                                
                                
                                
                                
                                
                        	case &h20  /'JMP'/
                                templ=geteal(): if abrt then exit Select 
                                pc=templ 
                                cycles-=5 
                                if (pc=&hFFFFFFFF) Then print #5,"Failed JMP"
                                Exit Select 
                        	case &h28  /'JMP far'/
                                oxpc=pc 
                                templ=readmeml_386(easeg,eaaddr) 
                                templ2=readmeml_386(easeg,eaaddr+4): if abrt then exit Select 
                                pc=templ                                 
                                loadcsjmp(templ2,oxpc) 
                                if (pc=&hFFFFFFFF) Then print #5,"Failed JMP far" 
                                cycles-=13
                                Exit Select 
                        	case &h30  /'PUSH l'/
                                templ=geteal(): if abrt then exit Select 
                                if ssegs Then ss0=oldss 
                                if stack32 Then 
                                        writememl_386(ss0,ESP-4,templ): if abrt then exit Select 
                                        ESP-=4 
                                else
                                        writememl_386(ss0,((SP-4) And &hFFFF),templ): if abrt then exit Select 
                                        SP-=4 
                                EndIf
                                cycles-=iif((modo=3),2,5) 
                                Exit Select 
                        	case Else 
                                'if deb=3 then print #5,"Bad 32-bit FF opcode ",hex(rmdat and &h38,2) 
                                x86illegal() 
                       End Select

                	case Else 
                        'if deb=3 then print #5,"Bad opcode ",opcode,op32 Shr 8,opcode Or op32,cs0 Shr 4,pc
                        x86illegal() 
                End Select
''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''
''''''''''''' FIN DE INSTRUCCIONES EXEC386
''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''                 
                
                
                
''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''               
            ' goto, el FINAL, viene desde el arriba del todo, del inicio
            opcodeend: 

                If use32=0 Then pc = pc And &hFFFF ' si estamos en modo 8086
                
                if ssegs Then 
                        ds0=oldds 
                        ss0=oldss 
                        'rds=DS1 
                        ssegs=0 
                EndIf
                
                'abrt=0
                if abrt Then 
                        tempi = abrt 
                        abrt = 0 
                        '''''''''''''''''
                        x86_doabrt(tempi) ' en X86SEG.BAS, esta es la unica llamada que se hace
                        ''''''''''''''''' 
                        if abrt Then 
                                abrt = 0 
                                CS1 = oldcs 
                                pc = oldpc 
                                print #5,"Segundo fallo en DIR: ";Hex(pc,8)
                                pmodeint(8, 0) 
                                if abrt Then 
                                        abrt = 0 
                                        Print #5,"Tercer fallo, se reinicia"
                                        softresetx86() 
                                EndIf
                        EndIf
                EndIf
                
                cycdiff=oldcyc-cycles 


                If (trap<>0) And ((flags And T_FLAG)<>0) And (noint=0) Then 
                			'Print #5,"TRAP !!!!";Hex(CS1,4),Hex(pc,4);" en Modo Protegido:";modoprotegido
                        If modoprotegido Then
                                pmodeint(1,0) 
                        Else
                                writememw_386(ss0,(SP-2) And &hFFFF,flags) 
                                writememw_386(ss0,(SP-4) And &hFFFF,CS1) 
                                writememw_386(ss0,(SP-6) And &hFFFF,pc) 
                                SP-=6 
                                addr=1 Shl 2 
                                flags = flags And inv(I_FLAG) 
                                flags = flags And inv(T_FLAG) 
                                pc=readmemw_386(0,addr) 
                                loadcs(readmemw_386(0,addr+2)) 
                        EndIf
                ElseIf ((flags And I_FLAG)<>0)  And  (  (  ((pic.pend And inv(pic.mask)) And inv(pic.mask2)) _
                			Or ((pic2.pend And inv(pic2.mask) ) And inv(pic2.mask2))  )<>0) And (noint=0) Then 
                	      ''''''''''''''''''
                	      temp=picinterrupt() ' IRQ
                	      ''''''''''''''''''
                        if (temp<>&hFF) Then 
                                if (inhlt) Then pc+=1 
                                ''if (deb=3) Then Print #5,"Hardware int:";Hex(temp,2);" ";ins;" ";ins2;" ";Hex(CS1,4);"(";Hex(cs0,8);"):";Hex(pc,8)
                                if modoprotegido Then 
                                		    'Print #5,"Int. Protegida:";temp
                                        pmodeint(temp,0) 
                                else
                                        writememw_386(ss0,(SP-2) And &hFFFF,flags) 
                                        writememw_386(ss0,(SP-4) And &hFFFF,CS1) 
                                        writememw_386(ss0,(SP-6) And &hFFFF,pc) 
                                        SP-=6 
                                        addr=temp Shl 2 
                                        flags = flags And inv(I_FLAG) 
                                        flags = flags And inv(T_FLAG) 
                                        oxpc=pc 
                                        pc=readmemw_386(0,addr) 
                                        loadcs(readmemw_386(0,addr+2)) 
                                        ''if (deb=3) Then If temp=&h76 Then Print #5,"INT to ";Hex(CS1,4);":";Hex(pc,4)
                                EndIf
                                inint=1 
                        EndIf
                EndIf
                
                if (noint) Then noint=0 
                
                ins+=1 
                
            Wend
                
          '''' atiende al HARD ''''
          clockhardware(cycdiff) 
                           
        Wend
End Sub
