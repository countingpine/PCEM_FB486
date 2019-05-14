

Sub rep386(byval fv As Integer ) 
        Dim As UByte temp 
        Dim As ULong c
        Dim As UByte temp2 
        Dim As UShort tempw,tempw2
        Dim As Integer changeds=0 
        Dim As ULong oldds 
        Dim As ULong templ,templ2 
        Dim As Integer tempz 
        Dim As Integer tempi 
        Dim As UShort of=flags 
        Dim As ULong ipc=oldpc
        Dim As ULong rep32=op32 
                
     ' GOTO
     startrep:
        temp=readmemb_386(cs0,pc):pc+=1
        opcode2=temp

        c=IIf((rep32 And &h200),ECX,CX)

        Select Case As Const  (temp Or rep32)
        	case &hC3, &h1C3, &h2C3, &h3C3 
                pc-=1 
        	case &h08 
                pc=ipc+1 
        	case &h26, &h126, &h226, &h326  /'ES:'/
                oldds=ds0 
                ds0=es0 
                'rds=ES1 
                changeds=1 
                goto startrep 
        	case &h2E, &h12E, &h22E, &h32E  /'CS:'/
                oldds=ds0 
                ds0=cs0 
                'rds=CS1 
                changeds=1 
                goto startrep 
        	case &h36, &h136, &h236, &h336  /'SS:'/
                oldds=ds0 
                ds0=ss0 
                'rds=SS1 
                changeds=1 
                goto startrep 
        	case &h3E, &h13E, &h23E, &h33E  /'DS:'/
                oldds=ds0 
                ds0=ds0 
                changeds=1 
                goto startrep 
        	case &h64, &h164, &h264, &h364  /'FS:'/
                oldds=ds0 
                ds0=fs0 
                'rds=FS1 
                changeds=1 
                goto startrep 
        	case &h65, &h165, &h265, &h365  /'GS:'/
                oldds=ds0 
                ds0=gs0 
                'rds=GS1 
                changeds=1 
                goto startrep 
        	case &h66, &h166, &h266, &h366  /'Data size prefix'/
                rep32 = rep32 Xor &h100 
                goto startrep 
        	case &h67, &h167, &h267, &h367  /'Address size prefix'/
                rep32 = rep32 Xor &h200 
                goto startrep 
        	case &h6C, &h16C   /'REP INSB'/
                if (c>0) Then 
                        If checkio_perm(DX)=0 Then Exit Select ' error IO
                        temp2=inb(DX) 
                        writememb_386(es0,DI,temp2):If abrt Then exit Select 
                        if (flags And D_FLAG) Then
                        	DI-=1 
                        Else
                           DI+=1 
                        EndIf
                        c-=1 
                        cycles-=15 
                EndIf
                if (c>0) Then 
                        firstrepcycle=0: pc=ipc: If ssegs Then ssegs+=1   
                else
                        firstrepcycle=1
                EndIf
              Exit Select
        	case &h26C, &h36C  /'REP INSB'/
                if (c>0) Then 
                        If checkio_perm(DX)=0 Then Exit Select ' error IO
                        temp2=inb(DX) 
                        writememb_386(es0,EDI,temp2):if abrt Then exit Select 
                        if (flags And D_FLAG) Then 
                        	EDI-=1 
                        else
                           EDI+=1 
                        EndIf
                        c-=1 
                        cycles-=15 
                EndIf
                if (c>0) Then 
                         firstrepcycle=0: pc=ipc: If ssegs Then ssegs+=1   
                else
                        firstrepcycle=1
                EndIf
              Exit Select
        	case &h6D   /'REP INSW'/
                if (c>0) Then 
                        tempw=inw(DX) 
                        writememw_386(es0,DI,tempw) 
                        if abrt Then exit Select 
                        if (flags And D_FLAG) Then 
                        	DI-=2 
                        else
                           DI+=2 
                        EndIf
                        c-=1 
                        cycles-=15 
                EndIf
                if (c>0) Then 
                        firstrepcycle=0: pc=ipc: If ssegs Then ssegs+=1   
                else
                        firstrepcycle=1
                EndIf
              Exit Select
        	case &h16D  /'REP INSL'/
                if (c>0) Then 
                        templ=inl(DX) 
                        writememl_386(es0,DI,templ) 
                        if abrt Then exit Select 
                        if (flags And D_FLAG) Then 
                        	DI-=4 
                        else
                           DI+=4 
                        EndIf
                        c-=1 
                        cycles-=15 
                EndIf
                if (c>0) Then 
                        firstrepcycle=0: pc=ipc: If ssegs Then ssegs+=1   
                else
                        firstrepcycle=1
                EndIf
              Exit Select
        	case &h26D  /'REP INSW'/
                if (c>0) Then 
                        tempw=inw(DX) 
                        writememw_386(es0,EDI,tempw) 
                        if abrt Then exit Select 
                        if (flags And D_FLAG) Then 
                        	EDI-=2 
                        else
                           EDI+=2 
                        EndIf
                        c-=1 
                        cycles-=15 
                EndIf
                if (c>0) Then 
                        firstrepcycle=0: pc=ipc: If ssegs Then ssegs+=1   
                else
                        firstrepcycle=1
                EndIf
              Exit Select
        	case &h36D  /'REP INSL'/
                if (c>0) Then 
                        templ=inl(DX) 
                        writememl_386(es0,EDI,templ) 
                        if abrt Then exit Select 
                        if (flags And D_FLAG) Then 
                        	EDI-=4 
                        else
                            EDI+=4 
                        EndIf
                        c-=1 
                        cycles-=15 
                EndIf
                if (c>0) Then 
                        firstrepcycle=0: pc=ipc: If ssegs Then ssegs+=1   
                else
                        firstrepcycle=1
                EndIf
              Exit Select
        	case &h6E, &h16E   /'REP OUTSB'/
                if (c>0) Then 
                        temp2=readmemb_386(ds0,SI):if abrt Then exit Select 
                        If checkio_perm(DX)=0 Then Exit Select ' error IO
                        outb(DX,temp2) 
                        if (flags And D_FLAG) Then 
                        	SI-=1 
                        else
                            SI+=1 
                        EndIf
                        c-=1 
                        cycles-=14 
                EndIf
                if (c>0) Then 
                        firstrepcycle=0: pc=ipc: If ssegs Then ssegs+=1   
                else
                        firstrepcycle=1
                EndIf
              Exit Select
        	case &h26E, &h36E  /'REP OUTSB'/
                if (c>0) Then 
                        temp2=readmemb_386(ds0,ESI):If abrt Then exit Select 
                        If checkio_perm(DX)=0 Then Exit Select ' error IO 
                        outb(DX,temp2) 
                        if (flags And D_FLAG) Then 
                        	ESI-=1 
                        else
                           ESI+=1 
                        EndIf
                        c-=1 
                        cycles-=14 
                EndIf
                if (c>0) Then 
                        firstrepcycle=0: pc=ipc: if ssegs Then ssegs+=1   
                else
                        firstrepcycle=1
                EndIf
              Exit Select
        	case &h6F   /'REP OUTSW'/
                if (c>0) Then 
                        tempw=readmemw_386(ds0,SI) 
                        if abrt Then exit Select 
                        outw(DX,tempw) 
                        if (flags And D_FLAG) Then 
                        	SI-=2 
                        else
                           SI+=2 
                        EndIf
                        c-=1 
                        cycles-=14 
                EndIf
                if (c>0) Then 
                        firstrepcycle=0: pc=ipc: if ssegs Then ssegs+=1   
                else
                        firstrepcycle=1
                EndIf
              Exit Select
        	case &h16F  /'REP OUTSL'/
                if (c > 0) Then 
                        templ = readmeml_386(ds0, SI) 
                        if abrt Then exit Select 
                        outl(DX, templ) 
                        if (flags  And D_FLAG) Then 
                        	SI -= 4 
                        else
                           SI += 4 
                        EndIf
                        c-=1 
                        cycles -= 14 
                EndIf
                if (c > 0) Then 
                        firstrepcycle = 0: pc=ipc: if ssegs Then ssegs+=1   
                else
                        firstrepcycle = 1
                EndIf
              Exit Select
        	case &h26F  /'REP OUTSW'/
                if (c>0) Then 
                        tempw=readmemw_386(ds0,ESI) 
                        if abrt Then exit Select 
                        outw(DX,tempw) 
                        if (flags And D_FLAG) Then 
                        	ESI-=2 
                        else
                            ESI+=2 
                        EndIf
                        c-=1 
                        cycles-=14 
                EndIf
                if (c>0) Then 
                        firstrepcycle=0: pc=ipc: if ssegs Then ssegs+=1   
                else
                        firstrepcycle=1
                EndIf
              Exit Select
        	case &h36F  /'REP OUTSL'/
                if (c > 0) Then 
                        templ = readmeml_386(ds0, ESI) 
                        if abrt Then exit Select 
                        outl(DX, templ) 
                        if (flags  And D_FLAG) Then 
                        	ESI -= 4 
                        else
                           ESI += 4 
                        EndIf
                        c-=1 
                        cycles -= 14 
                EndIf
                if (c > 0) Then 
                        firstrepcycle = 0: pc=ipc: if ssegs Then ssegs+=1   
                else
                        firstrepcycle = 1
                EndIf
              Exit Select
        	case &hA4, &h1A4   /'REP MOVSB'/
                if (c>0) Then 
                        temp2=readmemb_386(ds0,SI):  if abrt then exit Select 
                        writememb_386(es0,DI,temp2): if abrt then exit Select 
                        if (flags And D_FLAG) Then 
                                 DI-=1: SI-=1   
                        else
                                 DI+=1: SI+=1  
                        EndIf
                        c-=1 
                        cycles-=IIf((is486),3,4)
                EndIf
                if (c>0) Then 
                        firstrepcycle=0: pc=ipc: if ssegs Then ssegs+=1   
                else
                        firstrepcycle=1
                EndIf
              Exit Select
        	case &h2A4, &h3A4  /'REP MOVSB'/
                if (c>0) Then 
                        temp2=readmemb_386(ds0,ESI):  if abrt then exit Select 
                        writememb_386(es0,EDI,temp2): if abrt then exit Select 
                        if (flags And D_FLAG) Then 
                                 EDI-=1: ESI-=1   
                        else
                                 EDI+=1: ESI+=1  
                        EndIf
                        c-=1 
                        cycles-=IIf((is486),3,4)
                EndIf
                if (c>0) Then 
                        firstrepcycle=0: pc=ipc: if ssegs Then ssegs+=1   
                else
                        firstrepcycle=1
                EndIf
              Exit Select
        	case &hA5  /'REP MOVSW'/
                if (c>0) Then 
                        tempw=readmemw_386(ds0,SI):  if abrt then exit Select 
                        writememw_386(es0,DI,tempw): if abrt then exit Select 
                        if (flags And D_FLAG) Then 
                                 DI-=2: SI-=2   
                        else
                                 DI+=2: SI+=2  
                        EndIf
                        c-=1 
                        cycles-=IIf((is486),3,4) 
                EndIf
                if (c>0) Then 
                        firstrepcycle=0: pc=ipc: if ssegs Then ssegs+=1   
                else
                        firstrepcycle=1
                EndIf
              Exit Select
        	case &h1A5  /'REP MOVSL'/
                if (c>0) Then 
                        templ=readmeml_386(ds0,SI):  if abrt then exit Select 
                        writememl_386(es0,DI,templ): if abrt then exit Select 
                        if (flags And D_FLAG) Then 
                                 DI-=4: SI-=4   
                        else
                                 DI+=4: SI+=4  
                        EndIf
                        c-=1 
                        cycles-=IIf((is486),3,4)
                EndIf
                if (c>0) Then 
                        firstrepcycle=0: pc=ipc: if ssegs Then ssegs+=1   
                else
                        firstrepcycle=1
                EndIf
              Exit Select
        	case &h2A5  /'REP MOVSW'/
                if (c>0) Then 
                        tempw=readmemw_386(ds0,ESI):  if abrt then exit Select 
                        writememw_386(es0,EDI,tempw): if abrt then exit Select 
                        if (flags And D_FLAG) Then 
                                 EDI-=2: ESI-=2   
                        else
                                 EDI+=2: ESI+=2  
                        EndIf
                        c-=1 
                        cycles-=IIf((is486),3,4) 
                EndIf
                if (c>0) Then 
                        firstrepcycle=0: pc=ipc: if ssegs Then ssegs+=1   
                else
                        firstrepcycle=1
                EndIf
              Exit Select
        	case &h3A5  /'REP MOVSL'/
                if (c>0) Then 
                        templ=readmeml_386(ds0,ESI): if abrt then exit Select 
                        writememl_386(es0,EDI,templ): if abrt then exit Select 
                        if (flags And D_FLAG) Then 
                                 EDI-=4: ESI-=4   
                        else
                                 EDI+=4: ESI+=4  
                        EndIf
                        c-=1 
                        cycles-=IIf((is486),3,4)
                EndIf
                if (c>0) Then 
                        firstrepcycle=0: pc=ipc: if ssegs Then ssegs+=1   
                else
                        firstrepcycle=1
                EndIf
              Exit Select
        	case &hA6, &h1A6   /'REP CMPSB'/
                if (fv) Then 
                        flags = flags Or Z_FLAG 
                Else
                           flags = flags And inv(Z_FLAG)
                EndIf
                if (c>0)  And  (fv=iif((flags And Z_FLAG),1,0)) Then 
                        temp=readmemb_386(ds0,SI) 
                        temp2=readmemb_386(es0,DI) 
                        if abrt Then 
                            flags=of
                            Exit Select 
                        EndIf
                        if (flags And D_FLAG) Then 
                            DI-=1: SI-=1   
                        else
                            DI+=1: SI+=1  
                        EndIf
                        c-=1 
                        cycles-=IIf((is486),7,9) 
                        setsub8(temp,temp2) 
                EndIf
                if (c>0)  And  (fv=iif((flags And Z_FLAG),1,0)) Then 
                        firstrepcycle=0: pc=ipc: If ssegs Then ssegs+=1   
                else
                        firstrepcycle=1
                EndIf
              Exit Select
        	case &h2A6, &h3A6  /'REP CMPSB'/
                if (fv) Then 
                        flags = flags Or Z_FLAG 
                else
                           flags = flags And inv(Z_FLAG)
                EndIf
                if (c>0)  And  (fv=iif((flags And Z_FLAG),1,0)) Then 
                        temp=readmemb_386(ds0,ESI) 
                        temp2=readmemb_386(es0,EDI) 
                        if abrt Then 
                                 flags=of
                                 Exit Select  
                        EndIf
                        if (flags And D_FLAG) Then 
                                 EDI-=1
                                 ESI-=1   
                        else
                                 EDI+=1
                                 ESI+=1  
                        EndIf
                        c-=1 
                        cycles-=IIf((is486),7,9)
                        setsub8(temp,temp2) 
                EndIf
                if (c>0)  And  (fv=iif((flags And Z_FLAG),1,0)) Then 
                        firstrepcycle=0: pc=ipc : if ssegs Then ssegs+=1   
                else
                        firstrepcycle=1
                EndIf
              Exit Select
        	case &hA7  /'REP CMPSW'/
                if (fv) Then 
                        flags = flags Or Z_FLAG 
                else
                        flags = flags And inv(Z_FLAG)
                EndIf
                if (c>0)  And  (fv=iif((flags And Z_FLAG),1,0)) Then 
                        tempw=readmemw_386(ds0,SI) 
                        tempw2=readmemw_386(es0,DI) 
                        if abrt Then 
                                 flags=of
                                 Exit Select
                        EndIf
                        if (flags And D_FLAG) Then 
                                 DI-=2
                                 SI-=2   
                        else
                                 DI+=2
                                 SI+=2  
                        EndIf
                        c-=1 
                        cycles-=IIf((is486),7,9 )
                        setsub16(tempw,tempw2) 
                EndIf
                if (c>0)  And  (fv=iif((flags And Z_FLAG),1,0)) Then 
                         pc=ipc
                         firstrepcycle=0
                         if ssegs Then ssegs+=1   
                else
                        firstrepcycle=1
                EndIf
              Exit Select
        	case &h1A7  /'REP CMPSL'/
                if (fv) Then 
                        flags = flags Or Z_FLAG 
                else
                        flags = flags And inv(Z_FLAG)
                EndIf
                if (c>0)  And  (fv=iif((flags And Z_FLAG),1,0)) Then 
                        templ=readmeml_386(ds0,SI) 
                        templ2=readmeml_386(es0,DI) 
                        if abrt Then 
                                 flags=of
                                 Exit Select 
                        EndIf
                        if (flags And D_FLAG) Then 
                                 DI-=4
                                 SI-=4   
                        else
                                 DI+=4
                                 SI+=4  
                        EndIf
                        c-=1 
                        cycles-=IIf((is486),7,9)
                        setsub32(templ,templ2) 
                EndIf
                if (c>0)  And  (fv=iif((flags And Z_FLAG),1,0)) Then 
                         pc=ipc: firstrepcycle=0: if ssegs Then ssegs+=1   
                else
                        firstrepcycle=1
                EndIf
              Exit Select
        	case &h2A7  /'REP CMPSW'/
                if (fv) Then 
                        flags = flags Or Z_FLAG 
                else
                           flags = flags And inv(Z_FLAG)
                EndIf
                if (c>0)  And  (fv=iif((flags And Z_FLAG),1,0)) Then 
                        tempw=readmemw_386(ds0,ESI) 
                        tempw2=readmemw_386(es0,EDI) 
                        if abrt Then 
                                 flags=of
                                 Exit select 
                        EndIf
                        if (flags And D_FLAG) Then 
                                 EDI-=2: ESI-=2   
                        else
                                 EDI+=2: ESI+=2  
                        EndIf
                        c-=1 
                        cycles-=IIf((is486),7,9) 
                        setsub16(tempw,tempw2) 
                EndIf
                if (c>0)  And  (fv=iif((flags And Z_FLAG),1,0)) Then 
                        pc=ipc: firstrepcycle=0: if ssegs Then ssegs+=1   
                else
                        firstrepcycle=1
                EndIf
              Exit Select
        	case &h3A7  /'REP CMPSL'/
                if (fv) Then 
                        flags = flags Or Z_FLAG 
                else
                           flags = flags And inv(Z_FLAG)
                EndIf
                if (c>0)  And  (fv=iif((flags And Z_FLAG),1,0)) Then 
                        templ=readmeml_386(ds0,ESI) 
                        templ2=readmeml_386(es0,EDI) 
                        if abrt Then 
                                 flags=of
                                 Exit Select
                        EndIf
                        if (flags And D_FLAG) Then 
                              EDI-=4: ESI-=4   
                        else
                              EDI+=4: ESI+=4  
                        EndIf
                        c-=1 
                        cycles-=IIf((is486),7,9 )
                        setsub32(templ,templ2) 
                EndIf
                if (c>0)  And  (fv=iif((flags And Z_FLAG),1,0)) Then 
                        pc=ipc: firstrepcycle=0: if ssegs Then ssegs+=1   
                else
                        firstrepcycle=1
                EndIf
              Exit Select
        	case &hAA, &h1AA   /'REP STOSB'/
                if (c>0) Then 
                        writememb_386(es0,DI,AL) 
                        if abrt Then exit Select 
                        if (flags And D_FLAG) Then 
                        	DI-=1 
                        else
                           DI+=1 
                        EndIf
                        c-=1 
                        cycles-=IIf((is486),4,5)
                EndIf
                if (c>0) Then 
                        firstrepcycle=0: pc=ipc: if ssegs Then ssegs+=1   
                else
                        firstrepcycle=1
                EndIf
              Exit Select
        	case &h2AA, &h3AA  /'REP STOSB'/
                if (c>0) Then 
                        writememb_386(es0,EDI,AL) 
                        if abrt Then exit Select 
                        if (flags And D_FLAG) Then 
                        	EDI-=1 
                        else
                           EDI+=1 
                        EndIf
                        c-=1 
                        cycles-=iif((is486),4,5) 
                EndIf
                if (c>0) Then 
                        firstrepcycle=0: pc=ipc: if ssegs Then ssegs+=1   
                else
                        firstrepcycle=1
                EndIf
              Exit Select
        	Case &hAB  /'REP STOSW'/
                if (c>0) Then 
                        writememw_386(es0,DI,AX) 
                        if abrt Then exit Select 
                        if (flags And D_FLAG) Then 
                        	DI-=2 
                        else
                            DI+=2 
                        EndIf
                        c-=1 
                        cycles-=iif((is486),4,5) 
                EndIf
                if (c>0) Then 
                        firstrepcycle=0: pc=ipc: if ssegs Then ssegs+=1   
                else
                        firstrepcycle=1
                EndIf
              Exit Select
        	case &h2AB  /'REP STOSW'/
                if (c>0) Then 
                        writememw_386(es0,EDI,AX) 
                        if abrt Then exit Select 
                        if (flags And D_FLAG) Then 
                        	EDI-=2 
                        else
                           EDI+=2 
                        EndIf
                        c-=1 
                        cycles-=iif((is486),4,5) 
                EndIf
                if (c>0) Then 
                        firstrepcycle=0: pc=ipc: if ssegs Then ssegs+=1   
                else
                        firstrepcycle=1
                EndIf
              Exit Select
        	case &h1AB  /'REP STOSL'/
                if (c>0) Then 
                        writememl_386(es0,DI,EAX) 
                        if abrt Then exit Select 
                        if (flags And D_FLAG) Then 
                        	DI-=4 
                        else
                           DI+=4 
                        EndIf
                        c-=1 
                        cycles-=iif((is486),4,5) 
                EndIf
                if (c>0) Then 
                        firstrepcycle=0: pc=ipc: if ssegs Then ssegs+=1   
                else
                        firstrepcycle=1
                EndIf
              Exit Select
        	case &h3AB  /'REP STOSL'/
                if (c>0) Then 
                        writememl_386(es0,EDI,EAX) 
                        if abrt Then exit Select 
                        if (flags And D_FLAG) Then 
                        	EDI-=4 
                        else
                           EDI+=4 
                        EndIf
                        c-=1 
                        cycles-=iif((is486),4,5) 
                EndIf
                if (c>0) Then 
                        firstrepcycle=0: pc=ipc: if ssegs Then ssegs+=1   
                else
                        firstrepcycle=1
                EndIf
              Exit Select
        	case &hAC, &h1AC   /'REP LODSB'/
                if (c>0) Then 
                        AL=readmemb_386(ds0,SI) 
                        if abrt Then exit Select 
                        if (flags And D_FLAG) Then 
                        	SI-=1 
                        else
                           SI+=1 
                        EndIf
                        c-=1 
                        cycles-=5 
                EndIf
                if (c>0) Then 
                         firstrepcycle=0: pc=ipc: if ssegs Then ssegs+=1   
                else
                        firstrepcycle=1
                EndIf
              Exit Select
        	case &h2AC, &h3AC  /'REP LODSB'/
                if (c>0) Then 
                        AL=readmemb_386(ds0,ESI) 
                        if abrt Then exit Select 
                        if (flags And D_FLAG) Then 
                        	ESI-=1 
                        else
                            ESI+=1 
                        EndIf
                        c-=1 
                        cycles-=5 
                EndIf
                if (c>0) Then 
                        firstrepcycle=0: pc=ipc: if ssegs Then ssegs+=1   
                else
                        firstrepcycle=1
                EndIf
              Exit Select
        	case &hAD   /'REP LODSW'/
                if (c>0) Then 
                        AX=readmemw_386(ds0,SI) 
                        if abrt Then exit Select 
                        if (flags And D_FLAG) Then 
                        	SI-=2 
                        else
                           SI+=2 
                        EndIf
                        c-=1 
                        cycles-=5 
                EndIf
                if (c>0) Then 
                        firstrepcycle=0: pc=ipc: if ssegs Then ssegs+=1   
                else
                        firstrepcycle=1
                EndIf
              Exit Select
        	case &h1AD  /'REP LODSL'/
                if (c>0) Then 
                        EAX=readmeml_386(ds0,SI) 
                        if abrt Then exit Select 
                        if (flags And D_FLAG) Then 
                        	SI-=4 
                        else
                           SI+=4 
                        EndIf
                        c-=1 
                        cycles-=5 
                EndIf
                if (c>0) Then 
                        firstrepcycle=0: pc=ipc: if ssegs Then ssegs+=1   
                else
                        firstrepcycle=1
                EndIf
              Exit Select
        	case &h2AD  /'REP LODSW'/
                if (c>0) Then 
                        AX=readmemw_386(ds0,ESI) 
                        if abrt Then exit Select 
                        if (flags And D_FLAG) Then 
                        	ESI-=2 
                        else
                           ESI+=2 
                        EndIf
                        c-=1 
                        cycles-=5 
                EndIf
                if (c>0) Then 
                        firstrepcycle=0: pc=ipc: if ssegs Then ssegs+=1   
                else
                        firstrepcycle=1
                EndIf
              Exit Select
        	case &h3AD  /'REP LODSL'/
                if (c>0) Then 
                        EAX=readmeml_386(ds0,ESI) 
                        if abrt Then exit Select 
                        if (flags And D_FLAG) Then 
                        	ESI-=4 
                        else
                           ESI+=4 
                        EndIf
                        c-=1 
                        cycles-=5 
                EndIf
                if (c>0) Then 
                        firstrepcycle=0: pc=ipc: if ssegs Then ssegs+=1   
                else
                        firstrepcycle=1
                EndIf
              Exit Select
        	case &hAE, &h1AE   /'REP SCASB'/
                if (fv) Then 
                        flags = flags Or Z_FLAG 
                else
                        flags = flags And inv(Z_FLAG)
                EndIf
                if (c>0)  And  (fv=iif((flags And Z_FLAG),1,0)) Then 
                        temp2=readmemb_386(es0,DI) 
                        if abrt Then 
                                 flags=of
                                 Exit Select 
                        EndIf
                        setsub8(AL,temp2) 
                        if (flags And D_FLAG) Then 
                        	DI-=1 
                        else
                           DI+=1 
                        EndIf 
                        c-=1 
                        cycles-=iif((is486),5,8 )
                EndIf
                if (c>0)  And  (fv=iif((flags And Z_FLAG),1,0)) Then 
                        pc=ipc: firstrepcycle=0: if ssegs Then ssegs+=1   
                else
                        firstrepcycle=1
                EndIf
              Exit Select
        	case &h2AE, &h3AE  /'REP SCASB'/
                if (fv) Then 
                        flags = flags Or Z_FLAG 
                else
                        flags = flags And inv(Z_FLAG)
                EndIf
                if (c>0)  And  (fv=iif((flags And Z_FLAG),1,0)) Then 
                        temp2=readmemb_386(es0,EDI) 
                        if abrt Then 
                                 flags=of
                                 Exit select  
                        EndIf
                        setsub8(AL,temp2) 
                        if (flags And D_FLAG) Then 
                        	EDI-=1 
                        else
                            EDI+=1 
                        EndIf
                        c-=1 
                        cycles-=iif((is486),5,8 )
                EndIf
                if (c>0)  And  (fv=iif((flags And Z_FLAG),1,0)) Then 
                        pc=ipc: firstrepcycle=0: if ssegs Then ssegs+=1   
                else
                        firstrepcycle=1
                EndIf
              Exit Select
        	case &hAF   /'REP SCASW'/
                if (fv) Then 
                        flags = flags Or Z_FLAG 
                else
                        flags = flags And inv(Z_FLAG)
                EndIf
                if (c>0)  And  (fv=iif((flags And Z_FLAG),1,0)) Then 
                        tempw=readmemw_386(es0,DI) 
                        if abrt Then 
                                 flags=of
                                 Exit select   
                        EndIf
                        setsub16(AX,tempw) 
                        if (flags And D_FLAG) Then 
                        	DI-=2 
                        else
                           DI+=2 
                        EndIf
                        c-=1 
                        cycles-=iif((is486),5,8 )
                EndIf
                if (c>0)  And  (fv=iif((flags And Z_FLAG),1,0)) Then 
                        pc=ipc: firstrepcycle=0: if ssegs Then ssegs+=1   
                else
                        firstrepcycle=1
                EndIf
              Exit Select
        	case &h1AF  /'REP SCASL'/
                if (fv) Then 
                        flags = flags Or Z_FLAG 
                else
                        flags = flags And inv(Z_FLAG)
                EndIf
                if (c>0)  And  (fv=iif((flags And Z_FLAG),1,0)) Then 
                        templ=readmeml_386(es0,DI) 
                        if abrt Then 
                                 flags=of
                                 Exit select   
                        EndIf
                        setsub32(EAX,templ) 
                        if (flags And D_FLAG) Then 
                        	DI-=4 
                        else
                            DI+=4 
                        EndIf 
                        c-=1 
                        cycles-=iif((is486),5,8) 
                EndIf
                if (c>0)  And  (fv=iif((flags And Z_FLAG),1,0)) Then 
                        pc=ipc: firstrepcycle=0: if ssegs Then ssegs+=1   
                else
                        firstrepcycle=1
                EndIf
              Exit Select
        	case &h2AF  /'REP SCASW'/
                if (fv) Then 
                        flags = flags Or Z_FLAG 
                else
                        flags = flags And inv(Z_FLAG)
                EndIf
                if (c>0)  And  (fv=iif((flags And Z_FLAG),1,0)) Then 
                        tempw=readmemw_386(es0,EDI) 
                        if abrt Then 
                                 flags=of
                                 Exit select   
                        EndIf
                        setsub16(AX,tempw) 
                        if (flags And D_FLAG) Then 
                        	EDI-=2 
                        else
                           EDI+=2 
                        EndIf
                        c-=1 
                        cycles-=iif((is486),5,8 )
                EndIf
                if (c>0)  And  (fv=iif((flags And Z_FLAG),1,0)) Then 
                        pc=ipc: firstrepcycle=0: if ssegs Then ssegs+=1   
                else
                        firstrepcycle=1
                EndIf
              Exit Select
        	case &h3AF  /'REP SCASL'/
                if (fv) Then 
                        flags = flags Or Z_FLAG 
                else
                        flags = flags And inv(Z_FLAG)
                EndIf
                if (c>0)  And  (fv=iif((flags And Z_FLAG),1,0)) Then 
                        templ=readmeml_386(es0,EDI) 
                        if abrt Then 
                                 flags=of
                                 Exit select  
                        EndIf
                        setsub32(EAX,templ) 
                        if (flags And D_FLAG) Then 
                        	EDI-=4 
                        else
                             EDI+=4 
                        EndIf
                        c-=1 
                        cycles-=iif((is486),5,8 )
                EndIf
                if (c>0)  And  (fv=iif((flags And Z_FLAG),1,0)) Then 
                        pc=ipc: firstrepcycle=0: if ssegs Then ssegs+=1   
                else
                        firstrepcycle=1
                EndIf
              Exit Select
        	Case else
                pc=ipc 
                cycles-=20 
                x86illegal() 
                If deb=3 Then Print #5,"Bad REP ",temp,rep32 Shr 8
        End Select
        
        
       if (rep32 And &h200) Then 
            ECX=c 
       Else
            CX=c
       EndIf
       if (changeds) Then ds0=oldds 
End Sub




#Include "386c.bas"