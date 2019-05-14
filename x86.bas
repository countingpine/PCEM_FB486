


Sub x86_int(ByVal num As Integer ) 
	
        Dim As ULong  addr 
        pc=oldpc 
        if modoprotegido Then 
                pmodeint(num,0) 
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
                addr=num Shl 2 
                flags = flags And inv(I_FLAG) 
                flags = flags And inv(T_FLAG) 
                oxpc=pc 
                pc=readmemw_386(0,addr) 
                loadcs(readmemw_386(0,addr+2)) 
        EndIf
        cycles-=70 
        
End Sub



' esta rutina viene del modulo 286.C que yo no empleo, pero no se si debe ser asi.
Sub x86illegal() 
        Dim As UShort  addr 
        'Print #5,"x86 ilegal MSW,CR0,CS,PC,OPCODE:";Hex(msw,4);",";Hex(cr0,8);",";Hex(cs0,4);",";Hex(pc,8);",";Hex(opcode,2)
        if modoprotegido Then 
                pmodeint(6,0) 
        else
                If ssegs Then 
                      ss0=oldss
                      _ss.limit=oldsslimit
                      _ss.limitw=oldsslimitw  
                EndIf
                writememw_386(ss0,((SP-2) And &hFFFF),flags) 
                writememw_386(ss0,((SP-4) And &hFFFF),CS1) 
                writememw_386(ss0,((SP-6) And &hFFFF),pc) 
                SP-=6 
                flags = flags And inv(I_FLAG) 
                flags = flags And inv(T_FLAG) 
                addr=6 Shl 2 
                pc=readmemw_386(0,addr) 
                loadcs(readmemw_386(0,addr+2)) 
        EndIf
        cycles-=70 
End Sub


' deteccion del tipo de CPU: LO DEJO SOLO PARA UN i486 (instruccion &hA2)
Sub cpu_CPUID()
	' aqui solo llega si CPUID<>0, como es el caso de las 486DX2-66
       If (EAX = 0) Then
         EAX = &h00000001
         EBX = &h756e6547
         EDX = &h49656e69
         ECX = &h6c65746e
       elseif (EAX = 1) then
         EAX = CPUID
         EBX = 0
         ECX = 0
         EDX = 1' FPU=SI
       Else
         EAX = 0
       EndIf
End sub



' inicializa CPU
Sub resetx86(reset_cpu As Integer) 
	Dim a As Integer
	
	' inicializa puertos a FF al completo 
	For a=0 To 65535
		puertosb(a)=&hff
		puertosw(a)=&hffff
		puertosl(a)=&hffffffff
	Next
	
		'resets+=1 
		ins = 0 
		use32=0 
		stack32=0 
		pc=0 
		msw=0 
		cr0=0 
		eflags=0 
		cgate32=0 
		loadcs(&hFFFF) 
		rammask=&hFFFFFFFF 
		flags=2 
		
		initmmucache() ' CPU cache
		resetreadlookup() ' look ahead CPU
		
		makemod1table() 
		'x87_reset() ' no es necesario
		 
		EDX=reset_cpu ' cpu_set_edx()= reset EDX segun tabla de tipo CPU (0x303=386, 0x430=486)
		
		ESP=0 
		mmu_perm=4 		
		
		' esto es del PREFETCHCLEAR que estaba en el X86.C pero ¿¿¿SOLO se emplea en el Reset????
        'prefetchpc=pc
        'prefetchw=0
        'memcycs=cycdiff-cycles
        'fetchclocks=0		
End Sub



' esta rutina la usan el teclado (ctrl+alt+sup) la INS HALT, o las INS que generan errores graves.
' es para problemas gordos, que obligan a reininiar si o si
Sub softresetx86() 
	'Print "EL PC SE REINICIA!!!!":Sleep
        use32=0 
        stack32=0 
        pc=0 
        msw=0 
        cr0=0 
        eflags=0 
        cgate32=0 
        loadcs(&hFFFF) 
        flags=2 
End Sub



' primero de todo, inicializa PC
Sub init_PC(bios As String)
	keyboard_at_reset()


	' inicializa los registros CRTC de la VGA estandar (pone a cero el resto)
	resetvideo()
	initega()
	et4000_init() 
	svga_init()
	
	'et4000w32p_init() ' este llama a su vez a svga_init()
	'et4000w32_reset()

	loadCMOS(bios) ' carga el fichero "xxxxxxxx.nvr" de la CMOS

		''''''''''
		'pc_reset'
		''''''''''    
	     cpu_multi=2 ' multiplicador BUS

        ' comun en 386/486
        CPUID=&h433 ' INTEL DX2-66:ver los modelos en "http://datasheets.chipdb.org/Intel/x86/486/Intel486.htm"
        
        ' esto es solo para el 486, por ahora, suficiente
        	' accesos CPU i486DX
			timing_rr  = 1 'register dest - register src
			timing_rm  = 2 'register dest - memory src
			timing_mr  = 3 'memory dest   - register src
			timing_mm  = 3 'memory dest   - memory src
			timing_rml = 2 'register dest - memory src long
			timing_mrl = 3 'memory dest   - register src long
			timing_mml = 3 'memory dest   - memory src
			timing_bt  = 2 'branch taken
			timing_bnt = 1 'branch 0 =taken
        
        '''''''''''''''''''
        resetx86(CPUID)
        '''''''''''''''''''
        
        dma_reset()
        'fdc_reset()
        'lpt_reset() ' este creo que no existe, sino en su lugar esta LPT_INIT() que dclara los IO_SETHANDLER
        pic_reset()
        pit_reset()
        serial_reset()
        'serial1_init(&h3f8)
        'serial1_init(&h2f8)
        'mouse_serial_init()
        'mouse_serial_rcr()
        
        setpitclock(66666666) ' rspeed (aqui arriba) es la velocidad REAL de la CPU (66mhz=66666666=66.666.666)
		  
        'ali1429_reset()	
	'''''''''''''''''''''''''''''''''
     
	'install_int_ex(onesec,BPS_TO_TIMER(1)) ' ni idea, pero en WIN-timer.C esta vacio, declarado sin nada
	
	mem_updatecache()
	ali1429_reset()

	shadowbios=0
 
	makeznptable() 
	initoplist() ' solo para el DEBUG, la lista de OP del x86 de momento
End Sub








''''''''''''''''''''''''''''''''''''''''''''''''''''''''''
' estas rutinas son SOLO para el modulo DMA, y solo hay una llamada

'Sub FETCHCOMPLETE()
'        if (fetchcycles And 3)=0 Then return 
'        if prefetchw>3 Then return 'iif((is8086),4,3)) <<--viejo
'        if prefetchw=0 Then nextcyc=(4-(fetchcycles And 3)) 
'        cycles-=(4-(fetchcycles And 3)) 
'        fetchclocks+=(4-(fetchcycles And 3)) 
'        
'       'if (is8086  And   0 =(prefetchpc And 1)) Then 
'       '        prefetchqueue(prefetchw)=readmembf_x86(cs0+prefetchpc) ' readmembf_x86 es exclusivo de FETCHCOMPLETE, nadie mas lo usa
'       '        prefetchpc+=1 
'       '        prefetchw+=1 
'       'EndIf
'       
'       if (prefetchw<6) Then 
'               prefetchqueue(prefetchw)=readmembf_x86(cs0+prefetchpc) ' readmembf_x86 es exclusivo de FETCHCOMPLETE, nadie mas lo usa
'               prefetchpc+=1 
'               prefetchw+=1 
'       EndIf
'       fetchcycles+=(4-(fetchcycles And 3)) 
'End Sub

'Sub refreshread()  'rutina para DMA , llama a FETCHCOMPLETE  
'  FETCHCOMPLETE()
'  memcycs+=4
'End Sub
' ''''''''''''''''''''''''''''''''''''''''''''''''''''''''''




Sub fetcheal32sib() 
        Dim As UByte  sib 
        sib=rmdat Shr 8
        Select Case As Const  (modo)
        	case 0  
        		eaaddr=regs(sib And 7).l
        		pc+=1
        	case 1  
        		eaaddr=CULng(CByte(rmdat Shr 16))+regs(sib And 7).l
        		pc+=2
        	case 2  
        		eaaddr=(fastreadl(cs0+pc+1))+regs(sib And 7).l
        		pc+=5
        End Select
        'SIB byte present
        if ((sib And 7)=5) And (modo=0) Then 
            eaaddr=getlong() 
        elseif (sib And 6)=4 Then
				easeg=ss0 
            ea_rseg=SS1 
        EndIf
        if ((sib Shr 3) And 7) <> 4 Then eaaddr+=(regs((sib Shr 3) And 7).l) Shl (sib Shr 6) 
End Sub

Sub fetcheal32nosib() 
        if (rm=5) Then 
                easeg=ss0 
                ea_rseg=SS1 
        EndIf
        if (modo=1) Then 
                 eaaddr+=CULng(CByte(rmdat Shr 8))
                 pc+=1   
        else
                 eaaddr+=getlong()  
        EndIf
End Sub

Sub fetchea32() 
     eal_r = 0'NULL
     eal_w = 0'NULL

     if (op32 And &h200) Then 
	       pc+=1 
	       
	       reg=(rmdat Shr 3) And 7 
	       modo=(rmdat Shr 6) And 3 
	       rm=rmdat And 7 
	       
	       if (modo<>3) Then 
	         easeg=ds0 
	         ea_rseg=DS1 
	         if (rm=4) Then 
	           fetcheal32sib() 
	         else
	           eaaddr=regs(rm).l 
	           if (modo) Then 
	               fetcheal32nosib() 
	           ElseIf (rm=5) Then
						eaaddr=getlong()
	           EndIf
	         EndIf
	         if (easeg <> &hFFFFFFFF) And (((easeg + eaaddr) And &hFFF) <= &hFFC) Then 
	           If  readlookup2[(easeg + eaaddr) Shr 12] <> &hFFFFFFFF Then 
	           		eal_r = @ram[ readlookup2[(easeg + eaaddr) Shr 12] + ((easeg + eaaddr) And &hFFF)] 
	           EndIf
	           If writelookup2[(easeg + eaaddr) Shr 12] <> &hFFFFFFFF Then 
	           		eal_w = @ram[writelookup2[(easeg + eaaddr) Shr 12] + ((easeg + eaaddr) And &hFFF)] 
	           EndIf
	         EndIf
	       EndIf
     Else
	       pc+=1
	       
	       reg=(rmdat Shr 3) And 7 
	       modo=(rmdat Shr 6) And 3 
	       rm=rmdat And 7 
	       
	       if (modo<>3) Then 
	         if (modo=0) And (rm=6) Then 
		            eaaddr=(rmdat Shr 8) And &hFFFF
		            pc+=2
		            easeg=ds0
		            ea_rseg=DS1   
	         Else
			          Select Case As Const  (modo)
			          	case 0 
			               eaaddr=0 
			          	Case 1 
			               eaaddr=CUShort(CByte(rmdat Shr 8))
			               pc+=1 
			          	Case 2 
			               eaaddr=getword() 
			          End Select       
			          
			          ' coge la direccion de un registro, segun la tabla "makemod1table"
			          eaaddr+=((*mod1add(0, rm))+(*mod1add(1, rm))) 
			          easeg=*mod1seg(rm) 
			          If mod1seg(rm)= @ss0 Then 
			              ea_rseg=SS1 
			          Else
			              ea_rseg=DS1
			          EndIf
			          eaaddr = eaaddr And &hFFFF 
	         EndIf
	         if (easeg <> &hFFFFFFFF) And (((easeg + eaaddr) And &hFFF) <= &hFFC) Then 
	           If  readlookup2[(easeg + eaaddr) Shr 12] <> &hFFFFFFFF Then 
	           		eal_r = @ram[ readlookup2[(easeg + eaaddr) Shr 12] + ((easeg + eaaddr) And &hFFF)] 
	           EndIf
	           If writelookup2[(easeg + eaaddr) Shr 12] <> &hFFFFFFFF Then 
	           		eal_w = @ram[writelookup2[(easeg + eaaddr) Shr 12] + ((easeg + eaaddr) And &hFFF)] 
	           EndIf
	         EndIf
	       EndIf
     EndIf

End Sub






sub fetchea2()
	rmdat=fastreadl(cs0+pc)
   fetchea32()
	'if abrt break : al salir, debe romper el "select case"
End Sub




' tabla de almacen de registros, para luego acceder a ellos mediante punteros
Sub makemod1table() 
        mod1add(0, 0)= @BX: mod1add(0, 1)= @BX: mod1add(0, 2)= @BP: mod1add(0, 3)= @BP 
        mod1add(0, 4)= @SI: mod1add(0, 5)= @DI: mod1add(0, 6)= @BP: mod1add(0, 7)= @BX 
        mod1add(1, 0)= @SI: mod1add(1, 1)= @DI: mod1add(1, 2)= @SI: mod1add(1, 3)= @DI 

        mod1add(1, 4)= @zero: mod1add(1, 5)= @zero: mod1add(1, 6)= @zero: mod1add(1, 7)= @zero 
        
        slowrm(0)=0: slowrm(1)=1: slowrm(2)=1: slowrm(3)=0 
        
        mod1seg(0)= @ds0: mod1seg(1)= @ds0: mod1seg(2)= @ss0: mod1seg(3)= @ss0 
        mod1seg(4)= @ds0: mod1seg(5)= @ds0: mod1seg(6)= @ss0: mod1seg(7)= @ds0 
End Sub

                   
/'Flags'/
Sub makeznptable() 
        Dim As Integer c,d 
        'Print"Creando tabla Zero-Not-Parity"
        for  c=0 To 255
                d=0 
                if (c And 1) Then d+=1 
                if (c And 2) Then d+=1 
                if (c And 4) Then d+=1 
                if (c And 8) Then d+=1 
                if (c And 16) Then d+=1 
                if (c And 32) Then d+=1 
                if (c And 64) Then d+=1 
                if (c And 128) Then d+=1 
                if (d And 1) Then 
                   znptable8(c)=0 
                else
                   znptable8(c)=P_FLAG 'P_FLAG=&h0004
                EndIf
                'If (c = &hb1) Then print "znp8 b1 ", d, znptable8(c)
                if (c =0) Then znptable8(c) Or= Z_FLAG 'Z_FLAG=&h0040
                if (c And &h80) Then znptable8(c) or= N_FLAG 'N_FLAG=&h0080
        Next
        for  c=0 To  65535 
                d=0 
                if (c And 1) Then d+=1 
                if (c And 2) Then d+=1 
                if (c And 4) Then d+=1 
                if (c And 8) Then d+=1 
                if (c And 16) Then d+=1 
                if (c And 32) Then d+=1 
                if (c And 64) Then d+=1 
                if (c And 128) Then d+=1 
                if (d And 1) Then 
                   znptable16(c)=0 
                else
                   znptable16(c)=P_FLAG
                EndIf
                'If (c = &hb1) Then print "znp16 b1 ", d, znptable16(c) 
                'if (c = &h65b1) Then print "znp16 65b1 ", d, znptable16(c) 
                if (c =0) Then znptable16(c) or= Z_FLAG 
                if (c And &h8000) Then znptable16(c) or= N_FLAG 
        Next

End Sub



function NOTRM() As Byte
	if (modoprotegido=0) Or ((eflags and VM_FLAG)<>0) Then x86_int(6): Return 1
	Return 0
End Function
