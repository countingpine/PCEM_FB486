
Dim Shared as Integer mem_a20_alt=0
Dim Shared as Integer mem_a20_key=0
static Shared As Integer mem_a20_state = 0

'Dim Shared as Integer readlnum=0,writelnum=0
Dim Shared as Integer nopageerrors
Dim Shared as Integer cache=4 ' en la rutina "mem_updatecache" 4=256k, el maximo para un 486 dx266
Dim Shared as Integer memwaitstate
Dim Shared as ULong biosmask=&hFFFF ' BIOS de 64k
Dim Shared as Integer cachesize=256
'Dim Shared as UByte romext(32768)
Dim Shared as ULong mmucache(&h100000)
Dim Shared as Integer mmucaches(64)
Dim Shared as Integer mmunext=0
Dim Shared as Integer mmuflush=0
Dim Shared as Integer pctrans=0


static Shared as ULong mem_logical_addr


Declare Sub addwritelookup(virt As ULong , phys As ULong ) 
Declare Sub addreadlookup (virt As ULong , phys As ULong )



sub loadbinary(memaddr as ULong, filename as String, mem As UByte Ptr)
	if Dir(filename) = "" then print "No existe:";filename:Sleep:end
	Dim handle As ULong= FreeFile
	Dim tmpdata As String=""
	Dim n As ULong

	open filename for binary as #handle
	tmpdata = Space(lof(handle))
	get #handle, , tmpdata
	for n = 1 to len(tmpdata)
		mem[memaddr + n - 1] = asc(Mid(tmpdata, n, 1))
	next n
	close handle
end Sub



''''''''''''
Function leeram1(ByVal addr As ULong) As UByte

	' no se si esto es bueno, pero evito que se salga de la RAM total
	'addr=addr And rammask
	If addr > RAM_TOTAL Then Return &hff

	If addr>=&h000000 And addr<=&h09FFFF Then Return ram[addr] ' 640K RAM 
	'
	If addr>=&h0A0000 And addr<=&h0AFFFF Then Return svga_read(addr)' 64K VRAM VGA
	If addr>=&h0B0000 And addr<=&h0B7FFF Then Return svga_read(addr)' 32K VRAM MDA
	If addr>=&h0B8000 And addr<=&h0BFFFF Then Return svga_read(addr)' 32K VRAM CGA
	'
	If addr>=&h0C0000 And addr<=&H0C7FFF Then Return vrom[addr And &h7fff] ' 32K VROM VIDEO
	If addr>=&h0C8000 And addr<=&H0CFFFF Then Return &hff 'vrom[addr And &h7fff] ' 32K VROM VIDEO EXPANSION ( POR EJEMPLO, VIDEO ROM 64K)
	If addr>=&h0D0000 And addr<=&H0DFFFF Then Return ram[addr] ' 64k RAM zona alta --NO SE USAN?????? 
	If addr>=&h0E0000 And addr<=&H0EFFFF Then Return rom[addr And biosmask] ' 64K ROM BIOS EXPANSION

   If addr>=&h0F0000 And addr<=&H0FFFFF Then 
   	If shadowbios=0 Then Return rom[addr And biosmask]
	   Return ram[addr]
   EndIf
	
	If addr>=&h100000 And addr<=RAM_TOTAL Then Return ram[addr] ' RAM EXTENDIDA, DESDE 1mb

	' no deberia llegar aqui, si lo hace, algo va mal
	Print #5,"RAM leer   fuera rango:";Hex(addr,8)
	Return &hff
End Function
Function leeram2(ByVal addr As ULong) As UShort
	
	'If addr>=&h0A0000 And addr<=&h0AFFFF Then Return svga_readw(addr) ' 64K VRAM VGA 
	'If addr>=&h0B0000 And addr<=&h0B7FFF Then Return svga_readw(addr) ' 32K VRAM MDA
	'If addr>=&h0B8000 And addr<=&h0BFFFF Then Return svga_readw(addr) ' 32K VRAM CGA 	

   If addr>=&h0F0000 And addr<=&H0FFFFF Then 
   	If shadowbios=0 Then Return rom[addr And biosmask] Or (rom[(addr And biosmask)+1] Shl 8)
	   Return ram[addr] Or (ram[addr+1] Shl 8) 
   EndIf
		
  Return leeram1(addr) Or (leeram1(addr+1) Shl 8) 	
End Function
Function leeram4(ByVal addr As ULong) As ULong

	'If addr>=&h0A0000 And addr<=&h0AFFFF Then Return svga_readl(addr) ' 64K VRAM VGA 
	'If addr>=&h0B0000 And addr<=&h0B7FFF Then Return svga_readl(addr) ' 32K VRAM MDA
	'If addr>=&h0B8000 And addr<=&h0BFFFF Then Return svga_readl(addr) ' 32K VRAM CGA

   If addr>=&h0F0000 And addr<=&H0FFFFF Then 
   	If shadowbios=0 Then Return rom[addr And biosmask] Or (rom[(addr And biosmask)+1] Shl 8) Or (rom[(addr And biosmask)+2] Shl 16) Or (rom[(addr And biosmask)+3] Shl 24)
	   Return ram[addr] Or (ram[addr+1] Shl 8) Or (ram[addr+2] Shl 16) Or (ram[addr+3] Shl 24)
   EndIf
	
  Return leeram1(addr) Or (leeram1(addr+1) Shl 8) Or (leeram1(addr+2) Shl 16) Or (leeram1(addr+3) Shl 24)
End Function

''''''''''''''
sub grabaram1(ByVal addr As ULong, ByVal valor As ubyte)

   ' no se si esto es bueno, pero evito que se salga de la RAM total
	'addr=addr And rammask
	If addr > RAM_TOTAL Then Return 
	                           
	If addr>=&h000000 And addr<=&h09FFFF Then ram[addr]=valor: Return ' 640k ram
	'
	If addr>=&h0A0000 And addr<=&h0AFFFF Then svga_write(addr,valor):Return ' 64k vram vga
	If addr>=&h0B0000 And addr<=&h0B7FFF Then svga_write(addr,valor):Return ' 32k VRAM MDA, CGA, EGA
	If addr>=&h0B8000 And addr<=&h0BFFFF Then svga_write(addr,valor):Return ' 32k vram cga
	'
	If addr>=&h0C0000 And addr<=&H0C7FFF Then Return ' 32k vrom video
	If addr>=&h0C8000 And addr<=&H0CFFFF Then Return ' 32k vrom video
	If addr>=&h0D0000 And addr<=&H0DFFFF Then ram[addr]=valor: Return ' 64k ram alta --NO SE USAN??????
	If addr>=&h0E0000 And addr<=&H0EFFFF Then Return ' 64k rom expansion
	
	If addr>=&h0F0000 And addr<=&H0FFFFF Then
		If modo_bios=0 Or modo_bios=1 Then return
		   If shadowbios_write Then 
		   	ram[addr]=valor ' 64K ROM BIOS (las rom empiezan de FFFFF hacia abajo) FFFF0 vector de salto
		   EndIf
		return
	EndIf
	
	' expansion de RAM
	If addr>=&h100000 And addr<=RAM_TOTAL Then ram[addr]=valor: Return
	
	' no deberia llegar aqui, si lo hace, algo va mal
	Print #5,"RAM grabar fuera rango:";Hex(addr,8),Hex(valor,2)

End Sub
sub grabaram2(ByVal addr As ULong, ByVal valor As UShort)

	'If addr>=&h0A0000 And addr<=&h0AFFFF Then svga_writew(addr,valor):Return
	'If addr>=&h0B0000 And addr<=&h0B7FFF Then svga_writew(addr,valor):Return
	'If addr>=&h0B8000 And addr<=&h0BFFFF Then svga_writew(addr,valor):Return

	If addr>=&h0F0000 And addr<=&H0FFFFF Then
		If modo_bios=0 Or modo_bios=1 Then Return
		   If shadowbios_write Then 
			 ram[addr]=valor And &hff
			 ram[addr+1]=(valor Shr 8) And &hff
		   EndIf
		return
	EndIf
	
	 grabaram1(addr,valor And &hff)
	 grabaram1(addr+1,(valor Shr 8) And &hff)
End Sub
sub grabaram4(ByVal addr As ULong, ByVal valor As ULong)
	
	'If addr>=&h0A0000 And addr<=&h0AFFFF Then svga_writel(addr,valor):Return 
	'If addr>=&h0B0000 And addr<=&h0B7FFF Then svga_writel(addr,valor):Return 
	'If addr>=&h0B8000 And addr<=&h0BFFFF Then svga_writel(addr,valor):Return

	If addr>=&h0F0000 And addr<=&H0FFFFF Then
		If modo_bios=0 Or modo_bios=1 Then return
		   If shadowbios_write Then 
			 ram[addr]=valor And &hff
			 ram[addr+1]=(valor Shr 8) And &hff
			 ram[addr+2]=(valor Shr 16) And &hff
			 ram[addr+3]=(valor Shr 24) And &hff
		   EndIf
		return
	EndIf
	
	 grabaram1(addr,valor And &hff)
	 grabaram1(addr+1,(valor Shr  8) And &hff)
	 grabaram1(addr+2,(valor Shr 16) And &hff)
	 grabaram1(addr+3,(valor Shr 24) And &hff)
End Sub

'''''''''''''

Sub initmmucache() 
        Dim As Integer c 
        for  c=0 To &hFFFFF
        		mmucache(c)=&hFFFFFFFF
        Next
        for  c=0 To 63
        		mmucaches(c)=&hFFFFFFFF
        Next
        mmunext=0 
End Sub

Sub resetreadlookup() 
        Dim As Integer c 
        For  c=1 To 1024*1024 '*4 'Step 4 '?? creo que no, por que "readlookup2 ya es de 4 bytes)
        		readlookup2[c-1]=&hFFFFFFFF
        Next 
        for  c=0 To 255  
        		readlookup(c)=&hFFFFFFFF
        Next
        readlnext=0 
        For  c=1 To 1024*1024 '*4 'Step 4'?? creo que no, por que "writelookup2 ya es de 4 bytes)
        		writelookup2[c-1]=&hFFFFFFFF
        Next 
        for  c=0 To 255  
        		writelookup(c)=&hFFFFFFFF
        Next
        writelnext=0 
        pccache=&hFFFFFFFF 
        
        ' este lo pongo yo
        'For  c=1 To 1024*1024
        '		cachelookup2[c-1]=&hFF
        'Next 
End Sub


Sub flushmmucache() 
        Dim As integer c 
        for  c=0 To 255 
                if (readlookup(c)<>&hFFFFFFFF) Then 
                        readlookup2[readlookup(c)]=&hFFFFFFFF 
                        readlookup(c)=&hFFFFFFFF 
                EndIf
                if (writelookup(c)<>&hFFFFFFFF) Then 
                        writelookup2[writelookup(c)]=&hFFFFFFFF 
                        writelookup(c)=&hFFFFFFFF 
                EndIf
        Next
        mmuflush+=1 
        pccache=&hFFFFFFFF 
        
        'c=&hFFFFFFFF  ' es mejor asi??? (para que el puntero PCCACHE2 no apunte al infinito)
        pccache2=&hFFFFFFFF 
        
        for  c=0 To 63
                if (mmucaches(c)<>&hFFFFFFFF) Then 
                        mmucache(mmucaches(c))=&hFFFFFFFF 
                        mmucaches(c)=&hFFFFFFFF 
                EndIf
       Next

End Sub


Sub flushmmucache_cr3()
        Dim As Integer c
        for  c=0 To 255 
                if (readlookup(c)<>&hFFFFFFFF) Then 
                        readlookup2[readlookup(c)]=&hFFFFFFFF 
                        readlookup(c)=&hFFFFFFFF 
                EndIf
                if (writelookup(c)<>&hFFFFFFFF) Then 
                        writelookup2[writelookup(c)]=&hFFFFFFFF 
                        writelookup(c)=&hFFFFFFFF 
                EndIf
        Next
        for  c=0 To 63 
                if (mmucaches(c)<>&hFFFFFFFF) Then 
                        mmucache(mmucaches(c))=&hFFFFFFFF 
                        mmucaches(c)=&hFFFFFFFF 
                EndIf
        Next
End Sub


Sub mem_updatecache() 
        flushmmucache() 
        'if ( 0 =is386) Then ' solo para8086
        '        cachesize=256 
        '        memwaitstate=0 
        '        return 
        'EndIf
        'if (cpu_16bitbus) Then 
        '   memwaitstate = 512 * cpu_multi
        'else
           memwaitstate = 384 * cpu_multi
        'EndIf
      'Print #5,cache
        Select Case As Const  (cache)
        	case 0  
        		cachesize=32
        	case 1  
        		cachesize=64 
        	case 2  
        		cachesize=128
        	case 3  
        		cachesize=256
        	case 4  
        		cachesize=256
        		memwaitstate=0
       End Select
end Sub

Function mmutranslatereal(ByVal addr As ULong , ByVal rw As Integer ) As ULong 
        'Dim As Integer mmuout=0 
        Dim As ULong  addr2 
        Dim As ULong  temp,temp2,temp3 

        If abrt Then Return -1 'Print #5,"FALLO EN MMUTRANSLATEREAL":Return -1 

        addr2=cr3 + ((addr Shr 20) And &hFFC) 

        temp=leeram4(addr2) 'Shr 2) ' ----------------
        temp2=temp
        'For mmuout=-32 To 32 Step 4:Print Hex(leeram4(addr2+mmuout),8):Next
        'Print #5,"traslada ";Hex(addr,8),rw,Hex(temp,8),Hex(addr2,8),Hex(cr3)

        If (temp And 1)=0 Then 
        	'deb=2:printdebug()
                cr2=addr 
                temp = temp And 1 
                if (CPL=3) Then temp = temp Or 4 
                if rw Then temp = temp Or 2 
                abrt = ABRT_PF 
                abrt_error = temp 
                Return -1 
        EndIf
        temp=leeram4(((temp And inv(&hFFF))+((addr Shr 10) And &hFFC))) ' Shr 2) ' --------------
        temp3=temp And temp2 
        'Print #5,"traslada 2: ";Hex(temp,8),Hex(temp3,8)
        If ((temp And 1)=0)   Or   ((CPL=3) And ((temp3 And 4)=0)  And  (cpl_override=0)) _
        	    Or   ( (rw<>0)  And  ((temp3 And 2)=0)  And  ((CPL=3) Or (cr0 And WP_FLAG))) Then 
                cr2=addr 
                temp = temp And 1 
                if (CPL=3) Then temp = temp Or 4 
                if rw Then temp = temp Or 2 
                abrt = ABRT_PF 
                abrt_error = temp 
                Return -1 
        EndIf
        
        mmu_perm=temp And 4 
        'Print #5,Hex(addr,8),Hex((temp And inv(&hFFF))+(addr And &hFFF)),Hex(temp),Hex(cs0),Hex(pc),Hex(EDI):sleep
        grabaram4(addr2, leeram4(addr2) Or &h20) 
        temp3=(temp2 And inv(&hFFF)) + ((addr Shr 10) And &hFFC)') shr 2 ' ---------------
        grabaram4(temp3, leeram4(temp3) or iif(rw,&h60,&h20)) 

        return (temp And inv(&hFFF))+(addr And &hFFF) 
End Function



Sub addreadlookup(ByVal virt As ULong , ByVal phys As ULong )
'If virt<&h100000 Or virt>RAM_TOTAL Then Return
'print #5,"AddReadlookup:";Hex(virt);"  ";Hex(phys);"  ";Hex(cs0);"  ";Hex(ds0);"  ";Hex(es0);"  ";Hex(ss0);"  ";Hex(opcode);"  ";Hex(pc)
Return
        if readlookup2[virt Shr 12]<>&hFFFFFFFF Then Return 
        
        if cachelookup2[phys Shr 12]=0 Then 
'                readlnum+=1 
                cycles-=memwaitstate 
                if cachelookup(cachelnext) <> &hffffffff Then cachelookup2[cachelookup(cachelnext)] = 0 
                cachelookup(cachelnext) = phys Shr 12 
                cachelookup2[phys Shr 12] = 1 
                cachelnext = (cachelnext + 1) And (cachesize - 1) 
        EndIf
        
        if readlookup(readlnext)<>&hFFFFFFFF Then 
                readlookup2[readlookup(readlnext)]=&hFFFFFFFF 
        EndIf
        
        readlookup2[virt Shr 12]=phys And inv(&hFFF) 
        readlookupp(readlnext)=mmu_perm 
        readlookup(readlnext)=virt Shr 12: readlnext+=1
        readlnext = readlnext And (cachesize-1) 
End Sub

Sub addwritelookup(ByVal virt As ULong , ByVal phys As ULong ) 
'If virt<&h100000 Or virt>RAM_TOTAL Then Return
'print #5,"AddWritelookup:";Hex(virt);"  ";Hex(phys);"  ";Hex(virt Shr 12);"  ";Hex(writelookup2[virt Shr 12])
Return
        if writelookup2[virt Shr 12]<>&hFFFFFFFF Then Return 

        if cachelookup2[phys Shr 12]=0 Then 
'                writelnum+=1 
                cycles-=memwaitstate 
                if (cachelookup(cachelnext) <> &hffffffff) Then cachelookup2[cachelookup(cachelnext)] = 0 
                cachelookup(cachelnext) = phys Shr 12 
                cachelookup2[phys Shr 12] = 1 
                cachelnext = (cachelnext + 1) And (cachesize - 1) 
        EndIf
        
        cycles-=memwaitstate 
        if writelookup(writelnext)<>&hFFFFFFFF Then 
                writelookup2[writelookup(writelnext)]=&hFFFFFFFF 
        EndIf

        writelookup2[virt Shr 12]=phys And inv(&hFFF) 
        writelookupp(writelnext)=mmu_perm 
        writelookup(writelnext)=virt Shr 12: writelnext+=1 
        writelnext = writelnext And (cachesize-1) 
End Sub

























''''''''''''''''''''''''LEE BYTE'''''''''''''''''''''''''''''
Function readmemb_386l(ByVal seg As Ulong , ByVal addr As Ulong ) As UByte 

        If (seg=&hFFFFFFFF) Then 
        			 if (deb=3) Then print #5,"SEG:";Hex(seg,8);" ADDR:";Hex(addr,8)
                if (deb=3) Then print #5,"error segment rb FFFFFFFF":sleep
                return -1 
        EndIf
        mem_logical_addr = seg + addr
        addr = mem_logical_addr
                 
	        If (readlookup2[mem_logical_addr  Shr  12] <> &hFFFFFFFF) Then 
	                return leeram1(readlookup2[mem_logical_addr  Shr  12] + (mem_logical_addr  And &hFFF))
	        EndIf
        
        if (cr0  Shr  31) Then 
                addr = mmutranslatereal(addr, 0) 
                if (addr = &hFFFFFFFF) Then return &hFF 
        EndIf
        addr  = addr  And  rammask 

        'If (addr Shr 15)<(RAM_TOTAL Shr 15) Then
	        addreadlookup(mem_logical_addr, addr)
	        Return leeram1(addr)
        'EndIf
        
        'if (_mem_read_b[addr  Shr  15]) Then return _mem_read_b[addr  Shr  15](addr) 
        Return &hFF 
End Function
' esta llama a la anterior
Function readmemb_386(byval s As Ulong , byval a As ULong) As UByte
	Return iif( (readlookup2[(s+a) Shr 12]=&hFFFFFFFF) or (s=&hFFFFFFFF) ,  readmemb_386l(s,a), _
	       leeram1(readlookup2[(s+a) Shr 12] + ((s+a) And &hFFF))  )
End Function
''''''''''''''''''''''''''''''''''''''''''''''''''''''
































'''''''''''''''''''''' LEE PALABRA '''''''''''''''''''''''''''''

Function readmemw_386l(ByVal seg As Ulong , ByVal addr As ULong ) As UShort 
		  'Dim As ulong mem_logical_addr '''''''''''''''propia'''''''''''''''''''''''''''''''''''''''''''''
        Dim As ULong  addr2 =  seg + addr 
        mem_logical_addr = addr2

		  ' esta condicion solo se cumple si ADDR2=&HFFF nada mas.....
        if (addr2 And &hFFF)>&hFFE Then 
                if (cr0 Shr 31) Then 
                        If ( mmutranslatereal(addr2,   0) = &hffffffff ) Then Return 0
                        If ( mmutranslatereal(addr2+1, 0) = &hffffffff ) Then Return 0
                EndIf
                'if (is386) Then ' no lo necesito ya, siempre es 386 minimo
                	return readmemb_386l(seg,addr) Or (readmemb_386l(seg,addr+1) Shl 8) 
                'else
                '  Return readmembl(seg+addr) Or (readmembl(seg+addr+1) Shl 8) ' ya no me hace falta, no es 8086
                'EndIf
        EndIf
        if (seg=&hFFFFFFFF) Then 
                if (deb=3) Then print #5,"0 segment rw",CS1,cs0,pc,opcode,addr 
                return -1 
        EndIf
        if (cr0 Shr 31) Then 
                addr2=mmutranslatereal(addr2,0) 
                if (addr2=&hFFFFFFFF) Then return &hFFFF 
        EndIf
        addr2  = addr2  And  rammask 
		  'If (addr2 Shr 15)<(RAM_TOTAL Shr 15) Then
			  addreadlookup(mem_logical_addr, addr2)
	        Return leeram2(addr2)
		  'EndIf
        
        'if (_mem_read_w[addr2  Shr  15)) Then return _mem_read_w[addr2  Shr  15)(addr2) 
        'if (_mem_read_b[addr2  Shr  15)) Then return _mem_read_b[addr2  Shr  15)(addr2)  Or  (_mem_read_b[(addr2 + 1)  Shr  15)(addr2 + 1)  Shl  8) 
        Return &hffff 
End Function
' esta llama a la anterior
Function readmemw_386(byval s As Ulong , byval a As ULong) As UShort
	Return iif( (readlookup2[(s+a) Shr 12]=&hFFFFFFFF) or (s=&hFFFFFFFF)  or (((s+a) And &hFFF)>&hFFE)  , readmemw_386l(s,a), _
	       leeram2(readlookup2[(s+a) Shr 12] + ((s+a) And &hFFF)))
End Function
''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''



'''''''''''''''''''''' LEE LONG '''''''''''''''''''''''''''''''''''
Function readmeml_386l(ByVal seg As Ulong , ByVal addr As ULong ) As Ulong 
		  'Dim As ulong mem_logical_addr '''''''''''''''propia'''''''''''''''''''''''''''''''''''''''''''''
        Dim As ULong  addr2 =  seg + addr 
        mem_logical_addr = addr2
        
        if ((addr2 And &hFFF)>&hFFC) Then 
                if (cr0 Shr 31) Then 
                        if (mmutranslatereal(addr2,   0) = &hffffffff) Then return 0
                        if (mmutranslatereal(addr2+3, 0) = &hffffffff) Then return 0
                EndIf
                return readmemw_386(seg,addr) Or (readmemw_386(seg,addr+2) Shl 16) 
        EndIf
        if (seg=&hFFFFFFFF) Then 
                if deb=3 then print #5,"0 segment rl ",CS1,cs0,pc,opcode,addr 
                return -1 
        EndIf
        if (cr0 Shr 31) Then 
                addr2=mmutranslatereal(addr2,0) 
                if (addr2=&hFFFFFFFF) Then return &hFFFFFFFF 
        EndIf
        
        addr2 = addr2 And rammask 
        'If (addr2 Shr 15)<(RAM_TOTAL Shr 15) Then        
	        addreadlookup(mem_logical_addr, addr2)
			  Return leeram4(addr2)
        'EndIf
		  
        'if (_mem_read_l[addr2  Shr  15)) Then return _mem_read_l[addr2  Shr  15)(addr2) 
        'if (_mem_read_w[addr2  Shr  15)) Then return _mem_read_w[addr2  Shr  15)(addr2)  Or  (_mem_read_w[addr2  Shr  15)(addr2 + 2)  Shl  16) 
        'if (_mem_read_b[addr2  Shr  15)) Then return _mem_read_b[addr2  Shr  15)(addr2)  Or  (_mem_read_b[addr2  Shr  15)(addr2 + 1)  Shl  8)  Or  (_mem_read_b[addr2  Shr  15)(addr2 + 2)  Shl  16)  Or  (_mem_read_b[addr2  Shr  15)(addr2 + 3)  Shl  24) 
        Return &hffffffff 
End Function
' esta llama a la anterior
Function readmeml_386(byval s As Ulong , byval a As ULong) As ULong
	Return iif( (readlookup2[(s+a) Shr 12]=&hFFFFFFFF) or (s=&hFFFFFFFF)  or (((s+a) And &hFFF)>&hFFC)  ,readmeml_386l(s,a), _
	       leeram4(readlookup2[(s+a) Shr 12] + ((s+a) And &hFFF)))
End Function
'''''''''''''''''''''''''''''''''''''''''''''''''''''''''''






'''''''''''''''GRABA BYTE'''''''''''''''''''''''
Sub writememb_386l(ByVal seg As Ulong , ByVal addr As Ulong , ByVal valor As UByte ) 

        If (seg=&hFFFFFFFF) Then 
        	       if (deb=3) Then print #5,"SEG:";Hex(seg,8);" ADDR:";Hex(addr,8)
                if (deb=3) Then print #5,"error segment wb FFFFFFFF":sleep
                return 
        EndIf
        mem_logical_addr = addr + seg 
        addr = mem_logical_addr
        
        if (cr0  Shr  31) Then 
                addr = mmutranslatereal(addr, 1) 
                if (addr = &hFFFFFFFF) Then return 
        EndIf
        
        addr  = addr  And  rammask 
        'If (addr Shr 15)<(RAM_TOTAL Shr 15) Then
		     addwritelookup(mem_logical_addr, addr)
		     grabaram1(addr,valor)
        'EndIf
        
        'if (_mem_write_b[addr  Shr  15)) Then _mem_write_b[addr  Shr  15)(addr, valor) 
End Sub
' esta llama a la anterior
Sub writememb_386(ByVal s As Ulong, ByVal a As ULong, ByVal v As UByte) 
		If (writelookup2[(s+a) shr 12]=&hFFFFFFFF) Or (s=&hFFFFFFFF) Then 
			writememb_386l(s,a,v)
		else 
			grabaram1(writelookup2[(s+a) shr 12]+((s+a) And &hFFF),v)
		EndIf
End Sub
'''''''''''''''''''''''''''''''






'''''''''''''''GRABA PALABRA''''''''''''''''''''''''''''
Sub writememw_386l(ByVal seg As ULong , ByVal addr As Ulong , ByVal valor As UShort ) 
		  'Dim As ulong mem_logical_addr '''''''''''''''propia'''''''''''''''''''''''''''''''''''''''''''''
        Dim As ULong  addr2 =  seg + addr 
        mem_logical_addr = addr2
        
        ' esta condicion solo se cumple si ADDR2=&HFFF nada mas.....
        if ((addr2 And &hFFF)>&hFFE) Then 
                if (cr0 Shr 31) Then 
                        if (mmutranslatereal(addr2,   1) = &hffffffff) Then return 
                        if (mmutranslatereal(addr2+1, 1) = &hffffffff) Then return 
                EndIf
                'if (is386) Then ' ya no lo necesito, nunca sera menos de 386
                        writememb_386(seg,addr,valor) 
                        writememb_386(seg,addr+1,valor Shr 8) 
                'else
                '        writemembl(seg+addr,valor)  ' solo para el 8086
                '        writemembl(seg+addr+1,valor Shr 8) 
                'EndIf
                return 
        EndIf
        if (seg=&hFFFFFFFF) Then 
                if (deb=3) Then print #5,"0 segment ww",CS1,cs0,pc,opcode,addr 
                return 
        EndIf
        if (cr0 Shr 31) Then 
                addr2=mmutranslatereal(addr2,1) 
                if (addr2=&hFFFFFFFF) Then return 
        EndIf
        addr2  = addr2  And  rammask 
        'If (addr2 Shr 15)<(RAM_TOTAL Shr 15) Then		  
			  addwritelookup(mem_logical_addr, addr2)
	        grabaram2(addr2,valor)
        'EndIf
        
        'if (_mem_write_w[addr2  Shr  15)) Then 
        '        _mem_write_w[addr2  Shr  15)(addr2, valor) 
        '        return 
        'EndIf
        'if (_mem_write_b[addr2  Shr  15)) Then 
        '        _mem_write_b[addr2  Shr  15)(addr2, valor) 
        '        _mem_write_b[(addr2 + 1)  Shr  15)(addr2 + 1, val  Shr  8) 
        '        return 
        'EndIf
         
End Sub
' esta llama a la anterior
Sub writememw_386(ByVal s As Ulong, ByVal a As ULong, ByVal v As UShort)
		If  (writelookup2[(s+a) shr 12]=&hFFFFFFFF)  or  (s=&hFFFFFFFF) Or (((s+a) And &hFFF)>&hFFE) Then 
			writememw_386l(s,a,v)
		Else 
			grabaram2(writelookup2[(s+a) shr 12]+((s+a) And &hFFF),v)
		EndIf
End Sub
'''''''''''''''''''''''''''''''''''''''''''''





'''''''''''''''''''GRABA LONG''''''''''''''''''''''''''
Sub writememl_386l(ByVal seg As Ulong , ByVal addr As Ulong , ByVal valor As ULong ) 
		  'Dim As ulong mem_logical_addr '''''''''''''''propia'''''''''''''''''''''''''''''''''''''''''''''
        Dim As ULong  addr2 =  seg + addr 
        mem_logical_addr = addr2
        
        if ((addr2 And &hFFF)>&hFFC) Then 
                if (cr0 Shr 31) Then 
                        if (mmutranslatereal(addr2,   1) = &hffffffff) Then Return 
                        if (mmutranslatereal(addr2+3, 1) = &hffffffff) Then return 
                EndIf
                writememw_386l(seg,addr,valor) 
                writememw_386l(seg,addr+2,valor Shr 16) 
                return 
        EndIf
        if (seg=&hFFFFFFFF) Then 
                if deb=3 then print #5,"0 segment wl",CS1,cs0,pc,opcode,addr 
                return 
        EndIf
        if (cr0 Shr 31) Then 
                addr2=mmutranslatereal(addr2,1) 
                if (addr2=&hFFFFFFFF) Then return 
        EndIf
        addr2 = addr2 And rammask 
        'If (addr2 Shr 15)<(RAM_TOTAL Shr 15) Then
	        addwritelookup(mem_logical_addr, addr2)
	        grabaram4(addr2,valor)
        'EndIf
        
        'if (_mem_write_l[addr2  Shr  15)) Then 
        '        _mem_write_l[addr2  Shr  15)(addr2, valor) 
        '        return 
        'EndIf
        'if (_mem_write_w[addr2  Shr  15)) Then 
        '        _mem_write_w[addr2  Shr  15)(addr2,     valor) 
        '        _mem_write_w[addr2  Shr  15)(addr2 + 2, valor  Shr  16) 
        '        return 
        'EndIf
        'if (_mem_write_b[addr2  Shr  15)) Then 
        '        _mem_write_b[addr2  Shr  15)(addr2,     valor) 
        '        _mem_write_b[addr2  Shr  15)(addr2 + 1, valor  Shr  8) 
        '        _mem_write_b[addr2  Shr  15)(addr2 + 2, valor  Shr  16) 
        '        _mem_write_b[addr2  Shr  15)(addr2 + 3, valor  Shr  24) 
        '        return 
        'EndIf

End Sub
' esta llama a la anterior
Sub writememl_386(ByVal s As Ulong, ByVal a As ULong, ByVal v As ULong) 
		if (writelookup2[(s+a) shr 12]=&hFFFFFFFF) Or (s=&hFFFFFFFF) or (((s+a) And &hFFF)>&hFFC) Then 
			writememl_386l(s,a,v)
		else 
			grabaram4(writelookup2[(s+a) Shr 12]+((s+a) And &hFFF),v)
		EndIf
End Sub
''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''















' Esta rutina se llama solo desde el teclado, modulo "keyboard_at", runtina "keyboard_at_write"
' pero NO es para el teclado, sino para habilitar A20, y permitir mas de 1mb de RAM en 286 o posterior
' no entiendo por que lo activa el teclado, pero en internet, la gente se pregunta lo mismo!!!!
Sub mem_a20_recalc() 
        Dim As Integer state = mem_a20_key  Or  mem_a20_alt
        if (state<>0) And (mem_a20_state=0) Then 
            rammask = &hffffffff 
            flushmmucache() 
        elseif (state=0) And (mem_a20_state<>0) Then
				rammask = &hffefffff 
            flushmmucache() 
        EndIf
        'print #5,"rammask ahora ";Hex( rammask,8) 
        mem_a20_state = state 
End Sub









''''''''''''''''''''''''''''''''''''''''
' estas parecen usarse solo en X86SEG.C
''''''''''''''''''''''''''''''''''''''''
' esta es llamada unicamente por "writememb_x86", la de debajo suya
Sub writemembl_x86(ByVal addr As Ulong , ByVal valor As UByte ) 
        mem_logical_addr = addr 
        if (cr0 Shr 31) Then 
                addr = mmutranslatereal(addr,1) 
                if (addr = &hFFFFFFFF) Then return 
        EndIf
        addr  = addr  And  rammask 
        'If (addr Shr 15)<(RAM_TOTAL Shr 15) Then        
	        addwritelookup(mem_logical_addr, addr)
	        grabaram1(addr,valor)
        'EndIf
        
        'if (_mem_write_b[addr  Shr  15]) Then _mem_write_b[addr  Shr  15](addr, valor) 
End Sub
' esta llama unicamente a la de arriba de esta
Sub writememb_x86(ByVal a As ULong , ByVal v As UByte ) 
     'memcycs+=4 ' no se usa?
     if (writelookup2[a Shr 12]=&hFFFFFFFF) Then 
             writemembl_x86(a,v) ' la rutina de arriba de este grupo
     else
             grabaram1(writelookup2[a Shr 12]+(a And &hFFF),v)
     EndIf
End Sub



Sub writememw_x86(ByVal s As Ulong , ByVal a As ULong , ByVal v As UShort ) 
     'memcycs+=8 'Shr is8086)  ' ojo con esto, que NO deberia ser 8086
     if (writelookup2[(s+a) Shr 12]=&hFFFFFFFF)  Or  (s=&hFFFFFFFF) Then 
             writememw_386l(s,a,v) 
     else
             grabaram2(writelookup2[(s+a) Shr 12]+((s+a) And &hFFF),v)
     EndIf
End Sub

Sub writememl_x86(ByVal s As Ulong , ByVal a As Ulong , ByVal v As ULong ) 
     if (writelookup2[(s+a) Shr 12]=&hFFFFFFFF)  Or  (s=&hFFFFFFFF) Then 
             writememl_386l(s,a,v) 
     else
             grabaram4(writelookup2[(s+a) Shr 12]+((s+a) And &hFFF),v)
     EndIf
End Sub


' de lectura BYTE en X86_SEG unicamente y solo tres llamadas en TASKSWITCH
Function readmembl_x86(ByVal addr As ULong ) As UByte 
		 ' al parecer, solo lo usan las dos siguientes rutinas (aqui abajo ambas)
        mem_logical_addr = addr 
        if (cr0  Shr  31) Then 
                addr = mmutranslatereal(addr, 0) 
                if (addr = &hFFFFFFFF) Then return &hFF 
        EndIf
        addr  = addr  And  rammask 
        'If (addr Shr 15)<(RAM_TOTAL Shr 15) Then
	        addreadlookup(mem_logical_addr, addr)
	        Return leeram1(addr)
        'EndIf
        
        'if (_mem_read_b[addr  Shr  15]) Then return _mem_read_b[addr  Shr  15](addr) 
        return &hFF 
End Function

' esta es exclusivamente para el modulo "taskswitch286" del modulo X86SEG.C, y llama a la anterior a esta
Function readmemb_x86(ByVal a As Ulong ) As UByte 
     'if (a<>(cs0+pc)) Then memcycs+=4 ' no se de donde he sacado esto, pero en el original no va, lo quito por ahora
     if (readlookup2[a Shr 12]=&hFFFFFFFF) Then 
             return readmembl_x86(a) ' rutina anterior a esta
     Else
             return leeram1(readlookup2[a Shr 12]+(a And &hFFF))
     EndIf
End Function
''''''''''''''''''''''''''''''''''
















'''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''
''''' rutinas x86 ''''''

''''''''''''''''''''''''''''
' se usan en el modulo X86SEG.C nada mas, y es MUY, MUY importante, se emplea el 99% de las llamadas
'Function readmemw_x86(byval s As Ulong , ByVal a As ULong ) As UShort 
'     'if (a<>(cs0+pc)) Then memcycs+=8' Shr is8086) ' ojo con esto, que NO deberia ser 8086
'     if (readlookup2[(s+a) Shr 12]=&hFFFFFFFF)  Or  (s=&hFFFFFFFF) Or (((s+a) And &hFFF)>&hFFE) Then 
'             return readmemw_386(s,a) 
'     Else
'             return leeram2(readlookup2[(s+a) Shr 12]+((s+a) And &hFFF))
'     EndIf
'End Function


'Function readmeml_x86(byval s As Ulong , byval a As ULong) As ULong
'	Return IIf (    (readlookup2[(s+a) shr 12]=&hFFFFFFFF)  Or  (s=&hFFFFFFFF)    ,    readmeml_386l(s,a)    , _
'	         (leeram4(readlookup2[(s+a) Shr 12]+((s+a) And &hFFF)))   )
'End Function
'''''''''''''''''''''''''''''''







'''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''
' rutina UNICAMENTE para FETCHCOMPLETE,  nadie mas lo usa.
Function readmembf_x86(ByVal a As ULong ) As UByte 
     if (readlookup2[a Shr 12]=&hFFFFFFFF) Then 
             return readmembl_x86(a)  ' rutina anterior a esta
     else
             return leeram1(readlookup2[a Shr 12]+(a And &hFFF))
     EndIf
End Function
''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''










Function getpccache(ByVal a As ULong ) As UByte Ptr
        Dim As ULong a2=a 
     'Print #5,"getpccache:";Hex(readlookup2[a Shr 12]),Hex(a,8)
        If (readlookup2[a Shr 12]<>&hFFFFFFFF) Then 
        	'Print #5,Hex(a,8),Hex(readlookup2[a Shr 12] - (a And inv(&hFFF))):sleep
        	Return @ram[readlookup2[a Shr 12] - (a And inv(&hFFF))]
        EndIf

        If (cr0 Shr 31) Then 
		  		pctrans=1 
            a=mmutranslatereal(a,0) 
            pctrans=0 
            if (a=&hFFFFFFFF) Then Return ram ' es como devolver "@ram[0]", la primera direccion. 
        EndIf

        a = a And rammask 

        If (isram(a Shr 16)) Then 
        	   ' solo en zonas RAM (entre la 0 y &hA0000) y desde la FFFFF hasta acabar los megas)
             If ((a Shr 16)<>&hF) Or (shadowbios<>0) Then addreadlookup(a2,a) 
             'If ((a And &hFFFFF000) - (a2 And inv(&hFFF)))<>0 Then 
             '	Print #5,Hex(a And &hFFFFF000), hex(a2 And inv(&hFFF)),Hex((a And &hFFFFF000) - (a2 And inv(&hFFF))):sleep
             'EndIf
             Return @ram[(a And &hFFFFF000) - (a2 And inv(&hFFF))] 
        EndIf

        Select Case As Const  (a Shr 16)
        	case &hC  ' direccion C0000 VROM
        		' if (a and &h 8000) then return @romext ....  aqui va la lectura de ROMEXT, pero lo tengo desactivado
            Return @vrom[(a And &h7000) - (a2 And inv(&hFFF))]
        	case &hE  ' direccion E0000 ROM
        		If shadowbios Then Return @ram[ (a And &hFFF000) - (a2 And inv(&hFFF)) ] 
            Return @rom[((a And biosmask) And inv(&hfff)) - (a2 And inv(&hFFF))] ' ROM -->>ffff antes biosmask
        	case &hF  ' direccion F0000 ROM
        		If shadowbios Then Return @ram[ (a And &hFFF000) - (a2 And inv(&hFFF)) ] 
            Return @rom[((a And biosmask) And inv(&hfff)) - (a2 And inv(&hFFF))] ' ROM
        End Select

       return  @rom[(a And &hF000) - (a2 And inv(&hFFF))]

End Function




''''''' rutinas 386 ''''''''
Function fastreadw(ByVal a As ULong ) As UShort ' devuelve 16 bits, 2 bytes
        Dim As ULong t 

        ' esta condicion solo se cumple si ADDR2=&HFFF nada mas.....
        If ((a And &hFFF)>&hFFE) Then 
                t=readmemb_386(0,a) 
                t = t Or (readmemb_386(0,a+1) Shl 8) 
                return t
        EndIf
        ' si la lectura esta dentro de 4096 bytes, usa el cache de 4k para leer
        If ((a Shr 12)=pccache) Then Return (pccache2[a+1] Shl 8) Or pccache2[a]
        pccache2=getpccache(a)
        pccache=a Shr 12 
        return (pccache2[a+1] Shl 8) or pccache2[a]			
End Function

Function fastreadl(ByVal a As ULong ) As ULong ' devuelve 32bits (4 bytes)
     Dim As UByte Ptr t 
     Dim As ULong valor 

     ' si la lectura esta dentro de 4096 bytes, usa el cache de 4k para leer
     If ((a And &hFFF)<&hFFD) Then 
       if ((a Shr 12)<>pccache) Then 
       	'print #5,"fastreadl:";hex(a,8)':Sleep
            t = getpccache(a) 
            If abrt Then Print #5,"Error en fastreadl":Return 0 
            pccache2 = t 
            pccache=a Shr 12 
       EndIf
       Return (pccache2[a+3] Shl 24) Or (pccache2[a+2] Shl 16) Or (pccache2[a+1] Shl 8) Or pccache2[a]
     EndIf

     valor  = readmemb_386(0,a) 
     valor  = valor  Or (readmemb_386(0,a+1) Shl 8) 
     valor  = valor  Or (readmemb_386(0,a+2) Shl 16) 
     valor  = valor  Or (readmemb_386(0,a+3) Shl 24) 

     return valor 
End Function
